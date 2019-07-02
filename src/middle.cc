#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <sys/types.h>
#include <fcntl.h>
#include <sys/mman.h>

#include <capnp/message.h>
#include <capnp/serialize.h>
#include <iostream>
#include <experimental/filesystem>
namespace fs = std::experimental::filesystem;

#include "db.capnp.h"
#include "middle.h"

//TODO fix collapse & race handling

/* ------------------------------ Command Methods -----------------------------------------*/
Command::Command(trace_state* state, Blob&& cmd) : state(state), cmd(std::move(cmd)) {}

void Command::add_input(File* f) {
    // search through all files, do versioning
    f->interactions.push_front(this);
    f->users.insert(this);
    this->inputs.insert(f);
    // if we've read from the file previously, check for a race
    for (auto rd : this->rd_interactions) {
        if (f->filename.asPtr() == rd->filename.asPtr() && f->writer != this) {
            if (f->version == rd->version) {
                return;
            } else /* we've found a race */ {
                rd->collapse();
                this->has_race = true;
            }
        }
    }
    // mark that we've now interacted with this version of the file
    this->rd_interactions.insert(f);
}

void Command::add_output(File* f) {
    // if we've written to the file before, check for a race
    for (auto wr : this->wr_interactions) {
        if (f->filename.asPtr() == wr->filename.asPtr()) {
            f->producers.insert(this);
            this->outputs.insert(f);
            if (f->version == wr->version) {
                return;
            } else {
                wr->collapse();
                // TODO collapse all intermediates
                //for (auto it = this->state->files()
                //wr->has_race = true;
                this->has_race = true;
                return;
            }
        }
    }
    // if we haven't written to this file before, create a new version
    f->interactions.push_front(this);
    File* fnew = f->make_version();
    fnew->writer = this;
    fnew->producers.insert(this);
    this->outputs.insert(fnew);
    this->wr_interactions.insert(fnew);
    this->state->files.insert(fnew);
}

uint64_t Command::descendants(void) {
    uint64_t ret = 0;
    for (auto c : this->children) {
        ret += 1 + c->descendants();
    }
    return ret;
}

static uint64_t serialize_commands(Command* command, ::capnp::List<db::Command>::Builder& command_list, uint64_t start_index, std::map<Command*, uint64_t>& command_ids) {
    command_ids.insert(std::pair<Command*,uint64_t>(command, start_index));
    command_list[start_index].setExecutable(command->cmd);
    command_list[start_index].setOutOfDate(false);
    auto argv = command_list[start_index].initArgv(command->args.size());
    uint64_t argv_index = 0;
    for (auto arg = command->args.begin(); arg != command->args.end(); ++arg) {
        argv.set(argv_index, (*arg).asPtr());
        argv_index++;
    }

    uint64_t index = start_index + 1;
    for (auto c : command->children) {
        index = serialize_commands(c, command_list, index, command_ids);
    }

    command_list[start_index].setDescendants(index - start_index - 1);
    return index;
}

void Command::rerun_children(std::set<Command*>* to_rerun) {
    (*to_rerun).insert(this);
    for (auto o = this->outputs.begin(); o != this->outputs.end(); ++o) {
        for (auto user = (*o)->users.begin(); user != (*o)->users.end(); ++user) {
            (*user)->rerun_children(to_rerun);
        }
    }
}

// TODO propagate
void Command::print_changes(std::vector<Blob>& changes, std::set<Command*>* to_rerun) {
    for (unsigned int ch = 0; ch < changes.size(); ch++) {
        for (auto i : this->inputs) {
            //std::cout << i->filename << " vs " << changes[ch] << "\n";
            if (changes[ch].asPtr() == i->filename) {
                this->rerun_children(to_rerun);
                //for (auto o : this->outputs) {
                 //   changes.push_back(o->filename);
                //}
            }
        }
    }
    for (auto c : this->children) {
        c->print_changes(changes, to_rerun);
    }
}

/* ------------------------------- File Methods -------------------------------------------*/
File::File(Blob&& path, Command* writer, trace_state* state) : filename(std::move(path)), writer(writer), state(state) {
    this->dependable = true;
}

//TODO fix
void File::collapse(void) {
    //this->has_race = true;
    for (auto interaction : this->interactions) {
        //interaction->has_race = true;
        this->conflicts.push_front(interaction);
    }
}

// file can only be depended on if it wasn't truncated/created, and if the current command isn't
// the only writer
bool File::can_depend(Command* cmd) {
    if (this->writer == cmd) {
        return false;
    } else {
        return this->dependable;
    }
}

File* File::make_version(void) {
    File* f = new File(kj::heapArray(this->filename.asPtr()), this->writer, this->state);
    f->version = this->version + 1;
    return f;
}

/* ----------------------------- Process Methods ------------------------------------------*/
Process::Process(pid_t thread_id, Blob&& cwd, Command* command) : thread_id(thread_id), cwd(std::move(cwd)), command(command) {}

/* -------------------------- Trace_State Methods ----------------------------------------*/
// return latest version of the file, or create file and add to record if not found
File* trace_state::find_file(BlobPtr path) {
    File* ret = NULL;
    for (auto f : this->files) {
        if (f->filename.asPtr() == path) {
            if (ret == NULL || f->version > ret->version) {
                ret = f;
            }
        }
    }
    if (ret == NULL) {
        ret = new File(heapArray(path), NULL, this);
        this->files.insert(std::move(ret));
    }
    return ret;
}

void trace_state::serialize_graph(void) {
    ::capnp::MallocMessageBuilder message;

    db::Graph::Builder graph = message.initRoot<db::Graph>();

    // Serialize commands
    std::map<Command*, uint64_t> command_ids;
    uint64_t command_count = 0;
    for (auto c : this->commands) {
        command_count += 1 + c->descendants();
    }
    auto commands = graph.initCommands(command_count);
    uint64_t command_index = 0;
    for (auto c : this->commands) {
        command_index = serialize_commands(c, commands, command_index, command_ids);
    }

    // Serialize files
    std::map<File*, uint64_t> file_ids;
    uint64_t file_count = 0;
    uint64_t input_count = 0;
    uint64_t output_count = 0;
    for (auto file : this->files) {
        // We only care about files that are either written or read, so filter those out.
        if (!file->users.empty() || !file->producers.empty()) {
            file_ids.insert(std::pair<File*, uint64_t>(file, file_count));
            input_count += file->users.size();
            output_count += file->producers.size();
            file_count += 1;
        }
    }
    auto files = graph.initFiles(file_count);
    for (auto file_entry : file_ids) {
        auto file = file_entry.first;
        auto file_id = file_entry.second;
        files[file_id].setPath(file->filename.asPtr());

        // TODO: type and checksum/state

    }

    // Serialize dependencies
    auto inputs = graph.initInputs(input_count);
    auto outputs = graph.initOutputs(output_count);
    uint64_t input_index = 0;
    uint64_t output_index = 0;
    for (auto file_entry : file_ids) {
        auto file = file_entry.first;
        auto file_id = file_entry.second;
        for (auto user : file->users) {
            uint64_t user_id = command_ids[user];
            inputs[input_index].setFileID(file_id);
            inputs[input_index].setCommandID(user_id);
            input_index++;
        }
        for (auto producer : file->producers) {
            uint64_t producer_id = command_ids[producer];
            outputs[output_index].setFileID(file_id);
            outputs[output_index].setCommandID(producer_id);
            output_index++;
        }
    }

    // Serialize removal edges
    uint64_t removal_count = 0;
    for (auto c : command_ids) {
        removal_count += c.first->deleted_files.size();
    }
    auto removals = graph.initRemovals(removal_count);
    uint64_t removal_index = 0;
    for (auto c : command_ids) {
        for (auto f : c.first->deleted_files) {
            removals[removal_index].setFileID(file_ids[f]);
            removals[removal_index].setCommandID(c.second);
            removal_index++;
        }
    }

    // TODO: Is kj::OutputStream suitable?
    int db_file = open("db.dodo", O_CREAT | O_TRUNC | O_WRONLY, S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH);
    if (db_file < 0) {
        perror("Failed to open database");
        exit(2);
    }
    writeMessageToFd(db_file, message);
}

void trace_state::add_dependency(Process* proc, struct file_reference& file, enum dependency_type type) {
    BlobPtr path;
    Blob path_buf;
    if (file.fd == AT_FDCWD) {
        path = file.path.asPtr();
    } else {
        //fprintf(stdout, "[%d] Dep: %d -> ", proc->thread_id, file.fd);
        if (proc->fds.find(file.fd) == proc->fds.end()) {
            // TODO: Use a proper placeholder and stop fiddling with string manipulation
            std::string path_str = "file not found, fd: " + std::to_string(file.fd);
            path_buf = kj::heapArray((const kj::byte*) path_str.data(), path_str.size());
            path = path_buf.asPtr();
        } else {
            path = proc->fds.find(file.fd)->second.path.asPtr();
        }
    }
    File* f = this->find_file(path);

    //fprintf(stdout, "file: %.*s-%d ", (int)path.size(), path.asChars().begin(), f->version);
    switch (type) {
        case DEP_READ:
           // fprintf(stdout, "read");
            if (f->can_depend(proc->command)) {
                proc->command->add_input(f);
                //fprintf(stdout, ", depend");
            }
            break;
        case DEP_MODIFY:
            //fprintf(stdout, "modify");
            proc->command->add_output(f);
            f->dependable = true;
            f->writer = proc->command;
            break;
        case DEP_CREATE:
            //fprintf(stdout, "create");
            // create edge
            f = f->make_version();
            f->dependable = false;
            this->files.insert(f);
            proc->command->add_output(f);
            f->writer = proc->command;
            break;
        case DEP_REMOVE:
            //fprintf(stdout, "remove");
            proc->command->deleted_files.insert(f);    
            break;
    }
    if (!file.follow_links) {
        //fprintf(stdout, " (nofollow)");
    }
    if (file.path == NULL) {
        //fprintf(stdout, " FD %d\n", file.fd);
    } else if (file.fd == AT_FDCWD) {
        //fprintf(stdout, " %.*s\n", (int)file.path.size(), file.path.asChars().begin());
    } else {
        //fprintf(stdout, " {%d/}%.*s\n", file.fd, (int)file.path.size(), file.path.asChars().begin());
    }
}

void trace_state::add_change_cwd(Process* proc, struct file_reference& file) {
    //fprintf(stdout, "[%d] Change working directory to ", proc->thread_id);
    proc->cwd = kj::heapArray(file.path.asPtr());
    if (file.fd == AT_FDCWD) {
        //fprintf(stdout, " %.*s\n", (int)file.path.size(), file.path.asChars().begin());
    } else {
        //fprintf(stdout, " {%d/}%.*s\n", file.fd, (int)file.path.size(), file.path.asChars().begin());
    }
}

void trace_state::add_change_root(Process* proc, struct file_reference& file) {
    //fprintf(stdout, "[%d] Change root to ", proc->thread_id);
    proc->root = kj::heapArray(file.path.asPtr());
    if (file.fd == AT_FDCWD) {
        //fprintf(stdout, " %.*s\n", (int)file.path.size(), file.path.asChars().begin());
    } else {
        //fprintf(stdout, " {%d/}%.*s\n", file.fd, (int)file.path.size(), file.path.asChars().begin());
    }
}


// get filenames from their open
void trace_state::add_open(Process* proc, int fd, struct file_reference& file, int access_mode, bool is_rewrite) {
    //fprintf(stdout, "[%d] Open %d -> ", proc->thread_id, fd);
    // TODO take into account root and cwd
    File* f = this->find_file(file.path.asPtr());
    if (is_rewrite) {
        //fprintf(stdout, "REWRITE ");
        f = f->make_version();
        f->dependable = false;
        this->files.insert(f);
    }
    FileDescriptor desc;
    desc.path = kj::heapArray(file.path.asPtr());
    desc.access_mode = access_mode;
    proc->fds.insert(std::pair<int, FileDescriptor>(fd, std::move(desc)));
    if (file.fd == AT_FDCWD) {
        //fprintf(stdout, " %.*s\n", (int)file.path.size(), file.path.asChars().begin());
    } else {
        //fprintf(stdout, " {%d/}%.*s\n", file.fd, (int)file.path.size(), file.path.asChars().begin());
    }
}

// TODO handle pipes
void trace_state::add_pipe(Process* proc, int fds[2]) {
    //fprintf(stdout, "[%d] Pipe %d, %d\n", proc->thread_id, fds[0], fds[1]);
}

void trace_state::add_dup(Process* proc, int duped_fd, int new_fd) {
    //fprintf(stdout, "[%d] Dup %d <- %d\n", thread_id, duped_fd, new_fd);
    auto duped_file = proc->fds.find(duped_fd);
    if (duped_file != proc->fds.end()) {
        FileDescriptor new_file;
        new_file.path = kj::heapArray(duped_file->second.path.asPtr());
        new_file.access_mode = duped_file->second.access_mode;
        proc->fds.insert(std::pair<int, FileDescriptor>(new_fd, std::move(new_file)));
    }
}

// TODO deal with race conditions
void trace_state::add_mmap(Process* proc, int fd) {
    //fprintf(stdout, "[%d] Mmap %d\n", proc->thread_id, fd);
    FileDescriptor& desc = proc->fds.find(fd)->second;
    File* f = this->find_file(desc.path);
    // TODO: why is this useful?
    //if (f->filename.find("/lib/")!=std::string::npos) {
    //    return;
    //}
    f->mmaps.insert(proc);
    proc->mmaps.insert(f);
    std::cout << "MMAP ";
    if (desc.access_mode != O_WRONLY) {
        std::cout << "read ";
        proc->command->add_input(f);
    }
    if (desc.access_mode != O_RDONLY) {
        std::cout << "write";
        proc->command->add_output(f);
    }
    std::cout << "\n";
}

void trace_state::add_close(Process* proc, int fd) {
    //fprintf(stdout, "[%d] Close %d\n", proc->thread_id, fd);
    proc->fds.erase(fd);
}

// create process node
void trace_state::add_fork(Process* parent_proc, pid_t child_process_id) {
    //fprintf(stdout, "[%d] Fork %d\n", parent_proc->thread_id, child_process_id);
    this->processes.insert(std::pair<pid_t, Process*>(child_process_id, new Process(child_process_id, kj::heapArray(parent_proc->cwd.asPtr()), parent_proc->command)));
}

void trace_state::add_exec(Process* proc, Blob&& exe_path) {
    //fprintf(stdout, "[%d] Inside exec: %.*s\n", proc->thread_id, (int) exe_path.size(), exe_path.asChars().begin());
    Command* cmd = new Command(this, std::move(exe_path));
    //fprintf(stdout, "Pushed %.*s to commands list!\n", (int) cmd->cmd.size(), cmd->cmd.asChars().begin());
    proc->command->children.push_front(cmd);
    proc->command = cmd;
}

void trace_state::add_exec_argument(Process* proc, Blob&& argument, int index) {
    //fprintf(stdout, "[%d]     Arg %d: %.*s\n", process_id, index, (int)argument.size(), argument.asChars().begin());
    proc->command->args.push_back(std::move(argument));
}

void trace_state::add_exit(Process* proc) {
    //fprintf(stdout, "[%d] Exit\n", proc->thread_id);
    for (auto f = proc->mmaps.begin(); f != proc->mmaps.end(); ++f) {
        (*f)->mmaps.erase(proc);
    }
}

void trace_state::print_changes(std::vector<Blob>& changes) {
    if (changes.size() == 0) {
        return;
    }
    std::set<Command*> to_rerun;
    for (auto c : this->commands) {
        c->print_changes(changes, &to_rerun);
    }
    for (auto r : to_rerun) {
        //r->print();
    }
}




