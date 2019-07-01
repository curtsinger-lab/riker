#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <sys/types.h>
#include <fcntl.h>

#include <capnp/message.h>
#include <capnp/serialize.h>
#include <iostream>

#include "db.capnp.h"
#include "middle.h"

//TODO fix collapse & race handling

/* ------------------------------ Command Methods -----------------------------------------*/
Command::Command(trace_state* state, std::string cmd) : state(state), cmd(cmd) {}

void Command::add_input(File* f) {
    // search through all files, do versioning
    f->interactions.push_front(this);
    f->users.insert(this);
    this->inputs.insert(f);
    // if we've read from the file previously, check for a race
    for (auto rd : this->rd_interactions) {
        if (f->filename == rd->filename && f->writer != this) {
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
        if (f->filename == wr->filename) {
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
    command_list[start_index].setExecutable(::capnp::Data::Reader((const ::kj::byte*) command->cmd.data(), command->cmd.size()));
    command_list[start_index].setOutOfDate(false);
    auto argv = command_list[start_index].initArgv(command->args.size());
    uint64_t argv_index = 0;
    for (auto arg : command->args) {
        argv.set(argv_index, ::capnp::Data::Reader((const ::kj::byte*) arg.data(), arg.size()));
        argv_index++;
    }

    uint64_t index = start_index + 1;
    for (auto c : command->children) {
        index = serialize_commands(c, command_list, index, command_ids);
    }

    command_list[start_index].setDescendants(index - start_index - 1);
    return index;
}

/* ------------------------------- File Methods -------------------------------------------*/
File::File(std::string path, Command* writer, trace_state* state) : filename(path), writer(writer), state(state) {
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
    File* f = new File(this->filename, this->writer, this->state);
    f->version = this->version + 1;
    return f;
}

/* ----------------------------- Process Methods ------------------------------------------*/
Process::Process(std::string cwd, Command* command) : cwd(cwd), command(command) {}

/* -------------------------- Trace_State Methods ----------------------------------------*/
// return latest version of the file, or create file and add to record if not found
File* trace_state::find_file(std::string path) {
    File* ret = NULL;
    for (auto f : this->files) {
        if (f->filename == path) {
            if (ret == NULL || f->version > ret->version) {
                ret = f;
            }
        }
    }
    if (ret == NULL) {
        ret = new File(path, NULL, this);
        this->files.insert(ret);
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
        files[file_id].setPath(::capnp::Data::Reader((const ::kj::byte*) file->filename.data(), file->filename.size()));

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


    // TODO: Is kj::OutputStream suitable?
    int db_file = open("db.dodo", O_CREAT | O_TRUNC | O_WRONLY, S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH);
    if (db_file < 0) {
        perror("Failed to open database");
        exit(2);
    }
    writeMessageToFd(db_file, message);
}

void trace_state::add_dependency(pid_t thread_id, struct file_reference file, enum dependency_type type) {
    std::string path;
    Process* proc = this->processes.find(thread_id)->second;
    if (file.fd == AT_FDCWD) {
        path = std::string(file.path);
    } else {
        fprintf(stdout, "[%d] Dep: %d -> ",thread_id, file.fd);
        if (proc->fds.find(file.fd) == proc->fds.end()) {
            path = "file not found, fd: " + std::to_string(file.fd);
        } else {
            path = proc->fds.find(file.fd)->second;
        }
    }
    File* f = this->find_file(path);

    fprintf(stdout, "file: %s-%d ", path.c_str(), f->version);
    switch (type) {
        case DEP_READ:
            fprintf(stdout, "read");
            if (f->can_depend(proc->command)) {
                proc->command->add_input(f);
                fprintf(stdout, ", depend");
            }
            break;
        case DEP_MODIFY:
            fprintf(stdout, "modify");
            proc->command->add_output(f);
            f->dependable = true;
            f->writer = proc->command;
            break;
        case DEP_CREATE:
            fprintf(stdout, "create");
            // create edge
            break;
        case DEP_REMOVE:
            fprintf(stdout, "remove");
            // TODO deletion edges
            break;
    }
    if (!file.follow_links) {
        fprintf(stdout, " (nofollow)");
    }
    if (file.path == NULL) {
        fprintf(stdout, " FD %d\n", file.fd);
    } else if (file.fd == AT_FDCWD) {
        fprintf(stdout, " %s\n", file.path);
    } else {
        fprintf(stdout, " {%d/}%s\n", file.fd, file.path);
    }
}

void trace_state::add_change_cwd(pid_t thread_id, struct file_reference file) {
    fprintf(stdout, "[%d] Change working directory to ", thread_id);
    this->processes.find(thread_id)->second->cwd = file.path;
    if (file.fd == AT_FDCWD) {
        fprintf(stdout, "%s\n", file.path);
    } else {
        fprintf(stdout, "{%d/}%s\n", file.fd, file.path);
    }
}

void trace_state::add_change_root(pid_t thread_id, struct file_reference file) {
    fprintf(stdout, "[%d] Change root to ", thread_id);
    this->processes.find(thread_id)->second->root = file.path;
    if (file.fd == AT_FDCWD) {
        fprintf(stdout, "%s\n", file.path);
    } else {
        fprintf(stdout, "{%d/}%s\n", file.fd, file.path);
    }
}


// get filenames from their open
void trace_state::add_open(pid_t thread_id, int fd, struct file_reference file, int access_mode, bool is_rewrite) {
    fprintf(stdout, "[%d] Open %d -> ", thread_id, fd);
    // TODO take into account root and cwd
    Process* cur_proc = this->processes.find(thread_id)->second;
    File* f = this->find_file(file.path);
    if (is_rewrite) {
        fprintf(stdout, "REWRITE ");
        f = f->make_version();
        f->dependable = false;
        this->files.insert(f);
    }
    cur_proc->fds.insert(std::pair<int, std::string>(fd,file.path));
    if (file.fd == AT_FDCWD) {
        fprintf(stdout, "%s\n", file.path);
    } else {
        fprintf(stdout, "{%d/}%s\n", file.fd, file.path);
    }
}

// TODO handle pipes
void trace_state::add_pipe(pid_t thread_id, int fds[2]) {
    fprintf(stdout, "[%d] Pipe %d, %d\n", thread_id, fds[0], fds[1]);
}

void trace_state::add_dup(pid_t thread_id, int duped_fd, int new_fd) {
    fprintf(stdout, "[%d] Dup %d <- %d\n", thread_id, duped_fd, new_fd);
    Process* proc = this->processes.find(thread_id)->second;
    proc->fds.insert(std::pair<int, std::string>(new_fd, proc->fds.find(duped_fd)->second));
}

// TODO deal with race conditions
void trace_state::add_mmap(pid_t thread_id, int fd) {
    // TODO: look up the permissions that the file was opened with
    fprintf(stdout, "[%d] Mmap %d\n", thread_id, fd);
}

void trace_state::add_close(pid_t thread_id, int fd) {
    fprintf(stdout, "[%d] Close %d\n", thread_id, fd);
    this->processes.find(thread_id)->second->fds.erase(fd);
}

// create process node
void trace_state::add_fork(pid_t parent_thread_id, pid_t child_process_id) {
    fprintf(stdout, "[%d] Fork %d\n", parent_thread_id, child_process_id);
    Process* parent_proc = this->processes.find(parent_thread_id)->second;
    this->processes.insert(std::pair<pid_t, Process*>(child_process_id, new Process(parent_proc->cwd, parent_proc->command)));
}

// fill in process node
void trace_state::add_exec(pid_t process_id, char* exe_path) {
    fprintf(stdout, "[%d] Inside exec: %s\n", process_id, exe_path);
    Process* cur_proc = this->processes.find(process_id)->second;
    Command* cmd = new Command(this, exe_path);
    std::cout << "Pushed " << cmd->cmd << " to commands list!\n";
    cur_proc->command->children.push_front(cmd);
    cur_proc->command = cmd;
    free(exe_path);
}

void trace_state::add_exec_argument(pid_t process_id, char* argument, int index) {
    Process* cur_proc = this->processes.find(process_id)->second;
    std::string arg = std::string(argument);
    cur_proc->command->args.push_back(arg);
    fprintf(stdout, "[%d]     Arg %d: %s\n", process_id, index, argument);
    free(argument);
}

// TODO close relevant mmaps
void trace_state::add_exit(pid_t thread_id) {
    fprintf(stdout, "[%d] Exit\n", thread_id);
}

