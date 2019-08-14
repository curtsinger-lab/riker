#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <unistd.h>
#include <fcntl.h>

#include <capnp/message.h>
#include <capnp/serialize.h>
#include <iostream>

#include "db.capnp.h"
#include "fingerprint.h"
#include "middle.h"


/* ------------------------------ new_command Methods -----------------------------------------*/
new_command::new_command(trace_state* state, Blob&& cmd, new_command* parent, unsigned int depth) : state(state), cmd(std::move(cmd)), parent(parent), depth(depth) {}

void new_command::add_input(new_file* f) {
    // search through all files, do versioning
    f->interactions.push_front(this);
    f->users.insert(this);
    this->inputs.insert(f);

    // if we've read from the file previously, check for a race
    if (f->serialized.getReader().getType() == db::FileType::PIPE) {
        return;
    }

    for (auto rd : this->rd_interactions) {
        if (f->location == rd->location && f->writer != this) {
            if (f->version == rd->version) {
                return;
            } else /* we've found a race */ {
                std::set<new_command*> conflicts = f->collapse(rd->version);
                this->collapse(&conflicts);
            }
        }
    }
    // mark that we've now interacted with this version of the file
    this->rd_interactions.insert(f);
}

void new_command::add_output(new_file* f, size_t file_location) {
    // if we've written to the file before, check for a race
    if (f->serialized.getReader().getType() != db::FileType::PIPE) {
        for (auto wr : this->wr_interactions) {
            if (f->location == wr->location) {
                this->outputs.insert(f);
                if (f->version == wr->version) {
                    return;
                } else {
                    std::set<new_command*> conflicts = f->collapse(wr->version);
                    this->collapse(&conflicts);
                    // wr->has_race = true;
                    // this->has_race = true;
                    return;
                }
            }
        }
    }

    f->interactions.push_front(this);
    // if we haven't written to this file before, create a new version
    new_file* fnew;
    if ((f->creator != nullptr && f->writer == nullptr) || f->writer == this) {
        // Unless we just created it, in which case it is pristine
        fnew = f;
    } else {
        fnew = f->make_version();
        fnew->creator = nullptr;
    }
    fnew->writer = this;
    this->outputs.insert(fnew);
    this->wr_interactions.insert(fnew);
}

uint64_t new_command::descendants(void) {
    uint64_t ret = 0;
    for (auto c : this->children) {
        ret += 1 + c->descendants();
    }
    return ret;
}

static uint64_t serialize_commands(new_command* command, ::capnp::List<db::Command>::Builder& command_list, uint64_t start_index, std::map<new_command*, uint64_t>& command_ids, std::map<new_file*, uint64_t>& file_ids) {
    command_ids[command] = start_index;
    command_list[start_index].setExecutable(command->cmd);
    command_list[start_index].setOutOfDate(false);
    command_list[start_index].setCollapseWithParent(command->collapse_with_parent);
    auto argv = command_list[start_index].initArgv(command->args.size());
    uint64_t argv_index = 0;
    for (auto arg = command->args.begin(); arg != command->args.end(); ++arg) {
        argv.set(argv_index, (*arg).asPtr());
        argv_index++;
    }
    auto initial_fds = command_list[start_index].initInitialFDs(command->initial_fds.size());
    size_t fd_index = 0;
    for (auto fd = command->initial_fds.begin(); fd != command->initial_fds.end(); ++fd) {
        auto file_id = file_ids.find(fd->second.file);
        if (file_id == file_ids.end()) {
            continue;
        }

        initial_fds[fd_index].setFd(fd->first);
        initial_fds[fd_index].setFileID(file_id->second);
        initial_fds[fd_index].setCanRead(fd->second.access_mode != O_WRONLY);
        initial_fds[fd_index].setCanWrite(fd->second.access_mode != O_RDONLY);
        fd_index++;
    }

    uint64_t index = start_index + 1;
    for (auto c : command->children) {
        index = serialize_commands(c, command_list, index, command_ids, file_ids);
    }

    command_list[start_index].setDescendants(index - start_index - 1);
    return index;
}

// collapse the current command to the designated depth
new_command* new_command::collapse_helper(unsigned int depth) {
    new_command* cur_command = this;
    while (cur_command->depth > depth) {
        cur_command->collapse_with_parent = true;
        cur_command = cur_command->parent;
    }
    return cur_command;
}

void new_command::collapse(std::set<new_command*>* commands) {
    //std::cerr << "Collapsing set of size " << commands->size() << std::endl;
    unsigned int ansc_depth = this->depth;
    // find the minimum common depth
    for (auto c : *commands) {
        if (c->depth < ansc_depth) {
            ansc_depth = c->depth;
        }
    }
    bool fully_collapsed = false;
    while (!fully_collapsed) {
        //std::cerr << "  Target depth " << ansc_depth << std::endl;
        // collapse all commands to this depth
        new_command* prev_command = nullptr;
        new_command* cur_command;
        fully_collapsed = true;
        for (auto c : *commands) {
            cur_command = c->collapse_helper(ansc_depth);
            //std::cerr << "    " << std::string(c->cmd.asPtr().asChars().begin(), c->cmd.asPtr().size()) << " collapsed to " << std::string(cur_command->cmd.asPtr().asChars().begin(), cur_command->cmd.asPtr().size()) << std::endl;
            if (cur_command != prev_command && prev_command != nullptr) {
                fully_collapsed = false;
            }
            prev_command = cur_command;
        }
        ansc_depth--;
    }
}

/* -------------------------- FileDescriptor Methods --------------------------------------*/
FileDescriptor::FileDescriptor() {}
FileDescriptor::FileDescriptor(size_t location_index, int access_mode, bool cloexec) : location_index(location_index), access_mode(access_mode), cloexec(cloexec) {}


/* ------------------------------- new_file Methods -------------------------------------------*/
new_file::new_file(size_t location, bool is_pipe, BlobPtr path, new_command* creator, trace_state* state, new_file* prev_version) : location(location), serialized(state->temp_message.getOrphanage().newOrphan<db::File>()), creator(creator), writer(nullptr), state(state), prev_version(prev_version), version(0), known_removed(false) {
    // TODO: consider using orphans to avoid copying
    if (is_pipe) {
        this->serialized.get().setType(db::FileType::PIPE);
    } else {
        this->serialized.get().setType(db::FileType::REGULAR);
        this->serialized.get().setPath(path);
    }
}

// return a set of the commands which raced on this file, back to the parameter version
std::set<new_command*> new_file::collapse(unsigned int version) {
    //this->has_race = true;
    new_file* cur_file = this;
    std::set<new_command*> conflicts;
    while (cur_file->version != version) {
        // add writer and all readers to conflict set
        if (cur_file->writer != nullptr) {
            conflicts.insert(cur_file->writer);
        }
        for (auto rd : cur_file->interactions) {
            conflicts.insert(rd);
        }
        // add all mmaps to conflicts
        for (auto m : cur_file->mmaps) {
            conflicts.insert(m->command);
        }
        // step back a version
        cur_file = cur_file->prev_version;
    }
    return conflicts;
}

// file can only be depended on if it wasn't truncated/created, and if the current command isn't
// the only writer
bool new_file::can_depend(new_command* cmd) {
    return this->writer != cmd && (this->writer != nullptr || this->creator != cmd);
}

new_file* new_file::make_version(void) {
    // We are at the end of the current version, so snapshot with a fingerprint
    set_fingerprint(this->serialized.get(), true);
    this->serialized.get().setLatestVersion(false);

    new_file* f = new new_file(
        this->location,
        this->serialized.getReader().getType() == db::FileType::PIPE,
        this->serialized.getReader().getPath(),
        this->creator,
        this->state,
        this
    );
    f->version = this->version + 1;

    this->state->files.insert(f);
    this->state->latest_versions[this->location] = f;
    return f;
}

/* ----------------------------- Process Methods ------------------------------------------*/
Process::Process(pid_t thread_id, Blob&& cwd, new_command* command) : thread_id(thread_id), cwd(std::move(cwd)), command(command) {}

/* -------------------------- Trace_State Methods ----------------------------------------*/
// return latest version of the file, or create file and add to record if not found
size_t trace_state::find_file(BlobPtr path) {
    for (size_t index = 0; index < this->latest_versions.size(); index++) {
        if (this->latest_versions[index]->serialized.getReader().getType() != db::FileType::PIPE && this->latest_versions[index]->serialized.getReader().getPath() == path) {
            return index;
        }
    }
    size_t location = this->latest_versions.size();
    new_file* new_node = new new_file(location, false, kj::heapArray(path), nullptr, this, nullptr);
    this->files.insert(new_node);
    this->latest_versions.push_back(new_node);
    return location;
}

struct scc_info {
    size_t discovery_time;
    size_t lowlink;
    bool on_stack;
};

static void tarjan_scc(size_t* time, std::vector<new_command*>& scc_stack, std::map<new_command*, scc_info>& info_mapping, new_command* command) {
    size_t self_discovery_time = *time;
    size_t self_lowlink = *time;
    size_t stack_index = scc_stack.size();
    info_mapping[command].discovery_time = self_discovery_time;
    info_mapping[command].on_stack = true;
    *time += 1;
    scc_stack.push_back(command);

    auto handle_out_edge = [&](new_command* other) {
        auto other_entry = info_mapping.find(other);
        if (other_entry == info_mapping.end()) {
            tarjan_scc(time, scc_stack, info_mapping, other);
            size_t other_lowlink = info_mapping[other].lowlink;
            if (other_lowlink < self_lowlink) {
                self_lowlink = other_lowlink;
            }
        } else if (other_entry->second.on_stack) {
            if (other_entry->second.discovery_time < self_lowlink) {
                self_lowlink = other_entry->second.discovery_time;
            }
        }
    };
    for (auto child : command->children) {
        handle_out_edge(child);
    }
    for (auto out : command->outputs) {
        for (auto reader : out->users) {
            handle_out_edge(reader);
        }
    }

    info_mapping[command].lowlink = self_lowlink;
    if (self_lowlink == self_discovery_time) {
        std::set<new_command*> scc;
        for (size_t i = stack_index; i < scc_stack.size(); i++) {
            info_mapping[scc_stack[i]].on_stack = false;
            scc.insert(scc_stack[i]);
        }
        command->collapse(&scc);
        scc_stack.resize(stack_index);
    }
}

void trace_state::collapse_sccs(void) {
    // We use Tarjan's SCC algorithm
    size_t time = 0;
    std::vector<new_command*> scc_stack;
    std::map<new_command*, scc_info> info_mapping;

    for (auto c : this->commands) {
        tarjan_scc(&time, scc_stack, info_mapping, c);
    }
}

void trace_state::serialize_graph(void) {
    // Before serializing, search for strongly connected components and collapse them
    this->collapse_sccs();

    ::capnp::MallocMessageBuilder message;

    db::Graph::Builder graph = message.initRoot<db::Graph>();

    // Prepare files for serialization: we've already fingerprinted the old versions,
    // but we need to fingerprint the latest versions
    for (size_t location = 0; location < this->latest_versions.size(); location++) {
        set_fingerprint(this->latest_versions[location]->serialized.get(), !this->latest_versions[location]->users.empty());
        this->latest_versions[location]->serialized.get().setLatestVersion(true);
    }

    // Serialize files
    std::map<new_file*, uint64_t> file_ids;
    uint64_t file_count = 0;
    uint64_t input_count = 0;
    uint64_t output_count = 0;
    uint64_t create_count = 0;
    uint64_t modify_count = 0;
    for (auto file : this->files) {
        // We only care about files that are either written or read so filter those out.
        if (!file->users.empty() || file->writer != nullptr || file->creator != nullptr || (file->prev_version != nullptr && !file->known_removed)) {
            file_ids[file] = file_count;
            input_count += file->users.size();
            output_count += (file->writer != nullptr);
            create_count += (file->creator != nullptr);
            modify_count += (file->creator == nullptr && file->prev_version != nullptr && (file->prev_version->creator != nullptr || (file->prev_version->prev_version != nullptr && !file->prev_version->known_removed)));
            file_count += 1;
        }
    }
    auto files = graph.initFiles(file_count);
    for (auto file_entry : file_ids) {
        auto file = file_entry.first;
        auto file_id = file_entry.second;
        // TODO: Use orphans and avoid copying?
        files.setWithCaveats(file_id, file->serialized.getReader());
    }

    // Serialize commands
    std::map<new_command*, uint64_t> command_ids;
    uint64_t command_count = 0;
    for (auto c : this->commands) {
        command_count += 1 + c->descendants();
    }
    auto commands = graph.initCommands(command_count);
    uint64_t command_index = 0;
    for (auto c : this->commands) {
        command_index = serialize_commands(c, commands, command_index, command_ids, file_ids);
    }

    // Serialize dependencies
    auto inputs = graph.initInputs(input_count);
    auto outputs = graph.initOutputs(output_count);
    auto creations = graph.initCreations(create_count);
    auto modifications = graph.initModifications(modify_count);
    uint64_t input_index = 0;
    uint64_t output_index = 0;
    uint64_t create_index = 0;
    uint64_t modify_index = 0;
    for (auto file_entry : file_ids) {
        auto file = file_entry.first;
        auto file_id = file_entry.second;
        for (auto user : file->users) {
            uint64_t user_id = command_ids[user];
            inputs[input_index].setInputID(file_id);
            inputs[input_index].setOutputID(user_id);
            input_index++;
        }
        if (file->writer != nullptr) {
            uint64_t writer_id = command_ids[file->writer];
            outputs[output_index].setInputID(writer_id);
            outputs[output_index].setOutputID(file_id);
            output_index++;
        }
        if (file->creator != nullptr) {
            uint64_t creator_id = command_ids[file->creator];
            creations[create_index].setInputID(creator_id);
            creations[create_index].setOutputID(file_id);
            create_index++;
        }
        if (file->creator == nullptr && file->prev_version != nullptr && (file->prev_version->creator != nullptr || (file->prev_version->prev_version != nullptr && !file->prev_version->known_removed))) {
            uint64_t prev_id = file_ids[file->prev_version];
            modifications[modify_index].setInputID(prev_id);
            modifications[modify_index].setOutputID(file_id);
            modify_index++;
        }
    }

    // Serialize removal edges
    uint64_t removal_count = 0;
    for (auto c : command_ids) {
        for (auto f : c.first->deleted_files) {
            auto file_id = file_ids.find(f);
            if (file_id != file_ids.end()) {
                removal_count++;
            }
        }
    }
    auto removals = graph.initRemovals(removal_count);
    uint64_t removal_index = 0;
    for (auto c : command_ids) {
        for (auto f : c.first->deleted_files) {
            auto file_id = file_ids.find(f);
            if (file_id != file_ids.end()) {
                removals[removal_index].setInputID(c.second);
                removals[removal_index].setOutputID(file_ids[f]);
                removal_index++;
            }
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
    size_t file_location;
    if (file.fd == AT_FDCWD) {
        file_location = this->find_file(file.path.asPtr());
    } else {
        //fprintf(stdout, "[%d] Dep: %d -> ", proc->thread_id, file.fd);
        if (proc->fds.find(file.fd) == proc->fds.end()) {
            // TODO: Use a proper placeholder and stop fiddling with string manipulation
            std::string path_str;
            switch (file.fd) { // This is a temporary hack until we handle pipes well
            case 0:
                path_str = "<<stdin>>";
                break;
            case 1:
                path_str = "<<stdout>>";
                break;
            case 2:
                path_str = "<<stderr>>";
                break;
            default:
                path_str = "file not found, fd: " + std::to_string(file.fd);
                break;
            }
            Blob path_buf = kj::heapArray((const kj::byte*) path_str.data(), path_str.size());
            file_location = this->find_file(path_buf.asPtr());
        } else {
            file_location = proc->fds.find(file.fd)->second.location_index;
        }
    }
    new_file* f = this->latest_versions[file_location];

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
            proc->command->add_output(f, file_location);
            break;
        case DEP_CREATE:
            //fprintf(stdout, "create");
            // Creation means creation only if the file does not already exist.
            if (f->creator == nullptr && f->writer == nullptr) {
                bool file_exists;
                if (f->known_removed || f->serialized.getReader().getType() == db::FileType::PIPE) {
                    file_exists = false;
                } else {
                    struct stat stat_info;
                    auto path_string = std::string(f->serialized.getReader().getPath().asChars().begin(), f->serialized.getReader().getPath().size());
                    file_exists = (lstat(path_string.c_str(), &stat_info) == 0);
                }

                if (!file_exists) {
                    f->creator = proc->command;
                    f->known_removed = false;
                }
            }
            break;
        case DEP_REMOVE:
            //fprintf(stdout, "remove");
            proc->command->deleted_files.insert(f);
            f = f->make_version();
            f->creator = nullptr;
            f->writer = nullptr;
            f->known_removed = true;
            break;
    }
    if (!file.follow_links) {
        //fprintf(stdout, " (nofollow)");
    }
    if (file.path == nullptr) {
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
void trace_state::add_open(Process* proc, int fd, struct file_reference& file, int access_mode, bool is_rewrite, bool cloexec, mode_t mode) {
    //fprintf(stdout, "[%d] Open %d -> ", proc->thread_id, fd);
    // TODO take into account root and cwd
    size_t file_location = this->find_file(file.path.asPtr());
    new_file* f = this->latest_versions[file_location];
    if (is_rewrite && (f->creator != proc->command || f->writer != nullptr)) {
        //fprintf(stdout, "REWRITE ");
        f = f->make_version();
        f->creator = proc->command;
        f->writer = nullptr;
        f->serialized.get().setMode(mode);
    }
    proc->fds[fd] = FileDescriptor(file_location, access_mode, cloexec);
    if (file.fd == AT_FDCWD) {
        //fprintf(stdout, " %.*s\n", (int)file.path.size(), file.path.asChars().begin());
    } else {
        //fprintf(stdout, " {%d/}%.*s\n", file.fd, (int)file.path.size(), file.path.asChars().begin());
    }
}

void trace_state::add_pipe(Process* proc, int fds[2], bool cloexec) {
    //fprintf(stdout, "[%d] Pipe %d, %d\n", proc->thread_id, fds[0], fds[1]);
    size_t location = this->latest_versions.size();
    new_file* p = new new_file(location, true, Blob(), proc->command, this, NULL);
    this->files.insert(p);
    this->latest_versions.push_back(p);
    proc->fds[fds[0]] = FileDescriptor(location, O_RDONLY, cloexec);
    proc->fds[fds[1]] = FileDescriptor(location, O_WRONLY, cloexec);
}

void trace_state::add_dup(Process* proc, int duped_fd, int new_fd, bool cloexec) {
    //fprintf(stdout, "[%d] Dup %d <- %d\n", thread_id, duped_fd, new_fd);
    auto duped_file = proc->fds.find(duped_fd);
    if (duped_file == proc->fds.end()) {
        proc->fds.erase(new_fd);
    } else {
        proc->fds[new_fd] = FileDescriptor(duped_file->second.location_index, duped_file->second.access_mode, cloexec);
    }
}

void trace_state::add_set_cloexec(Process* proc, int fd, bool cloexec) {
    auto file = proc->fds.find(fd);
    if (file != proc->fds.end()) {
        file->second.cloexec = cloexec;
    }
}

// TODO deal with race conditions
void trace_state::add_mmap(Process* proc, int fd) {
    //fprintf(stdout, "[%d] Mmap %d\n", proc->thread_id, fd);
    FileDescriptor& desc = proc->fds.find(fd)->second;
    new_file* f = this->latest_versions[desc.location_index];
    f->mmaps.insert(proc);
    proc->mmaps.insert(f);
    //std::cout << "MMAP ";
    if (desc.access_mode != O_WRONLY) {
        //std::cout << "read ";
        proc->command->add_input(f);
    }
    if (desc.access_mode != O_RDONLY) {
        //std::cout << "write";
        proc->command->add_output(f, desc.location_index);
    }
    //std::cout << "\n";
}

void trace_state::add_close(Process* proc, int fd) {
    //fprintf(stdout, "[%d] Close %d\n", proc->thread_id, fd);
    proc->fds.erase(fd);
}

// create process node
void trace_state::add_fork(Process* parent_proc, pid_t child_process_id) {
    //fprintf(stdout, "[%d] Fork %d\n", parent_proc->thread_id, child_process_id);
    Process* child_proc = new Process(child_process_id, kj::heapArray(parent_proc->cwd.asPtr()), parent_proc->command);
    child_proc->fds = parent_proc->fds;
    this->processes[child_process_id] = child_proc;
}

void trace_state::add_exec(Process* proc, Blob&& exe_path) {
    //fprintf(stdout, "[%d] Inside exec: %.*s\n", proc->thread_id, (int) exe_path.size(), exe_path.asChars().begin());
    new_command* cmd = new new_command(this, std::move(exe_path), proc->command, proc->command->depth + 1);
    //fprintf(stdout, "Pushed %.*s to commands list!\n", (int) cmd->cmd.size(), cmd->cmd.asChars().begin());
    proc->command->children.push_front(cmd);
    proc->command = cmd;

    // Close all cloexec file descriptors
    for (auto fd_entry = proc->fds.begin(); fd_entry != proc->fds.end();) {
        if (fd_entry->second.cloexec) {
            fd_entry = proc->fds.erase(fd_entry);
        } else {
            ++fd_entry;
        }
    }

    // Close all mmaps, since the address space is replaced
    for (auto f = proc->mmaps.begin(); f != proc->mmaps.end(); ++f) {
        (*f)->mmaps.erase(proc);
    }

    // Mark the initial open file descriptors
    for (auto it = proc->fds.begin(); it != proc->fds.end(); ++it) {
        it->second.file = this->latest_versions[it->second.location_index];
    }
    cmd->initial_fds = proc->fds;

    // Assume that we can at any time write to stdout or stderr
    // TODO: Instead of checking whether we know about stdout and stderr,
    // tread the initial stdout and stderr properly as pipes
    if (proc->fds.find(fileno(stdout)) != proc->fds.end()) {
        this->add_mmap(proc, fileno(stdout));
    }
    if (proc->fds.find(fileno(stderr)) != proc->fds.end()) {
        this->add_mmap(proc, fileno(stderr));
    }
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
