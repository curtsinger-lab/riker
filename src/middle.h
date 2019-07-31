#pragma once

#include <vector>
#include <set>
#include <list>
#include <map>
#include <string>

#include <kj/array.h>
#include <capnp/orphan.h>
#include <capnp/message.h>

#include "db.capnp.h"

typedef kj::Array<kj::byte> Blob;
typedef kj::ArrayPtr<const kj::byte> BlobPtr;

enum dependency_type {
   DEP_READ,
   DEP_MODIFY,
   DEP_CREATE,
   DEP_REMOVE
};

struct file_reference {
    // fd may be AT_FDCWD and path may be NULL, but not both. If both are present, then
    // path is relative with respect to the directory in fd.
    int fd;
    Blob path;
    // Whether to follow the link if the reference points at a symlink
    bool follow_links;
};

struct File;
struct FileDescriptor;
struct Process;
struct trace_state;

struct Command {
    trace_state* state;
    Blob cmd;
    std::list<Command*> children;
    std::set<File*> inputs;
    std::set<File*> outputs;
    std::set<File*> wr_interactions;
    std::set<File*> rd_interactions;
    std::set<File*> deleted_files;
    std::list<Blob> args;
    Command* parent;
    unsigned int depth;
    bool collapse_with_parent;
    std::map<int, FileDescriptor> initial_fds;

    Command(trace_state* state, Blob&& args, Command* parent, unsigned int depth);
    void add_input(File* f);
    void add_output(File* f, size_t file_location);
    size_t descendants(void);
    Command* collapse_helper(unsigned int depth);
    void collapse(std::set<Command*>* commands);
};


struct File {
    capnp::Orphan<db::File> serialized;
    std::set<Command*> users;
    std::set<Process*> mmaps;
    std::list<Command*> interactions;
    std::list<Command*> conflicts;
    Command* creator;
    Command* writer;
    trace_state* state;
    File* prev_version;
    unsigned int version;
    bool known_removed;

    File(bool is_pipe, BlobPtr path, Command* creator, trace_state* state, File* prev_version);
    std::set<Command*> collapse(unsigned int depth);
    bool can_depend(Command* cmd);
    File* make_version(void);
};

struct FileDescriptor {
    size_t location_index; // Used in Process::fds
    File* file; // Used in Command::initial_fds
    int access_mode;
    bool cloexec;

    FileDescriptor();
    FileDescriptor(size_t location_index, int access_mode, bool cloexec);
};

struct Process {
    pid_t thread_id;
    Blob cwd;
    Blob root;
    std::map<int, FileDescriptor> fds;
    std::set<File*> mmaps;
    Command* command;

    Process(pid_t thread_id, Blob&& cwd, Command* command);
};

struct file_comparator {
    bool operator() (File* const& lhs, File* const& rhs) const {
      //  return false;

        auto lhs_reader = lhs->serialized.getReader();
        auto rhs_reader = rhs->serialized.getReader();
        if (lhs_reader.getType() == db::FileType::PIPE) {
            return true; 
        } else if (rhs_reader.getType() == db::FileType::PIPE) {
            return false;
        }

        auto path1 = lhs_reader.getPath();
        auto path2 = rhs_reader.getPath();
        
        // compare to find alphabetic order
        for (size_t i = 0; i < std::min(path1.size(), path2.size()); i++) {
            if (path1[i] != path2[i]) {
                return path1[i] < path2[i];
            }
        }

       
        // if the first min(path1.size(), path2(size()) characters are the same, order
        // the shorter string first
//return true;
        return path1.size() <= path2.size(); 
}
};

struct trace_state {
    std::set<File*, file_comparator> files;
    std::vector<File*> latest_versions;
    std::list<Command*> commands;
    std::map<pid_t, Process*> processes;
    Blob starting_dir;
    ::capnp::MallocMessageBuilder temp_message;

    size_t find_file(BlobPtr path);
    void serialize_graph(void);
    void collapse_sccs(void);
    void add_dependency(Process* proc, struct file_reference& file, enum dependency_type type);
    void add_change_cwd(Process* proc, struct file_reference& file);
    void add_change_root(Process* proc, struct file_reference& file);
    void add_open(Process* proc, int fd, struct file_reference& file, int access_mode, bool is_rewrite, bool cloexec, mode_t mode);
    void add_pipe(Process* proc, int fds[2], bool cloexec);
    void add_dup(Process* proc, int duped_fd, int new_fd, bool cloexec);
    void add_set_cloexec(Process* proc, int fd, bool cloexec);
    void add_mmap(Process* proc, int fd);
    void add_close(Process* proc, int fd);
    void add_fork(Process* parent_proc, pid_t child_process_id);
    void add_exec(Process* proc, Blob&& exe_path);
    void add_exec_argument(Process* proc, Blob&& argument, int index);
    void add_exit(Process* proc);
    void print_changes(std::vector<Blob>& changes);
};
