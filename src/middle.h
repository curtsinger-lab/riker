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

struct new_file;
struct FileDescriptor;
struct Process;
struct trace_state;

struct new_command {
    trace_state* state;
    Blob cmd;
    std::list<new_command*> children;
    std::set<new_file*> inputs;
    std::set<new_file*> outputs;
    std::set<new_file*> wr_interactions;
    std::set<new_file*> rd_interactions;
    std::set<new_file*> deleted_files;
    std::list<Blob> args;
    new_command* parent;
    unsigned int depth;
    bool collapse_with_parent;
    std::map<int, FileDescriptor> initial_fds;

    new_command(trace_state* state, Blob&& args, new_command* parent, unsigned int depth);
    void add_input(new_file* f);
    void add_output(new_file* f, size_t file_location);
    size_t descendants(void);
    new_command* collapse_helper(unsigned int depth);
    void collapse(std::set<new_command*>* commands);
};


struct new_file {
    size_t location;
    capnp::Orphan<db::File> serialized;
    std::set<new_command*> users;
    std::set<Process*> mmaps;
    std::list<new_command*> interactions;
    std::list<new_command*> conflicts;
    new_command* creator;
    new_command* writer;
    trace_state* state;
    new_file* prev_version;
    unsigned int version;
    bool known_removed;

    new_file(size_t location, bool is_pipe, BlobPtr path, new_command* creator, trace_state* state, new_file* prev_version);
    std::set<new_command*> collapse(unsigned int depth);
    bool can_depend(new_command* cmd);
    new_file* make_version(void);
};

struct FileDescriptor {
    size_t location_index; // Used in Process::fds
    new_file* file; // Used in new_command::initial_fds
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
    std::set<new_file*> mmaps;
    new_command* command;

    Process(pid_t thread_id, Blob&& cwd, new_command* command);
};

struct file_comparator {
    bool operator() (new_file* const& lhs, new_file* const& rhs) const {
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
    std::set<new_file*, file_comparator> files;
    std::vector<new_file*> latest_versions;
    std::list<new_command*> commands;
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
