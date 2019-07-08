#include <vector>
#include <set>
#include <list>
#include <map>
#include <string>

#include <kj/array.h>

typedef kj::Array<kj::byte> Blob;
typedef kj::ArrayPtr<kj::byte> BlobPtr;

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
    bool has_race;

    Command(trace_state* state, Blob&& args);
    void add_input(File* f);
    void add_output(File* f);
    size_t descendants(void);
    void rerun_children(std::set<Command*>* to_rerun);
    void print_changes(std::vector<Blob>& changes, std::set<Command*>* to_rerun);
};

struct File {
    Blob filename;
    std::set<Command*> users;
    std::set<Command*> producers; 
    std::set<Process*> mmaps;
    std::list<Command*> interactions;
    std::list<Command*> conflicts;
    Command* writer;
    trace_state* state;
    int id;
    int version;
    bool is_latest_version;
    bool dependable;

    File(Blob&& path, Command* writer, trace_state* state);
    void collapse(void);
    bool can_depend(Command* cmd);
    File* make_version(void);
};

struct FileDescriptor {
    Blob path;
    int access_mode;
    bool cloexec;

    FileDescriptor(Blob&& path, int access_mode, bool cloexec);
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

struct trace_state {
    std::set<File*> files;
    std::list<Command*> commands;
    std::map<pid_t, Process*> processes;
    Blob starting_dir;

    File* find_file(BlobPtr path);
    void serialize_graph(void);
    void add_dependency(Process* proc, struct file_reference& file, enum dependency_type type);
    void add_change_cwd(Process* proc, struct file_reference& file);
    void add_change_root(Process* proc, struct file_reference& file);
    void add_open(Process* proc, int fd, struct file_reference& file, int access_mode, bool is_rewrite, bool cloexec);
    void add_pipe(Process* proc, int fds[2]);
    void add_dup(Process* proc, int duped_fd, int new_fd, bool cloexec);
    void add_mmap(Process* proc, int fd);
    void add_close(Process* proc, int fd);
    void add_fork(Process* parent_proc, pid_t child_process_id);
    void add_exec(Process* proc, Blob&& exe_path);
    void add_exec_argument(Process* proc, Blob&& argument, int index);
    void add_exit(Process* proc);
    void print_changes(std::vector<Blob>& changes);
};
