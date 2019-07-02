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
struct trace_state;

struct Command {
    trace_state* state;
    Blob cmd;
    std::list<Command*> children;
    std::set<File*> inputs;
    std::set<File*> outputs;
    std::set<File*> wr_interactions;
    std::set<File*> rd_interactions;
    std::list<Blob> args;
    bool has_race;

    Command(trace_state* state, Blob&& args);
    void add_input(File* f);
    void add_output(File* f);
    size_t descendants(void);
};

struct File {
    Blob filename;
    std::set<Command*> users;
    std::set<Command*> producers;
    std::list<Command*> interactions;
    std::list<Command*> conflicts;
    Command* writer;
    trace_state* state;
    int id;
    int version;
    bool dependable;

    File(Blob&& path, Command* writer, trace_state* state);
    void collapse(void);
    bool can_depend(Command* cmd);
    File* make_version(void);
};

struct Process {
    Blob cwd;
    Blob root;
    std::map<int, Blob> fds;
    Command* command;

    Process(Blob&& cwd, Command* command);
};

struct trace_state {
    std::set<File*> files;
    std::list<Command*> commands;
    std::map<pid_t, Process*> processes;
    Blob starting_dir;

    File* find_file(BlobPtr path);
    void serialize_graph(void);
    void add_dependency(pid_t thread_id, struct file_reference& file, enum dependency_type type);
    void add_change_cwd(pid_t thread_id, struct file_reference& file);
    void add_change_root(pid_t thread_id, struct file_reference& file);
    void add_open(pid_t thread_id, int fd, struct file_reference& file, int access_mode, bool is_rewrite);
    void add_pipe(pid_t thread_id, int fds[2]);
    void add_dup(pid_t thread_id, int duped_fd, int new_fd);
    void add_mmap(pid_t thread_id, int fd);
    void add_close(pid_t thread_id, int fd);
    void add_fork(pid_t parent_thread_id, pid_t child_process_id);
    void add_exec(pid_t process_id, Blob&& exe_path);
    void add_exec_argument(pid_t process_id, Blob&& argument, int index);
    void add_exit(pid_t thread_id);
};
