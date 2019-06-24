#include "graph.h"

#include <set>
#include <list>
#include <map>
#include <string>

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
    char* path;
    // Whether to follow the link if the reference points at a symlink
    bool follow_links;
};

struct File;
struct trace_state;

struct Command {
    trace_state* state;
    std::string cmd;
    std::list<Command*> children;
    std::set<File*> inputs;
    std::set<File*> outputs;
    std::set<File*> wr_interactions;
    std::set<File*> rd_interactions;
    std::list<std::string> args;
    bool has_race;

    Command(trace_state* state, std::string args);
    Command* make_child(std::string args);
    void add_input(File* f);
    void add_output(File* f);
    std::string to_graph(void);
    void print(void);
};

struct File {
    std::string filename;
    std::set<Command*> users;
    std::set<Command*> producers;
    std::list<Command*> interactions;
    std::list<Command*> conflicts;
    Command* writer;
    int id;
    int version;
    bool dependable;

    File(std::string path, Command* writer);
    //bool is_local(void);
    bool is_intermediate(void);
    bool is_local(void);
    void collapse(void);
    bool can_depend(Command* cmd);
    File* make_version(void);
    void print_file(void);
    // TODO closed by list?
};

//TODO theres a decent chance this will turn out to be unecessary
struct Process {
    std::string cwd;
    std::string root;
    std::map<int, std::string> fds;
    Command* command;
    // file descriptors? probably subsumed by middle end

    Process(std::string cwd, Command* command);
    void print(void);
    std::string normpath(std::string path);
};

struct trace_state {
    std::set<File*> files;
    std::list<Command*> commands;
    std::map<pid_t, Process*> processes;
    std::string starting_dir;
    Graph g;    

    // Note that all strings (char*) passed to the following functions are transferring ownership,
    // so the callee is responsible for freeing them. This is not true of the paths inside the
    // file references: those will be freed shortly after the trace_add_* function is called.
    File* find_file(std::string path);
    void add_dependency(pid_t thread_id, struct file_reference file, enum dependency_type type);
    void add_change_cwd(pid_t thread_id, struct file_reference file);
    void add_change_root(pid_t thread_id, struct file_reference file);
    void add_open(pid_t thread_id, int fd, struct file_reference file, int access_mode, bool is_rewrite);
    void add_pipe(pid_t thread_id, int fds[2]);
    void add_dup(pid_t thread_id, int duped_fd, int new_fd);
    void add_mmap(pid_t thread_id, int fd);
    void add_close(pid_t thread_id, int fd);
    void add_fork(pid_t parent_thread_id, pid_t child_process_id);
    void add_exec(pid_t process_id, char* exe_path);
    void add_exec_argument(pid_t process_id, char* argument, int index);
    void add_exit(pid_t thread_id);
    void to_graph(void);
};
