#include "graph.h"

#include <vector>
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
struct Process;
struct trace_state;

struct Command {
    trace_state* state;
    std::string cmd;
    std::list<Command*> children;
    std::set<File*> inputs;
    std::set<File*> outputs;
    std::set<File*> wr_interactions;
    std::set<File*> rd_interactions;
    std::set<File*> deleted_files;
    std::list<std::string> args;
    bool has_race;

    Command(trace_state* state, std::string args);
    void add_input(File* f);
    void add_output(File* f);
    std::string to_graph(void);
    void print(void);
    void rerun_children(std::set<Command*>* to_rerun);
    void print_changes(std::vector<std::string> changes, std::set<Command*>* to_rerun);
};

struct File {
    std::string filename;
    std::set<Command*> users;
    std::set<Command*> producers; 
    std::set<Process*> mmaps;
    std::list<Command*> interactions;
    std::list<Command*> conflicts;
    Command* writer; 
    trace_state* state;
    int id;
    int version;
    bool dependable;

    File(std::string path, Command* writer, trace_state* state);
    bool is_local(void);
    bool is_intermediate(void);
    void collapse(void);
    bool can_depend(Command* cmd);
    File* make_version(void);
    void print_file(void);
};

struct Process {
    pid_t thread_id;
    std::string cwd;
    std::string root;
    std::map<int, std::string> fds;
    std::set<File*> mmaps;
    Command* command;

    Process(pid_t thread_id, std::string cwd, Command* command);
    void print(void);
};

struct trace_state {
    std::set<File*> files;
    std::list<Command*> commands;
    std::map<pid_t, Process*> processes;
    std::string starting_dir;
    Graph g;    
    bool show_sys;

    // Note that all strings (char*) passed to the following functions are transferring ownership,
    // so the callee is responsible for freeing them. This is not true of the paths inside the
    // file references: those will be freed shortly after the trace_add_* function is called.
    File* find_file(std::string path);
    void to_graph(void);
    void add_dependency(Process* proc, struct file_reference file, enum dependency_type type);
    void add_change_cwd(Process* proc, struct file_reference file);
    void add_change_root(Process* proc, struct file_reference file);
    void add_open(Process* proc, int fd, struct file_reference file, int access_mode, bool is_rewrite);
    void add_pipe(Process* proc, int fds[2]);
    void add_dup(Process* proc, int duped_fd, int new_fd);
    void add_mmap(Process* proc, int fd, int flag);
    void add_close(Process* proc, int fd);
    void add_fork(Process* proc, pid_t child_process_id);
    void add_exec(Process* proc, char* exe_path);
    void add_exec_argument(Process* proc, char* argument, int index);
    void add_exit(Process* proc);
    void print_changes(std::vector<std::string> changes);
};
