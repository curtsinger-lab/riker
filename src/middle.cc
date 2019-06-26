#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <sys/types.h>
#include <fcntl.h>

#include <iostream>
#include <experimental/filesystem>
namespace fs = std::experimental::filesystem;

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
    for (auto rds = this->rd_interactions.begin(); rds != this->rd_interactions.end(); ++rds) {
        if ((f->filename.compare((*rds)->filename) == 0) && (f->writer != this)) {
            if (f->version == (*rds)->version) {
                return;
            } else /* we've found a race */ {
                (*rds)->collapse();
                this->has_race = true;
            }
        }
    }
    // mark that we've now interacted with this version of the file
    this->rd_interactions.insert(f);
}

void Command::add_output(File* f) {
    // if we've written to the file before, check for a race 
    for (auto wrs = this->wr_interactions.begin(); wrs != this->wr_interactions.end(); ++wrs) {
        if (f->filename.compare((*wrs)->filename) == 0) {
            f->producers.insert(this);
            this->outputs.insert(f);
            if (f->version == (*wrs)->version)
                return; 
            else {
                (*wrs)->collapse();
                // TODO collapse all intermediates
                //for (auto it = this->state->files()
                //(*wrs)->has_race = true;
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

std::string Command::to_graph(void) {
    // create unique id per iteration of command, consisting of command and it's arguments
    std::string id = this->cmd;
    for (auto it = this->args.begin(); it != this->args.end(); ++it) {
        id += (*it);
    }
    // add command node
    this->state->g.add_node(id, this->cmd, "");
    
    // recurse for all child commands
    for (auto c = this->children.begin(); c != this->children.end(); ++c) {
        std::string child_id = (*c)->to_graph();
        this->state->g.add_edge(id, child_id, "style=dashed");
    }
    
    // draw input edges
    for (auto i = this->inputs.begin(); i != this->inputs.end(); ++i) {
        if ((*i)->is_local()) {
            // only draw input edges for non intermediates (intermediates handled in output loop)
            if (!((*i)->is_intermediate())) {
                std::string node_id = (*i)->filename + std::to_string((*i)->version);
                this->state->g.add_node(node_id, (*i)->filename, "shape=rectangle");
                this->state->g.add_edge(node_id, id, "arrowhead=empty");
            } 
        }
    }
    
    // draw output edges
    for (auto o = this->outputs.begin(); o != this->outputs.end(); ++o) { 
        std::string node_id = (*o)->filename + std::to_string((*o)->version);
        // draw temp nodes for intermediates and edges to all their users
        if ((*o)->is_intermediate()) {
            this->state->g.add_node(node_id, "\\<temp\\>", "shape=rectangle");
            this->state->g.add_edge(id, node_id, "arrowhead=empty");
            for (auto u = (*o)->users.begin(); u != (*o)->users.end(); ++u) {
                std::string out_id = (*u)->cmd;
                for (auto it = (*u)->args.begin(); it != (*u)->args.end(); ++it) {
                    out_id += (*it);
                }
                this->state->g.add_edge(node_id, out_id, "arrowhead=empty");
            }
        } else {
            this->state->g.add_node(node_id, (*o)->filename, "shape=rectangle");
            this->state->g.add_edge(id, node_id, "arrowhead=empty");
        }
    }

    // draw removal edges
    for (auto c = this->deleted_files.begin(); c != this->deleted_files.end(); ++c) {        
        std::string node_id = (*c)->filename + std::to_string((*c)->version);
        this->state->g.add_edge(id, node_id, "color=red");
    }    

    return id;   
}

// helpful for debugging
void Command::print(void) {
    std::cout << cmd << "\n";
    std::cout << "Inputs:\n";
    for (auto it=this->inputs.begin(); it != this->inputs.end(); ++it) {
        std::cout << '\t' << (*it)->filename << '\n';
    }
    std::cout << "Outputs:\n";
    for (auto it=this->outputs.begin(); it != this->outputs.end(); ++it) {
        std::cout << '\t' << (*it)->filename << '\n';
    }

}

/* ------------------------------- File Methods -------------------------------------------*/
File::File(std::string path, Command* writer, trace_state* state) : filename(path), writer(writer), state(state) {
    this->dependable = true;
}

//TODO add logic here
bool File::is_local(void) {
    if (this->state->show_sys) 
        return true;
    if (this->filename.find("usr")!=std::string::npos || this->filename.find("lib")!=std::string::npos || this->filename.find("dev")!=std::string::npos) {
        return false;
    } else {
        return true;
    }
}

bool File::is_intermediate(void) {
    if ((this->users.size()!=0)&&(this->producers.size()!=0)&&(this->filename.find("tmp")!=std::string::npos)) {
        return true;
    } else {
        return false;
    }
}

//TODO fix
void File::collapse(void) {
    //this->has_race = true;
    for (auto it = this->interactions.begin(); it != this->interactions.end(); ++it) {
        //(*it)->has_race = true;
        this->conflicts.push_front(*it);
    }
}

// file can only be depended on if it wasn't truncated/created, and if the current command isn't
// the only writer
bool File::can_depend(Command* cmd) {
    if(this->writer == cmd) {
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

// debugging aid
void File::print_file(void) {
    fprintf(stdout, "File: %s, Version: %d", this->filename.c_str(), this->version);
}

/* ----------------------------- Process Methods ------------------------------------------*/
Process::Process(pid_t thread_id, std::string cwd, Command* command) : thread_id(thread_id), cwd(cwd), command(command) {}

// debugging aid 
void Process::print(void) {
    this->command->print();
}

/* -------------------------- Trace_State Methods ----------------------------------------*/
// return latest version of the file, or create file and add to record if not found
File* trace_state::find_file(std::string path) {
    File* ret = NULL;
    for (auto f = this->files.begin(); f != this->files.end(); ++f) {
        if ((*f)->filename.compare(path) == 0) {
            if (ret == NULL) 
                ret = *f;
            else if ((*f)->version > ret->version) 
                ret = *f;
        }
    }
    if (ret == NULL) {
        ret = new File(path, NULL, this);
        this->files.insert(ret);
    }
    return ret;
}

void trace_state::to_graph(void) {
    this->g.start_graph();
    for (auto c = this->commands.begin(); c != this->commands.end(); ++c) {
        (*c)->to_graph();
    }
    this->g.close_graph();
}

void trace_state::add_dependency(Process* proc, struct file_reference file, enum dependency_type type) {
    if (file.fd == -100)   
        return;
    fprintf(stdout, "[%d] Dep: %d -> ",proc->thread_id, file.fd);
    std::string path;
    if (proc->fds.find(file.fd) == proc->fds.end()) {
        path = "file not found, fd: " + std::to_string(file.fd);
    } else {
        path = proc->fds.find(file.fd)->second;
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
            proc->command->deleted_files.insert(f);    
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

void trace_state::add_change_cwd(Process* proc, struct file_reference file) {
    fprintf(stdout, "[%d] Change working directory to ", proc->thread_id);
    //this->processes.find(thread_id)->second->cwd = file.path;
    if (file.fd == AT_FDCWD) {
        fprintf(stdout, "%s\n", file.path);
    } else {
        fprintf(stdout, "{%d/}%s\n", file.fd, file.path);
    }
}

void trace_state::add_change_root(Process* proc, struct file_reference file) {
    fprintf(stdout, "[%d] Change root to ", proc->thread_id);
    //this->processes.find(thread_id)->second->root = file.path;
    if (file.fd == AT_FDCWD) {
        fprintf(stdout, "%s\n", file.path);
    } else {
        fprintf(stdout, "{%d/}%s\n", file.fd, file.path);
    }
}


// get filenames from their open
void trace_state::add_open(Process* proc, int fd, struct file_reference file, int access_mode, bool is_rewrite) {
    fprintf(stdout, "[%d] Open %d -> ", proc->thread_id, fd);
    // TODO take into account root and cwd
    File* f = this->find_file(file.path);
    if (is_rewrite) {
        fprintf(stdout, "REWRITE ");
        f = f->make_version();
        f->dependable = false;
        this->files.insert(f);
    } 
    proc->fds.insert(std::pair<int, std::string>(fd,file.path));
    if (file.fd == AT_FDCWD) {
        fprintf(stdout, "%s\n", file.path);
    } else {
        fprintf(stdout, "{%d/}%s\n", file.fd, file.path);
    }
}

// TODO handle pipes
void trace_state::add_pipe(Process* proc, int fds[2]) {
    fprintf(stdout, "[%d] Pipe %d, %d\n", proc->thread_id, fds[0], fds[1]);
}

void trace_state::add_dup(Process* proc, int duped_fd, int new_fd) {
    fprintf(stdout, "[%d] Dup %d <- %d\n", proc->thread_id, duped_fd, new_fd);
    proc->fds.insert(std::pair<int, std::string>(new_fd, proc->fds.find(duped_fd)->second));
}

// TODO deal with race conditions
void trace_state::add_mmap(Process* proc, int fd) {
    // TODO: look up the permissions that the file was opened with
    fprintf(stdout, "[%d] Mmap %d\n", proc->thread_id, fd);
    File* f = this->find_file(proc->fds.find(fd)->second);
    f->mmaps.insert(proc);
    proc->mmaps.insert(f);
}

void trace_state::add_close(Process* proc, int fd) {
    fprintf(stdout, "[%d] Close %d\n", proc->thread_id, fd);
    proc->fds.erase(fd);
}

// create process node
void trace_state::add_fork(Process* proc, pid_t child_process_id) {
    fprintf(stdout, "[%d] Fork %d\n", proc->thread_id, child_process_id);
    this->processes.insert(std::pair<pid_t, Process*>(child_process_id, new Process(child_process_id, proc->cwd, proc->command)));
}

// fill in process node
void trace_state::add_exec(Process* proc, char* exe_path) {
    fprintf(stdout, "[%d] Inside exec: %s\n", proc->thread_id, exe_path);
    Command* cmd = new Command(this, exe_path);
    std::cout << "Pushed " << cmd->cmd << " to commands list!\n";
    proc->command->children.push_front(cmd);
    proc->command = cmd;     
    free(exe_path);
}

void trace_state::add_exec_argument(Process* proc, char* argument, int index) { 
    std::string arg = std::string(argument);
    proc->command->args.push_back(arg);
    fprintf(stdout, "[%d]     Arg %d: %s\n", proc->thread_id, index, argument);
    free(argument);
}

void trace_state::add_exit(Process* proc) {
    fprintf(stdout, "[%d] Exit\n", proc->thread_id);
    for (auto f = proc->mmaps.begin(); f != proc->mmaps.end(); ++f) {
        (*f)->mmaps.erase(proc);
    }
}

