#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <iostream>
#include <sys/types.h>
#include <fcntl.h>

//#include <filesystem>

#include "middle.h"

//TODO fix truncation and loops and whatnot
//hide sysfiles flag 


/* ------------------------------ Command Methods -----------------------------------------*/
Command::Command(trace_state* state, std::string cmd) : state(state), cmd(cmd) {}
/*
   Command* Command::make_child(std::string args) {
   Command* cmd = new Command(this->state, args);
   this->children.insert(cmd);
   return cmd;
   }
 */
//TODO find file earlier and pass 
void Command::add_input(File* f) {
    // TODO search through set to make sure we havent already listed it since objects
    // search through all files, do versioning 
    f->interactions.push_front(this);
    //if (f->can_depend()) {
    f->users.insert(this);
    this->inputs.insert(f);
    //}
    //if we've read from the file previously, check for a race
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
    this->rd_interactions.insert(f);
    //this->inputs.insert(new File(filename));
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
                //(*wrs)->has_race = true;
                this->has_race = true;
                return;
            }
        }
    }
    // if we haven't written to this file before, create a new version
    f->interactions.push_front(this);
    File* fnew = new File(f->filename, this);
    fnew->version = f->version + 1;
    fnew->producers.insert(this);
    fnew->dependable = true;
    this->outputs.insert(fnew);
    this->wr_interactions.insert(fnew);
    //TODO add to the context's files -> need to add a context field
    this->state->files.insert(fnew);     
}

std::string Command::to_graph(void) {
    std::cout << "ADDING COMMAND: " << this->cmd << "\n";
    std::string id = this->cmd;
    for (auto it = this->args.begin(); it != this->args.end(); ++it) {
        id += (*it);
    }
    this->state->g.add_node(id, this->cmd, "");
    //Graph g = this->state->g;
    // handle races
    for (auto c = this->children.begin(); c != this->children.end(); ++c) {
        std::string child_id = (*c)->to_graph();
        this->state->g.add_edge(id, child_id, "style=dashed");
    }

    for (auto i = this->inputs.begin(); i != this->inputs.end(); ++i) {
        if ((*i)->is_local()) {

            printf("adding input: %s\n", (*i)->filename.c_str());
            if (!((*i)->is_intermediate())) {
                std::string node_id = (*i)->filename + std::to_string((*i)->version);
                this->state->g.add_node(node_id, (*i)->filename, "shape=rectangle");
                this->state->g.add_edge(node_id, id, "arrowhead=empty");
                std::cout << "adding input, file address is: " << (*i) << "\n";
            } 
        }
    }

    for (auto o = this->outputs.begin(); o != this->outputs.end(); ++o) { 
        std::string node_id = (*o)->filename + std::to_string((*o)->version);
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

    return id;   
}

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
// this may just be duplicate of is intermediate, may not be necessary 
//TODO prune
/*
   bool File::is_local(void) {
   return false;
   }
 */

File::File(std::string path, Command* writer) : filename(path), writer(writer) {
    this->dependable = true;
}

//TODO add logic here
bool File::is_local(void) {
    //TODO sysfile flag;
    if (this->filename.find("usr")!=std::string::npos || this->filename.find("lib")!=std::string::npos || this->filename.find("dev")!=std::string::npos) {
        return false;
    } else {
        return true;
    }
}

bool File::is_intermediate(void) {
    //TODO sysfile flag
    // 
    if ((this->users.size()!=0)&&(this->producers.size()!=0)&&(this->filename.find("tmp")!=std::string::npos)) {
        return true;
    } else {
        return false;
    }
}

void File::collapse(void) {
    //this->has_race = true;
    for (auto it = this->interactions.begin(); it != this->interactions.end(); ++it) {
        //(*it)->has_race = true;
        this->conflicts.push_front(*it);
    }
}

//TODO rework
bool File::can_depend(Command* cmd) {
    if(this->writer == cmd) {
        return false;
    } else {
        return this->dependable;
    }
}

File* File::make_version(void) {
    File* f = new File(this->filename, this->writer);
    f->version = this->version + 1;
    return f;
}

void File::print_file(void) {
    fprintf(stdout, "File: %s, Version: %d", this->filename.c_str(), this->version);
}

/* ----------------------------- Process Methods ------------------------------------------*/
Process::Process(std::string cwd, Command* command) : cwd(cwd), command(command) {}

void Process::print(void) {
    this->command->print();
}
/* -------------------------- Trace_State Methods ----------------------------------------*/
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
        ret = new File(path, NULL);
        this->files.insert(ret);
    }

    return ret;
}

void trace_state::to_graph(void) {
    std::cout << "STARTING GRAPH\n";
    this->g.start_graph();
    for (auto c = this->commands.begin(); c != this->commands.end(); ++c) {
        (*c)->to_graph();
    }
    this->g.close_graph();
}

void trace_state::add_dependency(pid_t thread_id, struct file_reference file, enum dependency_type type) {
    if (file.fd == -100)   
        return;
    fprintf(stdout, "[%d] Dep: %d -> ",thread_id, file.fd);
    Process* proc = this->processes.find(thread_id)->second;
    std::string path;
    if (proc->fds.find(file.fd) == proc->fds.end()) {
        path = "file not found, index: " + std::to_string(file.fd);
        //std::cout << proc->fds.find(file.fd)->second << "\n";
    } else {
        path = proc->fds.find(file.fd)->second;
    }
    File* f = this->find_file(path);

    fprintf(stdout, "file: %s-%d ", path.c_str(), f->version);
    switch (type) {
        //TODO handle file side of things
        case DEP_READ:
            fprintf(stdout, "read");
            if (f->dependable)
                fprintf(stdout, ", dependable");
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
            // delete edge
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
    //TODO rethink versioning with open
    fprintf(stdout, "[%d] Open %d -> ", thread_id, fd);
    // take into account root and cwd
    Process* cur_proc = this->processes.find(thread_id)->second;
    //fprintf(stdout, "CWD %s -> ", cur_proc->cwd.c_str());
    //TODO root handling
    //std::string path = cur_proc->root + cur_proc->cwd + file.path;
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


// TODO handle these two
void trace_state::add_pipe(pid_t thread_id, int fds[2]) {
    fprintf(stdout, "[%d] Pipe %d, %d\n", thread_id, fds[0], fds[1]);
}

//TODO
void trace_state::add_dup(pid_t thread_id, int duped_fd, int new_fd) {
    fprintf(stdout, "[%d] Dup %d <- %d\n", thread_id, duped_fd, new_fd);
    Process* proc = this->processes.find(thread_id)->second;
    proc->fds.insert(std::pair<int, std::string>(new_fd, proc->fds.find(duped_fd)->second));
}

void trace_state::add_mmap(pid_t thread_id, int fd) {
    // TODO: look up the permissions that the file was opened with
    fprintf(stdout, "[%d] Mmap %d\n", thread_id, fd);
}

// TODO:REMOVE FROM FDs list!! 
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
    //cur_proc->command->children.push_front(cmd);
    //this->commands.push_front(cmd);
    std::cout << "Pushed " << cmd->cmd << " to commands list!\n";
    //TODO special handling for first exec
    /*
       printf("cur_proc: ");
       std::cout << cur_proc << '\n';    
       if (cur_proc == NULL) {
       printf("NEW PROC\n");
       cur_proc = new Process(this->starting_dir, cmd);
       this->commands.push_front(cmd);
       this->processes.insert(std::pair<pid_t, Process*>(process_id, cur_proc));
    // add it to commands
    // add it to process
    }
     */	
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

/*
   void trace_state::to_graph(void) {
   for (auto it = this->processes.cbegin(); it != this->processes.cend(); ++it) {
   std::cout << it->first << ": ";
   it->second->print();
//"\n";
}
return;
}
 */



