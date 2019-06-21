#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <iostream>

#include <sys/types.h>
#include <fcntl.h>

#include <filesystem>

#include "middle.h"

/* ------------------------------ Command Methods -----------------------------------------*/
Command::Command(trace_state* state, std::string args) : state(state), args(args) {}


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
    fnew->dependable = f->dependable;
    this->outputs.insert(fnew);
    this->wr_interactions.insert(fnew);
    //TODO add to the context's files -> need to add a context field
    this->state->files.insert(fnew);     
}

std::string Command::to_graph(void) {
    return NULL;    
}

void Command::print(void) {
    std::cout << args << "\n";
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

File::File(std::string path, Command* writer) : filename(path), writer(writer) {}

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

bool can_depend(void) {
    return true;
}

void File::print_file(void) {
    fprintf(stderr, "File: %s, Version: %d", this->filename.c_str(), this->version);
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

void trace_state::add_dependency(pid_t thread_id, struct file_reference file, enum dependency_type type) {
    fprintf(stderr, "[%d] Dep: ", thread_id);
    Process* proc = this->processes.find(thread_id)->second;
    std::string path = proc->fds.find(file.fd)->second;
    fprintf(stderr, "file: %s ", path.c_str());
    File* f = this->find_file(path);
    
    switch (type) {
    //TODO handle file side of things
    case DEP_READ:
        fprintf(stderr, "read");
        proc->command->add_input(f);
	break;
    case DEP_MODIFY:
        fprintf(stderr, "modify");
        proc->command->add_output(f);
        break;
    case DEP_CREATE:
        fprintf(stderr, "create");
        // create edge
	break;
    case DEP_REMOVE:
        fprintf(stderr, "remove");
        // delete edge
        break;
    }
    if (!file.follow_links) {
        fprintf(stderr, " (nofollow)");
    }
    if (file.path == NULL) {
        fprintf(stderr, " FD %d\n", file.fd);
    } else if (file.fd == AT_FDCWD) {
        fprintf(stderr, " %s\n", file.path);
    } else {
        fprintf(stderr, " {%d/}%s\n", file.fd, file.path);
    }
}

void trace_state::add_change_cwd(pid_t thread_id, struct file_reference file) {
    fprintf(stderr, "[%d] Change working directory to ", thread_id);
    this->processes.find(thread_id)->second->cwd = file.path;
    if (file.fd == AT_FDCWD) {
        fprintf(stderr, "%s\n", file.path);
    } else {
        fprintf(stderr, "{%d/}%s\n", file.fd, file.path);
    }
}

void trace_state::add_change_root(pid_t thread_id, struct file_reference file) {
    fprintf(stderr, "[%d] Change root to ", thread_id);
    this->processes.find(thread_id)->second->root = file.path;
    if (file.fd == AT_FDCWD) {
        fprintf(stderr, "%s\n", file.path);
    } else {
        fprintf(stderr, "{%d/}%s\n", file.fd, file.path);
    }
}


// get filenames from their open
void trace_state::add_open(pid_t thread_id, int fd, struct file_reference file, int access_mode, bool is_rewrite) {
    //TODO rethink versioning with open
    fprintf(stderr, "[%d] Open %d -> ", thread_id, fd);
    // take into account root and cwd
    Process* cur_proc = this->processes.find(thread_id)->second;
    //fprintf(stderr, "CWD %s -> ", cur_proc->cwd.c_str());
    //TODO root handling
    //std::string path = cur_proc->root + cur_proc->cwd + file.path;
    cur_proc->fds.insert(std::pair<int, std::string>(fd,file.path));
    if (file.fd == AT_FDCWD) {
        fprintf(stderr, "%s\n", file.path);
    } else {
        fprintf(stderr, "{%d/}%s\n", file.fd, file.path);
    }
}


// TODO handle these two
void trace_state::add_pipe(pid_t thread_id, int fds[2]) {
    fprintf(stderr, "[%d] Pipe %d, %d\n", thread_id, fds[0], fds[1]);
}

void trace_state::add_dup(pid_t thread_id, int duped_fd, int new_fd) {
    fprintf(stderr, "[%d] Dup %d <- %d\n", thread_id, duped_fd, new_fd);
}

void trace_state::add_mmap(pid_t thread_id, int fd) {
    // TODO: look up the permissions that the file was opened with
    fprintf(stderr, "[%d] Mmap %d\n", thread_id, fd);
}

// TODO: Do we need this?
// close edges?
void trace_state::add_close(pid_t thread_id, int fd) {
    fprintf(stderr, "[%d] Close %d\n", thread_id, fd);
}

// create process node
void trace_state::add_fork(pid_t parent_thread_id, pid_t child_process_id) {
    fprintf(stderr, "[%d] Fork %d\n", parent_thread_id, child_process_id);
    Process* parent_proc = this->processes.find(parent_thread_id)->second;
    this->processes.insert(std::pair<pid_t, Process*>(child_process_id, new Process(parent_proc->cwd, parent_proc->command)));
}

// fill in process node
void trace_state::add_exec(pid_t process_id, char* exe_path) {
    fprintf(stderr, "[%d] Inside exec: %s\n", process_id, exe_path);
    Process* cur_proc = this->processes.find(process_id)->second;
    Command* cmd = new Command(this, exe_path);
    //TODO special handling for first exec
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
	
    cur_proc->command->children.push_front(cmd);
    cur_proc->command = cmd;     
    free(exe_path);
}

void trace_state::add_exec_argument(pid_t process_id, char* argument, int index) {
    fprintf(stderr, "[%d]     Arg %d: %s\n", process_id, index, argument);
    free(argument);
}

// TODO close relevant mmaps
void trace_state::add_exit(pid_t thread_id) {
    fprintf(stderr, "[%d] Exit\n", thread_id);
}

void trace_state::to_graph(void) {
    for (auto it = this->processes.cbegin(); it != this->processes.cend(); ++it) {
        std::cout << it->first << ": ";
        it->second->print();
        //"\n";
    }
    return;
}




