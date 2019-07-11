#include "graph.h"
#include "fingerprint.h"
#include "db.capnp.h"

#include <stdio.h>
#include <stdint.h>
#include <fcntl.h>

#include <list>
#include <vector>
#include <set>
#include <queue>

#include <capnp/message.h>
#include <capnp/serialize.h>

#define UNCHANGED 0
#define CHANGED   1
#define UNKNOWN   2

struct db_command;
struct db_file {
    unsigned int id;
    bool is_pipe;
    unsigned int writer_id;
    std::string path;
    int status;

    db_file(unsigned int id, bool is_pipe, std::string path, int status) : id(id), is_pipe(is_pipe), path(path), status(status) {}
    bool is_local(void) {
        if (path.find("/usr/") != std::string::npos ||
                path.find("/lib/") != std::string::npos ||
                path.find("/etc/") != std::string::npos ||
                path.find("/dev/") != std::string::npos ||
                path.find("/proc/") != std::string::npos) {
            return false;
        } else {
            return true;
        }
    } 
};

//TODO setup arguments
struct db_command { 
    unsigned int id;
    unsigned int num_descendants;
    std::string executable;
    std::vector<std::string> args;
    std::set<db_file*> inputs;
    std::set<db_file*> outputs;
    std::set<db_file*> creations;
    std::set<db_file*> deletions;
    bool rerun;

    db_command(unsigned int id, unsigned int num_descendants, std::string executable) : id(id), num_descendants(num_descendants), executable(executable) {
        this->rerun = false;
    }
};

static void draw_graph_nodes(Graph* graph, bool show_sysfiles, db_command* commands[], db_file* files[], size_t command_count, size_t file_count) {
    for (size_t command_id = 0; command_id < command_count; command_id++) {
        std::string attr = "";
        if (commands[command_id]->rerun) {
            attr = "style=filled fillcolor=gold";
        }
        std::string label;
        if (commands[command_id]->args.size() > 0) {
            label = commands[command_id]->args[0];
        } else {
            label = commands[command_id]->executable;
        }
        graph->add_node("c" + std::to_string(command_id), label, attr);
    }

    for (size_t file_id = 0; file_id < file_count; file_id++) {
        if (show_sysfiles || files[file_id]->is_local()) {
            std::string attr;
            std::string label;
            if (files[file_id]->is_pipe) {
                attr = "shape=diamond";
                label = "[pipe]";
            } else {
                attr = "shape=rectangle";
                label = files[file_id]->path;
            }
            if (files[file_id]->status == CHANGED) {
                attr += " style=filled fillcolor=gold";
            }
            graph->add_node("f" + std::to_string(files[file_id]->id), label, attr);
        }
    }
}

static void draw_graph_edges(Graph* graph, bool show_sysfiles, db_command* commands[], db_file* files[], size_t start_id, size_t end_id) {
    size_t id = start_id;
    while (id < end_id) {
        auto root = commands[id];
        if (start_id > 0) {
            graph->add_edge("c" + std::to_string(start_id - 1), "c" + std::to_string(id), "style=dashed");
        }

        for (auto i : root->inputs) {
            if (show_sysfiles || i->is_local()) {
                graph->add_edge("f" + std::to_string(i->id), "c" + std::to_string(root->id), "arrowhead=empty");
            }
        }

        for (auto o : root->outputs) {
            if (show_sysfiles || o->is_local()) {
                graph->add_edge("c" + std::to_string(root->id), "f" + std::to_string(o->id), "arrowhead=empty");
            }
        }

        for (auto d : root->deletions) {
            if (show_sysfiles || d->is_local()) {
                graph->add_edge("c" + std::to_string(root->id), "f" + std::to_string(d->id), "color=red arrowhead=empty");
            }
        }

        for (auto c : root->creations) {
            if (show_sysfiles || c->is_local()) {
                graph->add_edge("c" + std::to_string(root->id), "f" + std::to_string(c->id), "color=blue arrowhead=empty");
            }
        }

        size_t children_start = id + 1;
        size_t children_end = children_start + root->num_descendants;
        draw_graph_edges(graph, show_sysfiles, commands, files, children_start, children_end);

        id = children_end;
    }
}


// Simulate the running of a command by marking all of the outputs of all descendants of a command as changed
static void simulate_run(db_command* commands[], db_command* cur_command) {
    cur_command->rerun = true;
    for (auto out : cur_command->outputs) {
        out->status = CHANGED;
    }

    // Running a command runs its children
    unsigned int current_id = cur_command->id + 1;
    unsigned int end_id = current_id + cur_command->num_descendants;
    while (current_id < end_id) {
        simulate_run(commands, commands[current_id]);
        current_id += commands[current_id]->num_descendants + 1;
    }
}

int main(int argc, char* argv[]) {

    int db = open("db.dodo", O_RDONLY);
    if (db < 0) {
        perror("Failed to open database");
        exit(2);
    }

    bool use_fingerprints = true;
    bool show_sysfiles = false;
    // Parse arguments
    for (int i = 1; i < argc; i++) {
        if ("--no-fingerprints" == std::string(argv[i])) {
            use_fingerprints = false;
        } else if ("--show-sysfiles" == std::string(argv[i])) {
            show_sysfiles = true;
        }
    }

    ::capnp::StreamFdMessageReader message(db);
    auto db_graph = message.getRoot<db::Graph>();

    // reconstruct the graph

    // initialize array of files
    db_file* files[db_graph.getFiles().size()];
    unsigned int file_id = 0;
    for (auto file : db_graph.getFiles()) {
        int flag = UNCHANGED;
        std::string path = std::string((const char*) file.getPath().begin(), file.getPath().size()); 
        bool is_pipe = (file.getType() == db::FileType::PIPE);
        // If the fingerprint has changed, mark it so
        if (is_pipe) {
            flag = UNKNOWN;
        } else if (use_fingerprints && !match_fingerprint(file)) {
            flag = CHANGED;
        }
        // if the path was passed as an argument to the dryrun, mark it as changed or unchanged,
        // whatever the user specified
        int explicit_flag = CHANGED;
        for (int i = 1; i < argc; i++) {
            if ("--changed" == std::string(argv[i])) {
                explicit_flag = CHANGED;
            } else if ("--unchanged" == std::string(argv[i])) {
                explicit_flag = UNCHANGED;
            } else if (path == std::string(argv[i])) {
                flag = explicit_flag;
            }
        }
        files[file_id] = new db_file(file_id, is_pipe, path, flag);
        file_id++;
    }

    // initialize array of commands
    db_command* commands[db_graph.getCommands().size()];
    unsigned int cmd_id = 0;
    for (auto cmd : db_graph.getCommands()) { 
        auto executable = std::string((const char*) cmd.getExecutable().begin(), cmd.getExecutable().size());
        commands[cmd_id] = new db_command(cmd_id, cmd.getDescendants(), executable);
        for (auto arg : cmd.getArgv()) {
            commands[cmd_id]->args.push_back(std::string((const char*) arg.begin(), arg.size()));
        }
        cmd_id++;
    } 

    // Add the dependencies
    for (auto dep : db_graph.getInputs()) {
        commands[dep.getCommandID()]->inputs.insert(files[dep.getFileID()]);
    }
    for (auto dep : db_graph.getOutputs()) {
        db_file* file = files[dep.getFileID()];
        file->writer_id = dep.getCommandID();
        // if the file is an output of a command, mark it's status as unknown (until the command is run/simulated)
        if (!(file->status == CHANGED)) {
            files[dep.getFileID()]->status = UNKNOWN;
        }
        commands[dep.getCommandID()]->outputs.insert(files[dep.getFileID()]);
    }
    //TODO what does the run do with removals?
    for (auto dep : db_graph.getRemovals()) {
        commands[dep.getCommandID()]->deletions.insert(files[dep.getFileID()]);
    }
    for (auto dep : db_graph.getCreates()) {
        commands[dep.getCommandID()]->creations.insert(files[dep.getFileID()]);
    }


    // initialize worklist to commands root 
    //TODO multiple roots 
    std::queue<db_command*> worklist;
    worklist.push(commands[0]);

    while (worklist.size() != 0) {
        // take command off list to run/check
        db_command* cur_command = worklist.front();
        worklist.pop();

        // check if command is ready to run
        bool ready = true;
        bool rerun = false;
        for (auto in : cur_command->inputs) {
            // if the input is an output of one of our descendants, run this command
            if (in->writer_id > cur_command->id && in->writer_id <= cur_command->id + cur_command->num_descendants) {
                rerun = true;
            } else if (in->status == UNKNOWN) {
                ready = false;
            } else if (in->status == CHANGED) {
                rerun = true;
            }
        }
        // if any dependencies are "unknown," put the command back in the worklist
        if (!ready) {
            worklist.push(cur_command);
        } else {
            if (rerun) {
                // if the command is ready to run and one of it's dependencies has changed, 
                //" rerun" it (print for now)
                std::cout << cur_command->executable;
                for (size_t arg_index = 1; arg_index < cur_command->args.size(); arg_index++) {
                    std::cout << " " << cur_command->args[arg_index];
                }
                std::cout << std::endl;
                // mark its outputs as changed
                simulate_run(commands, cur_command);
            } else {
                // if all inputs are unchanged, mark outputs as unchanged (unless they are explicitly marked as changed in the dryrun)
                for (auto out : cur_command->outputs) {
                    if (out->status == UNKNOWN) {
                        out->status = UNCHANGED;
                    }
                }

                // add command's direct children to worklist
                unsigned int current_id = cur_command->id + 1;
                unsigned int end_id = current_id + cur_command->num_descendants;
                while (current_id < end_id) {
                    worklist.push(commands[current_id]);
                    current_id += commands[current_id]->num_descendants + 1;
                }
            }
        }
    }

    Graph graph;
    graph.start_graph();
    draw_graph_nodes(&graph, show_sysfiles, commands, files, db_graph.getCommands().size(), db_graph.getFiles().size());
    draw_graph_edges(&graph, show_sysfiles, commands, files, 0, db_graph.getCommands().size());
    graph.close_graph();
}
