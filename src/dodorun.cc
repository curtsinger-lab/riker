#include "graph.h"
#include "fingerprint.h"
#include "db.capnp.h"

#include <stdio.h>
#include <stdint.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <spawn.h>

#include <limits>
#include <list>
#include <vector>
#include <set>
#include <queue>

#include <capnp/message.h>
#include <capnp/serialize.h>

extern char** environ;

#define UNCHANGED 0
#define CHANGED   1
#define UNKNOWN   2

struct db_command;
struct db_file {
    size_t id;
    bool is_pipe;
    bool is_cached;
    size_t writer_id;
    std::set<db_command*> readers;
    size_t reader_antidemands_left;
    bool propagated;
    std::string path;
    int status;

    db_file(unsigned int id, bool is_pipe, bool is_cached, std::string path, int status) : id(id), is_pipe(is_pipe), is_cached(is_cached), writer_id(std::numeric_limits<size_t>::max()), propagated(false), path(path), status(status) {}
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
    size_t id;
    size_t num_descendants;
    std::string executable;
    std::vector<std::string> args;
    std::set<db_file*> inputs;
    std::set<db_file*> outputs;
    std::set<db_file*> creations;
    std::set<db_file*> deletions;
    size_t output_antidemands_left;
    bool propagated;
    bool demanded;
    bool rerun;
    bool collapse_with_parent;
    db_command* cluster_root;

    db_command(size_t id, size_t num_descendants, std::string executable) : id(id), num_descendants(num_descendants), executable(executable), propagated(false), demanded(false), rerun(false), collapse_with_parent(false) {}
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

// Run a callback on a command and every child or descendant that has been collapsed with that
// command into a single cluster. The callback is also provided with information about whether
// a command is a leaf (i.e. has no children also in the cluster).
template<typename callback_ty>
static void cluster_for_each(db_command* commands[], db_command* parent, callback_ty& callback) {
    bool leaf = true;

    // Recursively call on all collapsed children
    unsigned int current_id = parent->id + 1;
    unsigned int end_id = current_id + parent->num_descendants;
    while (current_id < end_id) {
        if (commands[current_id]->collapse_with_parent) {
            leaf = false;
            cluster_for_each(commands, commands[current_id], callback);
        }
        current_id += commands[current_id]->num_descendants + 1;
    }

    // Call on the parent
    callback(parent, leaf);
}

static void propagate_file_demand(db_command* commands[], db_file* file, bool demanded);
static void propagate_command_demand(db_command* commands[], db_command* command, bool demanded) {
    // Go to the root of the cluster
    command = command->cluster_root;

    if (!command->demanded && (!command->propagated || demanded)) { // Only propagate once
        command->demanded = demanded;

        auto propagate = [&](db_command* cluster_elem, bool leaf) {
            // Propagate to all temp file inputs
            for (auto in : cluster_elem->inputs) {
                if (!demanded) {
                    in->reader_antidemands_left -= 1;
                }
                propagate_file_demand(commands, in, demanded);
            }

            if (demanded && leaf) {
                // If we are demanding a command, also demand its children
                size_t current_id = command->id + 1;
                size_t end_id = current_id + command->num_descendants;
                while (current_id < end_id) {
                    propagate_command_demand(commands, commands[current_id], demanded);
                    current_id += commands[current_id]->num_descendants + 1;
                }
            }
        };
        if (demanded || command->output_antidemands_left == 0) {
            command->propagated = true;
            cluster_for_each(commands, command, propagate);
        }
    }
}

static void propagate_file_demand(db_command* commands[], db_file* file, bool demanded) {
    if ((!file->propagated || demanded) && !file->is_cached && file->writer_id != std::numeric_limits<size_t>::max()) {
        if (demanded || file->reader_antidemands_left == 0) {
            file->propagated = true;
            if (!demanded) {
                commands[file->writer_id]->output_antidemands_left -= 1;
            }
            propagate_command_demand(commands, commands[file->writer_id], demanded);
        }
    }
}

// Simulate the running of a command by marking all of the outputs of all descendants of a command as changed
static void simulate_run(db_command* commands[], db_command* cur_command) {
    cur_command->rerun = true;
    for (auto out : cur_command->outputs) {
        out->status = CHANGED;
        for (auto reader : out->readers) {
            propagate_command_demand(commands, reader, true);
        }
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
    bool dry_run = false;
    // Parse arguments
    for (int i = 1; i < argc; i++) {
        if ("--no-fingerprints" == std::string(argv[i])) {
            use_fingerprints = false;
        } else if ("--show-sysfiles" == std::string(argv[i])) {
            show_sysfiles = true;
        } else if ("--dry-run" == std::string(argv[i])) {
            dry_run = true;
        }
    }

    ::capnp::StreamFdMessageReader message(db);
    auto db_graph = message.getRoot<db::Graph>();

    // reconstruct the graph

    // initialize array of files
    db_file* files[db_graph.getFiles().size()];
    unsigned int file_id = 0;
    for (auto file : db_graph.getFiles()) {
        int flag = UNKNOWN;
        std::string path = std::string((const char*) file.getPath().begin(), file.getPath().size()); 
        bool is_pipe = (file.getType() == db::FileType::PIPE);
        // if the path was passed as an argument to the dryrun, mark it as changed or unchanged,
        // whatever the user specified
        if (!is_pipe && file.getLatestVersion()) {
            if (use_fingerprints) {
                flag = match_fingerprint(file) ? UNCHANGED : CHANGED;
            } else {
                flag = UNCHANGED;
            }

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
        }
        bool is_cached = file.getLatestVersion() && !is_pipe;
        files[file_id] = new db_file(file_id, is_pipe, is_cached, path, flag);
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
        commands[cmd_id]->collapse_with_parent = cmd.getCollapseWithParent();
        cmd_id++;
    }

    // Add the dependencies
    for (auto dep : db_graph.getInputs()) {
        files[dep.getFileID()]->readers.insert(commands[dep.getCommandID()]);
        commands[dep.getCommandID()]->inputs.insert(files[dep.getFileID()]);
    }
    for (auto dep : db_graph.getOutputs()) {
        db_file* file = files[dep.getFileID()];
        file->writer_id = dep.getCommandID();
        // if the file is an output of a command, mark it's status as unknown (until the command is run/simulated)
        files[dep.getFileID()]->status = UNKNOWN;
         commands[dep.getCommandID()]->outputs.insert(files[dep.getFileID()]);
    }
    //TODO what does the run do with removals?
    for (auto dep : db_graph.getRemovals()) {
        commands[dep.getCommandID()]->deletions.insert(files[dep.getFileID()]);
    }
    for (auto dep : db_graph.getCreates()) {
        commands[dep.getCommandID()]->creations.insert(files[dep.getFileID()]);
    }

    // Collapsing codependencies and cycles
    // TODO: This probably wants to be part of the serialized representation, not here
    std::queue<db_command*> collapse_worklist;
    for (size_t root_id = 0; root_id < db_graph.getCommands().size(); root_id++) {
        // If this command has already been included in another cluster, then its dependencies
        // have already been considered, so move on.
        if (commands[root_id]->collapse_with_parent) {
            continue;
        }

        // At this point, we know that we are the root of a cluster and need to determine what other
        // commands are in the cluster. These commands can be forced into the cluster when an input
        // of the cluster is something else descended from the cluster, i.e. from the cluster's
        // root, i.e. from the current command.
        collapse_worklist.push(commands[root_id]);
        while (!collapse_worklist.empty()) {
            auto to_collapse = collapse_worklist.front();
            collapse_worklist.pop();

            for (auto in : to_collapse->inputs) {
                if (in->writer_id > root_id && in->writer_id <= root_id + commands[root_id]->num_descendants) {
                    // This input is a descendant of the cluster root. Mark it and all
                    // ancestors up to the root as part of the cluster by traversing downwards.
                    size_t parent_id = root_id;
                    while (parent_id != in->writer_id) {
                        // Loop through children to find the ancestor of the writer
                        size_t current_id = parent_id + 1;
                        size_t end_id = current_id + commands[parent_id]->num_descendants;
                        while (current_id < end_id) {
                            size_t next_id = current_id + commands[current_id]->num_descendants + 1;
                            if (next_id > in->writer_id) {
                                // Success: mark and move on to the next level
                                if (!commands[current_id]->collapse_with_parent) {
                                    commands[current_id]->collapse_with_parent = true;
                                    collapse_worklist.push(commands[current_id]);
                                }
                                parent_id = current_id;
                                break;
                            }
                            current_id = next_id;
                        }
                    }
                }
            }
        }
    }

    for (size_t file_id = 0; file_id < db_graph.getFiles().size(); file_id++) {
        files[file_id]->reader_antidemands_left = files[file_id]->readers.size();
    }

    for (size_t command_id = 0; command_id < db_graph.getCommands().size(); command_id++) {
        if (commands[command_id]->collapse_with_parent) {
            //std::cerr << "Command " << commands[command_id]->executable << " is collapsed with parent." << std::endl;
            continue;
        }

        // Map each command to its cluster root
        auto set_cluster_root = [&](db_command* cluster_member, bool leaf) {
            cluster_member->cluster_root = commands[command_id];
        };
        cluster_for_each(commands, commands[command_id], set_cluster_root);

        // count antidemands needed to declare a command safe to
        // descend (total non-cached cluster outputs)
        size_t antidemands_needed = 0;
        auto tally_non_cached_outputs = [&](db_command* cluster_member, bool leaf) {
            for (auto out : cluster_member->outputs) {
                if (!out->is_cached) {
                    antidemands_needed += 1;
                }
            }
            // Ignore in-cluster edges
            for (auto in : cluster_member->inputs) {
                if (!in->is_cached && command_id <= in->writer_id && in->writer_id <= command_id + commands[command_id]->num_descendants) {
                    in->reader_antidemands_left -= 1;
                }
            }
        };
        cluster_for_each(commands, commands[command_id], tally_non_cached_outputs);
        //std::cerr << "Found need for " << antidemands_needed << " antidemands." << std::endl;

        commands[command_id]->output_antidemands_left = antidemands_needed;
    }

    for (size_t file_id = 0; file_id < db_graph.getFiles().size(); file_id++) {
        if (!files[file_id]->is_cached && files[file_id]->writer_id != std::numeric_limits<size_t>::max() && files[file_id]->reader_antidemands_left == 0) {
            //std::cerr << files[file_id]->path << " has no need for reader antidemands." << std::endl;
            files[file_id]->propagated = true;
            commands[files[file_id]->writer_id]->cluster_root->output_antidemands_left -= 1;
        }
    }

    // initialize demands
    for (size_t file_id = 0; file_id < db_graph.getFiles().size(); file_id++) {
        if (files[file_id]->status == CHANGED) {
            // If this is not cached and generated by the build, rebuild it
            propagate_file_demand(commands, files[file_id], true);
            if (files[file_id]->writer_id == std::numeric_limits<size_t>::max()) {
                // If we won't make this decision later (because a writer exists), rerun
                // the readers.
                for (auto reader : files[file_id]->readers) {
                    //std::cerr << "  Demanding " << reader->executable << " due to dependency on changed " << files[file_id]->path << std::endl;
                    propagate_command_demand(commands, reader, true);
                }
            }
        }
    }

    // initialize worklist to commands root 
    //TODO multiple roots 
    std::queue<db_command*> worklist;
    worklist.push(commands[0]);

    while (worklist.size() != 0) {
        // take command off list to run/check
        db_command* cur_command = worklist.front();
        worklist.pop();
        //std::cerr << "Testing " << cur_command->executable << std::endl;

        // Step 1: If demanded, check if all inputs to the subtree are ready, then run
        // the command if so.
        if (cur_command->demanded) {
            //std::cerr << "  Demanded." << std::endl;
            bool ready = true;
            for (size_t command_index = cur_command->id; command_index <= cur_command->id + cur_command->num_descendants; command_index++) {
                auto test_command = commands[command_index];
                for (auto in : test_command->inputs) {
                    if (in->status == UNKNOWN && (in->writer_id < cur_command->id || in->writer_id > cur_command->id + cur_command->num_descendants)) {
                        //std::cerr << "  " << in->path << " is not ready." << std::endl;
                        ready = false;
                        break;
                    }
                }
                if (!ready) {
                    break;
                }
            }

            if (ready) {
                //std::cerr << "  Ready." << std::endl;
                // if the command is ready to run and one of it's dependencies has changed, rerun it
                std::cout << cur_command->executable;
                for (size_t arg_index = 1; arg_index < cur_command->args.size(); arg_index++) {
                    std::cout << " " << cur_command->args[arg_index];
                }
                std::cout << std::endl;
                if (dry_run) {
                    // mark its outputs as changed
                    simulate_run(commands, cur_command);
                } else {
                    // Spawn the child
                    pid_t child_pid;
                    char* child_argv[cur_command->args.size() + 1];
                    for (size_t arg_index = 0; arg_index < cur_command->args.size(); arg_index++) {
                        child_argv[arg_index] = strdup(cur_command->args[arg_index].c_str());
                    }
                    child_argv[cur_command->args.size()] = nullptr;
                    posix_spawn(&child_pid, cur_command->executable.c_str(), nullptr, nullptr, child_argv, environ);
                    for (size_t arg_index = 0; arg_index < cur_command->args.size(); arg_index++) {
                        free(child_argv[arg_index]);
                    }

                    // Wait for completion
                    // TODO: Consider detecting errors?
                    waitpid(child_pid, nullptr, 0);

                    // Mark all outputs appropriately
                    for (size_t command_index = cur_command->id; command_index <= cur_command->id + cur_command->num_descendants; command_index++) {
                        auto finished_command = commands[command_index];
                        finished_command->rerun = true;
                        for (auto out : finished_command->outputs) {
                            if (use_fingerprints && match_fingerprint(db_graph.getFiles()[out->id])) {
                                out->status = UNCHANGED;
                            } else {
                                out->status = CHANGED;
                                for (auto reader : out->readers) {
                                    propagate_command_demand(commands, reader, true);
                                }
                            }
                        }
                    }
                }
            } else {
                //std::cerr << "  Not ready." << std::endl;
                worklist.push(cur_command);
            }
        } else {
            bool ready = (cur_command->output_antidemands_left == 0);
            if (ready) {
                propagate_command_demand(commands, cur_command, false);

                auto check_inputs = [&](db_command* command, bool leaf) {
                    for (auto in : command->inputs) {
                        if (in->status == UNKNOWN && (in->writer_id < cur_command->id || in->writer_id > cur_command->id + cur_command->num_descendants)) {
                            //std::cerr << "  " << in->path << " is not ready." << std::endl;
                            ready = false;
                            break;
                        }
                    }
                };
                cluster_for_each(commands, cur_command, check_inputs);
            }
            if (ready) {
                // Step 2: If not demanded, descend if ready.
                //std::cerr << "  Descending." << std::endl;
                auto descend = [&](db_command* command, bool leaf) {
                    // if all inputs are unchanged, mark outputs as unchanged (unless they are explicitly marked as changed in the dryrun)
                    for (auto out : command->outputs) {
                        out->status = UNCHANGED;
                    }

                    if (leaf) {
                        // add command's direct children to worklist
                        unsigned int current_id = command->id + 1;
                        unsigned int end_id = current_id + command->num_descendants;
                        while (current_id < end_id) {
                            worklist.push(commands[current_id]);
                            current_id += commands[current_id]->num_descendants + 1;
                        }
                    }
                };
                cluster_for_each(commands, cur_command, descend);
            } else {
                // Step 3: Otherwise, this command is not ready. Try a different command.
                //std::cerr << "  Not ready (antidemands left = " << cur_command->output_antidemands_left << ")." << std::endl;
                worklist.push(cur_command);
            }
        }
    }

    Graph graph;
    graph.start_graph();
    draw_graph_nodes(&graph, show_sysfiles, commands, files, db_graph.getCommands().size(), db_graph.getFiles().size());
    draw_graph_edges(&graph, show_sysfiles, commands, files, 0, db_graph.getCommands().size());
    graph.close_graph();
}
