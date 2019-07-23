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
    std::string path;
    int status;

    db_file(unsigned int id, bool is_pipe, bool is_cached, std::string path, int status) : id(id), is_pipe(is_pipe), is_cached(is_cached), writer_id(std::numeric_limits<size_t>::max()), path(path), status(status) {}
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
    size_t last_reference_generation = 0;
    // Signed because we sometimes do operations out of order
    ssize_t references_remaining = 0;
    bool candidate_for_run = false;
    bool rerun = false;
    bool collapse_with_parent = false;
    db_command* cluster_root;

    db_command(size_t id, size_t num_descendants, std::string executable) : id(id), num_descendants(num_descendants), executable(executable) {}
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

// The broad overview of our algorithm to decide which commands to run is as follows:
// - We consider a parent command.
// - If we know we must run that parent, we run it.
// - If there could never be anything that triggers the parent to run, then we replace
//   the parent with its children and continue recursively.
//
// Both of those conditions can be seen as graph reachability problems. A command must be
// rerun if it is reachable from a changed file via a path that
// - Goes first through a read edge (i.e. a command that reads a changed file must rerun)
// - Then any combination of spawns-child edges and of reversed dependency edges through
//   non-cached files (i.e. if a command needs to rerun, so does its children; also, a command
//   that needs to rerun needs all of its input files available, and if some of those are not
//   cached, then we must run the command that generates those input files)
// This is fairly easy to track efficiently: we can simply store a flag indicating whether
// a command must be rerun and eagerly propagate it to all dependents, descendants, and
// non-cached inputs. Since the set of changed files only ever increases over the course
// of our run as files stop being in the unknown state, we only have to worry about this
// activation mechanism, and not any removal of reachability.
//
// The second condition, a may-trigger relationship, is more complicated. It is the transitive
// closure of the following:
// - Forward dependencies going through unknown or changed files (since a command may change a file
//   and a changed file will cause readers to rerun)
// - Reversed dependencies through non-cached files (since a command may demand its input files
//   be regenerated)
// - Spawns-child edges (since a command will run its children)
// This has two issues. First, due to including both dependencies and reverse dependencies, it
// is likely to have cycles. More trickily, since files can move from unknown to unchanged, the
// reachable portion of the graph actually shrinks over time. Since we are looking to detect
// non-reachability and edges only disappear, this is essentially equivalent to a problem of
// prompt garbage collection. We use a reference counting approach, modified to handle the
// cycles.
//
// Instead of just tracking the number of incoming primitive may-trigger edges for a single
// command (or a cluster of commands to handle codependencies), we track references to any
// command that is recursively dependent through entirely non-cached files. This is similar to
// tracking references into the strongly connected component instead of individual elements, but
// encodes the directionality of dependencies to cleanly continue working if the SCC is broken
// by files turning unchanged and therefore being removed from the (relevant) graph.

// Adjust the reference count for a command, properly propagating the change to producers of
// non-cached files that feed the given command. Calls the given callback on any nodes that
// drop to zero references.
template<typename callback_ty>
void adjust_refcounts(db_command* start, ssize_t adjustment, db_command* commands[], size_t* current_generation, callback_ty& zero_callback) {
    (*current_generation)++; // Grab a new generation for this scan
    std::vector<db_command*> worklist;
    worklist.push_back(start);
    while (!worklist.empty()) {
        db_command* node = worklist.back();
        worklist.pop_back();

        // We are treating clusters as individuals here.
        node = node->cluster_root;
        if (!node->rerun && node->last_reference_generation != *current_generation) {
            node->last_reference_generation = *current_generation;
            node->references_remaining += adjustment;

            //std::cerr << *current_generation << ": Adding " << adjustment << " references to " << node->executable << " (now " << node->references_remaining << ")" << std::endl;

            if (node->references_remaining == 0) {
                zero_callback(node);
            }

            // Propagate to all non-cached inputs
            auto propagate = [&](db_command* cluster_member, bool leaf) {
                for (auto in : cluster_member->inputs) {
                    // Ignore in-cluster edges
                    if (!in->is_cached
                     && (node->id > in->writer_id || in->writer_id > node->id + node->num_descendants)
                     && in->writer_id != std::numeric_limits<size_t>::max()) {
                        worklist.push_back(commands[in->writer_id]);
                    }
                }
            };
            cluster_for_each(commands, node, propagate);
        }
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
        // root, i.e. from the current command. Alternatively, if an output of the cluster is
        // not cached and used by a descendant, we also need to collapse.
        collapse_worklist.push(commands[root_id]);
        while (!collapse_worklist.empty()) {
            auto to_collapse = collapse_worklist.front();
            collapse_worklist.pop();

            auto collapse_linear = [&](size_t descendant_id) {
                size_t parent_id = root_id;
                while (parent_id != descendant_id) {
                    // Loop through children to find the ancestor of the writer
                    size_t current_id = parent_id + 1;
                    size_t end_id = current_id + commands[parent_id]->num_descendants;
                    while (current_id < end_id) {
                        size_t next_id = current_id + commands[current_id]->num_descendants + 1;
                        if (next_id > descendant_id) {
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
            };

            for (auto in : to_collapse->inputs) {
                if (in->writer_id > root_id && in->writer_id <= root_id + commands[root_id]->num_descendants) {
                    // This input is a descendant of the cluster root. Mark it and all
                    // ancestors up to the root as part of the cluster by traversing downwards.
                    collapse_linear(in->writer_id);
                }
            }
            for (auto out : to_collapse->outputs) {
                if (!out->is_cached) {
                    for (auto reader : out->readers) {
                        if (reader->id > root_id && reader->id <= root_id + commands[root_id]->num_descendants) {
                            collapse_linear(reader->id);
                        }
                    }
                }
            }
        }
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
    }

    // Whenever we scan through the graph, we wish to not double-count nodes, so we have a
    // "generation" for each scan that is unique. Whenever we touch a node, we mark that node as
    // last touched in the current generation so that if we see it again, we will recognize it
    // as already touched.
    size_t current_generation = 0;
    auto ignore_callback = [](auto ignored) {};

    for (size_t command_id = 0; command_id < db_graph.getCommands().size(); command_id++) {
        if (commands[command_id]->collapse_with_parent) {
            //std::cerr << "Command " << commands[command_id]->executable << " is collapsed with parent." << std::endl;
            continue;
        }

        // Count cluster inputs
        ssize_t cluster_inputs = 0;
        auto tally_inputs = [&](db_command* cluster_member, bool leaf) {
            for (auto in : cluster_member->inputs) {
                // Ignore in-cluster edges
                if (command_id > in->writer_id || in->writer_id > command_id + commands[command_id]->num_descendants) {
                    cluster_inputs += 1;
                    if (!in->is_cached && in->writer_id != std::numeric_limits<size_t>::max()) {
                        // We will later on overcount by including this edge in the counts for
                        // the commands leading into this edge. Correct for that.
                        adjust_refcounts(commands[in->writer_id], -1, commands, &current_generation, ignore_callback);
                    }
                }
            }
        };
        cluster_for_each(commands, commands[command_id], tally_inputs);

        // Adjust reference counts to account for those inputs
        // We start at one to record the spawns-child input from our parent
        adjust_refcounts(commands[command_id], cluster_inputs + 1, commands, &current_generation, ignore_callback);
    }

    std::queue<db_command*> propagate_rerun_worklist;
    std::queue<db_command*> descend_to_worklist;
    std::queue<db_command*> run_worklist;
    std::queue<db_command*> zero_reference_worklist;

    // For a callback on adjust_refcounts
    auto add_to_worklist = [&](db_command* node) {
        //std::cerr << node->executable << " will not run" << std::endl;
        zero_reference_worklist.push(node);
    };

    // Initialize the worklists
    //TODO multiple roots
    descend_to_worklist.push(commands[0]);
    for (size_t file_id = 0; file_id < db_graph.getFiles().size(); file_id++) {
        if (files[file_id]->status == CHANGED) {
            for (auto reader : files[file_id]->readers) {
                propagate_rerun_worklist.push(reader);
            }
        } else if (files[file_id]->status == UNCHANGED) {
            for (auto reader : files[file_id]->readers) {
                adjust_refcounts(reader, -1, commands, &current_generation, ignore_callback);
            }
        }
    }

    while (!propagate_rerun_worklist.empty() || !descend_to_worklist.empty() || !zero_reference_worklist.empty() || !run_worklist.empty()) {
        // First propagate reruns as far as we can
        if (!propagate_rerun_worklist.empty()) {
            db_command* rerun_root = propagate_rerun_worklist.front()->cluster_root;
            propagate_rerun_worklist.pop();

            if (rerun_root->candidate_for_run && !rerun_root->rerun) {
                run_worklist.push(rerun_root);
            }

            // Mark all descendents and their non-cached inputs
            size_t current_id = rerun_root->id;
            size_t end_id = current_id + 1 + rerun_root->num_descendants;
            while (current_id < end_id) {
                if (commands[current_id]->rerun) { // we've already propagated this subtree
                    current_id += commands[current_id]->num_descendants + 1;
                    continue;
                }

                commands[current_id]->rerun = true;
                for (auto in : commands[current_id]->inputs) {
                    if (!in->is_cached && in->writer_id != std::numeric_limits<size_t>::max()) {
                        propagate_rerun_worklist.push(commands[in->writer_id]);
                    }
                }
                current_id += 1;
            }
            continue;
        }

        // Then, descend as much as possible, queueing up whatever needs to be rerun
        if (!descend_to_worklist.empty()) {
            db_command* cur_command = descend_to_worklist.front();
            descend_to_worklist.pop();

            if (cur_command->rerun) {
                run_worklist.push(cur_command);
            } else {
                //std::cerr << "Removing parent reference from " << cur_command->executable << std::endl;
                // Remove the parent reference
                adjust_refcounts(cur_command, -1, commands, &current_generation, add_to_worklist);
                if (cur_command->references_remaining != 0) {
                    // Mark the command so that if we propogate a rerun here, we will add it
                    // to the run worklist
                    //std::cerr << "  Not ready yet" << std::endl;
                    cur_command->candidate_for_run = true;
                }
            }
            continue;
        }

        // If something hits 0 references, we will never run it, so mark outputs appropriately
        // and descend
        if (!zero_reference_worklist.empty()) {
            db_command* node = zero_reference_worklist.front();
            zero_reference_worklist.pop();

            auto descend = [&](db_command* cluster_member, bool leaf) {
                // Mark and propagate through outputs
                for (auto out : cluster_member->outputs) {
                    out->status = UNCHANGED;
                    //std::cerr << out->path << " will not be rebuilt" << std::endl;
                    for (auto reader : out->readers) {
                        adjust_refcounts(reader, -1, commands, &current_generation, add_to_worklist);
                    }
                }

                // Descend to children
                if (leaf) {
                    size_t current_id = cluster_member->id + 1;
                    size_t end_id = current_id + cluster_member->num_descendants;
                    while (current_id < end_id) {
                        descend_to_worklist.push(commands[current_id]);
                        current_id += commands[current_id]->num_descendants + 1;
                    }
                }
            };
            cluster_for_each(commands, node, descend);

            continue;
        }

        // Finally, find something available to run and run it.
        // TODO: There may be several things available. Run them in parallel by not
        // waiting immediately.
        if (!run_worklist.empty()) {
            // Loop until we find something to run
            db_command* cur_command;
            bool ready;
            do {
                cur_command = run_worklist.front();
                run_worklist.pop();

                ready = true;
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
                        run_worklist.push(cur_command);
                        break;
                    }
                }
            } while (!ready);

            // Print that we will run it
            std::cout << cur_command->executable;
            for (size_t arg_index = 1; arg_index < cur_command->args.size(); arg_index++) {
                std::cout << " " << cur_command->args[arg_index];
            }
            std::cout << std::endl;

            // Run it!
            if (!dry_run) {
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
            }

            // Mark all outputs appropriately
            for (size_t command_index = cur_command->id; command_index <= cur_command->id + cur_command->num_descendants; command_index++) {
                auto finished_command = commands[command_index];
                for (auto out : finished_command->outputs) {
                    if (!dry_run && use_fingerprints && match_fingerprint(db_graph.getFiles()[out->id])) {
                        out->status = UNCHANGED;
                        for (auto reader : out->readers) {
                            adjust_refcounts(reader, -1, commands, &current_generation, add_to_worklist);
                        }
                    } else {
                        out->status = CHANGED;
                        for (auto reader : out->readers) {
                            propagate_rerun_worklist.push(reader);
                        }
                    }
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
