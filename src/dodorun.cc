#include "dodorun.h"
#include "graph.h"
#include "fingerprint.h"
#include "db.capnp.h"

#include <stdio.h>
#include <stdint.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <spawn.h>
#include <unistd.h>

#include <limits>
#include <list>
#include <vector>
#include <set>
#include <map>
#include <queue>

#include <capnp/message.h>
#include <capnp/serialize.h>

extern char** environ;

#define UNCHANGED 0
#define CHANGED   1
#define UNKNOWN   2

bool db_file::is_local(void) {
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

static void draw_graph_nodes(Graph* graph, bool show_collapsed, bool show_sysfiles, db_command* commands[], db_file* files[], size_t command_count, size_t file_count) {
    for (size_t command_id = 0; command_id < command_count; command_id++) {
        if (!show_collapsed && commands[command_id]->collapse_with_parent) {
            continue;
        }

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
        size_t slash_loc = label.rfind('/');
        if (slash_loc != std::string::npos) {
            label = label.substr(slash_loc + 1);
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

static void draw_graph_command_edges(Graph* graph, bool show_collapsed, bool show_sysfiles, db_command* commands[], db_file* files[], size_t parent_id, size_t start_id, size_t end_id) {
    size_t id = start_id;
    while (id < end_id) {
        auto root = commands[id];
        std::string root_node;
        if (show_collapsed) {
            root_node = "c" + std::to_string(id);
        } else {
            root_node = "c" + std::to_string(root->cluster_root->id);
        }

        if (parent_id != std::numeric_limits<size_t>::max() && (show_collapsed || !root->collapse_with_parent)) {
            graph->add_edge("c" + std::to_string(parent_id), root_node, "style=dashed");
        }

        for (auto i : root->inputs) {
            if (show_sysfiles || i->is_local()) {
                graph->add_edge("f" + std::to_string(i->id), root_node, "arrowhead=empty");
            }
        }

        for (auto o : root->outputs) {
            if (show_sysfiles || o->is_local()) {
                graph->add_edge(root_node, "f" + std::to_string(o->id), "arrowhead=empty");
            }
        }

        for (auto d : root->deletions) {
            if (show_sysfiles || d->is_local()) {
                graph->add_edge(root_node, "f" + std::to_string(d->id), "color=red arrowhead=empty");
            }
        }

        for (auto c : root->creations) {
            if (show_sysfiles || c->is_local()) {
                graph->add_edge(root_node, "f" + std::to_string(c->id), "color=blue arrowhead=empty");
            }
        }

        size_t children_start = id + 1;
        size_t children_end = children_start + root->num_descendants;
        size_t children_parent;
        if (show_collapsed || !root->collapse_with_parent) {
            children_parent = id;
        } else {
            children_parent = parent_id;
        }
        draw_graph_command_edges(graph, show_collapsed, show_sysfiles, commands, files, children_parent, children_start, children_end);

        id = children_end;
    }
}

static void draw_graph_modification_edges(Graph* graph, bool show_sysfiles, db_file* files[], size_t file_count) {
    for (size_t file_id = 0; file_id < file_count; file_id++) {
        if (files[file_id]->prev_version != nullptr && (show_sysfiles || files[file_id]->is_local())) {
            graph->add_edge("f" + std::to_string(files[file_id]->prev_version->id), "f" + std::to_string(file_id), "color=orchid arrowhead=empty");
        }
    }
}

// Escape a string for correct printing as an argument or command on the shell
static void write_shell_escaped(std::ostream& out_stream, const std::string& input) {
    if (input.find_first_of(" \t\n&();|<>!{}'\"") == std::string::npos &&
        input != std::string("elif") &&
        input != std::string("fi") &&
        input != std::string("while") &&
        input != std::string("case") &&
        input != std::string("else") &&
        input != std::string("for") &&
        input != std::string("then") &&
        input != std::string("do") &&
        input != std::string("done") &&
        input != std::string("until") &&
        input != std::string("if") &&
        input != std::string("esac")) {
        out_stream << input;
        return;
    }

    out_stream << '\'';
    size_t escaped_so_far = 0;
    while (true) {
        size_t quote_offset = input.find('\'', escaped_so_far);
        if (quote_offset == std::string::npos) {
            out_stream.write(input.data() + escaped_so_far, input.size() - escaped_so_far);
            out_stream << '\'';
            return;
        } else {
            out_stream.write(input.data() + escaped_so_far, quote_offset - escaped_so_far);
            out_stream << "'\\''";
            escaped_so_far = quote_offset + 1;
        }
    }
}

static void mark_complete_for_deletions(db_command* command, bool dry_run) {
    for (auto in : command->inputs) {
        if (in->is_pipe) {
            // Pipes are not in the filesystem and not deleted
            continue;
        }

        in->readers_complete += 1;
        if (in->scheduled_for_deletion && in->readers_complete == in->readers.size()) {
            bool scheduled_for_creation = in->scheduled_for_creation;
            db_file* early_version = in;
            while (early_version->prev_version != nullptr) {
                early_version = early_version->prev_version;
                if (early_version->scheduled_for_creation) {
                    scheduled_for_creation = true;
                    break;
                }
            }
            if (scheduled_for_creation) {
                continue;
            }

            std::cout << "rm ";
            write_shell_escaped(std::cout, in->path);
            std::cout << std::endl;
            if (!dry_run) {
                unlink(in->path.c_str());
            }
            in->scheduled_for_deletion = false; // Don't delete twice
        }
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

RebuildState::RebuildState(int db_fd, bool use_fingerprints, std::set<std::string> const& explicitly_changed, std::set<std::string> const& explicitly_unchanged) : message(db_fd), db_graph(message.getRoot<db::Graph>()) {
    // reconstruct the graph
    size_t files_size = this->db_graph.getFiles().size();
    size_t commands_size = this->db_graph.getCommands().size();

    // initialize array of files
    this->files = new db_file*[files_size];
    unsigned int file_id = 0;
    for (auto file : this->db_graph.getFiles()) {
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

            if (explicitly_changed.find(path) != explicitly_changed.end()) {
                flag = CHANGED;
            } else if (explicitly_unchanged.find(path) != explicitly_unchanged.end()) {
                flag = UNCHANGED;
            }
        }
        bool is_cached = file.getLatestVersion() && !is_pipe;
        this->files[file_id] = new db_file(file_id, is_pipe, is_cached, path, flag);
        file_id++;

        //if (flag == CHANGED) {
            //std::cerr << path << " is changed" << std::endl;
        //}
    }

    // initialize array of commands
    this->commands = new db_command*[db_graph.getCommands().size()];
    unsigned int cmd_id = 0;
    for (auto cmd : this->db_graph.getCommands()) {
        auto executable = std::string((const char*) cmd.getExecutable().begin(), cmd.getExecutable().size());
        this->commands[cmd_id] = new db_command(cmd_id, cmd.getDescendants(), executable);
        for (auto arg : cmd.getArgv()) {
            this->commands[cmd_id]->args.push_back(std::string((const char*) arg.begin(), arg.size()));
        }
        this->commands[cmd_id]->collapse_with_parent = cmd.getCollapseWithParent();
        cmd_id++;
    }

    // Add the dependencies
    for (auto dep : this->db_graph.getInputs()) {
        this->files[dep.getInputID()]->readers.insert(this->commands[dep.getOutputID()]);
        this->commands[dep.getOutputID()]->inputs.insert(this->files[dep.getInputID()]);
    }
    for (auto dep : this->db_graph.getOutputs()) {
        db_file* file = this->files[dep.getOutputID()];
        file->writer_id = dep.getInputID();
        this->commands[file->writer_id]->outputs.insert(file);
    }
    for (auto dep : this->db_graph.getRemovals()) {
        this->commands[dep.getInputID()]->deletions.insert(this->files[dep.getOutputID()]);
    }
    for (auto dep : this->db_graph.getCreations()) {
        this->commands[dep.getInputID()]->creations.insert(this->files[dep.getOutputID()]);
    }
    for (auto dep : this->db_graph.getModifications()) {
        this->files[dep.getOutputID()]->prev_version = this->files[dep.getInputID()];
    }

    // Collapsing codependencies and cycles
    // TODO: This probably wants to be part of the serialized representation, not here
    std::queue<db_command*> collapse_worklist;
    for (size_t root_id = 0; root_id < commands_size; root_id++) {
        // If this command has already been included in another cluster, then its dependencies
        // have already been considered, so move on.
        if (this->commands[root_id]->collapse_with_parent) {
            continue;
        }

        // At this point, we know that we are the root of a cluster and need to determine what other
        // commands are in the cluster. These commands can be forced into the cluster when an input
        // of the cluster is something else descended from the cluster, i.e. from the cluster's
        // root, i.e. from the current command. Alternatively, if an output of the cluster is
        // not cached and used by a descendant, we also need to collapse.
        collapse_worklist.push(this->commands[root_id]);
        while (!collapse_worklist.empty()) {
            auto to_collapse = collapse_worklist.front();
            collapse_worklist.pop();

            auto collapse_linear = [&](size_t descendant_id) {
                size_t parent_id = root_id;
                while (parent_id != descendant_id) {
                    // Loop through children to find the ancestor of the writer
                    size_t current_id = parent_id + 1;
                    size_t end_id = current_id + this->commands[parent_id]->num_descendants;
                    while (current_id < end_id) {
                        size_t next_id = current_id + this->commands[current_id]->num_descendants + 1;
                        if (next_id > descendant_id) {
                            // Success: mark and move on to the next level
                            if (!this->commands[current_id]->collapse_with_parent) {
                                this->commands[current_id]->collapse_with_parent = true;
                                collapse_worklist.push(this->commands[current_id]);
                            }
                            parent_id = current_id;
                            break;
                        }
                        current_id = next_id;
                    }
                }
            };
            for (auto out : to_collapse->outputs) {
                if (!out->is_cached) {
                    for (auto reader : out->readers) {
                        if (reader->id > root_id && reader->id <= root_id + this->commands[root_id]->num_descendants) {
                            collapse_linear(reader->id);
                        }
                    }
                }
            }
        }
    }
}

void RebuildState::rebuild(bool use_fingerprints, bool dry_run, size_t parallel_jobs) {
    size_t files_size = this->db_graph.getFiles().size();
    size_t commands_size = this->db_graph.getCommands().size();

    // Set up "pipe clusters" as connected components through pipes
    std::vector<size_t> pipe_cluster_blocked;
    std::queue<db_command*> pipe_cluster_worklist;
    for (size_t command_id = 0; command_id < commands_size; command_id++) {
        if (this->commands[command_id]->pipe_cluster != std::numeric_limits<size_t>::max()) {
            // We have already assigned this to a cluster
            continue;
        }

        size_t pipe_cluster = pipe_cluster_blocked.size();
        pipe_cluster_blocked.push_back(0);

        pipe_cluster_worklist.push(this->commands[command_id]);
        while (!pipe_cluster_worklist.empty()) {
            auto to_include = pipe_cluster_worklist.front();
            pipe_cluster_worklist.pop();

            if (to_include->pipe_cluster == std::numeric_limits<size_t>::max()) {
                to_include->pipe_cluster = pipe_cluster;
                pipe_cluster_blocked[pipe_cluster] += 1;
                for (auto in : to_include->inputs) {
                    if (in->is_pipe && in->writer_id != std::numeric_limits<size_t>::max()) {
                        pipe_cluster_worklist.push(this->commands[in->writer_id]);
                    }
                }
                for (auto out : to_include->outputs) {
                    if (out->is_pipe) {
                        for (auto reader : out->readers) {
                            pipe_cluster_worklist.push(reader);
                        }
                    }
                }
            }
        }
    }
    // Everything is unlaunched
    std::vector<size_t> pipe_cluster_unlaunched = pipe_cluster_blocked;

    for (size_t command_id = 0; command_id < commands_size; command_id++) {
        // Initialize pipe reference counts
        for (auto initial_fd_entry : this->db_graph.getCommands()[command_id].getInitialFDs()) {
            auto file = this->files[initial_fd_entry.getFileID()];
            if (file->is_pipe) {
                if (initial_fd_entry.getCanRead()) {
                    file->pipe_reader_references += 1;
                } else {
                    file->pipe_writer_references += 1;
                }
            }
        }

        if (this->commands[command_id]->collapse_with_parent) {
            //std::cerr << "Command " << this->commands[command_id]->executable << " is collapsed with parent." << std::endl;
            continue;
        }

        // Map each command to its cluster root
        auto set_cluster_root = [&](db_command* cluster_member, bool leaf) {
            cluster_member->cluster_root = this->commands[command_id];
        };
        cluster_for_each(this->commands, this->commands[command_id], set_cluster_root);
    }

    // Whenever we scan through the graph, we wish to not double-count nodes, so we have a
    // "generation" for each scan that is unique. Whenever we touch a node, we mark that node as
    // last touched in the current generation so that if we see it again, we will recognize it
    // as already touched.
    size_t current_generation = 0;
    auto ignore_callback = [](auto ignored) {};

    for (size_t command_id = 0; command_id < commands_size; command_id++) {
        if (this->commands[command_id]->collapse_with_parent) {
            //std::cerr << "Command " << this->commands[command_id]->executable << " is collapsed with parent." << std::endl;
            continue;
        }

        //std::cerr << "Tallying inputs to " << this->commands[command_id]->executable << std::endl;

        // Count cluster inputs
        ssize_t cluster_inputs = 0;
        auto tally_inputs = [&](db_command* cluster_member, bool leaf) {
            for (auto in : cluster_member->inputs) {
                // Ignore in-cluster edges
                if (command_id > in->writer_id || in->writer_id > command_id + this->commands[command_id]->num_descendants) {
                    cluster_inputs += 1;
                    if (!in->is_cached && in->writer_id != std::numeric_limits<size_t>::max()) {
                        // We will later on overcount by including this edge in the counts for
                        // the commands leading into this edge. Correct for that.
                        adjust_refcounts(this->commands[in->writer_id], -1, this->commands, &current_generation, ignore_callback);
                    }
                }
            }
        };
        cluster_for_each(this->commands, this->commands[command_id], tally_inputs);

        // Adjust reference counts to account for those inputs
        // We start at one to record the spawns-child input from our parent
        adjust_refcounts(this->commands[command_id], cluster_inputs + 1, this->commands, &current_generation, ignore_callback);
    }

    pid_t dry_run_pid = 1; // A fake PID for use if we are doing dry run;

    std::queue<db_command*> propagate_rerun_worklist;
    std::queue<db_command*> descend_to_worklist;
    std::queue<db_command*> run_worklist;
    std::queue<db_command*> zero_reference_worklist;
    std::map<pid_t, db_command*> wait_worklist;
    size_t blocked_processes = 0;

    // For a callback on adjust_refcounts
    auto add_to_worklist = [&](db_command* node) {
        //std::cerr << node->executable << " will not run" << std::endl;
        zero_reference_worklist.push(node);
    };

    // Initialize the worklists
    //TODO multiple roots
    descend_to_worklist.push(this->commands[0]);
    for (size_t file_id = 0; file_id < files_size; file_id++) {
        if (this->files[file_id]->writer_id == std::numeric_limits<size_t>::max()) {
            if (this->files[file_id]->status == CHANGED) {
                for (auto reader : this->files[file_id]->readers) {
                    propagate_rerun_worklist.push(reader);
                }
            } else if (this->files[file_id]->status == UNCHANGED) {
                //std::cerr << this->files[file_id]->path << " is unchanged" << std::endl;
                for (auto reader : this->files[file_id]->readers) {
                    //std::cerr << "Decrementing ID " << reader->id << " (root " << reader->cluster_root->id << ")" << std::endl;
                    adjust_refcounts(reader, -1, this->commands, &current_generation, ignore_callback);
                }
            }
        } else {
            if (this->files[file_id]->status != CHANGED) {
                this->files[file_id]->status = UNKNOWN;
            } else {
                // Rerun the commands that produce changed files
                propagate_rerun_worklist.push(this->commands[this->files[file_id]->writer_id]);
            }
        }
    }

    while (true) {
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
                if (this->commands[current_id]->rerun) { // we've already propagated this subtree
                    current_id += this->commands[current_id]->num_descendants + 1;
                    continue;
                }

                this->commands[current_id]->rerun = true;
                for (auto in : this->commands[current_id]->inputs) {
                    if (!in->is_cached && in->writer_id != std::numeric_limits<size_t>::max()) {
                        propagate_rerun_worklist.push(this->commands[in->writer_id]);
                    }
                }
                // If we don't have fingerprints for an output (such as with a pipe), we know
                // immediately that whatever consumers there are will also need to rerun.
                for (auto out : this->commands[current_id]->outputs) {
                    if (this->db_graph.getFiles()[out->id].getFingerprintType() == db::FingerprintType::UNAVAILABLE) {
                        for (auto reader : out->readers) {
                            propagate_rerun_worklist.push(reader);
                        }
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
                adjust_refcounts(cur_command, -1, this->commands, &current_generation, add_to_worklist);
                if (cur_command->references_remaining != 0) {
                    // Mark the command so that if we propagate a rerun here, we will add it
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
                        if (reader->cluster_root == node) {
                            //std::cerr << "Skipping in-cluster edge" << std::endl;
                        } else {
                            adjust_refcounts(reader, -1, this->commands, &current_generation, add_to_worklist);
                        }
                    }
                }

                // Mark files for deletion that this command would delete
                for (auto cre : cluster_member->creations) {
                    // TODO: Actually create files. This probably should happen when
                    // running a command that needs the file already open.
                    cre->scheduled_for_creation = true;
                }
                for (auto del : cluster_member->deletions) {
                    del->scheduled_for_deletion = true;
                }

                // Mark ourselves as started yet unblocked
                pipe_cluster_unlaunched[cluster_member->pipe_cluster] -= 1;
                pipe_cluster_blocked[cluster_member->pipe_cluster] -= 1;
                if (pipe_cluster_unlaunched[cluster_member->pipe_cluster] == 0) {
                    blocked_processes -= pipe_cluster_blocked[cluster_member->pipe_cluster];
                }

                // Descend to children
                if (leaf) {
                    size_t current_id = cluster_member->id + 1;
                    size_t end_id = current_id + cluster_member->num_descendants;
                    while (current_id < end_id) {
                        descend_to_worklist.push(this->commands[current_id]);
                        current_id += this->commands[current_id]->num_descendants + 1;
                    }
                }
            };
            cluster_for_each(this->commands, node, descend);
            // We have to do this in a second pass so everything gets appropriately
            // marked for creation/deletion
            auto mark = [&](db_command* cluster_member, bool leaf) {
                mark_complete_for_deletions(cluster_member, dry_run);
            };
            cluster_for_each(this->commands, node, mark);

            continue;
        }

        // Find something available to run.
        // TODO: Heuristics about which jobs to run to maximize parallelism
        // TODO: Schedule around conflicting writes to a file
        db_command* run_command;
        bool ready = false;
        if (!run_worklist.empty() && wait_worklist.size() < parallel_jobs + blocked_processes) {
            // Loop until we find something to run
            size_t searched = 0;
            do {
                run_command = run_worklist.front();
                run_worklist.pop();

                ready = true;
                for (size_t command_index = run_command->id; command_index <= run_command->id + run_command->num_descendants; command_index++) {
                    auto test_command = this->commands[command_index];
                    for (auto in : test_command->inputs) {
                        if (!in->is_pipe && in->status == UNKNOWN && (in->writer_id < run_command->id || in->writer_id > run_command->id + run_command->num_descendants)) {
                            //std::cerr << "  " << in->path << " is not ready." << std::endl;
                            ready = false;
                            break;
                        }
                    }
                    // run through outputs to check for conflicting writes 
                    for (auto out : test_command->outputs) {
                        //std::cerr << "Checking for conflicts to: " << out->path << std::endl;
                        // check whether the file can conflict 
                        // if one of its conflicts are active, then this command is not ready 
                        if (out->is_pipe) {
                            continue;
                        }
                        for (ssize_t id = out->id - 1; id >= 0; id--) {
                            // conflicting file
                            if (this->files[id]->path == this->files[out->id]->path) {
                                if (this->files[id]->active) {
                                    ready = false;
                                    break;
                                }
                            } else {
                                break;
                            }
                        }
                        for (size_t id = out->id + 1; id < files_size; id++) {
                            // conflicting file
                            if (this->files[id]->path == this->files[out->id]->path) {
                                if (this->files[id]->active) {
                                    ready = false;
                                    break;
                                }
                            } else {
                                break;
                            }
                        }
                    }
                    if (!ready) {
                        run_worklist.push(run_command);
                        searched++;
                        break;
                    }
                }
            } while (!ready && searched < run_worklist.size());
        }

        // If we found something to run, run it
        if (ready) {
            // Print that we will run it
            write_shell_escaped(std::cout, run_command->executable);
            for (size_t arg_index = 1; arg_index < run_command->args.size(); arg_index++) {
                std::cout << " ";
                write_shell_escaped(std::cout, run_command->args[arg_index]);
            }
            // Print redirections
            for (auto initial_fd_entry : this->db_graph.getCommands()[run_command->id].getInitialFDs()) {
                std::cout << " ";
                if   (!(initial_fd_entry.getFd() == fileno(stdin) && initial_fd_entry.getCanRead() && !initial_fd_entry.getCanWrite())
                   && !(initial_fd_entry.getFd() == fileno(stdout) && !initial_fd_entry.getCanRead() && initial_fd_entry.getCanWrite())) {
                    std::cout << initial_fd_entry.getFd();
                }
                if (initial_fd_entry.getCanRead()) {
                    std::cout << '<';
                }
                if (initial_fd_entry.getCanWrite()) {
                    std::cout << '>';
                }
                if (this->files[initial_fd_entry.getFileID()]->is_pipe) {
                    std::cout << "/proc/dodo/pipes/" << initial_fd_entry.getFileID();
                } else {
                    write_shell_escaped(std::cout, this->files[initial_fd_entry.getFileID()]->path);
                }
            }
            std::cout << std::endl;

            // indicate that its outputs are active 
            for (auto out : run_command->outputs) {
                out->active = true;
            }

            // Run it!
            pid_t child_pid;
            if (dry_run) {
                child_pid = dry_run_pid;
                dry_run_pid++;
            } else {
                // Set up argv
                char* child_argv[run_command->args.size() + 1];
                for (size_t arg_index = 0; arg_index < run_command->args.size(); arg_index++) {
                    child_argv[arg_index] = strdup(run_command->args[arg_index].c_str());
                }
                child_argv[run_command->args.size()] = nullptr;
                // Set up initial fds
                posix_spawn_file_actions_t file_actions;
                if (posix_spawn_file_actions_init(&file_actions) != 0) {
                    perror("Failed to init file actions");
                    exit(2);
                }
                std::vector<int> opened_fds;
                int max_fd = 0;
                for (auto initial_fd_entry : this->db_graph.getCommands()[run_command->id].getInitialFDs()) {
                    int fd = initial_fd_entry.getFd();
                    if (fd > max_fd) {
                        max_fd = fd;
                    }
                }
                for (auto initial_fd_entry : this->db_graph.getCommands()[run_command->id].getInitialFDs()) {
                    auto file = this->files[initial_fd_entry.getFileID()];
                    int open_fd_storage;
                    int* open_fd_ref;
                    if (file->is_pipe) {
                        if (file->scheduled_for_creation) {
                            file->scheduled_for_creation = false;
                            int pipe_fds[2];
                            // FIXME(portability): pipe2 is Linux-specific; fall back to pipe+fcntl?
                            if (pipe2(pipe_fds, O_CLOEXEC) != 0) {
                                perror("Failed to create pipe");
                                continue;
                            }
                            file->pipe_reader_fd = pipe_fds[0];
                            file->pipe_writer_fd = pipe_fds[1];
                        }

                        if (initial_fd_entry.getCanRead()) {
                            open_fd_ref = &file->pipe_reader_fd;
                        } else { // TODO: check for invalid read/write combinations?
                            open_fd_ref = &file->pipe_writer_fd;
                        }
                    } else {
                        int flags = O_CLOEXEC;
                        if (initial_fd_entry.getCanRead() && initial_fd_entry.getCanWrite()) {
                            flags |= O_RDWR;
                        } else if (initial_fd_entry.getCanWrite()) {
                            flags |= O_WRONLY;
                        } else { // TODO: what if the database has no permissions for some reason?
                            flags |= O_RDONLY;
                        }
                        if (file->scheduled_for_creation) {
                            file->scheduled_for_creation = false;
                            flags |= O_CREAT | O_TRUNC;
                        }
                        mode_t mode = this->db_graph.getFiles()[initial_fd_entry.getFileID()].getMode();
                        open_fd_storage = open(file->path.c_str(), flags, mode);
                        if (open_fd_storage == -1) {
                            perror("Failed to open");
                            continue;
                        }
                        open_fd_ref = &open_fd_storage;
                    }
                    // Ensure that the dup2s won't step on each other's toes
                    if (*open_fd_ref <= max_fd) {
                        int new_fd = fcntl(*open_fd_ref, F_DUPFD_CLOEXEC, max_fd + 1);
                        if (new_fd == -1) {
                            perror("Failed to remap FD");
                            continue;
                        }
                        close(*open_fd_ref);
                        *open_fd_ref = new_fd;
                    }
                    if (!file->is_pipe) {
                        opened_fds.push_back(*open_fd_ref);
                    }
                    if (posix_spawn_file_actions_adddup2(&file_actions, *open_fd_ref, initial_fd_entry.getFd()) != 0) {
                        perror("Failed to add dup2 file action");
                        exit(2);
                    }
                }
                // Spawn the child
                posix_spawn(&child_pid, run_command->executable.c_str(), &file_actions, nullptr, child_argv, environ);
                // Free what we can
                for (size_t arg_index = 0; arg_index < run_command->args.size(); arg_index++) {
                    free(child_argv[arg_index]);
                }
                for (auto open_fd : opened_fds) {
                    close(open_fd);
                }
                posix_spawn_file_actions_destroy(&file_actions);
                for (auto initial_fd_entry : this->db_graph.getCommands()[run_command->id].getInitialFDs()) {
                    auto file = this->files[initial_fd_entry.getFileID()];
                    if (file->is_pipe) {
                        if (initial_fd_entry.getCanRead()) {
                            file->pipe_reader_references -= 1;
                            if (file->pipe_reader_references == 0) {
                                close(file->pipe_reader_fd);
                            }
                        } else {
                            file->pipe_writer_references -= 1;
                            if (file->pipe_writer_references == 0) {
                                close(file->pipe_writer_fd);
                            }
                        }
                    }
                }
            }

            // Mark the child as blocked if it is
            blocked_processes += 1;
            pipe_cluster_unlaunched[run_command->pipe_cluster] -= 1;
            if (pipe_cluster_unlaunched[run_command->pipe_cluster] == 0) {
                blocked_processes -= pipe_cluster_blocked[run_command->pipe_cluster];
            }
            //std::cerr << "Pipe cluster " << run_command->pipe_cluster << " at " << pipe_cluster_unlaunched[run_command->pipe_cluster] << "/" << pipe_cluster_blocked[run_command->pipe_cluster] << std::endl;
            //std::cerr << "  Blocked at " << blocked_processes << std::endl;
            //std::cerr << "  Launched at pid " << child_pid << std::endl;

            wait_worklist[child_pid] = run_command;
            continue;
        }

        if (!wait_worklist.empty()) {
            pid_t child_pid;
            if (dry_run) {
                child_pid = wait_worklist.begin()->first;
            } else {
                child_pid = wait(nullptr);
            }
            //std::cerr << "Waited at pid " << child_pid << std::endl;
            db_command* child_command;
            auto entry = wait_worklist.find(child_pid);
            if (entry == wait_worklist.end()) {
                continue;
            } else {
                child_command = entry->second;
                wait_worklist.erase(entry);
                
            }
            //std::cerr << child_command->executable << " has finished" << std::endl;

            // Finished processes were apparently not blocked
            if (pipe_cluster_unlaunched[child_command->pipe_cluster] > 0) {
                blocked_processes -= 1;
                pipe_cluster_blocked[child_command->pipe_cluster] -= 1;
                //std::cerr << "Pipe cluster " << child_command->pipe_cluster << " at " << pipe_cluster_unlaunched[child_command->pipe_cluster] << "/" << pipe_cluster_blocked[child_command->pipe_cluster] << std::endl;
                //std::cerr << "  Blocked at " << blocked_processes << std::endl;
            }

            // Mark all outputs appropriately
            for (size_t command_index = child_command->id; command_index <= child_command->id + child_command->num_descendants; command_index++) {
                auto finished_command = this->commands[command_index];
                for (auto out : finished_command->outputs) {
                    if (!dry_run && use_fingerprints && match_fingerprint(this->db_graph.getFiles()[out->id])) {
                        out->status = UNCHANGED;
                        out->active = false;
                        for (auto reader : out->readers) {
                            if (reader->cluster_root == child_command) {
                                //std::cerr << "Skipping in-cluster edge" << std::endl;
                            } else {
                                adjust_refcounts(reader, -1, this->commands, &current_generation, add_to_worklist);
                            }
                        }
                    } else {
                        out->status = CHANGED;
                        for (auto reader : out->readers) {
                            propagate_rerun_worklist.push(reader);
                        }
                    }
                }
                mark_complete_for_deletions(finished_command, dry_run);
            }

            
            // if we were this file's last reader, mark it as no longer active
            for (auto in : child_command->inputs) {
                if (in->readers_complete == in->readers.size()) {
                    //std::cerr << "Finished with file: " << in->path << std::endl;
                    in->active = false;
                }
            }

            continue;
        }

    
        if (run_worklist.empty()) {
            break;
        }

        // if we've reached an infinite loop, lazily find a candidate to run and add it to the
        // run_worklist 
        for (size_t command_id = 0; command_id < commands_size; command_id++) {
            if (this->commands[command_id]->candidate_for_run) {
                run_worklist.push(this->commands[command_id]);
                continue;
            }
        }

        std::cerr << "WARNING: Hit infinite loop. Ending build." << std::endl;
        break;
    }
}

void RebuildState::visualize(bool show_sysfiles, bool show_collapsed) {
    Graph graph;
    graph.start_graph();
    draw_graph_nodes(&graph, show_collapsed, show_sysfiles, this->commands, this->files, this->db_graph.getCommands().size(), this->db_graph.getFiles().size());
    draw_graph_command_edges(&graph, show_collapsed, show_sysfiles, this->commands, this->files, std::numeric_limits<size_t>::max(), 0, this->db_graph.getCommands().size());
    draw_graph_modification_edges(&graph, show_sysfiles, this->files, this->db_graph.getFiles().size());
    graph.close_graph();
}
