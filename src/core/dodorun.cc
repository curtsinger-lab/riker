#include "core/dodorun.hh"

#include <cassert>
#include <iostream>
#include <limits>
#include <queue>
#include <set>
#include <vector>

#include <unistd.h>

#include <capnp/blob.h>
#include <capnp/list.h>

#include "core/File.hh"
#include "db/db.capnp.h"
#include "ui/graphviz.hh"
#include "ui/util.hh"

using std::cerr;
using std::cout;
using std::endl;
using std::numeric_limits;
using std::queue;
using std::set;
using std::string;
using std::swap;
using std::to_string;

extern char** environ;

#define UNCHANGED 0
#define CHANGED 1
#define UNKNOWN 2

bool old_file::is_local() {
  if (path.find("/usr/") != string::npos || path.find("/lib/") != string::npos ||
      path.find("/etc/") != string::npos || path.find("/dev/") != string::npos ||
      path.find("/proc/") != string::npos) {
    return false;
  } else {
    return true;
  }
}

static void draw_graph_nodes(Graphviz* graph, bool show_collapsed, bool show_sysfiles,
                             old_command* commands[], old_file* files[], size_t command_count,
                             size_t file_count) {
  for (size_t command_id = 0; command_id < command_count; command_id++) {
    if (!show_collapsed && commands[command_id]->collapse_with_parent) {
      continue;
    }

    string attr = "";
    if (commands[command_id]->rerun) {
      attr = "style=filled fillcolor=gold";
    }
    string label;
    if (commands[command_id]->args.size() > 0) {
      label = commands[command_id]->args.front();
    } else {
      label = commands[command_id]->executable;
    }
    size_t slash_loc = label.rfind('/');
    if (slash_loc != string::npos) {
      label = label.substr(slash_loc + 1);
    }
    graph->add_node("c" + to_string(command_id), label, attr);
  }

  for (size_t file_id = 0; file_id < file_count; file_id++) {
    if (show_sysfiles || files[file_id]->is_local()) {
      string attr;
      string label;
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
      graph->add_node("f" + to_string(files[file_id]->id), label, attr);
    }
  }
}

static void draw_graph_command_edges(Graphviz* graph, bool show_collapsed, bool show_sysfiles,
                                     old_command* commands[], old_file* files[], size_t parent_id,
                                     size_t start_id, size_t end_id) {
  size_t id = start_id;
  while (id < end_id) {
    auto root = commands[id];
    string root_node;
    if (show_collapsed) {
      root_node = "c" + to_string(id);
    } else {
      root_node = "c" + to_string(root->cluster_root->id);
    }

    if (parent_id != numeric_limits<size_t>::max() &&
        (show_collapsed || !root->collapse_with_parent)) {
      graph->add_edge("c" + to_string(parent_id), root_node, "style=dashed");
    }

    for (auto i : root->inputs) {
      if (show_sysfiles || i->is_local()) {
        graph->add_edge("f" + to_string(i->id), root_node, "arrowhead=empty");
      }
    }

    for (auto o : root->outputs) {
      if (show_sysfiles || o->is_local()) {
        graph->add_edge(root_node, "f" + to_string(o->id), "arrowhead=empty");
      }
    }

    for (auto d : root->deletions) {
      if (show_sysfiles || d->is_local()) {
        graph->add_edge(root_node, "f" + to_string(d->id), "color=red arrowhead=empty");
      }
    }

    for (auto c : root->creations) {
      if (show_sysfiles || c->is_local()) {
        graph->add_edge(root_node, "f" + to_string(c->id), "color=blue arrowhead=empty");
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
    draw_graph_command_edges(graph, show_collapsed, show_sysfiles, commands, files, children_parent,
                             children_start, children_end);

    id = children_end;
  }
}

static void draw_graph_modification_edges(Graphviz* graph, bool show_sysfiles, old_file* files[],
                                          size_t file_count) {
  for (size_t file_id = 0; file_id < file_count; file_id++) {
    if (files[file_id]->prev_version != nullptr && (show_sysfiles || files[file_id]->is_local())) {
      graph->add_edge("f" + to_string(files[file_id]->prev_version->id), "f" + to_string(file_id),
                      "color=orchid arrowhead=empty");
    }
  }
}

static void mark_complete_for_deletions(old_command* command, bool dry_run) {
  for (auto in : command->inputs) {
    if (in->is_pipe) {
      // Pipes are not in the filesystem and not deleted
      continue;
    }

    in->readers_complete += 1;
    if (in->scheduled_for_deletion && in->readers_complete == in->readers.size()) {
      bool scheduled_for_creation = in->scheduled_for_creation;
      old_file* early_version = in;
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

      cout << "rm ";
      write_shell_escaped(cout, in->path);
      cout << endl;
      if (!dry_run) {
        unlink(in->path.c_str());
      }
      in->scheduled_for_deletion = false;  // Don't delete twice
    }
  }
}

// Run a callback on a command and every child or descendant that has been collapsed with that
// command into a single cluster. The callback is also provided with information about whether
// a command is a leaf (i.e. has no children also in the cluster).
template <typename callback_ty>
static void cluster_for_each(old_command* commands[], old_command* parent, callback_ty& callback) {
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
// reachable portion of the graph actually shrinks over time. The combination of these two
// requirements results is a decremental single-source reachability problem. We use an an approach
// inspired by ES-trees, maintaining a shortest-path distance from the single source on every
// vertex and updating it when necessary. To improve performance, we note that a set of edge weights
// can be assigned such that the length of a path is determined entirely by its endpoints and the
// number of backedges that it crosses. Therefore, we only measure distances in numbers of reversed
// edges, and for the acyclic forward component, the algorithm reduces to reference counting.
//
// Marking a file as unchanged is equivalent to removing the edge or edges that pass through it from
// the may-trigger graph. This is what we implement here.
bool on_edge_changed(size_t distance_through_edge, old_command* destination) {
  if (distance_through_edge > destination->distance) {
    return false;
  }
  // Due to the distanced being shortest path distances, distance_through_edge cannot be smaller
  assert(distance_through_edge == destination->distance);

  destination->equal_distance_references -= 1;
  return (destination->equal_distance_references ==
          0);  // Return whether the change need propagation
}

// NB: Never call this on an already-unreachable destination (this prevents weird overflow errors)
void RebuildState::remove_edge(size_t distance_through_edge, old_command* destination) {
  if (on_edge_changed(distance_through_edge, destination)) {
    // Propagate
    queue<old_command*> reevaluate_worklist;
    reevaluate_worklist.push(destination);

    while (!reevaluate_worklist.empty()) {
      old_command* to_reevaluate = reevaluate_worklist.front()->cluster_root;
      reevaluate_worklist.pop();

      // Scan outgoing edges for places that need changes
      auto scan_for_changes = [&](old_command* cluster_member, bool leaf) {
        for (auto out : cluster_member->outputs) {
          for (auto reader : out->readers) {
            auto outgoing_destination = reader->cluster_root;
            // NB: we use the old distance here
            if (outgoing_destination != to_reevaluate &&
                on_edge_changed(to_reevaluate->distance, outgoing_destination)) {
              reevaluate_worklist.push(outgoing_destination);
            }
          }
        }
        for (auto in : cluster_member->inputs) {
          if (!in->is_cached && in->writer_id != numeric_limits<size_t>::max()) {
            auto outgoing_destination = this->commands[in->writer_id]->cluster_root;
            if (outgoing_destination != to_reevaluate &&
                on_edge_changed(to_reevaluate->distance + 1, outgoing_destination)) {
              reevaluate_worklist.push(outgoing_destination);
            }
          }
        }
        if (leaf) {
          size_t current_id = cluster_member->id + 1;
          size_t end_id = current_id + cluster_member->num_descendants;
          while (current_id < end_id) {
            if (on_edge_changed(to_reevaluate->distance, this->commands[current_id])) {
              reevaluate_worklist.push(this->commands[current_id]);
            }
            current_id += this->commands[current_id]->num_descendants + 1;
          }
        }
      };
      cluster_for_each(this->commands, to_reevaluate, scan_for_changes);

      // Scan incoming edges to determine the new distance
      size_t new_distance = numeric_limits<size_t>::max();
      size_t new_references = 0;
      auto on_incoming_edge = [&](size_t incoming_distance) {
        if (incoming_distance < new_distance) {
          new_distance = incoming_distance;
          new_references = 1;
        } else if (incoming_distance == new_distance) {
          new_references += 1;
        }
      };
      auto tally_inputs = [&](old_command* cluster_member, bool leaf) {
        for (auto in : cluster_member->inputs) {
          if (in->writer_id != numeric_limits<size_t>::max()) {
            auto incoming_source = this->commands[in->writer_id]->cluster_root;
            if (incoming_source != to_reevaluate) {
              on_incoming_edge(incoming_source->distance);
            }
          }
        }
        for (auto out : cluster_member->outputs) {
          if (!out->is_cached) {
            for (auto reader : out->readers) {
              auto incoming_source = reader->cluster_root;
              if (incoming_source != to_reevaluate) {
                on_incoming_edge(incoming_source->distance + 1);
              }
            }
          }
        }
      };
      cluster_for_each(this->commands, to_reevaluate, tally_inputs);
      if (to_reevaluate->parent != nullptr) {
        on_incoming_edge(to_reevaluate->parent->cluster_root->distance);
      }
      to_reevaluate->distance = new_distance;
      to_reevaluate->equal_distance_references = new_references;

      // If this fires, we know that this will never run
      if (new_distance == numeric_limits<size_t>::max() && to_reevaluate->candidate_for_run) {
        // cerr << to_reevaluate->executable << " will not run" << endl;
        to_reevaluate->candidate_for_run = false;
        this->rerun_candidates -= 1;
        this->simulate_worklist.push(to_reevaluate);
      }
    }
  }
}

// Recursively initialize parent edges
void initialize_parent_edges(old_command* commands[], old_command* parent, size_t child_start_id,
                             size_t child_end_id) {
  size_t current_id = child_start_id;
  while (current_id < child_end_id) {
    commands[current_id]->parent = parent;
    size_t grandchildren_start = current_id + 1;
    size_t grandchildren_end = grandchildren_start + commands[current_id]->num_descendants;
    initialize_parent_edges(commands, commands[current_id], grandchildren_start, grandchildren_end);
    current_id = grandchildren_end;
  }
}

RebuildState::RebuildState(db::Graph::Reader old_graph, bool use_fingerprints,
                           set<string> const& explicitly_changed,
                           set<string> const& explicitly_unchanged) :
    old_graph(old_graph) {
  // reconstruct the graph
  auto old_files = this->old_graph.getFiles();
  size_t files_size = old_files.size();
  size_t commands_size = this->old_graph.getCommands().size();

  // initialize array of files
  this->files = new old_file*[files_size];
  unsigned int file_id = 0;
  for (auto file : old_files) {
    string path = string((const char*)file.getPath().begin(), file.getPath().size());
    bool is_pipe = (file.getType() == db::FileType::PIPE);
    bool is_cached = file.getLatestVersion() && !is_pipe;
    this->files[file_id] = new old_file(file_id, is_pipe, is_cached, path, UNKNOWN);
    file_id++;
  }

  // initialize array of commands
  this->commands = new old_command*[commands_size];
  unsigned int cmd_id = 0;
  for (auto cmd : this->old_graph.getCommands()) {
    auto executable = string((const char*)cmd.getExecutable().begin(), cmd.getExecutable().size());
    this->commands[cmd_id] = new old_command(cmd_id, cmd.getDescendants(), executable);
    for (auto arg : cmd.getArgv()) {
      this->commands[cmd_id]->args.push_back(string((const char*)arg.begin(), arg.size()));
    }
    this->commands[cmd_id]->collapse_with_parent = cmd.getCollapseWithParent();
    cmd_id++;
  }

  // Initialize parent edges
  initialize_parent_edges(this->commands, nullptr, 0, commands_size);

  // Add the dependencies
  for (auto dep : this->old_graph.getInputs()) {
    this->files[dep.getInputID()]->readers.insert(this->commands[dep.getOutputID()]);
    this->commands[dep.getOutputID()]->inputs.insert(this->files[dep.getInputID()]);
  }
  for (auto dep : this->old_graph.getOutputs()) {
    old_file* file = this->files[dep.getOutputID()];
    file->writer_id = dep.getInputID();
    this->commands[file->writer_id]->outputs.insert(file);
  }
  for (auto dep : this->old_graph.getRemovals()) {
    this->commands[dep.getInputID()]->deletions.insert(this->files[dep.getOutputID()]);
  }
  for (auto dep : this->old_graph.getCreations()) {
    this->commands[dep.getInputID()]->creations.insert(this->files[dep.getOutputID()]);
  }
  for (auto dep : this->old_graph.getModifications()) {
    this->files[dep.getOutputID()]->prev_version = this->files[dep.getInputID()];
  }

  // Collapsing codependencies and cycles
  // TODO: This probably wants to be part of the serialized representation, not here
  queue<old_command*> collapse_worklist;
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
            if (reader->id > root_id &&
                reader->id <= root_id + this->commands[root_id]->num_descendants) {
              collapse_linear(reader->id);
            }
          }
        }
      }
    }
  }

  // Set up "pipe clusters" as connected components through pipes
  queue<old_command*> pipe_cluster_worklist;
  for (size_t command_id = 0; command_id < commands_size; command_id++) {
    if (this->commands[command_id]->pipe_cluster != numeric_limits<size_t>::max()) {
      // We have already assigned this to a cluster
      continue;
    }

    size_t pipe_cluster = this->pipe_cluster_blocked.size();
    this->pipe_cluster_blocked.push_back(0);

    pipe_cluster_worklist.push(this->commands[command_id]);
    while (!pipe_cluster_worklist.empty()) {
      auto to_include = pipe_cluster_worklist.front();
      pipe_cluster_worklist.pop();

      if (to_include->pipe_cluster == numeric_limits<size_t>::max()) {
        to_include->pipe_cluster = pipe_cluster;
        this->pipe_cluster_blocked[pipe_cluster] += 1;
        for (auto in : to_include->inputs) {
          if (in->is_pipe && in->writer_id != numeric_limits<size_t>::max()) {
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
  this->pipe_cluster_unlaunched = this->pipe_cluster_blocked;

  for (size_t command_id = 0; command_id < commands_size; command_id++) {
    // Initialize pipe reference counts
    for (auto initial_fd_entry : this->old_graph.getCommands()[command_id].getInitialFDs()) {
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
      // cerr << "Command " << this->commands[command_id]->executable << " is collapsed with
      // parent." << endl;
      continue;
    }

    // Map each command to its cluster root
    auto set_cluster_root = [&](old_command* cluster_member, bool leaf) {
      cluster_member->cluster_root = this->commands[command_id];
    };
    cluster_for_each(this->commands, this->commands[command_id], set_cluster_root);
  }

  this->blocked_processes = 0;
  this->rerun_candidates = 0;

  // Initialize the worklists
  queue<old_command*> possibly_changed_worklist;  // For determining initial distances
  // TODO multiple roots
  this->descend_to_worklist.push(this->commands[0]);
  for (size_t file_id = 0; file_id < files_size; file_id++) {
    if (this->files[file_id]->is_pipe) {
      continue;
    }

    if (this->files[file_id]->writer_id == numeric_limits<size_t>::max() &&
        !this->files[file_id]->readers.empty()) {
      int flag;

      if (explicitly_changed.find(this->files[file_id]->path) != explicitly_changed.end()) {
        flag = CHANGED;
      } else if (explicitly_unchanged.find(this->files[file_id]->path) !=
                 explicitly_unchanged.end()) {
        flag = UNCHANGED;
      } else if (use_fingerprints) {
        flag = match_fingerprint(old_files[file_id]) ? UNCHANGED : CHANGED;
      } else {
        flag = UNCHANGED;
      }

      this->files[file_id]->status = flag;
      if (flag == CHANGED) {
        // cerr << this->files[file_id]->path << " is changed" << endl;
        for (auto reader : this->files[file_id]->readers) {
          // cerr << "  so " << reader->executable << " will be rerun" << endl;
          this->propagate_rerun_worklist.push(reader);
          possibly_changed_worklist.push(reader);
        }
      }
      // We don't need to mark the edges through unchanged files as removed since these are built
      // inputs, and aren't reachable from anywhere: we just won't include them in the reachable set
      // to begin with.
    } else if (old_files[file_id].getLatestVersion()) {
      if (explicitly_changed.find(this->files[file_id]->path) != explicitly_changed.end() ||
          (use_fingerprints && !match_fingerprint(old_files[file_id]))) {
        // cerr << this->files[file_id]->path << " is changed (output)" << endl;
        // cerr << "  so " << this->commands[this->files[file_id]->writer_id]->executable << "
        // will be rerun" << endl;
        // Rerun the commands that produce changed files
        this->propagate_rerun_worklist.push(this->commands[this->files[file_id]->writer_id]);
        possibly_changed_worklist.push(this->commands[this->files[file_id]->writer_id]);
      }
    }
  }

  // Initialize distanes
  queue<old_command*> next_distance_worklist;
  size_t current_distance = 0;
  while (!possibly_changed_worklist.empty()) {
    while (!possibly_changed_worklist.empty()) {
      auto reachable_command = possibly_changed_worklist.front()->cluster_root;
      possibly_changed_worklist.pop();

      if (current_distance < reachable_command->distance) {
        reachable_command->distance = current_distance;
        reachable_command->equal_distance_references = 1;

        // Expand through outgoing edges
        auto expand = [&](old_command* cluster_member, bool leaf) {
          for (auto out : cluster_member->outputs) {
            for (auto reader : out->readers) {
              if (reader->cluster_root != reachable_command) {
                possibly_changed_worklist.push(reader);
              }
            }
          }
          for (auto in : cluster_member->inputs) {
            if (!in->is_cached && in->writer_id != numeric_limits<size_t>::max()) {
              if (this->commands[in->writer_id]->cluster_root != reachable_command) {
                next_distance_worklist.push(this->commands[in->writer_id]);
              }
            }
          }
          if (leaf) {
            size_t current_id = cluster_member->id + 1;
            size_t end_id = current_id + cluster_member->num_descendants;
            while (current_id < end_id) {
              possibly_changed_worklist.push(this->commands[current_id]);
              current_id += this->commands[current_id]->num_descendants + 1;
            }
          }
        };
        cluster_for_each(this->commands, reachable_command, expand);
      } else if (current_distance == reachable_command->distance) {
        reachable_command->equal_distance_references += 1;
      }
    }

    swap(possibly_changed_worklist, next_distance_worklist);
    current_distance += 1;
  }
}

old_command* RebuildState::rebuild(bool use_fingerprints, bool dry_run, size_t running_jobs,
                                   size_t parallel_jobs) {
  auto old_files = this->old_graph.getFiles();
  size_t files_size = old_files.size();
  size_t commands_size = this->old_graph.getCommands().size();

  while (true) {
    // First propagate reruns as far as we can
    if (!this->propagate_rerun_worklist.empty()) {
      old_command* rerun_root = this->propagate_rerun_worklist.front()->cluster_root;
      this->propagate_rerun_worklist.pop();
      // cerr << rerun_root->executable << " must rerun" << endl;

      if (rerun_root->candidate_for_run && !rerun_root->rerun) {
        this->rerun_candidates -= 1;
        this->run_worklist.push(rerun_root);
      }

      // Mark all descendents and their non-cached inputs
      size_t current_id = rerun_root->id;
      size_t end_id = current_id + 1 + rerun_root->num_descendants;
      while (current_id < end_id) {
        if (this->commands[current_id]->rerun) {  // we've already propagated this subtree
          current_id += this->commands[current_id]->num_descendants + 1;
          continue;
        }

        this->commands[current_id]->rerun = true;
        for (auto in : this->commands[current_id]->inputs) {
          if (!in->is_cached && in->writer_id != numeric_limits<size_t>::max()) {
            this->propagate_rerun_worklist.push(this->commands[in->writer_id]);
          }
        }
        // If we don't have fingerprints for an output (such as with a pipe), we know
        // immediately that whatever consumers there are will also need to rerun.
        for (auto out : this->commands[current_id]->outputs) {
          if (old_files[out->id].getFingerprintType() == db::FingerprintType::UNAVAILABLE) {
            for (auto reader : out->readers) {
              this->propagate_rerun_worklist.push(reader);
            }
          }
        }
        current_id += 1;
      }
      continue;
    }

    // Then, descend as much as possible, queueing up whatever needs to be rerun
    if (!this->descend_to_worklist.empty()) {
      old_command* cur_command = this->descend_to_worklist.front();
      this->descend_to_worklist.pop();

      // cerr << "Removing parent reference from " << cur_command->executable << endl;
      if (cur_command->rerun) {
        // cerr << "  so rerunning" << endl;
        this->run_worklist.push(cur_command);
      } else {
        // We will only descend to a command if its parent will never run, so we don't need to
        // remove the edge TOOO(recognized commands): this might change
        assert(cur_command->parent == nullptr ||
               (!cur_command->parent->rerun &&
                cur_command->parent->cluster_root->distance == numeric_limits<size_t>::max()));

        if (cur_command->distance == numeric_limits<size_t>::max()) {
          this->simulate_worklist.push(cur_command);
        } else {
          // Mark the command so that if we propagate a rerun here, we will add it
          // to the run worklist
          // cerr << "  Not ready yet" << endl;
          cur_command->candidate_for_run = true;
          this->rerun_candidates += 1;
        }
      }
      continue;
    }

    // If something hits infinite distance, we will never run it, so mark outputs appropriately
    // and descend
    if (!this->simulate_worklist.empty()) {
      // Loop until we find something ready to simulate
      size_t searched = 0;
      bool ready = false;
      old_command* node;
      do {
        node = this->simulate_worklist.front();
        this->simulate_worklist.pop();

        // Sanity checks
        assert(!node->rerun);
        assert(node->distance == numeric_limits<size_t>::max());

        ready = true;
        auto check_inputs = [&](old_command* cluster_member, bool leaf) {
          for (auto in : cluster_member->inputs) {
            if (!in->is_pipe && in->status == UNKNOWN &&
                (in->writer_id < node->id || in->writer_id > node->id + node->num_descendants)) {
              // cerr << "  " << in->path << " is not ready." << endl;
              ready = false;
              break;
            }
          }
        };
        cluster_for_each(this->commands, node, check_inputs);

        if (!ready) {
          this->simulate_worklist.push(node);
          searched++;
        }
      } while (!ready && searched < this->simulate_worklist.size());

      auto descend = [&](old_command* cluster_member, bool leaf) {
        // for each input, if we are the last reader, mark the input as inactive
        for (auto in : node->inputs) {
          if (in->readers_complete == in->readers.size() - 1) {
            in->active = false;
          }
        }

        // Mark and propagate through outputs
        for (auto out : cluster_member->outputs) {
          // We don't need to remove the edges through `out` here because we already weren't
          // reachable from the changed root
          out->status = UNCHANGED;
          // cerr << out->path << " will not be rebuilt" << endl;
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
        this->pipe_cluster_unlaunched[cluster_member->pipe_cluster] -= 1;
        this->pipe_cluster_blocked[cluster_member->pipe_cluster] -= 1;
        if (this->pipe_cluster_unlaunched[cluster_member->pipe_cluster] == 0) {
          blocked_processes -= this->pipe_cluster_blocked[cluster_member->pipe_cluster];
        }

        // Descend to children
        if (leaf) {
          size_t current_id = cluster_member->id + 1;
          size_t end_id = current_id + cluster_member->num_descendants;
          while (current_id < end_id) {
            this->descend_to_worklist.push(this->commands[current_id]);
            current_id += this->commands[current_id]->num_descendants + 1;
          }
        }
      };
      if (ready) {
        cluster_for_each(this->commands, node, descend);

        // We have to do this in a second pass so everything gets appropriately
        // marked for creation/deletion
        auto mark = [&](old_command* cluster_member, bool leaf) {
          mark_complete_for_deletions(cluster_member, dry_run);
        };
        cluster_for_each(this->commands, node, mark);

        continue;
      }
    }

    // Find something available to run.
    // TODO: Heuristics about which jobs to run to maximize parallelism
    // TODO: Schedule around conflicting writes to a file
    old_command* run_command;
    bool ready = false;
    if (!this->run_worklist.empty() && running_jobs < parallel_jobs + blocked_processes) {
      // Loop until we find something to run
      size_t searched = 0;
      do {
        run_command = this->run_worklist.front();
        this->run_worklist.pop();

        ready = true;
        for (size_t command_index = run_command->id;
             command_index <= run_command->id + run_command->num_descendants; command_index++) {
          auto test_command = this->commands[command_index];
          for (auto in : test_command->inputs) {
            if (!in->is_pipe && in->status == UNKNOWN &&
                (in->writer_id < run_command->id ||
                 in->writer_id > run_command->id + run_command->num_descendants)) {
              // cerr << "  " << in->path << " is not ready." << endl;
              ready = false;
              break;
            }
          }
          // run through outputs to check for conflicting writes
          for (auto out : test_command->outputs) {
            // cerr << "Checking for conflicts to: " << out->path << endl;
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
            this->run_worklist.push(run_command);
            searched++;
            break;
          }
        }
      } while (!ready && searched < this->run_worklist.size());
    }

    // If we found something to run, run it
    if (ready) {
      // indicate that its outputs are active
      for (auto out : run_command->outputs) {
        out->active = true;
      }

      // Mark the child as blocked if it is
      blocked_processes += 1;
      this->pipe_cluster_unlaunched[run_command->pipe_cluster] -= 1;
      if (this->pipe_cluster_unlaunched[run_command->pipe_cluster] == 0) {
        blocked_processes -= this->pipe_cluster_blocked[run_command->pipe_cluster];
      }
      // cerr << "Pipe cluster " << run_command->pipe_cluster << " at " <<
      // this->pipe_cluster_unlaunched[run_command->pipe_cluster] << "/" <<
      // this->pipe_cluster_blocked[run_command->pipe_cluster] << endl; cerr << "  Blocked
      // at " << blocked_processes << endl;

      return run_command;
    }

    // if we've reached an infinite loop, lazily find a candidate to run and add it to the
    // run_worklist
    if (running_jobs == 0 && (!run_worklist.empty() || this->rerun_candidates != 0)) {
      bool found_fallback = false;
      for (size_t command_id = 0; command_id < commands_size; command_id++) {
        if (this->commands[command_id]->candidate_for_run && !this->commands[command_id]->rerun) {
          // cerr << "Fallback: " << this->commands[command_id]->executable << endl;
          this->propagate_rerun_worklist.push(this->commands[command_id]);
          found_fallback = true;
          break;
        }
      }

      if (found_fallback) {
        continue;
      } else {
        cerr << "WARNING: Hit infinite loop. Ending build." << endl;
        return nullptr;
      }
    }

    return nullptr;
  }
}

void RebuildState::mark_complete(bool use_fingerprints, bool dry_run, old_command* child_command) {
  auto old_files = this->old_graph.getFiles();

  // cerr << child_command->executable << " has finished" << endl;

  // Finished processes were apparently not blocked
  if (this->pipe_cluster_unlaunched[child_command->pipe_cluster] > 0) {
    blocked_processes -= 1;
    this->pipe_cluster_blocked[child_command->pipe_cluster] -= 1;
    // cerr << "Pipe cluster " << child_command->pipe_cluster << " at " <<
    // this->pipe_cluster_unlaunched[child_command->pipe_cluster] << "/" <<
    // this->pipe_cluster_blocked[child_command->pipe_cluster] << endl; cerr << "  Blocked
    // at " << blocked_processes << endl;
  }

  // Mark all outputs appropriately
  for (size_t command_index = child_command->id;
       command_index <= child_command->id + child_command->num_descendants; command_index++) {
    auto finished_command = this->commands[command_index];
    for (auto out : finished_command->outputs) {
      if (!dry_run && use_fingerprints && match_fingerprint(old_files[out->id])) {
        out->status = UNCHANGED;
        if (out->readers_complete == out->readers.size()) {
          out->active = false;
        }
        for (auto reader : out->readers) {
          if (reader->cluster_root->distance != numeric_limits<size_t>::max()) {
            this->remove_edge(finished_command->cluster_root->distance, reader->cluster_root);
          }
        }
      } else {
        out->status = CHANGED;
        for (auto reader : out->readers) {
          this->propagate_rerun_worklist.push(reader);
        }
      }
    }
    mark_complete_for_deletions(finished_command, dry_run);
  }

  // if we were this file's last reader, mark it as no longer active
  for (auto in : child_command->inputs) {
    if (in->readers_complete == in->readers.size()) {
      // cerr << "Finished with file: " << in->path << endl;
      in->active = false;
    }
  }
}

void RebuildState::visualize(bool show_sysfiles, bool show_collapsed) {
  Graphviz graph;
  graph.start_graph();
  draw_graph_nodes(&graph, show_collapsed, show_sysfiles, this->commands, this->files,
                   this->old_graph.getCommands().size(), this->old_graph.getFiles().size());
  draw_graph_command_edges(&graph, show_collapsed, show_sysfiles, this->commands, this->files,
                           numeric_limits<size_t>::max(), 0, this->old_graph.getCommands().size());
  draw_graph_modification_edges(&graph, show_sysfiles, this->files,
                                this->old_graph.getFiles().size());
  graph.close_graph();
}
