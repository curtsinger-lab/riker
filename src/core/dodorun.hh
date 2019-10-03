#include <cstddef>
#include <limits>
#include <list>
#include <queue>
#include <set>
#include <string>
#include <vector>

#include "db/db.capnp.h"

struct old_command;

using std::list;
using std::numeric_limits;
using std::queue;
using std::set;
using std::string;
using std::vector;

struct old_file {
  size_t id;
  bool is_pipe;
  bool is_cached;
  size_t writer_id;
  set<old_command*> readers;
  string path;
  int status;

  old_file* prev_version = nullptr;

  bool active = false;
  bool scheduled_for_creation = false;
  bool scheduled_for_deletion = false;

  // A reference counting-based solution to promptly delete temporary files that are
  // no longer needed. The scheduled_for_deletion field is set when we descend past a
  // command that would delete this file, and once all readers are complete, we actually
  // delete the file. N.B. this reference counting is completely unrelated to the reference
  // counting used to decide which commands to run.
  size_t readers_complete = 0;

  // When we open pipes, we need to hold their file descriptors open until all their users
  // start, since we can't refer to them by a path in the filesystem. (We could theoretically
  // use named pipes, which would unify some logic, but that would be more expensive, more
  // system-specific, and wouldn't actually save that much tracking since we need to know
  // when the readers of a pipe are launched anyway to determine if a process will be blocked.)
  size_t pipe_writer_references = 0;
  int pipe_writer_fd;
  size_t pipe_reader_references = 0;
  int pipe_reader_fd;

  old_file(unsigned int id, bool is_pipe, bool is_cached, string path, int status) :
      id(id),
      is_pipe(is_pipe),
      is_cached(is_cached),
      writer_id(numeric_limits<size_t>::max()),
      path(path),
      status(status) {}
  bool is_local();
};

struct old_command {
  size_t id;
  size_t num_descendants;
  string executable;
  list<string> args;
  set<old_file*> inputs;
  set<old_file*> outputs;
  set<old_file*> creations;
  set<old_file*> deletions;
  old_command* parent = nullptr;

  // The minimum number of reversed edges (through uncached temporaries) needed to reach this
  // command from the changed root.
  size_t distance = numeric_limits<size_t>::max();
  size_t equal_distance_references = 0;
  bool candidate_for_run = false;
  bool rerun = false;
  bool collapse_with_parent = false;
  old_command* cluster_root;

  // Since pipes have only a limited buffer size, the users of a pipe could block waiting
  // for each other to run. Therefore, if we haven't launched all of the commands connected
  // to a chain of pipes, we shouldn't count any of them towards our job limit; they may be
  // doing nothing, and not launching more commands could even lead to deadlock. Therefore,
  // we categorize commands into "pipe clusters" that all become unblocked simultaneously.
  size_t pipe_cluster = numeric_limits<size_t>::max();

  old_command(size_t id, size_t num_descendants, string executable) :
      id(id),
      num_descendants(num_descendants),
      executable(executable) {}
};

struct RebuildState {
  db::Graph::Reader old_graph;
  old_file** files;
  old_command** commands;

  vector<size_t> pipe_cluster_blocked;
  vector<size_t> pipe_cluster_unlaunched;

  size_t current_generation;

  queue<old_command*> propagate_rerun_worklist;
  queue<old_command*> descend_to_worklist;
  queue<old_command*> run_worklist;
  queue<old_command*> simulate_worklist;
  size_t blocked_processes;
  size_t rerun_candidates;

  RebuildState(db::Graph::Reader old_graph, bool use_fingerprints,
               set<string> const& explicitly_changed, set<string> const& explicitly_unchanged);
  old_command* rebuild(bool use_fingerprints, bool dry_run, size_t running_jobs,
                       size_t parallel_jobs);
  void mark_complete(bool use_fingerprints, bool dry_run, old_command* child_command);
  void visualize(bool show_sysfiles, bool show_collapsed);

 private:
  void remove_edge(size_t distance_through_edge, old_command* destination);
};
