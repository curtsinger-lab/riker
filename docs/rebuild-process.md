# Rebuild Process
TODO: Update thid document. The two-phase process is still relevant, but some of the details of how artifacts and versions are handled have changed.

I'm just jotting down some notes about how the rebuild process works to help me think about how to
structure the phases of the rebuild.

## Loading
At startup, we load the serialized build, which includes the full tree of commands and the IR steps
each command runs. Those steps reference artifact versions, which in turn reference artifacts. An
empty build is just a special case of this: there is a single root command with no steps, which
indicates that it has not yet been run.

## Change Detection
Once the build has been loaded, we need to determine which commands are directly impacted by any
changes to the filesystem. That includes changes to files that commands access, but also any
permission changes, directory modifications, etc. that would result in an observable change to the
command if it was to be rerun.

To detect changes, we run through all the steps of every command, in command-tree order; this may
not match the exact order steps were collected from the trace (e.g. two command may have interleaved
writes to the same file) but as far as I can tell, those cases where this order does not work are
cases where we would need to collapse commands anyway.

So, starting with the first step of the root command, we run each step against a model of the
filesystem. When we encounter a predicate step, such as CONTENTS_MATCH or REFERENCE_RESULT, we can
evaluate the predicate against the modeled filesystem. Reading a file written by another command is
easy to check; the version that was written should be the same instance that the reader expects to
find. When a predicate depends on something that was not created during the build, we go out to the
filesystem and actually check it against the real file contents.

Any command with at least one failed predicate have changed inputs and will need to be rerun. This
check does not yet verify that the final build products are in place, though; if a build produces
some file F, we want to make sure F still exists. If F has changed or been removed, we will need to
restore it. If we have a cached copy of F we could copy it into place, but if we don't have a cached
copy of F then we will need to be sure the command that produces F reruns.

## The Command Dependency Graph
At this stage, we have a small set of commands that absolutely must rerun; either their inputs
changed, or they produce some output we need and cannot restore without rerunning them. We may need
to run additional commands because they either (a) produce non-cached files that are needed, or (b)
consume files that are written by commands we already need to rerun. In the second case, we may
later discover that a command wrote the same data to a file, so the command that reads it may not
need to rerun; this is something we could handle later. The goal is to produce a set of commands
that will run, or at least to over-approximate that set. To do this, we use a command dependency
graph. 

The command dependency graph has a vertex corresponding to each command run in the previous build,
with directed edges between commands. An edge from command A to command B indicates that if command
A has to rerun, command B also needs to run (or at least it _may_ need to run; see below). This
graph is separate from the change detection process, but we currently build the graph at the same
time we walk through the IR steps for each command during the change detection phase. Any time one
command accesses something produced by another command, we create dependency edges. If command A
writes a file F that is read by command B, we create up to two edges. If F is not cached, command B
depends on command A because we need A to produce F before B can run. If F is cached, we can copy in
the cached version of the file instead of running A. Regardless of whether or not we have a cached
copy of F, command A depends on command B because running A may change F, which would require that
we rerun B. During the actual re-execution we may discover that A does not change F and could skip
B, but this is an optimization.

For now, we also create edges from a command to its children; this is because running a command will
always invoke its child commands as well. In the future, we could skip child commands that do not
need to rerun, but this is an unimplemented optimization.

Using the command graph, we are going to build a set of commands that will rerun. We begin by
marking the two sets of commands identified in the change detection phase: commands whose inputs
have changed, and commands that produce some build product that has been removed or modified. We
also mark commands that have never run, although this should only ever happen when we are planning
the execution of a brand new build.

When we mark a command, we also mark along all of the outbound edges in the command dependency
graph. In practice, this means we mark all the command's children, all commands that read its
outputs, and all commands that produce uncached inputs for this command. If we reach a command that
has already been marked, we can stop recusion along that path.

At this point, the set of marked commands includes every command we will need to run, and we are
ready to execute the rebuild.

## Rebuild Execution
Now that we have a set of commands that must rerun, we can begin the actual rebuilding process. I'll
start by describing a naive approach, and point out where it goes wrong.

### Naive Approach
The simplest way to implement rebuilds is to completely skip over commands that do not need to run.
We start at the root command; if it needs to run, execute it and replace its old IR trace with the
new one. If the root command does not need to rerun, recurse to each of its children. This is what
is currently implemented, but it has some issues.

The first problem comes up when dealing with cached artifacts. If we skip a command that does not
need to rerun, some later command may expect to see one or more of the skipped command's outputs. We
need to collect these outputs and put them in place before running commands that use them. Of course
nothing is that simple; a command may write several versions of an artifact at the same path, each
used by a different command. We would need to swap in each of the different cached versions before
running a child command.

Another issue relates to fine-grained dependencies; a command that we are going to rerun might
depend on a file having specific contents, but it could also depend on changes to the directories
along the path to that file that are made by commands we did not need to rerun. We would need to
perform each of these changes *in the correct sequence*, since a command may make a directory
writable, create a file, then remove write permissions before some later command reads the new file.

Finally, this approach makes it difficult to mix old traces with traces from re-executed commands.
If we load in a cached version of an artifact, we need to be sure the command that uses this
artifact uses the same Artifact class instance that was used to place the cached file. My first
attempt at implementing empty file "caching" did not do this, and the result was a confusing series
of failures in successive rebuilds. We end up with one artifact that is created by the old command
that did not rerun, and a separate artifact that corresponds to the file read by the re-executed
command.

To make this approach work, we need to load in cached artifact versions in the same order as the
command that we are skipping created them. We also need a model of the filesystem that allows us to
track which artifacts have been placed at what locations in the filesystem so we know that when a
re-executing command opens a particular file, it is accessing the artifact version written to that
file by some skipped command that appeared earlier in the rebuild. To me, this sounds like a perfect
use case for the trace IR and filesystem emulation we already use for the change detection phase.

### Trace-Based Rebuilds
Instead of the naive approach described above, we should implement rebuilds with an approach that
leverages the IR trace we have for each command: when we skip a command, emulate its effects using
the trace; when we encounter a command that must re-execute, run it in the environment created by
any preceding emulated commands. This ensures we've run all the appropriate steps in the right order
to prepare for commands that will run later, and it makes it much easier for us to connect the
cached artifact versions we write out to the filesystem with the artifact a re-executing command
gets when it accesses the path we wrote the cached artifact to.

When we run a rebuild, we will start at the root command. If the root command must rerun, we'll
execute it and replace its trace with a newly-collected trace. If the root command does not need to
rerun, we can instead walk through its IR trace and emulate the actions it would have performed if
it was re-executed. One way we could do this is to actually perform all of the writes, chmods, etc.
that the command would have performed, although this would be inefficient and difficult to support.
Performing all the I/O of a command is not likely to be faster than running the command in many
cases, which isn't great. A more serious issue is that we would need cached copies of every artifact
version a command creates to do this; we can't write a version to disk if we don't have the data
that would have been written. So while this is an intuitively useful approach, it wouldn't work in
practice. Still, for the sake of simplicity we'll assume this is the method we use for now and
return to address this issue later.

So, assuming we actually perform the actions of any command we are emulating, we still need some
additional information. When an emulated command writes an artifact version to disk we need to know
that this particular path corresponds to the artifact version we wrote previously. So, during re-
execution we have to keep a map from paths to artifact versions. This is useful when we re-execute
a different command that accesses this artifact version; we will correctly record the re-executed
command's dependency on the emulated command's output. Of course, we also need to advance the
version of the artifact if the re-executed command then writes to it. The same data structure we
used to record the cached artifact at a particular path will also need to reflect the new version
of this artifact that was created by the re-executing command. A third command could access this
artifact version later, and we would want to link all of these dependencies correctly.

This process requires a slight change to the way we do tracing and artifact tracking. When a command
performs some action, we need that to be routed through the Rebuild class, whether the command is
being emulated or is actually executing. This will allow us to update our records about where
artifacts reside on the filesystem. I've already moved the code to create and track artifacts over
from the tracer, but more changes are coming.

At this point we are still assuming that an emulated command will actually perform all of its
actions against the real filesystem. For reasons stated above, this is difficult to implement and
would be quite inefficient. Perhaps the best illustration of this inefficiency is in a rebuild where
no commands need to rerun: we would perform all of the I/O of the full build, while producing no new
effects. Instead of actually running all of the actions against the real filesystem, we can instead
perform them against an emulated filesystem. When an emulated command writes an artifact, we can
turn this into a pointer update in a hash map rather than an actual write to disk. Whenever a
command makes a reference to the filesystem, the contents of this emulated filesystem will take
precedence over the actual filesystem; when opening we first look in the emulated view, then fall
back to the filesystem if there is not a hit. When a re-executing command attempts to open a file
found in the emulated filesystem, we must first commit the changes we emulated out to the actual
filesystem. One nice side-effect of this approach is that it allows us to copy in cached artifacts
lazily.

With the right representation for Artifacts and ArtifactVersions, this approach should be fairly
straightforward to implement. We need to know what Artifact resides at each path in the filesystem.
When a command opens a file we have not seen before, we can spin up a new Artifact at that location.
This could even be done during the change detection phase; when we find an artifact in place at the
expected path, we could pre-populate our record of the filesystem with a reference to this artifact.
When we find a changed file at some path, we'll create a new Artifact at that location and an
initial version of that artifact. In this representation, an Artifact is just a pointer to the
latest ArtifactVersion. These Artifact instances exist only during builds, since the IR only cares
about versions and paths, not the Artifact itself. ArtifactVersions will keep `previous` and `next`
pointers so we can move back and forth through the versions of an artifact. In addition to these
pointers, versions will optionally store saved metadata, a fingerprint, and a reference to a cached
copy of the version. Finally, every version will have a `committed` flag, which indicates whether
the version is on disk or simply emulated. Reads from disk or writes by re-executing commands have
this flag set automatically, but versions loaded from the saved build will not have this flag set.
When a re-executing command wants to access an uncommitted version, we will simply ask the version
to commit itself to disk. Some versions can be committed immediately (like writing the entire
contents of a file), while others may require backtracking to the most recent committed version and
performing actions in sequence (such as appending to a file, adding entries to a directory, etc.).
For now we will only use the former method, but we could use this to quickly implement diff-based
file caching rather that storing many copies of a file with small changes between versions.

The current implementation of Artifacts and ArtifactVersions is quite different from this approach,
so I'll be working on making these changes first.

# Changes to Artifacts and Versions
There is a nearly one-to-one relationship between actions in the IR and versions. The two exception
seems to be existing files, whose contents and metadata do not correspond to any build action. Does
it make sense to think of actions themselves as the versions? This actually seems quite reasonable.

Launch actions are a little odd, since they don't manipulate artifacts. Maybe these should not be
included under the Action subtype.

One of the reasons this is appealing relates to fine-grained versioning. Right now, the only actions
we use are SET_CONTENTS and SET_METADATA. These are idempotent operations, which works fine. But
when we add tracing for directory operations, we're going to have finer-grained actions like LINK,
UNLINK, etc. to encode limited changes to the contents of a directory rather than thinking of these
as completely overwriting every entry of the directory. Every one of these new types of Actions will
have a corresponding type of Version, since we'll need to be able to track the same information
about each Version, and the Version will need to be able to commit its changes to the filesystem; in
other words, it must be able to perform the action it represents.

We will still need the Artifact class. When a Process has a file open, we need to be sure it is
able to reach the latest version of that file, even if the changes made to the file were recorded in
some other Process. The Artifact class is really just a wrapper around a pointer to the latest
action performed on an artifact.

Another nice thing about merging Actions and Versions is that it simplifies creating the Command
dependency graph. If a command has the predicate CONTENTS_MATCH(r1, v), where v is some Version,
that version will encode exactly what was done to produce the version, as well as the command that
performed that action. This directly encodes the dependency between commands, although it does miss
any dependencies on manipulations along the path to the given artifact. We'll still need to model
path resolution in a way that captures these kinds of dependencies.