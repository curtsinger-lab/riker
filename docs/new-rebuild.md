# Iterative Rebuild Process
For the iterative build process, we use the following set of command markings:
- Emulate (initial state)
- MayRun
- MustRun

The build will repeatedly loop over the entire build trace. On each iteration, the build will emulate all commands, except those marked MustRun; those will be executed and traced. The trace collected from a MustRun command replaces its old trace steps. Future iterations use this new trace.

When the trace concludes, we loop over all commands and return them to the Emulate marking. Note that for commands that were marked MustRun, they presumably ran and produced new trace data. This new trace replaces all the trace steps from before the command's run, and will be used for future emulation of the command.

We are now going to begin a recursive marking process to plan the next iteration. Markings are propagated using all of the marking rules appropriate to the mark being applied. MustRun is considered a higher-order marking than MayRun, so MayRun markings are overwritten by MustRun markings. When a recursive marking call does not increase the marking level (Emulate to MayRun or MustRun; MayRun to MustRun) the recursion can terminate without propagating any new markings.

We kick off the marking phase by looping over commands and applying the first two rules. This is analagous to marking the roots in a garbage collector.

  1. If a command has at least one failed predicate, we mark it MustRun.
  2. If a the filesystem is missing some un-cached output from a command, we mark the command MustRun. We can establish this by comparing artifacts in the modeled environment to the filesystem.

Next, we have the rules used to propagate markings.

When command C is marked MustRun:
  3. For each command D that produces uncached input V to C: mark D as MustRun
  4. For each command D that produces input V to C: if D is marked MayRun, mark D as MustRun (currently turned off---see notes below)
  5. For each command D that consumes output V from C: if V is cached mark D as MayRun. If not, mark D as MustRun.

When a command C is marked MayRun:
  6. For each command D that produces uncached input V to C: mark D as MayRun.
  7. For each command D that consumes output V from C: mark D as MayRun
  8. For each command D that consumes output V from C: if D is marked MustRun, mark C as MustRun (currently turned off---see notes below)

We should now have a completely marked set of commands. If any commands are marked MayRun, return to the beginning of this process and repeat. If not, the build is complete. Perform the post-build checks and write out the new trace.

---

## Explanation of Marking Rules

### 1. If a command has at least one failed predicate, we mark it MustRun.
The command with the failed predicate directly observed a change, so we know it must rerun.

### 2. If a the filesystem is missing some un-cached output from a command, we mark the command MustRun.
The output left in the environment is a target of the build, and must be updated or replaced. We know the command that produces this output will need to run because the output is not cached.

### 3. For each command D that produces uncached input V to C: mark D as MustRun
Command C has been marked MustRun, and in order to run correctly it needs an output from command D. Marking D ensures that we will execute command C in the correct environment.

### 4. For each command D that produces input V to C: if D is marked MayRun, mark D as MustRun
Command C has been marked MustRun, and we are only going to run it one time; this is a property we want to preserve. If command D produces an input to command C and command D might run on some future iteration, that could potentially cause C to run a second time. To avoid that, we will preemptively mark D as MustRun to make sure C's one and only run has the latest input form command D.

This rule is currently turned off, and builds work fine. We could potentially run a command multiple times, but it doesn't seem to happen in practice. It's also not necessarily bad to do so; our notion of build equivalence should make multiple runs okay. Repeat runs are just an efficiency issue, but enabling this rule forces more commands to run all the time rather than the occasional second run.

### 5. For each command D that consumes output V from C: if V is cached mark D as MayRun. If not, mark D as MustRun.
Command C has been marked MustRun, so it will run on the next iteration. C may produce different outputs than it did last time. Any command D that consumes those outputs could potentially rerun on a future iteration. However, if V is uncached (e.g. it is a pipe write) it is not possible to run D without C, so run D eagerly now.

### 6. For each command D that produces uncached input V to C: mark D as MayRun.
Command C has been marked MayRun. If we might need to run C, then we might need to run any command D that produces its input.

### 7. For each command D that consumes output V from C: mark D as MayRun
Command C has been marked MayRun. If C may run, it may produce different outputs. Any command D that reads them might also need to run.

### 8. For each command D that consumes output V from C: if D is marked MustRun, mark C as MustRun
Command C has been marked MayRun. If command D consumes output from C and must run on the next iteration, C could force a re-run of D on a future iteration. To ensure we run both C and D only once, mark both as MustRun now. This is a counterpart to rule 4, but this one goes into effect when the reader command D is marked MustRun before the writer command C is marked MayRun.

Ths rule is currently turned off. See the note for its counterpart, rule 4.
