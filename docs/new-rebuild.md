# Iterative Rebuild Process
For the iterative build process, we use the following set of command markings:
- Skip
- MayRun
- MustRun
- AlreadyRun
- Unknown (initial state)

The build will repeatedly loop over the entire build trace. On each iteration, the build will emulate all commands, except those marked MustRun; those will be executed and traced. The trace collected from a MustRun command replaces its old trace steps. Future iterations use this new trace.

When the iteration concludes, we loop over all commands. Commands that were marked MustRun were presumably run, so we mark them AlreadyRun. Commands marked MayRun are returned to the Unknown state. The Skip and AlreadyRun markings are left as-is.

Now we loop over commands again. We are going to perform some initial markings, roughly analagous to the roots in a garbage collector's marking phase:

1. If a command has at least one failed predicate, we mark it MustRun.
2. If a the filesystem is missing some un-cached output from a command, we mark the command MustRun. We can establish this by comparing artifacts in the modeled environment to the filesystem.

When a command C is marked MustRun:
3. Any command that produces one of C's inputs is marked MustRun unless that input is cached
4. Any command that consumes one of C's outputs is marked MayRun

Until we have command skipping, we also make these markings:
a. Any child of C is marked MustRun (all descendants are marked transitively)
b. If C's parent is marked MayRun, mark it as MustRun (MayRun ancestors will be marked transitively)

When a command C is marked MayRun:
5. Any command that produces one of C's inputs is marked MayRun unless that input is cached
6. Any command that consumes one of C's outputs is marked MayRun

Until we have skipping:
c. Any child of C is marked MayRun (all descendants are marked transitively)

At the end of the marking phase, any command that remains in the Unknown state is marked Skip.



---

Two commands, A and B. File F.
A writes F
B reads F
B writes F
A reads F

All version of F are cached.

In iteration 1, A observes a change to F. What happens?
Using rule 1, A is marked MustRun
Using rule 4, B is marked MayRun

Now we run another iteration. A executes, and B is emulated.
At the end of this run, A is marked as AlreadyRun, and B is returned to the Unknown state.

During A's run, A changes F.
That will cause B's MatchContent(reference to F, [some version]) to fail.

Using rule 1, B is marked MustRun.
Using rule 2, we should mark A as MayRun.

This is a problem, because A is marked AlreadyRun. Are we going to run it for a second time? Here are some possible approaches we could take:

i. We could fix this by not allowing commands to move out of the AlreadyRun state. But that is bad in this case; A ran, but not with the latest inputs. I think this breaks the build w.r.t. our definition of build equivalence.

ii. We could allow AlreadyRun to change back to MayRun. If A and B write back and forth 100 times, we could end up running A and B each 100 times. A's first write to F changes B's first write, which forces a rerun of A, which changes A's second write to F, forcing a rerun of B, which changes B's second write to F, etc. If either comand is nondeterministic, we could end up running infinitely. I don't like that at all.

iii. We could add a special rule for marking: if A is marked MustRun, we already look at its inputs. If any of the commands that produces its inputs are also users of A's outputs, mark those commands MustRun even if those inputs are cached. This approach isn't perfect, but I like it better.

The problem with fix iii. is that it doesn't handle communication cycles larger than two. For example:
A writes F
B reads F
B writes G
C reads G
C writes F
A reads F

B uses A's output, C uses B's output, and A uses C's output. None of those commands appear in both the prodecers of inputs and users of outputs lists for a single command, but we have the same problem as the simpler two-command scenario.

It would be nice if the fix involved a local decision about marking. I think this will fix the issue in the general case:

When command C is marked MustRun:
7. For each command D that produces an input to C: If D is marked MayRun, mark D as MustRun.

When a command C is marked MayRun:
8. For each command D that consumes an output from C: If D is marked MustRun, mark C as MustRun.

We need a pair of rules because we don't know the order that commands will be marked. Rule 7 works when the MayRun marking happens first, and rule 8 works when the MustRun marking happens first. As with all the prior rules, we always invoke the rules to propagate markings when we change a command's marking.

That would ensure that no future iteration could force us to re-run a command that has already executed, and should handle arbitrary cycles. I am fairly certain this fix will lead us to execute all or none of the commands in a communication cycle, and never a proper subset of the commands in the cycle.
