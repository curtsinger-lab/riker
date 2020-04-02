## System Design Principles

We should discuss all of these in the paper.

* Even though the system observes builds at syscall granularity, rebuilds happen at command granularity.  The reason is that this is not a record-and-replay system, so the only way to start a process is by calling a command.
* A reference is a data structure that resolves a {path, a pipe, file descriptor} to either an artifact or an error.
* Resolution is the process of obtaining an actual artifact or an error if that artifact cannot be referenced  (e.g., ENOENT).
* An artifact is either a file, a directory, a symlink, a pipe (and eventually, a socketpair).
* All artifacts are versioned; they are versioned at the time of read or write.
* Two output files F and G are build equivalent (F ~~ G) if 
    1. [simple equivalence] F and G hold the same contents, OR
      shorthand: F = G --> F ~~ G
    2. [transitive equivalence] Command C reads input file F and produces output file O. If command C is run again, consuming input file G and producing output file P, then O and P are eqivalent if F and G are equivalent
      shorthand: O := C(F), P := C(G), and F ~~ G --> O ~~ P
*  A directory is modeled as a map between a path and an artifact.
* The only ordering that matters is the ordering observable via stateful side effects.