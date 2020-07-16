# TraceIR Language
The TraceIR language is used to store the behavior of commands from a previous build. TraceIR is generated from the raw syscall trace of an executing command. The TraceIR can later be used to emulate the command's behavior; this is useful both for detecting when inputs to a command have changed in an observable way, and for replaying the effect of a command that we opt not to run during a rebuild.

A full trace contains a sequence of TraceIR steps, each marked with the command that performed that step. This is a complete list of the types of TraceIR steps:

## References
References are steps that produce a reference that can be used to reach an artifact. Each reference is resolved at run/emulation time, and can return either an artifact or an error code. In addition to the parameters specified with each reference type, every reference stores the expected outcome of resolving that reference, either SUCCESS or one of the standard POSIX error codes. Any change in the outcome of resolving a reference indicates that a command's input has changed.

**`Pipe() : Reference`**  
Create a reference to a new pipe

**`Symlink(dest : string) : Reference`**  
Create a reference to a new symbolic link with a specific destination

**`Dir() : Reference`**  
Create a reference to a new, empty directory.

**`Access(base : Access, path : string, flags : AccessFlags) : Reference`**  
Create a reference to a specific path, relative to some artifact reached via the `base` reference. The reference could resolve to any type of artifact. Access flags encode the permissions required (read, write, execute) as well as other flags specific to the open() system call.

## Predicates
**`Match(ref : Reference, v : VersionType)`**  
The artifact reached via `ref` must match the version `v`. There are several version types; all artifacts have metadata versions, files have content versions, directories have directory versions, etc. If the artifact reached via `ref` does *not* match version `v`, the command that performed this step has observed a change.

**`Join(child : Command, expected_code : int)`**  
The command performing this step waits for a child command to exit. The child is expected to exit with the specified exit code. If this code changes, the parent command has observed a change.

## Actions
**`Apply(ref : Reference, v : VersionType)`**  
Apply a version `v` to the artifact reached via `ref`. These are the same version types supported by the `Match` predicate.

**`Launch(child : Command)`**  
A command launches a child command. The child command stores the starting directory, root directory, user, group, arguments, environment variables, and initial file descriptors.

**`Exit(code : int)`**  
The current command exits with a particular exit code. ***This IR step is not implemented yet.***
