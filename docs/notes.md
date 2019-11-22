# Implementation Notes
This document describes some of the implementation details that are *planned*. They may not exactly match what is in place at the moment.

## Data Model
There are five key types that make up the data model for the build graph:
1. The build graph itelf
2. Commands that run during the build
3. Artifacts (files, dirs, pipes, etc.) used by commands
4. Specific versions of artifacts
5. References, which are the names commands use to reach artifacts

I'll discuss each type in more detail below.

### BuildGraph
The build graph must record a root command. This command invokes, directly or indirectly, every other command that runs as part of the build. The command will always be the Rikerfile where the build process is defined.

The build graph also holds a reference to the Rikerfile used to start the build. The full build depends on this reference (renaming or removing the file changes the build) but the actual Rikerfile command depends on the contents of this file (the artifact).

### Artifact
A node representing a specific file, directory, or pipe.
Maintains a sequence of versions.

Commands' read operations always reference the latest version of an artifact. This matches the behavior of files and pipes; writing a file immediately makes the updates visible to other users of that file. If commands access distinct files, they must be different Artifact instances as well; this is true even for artifacts that were loaded using the same path.

### Artifact::Version
Every artifact has at least one version. Versions are created when:

1. A command references an artifact that existed prior to the build
2. A command writes to an artifact

If a command writes to an artifact multiple times, and no other command reads that artifact between those writes, then these writes can collapse to the same version. This is strictly an optimization, not a requirement for correctness.

Versions are involved in tracking fingerprints and snapshots of artifacts at various stages during the build. Not every version needs a fingerprint or snapshot; this will be discussed in a later section.

### Command
A command corresponds to an `exec` call in the running program. This is the minimal unit of work we can perform on a future build. Commands track some basic information needed to rerun them:

1. Command line arguments
2. Environment variables
3. The executable file (which may not match the first command line argument)
4. The initial working directory
5. The initial root directory

Commands also keep a list of references they make to artifacts.

### Reference
A reference is the name that a command uses to refer to a file. That name could be a path, or just a file descriptor number if the file was opened before the command started. Rerunning the command would presumably make these same references, so we must check the contents of the artifacts they refer to when deciding whether inputs have changed.

In addition to the references set up through regular interactions with files, commands also have three special references: the executable file, the working directory, and the root directory. The executable file will not change, but the working directory and root could change during a command's execution. We need to save the reference to the *initial* working directory and root in addition to the current references.

References are chained together so we can reconstruct them when deciding whether a command's input has changed. These chains are slightly different for the two types of references:

Path References:
  The path string, e.g. "a/b.txt"
  The base directory, either cwd or a base given to a syscall like openat. This is a reference.
  The root directory, which is another reference.

Descriptor References:
  The descriptor number, e.g. 1
  The path used to open this file in another command.

In addition to the naming information tracked in each reference, we also need to know a bit more about references:

Artifact:
  The artifact this reference resolved to, or nothing if the reference did not successfully resolve (e.g. a failed `open` call).

Versions:
  The artifact versions read and written through this reference.

Permissions:
  The permissions the command used when setting up the reference. These roughly mirror mode and flags to the `open` syscall: read, write, and execute, as well as cloexec, create, truncate, exclusive, and nofollow. The full set of permissions only apply to path references; file descriptor references only have information about read/write permissions.

## Tracing

## Snapshots and Fingerprints

## Rebuilding

