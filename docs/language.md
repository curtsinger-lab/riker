# TraceIR Language
The TraceIR language is used to store the behavior of commands from a previous build. TraceIR is generated from the raw syscall trace of an executing command. The TraceIR can later be used to emulate the command's behavior; this is useful both for detecting when inputs to a command have changed in an observable way, and for replaying the effect of a command that we opt not to run during a rebuild.

A full trace contains a sequence of TraceIR steps, each marked with the command that performed that step. This is a complete list of the types of TraceIR steps:

## References
References are steps that produce a reference that can be used to reach an artifact. Each reference is resolved at run/emulation time, and can return either an artifact or an error code.

**`SpecialRef() : Ref`**  
Create a reference to a special artifact. Currently these include stdin, stdout, stderr, and the root directory.

**`PipeRef() : Ref`**  
Create a reference to a new anonymous pipe.

**`SymlinkRef(dest : string) : Ref`**  
Create a reference to a new anonymous symbolic link with a given destination

**`DirRef() : Ref`**  
Create a reference to a new anonymous directory.

**`FileRef() : Ref`**
Create a reference to a new anonymous file.

**`PathRef(base : Ref, path : string, flags : AccessFlags) : Ref`**  
Create a reference to a specific path, relative to some artifact reached via the `base` reference. The reference could resolve to any type of artifact. The flags encode the permissions required (read, write, execute) as well as other flags specific to the open() system call.

## Predicates
**`CompareRefs(ref1 : Ref, ref2 : Ref, type : RefComparison)`**
Check whether two references pass a specific comparison type. For example, RefComparison::SameInstance will pass if ref1 and ref2 refer to the same artifact. RefComparison::DifferentInstances will pass if ref1 and ref2 refer to different artifacts.

**`ExpectResult(ref : Ref, expected_result : int)`**  
Check the result of resolving a specific reference. The expected result is either SUCCESS or one of the standard POSIX error codes. Any change in the outcome of resolving a reference indicates that a command's input has changed.

**`MatchMetadata(ref : Ref, v : MetadataVersion)`**  
The artifact reached via `ref` must have metadata that matches version `v`.

**`MatchContent(ref : Ref, v : Version)`**
The artifact reached via `ref` must have content that matches the version `v`. Not all version types support matching, but supported version types are FileVersion (the contents of a file), SymlinkVersion (symlink's target), and ListedDir (the complete list of a directory's entries).
Version types like AddEntry and RemoveEntry, which are partial versions for directory artifacts, cannot be matched against.

**`Join(child : Command, expected_code : int)`**  
The command performing this step waits for a child command to exit. The child is expected to exit with the specified exit code. If this code changes, the parent command has observed a change.

## Actions
**`OpenRef(ref : Ref)`**
A command retains a handle to a given reference. Only opened references can be inherited by child commands (e.g. via file descriptors that are not opened with O_CLOEXEC).

**`CloseRef(ref : Ref)`**
A command has closed its final handle to a given reference.

**`UpdateMetadata(ref : Ref, v : MetadataVersion)`**
Get the artifact reached by `ref` and set its metadata to version `v`.

**`UpdateContent(ref : Ref, v : Version)`**  
Update the content of the artifact reached by `ref` with version `v`. Not all version types can be used to update an artifact's contents.

**`AddEntry(dir : Ref, name : str, target : Ref)`**
Add an entry called `name` to the directory referenced by `dir`. This entry points to the artifact reached via reference `target`.

**`RemoveEntry(dir : Ref, name : str, target : Ref)`**
Remove the entry called `name` from the directory referenced by `dir`. Prior to removal, the entry points to the artifact reached via `target.

**`Launch(child : Command)`**  
A command launches a child command. The child command stores the starting directory, root directory, user, group, arguments, environment variables, and initial file descriptors.

**`Exit(code : int)`**  
The current command exits with a particular exit code. ***This IR step is not implemented yet.***
