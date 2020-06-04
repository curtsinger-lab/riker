# Dependency Language
We can write down commands' dependences using an expression-based language, documented below. This doesn't capture any of the version lookup logic, but I think it does help make examples a bit more concise, and could be helpful as an actual representation for the implementation.

The language must by dynamically typed, since a core operation is to get an entry from a directory by name. The result of that operation could be either a file or directory. I'll use type alternatives to express the dynamically-typed results of different operations.

## Datatypes
- `File` -- A file at a particular version
- `Directory` -- A directory at a particular version
- `String` -- A string
- `Error` -- The result of an operation that results in some failure

## Primitives
- `/ :: Directory` -- the root directory
- `. :: Directory` -- the current working directory
- `NEWFILE :: File` -- an empty file
- `NEWDIR :: Directory` -- an empty directory
- `EXE :: File` -- the file object for the currently-running command's executable
- `"..." :: String` -- any string (used to access path names)
- `OK :: ?` -- A special value that compares equal to any non-error result
- Errors:
  - `ENOENT :: Error` -- Result from an operation that failed with error ENOENT
  - `EEXIST :: Error` -- Result from an operation that failed with error EEXIST
  - `EACCES :: Error` -- Result from an operation that failed  with error EACCES
  - ...

## Operations
Error handling with operations has one detail that I will omit from the descriptions, since it clutters the list. Anywhere a Directory or File can be passed in as a parameter, we could also pass in an Error. Any operation that receives an Error rather than a File or Directory will return that Error as a result. Operations that receive multiple Errors will return one of the two, but which one is going to depend on the behavior in POSIX.

`GET(String, Directory) -> File | Directory | Error`
: Get a named entry from a directory. On success, returns the file or directory with the specified name. On failure, returns an error code.

`LINK(File | Directory, String, Directory) -> Directory | Error`
: Link a file or directory into a directory with a given name. Result is a version of the directory that reflects the result of the operation, or an error.

`UNLINK(String, Directory) -> Directory | Error`
: Unlink the named entry in a directory. Result is either an error or a version of the directory that reflects the result of the operation.
  
`LIST(Directory) -> Directory | Error`
: List the contents of a directory. Result is either an error or the version of the directory that was listed.

`R(File) -> File | Error`
: Read from a file. Result is either an error or the version of the file that was read

`W(File) -> File | Error`
: Write to a file. Result is either an error, or the file version that reflects the write

`X(File) -> File | Error`
: Execute a file. Result is either an error, or the file version that was executed.

`STAT(File | Directory) -> File | Directory | Error`
: Stat a file or directory. Result is either an error or the file/directory version that was examined.
  
## Pending Issues
Some issues in the language that need to be resolved:

### Expressing Similar Operations
Is it better to express distinct operations like `open` with `O_CREAT | O_TRUNC` and navigating into a directory using different language constructs, or a single generic construct that uses flags to determine the behavior?

### Opening Versus Reading/Writing
There is currently no way to express the difference between opening a file in read mode, versus actually reading from the file. This distinction is important.

### Handling Symlinks
The language needs to be extended to include symlinks. The right approach is probably to add another top-level datatype. Will we need `O_NOFOLLOW` versions of other operations to handle them? Could we instead wrap them in a special NOFOLLOW operation to explicitly access the symlink itself?

### Metadata Access and Changes
The language includes a mostly unexamined `STAT` operation for accessing metadata, but we'll also need to add representations for `chmod` or `chown` operations.
