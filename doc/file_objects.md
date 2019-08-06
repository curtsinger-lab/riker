Handling of file objects:

Files are stored as objects within the traced program's "state." Files have both creators and writers. Files are tracked by version (with each version having at most one writer), as follows:

* on file open, a new version is created, with no writer
* on write, if the current command is not the file's writer, a new version is created 
* on deletion, a new version is created with the known_removed field set to true, with no writer

Fingerprinting of files is done on creation of a new version. 

Each file object also has a set of processes which have mmaped this file. A read mmap is treated as a read to the file, a write mmap as a write, and a read-write mmap as both a read and a write. Processes also have a set of files they have mmaped, and on process exit, these mmaps are erased from both the process' and the file's mmap sets. 

In our current implementation, pipes are treated as files which are always considered "changed" when deciding which commands to rerun. In the serialized file representation, each object has a type: either "regular" or "pipe."
