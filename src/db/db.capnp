@0x9a08508ff333af3a;

using Cxx = import "/capnp/c++.capnp";
$Cxx.namespace("db");

struct CommandRef {
  index @0 :Int64;
}

struct FileRef {
  index @0 :UInt64;
}

struct FileVersionRef {
  file  @0 :FileRef;
  index @1 :UInt64;
}

struct Command {
  struct Descriptor {
    fd       @0 :UInt16;  # The file descriptor number
    fileID   @1 :FileRef; # The file that should be opened
    readable @2 :Bool;    # Is this a readable file?
    writable @3 :Bool;    # Is this a writable file?
  }
  
  executable @0 :Text;                  # The executable file this command runs
  argv       @1 :List(Text);            # The arguments passed to this command
  cwd        @2 :Text;                  # The working directory this command should be started in
  children   @4 :UInt64;                # The count of this command's children (not descendants)
  initialFDs @3 :List(Descriptor);      # A list of file descriptors at the start of the command
  inputs     @5 :List(FileVersionRef);  # A list of this command's file version inputs
  outputs    @6 :List(FileVersionRef);  # A list of this command's file version outputs
}

struct File {
  enum Type {
    unknown   @0; # The file type is unknown, probably because it didn't exist
    regular   @1; # This is a regular file
    directory @2; # This is a directory
    symlink   @3; # This is a symlink
    pipe      @4; # This is a pipe
  }
  
  struct Version {
    action :union {
      create    @0 :CommandRef; # This command created the file
      reference @1 :Void;       # The file existed already, and was referenced
      write     @2 :CommandRef; # This command wrote to the file
      truncate  @3 :CommandRef; # This command truncated the file to zero size
      delete    @4 :CommandRef; # This command deleted the file
    }
    
    metadata :union {
      none   @5 :Void;    # For whatever reason, we do not have stat info for this file
      stat :group {
        size @6 :UInt64;  # The size of this file version
        sec  @7 :Int64;   # The mtime seconds for this file version
        nsec @8 :Int32;   # The mtime nanoseconds for this file version
      }
    }
  
    fingerprint :union {
      none   @9 :Void;  # No fingerprint for this file version
      blake2 @10 :Data; # The blake2sp checksum for this file
    }
  }
  
  path     @0 :Text;          # A canonical path the file
  type     @1 :Type;          # The type of file
  versions @2 :List(Version); # A list of this file's versions
}

struct Graph {
  commands @0 :List(Command); # All of the commands, listed via a preorder traversal
  files    @1 :List(File);    # All of the files
}
