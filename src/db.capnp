@0xdc02d836d6eb1de5;

using Cxx = import "/capnp/c++.capnp";
$Cxx.namespace("db");

struct Timespec {
  secondsSinceEpoch @0 :Int64;
  nanoseconds @1 :UInt32;
}

struct Command {
  descendants @0 :UInt64;
  # For the tree structure, how many children did this (recursively) execute?
  # Note: 0 can also mean that this node is opaque and cannot be split.

  executable @1 :Data;
  argv @2 :List(Data);
  # We don't use Text because paths may not be valid unicode
  initialFDs @3 :List(FDEntry);
  struct FDEntry {
    fd @0 :UInt16;
    fileID @1 :UInt64;
  }
  # A sorted list of the open and used file discriptors passed to a command on
  # startup. Typically, but not always, this is just the process's stdin, stdout,
  # and stderr.

  outOfDate @4 :Bool;
  # Whether we need to rerun this command if demanded.
}

enum FileType {
  regular @0;
  directory @1;
  symlink @2;
  pipe @3;
}

enum FingerprintType {
  unavailable @0;
  # There was some error generating a fingerprint
  nonexistent @1;
  # The file did not exist when it was fingerprinted
  metadataOnly @2;
}

struct File {
  path @0 :Data;
  type @1 :FileType;

  fingerprintType @2 :FingerprintType;
  size @3 :UInt64;
  modificationTime @4 :Timespec;
  inode @5 :UInt64;
  checksum @6 :Data;
}

struct Dependency {
  fileID @0 :UInt64;
  commandID @1 :UInt64;
}

struct Graph {
  commands @0 :List(Command);
  # A preorder traversal of the tree of commands

  files @1 :List(File);
  # TODO: What order are these in?

  inputs @2 :List(Dependency); # TODO: order?
  outputs @3 :List(Dependency); # TODO: order?
  creates @4 :List(Dependency); # TODO: order?
  removals @5 :List(Dependency); # TODO: order?
}
