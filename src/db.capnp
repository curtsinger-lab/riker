@0xb746a220c274d8b3;

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
  workingDirectory @3 :Data;
  # We don't use Text because paths may not be valid unicode
  struct FDEntry {
    fd @0 :UInt16;
    fileID @1 :UInt64;
    canRead @2 :Bool;
    canWrite @3 :Bool;
  }
  initialFDs @4 :List(FDEntry);
  # A sorted list of the open and used file discriptors passed to a command on
  # startup. Typically, but not always, this is just the process's stdin, stdout,
  # and stderr.

  outOfDate @5 :Bool;
  # Whether we need to rerun this command if demanded.
  collapseWithParent @6 :Bool;
  # Whether we should instead run this command's parent if this needs to rerun
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
  blake2sp @3;
}

struct File {
  path @0 :Data;
  type @1 :FileType;
  mode @2 :UInt16;

  fingerprintType @3 :FingerprintType;
  size @4 :UInt64;
  modificationTime @5 :Timespec;
  inode @6 :UInt64;
  checksum @7 :Data;

  latestVersion @8 :Bool;
}

struct Dependency {
  inputID @0 :UInt64;
  outputID @1 :UInt64;
}

struct Graph {
  commands @0 :List(Command);
  # A preorder traversal of the tree of commands

  files @1 :List(File);
  # TODO: What order are these in?

  inputs @2 :List(Dependency);
  # From files to commands
  # TODO: order?
  outputs @3 :List(Dependency);
  # From commands to files
  # TODO: order?
  creations @4 :List(Dependency);
  # From commands to files
  # TODO: order?
  removals @5 :List(Dependency);
  # From commands to files
  # TODO: order?
  modifications @6 :List(Dependency);
  # From files to files
  # TODO: order?
}
