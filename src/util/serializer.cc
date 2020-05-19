#include "serializer.hh"

#include <fstream>
#include <memory>
#include <string>

#include <fcntl.h>
#include <sys/stat.h>

#include <cereal/archives/binary.hpp>
#include <cereal/types/array.hpp>
#include <cereal/types/list.hpp>
#include <cereal/types/map.hpp>
#include <cereal/types/memory.hpp>
#include <cereal/types/optional.hpp>
#include <cereal/types/polymorphic.hpp>
#include <cereal/types/vector.hpp>

#include "data/AccessFlags.hh"
#include "data/Command.hh"
#include "data/IR.hh"
#include "data/InitialFD.hh"
#include "data/Version.hh"
#include "ui/log.hh"

using std::ifstream;
using std::ofstream;
using std::string;

enum : size_t { ArchiveVersion = 13 };

/// Try to load a build. Exit with an error if loading fails.
shared_ptr<Command> load_build(string filename, bool default_fallback) {
  try {
    // Open the file for reading. Must pass std::ios::binary!
    ifstream f(filename, std::ios::binary);

    // Initialize cereal's binary archive reader
    cereal::BinaryInputArchive archive(f);

    // Set up root fields for the load
    size_t version;
    shared_ptr<Command> root;

    // Attempt to load the build
    archive(version, root);

    // Is there a version mismatch?
    if (version != ArchiveVersion) {
      // Is default fallback enabled?
      if (default_fallback) {
        WARN << "Build database is outdated. Initializing a default build.";
        return Command::createRootCommand();
      } else {
        FAIL << "Build database is outdated. Rerun the build to create a new build database.";
      }
    }

    // If the version matches, return the root of the build
    return root;

  } catch (cereal::Exception e) {
    // If fallback to a default is allowed, get a default build
    if (default_fallback) return Command::createRootCommand();
    FAIL << "Failed to load the build database. Have you run a build yet?";
  }
  // Unreachable, but silences warnings
  exit(2);
}

// Save a build to a file
void save_build(string filename, shared_ptr<Command> root) {
  // Open the file for writing. Must pass std::ios::binary!
  ofstream f(filename, std::ios::binary);

  // Initialize cereal's binary archive writer
  cereal::BinaryOutputArchive archive(f);

  // Store the build
  size_t version = ArchiveVersion;
  archive(version, root);
}

/// Serialization function for struct stat
template <class Archive>
void serialize(Archive& ar, struct stat& s) {
  ar(s.st_mode, s.st_uid, s.st_gid, s.st_size, s.st_mtim);
}

/// Serialization function for struct timespec
template <class Archive>
void serialize(Archive& ar, struct timespec& ts) {
  ar(ts.tv_sec, ts.tv_nsec);
}

/** Register types and polymorphic relationships **/

CEREAL_REGISTER_TYPE(::Version);

CEREAL_REGISTER_TYPE(InitialPipeVersion);
CEREAL_REGISTER_POLYMORPHIC_RELATION(::Version, InitialPipeVersion);

CEREAL_REGISTER_TYPE(OpenedVersion);
CEREAL_REGISTER_POLYMORPHIC_RELATION(::Version, OpenedVersion);

CEREAL_REGISTER_TYPE(ModifiedVersion);
CEREAL_REGISTER_POLYMORPHIC_RELATION(::Version, ModifiedVersion);

CEREAL_REGISTER_TYPE(Reference);
CEREAL_REGISTER_POLYMORPHIC_RELATION(Step, Reference);

CEREAL_REGISTER_TYPE(Pipe);
CEREAL_REGISTER_POLYMORPHIC_RELATION(Reference, Pipe);

CEREAL_REGISTER_POLYMORPHIC_RELATION(Reference, Access);
CEREAL_REGISTER_TYPE(Access);

CEREAL_REGISTER_POLYMORPHIC_RELATION(Step, Predicate);

CEREAL_REGISTER_TYPE(ReferenceResult);
CEREAL_REGISTER_POLYMORPHIC_RELATION(Predicate, ReferenceResult);

CEREAL_REGISTER_TYPE(MetadataMatch);
CEREAL_REGISTER_POLYMORPHIC_RELATION(Predicate, MetadataMatch);

CEREAL_REGISTER_TYPE(ContentsMatch);
CEREAL_REGISTER_POLYMORPHIC_RELATION(Predicate, ContentsMatch);

CEREAL_REGISTER_POLYMORPHIC_RELATION(Step, Action);

CEREAL_REGISTER_TYPE(Launch);
CEREAL_REGISTER_POLYMORPHIC_RELATION(Action, Launch);

CEREAL_REGISTER_TYPE(SetMetadata);
CEREAL_REGISTER_POLYMORPHIC_RELATION(Action, SetMetadata);

CEREAL_REGISTER_TYPE(SetContents);
CEREAL_REGISTER_POLYMORPHIC_RELATION(Action, SetContents);
