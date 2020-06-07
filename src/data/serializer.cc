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
#include "util/log.hh"

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

  } catch (cereal::Exception& e) {
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
void save(Archive& ar, struct stat const& s) {
  uint8_t sizeval = s.st_size == 0 ? 0 : 1;
  ar((uint16_t)s.st_mode, s.st_uid, s.st_gid, sizeval, s.st_mtim);
}

template <class Archive>
void load(Archive& ar, struct stat& s) {
  uint8_t sizeval;
  uint16_t mode;
  ar(mode, s.st_uid, s.st_gid, sizeval, s.st_mtim);
  s.st_mode = mode;
  s.st_size = sizeval;
}

/// Serialization function for struct timespec
template <class Archive>
void serialize(Archive& ar, struct timespec& ts) {
  ar(ts.tv_sec, ts.tv_nsec);
}

/** Register types and polymorphic relationships **/

// References
CEREAL_REGISTER_TYPE(Pipe);
CEREAL_REGISTER_TYPE(Access);

// Predicates
CEREAL_REGISTER_TYPE(ReferenceResult);
CEREAL_REGISTER_TYPE(MetadataMatch);
CEREAL_REGISTER_TYPE(ContentsMatch);

// Actions
CEREAL_REGISTER_TYPE(Launch);
CEREAL_REGISTER_TYPE(SetMetadata);
CEREAL_REGISTER_TYPE(SetContents);
