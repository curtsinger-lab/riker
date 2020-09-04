#include "serializer.hh"

#include <filesystem>
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
#include <cereal/types/set.hpp>
#include <cereal/types/tuple.hpp>
#include <cereal/types/vector.hpp>

#include "core/AccessFlags.hh"
#include "core/Command.hh"
#include "core/FileDescriptor.hh"
#include "core/IR.hh"
#include "core/Trace.hh"
#include "util/log.hh"
#include "versions/DirVersion.hh"
#include "versions/FileVersion.hh"
#include "versions/MetadataVersion.hh"
#include "versions/SymlinkVersion.hh"
#include "versions/Version.hh"

using std::ifstream;
using std::ofstream;
using std::string;

enum : size_t { ArchiveVersion = 13 };

/// Try to load a build trace
shared_ptr<Trace> load_trace(string filename) noexcept {
  try {
    // Open the file for reading. Must pass std::ios::binary!
    ifstream f(filename, std::ios::binary);

    // Initialize cereal's binary archive reader
    cereal::BinaryInputArchive archive(f);

    // Set up root fields for the load
    size_t version;
    shared_ptr<Trace> trace;

    // Attempt to load the build
    archive(version, trace);

    // Is there a version mismatch?
    if (version != ArchiveVersion) {
      WARN << "Build database is outdated. Initializing a default build.";
      return Trace::getDefault();
    }

    // If the version matches, return the root of the build
    return trace;

  } catch (cereal::Exception& e) {
    // If loading failed, reutrn a default trace
    return Trace::getDefault();
  }
}

// Save a build to a file
void save_trace(string filename, shared_ptr<Trace> trace) noexcept {
  // Open the file for writing. Must pass std::ios::binary!
  ofstream f(filename, std::ios::binary);

  // Initialize cereal's binary archive writer
  cereal::BinaryOutputArchive archive(f);

  // Store the build
  size_t version = ArchiveVersion;
  archive(version, trace);
}

/// Serialization function for struct timespec
template <class Archive>
void serialize(Archive& ar, struct timespec& ts) noexcept {
  ar(ts.tv_sec, ts.tv_nsec);
}

/// Serialization function for std::filesystem::path
namespace std {
  namespace filesystem {
    template <class Archive>
    void load(Archive& ar, path& p) {
      string s;
      ar(s);
      p = s;
    }

    template <class Archive>
    void save(Archive& ar, const path& p) {
      ar(string(p));
    }
  }
}

/** Register types and polymorphic relationships **/

// Versions
CEREAL_REGISTER_TYPE(MetadataVersion);
CEREAL_REGISTER_TYPE(FileVersion);
CEREAL_REGISTER_TYPE(SymlinkVersion);

// Directory version types
CEREAL_REGISTER_TYPE(AddEntry);
CEREAL_REGISTER_TYPE(RemoveEntry);
CEREAL_REGISTER_TYPE(CreatedDir);
CEREAL_REGISTER_TYPE(ListedDir);

// IR Steps
CEREAL_REGISTER_TYPE(SpecialRef);
CEREAL_REGISTER_TYPE(PipeRef);
CEREAL_REGISTER_TYPE(FileRef);
CEREAL_REGISTER_TYPE(SymlinkRef);
CEREAL_REGISTER_TYPE(DirRef);
CEREAL_REGISTER_TYPE(PathRef);
CEREAL_REGISTER_TYPE(ExpectResult);
CEREAL_REGISTER_TYPE(MatchMetadata);
CEREAL_REGISTER_TYPE(MatchContent);
CEREAL_REGISTER_TYPE(UpdateMetadata);
CEREAL_REGISTER_TYPE(UpdateContent);
CEREAL_REGISTER_TYPE(Launch);
CEREAL_REGISTER_TYPE(Join);
CEREAL_REGISTER_TYPE(Exit);