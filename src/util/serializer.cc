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

#include "core/Artifact.hh"
#include "core/Build.hh"
#include "core/Command.hh"
#include "core/FileDescriptor.hh"
#include "core/IR.hh"
#include "ui/log.hh"

using std::ifstream;
using std::make_unique;
using std::ofstream;
using std::string;
using std::unique_ptr;

// Declare the current version of the archive. Increase this number each time the archive changes
// in a way that would make old versions incompatible. Every serialize function below can
// accommodate logic to deserialize an outdated version.
const uint32_t ArchiveVersion = 3;

// Load a saved build from a file
Build load_build(string filename) {
  // Open the file for reading. Must pass std::ios::binary!
  ifstream f(filename, std::ios::binary);

  // Initialize cereal's binary archive reader
  cereal::BinaryInputArchive archive(f);

  // Attempt to load the build
  Build b;
  archive(b);
  return b;
}

// Save a build to a file
void save_build(string filename, Build& b) {
  // Open the file for writing. Must pass std::ios::binary!
  ofstream f(filename, std::ios::binary);

  // Initialize cereal's binary archive writer
  cereal::BinaryOutputArchive archive(f);

  // Store the build
  archive(b);
}

/*
 * Serialization Functions
 * Each template function specifies the fields of a given class or struct that should be
 * serialized. These template functions are friends of the types they serialize, so they can
 * access private fields as needed. Friend declarations also give these functions access to
 * private member types.
 *
 * In addition to the serialization functions themselves, IR types are registered with cereal,
 * which is required for polymorphic types. All serialized types must be registered with
 * CEREAL_REGISTER_TYPE. Parent classes that are *not* serialized must be reported with the
 * CEREAL_REGISTER_POLYMORPHIC_RELATION macro.
 */

CEREAL_CLASS_VERSION(Build, ArchiveVersion);

template <class Archive>
void serialize(Archive& ar, Build& b, const uint32_t version) {
  if (version == ArchiveVersion) {
    ar(b._root, b._default_refs, b._default_artifacts);
  } else {
    throw db_version_exception(version);
  }
}

CEREAL_CLASS_VERSION(Command, ArchiveVersion);

template <class Archive>
void serialize(Archive& ar, Command& c, const uint32_t version) {
  if (version == ArchiveVersion) {
    ar(c._exe, c._args, c._initial_fds, c._steps);
  } else {
    throw db_version_exception(version);
  }
}

CEREAL_CLASS_VERSION(FileDescriptor, ArchiveVersion);

template <class Archive>
void serialize(Archive& ar, FileDescriptor& fd, const uint32_t version) {
  if (version == ArchiveVersion) {
    ar(fd._ref, fd._artifact, fd._writable, fd._cloexec);
  } else {
    throw db_version_exception(version);
  }
}

CEREAL_CLASS_VERSION(Artifact, ArchiveVersion);

template <class Archive>
void serialize(Archive& ar, Artifact& a, const uint32_t version) {
  if (version == ArchiveVersion) {
    ar(a._path, a._versions);
  } else {
    throw db_version_exception(version);
  }
}

CEREAL_CLASS_VERSION(Artifact::VersionData, ArchiveVersion);

template <class Archive>
void serialize(Archive& ar, Artifact::VersionData& v, const uint32_t version) {
  if (version == ArchiveVersion) {
    ar(v.metadata, v.fingerprint, v.saved);
  } else {
    throw db_version_exception(version);
  }
}

CEREAL_CLASS_VERSION(struct stat, ArchiveVersion);

template <class Archive>
void serialize(Archive& ar, struct stat& s, const uint32_t version) {
  if (version == ArchiveVersion) {
    ar(s.st_mode, s.st_uid, s.st_gid, s.st_size, s.st_mtim);
  } else {
    throw db_version_exception(version);
  }
}

CEREAL_CLASS_VERSION(struct timespec, ArchiveVersion);

template <class Archive>
void serialize(Archive& ar, struct timespec& ts, const uint32_t version) {
  if (version == ArchiveVersion) {
    ar(ts.tv_sec, ts.tv_nsec);
  } else {
    throw db_version_exception(version);
  }
}

CEREAL_CLASS_VERSION(ArtifactVersion, ArchiveVersion);

template <class Archive>
void serialize(Archive& ar, ArtifactVersion& v, const uint32_t version) {
  if (version == ArchiveVersion) {
    ar(v._artifact, v._index);
  } else {
    throw db_version_exception(version);
  }
}

// Polymorphic relations with non-serialized parent types must be registered as well
CEREAL_REGISTER_POLYMORPHIC_RELATION(Step, Reference)

CEREAL_CLASS_VERSION(Reference::Pipe, ArchiveVersion);
CEREAL_REGISTER_TYPE(Reference::Pipe)
CEREAL_REGISTER_POLYMORPHIC_RELATION(Reference, Reference::Pipe)
template <class Archive>
void serialize(Archive& ar, Reference::Pipe& p, const uint32_t version) {
  if (version == ArchiveVersion) {
    // No fields to serialize here
  } else {
    throw db_version_exception(version);
  }
}

CEREAL_CLASS_VERSION(Reference::Access, ArchiveVersion);
CEREAL_REGISTER_TYPE(Reference::Access)
CEREAL_REGISTER_POLYMORPHIC_RELATION(Reference, Reference::Access)
template <class Archive>
void serialize(Archive& ar, Reference::Access& a, const uint32_t version) {
  if (version == ArchiveVersion) {
    ar(a._path, a._flags);
  } else {
    throw db_version_exception(version);
  }
}

CEREAL_CLASS_VERSION(Reference::Access::Flags, ArchiveVersion);
template <class Archive>
void serialize(Archive& ar, Reference::Access::Flags& f, const uint32_t version) {
  if (version == ArchiveVersion) {
    ar(f.r, f.w, f.x, f.nofollow, f.truncate, f.create, f.exclusive);
  } else {
    throw db_version_exception(version);
  }
}

CEREAL_REGISTER_POLYMORPHIC_RELATION(Step, Predicate)

CEREAL_CLASS_VERSION(Predicate::IsOK, ArchiveVersion);
CEREAL_REGISTER_TYPE(Predicate::IsOK)
CEREAL_REGISTER_POLYMORPHIC_RELATION(Predicate, Predicate::IsOK)
template <class Archive>
void serialize(Archive& ar, Predicate::IsOK& p, const uint32_t version) {
  if (version == ArchiveVersion) {
    ar(p._ref);
  } else {
    throw db_version_exception(version);
  }
}

CEREAL_CLASS_VERSION(Predicate::IsError, ArchiveVersion);
CEREAL_REGISTER_TYPE(Predicate::IsError)
CEREAL_REGISTER_POLYMORPHIC_RELATION(Predicate, Predicate::IsError)
template <class Archive>
void serialize(Archive& ar, Predicate::IsError& p, const uint32_t version) {
  if (version == ArchiveVersion) {
    ar(p._ref, p._err);
  } else {
    throw db_version_exception(version);
  }
}

CEREAL_CLASS_VERSION(Predicate::MetadataMatch, ArchiveVersion);
CEREAL_REGISTER_TYPE(Predicate::MetadataMatch)
CEREAL_REGISTER_POLYMORPHIC_RELATION(Predicate, Predicate::MetadataMatch)
template <class Archive>
void serialize(Archive& ar, Predicate::MetadataMatch& p, const uint32_t version) {
  if (version == ArchiveVersion) {
    ar(p._ref, p._version);
  } else {
    throw db_version_exception(version);
  }
}

CEREAL_CLASS_VERSION(Predicate::ContentsMatch, ArchiveVersion);
CEREAL_REGISTER_TYPE(Predicate::ContentsMatch)
CEREAL_REGISTER_POLYMORPHIC_RELATION(Predicate, Predicate::ContentsMatch)
template <class Archive>
void serialize(Archive& ar, Predicate::ContentsMatch& p, const uint32_t version) {
  if (version == ArchiveVersion) {
    ar(p._ref, p._version);
  } else {
    throw db_version_exception(version);
  }
}

CEREAL_REGISTER_POLYMORPHIC_RELATION(Step, Action)

CEREAL_CLASS_VERSION(Action::Launch, ArchiveVersion);
CEREAL_REGISTER_TYPE(Action::Launch)
CEREAL_REGISTER_POLYMORPHIC_RELATION(Action, Action::Launch)
template <class Archive>
void serialize(Archive& ar, Action::Launch& a, const uint32_t version) {
  if (version == ArchiveVersion) {
    ar(a._cmd);
  } else {
    throw db_version_exception(version);
  }
}

CEREAL_CLASS_VERSION(Action::SetMetadata, ArchiveVersion);
CEREAL_REGISTER_TYPE(Action::SetMetadata)
CEREAL_REGISTER_POLYMORPHIC_RELATION(Action, Action::SetMetadata)
template <class Archive>
void serialize(Archive& ar, Action::SetMetadata& a, const uint32_t version) {
  if (version == ArchiveVersion) {
    ar(a._ref, a._version);
  } else {
    throw db_version_exception(version);
  }
}

CEREAL_CLASS_VERSION(Action::SetContents, ArchiveVersion);
CEREAL_REGISTER_TYPE(Action::SetContents)
CEREAL_REGISTER_POLYMORPHIC_RELATION(Action, Action::SetContents)
template <class Archive>
void serialize(Archive& ar, Action::SetContents& a, const uint32_t version) {
  if (version == ArchiveVersion) {
    ar(a._ref, a._version);
  } else {
    throw db_version_exception(version);
  }
}
