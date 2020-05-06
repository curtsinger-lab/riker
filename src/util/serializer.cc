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

#include "core/AccessFlags.hh"
#include "core/Artifact.hh"
#include "core/Command.hh"
#include "core/FileDescriptor.hh"
#include "core/IR.hh"
#include "core/Version.hh"
#include "ui/log.hh"

using std::ifstream;
using std::make_unique;
using std::ofstream;
using std::string;
using std::unique_ptr;

// Declare the current version of the archive. Increase this number each time the archive changes
// in a way that would make old versions incompatible. Every serialize function below can
// accommodate logic to deserialize an outdated version.
const uint32_t ArchiveVersion = 11;

// Load a saved build from a file
shared_ptr<Command> load_build(string filename) {
  // Open the file for reading. Must pass std::ios::binary!
  ifstream f(filename, std::ios::binary);

  // Initialize cereal's binary archive reader
  cereal::BinaryInputArchive archive(f);

  // Attempt to load the build
  shared_ptr<Command> root;
  archive(root);
  return root;
}

// Save a build to a file
void save_build(string filename, shared_ptr<Command> root) {
  // Open the file for writing. Must pass std::ios::binary!
  ofstream f(filename, std::ios::binary);

  // Initialize cereal's binary archive writer
  cereal::BinaryOutputArchive archive(f);

  // Store the build
  archive(root);
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

CEREAL_CLASS_VERSION(Command, ArchiveVersion);

template <class Archive>
void serialize(Archive& ar, Command& c, const uint32_t version) {
  if (version == ArchiveVersion) {
    ar(c._exe, c._args, c._initial_fds, c._steps, c._children);
  } else {
    throw db_version_exception(version);
  }
}

CEREAL_CLASS_VERSION(InitialFD, ArchiveVersion);

template <class Archive>
void serialize(Archive& ar, InitialFD& fd, const uint32_t version) {
  if (version == ArchiveVersion) {
    ar(fd._ref, fd._writable);
  } else {
    throw db_version_exception(version);
  }
}

CEREAL_CLASS_VERSION(Artifact, ArchiveVersion);

template <class Archive>
void serialize(Archive& ar, Artifact& a, const uint32_t version) {
  if (version == ArchiveVersion) {
    ar(a._path, a._latest);
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

CEREAL_CLASS_VERSION(::Version, ArchiveVersion);

template <class Archive>
void serialize(Archive& ar, Version& v, const uint32_t version) {
  if (version == ArchiveVersion) {
    ar(v._path, v._index, v._previous, v._next, v._creator, v._metadata, v._fingerprint, v._saved);
  } else {
    throw db_version_exception(version);
  }
}

// Polymorphic relations with non-serialized parent types must be registered as well
CEREAL_REGISTER_POLYMORPHIC_RELATION(Step, Reference)

CEREAL_CLASS_VERSION(Pipe, ArchiveVersion);
CEREAL_REGISTER_TYPE(Pipe)
CEREAL_REGISTER_POLYMORPHIC_RELATION(Reference, Pipe)
template <class Archive>
void serialize(Archive& ar, Pipe& p, const uint32_t version) {
  if (version == ArchiveVersion) {
    // No fields to serialize here
  } else {
    throw db_version_exception(version);
  }
}

CEREAL_CLASS_VERSION(Access, ArchiveVersion);
CEREAL_REGISTER_TYPE(Access)
CEREAL_REGISTER_POLYMORPHIC_RELATION(Reference, Access)
template <class Archive>
void serialize(Archive& ar, Access& a, const uint32_t version) {
  if (version == ArchiveVersion) {
    ar(a._path, a._flags);
  } else {
    throw db_version_exception(version);
  }
}

CEREAL_CLASS_VERSION(AccessFlags, ArchiveVersion);
template <class Archive>
void serialize(Archive& ar, AccessFlags& f, const uint32_t version) {
  if (version == ArchiveVersion) {
    ar(f.r, f.w, f.x, f.nofollow, f.truncate, f.create, f.exclusive);
  } else {
    throw db_version_exception(version);
  }
}

CEREAL_REGISTER_POLYMORPHIC_RELATION(Step, Predicate)

CEREAL_CLASS_VERSION(ReferenceResult, ArchiveVersion);
CEREAL_REGISTER_TYPE(ReferenceResult)
CEREAL_REGISTER_POLYMORPHIC_RELATION(Predicate, ReferenceResult)
template <class Archive>
void serialize(Archive& ar, ReferenceResult& p, const uint32_t version) {
  if (version == ArchiveVersion) {
    ar(p._ref, p._rc);
  } else {
    throw db_version_exception(version);
  }
}

CEREAL_CLASS_VERSION(MetadataMatch, ArchiveVersion);
CEREAL_REGISTER_TYPE(MetadataMatch)
CEREAL_REGISTER_POLYMORPHIC_RELATION(Predicate, MetadataMatch)
template <class Archive>
void serialize(Archive& ar, MetadataMatch& p, const uint32_t version) {
  if (version == ArchiveVersion) {
    ar(p._ref, p._version);
  } else {
    throw db_version_exception(version);
  }
}

CEREAL_CLASS_VERSION(ContentsMatch, ArchiveVersion);
CEREAL_REGISTER_TYPE(ContentsMatch)
CEREAL_REGISTER_POLYMORPHIC_RELATION(Predicate, ContentsMatch)
template <class Archive>
void serialize(Archive& ar, ContentsMatch& p, const uint32_t version) {
  if (version == ArchiveVersion) {
    ar(p._ref, p._version);
  } else {
    throw db_version_exception(version);
  }
}

CEREAL_REGISTER_POLYMORPHIC_RELATION(Step, Action)

CEREAL_CLASS_VERSION(Launch, ArchiveVersion);
CEREAL_REGISTER_TYPE(Launch)
CEREAL_REGISTER_POLYMORPHIC_RELATION(Action, Launch)
template <class Archive>
void serialize(Archive& ar, Launch& a, const uint32_t version) {
  if (version == ArchiveVersion) {
    ar(a._cmd);
  } else {
    throw db_version_exception(version);
  }
}

CEREAL_CLASS_VERSION(SetMetadata, ArchiveVersion);
CEREAL_REGISTER_TYPE(SetMetadata)
CEREAL_REGISTER_POLYMORPHIC_RELATION(Action, SetMetadata)
template <class Archive>
void serialize(Archive& ar, SetMetadata& a, const uint32_t version) {
  if (version == ArchiveVersion) {
    ar(a._ref, a._version);
  } else {
    throw db_version_exception(version);
  }
}

CEREAL_CLASS_VERSION(SetContents, ArchiveVersion);
CEREAL_REGISTER_TYPE(SetContents)
CEREAL_REGISTER_POLYMORPHIC_RELATION(Action, SetContents)
template <class Archive>
void serialize(Archive& ar, SetContents& a, const uint32_t version) {
  if (version == ArchiveVersion) {
    ar(a._ref, a._version);
  } else {
    throw db_version_exception(version);
  }
}
