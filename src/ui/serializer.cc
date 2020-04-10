#include "serializer.hh"

#include <fstream>
#include <string>

#include <cereal/archives/binary.hpp>
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

using std::ifstream;
using std::ofstream;
using std::string;

// Load a saved build from a file
bool load_build(string filename, Build& b) {
  // Open the file for reading. Must pass std::ios::binary!
  ifstream f(filename, std::ios::binary);

  // Initialize cereal's binary archive reader
  cereal::BinaryInputArchive archive(f);

  try {
    // Attempt to load the build
    archive(b);
    return true;
  } catch (cereal::Exception e) {
    // Return false on failure
    return false;
  }
}

// Save a build to a file
void save_build(string filename, const Build& b) {
  // Open the file for writing. Must pass std::ios::binary!
  ofstream f(filename, std::ios::binary);

  // Initialize cereal's binary archive writer
  cereal::BinaryOutputArchive archive(f);

  // Store the build
  archive(b);
}

/*
 * Serialization Functions
 * Each template function specifies the fields of a given class or struct that should be serialized.
 * These template functions are friends of the types they serialize, so they can access private
 * fields as needed. Friend declarations also give these functions access to private member types.
 *
 * In addition to the serialization functions themselves, IR types are registered with cereal, which
 * is required for polymorphic types. All serialized types must be registered with
 * CEREAL_REGISTER_TYPE. Parent classes that are *not* serialized must be reported with the
 * CEREAL_REGISTER_POLYMORPHIC_RELATION macro.
 */

template <class Archive>
void serialize(Archive& ar, Build& b) {
  ar(b._root, b._stdin_ref, b._stdin, b._stdout_ref, b._stdout, b._stderr_ref, b._stderr);
}

template <class Archive>
void serialize(Archive& ar, Command& c) {
  ar(c._exe, c._args, c._initial_fds, c._steps);
}

template <class Archive>
void serialize(Archive& ar, FileDescriptor& fd) {
  ar(fd._ref, fd._artifact, fd._writable, fd._cloexec);
}

template <class Archive>
void serialize(Archive& ar, Artifact& a) {
  ar(a._path, a._versions);
}

template <class Archive>
void serialize(Archive& ar, Artifact::Version& v) {
  ar(v.metadata, v.fingerprint, v.saved);
}

template <class Archive>
void serialize(Archive& ar, struct stat& s) {
  ar(s.st_mode, s.st_uid, s.st_gid, s.st_size, s.st_mtime, s.st_ctime);
}

template <class Archive>
void serialize(Archive& ar, Artifact::VersionRef& v) {
  ar(v._artifact, v._index);
}

// Polymorphic relations with non-serialized parent types must be registered as well
CEREAL_REGISTER_POLYMORPHIC_RELATION(Step, Reference)

CEREAL_REGISTER_TYPE(Reference::Pipe)
CEREAL_REGISTER_POLYMORPHIC_RELATION(Reference, Reference::Pipe)
template <class Archive>
void serialize(Archive& ar, Reference::Pipe& p) {
  // No fields to serialize here
}

CEREAL_REGISTER_TYPE(Reference::Access)
CEREAL_REGISTER_POLYMORPHIC_RELATION(Reference, Reference::Access)
template <class Archive>
void serialize(Archive& ar, Reference::Access& a) {
  ar(a._path, a._flags);
}

template <class Archive>
void serialize(Archive& ar, Reference::Access::Flags& f) {
  ar(f.r, f.w, f.x, f.nofollow, f.truncate, f.create, f.exclusive);
}

CEREAL_REGISTER_POLYMORPHIC_RELATION(Step, Predicate)

CEREAL_REGISTER_TYPE(Predicate::IsOK)
CEREAL_REGISTER_POLYMORPHIC_RELATION(Predicate, Predicate::IsOK)
template <class Archive>
void serialize(Archive& ar, Predicate::IsOK& p) {
  ar(p._ref);
}

CEREAL_REGISTER_TYPE(Predicate::IsError)
CEREAL_REGISTER_POLYMORPHIC_RELATION(Predicate, Predicate::IsError)
template <class Archive>
void serialize(Archive& ar, Predicate::IsError& p) {
  ar(p._ref, p._err);
}

CEREAL_REGISTER_TYPE(Predicate::MetadataMatch)
CEREAL_REGISTER_POLYMORPHIC_RELATION(Predicate, Predicate::MetadataMatch)
template <class Archive>
void serialize(Archive& ar, Predicate::MetadataMatch& p) {
  ar(p._ref, p._version);
}

CEREAL_REGISTER_TYPE(Predicate::ContentsMatch)
CEREAL_REGISTER_POLYMORPHIC_RELATION(Predicate, Predicate::ContentsMatch)
template <class Archive>
void serialize(Archive& ar, Predicate::ContentsMatch& p) {
  ar(p._ref, p._version);
}

CEREAL_REGISTER_POLYMORPHIC_RELATION(Step, Action)

CEREAL_REGISTER_TYPE(Action::Launch)
CEREAL_REGISTER_POLYMORPHIC_RELATION(Action, Action::Launch)
template <class Archive>
void serialize(Archive& ar, Action::Launch& a) {
  ar(a._cmd);
}

CEREAL_REGISTER_TYPE(Action::SetMetadata)
CEREAL_REGISTER_POLYMORPHIC_RELATION(Action, Action::SetMetadata)
template <class Archive>
void serialize(Archive& ar, Action::SetMetadata& a) {
  ar(a._ref, a._version);
}

CEREAL_REGISTER_TYPE(Action::SetContents)
CEREAL_REGISTER_POLYMORPHIC_RELATION(Action, Action::SetContents)
template <class Archive>
void serialize(Archive& ar, Action::SetContents& a) {
  ar(a._ref, a._version);
}
