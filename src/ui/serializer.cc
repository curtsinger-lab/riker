#include "serializer.hh"

#include <fstream>
#include <string>

#include <cereal/archives/binary.hpp>
#include <cereal/types/list.hpp>
#include <cereal/types/map.hpp>
#include <cereal/types/memory.hpp>
#include <cereal/types/polymorphic.hpp>
#include <cereal/types/vector.hpp>

#include "core/Artifact.hh"
#include "core/BuildGraph.hh"
#include "core/Command.hh"
#include "core/FileDescriptor.hh"
#include "core/IR.hh"

using std::ifstream;
using std::ofstream;
using std::string;

void save_build(string filename, BuildGraph& graph) {
  ofstream f(filename, std::ios::binary);
  cereal::BinaryOutputArchive archive(f);
  archive(graph);
}

bool load_build(string filename, BuildGraph& graph) {
  ifstream f(filename, std::ios::binary);
  cereal::BinaryInputArchive archive(f);

  try {
    archive(graph);
    return true;
  } catch (cereal::Exception e) {
    return false;
  }
}

template <class Archive>
void serialize(Archive& ar, BuildGraph& g) {
  ar(g._root, g._stdin_ref, g._stdin, g._stdout_ref, g._stdout, g._stderr_ref, g._stderr);
}

template <class Archive>
void serialize(Archive& ar, Command& c) {
  ar(c._id, c._exe, c._args, c._initial_fds, c._steps);
}

template <class Archive>
void serialize(Archive& ar, FileDescriptor& fd) {
  ar(fd._ref, fd._artifact, fd._writable, fd._cloexec);
}

template <class Archive>
void serialize(Archive& ar, Artifact& a) {
  ar(a._id, a._path, a._versions);
}

template <class Archive>
void serialize(Archive& ar, Artifact::Version& v) {
  ar(v.has_metadata, v.metadata, v.has_fingerprint, v.fingerprint);
}

template <class Archive>
void serialize(Archive& ar, struct stat& s) {
  ar(s.st_mode, s.st_uid, s.st_gid, s.st_size, s.st_mtime, s.st_ctime);
}

template <class Archive>
void serialize(Archive& ar, Artifact::VersionRef& v) {
  ar(v._artifact, v._index);
}

CEREAL_REGISTER_TYPE(Reference)
CEREAL_REGISTER_POLYMORPHIC_RELATION(Step, Reference)
template <class Archive>
void serialize(Archive& ar, Reference& r) {
  ar(r._id);
}

CEREAL_REGISTER_TYPE(Reference::Pipe)
template <class Archive>
void serialize(Archive& ar, Reference::Pipe& p) {
  ar(cereal::base_class<Reference>(&p));
}

CEREAL_REGISTER_TYPE(Reference::Access)
template <class Archive>
void serialize(Archive& ar, Reference::Access& a) {
  ar(cereal::base_class<Reference>(&a), a._path, a._flags);
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
