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
      return make_shared<Trace>();
    }

    // If the version matches, return the root of the build
    return trace;

  } catch (cereal::Exception& e) {
    // If loading failed, reutrn a default trace
    return make_shared<Trace>();
  }
}

#ifdef SERIALIZER_STATS

ofstream* output = nullptr;

struct stats_node {
 public:
  stats_node(string name) : _name(name), _start(output->tellp()) {}

  void addChild(stats_node* c) { _children.push_back(c); }

  void done() { _end = output->tellp(); }

  size_t totalSize() { return _end - _start; }

  size_t selfSize() {
    size_t child_size = 0;
    for (auto c : _children) {
      child_size += c->totalSize();
    }
    return totalSize() - child_size;
  }

  void print() {
    std::map<string, std::pair<size_t, size_t>> totals;
    tally(totals);

    for (auto [name, info] : totals) {
      auto [count, size] = info;
      float avg = (float)size / count;
      LOG << name << ": " << count << " objects, " << size << " bytes (" << avg << " bytes/obj)";
    }
  }

  void tally(std::map<string, std::pair<size_t, size_t>>& totals) {
    auto [count, size] = totals[_name];
    totals[_name] = {count + 1, size + selfSize()};
    for (auto c : _children) {
      c->tally(totals);
    }
  }

 private:
  string _name;
  size_t _start;
  size_t _end;
  list<stats_node*> _children;
};

stats_node* root_stats_node = nullptr;
stats_node* current = nullptr;
std::stack<stats_node*> ancestors;
#endif

// Save a build to a file
void save_trace(string filename, shared_ptr<Trace> trace) noexcept {
  // Open the file for writing. Must pass std::ios::binary!
  ofstream f(filename, std::ios::binary);

#ifdef SERIALIZER_STATS
  output = &f;
#endif

  // Initialize cereal's binary archive writer
  cereal::BinaryOutputArchive archive(f);

  // Store the build
  size_t version = ArchiveVersion;
  archive(version, trace);

#ifdef SERIALIZER_STATS
  output = nullptr;
  root_stats_node->print();
#endif
}

#ifdef SERIALIZER_STATS

#define COLLECT_STATS(C)                                  \
  namespace cereal {                                      \
    void prologue(BinaryOutputArchive& a, C const& val) { \
      if (output == nullptr) return;                      \
      if (current == nullptr) {                           \
        root_stats_node = new stats_node(#C);             \
        current = root_stats_node;                        \
      } else {                                            \
        ancestors.push(current);                          \
        current = new stats_node(#C);                     \
      }                                                   \
    }                                                     \
    void epilogue(BinaryOutputArchive& a, C const& val) { \
      if (output == nullptr) return;                      \
      current->done();                                    \
      if (ancestors.empty()) {                            \
        current = nullptr;                                \
      } else {                                            \
        auto child = current;                             \
        current = ancestors.top();                        \
        ancestors.pop();                                  \
        current->addChild(child);                         \
      }                                                   \
    }                                                     \
  }

COLLECT_STATS(Command);
COLLECT_STATS(AccessFlags);
COLLECT_STATS(FileDescriptor);
COLLECT_STATS(Version);
COLLECT_STATS(Pipe);
COLLECT_STATS(Access);
COLLECT_STATS(RefResult);
COLLECT_STATS(MetadataMatch);
COLLECT_STATS(ContentsMatch);
COLLECT_STATS(SetMetadata);
COLLECT_STATS(SetContents);
COLLECT_STATS(Launch);

#endif

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

// Refs
CEREAL_REGISTER_TYPE(Pipe);
CEREAL_REGISTER_TYPE(File);
CEREAL_REGISTER_TYPE(Symlink);
CEREAL_REGISTER_TYPE(Dir);
CEREAL_REGISTER_TYPE(Access);

// Predicates
CEREAL_REGISTER_TYPE(ExpectResult);
CEREAL_REGISTER_TYPE(MatchMetadata);
CEREAL_REGISTER_TYPE(MatchContent);

// Actions
CEREAL_REGISTER_TYPE(Launch);
CEREAL_REGISTER_TYPE(Join);
CEREAL_REGISTER_TYPE(UpdateMetadata);
CEREAL_REGISTER_TYPE(UpdateContent);
CEREAL_REGISTER_TYPE(Exit);