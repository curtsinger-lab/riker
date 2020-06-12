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
#include "core/Command.hh"
#include "core/FileDescriptor.hh"
#include "core/IR.hh"
#include "util/log.hh"
#include "versions/ContentVersion.hh"
#include "versions/MetadataVersion.hh"
#include "versions/Version.hh"

using std::ifstream;
using std::ofstream;
using std::string;

enum : size_t { ArchiveVersion = 13 };

/// Try to load a build. Exit with an error if loading fails.
shared_ptr<Command> load_build(string filename, bool default_fallback) noexcept {
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
void save_build(string filename, shared_ptr<Command> root) noexcept {
  // Open the file for writing. Must pass std::ios::binary!
  ofstream f(filename, std::ios::binary);

#ifdef SERIALIZER_STATS
  output = &f;
#endif

  // Initialize cereal's binary archive writer
  cereal::BinaryOutputArchive archive(f);

  // Store the build
  size_t version = ArchiveVersion;
  archive(version, root);

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
COLLECT_STATS(ReferenceResult);
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

/** Register types and polymorphic relationships **/

// Versions
CEREAL_REGISTER_TYPE(MetadataVersion);
CEREAL_REGISTER_TYPE(ContentVersion);

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
