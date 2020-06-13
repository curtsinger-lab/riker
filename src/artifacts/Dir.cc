#include "Dir.hh"

#include <filesystem>
#include <memory>
#include <string>
#include <tuple>

#include "build/Env.hh"
#include "core/IR.hh"
#include "util/log.hh"

using std::make_shared;
using std::shared_ptr;
using std::string;
using std::tuple;

namespace fs = std::filesystem;

tuple<shared_ptr<Artifact>, int> DirArtifact::resolvePath(shared_ptr<Command> c,
                                                          shared_ptr<Access> ref) noexcept {
  // INFO << "Resolving path " << ref->getRelativePath() << " relative to "
  //     << ref->getBase()->getFullPath();

  auto subref = ref->getBase();

  const auto& fullpath = ref->getRelativePath();

  if (fullpath.empty()) return {shared_from_this(), SUCCESS};

  auto iter = fullpath.begin();

  shared_ptr<Artifact> a;
  int rc;

  while (iter != fullpath.end()) {
    auto part = *iter;
    iter++;

    if (iter == fullpath.end()) {
      subref = make_shared<Access>(subref, part, ref->getFlags());
    } else {
      subref = make_shared<Access>(subref, part, AccessFlags{.x = true});
    }

    std::tie(a, rc) = _env.getFile(c, subref);
  }

  return tuple(a, rc);
}
