#include "Dir.hh"

#include <filesystem>
#include <memory>
#include <string>
#include <tuple>

#include "build/Env.hh"
#include "core/IR.hh"

using std::make_shared;
using std::shared_ptr;
using std::string;
using std::tuple;

namespace fs = std::filesystem;

tuple<shared_ptr<Artifact>, int> DirArtifact::resolvePath(shared_ptr<Command> c,
                                                          shared_ptr<Access> ref) noexcept {
  return _env.getFile(c, ref);
}
