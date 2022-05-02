#include <memory>

#include "artifacts/Artifact.hh"
#include "runtime/Command.hh"
#include "runtime/Ref.hh"
#include "versions/ContentVersion.hh"

class MetadataVersion;

/**
 * This class processes a build trace that has already been completed, and adds new predicates to
 * check against the state left at the end of a build.
 *
 * The PostBuildChecker class expects a template parameter that is an IRSink, which will receive all
 * of the original trace steps along with the additional steps for post-build checks. A likely use
 * case would be to instantiate a PostBuildChecker<IRBuffer>.
 */
template <class Next>
class PostBuildChecker : public Next {
 public:
  /// The constructor for a post-build checker passes any arguments along to the next layer
  template <typename... Args>
  PostBuildChecker(Args&&... args) noexcept : Next(std::forward<Args>(args)...) {}

  /// Handle an ExpectResult IR step
  virtual void expectResult(const IRSource& source,
                            const std::shared_ptr<Command>& command,
                            Scenario scenario,
                            Ref::ID ref,
                            int8_t expected) noexcept override {
    if(!command->isLaunched()){
      Next::expectResult(command, scenario, ref, expected);
      return;
    }                         
    if (scenario & Scenario::Build) {
      auto post_build = command->getRef(ref)->getResultCode();
      if (post_build == expected) {
        Next::expectResult(source, command, Scenario::Both, ref, expected);
      } else {
        Next::expectResult(source, command, Scenario::Build, ref, expected);
        Next::expectResult(source, command, Scenario::PostBuild, ref, post_build);
      }
    }
  }

  /// Handle a MatchMetadata IR step
  virtual void matchMetadata(const IRSource& source,
                             const std::shared_ptr<Command>& command,
                             Scenario scenario,
                             Ref::ID ref,
                             MetadataVersion expected) noexcept override {
    if(!command->isLaunched()){
      Next::matchMetadata(command, scenario, ref, expected);
      return;
    } 
    if (scenario & Scenario::Build) {
      // Did the reference resolve in the post-build state?
      if (command->getRef(ref)->isResolved()) {
        // Yes. Grab the outcome from the post-build match.
        auto post_build = command->getRef(ref)->getArtifact()->peekMetadata();

        // Is the post-build version match the version from during the build?
        if (post_build.matches(expected)) {
          // Yes. Emit a single predicate for both scenarios
          Next::matchMetadata(source, command, Scenario::Both, ref, expected);
        } else {
          // No. Emit separate predicates for each scenario
          Next::matchMetadata(source, command, Scenario::Build, ref, expected);
          Next::matchMetadata(source, command, Scenario::PostBuild, ref, post_build);
        }
      } else {
        // The reference did not resolve post build, so just emit a Build predicate
        Next::matchMetadata(source, command, Scenario::Build, ref, expected);
      }
    }
  }

  /// Handle a MatchContent IR step
  virtual void matchContent(const IRSource& source,
                            const std::shared_ptr<Command>& command,
                            Scenario scenario,
                            Ref::ID ref,
                            std::shared_ptr<ContentVersion> expected) noexcept override {
    if(!command->isLaunched()){
      Next::matchContent(command, scenario, ref, expected);
      return;
    } 
    if (scenario & Scenario::Build) {
      // Did the reference resolve in the post-build state?
      if (command->getRef(ref)->isResolved()) {
        // Yes. Grab the outcome from the post-build match
        const auto& post_build = command->getRef(ref)->getArtifact()->peekContent();

        // Does the post-build version match the version from during the build?
        if (post_build->matches(expected)) {
          // Yes. Emit a single predicate for both scenarios
          Next::matchContent(source, command, Scenario::Both, ref, expected);

        } else {
          // No. Emit separate predicates for each scenario
          Next::matchContent(source, command, Scenario::Build, ref, expected);
          Next::matchContent(source, command, Scenario::PostBuild, ref, post_build);
        }
      } else {
        // The reference did not resolve post build, so just emit a Build predicate
        Next::matchContent(source, command, Scenario::Build, ref, expected);
      }
    }
  }
};
