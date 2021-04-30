#include <memory>

#include "artifacts/Artifact.hh"
#include "runtime/Command.hh"
#include "runtime/Ref.hh"

class MetadataVersion;
class ContentVersion;

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
  virtual void expectResult(const std::shared_ptr<Command>& command,
                            Scenario scenario,
                            Ref::ID ref,
                            int expected) noexcept override {
    if (scenario & Scenario::Build) {
      auto post_build = command->getRef(ref)->getResultCode();
      if (post_build == expected) {
        Next::expectResult(command, Scenario::Both, ref, expected);
      } else {
        Next::expectResult(command, Scenario::Build, ref, expected);
        Next::expectResult(command, Scenario::PostBuild, ref, post_build);
      }
    }
  }

  /// Handle a MatchMetadata IR step
  virtual void matchMetadata(const std::shared_ptr<Command>& command,
                             Scenario scenario,
                             Ref::ID ref,
                             std::shared_ptr<MetadataVersion> expected) noexcept override {
    if (scenario & Scenario::Build) {
      // Did the reference resolve in the post-build state?
      if (command->getRef(ref)->isResolved()) {
        // Yes. Grab the outcome from the post-build match.
        const auto& post_build = command->getRef(ref)->getArtifact()->peekMetadata();

        // Is the post-build version match the version from during the build?
        if (post_build->matches(expected)) {
          // Yes. Emit a single predicate for both scenarios
          Next::matchMetadata(command, Scenario::Both, ref, expected);
        } else {
          // No. Emit separate predicates for each scenario
          Next::matchMetadata(command, Scenario::Build, ref, expected);
          Next::matchMetadata(command, Scenario::PostBuild, ref, post_build);
        }
      } else {
        // The reference did not resolve post build, so just emit a Build predicate
        Next::matchMetadata(command, Scenario::Build, ref, expected);
      }
    }
  }

  /// Handle a MatchContent IR step
  virtual void matchContent(const std::shared_ptr<Command>& command,
                            Scenario scenario,
                            Ref::ID ref,
                            std::shared_ptr<ContentVersion> expected) noexcept override {
    if (scenario & Scenario::Build) {
      // Did the reference resolve in the post-build state?
      if (command->getRef(ref)->isResolved()) {
        // Yes. Grab the outcome from the post-build match
        const auto& post_build = command->getRef(ref)->getArtifact()->peekContent();

        // Does the post-build version match the version from during the build?
        if (post_build->matches(expected)) {
          // Yes. Emit a single predicate for both scenarios
          Next::matchContent(command, Scenario::Both, ref, expected);

        } else {
          // No. Emit separate predicates for each scenario
          Next::matchContent(command, Scenario::Build, ref, expected);
          Next::matchContent(command, Scenario::PostBuild, ref, post_build);
        }
      } else {
        // The reference did not resolve post build, so just emit a Build predicate
        Next::matchContent(command, Scenario::Build, ref, expected);
      }
    }
  }
};
