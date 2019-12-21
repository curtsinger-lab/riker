#pragma once

#include <ostream>
#include <string>
#include <tuple>

using std::ostream;
using std::string;

/*
 * A command uses a collection of references to artifacts. Normal
 *
 * Each path reference is a string path, as well as any flags that influence path resolution.
 * These flags are:
 *   nofollow: determines whether a reference will access a symlink or the linked path
 *   are there others?
 *
 * A command also has one or more interactions with each path reference.
 * Interaction types are:
 *   Resolve: follow this path to an artifact with a set of permissions. This could fail.
 *            Resolution could create the artifact, depending on flags to open.
 *   Read: read the artifact this path resolves to. We'll record the version that was read.
 *   Write: write the artifact this path resolves to. We'll record the version this creates.
 *   Truncate: truncate the artifact this path resolves to. We'll record the version this creates.
 *   Link: link an artifact at this path
 *   Unlink: unlink the artifact at this path.
 *
 * At the end of an entire build, we'll look at how every path reference in every command resolves
 * and save that final state. On a future build, we will check to see if a command's references
 * match their final states from the last build. If they do, the command does not need to run.
 *
 * For input-only files, this logic is reasonable; none of the inputs are changed by the build, so
 * the final state is also the initial state. Changing inputs to a command would rerun the
 * command.
 *
 * For output-only files, we'd rerun a command if its output no longer exists (e.g. the user
 * removed it). We could potentially skip this and just link the file back into place.
 *
 * The tricky case is files that are both modified and read by the build. One command may create a
 * file, a second may read it, and a third may remove it. If the build process is idempotent,
 * rerunning the entire build would bring the file to whatever state it was in at the end of the
 * last build. Of course builds may not actually be idempotent; a build might append a message to
 * a log each time it is run. The log will finish in a different state than it started on each
 * build, but we don't want that change to trigger a rebuild. Recording the final state of a
 * reference means we will only trigger builds when some artifact is changed outside of the build
 * process; changes to artifacts or paths by the build itself will not induce future builds.
 */

/**
 * Representation of a path to a file.
 * Includes both the string part of the path, and whether links are followed. If the final element
 * of the path is a symlink and the path is marked nofollow, the path will resolve to the symlink
 * and not its destination.
 */
class Path {
 public:
  /// Create a path
  explicit Path(string path, bool follow_links = true) : _path(path), _follow_links(follow_links) {}

  /// Get the string portion of the path
  const string& getPath() const { return _path; }

  /// Check if the path follows links
  bool followLinks() const { return _follow_links; }

  /// Check if two paths are equal
  bool operator==(const Path& other) const {
    return std::tie(_path, _follow_links) == std::tie(other._path, other._follow_links);
  }

  /// Check if one path is less than another
  bool operator<(const Path& other) const {
    return std::tie(_path, _follow_links) < std::tie(other._path, other._follow_links);
  }

  /// Check if one path is greater than another
  bool operator>(const Path& other) const {
    return std::tie(_path, _follow_links) > std::tie(other._path, other._follow_links);
  }

  /// Make paths printable
  friend ostream& operator<<(ostream& o, const Path& p) {
    return o << "[Path " << p._path << (p._follow_links ? "" : " nofollow") << "]";
  }

 private:
  /// The string portion of a path
  string _path;

  /// Does this path follow links, o
  bool _follow_links;
};
