#pragma once

#include <memory>
#include <ostream>
#include <tuple>

#include "runtime/Command.hh"
#include "util/log.hh"

template <class V>
class VersionState {
 public:
  /// Set committed state with known writing command
  void update(std::shared_ptr<V> v) noexcept {
    // Make sure there is no uncommitted state
    _uncommitted_version.reset();
    _uncommitted_writer.reset();

    // Save the committed state
    _committed_version = v;
    _committed_writer.reset();
  }

  /// A command updates this version-managed state
  void update(std::shared_ptr<Command> writer, std::shared_ptr<V> writing) noexcept {
    // Is the command running?
    if (writer->mustRun()) {
      // Reset uncommitted state
      _uncommitted_version.reset();
      _uncommitted_writer.reset();

      // Save the new committed version
      _committed_version = writing;
      _committed_writer = writer;

    } else {
      // Save the new uncommitted version
      _uncommitted_version = writing;
      _uncommitted_writer = writer;
    }
  }

  /// Get the latest version and writer managed by this instance
  std::tuple<std::shared_ptr<V>, std::weak_ptr<Command>> getLatest() const noexcept {
    if (isUncommitted()) {
      return getUncommitted();
    } else {
      return getCommitted();
    }
  }

  /// Get the uncommitted version and writer managed by this state
  std::tuple<std::shared_ptr<V>, std::weak_ptr<Command>> getUncommitted() const noexcept {
    return std::tuple{_uncommitted_version, _uncommitted_writer};
  }

  /// Get the committed version managed by this state
  std::tuple<std::shared_ptr<V>, std::weak_ptr<Command>> getCommitted() const noexcept {
    return std::tuple{_committed_version, _committed_writer};
  }

  /// Check if there is some uncommitted state in place
  bool hasUncommittedState() const noexcept { return _uncommitted_version != nullptr; }

  /// Check if there is some committed state in place (may not be the latest update)
  bool hasCommittedState() const noexcept { return _committed_version != nullptr; }

  /// Check if the latest version managed by this instance is uncommitted
  bool isUncommitted() const noexcept { return hasUncommittedState(); }

  /// Check if the latest version managed by this instance is committed
  bool isCommitted() const noexcept { return !hasUncommittedState(); }

  /// If there is uncommitted state, treat it as committed
  void setCommitted() noexcept {
    if (_uncommitted_version) {
      _committed_version = std::move(_uncommitted_version);
      _committed_writer = std::move(_uncommitted_writer);
    }
  }

  /// Discard any uncommitted state
  void rollback() noexcept {
    _uncommitted_version.reset();
    _uncommitted_writer.reset();
  }

  friend std::ostream& operator<<(std::ostream& o, const VersionState<V>& v) {
    o << "[";

    bool printed = false;

    // Does the version state have an uncommitted version?
    if (v.hasUncommittedState()) {
      o << "uncommitted: " << v._uncommitted_version;
      if (auto writer = v._uncommitted_writer.lock(); writer) {
        o << " written by " << writer;
      }
      printed = true;
    }

    // Does the version state have a committed version?
    if (v.hasCommittedState()) {
      // Print a separator if needed
      if (printed) o << ", ";

      o << "committed: " << v._committed_version;
      if (auto writer = v._committed_writer.lock(); writer) {
        o << " written by " << writer;
      }
    }

    return o << "]";
  }

 private:
  /// The uncommitted version
  std::shared_ptr<V> _uncommitted_version;

  /// The command that wrote the uncommitted version
  std::weak_ptr<Command> _uncommitted_writer;

  /// The committed version
  std::shared_ptr<V> _committed_version;

  /// The command that wrote the committed version
  std::weak_ptr<Command> _committed_writer;
};
