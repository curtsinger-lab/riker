#pragma once

#include <unistd.h>

#include "artifacts/FileArtifact.hh"

class PipeArtifact final : public FileArtifact {
 public:
  using FileArtifact::FileArtifact;

  /************ Core Artifact Operations ************/

  /// Get the name of this artifact type
  virtual string getTypeName() const noexcept override { return "Pipe"; }

  // TODO: add other core methods once this is branched off from File

  /************ Miscellaneous ************/
  void open() noexcept {
    if (_read_fd == -1 && _write_fd == -1) {
      int pipefds[2];
      int rc = pipe2(pipefds, O_CLOEXEC);
      ASSERT(rc == 0) << "Failed to create pipe";
      _read_fd = pipefds[0];
      _write_fd = pipefds[1];
      LOG(exec) << "Created pipe with read fd " << _read_fd << " and write fd " << _write_fd;
    }
  }

  void setFDs(int read_fd, int write_fd) {
    _read_fd = read_fd;
    _write_fd = write_fd;
  }

  int getWriteFD() noexcept {
    open();
    return _write_fd;
  }

  int getReadFD() noexcept {
    open();
    return _read_fd;
  }

 private:
  int _read_fd = -1;
  int _write_fd = -1;
};