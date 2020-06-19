#pragma once

#include <unistd.h>

#include "artifacts/FileArtifact.hh"

class PipeArtifact final : public FileArtifact {
 public:
  using FileArtifact::FileArtifact;

  virtual string getTypeName() const noexcept override { return "Pipe"; }

  void open() noexcept {
    if (_read_fd == -1 && _write_fd == -1) {
      int pipefds[2];
      pipe(pipefds);
      _read_fd = pipefds[0];
      _write_fd = pipefds[1];
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