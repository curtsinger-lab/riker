#include "IRBuffer.hh"

#include <cstdlib>
#include <cstring>
#include <memory>
#include <sstream>
#include <string>

#include "util/log.hh"

using std::shared_ptr;
using std::string;
using std::stringstream;

static int open_tempfile() noexcept {
  // Open an anonymous temporary file that cannot be linked to the filesystem
  int fd = ::open("/tmp", O_RDWR | O_TMPFILE | O_EXCL, 0600);
  FAIL_IF(fd < 0) << "Failed to create temporary file: " << ERR;
  return fd;
}

static string get_fd_path(int fd) {
  stringstream ss;
  ss << "/proc/self/fd/" << fd;
  return ss.str();
}

// Create an IRBuffer with a temporary file to hold buffered steps
IRBuffer::IRBuffer() noexcept :
    _fd(open_tempfile()), _out(get_fd_path(_fd), std::ios::binary), _archive(_out) {}

IRBuffer::~IRBuffer() noexcept {
  ::close(_fd);
}

/// Send the stored IR trace to a sink
void IRBuffer::sendTo(IRSink& handler) noexcept {
  // Set the buffer to draining mode
  _draining = true;

  // Send steps while the list is not empty
  while (!_steps.empty()) {
    _steps.front()(handler);
    _steps.pop_front();
  }

  // Now the buffer can fill again
  _draining = false;
}

/// Identify a command with a given ID
void IRBuffer::addCommand(Command::ID id, shared_ptr<Command> cmd) noexcept {
  IRLoader::addCommand(id, cmd);
}

/// Add a MetadataVersion with a known ID to this input trace
void IRBuffer::addMetadataVersion(MetadataVersion::ID id, shared_ptr<MetadataVersion> mv) noexcept {
  IRLoader::addMetadataVersion(id, mv);
}

/// Add a ContentVersion with a known ID to this input trace
void IRBuffer::addContentVersion(ContentVersion::ID id, shared_ptr<ContentVersion> cv) noexcept {
  IRLoader::addContentVersion(id, cv);
}
