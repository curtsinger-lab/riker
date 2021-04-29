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

string open_tempfile() noexcept {
  char tmpname[] = "/tmp/tmpfileXXXXXX";
  int fd = mkstemp(tmpname);
  close(fd);
  return tmpname;
}

// Create an IRBuffer with a temporary file to hold buffered steps
IRBuffer::IRBuffer() noexcept : _out(open_tempfile(), std::ios::binary), _archive(_out) {}

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
