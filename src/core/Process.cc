#include "core/Process.hh"

#include <cstdio>
#include <list>
#include <memory>
#include <string>
#include <utility>

#include "core/BuildGraph.hh"
#include "core/Command.hh"
#include "core/File.hh"
#include "core/FileDescriptor.hh"

Process::Process(pid_t thread_id, std::string cwd, Command* command) :
    thread_id(thread_id),
    _command(command),
    _cwd(cwd) {}

void Process::traceChdir(std::string newdir) { _cwd = newdir; }

void Process::traceChroot(std::string newroot) { _root = newroot; }

void Process::traceClose(int fd) {
  fds.erase(fd);
}

std::shared_ptr<Process> Process::traceFork(pid_t child_pid) {
  auto child_proc = std::make_shared<Process>(child_pid, _cwd, _command);
  child_proc->fds = fds;
  return child_proc;
}

void Process::traceExec(BuildGraph& trace, std::string executable,
                        const std::list<std::string>& args) {
  _command = _command->createChild(executable, args);

  // Close all cloexec file descriptors
  for (auto fd_entry = fds.begin(); fd_entry != fds.end();) {
    if (fd_entry->second.cloexec) {
      fd_entry = fds.erase(fd_entry);
    } else {
      ++fd_entry;
    }
  }

  // Close all mmaps, since the address space is replaced
  for (auto file : mmaps) {
    file->removeMmap(shared_from_this());
  }

  // Mark the initial open file descriptors
  for (auto it = fds.begin(); it != fds.end(); ++it) {
    it->second.file = trace.getLatestVersion(it->second.location_index);
  }
  _command->setInitialFDs(fds);

  // Assume that we can at any time write to stdout or stderr
  // TODO: Instead of checking whether we know about stdout and stderr,
  // tread the initial stdout and stderr properly as pipes
  if (fds.find(fileno(stdout)) != fds.end()) {
    trace.add_mmap(this->thread_id, fileno(stdout));
  }
  if (fds.find(fileno(stderr)) != fds.end()) {
    trace.add_mmap(this->thread_id, fileno(stderr));
  }
}

void Process::traceExit() {
  for (auto f : mmaps) {
    f->removeMmap(shared_from_this());
  }
}
