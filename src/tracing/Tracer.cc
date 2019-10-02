#include "Tracer.hh"

#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>

#include "core/BuildGraph.hh"
#include "core/Process.hh"

void Tracer::newProcess(pid_t pid, std::shared_ptr<Command> cmd) {
  Process* proc = new Process(pid, ".", cmd);  // TODO: Fix cwd handling
  proc->setDefaultFds(_graph.getStdin(), _graph.getStdout(), _graph.getStderr());
  _processes.emplace(pid, proc);
}

void Tracer::traceChdir(pid_t pid, std::string path) {
  _processes[pid]->traceChdir(path);
}

void Tracer::traceChroot(pid_t pid, std::string path) {
  _processes[pid]->traceChroot(path);
}

void Tracer::traceFork(pid_t pid, pid_t child_pid) {
  _processes[child_pid] = _processes[pid]->traceFork(child_pid);
}

void Tracer::traceClone(pid_t pid, pid_t thread_id) {
  // Threads in the same process just appear as pid references to the same process
  _processes[thread_id] = _processes[pid];
}

void Tracer::traceExec(pid_t pid, std::string executable, const std::list<std::string>& args) {
  _processes[pid]->traceExec(*this, _graph, executable, args);

  auto f = _graph.getFile(executable);

  _processes[pid]->traceRead(f);
}

void Tracer::traceExit(pid_t pid) {
  _processes[pid]->traceExit();
}

void Tracer::traceOpen(pid_t pid, int fd, std::string path, int flags, mode_t mode) {
  auto f = _graph.getFile(path);
  _processes[pid]->traceOpen(fd, f, flags, mode);
}

void Tracer::traceClose(pid_t pid, int fd) {
  _processes[pid]->traceClose(fd);
}

void Tracer::tracePipe(pid_t pid, int fds[2], bool cloexec) {
  auto proc = _processes[pid];

  auto f = _graph.getPipe(proc->getCommand());

  proc->tracePipe(fds[0], fds[1], f, cloexec);
}

void Tracer::traceDup(pid_t pid, int duped_fd, int new_fd, bool cloexec) {
  _processes[pid]->traceDup(duped_fd, new_fd, cloexec);
}

void Tracer::traceSetCloexec(pid_t pid, int fd, bool cloexec) {
  _processes[pid]->traceSetCloexec(fd, cloexec);
}

void Tracer::traceMmap(pid_t pid, int fd) {
  _processes[pid]->traceMmap(fd);
}

void Tracer::traceRead(pid_t pid, struct file_reference& file) {
  if (file.fd == AT_FDCWD) {
    auto f = _graph.getFile(file.path);
    _processes[pid]->traceRead(f);

  } else {
    _processes[pid]->traceRead(file.fd);
  }
}

void Tracer::traceModify(pid_t pid, struct file_reference& file) {
  if (file.fd == AT_FDCWD) {
    auto f = _graph.getFile(file.path);
    _processes[pid]->traceModify(f);

  } else {
    _processes[pid]->traceModify(file.fd);
  }
}

void Tracer::traceCreate(pid_t pid, struct file_reference& file) {
  if (file.fd == AT_FDCWD) {
    auto f = _graph.getFile(file.path);
    _processes[pid]->traceCreate(f);

  } else {
    _processes[pid]->traceCreate(file.fd);
  }
}

void Tracer::traceRemove(pid_t pid, struct file_reference& file) {
  if (file.fd == AT_FDCWD) {
    auto f = _graph.getFile(file.path);
    _processes[pid]->traceRemove(f);

  } else {
    _processes[pid]->traceRemove(file.fd);
  }
}
