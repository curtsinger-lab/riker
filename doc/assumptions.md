We assume that
- Builds are referentially transparent (i.e. they give the same results on the same inputs)
  - This enables the use of checksums.
  - This may be tricky for systems that are not deterministic, which can happen due to compiler problems.
- Builds are idempotent (i.e. they can be run twice and will give the same result)
  - This allows us to integrate with existing incremental compilation solutions.
- Builds that depend on the nonexistence of a nonexistent file will either read the contents of the containing directory
  or attempt to open said nonexistent file. (They must not only stat or access the file.)
- All inter-process communication happens through or at least is triggered by either the filesystem or a pipe.
  - We ignore many other forms of communication, as described in our [syscall list](../src/syscalls.h).
- If a process conditionally writes to a file, either
  - the inputs that determine whether the conditional write happens are either written or read by a parent process, or
  - the conditional write is to a file that is only read by a descendant process if at all.
- Processes do not operate on nonexistent file descriptors.
