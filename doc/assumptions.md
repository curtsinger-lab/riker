We assume that
- Builds are referentially transparent (i.e. they give the same results on the same inputs)
  - This enables the use of checksums.
  - This may be tricky for systems that are not deterministic, which can happen due to compiler problems.
- Builds are idempotent (i.e. they can be run twice and will give the same result)
  - This allows us to integrate with existing incremental compilation solutions.
- Builds only depend on the content of files, not the metadata. This means that the mere existence of one file cannot trigger different behavior; only the reading of said file
  - This allows us to ignore, e.g. `stat`. FIXME: This is complicated because we do want to correctly capture and replay changes to file permissions
- All inter-process communication happens through or at least is triggered by either the filesystem or a pipe.
  - We ignore many other forms of communication, as described in our [syscall list](../src/syscalls.h).
