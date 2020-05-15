#pragma once

enum class FingerprintLevel { None, Local, All };

// Namespace to contain global flags that control build behavior
namespace options {

/// Should the build print commands as they are run?
inline bool print_on_run = false;

/// Is this a dry run?
inline bool dry_run = false;

/****** Optimization ******/
/// Any command can read the effects of its own writes without versioning or dependencies
inline bool ignore_self_reads = true;  // PAPER

/// Repeated writes by the same command with no interleaved read can be combined
inline bool combine_writes = true;  // PAPER

/// Skip repeated checks of the contents or metadata for the same reference
inline bool skip_repeat_checks = true;  // PAPER

/// Enable file-staging cache
inline bool enable_cache = false;  // PAPER

};  // namespace options