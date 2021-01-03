#pragma once

#include "artifacts/Artifact.hh"
#include "runtime/Command.hh"
#include "versions/Version.hh"

/// Returns true iff the version is fingerprintable
bool isFingerprintable(shared_ptr<Command> c,
                       shared_ptr<std::optional<fs::path>> p,
                       shared_ptr<Version> v);

/// Returns true iff the version is cachable
bool isCachable(shared_ptr<Command> c,
                shared_ptr<std::optional<fs::path>> p,
                shared_ptr<Version> v);