# Notes on Artifact Versioning
By default, we create a dependency on the latest version of an artifact any time it is read (or its metadata is accessed). Likewise, we create a new version of the artifact any time it is written (or its metadata is changed).

This approach is correct, but produces a large number of unnecessary dependencies and versions. Dealing with unnecessary dependencies is not too difficult; the code that checks for versions should use caching, which will avoid repeated hashing of files on disk. Creating unnecessary version is more of a problem: we do not want to fingerprint or save copies of files in intermediate versions. We will either need logic to decide when to skip fingerprints/saves, or we can add some logic to avoid creating unnecessary versions in the first place.

I'll outline the logic for versioning here:

## Accessing an Artifact
When a command accesses an artifact, we look at the latest version of that artifact. If the latest version was created by the same command that is accessing the artifact, there is no need to create a dependency. This is safe because we will never separate the actions that created this version from the dependency on that version; we can only run the entire command.

If a command accesses an artifact whose latest version was created by a different command, we do have to create a dependency. This is also true for versions with no creator, e.g. files that exist on the system prior to the build.

Repeated accesses to the same version of an artifact could also be elided, but that seems like a separate optimization.

## Changing an Artifact
When a command modifies an artifact, there is an implicit dependency on the latest version of that artifact. That is because the contents of the artifact are not independent of the prior version, except in cases where the entire artifact is overwritten. We will only implement this exception for truncation of files, but it could potentially be employed for large writes as well. In any case, this implicit dependency is subject to the same access versioning rules as regular reads.

Writing to an artifact does not need to create a new version of that artifact if the previous version was never accessed. In other words, we can think of combining a series of modifications of an artifact into a single modification, which creates a single new version. Because of the implicit access before the modification, only sequential writes from the same command will be combined in this way; interleaved writes by different commands will create accesses, preventing the write combining.

## Versioning Contents and Metadata Separately
The easiest way to deal with artifact contents and metadata is to think of them all as contents. Any access, whether to metadata or contents, will be treated the same. Likewise, any modification is equivalent, regardless of whether it modified contents or metadata. I'll start off with this implementation because it is easy to reason about.

## A Problem
Consider the following sequence of operations:
1. Chmod file F to add write permissions
2. Access file F in -w- mode. Access is successful.
3. Chmod file F to remove write permission
4. Access file F in r-- mode. Access is successful.

The two chmod actions set the metadata for file F, but we should not combine these modifications. When we re-examine this sequence to decide whether accesses would yield the same results, we would change the outcome of access 2 if we merged the two chmod actions into a single new version. To prevent this, we can think of any access to a file as a dependency on its metadata. This doesn't require any IR changes (the access itself is the reference) but it does mean we need to mark the version created by action 1 as accessed to prevent write combining.

An alternative approach would be to skip write combining entirely. Instead, we could use the logic above to decide which versions need fingerprints and copies. This doesn't seem obviously worse.

Maybe the best argument for not implementing write combining involves directory permissions:
1. Chmod directory /foo to add execute permissions
2. Access file /foo/bar/baz/bing/bang/boom. Access is successful
3. Chmod directory /foo to remove execute permissions

To prevent combination of writes 1 and 3, we would have to traverse the entire path in access 2 and mark the current version of every directory at each level as accessed, as well as the final file.

For now, I will set this aside and just work on a naive implementation that almost certainly does a ridiculous number of unnecessary fingerprints or saves. That will be easier to clean up than a mistake in implementing this optimization.
