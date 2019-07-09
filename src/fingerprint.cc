#include <kj/common.h>
#include <string>
#include <iostream>
#include <fstream>

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include "db.capnp.h"
#include "fingerprint.h"

// Assumes that the file path is already entered, returns whether
// the file was successfully fingerprinted
void set_fingerprint(db::File::Builder file) {
    // We can only fingerprint regular files for now. (Do we even want to try for others?)
    if (file.getType() != db::FileType::REGULAR) {
        file.setFingerprintType(db::FingerprintType::UNAVAILABLE);
        return;
    }

    auto path_string = std::string(file.getPath().asChars().begin(), file.getPath().size());
    struct stat stat_info;
    if (stat(path_string.c_str(), &stat_info) != 0) {
        if (errno == ENOENT) {
            file.setFingerprintType(db::FingerprintType::NONEXISTENT);
        } else {
            file.setFingerprintType(db::FingerprintType::UNAVAILABLE);
        }
        return;
    }

    file.setSize((uint64_t) stat_info.st_size);
    auto mod_time = file.initModificationTime();
    mod_time.setSecondsSinceEpoch(stat_info.st_mtim.tv_sec);
    mod_time.setNanoseconds(stat_info.st_mtim.tv_nsec);
    file.setInode(stat_info.st_ino);

    std::ifstream file_data;
    file_data.open(path_string.c_str(), std::ios::in | std::ios::binary);
    if (!file_data.good()) {
        file.setFingerprintType(db::FingerprintType::METADATA_ONLY);
        return;
    }
    // FIXME: Actually support checksumming
    file.setFingerprintType(db::FingerprintType::METADATA_ONLY);
    return;
}

bool match_fingerprint(db::File::Reader file) {
    // First, check if the fingerprint type is something we recognize. If not, we conservatively
    // assume a change.
    switch (file.getFingerprintType()) {
        case db::FingerprintType::UNAVAILABLE:
        case db::FingerprintType::NONEXISTENT:
        case db::FingerprintType::METADATA_ONLY:
            break;
        default:
            return false;
    }

    // If there is no fingerprint, assume no match
    if (file.getFingerprintType() == db::FingerprintType::UNAVAILABLE) {
        return false;
    }

    // All the rest of the fingerprint types require metadata.
    auto path_string = std::string(file.getPath().asChars().begin(), file.getPath().size());
    struct stat stat_info;
    if (stat(path_string.c_str(), &stat_info) != 0) {
        return (errno == ENOENT && file.getFingerprintType() == db::FingerprintType::NONEXISTENT);
    }

    // If the size is different, then the file is definitely different.
    if ((uint64_t)stat_info.st_size != file.getSize()) {
        return false;
    }
    // Otherwise, if the rest of the metadata matches, then we assume no change.
    if (file.getModificationTime().getSecondsSinceEpoch() == stat_info.st_mtim.tv_sec &&
              file.getModificationTime().getNanoseconds() == stat_info.st_mtim.tv_nsec &&
                                          file.getInode() == stat_info.st_ino) {
        return true;
    }

    // If all we have to go on is the metadata, and the metadata is different, we have
    // to assume that the change is real.
    if (file.getFingerprintType() == db::FingerprintType::METADATA_ONLY) {
        return false;
    }

    // Otherwise, avoid false negatives as commonly happen with insubstantial input changes
    // by checking against a checksum.
    // FIXME: Actually checksum the file
    return false;
}
