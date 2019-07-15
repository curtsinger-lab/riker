#include <kj/common.h>
#include <string>
#include <iostream>
#include <fstream>

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>

#include "util.h"
#include "db.capnp.h"
#include "blake2-wrapper.h"
#include "fingerprint.h"

// Assumes that the file path is already entered, returns whether
// the file was successfully fingerprinted
void set_fingerprint(db::File::Builder file, bool use_checksum) {
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

    if (!use_checksum || (stat_info.st_mode & S_IFMT) != S_IFREG) {
        // Only checksum regular files
        file.setFingerprintType(db::FingerprintType::METADATA_ONLY);
        return;
    }

    int file_fd = open(path_string.c_str(), O_RDONLY | O_CLOEXEC);
    if (file_fd < 0) { // Error opening
        file.setFingerprintType(db::FingerprintType::METADATA_ONLY);
        return;
    }

    auto checksum = file.initChecksum(BLAKE2S_OUTBYTES);
    blake2sp_state hash_state;
    blake2sp_init(&hash_state, BLAKE2S_OUTBYTES);
    char buffer[1 << 13];
    while (true) {
        auto bytes_read = read(file_fd, buffer, ARRAY_COUNT(buffer));
        if (bytes_read > 0) {
            blake2sp_update(&hash_state, buffer, bytes_read);
        } else if (bytes_read == 0) { // EOF
            blake2sp_final(&hash_state, checksum.begin(), checksum.size());
            file.setFingerprintType(db::FingerprintType::BLAKE2SP);
            return;
        } else { // Error reading
            file.setFingerprintType(db::FingerprintType::METADATA_ONLY);
            return;
        }
    }
}

bool match_fingerprint(db::File::Reader file) {
    // First, check if the fingerprint type is something we recognize. If not, we conservatively
    // assume a change.
    switch (file.getFingerprintType()) {
    case db::FingerprintType::UNAVAILABLE:
    case db::FingerprintType::NONEXISTENT:
    case db::FingerprintType::METADATA_ONLY:
    case db::FingerprintType::BLAKE2SP:
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
    int file_fd = open(path_string.c_str(), O_RDONLY | O_CLOEXEC);
    if (file_fd < 0) { // Error opening
        return false;
    }

    size_t hash_size = file.getChecksum().size();
    switch (file.getFingerprintType()) {
    case db::FingerprintType::BLAKE2SP: {
        KJ_STACK_ARRAY(kj::byte, checksum, hash_size, hash_size, hash_size);
        blake2sp_state hash_state;
        blake2sp_init(&hash_state, hash_size);
        char buffer[1 << 13];
        while (true) {
            auto bytes_read = read(file_fd, buffer, ARRAY_COUNT(buffer));
            if (bytes_read > 0) {
                blake2sp_update(&hash_state, buffer, bytes_read);
            } else if (bytes_read == 0) { // EOF
                blake2sp_final(&hash_state, checksum.begin(), checksum.size());
                return checksum.asConst() == file.getChecksum();
            } else { // Error reading
                return false;
            }
        }
    }
    default:
        return false;
    }
}
