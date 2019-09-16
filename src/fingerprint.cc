#include <kj/common.h>
#include <string>
#include <iostream>
#include <fstream>

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/vfs.h>
#include <linux/magic.h>
#include <unistd.h>
#include <fcntl.h>
#include <dirent.h>

#include "util.hh"
#include "db.capnp.h"
#include "blake2-wrapper.h"
#include "fingerprint.h"

// Filter out db.dodo from the directory listing when fingerprinting directories
static int filter_default(const struct dirent* e) {
  if (std::string(e->d_name) == "db.dodo") return 0;
  else return 1;
}

// For NFS filesystems, filter out .nfs files
static int filter_nfs(const struct dirent* e) {
  if (std::string(e->d_name).compare(0, 4, ".nfs") == 0) return 0;
  else return filter_default(e);
}

// Compute the blake2sp checksum of a directory's contents
// Return true on success
template<typename OutType>
static bool blake2sp_dir(std::string path_string, OutType checksum_output) {
    // Select a directory filter based on filesystem
    auto filter = filter_default;

    // Use statfs to check for NFS
    struct statfs fs;
    if (statfs(path_string.c_str(), &fs) == 0) {
        if (fs.f_type == NFS_SUPER_MAGIC) {
            filter = filter_nfs;
        }
    }

    // Get a sorted list of directory entries
    struct dirent **namelist;
    int entries = scandir(path_string.c_str(), &namelist, filter, alphasort);
    
    // Did reading the directory fail?
    if (entries == -1) {
        return false;
    }
    
    // Start a checksum
    blake2sp_state hash_state;
    blake2sp_init(&hash_state, BLAKE2S_OUTBYTES);
   
    //std::cerr << "Directory " << path_string << " contains:" << std::endl; 

    // Add each directory entry into the checksum
    for(int i=0; i<entries; i++) {
        //std::cerr << "  " << namelist[i]->d_name;

        /*if (namelist[i]->d_type == DT_BLK) {
          std::cerr << " (block device)" << std::endl;
        } else if (namelist[i]->d_type == DT_CHR) {
          std::cerr << " (character device)" << std::endl;
        } else if (namelist[i]->d_type == DT_DIR) {
          std::cerr << " (directory)" << std::endl;
        } else if (namelist[i]->d_type == DT_FIFO) {
          std::cerr << " (named pipe)" << std::endl;
        } else if (namelist[i]->d_type == DT_LNK) {
          std::cerr << " (symbolic link)" << std::endl;
        } else if (namelist[i]->d_type == DT_REG) {
          std::cerr << " (regular file)" << std::endl;
        } else if (namelist[i]->d_type == DT_SOCK) {
          std::cerr << " (socket)" << std::endl;
        } else if (namelist[i]->d_type == DT_UNKNOWN) {
          std::cerr << " (unknown)" << std::endl;
        } else {
          std::cerr << " (unrecognized type)" << std::endl;
        }*/

        // Hash the entry name and null terminator to mark boundaries between entries
        blake2sp_update(&hash_state, namelist[i]->d_name, strlen(namelist[i]->d_name)+1);
        free(namelist[i]);
    }
    
    // Clean up the name list
    free(namelist);
    
    // Finish the checksum
    blake2sp_final(&hash_state, checksum_output.begin(), checksum_output.size());
    return true;
}

// Compute the blake2sp checksum of a file's contents
// Return true on success
template<typename OutType>
static bool blake2sp_file(std::string path_string, OutType checksum_output) {
    int file_fd = open(path_string.c_str(), O_RDONLY | O_CLOEXEC);
    if (file_fd < 0) { // Error opening
        return false;
    }

    blake2sp_state hash_state;
    blake2sp_init(&hash_state, BLAKE2S_OUTBYTES);
    char buffer[1 << 13];
    while (true) {
        auto bytes_read = read(file_fd, buffer, ARRAY_COUNT(buffer));
        if (bytes_read > 0) {
            blake2sp_update(&hash_state, buffer, bytes_read);
        } else if (bytes_read == 0) { // EOF
            blake2sp_final(&hash_state, checksum_output.begin(), checksum_output.size());
            close(file_fd);
            return true;
        } else { // Error reading
            close(file_fd);
            return false;
        }
    }
}

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
    file.setMode(stat_info.st_mode);

    // Skip checksum when requested
    if (!use_checksum) {
        file.setFingerprintType(db::FingerprintType::METADATA_ONLY);
        return;
    }
    
    // Set up to record a checksum
    auto checksum = file.initChecksum(BLAKE2S_OUTBYTES);
    
    // Is this a file or a directory?
    if ((stat_info.st_mode & S_IFMT) == S_IFDIR) {
        // Checksum a directory
        if (blake2sp_dir(path_string, checksum)) {
            file.setFingerprintType(db::FingerprintType::BLAKE2SP);
        } else {
            // Accessing contents failed
            file.setFingerprintType(db::FingerprintType::METADATA_ONLY);
        }
        return;
        
    } else if ((stat_info.st_mode & S_IFMT) == S_IFREG) {
        // Checksum a file
        if (blake2sp_file(path_string, checksum)) {
            file.setFingerprintType(db::FingerprintType::BLAKE2SP);
        } else {
            // Accessing contents failed
            file.setFingerprintType(db::FingerprintType::METADATA_ONLY);
        }
        return;
    } else {
        // Don't checksum other types of files
        file.setFingerprintType(db::FingerprintType::METADATA_ONLY);
        return;
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
                                          file.getInode() == stat_info.st_ino &&
                                           file.getMode() == stat_info.st_mode) {
        return true;
    }

    // If all we have to go on is the metadata, and the metadata is different, we have
    // to assume that the change is real.
    if (file.getFingerprintType() == db::FingerprintType::METADATA_ONLY) {
        return false;
    }
    
    // What type of fingerprint are we dealing with?
    if (file.getFingerprintType() == db::FingerprintType::BLAKE2SP) {
        // Using blake2sp
        
        // Make space for the checksum output
        size_t hash_size = file.getChecksum().size();
        KJ_STACK_ARRAY(kj::byte, checksum, hash_size, hash_size, hash_size);
    
        // Is this a file or a directory?
        if ((stat_info.st_mode & S_IFMT) == S_IFDIR) {
            // Checksum a directory
            if (!blake2sp_dir(path_string, checksum)) {
                // Checksum failed. Return mismatch
                return false;
            }
        
        } else if ((stat_info.st_mode & S_IFMT) == S_IFREG) {
            // Checksum a regular file
            if (!blake2sp_file(path_string, checksum)) {
                // Checksum failed. Return mismatch
                return false;
            }
        } else {
            // Some other file type
            return false;
        }
        
        // Compare checksums
        return checksum.asConst() == file.getChecksum();
        
    } else {
        // Unrecognized fingerprint type
        return false;
    }
}
