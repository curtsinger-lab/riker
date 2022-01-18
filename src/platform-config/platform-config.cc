#include <cerrno>
#include <cstdio>
#include <cstdlib>
#include <filesystem>
#include <iostream>
#include <string>

#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>

namespace fs = std::filesystem;

using std::cout;
using std::endl;
using std::string;
using std::to_string;

const string get_errno_name();

void check_open_creat_dir() {
  int fd = open("out", O_CREAT | O_DIRECTORY, S_IRWXU | S_IRWXG);

  // Did the call fail or succeed?
  if (fd == -1) {
    cout << "#define OPEN_CREAT_DIR_RESULT " << get_errno_name() << endl;
  } else {
    cout << "#define OPEN_CREAT_DIR_RESULT SUCCESS" << endl;
  }

  // Did the call produce something?
  struct stat statbuf;
  if (stat("out", &statbuf)) {
    cout << "#define OPEN_CREAT_DIR_OUTPUT OUTPUT_NONE" << endl;
  } else if ((statbuf.st_mode & S_IFMT) == S_IFREG) {
    cout << "#define OPEN_CREAT_DIR_OUTPUT OUTPUT_FILE" << endl;
  } else {
    fprintf(stderr, "Unexpected output type from open_creat_dir check\n");
    exit(1);
  }
}

int main() {
  // Create a temporary working directory
  char working_dir_template[] = ".config-XXXXXX";
  char* working_dir = mkdtemp(working_dir_template);
  if (working_dir == NULL) {
    perror("Failed to create temporary working directory");
    exit(1);
  }

  // Move to the temporary working directory
  if (chdir(working_dir)) {
    perror("Failed to move to temporary working directory");
    exit(1);
  }

  // Emit file prologue
  cout << "#pragma once" << endl;
  cout << endl;
  cout << "#define OUTPUT_NONE 0" << endl;
  cout << "#define OUTPUT_FILE 1" << endl;
  cout << "#define OUTPUT_DIR 2" << endl;

  // Run checks
  check_open_creat_dir();

  // End with blank line
  cout << endl;

  // Move back to the starting directory
  if (chdir("..")) {
    perror("Failed to move back to starting directory");
    exit(1);
  }

  // Remove the temporary working directory
  fs::remove_all(working_dir);

  return 0;
}

const string get_errno_name() {
  switch (errno) {
    case EPERM:
      return "EPERM";
    case ENOENT:
      return "ENOENT";
    case ESRCH:
      return "ESRCH";
    case EINTR:
      return "EINTR";
    case EIO:
      return "EIO";
    case ENXIO:
      return "ENXIO";
    case E2BIG:
      return "E2BIG";
    case ENOEXEC:
      return "ENOEXEC";
    case EBADF:
      return "EBADF";
    case ECHILD:
      return "ECHILD";
    case EAGAIN:
      return "EAGAIN";
    case ENOMEM:
      return "ENOMEM";
    case EACCES:
      return "EACCES";
    case EFAULT:
      return "EFAULT";
    case ENOTBLK:
      return "ENOTBLK";
    case EBUSY:
      return "EBUSY";
    case EEXIST:
      return "EEXIST";
    case EXDEV:
      return "EXDEV";
    case ENODEV:
      return "ENODEV";
    case ENOTDIR:
      return "ENOTDIR";
    case EISDIR:
      return "EISDIR";
    case EINVAL:
      return "EINVAL";
    case ENFILE:
      return "ENFILE";
    case EMFILE:
      return "EMFILE";
    case ENOTTY:
      return "ENOTTY";
    case ETXTBSY:
      return "ETXTBSY";
    case EFBIG:
      return "EFBIG";
    case ENOSPC:
      return "ENOSPC";
    case ESPIPE:
      return "ESPIPE";
    case EROFS:
      return "EROFS";
    case EMLINK:
      return "EMLINK";
    case EPIPE:
      return "EPIPE";
    case EDOM:
      return "EDOM";
    case ERANGE:
      return "ERANGE";
    case ENOTSUP:
      return "ENOTSUP";
    case ECANCELED:
      return "ECANCELED";
    case EOWNERDEAD:
      return "EOWNERDEAD";
    case ENOTRECOVERABLE:
      return "ENOTRECOVERABLE";
    case ERFKILL:
      return "ERFKILL";
    case EHWPOISON:
      return "EHWPOISON";
    case EDEADLK:
      return "EDEADLK";
    case ENAMETOOLONG:
      return "ENAMETOOLONG";
    case ENOLCK:
      return "ENOLCK";
    case ENOSYS:
      return "ENOSYS";
    case ENOTEMPTY:
      return "ENOTEMPTY";
    case ELOOP:
      return "ELOOP";
    case ENOMSG:
      return "ENOMSG";
    case EIDRM:
      return "EIDRM";
    case ECHRNG:
      return "ECHRNG";
    case EL2NSYNC:
      return "EL2NSYNC";
    case EL3HLT:
      return "EL3HLT";
    case EL3RST:
      return "EL3RST";
    case ELNRNG:
      return "ELNRNG";
    case EUNATCH:
      return "EUNATCH";
    case ENOCSI:
      return "ENOCSI";
    case EL2HLT:
      return "EL2HLT";
    case EBADE:
      return "EBADE";
    case EBADR:
      return "EBADR";
    case EXFULL:
      return "EXFULL";
    case ENOANO:
      return "ENOANO";
    case EBADRQC:
      return "EBADRQC";
    case EBADSLT:
      return "EBADSLT";
    case EBFONT:
      return "EBFONT";
    case ENOSTR:
      return "ENOSTR";
    case ENODATA:
      return "ENODATA";
    case ETIME:
      return "ETIME";
    case ENOSR:
      return "ENOSR";
    case ENONET:
      return "ENONET";
    case ENOPKG:
      return "ENOPKG";
    case EREMOTE:
      return "EREMOTE";
    case ENOLINK:
      return "ENOLINK";
    case EADV:
      return "EADV";
    case ESRMNT:
      return "ESRMNT";
    case ECOMM:
      return "ECOMM";
    case EPROTO:
      return "EPROTO";
    case EMULTIHOP:
      return "EMULTIHOP";
    case EDOTDOT:
      return "EDOTDOT";
    case EBADMSG:
      return "EBADMSG";
    case EOVERFLOW:
      return "EOVERFLOW";
    case ENOTUNIQ:
      return "ENOTUNIQ";
    case EBADFD:
      return "EBADFD";
    case EREMCHG:
      return "EREMCHG";
    case ELIBACC:
      return "ELIBACC";
    case ELIBBAD:
      return "ELIBBAD";
    case ELIBSCN:
      return "ELIBSCN";
    case ELIBMAX:
      return "ELIBMAX";
    case ELIBEXEC:
      return "ELIBEXEC";
    case EILSEQ:
      return "EILSEQ";
    case ERESTART:
      return "ERESTART";
    case ESTRPIPE:
      return "ESTRPIPE";
    case EUSERS:
      return "EUSERS";
    case ENOTSOCK:
      return "ENOTSOCK";
    case EDESTADDRREQ:
      return "EDESTADDRREQ";
    case EMSGSIZE:
      return "EMSGSIZE";
    case EPROTOTYPE:
      return "EPROTOTYPE";
    case ENOPROTOOPT:
      return "ENOPROTOOPT";
    case EPROTONOSUPPORT:
      return "EPROTONOSUPPORT";
    case ESOCKTNOSUPPORT:
      return "ESOCKTNOSUPPORT";
    case EPFNOSUPPORT:
      return "EPFNOSUPPORT";
    case EAFNOSUPPORT:
      return "EAFNOSUPPORT";
    case EADDRINUSE:
      return "EADDRINUSE";
    case EADDRNOTAVAIL:
      return "EADDRNOTAVAIL";
    case ENETDOWN:
      return "ENETDOWN";
    case ENETUNREACH:
      return "ENETUNREACH";
    case ENETRESET:
      return "ENETRESET";
    case ECONNABORTED:
      return "ECONNABORTED";
    case ECONNRESET:
      return "ECONNRESET";
    case ENOBUFS:
      return "ENOBUFS";
    case EISCONN:
      return "EISCONN";
    case ENOTCONN:
      return "ENOTCONN";
    case ESHUTDOWN:
      return "ESHUTDOWN";
    case ETOOMANYREFS:
      return "ETOOMANYREFS";
    case ETIMEDOUT:
      return "ETIMEDOUT";
    case ECONNREFUSED:
      return "ECONNREFUSED";
    case EHOSTDOWN:
      return "EHOSTDOWN";
    case EHOSTUNREACH:
      return "EHOSTUNREACH";
    case EALREADY:
      return "EALREADY";
    case EINPROGRESS:
      return "EINPROGRESS";
    case ESTALE:
      return "ESTALE";
    case EUCLEAN:
      return "EUCLEAN";
    case ENOTNAM:
      return "ENOTNAM";
    case ENAVAIL:
      return "ENAVAIL";
    case EISNAM:
      return "EISNAM";
    case EREMOTEIO:
      return "EREMOTEIO";
    case EDQUOT:
      return "EDQUOT";
    case ENOMEDIUM:
      return "ENOMEDIUM";
    case EMEDIUMTYPE:
      return "EMEDIUMTYPE";
    case ENOKEY:
      return "ENOKEY";
    case EKEYEXPIRED:
      return "EKEYEXPIRED";
    case EKEYREVOKED:
      return "EKEYREVOKED";
    case EKEYREJECTED:
      return "EKEYREJECTED";
    default:
      return to_string(errno);
  }
}
