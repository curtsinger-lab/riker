#include <algorithm>
#include <array>
#include <cstdio>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <mutex>
#include <string>
#include <vector>

#include <pthread.h>
#include <unistd.h>

#include "artifacts/Artifact.hh"
#include "data/InputTrace.hh"
#include "runtime/Build.hh"
#include "runtime/env.hh"
#include "util/constants.hh"

using std::array;
using std::cout;
using std::endl;
using std::ifstream;
using std::lock_guard;
using std::make_shared;
using std::mutex;
using std::nullopt;
using std::ofstream;
using std::shared_ptr;
using std::string;
using std::unique_ptr;
using std::vector;

class SynchronizedFile {
 public:
  SynchronizedFile(const string& path) : _path(path) { myfile.open(path); }

  ~SynchronizedFile() { myfile.close(); }

  void write(const string& dataToWrite) {
    lock_guard<mutex> lock(_writerMutex);
    myfile << dataToWrite << endl;
  }

  void addPackage(const string& package) {
    lock_guard<mutex> lock(_writerMutex);
    if (find(packages.begin(), packages.end(), package) == packages.end()) {
      packages.push_back(package);
      myfile << package << endl;
    }
  }

 private:
  string _path;
  ofstream myfile;
  mutex _writerMutex;
  vector<string> packages;
};

string getPackage(string path) {
  string command = "dpkg -S " + path + " 2>&1";
  FILE* pipe = popen(command.c_str(), "r");
  if (!pipe) {
    exit(EXIT_FAILURE);
  }
  array<char, 128> buffer;
  string result;
  while (fgets(buffer.data(), buffer.size(), pipe) != NULL) {
    result += buffer.data();
  }
  pclose(pipe);
  return result;
}

struct thread_data {
  // string path_arg;
  shared_ptr<Artifact> path_ptr;
  shared_ptr<SynchronizedFile> file_ptr;
};

void* myThread(void* threadarg) {
  struct thread_data* my_data;
  my_data = (struct thread_data*)threadarg;
  // auto path = my_data->path_arg;
  auto file = my_data->file_ptr;
  auto a = my_data->path_ptr;

  // If a is a null pointer, then skip
  if (!a) pthread_exit(NULL);
  // If a is an anonymous artifact, skip
  if (a->getName().empty()) pthread_exit(NULL);
  // If a is a directory, skip
  if (a->getTypeName() == "Dir") pthread_exit(NULL);
  // If a is special, skip
  if (a->getTypeName() == "Special") pthread_exit(NULL);
  // Remove all files in source folder
  if (a->getName().find("src") != string::npos) pthread_exit(NULL);
  // Remove Riker related files
  if (a->getName().find("riker") != string::npos) pthread_exit(NULL);
  // Remove Rikerfile
  if (a->getName() == "Rikerfile") pthread_exit(NULL);
  // Remove Symlink
  if (a->getTypeName() == "Symlink") pthread_exit(NULL);
  // Remove ld.so.cache
  if (a->getName().find("ld.so.cache") != string::npos) pthread_exit(NULL);
  // Remove locale-archive
  if (a->getName().find("locale-archive") != string::npos) pthread_exit(NULL);
  // Remove proc/filesystems
  if (a->getName().find("proc/filesystems") != string::npos) pthread_exit(NULL);
  // Remove temporary files
  if (a->getName().find("tmp") != string::npos) pthread_exit(NULL);
  // Skip files without committed path
  if (a->getCommittedPath() == nullopt) pthread_exit(NULL);

  string path = a->getCommittedPath().value().string();
  // cout << path << endl;

  // if (path.find("include") == string::npos) {
  //   if (path.find("/usr/") != string::npos) {
  //     path.erase(0, 4);
  //   }
  //   path = "'*" + path + "'";
  // }
  // file->write(a->getTypeName() + ": " + path);
  // myfile << a->getTypeName() << ": " << path << endl;
  string result = getPackage(path);
  if (result.find("no path found") != string::npos) {
    int fd_original, fd_alt;
    string alternative;
    if (path.find("/usr/") == string::npos)
      alternative = "/usr" + path;
    else
      alternative = path.substr(4, string::npos);
    fd_original = open(path.c_str(), O_RDONLY);
    fd_alt = open(alternative.c_str(), O_RDONLY);
    if (fd_original < 0 || fd_alt < 0) {
      perror("Error opening the files");
    }
    struct stat file_stat_original, file_stat_alt;
    int ret;
    ret = fstat(fd_original, &file_stat_original);
    if (ret < 0) perror("Error getting original file stat");
    ret = fstat(fd_alt, &file_stat_alt);
    if (ret < 0) perror("Error getting alternative file stat");
    if (file_stat_original.st_ino == file_stat_alt.st_ino &&
        file_stat_original.st_dev == file_stat_alt.st_dev) {
      result = getPackage(alternative);
    }
  }
  // file->write(result);
  // myfile << result << endl;
  string package = result.substr(0, result.find(" "));
  package.pop_back();
  // if (find(packages.begin(), packages.end(), package) == packages.end()) {
  //   packages.push_back(package);
  //   file->write(package);
  // myfile << package << endl;
  // }
  file->addPackage(package);

  pthread_exit(NULL);
}

/**
 * Run the 'gen_deps' subcommand
 */
void do_gen_deps(vector<string> args) noexcept {
  // Turn on input/output tracking
  // options::track_inputs_outputs = true;

  // Load the serialized build trace
  auto [root_cmd, trace] = InputTrace::load(constants::DatabaseFilename, args);

  // Emulate the trace
  trace->sendTo(Build());

  // ofstream myfile;
  // myfile.open(".rkr-deps");
  auto synchronizedFile = make_shared<SynchronizedFile>(".rkr-deps");

  // list of packages needed
  // vector<string> packages;

  int num_threads = env::getArtifacts().size();
  pthread_t threads[num_threads];
  struct thread_data tds[num_threads];
  int rc, i = 0;

  // Iterate through each artifact
  for (const auto& weak_artifact : env::getArtifacts()) {
    auto a = weak_artifact.lock();

    // cout << a->getTypeName() << ": " << a->getName() << endl;

    // cout << "test" << endl;
    tds[i].path_ptr = a;
    tds[i].file_ptr = synchronizedFile;
    // struct thread_data td = {a, synchronizedFile};
    // pthread_t pid;
    rc = pthread_create(&threads[i], NULL, myThread, (void*)&tds[i]);

    if (rc) {
      cout << "Error: unable to create thread," << rc << endl;
      exit(-1);
    }
    i++;
  }

  void* status;

  for (i = 0; i < num_threads; i++) {
    rc = pthread_join(threads[i], &status);
    if (rc) {
      cout << "Error: unable to join," << rc << endl;
      exit(-1);
    }
    // cout << "Main: completed thread id:" << i;
    // cout << " exiting with status:" << status << endl;
  }
  // pthread_exit(NULL);
}

void do_install_deps(vector<string> args) noexcept {
  array<char, 128> buffer;
  string result, package;
  ifstream myfile(".rkr-deps");
  if (myfile.is_open()) {
    while (getline(myfile, package)) {
      string cmd = "dpkg-query -W " + package;
      unique_ptr<FILE, decltype(&pclose)> pipe(popen(cmd.c_str(), "r"), pclose);
      if (!pipe) exit(EXIT_FAILURE);
      while (fgets(buffer.data(), buffer.size(), pipe.get()) != nullptr) {
        result += buffer.data();
      }
      if (result.find("no packages found matching") != string::npos) {
        cout << "Installing" << package << endl;
        cmd = "dpkg -i " + package;
        system(cmd.c_str());
      } else {
        cout << package << " is already installed" << endl;
      }
    }
  }
  myfile.close();
}

void do_check_deps(vector<string> args) noexcept {
  ifstream f(".rkr-deps");
  if (f.is_open()) {
    cout << f.rdbuf();
  }
}