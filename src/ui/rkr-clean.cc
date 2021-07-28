#include <iostream>
#include <string>
#include <vector>

using std::cout;
using std::endl;
using std::string;
using std::vector;

/**
 * Run the 'clean' subcommand
 */
void do_clean(vector<string> args, bool clean_all) noexcept {
  string cmd = "rm -rf .rkr";
  system(cmd.c_str());
  cout << "Cached information removed" << endl;
}