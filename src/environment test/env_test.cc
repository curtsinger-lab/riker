#include <iostream>
#include <string>
#include <unordered_map>
#include <vector>

using std::cout;
using std::endl;
using std::string;
using std::unordered_map;
using std::vector;

extern char** environ;

enum { ADD, REPLACE, DELETE };
unordered_map<string, string> default_envar;

struct diff {
  string key;
  string value;
  int action;
};

vector<diff> getEnvDiff(vector<string> envar) {
  // unordered_map<string, string> envar_map;
  vector<diff> to_return;
  unordered_map<string, string> default_envar_copy(default_envar);
  for (string value : envar) {
    string key = value.substr(0, value.find("="));
    value.erase(0, value.find("=") + 1);
    string default_value;
    unordered_map<string, string>::const_iterator it;
    // Checking if the key is present in default_envar
    if ((it = default_envar_copy.find(key)) == default_envar_copy.end()) {
      // key is not present, so we are adding new key
      diff added_var = {key, value, ADD};
      to_return.push_back(added_var);
    } else {
      // Key is present
      default_value = default_envar_copy[key];
      // If the corresponding values are not the same, record diff
      if (default_value.compare(value) != 0) {
        diff changed_var = {key, value, REPLACE};
        to_return.push_back(changed_var);
      }
      default_envar_copy.erase(it);
    }
  }
  // Loop over all remaining elements in default_envar_copy
  auto it = default_envar_copy.begin();
  while (it != default_envar_copy.end()) {
    diff deleted_var = {it->first, it->second, DELETE};
    to_return.push_back(deleted_var);
    it = default_envar_copy.erase(it);
  }
  return to_return;
}

int main() {
  for (int i = 0; environ[i] != nullptr; i++) {
    string variable = string(environ[i]);
    string key = variable.substr(0, variable.find("="));
    variable.erase(0, variable.find("=") + 1);
    default_envar.insert({key, variable});
  }

  // for (auto const& pair : default_envar) {
  //   cout << pair.first << "=" << pair.second << endl;
  // }

  vector<string> envar = {"USER=chovanak"};
  vector<diff> difference = getEnvDiff(envar);
  for (vector<diff>::const_iterator i = difference.begin(); i != difference.end(); ++i) {
    cout << i->key << "=" << i->value << " " << i->action << endl;
  }

  return 1;
}