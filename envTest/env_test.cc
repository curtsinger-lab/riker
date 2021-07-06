#include <algorithm>
#include <iostream>
#include <sstream>
#include <string>
#include <unordered_map>
#include <vector>

using std::cout;
using std::endl;
using std::min;
using std::string;
using std::stringstream;
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

vector<string> getEnvironment(vector<diff> difference) {
  vector<string> to_return;
  unordered_map<string, string> default_envar_copy(default_envar);

  for (diff change : difference) {
    switch (change.action) {
      case ADD:
        // Add a new environment variable
        default_envar_copy.insert({change.key, change.value});
        break;
      case REPLACE:
        // Replace value for existing var with new key
        default_envar_copy[change.key] = change.value;
        break;
      case DELETE:
        default_envar_copy.erase(change.key);
        break;
    }
  }

  auto it = default_envar_copy.begin();
  while (it != default_envar_copy.end()) {
    string var = it->first + "=" + it->second;
    to_return.push_back(var);
    ++it;
  }
  return to_return;
}

// This function compares the given environment variables with the default one and return a vector
// of differences between the two
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
        // Analyze the difference to see if it is appending or prepending
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

vector<vector<int>> LevenshteinDistance(const vector<string> before_tokens,
                                        const vector<string> after_tokens) {
  // for all i and j, d[i,j] will hold the Levenshtein distance between
  // the first i characters of s and the first j characters of t
  /*       m
      b e f o r e
    a
    f
  n t
    e
    r

  */
  int m = before_tokens.size();
  int n = after_tokens.size();

  vector<vector<int>> d(n, vector<int>(m, 0));

  // set each element in d to zero

  // source prefixes can be transformed into empty string by
  // dropping all characters
  for (int i = 1; i < m; i++) {
    d[i][0] = i;
  }

  // target prefixes can be reached from empty source prefix
  // by inserting every character
  for (int j = 1; j < n; j++) {
    d[0][j] = j;
  }

  int substitutionCost;
  for (int j = 1; j < n; j++) {
    for (int i = 1; i < m; i++) {
      if (before_tokens[i] == after_tokens[j]) {
        substitutionCost = 0;
      } else {
        substitutionCost = 1;
      }

      d[i][j] = min({d[i - 1][j] + 1,                       // deletion
                     d[i][j - 1] + 1,                       // insertion
                     d[i - 1][j - 1] + substitutionCost});  // substitution
    }
  }

  return d;
}

void analyzeChanges(string before, string after, char delimiter) {
  // vector<string> before_tokens =  before.substr(0, before.find(delimiter));

  // Split before string into vector of tokens
  string tmp;
  stringstream ssb(before);
  vector<string> before_tokens;
  while (getline(ssb, tmp, delimiter)) {
    before_tokens.push_back(tmp);
  }
  // Split after string into vector of tokens
  stringstream ssa(after);
  vector<string> after_tokens;
  while (getline(ssa, tmp, delimiter)) {
    after_tokens.push_back(tmp);
  }

  vector<string> to_prepend;
  vector<string> to_append;
  vector<string> to_dequeue;
  vector<string> to_pop;
  // bool match_found = false;

  // for (string atoken : after_tokens) {
  //   for (string btoken : before_tokens) {
  //     if (atoken.compare(btoken) == 0) {
  //       match_found = true;
  //     } else if (!match_found) {
  //       to_prepend.push_back(atoken);
  //     } else {
  //       to_append.push_back(atoken);
  //     }
  //   }
  // }
}

int main() {
  for (int i = 0; environ[i] != nullptr; i++) {
    string variable = string(environ[i]);
    string key = variable.substr(0, variable.find("="));
    variable.erase(0, variable.find("=") + 1);
    default_envar.insert({key, variable});
  }

  vector<string> envar;
  vector<diff> difference = getEnvDiff(envar);
  //   for (vector<diff>::const_iterator i = difference.begin(); i != difference.end(); ++i) {
  //     cout << i->key << "=" << i->value << " " << i->action << endl;
  //   }

  vector<string> env = getEnvironment(difference);
  for (string s : env) {
    cout << s << endl;
  }
  return 1;
}