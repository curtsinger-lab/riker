#include <algorithm>
#include <climits>
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

enum { ADD, REPLACE, DELETE, APPEND, PREPEND, POP, DEQUEUE };
unordered_map<string, string> default_envar;

struct diff {
  string key;
  string value;
  int action;
};

vector<string> getEnvironment(vector<diff> difference) {
  vector<string> to_return;
  unordered_map<string, string> default_envar_copy(default_envar);
  string value;

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
      case POP:
        value = default_envar_copy.at(change.key);
        value.erase(value.length() - change.value.length() - 1);
        break;
      case DEQUEUE:
        default_envar_copy.at(change.key).erase(0, change.value.length() + 1);
        break;
      case APPEND:
        default_envar_copy.at(change.key).append(":").append(change.value);
        break;
      case PREPEND:
        default_envar_copy.at(change.key).insert(0, 1, ':').insert(0, change.value);
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

vector<vector<int>> LevenshteinDistance(const vector<string> before_tokens,
                                        const vector<string> after_tokens) {
  // for all i and j, d[i,j] will hold the Levenshtein distance between
  // the first i characters of s and the first j characters of t
  /*       j
           m
      b e f o r e
    a
    f
i n t
    e
    r

  */
  int m = before_tokens.size();
  int n = after_tokens.size();

  vector<vector<int>> d(n + 1, vector<int>(m + 1, 0));

  // set each element in d to zero

  // source prefixes can be transformed into empty string by
  // dropping all characters
  for (int i = 1; i <= n; i++) {
    d[i][0] = i;
  }

  // target prefixes can be reached from empty source prefix
  // by inserting every character
  for (int j = 1; j <= m; j++) {
    d[0][j] = j;
  }

  int substitutionCost;
  for (int j = 1; j <= m; j++) {
    for (int i = 1; i <= n; i++) {
      if (after_tokens[i - 1] == before_tokens[j - 1]) {
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

vector<diff> analyzeChanges(string before, string after, char delimiter, string key) {
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

  // vector<string> to_prepend;
  // vector<string> to_append;
  // vector<string> to_dequeue;
  // vector<string> to_pop;

  int m = before_tokens.size();
  int n = after_tokens.size();

  vector<vector<int>> dist(LevenshteinDistance(before_tokens, after_tokens));
  // cout << "    ";
  // for (string s : before_tokens) {
  //   cout << s << " ";
  // }
  // cout << endl;
  // for (int i = 0; i <= n; i++) {
  //   if (i != 0) {
  //     cout << after_tokens[i - 1] << " ";
  //   } else {
  //     cout << "  ";
  //   }
  //   for (int j = 0; j <= m; j++) {
  //     cout << dist[i][j] << " ";
  //   }
  //   cout << endl;
  // }
  // cout << endl << endl;
  int i = n, j = m;
  int up, left, diagonal, minimum;
  vector<diff> to_return;
  int switches = 0;
  bool previous_matched = false;
  diff changed_var;
  while (!(i == 0 && j == 0)) {
    // cout << "i=" << i << " j=" << j << " before[j]=" << before_tokens[j - 1]
    //     << " after[i]=" << after_tokens[i - 1] << endl;
    if (i == 0) {
      left = dist[i][j - 1];
      diagonal = INT_MAX;
      up = INT_MAX;
      minimum = left;
    }
    if (j == 0) {
      up = dist[i - 1][j];
      diagonal = INT_MAX;
      left = INT_MAX;
      minimum = up;
    }
    if (i != 0 && j != 0) {
      up = dist[i - 1][j];
      left = dist[i][j - 1];
      diagonal = dist[i - 1][j - 1];
      minimum = min({up, left, diagonal});
    }

    if (minimum == diagonal) {
      if (diagonal == dist[i][j] && switches < 2) {
        if (!previous_matched) {
          switches++;
          previous_matched = true;
        }
        i--;
        j--;
      } else {
        // cannot append/prepend changes.
        // return the REPLACE diff
        to_return.clear();
        diff changed_var = {key, after, REPLACE};
        to_return.push_back(changed_var);
        return to_return;
      }
    } else if (minimum == up) {
      if (switches == 0) {
        changed_var = {key, after_tokens[i - 1], APPEND};
      } else {
        if (previous_matched) {
          switches++;
          previous_matched = false;
        }
        changed_var = {key, after_tokens[i - 1], PREPEND};
      }
      to_return.push_back(changed_var);
      i--;
    } else {
      // to_return.clear();
      // diff changed_var = {key, after, REPLACE};
      // to_return.push_back(changed_var);
      // return to_return;
      if (switches == 0) {
        changed_var = {key, before_tokens[j - 1], POP};
      } else {
        if (previous_matched) {
          switches++;
          previous_matched = false;
        }
        changed_var = {key, before_tokens[j - 1], DEQUEUE};
      }
      to_return.push_back(changed_var);
      j--;
    }
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
        vector<diff> changed_vars = analyzeChanges(default_value, value, ':', key);
        // diff changed_var = {key, value, REPLACE};
        to_return.insert(to_return.end(), changed_vars.begin(), changed_vars.end());
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
  // string before = "e:f:g:h";
  // string after = "c:d:e:g";

  // vector<diff> diffs = analyzeChanges(before, after, ':', "key");

  // for (diff d : diffs) {
  //   cout << d.key << " " << d.value << " " << d.action << endl;
  // }

  // for (auto const& pair : default_envar) {
  //   cout << pair.first << "=" << pair.second << endl;
  // }
  vector<string> envar;
  envar.push_back(
      "PATH=/home/mayueran/.vscode-server/bin/507ce72a4466fbb27b715c3722558bb15afa9f48/bin:/home/"
      "mayueran/.vscode-server/bin/507ce72a4466fbb27b715c3722558bb15afa9f48/bin:/usr/local/sbin:/"
      "usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/snap/bin:/test/"
      "test");
  vector<diff> difference = getEnvDiff(envar);
  // for (vector<diff>::const_iterator i = difference.begin(); i != difference.end(); ++i) {
  //   cout << i->key << "=" << i->value << " " << i->action << endl;
  // }

  vector<string> env = getEnvironment(difference);
  for (string s : env) {
    cout << s << endl;
  }
  cout << (envar == env) << endl;
  return 1;
}