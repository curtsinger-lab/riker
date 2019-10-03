#include "ui/util.hh"

#include <cstddef>
#include <iostream>
#include <string>

using std::ostream;
using std::string;

void write_shell_escaped(ostream& out_stream, const string& input) {
  if (input.find_first_of(" \t\n&();|<>!{}'\"") == string::npos && input != string("elif") &&
      input != string("fi") && input != string("while") && input != string("case") &&
      input != string("else") && input != string("for") && input != string("then") &&
      input != string("do") && input != string("done") && input != string("until") &&
      input != string("if") && input != string("esac")) {
    out_stream << input;
    return;
  }

  out_stream << '\'';
  size_t escaped_so_far = 0;
  while (true) {
    size_t quote_offset = input.find('\'', escaped_so_far);
    if (quote_offset == string::npos) {
      out_stream.write(input.data() + escaped_so_far, input.size() - escaped_so_far);
      out_stream << '\'';
      return;
    } else {
      out_stream.write(input.data() + escaped_so_far, quote_offset - escaped_so_far);
      out_stream << "'\\''";
      escaped_so_far = quote_offset + 1;
    }
  }
}
