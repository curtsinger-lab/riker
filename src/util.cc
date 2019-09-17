#include "util.hh"

#include <iostream>
#include <string>

#include <kj/array.h>
#include <kj/vector.h>

// Convert a KJ blob to a C++ string
std::string blobToString(const kj::Array<kj::byte>& b) {
  return std::string(b.asPtr().asChars().begin(), b.asPtr().size());
}

// Convert a C++ string to a KJ blob
kj::Array<kj::byte> stringToBlob(std::string str) {
  kj::Vector<kj::byte> output;
  output.addAll(str);
  return output.releaseAsArray();
}


void write_shell_escaped(std::ostream &out_stream, const std::string &input) {
  if (input.find_first_of(" \t\n&();|<>!{}'\"") == std::string::npos &&
      input != std::string("elif") && input != std::string("fi") &&
      input != std::string("while") && input != std::string("case") &&
      input != std::string("else") && input != std::string("for") &&
      input != std::string("then") && input != std::string("do") &&
      input != std::string("done") && input != std::string("until") &&
      input != std::string("if") && input != std::string("esac")) {
    out_stream << input;
    return;
  }

  out_stream << '\'';
  size_t escaped_so_far = 0;
  while (true) {
    size_t quote_offset = input.find('\'', escaped_so_far);
    if (quote_offset == std::string::npos) {
      out_stream.write(input.data() + escaped_so_far,
                       input.size() - escaped_so_far);
      out_stream << '\'';
      return;
    } else {
      out_stream.write(input.data() + escaped_so_far,
                       quote_offset - escaped_so_far);
      out_stream << "'\\''";
      escaped_so_far = quote_offset + 1;
    }
  }
}
