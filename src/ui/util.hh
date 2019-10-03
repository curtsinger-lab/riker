#pragma once

#include <iostream>
#include <string>

using std::ostream;
using std::string;

// Escape a string for correct printing as an argument or command on the shell
void write_shell_escaped(ostream& out_stream, const string& input);
