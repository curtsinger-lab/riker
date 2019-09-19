#pragma once

#include <iostream>
#include <string>

// Escape a string for correct printing as an argument or command on the shell
void write_shell_escaped(std::ostream& out_stream, const std::string& input);
