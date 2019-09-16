#pragma once

#include <iostream>
#include <string>

#define ARRAY_COUNT(array) (sizeof(array) / sizeof(array[0]))

// Escape a string for correct printing as an argument or command on the shell
void write_shell_escaped(std::ostream &out_stream, const std::string &input);
