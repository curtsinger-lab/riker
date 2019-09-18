#pragma once

#include <iostream>
#include <string>

#include <kj/array.h>

#define ARRAY_COUNT(array) (sizeof(array) / sizeof(array[0]))

// Escape a string for correct printing as an argument or command on the shell
void write_shell_escaped(std::ostream& out_stream, const std::string& input);

// Convert a KJ blob to a C++ string
std::string blobToString(const kj::Array<kj::byte>& b);
std::string blobToString(const kj::ArrayPtr<const unsigned char>& b);

// Convert a C++ string to a KJ blob
kj::Array<kj::byte> stringToBlob(std::string str);
