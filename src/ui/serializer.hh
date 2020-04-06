#pragma once

#include <string>

using std::string;

class BuildGraph;

bool load_build(string filename, BuildGraph& graph);
void save_build(string filename, BuildGraph& graph);
