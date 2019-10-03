#pragma once

#include <fstream>
#include <string>

using std::ofstream;
using std::string;

struct Graphviz {
  ofstream graph;

  Graphviz();
  void start_graph();
  void add_node(string id, string label, string attr);
  void add_edge(string id1, string id2, string attr);
  void close_graph();
};
