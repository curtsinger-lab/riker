#pragma once

#include <fstream>
#include <string>

struct Graphviz {
  std::ofstream graph;

  Graphviz();
  void start_graph();
  void add_node(std::string id, std::string label, std::string attr);
  void add_edge(std::string id1, std::string id2, std::string attr);
  void close_graph();
};
