#include "ui/graphviz.hh"

#include <fstream>

Graphviz::Graphviz() {
  this->graph.open("out.dot");
}

void Graphviz::start_graph() {
  this->graph << "digraph {\n\tgraph [rankdir=LR]\n\tnode [fontname=Courier]\n";
}

void Graphviz::add_node(std::string id, std::string label, std::string attr) {
  this->graph << "\t\"" + id + "\" [label=\"" + label + "\" " + attr + "]\n";
}

void Graphviz::add_edge(std::string id1, std::string id2, std::string attr) {
  this->graph << "\t\"" + id1 + "\" -> \"" + id2 + "\" [" + attr + "]\n";
}

void Graphviz::close_graph() {
  this->graph << "}";
  this->graph.close();
}
