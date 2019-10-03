#include "ui/graphviz.hh"

#include <fstream>

using std::string;

Graphviz::Graphviz() {
  this->graph.open("out.dot");
}

void Graphviz::start_graph() {
  this->graph << "digraph {\n\tgraph [rankdir=LR]\n\tnode [fontname=Courier]\n";
}

void Graphviz::add_node(string id, string label, string attr) {
  this->graph << "\t\"" + id + "\" [label=\"" + label + "\" " + attr + "]\n";
}

void Graphviz::add_edge(string id1, string id2, string attr) {
  this->graph << "\t\"" + id1 + "\" -> \"" + id2 + "\" [" + attr + "]\n";
}

void Graphviz::close_graph() {
  this->graph << "}";
  this->graph.close();
}
