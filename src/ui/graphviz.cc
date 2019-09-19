#include "ui/graphviz.hh"

#include <cstdio>
#include <cstdlib>
#include <fstream>
#include <iostream>

Graphviz::Graphviz(void) { this->graph.open("out.dot"); }

void Graphviz::start_graph(void) {
  this->graph << "digraph {\n\tgraph [rankdir=LR]\n\tnode [fontname=Courier]\n";
}

void Graphviz::add_node(std::string id, std::string label, std::string attr) {
  this->graph << "\t\"" + id + "\" [label=\"" + label + "\" " + attr + "]\n";
}

void Graphviz::add_edge(std::string id1, std::string id2, std::string attr) {
  this->graph << "\t\"" + id1 + "\" -> \"" + id2 + "\" [" + attr + "]\n";
}

void Graphviz::close_graph(void) {
  this->graph << "}";
  this->graph.close();
}
