#include <stdio.h>
#include <stdlib.h>
#include <iostream>
#include <fstream>

#include "graph.h"

Graph::Graph(void) {
    this->graph.open("out.dot");
}

void Graph::start_graph(void) {
    this->graph << "digraph {\n\tgraph [rankdir=LR]\n\tnode [fontname=Courier]\n";
} 

void Graph::add_node(std::string id, std::string label, std::string attr) {
   this->graph << "\t\"" + id + "\" [label=\"" + label + "\" " + attr + "]\n";
}

void Graph::add_edge(std::string id1, std::string id2, std::string attr) {
    this->graph << "\t\"" + id1 + "\" -> \"" + id2 + "\" [" + attr + "]\n";
}

void Graph::close_graph(void) {
    this->graph << "}";
    this->graph.close();
}    
