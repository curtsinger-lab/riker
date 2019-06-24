#include <string>
#include <fstream>
#include <iostream>

struct Graph {
    std::ofstream graph;
    
    Graph(void);
    void start_graph(void);
    void add_node(std::string id, std::string label, std::string attr);
    void add_edge(std::string id1, std::string id2, std::string attr);
    void close_graph(void);
};
