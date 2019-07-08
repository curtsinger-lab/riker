#include "graph.h"
#include "db.capnp.h"

#include <stdio.h>
#include <stdint.h>
#include <fcntl.h>

#include <vector>

#include <capnp/message.h>
#include <capnp/serialize.h>

static void render_command_forest(Graph* graph, ::capnp::List<db::Command>::Reader commands, size_t start_id, size_t end_id) {
    size_t id = start_id;
    while (id < end_id) {
        auto root = commands[id];
        auto executable = std::string((const char*) root.getExecutable().begin(), root.getExecutable().size());
        graph->add_node("c" + std::to_string(id), executable, "");
        if (start_id > 0) {
            graph->add_edge("c" + std::to_string(start_id - 1), "c" + std::to_string(id), "style=dashed");
        }
        size_t children_start = id + 1;
        size_t children_end = children_start + root.getDescendants();
        render_command_forest(graph, commands, children_start, children_end);

        id = children_end;
    }
}

int main(int argc, char* argv[]) {
    bool show_sys;
    if (argc == 2 && strncmp(argv[1], "--show-sysfiles", 15) == 0) {
        show_sys = true;
    } else if (argc == 1) {
        show_sys = false;
    } else {
        fprintf(stderr, "Usage: %s [--show-sysfiles]\n", argv[0]);
        exit(1);
    }

    int db_file = open("db.dodo", O_RDONLY);
    if (db_file < 0) {
        perror("Failed to open database");
        exit(2);
    }

    ::capnp::StreamFdMessageReader message(db_file);
    auto db_graph = message.getRoot<db::Graph>();

    Graph graph;
    graph.start_graph();
    // Draw the commands
    render_command_forest(&graph, db_graph.getCommands(), 0, db_graph.getCommands().size());

    // Filter non-local files
    std::vector<bool> display_file;
    for (auto file : db_graph.getFiles()) {
        auto path = std::string((const char*) file.getPath().begin(), file.getPath().size());
        // TODO: a better heuristic
        if (!show_sys && (path.find("/usr/") != std::string::npos ||
                          path.find("/lib/") != std::string::npos ||
                          path.find("/dev/") != std::string::npos ||
                          path.find("/etc/") != std::string::npos ||
                          path.find("/proc/") != std::string::npos)) {
            display_file.push_back(false);
        } else {
            display_file.push_back(true);
        }
    }

    // Draw the files
    size_t file_id = 0;
    for (auto file : db_graph.getFiles()) {
        if (display_file[file_id]) {
            if (file.getType() == db::FileType::PIPE) {
                graph.add_node("f" + std::to_string(file_id), "[pipe]", "shape=diamond");
            } else {
                auto path = std::string((const char*) file.getPath().begin(), file.getPath().size());
                graph.add_node("f" + std::to_string(file_id), path, "shape=rectangle");
            }
        }
        file_id++;
    }

    // Draw the dependencies
    for (auto dep : db_graph.getInputs()) {
        if (display_file[dep.getFileID()]) {
            graph.add_edge("f" + std::to_string(dep.getFileID()), "c" + std::to_string(dep.getCommandID()), "arrowhead=empty");
        }
    }
    for (auto dep : db_graph.getOutputs()) {
        if (display_file[dep.getFileID()]) {
            graph.add_edge("c" + std::to_string(dep.getCommandID()), "f" + std::to_string(dep.getFileID()), "arrowhead=empty");
        }
    }
    for (auto dep : db_graph.getCreates()) {
        if (display_file[dep.getFileID()]) {
            graph.add_edge("c" + std::to_string(dep.getCommandID()), "f" + std::to_string(dep.getFileID()), "color=blue");
        }
    }

    // Draw the removals
    for (auto dep : db_graph.getRemovals()) {
        if (display_file[dep.getFileID()]) {
            graph.add_edge("c" + std::to_string(dep.getCommandID()), "f" + std::to_string(dep.getFileID()), "color=red");
        }
    }

    graph.close_graph();
}
