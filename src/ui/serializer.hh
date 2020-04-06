#pragma once

#include <string>

using std::string;

class BuildGraph;

/**
 * Load a serialized build
 * \param filename  The name of the file to load the build from
 * \param graph     A reference to a BuildGraph that will store the loaded build
 * \returns true on success, or false if the loading fails for any reason.
 *          On a failed load, graph may hold partially-deserialized data.
 */
bool load_build(string filename, BuildGraph& graph);

/**
 * Serialize a build to a file
 * \param filename  The name of the file to save the build to
 * \param graph     The build that should be saved
 */
void save_build(string filename, const BuildGraph& graph);
