#pragma once

#include <string>

using std::string;

class Build;

/**
 * Load a serialized build
 * \param filename  The name of the file to load the build from
 * \param b         A reference to a Build that will store the loaded build
 * \returns true on success, or false if the loading fails for any reason.
 *          On a failed load, b may hold partially-deserialized data.
 */
bool load_build(string filename, Build& b);

/**
 * Serialize a build to a file
 * \param filename  The name of the file to save the build to
 * \param b         The build that should be saved
 */
void save_build(string filename, const Build& b);
