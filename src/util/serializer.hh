#pragma once

#include <memory>
#include <string>

using std::string;
using std::unique_ptr;

class Build;

/**
 * Load a serialized build
 * \param filename  The name of the file to load the build from
 * \returns A pointer to the loaded build, or to nullptr if loading failed
 */
unique_ptr<Build> load_build(string filename);

/**
 * Serialize a build to a file
 * \param filename  The name of the file to save the build to
 * \param b         The build that should be saved
 */
void save_build(string filename, unique_ptr<Build>& b);
