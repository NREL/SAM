#ifndef SYSTEM_ADVISOR_MODEL_LIBRARY_EXTRACTOR_H
#define SYSTEM_ADVISOR_MODEL_LIBRARY_EXTRACTOR_H

#include <string>
#include <vector>

#include "data_structures.h"

std::vector<std::string> get_files_in_directory(const std::string& dir);

std::set<std::string> get_options_from_library(const std::string& cmod, const std::string& defaults_dir);


#endif //SYSTEM_ADVISOR_MODEL_LIBRARY_EXTRACTOR_H
