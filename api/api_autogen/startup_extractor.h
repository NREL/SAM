#ifndef SYSTEM_ADVISOR_MODEL_EXTRACT_STARTUP_H
#define SYSTEM_ADVISOR_MODEL_EXTRACT_STARTUP_H


#include <string>
#include <vector>
#include <unordered_map>

#include <lk/absyn.h>
#include <lk/parse.h>
#include <lk/eval.h>
#include <lk/stdlib.h>

#include "lk_env.h"


/**
 * This class extracts input pages for each technology-financial configuration from the startup.lk
 * script, as well as the set of compute modules required for each configuration.
 */

class startup_extractor {
private:
    std::vector<std::string> errors;

public:

    void print_config_to_input_pages();

    void print_config_to_modules();

    /// Populates SAM_config_to_input_pages and SAM_config_to_primary_modules
    bool load_startup_script(const std::string script_file);

    std::vector<std::string> get_unique_ui_forms();
};

#endif //SYSTEM_ADVISOR_MODEL_EXTRACT_STARTUP_H
