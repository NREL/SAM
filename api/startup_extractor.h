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

class startup_extractor{
private:
    std::unordered_map<std::string, std::vector<page_info>> config_to_input_pages;
    std::unordered_map<std::string, std::vector<std::string>> config_to_modules;

public:
    std::unordered_map<std::string, std::vector<page_info>> get_config_to_input_pages(){
        return config_to_input_pages;
    };

    std::unordered_map<std::string, std::vector<std::string>> get_config_to_modules(){
        return config_to_modules;
    };

    void print_config_to_input_pages();

    void print_config_to_modules();

    bool load_startup_script(const std::string script_file, std::vector<std::string>* errors);
};

#endif //SYSTEM_ADVISOR_MODEL_EXTRACT_STARTUP_H
