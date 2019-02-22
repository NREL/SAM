#ifndef __export_config_input_pages_
#define __export_config_input_pages_

#include <iostream>
#include <string>
#include <vector>
#include <unordered_map>

#include "startup_extractor.h"

/**
 * For each input page for a given configuration, stores the variables that are calculated
 * by equations or callbacks in both common and exclusive ui forms.
 *
 */

struct page_variables_per_config{
    std::string config_name;
    std::unordered_map<std::string, std::vector<std::string>> page_to_eqn_outputs;
    std::unordered_map<std::string, std::vector<std::string>> page_to_callback_cmods;
    std::unordered_map<std::string, std::vector<std::string>> page_to_callback_outputs;
};

static std::vector<page_variables_per_config> SAM_page_variables;

void print_page_variables_per_config(page_variables_per_config pvpc, std::string which_map){
    std::unordered_map<std::string, std::vector<std::string>>* map;
    if (which_map == "eqn")
        map = &pvpc.page_to_eqn_outputs;
    else if (which_map == "cmod")
        map = &pvpc.page_to_callback_cmods;
    else
        map = &pvpc.page_to_callback_outputs;

    // 'config_name' = {
    std::cout << "'" << pvpc.config_name << "': {\n";
    bool first = true;
    for (auto it = map->begin(); it != map->end(); ++it){
        if (it->second.size() == 0) continue;
        if (!first)
            std::cout << ",\n";
        // 'page_name' : [output_vars],
        std::cout << "\t'" << it->first << "': \n\t\t" << it->second;
        first = false;
    }
    std::cout << "\n}";
}



#endif //__export_config_input_pages_
