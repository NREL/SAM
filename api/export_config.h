#ifndef __export_config_input_pages_
#define __export_config_input_pages_

#include <iostream>
#include <string>
#include <vector>
#include <unordered_map>

#include "startup_extractor.h"

void print_config_variables_info(){
    for (auto it = SAM_config_to_case_variables.begin(); it != SAM_config_to_case_variables.end(); ++it){
        // 'config_name' = {
        std::cout << "'" << it->second.config_name << "': {\n";
        bool first = true;
        // equations: [inputs, outputs, ui_form]
        std::cout << "\t\t'equations': [\n";
        for (size_t n = 0; n < it->second.eqns_info.size(); n++){
            std::cout << "\t\t\t" << it->second.eqns_info[n].inputs << ",\n";
            std::cout << "\t\t\t" << it->second.eqns_info[n].outputs << ",\n";
            std::cout << "\t\t\t" << it->second.eqns_info[n].ui_form << "]\n";
        }
        // secondary_cmods: [cmod ...]
        std::cout << "\t\t'secondary_cmods':\n";
        for (size_t n = 0; n < it->second.secondary_cmods.size(); n++){
            std::cout << "\t\t\t" << it->second.secondary_cmods[n] << "\n";
        }
    }
}

#endif //__export_config_input_pages_
