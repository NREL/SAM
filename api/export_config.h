#ifndef __export_config_input_pages_
#define __export_config_input_pages_

#include <iostream>
#include <string>
#include <vector>
#include <unordered_map>

#include "startup_extractor.h"
#include "ui_form_extractor.h"

ui_form_extractor_database SAM_ui_extracted_db;

/// create the cmod and get all the variable names of desired type
std::vector<std::string> get_cmod_var_info(std::string cmod_name, std::string which_type){
    ssc_module_t p_mod = ssc_module_create(const_cast<char*>(cmod_name.c_str()));
    std::vector<std::string> variable_names;

    int var_index = 0;
    ssc_info_t mod_info = ssc_module_var_info(p_mod, var_index);
    while (mod_info){
        int var_type = ssc_info_var_type(mod_info);
        std::string name = ssc_info_name(mod_info);

        // if SSC_INPUT or SSC_INOUT
        if (which_type == "in"){
            if ( var_type == 1 || var_type == 3) {
                variable_names.push_back(name);
            }
        }
        else{
            if ( var_type == 2) {
                variable_names.push_back(name);
            }
        }
        ++var_index;
        mod_info = ssc_module_var_info(p_mod, var_index);
    }
    return variable_names;
}

/// get input information for compute modules that are used in all configurations
void load_primary_cmod_inputs() {
    // for primary modules, only require inputs
    for (auto it = SAM_config_to_primary_modules.begin(); it != SAM_config_to_primary_modules.end(); ++it){
        auto modules_vec = it->second;

        for (size_t i = 0; i < modules_vec.size(); i++){
            std::string cmod_name = modules_vec[i];

            if (SAM_cmod_to_inputs.find(cmod_name) == SAM_cmod_to_inputs.end()){
                std::vector<std::string> inputs_vec = get_cmod_var_info(cmod_name, "in");
                SAM_cmod_to_inputs.insert({cmod_name, inputs_vec});
            }
        }
    }
}

#endif //__export_config_input_pages_
