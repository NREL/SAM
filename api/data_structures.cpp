#include <vector>
#include <string>
#include <algorithm>

#include <ssc/sscapi.h>

#include "ui_form_extractor.h"
#include "data_structures.h"

std::unordered_map<std::string, std::unordered_map<std::string, secondary_cmod_info>> SAM_config_to_secondary_cmod_info;


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

void secondary_cmod_info::map_of_input(std::string input, std::string assignments){
    // if value has already been mapped
    if (input_name_to_assignments.find(input) != input_name_to_assignments.end()){
        std::cout << "already mapped";
    }
    else{
        input_name_to_assignments.insert({input, assignments});
    }
}

void secondary_cmod_info::map_of_output(std::string output, std::string assignments){
    if (output_name_to_assignments.find(output) != output_name_to_assignments.end()){
        std::cout << "already mapped";

    }
    else{
        output_name_to_assignments.insert({output, assignments});
    }
}



// move all implementations in here

std::vector<equation_info> find_eqn_info(std::string ui_var, std::string config){
    std::vector<equation_info> eqn_infos;

    std::vector<std::string> all_ui_forms = find_ui_forms_for_config(config);
    for (size_t i = 0; i < all_ui_forms.size(); i++){
        std::vector<equation_info>& eqn_infos = SAM_ui_form_to_eqn_info.find(all_ui_forms[i])->second;
        for (size_t j = 0; j < eqn_infos.size(); j++){
            auto input_names = eqn_infos[j].ui_inputs;
            if (std::find(input_names.begin(), input_names.end(), ui_var) != input_names.end()){
                eqn_infos.push_back(eqn_infos[j]);
            }
        }
    }
    return eqn_infos;
}

std::string find_ui_of_variable(std::string name, std::string config){
    std::vector<std::string> all_ui = find_ui_forms_for_config(config);

    for (size_t i = 0; i < all_ui.size(); i++){
        std::unordered_map<std::string, VarValue> ui_def = SAM_ui_form_to_defaults[all_ui[i]];
        if (ui_def.find(name) != ui_def.end()){
            return all_ui[i];
        }
    }
    return "";
}

std::vector<std::string> find_ui_forms_for_config(std::string config_name){
    std::vector<std::string> all_ui_forms;
    std::vector<page_info> pages = SAM_config_to_input_pages[config_name];

    for (size_t p = 0; p < pages.size(); p++) {
        for (size_t i = 0; i < pages[p].common_uiforms.size(); i++) {
            all_ui_forms.push_back(pages[p].common_uiforms[i]);
        }
        for (size_t i = 0; i < pages[p].exclusive_uiforms.size(); i++) {
            all_ui_forms.push_back(pages[p].exclusive_uiforms[i]);
        }
    }
    return all_ui_forms;
}

ui_form_extractor* find_ui_of_object(std::string obj, std::string config){
    std::vector<std::string> all_ui = find_ui_forms_for_config(config);

    for (size_t i = 0; i < all_ui.size(); i++){
        ui_form_extractor* ui_fe = SAM_ui_extracted_db.find(all_ui[i]);
        assert(ui_fe);
        if (find(ui_fe->m_onload_obj.begin(), ui_fe->m_onload_obj.end(), obj) != ui_fe->m_onload_obj.end())
            return ui_fe;
        if (find(ui_fe->m_onchange_obj.begin(), ui_fe->m_onchange_obj.end(), obj) != ui_fe->m_onchange_obj.end())
            return ui_fe;
        if (find(ui_fe->m_functions.begin(), ui_fe->m_functions.end(), obj) != ui_fe->m_functions.end())
            return ui_fe;
    }
    return nullptr;
}

VarValue* find_default_from_ui(std::string name, std::string config){
    std::vector<std::string> all_ui = find_ui_forms_for_config(config);

    for (size_t i = 0; i < all_ui.size(); i++){
        std::unordered_map<std::string, VarValue> ui_def = SAM_ui_form_to_defaults[all_ui[i]];
        if (ui_def.find(name) != ui_def.end()){
            return &ui_def[name];
        }
    }
    return NULL;
}




std::string which_cmod_as_input(std::string name, std::string config){
    auto primary_cmods = SAM_config_to_primary_modules[config];
    for (size_t i = 0; i < primary_cmods.size(); i++){
        std::string cmod = primary_cmods[i];
        auto inputs_vec = SAM_cmod_to_inputs[cmod];
        if (std::find(inputs_vec.begin(), inputs_vec.end(), name) != inputs_vec.end()){
            return cmod;
        }
    }
    return "";
}

std::string which_cmod_as_output(std::string name, std::string config){
    if (SAM_config_to_secondary_cmod_info.find(config) == SAM_config_to_secondary_cmod_info.end()){
        return "";
    }
    auto secondary_cmods = SAM_config_to_secondary_cmod_info[config];
    for (auto it = secondary_cmods.begin(); it != secondary_cmods.end(); ++it){
// need to populate variables for new cmods
    }
    return "";
}

std::vector<std::string> split_identity_string(std::string str, size_t n){
    size_t pos = str.find("args:");
    if (pos == std::string::npos)
        assert(false);
    std::vector<std::string> args;
    str = str.substr(pos+5);
    pos = str.find(":");
    while (pos != std::string::npos){
        args.push_back(str.substr(0, pos));
        str = str.substr(pos+1);
        pos = str.find(":");
    }
    args.push_back(str);
    assert(args.size() == n);
    return args;
}

template <typename T>
std::ostream& operator<<(std::ostream& os, const std::vector<T>& v)
{
    os << "(";
    for (int i = 0; i < v.size(); ++i) {
        os << "'" <<v[i] << "'";
        if (i != v.size() - 1)
            os << ", ";
    }
    os << ")";
    return os;
}


void print_ui_form_to_eqn_variable(){
    std::cout << "ui_form_to_eqn_var_map = {" << "\n";
    for (auto it = SAM_ui_form_to_eqn_info.begin(); it != SAM_ui_form_to_eqn_info.end(); ++it){
        if (it != SAM_ui_form_to_eqn_info.begin()) std::cout << ",\n";

        // 'ui_form' = {
        std::cout << "\t'" << it->first << "': {\n";
        for (size_t i = 0; i <it->second.size(); i++){
            if (i > 0) std::cout << ",\n";
            std::cout << "\t\t" << it->second[i].ui_inputs << ": \n";
            std::cout << "\t\t\t" << it->second[i].ui_outputs << "";
        }
        std::cout << "\t}";
    }
    std::cout << "}";
}

void print_config_variables_info(){
    std::cout << "config_variables_info = {\n";
    for (auto it = SAM_config_to_case_variables.begin(); it != SAM_config_to_case_variables.end(); ++it){
        // 'config_name' = {
        std::cout << "'" << it->second.config_name << "' : {\n";
        bool first = true;
        // equations: [inputs, outputs, ui_form]
        std::cout << "\t\t'equations': {\n";
        for (size_t n = 0; n < it->second.eqns_info.size(); n++){
            //std::cout << "\t\t\t" << it->second.eqns_info[n].inputs << ": \n";
            //std::cout << "\t\t\t\t(" << it->second.eqns_info[n].outputs << ",\n";
            //std::cout << "\t\t\t\t'" << it->second.eqns_info[n].ui_form << "')\n";
        }
        std::cout << "\t\t}\n";
        // secondary_cmods: [cmod ...]
        if (it->second.secondary_cmods.size() > 0){
            std::cout << "\t\t'secondary_cmods':\n";
            for (size_t n = 0; n < it->second.secondary_cmods.size(); n++){
                std::cout << "\t\t\t" << it->second.secondary_cmods[n] << "\n";
            }
        }
        std::cout << "\t}\n";
    }
}
