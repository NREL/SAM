#include <vector>
#include <string>
#include <algorithm>

#include <ssc/sscapi.h>

#include "ui_form_extractor.h"
#include "data_structures.h"

std::unordered_map<std::string, std::unordered_map<std::string, VarValue>> SAM_cmod_to_outputs;
std::unordered_map<std::string, std::unordered_map<std::string, size_t>> SAM_cmod_to_ssc_index;


void load_secondary_cmod_outputs(std::string cmod_name) {

    if (SAM_cmod_to_outputs.find(cmod_name) != SAM_cmod_to_outputs.end())
        return;

    SAM_cmod_to_outputs.insert({cmod_name, std::unordered_map<std::string, VarValue>()});
    auto outputs_map = &(SAM_cmod_to_outputs.find(cmod_name)->second);

    ssc_module_t p_mod = ssc_module_create(const_cast<char *>(cmod_name.c_str()));

    int var_index = 0;
    ssc_info_t mod_info = ssc_module_var_info(p_mod, var_index);
    while (mod_info) {
        int var_type = ssc_info_var_type(mod_info);

        // if SSC_OUTPUT
        if (var_type == 2) {
            std::string name = ssc_info_name(mod_info);
            outputs_map->insert({name, VarValue()});
            VarValue &vv = outputs_map->find(name)->second;

            int data_type = ssc_info_data_type(mod_info);

            switch (data_type) {
                case SSC_INVALID:
                    break;
                case SSC_STRING:
                    vv.Set(wxEmptyString);
                    break;
                case SSC_NUMBER:
                    vv.Set(0);
                    break;
                case SSC_ARRAY:
                    vv.Set(std::vector<int>());
                    break;
                case SSC_MATRIX:
                    vv.Set(matrix_t<float>());
                    break;
                case SSC_TABLE:
                    vv.Set(VarTable());
                    break;
            }
        }
        ++var_index;
        mod_info = ssc_module_var_info(p_mod, var_index);
    }
}

/// create the cmod and get all the variable names of desired type, saving the indices
std::vector<std::string> get_cmod_var_info(std::string cmod_name, std::string which_type) {

    if (SAM_cmod_to_ssc_index.find(cmod_name) != SAM_cmod_to_ssc_index.end()) {
        if (which_type == "in")
            return SAM_cmod_to_inputs[cmod_name];
    }
    ssc_module_t p_mod = ssc_module_create(const_cast<char *>(cmod_name.c_str()));
    std::vector<std::string> variable_names;

    SAM_cmod_to_ssc_index.insert({cmod_name, std::unordered_map<std::string, size_t>()});
    std::unordered_map<std::string, size_t> &index_map = SAM_cmod_to_ssc_index.find(cmod_name)->second;

    int var_index = 0;
    ssc_info_t mod_info = ssc_module_var_info(p_mod, var_index);
    while (mod_info) {
        int var_type = ssc_info_var_type(mod_info);
        std::string name = ssc_info_name(mod_info);

        // if SSC_INPUT or SSC_INOUT
        if (which_type == "in") {
            if (var_type == 1 || var_type == 3) {
                variable_names.push_back(name);
            }
        } else {
            if (var_type == 2) {
                variable_names.push_back(name);
            }
        }

//        if (index_map.find(name) != index_map.end())
//            std::cout << "get_cmod_var_info warning: " << name << " already exists in " << cmod_name << "\n";
        index_map.insert({name, (size_t) var_index});

        ++var_index;
        mod_info = ssc_module_var_info(p_mod, var_index);
    }
    return variable_names;
}

/// get input information for compute modules that are used in all configurations
void load_primary_cmod_inputs() {
    // for primary modules, only require inputs
    for (auto it = SAM_config_to_primary_modules.begin(); it != SAM_config_to_primary_modules.end(); ++it) {
        auto modules_vec = it->second;

        for (size_t i = 0; i < modules_vec.size(); i++) {
            std::string cmod_name = modules_vec[i];

            if (SAM_cmod_to_inputs.find(cmod_name) == SAM_cmod_to_inputs.end()) {
                std::vector<std::string> inputs_vec = get_cmod_var_info(cmod_name, "in");
                SAM_cmod_to_inputs.insert({cmod_name, inputs_vec});
            }
        }
    }
}



// move all implementations in here

std::vector<equation_info> find_eqn_info(std::string ui_var, std::string config) {
    std::vector<equation_info> eqn_infos;

    std::vector<std::string> all_ui_forms = find_ui_forms_for_config(config);
    for (size_t i = 0; i < all_ui_forms.size(); i++) {
        std::vector<equation_info> &eqn_infos = SAM_ui_form_to_eqn_info.find(all_ui_forms[i])->second;
        for (size_t j = 0; j < eqn_infos.size(); j++) {
            auto input_names = eqn_infos[j].all_inputs;
            if (std::find(input_names.begin(), input_names.end(), ui_var) != input_names.end()) {
                eqn_infos.push_back(eqn_infos[j]);
            }
        }
    }
    return eqn_infos;
}


std::string find_ui_of_variable(std::string name, std::string config) {
    std::vector<std::string> all_ui = find_ui_forms_for_config(config);

    for (size_t i = 0; i < all_ui.size(); i++) {
        std::unordered_map<std::string, VarValue> ui_def = SAM_ui_form_to_defaults[all_ui[i]];
        if (ui_def.find(name) != ui_def.end()) {
            return all_ui[i];
        }
    }
    return "";
}

std::vector<std::string> find_ui_forms_for_config(std::string config_name) {
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

ui_form_extractor *find_ui_of_object(std::string obj, std::string config) {
    std::vector<std::string> all_ui = find_ui_forms_for_config(config);

    for (size_t i = 0; i < all_ui.size(); i++) {
        ui_form_extractor *ui_fe = SAM_ui_extracted_db.find(all_ui[i]);
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

VarValue *find_default_from_ui(std::string name, std::string config) {
    std::vector<std::string> all_ui = find_ui_forms_for_config(config);

    for (size_t i = 0; i < all_ui.size(); i++) {
        std::unordered_map<std::string, VarValue> ui_def = SAM_ui_form_to_defaults[all_ui[i]];
        if (ui_def.find(name) != ui_def.end()) {
            return &ui_def[name];
        }
    }
    return NULL;
}


std::string which_cmod_as_input(std::string name, std::string config) {
    auto primary_cmods = SAM_config_to_primary_modules[config];
    for (size_t i = 0; i < primary_cmods.size(); i++) {
        std::string cmod = primary_cmods[i];
        auto inputs_vec = SAM_cmod_to_inputs[cmod];
        if (std::find(inputs_vec.begin(), inputs_vec.end(), name) != inputs_vec.end()) {
            return cmod;
        }
    }
    return "";
}


std::vector<std::string> split_identity_string(std::string str, size_t n) {
    size_t pos = str.find("args:");
    if (pos == std::string::npos)
        assert(false);
    std::vector<std::string> args;
    str = str.substr(pos + 5);
    pos = str.find(":");
    while (pos != std::string::npos) {
        args.push_back(str.substr(0, pos));
        str = str.substr(pos + 1);
        pos = str.find(":");
    }
    args.push_back(str);
    if (args.empty())
        args.push_back(str);
    return args;
}

std::string unescape(const std::string &s) {
    std::string res;
    std::string::const_iterator it = s.begin();
    while (it != s.end()) {
        char c = *it++;

        switch (c) {
            case '\t':
                break;
            case '\n':
                break;
            case '\"':
                break;
            case '\'':
                break;
            case ';':
                break;
            case ' ':
                break;
            default:
                res += c;
        }

    }
    return res;
}


void print_ui_form_to_eqn_variable() {
    std::cout << "ui_form_to_eqn_var_map = {" << "\n";
    for (auto it = SAM_ui_form_to_eqn_info.begin(); it != SAM_ui_form_to_eqn_info.end(); ++it) {
        if (it != SAM_ui_form_to_eqn_info.begin()) std::cout << ",\n";

        // 'ui_form' = {
        std::cout << "\t'" << it->first << "': {\n";
        for (size_t i = 0; i < it->second.size(); i++) {
            if (i > 0) std::cout << ",\n";
            std::cout << "\t\t" << it->second[i].all_inputs << ": \n";
            std::cout << "\t\t\t" << it->second[i].all_outputs << "";
        }
        std::cout << "\t}";
    }
    std::cout << "}";
}


equation_info &find_equation_info_from_edge(edge *e, std::string config) {
    auto ui_forms = find_ui_forms_for_config(config);
    for (size_t i = 0; i < ui_forms.size(); i++) {
        std::string &ui_form = ui_forms[i];

        auto &vec = SAM_ui_form_to_eqn_info[ui_form];
        for (size_t j = 0; j < vec.size(); j++) {
            equation_info &eq = vec.at(j);
            if (std::find(eq.all_inputs.begin(), eq.all_inputs.end(), e->src->name) != eq.all_inputs.end()
                && std::find(eq.all_outputs.begin(), eq.all_outputs.end(), e->dest->name) != eq.all_outputs.end()) {
                return eq;
            }
        }
    }
    throw std::runtime_error("could not find equation info");
}

