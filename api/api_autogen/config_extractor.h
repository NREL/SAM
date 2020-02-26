#ifndef SYSTEM_ADVISOR_MODEL_CONFIG_EXTRACTOR_H
#define SYSTEM_ADVISOR_MODEL_CONFIG_EXTRACTOR_H

#include <vector>
#include <string>

#include <lk/env.h>
//#include <variables.h>
#include "variables.h"

#include "variable_graph.h"
#include "config_extractor.h"
#include "callback_extractor.h"
#include "lk_env.h"

class config_extractor {
private:
    std::string config_name;

    lk::env_t m_env;

    std::string defaults_file_dir;

    digraph *var_graph;

    callback_extractor *cb_ext;

    size_t load_variables_into_graph(VarTable &vt);

    void export_to_ui_form_db(std::string ui);

    std::vector<std::string> errors;

public:

    std::unordered_map<equation_info *, std::string> completed_equation_signatures;
    std::unordered_map<callback_info *, std::string> completed_callback_signatures;

    config_extractor(std::string name, const std::string &defaults_file_dir);


    ~config_extractor() {
        delete var_graph;
        delete cb_ext;

        // clear global config-specific variables
        active_ui = "";
        active_config = "";
        active_cmod = "";
        active_subobject = "";
        active_method = -1;
        map_subobject = true;
        subobjects_completed.clear();
    }

    bool load_defaults_for_config();

    std::string spell_equation(lk::node_t *node);

    bool map_equations();

    void register_callback_functions();

    std::string get_name() { return config_name; }

    lk::env_t *get_env() { return &m_env; }

    std::vector<std::string> &get_errors() { return errors; }

    digraph *get_variable_graph() {
        return var_graph;
    }


};

#endif //SYSTEM_ADVISOR_MODEL_CONFIG_EXTRACTOR_H
