#ifndef SYSTEM_ADVISOR_MODEL_CONFIG_EXTRACTOR_H
#define SYSTEM_ADVISOR_MODEL_CONFIG_EXTRACTOR_H

#include <vector>
#include <string>

#include <lk/env.h>
#include <variables.h>

#include "variable_graph.h"
#include "lk_env.h"

class config_extractor{
private:
    std::string config_name;

    lk::env_t m_env;

    std::string defaults_file_dir = "../deploy/runtime/defaults/";

    digraph* var_graph;

    void load_variables_into_graph(VarTable &vt);

    void export_to_ui_form_db(std::string ui);
public:

    config_extractor(std::string name);

    ~config_extractor(){delete var_graph;}

    bool load_defaults_for_config();

    bool map_equations();

    void register_callback_functions();
};

#endif //SYSTEM_ADVISOR_MODEL_CONFIG_EXTRACTOR_H
