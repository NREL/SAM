#ifndef SYSTEM_ADVISOR_MODEL_EXPORTED_MAPS_H
#define SYSTEM_ADVISOR_MODEL_EXPORTED_MAPS_H

#include <string>
#include <unordered_map>
#include <set>
#include <iostream>

#include "variables.h"
#include "variable_graph.h"

/* List of all intermediate and exported data structures with descriptions */


/**
 * Each input page consists of a page_info with the sidebar title in the SAM GUI, ui forms which are common
 * to the page no matter what selection of variables is active, an exclusive variable which determines what
 * subset of ui forms should be shown, and those exclusive ui forms, of which only one is shown at a time.
 */
struct page_info{
    std::string sidebar_title;
    std::vector<std::string> common_uiforms;
    std::string exclusive_var = "";
    std::vector<std::string> exclusive_uiforms;
};


/**
 * Maps each technology-financial configuration to the ui forms in each SAM page
 * e.g. 'MSLF-None': { 'Location and Resource': {'common': Solar Resource Data } ...
 */
extern std::unordered_map<std::string, std::vector<page_info>> SAM_config_to_input_pages;


/**
 * Maps each technology-financial configuration to the primary compute_modules required
 * e.g. 'Biopower-LCOE Calculator': ('biomass', 'lcoefcr')
 */
extern std::unordered_map<std::string, std::vector<std::string>> SAM_config_to_primary_modules;


/**
 * All inputs to primary and secondary compute_modules: includes SSC_INPUT and SSC_INOUT
 */

extern std::unordered_map<std::string, std::vector<std::string>> SAM_cmod_to_inputs;

/**
 * All secondary compute_modules: SSC_OUT
 */

extern std::unordered_map<std::string, std::vector<std::string>> SAM_cmod_to_outputs;


/**
 * Maps each ui form to the config-independent default values found in included input pages
 */
extern std::unordered_map<std::string, std::unordered_map<std::string, VarValue>> SAM_ui_form_to_defaults;

/**
 * Maps each configuration to its specific defaults found in defaults text files
 */
 extern std::unordered_map<std::string, VarTable> SAM_config_to_defaults;


/**
 * Manages mapping and memory for ui_form_extractors
 */
class ui_form_extractor_database;
extern ui_form_extractor_database SAM_ui_extracted_db;


/**
 * Maps each ui form with the ui input/outputs of each equation. Required for tracking if ui variables
 * are changed via equations (becoming ui_outputs) before becoming primary ssc inputs.
 */
struct equation_info{
    std::vector<std::string> ui_inputs;
    std::vector<std::string> ui_outputs;
};

extern std::unordered_map<std::string, std::vector<equation_info>> SAM_ui_form_to_eqn_info;



/**
 * Mapping how ui variables are transformed by equations and callbacks before being assigned to primary ssc inputs
 * across
 */

extern std::unordered_map<std::string, digraph*> SAM_config_to_variable_graph;


/// Bookmarks active ui form during UI script parsing
extern std::string active_ui;

extern std::string active_object;

extern std::string active_subobject;

extern std::unordered_map<std::string, std::vector<std::string>> SAM_ui_obj_to_enabled_variables;


/**
 * Maps each config to the secondary cmod outputs that are used as inputs to the primary cmod
 * e.g. 'Wind Power-Residential': {'wind_obos': wind_turbine_rotor_diameter, rotorD)
 */

std::vector<std::string> get_cmod_var_info(std::string cmod_name, std::string which_type);

void load_primary_cmod_inputs();

/// Find which ui form a variable is defined inside for a given config
std::string find_ui_of_variable(std::string name, std::string config);

class ui_form_extractor;
ui_form_extractor* find_ui_of_object(std::string obj, std::string config);

/// Find the config-independent default VarValue for a variable for a given config
VarValue* find_default_from_ui(std::string name, std::string config);

/// Determine if a variable is a primary ssc input by returning cmod name
std::string which_cmod_as_input(std::string name, std::string config);

// utils

std::vector<std::string> split_identity_string(std::string str, size_t n);

static void clear_arg_string(lk::invoke_t& cxt){
    size_t pos = cxt.error().find("args");
    if (pos == std::string::npos)
        return;
    else if (pos == 0){
        cxt.clear_error();
    }
    else{
        cxt.error(cxt.error().substr(0, pos));
    }
}

std::string unescape(const std::string& s);

static bool argument_of_special(std::string& s){
    if (s.find("${") == std::string::npos)
        return false;
    size_t pos1 = s.find("${");
    size_t pos2 = s.find("}");
    s = s.substr(pos1 + 2, pos2);
    s = unescape(s);
    return true;
}

static bool argument_of_value(std::string& s){
    if (s.find("value(") == std::string::npos)
        return false;
    size_t pos1 = s.find("value(");
    size_t pos2 = s.find(")");
    s = s.substr(pos1 + 6, pos2);
    s = unescape(s);
    return true;
}

template <typename T>
std::ostream& operator<<(std::ostream& os, const std::vector<T>& v);

std::vector<std::string> find_ui_forms_for_config(std::string config_name);

void print_ui_form_to_eqn_variable();




#endif //SYSTEM_ADVISOR_MODEL_EXPORTED_MAPS_H
