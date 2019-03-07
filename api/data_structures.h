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
 * Maps each ui form with the ui input/outputs of each secondary cmod. Required for tracking which ui variables
 * are used as secondary cmod inputs and if the ui_outputs are assigned as primary ssc inputs.
 */

class secondary_cmod_info{
private:
    std::unordered_map<std::string, std::string> input_name_to_assignments;
    std::unordered_map<std::string, std::string> output_name_to_assignments;
public:
    secondary_cmod_info(){};

    std::string cmod_name;

    void map_of_input(std::string input, std::string assignments);

    void map_of_output(std::string output, std::string assignments);
};

extern std::unordered_map<std::string, std::unordered_map<std::string, secondary_cmod_info>> SAM_config_to_secondary_cmod_info;



/**
 * Mapping how ui variables are transformed by equations and callbacks before being assigned to primary ssc inputs
 * across
 */

extern std::unordered_map<std::string, digraph*> SAM_config_to_variable_graph;


/**
 * For each given configuration, stores the information required to match up ui variables with
 * the ssc variables for primary compute modules, considering equation evaluations and
 * secondary compute module simulations.
 *
 * primary variables that are not calculated,
 * the secondary variables which are non-calculated variables for secondary compute_modules
 * called within the case, and evaluated inputs which are outputs of equations and secondary
 * compute_modules. Maps from eqn and secondary cmod outputs to case inputs are stored,
 * as are maps from case inputs into eqns and secondary cmods.
 *
 */

struct config_variables_info{
    std::string config_name;

    /// non-calculated variables that are inputs to eqns and primary simulation
    std::vector<std::string> primary_inputs;

    /// non-calculated variables that are inputs to secondary and primary simulations
    std::vector<std::string> secondary_inputs;

    /// calculated from eqns and secondary cmods that are inputs to secondary and primary simulations
    std::vector<std::string> evaluated_inputs;

    std::vector<equation_info> eqns_info;

    /// first element is the ui variable name,
    std::set<std::pair<std::string, std::string>> ssc_variables_to_eval_inputs;
    std::set<std::pair<std::string, std::string>> eqn_outputs_to_ssc_variables;

    std::vector<std::string> secondary_cmods;

    std::set<std::pair<std::string, std::string>> ui_variables_to_secondary_inputs;
    std::set<std::pair<std::string, std::string>> secondary_outputs_to_ui_variables;

};

extern std::unordered_map<std::string, config_variables_info> SAM_config_to_case_variables;

/**
 * Maps each technology-financial configuration to the secondary compute_modules required
 * e.g. 'Biopower-LCOE Calculator': ('biomass', 'lcoefcr')
 */



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

/// Determine if a variable is a secondary ssc output by returning cmod name
std::string which_cmod_as_output(std::string name, std::string ui_form);


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


template <typename T>
std::ostream& operator<<(std::ostream& os, const std::vector<T>& v);

std::vector<std::string> find_ui_forms_for_config(std::string config_name);

void print_ui_form_to_eqn_variable();

void print_config_variables_info();



#endif //SYSTEM_ADVISOR_MODEL_EXPORTED_MAPS_H
