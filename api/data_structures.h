#ifndef SYSTEM_ADVISOR_MODEL_EXPORTED_MAPS_H
#define SYSTEM_ADVISOR_MODEL_EXPORTED_MAPS_H

#include <string>
#include <unordered_map>
#include <set>

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


/// Bookmarks active configuration during startup.lk parsing
static std::string active_config;


/**
 * Maps each technology-financial configuration to the ui forms in each SAM page
 * e.g. 'MSLF-None': { 'Location and Resource': {'common': Solar Resource Data } ...
 */
static std::unordered_map<std::string, std::vector<page_info>> SAM_config_to_input_pages;


/**
 * Maps each technology-financial configuration to the primary compute_modules required
 * e.g. 'Biopower-LCOE Calculator': ('biomass', 'lcoefcr')
 */
static std::unordered_map<std::string, std::vector<std::string>> SAM_config_to_primary_modules;


struct equation_info{
    std::string ui_form;
    std::vector<std::string> inputs;
    std::vector<std::string> outputs;
};

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
    std::set<std::pair<std::string, std::string>> ssc_variables_to_eqn_inputs;
    std::set<std::pair<std::string, std::string>> eqn_outputs_to_ssc_variables;

    std::vector<std::string> secondary_cmods;

    std::set<std::pair<std::string, std::string>> ui_variables_to_secondary_inputs;
    std::set<std::pair<std::string, std::string>> secondary_outputs_to_ui_variables;

};

static std::unordered_map<std::string, config_variables_info> SAM_config_to_case_variables;

/**
 * Maps each technology-financial configuration to the secondary compute_modules required
 * e.g. 'Biopower-LCOE Calculator': ('biomass', 'lcoefcr')
 */



/// Bookmarks active ui form during UI script parsing
static std::string active_ui;

/**
 * Maps each config to the secondary cmod outputs that are used as inputs to the primary cmod
 * e.g. 'Wind Power-Residential': {'wind_obos': wind_turbine_rotor_diameter, rotorD)
 */



#endif //SYSTEM_ADVISOR_MODEL_EXPORTED_MAPS_H
