/**
BSD-3-Clause
Copyright 2019 Alliance for Sustainable Energy, LLC
Redistribution and use in source and binary forms, with or without modification, are permitted provided
that the following conditions are met :
1.	Redistributions of source code must retain the above copyright notice, this list of conditions
and the following disclaimer.
2.	Redistributions in binary form must reproduce the above copyright notice, this list of conditions
and the following disclaimer in the documentation and/or other materials provided with the distribution.
3.	Neither the name of the copyright holder nor the names of its contributors may be used to endorse
or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED.IN NO EVENT SHALL THE COPYRIGHT HOLDER, CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES
DEPARTMENT OF ENERGY, NOR ANY OF THEIR EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
OR CONSEQUENTIAL DAMAGES(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#ifndef SYSTEM_ADVISOR_MODEL_EXPORTED_MAPS_H
#define SYSTEM_ADVISOR_MODEL_EXPORTED_MAPS_H

#include <string>
#include <unordered_map>
#include <map>
#include <set>
#include <iostream>

#include "equations.h"
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
extern std::map<std::string, std::vector<std::string>> SAM_config_to_primary_modules;

/**
 * Maps each compute_module variable to its index in the info table for each config
 */
extern std::unordered_map<std::string, std::unordered_map<std::string, size_t>> SAM_cmod_to_ssc_index;


/**
 * All inputs to primary and secondary compute_modules: includes SSC_INPUT and SSC_INOUT
 */

extern std::unordered_map<std::string, std::vector<std::string>> SAM_cmod_to_inputs;

/**
 * All secondary compute_modules: SSC_OUT
 */

extern std::unordered_map<std::string, std::unordered_map<std::string, VarValue>> SAM_cmod_to_outputs;


/**
 * Maps each ui form to the config-independent default values found in included input pages
 */
extern std::unordered_map<std::string, std::unordered_map<std::string, VarValue>> SAM_ui_form_to_defaults;

/**
 * Maps each configuration to its specific defaults found in defaults text files
 */
extern std::unordered_map<std::string, VarTable> SAM_config_to_defaults;

/**
 * Maps each financial or technology option to its long name and description
 */
extern std::map<std::string, std::pair<std::string, std::string>> SAM_option_to_description;

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
    std::vector<std::string> all_inputs;
    std::vector<std::string> all_outputs;

    std::vector<std::string> ssc_only_inputs;
    std::vector<std::string> ui_only_inputs;

    std::string ui_source;
    EqnData* eqn_data;
};

extern std::unordered_map<std::string, std::vector<equation_info>> SAM_ui_form_to_eqn_info;



/**
 * Mapping how ui variables are transformed by equations and callbacks before being assigned to primary ssc inputs
 * across
 */

extern std::unordered_map<std::string, digraph*> SAM_config_to_variable_graph;



/**
 * Maps each ui form with its callbacks. ui variables may be ssc
 * or only found in the ui. Creating using information from digraph
 */
struct callback_info{
    std::vector<std::string> ssc_only_inputs;
    std::vector<std::string> ui_only_inputs;

    std::vector<std::string> all_outputs;

    std::string method_name;
    std::string function_name;
    std::string ui_source;
};

/// Bookmarks active ui form during UI script parsing
extern std::string active_ui;

extern std::string active_object;

extern std::string active_subobject;

extern std::unordered_map<std::string, std::vector<std::string>> SAM_ui_obj_to_enabled_variables;


static std::unordered_map<std::string, std::string> config_to_cmod_name = {
        {"6parsolve", "SixParsolve"},
        {"AllEquityPartnershipFlip", "Equpartflip"},
        {"Battery", "Battery"},
        {"Belpe", "Belpe"},
        {"Biopower", "Biomass"},
        {"Commercial", "Commercial"},
        {"CommunitySolar", "Communitysolar"},
        {"DishStirling", "Tcsdish"},
        {"DSGLIPH", "LinearFresnelDsgIph"},
        {"DSLF", "TcslinearFresnel"},
        {"DSPT", "TcsdirectSteam"},
        {"EmpiricalTrough", "TcstroughEmpirical"},
        {"ETES", "EtesElectricResistance"},
        {"PTES", "EtesPtes"},
        {"FlatPlatePV", "Pvsamv1"},
        {"FuelCell", "Fuelcell"},
        {"GenericCSPSystem", "TcsgenericSolar"},
        {"GenericSystem", "GenericSystem"},
        {"GenericBattery", "GenericBattery"},
        {"GeothermalPower", "Geothermal"},
        {"HighXConcentratingPV", "Hcpv"},
        {"HostDeveloper", "HostDeveloper"},
        {"ISCC", "Tcsiscc"},
        {"LCOECalculator", "Lcoefcr"},
        {"LCOHCalculator", "IphToLcoefcr"},
        {"LeveragedPartnershipFlip", "Levpartflip"},
        {"MerchantPlant", "MerchantPlant"},
        {"MEtidal", "METidal"},
        {"MEwave", "MEWave"},
        {"MSLF", "TcsMSLF"},
        {"MSPT", "TcsmoltenSalt"},
        {"None", "None"},
        {"PhysicalTrough", "TroughPhysical"},
        {"PhysicalTroughIPH", "TroughPhysicalProcessHeat"},
        {"PVBattery", "PVBattery"},
        {"PVWatts", "Pvwattsv8"},
        {"PVWattsBattery", "BattWatts"},
        {"Residential", "Residential"},
        {"SaleLeaseback", "Saleleaseback"},
        {"SCO2", "SCO2"},
        {"SingleOwner", "Singleowner"},
        {"SolarWaterHeating", "Swh"},
        {"StandaloneBattery", "Standalonebattery"},
        {"ThirdParty", "Thirdpartyownership"},
        {"Utilityrate5", "Utilityrate5"},
        {"Utilityrateforecast", "Utilityrateforecast"},
        {"WindPower", "Windpower"}
};

/**
 * Maps each config to the secondary cmod outputs that are used as inputs to the primary cmod
 * e.g. 'Wind Power-Residential': {'wind_obos': wind_turbine_rotor_diameter, rotorD)
 */

std::vector<std::string> get_cmod_var_info(std::string cmod_name, std::string which_type);

void load_primary_cmod_inputs();

void load_secondary_cmod_outputs(std::string cmod_name);

page_info& find_page_info_of_variable(std::string name, std::string config);

/// Find which ui form a variable is defined inside for a given config
std::string find_ui_of_variable(std::string name, std::string config);

class ui_form_extractor;
ui_form_extractor* find_ui_of_object(std::string obj, std::string config);

/// Find the config-independent default VarValue for a variable for a given config
VarValue* find_default_from_ui(std::string name, std::string config);

/// Determine if a variable is a primary ssc input by returning cmod name
std::string which_cmod_as_input(std::string name, std::string config);

// SAM config utils

void get_tech_fin_of_config(const std::string& config, std::string& tech, std::string& fin);

// LK parsing utils

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
    s = unescape(s);
    size_t pos1 = s.find("value()(");
    s = s.substr(pos1 + 8);
    pos1 = s.find(")");
    s = s.substr(0, pos1);

    return true;
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

std::vector<std::string> find_ui_forms_for_config(std::string config_name);

void print_ui_form_to_eqn_variable();

equation_info& find_equation_info_from_edge(edge *edge, std::string config);


#endif //SYSTEM_ADVISOR_MODEL_EXPORTED_MAPS_H
