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

#include "config_extractor.h"
#include "ui_form_extractor.h"
#include "callback_extractor.h"
#include "data_structures.h"

std::unordered_map<std::string, VarTable> SAM_config_to_defaults;
std::unordered_map<std::string, digraph*> SAM_config_to_variable_graph;

config_extractor::config_extractor(std::string name, const std::string &defaults_dir) {
    config_name = name;
    cb_ext = new callback_extractor(name, m_env);
    var_graph = new digraph(name);
    defaults_file_dir = defaults_dir;
    bool loaded = load_defaults_for_config();

    assert(loaded);

//    assert(SAM_config_to_variable_graph.find(config_name) == SAM_config_to_variable_graph.end());
    SAM_config_to_variable_graph.insert({config_name, var_graph});
}


bool config_extractor::load_defaults_for_config(){
    if (SAM_config_to_defaults.find(active_config) != SAM_config_to_defaults.end())
        return true;

    size_t pos = config_name.find('-');
    if (config_name.find('-', pos+1) != std::string::npos)
        pos = config_name.find('-', pos+1);
    std::string filename = config_name.substr(0, pos) + "_" + config_name.substr(pos+1);
    std::string file = defaults_file_dir + filename + ".json";

    VarTable vt;
    bool read_ok = true;

    read_ok = vt.Read_JSON(file);

    if (!read_ok)
    {
        std::cout << "Error reading inputs from external source " << file <<"\n";
        return false;
    }

    SAM_config_to_defaults.insert({active_config, vt});
    load_variables_into_graph(vt);
    return true;
}

size_t config_extractor::load_variables_into_graph(VarTable &vt) {
    size_t n = 0;
    wxArrayString var_names = vt.ListAll(nullptr);
    for (size_t i = 0; i < var_names.size(); i++){
        std::string name = var_names[i].ToStdString();
        std::string cmod = which_cmod_as_input(name, config_name);
        bool is_ssc_var = (cmod.length() > 0);
        vertex* v = var_graph->add_vertex(name, is_ssc_var, find_ui_of_variable(name, config_name));
        v->cmod = cmod;
        if (is_ssc_var) n+=1;
    }
    return n;
}

std::string config_extractor::spell_equation(lk::node_t *node){
    return "";
}


bool config_extractor::map_equations(){
    std::vector<std::string> ui_forms = find_ui_forms_for_config(config_name);

    for (size_t i = 0; i < ui_forms.size(); i++){
        std::string ui = ui_forms[i];
        std::vector<equation_info>& eqns = SAM_ui_form_to_eqn_info[ui];
        for (size_t j = 0; j < eqns.size(); j++){
            EqnData* eq_data = eqns[j].eqn_data;

            std::vector<std::string> inputs = eqns[j].all_inputs;
            std::vector<std::string> outputs = eqns[j].all_outputs;

            for (size_t s = 0; s < inputs.size(); s++){
                for (size_t d = 0; d < outputs.size(); d++){
                    bool src_is_ssc, dest_is_ssc;

                    src_is_ssc = ( which_cmod_as_input(inputs[s], config_name).length() > 0 );
                    dest_is_ssc = ( which_cmod_as_input(outputs[d], config_name).length() > 0 );

                    // get location of eqn
                    std::string callstack = ui;
                    if (eq_data->result_is_output){
                        callstack += ":EQN";
                    }
                    else{
                        callstack += ":" + eq_data->outputs[0] + "_MIMO";
                    }

                    // get the expression
                    std::string expression = spell_equation(eq_data->tree);

                    if (!var_graph->add_edge(inputs[s], src_is_ssc, outputs[d], dest_is_ssc, EQN, callstack,
                                                      expression, ui, eq_data->tree)){
                        std::cout << "/* config_extractor::map_equations error adding edge between ";
                        std::cout << inputs[s] << " and " + outputs[d] + " */ \n";
                    }

                }
            }

        }
    }
	return true;
}

/// add this instance to ui_form_extractor_database for future reference
void config_extractor::export_to_ui_form_db(std::string ui_name){
    ui_form_extractor* ui_fe = SAM_ui_extracted_db.find(ui_name);
    assert(ui_fe);

    lk::vardata_t *cbvar = m_env.lookup( "on_load", true);
    if(cbvar && cbvar->type() == lk::vardata_t::HASH ){
        lk::varhash_t* h = cbvar->hash();
        for (auto it = h->begin(); it != h->end(); ++it){
            std::string obj_name = it->first.ToStdString();
            ui_fe->m_onload_obj.push_back(obj_name);
        }

    }


    cbvar = m_env.lookup( "on_change", true);
    if(cbvar && cbvar->type() == lk::vardata_t::HASH ){
        lk::varhash_t* h = cbvar->hash();
        for (auto it = h->begin(); it != h->end(); ++it){
            std::string obj_name = it->first.ToStdString();
            ui_fe->m_onchange_obj.push_back(obj_name);
        }

    }

}

/// setting active_config and active_ui
void config_extractor::register_callback_functions() {
    std::vector<page_info> pages = SAM_config_to_input_pages[config_name];


    // for all the ui forms, parse the callback functions
    std::vector<std::string> all_ui = find_ui_forms_for_config(config_name);

//    cb_ext.parse_and_export_eqns(SAM_ui_extracted_db.find("Financial TOD Factors")->get_callback_script());
//    cb_ext.extract_functions();
//    return;

    for (size_t i = 0; i < all_ui.size(); i++){
        active_ui = all_ui[i];
        cb_ext->parse_script(SAM_ui_extracted_db.find(all_ui[i])->get_callback_script());
        export_to_ui_form_db(active_ui);
    }
    active_ui = "";

    // run through first time to get variable mapping, making sure to run the equations for special variables...

    cb_ext->extract_functions();

    // run through until nothing changes...
}
