#include "config_extractor.h"
#include "ui_form_extractor.h"
#include "callback_extractor.h"
#include "data_structures.h"

std::unordered_map<std::string, VarTable> SAM_config_to_defaults;
std::unordered_map<std::string, digraph*> SAM_config_to_variable_graph;

config_extractor::config_extractor(std::string name){
    config_name = name;
    var_graph = new digraph(name);
    assert(load_defaults_for_config());
    assert(SAM_config_to_variable_graph.find(config_name) == SAM_config_to_variable_graph.end());
    SAM_config_to_variable_graph.insert({config_name, var_graph});
}


bool config_extractor::load_defaults_for_config(){
    if (SAM_config_to_defaults.find(active_config) != SAM_config_to_defaults.end())
        return true;

    size_t pos = config_name.find('-');
    if (config_name.find('-', pos+1) != std::string::npos)
        pos = config_name.find('-', pos+1);
    std::string filename = config_name.substr(0, pos) + "_" + config_name.substr(pos+1);
    std::string file = defaults_file_dir + filename + ".txt";

    wxFFileInputStream in(file);
    if (!in.IsOk())
    {
        std::cout << "config_extractor could not load defaults for " + active_config << "\n";
            return false;
    }

    VarTable vt;
    bool read_ok = true;

    read_ok = vt.Read_text(in);

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
        vertex* v = var_graph->add_vertex(name, is_ssc_var);
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
        std::vector<equation_info> eqns = SAM_ui_form_to_eqn_info[ui];
        for (size_t j = 0; j < eqns.size(); j++){
            EqnData* eq_data = eqns[j].eqn_data;

            std::vector<std::string> inputs = eqns[j].ui_inputs;
            std::vector<std::string> outputs = eqns[j].ui_outputs;

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
                        callstack += ":MIMO";
                    }

                    // get the expression
                    std::string expression = spell_equation(eq_data->tree);

                    if (!var_graph->add_edge(inputs[s], src_is_ssc, outputs[d], dest_is_ssc, EQN, callstack, expression)){
                        std::cout << "/* config_extractor::map_equations error adding edge between ";
                        std::cout << inputs[s] << " and " + outputs[d] + " */ \n";
                    }
                }
            }

        }
    }
}

/// add this instance to ui_form_extractor_database for future reference
void config_extractor::export_to_ui_form_db(std::string ui_name){
    ui_form_extractor* ui_fe = SAM_ui_extracted_db.find(ui_name);
    assert(ui_fe);

    lk::vardata_t *cbvar = m_env.lookup( "on_load", true);
    assert(cbvar->type() == lk::vardata_t::HASH );

    lk::varhash_t* h = cbvar->hash();
    for (auto it = h->begin(); it != h->end(); ++it){
        std::string obj_name = it->first.ToStdString();
        ui_fe->m_onload_obj.push_back(obj_name);
    }

    cbvar = m_env.lookup( "on_change", true);
    assert(cbvar->type() == lk::vardata_t::HASH );

    h = cbvar->hash();
    for (auto it = h->begin(); it != h->end(); ++it){
        std::string obj_name = it->first.ToStdString();
        ui_fe->m_onchange_obj.push_back(obj_name);
    }
}

/// setting active_config and active_ui
void config_extractor::register_callback_functions() {
    std::vector<page_info> pages = SAM_config_to_input_pages[config_name];

    callback_extractor cb_ext(config_name, m_env);

    // for all the ui forms, parse the callback functions
    std::vector<std::string> all_ui = find_ui_forms_for_config(config_name);

//    cb_ext.parse_script(SAM_ui_extracted_db.find("Financial TOD Factors")->get_callback_script());
//    cb_ext.extract_functions();
//    return;

    for (size_t i = 0; i < all_ui.size(); i++){
        active_ui = all_ui[i];
        cb_ext.parse_script(SAM_ui_extracted_db.find(all_ui[i])->get_callback_script());
        export_to_ui_form_db(active_ui);
    }
    // not used during extract_functions since all functions have been collected for a single config
    active_ui = "";

    // run through first time to get variable mapping, making sure to run the equations for special variables...

    cb_ext.extract_functions();

    // run through until nothing changes...
}