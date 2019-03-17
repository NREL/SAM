#include <string>
#include <iostream>
#include <fstream>

#include <shared/lib_util.h>
#include <ssc/sscapi.h>

#include "lk_env.h"
#include "lk_eval.h"
#include "data_structures.h"
#include "variable_graph.h"
#include "ui_form_extractor.h"

#include "builder_generator.h"
#include "builder_generator_helper.h"

std::unordered_map<std::string, std::vector<std::string>> builder_generator::m_config_to_modules
    = std::unordered_map<std::string, std::vector<std::string>>();

builder_generator::builder_generator(config_extractor *ce){
    config_ext = ce;
    config_name = config_ext->get_name();
    graph = config_ext->get_variable_graph();
    subgraph = new digraph(config_name);
    graph->subgraph_ssc_only(*subgraph);
    config_symbol = format_as_symbol(config_name);

    // load all cmod variables for this configuration and save the ssc types to the vertices
    auto cmods = SAM_config_to_primary_modules[active_config];
    for (size_t i = 0; i < cmods.size(); i++){
        std::string cmod_name = cmods[i];
        ssc_module_t p_mod = ssc_module_create(const_cast<char*>(cmod_name.c_str()));
        ssc_module_objects.insert({cmod_name, p_mod});

    }
}


enum{
    SOURCE,
    SINK,
    ISOLATED,
    CONNECTED
};

int get_vertex_type(vertex *v){
    if (v->edges_out.size() + v->edges_in.size() == 0)
        return ISOLATED;
    else if (v->edges_out.size() == 0)
        return SINK;
    else if (v->edges_in.size() == 0)
        return SOURCE;
    else
        return CONNECTED;
}
void builder_generator::encode_edge(edge* e) {

}

bool builder_generator::ssc_var_user_defined(std::string var_name){
    if (!graph->find_vertex(var_name, true))
        // not ssc
        return false;
    if (vertex* v = subgraph->find_vertex(var_name, true)){
        // not user defined unless it's a source node
        int type = get_vertex_type(v);
        if (type != SOURCE && type != ISOLATED)
            return false;
    }
    return true;
}

void builder_generator::select_ui_variables(std::string ui_name, std::map<std::string, vertex*>& var_map){
    std::unordered_map<std::string, VarValue> ui_def = SAM_ui_form_to_defaults[ui_name];
    for (auto it = ui_def.begin(); it != ui_def.end(); ++it){
        std::string var_name = it->first;
        VarValue* vv = &(it->second);

        if (ssc_var_user_defined(var_name)){
            vertex* v = graph->find_vertex(var_name, true);
            v->ui_form = ui_name;
            var_map.insert({var_name, v});
        }
    }
}

void builder_generator::gather_variables(){
    // gather modules by input page
    std::vector<page_info>& pg_info = SAM_config_to_input_pages.find(active_config)->second;

    for (size_t p = 0; p < pg_info.size(); p++){
        if (pg_info[p].common_uiforms.size() > 0){
            // add the ui form variables into a group based on the sidebar title
            std::string group_name = pg_info[p].sidebar_title
                    + (pg_info[p].exclusive_uiforms.size() > 0 ? "Common" : "");
            std::map<std::string, vertex*> map;
            modules.insert({group_name, map});
            modules_order.push_back(group_name);
            std::map<std::string, vertex*>* var_map = &(modules.find(group_name)->second);

            for (size_t i = 0; i < pg_info[p].common_uiforms.size(); i++) {
                // add all the variables and associate their VarValue with the vertex
                std::string ui_name = pg_info[p].common_uiforms[i];
                select_ui_variables(ui_name, *var_map);
            }
        }

        // add each exclusive form as its own (sub)module
        for (size_t i = 0; i < pg_info[p].exclusive_uiforms.size(); i++) {
            // add all the variables and associate their VarValue with the vertex
            std::string ui_name = pg_info[p].exclusive_uiforms[i];

            std::string submod_name = ui_name;
            modules.insert({submod_name, std::map<std::string, vertex*>()});
            modules_order.push_back(submod_name);
            std::map<std::string, vertex*>& var_map = modules.find(submod_name)->second;

            select_ui_variables(ui_name, var_map);
        }
    }

    // add modules and inputs that are not in UI
    std::vector<std::string> primary_cmods = SAM_config_to_primary_modules[config_name];

    // extra modules first such as adjustments factors
    for (size_t i = 0; i < primary_cmods.size(); i++) {
        std::string cmod_name = primary_cmods[i];
        std::vector<std::string> map = cmod_to_extra_modules[cmod_name];
        for (size_t j = 0; j < map.size(); j++){
            std::string module_name = map[j];
            // add the module
            modules.insert({module_name, std::map<std::string, vertex*>()});
            modules_order.push_back(module_name);

            // add the variables
            auto extra_vars = extra_modules_to_members[module_name];
            auto module_map = modules[module_name];
            for (size_t k = 0; k < extra_vars.size(); j++){
                vertex* v = graph->add_vertex(extra_vars[k], true);
                v->cmod = cmod_name;
                module_map.insert({extra_vars[k], v});
            }
        }

        // add an extra "common" module to catch unsorted variables
        modules.insert({"Common", std::map<std::string, vertex*>()});
        modules_order.push_back("Common");

        // go through all the ssc variables and make sure they are in the graph and in modules
        std::vector<std::string> all_ssc = SAM_cmod_to_inputs[cmod_name];
        for (size_t j = 0; j < all_ssc.size(); j++){
            std::string var = all_ssc[j];
            vertex* v = graph->find_vertex(var, true);
            if (!v){
                // see if it belongs to a module
                std::string md = find_module_of_var(var, cmod_name);

                v = graph->add_vertex(var, true);
                v->cmod = cmod_name;
                if (md.length() != 0){
                    modules.find(md)->second.insert({var, v});
                }
                // add it to "common"
                else{
                    modules.find("Common")->second.insert({var, v});
                }
            }
        }

        // delete Common if it's empty
        if (modules["Common"].size() == 0)
            modules.erase("Common");
    }

    m_config_to_modules.insert({config_name, modules_order});
}


void builder_generator::export_variables_json(){
    std::ofstream json;
    json.open(filepath + "/defaults/" + config_symbol + ".json");
    assert(json.is_open());

    json << "{\n";
    json << "\t\"" + config_symbol + "_defaults\": {\n";

    std::unordered_map<std::string, bool> completed_tables;
    for (size_t i = 0; i < modules_order.size(); i++){
        std::string module_name = modules_order[i];
        json << "\t\t\"" + module_name + "\": {\n";
        std::cout << "\n";

        std::map<std::string, vertex*>& map = modules.find(module_name)->second;
        for (auto it = map.begin(); it != map.end(); ++it){
            std::string var = it->first;
            vertex* v = it->second;

            int ssc_type = get_ssc_type(v, ssc_module_objects);

            // if it's a table entry, print the whole table rather than single entry
            if(var.find(":") != std::string::npos){
                var = var.substr(0, var.find(":"));
                ssc_type = SSC_TABLE;
                if (completed_tables.find(var) == completed_tables.end()){
                    completed_tables.insert({var, true});
                }
                else{
                    continue;
                }
            }

            json << "\t\t\t\"" + var + "\": {\n";
            json << "\t\t\t\t\"type\": \"" << ssc_type << "\",\n";
            json << "\t\t\t\t\"value\": ";

            VarValue* vv = SAM_config_to_defaults[config_name][var];

            // vv can be null in the case of variables not available in UI
            json << ssc_value_to_json(ssc_type, vv) << "\n\t\t\t}";

            if (std::next(it) != map.end()) json << ",";
            json << "\n";
        }
        json << "\t\t}";
        if (i != modules_order.size() - 1) json << ",";
        json << "\n";
    }
    json << "\t}\n}";

    json.close();

}



void builder_generator::create_builder_headers(std::string cmod_name, std::string module, std::ofstream &fx_file) {
    std::string module_symbol = format_as_symbol(module);

    std::string sig = "SAM_" + format_as_symbol(cmod_name) + "_" + module_symbol;

    // create the module-specific var_table wrapper

    fx_file << "\t/** \n";
    fx_file << "\t * Create a " << module_symbol << " variable table for a " << config_symbol << " system\n";
    fx_file << "\t * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)\n";
    fx_file << "\t * @param[in,out] err: a pointer to an error object\n";
    fx_file << "\t */\n";

    export_function_declaration(fx_file, sig, sig + "_create", {"const char* def"});

    fx_file << "\n";

    // setters
    std::map<std::string, vertex*> ssc_vars = modules[module];
    std::vector<vertex*> interface_vertices;
    for (auto it = ssc_vars.begin(); it != ssc_vars.end(); ++it){
        std::string var_name = it->first;

        vertex* v = it->second;

        interface_vertices.push_back(v);

        std::string var_symbol = sig + "_" + var_name;

        int ind = (int)SAM_cmod_to_ssc_index[cmod_name][var_name];
        ssc_info_t mod_info = ssc_module_var_info(ssc_module_objects[cmod_name], ind);

        fx_file << "\t/**\n";
        fx_file << "\t * Set " << var_name << ": " << ssc_info_label(mod_info) << "\n";
        fx_file << "\t * type: " << spell_type(ssc_info_data_type(mod_info)) << "\n";
        fx_file << "\t * units: ";
        std::string units_str = ssc_info_units(mod_info);
        if (units_str.length() > 0)
            fx_file << units_str << "\n";
        else
            fx_file << "None\n";

        fx_file << "\t * options: ";
        std::string meta_str = ssc_info_meta(mod_info);
        if (meta_str.length() > 0)
            fx_file << meta_str << "\n";
        else
            fx_file << "None\n";

        fx_file << "\t * constraints: ";
        std::string cons_str = ssc_info_constraints(mod_info);
        if (cons_str.length() > 0)
            fx_file << cons_str << "\n";
        else
            fx_file << "None\n";

        fx_file << "\t * required if: ";
        std::string req_str = ssc_info_required(mod_info);
        if (req_str.find('=') != std::string::npos)
            fx_file << req_str << "\n";
        else
            fx_file << "None\n";
        fx_file << "\t */\n";

        export_function_declaration(fx_file, "void", var_symbol + "_set", {sig + " ptr",
                                                                           print_parameter_type(v, cmod_name,
                                                                                                ssc_module_objects)});
    }

    // getters
    fx_file << "\n\t/**\n";
    fx_file << "\t * Getters\n\t */\n\n";

    for (size_t i = 0; i < interface_vertices.size(); i++){
        vertex* v = interface_vertices[i];
        std::string var_symbol = sig + "_" + v->name;

        export_function_declaration(fx_file, print_return_type(v, cmod_name, ssc_module_objects), var_symbol + "_get", {sig + " ptr"});
    }
}

void builder_generator::create_api_data(std::string cmod_name) {
    std::string cmod_symbol = format_as_symbol(cmod_name);

    std::ofstream data_file;
    data_file.open(filepath + "/" +  cmod_symbol + "-data.h");
    assert(data_file.is_open());

    data_file << "#ifndef SAM_" << util::upper_case(cmod_symbol) << "_DATA_H_\n";
    data_file << "#define SAM_" << util::upper_case(cmod_symbol) << "_DATA_H_\n\n";

    const char* includes = "#include <stdint.h>\n"
                           "#ifdef __cplusplus\n"
                           "extern \"C\"\n"
                           "{\n"
                           "#endif\n\n";

    data_file << includes;

    std::string sig = "\ttypedef void * SAM_" + cmod_symbol ;

    // declaration of technology system
    data_file << "\t /** Technology module object */\n";
    data_file << sig << ";\n";

    sig = sig + "_";

    // print out the equation and callbacks for the cmod
    create_builder_definitions(cmod_name);

    // declaration of modules and submodules by group
    std::ofstream fx_file;
    fx_file.open(filepath + "/" + cmod_symbol + "-builder.h");
    assert(fx_file.is_open());

    fx_file << "#ifndef SAM_" << util::upper_case(cmod_symbol) << "_FUNCTIONS_H_\n";
    fx_file << "#define SAM_" << util::upper_case(cmod_symbol) << "_FUNCTIONS_H_\n\n";
    fx_file << "#include \"" << cmod_symbol << "-data.h\"\n\n";

    fx_file << includes;

    for (size_t i = 0; i < modules_order.size(); i++){
        assert(modules[modules_order[i]].size() > 0);
        std::string module_name = modules_order[i];
        fx_file << "\t /** " << module_name << " */\n";
        fx_file << sig << format_as_symbol(module_name) << ";\n\n";
        create_builder_headers(cmod_name, module_name, fx_file);
        fx_file << "\n\n";
    }

    const char* footer = "#ifdef __cplusplus\n"
                         "} /* end of extern \"C\" { */\n"
                         "#endif\n\n"
                         "#endif";

    fx_file << footer;
    fx_file.close();

    data_file << footer;
    data_file.close();
}

void builder_generator::create_source_interfaces(std::vector<vertex *> &vertices) {
    for (size_t m = 0; m < modules_order.size(); m++){
        std::string module_name = modules_order[m];
        std::map<std::string, vertex*>& module_vertices = modules.find(module_name)->second;
        for (auto it = module_vertices.begin(); it != module_vertices.end(); ++it){
            vertex* v = it->second;
            int vertex_type = get_vertex_type(v);

            switch(vertex_type) {
                case SOURCE:
                    break;
                case SINK:
                    break;
                case ISOLATED:
                    break;
                case CONNECTED:
                    break;
            }
        }
    }
}

bool builder_generator::eqn_in_subgraph(equation_info eq){
    for (size_t i = 0; i < eq.ui_inputs.size(); i++ ){
        std::string name = eq.ui_inputs[i];
        bool is_ssc = which_cmod_as_input(name, config_name).length() > 0;
        if (!is_ssc && !subgraph->find_vertex(eq.ui_inputs[i], false))
            return false;
    }
    for (size_t i = 0; i < eq.ui_outputs.size(); i++ ){
        std::string name = eq.ui_outputs[i];
        bool is_ssc = which_cmod_as_input(name, config_name).length() > 0;
        if (is_ssc)
            return true;
        if (!is_ssc && !subgraph->find_vertex(eq.ui_outputs[i], false))
            return false;
    }
    return true;
}

void builder_generator::create_builder_definitions(std::string cmod_name) {
    std::ofstream fx_file;
    std::string cmod_symbol = format_as_symbol(cmod_name);

    fx_file.open(filepath + "/" +  cmod_symbol + "-builder.cpp");
    assert(fx_file.is_open());


    fx_file << "#include \"" << cmod_symbol << "-data.h\"\n\n";
    fx_file << "#include \"" << cmod_symbol << "-builder.h\"\n\n";

    std::string sig = "SAM_" + cmod_symbol;


    int n = 0;
    std::unordered_map<std::string, std::vector<vertex*>>& vertices = subgraph->get_vertices();
    std::vector<std::string> ui_forms = find_ui_forms_for_config(config_name);
    for (size_t i = 0; i < ui_forms.size(); i++){
        ui_form_extractor* ui_extractor = SAM_ui_extracted_db.find(ui_forms[i]);
        std::vector<equation_info>* eqns = ui_extractor->get_eqn_infos();
        for (size_t j = 0; j < eqns->size(); j++){
            equation_info eq_info = (*eqns)[j];
            if (eqn_in_subgraph(eq_info)){
                if (i == 2 & j == 10 && sig == "SAM_MSPT"){
                    std::cout << "stop here";
                }
                ui_extractor->translate_to_cplusplus(eq_info, fx_file, config_name );
                fx_file << "\n\n";

                n+=1;
            }
        }
    }
    std::cout << n << "\n";
    fx_file.close();
}

void builder_generator::create_interface(std::string fp) {
    filepath = fp;
    gather_variables();
    export_variables_json();

    std::vector<std::string> primary_cmods = SAM_config_to_primary_modules[config_name];

    for (size_t i = 0; i < primary_cmods.size(); i++) {

        create_api_data(primary_cmods[i]);
    }

    const char* error_obj = "\n"
                            "\t/** SAM error object\n"
                            "\t *\n"
                            "\t * if error_code != 0, there is an error\n"
                            "\t */\n"
                            "\ttypedef struct SAM_error{\n"
                            "\t\tint32_t error_code;\n"
                            "\t\tconst char* message;\n"
                            "\t} SAM_error;\n\n";

    auto udv = get_user_defined_variables();

    auto evalv = get_evaluated_variables();

    size_t all_ssc_vars = 0;

    auto cmods = SAM_config_to_primary_modules[config_name];

    auto all_vars = udv;

    for (size_t i = 0; i < cmods.size(); i++){
        auto vec = get_cmod_var_info(cmods[i], "in");
        all_vars = vec;
        all_ssc_vars += vec.size();
    }

    std::cout << config_name << ": \n";
    std::cout << "number user defined: " << udv.size() << "; number eval: " << evalv.size() << "\n";
    std::cout << "number of all ssc vars: " << all_ssc_vars << " in " << cmods.size() << " cmods\n";

    std::cout << udv << "\n" << evalv << "\n\n\n" << all_vars;

}

std::vector<std::string> builder_generator::get_user_defined_variables(){
    std::vector<std::string> vec;
    for (auto it = modules.begin(); it != modules.end(); ++it){
        for (auto it2 = it->second.begin(); it2 != it->second.end(); ++it2){
            vec.push_back(it2->second->name);
        }
    }
    return vec;
}

std::vector<std::string> builder_generator::get_evaluated_variables() {
    std::vector<std::string> vec;
    auto vertices = subgraph->get_vertices();
    for (auto it = vertices.begin(); it != vertices.end(); ++it){
        if (vertex* v = it->second.at(true)){
            if (get_vertex_type(v) != SOURCE)
                vec.push_back(v->name);
        }
    }
    return vec;
}

