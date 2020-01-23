#include <string>
#include <iostream>
#include <fstream>
#include <vector>
#include <memory>
#include <set>

#include <shared/lib_util.h>
#include <ssc/sscapi.h>
#include <ssc/ssc_equations.h>

#include "lk_env.h"
#include "lk_eval.h"
#include "data_structures.h"
#include "variable_graph.h"
#include "ui_form_extractor.h"

#include "builder_generator.h"
#include "builder_generator_helper.h"
#include "builder_C_API.h"
#include "builder_PySAM.h"

std::unordered_map<std::string, bool> SAM_completed_cmods;


std::unordered_map<std::string, std::vector<std::string> > builder_generator::m_config_to_modules
    = std::unordered_map<std::string, std::vector<std::string> >();

std::unordered_map<std::string, std::unordered_map<std::string, callback_info> > builder_generator::m_config_to_callback_info
        = std::unordered_map<std::string, std::unordered_map<std::string, callback_info> >();


builder_generator::builder_generator(config_extractor *ce){
    config_ext = ce;
    config_name = config_ext->get_name();
    graph = config_ext->get_variable_graph();
    vertex* v = graph->find_vertex("d_rec", false);
    vertex* v2 = graph->find_vertex("d_rec", true);

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


vertex * builder_generator::var_user_defined(std::string var_name){
    vertex* v = subgraph->find_vertex(var_name, true);
    vertex* v2 = subgraph->find_vertex(var_name, false);
    if (v){
        // not user defined unless it's a source node
        int type = get_vertex_type(v);
        if (type != SOURCE && type != ISOLATED)
            return nullptr;
        else
            return v;
    }
    else if (v2){
        int type = get_vertex_type(v2);
        if (type != SOURCE && type != ISOLATED)
            return nullptr;
        else
            return v2;
    }
    return nullptr;
}

void builder_generator::select_ui_variables(std::string ui_name, std::map<std::string, vertex*>& var_map){
    std::unordered_map<std::string, VarValue> ui_def = SAM_ui_form_to_defaults[ui_name];
    for (auto it = ui_def.begin(); it != ui_def.end(); ++it){
        std::string var_name = it->first;
        VarValue* vv = &(it->second);

        if (vertex* v = var_user_defined(var_name)){
            v->ui_form = ui_name;
            var_map.insert({var_name, v});
        }
    }
}

void builder_generator::gather_variables_ssc(const std::string &cmod_name) {
    ssc_module_t p_mod = ssc_module_create(const_cast<char*>(cmod_name.c_str()));

    int var_index = 0;
    ssc_info_t mod_info = ssc_module_var_info(p_mod, var_index);
    std::map<std::string, var_def> adj_map;
    std::map<std::string, var_def> outputs_map;
    while (mod_info){

        var_def vd;
        vd.is_ssc = true;
        vd.cmod = cmod_name;
        vd.constraints = ssc_info_constraints(mod_info);
        vd.meta = ssc_info_meta(mod_info);
        vd.doc = std::string(ssc_info_label(mod_info));
        std::string units = std::string(ssc_info_units(mod_info));
        if (units.length() != 0)
            vd.doc += " [" + units + "]";
        vd.reqif = ssc_info_required(mod_info);
        vd.group = ssc_info_group(mod_info);
        vd.name = ssc_info_name(mod_info);


        if (vd.group.length() == 0 || vd.group == vd.cmod){
			std::string ui_name = find_ui_of_variable(vd.name, config_name);
			if (ui_name.empty())
                vd.group = "Common";
			else {
				vd.group = format_as_symbol(ui_name);
				size_t pos = vd.group.find(format_as_symbol(cmod_name));
				if (pos != std::string::npos) {
					vd.group = vd.group.substr(pos + format_as_symbol(cmod_name).length());
				}
			}
        }


        std::vector<std::string> ssctype_str = {"invalid", "string", "number", "array", "matrix", "table"};
        vd.type_n = ssc_info_data_type(mod_info);
        vd.type = ssctype_str[vd.type_n];

        if (vd.group == "Adjustment Factors") {
            size_t pos = vd.name.find(':');
            size_t pos2 = vd.name.find("adjust");
            vd.name = vd.name.substr(0, pos2) + vd.name.substr(pos+1);
            adj_map.insert({vd.name, vd});
            ++var_index;
            mod_info = ssc_module_var_info(p_mod, var_index);
            continue;
        }

        int var_type = ssc_info_var_type(mod_info);

        size_t pos = vd.name.find(':');
        // if it's a table entry, x:y, add x_y as the variable symbol and keep x:y as the name
        if(pos != std::string::npos){
            std::string str = vd.name;
            std::replace(str.begin(), str.end(), ':', '_');
            if (var_type == 2){
                outputs_map.insert({str, vd});
            }
            else{
                auto it = m_vardefs.find(vd.group);
                // if the group doesn't exist, add it
                if (it == m_vardefs.end()){
                    m_vardefs.insert({vd.group, std::map<std::string, var_def>()});
                    it = m_vardefs.find(vd.group);
                    vardefs_order.push_back(vd.group);
                }
                it->second.insert({str, vd});
            }
        }
        else{
            // regular values
            std::string var_symbol = remove_periods(vd.name);
            if ( var_type == 1 || var_type == 3) {
                auto it = m_vardefs.find(vd.group);
                if (it == m_vardefs.end()){
                    m_vardefs.insert({vd.group, std::map<std::string, var_def>()});
                    it = m_vardefs.find(vd.group);
                    vardefs_order.push_back(vd.group);
                }

                modules_order.push_back(vd.group);
                it->second.insert({var_symbol, vd});
            }

            else if ( var_type == 2) {
                outputs_map.insert({var_symbol, vd});
            }
        }

        ++var_index;
        mod_info = ssc_module_var_info(p_mod, var_index);
    }
    if (adj_map.size() > 0){
        m_vardefs.insert({"AdjustmentFactors", adj_map});
        vardefs_order.push_back("AdjustmentFactors");
    }
    m_vardefs.insert({"Outputs", outputs_map});
    vardefs_order.push_back("Outputs");

}

void builder_generator::gather_equations(const std::string &cmod) {
    std::string cmod_symbol = format_as_symbol(cmod);
    size_t i = 0;
    while (ssc_equation_table[i].func != nullptr){
        ssc_equation_entry entry = ssc_equation_table[i];
        if (std::strcmp(entry.cmod, cmod_symbol.c_str()) != 0){
            i++;
            continue;
        }
        std::string group_name;
        std::string func_name = entry.name;
        size_t pos = func_name.find('_');
        // if the first underscore-delimited substring is a group name, the eqn goes under that group
        group_name = func_name.substr(0, pos);
        if (m_vardefs.find(group_name) == m_vardefs.end()){
            // otherwise the eqn goes under the cmod class
            group_name = cmod_symbol;
            pos = -1;
        }
        func_name = func_name.substr(pos + 1);
        auto it = m_eqn_entries.find(group_name);
        if (it == m_eqn_entries.end()){
            m_eqn_entries[group_name] = std::map<std::string, ssc_equation_entry>();
            it = m_eqn_entries.find(group_name);
        }
        it->second.insert({func_name, entry});
        i++;
    }
}

void builder_generator::gather_variables(){
    // gather modules by input page
    std::vector<page_info>& pg_info = SAM_config_to_input_pages.find(config_name)->second;

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

std::string defaults_filename(std::string cmod_symbol, const std::string &config_name){

    size_t pos = config_name.find_last_of('-');
    auto tech_entry = config_to_cmod_name.find(format_as_symbol(config_name.substr(0, pos)));
    if (tech_entry == config_to_cmod_name.end()){
        std::string err = "Error with config " + cmod_symbol + "\": tech entry in \"config_to_cmod_name\" not found.\n";
        throw std::runtime_error(err);
    }
    std::string tech = tech_entry->second;

    auto fin_entry = config_to_cmod_name.find(format_as_symbol(config_name.substr(pos+1)));
    if (fin_entry == config_to_cmod_name.end()){
        std::string err = "Error with config " + cmod_symbol + "\": fin entry in \"config_to_cmod_name\" not found.\n";
        throw std::runtime_error(err);
    }
    std::string fin = fin_entry->second;
    assert(tech.length() + fin.length());

    return cmod_symbol + "_" + format_as_symbol(config_name);

}

//
void builder_generator::export_variables_json(const std::string &cmod, const std::string &defaults_path) {
    std::ofstream json;

    json.open(defaults_path + "/" + defaults_filename(format_as_symbol(cmod), config_name) + ".json");

    // later implement for several financial models
    assert(json.is_open());

    json << "{\n";
    json << "\t\"defaults\": {\n";

    std::unordered_map<std::string, bool> completed_tables;
    for (size_t i = 0; i < vardefs_order.size(); i++){
        std::string module_name = vardefs_order[i];

        if (module_name == "Outputs")
            continue;

        std::string module_symbol = format_as_symbol(module_name);
        json << "\t\t\"" + module_symbol + "\": {";

        bool first = true;
        std::map<std::string, var_def>& map = m_vardefs.find(module_name)->second;
        for (auto it = map.begin(); it != map.end(); ++it){
            std::string var_symbol = it->first;
            var_def v = it->second;

            // if it's a file name, don't assign
            if (var_symbol.find("file") != std::string::npos){
                continue;
            }

            VarValue* vv = nullptr;

            // if adjustment factors, the default values are stored in a table
            if (module_name == "AdjustmentFactors"){
                size_t pos = v.name.find('_');
                std::string adj_type = "adjust";
                if (pos != std::string::npos){
                    adj_type = v.name.substr(0, pos + 1) + adj_type;
                }

                vv = SAM_config_to_defaults[config_name][adj_type];
                if (vv){
                    std::string name = v.name.substr(pos+1);
                    if (name == "hourly" && !(vv->Table().Get("en_hourly")->Boolean()))
                        continue;
                    if (name == "periods" && !(vv->Table().Get("en_periods")->Boolean()))
                        continue;
                    vv = vv->Table().Get(name);
                }
                else
                    continue;
            }
            else
                vv = SAM_config_to_defaults[config_name][v.name];

            // vv can be null in the case of variables not available in UI
            if (!vv && v.reqif != "*")
                continue;

            // if it's an empty string, don't assign
            if (vv && vv->Type() == VV_STRING && vv->AsString().length() == 0)
                continue;

            if (!first) json << ",";
            json << "\n\t\t\t\"" + remove_periods(var_symbol) + "\": ";
            json << ssc_value_to_json(v.type_n, vv);


            first = false;
        }
        json << "\n\t\t}";
        if (i != vardefs_order.size() - 2) json << ",";
        json << "\n";
    }
    json << "\t}\n}";

    json.close();

}


std::unordered_map<std::string, edge *> builder_generator::gather_functions() {
    std::unordered_map<std::string, edge*> unique_subgraph_edges;
    subgraph->get_unique_edge_expressions(unique_subgraph_edges);


    // group all the edges from the same LK object together
    std::unordered_map<std::string, std::unique_ptr<digraph> > fx_object_graphs;

    auto vertices = graph->get_vertices();
    for (auto it = vertices.begin(); it != vertices.end(); ++it){
        for (size_t is_ssc = 0; is_ssc < 2; is_ssc++){
            vertex* v = it->second.at(is_ssc);
            if (!v)
                continue;

            for (size_t i = 0; i < v->edges_out.size(); i++){
                edge* e = v->edges_out[i];
                auto edge_grp = fx_object_graphs.find(e->obj_name);
                if ( edge_grp == fx_object_graphs.end()){
                    std::unique_ptr<digraph> new_graph(new digraph(config_name));
                    vertex* src = new_graph->add_vertex(e->src->name, e->src->is_ssc_var, e->ui_form);
                    vertex* dest = new_graph->add_vertex(e->dest->name, e->dest->is_ssc_var, e->ui_form);
                    new_graph->add_edge(src, dest, e->type, e->obj_name, e->expression, e->ui_form, e->root);
                    fx_object_graphs[e->obj_name] = std::move(new_graph);
                }
                else{
                    vertex* s = edge_grp->second->add_vertex(e->src->name, e->src->is_ssc_var, e->ui_form);
                    vertex* d = edge_grp->second->add_vertex(e->dest->name, e->dest->is_ssc_var, e->ui_form);
                    edge_grp->second->add_edge(s, d, e->type, e->obj_name, e->expression, e->ui_form, e->root);
                }
            }
        }
    }

    // for edges in subgraph, sort information in equation_info and callback_info
    for (auto it = unique_subgraph_edges.begin(); it != unique_subgraph_edges.end(); ++it){

        edge* e = it->second;

        // for equations, all the inputs and outputs are stored in equation_info
        if (e->type == 0){

            equation_info& eq_info = find_equation_info_from_edge(e, config_name);

            for (size_t k = 0; k < eq_info.all_inputs.size(); k++){
                std::string name = eq_info.all_inputs[k];
                if (which_cmod_as_input(name, config_name).length() > 0)
                    eq_info.ssc_only_inputs.push_back(name);
                else{
                    eq_info.ui_only_inputs.push_back(name);

                }
            }

            for (size_t k = 0; k < eq_info.ui_only_inputs.size(); k++){
                for (size_t m = 0; m < eq_info.all_outputs.size(); m++){
                    vertex* src = subgraph->add_vertex(eq_info.ui_only_inputs[k], false, e->ui_form);
                    vertex* dest = subgraph->find_vertex(eq_info.all_outputs[m], true);
                    subgraph->add_edge(src, dest, e->type, e->obj_name, e->expression, e->ui_form, e->root);
                }
            }
        }
        // for callbacks, need to find inputs and outputs from the original graph
        else{
            auto group_graph = fx_object_graphs.find(it->first);
            assert(group_graph != fx_object_graphs.end());

            // add a new callback_info to the map
            callback_info cb_info;

            auto vec_it = m_config_to_callback_info.find(config_name);
            if (vec_it == m_config_to_callback_info.end())
                m_config_to_callback_info.insert({config_name, std::unordered_map<std::string, callback_info>()});
            auto& cb_info_vec = m_config_to_callback_info.find(config_name)->second;


            // get input/output info for callback_info, rest will be filled in during translation
            auto vertices = group_graph->second->get_vertices();

            // sort the vertices into inputs and outputs
            for (auto v_it = vertices.begin(); v_it != vertices.end(); ++v_it){
                for (size_t is_ssc = 0; is_ssc < 2; is_ssc++){
                    vertex* v = v_it->second.at(is_ssc);
                    if (!v)
                        continue;
                    int type = get_vertex_type(v);

                    // if it is another cmod, add all the ui inputs of that cmod
                    auto it = SAM_cmod_to_inputs.find(v->name);
                    if (it != SAM_cmod_to_inputs.end()) {
                        for (size_t i = 0; i < it->second.size(); i++) {
                            cb_info.ui_only_inputs.push_back(it->second[i]);
                            // add new inputs and connect it only to the secondary cmod
                            vertex* src = subgraph->add_vertex(it->second[i], false, e->ui_form);
                            src->cmod = v->name;

                            vertex* dest = subgraph->find_vertex(v->name, false);
                            dest->cmod = v->name;
                            subgraph->add_edge(src, dest, e->type, e->obj_name, e->expression, e->ui_form, e->root);

                            // add to original graph also
                            src = graph->add_vertex(it->second[i], false, e->ui_form);
                            src->cmod = v->name;
                            dest = graph->find_vertex(v->name, false);
                            dest->cmod = v->name;
                            graph->add_edge(src, dest, e->type, e->obj_name, e->expression, e->ui_form, e->root);
                        }
                        continue;
                    }

                    // if it's an input
                    if (type == SOURCE){
                        if (is_ssc)
                            cb_info.ssc_only_inputs.push_back(v->name);
                        // might be name of secondary compute module
                        else{
                            // add new input and copy its edges from original graph
                            cb_info.ui_only_inputs.push_back(v->name);
                            vertex* src = subgraph->add_vertex(v->name, false);

                            vertex* src_og = graph->find_vertex(v->name, false);
                            assert(src_og);
                            for (size_t m = 0; m < src_og->edges_out.size(); m++){
                                vertex* dest_og = src_og->edges_out[m]->dest;
                                vertex* dest = subgraph->add_vertex(dest_og->name, dest_og->is_ssc_var, dest_og->ui_form);

                                subgraph->add_edge(src, dest, e->type, e->obj_name, e->expression, e->ui_form, e->root);
                            }
                        }
                    }
                    else if (type == SINK){
                        cb_info.all_outputs.push_back(v->name);
                    }
                }
            }

            cb_info_vec.insert({e->obj_name, cb_info});

        }
    }

    return unique_subgraph_edges;
}



void builder_generator::create_api_header(std::string cmod_name) {
    std::string cmod_symbol = format_as_symbol(cmod_name);

    // declaration of modules and submodules by group
    std::ofstream fx_file;
    fx_file.open(filepath + "/" + cmod_symbol + "-builder.h");
    assert(fx_file.is_open());

    fx_file << "#ifndef SAM_" << util::upper_case(cmod_symbol) << "_FUNCTIONS_H_\n";
    fx_file << "#define SAM_" << util::upper_case(cmod_symbol) << "_FUNCTIONS_H_\n\n";
    fx_file << "#include \"" << cmod_symbol << "-data.h\"\n\n";

    const char* includes = "#include <stdint.h>\n"
                           "#ifdef __cplusplus\n"
                           "extern \"C\"\n"
                           "{\n"
                           "#endif\n\n";

    fx_file << includes;

    for (size_t i = 0; i < modules_order.size(); i++){
        if (modules[modules_order[i]].size() == 0){
            continue;
        }
        std::string module_name = modules_order[i];
//        create_SAM_headers(cmod_name, module_name, fx_file);
        fx_file << "\n\n";
    }

    const char* footer = "#ifdef __cplusplus\n"
                         "} /* end of extern \"C\" { */\n"
                         "#endif\n\n"
                         "#endif";

    fx_file << footer;
    fx_file.close();
}



bool builder_generator::eqn_in_subgraph(equation_info eq){
    for (size_t i = 0; i < eq.all_inputs.size(); i++ ){
        std::string name = eq.all_inputs[i];
        bool is_ssc = which_cmod_as_input(name, config_name).length() > 0;
        if (!is_ssc && !subgraph->find_vertex(eq.all_inputs[i], false))
            return false;
    }
    for (size_t i = 0; i < eq.all_outputs.size(); i++ ){
        std::string name = eq.all_outputs[i];
        bool is_ssc = which_cmod_as_input(name, config_name).length() > 0;
        if (is_ssc)
            return true;
        if (!is_ssc && !subgraph->find_vertex(eq.all_outputs[i], false))
            return false;
    }
    return true;
}

void builder_generator::create_cmod_builder_cpp(std::string cmod_name,
                                                const std::unordered_map<std::string, edge *> &unique_edge_obj_names) {
    // open header file
    std::ofstream header_file;
    header_file.open(filepath + "/cmod_" +  cmod_name + "-builder.h");
    assert(header_file.is_open());

    header_file << "#ifndef _CMOD_" << util::upper_case(cmod_name) << "_BUILDER_H_\n";
    header_file << "#define _CMOD_" << util::upper_case(cmod_name) << "_BUILDER_H_\n";

    const char* include_h = "\n"
                            "#include \"vartab.h\"";

    header_file << include_h << "\n\n\n";

    // open cpp file
    std::ofstream cpp_file;
    std::string cmod_symbol = format_as_symbol(cmod_name);

    cpp_file.open(filepath + "/cmod_" +  cmod_name + "-builder.cpp");
    assert(cpp_file.is_open());


    const char* include_cpp = "#include <string>\n"
                           "#include <vector>\n\n"
                           "#include \"vartab.h\"\n\n";

    cpp_file << include_cpp;
    cpp_file << "#include \"cmod_" << cmod_name << "-builder.h\"\n\n";

    std::string sig = "SAM_" + cmod_symbol;


    int number_functions_printed = 0;

    for (auto it = unique_edge_obj_names.begin(); it != unique_edge_obj_names.end(); ++it){
        edge* e = it->second;
        // translate equations
        if (e->type == 0){

            equation_info& eq_info = find_equation_info_from_edge(e, config_name);
            assert(eq_info.eqn_data->tree);

            try {
                if (config_ext->completed_equation_signatures.find(&eq_info)
                        == config_ext->completed_equation_signatures.end()){
                    translate_equation_to_cplusplus(config_ext, eq_info, cpp_file, cmod_name);
                }
            }
            catch (std::exception& e){
                std::cout << e.what() << "\n";
            }
            cpp_file << "\n\n";

            std::string fx_sig = config_ext->completed_equation_signatures[&eq_info];

            std::cout << e->ui_form << fx_sig;


            // make a nice comment block
            header_file << "//\n// Evaluates ";
            for (size_t k = 0; k < eq_info.all_outputs.size(); k++){
                header_file << eq_info.all_outputs[k];
                if (k != eq_info.all_outputs.size() - 1)
                    header_file << ", ";
            }
            header_file << " for a " << e->ui_form << " module\n";
            header_file << "// @param *vt: a var_table* that contains: ";
            for (size_t k = 0; k < eq_info.all_inputs.size(); k++){
                header_file << eq_info.all_inputs[k];
                if (k != eq_info.all_inputs.size() - 1)
                    header_file << ", ";
                else header_file << "\n";
            }
            header_file << "// @returns single value or var_table\n//\n";
            header_file << fx_sig << ";\n\n";

        }
        // translate callbacks
        else{
            std::string obj_name = e->obj_name;

            size_t pos = e->obj_name.find(":");
            if (pos != std::string::npos){
                std::string s = e->obj_name.substr(pos + 1);
                if (s != "MIMO")
                    obj_name = e->obj_name.substr(0, pos);
                else
                    obj_name = s;
            }

            std::vector<std::string> method_names = {"", "on_load", "on_change"};

            callback_info& cb_info = m_config_to_callback_info.find(config_name)->second.find(e->obj_name)->second;

            assert(cb_info.ui_only_inputs.size() + cb_info.ssc_only_inputs.size () > 0);

            cb_info.function_name = obj_name;
            cb_info.method_name = method_names[e->type];
            cb_info.ui_source = find_ui_of_object(obj_name, config_name)->ui_form_name;

            try {
                if (config_ext->completed_callback_signatures.find(&cb_info)
                    == config_ext->completed_callback_signatures.end()){
                    translate_callback_to_cplusplus(config_ext, cb_info, cpp_file, cmod_name);
                }
            }
            catch (std::exception& e){
                std::cout << e.what() << "\n";
            }

            cpp_file << "\n\n";

            std::string fx_sig = config_ext->completed_callback_signatures[&cb_info];

            std::cout << e->ui_form << fx_sig;


            // make a nice comment block
            header_file << "//\n// Function ";
            for (size_t k = 0; k < cb_info.all_outputs.size(); k++){
                header_file << cb_info.all_outputs[k];
                if (k != cb_info.all_outputs.size() - 1)
                    header_file << ", ";
            }
            header_file << " for a " << e->ui_form << " module\n";
            header_file << "// @param *vt: a var_table* that contains: ";
            for (size_t k = 0; k < cb_info.ssc_only_inputs.size(); k++){
                header_file << cb_info.ssc_only_inputs[k];
                if (k != cb_info.ssc_only_inputs.size() - 1)
                    header_file << ", ";
                else header_file << "\n";
            }
            for (size_t k = 0; k < cb_info.ui_only_inputs.size(); k++){
                header_file << cb_info.ui_only_inputs[k];
                if (k != cb_info.ui_only_inputs.size() - 1)
                    header_file << ", ";
                else header_file << "\n";
            }
            header_file << "// @param[in,out] *cxt: a invoke_t* that for storing the results\n";
            header_file << "// @returns single value or var_table\n//\n";
            header_file << fx_sig << ";\n\n";
        }
    }

    std::cout << number_functions_printed << "\n";
    cpp_file.close();


    header_file << "#endif";
    header_file.close();
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


void builder_generator::create_all(std::string cmod, const std::string &defaults_path, const std::string &api_path,
                                   const std::string &pysam_path, bool print_json) {

    bool print_capi = true;
    bool print_pysam = true;

    // gather functions before variables to add in ui-only variables that may be skipped in subgraph
//    std::unordered_map<std::string, edge*> unique_subgraph_edges = gather_functions();

    // epand the subgraph to include ui variables which may affect downstream ssc variables
//    graph->subgraph_ssc_to_ui(*subgraph);


    gather_variables_ssc(cmod);
    gather_equations(cmod);

    if (print_json){
        std::cout << "defaults JSON... ";
        export_variables_json(cmod, defaults_path);
    }

    if (SAM_completed_cmods.find(cmod)!= SAM_completed_cmods.end()){
        std::cout << "Done\n";
        return;
    }
//

    // create C API
    if (print_capi){
        std::cout << "C API files... ";
        builder_C_API c_API(this);

        c_API.create_SAM_headers(cmod, api_path + "/include");
        c_API.create_SAM_definitions(cmod, api_path + "/modules");
    }

    if (print_pysam){
        std::cout << "PySAM files... ";
        builder_PySAM pySAM(this);
        pySAM.create_PySAM_files(cmod, pysam_path);
    }

    SAM_completed_cmods.insert({cmod, 1});


//    gather_variables();


//    create_cmod_builder_cpp(primary_cmods[0], unique_subgraph_edges);


    std::cout << "Done\n";



    // print var_info table
//    print_var_info_table(config_name, filepath);


}



