#include <string>
#include <iostream>
#include <fstream>

#include <shared/lib_util.h>
#include <ssc/sscapi.h>

#include "lk_env.h"
#include "data_structures.h"
#include "variable_graph.h"

#include "builder_generator.h"


std::string format_as_code(std::string s){
    std::string::iterator end_pos = std::remove(s.begin(), s.end(), ' ');
    s.erase(end_pos, s.end());
    return s.substr(0, s.find('-'));
}

void builder_generator::load_cmods(){
    auto cmods = SAM_config_to_primary_modules[active_config];
    for (size_t i = 0; i < cmods.size(); i++){
        std::string cmod_name = cmods[i];
        ssc_module_t p_mod = ssc_module_create(const_cast<char*>(cmod_name.c_str()));
        ssc_module_objects.insert({cmod_name, p_mod});
    }
}

builder_generator::builder_generator(config_extractor *ce){
    config_ext = ce;
    config_name = config_ext->get_name();
    graph = config_ext->get_variable_graph();
    subgraph = new digraph(config_name);
    graph->subgraph_ssc_only(*subgraph);
    symbol_name = format_as_code(config_name);
    load_cmods();
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
        if (get_vertex_type(v) != SOURCE)
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
            std::string group_name = pg_info[p].sidebar_title + "Common";
            modules.insert({group_name, std::map<std::string, vertex*>()});
            modules_order.push_back(group_name);
            std::map<std::string, vertex*>& var_map = modules.find(group_name)->second;

            for (size_t i = 0; i < pg_info[p].common_uiforms.size(); i++) {
                // add all the variables and associate their VarValue with the vertex
                std::string ui_name = pg_info[p].common_uiforms[i];
                select_ui_variables(ui_name, var_map);
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
}

std::string get_parameter_type(vertex* v){
    VarValue* vv = find_default_from_ui(v->name, active_config);
    assert(vv);
    int data_type = vv->Type();

    switch(data_type) {
        case VV_INVALID:
            return "/* INVALID INPUT */";
        case VV_BINARY:
            return "/* NOT IMPLEMENTED: binary input */";
        case VV_TABLE:
            return "ssc_data_t table";
        case VV_STRING:
            return "const char* string";
        case VV_MATRIX:
            return "float* matrix, int nr, int nc";
        case VV_ARRAY:
            return "float* array, int length";
        case VV_NUMBER:
            return "float number";
    }
}

std::string get_return_type(vertex* v){
    VarValue* vv = find_default_from_ui(v->name, active_config);
    assert(vv);
    int data_type = vv->Type();

    switch(data_type) {
        case VV_INVALID:
            return "/* INVALID INPUT */";
        case VV_BINARY:
            return "/* NOT IMPLEMENTED: binary input */";
        case VV_TABLE:
            return "ssc_data_t";
        case VV_STRING:
            return "const char*";
        case VV_MATRIX:
            return "float*";
        case VV_ARRAY:
            return "float*";
        case VV_NUMBER:
            return "float";
    }
}

void export_function_declaration(std::ofstream& ff, std::string return_type, std::string name,
        std::vector<std::string> inputs){

    ff << "\tSAM_EXPORT " << return_type << " " << name << "(";
    for (size_t i = 0; i < inputs.size(); i++){
        ff << inputs[i] << ", ";
    }
    ff << "SAM_error* err);\n\n";
}

void builder_generator::create_api_functions(std::string module){
    std::ofstream fx_file;
    std::string module_symbol = format_as_code(module);
    fx_file.open(filepath + "/" + module_symbol + ".h");
    assert(fx_file.is_open());

    fx_file << "#ifndef SAM_" << util::upper_case(module_symbol) << "_FUNCTIONS_H_\n";
    fx_file << "#define SAM_" << util::upper_case(module_symbol) << "_FUNCTIONS_H_\n\n";
    fx_file << "#include \"" << symbol_name << "-data.h\"\n\n";

    const char* includes = "#ifdef __cplusplus\n"
                           "extern \"C\"\n"
                           "{\n"
                           "#endif\n\n";

    fx_file << includes;

    std::string sig = "SAM_" + symbol_name + "_" + module_symbol;

    // create the module-specific var_table wrapper

    fx_file << "/** \n";
    fx_file << " * Create a " << module_symbol << " variable table for a " << symbol_name << " system\\n";
    fx_file << " * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)\n";
    fx_file << " * @param[in,out] err: a pointer to an error object\n";
    fx_file << "*/\n\n";

    export_function_declaration(fx_file, sig, sig + "_Create", {"const char* def"});



    // setter
    std::map<std::string, vertex*> map = modules.find(module)->second;
    for (auto it = map.begin(); it != map.end(); ++it){
        std::string var_symbol = sig + "_" + it->first;
        vertex* v = it->second;

        fx_file << "/**\n";
        fx_file << " * "

        export_function_declaration(fx_file, "void", var_symbol + "_Set", {sig + "ptr", get_parameter_type(v)});

    }
    for (auto it = map.begin(); it != map.end(); ++it){
        std::string var_symbol = sig + "_" + it->first;
        vertex* v = it->second;

        // getter
        export_function_declaration(fx_file, get_return_type(v), var_symbol+"_Get", {sig + "ptr"});
    }

    const char* footer = "#ifdef __cplusplus\n"
                         "} /* end of extern \"C\" { */\n"
                         "#endif\n\n"
                         "#endif";

    fx_file << footer;
}

void builder_generator::create_api_data(){
    std::ofstream data_file;
    data_file.open(filepath + "/" +  symbol_name + "-data.h");
    assert(data_file.is_open());

    data_file << "#ifndef SAM_" << util::upper_case(symbol_name) << "_DATA_H_\n";
    data_file << "#define SAM_" << util::upper_case(symbol_name) << "_DATA_H_\n\n";

    const char* includes = "#include <stdint.h>\n"
                           "#ifdef __cplusplus\n"
                           "extern \"C\"\n"
                           "{\n"
                           "#endif\n\n";

    data_file << includes;

    std::string sig = "\ttypedef void * SAM_" + symbol_name ;

    // declaration of technology system
    data_file << "\t /** Technology module object */\n";
    data_file << sig << ";\n";

    sig = sig + "_";

    // declaration of modules and submodules
    for (size_t i = 0; i < modules_order.size(); i++){
        std::string module_name = modules_order[i];
        data_file << "\t /** " << module_name << " */\n";
        data_file << sig << format_as_code(module_name) << ";\n";
        create_api_functions(module_name);
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

    const char* footer = "#ifdef __cplusplus\n"
                         "} /* end of extern \"C\" { */\n"
                         "#endif\n\n"
                         "#endif";

    data_file << error_obj << footer;
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

void builder_generator::get_expressions(){
    std::unordered_map<std::string, std::vector<vertex*>>& vertices = subgraph->get_vertices();
    for (auto it = vertices.begin(); it != vertices.end(); ++it){
        if (vertex* v_ui = it->second.at(0)){
            for (size_t i = 0; i < v_ui->edges_out.size(); i++) {
                edge *e = v_ui->edges_out[i];
                encode_edge(e);
            }
        }
        if (vertex* v_ssc = it->second.at(1)){
            for (size_t i = 0; i < v_ssc->edges_out.size(); i++) {
                edge *e = v_ssc->edges_out[i];
                encode_edge(e);
            }
        }
    }
}

void builder_generator::generate_interface(std::string fp) {
    filepath = fp;
    gather_variables();
    create_api_data();

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

    std::cout << "number user defined: " << udv.size() << "; number eval: " << evalv.size() << "\n";
    std::cout << "number total: " << all_ssc_vars << " in " << cmods.size() << " cmods\n";

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