#ifndef SYSTEM_ADVISOR_MODEL_BUILDER_GENERATOR_H
#define SYSTEM_ADVISOR_MODEL_BUILDER_GENERATOR_H

#include <string>
#include <map>

#include "variable_graph.h"
#include "config_extractor.h"

class builder_generator {
private:
    std::string config_name;
    std::string config_symbol;
    config_extractor* config_ext;
    digraph* graph;
    digraph* subgraph;
    std::map<std::string, std::map<std::string, vertex*>> modules;
    std::vector<std::string> modules_order;

    std::string filepath;

    std::unordered_map<std::string, ssc_module_t> ssc_module_objects;

    static std::unordered_map<std::string, std::vector<std::string>> m_config_to_modules;

    static std::unordered_map<std::string, std::unordered_map<std::string, callback_info>> m_config_to_callback_info;

    vertex * var_user_defined(std::string var_name);

    void select_ui_variables(std::string ui_name, std::map<std::string, vertex*>& var_map);

    void gather_variables();

    void export_variables_json();

    std::unordered_map<std::string, edge *> gather_functions();

    void create_SAM_headers(std::string cmod_name, std::string module, std::ofstream &fx_file);

    void create_api_header(std::string cmod_name);
public:

    builder_generator(config_extractor* ce);

    ~builder_generator(){
        delete subgraph;
        for (auto it = ssc_module_objects.begin(); it != ssc_module_objects.end(); ++it){
            ssc_module_free(it->second);
        }
    }

    void print_graph(std::string graph_path){
        graph->print_dot(graph_path);
    }

    void print_subgraphs(std::string graph_path){
        subgraph->print_dot(graph_path, ".sgv");
        digraph subgraph_with_downstream_ui(config_name);
        graph->subgraph_ssc_to_ui(subgraph_with_downstream_ui);
//        subgraph_with_downstream_ui.print_dot(graph_path, ".sugv");
    }


    bool eqn_in_subgraph(equation_info eq);

    void create_cmod_builder_cpp(std::string cmod_name,
                                     const std::unordered_map<std::string, edge *> &unique_edge_obj_names);

    void create_all(std::string fp);

    std::vector<std::string> get_user_defined_variables();

    std::vector<std::string> get_evaluated_variables();

};

#endif