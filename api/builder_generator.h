#ifndef SYSTEM_ADVISOR_MODEL_BUILDER_GENERATOR_H
#define SYSTEM_ADVISOR_MODEL_BUILDER_GENERATOR_H

#include <string>
#include <map>

#include "variable_graph.h"
#include "config_extractor.h"

class builder_generator {
private:
    std::string config_name;
    std::string symbol_name;
    config_extractor* config_ext;
    digraph* graph;
    digraph* subgraph;
    std::map<std::string, std::map<std::string, vertex*>> modules;
    std::vector<std::string> modules_order;

    std::string filepath;

    std::unordered_map<std::string, ssc_module_t> ssc_module_objects;

    /// create cmod objects to access variable information
    void load_cmods();

    bool ssc_var_user_defined(std::string var_name);

    void select_ui_variables(std::string ui_name, std::map<std::string, vertex*>& var_map);

    void gather_variables();

    void create_api_functions(std::string module);

    void create_api_data();
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

    void print_subgraph(std::string graph_path){
        subgraph->print_dot(graph_path);
    }

    void encode_edge(edge* e);


    void create_source_interfaces(std::vector<vertex *> &vertices);

    void get_expressions();

    void generate_interface(std::string filepath);

    std::vector<std::string> get_user_defined_variables();

    std::vector<std::string> get_evaluated_variables();

};

#endif