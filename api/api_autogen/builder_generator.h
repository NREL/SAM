#ifndef SYSTEM_ADVISOR_MODEL_BUILDER_GENERATOR_H
#define SYSTEM_ADVISOR_MODEL_BUILDER_GENERATOR_H

#include <string>
#include <map>

#include <ssc/sscapi.h>
#include <ssc/ssc_equations.h>

#include "variable_graph.h"
#include "config_extractor.h"

struct var_def {
    std::string doc;
    std::string reqif;
    std::string constraints;
    std::string returned;
    std::string meta;
    std::string cmod;
    std::string group;
    std::string name;
    std::string type;
    int type_n;
    std::vector<var_def> table_entries;
    bool is_ssc;
    std::set<std::string> downstream;   // variables that are affected by this one
    std::set<std::string> upstream;     // variables that affect this one
};

class builder_C_API;

class builder_PySAM;

class builder_generator {
    friend builder_C_API;
    friend builder_PySAM;
private:
    config_extractor *config_ext;
    digraph *graph;
    digraph *subgraph;
    std::string filepath;

    // group: {var name: vertex}
    std::map<std::string, std::map<std::string, vertex *>> modules;
    std::vector<std::string> modules_order;

    // group: {var name: var definition}
    std::map<std::string, std::map<std::string, var_def>> m_vardefs;
    std::vector<std::string> vardefs_order;

    std::unordered_map<std::string, ssc_module_t> ssc_module_objects;

    // group: {eqn name: eqn entry}
    std::map<std::string, std::map<std::string, ssc_equation_entry>> m_eqn_entries;

    static std::unordered_map<std::string, std::vector<std::string>> m_config_to_modules;

    static std::unordered_map<std::string, std::unordered_map<std::string, callback_info>> m_config_to_callback_info;

    vertex *var_user_defined(std::string var_name);

    void select_ui_variables(std::string ui_name, std::map<std::string, vertex *> &var_map);

    void gather_variables_ssc(const std::string &cmod);

    void gather_equations(const std::string &cmod);

    void gather_variables();

    void export_variables_json(const std::string &cmod, const std::string &defaults_path);

    std::unordered_map<std::string, edge *> gather_functions();


    void create_api_header(std::string cmod_name);

public:
    std::string config_name;
    std::string config_symbol;

    builder_generator() { subgraph = nullptr; };

    explicit builder_generator(config_extractor *ce);

    ~builder_generator() {
        if (subgraph) delete subgraph;
        for (auto it = ssc_module_objects.begin(); it != ssc_module_objects.end(); ++it) {
            ssc_module_free(it->second);
        }
    }

    void print_graph(std::string graph_path) {
        graph->print_dot(graph_path);
    }

    void print_subgraphs(std::string graph_path) {
        subgraph->print_dot(graph_path, ".sgv");
        digraph subgraph_with_downstream_ui(config_name);
        graph->subgraph_ssc_to_ui(subgraph_with_downstream_ui);
//        subgraph_with_downstream_ui.print_dot(graph_path, ".sugv");
    }


    bool eqn_in_subgraph(equation_info eq);

    void create_cmod_builder_cpp(std::string cmod_name,
                                 const std::unordered_map<std::string, edge *> &unique_edge_obj_names);

    void create_all(std::string cmod, const std::string &defaults_path, const std::string &api_path,
                    const std::string &pysam_path, bool print_json = true);

    std::vector<std::string> get_user_defined_variables();

    std::vector<std::string> get_evaluated_variables();

};

#endif
