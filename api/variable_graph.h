#ifndef SYSTEM_ADVISOR_MODEL_VARIABLE_GRAPH_H
#define SYSTEM_ADVISOR_MODEL_VARIABLE_GRAPH_H

#include <iostream>
#include <string>
#include <vector>
#include <unordered_map>

#include <lk/absyn.h>

#include "variables.h"

/**
 * Implements a very simple directed graph for keeping track of variable transformations
 */

enum {
    EQN,
    LOAD,
    CHNG
};

class vertex;

// directed
class edge{
public:
    int type;
    std::string obj_name; // for call_backs
    vertex* src;
    vertex* dest;
    std::string expression;
    lk::node_t* root;

    edge(vertex* v_src, vertex* v_dest, int t, std::string obj = "", std::string expr = "") {
        src = v_src;
        dest = v_dest;
        type = t;
        obj_name = obj;
        expression = expr;
    }
};

// each vertex is responsible for the memory of its edges_out
class vertex{
public:
    std::string name;
    std::string ui_form;
    std::vector<edge*> edges_out;
    std::vector<edge*> edges_in;
    bool is_ssc_var;    // is an input to primary compute module
    std::string cmod;

    vertex(std::string n, bool is_ssc){
        name = n;
        is_ssc_var = is_ssc;
    }

    edge* get_edge_out_to(vertex* dest){
        for (size_t i = 0; i < edges_out.size(); i++){
            if (edges_out[i]->dest == dest)
                return edges_out[i];
        }
        return nullptr;
    }

    edge* get_edge_in_from(vertex* src){
        for (size_t i = 0; i < edges_in.size(); i++){
            if (edges_in[i]->src == src)
                return edges_out[i];
        }
        return nullptr;
    }

    ~vertex(){
        for (size_t e = 0; e < edges_out.size(); e++){
            delete edges_out[e];
        }
    }
};

class digraph{
private:
    std::string name;
    // vertices can have the same name but at 0 position is not an ssc var, and at 1 it is
    // secondary compute modules act as a non-ssc var vertex
    std::unordered_map<std::string, std::vector<vertex*>> vertices;


public:
    digraph(std::string n){name = n;}

    ~digraph(){
        for (auto it = vertices.begin(); it != vertices.end(); ++it){
            if (it->second[0])
                delete it->second[0];
            if (it->second[1])
                delete it->second[1];
        }
    }

    std::unordered_map<std::string, std::vector<vertex*>>& get_vertices(){return vertices;}

    vertex* add_vertex(std::string n, bool is_ssc);

    vertex* find_vertex(std::string n, bool is_ssc);

    vertex* find_vertex(vertex* v);

    void delete_vertex(vertex* v);

    edge* find_edge(std::string src_name, bool src_is_ssc, std::string dest_name, bool dest_is_ssc, int type);

    edge* find_edge(edge* edge);

    bool add_edge(vertex* src, vertex* dest, int type, std::string obj, std::string expression);

    bool add_edge(std::string src, bool src_is_ssc, std::string dest, bool dest_is_ssc,
            int type, std::string obj, std::string expression);

    void delete_edge(edge* e);

    /// rename vertices map key and vertex itself
    void rename_vertex(std::string old, bool is_ssc, std::string n);

    /// vertices inserted as tbd:var will be rename to cmod:var, with duplication check
    void rename_cmod_vertices(std::string cmod_name);

    bool copy_vertex_descendants(vertex *v);

    void subgraph_ssc_only(digraph& g);

    void subgraph_ssc_to_ui(digraph &subgraph);

    void print_vertex(vertex *v, std::ofstream &ofs, std::unordered_map<std::string, std::string> *obj_keys = nullptr,
                          std::unordered_map<std::string, std::string> *eqn_keys = nullptr);

    void print_dot(std::string filepath, std::string ext = ".gv");
};

#endif //SYSTEM_ADVISOR_MODEL_VARIABLE_GRAPH_H
