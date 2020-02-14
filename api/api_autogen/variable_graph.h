#ifndef SYSTEM_ADVISOR_MODEL_VARIABLE_GRAPH_H
#define SYSTEM_ADVISOR_MODEL_VARIABLE_GRAPH_H

#include <iostream>
#include <string>
#include <cstring>
#include <vector>
#include <unordered_map>

#include <lk/absyn.h>
#include <set>

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
    std::string obj_name; // for call_backs & eqns, MIMO's are named as "*first_output*_MIMO"
    vertex* src;
    vertex* dest;
    std::string expression;
    lk::node_t* root;
    std::string ui_form;

    edge(){}

    edge(vertex* v_src, vertex* v_dest, int t, std::string obj = "", std::string expr = "") {
        src = v_src;
        dest = v_dest;
        type = t;
        obj_name = obj;
        expression = expr;
    }

    edge( const edge &obj){
        type = obj.type;
        obj_name = obj.obj_name;
        src = obj.src;
        dest = obj.dest;
        expression = obj.expression;
        root = obj.root;
        ui_form = obj.ui_form;
    }

    edge& operator=( const edge &obj){
        type = obj.type;
        obj_name = obj.obj_name;
        src = obj.src;
        dest = obj.dest;
        expression = obj.expression;
        root = obj.root;
        ui_form = obj.ui_form;
        return *this;
    }
};

// each vertex is responsible for the memory of its edges_out
class vertex{
public:
    std::string name;
    std::string ui_form;
    std::vector<edge*> edges_out;
    std::vector<edge*> edges_in;
    bool is_ssc_var;    // name of primary compute module
    std::string cmod;

    vertex(){}

    vertex(std::string n, bool is_ssc){
        name = n;
        is_ssc_var = is_ssc;
    }

    vertex( const vertex &obj){
        name = obj.name;
        ui_form = obj.ui_form;
        is_ssc_var = obj.is_ssc_var;
        cmod = obj.cmod;
        edges_out = obj.edges_out;
        edges_in = obj.edges_in;
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
    digraph() = default;

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


    vertex* add_vertex(std::string n, bool is_ssc, std::string ui_source = "");

    vertex* find_vertex(std::string n, bool is_ssc);

    vertex* find_vertex(vertex* v);

    void delete_vertex(vertex* v);

    edge* find_edge(std::string src_name, bool src_is_ssc, std::string dest_name, bool dest_is_ssc, int type);

    edge* find_edge(edge* edge);

    edge *add_edge(vertex *src, vertex *dest, const int &type, const std::string &obj, const std::string &expression,
                       const std::string ui_form, lk::node_t *root);

    edge *add_edge(std::string src, bool src_is_ssc, std::string dest, bool dest_is_ssc, int type, std::string obj,
                       std::string expression, std::string ui_form, lk::node_t *root);

    void delete_edge(edge* e);

    /// rename vertices map key and vertex itself
    void rename_vertex(std::string old, bool is_ssc, std::string n);

    /// vertices inserted as tbd:var will be rename to cmod:var, with duplication check
    void rename_cmod_vertices(std::string cmod_name);

    static std::set<std::string> downstream_vertices(vertex *vert, std::string cmod = "");

    static std::set<std::string> upstream_vertices(vertex *vert, std::string cmod = "");

    bool copy_vertex_descendants(vertex *v);

    void subgraph_ssc_only(digraph& g);

    void subgraph_ssc_to_ui(digraph &subgraph);

    void get_unique_edge_expressions(std::unordered_map<std::string, edge*>& unique_edge_obj_names);

    void print_vertex(vertex *v, std::ofstream &ofs, std::unordered_map<std::string, std::string> *obj_keys = nullptr,
                          std::unordered_map<std::string, std::string> *eqn_keys = nullptr);

    void print_dot(std::string filepath, std::string ext = ".gv");
};

enum{
    SOURCE,
    SINK,
    ISOLATED,
    CONNECTED
};

static int get_vertex_type(vertex *v){
    if (v->edges_out.size() + v->edges_in.size() == 0)
        return ISOLATED;
    else if (v->edges_out.size() == 0)
        return SINK;
    else if (v->edges_in.size() == 0)
        return SOURCE;
    else
        return CONNECTED;
}

#endif //SYSTEM_ADVISOR_MODEL_VARIABLE_GRAPH_H
