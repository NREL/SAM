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

#ifndef SYSTEM_ADVISOR_MODEL_VARIABLE_GRAPH_H
#define SYSTEM_ADVISOR_MODEL_VARIABLE_GRAPH_H

#include <iostream>
#include <string>
#include <cstring>
#include <utility>
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
    lk::node_t* root{};
    std::string ui_form;

    edge(){}

    edge(vertex *v_src, vertex *v_dest, int t, std::string obj = "", std::string expr = "", std::string ui_form_name = "",
         lk::node_t *node_root = nullptr) {
        src = v_src;
        dest = v_dest;
        type = t;
        obj_name = std::move(obj);
        expression = std::move(expr);
        ui_form = std::move(ui_form_name);
        root = node_root;
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
        name = std::move(n);
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
        for (auto & i : edges_out){
            if (i->dest == dest)
                return i;
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
        for (auto & e : edges_out){
            delete e;
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
        for (auto & vert : vertices){
            if (vert.second[0])
                delete vert.second[0];
            if (vert.second[1])
                delete vert.second[1];
        }
    }

    std::unordered_map<std::string, std::vector<vertex*>>& get_vertices(){return vertices;}


    vertex* add_vertex(std::string n, bool is_ssc, std::string ui_source = "");

    vertex* find_vertex(std::string n, bool is_ssc);

    vertex* find_vertex(vertex* v);

    void delete_vertex(vertex* v);

    edge* find_edge(std::string src_name, bool src_is_ssc, std::string dest_name, bool dest_is_ssc, int type);

    edge* find_edge(edge* edge);

    static edge *add_edge(vertex *src, vertex *dest, const int &type, const std::string &obj, const std::string &expression,
                       const std::string& ui_form, lk::node_t *root);

    edge *add_edge(const std::string& src, bool src_is_ssc, const std::string& dest, bool dest_is_ssc, int type,
                   const std::string& obj, const std::string& expression, const std::string& ui_form, lk::node_t *root);

    static void delete_edge(edge* e);

    /// rename vertices map key and vertex itself
    void rename_vertex(const std::string& old, bool is_ssc, std::string n);

    /// vertices inserted as tbd:var will be rename to cmod:var, with duplication check
    void rename_cmod_vertices(const std::string& cmod_name);

    static std::set<std::string> downstream_vertices(vertex *vert, const std::string& cmod = "");

    static std::set<std::string> upstream_vertices(vertex *vert, const std::string& cmod = "");

    bool copy_vertex_descendants(vertex *v);

    void subgraph_ssc_only(digraph& g);

    void subgraph_ssc_to_ui(digraph &subgraph);

    void get_unique_edge_expressions(std::unordered_map<std::string, edge*>& unique_edge_obj_names);

    static void print_vertex(vertex *v, std::ofstream &ofs, std::unordered_map<std::string, std::string> *obj_keys = nullptr,
                          std::unordered_map<std::string, std::string> *eqn_keys = nullptr);

    void print_dot(const std::string& filepath, const std::string& ext = ".gv");
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
    else if (v->edges_out.empty())
        return SINK;
    else if (v->edges_in.empty())
        return SOURCE;
    else
        return CONNECTED;
}

#endif //SYSTEM_ADVISOR_MODEL_VARIABLE_GRAPH_H
