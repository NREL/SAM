#ifndef SYSTEM_ADVISOR_MODEL_VARIABLE_GRAPH_H
#define SYSTEM_ADVISOR_MODEL_VARIABLE_GRAPH_H

#include <iostream>
#include <string>
#include <vector>
#include <unordered_map>

#include <lk/absyn.h>

/**
 * Implements a very simple directed graph for keeping track of variable transformations
 */

enum {
    EQN,
    LOAD,
    CHNG
};

class vertex;

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

class vertex{
public:
    std::string name;
    std::vector<edge*> edges_out;
    std::vector<edge*> edges_in;
    bool is_ssc_var;    // is an input to primary compute module

    vertex(std::string n, bool is_ssc){
        name = n;
        is_ssc_var = is_ssc;
    }
};

class digraph{
private:
    std::string name;
    std::unordered_map<std::string, vertex*> vertices;
    std::vector<edge*> edges;

public:
    digraph(std::string n){name = n;}
    ~digraph(){
        for (auto it = vertices.begin(); it != vertices.end(); ++it){
            delete it->second;
        }
        for (size_t i = 0; i < edges.size(); i++){
            delete edges[i];
        }
    }

    void add_vertex(std::string n, bool is_ssc){
        if (find_vertex(n, is_ssc))
            return;
        vertex* v = new vertex(n, is_ssc);
        vertices.insert({n, v});
    }

    vertex* find_vertex(std::string n, bool is_ssc){
        if (vertices.find(n) != vertices.end()){
            vertex* v = vertices.find(n)->second;
            if (v->is_ssc_var == is_ssc)
                return v;
            else
                return nullptr;
        }
        else
            return nullptr;
    }

    void delete_vertex(vertex* v){
        assert(v);
        for (size_t i = 0; i < v->edges_in.size(); i++){
            delete_edge(v->edges_in[i]);
        }
        for (size_t i = 0; i < v->edges_out.size(); i++){
            delete_edge(v->edges_out[i]);
        }
        vertices.erase(v->name);
        delete v;
    }

    edge* find_edge(std::string v1, std::string v2, int type){
        for (size_t i = 0; i < edges.size(); i++){
            edge* e = edges[i];
            bool match = (std::strcmp(e->src->name.c_str(), v1.c_str()) == 0)
                    && (std::strcmp(e->dest->name.c_str(), v2.c_str()) == 0)
                    && (e->type == type);
            if (match)
                return e;
        }
        return nullptr;
    }

    bool add_edge(std::string src, bool src_is_ssc, std::string dest, bool dest_is_ssc,
            int type, std::string obj, std::string expression) {
        assert(type >= 0);
        if (find_edge(src, dest, type))
            return true;
        if (src.find("tbd") != -1 || dest.find("tbd") != -1)
            std::cout << "add_edge: " << dest << " to " << src << "\n";
        vertex* v1 = find_vertex(src, src_is_ssc);
        vertex* v2 = find_vertex(dest, dest_is_ssc);
        if (!v1 || !v2){
            std::cout << "digraph::add_edge error: could not find vertices " + (!v1? src : dest) << "\n";
            return false;
        }
        edge* e = new edge(v1, v2, type, obj, expression);
        edges.push_back(e);
        v1->edges_out.push_back(edges.back());
        v2->edges_in.push_back(edges.back());
        return true;
    }

    void update_edges(vertex* v){
        for (size_t e = 0; e < v->edges_in.size(); e++){
            edge* edge_in = v->edges_in[e];
            edge_in->dest = v;
        }
        for (size_t e = 0; e < v->edges_out.size(); e++){
            edge* edge_out = v->edges_out[e];
            edge_out->src = v;
        }
    }

    void delete_edge(edge* e){
        assert(e);
        auto src_edges_out = e->src->edges_out;
        for (size_t i = 0; i < src_edges_out.size(); i++){
            if (src_edges_out[i] == e){
                src_edges_out.erase(src_edges_out.begin() + i);
                break;
            }
        }
        auto dest_edges_in = e->dest->edges_in;
        for (size_t i = 0; i < dest_edges_in.size(); i++){
            if (dest_edges_in[i] == e){
                dest_edges_in.erase(dest_edges_in.begin() + i);
                break;
            }
        }
        for (size_t i = 0; i < edges.size(); i++){
            if (edges[i] == e)
                edges.erase(edges.begin() + i);
            delete e;
            break;
        }
    }

    void rename_vertex(vertex* v, std::string n){
        assert(v);
        v->name = n;
        update_edges(v);
    }

    // vertices inserted as tbd:var will be rename to cmod:var, with duplication check
    void rename_cmod_vertices(std::string cmod_name){
        for (auto it = vertices.begin(); it != vertices.end(); ++it){
            if (it->second->name.find("tbd:") != std::string::npos){
                vertex* v = it->second;
                std::string new_name = v->name;

                size_t pos = new_name.find("tbd:");
                new_name.replace(pos, 4, (cmod_name + ":"));

                if (find_vertex(new_name, false)){
                    delete_vertex(v);
                }
                else{
                    v->name = new_name;
                    update_edges(v);
                }
            }
        }
    }

    void print_dot();
};

#endif //SYSTEM_ADVISOR_MODEL_VARIABLE_GRAPH_H
