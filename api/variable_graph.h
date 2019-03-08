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
    std::vector<edge*> edges_out;
    std::vector<edge*> edges_in;
    bool is_ssc_var;    // is an input to primary compute module

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

    void add_vertex(std::string n, bool is_ssc){
        if (find_vertex(n, is_ssc))
            return;
        vertex* v = new vertex(n, is_ssc);
        auto it = vertices.find(n);
        if (it == vertices.end()){
            std::vector<vertex*> vs(2, nullptr);
            vertices.insert({n, vs});
        }
        vertices.find(n)->second.at((size_t)is_ssc) = v;
    }

    vertex* find_vertex(std::string n, bool is_ssc){
        auto it = vertices.find(n);
        if (it == vertices.end())
            return nullptr;
        std::vector<vertex*> vec = it->second;
        if (vertex* v = vec.at((size_t)is_ssc))
            return v;
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
        auto it = vertices.find(v->name)->second;
        if (it.at(0) == v){
            delete it.at(0);
        }
        else
            delete it.at(1);
    }

    edge* find_edge(std::string src_name, bool src_is_ssc, std::string dest_name, bool dest_is_ssc, int type){
        vertex* src = find_vertex(src_name, src_is_ssc);
        vertex* dest = find_vertex(dest_name, dest_is_ssc);
        if (!src || !dest)
            return nullptr;

        if (edge* e = src->get_edge_out_to(dest)){
            return e;
        }
        else
            return nullptr;
    }

    bool add_edge(std::string src, bool src_is_ssc, std::string dest, bool dest_is_ssc,
            int type, std::string obj, std::string expression) {
        assert(type >= 0);
        if (find_edge(src, src_is_ssc, dest, dest_is_ssc, type))
            return true;
        vertex* v1 = find_vertex(src, src_is_ssc);
        vertex* v2 = find_vertex(dest, dest_is_ssc);
        if (!v1 || !v2){
            std::cout << "digraph::add_edge error: could not find vertices ";
            std::cout << (!v1? src + " " + std::to_string(src_is_ssc)
                : dest + " " + std::to_string(dest_is_ssc) ) << "\n";
            return false;
        }
        edge* e = new edge(v1, v2, type, obj, expression);

        v1->edges_out.push_back(e);
        v2->edges_in.push_back(e);
        return true;
    }


    void delete_edge(edge* e){
        assert(e);
        vertex* src = e->src;
        for (size_t i = 0; i < src->edges_out.size(); i++){
            if (src->edges_out[i] == e){
                src->edges_out.erase(src->edges_out.begin() + i);
                break;
            }
        }
        vertex* dest = e->dest;
        for (size_t i = 0; i < dest->edges_in.size(); i++){
            if (dest->edges_in[i] == e){
                dest->edges_in.erase(dest->edges_in.begin() + i);
                break;
            }
        }
        delete e;
    }

    // rename vertices map key and vertex itself
    void rename_vertex(std::string old, bool is_ssc, std::string n){
        auto old_it = vertices.find(old);
        if (old_it == vertices.end()){
            std::cout << "digraph::rename_vertex error: could not find \'" << old << "\' vertex\n";
            assert(false);
        }

        // new entry by new name
        vertices.insert({n, std::vector<vertex*>(2, nullptr)});
        std::vector<vertex*>& new_vec = vertices.find(n)->second;

        // move ownership of pointer & rename vertex
        new_vec.at((size_t)is_ssc) = old_it->second.at((size_t)is_ssc);
        assert(new_vec.at((size_t)is_ssc));
        new_vec.at((size_t)is_ssc)->name = n;
        old_it->second.at((size_t)is_ssc) = nullptr;

        // if no vertices by the old name, delete hash entry
        if (!old_it->second.at(0) && !old_it->second.at(1))
            vertices.erase(old_it);
    }

    // vertices inserted as tbd:var will be rename to cmod:var, with duplication check
    void rename_cmod_vertices(std::string cmod_name){
        size_t not_ssc_var = 0; // secondary cmod variables are not primary
        for (auto it = vertices.begin(); it != vertices.end(); ++it){
            if (it->first.find("tbd:") != std::string::npos){

                std::string new_name = it->first;
                size_t pos = new_name.find("tbd:");
                new_name.replace(pos, 4, (cmod_name + ":"));

                rename_vertex(it->first, not_ssc_var, new_name);

            }
        }
    }

    void print_vertex(vertex *v);

    void print_dot();
};

#endif //SYSTEM_ADVISOR_MODEL_VARIABLE_GRAPH_H
