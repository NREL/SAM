#ifndef SYSTEM_ADVISOR_MODEL_VARIABLE_GRAPH_H
#define SYSTEM_ADVISOR_MODEL_VARIABLE_GRAPH_H

#include <iostream>
#include <string>
#include <vector>
#include <unordered_map>

/**
 * Implements a very simple directed graph for keeping track of variable transformations
 */

enum {
    EQN,
    LOAD,
    CHNG
};

class edge{
public:
    int type;
    std::string obj_name; // for call_backs
    std::string src, dest;

    edge(std::string v_src, std::string v_dest, int t, std::string obj = "") {
        src = v_src;
        dest = v_dest;
        type = t;
        obj_name = obj;
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
        if (find_vertex(n))
            return;
        vertex* v = new vertex(n, is_ssc);
        vertices.insert({n, v});
    }

    vertex* find_vertex(std::string n){
        if (vertices.find(n) != vertices.end())
            return vertices.find(n)->second;
        else
            return nullptr;
    }

    edge* find_edge(std::string v1, std::string v2, int type){
        for (size_t i = 0; i < edges.size(); i++){
            edge* e = edges[i];
            bool match = (std::strcmp(e->src.c_str(), v1.c_str()) == 0)
                    && (std::strcmp(e->dest.c_str(), v2.c_str()) == 0)
                    && (e->type == type);
            if (match)
                return e;
        }
        return nullptr;
    }

    bool add_edge(std::string src, std::string dest, int type, std::string obj){
        assert(type >= 0);
        if (find_edge(src, dest, type))
            return true;
        if (src.find("tbd") != -1 || dest.find("tbd") != -1)
            std::cout << 0;
        vertex* v1 = find_vertex(src);
        vertex* v2 = find_vertex(dest);
        if (!v1 || !v2){
            std::cout << "digraph::add_edge error: could not find vertices " + (!v1? src : dest) << "\n";
            return false;
        }
        edge* e = new edge(src, dest, type, obj);
        edges.push_back(e);
        v1->edges_out.push_back(edges.back());
        v2->edges_in.push_back(edges.back());
        return true;
    }

    void update_edges(vertex* v){
        for (size_t e = 0; e < v->edges_in.size(); e++){
            edge* edge_in = v->edges_in[e];
            edge_in->dest = v->name;
        }
        for (size_t e = 0; e < v->edges_out.size(); e++){
            edge* edge_out = v->edges_out[e];
            edge_out->src = v->name;
        }
    }

    void rename_vertex(vertex* v, std::string n){
        assert(v);
        v->name = n;
        update_edges(v);
    }

    void rename_cmod_vertices(std::string cmod_name){
        for (auto it = vertices.begin(); it != vertices.end(); ++it){
            if (it->second->name.find("tbd:") != std::string::npos){
                vertex* v = it->second;
                size_t pos = v->name.find("tbd:");
                v->name.replace(pos, 4, (cmod_name + ":"));
                // update all edges
                update_edges(v);
            }
        }
    }

    void print_dot();
};

#endif //SYSTEM_ADVISOR_MODEL_VARIABLE_GRAPH_H
