//
// Created by Guittet, Darice on 2019-03-04.
//
#include <iostream>

#include "variable_graph.h"

void digraph::print_vertex(vertex *v) {
    for (size_t i = 0; i < v->edges_out.size(); i++){
        std::cout << "\t" << v->edges_out[i]->src->name << " -> " << v->edges_out[i]->dest->name << ";\n";
    }
}

void digraph::print_dot() {
    std::string str = name;

    std::string::iterator end_pos = std::remove(str.begin(), str.end(), ' ');
    str.erase(end_pos, str.end());

    std::cout << "digraph " << str << " {\n";
    for (auto it = vertices.begin(); it != vertices.end(); ++it){
        for (size_t i = 0; i < 2; i++){
            if (vertex* v = it->second.at(i)){
                print_vertex(v);
            }
        }
    }
    std::cout << "}";
}