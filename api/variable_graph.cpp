//
// Created by Guittet, Darice on 2019-03-04.
//
#include <iostream>

#include "variable_graph.h"

void digraph::print_dot() {
    std::cout << "digraph " << name << " {\n";
    for (auto it = vertices.begin(); it != vertices.end(); ++it){
        auto e = it->second->edges_out;
        for (size_t i = 0; i < e.size(); i++){
            std::cout << "\t" << e[i]->src << " -> " << e[i]->dest << ";\n";
        }
    }
    std::cout << "}";
}