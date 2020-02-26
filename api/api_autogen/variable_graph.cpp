//
// Created by Guittet, Darice on 2019-03-04.
//
#include <iostream>
#include <fstream>

#include "variable_graph.h"
#include "data_structures.h"

vertex *digraph::add_vertex(std::string n, bool is_ssc, std::string ui_source) {
    if (vertex *v = find_vertex(n, is_ssc))
        return v;
    if (n.find('.') != std::string::npos)
        is_ssc = false;
    vertex *v = new vertex(n, is_ssc);
    auto it = vertices.find(n);
    if (it == vertices.end()) {
        std::vector<vertex *> vs(2, nullptr);
        vertices.insert({n, vs});
    }
    vertices.find(n)->second.at((size_t) is_ssc) = v;
    v->ui_form = ui_source;
    return v;
}

vertex *digraph::find_vertex(std::string n, bool is_ssc) {
    auto it = vertices.find(n);
    if (it == vertices.end())
        return nullptr;
    std::vector<vertex *> vec = it->second;
    if (vertex *v = vec.at((size_t) is_ssc))
        return v;
    return nullptr;
}

vertex *digraph::find_vertex(vertex *v) {
    return find_vertex(v->name, v->is_ssc_var);
}

void digraph::delete_vertex(vertex *v) {
    assert(v);
    for (size_t i = 0; i < v->edges_in.size(); i++) {
        delete_edge(v->edges_in[i]);
    }
    for (size_t i = 0; i < v->edges_out.size(); i++) {
        delete_edge(v->edges_out[i]);
    }
    auto it = vertices.find(v->name)->second;
    if (it.at(0) == v) {
        delete it.at(0);
    } else
        delete it.at(1);
}

edge *digraph::find_edge(std::string src_name, bool src_is_ssc, std::string dest_name, bool dest_is_ssc, int type) {
    vertex *src = find_vertex(src_name, src_is_ssc);
    vertex *dest = find_vertex(dest_name, dest_is_ssc);
    if (!src || !dest)
        return nullptr;

    if (edge *e = src->get_edge_out_to(dest)) {
        return e;
    } else
        return nullptr;
}

edge *digraph::find_edge(edge *edge) {
    return find_edge(edge->src->name, edge->src->is_ssc_var, edge->dest->name, edge->dest->is_ssc_var, edge->type);
}

edge *
digraph::add_edge(vertex *src, vertex *dest, const int &type, const std::string &obj, const std::string &expression,
                  const std::string ui_form = "", lk::node_t *root = 0) {
    if (!src || !dest) {
        std::cout << "/* digraph::add_edge error: vertices null */ \n";
        return nullptr;
    }
    edge *e = new edge(src, dest, type, obj, expression);
    e->ui_form = ui_form;
    e->root = root;
    src->edges_out.push_back(e);
    dest->edges_in.push_back(e);
    return e;
}


edge *digraph::add_edge(std::string src, bool src_is_ssc, std::string dest, bool dest_is_ssc, int type, std::string obj,
                        std::string expression, std::string ui_form, lk::node_t *root) {
    assert(type >= 0);

    if (edge *e = find_edge(src, src_is_ssc, dest, dest_is_ssc, type))
        return e;
    if (edge *e = find_edge(dest, dest_is_ssc, src, src_is_ssc, type))
        return e;
    if (std::strcmp(dest.c_str(), src.c_str()) == 0 && src_is_ssc == dest_is_ssc) {
        return nullptr;
    }

    vertex *v1 = find_vertex(src, src_is_ssc);
    if (!v1)
        v1 = add_vertex(src, src_is_ssc, ui_form);
    vertex *v2 = find_vertex(dest, dest_is_ssc);
    if (!v2)
        v2 = add_vertex(dest, dest_is_ssc, ui_form);
//    if (!v1 || !v2){
//        std::cout << "/* digraph::add_edge error: could not find vertices ";
//        std::cout << (!v1? src + " " + std::to_string(src_is_ssc)
//                         : dest + " " + std::to_string(dest_is_ssc) ) << " */\n";
//        return nullptr;
//    }

    return add_edge(v1, v2, type, obj, expression, ui_form, root);
}


void digraph::delete_edge(edge *e) {
    assert(e);
    vertex *src = e->src;
    for (size_t i = 0; i < src->edges_out.size(); i++) {
        if (src->edges_out[i] == e) {
            src->edges_out.erase(src->edges_out.begin() + i);
            break;
        }
    }
    vertex *dest = e->dest;
    for (size_t i = 0; i < dest->edges_in.size(); i++) {
        if (dest->edges_in[i] == e) {
            dest->edges_in.erase(dest->edges_in.begin() + i);
            break;
        }
    }
    delete e;
}

// rename vertices map key and vertex itself
void digraph::rename_vertex(std::string old, bool is_ssc, std::string n) {
    auto old_it = vertices.find(old);
    if (old_it == vertices.end()) {
        std::cout << "digraph::rename_vertex error: could not find \'" << old << "\' vertex\n";
        assert(false);
    }

    // new entry by new name
    vertices.insert({n, std::vector<vertex *>(2, nullptr)});
    std::vector<vertex *> &new_vec = vertices.find(n)->second;

    // move ownership of pointer & rename vertex and its edges
    new_vec.at((size_t) is_ssc) = old_it->second.at((size_t) is_ssc);
    vertex *v = new_vec.at((size_t) is_ssc);
    assert(v);
    v->name = n;
    v->cmod = n;
    for (size_t i = 0; i < v->edges_in.size(); i++) {
        edge *e = v->edges_in[i];
        if (e->obj_name.find("tbd") != std::string::npos) {
            size_t pos = e->obj_name.find("tbd");
            e->obj_name.replace(pos, 3, n);
        }
    }
    for (size_t i = 0; i < v->edges_out.size(); i++) {
        edge *e = v->edges_out[i];
        if (e->obj_name.find("tbd") != std::string::npos) {
            size_t pos = e->obj_name.find("tbd");
            e->obj_name.replace(pos, 3, n);
        }
    }
    old_it->second.at((size_t) is_ssc) = nullptr;

    // if no vertices by the old name, delete hash entry
    if (!old_it->second.at(0) && !old_it->second.at(1))
        vertices.erase(old_it);
}

// vertices inserted as tbd:var will be rename to cmod:var, with duplication check
void digraph::rename_cmod_vertices(std::string cmod_name) {
    bool not_ssc_var = false; // secondary cmod variables are not primary
    for (auto it = vertices.begin(); it != vertices.end(); ++it) {
        if (it->first.find("tbd:") != std::string::npos) {

            std::string new_name = it->first;
            size_t pos = new_name.find("tbd:");
            new_name.replace(pos, 4, (cmod_name + ":"));

            rename_vertex(it->first, not_ssc_var, new_name);
        }
    }
}

void digraph::get_unique_edge_expressions(std::unordered_map<std::string, edge *> &unique_edge_obj_names) {
    for (auto it = vertices.begin(); it != vertices.end(); ++it) {
        for (size_t is_ssc = 0; is_ssc < 2; is_ssc++) {
            vertex *v = it->second.at(is_ssc);
            if (!v)
                continue;
            for (size_t i = 0; i < v->edges_out.size(); i++) {
                edge *e = v->edges_out[i];
                if (unique_edge_obj_names.find(e->obj_name) == unique_edge_obj_names.end()) {
                    unique_edge_obj_names.insert({e->obj_name, e});
                }
            }
        }
    }
};

std::set<std::string> digraph::downstream_vertices(vertex *vert, std::string cmod) {
    std::set<std::string> names;
    for (auto e : vert->edges_out) {
        if ((cmod.length() > 0 && e->dest->cmod == cmod) | cmod.empty())
            names.insert(e->dest->name);
        std::set<std::string> downstream = downstream_vertices(e->dest, cmod);
        for (const auto &d : downstream)
            names.insert(d);
    }
    return names;
}

std::set<std::string> digraph::upstream_vertices(vertex *vert, std::string cmod) {
    std::set<std::string> names;
    for (auto e : vert->edges_in) {
        if ((cmod.length() > 0 && e->src->cmod == cmod) | cmod.empty())
            names.insert(e->src->name);
        std::set<std::string> downstream = upstream_vertices(e->src, cmod);
        for (const auto &d : downstream)
            names.insert(d);
    }
    return names;
}


bool digraph::copy_vertex_descendants(vertex *v) {
    // if vertex has already been added, so has its descendants
    if (find_vertex(v))
        return true;
    bool add = false;

    if (v->edges_out.size() == 0) {
        // if it's a terminal node and not an ssc var, don't copy
        if (!v->is_ssc_var)
            return false;
            // if it is a ssc var, add the vertex
        else {
            add_vertex(v->name, v->is_ssc_var, v->ui_form);
            return true;
        }
    }

    // if vertex is ssc, add it now in case none of its descendants are ssc
    if (v->is_ssc_var) {
        add_vertex(v->name, v->is_ssc_var, v->ui_form);
        add = true;
    }

    for (size_t i = 0; i < v->edges_out.size(); i++) {
        vertex *dest = v->edges_out[i]->dest;

        // if any of the descendants of this edge is ssc, copy
        if (copy_vertex_descendants(dest)) {
            add_vertex(v->name, v->is_ssc_var, v->ui_form);
            edge *e = v->edges_out[i];
            edge *e_new = add_edge(v->name, v->is_ssc_var, dest->name, dest->is_ssc_var, e->type, e->obj_name,
                                   e->expression, e->ui_form, e->root);
            add = true;
        }
    }

    return add;
}

void digraph::subgraph_ssc_only(digraph &new_graph) {
    for (auto it = vertices.begin(); it != vertices.end(); ++it) {
        if (vertex *src = it->second.at(1)) {
            if (src->edges_out.size() > 0) {
                new_graph.copy_vertex_descendants(src);
            }
        }
        if (vertex *src = it->second.at(0)) {
            if (SAM_cmod_to_outputs.find(src->name) != SAM_cmod_to_outputs.end()) {

                new_graph.copy_vertex_descendants(src);
            }
        }
    }
}

// copy into the subgraph all the upstream to source and downstream to sink variables
void digraph::subgraph_ssc_to_ui(digraph &subgraph) {
//    subgraph_ssc_only(subgraph);
    auto vertices_new = subgraph.get_vertices();
    for (auto it = vertices_new.begin(); it != vertices_new.end(); ++it) {
        for (size_t i = 0; i < 2; i++) {
            vertex *new_v = it->second.at(i);
            if (!new_v)
                continue;
            // get original vertex
            vertex *og_v = find_vertex(new_v->name, new_v->is_ssc_var);
            if (!og_v) {
                std::cout << "subgraph_ssc_to_ui warning::" << new_v->name << " in " << new_v->ui_form;
                std::cout << " not found in original graph for " << name << "\n";
                continue;
            }
//            for (size_t e = 0; e < og_v->edges_out.size(); e++){
//                edge* e_out = og_v->edges_out[e];
//
//                if (!subgraph.find_edge(e_out)){
//                    vertex* v = subgraph.add_vertex(e_out->dest->name, false, e_out->ui_form);
//                    subgraph.add_edge(new_v, v, e_out->type, e_out->obj_name, e_out->expression, e_out->ui_form, e_out->root);
//                }
//            }

            if (new_v->is_ssc_var) {
                for (size_t e = 0; e < og_v->edges_in.size(); e++) {
                    edge *e_in = og_v->edges_in[e];

                    if (!subgraph.find_edge(e_in)) {
                        vertex *v = subgraph.add_vertex(e_in->src->name, e_in->src->is_ssc_var, e_in->ui_form);
                        bool added = subgraph.add_edge(v, new_v, e_in->type, e_in->obj_name, e_in->expression,
                                                       e_in->ui_form, e_in->root);
                        assert(added);
                    }
                }
            }
        }
    }
}

std::string format_vertex_name(vertex *v) {
    // make sure first character is not a number
    std::string s;
    size_t sz = 0;

    try {
        (int) std::stod(v->name, &sz);
    }
    catch (std::invalid_argument) {}

    // if the name is just a numeric value, don't print it
    if (sz == v->name.length()) {
        return "";
    }

    size_t pos = v->name.find("[");
    if (pos != std::string::npos)
        s += v->name.substr(sz, pos);
    else
        s += v->name.substr(sz);

    s = "\"" + s + "\"";

    return s;
}

void digraph::print_vertex(vertex *v, std::ofstream &ofs, std::unordered_map<std::string, std::string> *obj_keys,
                           std::unordered_map<std::string, std::string> *eqn_keys) {

    const char *colors[] = {"black", "brown4", "darkorange3", "lightslateblue", "mediumorchid", "firebrick", "indigo",
                            "burlywood4", "azure4", "darkorchid4", "aquamarine3", "olivedrab", "palevioletred",
                            "darkgoldenrod2",
                            "gold4", "crimson", "chartreuse4", "sienna4", "skyblue4", "orange3", "seashell4", "sienna",
                            "sienna1",
                            "sienna2", "sienna3", "sienna4", "skyblue", "skyblue1", "skyblue2", "skyblue3", "skyblue4",
                            "slateblue"};
    size_t cnt = eqn_keys->size() + obj_keys->size();

    for (size_t i = 0; i < v->edges_out.size(); i++) {
        std::string src_str = format_vertex_name(v->edges_out[i]->src);
        std::string dest_str = format_vertex_name(v->edges_out[i]->dest);
        if (src_str.length() > 0 && dest_str.length() > 0) {
            ofs << "\t" << src_str << " -> " << dest_str;
            edge *e = v->edges_out[i];
            std::string edge_label = std::to_string(cnt);
            if (e->type == 0) {
                if (eqn_keys->find(e->obj_name) == eqn_keys->end()) {
                    eqn_keys->insert({e->obj_name, edge_label});
                    ofs << " [label=" << edge_label << ", style=dashed, color = ";
                    ofs << colors[(cnt % 31)] << "]" << ";\n";
                    cnt += 1;
                } else {
                    ofs << " [label=" << eqn_keys->find(e->obj_name)->second;
                    int ind = std::stoi(eqn_keys->find(e->obj_name)->second);
                    ofs << ", style=dashed, color = " << colors[(size_t) ind % 31] << "]" << ";\n";
                }
            } else {
                if (obj_keys->find(e->obj_name) == obj_keys->end()) {
                    obj_keys->insert({e->obj_name, edge_label});
                    ofs << " [label=" << edge_label << ", color = " << colors[(cnt % 31)] << "]" << ";\n";
                    cnt += 1;
                } else {
                    int ind = std::stoi(obj_keys->find(e->obj_name)->second);
                    ofs << " [label=" << obj_keys->find(e->obj_name)->second;
                    ofs << ", color = " << colors[(size_t) ind % 31] << "]" << ";\n";
                }
            }
        }
    }
}

void digraph::print_dot(std::string filepath, std::string ext) {

    std::string str = name;
    std::string::iterator end_pos = std::remove(str.begin(), str.end(), ' ');
    str.erase(end_pos, str.end());

    std::replace(str.begin(), str.end(), '-', '_');

    // setup print destination
    std::ofstream graph_file;
    std::ofstream legend_file;
    if (filepath.length() > 0) {
        graph_file.open(filepath + "/" + str + ext);
        legend_file.open(filepath + "/" + str + "_legend" + ext);
        assert(graph_file.is_open());
        assert(legend_file.is_open());

    } else {
        graph_file.copyfmt(std::cout);
        graph_file.clear(std::cout.rdstate());
        graph_file.basic_ios<char>::rdbuf(std::cout.rdbuf());
        legend_file.copyfmt(std::cout);
        legend_file.clear(std::cout.rdstate());
        legend_file.basic_ios<char>::rdbuf(std::cout.rdbuf());
    }


    // set up legend graph
    std::unordered_map<std::string, std::string> eqn_keys;
    std::unordered_map<std::string, std::string> obj_keys;

    // print graph of variables
    graph_file << "digraph " << str << " {\n";
    graph_file << "\t" << "label =\"" << name << "\";\n\tlabelloc=top;\n";
    graph_file << "\trankdir=LR;\n\tranksep=\"1\";\n";

    for (auto it = vertices.begin(); it != vertices.end(); ++it) {
        if (vertex *v = it->second.at(0)) {
            // make secondary cmods a different shape and color
            if (SAM_cmod_to_outputs.find(v->name) != SAM_cmod_to_outputs.end()) {
                graph_file << "\t" << format_vertex_name(v)
                           << " [shape=polygon, style=filled, fillcolor=darkslategray3]\n";
            }
        }
        // make nodes for ssc_variables colored
        if (vertex *v = it->second.at(1)) {
            if (v->edges_out.size() + v->edges_in.size() > 0)
                graph_file << "\t" << format_vertex_name(v) << " [style=filled, fillcolor=grey]\n";
        }
    }
    graph_file << "\n";
    for (auto it = vertices.begin(); it != vertices.end(); ++it) {
        for (size_t i = 0; i < 2; i++) {
            if (vertex *v = it->second.at(i)) {
                print_vertex(v, graph_file, &obj_keys, &eqn_keys);
            }
        }
    }
    graph_file << "}";
    graph_file.close();

    // print legend
    legend_file << "digraph " << str << "_legend {\n";
    legend_file << "\tlabel=\"Legend: " << name << "\";\n\tlabelloc=top;\n";
    legend_file << "\trankdir=LR;\tranksep=\"3\";\n";
    legend_file << "\tkey [label=<<table border=\"0\" cellpadding=\"2\" cellspacing=\"25\" cellborder=\"0\">\n";

    for (size_t i = 0; i < eqn_keys.size(); i++) {
        legend_file << "\t<tr><td align=\"right\" port=\"e" << i << "\">eqn_var" << i << "</td></tr>\n";
    }
    for (size_t i = 0; i < obj_keys.size(); i++) {
        legend_file << "\t<tr><td align=\"right\" port=\"o" << i << "\">func_var" << i << "</td></tr>\n";
    }

    legend_file << "\t</table>>]\n"
                   "\tkey2 [label=<<table border=\"0\" cellpadding=\"2\" cellspacing=\"25\" cellborder=\"0\">\n";

    for (size_t i = 0; i < eqn_keys.size(); i++) {
        legend_file << "\t<tr><td port=\"e" << i << "\">&nbsp;</td></tr>\n";
    }
    for (size_t i = 0; i < obj_keys.size(); i++) {
        legend_file << "\t<tr><td port=\"o" << i << "\">&nbsp;</td></tr>\n";
    }
    legend_file << "\t</table>>]\n";

    size_t i = 0;
    for (auto it = eqn_keys.begin(); it != eqn_keys.end(); ++it) {
        legend_file << "\tkey:e" << i << ":e -> key2:e" << i << ":w [style=dashed, label=\"" << it->second;
        legend_file << ": " << it->first << "\"]\n";
        i++;
    }
    i = 0;
    for (auto it = obj_keys.begin(); it != obj_keys.end(); ++it) {
        legend_file << "\tkey:o" << i << ":e -> key2:o" << i << ":w [label=\"" << it->second;
        legend_file << ": " << it->first << "\"]\n";
        i++;
    }
    legend_file << "}";
    legend_file.close();

}
