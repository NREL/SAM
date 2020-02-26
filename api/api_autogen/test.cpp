#include "test.h"

void make_test_graph(digraph &g) {
    vertex *ssc1 = g.add_vertex("ssc1", true);
    vertex *ssc2 = g.add_vertex("ssc2", true);
    vertex *ssc3 = g.add_vertex("ssc3", true);
    vertex *ui1 = g.add_vertex("ui1", false);
    vertex *ui2 = g.add_vertex("ui2", false);
    vertex *ui3 = g.add_vertex("ui3", false);
    vertex *ui4 = g.add_vertex("ui4", false);
    g.add_edge(ssc1, ssc2, 1, "", "", std::string(), nullptr);
    g.add_edge(ssc1, ui1, 1, "", "", std::string(), nullptr);
    g.add_edge(ui1, ssc2, 1, "", "", std::string(), nullptr);
    g.add_edge(ssc3, ui2, 1, "", "", std::string(), nullptr);
    g.add_edge(ui1, ui3, 1, "", "", std::string(), nullptr);
    g.add_edge(ui3, ssc2, 1, "", "", std::string(), nullptr);
    g.add_edge(ui4, ssc1, 1, "", "", std::string(), nullptr);

}

bool run_variable_graph_tests() {
    digraph *g = new digraph("test");
    make_test_graph(*g);
    g->print_dot(std::string());

    digraph *sub_g = new digraph("sub");
    g->subgraph_ssc_only(*sub_g);
    sub_g->print_dot(std::string());
    delete g;
    delete sub_g;
    return true;
}
