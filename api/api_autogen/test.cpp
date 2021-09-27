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

#include "test.h"

void make_test_graph(digraph& g){
    vertex* ssc1 = g.add_vertex("ssc1", true);
    vertex* ssc2 = g.add_vertex("ssc2", true);
    vertex* ssc3 = g.add_vertex("ssc3", true);
    vertex* ui1 = g.add_vertex("ui1", false);
    vertex* ui2 = g.add_vertex("ui2", false);
    vertex* ui3 = g.add_vertex("ui3", false);
    vertex* ui4 = g.add_vertex("ui4", false);
    g.add_edge(ssc1, ssc2, 1, "", "", std::string(), nullptr);
    g.add_edge(ssc1, ui1, 1, "", "", std::string(), nullptr);
    g.add_edge(ui1, ssc2, 1, "", "", std::string(), nullptr);
    g.add_edge(ssc3, ui2, 1, "", "", std::string(), nullptr);
    g.add_edge(ui1, ui3, 1, "", "", std::string(), nullptr);
    g.add_edge(ui3, ssc2, 1, "", "", std::string(), nullptr);
    g.add_edge(ui4, ssc1, 1, "", "", std::string(), nullptr);

}

bool run_variable_graph_tests(){
    digraph* g = new digraph("test");
    make_test_graph(*g);
    g->print_dot(std::string());

    digraph* sub_g = new digraph("sub");
    g->subgraph_ssc_only(*sub_g);
    sub_g->print_dot(std::string());
    delete g;
    delete sub_g;
    return true;
}
