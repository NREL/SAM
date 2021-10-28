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

#ifndef SYSTEM_ADVISOR_MODEL_CONFIG_EXTRACTOR_H
#define SYSTEM_ADVISOR_MODEL_CONFIG_EXTRACTOR_H

#include <vector>
#include <string>

#include <lk/env.h>
//#include <variables.h>
#include "variables.h"

#include "variable_graph.h"
#include "config_extractor.h"
#include "callback_extractor.h"
#include "lk_env.h"

class config_extractor{
private:
    std::string config_name;

    lk::env_t m_env;

    std::string defaults_file_dir;

    digraph* var_graph;

    callback_extractor *cb_ext;

    size_t load_variables_into_graph(VarTable &vt);

    void export_to_ui_form_db(std::string ui);

    std::vector<std::string> errors;

public:

    std::unordered_map<equation_info*, std::string> completed_equation_signatures;
    std::unordered_map<callback_info*, std::string> completed_callback_signatures;

    config_extractor(std::string name, const std::string &defaults_file_dir);


    ~config_extractor(){
        delete var_graph;
        delete cb_ext;

        // clear global config-specific variables
        active_ui = "";
        active_config = "";
        active_cmod = "";
        active_subobject = "";
        active_method = -1;
        map_subobject = true;
        subobjects_completed.clear();
    }

    bool load_defaults_for_config();

    std::string spell_equation(lk::node_t *node);

    bool map_equations();

    void register_callback_functions();

    std::string get_name() {return config_name;}

    lk::env_t* get_env() {return &m_env;}

    std::vector<std::string>& get_errors() {return errors;}

    digraph* get_variable_graph(){
            return var_graph;
    }


};

#endif //SYSTEM_ADVISOR_MODEL_CONFIG_EXTRACTOR_H
