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

#ifndef SYSTEM_ADVISOR_MODEL_BUILDER_GENERATOR_HELPER_H
#define SYSTEM_ADVISOR_MODEL_BUILDER_GENERATOR_HELPER_H

#include <map>
#include <vector>
#include <string>
#include <algorithm>
#include <fstream>

#include <ssc/sscapi.h>

#include "data_structures.h"
#include "variable_graph.h"
#include "lk_eval.h"
#include "config_extractor.h"

static std::map<std::string, std::vector<std::string>> cmod_to_extra_modules = {
        {"generic", {""}}
};

static std::map<std::string, std::vector<std::string>> extra_modules_to_members = {
        {"adjust", {"constant", "hourly", "periods"}},
        {"dc_adjust", {"constant", "hourly", "periods"}}
};

std::string find_module_of_var(std::string var, std::string cmod);

int get_varvalue_type(std::string name, std::string& config);

std::string print_parameter_type(vertex *v, std::string cmod,
                                 std::unordered_map<std::string, ssc_module_t> &module_map);

std::string print_return_type(vertex *v, std::string cmod,
                              std::unordered_map<std::string, ssc_module_t> &module_map);


void export_function_declaration(std::ofstream& ff, std::string return_type, std::string name,
                                 std::vector<std::string> inputs);

std::string spell_type(int type);

std::string var_value_to_json(VarValue* vv);

std::string ssc_value_to_json(int ssc_type, VarValue* vv);

std::string translate_lookup_type(std::string name, std::string config);

bool translate_equation_to_cplusplus(config_extractor *config_ext, equation_info &eqn_info, std::ofstream &of,
                                     const std::string &cmod);

bool translate_callback_to_cplusplus(config_extractor *config_ext, callback_info &cb_info, std::ofstream &of,
                                     const std::string &cmod);

void print_var_info_table(const std::string &config, const std::string &filepath);

#endif //SYSTEM_ADVISOR_MODEL_BUILDER_GENERATOR_INFO_H
