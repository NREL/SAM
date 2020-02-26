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
        {"adjust",    {"constant", "hourly", "periods"}},
        {"dc_adjust", {"constant", "hourly", "periods"}}
};

std::string find_module_of_var(std::string var, std::string cmod);

int get_varvalue_type(std::string name, std::string &config);

std::string print_parameter_type(vertex *v, std::string cmod,
                                 std::unordered_map<std::string, ssc_module_t> &module_map);

std::string print_return_type(vertex *v, std::string cmod,
                              std::unordered_map<std::string, ssc_module_t> &module_map);


void export_function_declaration(std::ofstream &ff, std::string return_type, std::string name,
                                 std::vector<std::string> inputs);

std::string spell_type(int type);

std::string var_value_to_json(VarValue *vv);

std::string ssc_value_to_json(int ssc_type, VarValue *vv);

std::string translate_lookup_type(std::string name, std::string config);

bool translate_equation_to_cplusplus(config_extractor *config_ext, equation_info &eqn_info, std::ofstream &of,
                                     const std::string &cmod);

bool translate_callback_to_cplusplus(config_extractor *config_ext, callback_info &cb_info, std::ofstream &of,
                                     const std::string &cmod);

void print_var_info_table(const std::string &config, const std::string &filepath);

#endif //SYSTEM_ADVISOR_MODEL_BUILDER_GENERATOR_INFO_H
