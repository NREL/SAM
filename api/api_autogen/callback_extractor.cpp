#include <iostream>
#include <sstream>
#include <algorithm>
#include <set>

#include <lk/absyn.h>
#include <lk/parse.h>
#include <lk/eval.h>
#include <lk/stdlib.h>
#include <lk/env.h>

#include "ui_form_extractor.h"
#include "callback_extractor.h"
#include "lk_env.h"
#include "data_structures.h"
#include "builder_generator_helper.h"

std::string active_object;
std::string active_ui;
std::string active_subobject;
std::string active_cmod;
int active_method = -1;
bool map_subobject;

std::unordered_map<std::string, std::vector<std::string>> SAM_ui_obj_to_enabled_variables;
extern std::unordered_map<std::string, digraph*> SAM_config_to_variable_graph;

size_t callback_extractor::parse_cmod_statement(std::string callback_script, size_t pos_start){
    size_t start = callback_script.find_first_of("\'\"", pos_start);
    size_t end = callback_script.find_first_of("\'\"", start+1);
    if (start >= end){
        std::cout << "Error extracting cmod name from callback script for " << config_name << "\n";
        return 0;
    }
    std::string str = callback_script.substr(start+1, (end-start-1));

    // remove white space & quotations
    std::string::iterator end_pos = std::remove(str.begin(), str.end(), ' ');
    str.erase(end_pos, str.end());

    //compute_modules.push_back(str);
    return end;
}


bool callback_extractor::parse_script(std::string callback_script) {
    lk::input_string in( callback_script );
    lk::parser parse( in );
    lk::node_t *tree = parse.script();

    if ( parse.error_count() != 0 || parse.token() != lk::lexer::END){
        for( int i=0;i<parse.error_count();i++ )
            errors.push_back( parse.error(i).ToStdString());
        errors.push_back( "callback script parsing did not reach end of input" );
        if ( tree ) delete tree;
        return false;
    }
    else
    {
        extractor_interpreter e( tree, m_env );

        bool ok = e.run();
        if ( !ok ){
            for( size_t i=0;i<e.error_count();i++ )
                errors.push_back( e.get_error(i).ToStdString());
            return false;
        }

    }
    return true;
}

int callback_extractor::invoke_method_type(const std::string &method_name) {
    lk::vardata_t *cbvar = m_env->lookup( method_name, true);

    if (!cbvar || cbvar->type() != lk::vardata_t::HASH )
    {
        std::cout << "callback_extractor::invoke_method_type: could not find " + method_name + " in " + config_name << "\n";
        return 0;
    }

    // get all instances of method
    int error = 0;

    lk::varhash_t* h = cbvar->hash();
    for (auto it = h->begin(); it != h->end(); ++it){

        std::string obj_name = it->first.ToStdString();
        lk::expr_t *p_define = it->second->deref().func();
        if ( p_define->oper != lk::expr_t::DEFINE )
        {
            std::cout << "callback_extractor::invoke_method_type: structure of " + obj_name;
            std::cout << " not " + method_name + " in " + config_name << "\n";
            return 0;
        }

        if ( p_define->right == 0 )
        {
            std::cout << "callback_extractor::invoke_method_type: block undefined for" + obj_name;
            std::cout << " not " + method_name + " in " + config_name << "\n";
            return 0;
        }


        active_ui = "";
        // clear active_cmod in case it was set previously in another object invocation
        active_cmod = "";
        active_object = obj_name;
        active_subobject = "";
        map_subobject = true;
        std::set<std::string> obj_to_skip = {"refresh_library", "Solar Resource Data", "user_specified_weather_file",
                                             "solar_data_file_name", "btnQueryOpenEI", "visualize_load_data",
                                             "visualize_thermal_load_data"};
        if (obj_to_skip.count(obj_name) > 0){
            return 0;
        }
        if (!invoke_function(p_define->right, obj_name))
            error += 1;

    }
    return error;
}

bool callback_extractor::invoke_function(lk::node_t *root, std::string f_name) {

    try {
        extractor_interpreter eval = extractor_interpreter(root, m_env);
        if ( !eval.run())
        {
            for (size_t i=0;i<eval.error_count();i++)
                errors.push_back(eval.get_error(i).ToStdString());
            return false;
        }
        return true;

    } catch(std::exception & ){
        std::cout << "callback_extractor::invoke_function error: could not invoke "<< f_name << " for " << config_name << "\n";
        return false;
    }

}

bool callback_extractor::extract_functions() {
    active_method = LOAD;
    int nerrors = invoke_method_type("on_load");
    if (nerrors > 0){
        std::cout << "callback_extractor::extract_functions error: " << nerrors << " 'on_load' obj errors\n";
        for (size_t i = 0; i < errors.size(); i++)
            std::cout << errors[i] << "\n";
    }

    active_method = CHNG;
    nerrors = invoke_method_type("on_change");
    if (nerrors > 0){
        std::cout << "callback_extractor::extract_functions error: " << nerrors << " 'on_change' obj errors\n";
        for (size_t i = 0; i < errors.size(); i++)
            std::cout << errors[i] << "\n";
    }

    active_method = -1;

    return true;
}


