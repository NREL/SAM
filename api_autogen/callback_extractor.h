#ifndef SYSTEM_ADVISOR_MODEL_CALLBACK_EXTRACTOR_H
#define SYSTEM_ADVISOR_MODEL_CALLBACK_EXTRACTOR_H

#include <string>
#include <vector>


#include "equations.h"
#include "lk_env.h"
#include "data_structures.h"
#include "lk_eval.h"
#include "config_extractor.h"

class callback_extractor{
private:

    std::string config_name;

    lk::env_t* m_env;

    std::vector<std::string> errors;


    int invoke_method_type(const std::string &method_name);

    bool invoke_function(lk::node_t *root, std::string f_name);


public:

    callback_extractor(std::string n, lk::env_t& e){
        config_name = n;
        m_env = &e;
        m_env->register_funcs( lk::stdlib_basic() );
        m_env->register_funcs( lk::stdlib_sysio() );
        m_env->register_funcs( lk::stdlib_math() );
        m_env->register_funcs( lk::stdlib_string() );
        m_env->register_funcs( invoke_ssc_funcs() );
        m_env->register_funcs( invoke_casecallback_funcs() );
    };

    size_t parse_cmod_statement(std::string callback_script, size_t pos_start);

    /// runs the callback script and saves each function in m_env
    bool parse_script(std::string callback_script);

    /// invokes each function
    bool extract_functions();

};

#endif //SYSTEM_ADVISOR_MODEL_CALLBACK_EXTRACTOR_H
