#ifndef SYSTEM_ADVISOR_MODEL_CALLBACK_EXTRACTOR_H
#define SYSTEM_ADVISOR_MODEL_CALLBACK_EXTRACTOR_H

#include <string>
#include <vector>
#include <lk/eval.h>

#include "equations.h"
#include "lk_env.h"
#include "data_structures.h"



class extractor_interpreter : public lk::eval{
public:
    extractor_interpreter(lk::node_t *tree, lk::env_t* env):lk::eval(tree, env){}

    bool interpret(lk::node_t *root, lk::env_t*cur_env, lk::vardata_t &result, unsigned int flags,
        unsigned int &ctl_id);

    virtual bool special_set( const lk_string &name, lk::vardata_t &val );
    virtual bool special_get( const lk_string &name, lk::vardata_t &val );

    void map_assignment(lk::node_t *src, lk::node_t *dest);

};

class callback_extractor{
private:
    std::string config_name;

    lk::env_t* m_env;

    std::vector<std::string> errors;

    struct cb_data{ lk::node_t *tree; std::string source; };

    std::vector<cb_data*> m_cblist;

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

    void export_to_secondary_cmod_info();

};

std::string spell_out(lk::expr_t *n, std::vector<std::string> &vertex_names, std::vector<bool> &vertex_is_ssc);

std::string spell_list(lk::list_t *l, std::vector<std::string> &vertex_names, std::vector<bool> &vertex_is_ssc,
                       bool map_literals);

#endif //SYSTEM_ADVISOR_MODEL_CALLBACK_EXTRACTOR_H
