#ifndef SYSTEM_ADVISOR_MODEL_EQUATION_EXTRACTOR_H
#define SYSTEM_ADVISOR_MODEL_EQUATION_EXTRACTOR_H

#include <string>
#include <vector>

#include "equations.h"
#include "data_structures.h"
#include "lk_env.h"
#include "config_extractor.h"

/**
 *  One per config?
 */

class equation_extractor : public EqnDatabase {
private:
    std::string ui_form_name;
    std::vector<std::string> errors;
    lk::env_t *m_env;

public:
    equation_extractor(std::string name) {
        ui_form_name = name;
        m_env = new lk::env_t;
        m_env->register_funcs(lk::stdlib_basic());
        m_env->register_funcs(lk::stdlib_sysio());
        m_env->register_funcs(lk::stdlib_math());
        m_env->register_funcs(lk::stdlib_string());
        m_env->register_funcs(invoke_ssc_funcs());
        m_env->register_funcs(invoke_casecallback_funcs());
    };

    ~equation_extractor() {
        delete m_env;
    }


    bool parse_and_export_eqns(std::string eqn_script);

    /// Returns the input and outputs of each equation
    void export_to_equation_info();


    std::vector<std::string> get_errors() {
        return errors;
    }
};


#endif //SYSTEM_ADVISOR_MODEL_EQUATION_EXTRACTOR_H
