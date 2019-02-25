#ifndef SYSTEM_ADVISOR_MODEL_EQUATION_EXTRACTOR_H
#define SYSTEM_ADVISOR_MODEL_EQUATION_EXTRACTOR_H

#include <string>
#include <vector>

#include "equations.h"
#include "data_structures.h"

/**
 *  One per config?
 */

class equation_extractor : public EqnDatabase{
private:
    std::string ui_form_name;
    std::vector<std::string> input_variables;
    std::vector<std::string> output_variables;

public:
    equation_extractor(std::string name){
        ui_form_name = name;
    };

    bool parse_script(std::string eqn_script);

    /// Returns the input and outputs of each equation
    void export_to_equation_info();
};

class callback_extractor{
private:
    std::string ui_form_name;
    std::vector<std::string> compute_modules;

    struct cb_data{ lk::node_t *tree; std::string source; };
    std::vector<cb_data*> m_cblist;

    lk::env_t* m_cbenv;

    lk::node_t *parse_functions(const std::string &method_name);

    bool invoke_function(lk::node_t *root, std::string f_name);

public:

    callback_extractor(std::string name, lk::env_t* env){
        ui_form_name = name;
        m_cbenv = env;
    };

    size_t parse_cmod_statement(std::string callback_script, size_t pos_start);

    bool parse_script(std::string callback_script);


    void export_to_secondary_cmod_info();

    std::vector<std::string> errors;
};

#endif //SYSTEM_ADVISOR_MODEL_EQUATION_EXTRACTOR_H
