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
    void export_to_equation_info(config_variables_info& cvi);
};

class callback_extractor{
private:
    std::string ui_form_name;
    std::vector<std::string> compute_modules;

public:
    callback_extractor(std::string name){
        ui_form_name = name;
    };

    size_t parse_cmod_statement(std::string callback_script, size_t pos_start);

    bool parse_script(std::string callback_script);

    void export_to_equation_info(config_variables_info &cvi);
};

#endif //SYSTEM_ADVISOR_MODEL_EQUATION_EXTRACTOR_H
