#ifndef SYSTEM_ADVISOR_MODEL_EQUATION_EXTRACTOR_H
#define SYSTEM_ADVISOR_MODEL_EQUATION_EXTRACTOR_H

#include <string>
#include <vector>

#include "equations.h"

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

    std::vector<std::string> get_output_variables();
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

    std::vector<std::string> get_compute_modules();
};

#endif //SYSTEM_ADVISOR_MODEL_EQUATION_EXTRACTOR_H
