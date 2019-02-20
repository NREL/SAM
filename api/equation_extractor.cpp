#include <iostream>

#include "equation_extractor.h"

bool equation_extractor::parse_script(std::string eqn_script){
    lk::input_string data( eqn_script );
    wxArrayString errors;

    Parse(data, &errors);

    if (errors.Count() > 0){
        for (size_t n = 0; n < errors.Count(); n++){
            std::cout << errors[n] << std::endl;
        }
        return false;
    }
    else return true;
}

std::vector<std::string> equation_extractor::get_output_variables(){
    output_variables.clear();
    std::vector<EqnData*> eqns = GetEquations();
    for (size_t i = 0; i < eqns.size(); i++){
        for (size_t n = 0; n < eqns[i]->outputs.Count(); n++){
            output_variables.push_back(eqns[i]->outputs[n]);
        }
    }
    return output_variables;
}
