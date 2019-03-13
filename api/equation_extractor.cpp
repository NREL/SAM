#include <iostream>
#include <algorithm>

#include <lk/absyn.h>
#include <lk/parse.h>
#include <lk/eval.h>
#include <lk/stdlib.h>

#include "equation_extractor.h"
#include "lk_env.h"

std::unordered_map<std::string, std::vector<equation_info>> SAM_ui_form_to_eqn_info;

bool equation_extractor::parse_script(std::string eqn_script){
    lk::input_string data( eqn_script );
    wxArrayString errors;

    Parse(data, &errors);

    if (errors.Count() > 0){
        std::cout << "Errors in " << ui_form_name << " form\n:";
        for (size_t n = 0; n < errors.Count(); n++){
            std::cout << errors[n] << std::endl;
        }
        return false;
    }
    export_to_equation_info();
    return true;
}

void equation_extractor::export_to_equation_info(){
    std::vector<equation_info> ei_vec;

    std::vector<EqnData*> eqns = GetEquations();
    for (size_t i = 0; i < eqns.size(); i++){
        equation_info ei;
        for (size_t n = 0; n < eqns[i]->inputs.Count(); n++){
            ei.ui_inputs.push_back(eqns[i]->inputs[n].ToStdString());
        }
        for (size_t n = 0; n < eqns[i]->outputs.Count(); n++){
            ei.ui_outputs.push_back(eqns[i]->outputs[n].ToStdString());
        }
        ei.eqn_data = eqns[i];
        ei_vec.push_back(ei);
    }
    if (ei_vec.size() > 0)
        SAM_ui_form_to_eqn_info.insert({ui_form_name, ei_vec});
}

