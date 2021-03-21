#include <iostream>
#include <fstream>
#include <algorithm>

#include <lk/absyn.h>
#include <lk/parse.h>
#include <lk/eval.h>
#include <lk/stdlib.h>

#include "equation_extractor.h"
#include "ui_form_extractor.h"
#include "lk_env.h"
#include "lk_eval.h"
#include "config_extractor.h"

std::unordered_map<std::string, std::vector<equation_info>> SAM_ui_form_to_eqn_info;

bool equation_extractor::parse_and_export_eqns(const std::string& eqn_script){
    lk::input_string data( eqn_script );
    wxArrayString err;

    Parse(data, &err);

    if (err.Count() > 0){
        std::cout << "Errors in " << ui_form_name << " form\n:";
        for (size_t n = 0; n < err.Count(); n++){
            std::cout << err[n] << std::endl;
        }
        return false;
    }
    export_to_equation_info();
    return true;
}

void equation_extractor::export_to_equation_info(){
    std::vector<equation_info> ei_vec;

    std::vector<EqnData*> eqns = GetEquations();
    for (auto & eqn : eqns){
        equation_info ei;
        for (size_t n = 0; n < eqn->inputs.Count(); n++){
            ei.all_inputs.push_back(eqn->inputs[n].ToStdString());
        }
        for (size_t n = 0; n < eqn->outputs.Count(); n++){
            ei.all_outputs.push_back(eqn->outputs[n].ToStdString());
        }
        ei.eqn_data = eqn;
        ei.ui_source = ui_form_name;
        ei_vec.push_back(ei);
    }
    if (!ei_vec.empty())
        SAM_ui_form_to_eqn_info.insert({ui_form_name, ei_vec});
}

std::string translate_fx(const function_builder& fb, std::ofstream &of) {
//    std::cout << fb.ret_type << " " << fb.name << "(";
//    for (size_t i = 0; i < fb.args.size(); i++){
//        std::cout << fb.args[i];
//        if (i != fb.args.size() - 1)
//            std::cout <<", ";
//    }
//    std::cout << "){\n";
//    std::cout << fb.block;
//    of << "}\n\n";
	std::string str;
	return str;
}




