#include <iostream>
#include <fstream>
#include <algorithm>

#include <lk/absyn.h>
#include <lk/parse.h>
#include <lk/eval.h>
#include <lk/stdlib.h>

#include "equation_extractor.h"
#include "lk_env.h"
#include "lk_eval.h"

std::unordered_map<std::string, std::vector<equation_info>> SAM_ui_form_to_eqn_info;

bool equation_extractor::parse_and_export_eqns(std::string eqn_script){
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

std::string translate_fx(function_builder fb, std::ofstream &of) {
//    std::cout << fb.ret_type << " " << fb.name << "(";
//    for (size_t i = 0; i < fb.args.size(); i++){
//        std::cout << fb.args[i];
//        if (i != fb.args.size() - 1)
//            std::cout <<", ";
//    }
//    std::cout << "){\n";
//    std::cout << fb.block;
//    of << "}\n\n";
}
std::vector<std::string> typestr = {"void", "float", "float*", "float*", "const char*", "table", "binary"};

std::string equation_extractor::translate_to_cplusplus(equation_info &eqn_info, std::ofstream &of) {

    translator e( eqn_info.eqn_data->tree, m_env );
    e.set_ui_source(ui_form_name);

    std::string result;
    unsigned int ctl = 0;
    bool ok = e.translate(eqn_info.eqn_data->tree, m_env, result, 0, ctl);
    if ( !ok ){
        for( size_t i=0;i<e.error_count();i++ ){
            std::cout << "equation_extractor::translate_to_cplusplus error " << e.get_error(i) << "\n";
            errors.push_back( e.get_error(i) );
        }
        return "";
    }

    auto aux_fx = e.get_aux_functions();
    for (auto it = aux_fx.begin(); it != aux_fx.end(); ++it){
        function_builder fb = it->second;
        of << fb.ret_type << " " << fb.name << "(";
        for (size_t i = 0; i < fb.args.size(); i++){
            of << fb.args[i];
            if (i != fb.args.size() - 1)
                of <<", ";
        }
        of << "){\n";
        of << indent(fb.block);
        of << "}\n\n";
    }

    std::string output_name = eqn_info.eqn_data->outputs[0];
    int type = find_default_from_ui(output_name, active_config)->Type();

    of << typestr[type] << " ";
    of << "SAM_" + format_as_code(ui_form_name) + "_" + output_name + "_eqn(var_table* vt){\n";
    of << indent(result);
    of << "\n}\n\n";

    return result;
}
