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

std::vector<std::string> typestr = {"void", "float", "util::matrix_t<ssc_number_t>"
        , "util::matrix_t<ssc_number_t>", "const char*", "var_table", "binary"};

std::string translate_lookup(std::string name, std::string config){
    VarValue* vv = find_default_from_ui(name, config);

    std::string result;
    int type = vv->Type();

    result += typestr[type] + " " + name + " = vt->lookup(\"" + name + "\")->";

    if (type == 1 || type == 2 || type == 3){
        result += "num;";
    }
    else if (type == 4){
        result += "str;";
    }
    else if (type == 5){
        result += "table;";
    }
    else{
        result += "ERROR: type not implemented";
    }
    return result;
}

bool equation_extractor::translate_to_cplusplus(equation_info &eqn_info, std::ofstream &of, std::string config) {

    translator e( eqn_info.eqn_data->tree, m_env );
    e.set_ui_source(ui_form_name);

    // figure out if single or multiple outputs
    std::string output_name = "MIMO";
    if (eqn_info.ui_outputs.size() == 1)
        output_name = eqn_info.ui_outputs[0];

    // translate eq
    std::string result;
    unsigned int ctl = 0;
    bool ok = e.translate(eqn_info.eqn_data->tree, m_env, result, 0, ctl, output_name);
    if ( !ok ){
        for( size_t i=0;i<e.error_count();i++ ){
            std::cout << "equation_extractor::translate_to_cplusplus error " << e.get_error(i) << "\n";
            errors.push_back( e.get_error(i) );
        }
        return false;
    }

    // print auxiliary functions above
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

    // if single output, return type is set by VarValue, otherwise return a table
    int type = 5;                   // var_table
    if (output_name != "MIMO")
        type = find_default_from_ui(output_name, active_config)->Type();

    // begin equation function
    of << typestr[type] << " ";
    of << "SAM_" + format_as_symbol(ui_form_name) + "_" + output_name + "_eqn(var_table* vt, lk::invoke_t* cxt){\n";

    // set up inputs and outputs (if MIMO) variable placeholders
    of << "\t// inputs\n";
    for (size_t i = 0; i < eqn_info.ui_inputs.size(); i++){
        std::string name = eqn_info.ui_inputs[i];
        of << "\t" << translate_lookup(name, config) << "\n";

    }
    of << "\n";
    if (output_name == "MIMO"){
        of << "\t// outputs\n";
        for (size_t i = 0; i < eqn_info.ui_outputs.size(); i++){
            std::string name = eqn_info.ui_outputs[i];
            VarValue* vv = find_default_from_ui(name, config);
            of << "\t" << typestr[vv->Type()] << " " + name << ";\n\n";
        }
    }

    // translated block
    of << indent(result);

    // if function has already returned, end definition now
    if (result.find("return") != std::string::npos){
        of << "}\n\n";
        return true;
    }

    // set up cxt values for UI
    of << "\tif (cxt){\n";
    size_t n = eqn_info.ui_outputs.size();
    if ( n==1 ){
        of << "\t\tcxt.result().assign(" + output_name + ");\n";
    }
    else{
        of << "\t\tcxt->result().empty_hash();\n";
        for (size_t i = 0; i < n; i++){
            std::string var = eqn_info.ui_outputs[i];
            of << "\t\tcxt.result().hash_item(\""<< var << "\").assign( \"" << var << "\" );\n";
        }
    }
    of << "\t}\n\n";

    // set up single return value or a table
    if (n > 1){
        of << "\tvar_table vt;\n";
        output_name = "vt";
        for (size_t i = 0; i < n; i++){
            std::string var = eqn_info.ui_outputs[i];

            int type = find_default_from_ui(var, config)->Type();
            if (type == VV_ARRAY || type == VV_MATRIX || type == VV_TABLE){
                of << "ERROR array, matrix, and table return not implemented\n";
                return false;
            }
            of << "\tvt.assign( \"" << var << "\", "<< var << " );\n";
        }
    }
    of << "\treturn " << output_name << ";\n";
    of << "\n}\n\n";
    return true;
}
