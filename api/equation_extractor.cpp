#include <iostream>
#include <algorithm>

#include "equation_extractor.h"

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
    else return true;
}

std::vector<std::string> equation_extractor::get_output_variables(){
    output_variables.clear();
    std::vector<EqnData*> eqns = GetEquations();
    for (size_t i = 0; i < eqns.size(); i++){
        // equations for which the result is the output
        for (size_t n = 0; n < eqns[i]->outputs.Count(); n++){
            output_variables.push_back(eqns[i]->outputs[n].ToStdString());
        }
    }
    return output_variables;
}

size_t callback_extractor::parse_cmod_statement(std::string callback_script, size_t pos_start){
    size_t start = callback_script.find_first_of("\'\"", pos_start);
    size_t end = callback_script.find_first_of("\'\"", start+1);
    if (start >= end){
        std::cout << "Error extracting cmod name from callback script for " << ui_form_name << "\n";
        return 0;
    }
    std::string str = callback_script.substr(start+1, (end-start-1));

    // remove white space & quotations
    std::string::iterator end_pos = std::remove(str.begin(), str.end(), ' ');
    str.erase(end_pos, str.end());

    compute_modules.push_back(str);
    return end;
}

bool callback_extractor::parse_script(std::string callback_script) {
    std::string cmod_handle = "ssc_exec(";

    size_t found = callback_script.find(cmod_handle);
    while (found != std::string::npos){
        size_t end = parse_cmod_statement(callback_script, found+cmod_handle.length());
        if (end == 0)
            return false;
        found = callback_script.find(cmod_handle, end);
    }
    return true;
}

std::vector<std::string> callback_extractor::get_compute_modules() {
    return compute_modules;
}