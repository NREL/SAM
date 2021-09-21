/**
BSD-3-Clause
Copyright 2019 Alliance for Sustainable Energy, LLC
Redistribution and use in source and binary forms, with or without modification, are permitted provided
that the following conditions are met :
1.	Redistributions of source code must retain the above copyright notice, this list of conditions
and the following disclaimer.
2.	Redistributions in binary form must reproduce the above copyright notice, this list of conditions
and the following disclaimer in the documentation and/or other materials provided with the distribution.
3.	Neither the name of the copyright holder nor the names of its contributors may be used to endorse
or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED.IN NO EVENT SHALL THE COPYRIGHT HOLDER, CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES
DEPARTMENT OF ENERGY, NOR ANY OF THEIR EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
OR CONSEQUENTIAL DAMAGES(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

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




