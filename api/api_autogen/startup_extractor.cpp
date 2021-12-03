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

#include <sstream>
#include <fstream>
#include <string>
#include <vector>
#include <iostream>
#include <unordered_map>
#include <ctime>

#include <lk/absyn.h>
#include <lk/parse.h>
#include <lk/eval.h>
#include <lk/stdlib.h>

#include <ssc/sscapi.h>

#include "data_structures.h"
#include "startup_extractor.h"

std::unordered_map<std::string, std::vector<page_info>> SAM_config_to_input_pages;
std::map<std::string, std::vector<std::string>> SAM_config_to_primary_modules;
std::map<std::string, std::pair<std::string, std::string>> SAM_option_to_description;

// print into dictionary format
void startup_extractor::print_config_to_input_pages(){
    std::cout << "config_to_input_pages_map = {\n";
    // configuration : {
    for (auto it = SAM_config_to_input_pages.begin(); it != SAM_config_to_input_pages.end(); ++it){
        if (it != SAM_config_to_input_pages.begin()) std::cout << ",\n";
        std::cout << "'" << it->first << "' :{\n\t";
        // group : { 'common' : (common_uiforms)
        for (size_t i = 0; i < it->second.size(); i++){
            std::cout << "'"<< it->second[i].sidebar_title << "' :{\n\t\t";
            if (!it->second[i].common_uiforms.empty() ){
                std::cout << "'common': \n\t\t\t";
                std::cout << it->second[i].common_uiforms;
            }
            // {exlusive_var : (exclusive_uiforms)
            if (!it->second[i].exclusive_var.empty() ){
                if (!it->second[i].common_uiforms.empty())
                    std::cout << ",\n\t\t" ;
                std::cout << "'" << it->second[i].exclusive_var << "' :\n\t\t\t" ;
                std::cout << it->second[i].exclusive_uiforms << "";
            }
            if (i < it->second.size()-1) std::cout << "},\n\t";
            else std::cout << "\n\t}\n";
        }
        std::cout << "}";
    }
    std::cout << "}\n\n";
}

void startup_extractor::print_config_to_modules(){
    std::cout << "config_to_modules_map = {\n";
    // configuration : {
    for (auto it = SAM_config_to_primary_modules.begin(); it != SAM_config_to_primary_modules.end(); ++it){
        std::cout << "'"<< it->first << "':\n\t" << it->second ;
        std::cout << ",\n";
    }
    std::cout << "}";
}

bool startup_extractor::load_startup_script(const std::string script_file){
    lk::input_string p( script_file );
    lk::parser parse( p );
    lk::node_t *tree = parse.script();

    if ( parse.error_count() != 0
         || parse.token() != lk::lexer::END)
    {
        for( int i=0;i<parse.error_count();i++ )
            errors.push_back( parse.error(i).ToStdString());
        errors.push_back( "parsing did not reach end of input" );
        return false;
    }
    else
    {
        lk::env_t env;
        env.register_funcs( lk::stdlib_basic() );
        env.register_funcs( lk::stdlib_sysio() );
        env.register_funcs( lk::stdlib_math() );
        env.register_funcs( lk::stdlib_string() );
        env.register_funcs( export_config_funcs(), 0 );

        lk::eval e( tree, &env );
        bool ok = e.run();
        if ( tree ) delete tree;

        if ( !ok )
            for( size_t i=0;i<e.error_count();i++ )
                errors.push_back( e.get_error(i).ToStdString());

        return ok;
    }
}

std::vector<std::string> startup_extractor::get_unique_ui_forms() {
    std::vector<std::string> unique_ui_forms;
    for (auto it = SAM_config_to_input_pages.begin(); it != SAM_config_to_input_pages.end(); ++it){
        std::vector<page_info> pg_info_vec = it->second;
        for (size_t i = 0; i < pg_info_vec.size(); i++){
            for (size_t n = 0; n < pg_info_vec[i].common_uiforms.size(); n++){
                std::string ui_name = pg_info_vec[i].common_uiforms[n];
                if (std::find(unique_ui_forms.begin(), unique_ui_forms.end(), ui_name) == unique_ui_forms.end())
                    unique_ui_forms.push_back(ui_name);
            }
            for (size_t n = 0; n < pg_info_vec[i].exclusive_uiforms.size(); n++){
                std::string ui_name = pg_info_vec[i].exclusive_uiforms[n];
                if (std::find(unique_ui_forms.begin(), unique_ui_forms.end(), ui_name) == unique_ui_forms.end())
                    unique_ui_forms.push_back(ui_name);
            }
        }
    }
    return unique_ui_forms;
}
