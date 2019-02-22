#include "startup_extractor.h"

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

#include "startup_extractor.h"

// print into dictionary format
void startup_extractor::print_config_to_input(){
    std::cout << "config_to_input_map = {\n";
    // configuration : {
    for (auto it = SAM_config_to_input_pages.begin(); it != SAM_config_to_input_pages.end(); ++it){
        if (it != SAM_config_to_input_pages.begin()) std::cout << ",\n";
        std::cout << "'" << it->first << "' :{\n\t";
        // group : { 'common' : (common_uiforms)
        for (size_t i = 0; i < it->second.size(); i++){
            std::cout << "'"<< it->second[i].sidebar_title << "' :{\n\t\t";
            if (it->second[i].common_uiforms.size() > 0 ){
                std::cout << "'common': \n\t\t\t";
                std::cout << it->second[i].common_uiforms;
            }
            // {exlusive_var : (exclusive_uiforms)
            if (it->second[i].exclusive_var.size() > 0 ){
                if (it->second[i].common_uiforms.size() > 0)
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
    for (auto it = SAM_config_to_modules.begin(); it != SAM_config_to_modules.end(); ++it){
        std::cout << "'"<< it->first << "':\n\t" << it->second ;
        std::cout << ",\n";
    }
    std::cout << "}";
}

bool startup_extractor::load_startup_script(const std::string script_file, std::vector<std::string>* errors){
    lk::input_string p( script_file );
    lk::parser parse( p );
    lk::node_t *tree = parse.script();

    if ( parse.error_count() != 0
         || parse.token() != lk::lexer::END)
    {
        if ( errors )
        {
            for( int i=0;i<parse.error_count();i++ )
                errors->push_back( parse.error(i) );
            errors->push_back( "parsing did not reach end of input" );
        }
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

        if ( !ok && errors )
            for( size_t i=0;i<e.error_count();i++ )
                errors->push_back( e.get_error(i) );

        config_to_input_pages = SAM_config_to_input_pages;
        config_to_modules = SAM_config_to_modules;
        return ok;
    }
}
