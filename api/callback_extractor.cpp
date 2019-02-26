#include <iostream>
#include <algorithm>

#include <lk/absyn.h>
#include <lk/parse.h>
#include <lk/eval.h>
#include <lk/stdlib.h>

#include "callback_extractor.h"
#include "lk_env.h"

size_t callback_extractor::parse_cmod_statement(std::string callback_script, size_t pos_start){
    size_t start = callback_script.find_first_of("\'\"", pos_start);
    size_t end = callback_script.find_first_of("\'\"", start+1);
    if (start >= end){
        std::cout << "Error extracting cmod name from callback script for " << config_name << "\n";
        return 0;
    }
    std::string str = callback_script.substr(start+1, (end-start-1));

    // remove white space & quotations
    std::string::iterator end_pos = std::remove(str.begin(), str.end(), ' ');
    str.erase(end_pos, str.end());

    //compute_modules.push_back(str);
    return end;
}


bool callback_extractor::parse_script(std::string callback_script) {
    lk::input_string in( callback_script );
    lk::parser parse( in );
    lk::node_t *tree = parse.script();

    if ( parse.error_count() != 0 || parse.token() != lk::lexer::END){
        for( int i=0;i<parse.error_count();i++ )
            errors.push_back( parse.error(i) );
        errors.push_back( "callback script parsing did not reach end of input" );
        if ( tree ) delete tree;
        return false;
    }
    else
    {
        lk::eval e( tree, m_env );

        bool ok = e.run();
        if ( !ok ){
            for( size_t i=0;i<e.error_count();i++ )
                errors.push_back( e.get_error(i) );
            return false;
        }

    }
    return true;
}

int callback_extractor::invoke_method_type(const std::string &method_name) {
    lk::vardata_t *cbvar = m_env->lookup( method_name, true);

    if (!cbvar || cbvar->type() != lk::vardata_t::HASH )
    {
        std::cout << "callback_extractor::invoke_method_type: could not find " + method_name + " in " + config_name << "\n";
        return 0;
    }

    // get all instances of method
    int error = 0;

    lk::varhash_t* h = cbvar->hash();
    for (auto it = h->begin(); it != h->end(); ++it){

        std::string obj_name = it->first.ToStdString();
        lk::expr_t *p_define = it->second->deref().func();
        if ( p_define->oper != lk::expr_t::DEFINE )
        {
            std::cout << "callback_extractor::invoke_method_type: structure of " + obj_name;
            std::cout << " not " + method_name + " in " + config_name << "\n";
            return 0;
        }

        if ( p_define->right == 0 )
        {
            std::cout << "callback_extractor::invoke_method_type: block undefined for" + obj_name;
            std::cout << " not " + method_name + " in " + config_name << "\n";
            return 0;
        }

        error += (int)invoke_function(p_define->right, obj_name);

    }
    return error;
}

bool callback_extractor::invoke_function(lk::node_t *root, std::string f_name) {

    try {
        lk::eval e( root, m_env );
        if ( !e.run() )
        {
            for (size_t i=0;i<e.error_count();i++)
                errors.push_back(e.get_error(i));
            return false;
        }
        return true;

    } catch(std::exception &e ){
        std::cout << "callback_extractor::invoke_function error: could not invoke "<< f_name << " for " << config_name << "\n";
        return false;
    }

}

bool callback_extractor::extract_functions() {
    invoke_method_type("on_change");
//        for( size_t i=0;i<cases.size();i++ )
//        {
//            Invoke( cases[i], pf.GetCaseName( cases[i] ), cb );
//
//            // recalculate equations in each case for each consecutive upgrade
//            // to make sure all variables are in sync
//            if ( cases[i]->RecalculateAll( true ) < 0 )
//                GetLog().push_back( log( WARNING, "Error updating calculated values in '" + pf.GetCaseName(cases[i]) + "' during upgrade process.  Please resolve any errors, save the project file, and reopen it." ) );
//        }
}