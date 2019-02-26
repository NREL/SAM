#include <iostream>
#include <algorithm>

#include <lk/absyn.h>
#include <lk/parse.h>
#include <lk/eval.h>
#include <lk/stdlib.h>

#include "equation_extractor.h"
#include "lk_env.h"

std::unordered_map<std::string, std::vector<equation_info>> SAM_ui_form_to_eqn_info;
std::unordered_map<std::string, std::vector<secondary_cmod_info>> SAM_ui_form_to_secondary_cmod_info;

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
        ei_vec.push_back(ei);
    }
    if (ei_vec.size() > 0)
        SAM_ui_form_to_eqn_info.insert({ui_form_name, ei_vec});
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

        m_cbenv.register_funcs( lk::stdlib_basic() );
        m_cbenv.register_funcs( lk::stdlib_sysio() );
        m_cbenv.register_funcs( lk::stdlib_math() );
        m_cbenv.register_funcs( lk::stdlib_string() );
        m_cbenv.register_funcs( invoke_ssc_funcs() );
        m_cbenv.register_funcs( invoke_casecallback_funcs() );


        lk::eval e( tree, &m_cbenv );

        bool ok = e.run();
        if ( !ok )
            for( size_t i=0;i<e.error_count();i++ )
                errors.push_back( e.get_error(i) );

        lk::node_t *cb = parse_functions("on_change");
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
}

lk::node_t * callback_extractor::parse_functions(const std::string &method_name){
    lk::vardata_t *cbvar = m_cbenv.lookup( method_name, true);

    if (!cbvar || cbvar->type() != lk::vardata_t::HASH )
    {
        //wxLogStatus("ScriptDatabase::Invoke: could not find " + method_name + " variable or not a hash");
        return 0;
    }

    // get all instances of method
    lk::varhash_t* h = cbvar->hash();
    for (auto it = h->begin(); it != h->end(); it++){
        std::string obj_name = it->first.ToStdString();
        lk::expr_t *p_define = it->second->deref().func();
        if ( p_define->oper != lk::expr_t::DEFINE )
        {
            std::cout << "ScriptDatabase::Invoke: improper function structure, must be a 'define' " << it->first;
            return 0;
        }

        if ( p_define->right == 0 )
        {
            std::cout << "ScriptDatabase::Invoke: function block nonexistent for " << it->first ;
            return 0;
        }

        if (!invoke_function(p_define->right, obj_name))
            std::cout << "extract_function error: " << errors;

        return p_define->right;

    }
}

bool callback_extractor::invoke_function(lk::node_t *root, std::string f_name) {


    try {
        lk::eval e( root, &m_cbenv );
        if ( !e.run() )
        {

            for (size_t i=0;i<e.error_count();i++)
                errors.push_back(e.get_error(i));

            return false;
        }
        return true;

    } catch(std::exception &e ){
        std::cout << f_name << " invoke_function exception\n";
        return false;
    }

}

void callback_extractor::export_to_secondary_cmod_info() {
    secondary_cmod_info scmi;


//    SAM_ui_form_to_secondary_cmod_info[ui_form_name] = scmi;
}