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

#include <vector>
#include <string>
#include <limits>

#include "builder_generator_helper.h"
#include "config_extractor.h"

std::string find_module_of_var(std::string var, std::string cmod){
    std::vector<std::string> extra_groups = cmod_to_extra_modules[cmod];
    if (extra_groups.size() == 0){
        return "";
    }
    for (size_t g = 0; g < extra_groups.size(); g++){
        std::vector<std::string> group_members = extra_modules_to_members[extra_groups[g]];
        if (std::find(group_members.begin(), group_members.end(), var) != group_members.end())
            return extra_groups[g];
    }
    return "";
}

int get_ssc_type(vertex* v, std::unordered_map<std::string, ssc_module_t> &mod_map){
    int ind = SAM_cmod_to_ssc_index[v->cmod][v->name];
    ssc_info_t info = ssc_module_var_info(mod_map[v->cmod], ind);
    return ssc_info_data_type(info);
}

int get_varvalue_type(std::string name, std::string& config){

    VarValue* vv = find_default_from_ui(name, config);
    if (!vv){
        VarTable* vt = &SAM_config_to_defaults[config];
        if ( VarValue *vv2 = vt->Get( name ) )
            vv = vv2;
    }
    if (!vv){
        return 0;
    }
    return vv->Type();

}

std::string print_parameter_type(vertex *v, std::string cmod,
                                        std::unordered_map<std::string, ssc_module_t> &module_map) {

    int ind = (int)SAM_cmod_to_ssc_index[cmod][v->name];
    ssc_info_t mod_info = ssc_module_var_info(module_map[cmod], ind);
    assert(mod_info);

    switch(ssc_info_data_type(mod_info)) {
        case SSC_STRING:
            return "const char* string";
        case SSC_MATRIX:
            return "float* matrix, int nr, int nc";
        case SSC_ARRAY:
            return "float* array, int length";
        case SSC_NUMBER:
            return "float number";
        case SSC_TABLE:
            return "var_table vt";
        default:
            return "ERROR";
    }
}

std::string print_return_type(vertex *v, std::string cmod,
                                     std::unordered_map<std::string, ssc_module_t> &module_map) {

    int ind = (int)SAM_cmod_to_ssc_index[cmod][v->name];
    ssc_info_t mod_info = ssc_module_var_info(module_map[cmod], ind);
    assert(mod_info);

    switch(ssc_info_data_type(mod_info)) {
        case SSC_STRING:
            return "const char*";
        case SSC_MATRIX:
            return "float*";
        case SSC_ARRAY:
            return "float*";
        case SSC_NUMBER:
            return "float";
        case SSC_TABLE:
            return "var_table";
        default:
            return "ERROR";
    }
}


void export_function_declaration(std::ofstream& ff, std::string return_type, std::string name,
                                        std::vector<std::string> inputs){

    ff << "\tSAM_EXPORT " << return_type << " " << name << "(";
    for (size_t i = 0; i < inputs.size(); i++){
        ff << inputs[i] << ", ";
    }
    ff << "SAM_error* err);\n\n";
}

std::string spell_type(int type){
    switch(type){
        case SSC_INVALID:
            return "invalid";
        case SSC_STRING:
            return "string";
        case SSC_NUMBER:
            return "numeric";
        case SSC_ARRAY:
            return "array";
        case SSC_MATRIX:
            return "matrix";
        case SSC_TABLE:
            return "table";
        default:
            return "";
    }
}

std::string var_value_to_json(VarValue* vv){
    assert(vv);
    std::string json;
    switch(vv->Type()){
        case VV_INVALID:
            json += "\"invalid\"";
            break;
        case VV_STRING:
        case VV_NUMBER:
            json += "\"" + vv->AsString() + "\"";
            break;
        case VV_ARRAY:{
            json += "[";
            std::vector<double> vec = vv->Array();
            for (size_t j = 0; j < vec.size(); j++){
                json += "\"" + std::to_string(vec[j]) + "\"";
                if (j != vec.size() - 1) json += ", ";
            }
            json += "]";
        }
            break;
        case VV_TABLE:{
            json += "{\n";

            VarTable vt = vv->Table();
            std::string buf = "";
            size_t i = 0;
            for (VarTable::iterator it = vt.begin(); it != vt.end(); ++it)
            {
                buf += "\"" + (it->first) + "\": " + var_value_to_json(it->second);
                if ( ++i < (vt.size())) buf += ",\n";
            }
            json += indent(buf, 6);
            json += "}";
        }
            break;
        default:
            json += "\"null\"";
    }
    return json;
}

std::string ssc_value_to_json(int ssc_type, VarValue* vv){

    std::string json;
    switch(ssc_type){
        case SSC_INVALID:
            json += "\"invalid\"";
            break;
        case SSC_STRING:
            json += "\"" + (vv? vv->AsString() : "" ) + "\"";
            break;
        case SSC_NUMBER:
            if (vv && vv->Value() > std::numeric_limits<double>::max()){
                char c[36];
                sprintf(c, "%e", std::numeric_limits<double>::max());
                json += c;
            }
            else
                json += (vv? vv->AsString() : "0" );
            break;
        case SSC_ARRAY:
            json += "[";
            if (vv){
                std::vector<double> vec = vv->Array();
                for (size_t j = 0; j < vec.size(); j++){
                    if (vec[j] > std::numeric_limits<double>::max()){
                        char c[36];
                        sprintf(c, "%e", std::numeric_limits<double>::max());
                        json += c;
                    }
                    else
                        json += std::to_string(vec[j]);
                    if (j != vec.size() - 1) json += ", ";
                }
            } else
                json += "0";
            json += "]";
            break;
        case SSC_MATRIX:
            json += "[\n";
            if (vv){
                matrix_t<double> mat = vv->Matrix();
                for (size_t i = 0; i < mat.nrows(); i++){
                    json += "\t\t\t\t[";
                    for (size_t j = 0; j < mat.ncols(); j++){
                        if (mat.at(i, j) > std::numeric_limits<double>::max()){
                            char c[36];
                            sprintf(c, "%e", std::numeric_limits<double>::max());
                            json += c;
                        }
                        else
                            json += std::to_string(mat.at(i, j));
                        if (j != mat.ncols() - 1) json += ", ";
                    }
                    json += "]";
                    if (i != mat.nrows() - 1) json += ",";
                    json += "\n";
                }
            } else
                json += "[0]";
            json += "\t\t\t]";
            break;
        case SSC_TABLE:
            json += "{\n";
            if (vv){
                VarTable vt = vv->Table();
                std::string buf = "";
                size_t i = 0;
                for (VarTable::iterator it = vt.begin(); it != vt.end(); ++it)
                {
                    buf += "\"" + (it->first) + "\": " + var_value_to_json(it->second);
                    if ( ++i < (vt.size())) buf += ",\n";
                }
                json += indent(buf,5);
                json += "}";
            }
            else{
                json += "\t\t\t\t}";
            }
            break;
        default:
            json += "null";
    }
    return json;
}


std::string translate_lookup_type(std::string name, std::string config){

    std::vector<std::string> typestr_core = {"void", "float", "util::matrix_t<ssc_number_t>"
            , "util::matrix_t<ssc_number_t>", "const char*", "var_table", "binary"};

    std::string result;

    int type = get_varvalue_type(name, config);

    if (type == 0)
        return "undefined " + name;

    result += typestr_core[type] + " " + name + " = vt->lookup(\"" + name + "\")->";

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

std::vector<std::string> typestr_core = {"void", "float", "util::matrix_t<ssc_number_t>"
        , "util::matrix_t<ssc_number_t>", "const char*", "var_table", "binary"};

bool translate_equation_to_cplusplus(config_extractor *config_ext, equation_info &eqn_info, std::ofstream &of,
                                     const std::string &cmod) {

    lk::env_t* env = config_ext->get_env();
    std::string config = config_ext->get_name();

    translator e( eqn_info.eqn_data->tree, env );
    e.set_ui_source(eqn_info.ui_source);

    size_t n_outputs = eqn_info.all_outputs.size();


    // figure out the return value of the eqn by whether its single or multiple outputs
    std::string output_name;                            // return type
    std::string subhandle = eqn_info.all_outputs[0];      // part of function name
    int type;
    if (n_outputs == 1){
        output_name = subhandle;
        type = find_default_from_ui(output_name, active_config)->Type();
    }
    else{
        // return MIMOs as table
        output_name = "vt";
        type = 5;
        subhandle += "_MIMO";
    }

    // save & print function signature
    std::string sig;
    sig += typestr_core[type] + " ";
    sig += format_as_symbol(cmod) + "_"
            + subhandle + "_eval(var_table* vt)";
    of << sig << "\n{\n";


    // set up inputs and outputs variable placeholders
    of << "\t// inputs\n";
    for (size_t i = 0; i < eqn_info.all_inputs.size(); i++){
        std::string name = eqn_info.all_inputs[i];
        of << "\t" << translate_lookup_type(name, config) << "\n";
    }
    of << "\n\t// outputs\n";
    for (size_t i = 0; i < eqn_info.all_outputs.size(); i++){
        std::string name = eqn_info.all_outputs[i];
        VarValue* vv = find_default_from_ui(name, config);
        of << "\t" << typestr_core[vv->Type()] << " " + name << ";\n";
    }
    of << "\n";

    // translate the equation
    std::string result;
    unsigned int ctl = 0;
    bool ok = e.translate(eqn_info.eqn_data->tree, env, result, 0, ctl, output_name);
    if ( !ok ){
        for( size_t i=0;i<e.error_count();i++ ){
            std::cout << "equation_extractor::equation_to_cplusplus error " << e.get_error(i) << "\n";
            config_ext->get_errors().push_back( e.get_error(i).ToStdString());
        }
        return false;
    }


    // print lambda functions and the enclosing function
    auto aux_fx = e.get_aux_functions();
    for (auto it = aux_fx.begin(); it != aux_fx.end(); ++it){
        of << indent(it->second) << "\n";
    }
    of << indent(result) << "\n\n";


    // check if function has already returned, then end definition now
    if (result.find("return") != std::string::npos){
        of << "}\n\n";
        return true;
    }



    // set up return argument: either a single return value or a table
    if (n_outputs > 1){
        of << "\tvar_table vt;\n";
        output_name = "vt";
        for (size_t i = 0; i < n_outputs; i++){
            std::string var = eqn_info.all_outputs[i];

//            int type = find_default_from_ui(var, config)->Type();

            of << "\tvt.assign( \"" << var << "\", "<< var << " );\n";
        }
    }
    else{
        of << "\treturn " << output_name << ";\n";
    }

    of << "\n}\n\n";

    // cache completed functions
    config_ext->completed_equation_signatures.insert({&eqn_info, sig});

    return true;
}

bool translate_callback_to_cplusplus(config_extractor *config_ext, callback_info &cb_info,
                                                std::ofstream &of, const std::string &cmod) {

    std::string config = config_ext->get_name();
    lk::env_t* env = config_ext->get_env();

    lk::vardata_t *cbvar = env->lookup( cb_info.method_name, true);

    lk::varhash_t* h = cbvar->hash();

    auto it = h->find(cb_info.function_name);
    assert(it != h->end());

    // get the # of outputs to figure out if returning a single value or a var_table
    size_t n_outputs = cb_info.all_outputs.size();

    std::string subhandle = cb_info.function_name;
    int type = -1;
    if (n_outputs == 1){
        VarValue* vv = find_default_from_ui(cb_info.all_outputs[0], config);
        if (vv)
            type = vv->Type();
        else{
            std::cout << "translate_callback_to_cpluscplus::warning:: single output " << cb_info.all_outputs[0];
            std::cout << " not found, for function: " << cb_info.function_name << " in ui " << cb_info.ui_source << "\n";
            return false;
        }
    }
    else{
        // return multiple values as table
        type = 5;
    }

    // save & print function signature
    std::string sig;
    sig += typestr_core[type] + " ";
    sig += format_as_symbol(cmod) + "_" + format_as_symbol(cb_info.ui_source) + "_"
            + format_as_symbol(subhandle) + "_func(var_table* vt)";
    of << sig << "\n{\n";


    // set up inputs and outputs variable placeholders
    of << "\t// inputs\n";
    for (size_t i = 0; i < cb_info.ssc_only_inputs.size(); i++){
        std::string name = cb_info.ssc_only_inputs[i];
        of << "\t" << translate_lookup_type(name, config) << "\n";
    }
    for (size_t i = 0; i < cb_info.ui_only_inputs.size(); i++){
        std::string name = cb_info.ui_only_inputs[i];
        of << "\t" << translate_lookup_type(name, config) << "\n";
    }
    of << "\n\t// outputs\n";
    for (size_t i = 0; i < cb_info.all_outputs.size(); i++){
        std::string name = cb_info.all_outputs[i];
        VarValue* vv = find_default_from_ui(name, config);
        if (vv)     // if it's not a local variable
            of << "\t" << typestr_core[vv->Type()] << " " + name << ";\n";
    }
    of << "\n";


    // translate the callback

    lk::expr_t *p_define = it->second->deref().func();

    translator e = translator(p_define->right, env);
    e.set_ui_source(cb_info.ui_source);

    std::string result, output_name;
    unsigned int ctl = 0;
    bool ok = e.translate(p_define, env, result, 0, ctl, output_name);
    of << result << "\n\n";
    if ( !ok ){
        for( size_t i=0;i<e.error_count();i++ ){
            std::cout << "equation_extractor::equation_to_cplusplus error " << e.get_error(i).ToStdString() << "\n";
            config_ext->get_errors().push_back( e.get_error(i).ToStdString());
        }
        return false;
    }

    config_ext->completed_callback_signatures.insert({&cb_info, sig});
    return true;
}

void print_var_info_table(const std::string &config_name, const std::string &filepath) {
    // print new ssc var_info table

    std::vector<page_info>& pg_info = SAM_config_to_input_pages.find(active_config)->second;

    std::unordered_map<std::string, std::unordered_map<std::string, bool>> module_to_variables;

    for (size_t p = 0; p < pg_info.size(); p++){
        if (pg_info[p].common_uiforms.size() > 0){
            // add the ui form variables into a group based on the sidebar title
            std::string group_name = pg_info[p].sidebar_title
                                     + (pg_info[p].exclusive_uiforms.size() > 0 ? "Common" : "");
            std::unordered_map<std::string, bool> map;
            module_to_variables.insert({group_name, map});

            auto* var_map = &(module_to_variables.find(group_name)->second);

            for (size_t i = 0; i < pg_info[p].common_uiforms.size(); i++) {
                // add all the variables and associate their VarValue with the vertex
                std::string ui_name = pg_info[p].common_uiforms[i];
                std::unordered_map<std::string, VarValue> ui_def = SAM_ui_form_to_defaults[ui_name];
                for (auto it = ui_def.begin(); it != ui_def.end(); ++it){
                    std::string var_name = it->first;

                    var_map->insert({var_name, true});
                }

            }
        }

        // add each exclusive form as its own (sub)module
        for (size_t i = 0; i < pg_info[p].exclusive_uiforms.size(); i++) {
            // add all the variables and associate their VarValue with the vertex
            std::string ui_name = pg_info[p].exclusive_uiforms[i];

            std::string submod_name = ui_name;
            module_to_variables.insert({submod_name, std::unordered_map<std::string, bool>()});

            auto& var_map = module_to_variables.find(submod_name)->second;

            std::unordered_map<std::string, VarValue> ui_def = SAM_ui_form_to_defaults[ui_name];
            for (auto it = ui_def.begin(); it != ui_def.end(); ++it){
                std::string var_name = it->first;

                var_map.insert({var_name, true});
            }
        }
    }


    auto cmods = SAM_config_to_primary_modules[config_name];


    for (size_t i = 0; i < cmods.size(); i++){
        std::string cmod_name = cmods[i];

        std::ofstream var_info_fx;
        var_info_fx.open(filepath + "/varinfo/" + config_name+ "_" + cmod_name + ".cpp");
        assert(var_info_fx.is_open());

        var_info_fx << "static var_info _cm_vtab_" + cmod_name + "[] = {\n"
                                                                 "\t// VARTYPE\tDATATYPE\tNAME\tLABEL\tUNITS\tMETA\tGROUP\tREQUIRED_IF\tCONSTRAINTS\tUI_HINTS\n";


        ssc_module_t p_mod = ssc_module_create(const_cast<char*>(cmod_name.c_str()));
        std::vector<std::string> variable_names;


        SAM_cmod_to_ssc_index.insert({cmod_name, std::unordered_map<std::string, size_t>()});
        std::unordered_map<std::string, size_t>& index_map = SAM_cmod_to_ssc_index.find(cmod_name)->second;

        int var_index = 0;
        ssc_info_t mod_info = ssc_module_var_info(p_mod, var_index);
        while (mod_info){
            int var_type = ssc_info_var_type(mod_info);
            int data_type = ssc_info_data_type(mod_info);
            std::string name = ssc_info_name(mod_info);
            std::string label = ssc_info_label(mod_info);
            std::string units = ssc_info_units(mod_info);
            std::string meta = ssc_info_meta(mod_info);
            std::string group, uihints;

            for (auto it = module_to_variables.begin(); it != module_to_variables.end(); ++it){
                auto var_it = it->second.find(name);
                if (var_it != it->second.end())
                    group = it->first;
            }

            std::string required_if = ssc_info_required(mod_info);
            std::string constraints = ssc_info_constraints(mod_info);
            if (ssc_info_uihint(mod_info))
                uihints = ssc_info_uihint(mod_info);

            std::vector<std::string> var_str = {"", "SSC_INPUT", "SSC_OUTPUT", "SSCINOUT"};
            std::vector<std::string> data_str= {"SSC_INVALID", "SSC_STRING", "SSC_NUMBER", "SSC_ARRAY", "SSC_MATRIX", "SSC_TABLE"};

            var_info_fx << "{ \t" << var_str[var_type] << ", \t" << data_str[data_type]<< ", \t\"" << name << "\", \t\"" << label << "\", \t\"";
            var_info_fx << units << "\", \t\"" << meta << "\", \t\"" << group << "\", \t\"" << required_if;
            var_info_fx << "\", \t\"" << constraints << "\", \t\"" << uihints << "\"},\n";


            ++var_index;
            mod_info = ssc_module_var_info(p_mod, var_index);
        }
        var_info_fx << "var_info_invalid};";
        var_info_fx.close();
    }

}
