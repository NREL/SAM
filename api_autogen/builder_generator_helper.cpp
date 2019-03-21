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
            std::vector<float> vec = vv->Array();
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
        case SSC_NUMBER:
            json += "\"" + (vv? vv->AsString() : "0" ) + "\"";
            break;
        case SSC_ARRAY:
            json += "[";
            if (vv){
                std::vector<float> vec = vv->Array();
                for (size_t j = 0; j < vec.size(); j++){
                    json += "\"" + std::to_string(vec[j]) + "\"";
                    if (j != vec.size() - 1) json += ", ";
                }
            } else
                json += "\"0\"";
            json += "]";
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
            json += "\"null\"";
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
            config_ext->get_errors().push_back( e.get_error(i) );
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
            std::cout << "equation_extractor::equation_to_cplusplus error " << e.get_error(i) << "\n";
            config_ext->get_errors().push_back( e.get_error(i) );
        }
        return false;
    }

    config_ext->completed_callback_signatures.insert({&cb_info, sig});
}