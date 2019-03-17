#ifndef SYSTEM_ADVISOR_MODEL_BUILDER_GENERATOR_HELPER_H
#define SYSTEM_ADVISOR_MODEL_BUILDER_GENERATOR_HELPER_H

#include <map>
#include <vector>
#include <string>
#include <algorithm>

#include "data_structures.h"
#include "variable_graph.h"
#include <ssc/sscapi.h>

static std::map<std::string, std::vector<std::string>> cmod_to_extra_modules = {
        {"generic", {""}}
};

static std::map<std::string, std::vector<std::string>> extra_modules_to_members = {
        {"adjust", {"constant", "hourly", "periods"}},
        {"dc_adjust", {"constant", "hourly", "periods"}}
};

static std::string find_module_of_var(std::string var, std::string cmod){
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

int get_ssc_type(vertex* v, std::unordered_map<std::string, ssc_module_t>& mod_map){
    int ind = SAM_cmod_to_ssc_index[v->cmod][v->name];
    ssc_info_t info = ssc_module_var_info(mod_map[v->cmod], ind);
    return ssc_info_data_type(info);
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

#endif //SYSTEM_ADVISOR_MODEL_BUILDER_GENERATOR_INFO_H
