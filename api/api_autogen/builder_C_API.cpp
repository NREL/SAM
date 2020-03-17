#include <shared/lib_util.h>

#include "builder_C_API.h"
#include "builder_generator_helper.h"

void builder_C_API::create_SAM_headers(const std::string &cmod, const std::string &file_dir) {
    std::string cmod_symbol = format_as_symbol(cmod);

    std::ofstream fx_file;
    fx_file.open(file_dir + "/SAM_" + cmod_symbol + ".h");
    assert(fx_file.is_open());

    fx_file << "#ifndef SAM_" << util::upper_case(cmod_symbol)<< "_H_\n"
               "#define SAM_" << util::upper_case(cmod_symbol)<< "_H_\n"
               "\n"
               "#include \"visibility.h\"\n"
               "#include \"SAM_api.h\"\n"
               "\n"
               "\n"
               "#include <stdint.h>\n"
               "#ifdef __cplusplus\n"
               "extern \"C\"\n"
               "{\n"
               "#endif\n"
               "\n"
               "\t//\n"
               "\t// "<< cmod_symbol << " Technology Model\n"
               "\t//\n\n";

    // create the module-specific var_table wrapper, constructor, exec and destructor

    fx_file << "\t/** \n";
    fx_file << "\t * Create a " << cmod_symbol << " variable table.\n";
    fx_file << "\t * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)\n";
    fx_file << "\t * @param[in,out] err: a pointer to an error object\n";
    fx_file << "\t */\n\n";

    fx_file << "\tSAM_EXPORT typedef void * SAM_" << cmod_symbol << ";\n\n";

    fx_file << "\tSAM_EXPORT SAM_" << cmod_symbol << " SAM_" << cmod_symbol<< "_construct(const char* def, SAM_error* err);\n"
               "\n"
               "\t/// verbosity level 0 or 1. Returns 1 on success\n"
               "\tSAM_EXPORT int SAM_" << cmod_symbol  << "_execute(SAM_" <<cmod_symbol << " data, int verbosity, SAM_error* err);\n"
               "\n"
               "\tSAM_EXPORT void SAM_" <<cmod_symbol << "_destruct(SAM_" << cmod_symbol << " system);\n\n";

    // start ssc variables

    // setters, none for outputs
    for (size_t i = 0; i < root->vardefs_order.size() - 1; i++) {
        auto mm = root->m_vardefs.find(root->vardefs_order[i]);
        std::map<std::string, var_def> vardefs = mm->second;

        if (mm->first == "AdjustmentFactors")
            continue;

        std::string module_symbol = format_as_symbol(mm->first);

        fx_file << "\n"
                   "\t//\n"
                   "\t// " << module_symbol << " parameters\n"
                   "\t//\n\n";
        for (auto it = vardefs.begin(); it != vardefs.end(); ++it) {
            std::string var_symbol = it->first;

            var_def vd = it->second;
            std::string var_name = vd.name;

            fx_file << "\t/**\n";
            fx_file << "\t * Set " << var_name << ": " << vd.doc << "\n";
            fx_file << "\t * options: ";
            if (vd.meta.length() > 0)
                fx_file << vd.meta << "\n";
            else
                fx_file << "None\n";

            fx_file << "\t * constraints: ";
            if (vd.constraints.length() > 0)
                fx_file << vd.constraints << "\n";
            else
                fx_file << "None\n";

            fx_file << "\t * required if: ";
            if (vd.reqif.length() > 0)
                fx_file << vd.reqif << "\n";
            else
                fx_file << "None\n";
            fx_file << "\t */\n";

            // 	SAM_EXPORT void SAM_GenericSystem_PowerPlant_derate_nset(SAM_GenericSystem ptr, double number, SAM_error *err);

            fx_file << "\tSAM_EXPORT void SAM_" << cmod_symbol << "_" << module_symbol << "_" << var_symbol << "_";
            if (vd.type == "number") {
                fx_file << "nset(SAM_" << cmod_symbol << " ptr, double number, SAM_error *err);\n\n";
            } else if (vd.type == "string") {
                fx_file << "sset(SAM_" << cmod_symbol << " ptr, const char* str, SAM_error *err);\n\n";
            } else if (vd.type == "array") {
                fx_file << "aset(SAM_" << cmod_symbol << " ptr, double* arr, int length, SAM_error *err);\n\n";
            } else if (vd.type == "matrix") {
                fx_file << "mset(SAM_" << cmod_symbol << " ptr, double* mat, int nrows, int ncols, SAM_error *err);\n\n";
            } else if (vd.type == "table") {
                fx_file << "tset(SAM_" << cmod_symbol << " ptr, SAM_table tab, SAM_error *err);\n\n";
            } else {
                throw std::runtime_error(vd.type + " for " + var_name);
            }
        }
    }
    for (size_t i = 0; i < root->vardefs_order.size(); i++) {
        auto mm = root->m_vardefs.find(root->vardefs_order[i]);
        std::map<std::string, var_def> vardefs = mm->second;
        std::string module_symbol = format_as_symbol(mm->first);

        if (mm->first == "AdjustmentFactors")
            continue;

        // getters
        fx_file << "\n\t/**\n";
        fx_file << "\t * " << module_symbol << " Getters\n\t */\n\n";

        for (auto it = vardefs.begin(); it != vardefs.end(); ++it){
            std::string var_symbol = it->first;

            var_def vd = it->second;
            std::string var_name = vd.name;

            if (vd.type == "number"){
                fx_file << "\tSAM_EXPORT double SAM_" << cmod_symbol << "_" << module_symbol << "_" << var_symbol << "_";
                fx_file << "nget(SAM_" << cmod_symbol << " ptr, SAM_error *err);\n\n";
            }
            else if (vd.type == "string"){
                fx_file << "\tSAM_EXPORT const char* SAM_" << cmod_symbol << "_" << module_symbol << "_" << var_symbol << "_";
                fx_file << "sget(SAM_" << cmod_symbol << " ptr, SAM_error *err);\n\n";
            }
            else if (vd.type == "array"){
                fx_file << "\tSAM_EXPORT double* SAM_" << cmod_symbol << "_" << module_symbol << "_" << var_symbol << "_";
                fx_file << "aget(SAM_" << cmod_symbol << " ptr, int* length, SAM_error *err);\n\n";
            }
            else if (vd.type == "matrix"){
                fx_file << "\tSAM_EXPORT double* SAM_" << cmod_symbol << "_" << module_symbol << "_" << var_symbol << "_";
                fx_file << "mget(SAM_" << cmod_symbol << " ptr, int* nrows, int* ncols, SAM_error *err);\n\n";
            }
            else if (vd.type == "table"){
                fx_file << "\tSAM_EXPORT SAM_table SAM_" << cmod_symbol << "_" << module_symbol << "_" << var_symbol << "_";
                fx_file << "tget(SAM_" << cmod_symbol << " ptr, SAM_error *err);\n\n";
            }
            else{
                throw std::runtime_error(vd.type + " for " + var_name);
            }
        }
    }
    fx_file << "#ifdef __cplusplus\n"
               "} /* end of extern \"C\" { */\n"
               "#endif\n"
               "\n"
               "#endif";
    fx_file.close();
}

void builder_C_API::create_SAM_definitions(const std::string &cmod, const std::string &file_dir) {
    std::string cmod_symbol = format_as_symbol(cmod);

    std::ofstream fx_file;
    fx_file.open(file_dir + "/SAM_" + cmod_symbol + ".cpp");
    assert(fx_file.is_open());

    fx_file << "#include <string>\n"
               "#include <utility>\n"
               "#include <vector>\n"
               "#include <memory>\n"
               "#include <iostream>\n"
               "\n"
               "#include <ssc/sscapi.h>\n"
               "\n"
               "#include \"SAM_api.h\"\n"
               "#include \"ErrorHandler.h\"\n"
               "#include \"SAM_" << cmod_symbol << ".h\"\n\n";

    fx_file << "SAM_EXPORT SAM_" << cmod_symbol << " SAM_" << cmod_symbol << "_construct(const char* def, SAM_error* err){\n"
               "\tSAM_" << cmod_symbol << " result = nullptr;\n"
               "\ttranslateExceptions(err, [&]{\n"
               "\t\tresult = ssc_data_create();\n"
               "\t});\n"
               "\treturn result;\n"
               "}\n"
               "\n"
               "SAM_EXPORT int SAM_" << cmod_symbol << "_execute(SAM_" << cmod_symbol << " data, int verbosity, SAM_error* err){\n"
               "\tint n_err = 0;\n"
               "\ttranslateExceptions(err, [&]{\n"
               "\t\tn_err += SAM_module_exec(\"" << cmod << "\", data, verbosity, err);\n"
               "\t});\n"
               "\treturn n_err;\n"
               "}\n"
               "\n"
               "\n"
               "SAM_EXPORT void SAM_" << cmod_symbol << "_destruct(SAM_" << cmod_symbol << " system)\n"
               "{\n"
               "\tssc_data_free(system);\n"
               "}\n\n";

    // start ssc variables

    // setters
    for (size_t i = 0; i < root->vardefs_order.size() - 1; i++) {
        auto mm = root->m_vardefs.find(root->vardefs_order[i]);
        std::map<std::string, var_def> vardefs = mm->second;
        std::string module_symbol = format_as_symbol(mm->first);

        if (mm->first == "AdjustmentFactors")
            continue;

        for (auto it = vardefs.begin(); it != vardefs.end(); ++it) {
            std::string var_symbol = it->first;

            var_def vd = it->second;
            std::string var_name = vd.name;

            fx_file << "SAM_EXPORT void SAM_" << cmod_symbol << "_" << module_symbol << "_" << var_symbol;
            if (vd.type == "number") {
                fx_file << "_nset(SAM_" << cmod_symbol << " ptr, double number, SAM_error *err){\n"
                                                          "\ttranslateExceptions(err, [&]{\n"
                                                          "\t\tssc_data_set_number(ptr, \"" << var_name
                        << "\", number);\n\t});\n}";
            } else if (vd.type == "string") {
                fx_file << "_sset(SAM_" << cmod_symbol << " ptr, const char* str, SAM_error *err){\n"
                                                          "\ttranslateExceptions(err, [&]{\n"
                                                          "\t\tssc_data_set_string(ptr, \"" << var_name
                        << "\", str);\n\t});\n}";
            } else if (vd.type == "array") {
                fx_file << "_aset(SAM_" << cmod_symbol << " ptr, double* arr, int length, SAM_error *err){\n"
                                                          "\ttranslateExceptions(err, [&]{\n"
                                                          "\t\tssc_data_set_array(ptr, \"" << var_name
                        << "\", arr, length);\n\t});\n}";
            } else if (vd.type == "matrix") {
                fx_file << "_mset(SAM_" << cmod_symbol << " ptr, double* mat, int nrows, int ncols, SAM_error *err){\n"
                                                          "\ttranslateExceptions(err, [&]{\n"
                                                          "\t\tssc_data_set_matrix(ptr, \"" << var_name
                        << "\", mat, nrows, ncols);\n\t});\n}";
            } else if (vd.type == "table") {
                fx_file << "_tset(SAM_" << cmod_symbol << " ptr, SAM_table tab, SAM_error *err){\n"
                                                          "\tSAM_table_set_table(ptr, \"" << var_name << "\", tab, err);\n"
                                                          "}\n\n";
            } else {
                throw std::runtime_error(vd.type + " for " + var_name);
            }
            fx_file << "\n\n";
        }
    }

    // getters
    for (size_t i = 0; i < root->vardefs_order.size(); i++) {
        auto mm = root->m_vardefs.find(root->vardefs_order[i]);
        std::map<std::string, var_def> vardefs = mm->second;
        std::string module_symbol = format_as_symbol(mm->first);

        if (mm->first == "AdjustmentFactors")
            continue;

        for (auto it = vardefs.begin(); it != vardefs.end(); ++it){
            std::string var_symbol = it->first;

            var_def vd = it->second;
            std::string var_name = vd.name;

            if (vd.type == "number"){
                fx_file << "SAM_EXPORT double SAM_" << cmod_symbol << "_" << module_symbol << "_" << var_symbol << "_";
                fx_file << "nget(SAM_" << cmod_symbol << " ptr, SAM_error *err){\n";
                fx_file << "\tdouble result;\n"
                           "\ttranslateExceptions(err, [&]{\n"
                           "\tif (!ssc_data_get_number(ptr, \"" << var_name<< "\", &result))\n"
                           "\t\tmake_access_error(\"SAM_" << cmod_symbol << "\", \"" << var_name << "\");\n"
                           "\t});\n\treturn result;\n}\n\n";
            }
            else if (vd.type == "string"){
                fx_file << "SAM_EXPORT const char* SAM_" << cmod_symbol << "_" << module_symbol << "_" << var_symbol << "_";
                fx_file << "sget(SAM_" << cmod_symbol << " ptr, SAM_error *err){\n";
                fx_file << "\tconst char* result = nullptr;\n"
                           "\ttranslateExceptions(err, [&]{\n"
                           "\tresult = ssc_data_get_string(ptr, \"" << var_name<< "\");\n"
                           "\tif (!result)\n"
                           "\t\tmake_access_error(\"SAM_" << cmod_symbol << "\", \"" << var_name << "\");\n"
                           "\t});\n\treturn result;\n}\n\n";
            }
            else if (vd.type == "array"){
                fx_file << "SAM_EXPORT double* SAM_" << cmod_symbol << "_" << module_symbol << "_" << var_symbol << "_";
                fx_file << "aget(SAM_" << cmod_symbol << " ptr, int* length, SAM_error *err){\n";
                fx_file << "\tdouble* result = nullptr;\n"
                           "\ttranslateExceptions(err, [&]{\n"
                           "\tresult = ssc_data_get_array(ptr, \"" << var_name<< "\", length);\n"
                           "\tif (!result)\n"
                           "\t\tmake_access_error(\"SAM_" << cmod_symbol << "\", \"" << var_name << "\");\n"
                           "\t});\n\treturn result;\n}\n\n";

            }
            else if (vd.type == "matrix"){
                fx_file << "SAM_EXPORT double* SAM_" << cmod_symbol << "_" << module_symbol << "_" << var_symbol << "_";
                fx_file << "mget(SAM_" << cmod_symbol << " ptr, int* nrows, int* ncols, SAM_error *err){\n";
                fx_file << "\tdouble* result = nullptr;\n"
                           "\ttranslateExceptions(err, [&]{\n"
                           "\tresult = ssc_data_get_matrix(ptr, \"" << var_name<< "\", nrows, ncols);\n"
                           "\tif (!result)\n"
                           "\t\tmake_access_error(\"SAM_" << cmod_symbol << "\", \"" << var_name << "\");\n"
                           "\t});\n\treturn result;\n}\n\n";
            }
            else if (vd.type == "table"){
                fx_file << "SAM_EXPORT SAM_table SAM_" << cmod_symbol << "_" << module_symbol << "_" << var_symbol << "_";
                fx_file << "tget(SAM_" << cmod_symbol << " ptr, SAM_error *err){\n";
                fx_file << "\tSAM_table result = nullptr;\n"
                           "\ttranslateExceptions(err, [&]{\n"
                           "\tresult = ssc_data_get_table(ptr, \"" << var_name<< "\");\n"
                           "\tif (!result)\n"
                           "\t\tmake_access_error(\"SAM_" << cmod_symbol << "\", \"" << var_name << "\");\n"
                           "\t});\n\treturn result;\n}\n\n";
            }
            else{
                throw std::runtime_error(vd.type + " for " + var_name);
            }
            fx_file << "\n\n";

        }
    }
    fx_file.close();
}