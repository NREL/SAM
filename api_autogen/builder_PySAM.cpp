#include <shared/lib_util.h>

#include "builder_PySAM.h"
#include "builder_generator_helper.h"

void builder_PySAM::create_PySAM_files(const std::string &file_dir, const std::string &cmod){
    std::string cmod_symbol = format_as_symbol(cmod);

    std::ofstream fx_file;
    fx_file.open(file_dir + "/PySAM/" + cmod_symbol + ".c");
    assert(fx_file.is_open());

    fx_file << "#include <Python.h>\n"
               "\n"
               "#include \"SAM_GenericSystem.h\"\n"
               "#include \"SAM_api.h\"\n"
               "\n"
               "#include \"PySAM_utils.h\"\n\n\n";


    // setters, none for outputs
    for (size_t i = 0; i < root->vardefs_order.size() - 1; i++) {
        auto mm = root->m_vardefs.find(root->vardefs_order[i]);
        std::map<std::string, var_def> vardefs = mm->second;

        std::string module_symbol = format_as_symbol(mm->first);

        fx_file << "\n"
                   "\t/*\n"
                   "\t * " << module_symbol << " Group\n"
                   "\t */ \n\n";

        // group description as object

        fx_file << "typedef struct {\n"
                   "\tPyObject_HEAD\n"
                   "\tSAM_" << cmod_symbol << "   data_ptr;\n"
                   "} " << module_symbol << "Object;\n"
                   "\n"
                   "static PyTypeObject " << module_symbol << "_Type;\n\n";
        fx_file << "static PyObject *\n"
                   << module_symbol << "_new(SAM_" << cmod_symbol << " data_ptr)\n"
                   "{\n"
                   "\tPyObject* new_obj = " << module_symbol << "_Type.tp_alloc(&" << module_symbol << "_Type,0);\n"
                   "\n"
                   "\t" << module_symbol << "Object* " << module_symbol << "_obj = (" << module_symbol << "Object*)new_obj;\n"
                   "\n"
                   "\t" << module_symbol << "_obj->data_ptr = data_ptr;\n"
                   "\n"
                   "\treturn new_obj;\n"
                   "}\n\n";

        fx_file << "/* Plant methods */\n\n";

        // group methods

        fx_file << "static PyObject *\n"
                << module_symbol << "_assign(" << module_symbol << "Object *self, PyObject *args)\n"
                   "{\n"
                   "    PyObject* dict;\n"
                   "    if (!PyArg_ParseTuple(args, \"O:assign\", &dict)){\n"
                   "        return NULL;\n"
                   "    }\n"
                   "\n"
                   "    if (!PySAM_assign_from_dict(self->data_ptr, dict, \"" << cmod_symbol << "\", \"" << module_symbol << "\")){\n"
                   "        return NULL;\n"
                   "    }\n"
                   "\n"
                   "    Py_INCREF(Py_None);\n"
                   "    return Py_None;\n"
                   "}\n"
                   "\n"
                   "static PyObject *\n"
                << module_symbol << "_export(" << module_symbol << "Object *self, PyObject *args)\n"
                   "{\n"
                   "    PyTypeObject* tp = &" << module_symbol << "_Type;\n"
                   "    PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);\n"
                   "    return dict;\n"
                   "}\n"
                   "\n"
                   "static PyMethodDef " << module_symbol << "_methods[] = {\n"
                   "        {\"assign\",            (PyCFunction)" << module_symbol << "_assign,  METH_VARARGS,\n"
                   "                PyDoc_STR(\"assign() -> None\\n Assign attributes from dictionary\")},\n"
                   "        {\"export\",            (PyCFunction)" << module_symbol << "_export,  METH_VARARGS,\n"
                   "                PyDoc_STR(\"export() -> None\\n Export attributes into dictionary\")},\n"
                   "        {NULL,              NULL}           /* sentinel */\n"
                   "};\n\n";

        for (auto it = vardefs.begin(); it != vardefs.end(); ++it) {
            std::string var_name = it->first;

            var_def vd = it->second;

            if (vd.type == "number") {
                fx_file << "static PyObject *\n"
                        << module_symbol << "_get_" << var_name << "(" << module_symbol << "Object *self, void *closure)\n"
                           "{\n"
                           "\tPySAM_FLOAT_GETTER(SAM_" << cmod_symbol << "_" << module_symbol << "_" << var_name << "_fget)\n"
                           "}\n"
                           "\n"
                           "static int\n"
                        << module_symbol << "_set_" << var_name << "(" << module_symbol << "Object *self, PyObject *value, void *closure)\n"
                           "{\n"
                           "\tPySAM_FLOAT_SETTER(SAM_" << cmod_symbol << "_" << module_symbol << "_" << var_name << "_fset)\n"
                           "}\n\n";
            } else if (vd.type == "string") {
                fx_file << "static PyObject *\n"
                        << module_symbol << "_get_" << var_name << "(" << module_symbol << "Object *self, void *closure)\n"
                           "{\n"
                           "\tPySAM_STRING_GETTER(SAM_" << cmod_symbol << "_" << module_symbol << "_" << var_name << "_sget)\n"
                           "}\n"
                           "\n"
                           "static int\n"
                        << module_symbol << "_set_" << var_name << "(" << module_symbol << "Object *self, PyObject *value, void *closure)\n"
                           "{\n"
                           "\tPySAM_STRING_SETTER(SAM_" << cmod_symbol << "_" << module_symbol << "_" << var_name << "_sset)\n"
                           "}\n\n";
            } else if (vd.type == "array") {
                fx_file << "static PyObject *\n"
                        << module_symbol << "_get_" << var_name << "(" << module_symbol << "Object *self, void *closure)\n"
                           "{\n"
                           "\treturn PySAM_array_getter(SAM_" << cmod_symbol << "_" << module_symbol << "_" << var_name << "_aget, self->data_ptr);\n"
                           "}\n"
                           "\n"
                           "static int\n"
                        << module_symbol << "_set_" << var_name << "(" << module_symbol << "Object *self, PyObject *value, void *closure)\n"
                           "{\n"
                           "\t    return PySAM_array_setter(value, SAM_" << cmod_symbol << "_" << module_symbol << "_" << var_name << "_aset, self->data_ptr);\n"
                           "}\n\n";
            } else if (vd.type == "matrix") {
                fx_file << "static PyObject *\n"
                        << module_symbol << "_get_" << var_name << "(" << module_symbol << "Object *self, void *closure)\n"
                            "{\n"
                            "\treturn PySAM_matrix_getter(SAM_" << cmod_symbol << "_" << module_symbol << "_" << var_name << "_mget, self->data_ptr);\n"
                            "}\n"
                            "\n"
                            "static int\n"
                        << module_symbol << "_set_" << var_name << "(" << module_symbol << "Object *self, PyObject *value, void *closure)\n"
                            "{\n"
                            "\t    return PySAM_matrix_setter(value, SAM_" << cmod_symbol << "_" << module_symbol << "_" << var_name << "_mset, self->data_ptr);\n"
                            "}\n\n";
            } else if (vd.type == "table") {
                fx_file << "tset(SAM_" << cmod_symbol << " ptr, SAM_table tab, SAM_error *err);\n\n";
            } else {
                throw std::runtime_error(vd.type + " for " + var_name);
            }
        }

        for (auto it = vardefs.begin(); it != vardefs.end(); ++it) {
            std::string var_name = it->first;

            var_def vd = it->second;


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

            // 	SAM_EXPORT void SAM_GenericSystem_PowerPlant_derate_fset(SAM_GenericSystem ptr, float number, SAM_error *err);

            fx_file << "\tSAM_EXPORT void SAM_" << cmod_symbol << "_" << module_symbol << "_" << var_name << "_";
            if (vd.type == "number") {
                fx_file << "fset(SAM_" << cmod_symbol << " ptr, float number, SAM_error *err);\n\n";
            } else if (vd.type == "string") {
                fx_file << "sset(SAM_" << cmod_symbol << " ptr, const char* str, SAM_error *err);\n\n";
            } else if (vd.type == "array") {
                fx_file << "aset(SAM_" << cmod_symbol << " ptr, float* arr, int length, SAM_error *err);\n\n";
            } else if (vd.type == "matrix") {
                fx_file << "mset(SAM_" << cmod_symbol << " ptr, float* mat, int nrows, int ncols, SAM_error *err);\n\n";
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

        // getters
        fx_file << "\n\t/**\n";
        fx_file << "\t * " << module_symbol << " Getters\n\t */\n\n";

        for (auto it = vardefs.begin(); it != vardefs.end(); ++it){
            std::string var_name = it->first;

            var_def vd = it->second;

            if (vd.type == "number"){
                fx_file << "\tSAM_EXPORT float SAM_" << cmod_symbol << "_" << module_symbol << "_" << var_name << "_";
                fx_file << "fget(SAM_" << cmod_symbol << " ptr, SAM_error *err);\n\n";
            }
            else if (vd.type == "string"){
                fx_file << "\tSAM_EXPORT const char* SAM_" << cmod_symbol << "_" << module_symbol << "_" << var_name << "_";
                fx_file << "sget(SAM_" << cmod_symbol << " ptr, SAM_error *err);\n\n";
            }
            else if (vd.type == "array"){
                fx_file << "\tSAM_EXPORT float* SAM_" << cmod_symbol << "_" << module_symbol << "_" << var_name << "_";
                fx_file << "aget(SAM_" << cmod_symbol << " ptr, int* length, SAM_error *err);\n\n";
            }
            else if (vd.type == "matrix"){
                fx_file << "\tSAM_EXPORT float* SAM_" << cmod_symbol << "_" << module_symbol << "_" << var_name << "_";
                fx_file << "mget(SAM_" << cmod_symbol << " ptr, int* nrows, int* ncols, SAM_error *err);\n\n";
            }
            else if (vd.type == "table"){
                fx_file << "\tSAM_EXPORT SAM_table SAM_" << cmod_symbol << "_" << module_symbol << "_" << var_name << "_";
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
