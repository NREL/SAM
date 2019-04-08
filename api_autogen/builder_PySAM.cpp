#include <iostream>

#include <shared/lib_util.h>

#include "builder_PySAM.h"
#include "builder_generator_helper.h"

std::string all_fin_of_tech(const std::string config){
    std::string str;
    std::string tech = config.substr(0, config.find_last_of('-'));
    for (auto it = SAM_config_to_primary_modules.begin(); it != SAM_config_to_primary_modules.end(); ++it){
        std::string config_name = it->first;
        size_t pos = config.find_last_of('-');
        if (config_name.substr(0, pos) == tech)
            str += config_name.substr(pos+1) + ", ";
    }
    return str;
}

void builder_PySAM::create_PySAM_files(const std::string &file_dir, const std::string &cmod){
    std::string cmod_symbol = format_as_symbol(cmod);

    std::string tech_symbol = cmod_symbol;
    if(cmod_symbol == "Battery")
        tech_symbol = "StandAloneBattery";
    else if (root->m_vardefs.find(cmod_symbol) != root->m_vardefs.end())
        tech_symbol += "Model";

    std::ofstream fx_file;
    fx_file.open(file_dir + "/PySAM/" + tech_symbol + ".c");
    assert(fx_file.is_open());

    fx_file << "#include <Python.h>\n"
               "\n"
               "#include <SAM_" << cmod_symbol << ".h>\n"
               "#include <SAM_api.h>\n"
               "\n"
               "#include \"PySAM_utils.h\"\n\n\n";



    // setters, none for outputs
    for (size_t i = 0; i < root->vardefs_order.size() ; i++) {
        auto mm = root->m_vardefs.find(root->vardefs_order[i]);
        std::map<std::string, var_def> vardefs = mm->second;

        bool output = 0;
        std::string module_symbol = format_as_symbol(mm->first);

        if (module_symbol == "AdjustmentFactors")
            continue;

        if (module_symbol == "Outputs"){
            output = 1;
        }


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
                   "static PyTypeObject " << module_symbol
                << "_Type;\n\n";
        fx_file << "static PyObject *\n"
                << module_symbol << "_new(SAM_" << cmod_symbol << " data_ptr)\n"
                   "{\n"
                   "\tPyObject* new_obj = " << module_symbol
                << "_Type.tp_alloc(&" << module_symbol << "_Type,0);\n"
                   "\n"
                   "\t" << module_symbol << "Object* " << module_symbol
                << "_obj = (" << module_symbol << "Object*)new_obj;\n"
                   "\n"
                   "\t" << module_symbol << "_obj->data_ptr = data_ptr;\n"
                   "\n"
                   "\treturn new_obj;\n"
                   "}\n\n";

        fx_file << "/* " << module_symbol << " methods */\n\n";

        // group methods

        fx_file << "static PyObject *\n"
                << module_symbol << "_assign(" << module_symbol << "Object *self, PyObject *args)\n"
                   "{\n"
                   "\tPyObject* dict;\n"
                   "\tif (!PyArg_ParseTuple(args, \"O:assign\", &dict)){\n"
                   "\t\treturn NULL;\n"
                   "\t}\n"
                   "\n"
                   "\tif (!PySAM_assign_from_dict(self->data_ptr, dict, \""
                << cmod_symbol << "\", \"" << module_symbol << "\")){\n"
                   "\t\treturn NULL;\n"
                   "\t}\n"
                   "\n"
                   "\tPy_INCREF(Py_None);\n"
                   "\treturn Py_None;\n"
                   "}\n"
                   "\n"
                   "static PyObject *\n"
                << module_symbol << "_export(" << module_symbol << "Object *self, PyObject *args)\n"
                   "{\n"
                   "\tPyTypeObject* tp = &" << module_symbol
                << "_Type;\n"
                   "\tPyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);\n"
                   "\treturn dict;\n"
                   "}\n"
                   "\n"
                   "static PyMethodDef " << module_symbol << "_methods[] = {\n"
                   "\t\t{\"assign\",            (PyCFunction)"
                << module_symbol << "_assign,  METH_VARARGS,\n"
                   "\t\t\tPyDoc_STR(\"assign() -> None\\n Assign attributes from dictionary\")},\n"
                   "\t\t{\"export\",            (PyCFunction)" << module_symbol
                << "_export,  METH_VARARGS,\n"
                   "\t\t\tPyDoc_STR(\"export() -> None\\n Export attributes into dictionary\")},\n"
                   "\t\t{NULL,              NULL}           /* sentinel */\n"
                   "};\n\n";

        // variable getter and setter (setters if not output)
        for (auto it = vardefs.begin(); it != vardefs.end(); ++it) {
            std::string var_symbol = it->first;

            var_def vd = it->second;

            if (module_symbol == "AdjustmentFactors")
                continue;


            if (vd.type == "number") {
                fx_file << "static PyObject *\n"
                        << module_symbol << "_get_" << var_symbol << "(" << module_symbol
                        << "Object *self, void *closure)\n"
                           "{\n"
                           "\treturn PySAM_float_getter(SAM_" << cmod_symbol << "_" << module_symbol << "_" << var_symbol
                        << "_fget, self->data_ptr);\n"
                           "}\n"
                           "\n";
                if (output)
                    continue;
                fx_file << "static int\n"
                        << module_symbol << "_set_" << var_symbol << "(" << module_symbol
                        << "Object *self, PyObject *value, void *closure)\n"
                           "{\n"
                           "\treturn PySAM_float_setter(value, SAM_" << cmod_symbol << "_" << module_symbol << "_"
                        << var_symbol << "_fset, self->data_ptr);\n"
                           "}\n\n";
            } else if (vd.type == "string") {
                fx_file << "static PyObject *\n"
                        << module_symbol << "_get_" << var_symbol << "(" << module_symbol
                        << "Object *self, void *closure)\n"
                           "{\n"
                           "\treturn PySAM_string_getter(SAM_" << cmod_symbol << "_" << module_symbol << "_"
                        << var_symbol << "_sget, self->data_ptr);\n"
                           "}\n"
                           "\n";
                if (output)
                    continue;
                fx_file << "static int\n"
                        << module_symbol << "_set_" << var_symbol << "(" << module_symbol
                        << "Object *self, PyObject *value, void *closure)\n"
                           "{\n"
                           "\treturn PySAM_string_setter(value, SAM_" << cmod_symbol << "_" << module_symbol << "_"
                        << var_symbol << "_sset, self->data_ptr);\n"
                           "}\n\n";
            } else if (vd.type == "array") {
                fx_file << "static PyObject *\n"
                        << module_symbol << "_get_" << var_symbol << "(" << module_symbol
                        << "Object *self, void *closure)\n"
                           "{\n"
                           "\treturn PySAM_array_getter(SAM_" << cmod_symbol << "_" << module_symbol << "_" << var_symbol
                        << "_aget, self->data_ptr);\n"
                           "}\n"
                           "\n";
                if (output)
                    continue;
                fx_file << "static int\n"
                        << module_symbol << "_set_" << var_symbol << "(" << module_symbol
                        << "Object *self, PyObject *value, void *closure)\n"
                           "{\n"
                           "\t\treturn PySAM_array_setter(value, SAM_" << cmod_symbol << "_" << module_symbol << "_"
                        << var_symbol << "_aset, self->data_ptr);\n"
                           "}\n\n";
            } else if (vd.type == "matrix") {
                fx_file << "static PyObject *\n"
                        << module_symbol << "_get_" << var_symbol << "(" << module_symbol
                        << "Object *self, void *closure)\n"
                           "{\n"
                           "\treturn PySAM_matrix_getter(SAM_" << cmod_symbol << "_" << module_symbol << "_" << var_symbol
                        << "_mget, self->data_ptr);\n"
                           "}\n"
                           "\n";
                if (output)
                    continue;
                fx_file << "static int\n"
                        << module_symbol << "_set_" << var_symbol << "(" << module_symbol
                        << "Object *self, PyObject *value, void *closure)\n"
                           "{\n"
                           "\t\treturn PySAM_matrix_setter(value, SAM_" << cmod_symbol << "_" << module_symbol << "_"
                        << var_symbol << "_mset, self->data_ptr);\n"
                           "}\n\n";
            } else if (vd.type == "table") {
                fx_file << "static PyObject *\n"
                           "" << module_symbol << "_get_" << var_symbol << "(" << module_symbol
                        << "Object *self, void *closure)\n"
                           "{\n"
                           "\treturn PySAM_table_getter(SAM_" << cmod_symbol << "_" << module_symbol << "_" << var_symbol
                        << "_tget, self->data_ptr);\n"
                           "}\n"
                           "\n";
                if (output)
                    continue;
                fx_file << "static int\n"
                           "" << module_symbol << "_set_" << var_symbol << "(" << module_symbol
                        << "Object *self, PyObject *value, void *closure)\n"
                           "{\n"
                           "\treturn PySAM_table_setter(value, SAM_" << cmod_symbol << "_" << module_symbol << "_"
                        << var_symbol << "_tset, self->data_ptr);\n"
                           "}\n\n";
            } else {
                throw std::runtime_error(vd.type + " for " + var_symbol);
            }
        }

        fx_file << "static PyGetSetDef " << module_symbol << "_getset[] = {\n";
        for (auto it = vardefs.begin(); it != vardefs.end(); ++it) {
            std::string var_symbol = it->first;

            var_def vd = it->second;

            if (module_symbol == "AdjustmentFactors")
                continue;

            // make the PyGetSetDef struct

            std::string doc = vd.doc;
            doc += ", " + vd.type + ".";


            fx_file << "{\"" << var_symbol << "\", (getter)" << module_symbol << "_get_" << var_symbol << ",";


            if (output)
                fx_file << "(setter)0,\n";
            else{
                doc += "\\n ";
                if (vd.meta.length() > 0)
                    doc += vd.meta + "; ";

                if (vd.constraints.length() > 0) {
                    doc += "Constraints: ";
                    doc += vd.constraints + "; ";
                }

                if (vd.reqif.length() > 0) {
                    doc += "Required if: ";
                    doc += vd.reqif + ".";
                }

                fx_file << "(setter)" << module_symbol << "_set_" << var_symbol << ",\n";
            }

            fx_file << "\t\"" << doc << "\",\n \tNULL},\n";

        }
        fx_file << "\t{NULL}  /* Sentinel */\n};\n\n";


        // define the module type
        fx_file << "static PyTypeObject " << module_symbol << "_Type = {\n"
                      "\t\t/* The ob_type field must be initialized in the module init function\n"
                      "\t\t * to be portable to Windows without using C++. */\n"
                      "\t\tPyVarObject_HEAD_INIT(NULL, 0)\n"
                      "\t\t\"" << tech_symbol << "." << module_symbol << "\",             /*tp_name*/\n"
                      "\t\tsizeof(" << module_symbol << "Object),          /*tp_basicsize*/\n"
                      "\t\t0,                          /*tp_itemsize*/\n"
                      "\t\t/* methods */\n"
                      "\t\t0,    /*tp_dealloc*/\n"
                      "\t\t0,                          /*tp_print*/\n"
                      "\t\t(getattrfunc)0,             /*tp_getattr*/\n"
                      "\t\t0,                          /*tp_setattr*/\n"
                      "\t\t0,                          /*tp_reserved*/\n"
                      "\t\t0,                          /*tp_repr*/\n"
                      "\t\t0,                          /*tp_as_number*/\n"
                      "\t\t0,                          /*tp_as_sequence*/\n"
                      "\t\t0,                          /*tp_as_mapping*/\n"
                      "\t\t0,                          /*tp_hash*/\n"
                      "\t\t0,                          /*tp_call*/\n"
                      "\t\t0,                          /*tp_str*/\n"
                      "\t\t0,                          /*tp_getattro*/\n"
                      "\t\t0,                          /*tp_setattro*/\n"
                      "\t\t0,                          /*tp_as_buffer*/\n"
                      "\t\tPy_TPFLAGS_DEFAULT,         /*tp_flags*/\n"
                      "\t\t0,                          /*tp_doc*/\n"
                      "\t\t0,                          /*tp_traverse*/\n"
                      "\t\t0,                          /*tp_clear*/\n"
                      "\t\t0,                          /*tp_richcompare*/\n"
                      "\t\t0,                          /*tp_weaklistoffset*/\n"
                      "\t\t0,                          /*tp_iter*/\n"
                      "\t\t0,                          /*tp_iternext*/\n"
                      "\t\t" << module_symbol << "_methods,         /*tp_methods*/\n"
                      "\t\t0,                          /*tp_members*/\n"
                      "\t\t" << module_symbol << "_getset,          /*tp_getset*/\n"
                      "\t\t0,                          /*tp_base*/\n"
                      "\t\t0,                          /*tp_dict*/\n"
                      "\t\t0,                          /*tp_descr_get*/\n"
                      "\t\t0,                          /*tp_descr_set*/\n"
                      "\t\t0,                          /*tp_dictoffset*/\n"
                      "\t\t0,                          /*tp_init*/\n"
                      "\t\t0,                          /*tp_alloc*/\n"
                      "\t\t0,             /*tp_new*/\n"
                      "\t\t0,                          /*tp_free*/\n"
                      "\t\t0,                          /*tp_is_gc*/\n"
                      "};\n\n";
    }

    // define the technology

    fx_file << "/*\n"
               " * " << tech_symbol << "\n"
               " */\n"
               "\n"
               "typedef struct {\n"
               "\tPyObject_HEAD\n"
               "\tPyObject            *x_attr;        /* Attributes dictionary */\n"
               "\tSAM_" << cmod_symbol << "   data_ptr;\n"
               "} " << tech_symbol << "Object;\n"
               "\n"
               "static PyTypeObject " << tech_symbol << "_Type;\n"
               "\n"
               "#define " << tech_symbol << "Object_Check(v)      (Py_TYPE(v) == &" << tech_symbol << "_Type)\n"
               "\n"
               "static " << tech_symbol << "Object *\n"
               "new" << tech_symbol << "Object(void* data_ptr)\n"
               "{\n"
               "\t" << tech_symbol << "Object *self;\n"
               "\tself = PyObject_New(" << tech_symbol << "Object, &" << tech_symbol << "_Type);\n"
               "\n"
               "\tPySAM_TECH_ATTR(\"" << tech_symbol << "\", SAM_" << cmod_symbol << "_construct)\n\n";

    // add the group types
    for (size_t i = 0; i < root->vardefs_order.size() ; i++) {
        auto mm = root->m_vardefs.find(root->vardefs_order[i]);
        std::map<std::string, var_def> vardefs = mm->second;

        std::string module_symbol = format_as_symbol(mm->first);

        if (module_symbol == "AdjustmentFactors")
            continue;
    
        fx_file << "PyObject* " << module_symbol << "_obj = " << module_symbol << "_new(self->data_ptr);\n"
                   "\tPyDict_SetItemString(attr_dict, \"" << module_symbol << "\", " << module_symbol << "_obj);\n"
                   "\tPy_DECREF(" << module_symbol << "_obj);\n\n";
    }
    
    // add adjustment group and close
    
    fx_file << "PyObject* AdjustmentFactorsModule = PyImport_ImportModule(\"AdjustmentFactors\");\n"
               "\n"
               "\tPyObject* data_cap = PyCapsule_New(self->data_ptr, NULL, NULL);\n"
               "\tPyObject* Adjust_obj = PyObject_CallMethod(AdjustmentFactorsModule, \"new\", \"(O)\", data_cap);\n"
               "\tPy_XDECREF(data_cap);\n"
               "\tPy_XDECREF(AdjustmentFactorsModule);\n"
               "\n"
               "\tif (!Adjust_obj){\n"
               "\t\tPyErr_SetString(PySAM_ErrorObject, \"Couldn't create AdjustmentFactorsObject\\n\");\n"
               "\t\treturn NULL;\n"
               "\t}\n"
               "\n"
               "\tPyDict_SetItemString(attr_dict, \"AdjustmentFactors\", Adjust_obj);\n"
               "\tPy_DECREF(Adjust_obj);\n"
               "\n"
               "\treturn self;\n"
               "}\n\n";
    
    // add methods
    fx_file << "/* " << tech_symbol << " methods */\n"
               "\n"
               "static void\n"
               "" << tech_symbol << "_dealloc(" << tech_symbol << "Object *self)\n"
               "{\n"
               "\tPy_XDECREF(self->x_attr);\n"
               "\tSAM_" << cmod_symbol << "_destruct(self->data_ptr);\n"
               "\tPyObject_Del(self);\n"
               "}\n"
               "\n"
               "\n"
               "static PyObject *\n"
               "" << tech_symbol << "_execute(" << tech_symbol << "Object *self, PyObject *args)\n"
               "{\n"
               "\tint verbosity = 0;\n"
               "\n"
               "\tif (!PyArg_ParseTuple(args, \"|i\", &verbosity))\n"
               "\t\treturn NULL;\n"
               "\n"
               "\tSAM_error error = new_error();\n"
               "\tSAM_" << cmod_symbol << "_execute(self->data_ptr, verbosity, &error);\n"
               "\tif (PySAM_has_error(error )) return NULL;\n"
               "\n"
               "\tPy_INCREF(Py_None);\n"
               "\treturn Py_None;\n"
               "}\n"
               "\n"
               "\n"
               "static PyObject *\n"
               "" << tech_symbol << "_assign(" << tech_symbol << "Object *self, PyObject *args)\n"
               "{\n"
               "\tPyObject* dict;\n"
               "\tif (!PyArg_ParseTuple(args, \"O:assign\", &dict)){\n"
               "\t\treturn NULL;\n"
               "\t}\n"
               "\n"
               "\tif (!PySAM_assign_from_nested_dict((PyObject*)self, self->x_attr, self->data_ptr, dict, \"" << tech_symbol << "\"))\n"
               "\t\treturn NULL;\n"
               "\n"
               "\tPy_INCREF(Py_None);\n"
               "\treturn Py_None;\n"
               "}\n"
               "\n"
               "\n"
               "static PyObject *\n"
               "" << tech_symbol << "_export(" << tech_symbol << "Object *self, PyObject *args)\n"
               "{\n"
               "\treturn PySAM_export_to_nested_dict((PyObject *) self, self->x_attr);\n"
               "}\n"
               "\n"
               "static PyMethodDef " << tech_symbol << "_methods[] = {\n"
               "\t\t{\"execute\",            (PyCFunction)" << tech_symbol << "_execute,  METH_VARARGS,\n"
               "\t\t\t\tPyDoc_STR(\"execute(int verbosity) -> None\\n Execute simulation with verbosity level 0 (default) or 1\")},\n"
               "\t\t{\"assign\",            (PyCFunction)" << tech_symbol << "_assign,  METH_VARARGS,\n"
               "\t\t\t\tPyDoc_STR(\"assign(dict) -> None\\n Assign attributes from nested dictionary, except for Outputs\")},\n"
               "\t\t{\"export\",            (PyCFunction)" << tech_symbol << "_export,  METH_VARARGS,\n"
               "\t\t\t\tPyDoc_STR(\"assign() -> None\\n Export attributes into dictionary\")},\n"
               "\t\t{NULL,              NULL}           /* sentinel */\n"
               "};\n"
               "\n"
               "static PyObject *\n"
               "" << tech_symbol << "_getattro(" << tech_symbol << "Object *self, PyObject *name)\n"
               "{\n"
               "\treturn PySAM_get_attr((PyObject*) self, (PyObject*) self->x_attr, name);\n"
               "}\n"
               "\n"
               "static int\n"
               "" << tech_symbol << "_setattr(" << tech_symbol << "Object *self, const char *name, PyObject *v)\n"
               "{\n"
               "\treturn PySAM_set_attr((PyObject*)self, (PyObject*)self->x_attr, name, v);\n"
               "}\n\n";
    
    // define technology type
    fx_file << "static PyTypeObject " << tech_symbol << "_Type = {\n"
               "\t\t/* The ob_type field must be initialized in the module init function\n"
               "\t\t * to be portable to Windows without using C++. */\n"
               "\t\tPyVarObject_HEAD_INIT(NULL, 0)\n"
               "\t\t\"" << tech_symbol << "\",            /*tp_name*/\n"
               "\t\tsizeof(" << tech_symbol << "Object),/*tp_basicsize*/\n"
               "\t\t0,                          /*tp_itemsize*/\n"
               "\t\t/* methods */\n"
               "\t\t(destructor)" << tech_symbol << "_dealloc,    /*tp_dealloc*/\n"
               "\t\t0,                          /*tp_print*/\n"
               "\t\t(getattrfunc)0,             /*tp_getattr*/\n"
               "\t\t(setattrfunc)" << tech_symbol << "_setattr,   /*tp_setattr*/\n"
               "\t\t0,                          /*tp_reserved*/\n"
               "\t\t0,                          /*tp_repr*/\n"
               "\t\t0,                          /*tp_as_number*/\n"
               "\t\t0,                          /*tp_as_sequence*/\n"
               "\t\t0,                          /*tp_as_mapping*/\n"
               "\t\t0,                          /*tp_hash*/\n"
               "\t\t0,                          /*tp_call*/\n"
               "\t\t0,                          /*tp_str*/\n"
               "\t\t(getattrofunc)" << tech_symbol << "_getattro, /*tp_getattro*/\n"
               "\t\t0,                          /*tp_setattro*/\n"
               "\t\t0,                          /*tp_as_buffer*/\n"
               "\t\tPy_TPFLAGS_DEFAULT,         /*tp_flags*/\n"
               "\t\t\"see html for help\",        /*tp_doc*/\n"
               "\t\t0,                          /*tp_traverse*/\n"
               "\t\t0,                          /*tp_clear*/\n"
               "\t\t0,                          /*tp_richcompare*/\n"
               "\t\t0,                          /*tp_weaklistoffset*/\n"
               "\t\t0,                          /*tp_iter*/\n"
               "\t\t0,                          /*tp_iternext*/\n"
               "\t\t" << tech_symbol << "_methods,      /*tp_methods*/\n"
               "\t\t0,                          /*tp_members*/\n"
               "\t\t0,       /*tp_getset*/\n"
               "\t\t0,                          /*tp_base*/\n"
               "\t\t0,                          /*tp_dict*/\n"
               "\t\t0,                          /*tp_descr_get*/\n"
               "\t\t0,                          /*tp_descr_set*/\n"
               "\t\t0,                          /*tp_dictoffset*/\n"
               "\t\t0,                          /*tp_init*/\n"
               "\t\t0,                          /*tp_alloc*/\n"
               "\t\t0,                          /*tp_new*/\n"
               "\t\t0,                          /*tp_free*/\n"
               "\t\t0,                          /*tp_is_gc*/\n"
               "};\n\n";

    // define module's new function
    
    fx_file << "/* --------------------------------------------------------------------- */\n"
               "\n"
               "\n"
               "/* Function of no arguments returning new " << tech_symbol << " object */\n"
               "\n"
               "static PyObject *\n"
               "" << tech_symbol << "_new(PyObject *self, PyObject *args)\n"
               "{\n"
               "\t" << tech_symbol << "Object *rv;\n"
               "\trv = new" << tech_symbol << "Object(0);\n"
               "\tif (rv == NULL)\n"
               "\t\treturn NULL;\n"
               "\treturn (PyObject *)rv;\n"
               "}\n\n";

    // wrapping fx

    fx_file << "static PyObject *\n"
               "" << tech_symbol << "_wrap(PyObject *self, PyObject *args)\n"
               "{\n"
               "\t" << tech_symbol << "Object *rv;\n"
               "\tlong int ptr = 0;\n"
               "\tif (!PyArg_ParseTuple(args, \"l:wrap\", &ptr)){\n"
               "\t\tPyErr_BadArgument();\n"
               "\t\treturn NULL;\n"
               "\t}\n"
               "\trv = new" << tech_symbol << "Object((void*)ptr);\n"
               "\tif (rv == NULL)\n"
               "\t\treturn NULL;\n"
               "\treturn (PyObject *)rv;\n"
               "}\n\n";

    // defaults loading fx

    fx_file << "static PyObject *\n"
               "" << tech_symbol << "_default(PyObject *self, PyObject *args)\n"
               "{\n"
               "\t" << tech_symbol << "Object *rv;\n"
               "\tchar* fin = 0;\n"
               "\tif (!PyArg_ParseTuple(args, \"s:default\", &fin)){\n"
               "\t\tPyErr_BadArgument();\n"
               "\t\treturn NULL;\n"
               "\t}\n"
               "\trv = new" << tech_symbol << "Object(0);\n"
               "\tif (rv == NULL)\n"
               "\t\treturn NULL;\n"
               "\n"
               "\tPySAM_load_defaults((PyObject*)rv, rv->x_attr, rv->data_ptr, \"" << cmod_symbol << "\", fin);\n"
               "\n"
               "\treturn (PyObject *)rv;\n"
               "}\n\n";


    fx_file << "/* ---------- */\n"
               "\n"
               "\n"
               "/* List of functions defined in the module */\n"
               "\n"
               "static PyMethodDef " << tech_symbol << "Module_methods[] = {\n"
               "\t\t{\"new\",             " << tech_symbol << "_new,         METH_VARARGS,\n"
               "\t\t\t\tPyDoc_STR(\"new() -> new " << tech_symbol << " object\")},\n"
               "\t\t{\"default\",             " << tech_symbol << "_default,         METH_VARARGS,\n"
               "\t\t\t\tPyDoc_STR(\"default(financial) -> new " << tech_symbol << " object with financial model-specific default attributes\\n\"\n"
                                                                                  "\t\t\t\t\"Options: " << all_fin_of_tech(config_name) << "\")},\n"
               "\t\t{\"wrap\",             " << tech_symbol << "_wrap,         METH_VARARGS,\n"
               "\t\t\t\tPyDoc_STR(\"wrap(ssc_data_t) -> new " << tech_symbol << " object around existing PySSC data\")},\n"
               "\t\t{NULL,              NULL}           /* sentinel */\n"
               "};\n"
               "\n"
               "PyDoc_STRVAR(module_doc,\n"
               "\t\t\t \"Refer to http://www.github.com/nrel/PySAM for source code.\");\n\n\n";

    // define the execution of module and adjustmentfactors type
    fx_file << "static int\n"
               "" << tech_symbol << "Module_exec(PyObject *m)\n"
               "{\n"
               "\t/* Finalize the type object including setting type of the new type\n"
               "\t * object; doing it here is required for portability, too. */\n"
               "\n"
               "\t" << tech_symbol << "_Type.tp_dict = PyDict_New();\n"
               "\tif (!" << tech_symbol << "_Type.tp_dict) { goto fail; }\n"
               "\n"
               "\t/// Add the AdjustmentFactors type object to " << tech_symbol << "_Type\n"
               "\tPyObject* AdjustmentFactorsModule = PyImport_ImportModule(\"AdjustmentFactors\");\n"
               "\tif (!AdjustmentFactorsModule){\n"
               "\t\tPyErr_SetImportError(PyUnicode_FromString(\"Could not import AdjustmentFactors module.\"), NULL, NULL);\n"
               "\t}\n"
               "\n"
               "\tPyTypeObject* AdjustmentFactors_Type = (PyTypeObject*)PyObject_GetAttrString(AdjustmentFactorsModule, \"AdjustmentFactors\");\n"
               "\tif (!AdjustmentFactors_Type){\n"
               "\t\tPyErr_SetImportError(PyUnicode_FromString(\"Could not import AdjustmentFactors type.\"), NULL, NULL);\n"
               "\t}\n"
               "\tPy_XDECREF(AdjustmentFactorsModule);\n"
               "\n"
               "\tif (PyType_Ready(AdjustmentFactors_Type) < 0) { goto fail; }\n"
               "\tPyDict_SetItemString(" << tech_symbol << "_Type.tp_dict,\n"
               "\t\t\t\t\t\t \"AdjustmentFactors\",\n"
               "\t\t\t\t\t\t (PyObject*)AdjustmentFactors_Type);\n"
               "\tPy_DECREF(&AdjustmentFactors_Type);\n"
               "\tPy_XDECREF(AdjustmentFactors_Type);\n\n";

    // add the group types
    for (size_t i = 0; i < root->vardefs_order.size() ; i++) {
        auto mm = root->m_vardefs.find(root->vardefs_order[i]);
        std::map<std::string, var_def> vardefs = mm->second;

        std::string module_symbol = format_as_symbol(mm->first);
        std::string name = module_symbol;

        if (module_symbol == "AdjustmentFactors")
            continue;

        fx_file << "\t/// Add the " << module_symbol << " type object to " << tech_symbol << "_Type\n"
                   "\tif (PyType_Ready(&" << module_symbol << "_Type) < 0) { goto fail; }\n"
                   "\tPyDict_SetItemString(" << tech_symbol << "_Type.tp_dict,\n"
                   "\t\t\t\t\"" << name << "\",\n"
                   "\t\t\t\t(PyObject*)&" << module_symbol << "_Type);\n"
                   "\tPy_DECREF(&" << module_symbol << "_Type);\n\n";
    }

    // add the tech and close
    fx_file << "\t/// Add the " << tech_symbol << " type object to the module\n"
               "\tif (PyType_Ready(&" << tech_symbol << "_Type) < 0) { goto fail; }\n"
               "\tPyModule_AddObject(m,\n"
               "\t\t\t\t\"" << tech_symbol << "\",\n"
               "\t\t\t\t(PyObject*)&" << tech_symbol << "_Type);\n\n";

    fx_file << "\tif (PySAM_load_lib(m) < 0) goto fail;\n"
               "\tif (PySAM_init_error() < 0) goto fail;\n"
               "\n"
               "\treturn 0;\n"
               "\tfail:\n"
               "\tPy_XDECREF(m);\n"
               "\treturn -1;\n"
               "}\n\n";

    // module slot inits

    fx_file << "static struct PyModuleDef_Slot " << tech_symbol << "Module_slots[] = {\n"
               "\t\t{Py_mod_exec, " << tech_symbol << "Module_exec},\n"
               "\t\t{0, NULL},\n"
               "};\n"
               "\n"
               "static struct PyModuleDef " << tech_symbol << "Module = {\n"
               "\t\tPyModuleDef_HEAD_INIT,\n"
               "\t\t\"" << tech_symbol << "\",\n"
               "\t\tmodule_doc,\n"
               "\t\t0,\n"
               "\t\t" << tech_symbol << "Module_methods,\n"
               "\t\t" << tech_symbol << "Module_slots,\n"
               "\t\tNULL,\n"
               "\t\tNULL,\n"
               "\t\tNULL\n"
               "};\n"
               "\n"
               "/* Export function for the module */\n"
               "\n"
               "PyMODINIT_FUNC\n"
               "PyInit_" << tech_symbol << "(void)\n"
               "{\n"
               "\treturn PyModuleDef_Init(&" << tech_symbol << "Module);\n"
               "}";

    fx_file.close();

    bool print_setuppy = false;
    if (!print_setuppy) return;

    std::cout << "                 Extension('" << tech_symbol << "',\n"
                 "                           ['" << tech_symbol << ".c'],\n"
                 "                           include_dirs=[libpath],\n"
                 "                           library_dirs=[libpath],\n"
                 "                           libraries=libs,\n"
                 "                           extra_link_args=extra_link_args\n"
                 "                           ),\n";

}
