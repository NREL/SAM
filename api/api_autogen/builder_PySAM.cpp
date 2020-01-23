#include <iostream>
#include <set>

#include <shared/lib_util.h>
#include <ssc/ssc_equations.h>

#include "builder_PySAM.h"
#include "builder_generator_helper.h"

std::string all_options_of_cmod(const std::string &cmod_symbol, const std::string& config_name) {
    size_t pos = config_name.find_last_of('-');
    // if not a tech-fin config, it's a single cmod so it won't have any default configuration options
    if (pos == std::string::npos)
        return "";
    std::string tech = config_to_cmod_name[format_as_symbol(config_name.substr(0, pos))];
    std::string fin = config_to_cmod_name[format_as_symbol(config_name.substr(pos+1))];
    assert(tech.length() + fin.length());

    std::set<std::string> config_set;
    for (auto it = SAM_config_to_primary_modules.begin(); it != SAM_config_to_primary_modules.end(); ++it){
        std::vector<std::string> primary_cmods = SAM_config_to_primary_modules[it->first];
        for (const auto& i : primary_cmods) {
            if (format_as_symbol(i) == cmod_symbol){
                config_set.insert(it->first);
                break;
            }
        }
    }

    std::string str = "config options:\\n\\n";
    for (auto it = config_set.begin(); it != config_set.end(); ++it){
        if (it != config_set.begin())
            str += "\\n";
        str += "- \\\"" + format_as_symbol(*it) + "\\\"";
    }
    assert(str.length());
    return str;
}

// in the future, pull this directly from startup.lk but for now just keep this
// maps cmod to string description
std::string module_doc(const std::string& tech_symbol){
    static std::unordered_map<std::string, std::string> desc = {
            {"Battwatts", "Simplified battery storage model"},
            {"Belpe", "Electric load calculator for residential buildings"},
            {"Biomass", "Biomass combustion for electricity generation"},
            {"CashloanModel", "Financial model for residential and commercial behind-the-meter projects"},
            {"Equpartflip", "PPA all equity partnership flip (no debt) financial model"},
            {"Fuelcell", "Fuel cell model"},
            {"GenericSystem", "Basic power system model using either capacity, capacity factor, and heat rate, or an hourly power generation profile as input"},
            {"Geothermal", "Geothermal power model for hydrothermal and EGS systems with flash or binary conversion"},
            {"Grid", "Electric grid model"},
            {"Hcpv", "Concentrating photovoltaic system with a high concentration photovoltaic module model and separate inverter model"},
            {"HostDeveloper", "Third party ownership with PPA financial model from host and developer perspective"},
            {"IphToLcoefcr", "Calculate levelized cost of heat using fixed charge rate method for industrial process heat models"},
            {"Lcoefcr", "Calculate levelized cost of electricity using fixed charge rate method instead of cash flow"},
            {"Levpartflip", "PPA leveraged partnership flip (with debt) financial model"},
            {"LinearFresnelDsgIph", "Linear Fresnel model with steam heat transfer fluid for industrial process heat applications"},
            {"Merchantplant", "Merchant plant providing energy, capacity and ancillary services."},
            {"MhkWave", "Wave marine hydrokinetic system"},
            {"MhkTidal", "Tidal marine hydrokinetic system"},
            {"Pvsamv1", "Detailed photovoltaic system model with separate components for module and inverter"},
            {"Pvwattsv5", "PVWatts photovoltaic system model with simple inputs"},
            {"Pvwattsv5Lifetime", "PVWatts photovoltaic system model for multi-year lifetime analysis"},
            {"Saleleaseback", "PPA sale leaseback partnership financial model"},
            {"Sco2AirCooler", "Supercritical CO2 Power Cycle Air Cooler"},
            {"Sco2CspSystem", "Supercritical CO2 Power Cycle Design and Off-Design Simulation"},
            {"Sco2CspUdPcTables", "Supercritical CO2 Power Cycle"},
            {"Sco2DesignPoint", "Supercritical CO2 Power Cycle Design Point"},
            {"Sco2DesignCycle", "Supercritical CO2 Power Cycle Design"},
            {"Sco2Offdesign", "Supercritical CO2 Power Cycle Off Design"},
            {"Singleowner", "PPA single owner financial model"},
            {"StandAloneBattery", "Detailed battery storage model"},
            {"Swh", "Solar water heating model for residential and commercial building applications"},
            {"TcsdirectSteam", "CSP direct steam power tower model for power generation"},
            {"Tcsdish", "CSP dish-Stirling model with parameters for SES and WGA-ADDS systems for power generation"},
            {"TcsgenericSolar", "CSP power system model with solar field characterized using a table of optical efficiency values"},
            {"Tcsiscc", "CSP molten salt power tower system with a natural gas combined cycle power plant"},
            {"TcslinearFresnel", "Process heat linear direct steam "},
            {"TcsmoltenSalt", "CSP molten salt power tower for power generation"},
            {"TcsMSLF", "CSP linear Fresnel with molten salt heat transfer fluid for power generation"},
            {"TcstroughEmpirical", "CSP parabolic trough model based on empirically-derived coefficients and equations for power generation"},
            {"TroughPhysical", "CSP parabolic trough model based on heat transfer and thermodynamic principles for power generation"},
            {"Thermalrate", "Thermal flat rate structure net revenue calculator"},
            {"Thirdpartyownership", "Third party ownership with PPA or lease agreement financial model from host perspective"},
            {"TroughPhysical", "CSP parabolic trough system using heat transfer and thermodynamic component models"},
            {"TroughPhysicalProcessHeat", "Parabolic trough for industrial process heat applications"},
            {"Utilityrate5", "Retail electricity bill calculator"},
            {"Windpower", "Wind power system with one or more wind turbines"}
    };

    auto it = desc.find(tech_symbol);
    if (it == desc.end())
        return tech_symbol;
    return it->second;
}

std::string get_params_str(const std::string &doc){
    std::string params;
    size_t startpos = doc.find("Input:");
    startpos = doc.find("\\n", startpos);
    size_t endpos = doc.find("Output:");
    startpos = doc.find("'", startpos+2);
    while (startpos < endpos){
        if (params.length() > 0)
            params += ", ";
        size_t word_end = doc.find("'", startpos+1);
        params += doc.substr(startpos+1, word_end - startpos - 1);
        startpos = doc.find("'", doc.find("\\n", word_end));
    }
    return params;
}

void builder_PySAM::create_PySAM_files(const std::string &cmod, const std::string &file_dir) {
    std::string cmod_symbol = format_as_symbol(cmod);

    std::string tech_symbol = cmod_symbol;
    if(cmod_symbol == "Battery")
        tech_symbol = "StandAloneBattery";
    else if (cmod_symbol == "6parsolve")
        tech_symbol = "SixParsolve";
    else if (root->m_vardefs.find(cmod_symbol) != root->m_vardefs.end())
        tech_symbol += "Model";

    std::ofstream fx_file;
    fx_file.open(file_dir + "/modules/" + tech_symbol + ".c");
    assert(fx_file.is_open());

    fx_file << "#include <Python.h>\n"
               "\n"
               "#include <SAM_" << cmod_symbol << ".h>\n"
               "#include <SAM_api.h>\n"
               "\n"
               "#include \"PySAM_utils.h\"\n\n";

    // declare cmod type and import equations if necessary
    if (!root->m_eqn_entries.empty())
        fx_file << "#include \"" << tech_symbol << "_eqns.c\"\n\n";

    // setters, none for outputs
    for (const auto& i : root->vardefs_order) {
        auto mm = root->m_vardefs.find(i);
        std::map<std::string, var_def> vardefs = mm->second;

        bool output = false;
        std::string module_symbol = format_as_symbol(mm->first);

        if (module_symbol == "AdjustmentFactors")
            continue;

        if (module_symbol == "Outputs"){
            output = true;
        }


        fx_file << "\n"
                   "/*\n"
                   " * " << module_symbol << " Group\n"
                                               " */ \n\n";

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
                   "\n";

        // add ssc equations docs for eqns that are under a variable group
        auto group_it = root->m_eqn_entries.find(module_symbol);
        if (group_it != root->m_eqn_entries.end()){
            auto func_map = group_it->second;
            for (const auto& func_it : func_map){
                fx_file << "static const char* const " << func_it.second.name << "_doc = \n";
                fx_file << "\"" << func_it.second.doc << "\";\n\n";
            }
        }

        fx_file << "static PyMethodDef " << module_symbol << "_methods[] = {\n"
                   "\t\t{\"assign\",            (PyCFunction)"
                << module_symbol << "_assign,  METH_VARARGS,\n"
                   "\t\t\tPyDoc_STR(\"assign() -> None\\n Assign attributes from dictionary\\n\\n"
                   "``" << module_symbol << "_vals = { var: val, ...}``\")},\n"
                   "\t\t{\"export\",            (PyCFunction)" << module_symbol
                << "_export,  METH_VARARGS,\n"
                   "\t\t\tPyDoc_STR(\"export() -> dict\\n Export attributes into dictionary\")},\n";

        // add ssc equations as methods under the variable group
        if (group_it != root->m_eqn_entries.end()){
            auto func_map = group_it->second;
            for (const auto& func_it : func_map){
                fx_file << "\t\t{\"" << func_it.first << "\", (PyCFunction)" << func_it.second.name;
                fx_file << ", METH_VARARGS | METH_KEYWORDS,\n"
                           "\t\t\t" << func_it.second.name << "_doc},\n";
            }
        }

        fx_file << "\t\t{NULL,              NULL}           /* sentinel */\n"
                   "};\n\n";

        // variable getter and setter (setters if not output)
        for (const auto& it : vardefs) {
            std::string var_symbol = it.first;

            var_def vd = it.second;

            if (module_symbol == "AdjustmentFactors")
                continue;


            if (vd.type == "number") {
                fx_file << "static PyObject *\n"
                        << module_symbol << "_get_" << var_symbol << "(" << module_symbol
                        << "Object *self, void *closure)\n"
                           "{\n"
                           "\treturn PySAM_double_getter(SAM_" << cmod_symbol << "_" << module_symbol << "_" << var_symbol
                        << "_nget, self->data_ptr);\n"
                           "}\n"
                           "\n";
                if (output)
                    continue;
                fx_file << "static int\n"
                        << module_symbol << "_set_" << var_symbol << "(" << module_symbol
                        << "Object *self, PyObject *value, void *closure)\n"
                           "{\n"
                           "\treturn PySAM_double_setter(value, SAM_" << cmod_symbol << "_" << module_symbol << "_"
                        << var_symbol << "_nset, self->data_ptr);\n"
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
                           "\treturn PySAM_array_setter(value, SAM_" << cmod_symbol << "_" << module_symbol << "_"
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
        for (auto& it : vardefs) {
            std::string var_symbol = it.first;

            var_def vd = it.second;

            if (module_symbol == "AdjustmentFactors")
                continue;

            // make the PyGetSetDef struct

            std::vector<std::string> ssctype_str = {"None", "str", "float", "sequence", "sequence[sequence]", "dict"};

            std::string doc = "*" + ssctype_str[vd.type_n] + "*";
            if (vd.doc.length() > 0)
                doc += ": " + vd.doc;

            fx_file << "{\"" << var_symbol << "\", (getter)" << module_symbol << "_get_" << var_symbol << ",";

            if (output)
                fx_file << "(setter)0,\n";
            else{
                if (vd.meta.length() > 0){
                    doc += "\\n\\n";
                    if (vd.meta.find('=') != std::string::npos)
                        doc += "*Options*: " + vd.meta ;
                    else
                        doc += "*Info*: " + vd.meta;
                }

                if (vd.constraints.length() > 0) {
                    doc += "\\n\\n";
                    doc += "*Constraints*: ";
                    doc += vd.constraints;
                }

                if (vd.reqif.length() > 0) {
                    doc += "\\n\\n*Required*: ";
                    if (vd.reqif == "*"){
                        doc += "True";
                    }
                    else if (vd.reqif == "?")
                        doc += "False";
                    else{
                        // "?=x" means if not provided set to x
                        size_t pos = vd.reqif.find("?=");
                        if (pos != std::string::npos){
                            doc += "If not provided, assumed to be " + vd.reqif.substr(pos+2);
                        }
                        else{
                            pos = vd.reqif.find('=');
                            if (pos != std::string::npos){
                                doc += "True if " + vd.reqif;

                            } else
                                doc += vd.reqif + "";
                        }
                    }
                }

                fx_file << "(setter)" << module_symbol << "_set_" << var_symbol << ",\n";
            }

            fx_file << "\tPyDoc_STR(\"" << doc << "\"),\n \tNULL},\n";

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
                      "\t\t0,                          /*tp_weaklistofnset*/\n"
                      "\t\t0,                          /*tp_iter*/\n"
                      "\t\t0,                          /*tp_iternext*/\n"
                      "\t\t" << module_symbol << "_methods,         /*tp_methods*/\n"
                      "\t\t0,                          /*tp_members*/\n"
                      "\t\t" << module_symbol << "_getset,          /*tp_getset*/\n"
                      "\t\t0,                          /*tp_base*/\n"
                      "\t\t0,                          /*tp_dict*/\n"
                      "\t\t0,                          /*tp_descr_get*/\n"
                      "\t\t0,                          /*tp_descr_set*/\n"
                      "\t\t0,                          /*tp_dictofnset*/\n"
                      "\t\t0,                          /*tp_init*/\n"
                      "\t\t0,                          /*tp_alloc*/\n"
                      "\t\t0,             /*tp_new*/\n"
                      "\t\t0,                          /*tp_free*/\n"
                      "\t\t0,                          /*tp_is_gc*/\n"
                      "};\n\n";
    }

    // define the technology

    fx_file <<  "/*\n"
                " * " << tech_symbol << "\n"
                " */\n"
                "\n"
                "typedef struct {\n"
                "\tPyObject_HEAD\n"
                "\tPyObject            *x_attr;        /* Attributes dictionary */\n"
                "\tSAM_" << cmod_symbol << "   data_ptr;\n"
                "} " << tech_symbol << "Object;\n"
                "\n"
                "static PyTypeObject " << tech_symbol << "_Type;\n\n";

    fx_file << "static " << tech_symbol << "Object *\n"
               "new" << tech_symbol << "Object(void* data_ptr)\n"
               "{\n"
               "\t" << tech_symbol << "Object *self;\n"
               "\tself = PyObject_New(" << tech_symbol << "Object, &" << tech_symbol << "_Type);\n"
               "\n"
               "\tPySAM_TECH_ATTR(\"" << tech_symbol << "\", SAM_" << cmod_symbol << "_construct)\n\n";

    // add the group types
    for (auto& i : root->vardefs_order) {
        auto mm = root->m_vardefs.find(i);
        std::map<std::string, var_def> vardefs = mm->second;

        std::string module_symbol = format_as_symbol(mm->first);

        if (module_symbol == "AdjustmentFactors"){
            fx_file << "\tPyObject* AdjustmentFactorsModule = PyImport_ImportModule(\"AdjustmentFactors\");\n"
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
                       "\tPy_DECREF(Adjust_obj);\n\n";
        }
        else{
            fx_file << "\tPyObject* " << module_symbol << "_obj = " << module_symbol << "_new(self->data_ptr);\n"
                       "\tPyDict_SetItemString(attr_dict, \"" << module_symbol << "\", " << module_symbol << "_obj);\n"
                       "\tPy_DECREF(" << module_symbol << "_obj);\n\n";
        }
    }
    
    //  and close

    fx_file << "\n\treturn self;\n"
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
               "\tif (!PySAM_assign_from_nested_dict((PyObject*)self, self->x_attr, self->data_ptr, dict, \"" << cmod_symbol << "\"))\n"
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
               "\n";

    // add ssc equations docs for eqns that are under the cmod class
    auto cmod_it = root->m_eqn_entries.find(cmod_symbol);
    if (cmod_it != root->m_eqn_entries.end()){
        auto func_map = cmod_it->second;
        for (const auto& func_it : func_map){
            fx_file << "static const char* const " << func_it.second.name << "_doc = \n";
            fx_file << "\"" << func_it.second.doc << "\";\n\n";
        }
    }

    fx_file << "static PyMethodDef " << tech_symbol << "_methods[] = {\n"
               "\t\t{\"execute\",            (PyCFunction)" << tech_symbol << "_execute,  METH_VARARGS,\n"
               "\t\t\t\tPyDoc_STR(\"execute(int verbosity) -> None\\n Execute simulation with verbosity level 0 (default) or 1\")},\n"
               "\t\t{\"assign\",            (PyCFunction)" << tech_symbol << "_assign,  METH_VARARGS,\n"
               "\t\t\t\tPyDoc_STR(\"assign(dict) -> None\\n Assign attributes from nested dictionary, except for Outputs\\n\\n"
               "``nested_dict = { '" << root->vardefs_order[0] << "': { var: val, ...}, ...}``"
               "\")},\n"
               "\t\t{\"export\",            (PyCFunction)" << tech_symbol << "_export,  METH_VARARGS,\n"
               "\t\t\t\tPyDoc_STR(\"export() -> dict\\n Export attributes into nested dictionary\")},\n";

    // add ssc equations as methods under the cmod class
    if (cmod_it != root->m_eqn_entries.end()){
        auto func_map = cmod_it->second;
        for (const auto& func_it : func_map){
            fx_file << "\t\t{\"" << func_it.first << "\", (PyCFunction)" << func_it.second.name;
            fx_file << ", METH_VARARGS | METH_KEYWORDS,\n"
                       "\t\t\t" << func_it.second.name << "_doc},\n";
        }
    }

    fx_file << "\t\t{NULL,              NULL}           /* sentinel */\n"
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
               "\t\t\"This class contains all the variable information for running a simulation. Variables are grouped"
               " together in the subclasses as properties. If property assignments are the wrong type, an error is thrown."
               "\",        /*tp_doc*/\n"
               "\t\t0,                          /*tp_traverse*/\n"
               "\t\t0,                          /*tp_clear*/\n"
               "\t\t0,                          /*tp_richcompare*/\n"
               "\t\t0,                          /*tp_weaklistofnset*/\n"
               "\t\t0,                          /*tp_iter*/\n"
               "\t\t0,                          /*tp_iternext*/\n"
               "\t\t" << tech_symbol << "_methods,      /*tp_methods*/\n"
               "\t\t0,                          /*tp_members*/\n"
               "\t\t0,       /*tp_getset*/\n"
               "\t\t0,                          /*tp_base*/\n"
               "\t\t0,                          /*tp_dict*/\n"
               "\t\t0,                          /*tp_descr_get*/\n"
               "\t\t0,                          /*tp_descr_set*/\n"
               "\t\t0,                          /*tp_dictofnset*/\n"
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
               "\tlong long int ptr = 0;  // 64 bit arch\n"
               "\tif (!PyArg_ParseTuple(args, \"L:wrap\", &ptr)){\n"
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
               "\tchar* def = 0;\n"
               "\tif (!PyArg_ParseTuple(args, \"s:default\", &def)){\n"
               "\t\tPyErr_BadArgument();\n"
               "\t\treturn NULL;\n"
               "\t}\n"
               "\trv = new" << tech_symbol << "Object(0);\n"
               "\tif (rv == NULL)\n"
               "\t\treturn NULL;\n"
               "\n"
               "\tPySAM_load_defaults((PyObject*)rv, rv->x_attr, rv->data_ptr, \"" << cmod_symbol << "\", def);\n"
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
               "\t\t\t\tPyDoc_STR(\"new() -> " << tech_symbol << "\")},\n"
               "\t\t{\"default\",             " << tech_symbol << "_default,         METH_VARARGS,\n"
               "\t\t\t\tPyDoc_STR(\"default(config) -> " << tech_symbol << "\\n\\nUse financial model-specific default attributes\\n\"\n"
                                                                                  "\t\t\t\t\"" << all_options_of_cmod(cmod_symbol, config_name) << "\")},\n"
               "\t\t{\"wrap\",             " << tech_symbol << "_wrap,         METH_VARARGS,\n"
               "\t\t\t\tPyDoc_STR(\"wrap(ssc_data_t) -> " << tech_symbol << "\\n\\nUse existing PySSC data\\n\\n.. warning::\\n\\n"
                                                                                "\tDo not call PySSC.data_free on the ssc_data_t provided to ``wrap``\")},\n"
               "\t\t{NULL,              NULL}           /* sentinel */\n"
               "};\n"
               "\n"
               "PyDoc_STRVAR(module_doc,\n"
               "\t\t\t \"" << module_doc(tech_symbol) << "\");\n\n\n";

    // define the execution of module and adjustmentfactors type
    fx_file << "static int\n"
               "" << tech_symbol << "Module_exec(PyObject *m)\n"
               "{\n"
               "\t/* Finalize the type object including setting type of the new type\n"
               "\t * object; doing it here is required for portability, too. */\n"
               "\n"
               "\tif (PySAM_load_lib(m) < 0) goto fail;\n"
               "\tif (PySAM_init_error(m) < 0) goto fail;\n"
               "\n"
               "\t" << tech_symbol << "_Type.tp_dict = PyDict_New();\n"
               "\tif (!" << tech_symbol << "_Type.tp_dict) { goto fail; }\n"
               "\n";

    if (root->m_vardefs.find("AdjustmentFactors") != root->m_vardefs.end()){
        fx_file << "\t/// Add the AdjustmentFactors type object to " << tech_symbol << "_Type\n"
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
    }


    // add the group types
    for (auto& i : root->vardefs_order) {
        auto mm = root->m_vardefs.find(i);
        std::map<std::string, var_def> vardefs = mm->second;

        std::string module_symbol = format_as_symbol(mm->first);

        if (module_symbol == "AdjustmentFactors")
            continue;

        fx_file << "\t/// Add the " << module_symbol << " type object to " << tech_symbol << "_Type\n"
                   "\tif (PyType_Ready(&" << module_symbol << "_Type) < 0) { goto fail; }\n"
                   "\tPyDict_SetItemString(" << tech_symbol << "_Type.tp_dict,\n"
                   "\t\t\t\t\"" << module_symbol << "\",\n"
                   "\t\t\t\t(PyObject*)&" << module_symbol << "_Type);\n"
                   "\tPy_DECREF(&" << module_symbol << "_Type);\n\n";
    }

    // add the tech and close
    fx_file << "\t/// Add the " << tech_symbol << " type object to the module\n"
               "\tif (PyType_Ready(&" << tech_symbol << "_Type) < 0) { goto fail; }\n"
               "\tPyModule_AddObject(m,\n"
               "\t\t\t\t\"" << tech_symbol << "\",\n"
               "\t\t\t\t(PyObject*)&" << tech_symbol << "_Type);\n\n";

    fx_file << "\treturn 0;\n"
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

    // export .rst documentation files for sphinx

    fx_file.open(file_dir + "/docs/modules/" + tech_symbol + ".rst");
    assert(fx_file.is_open());

    fx_file << ".. _" << tech_symbol << ":\n\n";

    fx_file << cmod_symbol << "\n**************************\n\n";

    std::string cmod_doc = "Wrapper for SAM Simulation Core model: `cmod_" + cmod;
    cmod_doc += ".cpp <https://github.com/NREL/ssc/blob/develop/ssc/cmod_" + util::lower_case(cmod) + ".cpp>`_\n\n";

    fx_file << cmod_doc;

    fx_file << "Creating an Instance\n=========================\n\n"
               "There are three methods to create a new instance of a PySAM module. Using ``default`` populates the new"
               "class' attributes with default values specific to a ``config``. Each technology-financial"
               "configuration corresponds to a SAM GUI configuration. Using ``new`` creates an instance with empty "
               "attributes. The ``wrap`` function allows compatibility with PySSC, for details, refer to :doc:`../PySSC`.\n\n"
               "**" << tech_symbol << " model description**\n\n";

    fx_file << ".. automodule:: PySAM." << tech_symbol << "\n";
    fx_file << "\t:members:\n\n";

    fx_file << "Functions\n=========================\n\n"
               ".. autoclass:: PySAM." << tech_symbol << "." << tech_symbol << "\n\t:members:\n\n";

    for (const auto& i : root->vardefs_order) {
        auto mm = root->m_vardefs.find(i);
        std::map<std::string, var_def> vardefs = mm->second;

        std::string module_symbol = format_as_symbol(mm->first);

        fx_file << module_symbol << " Group\n==============\n\n";
        fx_file << ".. autoclass:: PySAM." << tech_symbol << "." << tech_symbol << "." << module_symbol << "\n";
        fx_file << "\t:members:\n\n";
    }

    fx_file.close();

    // export .pyi stub files for static typing

    fx_file.open(file_dir + "/stubs/" + tech_symbol + ".pyi");
    assert(fx_file.is_open());

    for (const auto& i : root->vardefs_order) {
        auto mm = root->m_vardefs.find(i);
        std::map<std::string, var_def> vardefs = mm->second;

        std::string module_symbol = format_as_symbol(mm->first);

        if (module_symbol == "AdjustmentFactors"){
            fx_file << "class AdjustmentFactors(object):\n"
                       "\tdef assign(self): \n"
                       "\t\tpass\n"
                       "\n"
                       "\tdef export(self): \n"
                       "\t\treturn {}\n"
                       "\n"
                       "\tdef __init__(self, *args, **kwargs): # real signature unknown\n"
                       "\t\tpass\n"
                       "\n"
                       "\tconstant = float\n"
                       "\tdc_constant = float\n"
                       "\tdc_hourly = tuple\n"
                       "\tdc_periods = tuple\n"
                       "\thourly = tuple\n"
                       "\tperiods = tuple\n"
                       "\tsf_constant = float\n"
                       "\tsf_hourly = tuple\n"
                       "\tsf_periods = tuple\n\n";
            continue;
        }

        fx_file << "class " << module_symbol << "(object):\n";
        fx_file << "\tdef assign(self): \n"
                   "\t\tpass\n"
                   "\n"
                   "\tdef export(self) -> Dict[Dict]\n"
                   "\t\tpass\n"
                   "\n"
                   "\tdef __init__(self, *args, **kwargs): \n"
                   "\t\tpass\n\n";

        // add ssc equations
        auto group_it = root->m_eqn_entries.find(module_symbol);
        if (group_it != root->m_eqn_entries.end()){
            auto func_map = group_it->second;
            for (const auto& func_it : func_map){
                fx_file << "\tdef "<< func_it.first << "(self, " << get_params_str(func_it.second.doc) << "):\n\t\tpass\n";
            }
        }
        fx_file << "\n";

        std::vector<std::string> statictype_str = {"None", "str", "float", "tuple", "tuple", "dict"};

        for (const auto& it : vardefs) {
            std::string var_symbol = it.first;
            fx_file << "\t" << var_symbol << " = " << statictype_str[it.second.type_n] << "\n";
        }
        fx_file << "\n\n";
    }

    fx_file << "class " << tech_symbol << "(object):\n";
    fx_file << "\tdef assign(self, dict):\n"
               "\t\tpass\n"
               "\n"
               "\tdef execute(self, int_verbosity):\n"
               "\t\tpass\n"
               "\n"
               "\tdef export(self):\n"
               "\t\tpass\n"
               "\n"
               "\tdef __getattribute__(self, *args, **kwargs):\n"
               "\t\tpass\n"
               "\n"
               "\tdef __init__(self, *args, **kwargs):\n"
               "\t\tpass\n\n";

    for (const auto& i : root->vardefs_order) {
        auto mm = root->m_vardefs.find(i);
        std::map<std::string, var_def> vardefs = mm->second;

        std::string module_symbol = format_as_symbol(mm->first);

        fx_file << "\t" << module_symbol << " = " << module_symbol << "\n";
    }
    fx_file << "\n\n";

    fx_file << "def default(config) -> " << tech_symbol <<"\n"
               "\tpass\n"
               "\n"
               "def new() -> " << tech_symbol << "\n"
               "\tpass\n"
               "\n"
               "def wrap(ssc_data_t) -> " << tech_symbol << "\n"
               "\tpass\n"
               "\n"
               "__loader__ = None \n"
               "\n"
               "__spec__ = None\n";
    fx_file.close();
}
