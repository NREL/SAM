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

#include <iostream>
#include <set>

#include <shared/lib_util.h>
#include <ssc/ssc_equations.h>

#include "library_extractor.h"
#include "builder_PySAM.h"
#include "builder_generator_helper.h"


// in the future, pull this directly from startup.lk but for now just keep this
// maps cmod to string description
std::string module_doc(const std::string& tech_symbol){
    static std::unordered_map<std::string, std::string> desc = {
            {"Battery", "Detailed battery storage model"},
            {"BatteryStateful", "BatteryStateful has two major differences from the Battery module: 1) it contains only the “physical” component models of the battery (thermal, voltage, capacity, lifetime) and none of the dispatch methods (peak shaving, etc) of the Battery module; 2) the Battery module runs annual or multi-year simulations in a single execution, whereas BatteryStateful is run one timestep at a time using control variables, current or power, and can be run at sub-minute timesteps."},
            {"Battwatts", "Simplified battery storage model"},
            {"Belpe", "Electric load calculator for residential buildings"},
            {"Biomass", "Biomass combustion for electricity generation"},
            {"CashloanModel", "Financial model for residential and commercial behind-the-meter projects"},
            {"Communitysolar", "Community solar owner financial model"},
            {"Equpartflip", "PPA all equity partnership flip (no debt) financial model"},
            {"ETES", "Electric thermal energy storage"},
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
			{"Pvwattsv8", "Photovoltaic system using basic NREL PVWatts V8 algorithm. Does not do detailed degradation or loss modeling. If those are important, please use pvsamv1."},
            {"Saleleaseback", "PPA sale leaseback partnership financial model"},
            {"Sco2AirCooler", "Supercritical CO2 Power Cycle Air Cooler"},
            {"Sco2CspSystem", "Supercritical CO2 Power Cycle Design and Off-Design Simulation"},
            {"Sco2CspUdPcTables", "Supercritical CO2 Power Cycle"},
            {"Singleowner", "PPA single owner financial model"},
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
            {"WaveFileReader", "Load wave resource data from file. Data can be in either probability distribution format or 3-hour time series arrays"},
            {"Windpower", "Wind power system with one or more wind turbines"}
    };

    auto it = desc.find(tech_symbol);
    if (it == desc.end())
        return tech_symbol;
    return it->second;
}

bool check_inputs_consistent(const std::string &tech_symbol){
    if (tech_symbol == "TcsmoltenSalt" || tech_symbol == "TroughPhysical" || tech_symbol == "TroughPhysicalProcessHeat")
        return true;
    else
        return false;
}

std::string get_params_str(const std::string &doc){
    std::string params;
    size_t startpos = doc.find("Input:");
    startpos = doc.find("\\n", startpos);
    size_t endpos = doc.find("Output:");
    startpos = doc.find('\'', startpos+2);
    while (startpos < endpos){
        if (params.length() > 0)
            params += ", ";
        size_t word_end = doc.find('\'', startpos+1);
        params += doc.substr(startpos+1, word_end - startpos - 1);
        startpos = doc.find('\'', doc.find("\\n", word_end));
    }
    return params;
}

void builder_PySAM::set_config_options(const std::set<std::string>& configs) {
    for (const auto& i : configs)
        config_options.insert(i);
}

std::string builder_PySAM::get_config_options() {
    if (config_options.empty())
        return "None";
    std::string config_str = "`config` options:\\n\\n";
    for (auto it = config_options.begin(); it != config_options.end(); ++it){
        if (it != config_options.begin())
            config_str += "\\n";
        config_str += "- \\\"" + format_as_symbol(*it) + "\\\"";
    }
    assert(config_str.length());
    return config_str;
}


void builder_PySAM::all_options_of_cmod(const std::string &cmod) {
    std::string cmod_symbol = format_as_symbol(cmod);
    size_t pos = config_name.find_last_of('-');
    // if not a tech-fin config, it's a single cmod so it won't have any default configuration options
    if (pos == std::string::npos)
        return;
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

    set_config_options(config_set);
}

void builder_PySAM::create_PySAM_files(const std::string &cmod, const std::string &file_dir, bool stateful) {
    std::string cmod_symbol = format_as_symbol(cmod);

    std::string tech_symbol = cmod_symbol;
    if (cmod_symbol == "6parsolve")
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
        std::string group_symbol = format_as_symbol(mm->first);

        if (group_symbol == "AdjustmentFactors")
            continue;

        if (group_symbol == "Outputs"){
            output = true;
            if (vardefs.empty())
                continue;
        }


        fx_file << "\n"
                   "/*\n"
                   " * " << group_symbol << " Group\n"
                                               " */ \n\n";

        // group description as object

        fx_file << "static PyTypeObject " << group_symbol << "_Type;\n\n";
        fx_file << "static PyObject *\n"
                << group_symbol << "_new(SAM_" << cmod_symbol << " data_ptr)\n"
                   "{\n"
                   "\tPyObject* new_obj = " << group_symbol << "_Type.tp_alloc(&" << group_symbol << "_Type,0);\n"
                   "\n"
                   "\tVarGroupObject* " << group_symbol << "_obj = (VarGroupObject*)new_obj;\n"
                   "\n"
                   "\t" << group_symbol << "_obj->data_ptr = (SAM_table)data_ptr;\n"
                   "\n"
                   "\treturn new_obj;\n"
                   "}\n\n";

        fx_file << "/* " << group_symbol << " methods */\n\n";

        // group methods

        fx_file << "static PyObject *\n"
                << group_symbol << "_assign(VarGroupObject *self, PyObject *args)\n"
                   "{\n"
                   "\tPyObject* dict;\n"
                   "\tif (!PyArg_ParseTuple(args, \"O:assign\", &dict)){\n"
                   "\t\treturn NULL;\n"
                   "\t}\n"
                   "\n"
                   "\tif (!PySAM_assign_from_dict(self->data_ptr, dict, \""
                   << cmod_symbol << "\", \"" << group_symbol << "\")){\n"
                   "\t\treturn NULL;\n"
                   "\t}\n"
                   "\n"
                   "\tPy_INCREF(Py_None);\n"
                   "\treturn Py_None;\n"
                   "}\n\n";
        fx_file << "static PyObject *\n"
                << group_symbol << "_replace(VarGroupObject *self, PyObject *args)\n"
                   "{\n"
                   "\tPyObject* dict;\n"
                   "\tif (!PyArg_ParseTuple(args, \"O:assign\", &dict)){\n"
                   "\t\treturn NULL;\n"
                   "\t}\n"
                   "\tPyTypeObject* tp = &" << group_symbol << "_Type;\n\n"
                   "\tif (!PySAM_replace_from_dict(tp, self->data_ptr, dict, \""
                   << cmod_symbol << "\", \"" << group_symbol << "\")){\n"
                   "\t\treturn NULL;\n"
                   "\t}\n\n"
                   "\tPy_INCREF(Py_None);\n"
                   "\treturn Py_None;\n"
                   "}\n\n";
        fx_file << "static PyObject *\n"
                << group_symbol << "_export(VarGroupObject *self, PyObject *args)\n"
                   "{\n"
                   "\tPyTypeObject* tp = &" << group_symbol << "_Type;\n"
                   "\tPyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);\n"
                   "\treturn dict;\n"
                   "}\n"
                   "\n";


        fx_file << "static PyMethodDef " << group_symbol << "_methods[] = {\n"
                   "\t\t{\"assign\",            (PyCFunction)" << group_symbol << "_assign,  METH_VARARGS,\n"
                   "\t\t\tPyDoc_STR(\"assign(dict) -> None\\n Assign attributes from dictionary, overwriting but not removing values\\n\\n"
                   "``" << group_symbol << "_vals = { var: val, ...}``\")},\n"
                   "\t\t{\"replace\",            (PyCFunction)" << group_symbol << "_replace,  METH_VARARGS,\n"
                   "\t\t\tPyDoc_STR(\"replace(dict) -> None\\n Replace attributes from dictionary, unassigning values not present in input dict\\n\\n"
                   "``" << group_symbol << "_vals = { var: val, ...}``\")},\n"
                   "\t\t{\"export\",            (PyCFunction)" << group_symbol << "_export,  METH_VARARGS,\n"
                   "\t\t\tPyDoc_STR(\"export() -> dict\\n Export attributes into dictionary\")},\n";


        // add ssc equations as methods under the variable group
        auto group_it = root->m_eqn_entries.find(group_symbol);
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

            if (group_symbol == "AdjustmentFactors")
                continue;


            if (vd.type == "number") {
                fx_file << "static PyObject *\n"
                        << group_symbol << "_get_" << var_symbol << "(VarGroupObject *self, void *closure)\n"
                           "{\n"
                           "\treturn PySAM_double_getter(SAM_" << cmod_symbol << "_" << group_symbol << "_" << var_symbol
                        << "_nget, self->data_ptr);\n"
                           "}\n"
                           "\n";
                if (output)
                    continue;
                fx_file << "static int\n"
                        << group_symbol << "_set_" << var_symbol << "(VarGroupObject *self, PyObject *value, void *closure)\n"
                           "{\n"
                           "\treturn PySAM_double_setter(value, SAM_" << cmod_symbol << "_" << group_symbol << "_"
                        << var_symbol << "_nset, self->data_ptr);\n"
                           "}\n\n";
            } else if (vd.type == "string") {
                fx_file << "static PyObject *\n"
                        << group_symbol << "_get_" << var_symbol << "(VarGroupObject *self, void *closure)\n"
                           "{\n"
                           "\treturn PySAM_string_getter(SAM_" << cmod_symbol << "_" << group_symbol << "_"
                        << var_symbol << "_sget, self->data_ptr);\n"
                           "}\n"
                           "\n";
                if (output)
                    continue;
                fx_file << "static int\n"
                        << group_symbol << "_set_" << var_symbol << "(VarGroupObject *self, PyObject *value, void *closure)\n"
                           "{\n"
                           "\treturn PySAM_string_setter(value, SAM_" << cmod_symbol << "_" << group_symbol << "_"
                        << var_symbol << "_sset, self->data_ptr);\n"
                           "}\n\n";
            } else if (vd.type == "array") {
                fx_file << "static PyObject *\n"
                        << group_symbol << "_get_" << var_symbol << "(VarGroupObject *self, void *closure)\n"
                           "{\n"
                           "\treturn PySAM_array_getter(SAM_" << cmod_symbol << "_" << group_symbol << "_" << var_symbol
                        << "_aget, self->data_ptr);\n"
                           "}\n"
                           "\n";
                if (output)
                    continue;
                fx_file << "static int\n"
                        << group_symbol << "_set_" << var_symbol << "(VarGroupObject *self, PyObject *value, void *closure)\n"
                           "{\n"
                           "\treturn PySAM_array_setter(value, SAM_" << cmod_symbol << "_" << group_symbol << "_"
                        << var_symbol << "_aset, self->data_ptr);\n"
                           "}\n\n";
            } else if (vd.type == "matrix") {
                fx_file << "static PyObject *\n"
                        << group_symbol << "_get_" << var_symbol << "(VarGroupObject *self, void *closure)\n"
                           "{\n"
                           "\treturn PySAM_matrix_getter(SAM_" << cmod_symbol << "_" << group_symbol << "_" << var_symbol
                        << "_mget, self->data_ptr);\n"
                           "}\n"
                           "\n";
                if (output)
                    continue;
                fx_file << "static int\n"
                        << group_symbol << "_set_" << var_symbol << "(VarGroupObject *self, PyObject *value, void *closure)\n"
                           "{\n"
                           "\t\treturn PySAM_matrix_setter(value, SAM_" << cmod_symbol << "_" << group_symbol << "_"
                        << var_symbol << "_mset, self->data_ptr);\n"
                           "}\n\n";
            } else if (vd.type == "table") {
                fx_file << "static PyObject *\n"
                           "" << group_symbol << "_get_" << var_symbol << "(VarGroupObject *self, void *closure)\n"
                           "{\n"
                           "\treturn PySAM_table_getter(SAM_" << cmod_symbol << "_" << group_symbol << "_" << var_symbol
                        << "_tget, self->data_ptr);\n"
                           "}\n"
                           "\n";
                if (output)
                    continue;
                fx_file << "static int\n"
                           "" << group_symbol << "_set_" << var_symbol << "(VarGroupObject *self, PyObject *value, void *closure)\n"
                           "{\n"
                           "\treturn PySAM_table_setter(value, SAM_" << cmod_symbol << "_" << group_symbol << "_"
                        << var_symbol << "_tset, self->data_ptr);\n"
                           "}\n\n";
            } else {
                throw std::runtime_error(vd.type + " for " + var_symbol);
            }
        }

        fx_file << "static PyGetSetDef " << group_symbol << "_getset[] = {\n";
        for (auto& it : vardefs) {
            std::string var_symbol = it.first;

            var_def vd = it.second;

            if (group_symbol == "AdjustmentFactors")
                continue;

            // make the PyGetSetDef struct

            std::vector<std::string> ssctype_str = {"None", "str", "float", "sequence", "sequence[sequence]", "dict"};

            std::string doc = "*" + ssctype_str[vd.type_n] + "*";
            if (vd.doc.length() > 0)
                doc += ": " + vd.doc;

            fx_file << "{\"" << var_symbol << "\", (getter)" << group_symbol << "_get_" << var_symbol << ",";

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

                if (!check_inputs_consistent(tech_symbol)) {
                    if (!vd.downstream.empty()) {
                        doc += "\\n\\n";
                        doc += "*Changes to this variable may require updating the values of the following*: \\n";
                        for (const auto &ds: vd.downstream)
                            doc += "\\t - " + ds + "\\n";
                    }

                    if (!vd.upstream.empty()) {
                        doc += "\\n\\n";
                        doc += "*This variable may need to be updated if the values of the following have changed*: \\n";
                        for (const auto &ds: vd.upstream)
                            doc += "\\t - " + ds + "\\n";
                    }
                }

                fx_file << "(setter)" << group_symbol << "_set_" << var_symbol << ",\n";
            }

            fx_file << "\tPyDoc_STR(\"" << doc << "\"),\n \tNULL},\n";

        }
        fx_file << "\t{NULL}  /* Sentinel */\n};\n\n";


        // define the module type
        fx_file << "static PyTypeObject " << group_symbol << "_Type = {\n"
                      "\t\t/* The ob_type field must be initialized in the module init function\n"
                      "\t\t * to be portable to Windows without using C++. */\n"
                      "\t\tPyVarObject_HEAD_INIT(NULL, 0)\n"
                      "\t\t\"" << tech_symbol << "." << group_symbol << "\",             /*tp_name*/\n"
                      "\t\tsizeof(VarGroupObject),          /*tp_basicsize*/\n"
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
                      "\t\t" << group_symbol << "_methods,         /*tp_methods*/\n"
                      "\t\t0,                          /*tp_members*/\n"
                      "\t\t" << group_symbol << "_getset,          /*tp_getset*/\n"
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
    std::string cmod_type = stateful ? "CmodStateful" : "Cmod";
    std::string object_type = cmod_type + "Object";

    fx_file <<  "/*\n"
                " * " << tech_symbol << "\n"
                " */\n"
                "\n"
                "static PyTypeObject " << tech_symbol << "_Type;\n\n";

    fx_file << "static " << object_type << " *\n"
               "new" << tech_symbol << "Object(void* data_ptr)\n"
               "{\n"
               "\t" << object_type << " *self;\n"
               "\tself = PyObject_New(" << object_type << ", &" << tech_symbol << "_Type);\n"
               "\n";
    fx_file << "\tPySAM_TECH_ATTR()\n\n";

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
                       "\t\tPyErr_SetString(PyExc_Exception, \"Couldn't create AdjustmentFactorsObject\\n\");\n"
                       "\t\treturn NULL;\n"
                       "\t}\n"
                       "\n"
                       "\tPyDict_SetItemString(attr_dict, \"AdjustmentFactors\", Adjust_obj);\n"
                       "\tPy_DECREF(Adjust_obj);\n\n";
        }
        else if (vardefs.empty()) {
            continue;
        }
        else{
            fx_file << "\tPyObject* " << module_symbol << "_obj = " << module_symbol << "_new(self->data_ptr);\n"
                       "\tPyDict_SetItemString(attr_dict, \"" << module_symbol << "\", " << module_symbol << "_obj);\n"
                       "\tPy_DECREF(" << module_symbol << "_obj);\n\n";
        }
    }

    //  and close

    fx_file << "\treturn self;\n"
               "}\n\n";

    // add methods
    fx_file << "/* " << tech_symbol << " methods */\n"
               "\n"
               "static void\n"
               "" << tech_symbol << "_dealloc(" << object_type << " *self)\n"
               "{\n"
               "\tPy_XDECREF(self->x_attr);\n\n"
               "\tif (!self->data_owner_ptr) {\n"
               "\t\tSAM_error error = new_error();\n"
               "\t\tSAM_table_destruct(self->data_ptr, &error);\n"
               "\t\tPySAM_has_error(error);\n\t}\n";

    if (stateful) {
        fx_file << "\tif (self->cmod_ptr) {\n"
                   "\t\tSAM_error error = new_error();\n"
                   "\t\tSAM_module_destruct(self->cmod_ptr, &error);\n"
                   "\t\tPySAM_has_error(error);\n\t}\n";
    }

    fx_file << "\tPyObject_Del(self);\n"
               "}\n\n\n";

    if (stateful) {
        fx_file << "static PyObject *\n"
                   "" << tech_symbol << "_setup(" << object_type << " *self, PyObject *args)\n"
                   "{\n\tSAM_error error = new_error();\n"
                   "\tself->cmod_ptr = SAM_" << cmod_symbol << "_setup(self->data_ptr, &error);\n"
                   "\tif (PySAM_has_error(error )) return NULL;\n"
                   "\tPy_INCREF(Py_None);\n"
                   "\treturn Py_None;\n}\n\n\n";
        fx_file << "static PyObject *\n"
                   "" << tech_symbol << "_execute(" << object_type << " *self, PyObject *args)\n"
                   "{\n\tint verbosity = 0;\n\n"
                   "\tif (!PyArg_ParseTuple(args, \"|i\", &verbosity))\n"
                   "\t\treturn NULL;\n\n"
                   "\tSAM_error error = new_error();\n"
                   "\tSAM_stateful_module_exec(self->cmod_ptr, self->data_ptr, verbosity, &error);\n"
                   "\tif (PySAM_has_error(error )) return NULL;\n"
                   "\tPy_INCREF(Py_None);\n"
                   "\treturn Py_None;\n}\n\n\n";
    }
    else {
        fx_file << "static PyObject *\n"
                   "" << tech_symbol << "_execute(" << object_type << " *self, PyObject *args)\n"
                   "{\n\tint verbosity = 0;\n\n"
                   "\tif (!PyArg_ParseTuple(args, \"|i\", &verbosity))\n"
                   "\t\treturn NULL;\n\n"
                   "\tSAM_error error = new_error();\n"
                   "\tSAM_" << cmod_symbol << "_execute(self->data_ptr, verbosity, &error);\n"
                   "\tif (PySAM_has_error(error )) return NULL;\n"
                   "\tPy_INCREF(Py_None);\n"
                   "\treturn Py_None;\n}\n\n\n";
    }

    fx_file << "static PyObject *\n"
               "" << tech_symbol << "_assign(" << object_type << " *self, PyObject *args)\n"
               "{\n"
               "\tPyObject* dict;\n"
               "\tif (!PyArg_ParseTuple(args, \"O:assign\", &dict)){\n"
               "\t\treturn NULL;\n"
               "\t}\n\n"
               "\tif (!PySAM_assign_from_nested_dict((PyObject*)self, self->x_attr, self->data_ptr, dict, \"" << cmod_symbol << "\"))\n"
               "\t\treturn NULL;\n\n"
               "\tPy_INCREF(Py_None);\n"
               "\treturn Py_None;\n"
               "}\n\n";
    fx_file << "static PyObject *\n"
            << tech_symbol << "_replace(CmodObject *self, PyObject *args)\n"
               "{\n"
               "\tPyObject* dict;\n"
               "\tif (!PyArg_ParseTuple(args, \"O:assign\", &dict)){\n"
               "\t\treturn NULL;\n"
               "\t}\n\n"
               "\tif (!PySAM_replace_from_nested_dict((PyObject*)self, self->x_attr, self->data_ptr, dict, \"" << cmod_symbol << "\"))\n"
               "\t\treturn NULL;\n\n"
               "\tPy_INCREF(Py_None);\n"
               "\treturn Py_None;\n"
               "}\n\n";
    fx_file << "static PyObject *\n"
            << tech_symbol << "_export(" << object_type << " *self, PyObject *args)\n"
               "{\n"
               "\treturn PySAM_export_to_nested_dict((PyObject *) self, self->x_attr);\n"
               "}\n\n";

    // define fx to set or get a ssc variable by name
    fx_file << "static PyObject *\n"
            << tech_symbol << "_value(" << object_type << " *self, PyObject *args)\n"
                              "{\n\treturn " << cmod_type << "_value(self, args);\n}\n\n";

    fx_file << "static PyObject *\n"
            << tech_symbol << "_unassign(" << object_type << " *self, PyObject *args)\n"
                              "{\n\treturn " << cmod_type << "_unassign(self, args);\n}\n\n";

    fx_file << "static PyMethodDef " << tech_symbol << "_methods[] = {\n";

    if (stateful) {
        fx_file << "\t\t{\"setup\",            (PyCFunction)" << tech_symbol << "_setup,  METH_VARARGS,\n"
                   "\t\t\t\tPyDoc_STR(\"setup() -> None\\n Setup parameters in simulation\")},\n";
    }

    fx_file << "\t\t{\"execute\",           (PyCFunction)" << tech_symbol << "_execute,  METH_VARARGS,\n"
               "\t\t\t\tPyDoc_STR(\"execute(int verbosity) -> None\\n Execute simulation with verbosity level 0 (default) or 1\")},\n"
               "\t\t{\"assign\",            (PyCFunction)" << tech_symbol << "_assign,  METH_VARARGS,\n"
               "\t\t\t\tPyDoc_STR(\"assign(dict) -> None\\n Assign attributes from nested dictionary, except for Outputs\\n\\n"
               "``nested_dict = { '" << root->vardefs_order[0] << "': { var: val, ...}, ...}``\")},\n"
               "\t\t{\"replace\",            (PyCFunction)" << tech_symbol << "_replace,  METH_VARARGS,\n"
               "\t\t\t\tPyDoc_STR(\"replace(dict) -> None\\n Replace attributes from nested dictionary, except for Outputs. Unassigns all values in each Group then assigns from the input dict.\\n\\n"
               "``nested_dict = { '" << root->vardefs_order[0] << "': { var: val, ...}, ...}``\")},\n"
               "\t\t{\"export\",            (PyCFunction)" << tech_symbol << "_export,  METH_VARARGS,\n"
               "\t\t\t\tPyDoc_STR(\"export() -> dict\\n Export attributes into nested dictionary\")},\n"
               "\t\t{\"value\",             (PyCFunction)" << tech_symbol << "_value, METH_VARARGS,\n"
               "\t\t\t\tPyDoc_STR(\"value(name, optional value) -> Union[None, float, dict, sequence, str]\\n Get or set by name a value in any of the variable groups.\")},\n"
               "\t\t{\"unassign\",          (PyCFunction)" << tech_symbol << "_unassign, METH_VARARGS,\n"
               "\t\t\t\tPyDoc_STR(\"unassign(name) -> None\\n Unassign a value in any of the variable groups.\")},\n";

    // add ssc equations as methods under the cmod class
    auto cmod_it = root->m_eqn_entries.find(cmod_symbol);
    if (cmod_it != root->m_eqn_entries.end()){
        auto func_map = cmod_it->second;
        for (const auto& func_it : func_map){
            if (func_it.second.PySAM_export) {
                fx_file << "\t\t{\"" << func_it.first << "\", (PyCFunction)" << func_it.second.name;
                fx_file << ", METH_VARARGS | METH_KEYWORDS,\n"
                           "\t\t\t" << func_it.second.name << "_doc},\n";
            }
        }
    }

    fx_file << "\t\t{NULL,              NULL}           /* sentinel */\n"
               "};\n"
               "\n"
               "static PyObject *\n"
               "" << tech_symbol << "_getattro(" << object_type << " *self, PyObject *name)\n"
               "{\n"
               "\treturn PySAM_get_attr((PyObject*) self, (PyObject*) self->x_attr, name);\n"
               "}\n"
               "\n"
               "static int\n"
               "" << tech_symbol << "_setattr(" << object_type << " *self, const char *name, PyObject *v)\n"
               "{\n"
               "\treturn PySAM_set_attr((PyObject*)self, (PyObject*)self->x_attr, name, v);\n"
               "}\n\n";

    // define technology type

    fx_file << "static PyTypeObject " << tech_symbol << "_Type = {\n"
               "\t\t/* The ob_type field must be initialized in the module init function\n"
               "\t\t * to be portable to Windows without using C++. */\n"
               "\t\tPyVarObject_HEAD_INIT(NULL, 0)\n"
               "\t\t\"" << tech_symbol << "\",            /*tp_name*/\n"
               "\t\tsizeof(" << object_type << "),/*tp_basicsize*/\n"
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
               "\t" << object_type << " *rv;\n"
               "\trv = new" << tech_symbol << "Object(0);\n"
               "\tif (rv == NULL)\n"
               "\t\treturn NULL;\n\n"
               "\trv->data_owner_ptr = NULL;\n";

    if (stateful)
        fx_file << "\trv->cmod_ptr = NULL;\n";
    fx_file << "\treturn (PyObject *)rv;\n"
               "}\n\n";

    // wrapping fx

    fx_file << "static PyObject *\n"
               "" << tech_symbol << "_wrap(PyObject *self, PyObject *args)\n"
               "{\n"
               "\t" << object_type << " *rv;\n"
               "\tlong long int ptr = 0;  // 64 bit arch\n"
               "\tif (!PyArg_ParseTuple(args, \"L:wrap\", &ptr)){\n"
               "\t\tPyErr_BadArgument();\n"
               "\t\treturn NULL;\n"
               "\t}\n"
               "\trv = new" << tech_symbol << "Object((void*)ptr);\n"
               "\tif (rv == NULL)\n"
               "\t\treturn NULL;\n\n"
               "\trv->data_owner_ptr = NULL;\n";
    if (stateful) {
        fx_file << "\trv->cmod_ptr = NULL;\n";
    }
    fx_file << "\treturn (PyObject *)rv;\n"
               "}\n\n";

    // defaults loading fx

    fx_file << "static PyObject *\n"
               "" << tech_symbol << "_default(PyObject *self, PyObject *args)\n"
               "{\n"
               "\t" << object_type << " *rv;\n"
               "\tchar* def = 0;\n"
               "\tif (!PyArg_ParseTuple(args, \"s:default\", &def)){\n"
               "\t\tPyErr_BadArgument();\n"
               "\t\treturn NULL;\n"
               "\t}\n"
               "\trv = new" << tech_symbol << "Object(0);\n"
               "\tif (rv == NULL)\n"
               "\t\treturn NULL;\n"
               "\n"
               "\trv->data_owner_ptr = NULL;\n"
               "\tif (PySAM_load_defaults((PyObject*)rv, rv->x_attr, rv->data_ptr, \"" << cmod_symbol << "\", def) < 0) {\n"
               "\t\t" << tech_symbol << "_dealloc(rv);\n\t\treturn NULL;\n\t}"
               "\n";
    if (stateful) {
        fx_file << "\trv->cmod_ptr = NULL;\n";
    }
    fx_file << "\treturn (PyObject *)rv;\n"
               "}\n\n";

    // creating module from shared ssc data with defaults fx
    fx_file << "static PyObject *\n"
               << tech_symbol << "_from_existing(PyObject *self, PyObject *args)\n"
               "{\n"
               "\t" << object_type << " *rv;\n"
               "\tPyObject * module = 0;\n"
               "\tchar* def = 0;\n"
               "\tif (!PyArg_ParseTuple(args, \"O|s:from_existing\", &module, &def)){\n"
               "\t\tPyErr_BadArgument();\n"
               "\t\treturn NULL;\n"
               "\t}\n"
               "\t" << object_type << " *module_obj = (" << object_type << " *)module;\n"
               "\tSAM_table ptr = module_obj->data_ptr;\n"
               "\n"
               "\t// do a rough validity check on the data by checking its size\n"
               "\tSAM_error error = new_error();\n"
               "\tint data_size = SAM_table_size(ptr, &error);\n"
               "\tif (PySAM_has_error(error))\n"
               "\t\tgoto fail;\n"
               "\tif (data_size < 0)\n"
               "\t\tgoto fail;\n"
               "\n"
               "\trv = new" << tech_symbol << "Object((void*)ptr);\n"
               "\tif (rv == NULL)\n"
               "\t\tgoto fail;\n"
               "\trv->data_owner_ptr = module;\n";
    if (stateful) {
        fx_file << "\trv->cmod_ptr = NULL;\n";
    }
    fx_file << "\tif (!def)\n"
               "\t\treturn (PyObject *)rv;\n"
               "\tPySAM_load_defaults((PyObject*)rv, rv->x_attr, rv->data_ptr, \"" << cmod_symbol << "\", def);\n"
               "\treturn (PyObject *)rv;\n"
               "\n"
               "\tfail:\n"
               "\tPy_DECREF(module);\n"
               "\treturn NULL;\n"
               "}";

    fx_file << "/* ---------- */\n"
               "\n"
               "\n"
               "/* List of functions defined in the module */\n"
               "\n"
               "static PyMethodDef " << tech_symbol << "Module_methods[] = {\n"
               "\t\t{\"new\",             " << tech_symbol << "_new,         METH_VARARGS,\n"
               "\t\t\t\tPyDoc_STR(\"new() -> " << tech_symbol << "\")},\n"
               "\t\t{\"default\",             " << tech_symbol << "_default,         METH_VARARGS,\n"
               "\t\t\t\tPyDoc_STR(\"default(config) -> " << tech_symbol << "\\n\\nUse default attributes\\n\"\n"
                                                                                  "\t\t\t\t\"" << get_config_options() << "\")},\n"
               "\t\t{\"wrap\",             " << tech_symbol << "_wrap,         METH_VARARGS,\n"
               "\t\t\t\tPyDoc_STR(\"wrap(ssc_data_t) -> " << tech_symbol << "\\n\\nUse existing PySSC data\\n\\n.. warning::\\n\\n"
                                                                                "\tDo not call PySSC.data_free on the ssc_data_t provided to ``wrap``\")},\n"
               "\t\t{\"from_existing\",   " << tech_symbol << "_from_existing,        METH_VARARGS,\n"
               "\t\t\t\tPyDoc_STR(\"from_existing(data, optional config) -> " << tech_symbol << "\\n\\nShare underlying data with an existing PySAM class. If config provided, default attributes are loaded otherwise.\")},\n"
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

        if (module_symbol == "AdjustmentFactors" || vardefs.empty())
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

    fx_file << cmod_symbol << "\n***********************************\n\n";

    std::string cmod_doc = "Wrapper for SAM Simulation Core model: `cmod_" + cmod;
    cmod_doc += ".cpp <https://github.com/NREL/ssc/blob/develop/ssc/cmod_" + util::lower_case(cmod) + ".cpp>`_\n\n";

    fx_file << cmod_doc;

    if (!check_inputs_consistent(tech_symbol)) {
        fx_file << "Input Consistency Warning\n"
                   "==================================\n"
                   "\n"
                   "As described in :ref:`Possible Problems <possible_problems>`, some input parameters are interdependent but the equations \n"
                   "that enforce consistency are not available in this PySAM module. Therefore,\n"
                   "the onus is on the PySAM user to check that interdependencies are correctly handled. The variables which may require\n"
                   "additional logic include:\n\n";

        std::set<std::string> dependent_vars;
        for (const auto& i : root->vardefs_order) {
            auto mm = root->m_vardefs.find(i);
            std::map<std::string, var_def> vardefs = mm->second;
            for (auto& it : vardefs) {
                std::string var_symbol = it.first;
                var_def vd = it.second;

                for (const auto & ds: vd.downstream)
                    dependent_vars.insert(ds);

                for (const auto & ds: vd.upstream)
                    dependent_vars.insert(ds);
            }
        }

        for (const auto& i : dependent_vars)
            fx_file << " - " << i << "\n";

        fx_file << "\n"
                   "Provided for each of these inputs is a list of other inputs that are potentially interdependent. \n\n";
    }

    fx_file << "Creating an Instance\n===================================\n\n"
               "Refer to the :ref:`Initializing a Model <initializing>` page for details on the different ways to create an instance of a PySAM class.\n\n"
               "**" << tech_symbol << " model description**\n\n";

    fx_file << ".. automodule:: PySAM." << tech_symbol << "\n";
    fx_file << "\t:members:\n\n";

    fx_file << "Functions\n===================================\n\n"
               ".. autoclass:: PySAM." << tech_symbol << "." << tech_symbol << "\n\t:members:\n\n";

    for (const auto& i : root->vardefs_order) {
        auto mm = root->m_vardefs.find(i);
        std::map<std::string, var_def> vardefs = mm->second;

        std::string module_symbol = format_as_symbol(mm->first);

        if (module_symbol == "Outputs" && mm->second.empty())
            continue;

        fx_file << module_symbol << " Group\n======================================================\n\n";
        fx_file << ".. autoclass:: PySAM." << tech_symbol << "." << tech_symbol << "." << module_symbol << "\n";
        fx_file << "\t:members:\n\n";
    }

    fx_file.close();

    // export .pyi stub files for static typing

    fx_file.open(file_dir + "/stubs/stubs/" + tech_symbol + ".pyi");
    assert(fx_file.is_open());

    fx_file << "class " << tech_symbol << "(object):\n";
    fx_file << "\tdef assign(self, dict):\n"
               "\t\tpass\n"
               "\n"
               "\tdef value(self, name, value=None):\n"
               "\t\tpass\n"
               "\n"
               "\tdef unassign(self, name):\n"
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

    // add ssc equations as methods under the cmod class
    if (cmod_it != root->m_eqn_entries.end()){
        auto func_map = cmod_it->second;
        for (const auto& func_it : func_map){
            fx_file << "\tdef "<< func_it.first << "(self, args):\n\t\tpass\n";
        }
    }

    for (const auto& i : root->vardefs_order) {
        auto mm = root->m_vardefs.find(i);
        std::map<std::string, var_def> vardefs = mm->second;

        std::string module_symbol = format_as_symbol(mm->first);

        if (module_symbol == "AdjustmentFactors"){
            fx_file << "\tclass AdjustmentFactors(object):\n"
                       "\t\tdef assign(self): \n"
                       "\t\t\tpass\n"
                       "\t\n"
                       "\t\tdef export(self): \n"
                       "\t\t\treturn {}\n"
                       "\t\n"
                       "\t\tdef __init__(self, *args, **kwargs): # real signature unknown\n"
                       "\t\t\tpass\n"
                       "\t\n"
                       "\t\tconstant = float\n"
                       "\t\tdc_constant = float\n"
                       "\t\tdc_hourly = tuple\n"
                       "\t\tdc_periods = tuple\n"
                       "\t\thourly = tuple\n"
                       "\t\tperiods = tuple\n"
                       "\t\tsf_constant = float\n"
                       "\t\tsf_hourly = tuple\n"
                       "\t\tsf_periods = tuple\n\n";
            continue;
        }

        fx_file << "\tclass " << module_symbol << "(object):\n";
        fx_file << "\t\tdef assign(self): \n"
                   "\t\t\tpass\n"
                   "\t\n"
                   "\t\tdef export(self) -> dict:\n"
                   "\t\t\tpass\n"
                   "\t\n"
                   "\t\tdef __init__(self, *args, **kwargs): \n"
                   "\t\t\tpass\n\n";

        // add ssc equations
        auto group_it = root->m_eqn_entries.find(module_symbol);
        if (group_it != root->m_eqn_entries.end()){
            auto func_map = group_it->second;
            for (const auto& func_it : func_map){
                fx_file << "\t\tdef "<< func_it.first << "(self, args):\n\t\tpass\n";
            }
        }
        fx_file << "\n";

        std::vector<std::string> statictype_str = {"None", "str", "float", "tuple", "tuple", "dict"};

        for (const auto& it : vardefs) {
            std::string var_symbol = it.first;
            if (var_symbol == "global")
                continue;
            fx_file << "\t\t" << var_symbol << " = " << statictype_str[it.second.type_n] << "\n";
        }
        fx_file << "\n\n";
    }

    fx_file << "\n\n";

    fx_file << "def default(config) -> " << tech_symbol <<":\n"
               "\tpass\n"
               "\n"
               "def new() -> " << tech_symbol << ":\n"
               "\tpass\n"
               "\n"
               "def wrap(ssc_data_t) -> " << tech_symbol << ":\n"
               "\tpass\n"
               "\n"
               "def from_existing(model, config=\"\") -> " << tech_symbol << ":\n"
               "\tpass\n"
               "\n"
               "__loader__ = None \n"
               "\n"
               "__spec__ = None\n";
    fx_file.close();
}
