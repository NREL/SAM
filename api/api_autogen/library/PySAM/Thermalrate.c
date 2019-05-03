#include <Python.h>

#include <SAM_Thermalrate.h>
#include <SAM_api.h>

#include "PySAM_utils.h"



	/*
	 * ThermalRate Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Thermalrate   data_ptr;
} ThermalRateObject;

static PyTypeObject ThermalRate_Type;

static PyObject *
ThermalRate_new(SAM_Thermalrate data_ptr)
{
	PyObject* new_obj = ThermalRate_Type.tp_alloc(&ThermalRate_Type,0);

	ThermalRateObject* ThermalRate_obj = (ThermalRateObject*)new_obj;

	ThermalRate_obj->data_ptr = data_ptr;

	return new_obj;
}

/* ThermalRate methods */

static PyObject *
ThermalRate_assign(ThermalRateObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Thermalrate", "ThermalRate")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
ThermalRate_export(ThermalRateObject *self, PyObject *args)
{
	PyTypeObject* tp = &ThermalRate_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef ThermalRate_methods[] = {
		{"assign",            (PyCFunction)ThermalRate_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``ThermalRate_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)ThermalRate_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
ThermalRate_get_en_thermal_rates(ThermalRateObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Thermalrate_ThermalRate_en_thermal_rates_nget, self->data_ptr);
}

static int
ThermalRate_set_en_thermal_rates(ThermalRateObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Thermalrate_ThermalRate_en_thermal_rates_nset, self->data_ptr);
}

static PyObject *
ThermalRate_get_fuelcell_power_thermal(ThermalRateObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Thermalrate_ThermalRate_fuelcell_power_thermal_aget, self->data_ptr);
}

static int
ThermalRate_set_fuelcell_power_thermal(ThermalRateObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Thermalrate_ThermalRate_fuelcell_power_thermal_aset, self->data_ptr);
}

static PyObject *
ThermalRate_get_thermal_buy_rate(ThermalRateObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Thermalrate_ThermalRate_thermal_buy_rate_aget, self->data_ptr);
}

static int
ThermalRate_set_thermal_buy_rate(ThermalRateObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Thermalrate_ThermalRate_thermal_buy_rate_aset, self->data_ptr);
}

static PyObject *
ThermalRate_get_thermal_buy_rate_flat(ThermalRateObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Thermalrate_ThermalRate_thermal_buy_rate_flat_nget, self->data_ptr);
}

static int
ThermalRate_set_thermal_buy_rate_flat(ThermalRateObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Thermalrate_ThermalRate_thermal_buy_rate_flat_nset, self->data_ptr);
}

static PyObject *
ThermalRate_get_thermal_buy_rate_option(ThermalRateObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Thermalrate_ThermalRate_thermal_buy_rate_option_nget, self->data_ptr);
}

static int
ThermalRate_set_thermal_buy_rate_option(ThermalRateObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Thermalrate_ThermalRate_thermal_buy_rate_option_nset, self->data_ptr);
}

static PyObject *
ThermalRate_get_thermal_degradation(ThermalRateObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Thermalrate_ThermalRate_thermal_degradation_aget, self->data_ptr);
}

static int
ThermalRate_set_thermal_degradation(ThermalRateObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Thermalrate_ThermalRate_thermal_degradation_aset, self->data_ptr);
}

static PyObject *
ThermalRate_get_thermal_load(ThermalRateObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Thermalrate_ThermalRate_thermal_load_aget, self->data_ptr);
}

static int
ThermalRate_set_thermal_load(ThermalRateObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Thermalrate_ThermalRate_thermal_load_aset, self->data_ptr);
}

static PyObject *
ThermalRate_get_thermal_load_escalation(ThermalRateObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Thermalrate_ThermalRate_thermal_load_escalation_aget, self->data_ptr);
}

static int
ThermalRate_set_thermal_load_escalation(ThermalRateObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Thermalrate_ThermalRate_thermal_load_escalation_aset, self->data_ptr);
}

static PyObject *
ThermalRate_get_thermal_rate_escalation(ThermalRateObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Thermalrate_ThermalRate_thermal_rate_escalation_aget, self->data_ptr);
}

static int
ThermalRate_set_thermal_rate_escalation(ThermalRateObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Thermalrate_ThermalRate_thermal_rate_escalation_aset, self->data_ptr);
}

static PyObject *
ThermalRate_get_thermal_sell_rate(ThermalRateObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Thermalrate_ThermalRate_thermal_sell_rate_aget, self->data_ptr);
}

static int
ThermalRate_set_thermal_sell_rate(ThermalRateObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Thermalrate_ThermalRate_thermal_sell_rate_aset, self->data_ptr);
}

static PyObject *
ThermalRate_get_thermal_sell_rate_flat(ThermalRateObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Thermalrate_ThermalRate_thermal_sell_rate_flat_nget, self->data_ptr);
}

static int
ThermalRate_set_thermal_sell_rate_flat(ThermalRateObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Thermalrate_ThermalRate_thermal_sell_rate_flat_nset, self->data_ptr);
}

static PyObject *
ThermalRate_get_thermal_sell_rate_option(ThermalRateObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Thermalrate_ThermalRate_thermal_sell_rate_option_nget, self->data_ptr);
}

static int
ThermalRate_set_thermal_sell_rate_option(ThermalRateObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Thermalrate_ThermalRate_thermal_sell_rate_option_nset, self->data_ptr);
}

static PyGetSetDef ThermalRate_getset[] = {
{"en_thermal_rates", (getter)ThermalRate_get_en_thermal_rates,(setter)ThermalRate_set_en_thermal_rates,
	PyDoc_STR("*float*: Optionally enable/disable thermal_rate [years]\n\n*Constraints*: INTEGER,MIN=0,MAX=1"),
 	NULL},
{"fuelcell_power_thermal", (getter)ThermalRate_get_fuelcell_power_thermal,(setter)ThermalRate_set_fuelcell_power_thermal,
	PyDoc_STR("*sequence*: Fuel cell power generated [kW-t]\n\n*Required*: True"),
 	NULL},
{"thermal_buy_rate", (getter)ThermalRate_get_thermal_buy_rate,(setter)ThermalRate_set_thermal_buy_rate,
	PyDoc_STR("*sequence*: Thermal buy rate [$/kW-t]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"thermal_buy_rate_flat", (getter)ThermalRate_get_thermal_buy_rate_flat,(setter)ThermalRate_set_thermal_buy_rate_flat,
	PyDoc_STR("*float*: Thermal buy rate flat [$/kW-t]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"thermal_buy_rate_option", (getter)ThermalRate_get_thermal_buy_rate_option,(setter)ThermalRate_set_thermal_buy_rate_option,
	PyDoc_STR("*float*: Thermal buy rate option [0/1]\n\n*Options*: 0=flat,1=timestep\n\n*Constraints*: INTEGER,MIN=0,MAX=1\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"thermal_degradation", (getter)ThermalRate_get_thermal_degradation,(setter)ThermalRate_set_thermal_degradation,
	PyDoc_STR("*sequence*: Annual energy degradation [%]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"thermal_load", (getter)ThermalRate_get_thermal_load,(setter)ThermalRate_set_thermal_load,
	PyDoc_STR("*sequence*: Thermal load (year 1) [kW-t]"),
 	NULL},
{"thermal_load_escalation", (getter)ThermalRate_get_thermal_load_escalation,(setter)ThermalRate_set_thermal_load_escalation,
	PyDoc_STR("*sequence*: Annual load escalation [%/year]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"thermal_rate_escalation", (getter)ThermalRate_get_thermal_rate_escalation,(setter)ThermalRate_set_thermal_rate_escalation,
	PyDoc_STR("*sequence*: Annual thermal rate escalation [%/year]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"thermal_sell_rate", (getter)ThermalRate_get_thermal_sell_rate,(setter)ThermalRate_set_thermal_sell_rate,
	PyDoc_STR("*sequence*: Thermal sell rate [$/kW-t]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"thermal_sell_rate_flat", (getter)ThermalRate_get_thermal_sell_rate_flat,(setter)ThermalRate_set_thermal_sell_rate_flat,
	PyDoc_STR("*float*: Thermal sell rate flat [$/kW-t]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"thermal_sell_rate_option", (getter)ThermalRate_get_thermal_sell_rate_option,(setter)ThermalRate_set_thermal_sell_rate_option,
	PyDoc_STR("*float*: Thermal sell rate option [0/1]\n\n*Options*: 0=flat,1=timestep\n\n*Constraints*: INTEGER,MIN=0,MAX=1\n\n*Required*: set to 0 if not provided."),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject ThermalRate_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Thermalrate.ThermalRate",             /*tp_name*/
		sizeof(ThermalRateObject),          /*tp_basicsize*/
		0,                          /*tp_itemsize*/
		/* methods */
		0,    /*tp_dealloc*/
		0,                          /*tp_print*/
		(getattrfunc)0,             /*tp_getattr*/
		0,                          /*tp_setattr*/
		0,                          /*tp_reserved*/
		0,                          /*tp_repr*/
		0,                          /*tp_as_number*/
		0,                          /*tp_as_sequence*/
		0,                          /*tp_as_mapping*/
		0,                          /*tp_hash*/
		0,                          /*tp_call*/
		0,                          /*tp_str*/
		0,                          /*tp_getattro*/
		0,                          /*tp_setattro*/
		0,                          /*tp_as_buffer*/
		Py_TPFLAGS_DEFAULT,         /*tp_flags*/
		0,                          /*tp_doc*/
		0,                          /*tp_traverse*/
		0,                          /*tp_clear*/
		0,                          /*tp_richcompare*/
		0,                          /*tp_weaklistofnset*/
		0,                          /*tp_iter*/
		0,                          /*tp_iternext*/
		ThermalRate_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		ThermalRate_getset,          /*tp_getset*/
		0,                          /*tp_base*/
		0,                          /*tp_dict*/
		0,                          /*tp_descr_get*/
		0,                          /*tp_descr_set*/
		0,                          /*tp_dictofnset*/
		0,                          /*tp_init*/
		0,                          /*tp_alloc*/
		0,             /*tp_new*/
		0,                          /*tp_free*/
		0,                          /*tp_is_gc*/
};


	/*
	 * Lifetime Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Thermalrate   data_ptr;
} LifetimeObject;

static PyTypeObject Lifetime_Type;

static PyObject *
Lifetime_new(SAM_Thermalrate data_ptr)
{
	PyObject* new_obj = Lifetime_Type.tp_alloc(&Lifetime_Type,0);

	LifetimeObject* Lifetime_obj = (LifetimeObject*)new_obj;

	Lifetime_obj->data_ptr = data_ptr;

	return new_obj;
}

/* Lifetime methods */

static PyObject *
Lifetime_assign(LifetimeObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Thermalrate", "Lifetime")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
Lifetime_export(LifetimeObject *self, PyObject *args)
{
	PyTypeObject* tp = &Lifetime_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef Lifetime_methods[] = {
		{"assign",            (PyCFunction)Lifetime_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``Lifetime_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)Lifetime_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
Lifetime_get_analysis_period(LifetimeObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Thermalrate_Lifetime_analysis_period_nget, self->data_ptr);
}

static int
Lifetime_set_analysis_period(LifetimeObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Thermalrate_Lifetime_analysis_period_nset, self->data_ptr);
}

static PyObject *
Lifetime_get_inflation_rate(LifetimeObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Thermalrate_Lifetime_inflation_rate_nget, self->data_ptr);
}

static int
Lifetime_set_inflation_rate(LifetimeObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Thermalrate_Lifetime_inflation_rate_nset, self->data_ptr);
}

static PyObject *
Lifetime_get_system_use_lifetime_output(LifetimeObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Thermalrate_Lifetime_system_use_lifetime_output_nget, self->data_ptr);
}

static int
Lifetime_set_system_use_lifetime_output(LifetimeObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Thermalrate_Lifetime_system_use_lifetime_output_nset, self->data_ptr);
}

static PyGetSetDef Lifetime_getset[] = {
{"analysis_period", (getter)Lifetime_get_analysis_period,(setter)Lifetime_set_analysis_period,
	PyDoc_STR("*float*: Number of years in analysis [years]\n\n*Constraints*: INTEGER,POSITIVE\n\n*Required*: True"),
 	NULL},
{"inflation_rate", (getter)Lifetime_get_inflation_rate,(setter)Lifetime_set_inflation_rate,
	PyDoc_STR("*float*: Inflation rate [%]\n\n*Constraints*: MIN=-99\n\n*Required*: True"),
 	NULL},
{"system_use_lifetime_output", (getter)Lifetime_get_system_use_lifetime_output,(setter)Lifetime_set_system_use_lifetime_output,
	PyDoc_STR("*float*: Lifetime hourly system outputs [0/1]\n\n*Options*: 0=hourly first year,1=hourly lifetime\n\n*Constraints*: INTEGER,MIN=0,MAX=1\n\n*Required*: True"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject Lifetime_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Thermalrate.Lifetime",             /*tp_name*/
		sizeof(LifetimeObject),          /*tp_basicsize*/
		0,                          /*tp_itemsize*/
		/* methods */
		0,    /*tp_dealloc*/
		0,                          /*tp_print*/
		(getattrfunc)0,             /*tp_getattr*/
		0,                          /*tp_setattr*/
		0,                          /*tp_reserved*/
		0,                          /*tp_repr*/
		0,                          /*tp_as_number*/
		0,                          /*tp_as_sequence*/
		0,                          /*tp_as_mapping*/
		0,                          /*tp_hash*/
		0,                          /*tp_call*/
		0,                          /*tp_str*/
		0,                          /*tp_getattro*/
		0,                          /*tp_setattro*/
		0,                          /*tp_as_buffer*/
		Py_TPFLAGS_DEFAULT,         /*tp_flags*/
		0,                          /*tp_doc*/
		0,                          /*tp_traverse*/
		0,                          /*tp_clear*/
		0,                          /*tp_richcompare*/
		0,                          /*tp_weaklistofnset*/
		0,                          /*tp_iter*/
		0,                          /*tp_iternext*/
		Lifetime_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		Lifetime_getset,          /*tp_getset*/
		0,                          /*tp_base*/
		0,                          /*tp_dict*/
		0,                          /*tp_descr_get*/
		0,                          /*tp_descr_set*/
		0,                          /*tp_dictofnset*/
		0,                          /*tp_init*/
		0,                          /*tp_alloc*/
		0,             /*tp_new*/
		0,                          /*tp_free*/
		0,                          /*tp_is_gc*/
};


	/*
	 * Outputs Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Thermalrate   data_ptr;
} OutputsObject;

static PyTypeObject Outputs_Type;

static PyObject *
Outputs_new(SAM_Thermalrate data_ptr)
{
	PyObject* new_obj = Outputs_Type.tp_alloc(&Outputs_Type,0);

	OutputsObject* Outputs_obj = (OutputsObject*)new_obj;

	Outputs_obj->data_ptr = data_ptr;

	return new_obj;
}

/* Outputs methods */

static PyObject *
Outputs_assign(OutputsObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Thermalrate", "Outputs")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
Outputs_export(OutputsObject *self, PyObject *args)
{
	PyTypeObject* tp = &Outputs_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef Outputs_methods[] = {
		{"assign",            (PyCFunction)Outputs_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``Outputs_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)Outputs_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
Outputs_get_thermal_cost_with_system_year1(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Thermalrate_Outputs_thermal_cost_with_system_year1_nget, self->data_ptr);
}

static PyObject *
Outputs_get_thermal_cost_without_system_year1(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Thermalrate_Outputs_thermal_cost_without_system_year1_nget, self->data_ptr);
}

static PyObject *
Outputs_get_thermal_load_year1(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Thermalrate_Outputs_thermal_load_year1_nget, self->data_ptr);
}

static PyObject *
Outputs_get_thermal_revenue_with_system(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Thermalrate_Outputs_thermal_revenue_with_system_aget, self->data_ptr);
}

static PyObject *
Outputs_get_thermal_revenue_without_system(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Thermalrate_Outputs_thermal_revenue_without_system_aget, self->data_ptr);
}

static PyObject *
Outputs_get_thermal_savings_year1(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Thermalrate_Outputs_thermal_savings_year1_nget, self->data_ptr);
}

static PyGetSetDef Outputs_getset[] = {
{"thermal_cost_with_system_year1", (getter)Outputs_get_thermal_cost_with_system_year1,(setter)0,
	PyDoc_STR("*float*: Thermal cost with sytem (year 1) [$]"),
 	NULL},
{"thermal_cost_without_system_year1", (getter)Outputs_get_thermal_cost_without_system_year1,(setter)0,
	PyDoc_STR("*float*: Thermal cost without system (year 1) [$]"),
 	NULL},
{"thermal_load_year1", (getter)Outputs_get_thermal_load_year1,(setter)0,
	PyDoc_STR("*float*: Thermal load (year 1) [$]"),
 	NULL},
{"thermal_revenue_with_system", (getter)Outputs_get_thermal_revenue_with_system,(setter)0,
	PyDoc_STR("*sequence*: Thermal revenue with system [$]"),
 	NULL},
{"thermal_revenue_without_system", (getter)Outputs_get_thermal_revenue_without_system,(setter)0,
	PyDoc_STR("*sequence*: Thermal revenue without system [$]"),
 	NULL},
{"thermal_savings_year1", (getter)Outputs_get_thermal_savings_year1,(setter)0,
	PyDoc_STR("*float*: Thermal savings (year 1) [$]"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject Outputs_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Thermalrate.Outputs",             /*tp_name*/
		sizeof(OutputsObject),          /*tp_basicsize*/
		0,                          /*tp_itemsize*/
		/* methods */
		0,    /*tp_dealloc*/
		0,                          /*tp_print*/
		(getattrfunc)0,             /*tp_getattr*/
		0,                          /*tp_setattr*/
		0,                          /*tp_reserved*/
		0,                          /*tp_repr*/
		0,                          /*tp_as_number*/
		0,                          /*tp_as_sequence*/
		0,                          /*tp_as_mapping*/
		0,                          /*tp_hash*/
		0,                          /*tp_call*/
		0,                          /*tp_str*/
		0,                          /*tp_getattro*/
		0,                          /*tp_setattro*/
		0,                          /*tp_as_buffer*/
		Py_TPFLAGS_DEFAULT,         /*tp_flags*/
		0,                          /*tp_doc*/
		0,                          /*tp_traverse*/
		0,                          /*tp_clear*/
		0,                          /*tp_richcompare*/
		0,                          /*tp_weaklistofnset*/
		0,                          /*tp_iter*/
		0,                          /*tp_iternext*/
		Outputs_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		Outputs_getset,          /*tp_getset*/
		0,                          /*tp_base*/
		0,                          /*tp_dict*/
		0,                          /*tp_descr_get*/
		0,                          /*tp_descr_set*/
		0,                          /*tp_dictofnset*/
		0,                          /*tp_init*/
		0,                          /*tp_alloc*/
		0,             /*tp_new*/
		0,                          /*tp_free*/
		0,                          /*tp_is_gc*/
};

/*
 * Thermalrate
 */

typedef struct {
	PyObject_HEAD
	PyObject            *x_attr;        /* Attributes dictionary */
	SAM_Thermalrate   data_ptr;
} ThermalrateObject;

static PyTypeObject Thermalrate_Type;

#define ThermalrateObject_Check(v)      (Py_TYPE(v) == &Thermalrate_Type)

static ThermalrateObject *
newThermalrateObject(void* data_ptr)
{
	ThermalrateObject *self;
	self = PyObject_New(ThermalrateObject, &Thermalrate_Type);

	PySAM_TECH_ATTR("Thermalrate", SAM_Thermalrate_construct)

	PyObject* ThermalRate_obj = ThermalRate_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "ThermalRate", ThermalRate_obj);
	Py_DECREF(ThermalRate_obj);

	PyObject* Lifetime_obj = Lifetime_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "Lifetime", Lifetime_obj);
	Py_DECREF(Lifetime_obj);

	PyObject* Outputs_obj = Outputs_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "Outputs", Outputs_obj);
	Py_DECREF(Outputs_obj);


	return self;
}

/* Thermalrate methods */

static void
Thermalrate_dealloc(ThermalrateObject *self)
{
	Py_XDECREF(self->x_attr);
	SAM_Thermalrate_destruct(self->data_ptr);
	PyObject_Del(self);
}


static PyObject *
Thermalrate_execute(ThermalrateObject *self, PyObject *args)
{
	int verbosity = 0;

	if (!PyArg_ParseTuple(args, "|i", &verbosity))
		return NULL;

	SAM_error error = new_error();
	SAM_Thermalrate_execute(self->data_ptr, verbosity, &error);
	if (PySAM_has_error(error )) return NULL;

	Py_INCREF(Py_None);
	return Py_None;
}


static PyObject *
Thermalrate_assign(ThermalrateObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_nested_dict((PyObject*)self, self->x_attr, self->data_ptr, dict, "Thermalrate"))
		return NULL;

	Py_INCREF(Py_None);
	return Py_None;
}


static PyObject *
Thermalrate_export(ThermalrateObject *self, PyObject *args)
{
	return PySAM_export_to_nested_dict((PyObject *) self, self->x_attr);
}

static PyMethodDef Thermalrate_methods[] = {
		{"execute",            (PyCFunction)Thermalrate_execute,  METH_VARARGS,
				PyDoc_STR("execute(int verbosity) -> None\n Execute simulation with verbosity level 0 (default) or 1")},
		{"assign",            (PyCFunction)Thermalrate_assign,  METH_VARARGS,
				PyDoc_STR("assign(dict) -> None\n Assign attributes from nested dictionary, except for Outputs\n\n``nested_dict = { 'Thermal Rate': { var: val, ...}, ...}``")},
		{"export",            (PyCFunction)Thermalrate_export,  METH_VARARGS,
				PyDoc_STR("export() -> dict\n Export attributes into nested dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
Thermalrate_getattro(ThermalrateObject *self, PyObject *name)
{
	return PySAM_get_attr((PyObject*) self, (PyObject*) self->x_attr, name);
}

static int
Thermalrate_setattr(ThermalrateObject *self, const char *name, PyObject *v)
{
	return PySAM_set_attr((PyObject*)self, (PyObject*)self->x_attr, name, v);
}

static PyTypeObject Thermalrate_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Thermalrate",            /*tp_name*/
		sizeof(ThermalrateObject),/*tp_basicsize*/
		0,                          /*tp_itemsize*/
		/* methods */
		(destructor)Thermalrate_dealloc,    /*tp_dealloc*/
		0,                          /*tp_print*/
		(getattrfunc)0,             /*tp_getattr*/
		(setattrfunc)Thermalrate_setattr,   /*tp_setattr*/
		0,                          /*tp_reserved*/
		0,                          /*tp_repr*/
		0,                          /*tp_as_number*/
		0,                          /*tp_as_sequence*/
		0,                          /*tp_as_mapping*/
		0,                          /*tp_hash*/
		0,                          /*tp_call*/
		0,                          /*tp_str*/
		(getattrofunc)Thermalrate_getattro, /*tp_getattro*/
		0,                          /*tp_setattro*/
		0,                          /*tp_as_buffer*/
		Py_TPFLAGS_DEFAULT,         /*tp_flags*/
		"This class contains all the variable information for running a simulation. Variables are grouped together in the subclasses as properties. If property assignments are the wrong type, an error is thrown.",        /*tp_doc*/
		0,                          /*tp_traverse*/
		0,                          /*tp_clear*/
		0,                          /*tp_richcompare*/
		0,                          /*tp_weaklistofnset*/
		0,                          /*tp_iter*/
		0,                          /*tp_iternext*/
		Thermalrate_methods,      /*tp_methods*/
		0,                          /*tp_members*/
		0,       /*tp_getset*/
		0,                          /*tp_base*/
		0,                          /*tp_dict*/
		0,                          /*tp_descr_get*/
		0,                          /*tp_descr_set*/
		0,                          /*tp_dictofnset*/
		0,                          /*tp_init*/
		0,                          /*tp_alloc*/
		0,                          /*tp_new*/
		0,                          /*tp_free*/
		0,                          /*tp_is_gc*/
};

/* --------------------------------------------------------------------- */


/* Function of no arguments returning new Thermalrate object */

static PyObject *
Thermalrate_new(PyObject *self, PyObject *args)
{
	ThermalrateObject *rv;
	rv = newThermalrateObject(0);
	if (rv == NULL)
		return NULL;
	return (PyObject *)rv;
}

static PyObject *
Thermalrate_wrap(PyObject *self, PyObject *args)
{
	ThermalrateObject *rv;
	long long int ptr = 0;  // 64 bit arch
	if (!PyArg_ParseTuple(args, "L:wrap", &ptr)){
		PyErr_BadArgument();
		return NULL;
	}
	rv = newThermalrateObject((void*)ptr);
	if (rv == NULL)
		return NULL;
	return (PyObject *)rv;
}

static PyObject *
Thermalrate_default(PyObject *self, PyObject *args)
{
	ThermalrateObject *rv;
	char* def = 0;
	if (!PyArg_ParseTuple(args, "s:default", &def)){
		PyErr_BadArgument();
		return NULL;
	}
	rv = newThermalrateObject(0);
	if (rv == NULL)
		return NULL;

	PySAM_load_defaults((PyObject*)rv, rv->x_attr, rv->data_ptr, "Thermalrate", def);

	return (PyObject *)rv;
}

/* ---------- */


/* List of functions defined in the module */

static PyMethodDef ThermalrateModule_methods[] = {
		{"new",             Thermalrate_new,         METH_VARARGS,
				PyDoc_STR("new() -> Thermalrate")},
		{"default",             Thermalrate_default,         METH_VARARGS,
				PyDoc_STR("default(config) -> Thermalrate\n\nUse financial model-specific default attributes\n"
				"config options:\n\n- \"FuelCellCommercial\"\n- \"FuelCellSingleOwner\"")},
		{"wrap",             Thermalrate_wrap,         METH_VARARGS,
				PyDoc_STR("wrap(ssc_data_t) -> Thermalrate\n\nUse existing PySSC data\n\n.. warning::\n\n	Do not call PySSC.data_free on the ssc_data_t provided to ``wrap``")},
		{NULL,              NULL}           /* sentinel */
};

PyDoc_STRVAR(module_doc,
			 "Thermal flat rate structure net revenue calculator");


static int
ThermalrateModule_exec(PyObject *m)
{
	/* Finalize the type object including setting type of the new type
	 * object; doing it here is required for portability, too. */

	if (PySAM_load_lib(m) < 0) goto fail;
	if (PySAM_init_error(m) < 0) goto fail;

	Thermalrate_Type.tp_dict = PyDict_New();
	if (!Thermalrate_Type.tp_dict) { goto fail; }

	/// Add the ThermalRate type object to Thermalrate_Type
	if (PyType_Ready(&ThermalRate_Type) < 0) { goto fail; }
	PyDict_SetItemString(Thermalrate_Type.tp_dict,
				"ThermalRate",
				(PyObject*)&ThermalRate_Type);
	Py_DECREF(&ThermalRate_Type);

	/// Add the Lifetime type object to Thermalrate_Type
	if (PyType_Ready(&Lifetime_Type) < 0) { goto fail; }
	PyDict_SetItemString(Thermalrate_Type.tp_dict,
				"Lifetime",
				(PyObject*)&Lifetime_Type);
	Py_DECREF(&Lifetime_Type);

	/// Add the Outputs type object to Thermalrate_Type
	if (PyType_Ready(&Outputs_Type) < 0) { goto fail; }
	PyDict_SetItemString(Thermalrate_Type.tp_dict,
				"Outputs",
				(PyObject*)&Outputs_Type);
	Py_DECREF(&Outputs_Type);

	/// Add the Thermalrate type object to the module
	if (PyType_Ready(&Thermalrate_Type) < 0) { goto fail; }
	PyModule_AddObject(m,
				"Thermalrate",
				(PyObject*)&Thermalrate_Type);

	return 0;
	fail:
	Py_XDECREF(m);
	return -1;
}

static struct PyModuleDef_Slot ThermalrateModule_slots[] = {
		{Py_mod_exec, ThermalrateModule_exec},
		{0, NULL},
};

static struct PyModuleDef ThermalrateModule = {
		PyModuleDef_HEAD_INIT,
		"Thermalrate",
		module_doc,
		0,
		ThermalrateModule_methods,
		ThermalrateModule_slots,
		NULL,
		NULL,
		NULL
};

/* Export function for the module */

PyMODINIT_FUNC
PyInit_Thermalrate(void)
{
	return PyModuleDef_Init(&ThermalrateModule);
}