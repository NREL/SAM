#include <Python.h>

#include <SAM_Levpartflip.h>
#include <SAM_api.h>

#include "PySAM_utils.h"



	/*
	 * FinancialParameters Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Levpartflip   data_ptr;
} FinancialParametersObject;

static PyTypeObject FinancialParameters_Type;

static PyObject *
FinancialParameters_new(SAM_Levpartflip data_ptr)
{
	PyObject* new_obj = FinancialParameters_Type.tp_alloc(&FinancialParameters_Type,0);

	FinancialParametersObject* FinancialParameters_obj = (FinancialParametersObject*)new_obj;

	FinancialParameters_obj->data_ptr = data_ptr;

	return new_obj;
}

/* FinancialParameters methods */

static PyObject *
FinancialParameters_assign(FinancialParametersObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Levpartflip", "FinancialParameters")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
FinancialParameters_export(FinancialParametersObject *self, PyObject *args)
{
	PyTypeObject* tp = &FinancialParameters_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef FinancialParameters_methods[] = {
		{"assign",            (PyCFunction)FinancialParameters_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``FinancialParameters_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)FinancialParameters_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
FinancialParameters_get_analysis_period(FinancialParametersObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_FinancialParameters_analysis_period_nget, self->data_ptr);
}

static int
FinancialParameters_set_analysis_period(FinancialParametersObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_FinancialParameters_analysis_period_nset, self->data_ptr);
}

static PyObject *
FinancialParameters_get_federal_tax_rate(FinancialParametersObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_FinancialParameters_federal_tax_rate_aget, self->data_ptr);
}

static int
FinancialParameters_set_federal_tax_rate(FinancialParametersObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Levpartflip_FinancialParameters_federal_tax_rate_aset, self->data_ptr);
}

static PyObject *
FinancialParameters_get_inflation_rate(FinancialParametersObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_FinancialParameters_inflation_rate_nget, self->data_ptr);
}

static int
FinancialParameters_set_inflation_rate(FinancialParametersObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_FinancialParameters_inflation_rate_nset, self->data_ptr);
}

static PyObject *
FinancialParameters_get_insurance_rate(FinancialParametersObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_FinancialParameters_insurance_rate_nget, self->data_ptr);
}

static int
FinancialParameters_set_insurance_rate(FinancialParametersObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_FinancialParameters_insurance_rate_nset, self->data_ptr);
}

static PyObject *
FinancialParameters_get_prop_tax_assessed_decline(FinancialParametersObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_FinancialParameters_prop_tax_assessed_decline_nget, self->data_ptr);
}

static int
FinancialParameters_set_prop_tax_assessed_decline(FinancialParametersObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_FinancialParameters_prop_tax_assessed_decline_nset, self->data_ptr);
}

static PyObject *
FinancialParameters_get_prop_tax_cost_assessed_percent(FinancialParametersObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_FinancialParameters_prop_tax_cost_assessed_percent_nget, self->data_ptr);
}

static int
FinancialParameters_set_prop_tax_cost_assessed_percent(FinancialParametersObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_FinancialParameters_prop_tax_cost_assessed_percent_nset, self->data_ptr);
}

static PyObject *
FinancialParameters_get_property_tax_rate(FinancialParametersObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_FinancialParameters_property_tax_rate_nget, self->data_ptr);
}

static int
FinancialParameters_set_property_tax_rate(FinancialParametersObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_FinancialParameters_property_tax_rate_nset, self->data_ptr);
}

static PyObject *
FinancialParameters_get_real_discount_rate(FinancialParametersObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_FinancialParameters_real_discount_rate_nget, self->data_ptr);
}

static int
FinancialParameters_set_real_discount_rate(FinancialParametersObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_FinancialParameters_real_discount_rate_nset, self->data_ptr);
}

static PyObject *
FinancialParameters_get_state_tax_rate(FinancialParametersObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_FinancialParameters_state_tax_rate_aget, self->data_ptr);
}

static int
FinancialParameters_set_state_tax_rate(FinancialParametersObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Levpartflip_FinancialParameters_state_tax_rate_aset, self->data_ptr);
}

static PyObject *
FinancialParameters_get_system_capacity(FinancialParametersObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_FinancialParameters_system_capacity_nget, self->data_ptr);
}

static int
FinancialParameters_set_system_capacity(FinancialParametersObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_FinancialParameters_system_capacity_nset, self->data_ptr);
}

static PyObject *
FinancialParameters_get_system_heat_rate(FinancialParametersObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_FinancialParameters_system_heat_rate_nget, self->data_ptr);
}

static int
FinancialParameters_set_system_heat_rate(FinancialParametersObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_FinancialParameters_system_heat_rate_nset, self->data_ptr);
}

static PyGetSetDef FinancialParameters_getset[] = {
{"analysis_period", (getter)FinancialParameters_get_analysis_period,(setter)FinancialParameters_set_analysis_period,
	PyDoc_STR("*float*: Analyis period [years]\n\n*Constraints*: INTEGER,MIN=0,MAX=50\n\n*Required*: set to 30 if not provided."),
 	NULL},
{"federal_tax_rate", (getter)FinancialParameters_get_federal_tax_rate,(setter)FinancialParameters_set_federal_tax_rate,
	PyDoc_STR("*sequence*: Federal income tax rate [%]\n\n*Required*: True"),
 	NULL},
{"inflation_rate", (getter)FinancialParameters_get_inflation_rate,(setter)FinancialParameters_set_inflation_rate,
	PyDoc_STR("*float*: Inflation rate [%]\n\n*Constraints*: MIN=-99\n\n*Required*: True"),
 	NULL},
{"insurance_rate", (getter)FinancialParameters_get_insurance_rate,(setter)FinancialParameters_set_insurance_rate,
	PyDoc_STR("*float*: Insurance rate [%]\n\n*Constraints*: MIN=0,MAX=100\n\n*Required*: set to 0.0 if not provided."),
 	NULL},
{"prop_tax_assessed_decline", (getter)FinancialParameters_get_prop_tax_assessed_decline,(setter)FinancialParameters_set_prop_tax_assessed_decline,
	PyDoc_STR("*float*: Assessed value annual decline [%]\n\n*Constraints*: MIN=0,MAX=100\n\n*Required*: set to 5 if not provided."),
 	NULL},
{"prop_tax_cost_assessed_percent", (getter)FinancialParameters_get_prop_tax_cost_assessed_percent,(setter)FinancialParameters_set_prop_tax_cost_assessed_percent,
	PyDoc_STR("*float*: Percent of pre-financing costs assessed [%]\n\n*Constraints*: MIN=0,MAX=100\n\n*Required*: set to 95 if not provided."),
 	NULL},
{"property_tax_rate", (getter)FinancialParameters_get_property_tax_rate,(setter)FinancialParameters_set_property_tax_rate,
	PyDoc_STR("*float*: Property tax rate [%]\n\n*Constraints*: MIN=0,MAX=100\n\n*Required*: set to 0.0 if not provided."),
 	NULL},
{"real_discount_rate", (getter)FinancialParameters_get_real_discount_rate,(setter)FinancialParameters_set_real_discount_rate,
	PyDoc_STR("*float*: Real discount rate [%]\n\n*Constraints*: MIN=-99\n\n*Required*: True"),
 	NULL},
{"state_tax_rate", (getter)FinancialParameters_get_state_tax_rate,(setter)FinancialParameters_set_state_tax_rate,
	PyDoc_STR("*sequence*: State income tax rate [%]\n\n*Required*: True"),
 	NULL},
{"system_capacity", (getter)FinancialParameters_get_system_capacity,(setter)FinancialParameters_set_system_capacity,
	PyDoc_STR("*float*: System nameplate capacity [kW]\n\n*Constraints*: POSITIVE\n\n*Required*: True"),
 	NULL},
{"system_heat_rate", (getter)FinancialParameters_get_system_heat_rate,(setter)FinancialParameters_set_system_heat_rate,
	PyDoc_STR("*float*: System heat rate [MMBTus/MWh]\n\n*Constraints*: MIN=0\n\n*Required*: set to 0.0 if not provided."),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject FinancialParameters_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Levpartflip.FinancialParameters",             /*tp_name*/
		sizeof(FinancialParametersObject),          /*tp_basicsize*/
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
		FinancialParameters_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		FinancialParameters_getset,          /*tp_getset*/
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
	 * SystemCosts Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Levpartflip   data_ptr;
} SystemCostsObject;

static PyTypeObject SystemCosts_Type;

static PyObject *
SystemCosts_new(SAM_Levpartflip data_ptr)
{
	PyObject* new_obj = SystemCosts_Type.tp_alloc(&SystemCosts_Type,0);

	SystemCostsObject* SystemCosts_obj = (SystemCostsObject*)new_obj;

	SystemCosts_obj->data_ptr = data_ptr;

	return new_obj;
}

/* SystemCosts methods */

static PyObject *
SystemCosts_assign(SystemCostsObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Levpartflip", "SystemCosts")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
SystemCosts_export(SystemCostsObject *self, PyObject *args)
{
	PyTypeObject* tp = &SystemCosts_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef SystemCosts_methods[] = {
		{"assign",            (PyCFunction)SystemCosts_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``SystemCosts_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)SystemCosts_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
SystemCosts_get_add_om_num_types(SystemCostsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_SystemCosts_add_om_num_types_nget, self->data_ptr);
}

static int
SystemCosts_set_add_om_num_types(SystemCostsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_SystemCosts_add_om_num_types_nset, self->data_ptr);
}

static PyObject *
SystemCosts_get_annual_fuel_usage(SystemCostsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_SystemCosts_annual_fuel_usage_nget, self->data_ptr);
}

static int
SystemCosts_set_annual_fuel_usage(SystemCostsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_SystemCosts_annual_fuel_usage_nset, self->data_ptr);
}

static PyObject *
SystemCosts_get_om_capacity(SystemCostsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_SystemCosts_om_capacity_aget, self->data_ptr);
}

static int
SystemCosts_set_om_capacity(SystemCostsObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Levpartflip_SystemCosts_om_capacity_aset, self->data_ptr);
}

static PyObject *
SystemCosts_get_om_capacity1(SystemCostsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_SystemCosts_om_capacity1_aget, self->data_ptr);
}

static int
SystemCosts_set_om_capacity1(SystemCostsObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Levpartflip_SystemCosts_om_capacity1_aset, self->data_ptr);
}

static PyObject *
SystemCosts_get_om_capacity1_nameplate(SystemCostsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_SystemCosts_om_capacity1_nameplate_nget, self->data_ptr);
}

static int
SystemCosts_set_om_capacity1_nameplate(SystemCostsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_SystemCosts_om_capacity1_nameplate_nset, self->data_ptr);
}

static PyObject *
SystemCosts_get_om_capacity2(SystemCostsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_SystemCosts_om_capacity2_aget, self->data_ptr);
}

static int
SystemCosts_set_om_capacity2(SystemCostsObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Levpartflip_SystemCosts_om_capacity2_aset, self->data_ptr);
}

static PyObject *
SystemCosts_get_om_capacity2_nameplate(SystemCostsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_SystemCosts_om_capacity2_nameplate_nget, self->data_ptr);
}

static int
SystemCosts_set_om_capacity2_nameplate(SystemCostsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_SystemCosts_om_capacity2_nameplate_nset, self->data_ptr);
}

static PyObject *
SystemCosts_get_om_capacity_escal(SystemCostsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_SystemCosts_om_capacity_escal_nget, self->data_ptr);
}

static int
SystemCosts_set_om_capacity_escal(SystemCostsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_SystemCosts_om_capacity_escal_nset, self->data_ptr);
}

static PyObject *
SystemCosts_get_om_fixed(SystemCostsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_SystemCosts_om_fixed_aget, self->data_ptr);
}

static int
SystemCosts_set_om_fixed(SystemCostsObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Levpartflip_SystemCosts_om_fixed_aset, self->data_ptr);
}

static PyObject *
SystemCosts_get_om_fixed1(SystemCostsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_SystemCosts_om_fixed1_aget, self->data_ptr);
}

static int
SystemCosts_set_om_fixed1(SystemCostsObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Levpartflip_SystemCosts_om_fixed1_aset, self->data_ptr);
}

static PyObject *
SystemCosts_get_om_fixed2(SystemCostsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_SystemCosts_om_fixed2_aget, self->data_ptr);
}

static int
SystemCosts_set_om_fixed2(SystemCostsObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Levpartflip_SystemCosts_om_fixed2_aset, self->data_ptr);
}

static PyObject *
SystemCosts_get_om_fixed_escal(SystemCostsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_SystemCosts_om_fixed_escal_nget, self->data_ptr);
}

static int
SystemCosts_set_om_fixed_escal(SystemCostsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_SystemCosts_om_fixed_escal_nset, self->data_ptr);
}

static PyObject *
SystemCosts_get_om_fuel_cost(SystemCostsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_SystemCosts_om_fuel_cost_aget, self->data_ptr);
}

static int
SystemCosts_set_om_fuel_cost(SystemCostsObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Levpartflip_SystemCosts_om_fuel_cost_aset, self->data_ptr);
}

static PyObject *
SystemCosts_get_om_fuel_cost_escal(SystemCostsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_SystemCosts_om_fuel_cost_escal_nget, self->data_ptr);
}

static int
SystemCosts_set_om_fuel_cost_escal(SystemCostsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_SystemCosts_om_fuel_cost_escal_nset, self->data_ptr);
}

static PyObject *
SystemCosts_get_om_opt_fuel_1_cost(SystemCostsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_SystemCosts_om_opt_fuel_1_cost_aget, self->data_ptr);
}

static int
SystemCosts_set_om_opt_fuel_1_cost(SystemCostsObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Levpartflip_SystemCosts_om_opt_fuel_1_cost_aset, self->data_ptr);
}

static PyObject *
SystemCosts_get_om_opt_fuel_1_cost_escal(SystemCostsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_SystemCosts_om_opt_fuel_1_cost_escal_nget, self->data_ptr);
}

static int
SystemCosts_set_om_opt_fuel_1_cost_escal(SystemCostsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_SystemCosts_om_opt_fuel_1_cost_escal_nset, self->data_ptr);
}

static PyObject *
SystemCosts_get_om_opt_fuel_1_usage(SystemCostsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_SystemCosts_om_opt_fuel_1_usage_nget, self->data_ptr);
}

static int
SystemCosts_set_om_opt_fuel_1_usage(SystemCostsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_SystemCosts_om_opt_fuel_1_usage_nset, self->data_ptr);
}

static PyObject *
SystemCosts_get_om_opt_fuel_2_cost(SystemCostsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_SystemCosts_om_opt_fuel_2_cost_aget, self->data_ptr);
}

static int
SystemCosts_set_om_opt_fuel_2_cost(SystemCostsObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Levpartflip_SystemCosts_om_opt_fuel_2_cost_aset, self->data_ptr);
}

static PyObject *
SystemCosts_get_om_opt_fuel_2_cost_escal(SystemCostsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_SystemCosts_om_opt_fuel_2_cost_escal_nget, self->data_ptr);
}

static int
SystemCosts_set_om_opt_fuel_2_cost_escal(SystemCostsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_SystemCosts_om_opt_fuel_2_cost_escal_nset, self->data_ptr);
}

static PyObject *
SystemCosts_get_om_opt_fuel_2_usage(SystemCostsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_SystemCosts_om_opt_fuel_2_usage_nget, self->data_ptr);
}

static int
SystemCosts_set_om_opt_fuel_2_usage(SystemCostsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_SystemCosts_om_opt_fuel_2_usage_nset, self->data_ptr);
}

static PyObject *
SystemCosts_get_om_production(SystemCostsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_SystemCosts_om_production_aget, self->data_ptr);
}

static int
SystemCosts_set_om_production(SystemCostsObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Levpartflip_SystemCosts_om_production_aset, self->data_ptr);
}

static PyObject *
SystemCosts_get_om_production1(SystemCostsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_SystemCosts_om_production1_aget, self->data_ptr);
}

static int
SystemCosts_set_om_production1(SystemCostsObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Levpartflip_SystemCosts_om_production1_aset, self->data_ptr);
}

static PyObject *
SystemCosts_get_om_production1_values(SystemCostsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_SystemCosts_om_production1_values_aget, self->data_ptr);
}

static int
SystemCosts_set_om_production1_values(SystemCostsObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Levpartflip_SystemCosts_om_production1_values_aset, self->data_ptr);
}

static PyObject *
SystemCosts_get_om_production2(SystemCostsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_SystemCosts_om_production2_aget, self->data_ptr);
}

static int
SystemCosts_set_om_production2(SystemCostsObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Levpartflip_SystemCosts_om_production2_aset, self->data_ptr);
}

static PyObject *
SystemCosts_get_om_production2_values(SystemCostsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_SystemCosts_om_production2_values_aget, self->data_ptr);
}

static int
SystemCosts_set_om_production2_values(SystemCostsObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Levpartflip_SystemCosts_om_production2_values_aset, self->data_ptr);
}

static PyObject *
SystemCosts_get_om_production_escal(SystemCostsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_SystemCosts_om_production_escal_nget, self->data_ptr);
}

static int
SystemCosts_set_om_production_escal(SystemCostsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_SystemCosts_om_production_escal_nset, self->data_ptr);
}

static PyObject *
SystemCosts_get_om_replacement_cost1(SystemCostsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_SystemCosts_om_replacement_cost1_aget, self->data_ptr);
}

static int
SystemCosts_set_om_replacement_cost1(SystemCostsObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Levpartflip_SystemCosts_om_replacement_cost1_aset, self->data_ptr);
}

static PyObject *
SystemCosts_get_om_replacement_cost2(SystemCostsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_SystemCosts_om_replacement_cost2_aget, self->data_ptr);
}

static int
SystemCosts_set_om_replacement_cost2(SystemCostsObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Levpartflip_SystemCosts_om_replacement_cost2_aset, self->data_ptr);
}

static PyObject *
SystemCosts_get_om_replacement_cost_escal(SystemCostsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_SystemCosts_om_replacement_cost_escal_nget, self->data_ptr);
}

static int
SystemCosts_set_om_replacement_cost_escal(SystemCostsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_SystemCosts_om_replacement_cost_escal_nset, self->data_ptr);
}

static PyObject *
SystemCosts_get_total_installed_cost(SystemCostsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_SystemCosts_total_installed_cost_nget, self->data_ptr);
}

static int
SystemCosts_set_total_installed_cost(SystemCostsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_SystemCosts_total_installed_cost_nset, self->data_ptr);
}

static PyGetSetDef SystemCosts_getset[] = {
{"add_om_num_types", (getter)SystemCosts_get_add_om_num_types,(setter)SystemCosts_set_add_om_num_types,
	PyDoc_STR("*float*: Number of O and M types\n\n*Constraints*: INTEGER,MIN=0,MAX=2\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"annual_fuel_usage", (getter)SystemCosts_get_annual_fuel_usage,(setter)SystemCosts_set_annual_fuel_usage,
	PyDoc_STR("*float*: Fuel usage [kWht]\n\n*Constraints*: MIN=0\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"om_capacity", (getter)SystemCosts_get_om_capacity,(setter)SystemCosts_set_om_capacity,
	PyDoc_STR("*sequence*: Capacity-based System Costs amount [$/kWcap]\n\n*Required*: set to 0.0 if not provided."),
 	NULL},
{"om_capacity1", (getter)SystemCosts_get_om_capacity1,(setter)SystemCosts_set_om_capacity1,
	PyDoc_STR("*sequence*: Battery capacity-based System Costs amount [$/kWcap]\n\n*Required*: set to 0.0 if not provided."),
 	NULL},
{"om_capacity1_nameplate", (getter)SystemCosts_get_om_capacity1_nameplate,(setter)SystemCosts_set_om_capacity1_nameplate,
	PyDoc_STR("*float*: Battery capacity for System Costs values [kW]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"om_capacity2", (getter)SystemCosts_get_om_capacity2,(setter)SystemCosts_set_om_capacity2,
	PyDoc_STR("*sequence*: Fuel cell capacity-based System Costs amount [$/kWcap]\n\n*Required*: set to 0.0 if not provided."),
 	NULL},
{"om_capacity2_nameplate", (getter)SystemCosts_get_om_capacity2_nameplate,(setter)SystemCosts_set_om_capacity2_nameplate,
	PyDoc_STR("*float*: Fuel cell capacity for System Costs values [kW]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"om_capacity_escal", (getter)SystemCosts_get_om_capacity_escal,(setter)SystemCosts_set_om_capacity_escal,
	PyDoc_STR("*float*: Capacity-based System Costs escalation [%/year]\n\n*Required*: set to 0.0 if not provided."),
 	NULL},
{"om_fixed", (getter)SystemCosts_get_om_fixed,(setter)SystemCosts_set_om_fixed,
	PyDoc_STR("*sequence*: Fixed System Costs annual amount [$/year]\n\n*Required*: set to 0.0 if not provided."),
 	NULL},
{"om_fixed1", (getter)SystemCosts_get_om_fixed1,(setter)SystemCosts_set_om_fixed1,
	PyDoc_STR("*sequence*: Battery fixed System Costs annual amount [$/year]\n\n*Required*: set to 0.0 if not provided."),
 	NULL},
{"om_fixed2", (getter)SystemCosts_get_om_fixed2,(setter)SystemCosts_set_om_fixed2,
	PyDoc_STR("*sequence*: Fuel cell fixed System Costs annual amount [$/year]\n\n*Required*: set to 0.0 if not provided."),
 	NULL},
{"om_fixed_escal", (getter)SystemCosts_get_om_fixed_escal,(setter)SystemCosts_set_om_fixed_escal,
	PyDoc_STR("*float*: Fixed System Costs escalation [%/year]\n\n*Required*: set to 0.0 if not provided."),
 	NULL},
{"om_fuel_cost", (getter)SystemCosts_get_om_fuel_cost,(setter)SystemCosts_set_om_fuel_cost,
	PyDoc_STR("*sequence*: Fuel cost [$/MMBtu]\n\n*Required*: set to 0.0 if not provided."),
 	NULL},
{"om_fuel_cost_escal", (getter)SystemCosts_get_om_fuel_cost_escal,(setter)SystemCosts_set_om_fuel_cost_escal,
	PyDoc_STR("*float*: Fuel cost escalation [%/year]\n\n*Required*: set to 0.0 if not provided."),
 	NULL},
{"om_opt_fuel_1_cost", (getter)SystemCosts_get_om_opt_fuel_1_cost,(setter)SystemCosts_set_om_opt_fuel_1_cost,
	PyDoc_STR("*sequence*: Biomass feedstock cost [$/unit]\n\n*Required*: set to 0.0 if not provided."),
 	NULL},
{"om_opt_fuel_1_cost_escal", (getter)SystemCosts_get_om_opt_fuel_1_cost_escal,(setter)SystemCosts_set_om_opt_fuel_1_cost_escal,
	PyDoc_STR("*float*: Biomass feedstock cost escalation [%/year]\n\n*Required*: set to 0.0 if not provided."),
 	NULL},
{"om_opt_fuel_1_usage", (getter)SystemCosts_get_om_opt_fuel_1_usage,(setter)SystemCosts_set_om_opt_fuel_1_usage,
	PyDoc_STR("*float*: Biomass feedstock usage [unit]\n\n*Required*: set to 0.0 if not provided."),
 	NULL},
{"om_opt_fuel_2_cost", (getter)SystemCosts_get_om_opt_fuel_2_cost,(setter)SystemCosts_set_om_opt_fuel_2_cost,
	PyDoc_STR("*sequence*: Coal feedstock cost [$/unit]\n\n*Required*: set to 0.0 if not provided."),
 	NULL},
{"om_opt_fuel_2_cost_escal", (getter)SystemCosts_get_om_opt_fuel_2_cost_escal,(setter)SystemCosts_set_om_opt_fuel_2_cost_escal,
	PyDoc_STR("*float*: Coal feedstock cost escalation [%/year]\n\n*Required*: set to 0.0 if not provided."),
 	NULL},
{"om_opt_fuel_2_usage", (getter)SystemCosts_get_om_opt_fuel_2_usage,(setter)SystemCosts_set_om_opt_fuel_2_usage,
	PyDoc_STR("*float*: Coal feedstock usage [unit]\n\n*Required*: set to 0.0 if not provided."),
 	NULL},
{"om_production", (getter)SystemCosts_get_om_production,(setter)SystemCosts_set_om_production,
	PyDoc_STR("*sequence*: Production-based System Costs amount [$/MWh]\n\n*Required*: set to 0.0 if not provided."),
 	NULL},
{"om_production1", (getter)SystemCosts_get_om_production1,(setter)SystemCosts_set_om_production1,
	PyDoc_STR("*sequence*: Battery production-based System Costs amount [$/MWh]\n\n*Required*: set to 0.0 if not provided."),
 	NULL},
{"om_production1_values", (getter)SystemCosts_get_om_production1_values,(setter)SystemCosts_set_om_production1_values,
	PyDoc_STR("*sequence*: Battery production for System Costs values [kWh]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"om_production2", (getter)SystemCosts_get_om_production2,(setter)SystemCosts_set_om_production2,
	PyDoc_STR("*sequence*: Fuel cell production-based System Costs amount [$/MWh]\n\n*Required*: set to 0.0 if not provided."),
 	NULL},
{"om_production2_values", (getter)SystemCosts_get_om_production2_values,(setter)SystemCosts_set_om_production2_values,
	PyDoc_STR("*sequence*: Fuel cell production for System Costs values [kWh]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"om_production_escal", (getter)SystemCosts_get_om_production_escal,(setter)SystemCosts_set_om_production_escal,
	PyDoc_STR("*float*: Production-based System Costs escalation [%/year]\n\n*Required*: set to 0.0 if not provided."),
 	NULL},
{"om_replacement_cost1", (getter)SystemCosts_get_om_replacement_cost1,(setter)SystemCosts_set_om_replacement_cost1,
	PyDoc_STR("*sequence*: Repacement cost 1 [$/kWh]\n\n*Required*: set to 0.0 if not provided."),
 	NULL},
{"om_replacement_cost2", (getter)SystemCosts_get_om_replacement_cost2,(setter)SystemCosts_set_om_replacement_cost2,
	PyDoc_STR("*sequence*: Repacement cost 2 [$/kW]\n\n*Required*: set to 0.0 if not provided."),
 	NULL},
{"om_replacement_cost_escal", (getter)SystemCosts_get_om_replacement_cost_escal,(setter)SystemCosts_set_om_replacement_cost_escal,
	PyDoc_STR("*float*: Replacement cost escalation [%/year]\n\n*Required*: set to 0.0 if not provided."),
 	NULL},
{"total_installed_cost", (getter)SystemCosts_get_total_installed_cost,(setter)SystemCosts_set_total_installed_cost,
	PyDoc_STR("*float*: Installed cost [$]\n\n*Required*: True"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject SystemCosts_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Levpartflip.SystemCosts",             /*tp_name*/
		sizeof(SystemCostsObject),          /*tp_basicsize*/
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
		SystemCosts_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		SystemCosts_getset,          /*tp_getset*/
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
	 * TaxCreditIncentives Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Levpartflip   data_ptr;
} TaxCreditIncentivesObject;

static PyTypeObject TaxCreditIncentives_Type;

static PyObject *
TaxCreditIncentives_new(SAM_Levpartflip data_ptr)
{
	PyObject* new_obj = TaxCreditIncentives_Type.tp_alloc(&TaxCreditIncentives_Type,0);

	TaxCreditIncentivesObject* TaxCreditIncentives_obj = (TaxCreditIncentivesObject*)new_obj;

	TaxCreditIncentives_obj->data_ptr = data_ptr;

	return new_obj;
}

/* TaxCreditIncentives methods */

static PyObject *
TaxCreditIncentives_assign(TaxCreditIncentivesObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Levpartflip", "TaxCreditIncentives")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
TaxCreditIncentives_export(TaxCreditIncentivesObject *self, PyObject *args)
{
	PyTypeObject* tp = &TaxCreditIncentives_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef TaxCreditIncentives_methods[] = {
		{"assign",            (PyCFunction)TaxCreditIncentives_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``TaxCreditIncentives_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)TaxCreditIncentives_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
TaxCreditIncentives_get_itc_fed_amount(TaxCreditIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_TaxCreditIncentives_itc_fed_amount_nget, self->data_ptr);
}

static int
TaxCreditIncentives_set_itc_fed_amount(TaxCreditIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_TaxCreditIncentives_itc_fed_amount_nset, self->data_ptr);
}

static PyObject *
TaxCreditIncentives_get_itc_fed_amount_deprbas_fed(TaxCreditIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_TaxCreditIncentives_itc_fed_amount_deprbas_fed_nget, self->data_ptr);
}

static int
TaxCreditIncentives_set_itc_fed_amount_deprbas_fed(TaxCreditIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_TaxCreditIncentives_itc_fed_amount_deprbas_fed_nset, self->data_ptr);
}

static PyObject *
TaxCreditIncentives_get_itc_fed_amount_deprbas_sta(TaxCreditIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_TaxCreditIncentives_itc_fed_amount_deprbas_sta_nget, self->data_ptr);
}

static int
TaxCreditIncentives_set_itc_fed_amount_deprbas_sta(TaxCreditIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_TaxCreditIncentives_itc_fed_amount_deprbas_sta_nset, self->data_ptr);
}

static PyObject *
TaxCreditIncentives_get_itc_fed_percent(TaxCreditIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_TaxCreditIncentives_itc_fed_percent_nget, self->data_ptr);
}

static int
TaxCreditIncentives_set_itc_fed_percent(TaxCreditIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_TaxCreditIncentives_itc_fed_percent_nset, self->data_ptr);
}

static PyObject *
TaxCreditIncentives_get_itc_fed_percent_deprbas_fed(TaxCreditIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_TaxCreditIncentives_itc_fed_percent_deprbas_fed_nget, self->data_ptr);
}

static int
TaxCreditIncentives_set_itc_fed_percent_deprbas_fed(TaxCreditIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_TaxCreditIncentives_itc_fed_percent_deprbas_fed_nset, self->data_ptr);
}

static PyObject *
TaxCreditIncentives_get_itc_fed_percent_deprbas_sta(TaxCreditIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_TaxCreditIncentives_itc_fed_percent_deprbas_sta_nget, self->data_ptr);
}

static int
TaxCreditIncentives_set_itc_fed_percent_deprbas_sta(TaxCreditIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_TaxCreditIncentives_itc_fed_percent_deprbas_sta_nset, self->data_ptr);
}

static PyObject *
TaxCreditIncentives_get_itc_fed_percent_maxvalue(TaxCreditIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_TaxCreditIncentives_itc_fed_percent_maxvalue_nget, self->data_ptr);
}

static int
TaxCreditIncentives_set_itc_fed_percent_maxvalue(TaxCreditIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_TaxCreditIncentives_itc_fed_percent_maxvalue_nset, self->data_ptr);
}

static PyObject *
TaxCreditIncentives_get_itc_sta_amount(TaxCreditIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_TaxCreditIncentives_itc_sta_amount_nget, self->data_ptr);
}

static int
TaxCreditIncentives_set_itc_sta_amount(TaxCreditIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_TaxCreditIncentives_itc_sta_amount_nset, self->data_ptr);
}

static PyObject *
TaxCreditIncentives_get_itc_sta_amount_deprbas_fed(TaxCreditIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_TaxCreditIncentives_itc_sta_amount_deprbas_fed_nget, self->data_ptr);
}

static int
TaxCreditIncentives_set_itc_sta_amount_deprbas_fed(TaxCreditIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_TaxCreditIncentives_itc_sta_amount_deprbas_fed_nset, self->data_ptr);
}

static PyObject *
TaxCreditIncentives_get_itc_sta_amount_deprbas_sta(TaxCreditIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_TaxCreditIncentives_itc_sta_amount_deprbas_sta_nget, self->data_ptr);
}

static int
TaxCreditIncentives_set_itc_sta_amount_deprbas_sta(TaxCreditIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_TaxCreditIncentives_itc_sta_amount_deprbas_sta_nset, self->data_ptr);
}

static PyObject *
TaxCreditIncentives_get_itc_sta_percent(TaxCreditIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_TaxCreditIncentives_itc_sta_percent_nget, self->data_ptr);
}

static int
TaxCreditIncentives_set_itc_sta_percent(TaxCreditIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_TaxCreditIncentives_itc_sta_percent_nset, self->data_ptr);
}

static PyObject *
TaxCreditIncentives_get_itc_sta_percent_deprbas_fed(TaxCreditIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_TaxCreditIncentives_itc_sta_percent_deprbas_fed_nget, self->data_ptr);
}

static int
TaxCreditIncentives_set_itc_sta_percent_deprbas_fed(TaxCreditIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_TaxCreditIncentives_itc_sta_percent_deprbas_fed_nset, self->data_ptr);
}

static PyObject *
TaxCreditIncentives_get_itc_sta_percent_deprbas_sta(TaxCreditIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_TaxCreditIncentives_itc_sta_percent_deprbas_sta_nget, self->data_ptr);
}

static int
TaxCreditIncentives_set_itc_sta_percent_deprbas_sta(TaxCreditIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_TaxCreditIncentives_itc_sta_percent_deprbas_sta_nset, self->data_ptr);
}

static PyObject *
TaxCreditIncentives_get_itc_sta_percent_maxvalue(TaxCreditIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_TaxCreditIncentives_itc_sta_percent_maxvalue_nget, self->data_ptr);
}

static int
TaxCreditIncentives_set_itc_sta_percent_maxvalue(TaxCreditIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_TaxCreditIncentives_itc_sta_percent_maxvalue_nset, self->data_ptr);
}

static PyObject *
TaxCreditIncentives_get_ptc_fed_amount(TaxCreditIncentivesObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_TaxCreditIncentives_ptc_fed_amount_aget, self->data_ptr);
}

static int
TaxCreditIncentives_set_ptc_fed_amount(TaxCreditIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Levpartflip_TaxCreditIncentives_ptc_fed_amount_aset, self->data_ptr);
}

static PyObject *
TaxCreditIncentives_get_ptc_fed_escal(TaxCreditIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_TaxCreditIncentives_ptc_fed_escal_nget, self->data_ptr);
}

static int
TaxCreditIncentives_set_ptc_fed_escal(TaxCreditIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_TaxCreditIncentives_ptc_fed_escal_nset, self->data_ptr);
}

static PyObject *
TaxCreditIncentives_get_ptc_fed_term(TaxCreditIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_TaxCreditIncentives_ptc_fed_term_nget, self->data_ptr);
}

static int
TaxCreditIncentives_set_ptc_fed_term(TaxCreditIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_TaxCreditIncentives_ptc_fed_term_nset, self->data_ptr);
}

static PyObject *
TaxCreditIncentives_get_ptc_sta_amount(TaxCreditIncentivesObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_TaxCreditIncentives_ptc_sta_amount_aget, self->data_ptr);
}

static int
TaxCreditIncentives_set_ptc_sta_amount(TaxCreditIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Levpartflip_TaxCreditIncentives_ptc_sta_amount_aset, self->data_ptr);
}

static PyObject *
TaxCreditIncentives_get_ptc_sta_escal(TaxCreditIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_TaxCreditIncentives_ptc_sta_escal_nget, self->data_ptr);
}

static int
TaxCreditIncentives_set_ptc_sta_escal(TaxCreditIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_TaxCreditIncentives_ptc_sta_escal_nset, self->data_ptr);
}

static PyObject *
TaxCreditIncentives_get_ptc_sta_term(TaxCreditIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_TaxCreditIncentives_ptc_sta_term_nget, self->data_ptr);
}

static int
TaxCreditIncentives_set_ptc_sta_term(TaxCreditIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_TaxCreditIncentives_ptc_sta_term_nset, self->data_ptr);
}

static PyGetSetDef TaxCreditIncentives_getset[] = {
{"itc_fed_amount", (getter)TaxCreditIncentives_get_itc_fed_amount,(setter)TaxCreditIncentives_set_itc_fed_amount,
	PyDoc_STR("*float*: Federal amount-based ITC amount [$]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"itc_fed_amount_deprbas_fed", (getter)TaxCreditIncentives_get_itc_fed_amount_deprbas_fed,(setter)TaxCreditIncentives_set_itc_fed_amount_deprbas_fed,
	PyDoc_STR("*float*: Federal amount-based ITC reduces federal depreciation basis [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"itc_fed_amount_deprbas_sta", (getter)TaxCreditIncentives_get_itc_fed_amount_deprbas_sta,(setter)TaxCreditIncentives_set_itc_fed_amount_deprbas_sta,
	PyDoc_STR("*float*: Federal amount-based ITC reduces state depreciation basis [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"itc_fed_percent", (getter)TaxCreditIncentives_get_itc_fed_percent,(setter)TaxCreditIncentives_set_itc_fed_percent,
	PyDoc_STR("*float*: Federal percentage-based ITC percent [%]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"itc_fed_percent_deprbas_fed", (getter)TaxCreditIncentives_get_itc_fed_percent_deprbas_fed,(setter)TaxCreditIncentives_set_itc_fed_percent_deprbas_fed,
	PyDoc_STR("*float*: Federal percentage-based ITC reduces federal depreciation basis [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"itc_fed_percent_deprbas_sta", (getter)TaxCreditIncentives_get_itc_fed_percent_deprbas_sta,(setter)TaxCreditIncentives_set_itc_fed_percent_deprbas_sta,
	PyDoc_STR("*float*: Federal percentage-based ITC reduces state depreciation basis [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"itc_fed_percent_maxvalue", (getter)TaxCreditIncentives_get_itc_fed_percent_maxvalue,(setter)TaxCreditIncentives_set_itc_fed_percent_maxvalue,
	PyDoc_STR("*float*: Federal percentage-based ITC maximum value [$]\n\n*Required*: set to 1e99 if not provided."),
 	NULL},
{"itc_sta_amount", (getter)TaxCreditIncentives_get_itc_sta_amount,(setter)TaxCreditIncentives_set_itc_sta_amount,
	PyDoc_STR("*float*: State amount-based ITC amount [$]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"itc_sta_amount_deprbas_fed", (getter)TaxCreditIncentives_get_itc_sta_amount_deprbas_fed,(setter)TaxCreditIncentives_set_itc_sta_amount_deprbas_fed,
	PyDoc_STR("*float*: State amount-based ITC reduces federal depreciation basis [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"itc_sta_amount_deprbas_sta", (getter)TaxCreditIncentives_get_itc_sta_amount_deprbas_sta,(setter)TaxCreditIncentives_set_itc_sta_amount_deprbas_sta,
	PyDoc_STR("*float*: State amount-based ITC reduces state depreciation basis [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"itc_sta_percent", (getter)TaxCreditIncentives_get_itc_sta_percent,(setter)TaxCreditIncentives_set_itc_sta_percent,
	PyDoc_STR("*float*: State percentage-based ITC percent [%]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"itc_sta_percent_deprbas_fed", (getter)TaxCreditIncentives_get_itc_sta_percent_deprbas_fed,(setter)TaxCreditIncentives_set_itc_sta_percent_deprbas_fed,
	PyDoc_STR("*float*: State percentage-based ITC reduces federal depreciation basis [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"itc_sta_percent_deprbas_sta", (getter)TaxCreditIncentives_get_itc_sta_percent_deprbas_sta,(setter)TaxCreditIncentives_set_itc_sta_percent_deprbas_sta,
	PyDoc_STR("*float*: State percentage-based ITC reduces state depreciation basis [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"itc_sta_percent_maxvalue", (getter)TaxCreditIncentives_get_itc_sta_percent_maxvalue,(setter)TaxCreditIncentives_set_itc_sta_percent_maxvalue,
	PyDoc_STR("*float*: State percentage-based ITC maximum Value [$]\n\n*Required*: set to 1e99 if not provided."),
 	NULL},
{"ptc_fed_amount", (getter)TaxCreditIncentives_get_ptc_fed_amount,(setter)TaxCreditIncentives_set_ptc_fed_amount,
	PyDoc_STR("*sequence*: Federal PTC amount [$/kWh]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"ptc_fed_escal", (getter)TaxCreditIncentives_get_ptc_fed_escal,(setter)TaxCreditIncentives_set_ptc_fed_escal,
	PyDoc_STR("*float*: Federal PTC escalation [%/year]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"ptc_fed_term", (getter)TaxCreditIncentives_get_ptc_fed_term,(setter)TaxCreditIncentives_set_ptc_fed_term,
	PyDoc_STR("*float*: Federal PTC term [years]\n\n*Required*: set to 10 if not provided."),
 	NULL},
{"ptc_sta_amount", (getter)TaxCreditIncentives_get_ptc_sta_amount,(setter)TaxCreditIncentives_set_ptc_sta_amount,
	PyDoc_STR("*sequence*: State PTC amount [$/kWh]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"ptc_sta_escal", (getter)TaxCreditIncentives_get_ptc_sta_escal,(setter)TaxCreditIncentives_set_ptc_sta_escal,
	PyDoc_STR("*float*: State PTC escalation [%/year]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"ptc_sta_term", (getter)TaxCreditIncentives_get_ptc_sta_term,(setter)TaxCreditIncentives_set_ptc_sta_term,
	PyDoc_STR("*float*: State PTC term [years]\n\n*Required*: set to 10 if not provided."),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject TaxCreditIncentives_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Levpartflip.TaxCreditIncentives",             /*tp_name*/
		sizeof(TaxCreditIncentivesObject),          /*tp_basicsize*/
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
		TaxCreditIncentives_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		TaxCreditIncentives_getset,          /*tp_getset*/
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
	 * PaymentIncentives Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Levpartflip   data_ptr;
} PaymentIncentivesObject;

static PyTypeObject PaymentIncentives_Type;

static PyObject *
PaymentIncentives_new(SAM_Levpartflip data_ptr)
{
	PyObject* new_obj = PaymentIncentives_Type.tp_alloc(&PaymentIncentives_Type,0);

	PaymentIncentivesObject* PaymentIncentives_obj = (PaymentIncentivesObject*)new_obj;

	PaymentIncentives_obj->data_ptr = data_ptr;

	return new_obj;
}

/* PaymentIncentives methods */

static PyObject *
PaymentIncentives_assign(PaymentIncentivesObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Levpartflip", "PaymentIncentives")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
PaymentIncentives_export(PaymentIncentivesObject *self, PyObject *args)
{
	PyTypeObject* tp = &PaymentIncentives_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef PaymentIncentives_methods[] = {
		{"assign",            (PyCFunction)PaymentIncentives_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``PaymentIncentives_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)PaymentIncentives_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
PaymentIncentives_get_cbi_fed_amount(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_cbi_fed_amount_nget, self->data_ptr);
}

static int
PaymentIncentives_set_cbi_fed_amount(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_cbi_fed_amount_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_cbi_fed_deprbas_fed(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_cbi_fed_deprbas_fed_nget, self->data_ptr);
}

static int
PaymentIncentives_set_cbi_fed_deprbas_fed(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_cbi_fed_deprbas_fed_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_cbi_fed_deprbas_sta(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_cbi_fed_deprbas_sta_nget, self->data_ptr);
}

static int
PaymentIncentives_set_cbi_fed_deprbas_sta(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_cbi_fed_deprbas_sta_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_cbi_fed_maxvalue(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_cbi_fed_maxvalue_nget, self->data_ptr);
}

static int
PaymentIncentives_set_cbi_fed_maxvalue(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_cbi_fed_maxvalue_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_cbi_fed_tax_fed(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_cbi_fed_tax_fed_nget, self->data_ptr);
}

static int
PaymentIncentives_set_cbi_fed_tax_fed(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_cbi_fed_tax_fed_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_cbi_fed_tax_sta(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_cbi_fed_tax_sta_nget, self->data_ptr);
}

static int
PaymentIncentives_set_cbi_fed_tax_sta(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_cbi_fed_tax_sta_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_cbi_oth_amount(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_cbi_oth_amount_nget, self->data_ptr);
}

static int
PaymentIncentives_set_cbi_oth_amount(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_cbi_oth_amount_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_cbi_oth_deprbas_fed(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_cbi_oth_deprbas_fed_nget, self->data_ptr);
}

static int
PaymentIncentives_set_cbi_oth_deprbas_fed(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_cbi_oth_deprbas_fed_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_cbi_oth_deprbas_sta(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_cbi_oth_deprbas_sta_nget, self->data_ptr);
}

static int
PaymentIncentives_set_cbi_oth_deprbas_sta(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_cbi_oth_deprbas_sta_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_cbi_oth_maxvalue(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_cbi_oth_maxvalue_nget, self->data_ptr);
}

static int
PaymentIncentives_set_cbi_oth_maxvalue(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_cbi_oth_maxvalue_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_cbi_oth_tax_fed(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_cbi_oth_tax_fed_nget, self->data_ptr);
}

static int
PaymentIncentives_set_cbi_oth_tax_fed(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_cbi_oth_tax_fed_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_cbi_oth_tax_sta(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_cbi_oth_tax_sta_nget, self->data_ptr);
}

static int
PaymentIncentives_set_cbi_oth_tax_sta(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_cbi_oth_tax_sta_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_cbi_sta_amount(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_cbi_sta_amount_nget, self->data_ptr);
}

static int
PaymentIncentives_set_cbi_sta_amount(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_cbi_sta_amount_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_cbi_sta_deprbas_fed(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_cbi_sta_deprbas_fed_nget, self->data_ptr);
}

static int
PaymentIncentives_set_cbi_sta_deprbas_fed(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_cbi_sta_deprbas_fed_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_cbi_sta_deprbas_sta(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_cbi_sta_deprbas_sta_nget, self->data_ptr);
}

static int
PaymentIncentives_set_cbi_sta_deprbas_sta(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_cbi_sta_deprbas_sta_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_cbi_sta_maxvalue(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_cbi_sta_maxvalue_nget, self->data_ptr);
}

static int
PaymentIncentives_set_cbi_sta_maxvalue(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_cbi_sta_maxvalue_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_cbi_sta_tax_fed(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_cbi_sta_tax_fed_nget, self->data_ptr);
}

static int
PaymentIncentives_set_cbi_sta_tax_fed(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_cbi_sta_tax_fed_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_cbi_sta_tax_sta(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_cbi_sta_tax_sta_nget, self->data_ptr);
}

static int
PaymentIncentives_set_cbi_sta_tax_sta(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_cbi_sta_tax_sta_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_cbi_uti_amount(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_cbi_uti_amount_nget, self->data_ptr);
}

static int
PaymentIncentives_set_cbi_uti_amount(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_cbi_uti_amount_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_cbi_uti_deprbas_fed(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_cbi_uti_deprbas_fed_nget, self->data_ptr);
}

static int
PaymentIncentives_set_cbi_uti_deprbas_fed(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_cbi_uti_deprbas_fed_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_cbi_uti_deprbas_sta(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_cbi_uti_deprbas_sta_nget, self->data_ptr);
}

static int
PaymentIncentives_set_cbi_uti_deprbas_sta(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_cbi_uti_deprbas_sta_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_cbi_uti_maxvalue(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_cbi_uti_maxvalue_nget, self->data_ptr);
}

static int
PaymentIncentives_set_cbi_uti_maxvalue(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_cbi_uti_maxvalue_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_cbi_uti_tax_fed(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_cbi_uti_tax_fed_nget, self->data_ptr);
}

static int
PaymentIncentives_set_cbi_uti_tax_fed(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_cbi_uti_tax_fed_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_cbi_uti_tax_sta(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_cbi_uti_tax_sta_nget, self->data_ptr);
}

static int
PaymentIncentives_set_cbi_uti_tax_sta(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_cbi_uti_tax_sta_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_ibi_fed_amount(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_ibi_fed_amount_nget, self->data_ptr);
}

static int
PaymentIncentives_set_ibi_fed_amount(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_ibi_fed_amount_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_ibi_fed_amount_deprbas_fed(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_ibi_fed_amount_deprbas_fed_nget, self->data_ptr);
}

static int
PaymentIncentives_set_ibi_fed_amount_deprbas_fed(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_ibi_fed_amount_deprbas_fed_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_ibi_fed_amount_deprbas_sta(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_ibi_fed_amount_deprbas_sta_nget, self->data_ptr);
}

static int
PaymentIncentives_set_ibi_fed_amount_deprbas_sta(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_ibi_fed_amount_deprbas_sta_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_ibi_fed_amount_tax_fed(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_ibi_fed_amount_tax_fed_nget, self->data_ptr);
}

static int
PaymentIncentives_set_ibi_fed_amount_tax_fed(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_ibi_fed_amount_tax_fed_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_ibi_fed_amount_tax_sta(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_ibi_fed_amount_tax_sta_nget, self->data_ptr);
}

static int
PaymentIncentives_set_ibi_fed_amount_tax_sta(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_ibi_fed_amount_tax_sta_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_ibi_fed_percent(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_ibi_fed_percent_nget, self->data_ptr);
}

static int
PaymentIncentives_set_ibi_fed_percent(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_ibi_fed_percent_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_ibi_fed_percent_deprbas_fed(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_ibi_fed_percent_deprbas_fed_nget, self->data_ptr);
}

static int
PaymentIncentives_set_ibi_fed_percent_deprbas_fed(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_ibi_fed_percent_deprbas_fed_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_ibi_fed_percent_deprbas_sta(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_ibi_fed_percent_deprbas_sta_nget, self->data_ptr);
}

static int
PaymentIncentives_set_ibi_fed_percent_deprbas_sta(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_ibi_fed_percent_deprbas_sta_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_ibi_fed_percent_maxvalue(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_ibi_fed_percent_maxvalue_nget, self->data_ptr);
}

static int
PaymentIncentives_set_ibi_fed_percent_maxvalue(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_ibi_fed_percent_maxvalue_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_ibi_fed_percent_tax_fed(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_ibi_fed_percent_tax_fed_nget, self->data_ptr);
}

static int
PaymentIncentives_set_ibi_fed_percent_tax_fed(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_ibi_fed_percent_tax_fed_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_ibi_fed_percent_tax_sta(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_ibi_fed_percent_tax_sta_nget, self->data_ptr);
}

static int
PaymentIncentives_set_ibi_fed_percent_tax_sta(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_ibi_fed_percent_tax_sta_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_ibi_oth_amount(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_ibi_oth_amount_nget, self->data_ptr);
}

static int
PaymentIncentives_set_ibi_oth_amount(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_ibi_oth_amount_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_ibi_oth_amount_deprbas_fed(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_ibi_oth_amount_deprbas_fed_nget, self->data_ptr);
}

static int
PaymentIncentives_set_ibi_oth_amount_deprbas_fed(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_ibi_oth_amount_deprbas_fed_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_ibi_oth_amount_deprbas_sta(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_ibi_oth_amount_deprbas_sta_nget, self->data_ptr);
}

static int
PaymentIncentives_set_ibi_oth_amount_deprbas_sta(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_ibi_oth_amount_deprbas_sta_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_ibi_oth_amount_tax_fed(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_ibi_oth_amount_tax_fed_nget, self->data_ptr);
}

static int
PaymentIncentives_set_ibi_oth_amount_tax_fed(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_ibi_oth_amount_tax_fed_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_ibi_oth_amount_tax_sta(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_ibi_oth_amount_tax_sta_nget, self->data_ptr);
}

static int
PaymentIncentives_set_ibi_oth_amount_tax_sta(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_ibi_oth_amount_tax_sta_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_ibi_oth_percent(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_ibi_oth_percent_nget, self->data_ptr);
}

static int
PaymentIncentives_set_ibi_oth_percent(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_ibi_oth_percent_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_ibi_oth_percent_deprbas_fed(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_ibi_oth_percent_deprbas_fed_nget, self->data_ptr);
}

static int
PaymentIncentives_set_ibi_oth_percent_deprbas_fed(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_ibi_oth_percent_deprbas_fed_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_ibi_oth_percent_deprbas_sta(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_ibi_oth_percent_deprbas_sta_nget, self->data_ptr);
}

static int
PaymentIncentives_set_ibi_oth_percent_deprbas_sta(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_ibi_oth_percent_deprbas_sta_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_ibi_oth_percent_maxvalue(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_ibi_oth_percent_maxvalue_nget, self->data_ptr);
}

static int
PaymentIncentives_set_ibi_oth_percent_maxvalue(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_ibi_oth_percent_maxvalue_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_ibi_oth_percent_tax_fed(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_ibi_oth_percent_tax_fed_nget, self->data_ptr);
}

static int
PaymentIncentives_set_ibi_oth_percent_tax_fed(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_ibi_oth_percent_tax_fed_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_ibi_oth_percent_tax_sta(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_ibi_oth_percent_tax_sta_nget, self->data_ptr);
}

static int
PaymentIncentives_set_ibi_oth_percent_tax_sta(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_ibi_oth_percent_tax_sta_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_ibi_sta_amount(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_ibi_sta_amount_nget, self->data_ptr);
}

static int
PaymentIncentives_set_ibi_sta_amount(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_ibi_sta_amount_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_ibi_sta_amount_deprbas_fed(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_ibi_sta_amount_deprbas_fed_nget, self->data_ptr);
}

static int
PaymentIncentives_set_ibi_sta_amount_deprbas_fed(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_ibi_sta_amount_deprbas_fed_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_ibi_sta_amount_deprbas_sta(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_ibi_sta_amount_deprbas_sta_nget, self->data_ptr);
}

static int
PaymentIncentives_set_ibi_sta_amount_deprbas_sta(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_ibi_sta_amount_deprbas_sta_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_ibi_sta_amount_tax_fed(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_ibi_sta_amount_tax_fed_nget, self->data_ptr);
}

static int
PaymentIncentives_set_ibi_sta_amount_tax_fed(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_ibi_sta_amount_tax_fed_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_ibi_sta_amount_tax_sta(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_ibi_sta_amount_tax_sta_nget, self->data_ptr);
}

static int
PaymentIncentives_set_ibi_sta_amount_tax_sta(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_ibi_sta_amount_tax_sta_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_ibi_sta_percent(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_ibi_sta_percent_nget, self->data_ptr);
}

static int
PaymentIncentives_set_ibi_sta_percent(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_ibi_sta_percent_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_ibi_sta_percent_deprbas_fed(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_ibi_sta_percent_deprbas_fed_nget, self->data_ptr);
}

static int
PaymentIncentives_set_ibi_sta_percent_deprbas_fed(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_ibi_sta_percent_deprbas_fed_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_ibi_sta_percent_deprbas_sta(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_ibi_sta_percent_deprbas_sta_nget, self->data_ptr);
}

static int
PaymentIncentives_set_ibi_sta_percent_deprbas_sta(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_ibi_sta_percent_deprbas_sta_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_ibi_sta_percent_maxvalue(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_ibi_sta_percent_maxvalue_nget, self->data_ptr);
}

static int
PaymentIncentives_set_ibi_sta_percent_maxvalue(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_ibi_sta_percent_maxvalue_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_ibi_sta_percent_tax_fed(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_ibi_sta_percent_tax_fed_nget, self->data_ptr);
}

static int
PaymentIncentives_set_ibi_sta_percent_tax_fed(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_ibi_sta_percent_tax_fed_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_ibi_sta_percent_tax_sta(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_ibi_sta_percent_tax_sta_nget, self->data_ptr);
}

static int
PaymentIncentives_set_ibi_sta_percent_tax_sta(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_ibi_sta_percent_tax_sta_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_ibi_uti_amount(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_ibi_uti_amount_nget, self->data_ptr);
}

static int
PaymentIncentives_set_ibi_uti_amount(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_ibi_uti_amount_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_ibi_uti_amount_deprbas_fed(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_ibi_uti_amount_deprbas_fed_nget, self->data_ptr);
}

static int
PaymentIncentives_set_ibi_uti_amount_deprbas_fed(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_ibi_uti_amount_deprbas_fed_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_ibi_uti_amount_deprbas_sta(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_ibi_uti_amount_deprbas_sta_nget, self->data_ptr);
}

static int
PaymentIncentives_set_ibi_uti_amount_deprbas_sta(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_ibi_uti_amount_deprbas_sta_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_ibi_uti_amount_tax_fed(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_ibi_uti_amount_tax_fed_nget, self->data_ptr);
}

static int
PaymentIncentives_set_ibi_uti_amount_tax_fed(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_ibi_uti_amount_tax_fed_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_ibi_uti_amount_tax_sta(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_ibi_uti_amount_tax_sta_nget, self->data_ptr);
}

static int
PaymentIncentives_set_ibi_uti_amount_tax_sta(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_ibi_uti_amount_tax_sta_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_ibi_uti_percent(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_ibi_uti_percent_nget, self->data_ptr);
}

static int
PaymentIncentives_set_ibi_uti_percent(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_ibi_uti_percent_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_ibi_uti_percent_deprbas_fed(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_ibi_uti_percent_deprbas_fed_nget, self->data_ptr);
}

static int
PaymentIncentives_set_ibi_uti_percent_deprbas_fed(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_ibi_uti_percent_deprbas_fed_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_ibi_uti_percent_deprbas_sta(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_ibi_uti_percent_deprbas_sta_nget, self->data_ptr);
}

static int
PaymentIncentives_set_ibi_uti_percent_deprbas_sta(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_ibi_uti_percent_deprbas_sta_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_ibi_uti_percent_maxvalue(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_ibi_uti_percent_maxvalue_nget, self->data_ptr);
}

static int
PaymentIncentives_set_ibi_uti_percent_maxvalue(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_ibi_uti_percent_maxvalue_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_ibi_uti_percent_tax_fed(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_ibi_uti_percent_tax_fed_nget, self->data_ptr);
}

static int
PaymentIncentives_set_ibi_uti_percent_tax_fed(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_ibi_uti_percent_tax_fed_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_ibi_uti_percent_tax_sta(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_ibi_uti_percent_tax_sta_nget, self->data_ptr);
}

static int
PaymentIncentives_set_ibi_uti_percent_tax_sta(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_ibi_uti_percent_tax_sta_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_pbi_fed_amount(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_PaymentIncentives_pbi_fed_amount_aget, self->data_ptr);
}

static int
PaymentIncentives_set_pbi_fed_amount(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Levpartflip_PaymentIncentives_pbi_fed_amount_aset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_pbi_fed_escal(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_pbi_fed_escal_nget, self->data_ptr);
}

static int
PaymentIncentives_set_pbi_fed_escal(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_pbi_fed_escal_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_pbi_fed_tax_fed(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_pbi_fed_tax_fed_nget, self->data_ptr);
}

static int
PaymentIncentives_set_pbi_fed_tax_fed(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_pbi_fed_tax_fed_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_pbi_fed_tax_sta(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_pbi_fed_tax_sta_nget, self->data_ptr);
}

static int
PaymentIncentives_set_pbi_fed_tax_sta(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_pbi_fed_tax_sta_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_pbi_fed_term(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_pbi_fed_term_nget, self->data_ptr);
}

static int
PaymentIncentives_set_pbi_fed_term(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_pbi_fed_term_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_pbi_oth_amount(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_PaymentIncentives_pbi_oth_amount_aget, self->data_ptr);
}

static int
PaymentIncentives_set_pbi_oth_amount(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Levpartflip_PaymentIncentives_pbi_oth_amount_aset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_pbi_oth_escal(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_pbi_oth_escal_nget, self->data_ptr);
}

static int
PaymentIncentives_set_pbi_oth_escal(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_pbi_oth_escal_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_pbi_oth_tax_fed(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_pbi_oth_tax_fed_nget, self->data_ptr);
}

static int
PaymentIncentives_set_pbi_oth_tax_fed(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_pbi_oth_tax_fed_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_pbi_oth_tax_sta(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_pbi_oth_tax_sta_nget, self->data_ptr);
}

static int
PaymentIncentives_set_pbi_oth_tax_sta(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_pbi_oth_tax_sta_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_pbi_oth_term(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_pbi_oth_term_nget, self->data_ptr);
}

static int
PaymentIncentives_set_pbi_oth_term(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_pbi_oth_term_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_pbi_sta_amount(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_PaymentIncentives_pbi_sta_amount_aget, self->data_ptr);
}

static int
PaymentIncentives_set_pbi_sta_amount(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Levpartflip_PaymentIncentives_pbi_sta_amount_aset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_pbi_sta_escal(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_pbi_sta_escal_nget, self->data_ptr);
}

static int
PaymentIncentives_set_pbi_sta_escal(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_pbi_sta_escal_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_pbi_sta_tax_fed(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_pbi_sta_tax_fed_nget, self->data_ptr);
}

static int
PaymentIncentives_set_pbi_sta_tax_fed(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_pbi_sta_tax_fed_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_pbi_sta_tax_sta(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_pbi_sta_tax_sta_nget, self->data_ptr);
}

static int
PaymentIncentives_set_pbi_sta_tax_sta(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_pbi_sta_tax_sta_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_pbi_sta_term(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_pbi_sta_term_nget, self->data_ptr);
}

static int
PaymentIncentives_set_pbi_sta_term(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_pbi_sta_term_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_pbi_uti_amount(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_PaymentIncentives_pbi_uti_amount_aget, self->data_ptr);
}

static int
PaymentIncentives_set_pbi_uti_amount(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Levpartflip_PaymentIncentives_pbi_uti_amount_aset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_pbi_uti_escal(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_pbi_uti_escal_nget, self->data_ptr);
}

static int
PaymentIncentives_set_pbi_uti_escal(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_pbi_uti_escal_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_pbi_uti_tax_fed(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_pbi_uti_tax_fed_nget, self->data_ptr);
}

static int
PaymentIncentives_set_pbi_uti_tax_fed(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_pbi_uti_tax_fed_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_pbi_uti_tax_sta(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_pbi_uti_tax_sta_nget, self->data_ptr);
}

static int
PaymentIncentives_set_pbi_uti_tax_sta(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_pbi_uti_tax_sta_nset, self->data_ptr);
}

static PyObject *
PaymentIncentives_get_pbi_uti_term(PaymentIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_PaymentIncentives_pbi_uti_term_nget, self->data_ptr);
}

static int
PaymentIncentives_set_pbi_uti_term(PaymentIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_PaymentIncentives_pbi_uti_term_nset, self->data_ptr);
}

static PyGetSetDef PaymentIncentives_getset[] = {
{"cbi_fed_amount", (getter)PaymentIncentives_get_cbi_fed_amount,(setter)PaymentIncentives_set_cbi_fed_amount,
	PyDoc_STR("*float*: Federal CBI amount [$/Watt]\n\n*Required*: set to 0.0 if not provided."),
 	NULL},
{"cbi_fed_deprbas_fed", (getter)PaymentIncentives_get_cbi_fed_deprbas_fed,(setter)PaymentIncentives_set_cbi_fed_deprbas_fed,
	PyDoc_STR("*float*: Federal CBI reduces federal depreciation basis [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"cbi_fed_deprbas_sta", (getter)PaymentIncentives_get_cbi_fed_deprbas_sta,(setter)PaymentIncentives_set_cbi_fed_deprbas_sta,
	PyDoc_STR("*float*: Federal CBI reduces state depreciation basis [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"cbi_fed_maxvalue", (getter)PaymentIncentives_get_cbi_fed_maxvalue,(setter)PaymentIncentives_set_cbi_fed_maxvalue,
	PyDoc_STR("*float*: Federal CBI maximum [$]\n\n*Required*: set to 1e99 if not provided."),
 	NULL},
{"cbi_fed_tax_fed", (getter)PaymentIncentives_get_cbi_fed_tax_fed,(setter)PaymentIncentives_set_cbi_fed_tax_fed,
	PyDoc_STR("*float*: Federal CBI federal taxable [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"cbi_fed_tax_sta", (getter)PaymentIncentives_get_cbi_fed_tax_sta,(setter)PaymentIncentives_set_cbi_fed_tax_sta,
	PyDoc_STR("*float*: Federal CBI state taxable [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"cbi_oth_amount", (getter)PaymentIncentives_get_cbi_oth_amount,(setter)PaymentIncentives_set_cbi_oth_amount,
	PyDoc_STR("*float*: Other CBI amount [$/Watt]\n\n*Required*: set to 0.0 if not provided."),
 	NULL},
{"cbi_oth_deprbas_fed", (getter)PaymentIncentives_get_cbi_oth_deprbas_fed,(setter)PaymentIncentives_set_cbi_oth_deprbas_fed,
	PyDoc_STR("*float*: Other CBI reduces federal depreciation basis [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"cbi_oth_deprbas_sta", (getter)PaymentIncentives_get_cbi_oth_deprbas_sta,(setter)PaymentIncentives_set_cbi_oth_deprbas_sta,
	PyDoc_STR("*float*: Other CBI reduces state depreciation basis [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"cbi_oth_maxvalue", (getter)PaymentIncentives_get_cbi_oth_maxvalue,(setter)PaymentIncentives_set_cbi_oth_maxvalue,
	PyDoc_STR("*float*: Other CBI maximum [$]\n\n*Required*: set to 1e99 if not provided."),
 	NULL},
{"cbi_oth_tax_fed", (getter)PaymentIncentives_get_cbi_oth_tax_fed,(setter)PaymentIncentives_set_cbi_oth_tax_fed,
	PyDoc_STR("*float*: Other CBI federal taxable [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"cbi_oth_tax_sta", (getter)PaymentIncentives_get_cbi_oth_tax_sta,(setter)PaymentIncentives_set_cbi_oth_tax_sta,
	PyDoc_STR("*float*: Other CBI state taxable [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"cbi_sta_amount", (getter)PaymentIncentives_get_cbi_sta_amount,(setter)PaymentIncentives_set_cbi_sta_amount,
	PyDoc_STR("*float*: State CBI amount [$/Watt]\n\n*Required*: set to 0.0 if not provided."),
 	NULL},
{"cbi_sta_deprbas_fed", (getter)PaymentIncentives_get_cbi_sta_deprbas_fed,(setter)PaymentIncentives_set_cbi_sta_deprbas_fed,
	PyDoc_STR("*float*: State CBI reduces federal depreciation basis [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"cbi_sta_deprbas_sta", (getter)PaymentIncentives_get_cbi_sta_deprbas_sta,(setter)PaymentIncentives_set_cbi_sta_deprbas_sta,
	PyDoc_STR("*float*: State CBI reduces state depreciation basis [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"cbi_sta_maxvalue", (getter)PaymentIncentives_get_cbi_sta_maxvalue,(setter)PaymentIncentives_set_cbi_sta_maxvalue,
	PyDoc_STR("*float*: State CBI maximum [$]\n\n*Required*: set to 1e99 if not provided."),
 	NULL},
{"cbi_sta_tax_fed", (getter)PaymentIncentives_get_cbi_sta_tax_fed,(setter)PaymentIncentives_set_cbi_sta_tax_fed,
	PyDoc_STR("*float*: State CBI federal taxable [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"cbi_sta_tax_sta", (getter)PaymentIncentives_get_cbi_sta_tax_sta,(setter)PaymentIncentives_set_cbi_sta_tax_sta,
	PyDoc_STR("*float*: State CBI state taxable [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"cbi_uti_amount", (getter)PaymentIncentives_get_cbi_uti_amount,(setter)PaymentIncentives_set_cbi_uti_amount,
	PyDoc_STR("*float*: Utility CBI amount [$/Watt]\n\n*Required*: set to 0.0 if not provided."),
 	NULL},
{"cbi_uti_deprbas_fed", (getter)PaymentIncentives_get_cbi_uti_deprbas_fed,(setter)PaymentIncentives_set_cbi_uti_deprbas_fed,
	PyDoc_STR("*float*: Utility CBI reduces federal depreciation basis [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"cbi_uti_deprbas_sta", (getter)PaymentIncentives_get_cbi_uti_deprbas_sta,(setter)PaymentIncentives_set_cbi_uti_deprbas_sta,
	PyDoc_STR("*float*: Utility CBI reduces state depreciation basis [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"cbi_uti_maxvalue", (getter)PaymentIncentives_get_cbi_uti_maxvalue,(setter)PaymentIncentives_set_cbi_uti_maxvalue,
	PyDoc_STR("*float*: Utility CBI maximum [$]\n\n*Required*: set to 1e99 if not provided."),
 	NULL},
{"cbi_uti_tax_fed", (getter)PaymentIncentives_get_cbi_uti_tax_fed,(setter)PaymentIncentives_set_cbi_uti_tax_fed,
	PyDoc_STR("*float*: Utility CBI federal taxable [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"cbi_uti_tax_sta", (getter)PaymentIncentives_get_cbi_uti_tax_sta,(setter)PaymentIncentives_set_cbi_uti_tax_sta,
	PyDoc_STR("*float*: Utility CBI state taxable [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"ibi_fed_amount", (getter)PaymentIncentives_get_ibi_fed_amount,(setter)PaymentIncentives_set_ibi_fed_amount,
	PyDoc_STR("*float*: Federal amount-based IBI amount [$]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"ibi_fed_amount_deprbas_fed", (getter)PaymentIncentives_get_ibi_fed_amount_deprbas_fed,(setter)PaymentIncentives_set_ibi_fed_amount_deprbas_fed,
	PyDoc_STR("*float*: Federal amount-based IBI reduces federal depreciation basis [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"ibi_fed_amount_deprbas_sta", (getter)PaymentIncentives_get_ibi_fed_amount_deprbas_sta,(setter)PaymentIncentives_set_ibi_fed_amount_deprbas_sta,
	PyDoc_STR("*float*: Federal amount-based IBI reduces state depreciation basis [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"ibi_fed_amount_tax_fed", (getter)PaymentIncentives_get_ibi_fed_amount_tax_fed,(setter)PaymentIncentives_set_ibi_fed_amount_tax_fed,
	PyDoc_STR("*float*: Federal amount-based IBI federal taxable [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"ibi_fed_amount_tax_sta", (getter)PaymentIncentives_get_ibi_fed_amount_tax_sta,(setter)PaymentIncentives_set_ibi_fed_amount_tax_sta,
	PyDoc_STR("*float*: Federal amount-based IBI state taxable [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"ibi_fed_percent", (getter)PaymentIncentives_get_ibi_fed_percent,(setter)PaymentIncentives_set_ibi_fed_percent,
	PyDoc_STR("*float*: Federal percentage-based IBI percent [%]\n\n*Required*: set to 0.0 if not provided."),
 	NULL},
{"ibi_fed_percent_deprbas_fed", (getter)PaymentIncentives_get_ibi_fed_percent_deprbas_fed,(setter)PaymentIncentives_set_ibi_fed_percent_deprbas_fed,
	PyDoc_STR("*float*: Federal percentage-based IBI reduces federal depreciation basis [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"ibi_fed_percent_deprbas_sta", (getter)PaymentIncentives_get_ibi_fed_percent_deprbas_sta,(setter)PaymentIncentives_set_ibi_fed_percent_deprbas_sta,
	PyDoc_STR("*float*: Federal percentage-based IBI reduces state depreciation basis [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"ibi_fed_percent_maxvalue", (getter)PaymentIncentives_get_ibi_fed_percent_maxvalue,(setter)PaymentIncentives_set_ibi_fed_percent_maxvalue,
	PyDoc_STR("*float*: Federal percentage-based IBI maximum value [$]\n\n*Required*: set to 1e99 if not provided."),
 	NULL},
{"ibi_fed_percent_tax_fed", (getter)PaymentIncentives_get_ibi_fed_percent_tax_fed,(setter)PaymentIncentives_set_ibi_fed_percent_tax_fed,
	PyDoc_STR("*float*: Federal percentage-based IBI federal taxable [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"ibi_fed_percent_tax_sta", (getter)PaymentIncentives_get_ibi_fed_percent_tax_sta,(setter)PaymentIncentives_set_ibi_fed_percent_tax_sta,
	PyDoc_STR("*float*: Federal percentage-based IBI state taxable [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"ibi_oth_amount", (getter)PaymentIncentives_get_ibi_oth_amount,(setter)PaymentIncentives_set_ibi_oth_amount,
	PyDoc_STR("*float*: Other amount-based IBI amount [$]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"ibi_oth_amount_deprbas_fed", (getter)PaymentIncentives_get_ibi_oth_amount_deprbas_fed,(setter)PaymentIncentives_set_ibi_oth_amount_deprbas_fed,
	PyDoc_STR("*float*: Other amount-based IBI reduces federal depreciation basis [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"ibi_oth_amount_deprbas_sta", (getter)PaymentIncentives_get_ibi_oth_amount_deprbas_sta,(setter)PaymentIncentives_set_ibi_oth_amount_deprbas_sta,
	PyDoc_STR("*float*: Other amount-based IBI reduces state depreciation basis [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"ibi_oth_amount_tax_fed", (getter)PaymentIncentives_get_ibi_oth_amount_tax_fed,(setter)PaymentIncentives_set_ibi_oth_amount_tax_fed,
	PyDoc_STR("*float*: Other amount-based IBI federal taxable [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"ibi_oth_amount_tax_sta", (getter)PaymentIncentives_get_ibi_oth_amount_tax_sta,(setter)PaymentIncentives_set_ibi_oth_amount_tax_sta,
	PyDoc_STR("*float*: Other amount-based IBI state taxable [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"ibi_oth_percent", (getter)PaymentIncentives_get_ibi_oth_percent,(setter)PaymentIncentives_set_ibi_oth_percent,
	PyDoc_STR("*float*: Other percentage-based IBI percent [%]\n\n*Required*: set to 0.0 if not provided."),
 	NULL},
{"ibi_oth_percent_deprbas_fed", (getter)PaymentIncentives_get_ibi_oth_percent_deprbas_fed,(setter)PaymentIncentives_set_ibi_oth_percent_deprbas_fed,
	PyDoc_STR("*float*: Other percentage-based IBI reduces federal depreciation basis [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"ibi_oth_percent_deprbas_sta", (getter)PaymentIncentives_get_ibi_oth_percent_deprbas_sta,(setter)PaymentIncentives_set_ibi_oth_percent_deprbas_sta,
	PyDoc_STR("*float*: Other percentage-based IBI reduces state depreciation basis [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"ibi_oth_percent_maxvalue", (getter)PaymentIncentives_get_ibi_oth_percent_maxvalue,(setter)PaymentIncentives_set_ibi_oth_percent_maxvalue,
	PyDoc_STR("*float*: Other percentage-based IBI maximum value [$]\n\n*Required*: set to 1e99 if not provided."),
 	NULL},
{"ibi_oth_percent_tax_fed", (getter)PaymentIncentives_get_ibi_oth_percent_tax_fed,(setter)PaymentIncentives_set_ibi_oth_percent_tax_fed,
	PyDoc_STR("*float*: Other percentage-based IBI federal taxable [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"ibi_oth_percent_tax_sta", (getter)PaymentIncentives_get_ibi_oth_percent_tax_sta,(setter)PaymentIncentives_set_ibi_oth_percent_tax_sta,
	PyDoc_STR("*float*: Other percentage-based IBI state taxable [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"ibi_sta_amount", (getter)PaymentIncentives_get_ibi_sta_amount,(setter)PaymentIncentives_set_ibi_sta_amount,
	PyDoc_STR("*float*: State amount-based IBI amount [$]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"ibi_sta_amount_deprbas_fed", (getter)PaymentIncentives_get_ibi_sta_amount_deprbas_fed,(setter)PaymentIncentives_set_ibi_sta_amount_deprbas_fed,
	PyDoc_STR("*float*: State amount-based IBI reduces federal depreciation basis [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"ibi_sta_amount_deprbas_sta", (getter)PaymentIncentives_get_ibi_sta_amount_deprbas_sta,(setter)PaymentIncentives_set_ibi_sta_amount_deprbas_sta,
	PyDoc_STR("*float*: State amount-based IBI reduces state depreciation basis [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"ibi_sta_amount_tax_fed", (getter)PaymentIncentives_get_ibi_sta_amount_tax_fed,(setter)PaymentIncentives_set_ibi_sta_amount_tax_fed,
	PyDoc_STR("*float*: State amount-based IBI federal taxable [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"ibi_sta_amount_tax_sta", (getter)PaymentIncentives_get_ibi_sta_amount_tax_sta,(setter)PaymentIncentives_set_ibi_sta_amount_tax_sta,
	PyDoc_STR("*float*: State amount-based IBI state taxable [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"ibi_sta_percent", (getter)PaymentIncentives_get_ibi_sta_percent,(setter)PaymentIncentives_set_ibi_sta_percent,
	PyDoc_STR("*float*: State percentage-based IBI percent [%]\n\n*Required*: set to 0.0 if not provided."),
 	NULL},
{"ibi_sta_percent_deprbas_fed", (getter)PaymentIncentives_get_ibi_sta_percent_deprbas_fed,(setter)PaymentIncentives_set_ibi_sta_percent_deprbas_fed,
	PyDoc_STR("*float*: State percentage-based IBI reduces federal depreciation basis [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"ibi_sta_percent_deprbas_sta", (getter)PaymentIncentives_get_ibi_sta_percent_deprbas_sta,(setter)PaymentIncentives_set_ibi_sta_percent_deprbas_sta,
	PyDoc_STR("*float*: State percentage-based IBI reduces state depreciation basis [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"ibi_sta_percent_maxvalue", (getter)PaymentIncentives_get_ibi_sta_percent_maxvalue,(setter)PaymentIncentives_set_ibi_sta_percent_maxvalue,
	PyDoc_STR("*float*: State percentage-based IBI maximum value [$]\n\n*Required*: set to 1e99 if not provided."),
 	NULL},
{"ibi_sta_percent_tax_fed", (getter)PaymentIncentives_get_ibi_sta_percent_tax_fed,(setter)PaymentIncentives_set_ibi_sta_percent_tax_fed,
	PyDoc_STR("*float*: State percentage-based IBI federal taxable [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"ibi_sta_percent_tax_sta", (getter)PaymentIncentives_get_ibi_sta_percent_tax_sta,(setter)PaymentIncentives_set_ibi_sta_percent_tax_sta,
	PyDoc_STR("*float*: State percentage-based IBI state taxable [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"ibi_uti_amount", (getter)PaymentIncentives_get_ibi_uti_amount,(setter)PaymentIncentives_set_ibi_uti_amount,
	PyDoc_STR("*float*: Utility amount-based IBI amount [$]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"ibi_uti_amount_deprbas_fed", (getter)PaymentIncentives_get_ibi_uti_amount_deprbas_fed,(setter)PaymentIncentives_set_ibi_uti_amount_deprbas_fed,
	PyDoc_STR("*float*: Utility amount-based IBI reduces federal depreciation basis [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"ibi_uti_amount_deprbas_sta", (getter)PaymentIncentives_get_ibi_uti_amount_deprbas_sta,(setter)PaymentIncentives_set_ibi_uti_amount_deprbas_sta,
	PyDoc_STR("*float*: Utility amount-based IBI reduces state depreciation basis [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"ibi_uti_amount_tax_fed", (getter)PaymentIncentives_get_ibi_uti_amount_tax_fed,(setter)PaymentIncentives_set_ibi_uti_amount_tax_fed,
	PyDoc_STR("*float*: Utility amount-based IBI federal taxable [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"ibi_uti_amount_tax_sta", (getter)PaymentIncentives_get_ibi_uti_amount_tax_sta,(setter)PaymentIncentives_set_ibi_uti_amount_tax_sta,
	PyDoc_STR("*float*: Utility amount-based IBI state taxable [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"ibi_uti_percent", (getter)PaymentIncentives_get_ibi_uti_percent,(setter)PaymentIncentives_set_ibi_uti_percent,
	PyDoc_STR("*float*: Utility percentage-based IBI percent [%]\n\n*Required*: set to 0.0 if not provided."),
 	NULL},
{"ibi_uti_percent_deprbas_fed", (getter)PaymentIncentives_get_ibi_uti_percent_deprbas_fed,(setter)PaymentIncentives_set_ibi_uti_percent_deprbas_fed,
	PyDoc_STR("*float*: Utility percentage-based IBI reduces federal depreciation basis [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"ibi_uti_percent_deprbas_sta", (getter)PaymentIncentives_get_ibi_uti_percent_deprbas_sta,(setter)PaymentIncentives_set_ibi_uti_percent_deprbas_sta,
	PyDoc_STR("*float*: Utility percentage-based IBI reduces state depreciation basis [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"ibi_uti_percent_maxvalue", (getter)PaymentIncentives_get_ibi_uti_percent_maxvalue,(setter)PaymentIncentives_set_ibi_uti_percent_maxvalue,
	PyDoc_STR("*float*: Utility percentage-based IBI maximum value [$]\n\n*Required*: set to 1e99 if not provided."),
 	NULL},
{"ibi_uti_percent_tax_fed", (getter)PaymentIncentives_get_ibi_uti_percent_tax_fed,(setter)PaymentIncentives_set_ibi_uti_percent_tax_fed,
	PyDoc_STR("*float*: Utility percentage-based IBI federal taxable [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"ibi_uti_percent_tax_sta", (getter)PaymentIncentives_get_ibi_uti_percent_tax_sta,(setter)PaymentIncentives_set_ibi_uti_percent_tax_sta,
	PyDoc_STR("*float*: Utility percentage-based IBI state taxable [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"pbi_fed_amount", (getter)PaymentIncentives_get_pbi_fed_amount,(setter)PaymentIncentives_set_pbi_fed_amount,
	PyDoc_STR("*sequence*: Federal PBI amount [$/kWh]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"pbi_fed_escal", (getter)PaymentIncentives_get_pbi_fed_escal,(setter)PaymentIncentives_set_pbi_fed_escal,
	PyDoc_STR("*float*: Federal PBI escalation [%]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"pbi_fed_tax_fed", (getter)PaymentIncentives_get_pbi_fed_tax_fed,(setter)PaymentIncentives_set_pbi_fed_tax_fed,
	PyDoc_STR("*float*: Federal PBI federal taxable [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"pbi_fed_tax_sta", (getter)PaymentIncentives_get_pbi_fed_tax_sta,(setter)PaymentIncentives_set_pbi_fed_tax_sta,
	PyDoc_STR("*float*: Federal PBI state taxable [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"pbi_fed_term", (getter)PaymentIncentives_get_pbi_fed_term,(setter)PaymentIncentives_set_pbi_fed_term,
	PyDoc_STR("*float*: Federal PBI term [years]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"pbi_oth_amount", (getter)PaymentIncentives_get_pbi_oth_amount,(setter)PaymentIncentives_set_pbi_oth_amount,
	PyDoc_STR("*sequence*: Other PBI amount [$/kWh]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"pbi_oth_escal", (getter)PaymentIncentives_get_pbi_oth_escal,(setter)PaymentIncentives_set_pbi_oth_escal,
	PyDoc_STR("*float*: Other PBI escalation [%]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"pbi_oth_tax_fed", (getter)PaymentIncentives_get_pbi_oth_tax_fed,(setter)PaymentIncentives_set_pbi_oth_tax_fed,
	PyDoc_STR("*float*: Other PBI federal taxable [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"pbi_oth_tax_sta", (getter)PaymentIncentives_get_pbi_oth_tax_sta,(setter)PaymentIncentives_set_pbi_oth_tax_sta,
	PyDoc_STR("*float*: Other PBI state taxable [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"pbi_oth_term", (getter)PaymentIncentives_get_pbi_oth_term,(setter)PaymentIncentives_set_pbi_oth_term,
	PyDoc_STR("*float*: Other PBI term [years]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"pbi_sta_amount", (getter)PaymentIncentives_get_pbi_sta_amount,(setter)PaymentIncentives_set_pbi_sta_amount,
	PyDoc_STR("*sequence*: State PBI amount [$/kWh]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"pbi_sta_escal", (getter)PaymentIncentives_get_pbi_sta_escal,(setter)PaymentIncentives_set_pbi_sta_escal,
	PyDoc_STR("*float*: State PBI escalation [%]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"pbi_sta_tax_fed", (getter)PaymentIncentives_get_pbi_sta_tax_fed,(setter)PaymentIncentives_set_pbi_sta_tax_fed,
	PyDoc_STR("*float*: State PBI federal taxable [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"pbi_sta_tax_sta", (getter)PaymentIncentives_get_pbi_sta_tax_sta,(setter)PaymentIncentives_set_pbi_sta_tax_sta,
	PyDoc_STR("*float*: State PBI state taxable [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"pbi_sta_term", (getter)PaymentIncentives_get_pbi_sta_term,(setter)PaymentIncentives_set_pbi_sta_term,
	PyDoc_STR("*float*: State PBI term [years]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"pbi_uti_amount", (getter)PaymentIncentives_get_pbi_uti_amount,(setter)PaymentIncentives_set_pbi_uti_amount,
	PyDoc_STR("*sequence*: Utility PBI amount [$/kWh]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"pbi_uti_escal", (getter)PaymentIncentives_get_pbi_uti_escal,(setter)PaymentIncentives_set_pbi_uti_escal,
	PyDoc_STR("*float*: Utility PBI escalation [%]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"pbi_uti_tax_fed", (getter)PaymentIncentives_get_pbi_uti_tax_fed,(setter)PaymentIncentives_set_pbi_uti_tax_fed,
	PyDoc_STR("*float*: Utility PBI federal taxable [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"pbi_uti_tax_sta", (getter)PaymentIncentives_get_pbi_uti_tax_sta,(setter)PaymentIncentives_set_pbi_uti_tax_sta,
	PyDoc_STR("*float*: Utility PBI state taxable [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"pbi_uti_term", (getter)PaymentIncentives_get_pbi_uti_term,(setter)PaymentIncentives_set_pbi_uti_term,
	PyDoc_STR("*float*: Utility PBI term [years]\n\n*Required*: set to 0 if not provided."),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject PaymentIncentives_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Levpartflip.PaymentIncentives",             /*tp_name*/
		sizeof(PaymentIncentivesObject),          /*tp_basicsize*/
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
		PaymentIncentives_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		PaymentIncentives_getset,          /*tp_getset*/
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
	 * SystemOutput Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Levpartflip   data_ptr;
} SystemOutputObject;

static PyTypeObject SystemOutput_Type;

static PyObject *
SystemOutput_new(SAM_Levpartflip data_ptr)
{
	PyObject* new_obj = SystemOutput_Type.tp_alloc(&SystemOutput_Type,0);

	SystemOutputObject* SystemOutput_obj = (SystemOutputObject*)new_obj;

	SystemOutput_obj->data_ptr = data_ptr;

	return new_obj;
}

/* SystemOutput methods */

static PyObject *
SystemOutput_assign(SystemOutputObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Levpartflip", "SystemOutput")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
SystemOutput_export(SystemOutputObject *self, PyObject *args)
{
	PyTypeObject* tp = &SystemOutput_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef SystemOutput_methods[] = {
		{"assign",            (PyCFunction)SystemOutput_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``SystemOutput_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)SystemOutput_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
SystemOutput_get_degradation(SystemOutputObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_SystemOutput_degradation_aget, self->data_ptr);
}

static int
SystemOutput_set_degradation(SystemOutputObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Levpartflip_SystemOutput_degradation_aset, self->data_ptr);
}

static PyObject *
SystemOutput_get_gen(SystemOutputObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_SystemOutput_gen_aget, self->data_ptr);
}

static int
SystemOutput_set_gen(SystemOutputObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Levpartflip_SystemOutput_gen_aset, self->data_ptr);
}

static PyObject *
SystemOutput_get_system_capacity(SystemOutputObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_SystemOutput_system_capacity_nget, self->data_ptr);
}

static int
SystemOutput_set_system_capacity(SystemOutputObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_SystemOutput_system_capacity_nset, self->data_ptr);
}

static PyGetSetDef SystemOutput_getset[] = {
{"degradation", (getter)SystemOutput_get_degradation,(setter)SystemOutput_set_degradation,
	PyDoc_STR("*sequence*: Annual energy degradation\n\n*Required*: True"),
 	NULL},
{"gen", (getter)SystemOutput_get_gen,(setter)SystemOutput_set_gen,
	PyDoc_STR("*sequence*: Power generated by renewable resource [kW]\n\n*Required*: True"),
 	NULL},
{"system_capacity", (getter)SystemOutput_get_system_capacity,(setter)SystemOutput_set_system_capacity,
	PyDoc_STR("*float*: System nameplate capacity [kW]\n\n*Constraints*: MIN=1e-3\n\n*Required*: True"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject SystemOutput_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Levpartflip.SystemOutput",             /*tp_name*/
		sizeof(SystemOutputObject),          /*tp_basicsize*/
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
		SystemOutput_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		SystemOutput_getset,          /*tp_getset*/
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
	 * Moratorium Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Levpartflip   data_ptr;
} MoratoriumObject;

static PyTypeObject Moratorium_Type;

static PyObject *
Moratorium_new(SAM_Levpartflip data_ptr)
{
	PyObject* new_obj = Moratorium_Type.tp_alloc(&Moratorium_Type,0);

	MoratoriumObject* Moratorium_obj = (MoratoriumObject*)new_obj;

	Moratorium_obj->data_ptr = data_ptr;

	return new_obj;
}

/* Moratorium methods */

static PyObject *
Moratorium_assign(MoratoriumObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Levpartflip", "Moratorium")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
Moratorium_export(MoratoriumObject *self, PyObject *args)
{
	PyTypeObject* tp = &Moratorium_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef Moratorium_methods[] = {
		{"assign",            (PyCFunction)Moratorium_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``Moratorium_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)Moratorium_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
Moratorium_get_loan_moratorium(MoratoriumObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Moratorium_loan_moratorium_nget, self->data_ptr);
}

static int
Moratorium_set_loan_moratorium(MoratoriumObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_Moratorium_loan_moratorium_nset, self->data_ptr);
}

static PyGetSetDef Moratorium_getset[] = {
{"loan_moratorium", (getter)Moratorium_get_loan_moratorium,(setter)Moratorium_set_loan_moratorium,
	PyDoc_STR("*float*: Loan moratorium period [years]\n\n*Constraints*: INTEGER,MIN=0\n\n*Required*: set to 0 if not provided."),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject Moratorium_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Levpartflip.Moratorium",             /*tp_name*/
		sizeof(MoratoriumObject),          /*tp_basicsize*/
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
		Moratorium_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		Moratorium_getset,          /*tp_getset*/
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
	 * Recapitalization Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Levpartflip   data_ptr;
} RecapitalizationObject;

static PyTypeObject Recapitalization_Type;

static PyObject *
Recapitalization_new(SAM_Levpartflip data_ptr)
{
	PyObject* new_obj = Recapitalization_Type.tp_alloc(&Recapitalization_Type,0);

	RecapitalizationObject* Recapitalization_obj = (RecapitalizationObject*)new_obj;

	Recapitalization_obj->data_ptr = data_ptr;

	return new_obj;
}

/* Recapitalization methods */

static PyObject *
Recapitalization_assign(RecapitalizationObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Levpartflip", "Recapitalization")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
Recapitalization_export(RecapitalizationObject *self, PyObject *args)
{
	PyTypeObject* tp = &Recapitalization_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef Recapitalization_methods[] = {
		{"assign",            (PyCFunction)Recapitalization_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``Recapitalization_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)Recapitalization_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
Recapitalization_get_system_lifetime_recapitalize(RecapitalizationObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Recapitalization_system_lifetime_recapitalize_aget, self->data_ptr);
}

static int
Recapitalization_set_system_lifetime_recapitalize(RecapitalizationObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Levpartflip_Recapitalization_system_lifetime_recapitalize_aset, self->data_ptr);
}

static PyObject *
Recapitalization_get_system_recapitalization_cost(RecapitalizationObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Recapitalization_system_recapitalization_cost_nget, self->data_ptr);
}

static int
Recapitalization_set_system_recapitalization_cost(RecapitalizationObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_Recapitalization_system_recapitalization_cost_nset, self->data_ptr);
}

static PyObject *
Recapitalization_get_system_recapitalization_escalation(RecapitalizationObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Recapitalization_system_recapitalization_escalation_nget, self->data_ptr);
}

static int
Recapitalization_set_system_recapitalization_escalation(RecapitalizationObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_Recapitalization_system_recapitalization_escalation_nset, self->data_ptr);
}

static PyObject *
Recapitalization_get_system_use_recapitalization(RecapitalizationObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Recapitalization_system_use_recapitalization_nget, self->data_ptr);
}

static int
Recapitalization_set_system_use_recapitalization(RecapitalizationObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_Recapitalization_system_use_recapitalization_nset, self->data_ptr);
}

static PyGetSetDef Recapitalization_getset[] = {
{"system_lifetime_recapitalize", (getter)Recapitalization_get_system_lifetime_recapitalize,(setter)Recapitalization_set_system_lifetime_recapitalize,
	PyDoc_STR("*sequence*: Recapitalization boolean\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"system_recapitalization_cost", (getter)Recapitalization_get_system_recapitalization_cost,(setter)Recapitalization_set_system_recapitalization_cost,
	PyDoc_STR("*float*: Recapitalization cost [$]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"system_recapitalization_escalation", (getter)Recapitalization_get_system_recapitalization_escalation,(setter)Recapitalization_set_system_recapitalization_escalation,
	PyDoc_STR("*float*: Recapitalization escalation (above inflation) [%]\n\n*Constraints*: MIN=0,MAX=100\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"system_use_recapitalization", (getter)Recapitalization_get_system_use_recapitalization,(setter)Recapitalization_set_system_use_recapitalization,
	PyDoc_STR("*float*: Recapitalization expenses [0/1]\n\n*Options*: 0=None,1=Recapitalize\n\n*Constraints*: INTEGER,MIN=0\n\n*Required*: set to 0 if not provided."),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject Recapitalization_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Levpartflip.Recapitalization",             /*tp_name*/
		sizeof(RecapitalizationObject),          /*tp_basicsize*/
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
		Recapitalization_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		Recapitalization_getset,          /*tp_getset*/
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
	 * TimeOfDelivery Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Levpartflip   data_ptr;
} TimeOfDeliveryObject;

static PyTypeObject TimeOfDelivery_Type;

static PyObject *
TimeOfDelivery_new(SAM_Levpartflip data_ptr)
{
	PyObject* new_obj = TimeOfDelivery_Type.tp_alloc(&TimeOfDelivery_Type,0);

	TimeOfDeliveryObject* TimeOfDelivery_obj = (TimeOfDeliveryObject*)new_obj;

	TimeOfDelivery_obj->data_ptr = data_ptr;

	return new_obj;
}

/* TimeOfDelivery methods */

static PyObject *
TimeOfDelivery_assign(TimeOfDeliveryObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Levpartflip", "TimeOfDelivery")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
TimeOfDelivery_export(TimeOfDeliveryObject *self, PyObject *args)
{
	PyTypeObject* tp = &TimeOfDelivery_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef TimeOfDelivery_methods[] = {
		{"assign",            (PyCFunction)TimeOfDelivery_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``TimeOfDelivery_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)TimeOfDelivery_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
TimeOfDelivery_get_dispatch_factor1(TimeOfDeliveryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_TimeOfDelivery_dispatch_factor1_nget, self->data_ptr);
}

static int
TimeOfDelivery_set_dispatch_factor1(TimeOfDeliveryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_TimeOfDelivery_dispatch_factor1_nset, self->data_ptr);
}

static PyObject *
TimeOfDelivery_get_dispatch_factor2(TimeOfDeliveryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_TimeOfDelivery_dispatch_factor2_nget, self->data_ptr);
}

static int
TimeOfDelivery_set_dispatch_factor2(TimeOfDeliveryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_TimeOfDelivery_dispatch_factor2_nset, self->data_ptr);
}

static PyObject *
TimeOfDelivery_get_dispatch_factor3(TimeOfDeliveryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_TimeOfDelivery_dispatch_factor3_nget, self->data_ptr);
}

static int
TimeOfDelivery_set_dispatch_factor3(TimeOfDeliveryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_TimeOfDelivery_dispatch_factor3_nset, self->data_ptr);
}

static PyObject *
TimeOfDelivery_get_dispatch_factor4(TimeOfDeliveryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_TimeOfDelivery_dispatch_factor4_nget, self->data_ptr);
}

static int
TimeOfDelivery_set_dispatch_factor4(TimeOfDeliveryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_TimeOfDelivery_dispatch_factor4_nset, self->data_ptr);
}

static PyObject *
TimeOfDelivery_get_dispatch_factor5(TimeOfDeliveryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_TimeOfDelivery_dispatch_factor5_nget, self->data_ptr);
}

static int
TimeOfDelivery_set_dispatch_factor5(TimeOfDeliveryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_TimeOfDelivery_dispatch_factor5_nset, self->data_ptr);
}

static PyObject *
TimeOfDelivery_get_dispatch_factor6(TimeOfDeliveryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_TimeOfDelivery_dispatch_factor6_nget, self->data_ptr);
}

static int
TimeOfDelivery_set_dispatch_factor6(TimeOfDeliveryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_TimeOfDelivery_dispatch_factor6_nset, self->data_ptr);
}

static PyObject *
TimeOfDelivery_get_dispatch_factor7(TimeOfDeliveryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_TimeOfDelivery_dispatch_factor7_nget, self->data_ptr);
}

static int
TimeOfDelivery_set_dispatch_factor7(TimeOfDeliveryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_TimeOfDelivery_dispatch_factor7_nset, self->data_ptr);
}

static PyObject *
TimeOfDelivery_get_dispatch_factor8(TimeOfDeliveryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_TimeOfDelivery_dispatch_factor8_nget, self->data_ptr);
}

static int
TimeOfDelivery_set_dispatch_factor8(TimeOfDeliveryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_TimeOfDelivery_dispatch_factor8_nset, self->data_ptr);
}

static PyObject *
TimeOfDelivery_get_dispatch_factor9(TimeOfDeliveryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_TimeOfDelivery_dispatch_factor9_nget, self->data_ptr);
}

static int
TimeOfDelivery_set_dispatch_factor9(TimeOfDeliveryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_TimeOfDelivery_dispatch_factor9_nset, self->data_ptr);
}

static PyObject *
TimeOfDelivery_get_dispatch_factors_ts(TimeOfDeliveryObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_TimeOfDelivery_dispatch_factors_ts_aget, self->data_ptr);
}

static int
TimeOfDelivery_set_dispatch_factors_ts(TimeOfDeliveryObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Levpartflip_TimeOfDelivery_dispatch_factors_ts_aset, self->data_ptr);
}

static PyObject *
TimeOfDelivery_get_dispatch_sched_weekday(TimeOfDeliveryObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Levpartflip_TimeOfDelivery_dispatch_sched_weekday_mget, self->data_ptr);
}

static int
TimeOfDelivery_set_dispatch_sched_weekday(TimeOfDeliveryObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_Levpartflip_TimeOfDelivery_dispatch_sched_weekday_mset, self->data_ptr);
}

static PyObject *
TimeOfDelivery_get_dispatch_sched_weekend(TimeOfDeliveryObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Levpartflip_TimeOfDelivery_dispatch_sched_weekend_mget, self->data_ptr);
}

static int
TimeOfDelivery_set_dispatch_sched_weekend(TimeOfDeliveryObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_Levpartflip_TimeOfDelivery_dispatch_sched_weekend_mset, self->data_ptr);
}

static PyObject *
TimeOfDelivery_get_ppa_multiplier_model(TimeOfDeliveryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_TimeOfDelivery_ppa_multiplier_model_nget, self->data_ptr);
}

static int
TimeOfDelivery_set_ppa_multiplier_model(TimeOfDeliveryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_TimeOfDelivery_ppa_multiplier_model_nset, self->data_ptr);
}

static PyObject *
TimeOfDelivery_get_system_use_lifetime_output(TimeOfDeliveryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_TimeOfDelivery_system_use_lifetime_output_nget, self->data_ptr);
}

static int
TimeOfDelivery_set_system_use_lifetime_output(TimeOfDeliveryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_TimeOfDelivery_system_use_lifetime_output_nset, self->data_ptr);
}

static PyGetSetDef TimeOfDelivery_getset[] = {
{"dispatch_factor1", (getter)TimeOfDelivery_get_dispatch_factor1,(setter)TimeOfDelivery_set_dispatch_factor1,
	PyDoc_STR("*float*: TOD factor for period 1\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"dispatch_factor2", (getter)TimeOfDelivery_get_dispatch_factor2,(setter)TimeOfDelivery_set_dispatch_factor2,
	PyDoc_STR("*float*: TOD factor for period 2\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"dispatch_factor3", (getter)TimeOfDelivery_get_dispatch_factor3,(setter)TimeOfDelivery_set_dispatch_factor3,
	PyDoc_STR("*float*: TOD factor for period 3\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"dispatch_factor4", (getter)TimeOfDelivery_get_dispatch_factor4,(setter)TimeOfDelivery_set_dispatch_factor4,
	PyDoc_STR("*float*: TOD factor for period 4\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"dispatch_factor5", (getter)TimeOfDelivery_get_dispatch_factor5,(setter)TimeOfDelivery_set_dispatch_factor5,
	PyDoc_STR("*float*: TOD factor for period 5\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"dispatch_factor6", (getter)TimeOfDelivery_get_dispatch_factor6,(setter)TimeOfDelivery_set_dispatch_factor6,
	PyDoc_STR("*float*: TOD factor for period 6\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"dispatch_factor7", (getter)TimeOfDelivery_get_dispatch_factor7,(setter)TimeOfDelivery_set_dispatch_factor7,
	PyDoc_STR("*float*: TOD factor for period 7\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"dispatch_factor8", (getter)TimeOfDelivery_get_dispatch_factor8,(setter)TimeOfDelivery_set_dispatch_factor8,
	PyDoc_STR("*float*: TOD factor for period 8\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"dispatch_factor9", (getter)TimeOfDelivery_get_dispatch_factor9,(setter)TimeOfDelivery_set_dispatch_factor9,
	PyDoc_STR("*float*: TOD factor for period 9\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"dispatch_factors_ts", (getter)TimeOfDelivery_get_dispatch_factors_ts,(setter)TimeOfDelivery_set_dispatch_factors_ts,
	PyDoc_STR("*sequence*: Dispatch payment factor array\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"dispatch_sched_weekday", (getter)TimeOfDelivery_get_dispatch_sched_weekday,(setter)TimeOfDelivery_set_dispatch_sched_weekday,
	PyDoc_STR("*sequence[sequence]*: Diurnal weekday TOD periods [1..9]\n\n*Info*: 12 x 24 matrix\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"dispatch_sched_weekend", (getter)TimeOfDelivery_get_dispatch_sched_weekend,(setter)TimeOfDelivery_set_dispatch_sched_weekend,
	PyDoc_STR("*sequence[sequence]*: Diurnal weekend TOD periods [1..9]\n\n*Info*: 12 x 24 matrix\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"ppa_multiplier_model", (getter)TimeOfDelivery_get_ppa_multiplier_model,(setter)TimeOfDelivery_set_ppa_multiplier_model,
	PyDoc_STR("*float*: PPA multiplier model [0/1]\n\n*Options*: 0=diurnal,1=timestep\n\n*Constraints*: INTEGER,MIN=0\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"system_use_lifetime_output", (getter)TimeOfDelivery_get_system_use_lifetime_output,(setter)TimeOfDelivery_set_system_use_lifetime_output,
	PyDoc_STR("*float*: Lifetime hourly system outputs [0/1]\n\n*Options*: 0=hourly first year,1=hourly lifetime\n\n*Constraints*: INTEGER,MIN=0\n\n*Required*: True"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject TimeOfDelivery_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Levpartflip.TimeOfDelivery",             /*tp_name*/
		sizeof(TimeOfDeliveryObject),          /*tp_basicsize*/
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
		TimeOfDelivery_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		TimeOfDelivery_getset,          /*tp_getset*/
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
	 * ReserveAccounts Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Levpartflip   data_ptr;
} ReserveAccountsObject;

static PyTypeObject ReserveAccounts_Type;

static PyObject *
ReserveAccounts_new(SAM_Levpartflip data_ptr)
{
	PyObject* new_obj = ReserveAccounts_Type.tp_alloc(&ReserveAccounts_Type,0);

	ReserveAccountsObject* ReserveAccounts_obj = (ReserveAccountsObject*)new_obj;

	ReserveAccounts_obj->data_ptr = data_ptr;

	return new_obj;
}

/* ReserveAccounts methods */

static PyObject *
ReserveAccounts_assign(ReserveAccountsObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Levpartflip", "ReserveAccounts")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
ReserveAccounts_export(ReserveAccountsObject *self, PyObject *args)
{
	PyTypeObject* tp = &ReserveAccounts_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef ReserveAccounts_methods[] = {
		{"assign",            (PyCFunction)ReserveAccounts_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``ReserveAccounts_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)ReserveAccounts_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
ReserveAccounts_get_equip1_reserve_cost(ReserveAccountsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_ReserveAccounts_equip1_reserve_cost_nget, self->data_ptr);
}

static int
ReserveAccounts_set_equip1_reserve_cost(ReserveAccountsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_ReserveAccounts_equip1_reserve_cost_nset, self->data_ptr);
}

static PyObject *
ReserveAccounts_get_equip1_reserve_freq(ReserveAccountsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_ReserveAccounts_equip1_reserve_freq_nget, self->data_ptr);
}

static int
ReserveAccounts_set_equip1_reserve_freq(ReserveAccountsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_ReserveAccounts_equip1_reserve_freq_nset, self->data_ptr);
}

static PyObject *
ReserveAccounts_get_equip2_reserve_cost(ReserveAccountsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_ReserveAccounts_equip2_reserve_cost_nget, self->data_ptr);
}

static int
ReserveAccounts_set_equip2_reserve_cost(ReserveAccountsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_ReserveAccounts_equip2_reserve_cost_nset, self->data_ptr);
}

static PyObject *
ReserveAccounts_get_equip2_reserve_freq(ReserveAccountsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_ReserveAccounts_equip2_reserve_freq_nget, self->data_ptr);
}

static int
ReserveAccounts_set_equip2_reserve_freq(ReserveAccountsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_ReserveAccounts_equip2_reserve_freq_nset, self->data_ptr);
}

static PyObject *
ReserveAccounts_get_equip3_reserve_cost(ReserveAccountsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_ReserveAccounts_equip3_reserve_cost_nget, self->data_ptr);
}

static int
ReserveAccounts_set_equip3_reserve_cost(ReserveAccountsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_ReserveAccounts_equip3_reserve_cost_nset, self->data_ptr);
}

static PyObject *
ReserveAccounts_get_equip3_reserve_freq(ReserveAccountsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_ReserveAccounts_equip3_reserve_freq_nget, self->data_ptr);
}

static int
ReserveAccounts_set_equip3_reserve_freq(ReserveAccountsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_ReserveAccounts_equip3_reserve_freq_nset, self->data_ptr);
}

static PyObject *
ReserveAccounts_get_reserves_interest(ReserveAccountsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_ReserveAccounts_reserves_interest_nget, self->data_ptr);
}

static int
ReserveAccounts_set_reserves_interest(ReserveAccountsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_ReserveAccounts_reserves_interest_nset, self->data_ptr);
}

static PyGetSetDef ReserveAccounts_getset[] = {
{"equip1_reserve_cost", (getter)ReserveAccounts_get_equip1_reserve_cost,(setter)ReserveAccounts_set_equip1_reserve_cost,
	PyDoc_STR("*float*: Major equipment reserve 1 cost [$/W]\n\n*Constraints*: MIN=0\n\n*Required*: set to 0.25 if not provided."),
 	NULL},
{"equip1_reserve_freq", (getter)ReserveAccounts_get_equip1_reserve_freq,(setter)ReserveAccounts_set_equip1_reserve_freq,
	PyDoc_STR("*float*: Major equipment reserve 1 frequency [years]\n\n*Constraints*: INTEGER,MIN=0\n\n*Required*: set to 12 if not provided."),
 	NULL},
{"equip2_reserve_cost", (getter)ReserveAccounts_get_equip2_reserve_cost,(setter)ReserveAccounts_set_equip2_reserve_cost,
	PyDoc_STR("*float*: Major equipment reserve 2 cost [$/W]\n\n*Constraints*: MIN=0\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"equip2_reserve_freq", (getter)ReserveAccounts_get_equip2_reserve_freq,(setter)ReserveAccounts_set_equip2_reserve_freq,
	PyDoc_STR("*float*: Major equipment reserve 2 frequency [years]\n\n*Constraints*: INTEGER,MIN=0\n\n*Required*: set to 15 if not provided."),
 	NULL},
{"equip3_reserve_cost", (getter)ReserveAccounts_get_equip3_reserve_cost,(setter)ReserveAccounts_set_equip3_reserve_cost,
	PyDoc_STR("*float*: Major equipment reserve 3 cost [$/W]\n\n*Constraints*: MIN=0\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"equip3_reserve_freq", (getter)ReserveAccounts_get_equip3_reserve_freq,(setter)ReserveAccounts_set_equip3_reserve_freq,
	PyDoc_STR("*float*: Major equipment reserve 3 frequency [years]\n\n*Constraints*: INTEGER,MIN=0\n\n*Required*: set to 20 if not provided."),
 	NULL},
{"reserves_interest", (getter)ReserveAccounts_get_reserves_interest,(setter)ReserveAccounts_set_reserves_interest,
	PyDoc_STR("*float*: Interest on reserves [%]\n\n*Constraints*: MIN=0,MAX=100\n\n*Required*: set to 1.75 if not provided."),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject ReserveAccounts_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Levpartflip.ReserveAccounts",             /*tp_name*/
		sizeof(ReserveAccountsObject),          /*tp_basicsize*/
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
		ReserveAccounts_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		ReserveAccounts_getset,          /*tp_getset*/
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
	 * Depreciation Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Levpartflip   data_ptr;
} DepreciationObject;

static PyTypeObject Depreciation_Type;

static PyObject *
Depreciation_new(SAM_Levpartflip data_ptr)
{
	PyObject* new_obj = Depreciation_Type.tp_alloc(&Depreciation_Type,0);

	DepreciationObject* Depreciation_obj = (DepreciationObject*)new_obj;

	Depreciation_obj->data_ptr = data_ptr;

	return new_obj;
}

/* Depreciation methods */

static PyObject *
Depreciation_assign(DepreciationObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Levpartflip", "Depreciation")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
Depreciation_export(DepreciationObject *self, PyObject *args)
{
	PyTypeObject* tp = &Depreciation_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef Depreciation_methods[] = {
		{"assign",            (PyCFunction)Depreciation_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``Depreciation_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)Depreciation_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
Depreciation_get_depr_alloc_custom_percent(DepreciationObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Depreciation_depr_alloc_custom_percent_nget, self->data_ptr);
}

static int
Depreciation_set_depr_alloc_custom_percent(DepreciationObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_Depreciation_depr_alloc_custom_percent_nset, self->data_ptr);
}

static PyObject *
Depreciation_get_depr_alloc_macrs_15_percent(DepreciationObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Depreciation_depr_alloc_macrs_15_percent_nget, self->data_ptr);
}

static int
Depreciation_set_depr_alloc_macrs_15_percent(DepreciationObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_Depreciation_depr_alloc_macrs_15_percent_nset, self->data_ptr);
}

static PyObject *
Depreciation_get_depr_alloc_macrs_5_percent(DepreciationObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Depreciation_depr_alloc_macrs_5_percent_nget, self->data_ptr);
}

static int
Depreciation_set_depr_alloc_macrs_5_percent(DepreciationObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_Depreciation_depr_alloc_macrs_5_percent_nset, self->data_ptr);
}

static PyObject *
Depreciation_get_depr_alloc_sl_15_percent(DepreciationObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Depreciation_depr_alloc_sl_15_percent_nget, self->data_ptr);
}

static int
Depreciation_set_depr_alloc_sl_15_percent(DepreciationObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_Depreciation_depr_alloc_sl_15_percent_nset, self->data_ptr);
}

static PyObject *
Depreciation_get_depr_alloc_sl_20_percent(DepreciationObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Depreciation_depr_alloc_sl_20_percent_nget, self->data_ptr);
}

static int
Depreciation_set_depr_alloc_sl_20_percent(DepreciationObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_Depreciation_depr_alloc_sl_20_percent_nset, self->data_ptr);
}

static PyObject *
Depreciation_get_depr_alloc_sl_39_percent(DepreciationObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Depreciation_depr_alloc_sl_39_percent_nget, self->data_ptr);
}

static int
Depreciation_set_depr_alloc_sl_39_percent(DepreciationObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_Depreciation_depr_alloc_sl_39_percent_nset, self->data_ptr);
}

static PyObject *
Depreciation_get_depr_alloc_sl_5_percent(DepreciationObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Depreciation_depr_alloc_sl_5_percent_nget, self->data_ptr);
}

static int
Depreciation_set_depr_alloc_sl_5_percent(DepreciationObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_Depreciation_depr_alloc_sl_5_percent_nset, self->data_ptr);
}

static PyObject *
Depreciation_get_depr_bonus_fed(DepreciationObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Depreciation_depr_bonus_fed_nget, self->data_ptr);
}

static int
Depreciation_set_depr_bonus_fed(DepreciationObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_Depreciation_depr_bonus_fed_nset, self->data_ptr);
}

static PyObject *
Depreciation_get_depr_bonus_fed_custom(DepreciationObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Depreciation_depr_bonus_fed_custom_nget, self->data_ptr);
}

static int
Depreciation_set_depr_bonus_fed_custom(DepreciationObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_Depreciation_depr_bonus_fed_custom_nset, self->data_ptr);
}

static PyObject *
Depreciation_get_depr_bonus_fed_macrs_15(DepreciationObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Depreciation_depr_bonus_fed_macrs_15_nget, self->data_ptr);
}

static int
Depreciation_set_depr_bonus_fed_macrs_15(DepreciationObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_Depreciation_depr_bonus_fed_macrs_15_nset, self->data_ptr);
}

static PyObject *
Depreciation_get_depr_bonus_fed_macrs_5(DepreciationObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Depreciation_depr_bonus_fed_macrs_5_nget, self->data_ptr);
}

static int
Depreciation_set_depr_bonus_fed_macrs_5(DepreciationObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_Depreciation_depr_bonus_fed_macrs_5_nset, self->data_ptr);
}

static PyObject *
Depreciation_get_depr_bonus_fed_sl_15(DepreciationObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Depreciation_depr_bonus_fed_sl_15_nget, self->data_ptr);
}

static int
Depreciation_set_depr_bonus_fed_sl_15(DepreciationObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_Depreciation_depr_bonus_fed_sl_15_nset, self->data_ptr);
}

static PyObject *
Depreciation_get_depr_bonus_fed_sl_20(DepreciationObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Depreciation_depr_bonus_fed_sl_20_nget, self->data_ptr);
}

static int
Depreciation_set_depr_bonus_fed_sl_20(DepreciationObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_Depreciation_depr_bonus_fed_sl_20_nset, self->data_ptr);
}

static PyObject *
Depreciation_get_depr_bonus_fed_sl_39(DepreciationObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Depreciation_depr_bonus_fed_sl_39_nget, self->data_ptr);
}

static int
Depreciation_set_depr_bonus_fed_sl_39(DepreciationObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_Depreciation_depr_bonus_fed_sl_39_nset, self->data_ptr);
}

static PyObject *
Depreciation_get_depr_bonus_fed_sl_5(DepreciationObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Depreciation_depr_bonus_fed_sl_5_nget, self->data_ptr);
}

static int
Depreciation_set_depr_bonus_fed_sl_5(DepreciationObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_Depreciation_depr_bonus_fed_sl_5_nset, self->data_ptr);
}

static PyObject *
Depreciation_get_depr_bonus_sta(DepreciationObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Depreciation_depr_bonus_sta_nget, self->data_ptr);
}

static int
Depreciation_set_depr_bonus_sta(DepreciationObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_Depreciation_depr_bonus_sta_nset, self->data_ptr);
}

static PyObject *
Depreciation_get_depr_bonus_sta_custom(DepreciationObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Depreciation_depr_bonus_sta_custom_nget, self->data_ptr);
}

static int
Depreciation_set_depr_bonus_sta_custom(DepreciationObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_Depreciation_depr_bonus_sta_custom_nset, self->data_ptr);
}

static PyObject *
Depreciation_get_depr_bonus_sta_macrs_15(DepreciationObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Depreciation_depr_bonus_sta_macrs_15_nget, self->data_ptr);
}

static int
Depreciation_set_depr_bonus_sta_macrs_15(DepreciationObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_Depreciation_depr_bonus_sta_macrs_15_nset, self->data_ptr);
}

static PyObject *
Depreciation_get_depr_bonus_sta_macrs_5(DepreciationObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Depreciation_depr_bonus_sta_macrs_5_nget, self->data_ptr);
}

static int
Depreciation_set_depr_bonus_sta_macrs_5(DepreciationObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_Depreciation_depr_bonus_sta_macrs_5_nset, self->data_ptr);
}

static PyObject *
Depreciation_get_depr_bonus_sta_sl_15(DepreciationObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Depreciation_depr_bonus_sta_sl_15_nget, self->data_ptr);
}

static int
Depreciation_set_depr_bonus_sta_sl_15(DepreciationObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_Depreciation_depr_bonus_sta_sl_15_nset, self->data_ptr);
}

static PyObject *
Depreciation_get_depr_bonus_sta_sl_20(DepreciationObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Depreciation_depr_bonus_sta_sl_20_nget, self->data_ptr);
}

static int
Depreciation_set_depr_bonus_sta_sl_20(DepreciationObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_Depreciation_depr_bonus_sta_sl_20_nset, self->data_ptr);
}

static PyObject *
Depreciation_get_depr_bonus_sta_sl_39(DepreciationObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Depreciation_depr_bonus_sta_sl_39_nget, self->data_ptr);
}

static int
Depreciation_set_depr_bonus_sta_sl_39(DepreciationObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_Depreciation_depr_bonus_sta_sl_39_nset, self->data_ptr);
}

static PyObject *
Depreciation_get_depr_bonus_sta_sl_5(DepreciationObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Depreciation_depr_bonus_sta_sl_5_nget, self->data_ptr);
}

static int
Depreciation_set_depr_bonus_sta_sl_5(DepreciationObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_Depreciation_depr_bonus_sta_sl_5_nset, self->data_ptr);
}

static PyObject *
Depreciation_get_depr_custom_schedule(DepreciationObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Depreciation_depr_custom_schedule_aget, self->data_ptr);
}

static int
Depreciation_set_depr_custom_schedule(DepreciationObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Levpartflip_Depreciation_depr_custom_schedule_aset, self->data_ptr);
}

static PyObject *
Depreciation_get_depr_fedbas_method(DepreciationObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Depreciation_depr_fedbas_method_nget, self->data_ptr);
}

static int
Depreciation_set_depr_fedbas_method(DepreciationObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_Depreciation_depr_fedbas_method_nset, self->data_ptr);
}

static PyObject *
Depreciation_get_depr_itc_fed_custom(DepreciationObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Depreciation_depr_itc_fed_custom_nget, self->data_ptr);
}

static int
Depreciation_set_depr_itc_fed_custom(DepreciationObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_Depreciation_depr_itc_fed_custom_nset, self->data_ptr);
}

static PyObject *
Depreciation_get_depr_itc_fed_macrs_15(DepreciationObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Depreciation_depr_itc_fed_macrs_15_nget, self->data_ptr);
}

static int
Depreciation_set_depr_itc_fed_macrs_15(DepreciationObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_Depreciation_depr_itc_fed_macrs_15_nset, self->data_ptr);
}

static PyObject *
Depreciation_get_depr_itc_fed_macrs_5(DepreciationObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Depreciation_depr_itc_fed_macrs_5_nget, self->data_ptr);
}

static int
Depreciation_set_depr_itc_fed_macrs_5(DepreciationObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_Depreciation_depr_itc_fed_macrs_5_nset, self->data_ptr);
}

static PyObject *
Depreciation_get_depr_itc_fed_sl_15(DepreciationObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Depreciation_depr_itc_fed_sl_15_nget, self->data_ptr);
}

static int
Depreciation_set_depr_itc_fed_sl_15(DepreciationObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_Depreciation_depr_itc_fed_sl_15_nset, self->data_ptr);
}

static PyObject *
Depreciation_get_depr_itc_fed_sl_20(DepreciationObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Depreciation_depr_itc_fed_sl_20_nget, self->data_ptr);
}

static int
Depreciation_set_depr_itc_fed_sl_20(DepreciationObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_Depreciation_depr_itc_fed_sl_20_nset, self->data_ptr);
}

static PyObject *
Depreciation_get_depr_itc_fed_sl_39(DepreciationObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Depreciation_depr_itc_fed_sl_39_nget, self->data_ptr);
}

static int
Depreciation_set_depr_itc_fed_sl_39(DepreciationObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_Depreciation_depr_itc_fed_sl_39_nset, self->data_ptr);
}

static PyObject *
Depreciation_get_depr_itc_fed_sl_5(DepreciationObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Depreciation_depr_itc_fed_sl_5_nget, self->data_ptr);
}

static int
Depreciation_set_depr_itc_fed_sl_5(DepreciationObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_Depreciation_depr_itc_fed_sl_5_nset, self->data_ptr);
}

static PyObject *
Depreciation_get_depr_itc_sta_custom(DepreciationObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Depreciation_depr_itc_sta_custom_nget, self->data_ptr);
}

static int
Depreciation_set_depr_itc_sta_custom(DepreciationObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_Depreciation_depr_itc_sta_custom_nset, self->data_ptr);
}

static PyObject *
Depreciation_get_depr_itc_sta_macrs_15(DepreciationObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Depreciation_depr_itc_sta_macrs_15_nget, self->data_ptr);
}

static int
Depreciation_set_depr_itc_sta_macrs_15(DepreciationObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_Depreciation_depr_itc_sta_macrs_15_nset, self->data_ptr);
}

static PyObject *
Depreciation_get_depr_itc_sta_macrs_5(DepreciationObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Depreciation_depr_itc_sta_macrs_5_nget, self->data_ptr);
}

static int
Depreciation_set_depr_itc_sta_macrs_5(DepreciationObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_Depreciation_depr_itc_sta_macrs_5_nset, self->data_ptr);
}

static PyObject *
Depreciation_get_depr_itc_sta_sl_15(DepreciationObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Depreciation_depr_itc_sta_sl_15_nget, self->data_ptr);
}

static int
Depreciation_set_depr_itc_sta_sl_15(DepreciationObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_Depreciation_depr_itc_sta_sl_15_nset, self->data_ptr);
}

static PyObject *
Depreciation_get_depr_itc_sta_sl_20(DepreciationObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Depreciation_depr_itc_sta_sl_20_nget, self->data_ptr);
}

static int
Depreciation_set_depr_itc_sta_sl_20(DepreciationObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_Depreciation_depr_itc_sta_sl_20_nset, self->data_ptr);
}

static PyObject *
Depreciation_get_depr_itc_sta_sl_39(DepreciationObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Depreciation_depr_itc_sta_sl_39_nget, self->data_ptr);
}

static int
Depreciation_set_depr_itc_sta_sl_39(DepreciationObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_Depreciation_depr_itc_sta_sl_39_nset, self->data_ptr);
}

static PyObject *
Depreciation_get_depr_itc_sta_sl_5(DepreciationObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Depreciation_depr_itc_sta_sl_5_nget, self->data_ptr);
}

static int
Depreciation_set_depr_itc_sta_sl_5(DepreciationObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_Depreciation_depr_itc_sta_sl_5_nset, self->data_ptr);
}

static PyObject *
Depreciation_get_depr_stabas_method(DepreciationObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Depreciation_depr_stabas_method_nget, self->data_ptr);
}

static int
Depreciation_set_depr_stabas_method(DepreciationObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_Depreciation_depr_stabas_method_nset, self->data_ptr);
}

static PyObject *
Depreciation_get_equip_reserve_depr_fed(DepreciationObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Depreciation_equip_reserve_depr_fed_nget, self->data_ptr);
}

static int
Depreciation_set_equip_reserve_depr_fed(DepreciationObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_Depreciation_equip_reserve_depr_fed_nset, self->data_ptr);
}

static PyObject *
Depreciation_get_equip_reserve_depr_sta(DepreciationObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Depreciation_equip_reserve_depr_sta_nget, self->data_ptr);
}

static int
Depreciation_set_equip_reserve_depr_sta(DepreciationObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_Depreciation_equip_reserve_depr_sta_nset, self->data_ptr);
}

static PyGetSetDef Depreciation_getset[] = {
{"depr_alloc_custom_percent", (getter)Depreciation_get_depr_alloc_custom_percent,(setter)Depreciation_set_depr_alloc_custom_percent,
	PyDoc_STR("*float*: Custom depreciation federal and state allocation [%]\n\n*Constraints*: MIN=0,MAX=100\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"depr_alloc_macrs_15_percent", (getter)Depreciation_get_depr_alloc_macrs_15_percent,(setter)Depreciation_set_depr_alloc_macrs_15_percent,
	PyDoc_STR("*float*: 15-yr MACRS depreciation federal and state allocation [%]\n\n*Constraints*: MIN=0,MAX=100\n\n*Required*: set to 1.5 if not provided."),
 	NULL},
{"depr_alloc_macrs_5_percent", (getter)Depreciation_get_depr_alloc_macrs_5_percent,(setter)Depreciation_set_depr_alloc_macrs_5_percent,
	PyDoc_STR("*float*: 5-yr MACRS depreciation federal and state allocation [%]\n\n*Constraints*: MIN=0,MAX=100\n\n*Required*: set to 89 if not provided."),
 	NULL},
{"depr_alloc_sl_15_percent", (getter)Depreciation_get_depr_alloc_sl_15_percent,(setter)Depreciation_set_depr_alloc_sl_15_percent,
	PyDoc_STR("*float*: 15-yr straight line depreciation federal and state allocation [%]\n\n*Constraints*: MIN=0,MAX=100\n\n*Required*: set to 3 if not provided."),
 	NULL},
{"depr_alloc_sl_20_percent", (getter)Depreciation_get_depr_alloc_sl_20_percent,(setter)Depreciation_set_depr_alloc_sl_20_percent,
	PyDoc_STR("*float*: 20-yr straight line depreciation federal and state allocation [%]\n\n*Constraints*: MIN=0,MAX=100\n\n*Required*: set to 3 if not provided."),
 	NULL},
{"depr_alloc_sl_39_percent", (getter)Depreciation_get_depr_alloc_sl_39_percent,(setter)Depreciation_set_depr_alloc_sl_39_percent,
	PyDoc_STR("*float*: 39-yr straight line depreciation federal and state allocation [%]\n\n*Constraints*: MIN=0,MAX=100\n\n*Required*: set to 0.5 if not provided."),
 	NULL},
{"depr_alloc_sl_5_percent", (getter)Depreciation_get_depr_alloc_sl_5_percent,(setter)Depreciation_set_depr_alloc_sl_5_percent,
	PyDoc_STR("*float*: 5-yr straight line depreciation federal and state allocation [%]\n\n*Constraints*: MIN=0,MAX=100\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"depr_bonus_fed", (getter)Depreciation_get_depr_bonus_fed,(setter)Depreciation_set_depr_bonus_fed,
	PyDoc_STR("*float*: Federal bonus depreciation [%]\n\n*Constraints*: MIN=0,MAX=100\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"depr_bonus_fed_custom", (getter)Depreciation_get_depr_bonus_fed_custom,(setter)Depreciation_set_depr_bonus_fed_custom,
	PyDoc_STR("*float*: Federal bonus depreciation custom [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"depr_bonus_fed_macrs_15", (getter)Depreciation_get_depr_bonus_fed_macrs_15,(setter)Depreciation_set_depr_bonus_fed_macrs_15,
	PyDoc_STR("*float*: Federal bonus depreciation 15-yr MACRS [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"depr_bonus_fed_macrs_5", (getter)Depreciation_get_depr_bonus_fed_macrs_5,(setter)Depreciation_set_depr_bonus_fed_macrs_5,
	PyDoc_STR("*float*: Federal bonus depreciation 5-yr MACRS [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"depr_bonus_fed_sl_15", (getter)Depreciation_get_depr_bonus_fed_sl_15,(setter)Depreciation_set_depr_bonus_fed_sl_15,
	PyDoc_STR("*float*: Federal bonus depreciation 15-yr straight line [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"depr_bonus_fed_sl_20", (getter)Depreciation_get_depr_bonus_fed_sl_20,(setter)Depreciation_set_depr_bonus_fed_sl_20,
	PyDoc_STR("*float*: Federal bonus depreciation 20-yr straight line [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"depr_bonus_fed_sl_39", (getter)Depreciation_get_depr_bonus_fed_sl_39,(setter)Depreciation_set_depr_bonus_fed_sl_39,
	PyDoc_STR("*float*: Federal bonus depreciation 39-yr straight line [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"depr_bonus_fed_sl_5", (getter)Depreciation_get_depr_bonus_fed_sl_5,(setter)Depreciation_set_depr_bonus_fed_sl_5,
	PyDoc_STR("*float*: Federal bonus depreciation 5-yr straight line [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"depr_bonus_sta", (getter)Depreciation_get_depr_bonus_sta,(setter)Depreciation_set_depr_bonus_sta,
	PyDoc_STR("*float*: State bonus depreciation [%]\n\n*Constraints*: MIN=0,MAX=100\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"depr_bonus_sta_custom", (getter)Depreciation_get_depr_bonus_sta_custom,(setter)Depreciation_set_depr_bonus_sta_custom,
	PyDoc_STR("*float*: State bonus depreciation custom [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"depr_bonus_sta_macrs_15", (getter)Depreciation_get_depr_bonus_sta_macrs_15,(setter)Depreciation_set_depr_bonus_sta_macrs_15,
	PyDoc_STR("*float*: State bonus depreciation 15-yr MACRS [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"depr_bonus_sta_macrs_5", (getter)Depreciation_get_depr_bonus_sta_macrs_5,(setter)Depreciation_set_depr_bonus_sta_macrs_5,
	PyDoc_STR("*float*: State bonus depreciation 5-yr MACRS [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"depr_bonus_sta_sl_15", (getter)Depreciation_get_depr_bonus_sta_sl_15,(setter)Depreciation_set_depr_bonus_sta_sl_15,
	PyDoc_STR("*float*: State bonus depreciation 15-yr straight line [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"depr_bonus_sta_sl_20", (getter)Depreciation_get_depr_bonus_sta_sl_20,(setter)Depreciation_set_depr_bonus_sta_sl_20,
	PyDoc_STR("*float*: State bonus depreciation 20-yr straight line [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"depr_bonus_sta_sl_39", (getter)Depreciation_get_depr_bonus_sta_sl_39,(setter)Depreciation_set_depr_bonus_sta_sl_39,
	PyDoc_STR("*float*: State bonus depreciation 39-yr straight line [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"depr_bonus_sta_sl_5", (getter)Depreciation_get_depr_bonus_sta_sl_5,(setter)Depreciation_set_depr_bonus_sta_sl_5,
	PyDoc_STR("*float*: State bonus depreciation 5-yr straight line [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"depr_custom_schedule", (getter)Depreciation_get_depr_custom_schedule,(setter)Depreciation_set_depr_custom_schedule,
	PyDoc_STR("*sequence*: Custom depreciation schedule [%]\n\n*Required*: True"),
 	NULL},
{"depr_fedbas_method", (getter)Depreciation_get_depr_fedbas_method,(setter)Depreciation_set_depr_fedbas_method,
	PyDoc_STR("*float*: Method of federal depreciation reduction\n\n*Options*: 0=5yr MACRS,1=Proportional\n\n*Constraints*: INTEGER,MIN=0,MAX=1\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"depr_itc_fed_custom", (getter)Depreciation_get_depr_itc_fed_custom,(setter)Depreciation_set_depr_itc_fed_custom,
	PyDoc_STR("*float*: Federal itc depreciation custom [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"depr_itc_fed_macrs_15", (getter)Depreciation_get_depr_itc_fed_macrs_15,(setter)Depreciation_set_depr_itc_fed_macrs_15,
	PyDoc_STR("*float*: Federal itc depreciation 15-yr MACRS [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"depr_itc_fed_macrs_5", (getter)Depreciation_get_depr_itc_fed_macrs_5,(setter)Depreciation_set_depr_itc_fed_macrs_5,
	PyDoc_STR("*float*: Federal itc depreciation 5-yr MACRS [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"depr_itc_fed_sl_15", (getter)Depreciation_get_depr_itc_fed_sl_15,(setter)Depreciation_set_depr_itc_fed_sl_15,
	PyDoc_STR("*float*: Federal itc depreciation 15-yr straight line [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"depr_itc_fed_sl_20", (getter)Depreciation_get_depr_itc_fed_sl_20,(setter)Depreciation_set_depr_itc_fed_sl_20,
	PyDoc_STR("*float*: Federal itc depreciation 20-yr straight line [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"depr_itc_fed_sl_39", (getter)Depreciation_get_depr_itc_fed_sl_39,(setter)Depreciation_set_depr_itc_fed_sl_39,
	PyDoc_STR("*float*: Federal itc depreciation 39-yr straight line [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"depr_itc_fed_sl_5", (getter)Depreciation_get_depr_itc_fed_sl_5,(setter)Depreciation_set_depr_itc_fed_sl_5,
	PyDoc_STR("*float*: Federal itc depreciation 5-yr straight line [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"depr_itc_sta_custom", (getter)Depreciation_get_depr_itc_sta_custom,(setter)Depreciation_set_depr_itc_sta_custom,
	PyDoc_STR("*float*: State itc depreciation custom [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"depr_itc_sta_macrs_15", (getter)Depreciation_get_depr_itc_sta_macrs_15,(setter)Depreciation_set_depr_itc_sta_macrs_15,
	PyDoc_STR("*float*: State itc depreciation 15-yr MACRS [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"depr_itc_sta_macrs_5", (getter)Depreciation_get_depr_itc_sta_macrs_5,(setter)Depreciation_set_depr_itc_sta_macrs_5,
	PyDoc_STR("*float*: State itc depreciation 5-yr MACRS [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"depr_itc_sta_sl_15", (getter)Depreciation_get_depr_itc_sta_sl_15,(setter)Depreciation_set_depr_itc_sta_sl_15,
	PyDoc_STR("*float*: State itc depreciation 15-yr straight line [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"depr_itc_sta_sl_20", (getter)Depreciation_get_depr_itc_sta_sl_20,(setter)Depreciation_set_depr_itc_sta_sl_20,
	PyDoc_STR("*float*: State itc depreciation 20-yr straight line [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"depr_itc_sta_sl_39", (getter)Depreciation_get_depr_itc_sta_sl_39,(setter)Depreciation_set_depr_itc_sta_sl_39,
	PyDoc_STR("*float*: State itc depreciation 39-yr straight line [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"depr_itc_sta_sl_5", (getter)Depreciation_get_depr_itc_sta_sl_5,(setter)Depreciation_set_depr_itc_sta_sl_5,
	PyDoc_STR("*float*: State itc depreciation 5-yr straight line [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"depr_stabas_method", (getter)Depreciation_get_depr_stabas_method,(setter)Depreciation_set_depr_stabas_method,
	PyDoc_STR("*float*: Method of state depreciation reduction\n\n*Options*: 0=5yr MACRS,1=Proportional\n\n*Constraints*: INTEGER,MIN=0,MAX=1\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"equip_reserve_depr_fed", (getter)Depreciation_get_equip_reserve_depr_fed,(setter)Depreciation_set_equip_reserve_depr_fed,
	PyDoc_STR("*float*: Major equipment reserve federal depreciation\n\n*Options*: 0=5yr MACRS,1=15yr MACRS,2=5yr SL,3=15yr SL, 4=20yr SL,5=39yr SL,6=Custom\n\n*Constraints*: INTEGER,MIN=0,MAX=6\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"equip_reserve_depr_sta", (getter)Depreciation_get_equip_reserve_depr_sta,(setter)Depreciation_set_equip_reserve_depr_sta,
	PyDoc_STR("*float*: Major equipment reserve state depreciation\n\n*Options*: 0=5yr MACRS,1=15yr MACRS,2=5yr SL,3=15yr SL, 4=20yr SL,5=39yr SL,6=Custom\n\n*Constraints*: INTEGER,MIN=0,MAX=6\n\n*Required*: set to 0 if not provided."),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject Depreciation_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Levpartflip.Depreciation",             /*tp_name*/
		sizeof(DepreciationObject),          /*tp_basicsize*/
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
		Depreciation_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		Depreciation_getset,          /*tp_getset*/
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
	 * SalvageValue Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Levpartflip   data_ptr;
} SalvageValueObject;

static PyTypeObject SalvageValue_Type;

static PyObject *
SalvageValue_new(SAM_Levpartflip data_ptr)
{
	PyObject* new_obj = SalvageValue_Type.tp_alloc(&SalvageValue_Type,0);

	SalvageValueObject* SalvageValue_obj = (SalvageValueObject*)new_obj;

	SalvageValue_obj->data_ptr = data_ptr;

	return new_obj;
}

/* SalvageValue methods */

static PyObject *
SalvageValue_assign(SalvageValueObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Levpartflip", "SalvageValue")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
SalvageValue_export(SalvageValueObject *self, PyObject *args)
{
	PyTypeObject* tp = &SalvageValue_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef SalvageValue_methods[] = {
		{"assign",            (PyCFunction)SalvageValue_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``SalvageValue_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)SalvageValue_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
SalvageValue_get_salvage_percentage(SalvageValueObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_SalvageValue_salvage_percentage_nget, self->data_ptr);
}

static int
SalvageValue_set_salvage_percentage(SalvageValueObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_SalvageValue_salvage_percentage_nset, self->data_ptr);
}

static PyGetSetDef SalvageValue_getset[] = {
{"salvage_percentage", (getter)SalvageValue_get_salvage_percentage,(setter)SalvageValue_set_salvage_percentage,
	PyDoc_STR("*float*: Net pre-tax cash salvage value [%]\n\n*Constraints*: MIN=0,MAX=100\n\n*Required*: set to 10 if not provided."),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject SalvageValue_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Levpartflip.SalvageValue",             /*tp_name*/
		sizeof(SalvageValueObject),          /*tp_basicsize*/
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
		SalvageValue_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		SalvageValue_getset,          /*tp_getset*/
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
	 * SolutionMode Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Levpartflip   data_ptr;
} SolutionModeObject;

static PyTypeObject SolutionMode_Type;

static PyObject *
SolutionMode_new(SAM_Levpartflip data_ptr)
{
	PyObject* new_obj = SolutionMode_Type.tp_alloc(&SolutionMode_Type,0);

	SolutionModeObject* SolutionMode_obj = (SolutionModeObject*)new_obj;

	SolutionMode_obj->data_ptr = data_ptr;

	return new_obj;
}

/* SolutionMode methods */

static PyObject *
SolutionMode_assign(SolutionModeObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Levpartflip", "SolutionMode")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
SolutionMode_export(SolutionModeObject *self, PyObject *args)
{
	PyTypeObject* tp = &SolutionMode_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef SolutionMode_methods[] = {
		{"assign",            (PyCFunction)SolutionMode_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``SolutionMode_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)SolutionMode_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
SolutionMode_get_ppa_escalation(SolutionModeObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_SolutionMode_ppa_escalation_nget, self->data_ptr);
}

static int
SolutionMode_set_ppa_escalation(SolutionModeObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_SolutionMode_ppa_escalation_nset, self->data_ptr);
}

static PyObject *
SolutionMode_get_ppa_price_input(SolutionModeObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_SolutionMode_ppa_price_input_nget, self->data_ptr);
}

static int
SolutionMode_set_ppa_price_input(SolutionModeObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_SolutionMode_ppa_price_input_nset, self->data_ptr);
}

static PyObject *
SolutionMode_get_ppa_soln_max(SolutionModeObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_SolutionMode_ppa_soln_max_nget, self->data_ptr);
}

static int
SolutionMode_set_ppa_soln_max(SolutionModeObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_SolutionMode_ppa_soln_max_nset, self->data_ptr);
}

static PyObject *
SolutionMode_get_ppa_soln_max_iterations(SolutionModeObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_SolutionMode_ppa_soln_max_iterations_nget, self->data_ptr);
}

static int
SolutionMode_set_ppa_soln_max_iterations(SolutionModeObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_SolutionMode_ppa_soln_max_iterations_nset, self->data_ptr);
}

static PyObject *
SolutionMode_get_ppa_soln_min(SolutionModeObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_SolutionMode_ppa_soln_min_nget, self->data_ptr);
}

static int
SolutionMode_set_ppa_soln_min(SolutionModeObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_SolutionMode_ppa_soln_min_nset, self->data_ptr);
}

static PyObject *
SolutionMode_get_ppa_soln_mode(SolutionModeObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_SolutionMode_ppa_soln_mode_nget, self->data_ptr);
}

static int
SolutionMode_set_ppa_soln_mode(SolutionModeObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_SolutionMode_ppa_soln_mode_nset, self->data_ptr);
}

static PyObject *
SolutionMode_get_ppa_soln_tolerance(SolutionModeObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_SolutionMode_ppa_soln_tolerance_nget, self->data_ptr);
}

static int
SolutionMode_set_ppa_soln_tolerance(SolutionModeObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_SolutionMode_ppa_soln_tolerance_nset, self->data_ptr);
}

static PyGetSetDef SolutionMode_getset[] = {
{"ppa_escalation", (getter)SolutionMode_get_ppa_escalation,(setter)SolutionMode_set_ppa_escalation,
	PyDoc_STR("*float*: PPA escalation [%]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"ppa_price_input", (getter)SolutionMode_get_ppa_price_input,(setter)SolutionMode_set_ppa_price_input,
	PyDoc_STR("*float*: Initial year PPA price [$/kWh]\n\n*Required*: set to 10 if not provided."),
 	NULL},
{"ppa_soln_max", (getter)SolutionMode_get_ppa_soln_max,(setter)SolutionMode_set_ppa_soln_max,
	PyDoc_STR("*float*: PPA solution maximum ppa [cents/kWh]\n\n*Required*: set to 100 if not provided."),
 	NULL},
{"ppa_soln_max_iterations", (getter)SolutionMode_get_ppa_soln_max_iterations,(setter)SolutionMode_set_ppa_soln_max_iterations,
	PyDoc_STR("*float*: PPA solution maximum number of iterations\n\n*Constraints*: INTEGER,MIN=1\n\n*Required*: set to 100 if not provided."),
 	NULL},
{"ppa_soln_min", (getter)SolutionMode_get_ppa_soln_min,(setter)SolutionMode_set_ppa_soln_min,
	PyDoc_STR("*float*: PPA solution minimum ppa [cents/kWh]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"ppa_soln_mode", (getter)SolutionMode_get_ppa_soln_mode,(setter)SolutionMode_set_ppa_soln_mode,
	PyDoc_STR("*float*: PPA solution mode [0/1]\n\n*Options*: 0=solve ppa,1=specify ppa\n\n*Constraints*: INTEGER,MIN=0,MAX=1\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"ppa_soln_tolerance", (getter)SolutionMode_get_ppa_soln_tolerance,(setter)SolutionMode_set_ppa_soln_tolerance,
	PyDoc_STR("*float*: PPA solution tolerance\n\n*Required*: set to 1e-3 if not provided."),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject SolutionMode_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Levpartflip.SolutionMode",             /*tp_name*/
		sizeof(SolutionModeObject),          /*tp_basicsize*/
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
		SolutionMode_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		SolutionMode_getset,          /*tp_getset*/
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
	 * ConstructionFinancing Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Levpartflip   data_ptr;
} ConstructionFinancingObject;

static PyTypeObject ConstructionFinancing_Type;

static PyObject *
ConstructionFinancing_new(SAM_Levpartflip data_ptr)
{
	PyObject* new_obj = ConstructionFinancing_Type.tp_alloc(&ConstructionFinancing_Type,0);

	ConstructionFinancingObject* ConstructionFinancing_obj = (ConstructionFinancingObject*)new_obj;

	ConstructionFinancing_obj->data_ptr = data_ptr;

	return new_obj;
}

/* ConstructionFinancing methods */

static PyObject *
ConstructionFinancing_assign(ConstructionFinancingObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Levpartflip", "ConstructionFinancing")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
ConstructionFinancing_export(ConstructionFinancingObject *self, PyObject *args)
{
	PyTypeObject* tp = &ConstructionFinancing_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef ConstructionFinancing_methods[] = {
		{"assign",            (PyCFunction)ConstructionFinancing_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``ConstructionFinancing_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)ConstructionFinancing_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
ConstructionFinancing_get_construction_financing_cost(ConstructionFinancingObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_ConstructionFinancing_construction_financing_cost_nget, self->data_ptr);
}

static int
ConstructionFinancing_set_construction_financing_cost(ConstructionFinancingObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_ConstructionFinancing_construction_financing_cost_nset, self->data_ptr);
}

static PyGetSetDef ConstructionFinancing_getset[] = {
{"construction_financing_cost", (getter)ConstructionFinancing_get_construction_financing_cost,(setter)ConstructionFinancing_set_construction_financing_cost,
	PyDoc_STR("*float*: Construction financing total [$]\n\n*Required*: True"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject ConstructionFinancing_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Levpartflip.ConstructionFinancing",             /*tp_name*/
		sizeof(ConstructionFinancingObject),          /*tp_basicsize*/
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
		ConstructionFinancing_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		ConstructionFinancing_getset,          /*tp_getset*/
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
	 * ProjectTermDebt Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Levpartflip   data_ptr;
} ProjectTermDebtObject;

static PyTypeObject ProjectTermDebt_Type;

static PyObject *
ProjectTermDebt_new(SAM_Levpartflip data_ptr)
{
	PyObject* new_obj = ProjectTermDebt_Type.tp_alloc(&ProjectTermDebt_Type,0);

	ProjectTermDebtObject* ProjectTermDebt_obj = (ProjectTermDebtObject*)new_obj;

	ProjectTermDebt_obj->data_ptr = data_ptr;

	return new_obj;
}

/* ProjectTermDebt methods */

static PyObject *
ProjectTermDebt_assign(ProjectTermDebtObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Levpartflip", "ProjectTermDebt")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
ProjectTermDebt_export(ProjectTermDebtObject *self, PyObject *args)
{
	PyTypeObject* tp = &ProjectTermDebt_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef ProjectTermDebt_methods[] = {
		{"assign",            (PyCFunction)ProjectTermDebt_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``ProjectTermDebt_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)ProjectTermDebt_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
ProjectTermDebt_get_debt_option(ProjectTermDebtObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_ProjectTermDebt_debt_option_nget, self->data_ptr);
}

static int
ProjectTermDebt_set_debt_option(ProjectTermDebtObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_ProjectTermDebt_debt_option_nset, self->data_ptr);
}

static PyObject *
ProjectTermDebt_get_debt_percent(ProjectTermDebtObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_ProjectTermDebt_debt_percent_nget, self->data_ptr);
}

static int
ProjectTermDebt_set_debt_percent(ProjectTermDebtObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_ProjectTermDebt_debt_percent_nset, self->data_ptr);
}

static PyObject *
ProjectTermDebt_get_dscr(ProjectTermDebtObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_ProjectTermDebt_dscr_nget, self->data_ptr);
}

static int
ProjectTermDebt_set_dscr(ProjectTermDebtObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_ProjectTermDebt_dscr_nset, self->data_ptr);
}

static PyObject *
ProjectTermDebt_get_dscr_reserve_months(ProjectTermDebtObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_ProjectTermDebt_dscr_reserve_months_nget, self->data_ptr);
}

static int
ProjectTermDebt_set_dscr_reserve_months(ProjectTermDebtObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_ProjectTermDebt_dscr_reserve_months_nset, self->data_ptr);
}

static PyObject *
ProjectTermDebt_get_payment_option(ProjectTermDebtObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_ProjectTermDebt_payment_option_nget, self->data_ptr);
}

static int
ProjectTermDebt_set_payment_option(ProjectTermDebtObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_ProjectTermDebt_payment_option_nset, self->data_ptr);
}

static PyObject *
ProjectTermDebt_get_term_int_rate(ProjectTermDebtObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_ProjectTermDebt_term_int_rate_nget, self->data_ptr);
}

static int
ProjectTermDebt_set_term_int_rate(ProjectTermDebtObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_ProjectTermDebt_term_int_rate_nset, self->data_ptr);
}

static PyObject *
ProjectTermDebt_get_term_tenor(ProjectTermDebtObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_ProjectTermDebt_term_tenor_nget, self->data_ptr);
}

static int
ProjectTermDebt_set_term_tenor(ProjectTermDebtObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_ProjectTermDebt_term_tenor_nset, self->data_ptr);
}

static PyGetSetDef ProjectTermDebt_getset[] = {
{"debt_option", (getter)ProjectTermDebt_get_debt_option,(setter)ProjectTermDebt_set_debt_option,
	PyDoc_STR("*float*: Debt option [0/1]\n\n*Options*: 0=debt percent,1=dscr\n\n*Constraints*: INTEGER,MIN=0,MAX=1\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"debt_percent", (getter)ProjectTermDebt_get_debt_percent,(setter)ProjectTermDebt_set_debt_percent,
	PyDoc_STR("*float*: Debt percent [%]\n\n*Constraints*: MIN=0,MAX=100\n\n*Required*: set to 50 if not provided."),
 	NULL},
{"dscr", (getter)ProjectTermDebt_get_dscr,(setter)ProjectTermDebt_set_dscr,
	PyDoc_STR("*float*: Debt service coverage ratio\n\n*Constraints*: MIN=0\n\n*Required*: set to 1.5 if not provided."),
 	NULL},
{"dscr_reserve_months", (getter)ProjectTermDebt_get_dscr_reserve_months,(setter)ProjectTermDebt_set_dscr_reserve_months,
	PyDoc_STR("*float*: Debt service reserve account [months P&I]\n\n*Constraints*: MIN=0\n\n*Required*: set to 6 if not provided."),
 	NULL},
{"payment_option", (getter)ProjectTermDebt_get_payment_option,(setter)ProjectTermDebt_set_payment_option,
	PyDoc_STR("*float*: Debt repayment option [0/1]\n\n*Options*: 0=Equal payments (standard amortization),1=Fixed principal declining interest\n\n*Constraints*: INTEGER,MIN=0,MAX=1\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"term_int_rate", (getter)ProjectTermDebt_get_term_int_rate,(setter)ProjectTermDebt_set_term_int_rate,
	PyDoc_STR("*float*: Term financing interest rate [%]\n\n*Constraints*: MIN=0,MAX=100\n\n*Required*: set to 8.5 if not provided."),
 	NULL},
{"term_tenor", (getter)ProjectTermDebt_get_term_tenor,(setter)ProjectTermDebt_set_term_tenor,
	PyDoc_STR("*float*: Term financing tenor [years]\n\n*Constraints*: INTEGER,MIN=0\n\n*Required*: set to 10 if not provided."),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject ProjectTermDebt_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Levpartflip.ProjectTermDebt",             /*tp_name*/
		sizeof(ProjectTermDebtObject),          /*tp_basicsize*/
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
		ProjectTermDebt_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		ProjectTermDebt_getset,          /*tp_getset*/
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
	 * OtherCapitalCosts Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Levpartflip   data_ptr;
} OtherCapitalCostsObject;

static PyTypeObject OtherCapitalCosts_Type;

static PyObject *
OtherCapitalCosts_new(SAM_Levpartflip data_ptr)
{
	PyObject* new_obj = OtherCapitalCosts_Type.tp_alloc(&OtherCapitalCosts_Type,0);

	OtherCapitalCostsObject* OtherCapitalCosts_obj = (OtherCapitalCostsObject*)new_obj;

	OtherCapitalCosts_obj->data_ptr = data_ptr;

	return new_obj;
}

/* OtherCapitalCosts methods */

static PyObject *
OtherCapitalCosts_assign(OtherCapitalCostsObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Levpartflip", "OtherCapitalCosts")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
OtherCapitalCosts_export(OtherCapitalCostsObject *self, PyObject *args)
{
	PyTypeObject* tp = &OtherCapitalCosts_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef OtherCapitalCosts_methods[] = {
		{"assign",            (PyCFunction)OtherCapitalCosts_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``OtherCapitalCosts_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)OtherCapitalCosts_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
OtherCapitalCosts_get_cost_debt_closing(OtherCapitalCostsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_OtherCapitalCosts_cost_debt_closing_nget, self->data_ptr);
}

static int
OtherCapitalCosts_set_cost_debt_closing(OtherCapitalCostsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_OtherCapitalCosts_cost_debt_closing_nset, self->data_ptr);
}

static PyObject *
OtherCapitalCosts_get_cost_debt_fee(OtherCapitalCostsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_OtherCapitalCosts_cost_debt_fee_nget, self->data_ptr);
}

static int
OtherCapitalCosts_set_cost_debt_fee(OtherCapitalCostsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_OtherCapitalCosts_cost_debt_fee_nset, self->data_ptr);
}

static PyObject *
OtherCapitalCosts_get_cost_dev_fee_percent(OtherCapitalCostsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_OtherCapitalCosts_cost_dev_fee_percent_nget, self->data_ptr);
}

static int
OtherCapitalCosts_set_cost_dev_fee_percent(OtherCapitalCostsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_OtherCapitalCosts_cost_dev_fee_percent_nset, self->data_ptr);
}

static PyObject *
OtherCapitalCosts_get_cost_equity_closing(OtherCapitalCostsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_OtherCapitalCosts_cost_equity_closing_nget, self->data_ptr);
}

static int
OtherCapitalCosts_set_cost_equity_closing(OtherCapitalCostsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_OtherCapitalCosts_cost_equity_closing_nset, self->data_ptr);
}

static PyObject *
OtherCapitalCosts_get_cost_other_financing(OtherCapitalCostsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_OtherCapitalCosts_cost_other_financing_nget, self->data_ptr);
}

static int
OtherCapitalCosts_set_cost_other_financing(OtherCapitalCostsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_OtherCapitalCosts_cost_other_financing_nset, self->data_ptr);
}

static PyObject *
OtherCapitalCosts_get_months_receivables_reserve(OtherCapitalCostsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_OtherCapitalCosts_months_receivables_reserve_nget, self->data_ptr);
}

static int
OtherCapitalCosts_set_months_receivables_reserve(OtherCapitalCostsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_OtherCapitalCosts_months_receivables_reserve_nset, self->data_ptr);
}

static PyObject *
OtherCapitalCosts_get_months_working_reserve(OtherCapitalCostsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_OtherCapitalCosts_months_working_reserve_nget, self->data_ptr);
}

static int
OtherCapitalCosts_set_months_working_reserve(OtherCapitalCostsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_OtherCapitalCosts_months_working_reserve_nset, self->data_ptr);
}

static PyGetSetDef OtherCapitalCosts_getset[] = {
{"cost_debt_closing", (getter)OtherCapitalCosts_get_cost_debt_closing,(setter)OtherCapitalCosts_set_cost_debt_closing,
	PyDoc_STR("*float*: Debt closing cost [$]\n\n*Constraints*: MIN=0\n\n*Required*: set to 250000 if not provided."),
 	NULL},
{"cost_debt_fee", (getter)OtherCapitalCosts_get_cost_debt_fee,(setter)OtherCapitalCosts_set_cost_debt_fee,
	PyDoc_STR("*float*: Debt closing fee (% of total debt amount) [%]\n\n*Constraints*: MIN=0\n\n*Required*: set to 1.5 if not provided."),
 	NULL},
{"cost_dev_fee_percent", (getter)OtherCapitalCosts_get_cost_dev_fee_percent,(setter)OtherCapitalCosts_set_cost_dev_fee_percent,
	PyDoc_STR("*float*: Development fee (% pre-financing cost) [%]\n\n*Constraints*: MIN=0,MAX=100\n\n*Required*: set to 3 if not provided."),
 	NULL},
{"cost_equity_closing", (getter)OtherCapitalCosts_get_cost_equity_closing,(setter)OtherCapitalCosts_set_cost_equity_closing,
	PyDoc_STR("*float*: Equity closing cost [$]\n\n*Constraints*: MIN=0\n\n*Required*: set to 100000 if not provided."),
 	NULL},
{"cost_other_financing", (getter)OtherCapitalCosts_get_cost_other_financing,(setter)OtherCapitalCosts_set_cost_other_financing,
	PyDoc_STR("*float*:  [$]\n\n*Info*: Other financing cost\n\n*Constraints*: MIN=0\n\n*Required*: set to 150000 if not provided."),
 	NULL},
{"months_receivables_reserve", (getter)OtherCapitalCosts_get_months_receivables_reserve,(setter)OtherCapitalCosts_set_months_receivables_reserve,
	PyDoc_STR("*float*: Receivables reserve months of PPA revenue [months]\n\n*Constraints*: MIN=0\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"months_working_reserve", (getter)OtherCapitalCosts_get_months_working_reserve,(setter)OtherCapitalCosts_set_months_working_reserve,
	PyDoc_STR("*float*: Working capital reserve months of operating costs [months]\n\n*Constraints*: MIN=0\n\n*Required*: set to 6 if not provided."),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject OtherCapitalCosts_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Levpartflip.OtherCapitalCosts",             /*tp_name*/
		sizeof(OtherCapitalCostsObject),          /*tp_basicsize*/
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
		OtherCapitalCosts_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		OtherCapitalCosts_getset,          /*tp_getset*/
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
	 * IRRTargets Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Levpartflip   data_ptr;
} IRRTargetsObject;

static PyTypeObject IRRTargets_Type;

static PyObject *
IRRTargets_new(SAM_Levpartflip data_ptr)
{
	PyObject* new_obj = IRRTargets_Type.tp_alloc(&IRRTargets_Type,0);

	IRRTargetsObject* IRRTargets_obj = (IRRTargetsObject*)new_obj;

	IRRTargets_obj->data_ptr = data_ptr;

	return new_obj;
}

/* IRRTargets methods */

static PyObject *
IRRTargets_assign(IRRTargetsObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Levpartflip", "IRRTargets")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
IRRTargets_export(IRRTargetsObject *self, PyObject *args)
{
	PyTypeObject* tp = &IRRTargets_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef IRRTargets_methods[] = {
		{"assign",            (PyCFunction)IRRTargets_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``IRRTargets_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)IRRTargets_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
IRRTargets_get_flip_target_percent(IRRTargetsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_IRRTargets_flip_target_percent_nget, self->data_ptr);
}

static int
IRRTargets_set_flip_target_percent(IRRTargetsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_IRRTargets_flip_target_percent_nset, self->data_ptr);
}

static PyObject *
IRRTargets_get_flip_target_year(IRRTargetsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_IRRTargets_flip_target_year_nget, self->data_ptr);
}

static int
IRRTargets_set_flip_target_year(IRRTargetsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_IRRTargets_flip_target_year_nset, self->data_ptr);
}

static PyObject *
IRRTargets_get_tax_investor_equity_percent(IRRTargetsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_IRRTargets_tax_investor_equity_percent_nget, self->data_ptr);
}

static int
IRRTargets_set_tax_investor_equity_percent(IRRTargetsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_IRRTargets_tax_investor_equity_percent_nset, self->data_ptr);
}

static PyObject *
IRRTargets_get_tax_investor_postflip_cash_percent(IRRTargetsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_IRRTargets_tax_investor_postflip_cash_percent_nget, self->data_ptr);
}

static int
IRRTargets_set_tax_investor_postflip_cash_percent(IRRTargetsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_IRRTargets_tax_investor_postflip_cash_percent_nset, self->data_ptr);
}

static PyObject *
IRRTargets_get_tax_investor_postflip_tax_percent(IRRTargetsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_IRRTargets_tax_investor_postflip_tax_percent_nget, self->data_ptr);
}

static int
IRRTargets_set_tax_investor_postflip_tax_percent(IRRTargetsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_IRRTargets_tax_investor_postflip_tax_percent_nset, self->data_ptr);
}

static PyObject *
IRRTargets_get_tax_investor_preflip_cash_percent(IRRTargetsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_IRRTargets_tax_investor_preflip_cash_percent_nget, self->data_ptr);
}

static int
IRRTargets_set_tax_investor_preflip_cash_percent(IRRTargetsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_IRRTargets_tax_investor_preflip_cash_percent_nset, self->data_ptr);
}

static PyObject *
IRRTargets_get_tax_investor_preflip_tax_percent(IRRTargetsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_IRRTargets_tax_investor_preflip_tax_percent_nget, self->data_ptr);
}

static int
IRRTargets_set_tax_investor_preflip_tax_percent(IRRTargetsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_IRRTargets_tax_investor_preflip_tax_percent_nset, self->data_ptr);
}

static PyGetSetDef IRRTargets_getset[] = {
{"flip_target_percent", (getter)IRRTargets_get_flip_target_percent,(setter)IRRTargets_set_flip_target_percent,
	PyDoc_STR("*float*: After-tax flip/return target [%]\n\n*Constraints*: MIN=0,MAX=100\n\n*Required*: set to 11 if not provided."),
 	NULL},
{"flip_target_year", (getter)IRRTargets_get_flip_target_year,(setter)IRRTargets_set_flip_target_year,
	PyDoc_STR("*float*: Return target year\n\n*Constraints*: MIN=1\n\n*Required*: set to 11 if not provided."),
 	NULL},
{"tax_investor_equity_percent", (getter)IRRTargets_get_tax_investor_equity_percent,(setter)IRRTargets_set_tax_investor_equity_percent,
	PyDoc_STR("*float*: Tax investor equity [%]\n\n*Constraints*: MIN=0,MAX=100\n\n*Required*: set to 98 if not provided."),
 	NULL},
{"tax_investor_postflip_cash_percent", (getter)IRRTargets_get_tax_investor_postflip_cash_percent,(setter)IRRTargets_set_tax_investor_postflip_cash_percent,
	PyDoc_STR("*float*: Tax investor post-flip cash  [%]\n\n*Constraints*: MIN=0,MAX=100\n\n*Required*: set to 15 if not provided."),
 	NULL},
{"tax_investor_postflip_tax_percent", (getter)IRRTargets_get_tax_investor_postflip_tax_percent,(setter)IRRTargets_set_tax_investor_postflip_tax_percent,
	PyDoc_STR("*float*: Tax investor post-flip tax benefit  [%]\n\n*Constraints*: MIN=0,MAX=100\n\n*Required*: set to 15 if not provided."),
 	NULL},
{"tax_investor_preflip_cash_percent", (getter)IRRTargets_get_tax_investor_preflip_cash_percent,(setter)IRRTargets_set_tax_investor_preflip_cash_percent,
	PyDoc_STR("*float*: Tax investor pre-flip cash  [%]\n\n*Constraints*: MIN=0,MAX=100\n\n*Required*: set to 98 if not provided."),
 	NULL},
{"tax_investor_preflip_tax_percent", (getter)IRRTargets_get_tax_investor_preflip_tax_percent,(setter)IRRTargets_set_tax_investor_preflip_tax_percent,
	PyDoc_STR("*float*: Tax investor pre-flip tax benefit  [%]\n\n*Constraints*: MIN=0,MAX=100\n\n*Required*: set to 98 if not provided."),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject IRRTargets_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Levpartflip.IRRTargets",             /*tp_name*/
		sizeof(IRRTargetsObject),          /*tp_basicsize*/
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
		IRRTargets_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		IRRTargets_getset,          /*tp_getset*/
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
	 * CashIncentives Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Levpartflip   data_ptr;
} CashIncentivesObject;

static PyTypeObject CashIncentives_Type;

static PyObject *
CashIncentives_new(SAM_Levpartflip data_ptr)
{
	PyObject* new_obj = CashIncentives_Type.tp_alloc(&CashIncentives_Type,0);

	CashIncentivesObject* CashIncentives_obj = (CashIncentivesObject*)new_obj;

	CashIncentives_obj->data_ptr = data_ptr;

	return new_obj;
}

/* CashIncentives methods */

static PyObject *
CashIncentives_assign(CashIncentivesObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Levpartflip", "CashIncentives")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
CashIncentives_export(CashIncentivesObject *self, PyObject *args)
{
	PyTypeObject* tp = &CashIncentives_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef CashIncentives_methods[] = {
		{"assign",            (PyCFunction)CashIncentives_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``CashIncentives_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)CashIncentives_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
CashIncentives_get_pbi_fed_for_ds(CashIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_CashIncentives_pbi_fed_for_ds_nget, self->data_ptr);
}

static int
CashIncentives_set_pbi_fed_for_ds(CashIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_CashIncentives_pbi_fed_for_ds_nset, self->data_ptr);
}

static PyObject *
CashIncentives_get_pbi_oth_for_ds(CashIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_CashIncentives_pbi_oth_for_ds_nget, self->data_ptr);
}

static int
CashIncentives_set_pbi_oth_for_ds(CashIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_CashIncentives_pbi_oth_for_ds_nset, self->data_ptr);
}

static PyObject *
CashIncentives_get_pbi_sta_for_ds(CashIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_CashIncentives_pbi_sta_for_ds_nget, self->data_ptr);
}

static int
CashIncentives_set_pbi_sta_for_ds(CashIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_CashIncentives_pbi_sta_for_ds_nset, self->data_ptr);
}

static PyObject *
CashIncentives_get_pbi_uti_for_ds(CashIncentivesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_CashIncentives_pbi_uti_for_ds_nget, self->data_ptr);
}

static int
CashIncentives_set_pbi_uti_for_ds(CashIncentivesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_CashIncentives_pbi_uti_for_ds_nset, self->data_ptr);
}

static PyGetSetDef CashIncentives_getset[] = {
{"pbi_fed_for_ds", (getter)CashIncentives_get_pbi_fed_for_ds,(setter)CashIncentives_set_pbi_fed_for_ds,
	PyDoc_STR("*float*: Federal PBI available for debt service [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"pbi_oth_for_ds", (getter)CashIncentives_get_pbi_oth_for_ds,(setter)CashIncentives_set_pbi_oth_for_ds,
	PyDoc_STR("*float*: Other PBI available for debt service [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"pbi_sta_for_ds", (getter)CashIncentives_get_pbi_sta_for_ds,(setter)CashIncentives_set_pbi_sta_for_ds,
	PyDoc_STR("*float*: State PBI available for debt service [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"pbi_uti_for_ds", (getter)CashIncentives_get_pbi_uti_for_ds,(setter)CashIncentives_set_pbi_uti_for_ds,
	PyDoc_STR("*float*: Utility PBI available for debt service [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject CashIncentives_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Levpartflip.CashIncentives",             /*tp_name*/
		sizeof(CashIncentivesObject),          /*tp_basicsize*/
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
		CashIncentives_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		CashIncentives_getset,          /*tp_getset*/
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
	 * Battery Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Levpartflip   data_ptr;
} BatteryObject;

static PyTypeObject Battery_Type;

static PyObject *
Battery_new(SAM_Levpartflip data_ptr)
{
	PyObject* new_obj = Battery_Type.tp_alloc(&Battery_Type,0);

	BatteryObject* Battery_obj = (BatteryObject*)new_obj;

	Battery_obj->data_ptr = data_ptr;

	return new_obj;
}

/* Battery methods */

static PyObject *
Battery_assign(BatteryObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Levpartflip", "Battery")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
Battery_export(BatteryObject *self, PyObject *args)
{
	PyTypeObject* tp = &Battery_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef Battery_methods[] = {
		{"assign",            (PyCFunction)Battery_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``Battery_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)Battery_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
Battery_get_batt_bank_replacement(BatteryObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Battery_batt_bank_replacement_aget, self->data_ptr);
}

static int
Battery_set_batt_bank_replacement(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Levpartflip_Battery_batt_bank_replacement_aset, self->data_ptr);
}

static PyObject *
Battery_get_batt_computed_bank_capacity(BatteryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Battery_batt_computed_bank_capacity_nget, self->data_ptr);
}

static int
Battery_set_batt_computed_bank_capacity(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_Battery_batt_computed_bank_capacity_nset, self->data_ptr);
}

static PyObject *
Battery_get_batt_replacement_option(BatteryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Battery_batt_replacement_option_nget, self->data_ptr);
}

static int
Battery_set_batt_replacement_option(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_Battery_batt_replacement_option_nset, self->data_ptr);
}

static PyObject *
Battery_get_batt_replacement_schedule(BatteryObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Battery_batt_replacement_schedule_aget, self->data_ptr);
}

static int
Battery_set_batt_replacement_schedule(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Levpartflip_Battery_batt_replacement_schedule_aset, self->data_ptr);
}

static PyObject *
Battery_get_battery_per_kWh(BatteryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Battery_battery_per_kWh_nget, self->data_ptr);
}

static int
Battery_set_battery_per_kWh(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_Battery_battery_per_kWh_nset, self->data_ptr);
}

static PyObject *
Battery_get_en_batt(BatteryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Battery_en_batt_nget, self->data_ptr);
}

static int
Battery_set_en_batt(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Levpartflip_Battery_en_batt_nset, self->data_ptr);
}

static PyGetSetDef Battery_getset[] = {
{"batt_bank_replacement", (getter)Battery_get_batt_bank_replacement,(setter)Battery_set_batt_bank_replacement,
	PyDoc_STR("*sequence*: Battery bank replacements per year [number/year]"),
 	NULL},
{"batt_computed_bank_capacity", (getter)Battery_get_batt_computed_bank_capacity,(setter)Battery_set_batt_computed_bank_capacity,
	PyDoc_STR("*float*: Battery bank capacity [kWh]\n\n*Required*: set to 0.0 if not provided."),
 	NULL},
{"batt_replacement_option", (getter)Battery_get_batt_replacement_option,(setter)Battery_set_batt_replacement_option,
	PyDoc_STR("*float*: Enable battery replacement? [0=none,1=capacity based,2=user schedule]\n\n*Constraints*: INTEGER,MIN=0,MAX=2\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"batt_replacement_schedule", (getter)Battery_get_batt_replacement_schedule,(setter)Battery_set_batt_replacement_schedule,
	PyDoc_STR("*sequence*: Battery bank replacements per year (user specified) [number/year]"),
 	NULL},
{"battery_per_kWh", (getter)Battery_get_battery_per_kWh,(setter)Battery_set_battery_per_kWh,
	PyDoc_STR("*float*: Battery cost [$/kWh]\n\n*Required*: set to 0.0 if not provided."),
 	NULL},
{"en_batt", (getter)Battery_get_en_batt,(setter)Battery_set_en_batt,
	PyDoc_STR("*float*: Enable battery storage model [0/1]\n\n*Required*: set to 0 if not provided."),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject Battery_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Levpartflip.Battery",             /*tp_name*/
		sizeof(BatteryObject),          /*tp_basicsize*/
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
		Battery_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		Battery_getset,          /*tp_getset*/
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
	SAM_Levpartflip   data_ptr;
} OutputsObject;

static PyTypeObject Outputs_Type;

static PyObject *
Outputs_new(SAM_Levpartflip data_ptr)
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

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Levpartflip", "Outputs")){
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
Outputs_get_adjusted_installed_cost(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_adjusted_installed_cost_nget, self->data_ptr);
}

static PyObject *
Outputs_get_analysis_period_irr(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_analysis_period_irr_nget, self->data_ptr);
}

static PyObject *
Outputs_get_cash_for_debt_service(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_cash_for_debt_service_nget, self->data_ptr);
}

static PyObject *
Outputs_get_cbi_fedtax_total(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_cbi_fedtax_total_nget, self->data_ptr);
}

static PyObject *
Outputs_get_cbi_statax_total(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_cbi_statax_total_nget, self->data_ptr);
}

static PyObject *
Outputs_get_cbi_total(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_cbi_total_nget, self->data_ptr);
}

static PyObject *
Outputs_get_cbi_total_fed(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_cbi_total_fed_nget, self->data_ptr);
}

static PyObject *
Outputs_get_cbi_total_oth(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_cbi_total_oth_nget, self->data_ptr);
}

static PyObject *
Outputs_get_cbi_total_sta(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_cbi_total_sta_nget, self->data_ptr);
}

static PyObject *
Outputs_get_cbi_total_uti(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_cbi_total_uti_nget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_annual_costs(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_annual_costs_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_battery_replacement_cost(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_battery_replacement_cost_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_battery_replacement_cost_schedule(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_battery_replacement_cost_schedule_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_cash_for_ds(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_cash_for_ds_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_debt_balance(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_debt_balance_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_debt_payment_interest(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_debt_payment_interest_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_debt_payment_principal(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_debt_payment_principal_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_debt_payment_total(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_debt_payment_total_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_debt_size(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_debt_size_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_disbursement_debtservice(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_disbursement_debtservice_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_disbursement_equip1(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_disbursement_equip1_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_disbursement_equip2(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_disbursement_equip2_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_disbursement_equip3(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_disbursement_equip3_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_disbursement_om(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_disbursement_om_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_disbursement_receivables(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_disbursement_receivables_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_ebitda(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_ebitda_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_effective_tax_frac(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_effective_tax_frac_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_energy_net(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_energy_net_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_energy_net_apr(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_energy_net_apr_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_energy_net_aug(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_energy_net_aug_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_energy_net_dec(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_energy_net_dec_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_energy_net_dispatch1(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_energy_net_dispatch1_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_energy_net_dispatch2(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_energy_net_dispatch2_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_energy_net_dispatch3(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_energy_net_dispatch3_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_energy_net_dispatch4(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_energy_net_dispatch4_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_energy_net_dispatch5(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_energy_net_dispatch5_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_energy_net_dispatch6(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_energy_net_dispatch6_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_energy_net_dispatch7(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_energy_net_dispatch7_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_energy_net_dispatch8(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_energy_net_dispatch8_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_energy_net_dispatch9(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_energy_net_dispatch9_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_energy_net_feb(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_energy_net_feb_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_energy_net_jan(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_energy_net_jan_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_energy_net_jul(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_energy_net_jul_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_energy_net_jun(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_energy_net_jun_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_energy_net_mar(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_energy_net_mar_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_energy_net_may(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_energy_net_may_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_energy_net_monthly_firstyear_TOD1(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_energy_net_monthly_firstyear_TOD1_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_energy_net_monthly_firstyear_TOD2(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_energy_net_monthly_firstyear_TOD2_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_energy_net_monthly_firstyear_TOD3(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_energy_net_monthly_firstyear_TOD3_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_energy_net_monthly_firstyear_TOD4(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_energy_net_monthly_firstyear_TOD4_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_energy_net_monthly_firstyear_TOD5(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_energy_net_monthly_firstyear_TOD5_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_energy_net_monthly_firstyear_TOD6(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_energy_net_monthly_firstyear_TOD6_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_energy_net_monthly_firstyear_TOD7(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_energy_net_monthly_firstyear_TOD7_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_energy_net_monthly_firstyear_TOD8(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_energy_net_monthly_firstyear_TOD8_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_energy_net_monthly_firstyear_TOD9(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_energy_net_monthly_firstyear_TOD9_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_energy_net_nov(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_energy_net_nov_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_energy_net_oct(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_energy_net_oct_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_energy_net_sep(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_energy_net_sep_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_energy_value(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_energy_value_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_feddepr_custom(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_feddepr_custom_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_feddepr_macrs_15(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_feddepr_macrs_15_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_feddepr_macrs_5(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_feddepr_macrs_5_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_feddepr_me1(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_feddepr_me1_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_feddepr_me2(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_feddepr_me2_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_feddepr_me3(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_feddepr_me3_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_feddepr_sl_15(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_feddepr_sl_15_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_feddepr_sl_20(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_feddepr_sl_20_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_feddepr_sl_39(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_feddepr_sl_39_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_feddepr_sl_5(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_feddepr_sl_5_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_feddepr_total(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_feddepr_total_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_federal_tax_frac(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_federal_tax_frac_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_fedtax(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_fedtax_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_fedtax_income_prior_incentives(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_fedtax_income_prior_incentives_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_fedtax_income_with_incentives(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_fedtax_income_with_incentives_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_fedtax_taxable_incentives(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_fedtax_taxable_incentives_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_funding_debtservice(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_funding_debtservice_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_funding_equip1(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_funding_equip1_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_funding_equip2(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_funding_equip2_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_funding_equip3(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_funding_equip3_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_funding_om(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_funding_om_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_funding_receivables(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_funding_receivables_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_insurance_expense(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_insurance_expense_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_length(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_cf_length_nget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_net_salvage_value(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_net_salvage_value_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_om_capacity_expense(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_om_capacity_expense_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_om_fixed_expense(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_om_fixed_expense_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_om_fuel_expense(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_om_fuel_expense_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_om_opt_fuel_1_expense(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_om_opt_fuel_1_expense_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_om_opt_fuel_2_expense(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_om_opt_fuel_2_expense_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_om_production_expense(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_om_production_expense_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_operating_expenses(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_operating_expenses_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_pbi_fedtax_total(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_pbi_fedtax_total_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_pbi_statax_total(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_pbi_statax_total_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_pbi_total(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_pbi_total_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_pbi_total_fed(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_pbi_total_fed_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_pbi_total_oth(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_pbi_total_oth_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_pbi_total_sta(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_pbi_total_sta_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_pbi_total_uti(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_pbi_total_uti_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_ppa_price(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_ppa_price_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_pretax_cashflow(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_pretax_cashflow_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_pretax_dscr(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_pretax_dscr_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_project_dsra(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_project_dsra_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_project_financing_activities(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_project_financing_activities_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_project_investing_activities(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_project_investing_activities_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_project_me1cs(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_project_me1cs_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_project_me1ra(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_project_me1ra_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_project_me2cs(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_project_me2cs_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_project_me2ra(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_project_me2ra_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_project_me3cs(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_project_me3cs_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_project_me3ra(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_project_me3ra_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_project_mecs(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_project_mecs_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_project_operating_activities(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_project_operating_activities_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_project_ra(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_project_ra_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_project_receivablesra(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_project_receivablesra_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_project_return_aftertax(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_project_return_aftertax_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_project_return_aftertax_cash(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_project_return_aftertax_cash_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_project_return_aftertax_irr(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_project_return_aftertax_irr_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_project_return_aftertax_npv(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_project_return_aftertax_npv_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_project_return_pretax(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_project_return_pretax_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_project_return_pretax_irr(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_project_return_pretax_irr_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_project_return_pretax_npv(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_project_return_pretax_npv_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_project_wcra(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_project_wcra_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_property_tax_assessed_value(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_property_tax_assessed_value_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_property_tax_expense(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_property_tax_expense_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_ptc_fed(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_ptc_fed_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_ptc_sta(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_ptc_sta_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_pv_cash_for_ds(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_pv_cash_for_ds_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_pv_interest_factor(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_pv_interest_factor_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_recapitalization(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_recapitalization_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_reserve_debtservice(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_reserve_debtservice_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_reserve_equip1(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_reserve_equip1_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_reserve_equip2(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_reserve_equip2_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_reserve_equip3(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_reserve_equip3_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_reserve_interest(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_reserve_interest_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_reserve_om(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_reserve_om_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_reserve_receivables(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_reserve_receivables_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_reserve_total(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_reserve_total_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_revenue_apr(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_revenue_apr_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_revenue_aug(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_revenue_aug_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_revenue_dec(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_revenue_dec_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_revenue_dispatch1(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_revenue_dispatch1_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_revenue_dispatch2(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_revenue_dispatch2_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_revenue_dispatch3(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_revenue_dispatch3_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_revenue_dispatch4(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_revenue_dispatch4_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_revenue_dispatch5(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_revenue_dispatch5_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_revenue_dispatch6(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_revenue_dispatch6_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_revenue_dispatch7(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_revenue_dispatch7_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_revenue_dispatch8(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_revenue_dispatch8_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_revenue_dispatch9(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_revenue_dispatch9_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_revenue_feb(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_revenue_feb_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_revenue_jan(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_revenue_jan_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_revenue_jul(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_revenue_jul_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_revenue_jun(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_revenue_jun_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_revenue_mar(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_revenue_mar_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_revenue_may(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_revenue_may_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_revenue_monthly_firstyear_TOD1(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_revenue_monthly_firstyear_TOD1_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_revenue_monthly_firstyear_TOD2(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_revenue_monthly_firstyear_TOD2_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_revenue_monthly_firstyear_TOD3(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_revenue_monthly_firstyear_TOD3_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_revenue_monthly_firstyear_TOD4(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_revenue_monthly_firstyear_TOD4_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_revenue_monthly_firstyear_TOD5(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_revenue_monthly_firstyear_TOD5_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_revenue_monthly_firstyear_TOD6(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_revenue_monthly_firstyear_TOD6_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_revenue_monthly_firstyear_TOD7(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_revenue_monthly_firstyear_TOD7_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_revenue_monthly_firstyear_TOD8(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_revenue_monthly_firstyear_TOD8_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_revenue_monthly_firstyear_TOD9(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_revenue_monthly_firstyear_TOD9_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_revenue_nov(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_revenue_nov_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_revenue_oct(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_revenue_oct_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_revenue_sep(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_revenue_sep_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_sponsor_aftertax(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_sponsor_aftertax_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_sponsor_aftertax_cash(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_sponsor_aftertax_cash_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_sponsor_aftertax_irr(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_sponsor_aftertax_irr_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_sponsor_aftertax_itc(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_sponsor_aftertax_itc_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_sponsor_aftertax_npv(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_sponsor_aftertax_npv_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_sponsor_aftertax_ptc(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_sponsor_aftertax_ptc_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_sponsor_aftertax_tax(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_sponsor_aftertax_tax_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_sponsor_pretax(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_sponsor_pretax_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_sponsor_pretax_irr(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_sponsor_pretax_irr_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_sponsor_pretax_npv(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_sponsor_pretax_npv_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_stadepr_custom(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_stadepr_custom_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_stadepr_macrs_15(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_stadepr_macrs_15_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_stadepr_macrs_5(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_stadepr_macrs_5_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_stadepr_me1(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_stadepr_me1_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_stadepr_me2(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_stadepr_me2_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_stadepr_me3(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_stadepr_me3_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_stadepr_sl_15(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_stadepr_sl_15_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_stadepr_sl_20(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_stadepr_sl_20_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_stadepr_sl_39(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_stadepr_sl_39_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_stadepr_sl_5(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_stadepr_sl_5_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_stadepr_total(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_stadepr_total_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_statax(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_statax_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_statax_income_prior_incentives(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_statax_income_prior_incentives_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_statax_income_with_incentives(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_statax_income_with_incentives_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_statax_taxable_incentives(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_statax_taxable_incentives_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_state_tax_frac(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_state_tax_frac_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_tax_investor_aftertax(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_tax_investor_aftertax_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_tax_investor_aftertax_cash(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_tax_investor_aftertax_cash_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_tax_investor_aftertax_irr(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_tax_investor_aftertax_irr_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_tax_investor_aftertax_itc(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_tax_investor_aftertax_itc_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_tax_investor_aftertax_max_irr(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_tax_investor_aftertax_max_irr_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_tax_investor_aftertax_npv(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_tax_investor_aftertax_npv_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_tax_investor_aftertax_ptc(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_tax_investor_aftertax_ptc_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_tax_investor_aftertax_tax(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_tax_investor_aftertax_tax_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_tax_investor_pretax(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_tax_investor_pretax_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_tax_investor_pretax_irr(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_tax_investor_pretax_irr_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_tax_investor_pretax_npv(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_tax_investor_pretax_npv_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cf_total_revenue(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_cf_total_revenue_aget, self->data_ptr);
}

static PyObject *
Outputs_get_cost_debt_upfront(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_cost_debt_upfront_nget, self->data_ptr);
}

static PyObject *
Outputs_get_cost_financing(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_cost_financing_nget, self->data_ptr);
}

static PyObject *
Outputs_get_cost_installed(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_cost_installed_nget, self->data_ptr);
}

static PyObject *
Outputs_get_cost_installedperwatt(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_cost_installedperwatt_nget, self->data_ptr);
}

static PyObject *
Outputs_get_cost_prefinancing(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_cost_prefinancing_nget, self->data_ptr);
}

static PyObject *
Outputs_get_debt_fraction(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_debt_fraction_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_alloc_custom(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_alloc_custom_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_alloc_macrs_15(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_alloc_macrs_15_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_alloc_macrs_5(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_alloc_macrs_5_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_alloc_none(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_alloc_none_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_alloc_none_percent(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_alloc_none_percent_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_alloc_sl_15(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_alloc_sl_15_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_alloc_sl_20(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_alloc_sl_20_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_alloc_sl_39(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_alloc_sl_39_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_alloc_sl_5(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_alloc_sl_5_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_alloc_total(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_alloc_total_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_after_itc_custom(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_after_itc_custom_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_after_itc_macrs_15(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_after_itc_macrs_15_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_after_itc_macrs_5(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_after_itc_macrs_5_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_after_itc_sl_15(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_after_itc_sl_15_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_after_itc_sl_20(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_after_itc_sl_20_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_after_itc_sl_39(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_after_itc_sl_39_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_after_itc_sl_5(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_after_itc_sl_5_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_after_itc_total(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_after_itc_total_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_cbi_reduc_custom(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_cbi_reduc_custom_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_cbi_reduc_macrs_15(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_cbi_reduc_macrs_15_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_cbi_reduc_macrs_5(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_cbi_reduc_macrs_5_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_cbi_reduc_sl_15(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_cbi_reduc_sl_15_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_cbi_reduc_sl_20(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_cbi_reduc_sl_20_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_cbi_reduc_sl_39(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_cbi_reduc_sl_39_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_cbi_reduc_sl_5(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_cbi_reduc_sl_5_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_cbi_reduc_total(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_cbi_reduc_total_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_custom(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_custom_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_first_year_bonus_custom(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_first_year_bonus_custom_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_first_year_bonus_macrs_15(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_first_year_bonus_macrs_15_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_first_year_bonus_macrs_5(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_first_year_bonus_macrs_5_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_first_year_bonus_sl_15(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_first_year_bonus_sl_15_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_first_year_bonus_sl_20(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_first_year_bonus_sl_20_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_first_year_bonus_sl_39(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_first_year_bonus_sl_39_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_first_year_bonus_sl_5(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_first_year_bonus_sl_5_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_first_year_bonus_total(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_first_year_bonus_total_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_fixed_amount_custom(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_fixed_amount_custom_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_fixed_amount_macrs_15(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_fixed_amount_macrs_15_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_fixed_amount_macrs_5(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_fixed_amount_macrs_5_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_fixed_amount_sl_15(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_fixed_amount_sl_15_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_fixed_amount_sl_20(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_fixed_amount_sl_20_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_fixed_amount_sl_39(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_fixed_amount_sl_39_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_fixed_amount_sl_5(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_fixed_amount_sl_5_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_fixed_amount_total(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_fixed_amount_total_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_ibi_reduc_custom(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_ibi_reduc_custom_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_ibi_reduc_macrs_15(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_ibi_reduc_macrs_15_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_ibi_reduc_macrs_5(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_ibi_reduc_macrs_5_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_ibi_reduc_sl_15(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_ibi_reduc_sl_15_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_ibi_reduc_sl_20(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_ibi_reduc_sl_20_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_ibi_reduc_sl_39(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_ibi_reduc_sl_39_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_ibi_reduc_sl_5(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_ibi_reduc_sl_5_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_ibi_reduc_total(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_ibi_reduc_total_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_itc_fed_reduction_custom(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_itc_fed_reduction_custom_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_itc_fed_reduction_macrs_15(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_itc_fed_reduction_macrs_15_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_itc_fed_reduction_macrs_5(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_itc_fed_reduction_macrs_5_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_itc_fed_reduction_sl_15(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_itc_fed_reduction_sl_15_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_itc_fed_reduction_sl_20(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_itc_fed_reduction_sl_20_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_itc_fed_reduction_sl_39(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_itc_fed_reduction_sl_39_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_itc_fed_reduction_sl_5(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_itc_fed_reduction_sl_5_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_itc_fed_reduction_total(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_itc_fed_reduction_total_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_itc_sta_reduction_custom(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_itc_sta_reduction_custom_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_itc_sta_reduction_macrs_15(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_itc_sta_reduction_macrs_15_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_itc_sta_reduction_macrs_5(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_itc_sta_reduction_macrs_5_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_itc_sta_reduction_sl_15(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_itc_sta_reduction_sl_15_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_itc_sta_reduction_sl_20(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_itc_sta_reduction_sl_20_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_itc_sta_reduction_sl_39(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_itc_sta_reduction_sl_39_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_itc_sta_reduction_sl_5(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_itc_sta_reduction_sl_5_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_itc_sta_reduction_total(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_itc_sta_reduction_total_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_macrs_15(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_macrs_15_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_macrs_5(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_macrs_5_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_percent_amount_custom(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_percent_amount_custom_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_percent_amount_macrs_15(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_percent_amount_macrs_15_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_percent_amount_macrs_5(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_percent_amount_macrs_5_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_percent_amount_sl_15(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_percent_amount_sl_15_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_percent_amount_sl_20(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_percent_amount_sl_20_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_percent_amount_sl_39(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_percent_amount_sl_39_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_percent_amount_sl_5(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_percent_amount_sl_5_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_percent_amount_total(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_percent_amount_total_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_percent_custom(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_percent_custom_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_percent_macrs_15(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_percent_macrs_15_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_percent_macrs_5(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_percent_macrs_5_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_percent_qual_custom(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_percent_qual_custom_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_percent_qual_macrs_15(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_percent_qual_macrs_15_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_percent_qual_macrs_5(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_percent_qual_macrs_5_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_percent_qual_sl_15(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_percent_qual_sl_15_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_percent_qual_sl_20(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_percent_qual_sl_20_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_percent_qual_sl_39(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_percent_qual_sl_39_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_percent_qual_sl_5(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_percent_qual_sl_5_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_percent_qual_total(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_percent_qual_total_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_percent_sl_15(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_percent_sl_15_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_percent_sl_20(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_percent_sl_20_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_percent_sl_39(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_percent_sl_39_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_percent_sl_5(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_percent_sl_5_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_percent_total(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_percent_total_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_prior_itc_custom(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_prior_itc_custom_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_prior_itc_macrs_15(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_prior_itc_macrs_15_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_prior_itc_macrs_5(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_prior_itc_macrs_5_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_prior_itc_sl_15(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_prior_itc_sl_15_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_prior_itc_sl_20(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_prior_itc_sl_20_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_prior_itc_sl_39(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_prior_itc_sl_39_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_prior_itc_sl_5(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_prior_itc_sl_5_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_prior_itc_total(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_prior_itc_total_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_sl_15(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_sl_15_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_sl_20(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_sl_20_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_sl_39(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_sl_39_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_sl_5(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_sl_5_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_fedbas_total(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_fedbas_total_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_after_itc_custom(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_after_itc_custom_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_after_itc_macrs_15(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_after_itc_macrs_15_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_after_itc_macrs_5(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_after_itc_macrs_5_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_after_itc_sl_15(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_after_itc_sl_15_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_after_itc_sl_20(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_after_itc_sl_20_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_after_itc_sl_39(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_after_itc_sl_39_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_after_itc_sl_5(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_after_itc_sl_5_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_after_itc_total(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_after_itc_total_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_cbi_reduc_custom(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_cbi_reduc_custom_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_cbi_reduc_macrs_15(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_cbi_reduc_macrs_15_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_cbi_reduc_macrs_5(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_cbi_reduc_macrs_5_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_cbi_reduc_sl_15(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_cbi_reduc_sl_15_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_cbi_reduc_sl_20(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_cbi_reduc_sl_20_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_cbi_reduc_sl_39(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_cbi_reduc_sl_39_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_cbi_reduc_sl_5(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_cbi_reduc_sl_5_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_cbi_reduc_total(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_cbi_reduc_total_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_custom(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_custom_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_first_year_bonus_custom(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_first_year_bonus_custom_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_first_year_bonus_macrs_15(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_first_year_bonus_macrs_15_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_first_year_bonus_macrs_5(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_first_year_bonus_macrs_5_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_first_year_bonus_sl_15(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_first_year_bonus_sl_15_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_first_year_bonus_sl_20(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_first_year_bonus_sl_20_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_first_year_bonus_sl_39(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_first_year_bonus_sl_39_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_first_year_bonus_sl_5(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_first_year_bonus_sl_5_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_first_year_bonus_total(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_first_year_bonus_total_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_fixed_amount_custom(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_fixed_amount_custom_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_fixed_amount_macrs_15(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_fixed_amount_macrs_15_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_fixed_amount_macrs_5(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_fixed_amount_macrs_5_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_fixed_amount_sl_15(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_fixed_amount_sl_15_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_fixed_amount_sl_20(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_fixed_amount_sl_20_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_fixed_amount_sl_39(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_fixed_amount_sl_39_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_fixed_amount_sl_5(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_fixed_amount_sl_5_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_fixed_amount_total(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_fixed_amount_total_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_ibi_reduc_custom(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_ibi_reduc_custom_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_ibi_reduc_macrs_15(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_ibi_reduc_macrs_15_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_ibi_reduc_macrs_5(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_ibi_reduc_macrs_5_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_ibi_reduc_sl_15(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_ibi_reduc_sl_15_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_ibi_reduc_sl_20(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_ibi_reduc_sl_20_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_ibi_reduc_sl_39(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_ibi_reduc_sl_39_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_ibi_reduc_sl_5(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_ibi_reduc_sl_5_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_ibi_reduc_total(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_ibi_reduc_total_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_itc_fed_reduction_custom(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_itc_fed_reduction_custom_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_itc_fed_reduction_macrs_15(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_itc_fed_reduction_macrs_15_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_itc_fed_reduction_macrs_5(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_itc_fed_reduction_macrs_5_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_itc_fed_reduction_sl_15(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_itc_fed_reduction_sl_15_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_itc_fed_reduction_sl_20(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_itc_fed_reduction_sl_20_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_itc_fed_reduction_sl_39(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_itc_fed_reduction_sl_39_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_itc_fed_reduction_sl_5(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_itc_fed_reduction_sl_5_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_itc_fed_reduction_total(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_itc_fed_reduction_total_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_itc_sta_reduction_custom(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_itc_sta_reduction_custom_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_itc_sta_reduction_macrs_15(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_itc_sta_reduction_macrs_15_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_itc_sta_reduction_macrs_5(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_itc_sta_reduction_macrs_5_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_itc_sta_reduction_sl_15(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_itc_sta_reduction_sl_15_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_itc_sta_reduction_sl_20(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_itc_sta_reduction_sl_20_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_itc_sta_reduction_sl_39(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_itc_sta_reduction_sl_39_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_itc_sta_reduction_sl_5(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_itc_sta_reduction_sl_5_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_itc_sta_reduction_total(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_itc_sta_reduction_total_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_macrs_15(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_macrs_15_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_macrs_5(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_macrs_5_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_percent_amount_custom(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_percent_amount_custom_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_percent_amount_macrs_15(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_percent_amount_macrs_15_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_percent_amount_macrs_5(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_percent_amount_macrs_5_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_percent_amount_sl_15(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_percent_amount_sl_15_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_percent_amount_sl_20(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_percent_amount_sl_20_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_percent_amount_sl_39(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_percent_amount_sl_39_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_percent_amount_sl_5(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_percent_amount_sl_5_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_percent_amount_total(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_percent_amount_total_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_percent_custom(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_percent_custom_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_percent_macrs_15(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_percent_macrs_15_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_percent_macrs_5(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_percent_macrs_5_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_percent_qual_custom(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_percent_qual_custom_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_percent_qual_macrs_15(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_percent_qual_macrs_15_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_percent_qual_macrs_5(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_percent_qual_macrs_5_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_percent_qual_sl_15(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_percent_qual_sl_15_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_percent_qual_sl_20(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_percent_qual_sl_20_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_percent_qual_sl_39(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_percent_qual_sl_39_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_percent_qual_sl_5(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_percent_qual_sl_5_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_percent_qual_total(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_percent_qual_total_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_percent_sl_15(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_percent_sl_15_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_percent_sl_20(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_percent_sl_20_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_percent_sl_39(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_percent_sl_39_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_percent_sl_5(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_percent_sl_5_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_percent_total(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_percent_total_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_prior_itc_custom(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_prior_itc_custom_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_prior_itc_macrs_15(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_prior_itc_macrs_15_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_prior_itc_macrs_5(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_prior_itc_macrs_5_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_prior_itc_sl_15(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_prior_itc_sl_15_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_prior_itc_sl_20(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_prior_itc_sl_20_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_prior_itc_sl_39(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_prior_itc_sl_39_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_prior_itc_sl_5(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_prior_itc_sl_5_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_prior_itc_total(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_prior_itc_total_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_sl_15(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_sl_15_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_sl_20(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_sl_20_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_sl_39(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_sl_39_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_sl_5(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_sl_5_nget, self->data_ptr);
}

static PyObject *
Outputs_get_depr_stabas_total(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_depr_stabas_total_nget, self->data_ptr);
}

static PyObject *
Outputs_get_effective_tax_rate(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_effective_tax_rate_nget, self->data_ptr);
}

static PyObject *
Outputs_get_firstyear_energy_dispatch1(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_firstyear_energy_dispatch1_nget, self->data_ptr);
}

static PyObject *
Outputs_get_firstyear_energy_dispatch2(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_firstyear_energy_dispatch2_nget, self->data_ptr);
}

static PyObject *
Outputs_get_firstyear_energy_dispatch3(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_firstyear_energy_dispatch3_nget, self->data_ptr);
}

static PyObject *
Outputs_get_firstyear_energy_dispatch4(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_firstyear_energy_dispatch4_nget, self->data_ptr);
}

static PyObject *
Outputs_get_firstyear_energy_dispatch5(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_firstyear_energy_dispatch5_nget, self->data_ptr);
}

static PyObject *
Outputs_get_firstyear_energy_dispatch6(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_firstyear_energy_dispatch6_nget, self->data_ptr);
}

static PyObject *
Outputs_get_firstyear_energy_dispatch7(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_firstyear_energy_dispatch7_nget, self->data_ptr);
}

static PyObject *
Outputs_get_firstyear_energy_dispatch8(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_firstyear_energy_dispatch8_nget, self->data_ptr);
}

static PyObject *
Outputs_get_firstyear_energy_dispatch9(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_firstyear_energy_dispatch9_nget, self->data_ptr);
}

static PyObject *
Outputs_get_firstyear_energy_price1(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_firstyear_energy_price1_nget, self->data_ptr);
}

static PyObject *
Outputs_get_firstyear_energy_price2(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_firstyear_energy_price2_nget, self->data_ptr);
}

static PyObject *
Outputs_get_firstyear_energy_price3(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_firstyear_energy_price3_nget, self->data_ptr);
}

static PyObject *
Outputs_get_firstyear_energy_price4(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_firstyear_energy_price4_nget, self->data_ptr);
}

static PyObject *
Outputs_get_firstyear_energy_price5(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_firstyear_energy_price5_nget, self->data_ptr);
}

static PyObject *
Outputs_get_firstyear_energy_price6(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_firstyear_energy_price6_nget, self->data_ptr);
}

static PyObject *
Outputs_get_firstyear_energy_price7(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_firstyear_energy_price7_nget, self->data_ptr);
}

static PyObject *
Outputs_get_firstyear_energy_price8(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_firstyear_energy_price8_nget, self->data_ptr);
}

static PyObject *
Outputs_get_firstyear_energy_price9(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_firstyear_energy_price9_nget, self->data_ptr);
}

static PyObject *
Outputs_get_firstyear_revenue_dispatch1(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_firstyear_revenue_dispatch1_nget, self->data_ptr);
}

static PyObject *
Outputs_get_firstyear_revenue_dispatch2(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_firstyear_revenue_dispatch2_nget, self->data_ptr);
}

static PyObject *
Outputs_get_firstyear_revenue_dispatch3(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_firstyear_revenue_dispatch3_nget, self->data_ptr);
}

static PyObject *
Outputs_get_firstyear_revenue_dispatch4(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_firstyear_revenue_dispatch4_nget, self->data_ptr);
}

static PyObject *
Outputs_get_firstyear_revenue_dispatch5(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_firstyear_revenue_dispatch5_nget, self->data_ptr);
}

static PyObject *
Outputs_get_firstyear_revenue_dispatch6(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_firstyear_revenue_dispatch6_nget, self->data_ptr);
}

static PyObject *
Outputs_get_firstyear_revenue_dispatch7(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_firstyear_revenue_dispatch7_nget, self->data_ptr);
}

static PyObject *
Outputs_get_firstyear_revenue_dispatch8(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_firstyear_revenue_dispatch8_nget, self->data_ptr);
}

static PyObject *
Outputs_get_firstyear_revenue_dispatch9(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_firstyear_revenue_dispatch9_nget, self->data_ptr);
}

static PyObject *
Outputs_get_flip_actual_irr(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_flip_actual_irr_nget, self->data_ptr);
}

static PyObject *
Outputs_get_flip_actual_year(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_flip_actual_year_nget, self->data_ptr);
}

static PyObject *
Outputs_get_flip_target_irr(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_flip_target_irr_nget, self->data_ptr);
}

static PyObject *
Outputs_get_flip_target_year(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_flip_target_year_nget, self->data_ptr);
}

static PyObject *
Outputs_get_ibi_fedtax_total(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_ibi_fedtax_total_nget, self->data_ptr);
}

static PyObject *
Outputs_get_ibi_statax_total(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_ibi_statax_total_nget, self->data_ptr);
}

static PyObject *
Outputs_get_ibi_total(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_ibi_total_nget, self->data_ptr);
}

static PyObject *
Outputs_get_ibi_total_fed(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_ibi_total_fed_nget, self->data_ptr);
}

static PyObject *
Outputs_get_ibi_total_oth(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_ibi_total_oth_nget, self->data_ptr);
}

static PyObject *
Outputs_get_ibi_total_sta(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_ibi_total_sta_nget, self->data_ptr);
}

static PyObject *
Outputs_get_ibi_total_uti(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_ibi_total_uti_nget, self->data_ptr);
}

static PyObject *
Outputs_get_issuance_of_equity(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_issuance_of_equity_nget, self->data_ptr);
}

static PyObject *
Outputs_get_itc_disallow_fed_fixed_custom(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_itc_disallow_fed_fixed_custom_nget, self->data_ptr);
}

static PyObject *
Outputs_get_itc_disallow_fed_fixed_macrs_15(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_itc_disallow_fed_fixed_macrs_15_nget, self->data_ptr);
}

static PyObject *
Outputs_get_itc_disallow_fed_fixed_macrs_5(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_itc_disallow_fed_fixed_macrs_5_nget, self->data_ptr);
}

static PyObject *
Outputs_get_itc_disallow_fed_fixed_sl_15(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_itc_disallow_fed_fixed_sl_15_nget, self->data_ptr);
}

static PyObject *
Outputs_get_itc_disallow_fed_fixed_sl_20(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_itc_disallow_fed_fixed_sl_20_nget, self->data_ptr);
}

static PyObject *
Outputs_get_itc_disallow_fed_fixed_sl_39(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_itc_disallow_fed_fixed_sl_39_nget, self->data_ptr);
}

static PyObject *
Outputs_get_itc_disallow_fed_fixed_sl_5(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_itc_disallow_fed_fixed_sl_5_nget, self->data_ptr);
}

static PyObject *
Outputs_get_itc_disallow_fed_fixed_total(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_itc_disallow_fed_fixed_total_nget, self->data_ptr);
}

static PyObject *
Outputs_get_itc_disallow_fed_percent_custom(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_itc_disallow_fed_percent_custom_nget, self->data_ptr);
}

static PyObject *
Outputs_get_itc_disallow_fed_percent_macrs_15(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_itc_disallow_fed_percent_macrs_15_nget, self->data_ptr);
}

static PyObject *
Outputs_get_itc_disallow_fed_percent_macrs_5(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_itc_disallow_fed_percent_macrs_5_nget, self->data_ptr);
}

static PyObject *
Outputs_get_itc_disallow_fed_percent_sl_15(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_itc_disallow_fed_percent_sl_15_nget, self->data_ptr);
}

static PyObject *
Outputs_get_itc_disallow_fed_percent_sl_20(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_itc_disallow_fed_percent_sl_20_nget, self->data_ptr);
}

static PyObject *
Outputs_get_itc_disallow_fed_percent_sl_39(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_itc_disallow_fed_percent_sl_39_nget, self->data_ptr);
}

static PyObject *
Outputs_get_itc_disallow_fed_percent_sl_5(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_itc_disallow_fed_percent_sl_5_nget, self->data_ptr);
}

static PyObject *
Outputs_get_itc_disallow_fed_percent_total(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_itc_disallow_fed_percent_total_nget, self->data_ptr);
}

static PyObject *
Outputs_get_itc_disallow_sta_fixed_custom(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_itc_disallow_sta_fixed_custom_nget, self->data_ptr);
}

static PyObject *
Outputs_get_itc_disallow_sta_fixed_macrs_15(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_itc_disallow_sta_fixed_macrs_15_nget, self->data_ptr);
}

static PyObject *
Outputs_get_itc_disallow_sta_fixed_macrs_5(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_itc_disallow_sta_fixed_macrs_5_nget, self->data_ptr);
}

static PyObject *
Outputs_get_itc_disallow_sta_fixed_sl_15(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_itc_disallow_sta_fixed_sl_15_nget, self->data_ptr);
}

static PyObject *
Outputs_get_itc_disallow_sta_fixed_sl_20(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_itc_disallow_sta_fixed_sl_20_nget, self->data_ptr);
}

static PyObject *
Outputs_get_itc_disallow_sta_fixed_sl_39(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_itc_disallow_sta_fixed_sl_39_nget, self->data_ptr);
}

static PyObject *
Outputs_get_itc_disallow_sta_fixed_sl_5(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_itc_disallow_sta_fixed_sl_5_nget, self->data_ptr);
}

static PyObject *
Outputs_get_itc_disallow_sta_fixed_total(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_itc_disallow_sta_fixed_total_nget, self->data_ptr);
}

static PyObject *
Outputs_get_itc_disallow_sta_percent_custom(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_itc_disallow_sta_percent_custom_nget, self->data_ptr);
}

static PyObject *
Outputs_get_itc_disallow_sta_percent_macrs_15(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_itc_disallow_sta_percent_macrs_15_nget, self->data_ptr);
}

static PyObject *
Outputs_get_itc_disallow_sta_percent_macrs_5(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_itc_disallow_sta_percent_macrs_5_nget, self->data_ptr);
}

static PyObject *
Outputs_get_itc_disallow_sta_percent_sl_15(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_itc_disallow_sta_percent_sl_15_nget, self->data_ptr);
}

static PyObject *
Outputs_get_itc_disallow_sta_percent_sl_20(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_itc_disallow_sta_percent_sl_20_nget, self->data_ptr);
}

static PyObject *
Outputs_get_itc_disallow_sta_percent_sl_39(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_itc_disallow_sta_percent_sl_39_nget, self->data_ptr);
}

static PyObject *
Outputs_get_itc_disallow_sta_percent_sl_5(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_itc_disallow_sta_percent_sl_5_nget, self->data_ptr);
}

static PyObject *
Outputs_get_itc_disallow_sta_percent_total(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_itc_disallow_sta_percent_total_nget, self->data_ptr);
}

static PyObject *
Outputs_get_itc_fed_fixed_total(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_itc_fed_fixed_total_nget, self->data_ptr);
}

static PyObject *
Outputs_get_itc_fed_percent_total(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_itc_fed_percent_total_nget, self->data_ptr);
}

static PyObject *
Outputs_get_itc_fed_qual_custom(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_itc_fed_qual_custom_nget, self->data_ptr);
}

static PyObject *
Outputs_get_itc_fed_qual_macrs_15(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_itc_fed_qual_macrs_15_nget, self->data_ptr);
}

static PyObject *
Outputs_get_itc_fed_qual_macrs_5(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_itc_fed_qual_macrs_5_nget, self->data_ptr);
}

static PyObject *
Outputs_get_itc_fed_qual_sl_15(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_itc_fed_qual_sl_15_nget, self->data_ptr);
}

static PyObject *
Outputs_get_itc_fed_qual_sl_20(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_itc_fed_qual_sl_20_nget, self->data_ptr);
}

static PyObject *
Outputs_get_itc_fed_qual_sl_39(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_itc_fed_qual_sl_39_nget, self->data_ptr);
}

static PyObject *
Outputs_get_itc_fed_qual_sl_5(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_itc_fed_qual_sl_5_nget, self->data_ptr);
}

static PyObject *
Outputs_get_itc_fed_qual_total(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_itc_fed_qual_total_nget, self->data_ptr);
}

static PyObject *
Outputs_get_itc_sta_fixed_total(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_itc_sta_fixed_total_nget, self->data_ptr);
}

static PyObject *
Outputs_get_itc_sta_percent_total(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_itc_sta_percent_total_nget, self->data_ptr);
}

static PyObject *
Outputs_get_itc_sta_qual_custom(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_itc_sta_qual_custom_nget, self->data_ptr);
}

static PyObject *
Outputs_get_itc_sta_qual_macrs_15(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_itc_sta_qual_macrs_15_nget, self->data_ptr);
}

static PyObject *
Outputs_get_itc_sta_qual_macrs_5(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_itc_sta_qual_macrs_5_nget, self->data_ptr);
}

static PyObject *
Outputs_get_itc_sta_qual_sl_15(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_itc_sta_qual_sl_15_nget, self->data_ptr);
}

static PyObject *
Outputs_get_itc_sta_qual_sl_20(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_itc_sta_qual_sl_20_nget, self->data_ptr);
}

static PyObject *
Outputs_get_itc_sta_qual_sl_39(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_itc_sta_qual_sl_39_nget, self->data_ptr);
}

static PyObject *
Outputs_get_itc_sta_qual_sl_5(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_itc_sta_qual_sl_5_nget, self->data_ptr);
}

static PyObject *
Outputs_get_itc_sta_qual_total(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_itc_sta_qual_total_nget, self->data_ptr);
}

static PyObject *
Outputs_get_itc_total(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_itc_total_nget, self->data_ptr);
}

static PyObject *
Outputs_get_itc_total_fed(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_itc_total_fed_nget, self->data_ptr);
}

static PyObject *
Outputs_get_itc_total_sta(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_itc_total_sta_nget, self->data_ptr);
}

static PyObject *
Outputs_get_lcoe_nom(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_lcoe_nom_nget, self->data_ptr);
}

static PyObject *
Outputs_get_lcoe_real(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_lcoe_real_nget, self->data_ptr);
}

static PyObject *
Outputs_get_lcoptc_fed_nom(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_lcoptc_fed_nom_nget, self->data_ptr);
}

static PyObject *
Outputs_get_lcoptc_fed_real(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_lcoptc_fed_real_nget, self->data_ptr);
}

static PyObject *
Outputs_get_lcoptc_sta_nom(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_lcoptc_sta_nom_nget, self->data_ptr);
}

static PyObject *
Outputs_get_lcoptc_sta_real(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_lcoptc_sta_real_nget, self->data_ptr);
}

static PyObject *
Outputs_get_lppa_nom(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_lppa_nom_nget, self->data_ptr);
}

static PyObject *
Outputs_get_lppa_real(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_lppa_real_nget, self->data_ptr);
}

static PyObject *
Outputs_get_min_dscr(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_min_dscr_nget, self->data_ptr);
}

static PyObject *
Outputs_get_nominal_discount_rate(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_nominal_discount_rate_nget, self->data_ptr);
}

static PyObject *
Outputs_get_npv_annual_costs(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_npv_annual_costs_nget, self->data_ptr);
}

static PyObject *
Outputs_get_npv_energy_nom(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_npv_energy_nom_nget, self->data_ptr);
}

static PyObject *
Outputs_get_npv_energy_real(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_npv_energy_real_nget, self->data_ptr);
}

static PyObject *
Outputs_get_npv_ppa_revenue(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_npv_ppa_revenue_nget, self->data_ptr);
}

static PyObject *
Outputs_get_ppa(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_ppa_nget, self->data_ptr);
}

static PyObject *
Outputs_get_ppa_escalation(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_ppa_escalation_nget, self->data_ptr);
}

static PyObject *
Outputs_get_ppa_multipliers(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Levpartflip_Outputs_ppa_multipliers_aget, self->data_ptr);
}

static PyObject *
Outputs_get_ppa_price(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_ppa_price_nget, self->data_ptr);
}

static PyObject *
Outputs_get_present_value_fuel(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_present_value_fuel_nget, self->data_ptr);
}

static PyObject *
Outputs_get_present_value_insandproptax(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_present_value_insandproptax_nget, self->data_ptr);
}

static PyObject *
Outputs_get_present_value_oandm(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_present_value_oandm_nget, self->data_ptr);
}

static PyObject *
Outputs_get_present_value_oandm_nonfuel(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_present_value_oandm_nonfuel_nget, self->data_ptr);
}

static PyObject *
Outputs_get_prop_tax_assessed_value(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_prop_tax_assessed_value_nget, self->data_ptr);
}

static PyObject *
Outputs_get_purchase_of_property(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_purchase_of_property_nget, self->data_ptr);
}

static PyObject *
Outputs_get_pv_cafds(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_pv_cafds_nget, self->data_ptr);
}

static PyObject *
Outputs_get_salvage_value(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_salvage_value_nget, self->data_ptr);
}

static PyObject *
Outputs_get_size_of_debt(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_size_of_debt_nget, self->data_ptr);
}

static PyObject *
Outputs_get_size_of_equity(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_size_of_equity_nget, self->data_ptr);
}

static PyObject *
Outputs_get_sponsor_aftertax_development(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_sponsor_aftertax_development_nget, self->data_ptr);
}

static PyObject *
Outputs_get_sponsor_aftertax_equity(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_sponsor_aftertax_equity_nget, self->data_ptr);
}

static PyObject *
Outputs_get_sponsor_aftertax_irr(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_sponsor_aftertax_irr_nget, self->data_ptr);
}

static PyObject *
Outputs_get_sponsor_aftertax_npv(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_sponsor_aftertax_npv_nget, self->data_ptr);
}

static PyObject *
Outputs_get_sponsor_pretax_development(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_sponsor_pretax_development_nget, self->data_ptr);
}

static PyObject *
Outputs_get_sponsor_pretax_equity(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_sponsor_pretax_equity_nget, self->data_ptr);
}

static PyObject *
Outputs_get_sponsor_pretax_irr(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_sponsor_pretax_irr_nget, self->data_ptr);
}

static PyObject *
Outputs_get_sponsor_pretax_npv(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_sponsor_pretax_npv_nget, self->data_ptr);
}

static PyObject *
Outputs_get_tax_investor_aftertax_irr(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_tax_investor_aftertax_irr_nget, self->data_ptr);
}

static PyObject *
Outputs_get_tax_investor_aftertax_npv(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_tax_investor_aftertax_npv_nget, self->data_ptr);
}

static PyObject *
Outputs_get_tax_investor_pretax_irr(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_tax_investor_pretax_irr_nget, self->data_ptr);
}

static PyObject *
Outputs_get_tax_investor_pretax_npv(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_tax_investor_pretax_npv_nget, self->data_ptr);
}

static PyObject *
Outputs_get_wacc(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Levpartflip_Outputs_wacc_nget, self->data_ptr);
}

static PyGetSetDef Outputs_getset[] = {
{"adjusted_installed_cost", (getter)Outputs_get_adjusted_installed_cost,(setter)0,
	PyDoc_STR("*float*: Initial cost less cash incentives [$]"),
 	NULL},
{"analysis_period_irr", (getter)Outputs_get_analysis_period_irr,(setter)0,
	PyDoc_STR("*float*: Analysis period IRR [%]"),
 	NULL},
{"cash_for_debt_service", (getter)Outputs_get_cash_for_debt_service,(setter)0,
	PyDoc_STR("*float*: Cash available for debt service [$]"),
 	NULL},
{"cbi_fedtax_total", (getter)Outputs_get_cbi_fedtax_total,(setter)0,
	PyDoc_STR("*float*: Federal taxable CBI income [$]"),
 	NULL},
{"cbi_statax_total", (getter)Outputs_get_cbi_statax_total,(setter)0,
	PyDoc_STR("*float*: State taxable CBI income [$]"),
 	NULL},
{"cbi_total", (getter)Outputs_get_cbi_total,(setter)0,
	PyDoc_STR("*float*: Total CBI income [$]"),
 	NULL},
{"cbi_total_fed", (getter)Outputs_get_cbi_total_fed,(setter)0,
	PyDoc_STR("*float*: Federal CBI income [$]"),
 	NULL},
{"cbi_total_oth", (getter)Outputs_get_cbi_total_oth,(setter)0,
	PyDoc_STR("*float*: Other CBI income [$]"),
 	NULL},
{"cbi_total_sta", (getter)Outputs_get_cbi_total_sta,(setter)0,
	PyDoc_STR("*float*: State CBI income [$]"),
 	NULL},
{"cbi_total_uti", (getter)Outputs_get_cbi_total_uti,(setter)0,
	PyDoc_STR("*float*: Utility CBI income [$]"),
 	NULL},
{"cf_annual_costs", (getter)Outputs_get_cf_annual_costs,(setter)0,
	PyDoc_STR("*sequence*: Annual costs [$]"),
 	NULL},
{"cf_battery_replacement_cost", (getter)Outputs_get_cf_battery_replacement_cost,(setter)0,
	PyDoc_STR("*sequence*: Battery replacement cost [$]"),
 	NULL},
{"cf_battery_replacement_cost_schedule", (getter)Outputs_get_cf_battery_replacement_cost_schedule,(setter)0,
	PyDoc_STR("*sequence*: Battery replacement cost schedule [$/kWh]"),
 	NULL},
{"cf_cash_for_ds", (getter)Outputs_get_cf_cash_for_ds,(setter)0,
	PyDoc_STR("*sequence*: Cash available for debt service (CAFDS) [$]"),
 	NULL},
{"cf_debt_balance", (getter)Outputs_get_cf_debt_balance,(setter)0,
	PyDoc_STR("*sequence*: Debt balance [$]"),
 	NULL},
{"cf_debt_payment_interest", (getter)Outputs_get_cf_debt_payment_interest,(setter)0,
	PyDoc_STR("*sequence*: Debt interest payment [$]"),
 	NULL},
{"cf_debt_payment_principal", (getter)Outputs_get_cf_debt_payment_principal,(setter)0,
	PyDoc_STR("*sequence*: Debt principal payment [$]"),
 	NULL},
{"cf_debt_payment_total", (getter)Outputs_get_cf_debt_payment_total,(setter)0,
	PyDoc_STR("*sequence*: Debt total payment [$]"),
 	NULL},
{"cf_debt_size", (getter)Outputs_get_cf_debt_size,(setter)0,
	PyDoc_STR("*sequence*: Size of debt [$]"),
 	NULL},
{"cf_disbursement_debtservice", (getter)Outputs_get_cf_disbursement_debtservice,(setter)0,
	PyDoc_STR("*sequence*: Reserve disbursement debt service [$]"),
 	NULL},
{"cf_disbursement_equip1", (getter)Outputs_get_cf_disbursement_equip1,(setter)0,
	PyDoc_STR("*sequence*: Reserve disbursement major equipment 1 [$]"),
 	NULL},
{"cf_disbursement_equip2", (getter)Outputs_get_cf_disbursement_equip2,(setter)0,
	PyDoc_STR("*sequence*: Reserve disbursement major equipment 2 [$]"),
 	NULL},
{"cf_disbursement_equip3", (getter)Outputs_get_cf_disbursement_equip3,(setter)0,
	PyDoc_STR("*sequence*: Reserve disbursement major equipment 3 [$]"),
 	NULL},
{"cf_disbursement_om", (getter)Outputs_get_cf_disbursement_om,(setter)0,
	PyDoc_STR("*sequence*: Reserve disbursement working capital [$]"),
 	NULL},
{"cf_disbursement_receivables", (getter)Outputs_get_cf_disbursement_receivables,(setter)0,
	PyDoc_STR("*sequence*: Reserve disbursement receivables [$]"),
 	NULL},
{"cf_ebitda", (getter)Outputs_get_cf_ebitda,(setter)0,
	PyDoc_STR("*sequence*: EBITDA [$]"),
 	NULL},
{"cf_effective_tax_frac", (getter)Outputs_get_cf_effective_tax_frac,(setter)0,
	PyDoc_STR("*sequence*: Effective income tax rate [frac]"),
 	NULL},
{"cf_energy_net", (getter)Outputs_get_cf_energy_net,(setter)0,
	PyDoc_STR("*sequence*: Energy [kWh]"),
 	NULL},
{"cf_energy_net_apr", (getter)Outputs_get_cf_energy_net_apr,(setter)0,
	PyDoc_STR("*sequence*: Energy produced by the system in April [kWh]"),
 	NULL},
{"cf_energy_net_aug", (getter)Outputs_get_cf_energy_net_aug,(setter)0,
	PyDoc_STR("*sequence*: Energy produced by the system in August [kWh]"),
 	NULL},
{"cf_energy_net_dec", (getter)Outputs_get_cf_energy_net_dec,(setter)0,
	PyDoc_STR("*sequence*: Energy produced by the system in December [kWh]"),
 	NULL},
{"cf_energy_net_dispatch1", (getter)Outputs_get_cf_energy_net_dispatch1,(setter)0,
	PyDoc_STR("*sequence*: Energy produced by the system in TOD period 1 [kWh]"),
 	NULL},
{"cf_energy_net_dispatch2", (getter)Outputs_get_cf_energy_net_dispatch2,(setter)0,
	PyDoc_STR("*sequence*: Energy produced by the system in TOD period 2 [kWh]"),
 	NULL},
{"cf_energy_net_dispatch3", (getter)Outputs_get_cf_energy_net_dispatch3,(setter)0,
	PyDoc_STR("*sequence*: Energy produced by the system in TOD period 3 [kWh]"),
 	NULL},
{"cf_energy_net_dispatch4", (getter)Outputs_get_cf_energy_net_dispatch4,(setter)0,
	PyDoc_STR("*sequence*: Energy produced by the system in TOD period 4 [kWh]"),
 	NULL},
{"cf_energy_net_dispatch5", (getter)Outputs_get_cf_energy_net_dispatch5,(setter)0,
	PyDoc_STR("*sequence*: Energy produced by the system in TOD period 5 [kWh]"),
 	NULL},
{"cf_energy_net_dispatch6", (getter)Outputs_get_cf_energy_net_dispatch6,(setter)0,
	PyDoc_STR("*sequence*: Energy produced by the system in TOD period 6 [kWh]"),
 	NULL},
{"cf_energy_net_dispatch7", (getter)Outputs_get_cf_energy_net_dispatch7,(setter)0,
	PyDoc_STR("*sequence*: Energy produced by the system in TOD period 7 [kWh]"),
 	NULL},
{"cf_energy_net_dispatch8", (getter)Outputs_get_cf_energy_net_dispatch8,(setter)0,
	PyDoc_STR("*sequence*: Energy produced by the system in TOD period 8 [kWh]"),
 	NULL},
{"cf_energy_net_dispatch9", (getter)Outputs_get_cf_energy_net_dispatch9,(setter)0,
	PyDoc_STR("*sequence*: Energy produced by the system in TOD period 9 [kWh]"),
 	NULL},
{"cf_energy_net_feb", (getter)Outputs_get_cf_energy_net_feb,(setter)0,
	PyDoc_STR("*sequence*: Energy produced by the system in February [kWh]"),
 	NULL},
{"cf_energy_net_jan", (getter)Outputs_get_cf_energy_net_jan,(setter)0,
	PyDoc_STR("*sequence*: Energy produced by the system in January [kWh]"),
 	NULL},
{"cf_energy_net_jul", (getter)Outputs_get_cf_energy_net_jul,(setter)0,
	PyDoc_STR("*sequence*: Energy produced by the system in July [kWh]"),
 	NULL},
{"cf_energy_net_jun", (getter)Outputs_get_cf_energy_net_jun,(setter)0,
	PyDoc_STR("*sequence*: Energy produced by the system in June [kWh]"),
 	NULL},
{"cf_energy_net_mar", (getter)Outputs_get_cf_energy_net_mar,(setter)0,
	PyDoc_STR("*sequence*: Energy produced by the system in March [kWh]"),
 	NULL},
{"cf_energy_net_may", (getter)Outputs_get_cf_energy_net_may,(setter)0,
	PyDoc_STR("*sequence*: Energy produced by the system in May [kWh]"),
 	NULL},
{"cf_energy_net_monthly_firstyear_TOD1", (getter)Outputs_get_cf_energy_net_monthly_firstyear_TOD1,(setter)0,
	PyDoc_STR("*sequence*: First year energy from the system by month for TOD period 1 [kWh]"),
 	NULL},
{"cf_energy_net_monthly_firstyear_TOD2", (getter)Outputs_get_cf_energy_net_monthly_firstyear_TOD2,(setter)0,
	PyDoc_STR("*sequence*: First year energy from the system by month for TOD period 2 [kWh]"),
 	NULL},
{"cf_energy_net_monthly_firstyear_TOD3", (getter)Outputs_get_cf_energy_net_monthly_firstyear_TOD3,(setter)0,
	PyDoc_STR("*sequence*: First year energy from the system by month for TOD period 3 [kWh]"),
 	NULL},
{"cf_energy_net_monthly_firstyear_TOD4", (getter)Outputs_get_cf_energy_net_monthly_firstyear_TOD4,(setter)0,
	PyDoc_STR("*sequence*: First year energy from the system by month for TOD period 4 [kWh]"),
 	NULL},
{"cf_energy_net_monthly_firstyear_TOD5", (getter)Outputs_get_cf_energy_net_monthly_firstyear_TOD5,(setter)0,
	PyDoc_STR("*sequence*: First year energy from the system by month for TOD period 5 [kWh]"),
 	NULL},
{"cf_energy_net_monthly_firstyear_TOD6", (getter)Outputs_get_cf_energy_net_monthly_firstyear_TOD6,(setter)0,
	PyDoc_STR("*sequence*: First year energy from the system by month for TOD period 6 [kWh]"),
 	NULL},
{"cf_energy_net_monthly_firstyear_TOD7", (getter)Outputs_get_cf_energy_net_monthly_firstyear_TOD7,(setter)0,
	PyDoc_STR("*sequence*: First year energy from the system by month for TOD period 7 [kWh]"),
 	NULL},
{"cf_energy_net_monthly_firstyear_TOD8", (getter)Outputs_get_cf_energy_net_monthly_firstyear_TOD8,(setter)0,
	PyDoc_STR("*sequence*: First year energy from the system by month for TOD period 8 [kWh]"),
 	NULL},
{"cf_energy_net_monthly_firstyear_TOD9", (getter)Outputs_get_cf_energy_net_monthly_firstyear_TOD9,(setter)0,
	PyDoc_STR("*sequence*: First year energy from the system by month for TOD period 9 [kWh]"),
 	NULL},
{"cf_energy_net_nov", (getter)Outputs_get_cf_energy_net_nov,(setter)0,
	PyDoc_STR("*sequence*: Energy produced by the system in November [kWh]"),
 	NULL},
{"cf_energy_net_oct", (getter)Outputs_get_cf_energy_net_oct,(setter)0,
	PyDoc_STR("*sequence*: Energy produced by the system in October [kWh]"),
 	NULL},
{"cf_energy_net_sep", (getter)Outputs_get_cf_energy_net_sep,(setter)0,
	PyDoc_STR("*sequence*: Energy produced by the system in September [kWh]"),
 	NULL},
{"cf_energy_value", (getter)Outputs_get_cf_energy_value,(setter)0,
	PyDoc_STR("*sequence*: PPA revenue to project [$]"),
 	NULL},
{"cf_feddepr_custom", (getter)Outputs_get_cf_feddepr_custom,(setter)0,
	PyDoc_STR("*sequence*: Federal depreciation from custom [$]"),
 	NULL},
{"cf_feddepr_macrs_15", (getter)Outputs_get_cf_feddepr_macrs_15,(setter)0,
	PyDoc_STR("*sequence*: Federal depreciation from 15-yr MACRS [$]"),
 	NULL},
{"cf_feddepr_macrs_5", (getter)Outputs_get_cf_feddepr_macrs_5,(setter)0,
	PyDoc_STR("*sequence*: Federal depreciation from 5-yr MACRS [$]"),
 	NULL},
{"cf_feddepr_me1", (getter)Outputs_get_cf_feddepr_me1,(setter)0,
	PyDoc_STR("*sequence*: Federal depreciation from major equipment 1 [$]"),
 	NULL},
{"cf_feddepr_me2", (getter)Outputs_get_cf_feddepr_me2,(setter)0,
	PyDoc_STR("*sequence*: Federal depreciation from major equipment 2 [$]"),
 	NULL},
{"cf_feddepr_me3", (getter)Outputs_get_cf_feddepr_me3,(setter)0,
	PyDoc_STR("*sequence*: Federal depreciation from major equipment 3 [$]"),
 	NULL},
{"cf_feddepr_sl_15", (getter)Outputs_get_cf_feddepr_sl_15,(setter)0,
	PyDoc_STR("*sequence*: Federal depreciation from 15-yr straight line [$]"),
 	NULL},
{"cf_feddepr_sl_20", (getter)Outputs_get_cf_feddepr_sl_20,(setter)0,
	PyDoc_STR("*sequence*: Federal depreciation from 20-yr straight line [$]"),
 	NULL},
{"cf_feddepr_sl_39", (getter)Outputs_get_cf_feddepr_sl_39,(setter)0,
	PyDoc_STR("*sequence*: Federal depreciation from 39-yr straight line [$]"),
 	NULL},
{"cf_feddepr_sl_5", (getter)Outputs_get_cf_feddepr_sl_5,(setter)0,
	PyDoc_STR("*sequence*: Federal depreciation from 5-yr straight line [$]"),
 	NULL},
{"cf_feddepr_total", (getter)Outputs_get_cf_feddepr_total,(setter)0,
	PyDoc_STR("*sequence*: Total federal tax depreciation [$]"),
 	NULL},
{"cf_federal_tax_frac", (getter)Outputs_get_cf_federal_tax_frac,(setter)0,
	PyDoc_STR("*sequence*: Federal income tax rate [frac]"),
 	NULL},
{"cf_fedtax", (getter)Outputs_get_cf_fedtax,(setter)0,
	PyDoc_STR("*sequence*: Federal tax benefit/(liability) [$]"),
 	NULL},
{"cf_fedtax_income_prior_incentives", (getter)Outputs_get_cf_fedtax_income_prior_incentives,(setter)0,
	PyDoc_STR("*sequence*: Federal taxable income without incentives [$]"),
 	NULL},
{"cf_fedtax_income_with_incentives", (getter)Outputs_get_cf_fedtax_income_with_incentives,(setter)0,
	PyDoc_STR("*sequence*: Federal taxable income [$]"),
 	NULL},
{"cf_fedtax_taxable_incentives", (getter)Outputs_get_cf_fedtax_taxable_incentives,(setter)0,
	PyDoc_STR("*sequence*: Federal taxable incentives [$]"),
 	NULL},
{"cf_funding_debtservice", (getter)Outputs_get_cf_funding_debtservice,(setter)0,
	PyDoc_STR("*sequence*: Reserve funding debt service [$]"),
 	NULL},
{"cf_funding_equip1", (getter)Outputs_get_cf_funding_equip1,(setter)0,
	PyDoc_STR("*sequence*: Reserve funding major equipment 1 [$]"),
 	NULL},
{"cf_funding_equip2", (getter)Outputs_get_cf_funding_equip2,(setter)0,
	PyDoc_STR("*sequence*: Reserve funding major equipment 2 [$]"),
 	NULL},
{"cf_funding_equip3", (getter)Outputs_get_cf_funding_equip3,(setter)0,
	PyDoc_STR("*sequence*: Reserve funding major equipment 3 [$]"),
 	NULL},
{"cf_funding_om", (getter)Outputs_get_cf_funding_om,(setter)0,
	PyDoc_STR("*sequence*: Reserve funding working capital [$]"),
 	NULL},
{"cf_funding_receivables", (getter)Outputs_get_cf_funding_receivables,(setter)0,
	PyDoc_STR("*sequence*: Reserve funding receivables [$]"),
 	NULL},
{"cf_insurance_expense", (getter)Outputs_get_cf_insurance_expense,(setter)0,
	PyDoc_STR("*sequence*: Insurance expense [$]"),
 	NULL},
{"cf_length", (getter)Outputs_get_cf_length,(setter)0,
	PyDoc_STR("*float*: Number of periods in cashflow"),
 	NULL},
{"cf_net_salvage_value", (getter)Outputs_get_cf_net_salvage_value,(setter)0,
	PyDoc_STR("*sequence*: Salvage value [$]"),
 	NULL},
{"cf_om_capacity_expense", (getter)Outputs_get_cf_om_capacity_expense,(setter)0,
	PyDoc_STR("*sequence*: O&M capacity-based expense [$]"),
 	NULL},
{"cf_om_fixed_expense", (getter)Outputs_get_cf_om_fixed_expense,(setter)0,
	PyDoc_STR("*sequence*: O&M fixed expense [$]"),
 	NULL},
{"cf_om_fuel_expense", (getter)Outputs_get_cf_om_fuel_expense,(setter)0,
	PyDoc_STR("*sequence*: O&M fuel expense [$]"),
 	NULL},
{"cf_om_opt_fuel_1_expense", (getter)Outputs_get_cf_om_opt_fuel_1_expense,(setter)0,
	PyDoc_STR("*sequence*: O&M biomass feedstock expense [$]"),
 	NULL},
{"cf_om_opt_fuel_2_expense", (getter)Outputs_get_cf_om_opt_fuel_2_expense,(setter)0,
	PyDoc_STR("*sequence*: O&M coal feedstock expense [$]"),
 	NULL},
{"cf_om_production_expense", (getter)Outputs_get_cf_om_production_expense,(setter)0,
	PyDoc_STR("*sequence*: O&M production-based expense [$]"),
 	NULL},
{"cf_operating_expenses", (getter)Outputs_get_cf_operating_expenses,(setter)0,
	PyDoc_STR("*sequence*: Total operating expenses [$]"),
 	NULL},
{"cf_pbi_fedtax_total", (getter)Outputs_get_cf_pbi_fedtax_total,(setter)0,
	PyDoc_STR("*sequence*: Federal taxable PBI income [$]"),
 	NULL},
{"cf_pbi_statax_total", (getter)Outputs_get_cf_pbi_statax_total,(setter)0,
	PyDoc_STR("*sequence*: State taxable PBI income [$]"),
 	NULL},
{"cf_pbi_total", (getter)Outputs_get_cf_pbi_total,(setter)0,
	PyDoc_STR("*sequence*: Total PBI income [$]"),
 	NULL},
{"cf_pbi_total_fed", (getter)Outputs_get_cf_pbi_total_fed,(setter)0,
	PyDoc_STR("*sequence*: Federal PBI income [$]"),
 	NULL},
{"cf_pbi_total_oth", (getter)Outputs_get_cf_pbi_total_oth,(setter)0,
	PyDoc_STR("*sequence*: Other PBI income [$]"),
 	NULL},
{"cf_pbi_total_sta", (getter)Outputs_get_cf_pbi_total_sta,(setter)0,
	PyDoc_STR("*sequence*: State PBI income [$]"),
 	NULL},
{"cf_pbi_total_uti", (getter)Outputs_get_cf_pbi_total_uti,(setter)0,
	PyDoc_STR("*sequence*: Utility PBI income [$]"),
 	NULL},
{"cf_ppa_price", (getter)Outputs_get_cf_ppa_price,(setter)0,
	PyDoc_STR("*sequence*: PPA price [cents/kWh]"),
 	NULL},
{"cf_pretax_cashflow", (getter)Outputs_get_cf_pretax_cashflow,(setter)0,
	PyDoc_STR("*sequence*: Pre-tax project cash flow [$]"),
 	NULL},
{"cf_pretax_dscr", (getter)Outputs_get_cf_pretax_dscr,(setter)0,
	PyDoc_STR("*sequence*: Pre-tax DSCR"),
 	NULL},
{"cf_project_dsra", (getter)Outputs_get_cf_project_dsra,(setter)0,
	PyDoc_STR("*sequence*: Reserve (increase)/decrease debt service [$]"),
 	NULL},
{"cf_project_financing_activities", (getter)Outputs_get_cf_project_financing_activities,(setter)0,
	PyDoc_STR("*sequence*: Project cash flow from financing activities [$]"),
 	NULL},
{"cf_project_investing_activities", (getter)Outputs_get_cf_project_investing_activities,(setter)0,
	PyDoc_STR("*sequence*: Project cash flow from investing activities [$]"),
 	NULL},
{"cf_project_me1cs", (getter)Outputs_get_cf_project_me1cs,(setter)0,
	PyDoc_STR("*sequence*: Reserve capital spending major equipment 1 [$]"),
 	NULL},
{"cf_project_me1ra", (getter)Outputs_get_cf_project_me1ra,(setter)0,
	PyDoc_STR("*sequence*: Reserve (increase)/decrease major equipment 1 [$]"),
 	NULL},
{"cf_project_me2cs", (getter)Outputs_get_cf_project_me2cs,(setter)0,
	PyDoc_STR("*sequence*: Reserve capital spending major equipment 2 [$]"),
 	NULL},
{"cf_project_me2ra", (getter)Outputs_get_cf_project_me2ra,(setter)0,
	PyDoc_STR("*sequence*: Reserve (increase)/decrease major equipment 2 [$]"),
 	NULL},
{"cf_project_me3cs", (getter)Outputs_get_cf_project_me3cs,(setter)0,
	PyDoc_STR("*sequence*: Reserve capital spending major equipment 3 [$]"),
 	NULL},
{"cf_project_me3ra", (getter)Outputs_get_cf_project_me3ra,(setter)0,
	PyDoc_STR("*sequence*: Reserve (increase)/decrease major equipment 3 [$]"),
 	NULL},
{"cf_project_mecs", (getter)Outputs_get_cf_project_mecs,(setter)0,
	PyDoc_STR("*sequence*: Reserve capital spending major equipment total [$]"),
 	NULL},
{"cf_project_operating_activities", (getter)Outputs_get_cf_project_operating_activities,(setter)0,
	PyDoc_STR("*sequence*: Project cash flow from operating activities [$]"),
 	NULL},
{"cf_project_ra", (getter)Outputs_get_cf_project_ra,(setter)0,
	PyDoc_STR("*sequence*: Reserve (increase)/decrease total reserve account [$]"),
 	NULL},
{"cf_project_receivablesra", (getter)Outputs_get_cf_project_receivablesra,(setter)0,
	PyDoc_STR("*sequence*: Reserve (increase)/decrease receivables [$]"),
 	NULL},
{"cf_project_return_aftertax", (getter)Outputs_get_cf_project_return_aftertax,(setter)0,
	PyDoc_STR("*sequence*: Project after-tax returns [$]"),
 	NULL},
{"cf_project_return_aftertax_cash", (getter)Outputs_get_cf_project_return_aftertax_cash,(setter)0,
	PyDoc_STR("*sequence*: Project after-tax operating cash [$]"),
 	NULL},
{"cf_project_return_aftertax_irr", (getter)Outputs_get_cf_project_return_aftertax_irr,(setter)0,
	PyDoc_STR("*sequence*: Project after-tax cumulative IRR [%]"),
 	NULL},
{"cf_project_return_aftertax_npv", (getter)Outputs_get_cf_project_return_aftertax_npv,(setter)0,
	PyDoc_STR("*sequence*: Project after-tax cumulative NPV [$]"),
 	NULL},
{"cf_project_return_pretax", (getter)Outputs_get_cf_project_return_pretax,(setter)0,
	PyDoc_STR("*sequence*: Project pre-tax returns [$]"),
 	NULL},
{"cf_project_return_pretax_irr", (getter)Outputs_get_cf_project_return_pretax_irr,(setter)0,
	PyDoc_STR("*sequence*: Project pre-tax cumulative IRR [%]"),
 	NULL},
{"cf_project_return_pretax_npv", (getter)Outputs_get_cf_project_return_pretax_npv,(setter)0,
	PyDoc_STR("*sequence*: Project pre-tax cumulative NPV [$]"),
 	NULL},
{"cf_project_wcra", (getter)Outputs_get_cf_project_wcra,(setter)0,
	PyDoc_STR("*sequence*: Reserve (increase)/decrease working capital [$]"),
 	NULL},
{"cf_property_tax_assessed_value", (getter)Outputs_get_cf_property_tax_assessed_value,(setter)0,
	PyDoc_STR("*sequence*: Property tax net assessed value [$]"),
 	NULL},
{"cf_property_tax_expense", (getter)Outputs_get_cf_property_tax_expense,(setter)0,
	PyDoc_STR("*sequence*: Property tax expense [$]"),
 	NULL},
{"cf_ptc_fed", (getter)Outputs_get_cf_ptc_fed,(setter)0,
	PyDoc_STR("*sequence*: Federal PTC income [$]"),
 	NULL},
{"cf_ptc_sta", (getter)Outputs_get_cf_ptc_sta,(setter)0,
	PyDoc_STR("*sequence*: State PTC income [$]"),
 	NULL},
{"cf_pv_cash_for_ds", (getter)Outputs_get_cf_pv_cash_for_ds,(setter)0,
	PyDoc_STR("*sequence*: Present value of CAFDS [$]"),
 	NULL},
{"cf_pv_interest_factor", (getter)Outputs_get_cf_pv_interest_factor,(setter)0,
	PyDoc_STR("*sequence*: Present value interest factor for CAFDS"),
 	NULL},
{"cf_recapitalization", (getter)Outputs_get_cf_recapitalization,(setter)0,
	PyDoc_STR("*sequence*: Recapitalization operating expense [$]"),
 	NULL},
{"cf_reserve_debtservice", (getter)Outputs_get_cf_reserve_debtservice,(setter)0,
	PyDoc_STR("*sequence*: Reserve account debt service [$]"),
 	NULL},
{"cf_reserve_equip1", (getter)Outputs_get_cf_reserve_equip1,(setter)0,
	PyDoc_STR("*sequence*: Reserve account major equipment 1 [$]"),
 	NULL},
{"cf_reserve_equip2", (getter)Outputs_get_cf_reserve_equip2,(setter)0,
	PyDoc_STR("*sequence*: Reserve account major equipment 2 [$]"),
 	NULL},
{"cf_reserve_equip3", (getter)Outputs_get_cf_reserve_equip3,(setter)0,
	PyDoc_STR("*sequence*: Reserve account major equipment 3 [$]"),
 	NULL},
{"cf_reserve_interest", (getter)Outputs_get_cf_reserve_interest,(setter)0,
	PyDoc_STR("*sequence*: Interest earned on reserve accounts [$]"),
 	NULL},
{"cf_reserve_om", (getter)Outputs_get_cf_reserve_om,(setter)0,
	PyDoc_STR("*sequence*: Reserve account working capital [$]"),
 	NULL},
{"cf_reserve_receivables", (getter)Outputs_get_cf_reserve_receivables,(setter)0,
	PyDoc_STR("*sequence*: Reserve account receivables [$]"),
 	NULL},
{"cf_reserve_total", (getter)Outputs_get_cf_reserve_total,(setter)0,
	PyDoc_STR("*sequence*: Reserve account total reserves [$]"),
 	NULL},
{"cf_revenue_apr", (getter)Outputs_get_cf_revenue_apr,(setter)0,
	PyDoc_STR("*sequence*: Revenue from the system in April [$]"),
 	NULL},
{"cf_revenue_aug", (getter)Outputs_get_cf_revenue_aug,(setter)0,
	PyDoc_STR("*sequence*: Revenue from the system in August [$]"),
 	NULL},
{"cf_revenue_dec", (getter)Outputs_get_cf_revenue_dec,(setter)0,
	PyDoc_STR("*sequence*: Revenue from the system in December [$]"),
 	NULL},
{"cf_revenue_dispatch1", (getter)Outputs_get_cf_revenue_dispatch1,(setter)0,
	PyDoc_STR("*sequence*: Revenue from the system in TOD period 1 [$]"),
 	NULL},
{"cf_revenue_dispatch2", (getter)Outputs_get_cf_revenue_dispatch2,(setter)0,
	PyDoc_STR("*sequence*: Revenue from the system in TOD period 2 [$]"),
 	NULL},
{"cf_revenue_dispatch3", (getter)Outputs_get_cf_revenue_dispatch3,(setter)0,
	PyDoc_STR("*sequence*: Revenue from the system in TOD period 3 [$]"),
 	NULL},
{"cf_revenue_dispatch4", (getter)Outputs_get_cf_revenue_dispatch4,(setter)0,
	PyDoc_STR("*sequence*: Revenue from the system in TOD period 4 [$]"),
 	NULL},
{"cf_revenue_dispatch5", (getter)Outputs_get_cf_revenue_dispatch5,(setter)0,
	PyDoc_STR("*sequence*: Revenue from the system in TOD period 5 [$]"),
 	NULL},
{"cf_revenue_dispatch6", (getter)Outputs_get_cf_revenue_dispatch6,(setter)0,
	PyDoc_STR("*sequence*: Revenue from the system in TOD period 6 [$]"),
 	NULL},
{"cf_revenue_dispatch7", (getter)Outputs_get_cf_revenue_dispatch7,(setter)0,
	PyDoc_STR("*sequence*: Revenue from the system in TOD period 7 [$]"),
 	NULL},
{"cf_revenue_dispatch8", (getter)Outputs_get_cf_revenue_dispatch8,(setter)0,
	PyDoc_STR("*sequence*: Revenue from the system in TOD period 8 [$]"),
 	NULL},
{"cf_revenue_dispatch9", (getter)Outputs_get_cf_revenue_dispatch9,(setter)0,
	PyDoc_STR("*sequence*: Revenue from the system in TOD period 9 [$]"),
 	NULL},
{"cf_revenue_feb", (getter)Outputs_get_cf_revenue_feb,(setter)0,
	PyDoc_STR("*sequence*: Revenue from the system in February [$]"),
 	NULL},
{"cf_revenue_jan", (getter)Outputs_get_cf_revenue_jan,(setter)0,
	PyDoc_STR("*sequence*: Revenue from the system in January [$]"),
 	NULL},
{"cf_revenue_jul", (getter)Outputs_get_cf_revenue_jul,(setter)0,
	PyDoc_STR("*sequence*: Revenue from the system in July [$]"),
 	NULL},
{"cf_revenue_jun", (getter)Outputs_get_cf_revenue_jun,(setter)0,
	PyDoc_STR("*sequence*: Revenue from the system in June [$]"),
 	NULL},
{"cf_revenue_mar", (getter)Outputs_get_cf_revenue_mar,(setter)0,
	PyDoc_STR("*sequence*: Revenue from the system in March [$]"),
 	NULL},
{"cf_revenue_may", (getter)Outputs_get_cf_revenue_may,(setter)0,
	PyDoc_STR("*sequence*: Revenue from the system in May [$]"),
 	NULL},
{"cf_revenue_monthly_firstyear_TOD1", (getter)Outputs_get_cf_revenue_monthly_firstyear_TOD1,(setter)0,
	PyDoc_STR("*sequence*: First year revenue from the system by month for TOD period 1 [$]"),
 	NULL},
{"cf_revenue_monthly_firstyear_TOD2", (getter)Outputs_get_cf_revenue_monthly_firstyear_TOD2,(setter)0,
	PyDoc_STR("*sequence*: First year revenue from the system by month for TOD period 2 [$]"),
 	NULL},
{"cf_revenue_monthly_firstyear_TOD3", (getter)Outputs_get_cf_revenue_monthly_firstyear_TOD3,(setter)0,
	PyDoc_STR("*sequence*: First year revenue from the system by month for TOD period 3 [$]"),
 	NULL},
{"cf_revenue_monthly_firstyear_TOD4", (getter)Outputs_get_cf_revenue_monthly_firstyear_TOD4,(setter)0,
	PyDoc_STR("*sequence*: First year revenue from the system by month for TOD period 4 [$]"),
 	NULL},
{"cf_revenue_monthly_firstyear_TOD5", (getter)Outputs_get_cf_revenue_monthly_firstyear_TOD5,(setter)0,
	PyDoc_STR("*sequence*: First year revenue from the system by month for TOD period 5 [$]"),
 	NULL},
{"cf_revenue_monthly_firstyear_TOD6", (getter)Outputs_get_cf_revenue_monthly_firstyear_TOD6,(setter)0,
	PyDoc_STR("*sequence*: First year revenue from the system by month for TOD period 6 [$]"),
 	NULL},
{"cf_revenue_monthly_firstyear_TOD7", (getter)Outputs_get_cf_revenue_monthly_firstyear_TOD7,(setter)0,
	PyDoc_STR("*sequence*: First year revenue from the system by month for TOD period 7 [$]"),
 	NULL},
{"cf_revenue_monthly_firstyear_TOD8", (getter)Outputs_get_cf_revenue_monthly_firstyear_TOD8,(setter)0,
	PyDoc_STR("*sequence*: First year revenue from the system by month for TOD period 8 [$]"),
 	NULL},
{"cf_revenue_monthly_firstyear_TOD9", (getter)Outputs_get_cf_revenue_monthly_firstyear_TOD9,(setter)0,
	PyDoc_STR("*sequence*: First year revenue from the system by month for TOD period 9 [$]"),
 	NULL},
{"cf_revenue_nov", (getter)Outputs_get_cf_revenue_nov,(setter)0,
	PyDoc_STR("*sequence*: Revenue from the system in November [$]"),
 	NULL},
{"cf_revenue_oct", (getter)Outputs_get_cf_revenue_oct,(setter)0,
	PyDoc_STR("*sequence*: Revenue from the system in October [$]"),
 	NULL},
{"cf_revenue_sep", (getter)Outputs_get_cf_revenue_sep,(setter)0,
	PyDoc_STR("*sequence*: Revenue from the system in September [$]"),
 	NULL},
{"cf_sponsor_aftertax", (getter)Outputs_get_cf_sponsor_aftertax,(setter)0,
	PyDoc_STR("*sequence*: Developer after-tax total [$]"),
 	NULL},
{"cf_sponsor_aftertax_cash", (getter)Outputs_get_cf_sponsor_aftertax_cash,(setter)0,
	PyDoc_STR("*sequence*: Developer after-tax cash returns [$]"),
 	NULL},
{"cf_sponsor_aftertax_irr", (getter)Outputs_get_cf_sponsor_aftertax_irr,(setter)0,
	PyDoc_STR("*sequence*: Developer after-tax cumulative IRR [%]"),
 	NULL},
{"cf_sponsor_aftertax_itc", (getter)Outputs_get_cf_sponsor_aftertax_itc,(setter)0,
	PyDoc_STR("*sequence*: Developer share of ITC [$]"),
 	NULL},
{"cf_sponsor_aftertax_npv", (getter)Outputs_get_cf_sponsor_aftertax_npv,(setter)0,
	PyDoc_STR("*sequence*: Developer after-tax cumulative NPV [$]"),
 	NULL},
{"cf_sponsor_aftertax_ptc", (getter)Outputs_get_cf_sponsor_aftertax_ptc,(setter)0,
	PyDoc_STR("*sequence*: Developer share of PTC [$]"),
 	NULL},
{"cf_sponsor_aftertax_tax", (getter)Outputs_get_cf_sponsor_aftertax_tax,(setter)0,
	PyDoc_STR("*sequence*: Developer share of tax benefit/(liability) [$]"),
 	NULL},
{"cf_sponsor_pretax", (getter)Outputs_get_cf_sponsor_pretax,(setter)0,
	PyDoc_STR("*sequence*: Developer pre-tax returns [$]"),
 	NULL},
{"cf_sponsor_pretax_irr", (getter)Outputs_get_cf_sponsor_pretax_irr,(setter)0,
	PyDoc_STR("*sequence*: Developer pre-tax cumulative IRR [%]"),
 	NULL},
{"cf_sponsor_pretax_npv", (getter)Outputs_get_cf_sponsor_pretax_npv,(setter)0,
	PyDoc_STR("*sequence*: Developer pre-tax cumulative NPV [$]"),
 	NULL},
{"cf_stadepr_custom", (getter)Outputs_get_cf_stadepr_custom,(setter)0,
	PyDoc_STR("*sequence*: State depreciation from custom [$]"),
 	NULL},
{"cf_stadepr_macrs_15", (getter)Outputs_get_cf_stadepr_macrs_15,(setter)0,
	PyDoc_STR("*sequence*: State depreciation from 15-yr MACRS [$]"),
 	NULL},
{"cf_stadepr_macrs_5", (getter)Outputs_get_cf_stadepr_macrs_5,(setter)0,
	PyDoc_STR("*sequence*: State depreciation from 5-yr MACRS [$]"),
 	NULL},
{"cf_stadepr_me1", (getter)Outputs_get_cf_stadepr_me1,(setter)0,
	PyDoc_STR("*sequence*: State depreciation from major equipment 1 [$]"),
 	NULL},
{"cf_stadepr_me2", (getter)Outputs_get_cf_stadepr_me2,(setter)0,
	PyDoc_STR("*sequence*: State depreciation from major equipment 2 [$]"),
 	NULL},
{"cf_stadepr_me3", (getter)Outputs_get_cf_stadepr_me3,(setter)0,
	PyDoc_STR("*sequence*: State depreciation from major equipment 3 [$]"),
 	NULL},
{"cf_stadepr_sl_15", (getter)Outputs_get_cf_stadepr_sl_15,(setter)0,
	PyDoc_STR("*sequence*: State depreciation from 15-yr straight line [$]"),
 	NULL},
{"cf_stadepr_sl_20", (getter)Outputs_get_cf_stadepr_sl_20,(setter)0,
	PyDoc_STR("*sequence*: State depreciation from 20-yr straight line [$]"),
 	NULL},
{"cf_stadepr_sl_39", (getter)Outputs_get_cf_stadepr_sl_39,(setter)0,
	PyDoc_STR("*sequence*: State depreciation from 39-yr straight line [$]"),
 	NULL},
{"cf_stadepr_sl_5", (getter)Outputs_get_cf_stadepr_sl_5,(setter)0,
	PyDoc_STR("*sequence*: State depreciation from 5-yr straight line [$]"),
 	NULL},
{"cf_stadepr_total", (getter)Outputs_get_cf_stadepr_total,(setter)0,
	PyDoc_STR("*sequence*: Total state tax depreciation [$]"),
 	NULL},
{"cf_statax", (getter)Outputs_get_cf_statax,(setter)0,
	PyDoc_STR("*sequence*: State tax benefit/(liability) [$]"),
 	NULL},
{"cf_statax_income_prior_incentives", (getter)Outputs_get_cf_statax_income_prior_incentives,(setter)0,
	PyDoc_STR("*sequence*: State taxable income without incentives [$]"),
 	NULL},
{"cf_statax_income_with_incentives", (getter)Outputs_get_cf_statax_income_with_incentives,(setter)0,
	PyDoc_STR("*sequence*: State taxable income [$]"),
 	NULL},
{"cf_statax_taxable_incentives", (getter)Outputs_get_cf_statax_taxable_incentives,(setter)0,
	PyDoc_STR("*sequence*: State taxable incentives [$]"),
 	NULL},
{"cf_state_tax_frac", (getter)Outputs_get_cf_state_tax_frac,(setter)0,
	PyDoc_STR("*sequence*: State income tax rate [frac]"),
 	NULL},
{"cf_tax_investor_aftertax", (getter)Outputs_get_cf_tax_investor_aftertax,(setter)0,
	PyDoc_STR("*sequence*: Tax investor after-tax returns [$]"),
 	NULL},
{"cf_tax_investor_aftertax_cash", (getter)Outputs_get_cf_tax_investor_aftertax_cash,(setter)0,
	PyDoc_STR("*sequence*: Tax investor after-tax operating cash [$]"),
 	NULL},
{"cf_tax_investor_aftertax_irr", (getter)Outputs_get_cf_tax_investor_aftertax_irr,(setter)0,
	PyDoc_STR("*sequence*: Tax investor after-tax cumulative IRR [%]"),
 	NULL},
{"cf_tax_investor_aftertax_itc", (getter)Outputs_get_cf_tax_investor_aftertax_itc,(setter)0,
	PyDoc_STR("*sequence*: Tax investor share of ITC [$]"),
 	NULL},
{"cf_tax_investor_aftertax_max_irr", (getter)Outputs_get_cf_tax_investor_aftertax_max_irr,(setter)0,
	PyDoc_STR("*sequence*: Tax investor after-tax maximum IRR [%]"),
 	NULL},
{"cf_tax_investor_aftertax_npv", (getter)Outputs_get_cf_tax_investor_aftertax_npv,(setter)0,
	PyDoc_STR("*sequence*: Tax investor after-tax cumulative NPV [$]"),
 	NULL},
{"cf_tax_investor_aftertax_ptc", (getter)Outputs_get_cf_tax_investor_aftertax_ptc,(setter)0,
	PyDoc_STR("*sequence*: Tax investor share of PTC [$]"),
 	NULL},
{"cf_tax_investor_aftertax_tax", (getter)Outputs_get_cf_tax_investor_aftertax_tax,(setter)0,
	PyDoc_STR("*sequence*: Tax investor share of tax benefit/(liability) [$]"),
 	NULL},
{"cf_tax_investor_pretax", (getter)Outputs_get_cf_tax_investor_pretax,(setter)0,
	PyDoc_STR("*sequence*: Tax investor pre-tax returns [$]"),
 	NULL},
{"cf_tax_investor_pretax_irr", (getter)Outputs_get_cf_tax_investor_pretax_irr,(setter)0,
	PyDoc_STR("*sequence*: Tax investor pre-tax cumulative IRR [%]"),
 	NULL},
{"cf_tax_investor_pretax_npv", (getter)Outputs_get_cf_tax_investor_pretax_npv,(setter)0,
	PyDoc_STR("*sequence*: Tax investor pre-tax cumulative NPV [$]"),
 	NULL},
{"cf_total_revenue", (getter)Outputs_get_cf_total_revenue,(setter)0,
	PyDoc_STR("*sequence*: Total revenue to project [$]"),
 	NULL},
{"cost_debt_upfront", (getter)Outputs_get_cost_debt_upfront,(setter)0,
	PyDoc_STR("*float*: Debt up-front fee [$]"),
 	NULL},
{"cost_financing", (getter)Outputs_get_cost_financing,(setter)0,
	PyDoc_STR("*float*: Financing Cost [$]"),
 	NULL},
{"cost_installed", (getter)Outputs_get_cost_installed,(setter)0,
	PyDoc_STR("*float*: Net capital cost"),
 	NULL},
{"cost_installedperwatt", (getter)Outputs_get_cost_installedperwatt,(setter)0,
	PyDoc_STR("*float*: Net capital cost per watt [$/W]"),
 	NULL},
{"cost_prefinancing", (getter)Outputs_get_cost_prefinancing,(setter)0,
	PyDoc_STR("*float*: Total installed cost [$]"),
 	NULL},
{"debt_fraction", (getter)Outputs_get_debt_fraction,(setter)0,
	PyDoc_STR("*float*: Debt percent [%]"),
 	NULL},
{"depr_alloc_custom", (getter)Outputs_get_depr_alloc_custom,(setter)0,
	PyDoc_STR("*float*: Custom straight line depreciation federal and state allocation [$]"),
 	NULL},
{"depr_alloc_macrs_15", (getter)Outputs_get_depr_alloc_macrs_15,(setter)0,
	PyDoc_STR("*float*: 15-yr MACRS depreciation federal and state allocation [$]"),
 	NULL},
{"depr_alloc_macrs_5", (getter)Outputs_get_depr_alloc_macrs_5,(setter)0,
	PyDoc_STR("*float*: 5-yr MACRS depreciation federal and state allocation [$]"),
 	NULL},
{"depr_alloc_none", (getter)Outputs_get_depr_alloc_none,(setter)0,
	PyDoc_STR("*float*: Non-depreciable federal and state allocation [$]"),
 	NULL},
{"depr_alloc_none_percent", (getter)Outputs_get_depr_alloc_none_percent,(setter)0,
	PyDoc_STR("*float*: Non-depreciable federal and state allocation [%]"),
 	NULL},
{"depr_alloc_sl_15", (getter)Outputs_get_depr_alloc_sl_15,(setter)0,
	PyDoc_STR("*float*: 15-yr straight line depreciation federal and state allocation [$]"),
 	NULL},
{"depr_alloc_sl_20", (getter)Outputs_get_depr_alloc_sl_20,(setter)0,
	PyDoc_STR("*float*: 20-yr straight line depreciation federal and state allocation [$]"),
 	NULL},
{"depr_alloc_sl_39", (getter)Outputs_get_depr_alloc_sl_39,(setter)0,
	PyDoc_STR("*float*: 39-yr straight line depreciation federal and state allocation [$]"),
 	NULL},
{"depr_alloc_sl_5", (getter)Outputs_get_depr_alloc_sl_5,(setter)0,
	PyDoc_STR("*float*: 5-yr straight line depreciation federal and state allocation [$]"),
 	NULL},
{"depr_alloc_total", (getter)Outputs_get_depr_alloc_total,(setter)0,
	PyDoc_STR("*float*: Total depreciation federal and state allocation [$]"),
 	NULL},
{"depr_fedbas_after_itc_custom", (getter)Outputs_get_depr_fedbas_after_itc_custom,(setter)0,
	PyDoc_STR("*float*: Custom straight line federal depreciation basis after ITC reduction [$]"),
 	NULL},
{"depr_fedbas_after_itc_macrs_15", (getter)Outputs_get_depr_fedbas_after_itc_macrs_15,(setter)0,
	PyDoc_STR("*float*: 15-yr MACRS federal depreciation basis after ITC reduction [$]"),
 	NULL},
{"depr_fedbas_after_itc_macrs_5", (getter)Outputs_get_depr_fedbas_after_itc_macrs_5,(setter)0,
	PyDoc_STR("*float*: 5-yr MACRS federal depreciation basis after ITC reduction [$]"),
 	NULL},
{"depr_fedbas_after_itc_sl_15", (getter)Outputs_get_depr_fedbas_after_itc_sl_15,(setter)0,
	PyDoc_STR("*float*: 15-yr straight line federal depreciation basis after ITC reduction [$]"),
 	NULL},
{"depr_fedbas_after_itc_sl_20", (getter)Outputs_get_depr_fedbas_after_itc_sl_20,(setter)0,
	PyDoc_STR("*float*: 20-yr straight line federal depreciation basis after ITC reduction [$]"),
 	NULL},
{"depr_fedbas_after_itc_sl_39", (getter)Outputs_get_depr_fedbas_after_itc_sl_39,(setter)0,
	PyDoc_STR("*float*: 39-yr straight line federal depreciation basis after ITC reduction [$]"),
 	NULL},
{"depr_fedbas_after_itc_sl_5", (getter)Outputs_get_depr_fedbas_after_itc_sl_5,(setter)0,
	PyDoc_STR("*float*: 5-yr straight line federal depreciation basis after ITC reduction [$]"),
 	NULL},
{"depr_fedbas_after_itc_total", (getter)Outputs_get_depr_fedbas_after_itc_total,(setter)0,
	PyDoc_STR("*float*: Total federal depreciation basis after ITC reduction [$]"),
 	NULL},
{"depr_fedbas_cbi_reduc_custom", (getter)Outputs_get_depr_fedbas_cbi_reduc_custom,(setter)0,
	PyDoc_STR("*float*: Custom straight line federal cbi reduction [$]"),
 	NULL},
{"depr_fedbas_cbi_reduc_macrs_15", (getter)Outputs_get_depr_fedbas_cbi_reduc_macrs_15,(setter)0,
	PyDoc_STR("*float*: 15-yr MACRS federal cbi reduction [$]"),
 	NULL},
{"depr_fedbas_cbi_reduc_macrs_5", (getter)Outputs_get_depr_fedbas_cbi_reduc_macrs_5,(setter)0,
	PyDoc_STR("*float*: 5-yr MACRS federal cbi reduction [$]"),
 	NULL},
{"depr_fedbas_cbi_reduc_sl_15", (getter)Outputs_get_depr_fedbas_cbi_reduc_sl_15,(setter)0,
	PyDoc_STR("*float*: 15-yr straight line federal cbi reduction [$]"),
 	NULL},
{"depr_fedbas_cbi_reduc_sl_20", (getter)Outputs_get_depr_fedbas_cbi_reduc_sl_20,(setter)0,
	PyDoc_STR("*float*: 20-yr straight line federal cbi reduction [$]"),
 	NULL},
{"depr_fedbas_cbi_reduc_sl_39", (getter)Outputs_get_depr_fedbas_cbi_reduc_sl_39,(setter)0,
	PyDoc_STR("*float*: 39-yr straight line federal cbi reduction [$]"),
 	NULL},
{"depr_fedbas_cbi_reduc_sl_5", (getter)Outputs_get_depr_fedbas_cbi_reduc_sl_5,(setter)0,
	PyDoc_STR("*float*: 5-yr straight line federal cbi reduction [$]"),
 	NULL},
{"depr_fedbas_cbi_reduc_total", (getter)Outputs_get_depr_fedbas_cbi_reduc_total,(setter)0,
	PyDoc_STR("*float*: Total federal cbi reduction [$]"),
 	NULL},
{"depr_fedbas_custom", (getter)Outputs_get_depr_fedbas_custom,(setter)0,
	PyDoc_STR("*float*: Custom straight line federal depreciation basis [$]"),
 	NULL},
{"depr_fedbas_first_year_bonus_custom", (getter)Outputs_get_depr_fedbas_first_year_bonus_custom,(setter)0,
	PyDoc_STR("*float*: Custom straight line federal first year bonus depreciation [$]"),
 	NULL},
{"depr_fedbas_first_year_bonus_macrs_15", (getter)Outputs_get_depr_fedbas_first_year_bonus_macrs_15,(setter)0,
	PyDoc_STR("*float*: 15-yr MACRS federal first year bonus depreciation [$]"),
 	NULL},
{"depr_fedbas_first_year_bonus_macrs_5", (getter)Outputs_get_depr_fedbas_first_year_bonus_macrs_5,(setter)0,
	PyDoc_STR("*float*: 5-yr MACRS federal first year bonus depreciation [$]"),
 	NULL},
{"depr_fedbas_first_year_bonus_sl_15", (getter)Outputs_get_depr_fedbas_first_year_bonus_sl_15,(setter)0,
	PyDoc_STR("*float*: 15-yr straight line federal first year bonus depreciation [$]"),
 	NULL},
{"depr_fedbas_first_year_bonus_sl_20", (getter)Outputs_get_depr_fedbas_first_year_bonus_sl_20,(setter)0,
	PyDoc_STR("*float*: 20-yr straight line federal first year bonus depreciation [$]"),
 	NULL},
{"depr_fedbas_first_year_bonus_sl_39", (getter)Outputs_get_depr_fedbas_first_year_bonus_sl_39,(setter)0,
	PyDoc_STR("*float*: 39-yr straight line federal first year bonus depreciation [$]"),
 	NULL},
{"depr_fedbas_first_year_bonus_sl_5", (getter)Outputs_get_depr_fedbas_first_year_bonus_sl_5,(setter)0,
	PyDoc_STR("*float*: 5-yr straight line federal first year bonus depreciation [$]"),
 	NULL},
{"depr_fedbas_first_year_bonus_total", (getter)Outputs_get_depr_fedbas_first_year_bonus_total,(setter)0,
	PyDoc_STR("*float*: Total federal first year bonus depreciation [$]"),
 	NULL},
{"depr_fedbas_fixed_amount_custom", (getter)Outputs_get_depr_fedbas_fixed_amount_custom,(setter)0,
	PyDoc_STR("*float*: Custom straight line depreciation ITC basis from federal fixed amount [$]"),
 	NULL},
{"depr_fedbas_fixed_amount_macrs_15", (getter)Outputs_get_depr_fedbas_fixed_amount_macrs_15,(setter)0,
	PyDoc_STR("*float*: 15-yr MACRS depreciation ITC basis from federal fixed amount [$]"),
 	NULL},
{"depr_fedbas_fixed_amount_macrs_5", (getter)Outputs_get_depr_fedbas_fixed_amount_macrs_5,(setter)0,
	PyDoc_STR("*float*: 5-yr MACRS depreciation ITC basis from federal fixed amount [$]"),
 	NULL},
{"depr_fedbas_fixed_amount_sl_15", (getter)Outputs_get_depr_fedbas_fixed_amount_sl_15,(setter)0,
	PyDoc_STR("*float*: 15-yr straight line depreciation ITC basis from federal fixed amount [$]"),
 	NULL},
{"depr_fedbas_fixed_amount_sl_20", (getter)Outputs_get_depr_fedbas_fixed_amount_sl_20,(setter)0,
	PyDoc_STR("*float*: 20-yr straight line depreciation ITC basis from federal fixed amount [$]"),
 	NULL},
{"depr_fedbas_fixed_amount_sl_39", (getter)Outputs_get_depr_fedbas_fixed_amount_sl_39,(setter)0,
	PyDoc_STR("*float*: 39-yr straight line depreciation ITC basis from federal fixed amount [$]"),
 	NULL},
{"depr_fedbas_fixed_amount_sl_5", (getter)Outputs_get_depr_fedbas_fixed_amount_sl_5,(setter)0,
	PyDoc_STR("*float*: 5-yr straight line depreciation ITC basis from federal fixed amount [$]"),
 	NULL},
{"depr_fedbas_fixed_amount_total", (getter)Outputs_get_depr_fedbas_fixed_amount_total,(setter)0,
	PyDoc_STR("*float*: Total depreciation ITC basis from federal fixed amount [$]"),
 	NULL},
{"depr_fedbas_ibi_reduc_custom", (getter)Outputs_get_depr_fedbas_ibi_reduc_custom,(setter)0,
	PyDoc_STR("*float*: Custom straight line federal ibi reduction [$]"),
 	NULL},
{"depr_fedbas_ibi_reduc_macrs_15", (getter)Outputs_get_depr_fedbas_ibi_reduc_macrs_15,(setter)0,
	PyDoc_STR("*float*: 15-yr MACRS federal ibi reduction [$]"),
 	NULL},
{"depr_fedbas_ibi_reduc_macrs_5", (getter)Outputs_get_depr_fedbas_ibi_reduc_macrs_5,(setter)0,
	PyDoc_STR("*float*: 5-yr MACRS federal ibi reduction [$]"),
 	NULL},
{"depr_fedbas_ibi_reduc_sl_15", (getter)Outputs_get_depr_fedbas_ibi_reduc_sl_15,(setter)0,
	PyDoc_STR("*float*: 15-yr straight line federal ibi reduction [$]"),
 	NULL},
{"depr_fedbas_ibi_reduc_sl_20", (getter)Outputs_get_depr_fedbas_ibi_reduc_sl_20,(setter)0,
	PyDoc_STR("*float*: 20-yr straight line federal ibi reduction [$]"),
 	NULL},
{"depr_fedbas_ibi_reduc_sl_39", (getter)Outputs_get_depr_fedbas_ibi_reduc_sl_39,(setter)0,
	PyDoc_STR("*float*: 39-yr straight line federal ibi reduction [$]"),
 	NULL},
{"depr_fedbas_ibi_reduc_sl_5", (getter)Outputs_get_depr_fedbas_ibi_reduc_sl_5,(setter)0,
	PyDoc_STR("*float*: 5-yr straight line federal ibi reduction [$]"),
 	NULL},
{"depr_fedbas_ibi_reduc_total", (getter)Outputs_get_depr_fedbas_ibi_reduc_total,(setter)0,
	PyDoc_STR("*float*: Total federal ibi reduction [$]"),
 	NULL},
{"depr_fedbas_itc_fed_reduction_custom", (getter)Outputs_get_depr_fedbas_itc_fed_reduction_custom,(setter)0,
	PyDoc_STR("*float*: Custom straight line federal basis federal ITC reduciton [$]"),
 	NULL},
{"depr_fedbas_itc_fed_reduction_macrs_15", (getter)Outputs_get_depr_fedbas_itc_fed_reduction_macrs_15,(setter)0,
	PyDoc_STR("*float*: 15-yr MACRS federal basis federal ITC reduciton [$]"),
 	NULL},
{"depr_fedbas_itc_fed_reduction_macrs_5", (getter)Outputs_get_depr_fedbas_itc_fed_reduction_macrs_5,(setter)0,
	PyDoc_STR("*float*: 5-yr MACRS federal basis federal ITC reduciton [$]"),
 	NULL},
{"depr_fedbas_itc_fed_reduction_sl_15", (getter)Outputs_get_depr_fedbas_itc_fed_reduction_sl_15,(setter)0,
	PyDoc_STR("*float*: 15-yr straight line federal basis federal ITC reduciton [$]"),
 	NULL},
{"depr_fedbas_itc_fed_reduction_sl_20", (getter)Outputs_get_depr_fedbas_itc_fed_reduction_sl_20,(setter)0,
	PyDoc_STR("*float*: 20-yr straight line federal basis federal ITC reduciton [$]"),
 	NULL},
{"depr_fedbas_itc_fed_reduction_sl_39", (getter)Outputs_get_depr_fedbas_itc_fed_reduction_sl_39,(setter)0,
	PyDoc_STR("*float*: 39-yr straight line federal basis federal ITC reduciton [$]"),
 	NULL},
{"depr_fedbas_itc_fed_reduction_sl_5", (getter)Outputs_get_depr_fedbas_itc_fed_reduction_sl_5,(setter)0,
	PyDoc_STR("*float*: 5-yr straight line federal basis federal ITC reduciton [$]"),
 	NULL},
{"depr_fedbas_itc_fed_reduction_total", (getter)Outputs_get_depr_fedbas_itc_fed_reduction_total,(setter)0,
	PyDoc_STR("*float*: Total federal basis federal ITC reduciton [$]"),
 	NULL},
{"depr_fedbas_itc_sta_reduction_custom", (getter)Outputs_get_depr_fedbas_itc_sta_reduction_custom,(setter)0,
	PyDoc_STR("*float*: Custom straight line federal basis state ITC reduciton [$]"),
 	NULL},
{"depr_fedbas_itc_sta_reduction_macrs_15", (getter)Outputs_get_depr_fedbas_itc_sta_reduction_macrs_15,(setter)0,
	PyDoc_STR("*float*: 15-yr MACRS federal basis state ITC reduciton [$]"),
 	NULL},
{"depr_fedbas_itc_sta_reduction_macrs_5", (getter)Outputs_get_depr_fedbas_itc_sta_reduction_macrs_5,(setter)0,
	PyDoc_STR("*float*: 5-yr MACRS federal basis state ITC reduciton [$]"),
 	NULL},
{"depr_fedbas_itc_sta_reduction_sl_15", (getter)Outputs_get_depr_fedbas_itc_sta_reduction_sl_15,(setter)0,
	PyDoc_STR("*float*: 15-yr straight line federal basis state ITC reduciton [$]"),
 	NULL},
{"depr_fedbas_itc_sta_reduction_sl_20", (getter)Outputs_get_depr_fedbas_itc_sta_reduction_sl_20,(setter)0,
	PyDoc_STR("*float*: 20-yr straight line federal basis state ITC reduciton [$]"),
 	NULL},
{"depr_fedbas_itc_sta_reduction_sl_39", (getter)Outputs_get_depr_fedbas_itc_sta_reduction_sl_39,(setter)0,
	PyDoc_STR("*float*: 39-yr straight line federal basis state ITC reduciton [$]"),
 	NULL},
{"depr_fedbas_itc_sta_reduction_sl_5", (getter)Outputs_get_depr_fedbas_itc_sta_reduction_sl_5,(setter)0,
	PyDoc_STR("*float*: 5-yr straight line federal basis state ITC reduciton [$]"),
 	NULL},
{"depr_fedbas_itc_sta_reduction_total", (getter)Outputs_get_depr_fedbas_itc_sta_reduction_total,(setter)0,
	PyDoc_STR("*float*: Total federal basis state ITC reduciton [$]"),
 	NULL},
{"depr_fedbas_macrs_15", (getter)Outputs_get_depr_fedbas_macrs_15,(setter)0,
	PyDoc_STR("*float*: 15-yr MACRS federal depreciation basis [$]"),
 	NULL},
{"depr_fedbas_macrs_5", (getter)Outputs_get_depr_fedbas_macrs_5,(setter)0,
	PyDoc_STR("*float*: 5-yr MACRS federal depreciation basis [$]"),
 	NULL},
{"depr_fedbas_percent_amount_custom", (getter)Outputs_get_depr_fedbas_percent_amount_custom,(setter)0,
	PyDoc_STR("*float*: Custom straight line depreciation ITC basis from federal percentage [$]"),
 	NULL},
{"depr_fedbas_percent_amount_macrs_15", (getter)Outputs_get_depr_fedbas_percent_amount_macrs_15,(setter)0,
	PyDoc_STR("*float*: 15-yr MACRS depreciation ITC basis from federal percentage [$]"),
 	NULL},
{"depr_fedbas_percent_amount_macrs_5", (getter)Outputs_get_depr_fedbas_percent_amount_macrs_5,(setter)0,
	PyDoc_STR("*float*: 5-yr MACRS depreciation ITC basis from federal percentage [$]"),
 	NULL},
{"depr_fedbas_percent_amount_sl_15", (getter)Outputs_get_depr_fedbas_percent_amount_sl_15,(setter)0,
	PyDoc_STR("*float*: 15-yr straight line depreciation ITC basis from federal percentage [$]"),
 	NULL},
{"depr_fedbas_percent_amount_sl_20", (getter)Outputs_get_depr_fedbas_percent_amount_sl_20,(setter)0,
	PyDoc_STR("*float*: 20-yr straight line depreciation ITC basis from federal percentage [$]"),
 	NULL},
{"depr_fedbas_percent_amount_sl_39", (getter)Outputs_get_depr_fedbas_percent_amount_sl_39,(setter)0,
	PyDoc_STR("*float*: 39-yr straight line depreciation ITC basis from federal percentage [$]"),
 	NULL},
{"depr_fedbas_percent_amount_sl_5", (getter)Outputs_get_depr_fedbas_percent_amount_sl_5,(setter)0,
	PyDoc_STR("*float*: 5-yr straight line depreciation ITC basis from federal percentage [$]"),
 	NULL},
{"depr_fedbas_percent_amount_total", (getter)Outputs_get_depr_fedbas_percent_amount_total,(setter)0,
	PyDoc_STR("*float*: Total depreciation ITC basis from federal percentage [$]"),
 	NULL},
{"depr_fedbas_percent_custom", (getter)Outputs_get_depr_fedbas_percent_custom,(setter)0,
	PyDoc_STR("*float*: Custom straight line federal percent of total depreciable basis [%]"),
 	NULL},
{"depr_fedbas_percent_macrs_15", (getter)Outputs_get_depr_fedbas_percent_macrs_15,(setter)0,
	PyDoc_STR("*float*: 15-yr MACRS federal percent of total depreciable basis [%]"),
 	NULL},
{"depr_fedbas_percent_macrs_5", (getter)Outputs_get_depr_fedbas_percent_macrs_5,(setter)0,
	PyDoc_STR("*float*: 5-yr MACRS federal percent of total depreciable basis [%]"),
 	NULL},
{"depr_fedbas_percent_qual_custom", (getter)Outputs_get_depr_fedbas_percent_qual_custom,(setter)0,
	PyDoc_STR("*float*: Custom straight line federal percent of qualifying costs [%]"),
 	NULL},
{"depr_fedbas_percent_qual_macrs_15", (getter)Outputs_get_depr_fedbas_percent_qual_macrs_15,(setter)0,
	PyDoc_STR("*float*: 15-yr MACRS federal percent of qualifying costs [%]"),
 	NULL},
{"depr_fedbas_percent_qual_macrs_5", (getter)Outputs_get_depr_fedbas_percent_qual_macrs_5,(setter)0,
	PyDoc_STR("*float*: 5-yr MACRS federal percent of qualifying costs [%]"),
 	NULL},
{"depr_fedbas_percent_qual_sl_15", (getter)Outputs_get_depr_fedbas_percent_qual_sl_15,(setter)0,
	PyDoc_STR("*float*: 15-yr straight line federal percent of qualifying costs [%]"),
 	NULL},
{"depr_fedbas_percent_qual_sl_20", (getter)Outputs_get_depr_fedbas_percent_qual_sl_20,(setter)0,
	PyDoc_STR("*float*: 20-yr straight line federal percent of qualifying costs [%]"),
 	NULL},
{"depr_fedbas_percent_qual_sl_39", (getter)Outputs_get_depr_fedbas_percent_qual_sl_39,(setter)0,
	PyDoc_STR("*float*: 39-yr straight line federal percent of qualifying costs [%]"),
 	NULL},
{"depr_fedbas_percent_qual_sl_5", (getter)Outputs_get_depr_fedbas_percent_qual_sl_5,(setter)0,
	PyDoc_STR("*float*: 5-yr straight line federal percent of qualifying costs [%]"),
 	NULL},
{"depr_fedbas_percent_qual_total", (getter)Outputs_get_depr_fedbas_percent_qual_total,(setter)0,
	PyDoc_STR("*float*: Total federal percent of qualifying costs [%]"),
 	NULL},
{"depr_fedbas_percent_sl_15", (getter)Outputs_get_depr_fedbas_percent_sl_15,(setter)0,
	PyDoc_STR("*float*: 15-yr straight line federal percent of total depreciable basis [%]"),
 	NULL},
{"depr_fedbas_percent_sl_20", (getter)Outputs_get_depr_fedbas_percent_sl_20,(setter)0,
	PyDoc_STR("*float*: 20-yr straight line federal percent of total depreciable basis [%]"),
 	NULL},
{"depr_fedbas_percent_sl_39", (getter)Outputs_get_depr_fedbas_percent_sl_39,(setter)0,
	PyDoc_STR("*float*: 39-yr straight line federal percent of total depreciable basis [%]"),
 	NULL},
{"depr_fedbas_percent_sl_5", (getter)Outputs_get_depr_fedbas_percent_sl_5,(setter)0,
	PyDoc_STR("*float*: 5-yr straight line federal percent of total depreciable basis [%]"),
 	NULL},
{"depr_fedbas_percent_total", (getter)Outputs_get_depr_fedbas_percent_total,(setter)0,
	PyDoc_STR("*float*: Total federal percent of total depreciable basis [%]"),
 	NULL},
{"depr_fedbas_prior_itc_custom", (getter)Outputs_get_depr_fedbas_prior_itc_custom,(setter)0,
	PyDoc_STR("*float*: Custom straight line federal depreciation basis prior ITC reduction [$]"),
 	NULL},
{"depr_fedbas_prior_itc_macrs_15", (getter)Outputs_get_depr_fedbas_prior_itc_macrs_15,(setter)0,
	PyDoc_STR("*float*: 15-yr MACRS federal depreciation basis prior ITC reduction [$]"),
 	NULL},
{"depr_fedbas_prior_itc_macrs_5", (getter)Outputs_get_depr_fedbas_prior_itc_macrs_5,(setter)0,
	PyDoc_STR("*float*: 5-yr MACRS federal depreciation basis prior ITC reduction [$]"),
 	NULL},
{"depr_fedbas_prior_itc_sl_15", (getter)Outputs_get_depr_fedbas_prior_itc_sl_15,(setter)0,
	PyDoc_STR("*float*: 15-yr straight line federal depreciation basis prior ITC reduction [$]"),
 	NULL},
{"depr_fedbas_prior_itc_sl_20", (getter)Outputs_get_depr_fedbas_prior_itc_sl_20,(setter)0,
	PyDoc_STR("*float*: 20-yr straight line federal depreciation basis prior ITC reduction [$]"),
 	NULL},
{"depr_fedbas_prior_itc_sl_39", (getter)Outputs_get_depr_fedbas_prior_itc_sl_39,(setter)0,
	PyDoc_STR("*float*: 39-yr straight line federal depreciation basis prior ITC reduction [$]"),
 	NULL},
{"depr_fedbas_prior_itc_sl_5", (getter)Outputs_get_depr_fedbas_prior_itc_sl_5,(setter)0,
	PyDoc_STR("*float*: 5-yr straight line federal depreciation basis prior ITC reduction [$]"),
 	NULL},
{"depr_fedbas_prior_itc_total", (getter)Outputs_get_depr_fedbas_prior_itc_total,(setter)0,
	PyDoc_STR("*float*: Total federal depreciation basis prior ITC reduction [$]"),
 	NULL},
{"depr_fedbas_sl_15", (getter)Outputs_get_depr_fedbas_sl_15,(setter)0,
	PyDoc_STR("*float*: 15-yr straight line federal depreciation basis [$]"),
 	NULL},
{"depr_fedbas_sl_20", (getter)Outputs_get_depr_fedbas_sl_20,(setter)0,
	PyDoc_STR("*float*: 20-yr straight line federal depreciation basis [$]"),
 	NULL},
{"depr_fedbas_sl_39", (getter)Outputs_get_depr_fedbas_sl_39,(setter)0,
	PyDoc_STR("*float*: 39-yr straight line federal depreciation basis [$]"),
 	NULL},
{"depr_fedbas_sl_5", (getter)Outputs_get_depr_fedbas_sl_5,(setter)0,
	PyDoc_STR("*float*: 5-yr straight line federal depreciation basis [$]"),
 	NULL},
{"depr_fedbas_total", (getter)Outputs_get_depr_fedbas_total,(setter)0,
	PyDoc_STR("*float*: Total federal depreciation basis [$]"),
 	NULL},
{"depr_stabas_after_itc_custom", (getter)Outputs_get_depr_stabas_after_itc_custom,(setter)0,
	PyDoc_STR("*float*: Custom straight line state depreciation basis after ITC reduction [$]"),
 	NULL},
{"depr_stabas_after_itc_macrs_15", (getter)Outputs_get_depr_stabas_after_itc_macrs_15,(setter)0,
	PyDoc_STR("*float*: 15-yr MACRS state depreciation basis after ITC reduction [$]"),
 	NULL},
{"depr_stabas_after_itc_macrs_5", (getter)Outputs_get_depr_stabas_after_itc_macrs_5,(setter)0,
	PyDoc_STR("*float*: 5-yr MACRS state depreciation basis after ITC reduction [$]"),
 	NULL},
{"depr_stabas_after_itc_sl_15", (getter)Outputs_get_depr_stabas_after_itc_sl_15,(setter)0,
	PyDoc_STR("*float*: 15-yr straight line state depreciation basis after ITC reduction [$]"),
 	NULL},
{"depr_stabas_after_itc_sl_20", (getter)Outputs_get_depr_stabas_after_itc_sl_20,(setter)0,
	PyDoc_STR("*float*: 20-yr straight line state depreciation basis after ITC reduction [$]"),
 	NULL},
{"depr_stabas_after_itc_sl_39", (getter)Outputs_get_depr_stabas_after_itc_sl_39,(setter)0,
	PyDoc_STR("*float*: 39-yr straight line state depreciation basis after ITC reduction [$]"),
 	NULL},
{"depr_stabas_after_itc_sl_5", (getter)Outputs_get_depr_stabas_after_itc_sl_5,(setter)0,
	PyDoc_STR("*float*: 5-yr straight line state depreciation basis after ITC reduction [$]"),
 	NULL},
{"depr_stabas_after_itc_total", (getter)Outputs_get_depr_stabas_after_itc_total,(setter)0,
	PyDoc_STR("*float*: Total state depreciation basis after ITC reduction [$]"),
 	NULL},
{"depr_stabas_cbi_reduc_custom", (getter)Outputs_get_depr_stabas_cbi_reduc_custom,(setter)0,
	PyDoc_STR("*float*: Custom straight line state cbi reduction [$]"),
 	NULL},
{"depr_stabas_cbi_reduc_macrs_15", (getter)Outputs_get_depr_stabas_cbi_reduc_macrs_15,(setter)0,
	PyDoc_STR("*float*: 15-yr MACRS state cbi reduction [$]"),
 	NULL},
{"depr_stabas_cbi_reduc_macrs_5", (getter)Outputs_get_depr_stabas_cbi_reduc_macrs_5,(setter)0,
	PyDoc_STR("*float*: 5-yr MACRS state cbi reduction [$]"),
 	NULL},
{"depr_stabas_cbi_reduc_sl_15", (getter)Outputs_get_depr_stabas_cbi_reduc_sl_15,(setter)0,
	PyDoc_STR("*float*: 15-yr straight line state cbi reduction [$]"),
 	NULL},
{"depr_stabas_cbi_reduc_sl_20", (getter)Outputs_get_depr_stabas_cbi_reduc_sl_20,(setter)0,
	PyDoc_STR("*float*: 20-yr straight line state cbi reduction [$]"),
 	NULL},
{"depr_stabas_cbi_reduc_sl_39", (getter)Outputs_get_depr_stabas_cbi_reduc_sl_39,(setter)0,
	PyDoc_STR("*float*: 39-yr straight line state cbi reduction [$]"),
 	NULL},
{"depr_stabas_cbi_reduc_sl_5", (getter)Outputs_get_depr_stabas_cbi_reduc_sl_5,(setter)0,
	PyDoc_STR("*float*: 5-yr straight line state cbi reduction [$]"),
 	NULL},
{"depr_stabas_cbi_reduc_total", (getter)Outputs_get_depr_stabas_cbi_reduc_total,(setter)0,
	PyDoc_STR("*float*: Total state cbi reduction [$]"),
 	NULL},
{"depr_stabas_custom", (getter)Outputs_get_depr_stabas_custom,(setter)0,
	PyDoc_STR("*float*: Custom straight line state depreciation basis [$]"),
 	NULL},
{"depr_stabas_first_year_bonus_custom", (getter)Outputs_get_depr_stabas_first_year_bonus_custom,(setter)0,
	PyDoc_STR("*float*: Custom straight line state first year bonus depreciation [$]"),
 	NULL},
{"depr_stabas_first_year_bonus_macrs_15", (getter)Outputs_get_depr_stabas_first_year_bonus_macrs_15,(setter)0,
	PyDoc_STR("*float*: 15-yr MACRS state first year bonus depreciation [$]"),
 	NULL},
{"depr_stabas_first_year_bonus_macrs_5", (getter)Outputs_get_depr_stabas_first_year_bonus_macrs_5,(setter)0,
	PyDoc_STR("*float*: 5-yr MACRS state first year bonus depreciation [$]"),
 	NULL},
{"depr_stabas_first_year_bonus_sl_15", (getter)Outputs_get_depr_stabas_first_year_bonus_sl_15,(setter)0,
	PyDoc_STR("*float*: 15-yr straight line state first year bonus depreciation [$]"),
 	NULL},
{"depr_stabas_first_year_bonus_sl_20", (getter)Outputs_get_depr_stabas_first_year_bonus_sl_20,(setter)0,
	PyDoc_STR("*float*: 20-yr straight line state first year bonus depreciation [$]"),
 	NULL},
{"depr_stabas_first_year_bonus_sl_39", (getter)Outputs_get_depr_stabas_first_year_bonus_sl_39,(setter)0,
	PyDoc_STR("*float*: 39-yr straight line state first year bonus depreciation [$]"),
 	NULL},
{"depr_stabas_first_year_bonus_sl_5", (getter)Outputs_get_depr_stabas_first_year_bonus_sl_5,(setter)0,
	PyDoc_STR("*float*: 5-yr straight line state first year bonus depreciation [$]"),
 	NULL},
{"depr_stabas_first_year_bonus_total", (getter)Outputs_get_depr_stabas_first_year_bonus_total,(setter)0,
	PyDoc_STR("*float*: Total state first year bonus depreciation [$]"),
 	NULL},
{"depr_stabas_fixed_amount_custom", (getter)Outputs_get_depr_stabas_fixed_amount_custom,(setter)0,
	PyDoc_STR("*float*: Custom straight line depreciation ITC basis from state fixed amount [$]"),
 	NULL},
{"depr_stabas_fixed_amount_macrs_15", (getter)Outputs_get_depr_stabas_fixed_amount_macrs_15,(setter)0,
	PyDoc_STR("*float*: 15-yr MACRS depreciation ITC basis from state fixed amount [$]"),
 	NULL},
{"depr_stabas_fixed_amount_macrs_5", (getter)Outputs_get_depr_stabas_fixed_amount_macrs_5,(setter)0,
	PyDoc_STR("*float*: 5-yr MACRS depreciation ITC basis from state fixed amount [$]"),
 	NULL},
{"depr_stabas_fixed_amount_sl_15", (getter)Outputs_get_depr_stabas_fixed_amount_sl_15,(setter)0,
	PyDoc_STR("*float*: 15-yr straight line depreciation ITC basis from state fixed amount [$]"),
 	NULL},
{"depr_stabas_fixed_amount_sl_20", (getter)Outputs_get_depr_stabas_fixed_amount_sl_20,(setter)0,
	PyDoc_STR("*float*: 20-yr straight line depreciation ITC basis from state fixed amount [$]"),
 	NULL},
{"depr_stabas_fixed_amount_sl_39", (getter)Outputs_get_depr_stabas_fixed_amount_sl_39,(setter)0,
	PyDoc_STR("*float*: 39-yr straight line depreciation ITC basis from state fixed amount [$]"),
 	NULL},
{"depr_stabas_fixed_amount_sl_5", (getter)Outputs_get_depr_stabas_fixed_amount_sl_5,(setter)0,
	PyDoc_STR("*float*: 5-yr straight line depreciation ITC basis from state fixed amount [$]"),
 	NULL},
{"depr_stabas_fixed_amount_total", (getter)Outputs_get_depr_stabas_fixed_amount_total,(setter)0,
	PyDoc_STR("*float*: Total depreciation ITC basis from state fixed amount [$]"),
 	NULL},
{"depr_stabas_ibi_reduc_custom", (getter)Outputs_get_depr_stabas_ibi_reduc_custom,(setter)0,
	PyDoc_STR("*float*: Custom straight line state ibi reduction [$]"),
 	NULL},
{"depr_stabas_ibi_reduc_macrs_15", (getter)Outputs_get_depr_stabas_ibi_reduc_macrs_15,(setter)0,
	PyDoc_STR("*float*: 15-yr MACRS state ibi reduction [$]"),
 	NULL},
{"depr_stabas_ibi_reduc_macrs_5", (getter)Outputs_get_depr_stabas_ibi_reduc_macrs_5,(setter)0,
	PyDoc_STR("*float*: 5-yr MACRS state ibi reduction [$]"),
 	NULL},
{"depr_stabas_ibi_reduc_sl_15", (getter)Outputs_get_depr_stabas_ibi_reduc_sl_15,(setter)0,
	PyDoc_STR("*float*: 15-yr straight line state ibi reduction [$]"),
 	NULL},
{"depr_stabas_ibi_reduc_sl_20", (getter)Outputs_get_depr_stabas_ibi_reduc_sl_20,(setter)0,
	PyDoc_STR("*float*: 20-yr straight line state ibi reduction [$]"),
 	NULL},
{"depr_stabas_ibi_reduc_sl_39", (getter)Outputs_get_depr_stabas_ibi_reduc_sl_39,(setter)0,
	PyDoc_STR("*float*: 39-yr straight line state ibi reduction [$]"),
 	NULL},
{"depr_stabas_ibi_reduc_sl_5", (getter)Outputs_get_depr_stabas_ibi_reduc_sl_5,(setter)0,
	PyDoc_STR("*float*: 5-yr straight line state ibi reduction [$]"),
 	NULL},
{"depr_stabas_ibi_reduc_total", (getter)Outputs_get_depr_stabas_ibi_reduc_total,(setter)0,
	PyDoc_STR("*float*: Total state ibi reduction [$]"),
 	NULL},
{"depr_stabas_itc_fed_reduction_custom", (getter)Outputs_get_depr_stabas_itc_fed_reduction_custom,(setter)0,
	PyDoc_STR("*float*: Custom straight line state basis federal ITC reduciton [$]"),
 	NULL},
{"depr_stabas_itc_fed_reduction_macrs_15", (getter)Outputs_get_depr_stabas_itc_fed_reduction_macrs_15,(setter)0,
	PyDoc_STR("*float*: 15-yr MACRS state basis federal ITC reduciton [$]"),
 	NULL},
{"depr_stabas_itc_fed_reduction_macrs_5", (getter)Outputs_get_depr_stabas_itc_fed_reduction_macrs_5,(setter)0,
	PyDoc_STR("*float*: 5-yr MACRS state basis federal ITC reduciton [$]"),
 	NULL},
{"depr_stabas_itc_fed_reduction_sl_15", (getter)Outputs_get_depr_stabas_itc_fed_reduction_sl_15,(setter)0,
	PyDoc_STR("*float*: 15-yr straight line state basis federal ITC reduciton [$]"),
 	NULL},
{"depr_stabas_itc_fed_reduction_sl_20", (getter)Outputs_get_depr_stabas_itc_fed_reduction_sl_20,(setter)0,
	PyDoc_STR("*float*: 20-yr straight line state basis federal ITC reduciton [$]"),
 	NULL},
{"depr_stabas_itc_fed_reduction_sl_39", (getter)Outputs_get_depr_stabas_itc_fed_reduction_sl_39,(setter)0,
	PyDoc_STR("*float*: 39-yr straight line state basis federal ITC reduciton [$]"),
 	NULL},
{"depr_stabas_itc_fed_reduction_sl_5", (getter)Outputs_get_depr_stabas_itc_fed_reduction_sl_5,(setter)0,
	PyDoc_STR("*float*: 5-yr straight line state basis federal ITC reduciton [$]"),
 	NULL},
{"depr_stabas_itc_fed_reduction_total", (getter)Outputs_get_depr_stabas_itc_fed_reduction_total,(setter)0,
	PyDoc_STR("*float*: Total state basis federal ITC reduciton [$]"),
 	NULL},
{"depr_stabas_itc_sta_reduction_custom", (getter)Outputs_get_depr_stabas_itc_sta_reduction_custom,(setter)0,
	PyDoc_STR("*float*: Custom straight line state basis state ITC reduciton [$]"),
 	NULL},
{"depr_stabas_itc_sta_reduction_macrs_15", (getter)Outputs_get_depr_stabas_itc_sta_reduction_macrs_15,(setter)0,
	PyDoc_STR("*float*: 15-yr MACRS state basis state ITC reduciton [$]"),
 	NULL},
{"depr_stabas_itc_sta_reduction_macrs_5", (getter)Outputs_get_depr_stabas_itc_sta_reduction_macrs_5,(setter)0,
	PyDoc_STR("*float*: 5-yr MACRS state basis state ITC reduciton [$]"),
 	NULL},
{"depr_stabas_itc_sta_reduction_sl_15", (getter)Outputs_get_depr_stabas_itc_sta_reduction_sl_15,(setter)0,
	PyDoc_STR("*float*: 15-yr straight line state basis state ITC reduciton [$]"),
 	NULL},
{"depr_stabas_itc_sta_reduction_sl_20", (getter)Outputs_get_depr_stabas_itc_sta_reduction_sl_20,(setter)0,
	PyDoc_STR("*float*: 20-yr straight line state basis state ITC reduciton [$]"),
 	NULL},
{"depr_stabas_itc_sta_reduction_sl_39", (getter)Outputs_get_depr_stabas_itc_sta_reduction_sl_39,(setter)0,
	PyDoc_STR("*float*: 39-yr straight line state basis state ITC reduciton [$]"),
 	NULL},
{"depr_stabas_itc_sta_reduction_sl_5", (getter)Outputs_get_depr_stabas_itc_sta_reduction_sl_5,(setter)0,
	PyDoc_STR("*float*: 5-yr straight line state basis state ITC reduciton [$]"),
 	NULL},
{"depr_stabas_itc_sta_reduction_total", (getter)Outputs_get_depr_stabas_itc_sta_reduction_total,(setter)0,
	PyDoc_STR("*float*: Total state basis state ITC reduciton [$]"),
 	NULL},
{"depr_stabas_macrs_15", (getter)Outputs_get_depr_stabas_macrs_15,(setter)0,
	PyDoc_STR("*float*: 15-yr MACRS state depreciation basis [$]"),
 	NULL},
{"depr_stabas_macrs_5", (getter)Outputs_get_depr_stabas_macrs_5,(setter)0,
	PyDoc_STR("*float*: 5-yr MACRS state depreciation basis [$]"),
 	NULL},
{"depr_stabas_percent_amount_custom", (getter)Outputs_get_depr_stabas_percent_amount_custom,(setter)0,
	PyDoc_STR("*float*: Custom straight line depreciation ITC basis from state percentage [$]"),
 	NULL},
{"depr_stabas_percent_amount_macrs_15", (getter)Outputs_get_depr_stabas_percent_amount_macrs_15,(setter)0,
	PyDoc_STR("*float*: 15-yr MACRS depreciation ITC basis from state percentage [$]"),
 	NULL},
{"depr_stabas_percent_amount_macrs_5", (getter)Outputs_get_depr_stabas_percent_amount_macrs_5,(setter)0,
	PyDoc_STR("*float*: 5-yr MACRS depreciation ITC basis from state percentage [$]"),
 	NULL},
{"depr_stabas_percent_amount_sl_15", (getter)Outputs_get_depr_stabas_percent_amount_sl_15,(setter)0,
	PyDoc_STR("*float*: 15-yr straight line depreciation ITC basis from state percentage [$]"),
 	NULL},
{"depr_stabas_percent_amount_sl_20", (getter)Outputs_get_depr_stabas_percent_amount_sl_20,(setter)0,
	PyDoc_STR("*float*: 20-yr straight line depreciation ITC basis from state percentage [$]"),
 	NULL},
{"depr_stabas_percent_amount_sl_39", (getter)Outputs_get_depr_stabas_percent_amount_sl_39,(setter)0,
	PyDoc_STR("*float*: 39-yr straight line depreciation ITC basis from state percentage [$]"),
 	NULL},
{"depr_stabas_percent_amount_sl_5", (getter)Outputs_get_depr_stabas_percent_amount_sl_5,(setter)0,
	PyDoc_STR("*float*: 5-yr straight line depreciation ITC basis from state percentage [$]"),
 	NULL},
{"depr_stabas_percent_amount_total", (getter)Outputs_get_depr_stabas_percent_amount_total,(setter)0,
	PyDoc_STR("*float*: Total depreciation ITC basis from state percentage [$]"),
 	NULL},
{"depr_stabas_percent_custom", (getter)Outputs_get_depr_stabas_percent_custom,(setter)0,
	PyDoc_STR("*float*: Custom straight line state percent of total depreciable basis [%]"),
 	NULL},
{"depr_stabas_percent_macrs_15", (getter)Outputs_get_depr_stabas_percent_macrs_15,(setter)0,
	PyDoc_STR("*float*: 15-yr MACRS state percent of total depreciable basis [%]"),
 	NULL},
{"depr_stabas_percent_macrs_5", (getter)Outputs_get_depr_stabas_percent_macrs_5,(setter)0,
	PyDoc_STR("*float*: 5-yr MACRS state percent of total depreciable basis [%]"),
 	NULL},
{"depr_stabas_percent_qual_custom", (getter)Outputs_get_depr_stabas_percent_qual_custom,(setter)0,
	PyDoc_STR("*float*: Custom straight line state percent of qualifying costs [%]"),
 	NULL},
{"depr_stabas_percent_qual_macrs_15", (getter)Outputs_get_depr_stabas_percent_qual_macrs_15,(setter)0,
	PyDoc_STR("*float*: 15-yr MACRS state percent of qualifying costs [%]"),
 	NULL},
{"depr_stabas_percent_qual_macrs_5", (getter)Outputs_get_depr_stabas_percent_qual_macrs_5,(setter)0,
	PyDoc_STR("*float*: 5-yr MACRS state percent of qualifying costs [%]"),
 	NULL},
{"depr_stabas_percent_qual_sl_15", (getter)Outputs_get_depr_stabas_percent_qual_sl_15,(setter)0,
	PyDoc_STR("*float*: 15-yr straight line state percent of qualifying costs [%]"),
 	NULL},
{"depr_stabas_percent_qual_sl_20", (getter)Outputs_get_depr_stabas_percent_qual_sl_20,(setter)0,
	PyDoc_STR("*float*: 20-yr straight line state percent of qualifying costs [%]"),
 	NULL},
{"depr_stabas_percent_qual_sl_39", (getter)Outputs_get_depr_stabas_percent_qual_sl_39,(setter)0,
	PyDoc_STR("*float*: 39-yr straight line state percent of qualifying costs [%]"),
 	NULL},
{"depr_stabas_percent_qual_sl_5", (getter)Outputs_get_depr_stabas_percent_qual_sl_5,(setter)0,
	PyDoc_STR("*float*: 5-yr straight line state percent of qualifying costs [%]"),
 	NULL},
{"depr_stabas_percent_qual_total", (getter)Outputs_get_depr_stabas_percent_qual_total,(setter)0,
	PyDoc_STR("*float*: Total state percent of qualifying costs [%]"),
 	NULL},
{"depr_stabas_percent_sl_15", (getter)Outputs_get_depr_stabas_percent_sl_15,(setter)0,
	PyDoc_STR("*float*: 15-yr straight line state percent of total depreciable basis [%]"),
 	NULL},
{"depr_stabas_percent_sl_20", (getter)Outputs_get_depr_stabas_percent_sl_20,(setter)0,
	PyDoc_STR("*float*: 20-yr straight line state percent of total depreciable basis [%]"),
 	NULL},
{"depr_stabas_percent_sl_39", (getter)Outputs_get_depr_stabas_percent_sl_39,(setter)0,
	PyDoc_STR("*float*: 39-yr straight line state percent of total depreciable basis [%]"),
 	NULL},
{"depr_stabas_percent_sl_5", (getter)Outputs_get_depr_stabas_percent_sl_5,(setter)0,
	PyDoc_STR("*float*: 5-yr straight line state percent of total depreciable basis [%]"),
 	NULL},
{"depr_stabas_percent_total", (getter)Outputs_get_depr_stabas_percent_total,(setter)0,
	PyDoc_STR("*float*: Total state percent of total depreciable basis [%]"),
 	NULL},
{"depr_stabas_prior_itc_custom", (getter)Outputs_get_depr_stabas_prior_itc_custom,(setter)0,
	PyDoc_STR("*float*: Custom straight line state depreciation basis prior ITC reduction [$]"),
 	NULL},
{"depr_stabas_prior_itc_macrs_15", (getter)Outputs_get_depr_stabas_prior_itc_macrs_15,(setter)0,
	PyDoc_STR("*float*: 15-yr MACRS state depreciation basis prior ITC reduction [$]"),
 	NULL},
{"depr_stabas_prior_itc_macrs_5", (getter)Outputs_get_depr_stabas_prior_itc_macrs_5,(setter)0,
	PyDoc_STR("*float*: 5-yr MACRS state depreciation basis prior ITC reduction [$]"),
 	NULL},
{"depr_stabas_prior_itc_sl_15", (getter)Outputs_get_depr_stabas_prior_itc_sl_15,(setter)0,
	PyDoc_STR("*float*: 15-yr straight line state depreciation basis prior ITC reduction [$]"),
 	NULL},
{"depr_stabas_prior_itc_sl_20", (getter)Outputs_get_depr_stabas_prior_itc_sl_20,(setter)0,
	PyDoc_STR("*float*: 20-yr straight line state depreciation basis prior ITC reduction [$]"),
 	NULL},
{"depr_stabas_prior_itc_sl_39", (getter)Outputs_get_depr_stabas_prior_itc_sl_39,(setter)0,
	PyDoc_STR("*float*: 39-yr straight line state depreciation basis prior ITC reduction [$]"),
 	NULL},
{"depr_stabas_prior_itc_sl_5", (getter)Outputs_get_depr_stabas_prior_itc_sl_5,(setter)0,
	PyDoc_STR("*float*: 5-yr straight line state depreciation basis prior ITC reduction [$]"),
 	NULL},
{"depr_stabas_prior_itc_total", (getter)Outputs_get_depr_stabas_prior_itc_total,(setter)0,
	PyDoc_STR("*float*: Total state depreciation basis prior ITC reduction [$]"),
 	NULL},
{"depr_stabas_sl_15", (getter)Outputs_get_depr_stabas_sl_15,(setter)0,
	PyDoc_STR("*float*: 15-yr straight line state depreciation basis [$]"),
 	NULL},
{"depr_stabas_sl_20", (getter)Outputs_get_depr_stabas_sl_20,(setter)0,
	PyDoc_STR("*float*: 20-yr straight line state depreciation basis [$]"),
 	NULL},
{"depr_stabas_sl_39", (getter)Outputs_get_depr_stabas_sl_39,(setter)0,
	PyDoc_STR("*float*: 39-yr straight line state depreciation basis [$]"),
 	NULL},
{"depr_stabas_sl_5", (getter)Outputs_get_depr_stabas_sl_5,(setter)0,
	PyDoc_STR("*float*: 5-yr straight line state depreciation basis [$]"),
 	NULL},
{"depr_stabas_total", (getter)Outputs_get_depr_stabas_total,(setter)0,
	PyDoc_STR("*float*: Total state depreciation basis [$]"),
 	NULL},
{"effective_tax_rate", (getter)Outputs_get_effective_tax_rate,(setter)0,
	PyDoc_STR("*float*: Effective tax rate [%]"),
 	NULL},
{"firstyear_energy_dispatch1", (getter)Outputs_get_firstyear_energy_dispatch1,(setter)0,
	PyDoc_STR("*float*: First year energy from the system in TOD period 1 [kWh]"),
 	NULL},
{"firstyear_energy_dispatch2", (getter)Outputs_get_firstyear_energy_dispatch2,(setter)0,
	PyDoc_STR("*float*: First year energy from the system in TOD period 2 [kWh]"),
 	NULL},
{"firstyear_energy_dispatch3", (getter)Outputs_get_firstyear_energy_dispatch3,(setter)0,
	PyDoc_STR("*float*: First year energy from the system in TOD period 3 [kWh]"),
 	NULL},
{"firstyear_energy_dispatch4", (getter)Outputs_get_firstyear_energy_dispatch4,(setter)0,
	PyDoc_STR("*float*: First year energy from the system in TOD period 4 [kWh]"),
 	NULL},
{"firstyear_energy_dispatch5", (getter)Outputs_get_firstyear_energy_dispatch5,(setter)0,
	PyDoc_STR("*float*: First year energy from the system in TOD period 5 [kWh]"),
 	NULL},
{"firstyear_energy_dispatch6", (getter)Outputs_get_firstyear_energy_dispatch6,(setter)0,
	PyDoc_STR("*float*: First year energy from the system in TOD period 6 [kWh]"),
 	NULL},
{"firstyear_energy_dispatch7", (getter)Outputs_get_firstyear_energy_dispatch7,(setter)0,
	PyDoc_STR("*float*: First year energy from the system in TOD period 7 [kWh]"),
 	NULL},
{"firstyear_energy_dispatch8", (getter)Outputs_get_firstyear_energy_dispatch8,(setter)0,
	PyDoc_STR("*float*: First year energy from the system in TOD period 8 [kWh]"),
 	NULL},
{"firstyear_energy_dispatch9", (getter)Outputs_get_firstyear_energy_dispatch9,(setter)0,
	PyDoc_STR("*float*: First year energy from the system in TOD period 9 [kWh]"),
 	NULL},
{"firstyear_energy_price1", (getter)Outputs_get_firstyear_energy_price1,(setter)0,
	PyDoc_STR("*float*: First year energy price for TOD period 1 [cents/kWh]"),
 	NULL},
{"firstyear_energy_price2", (getter)Outputs_get_firstyear_energy_price2,(setter)0,
	PyDoc_STR("*float*: First year energy price for TOD period 2 [cents/kWh]"),
 	NULL},
{"firstyear_energy_price3", (getter)Outputs_get_firstyear_energy_price3,(setter)0,
	PyDoc_STR("*float*: First year energy price for TOD period 3 [cents/kWh]"),
 	NULL},
{"firstyear_energy_price4", (getter)Outputs_get_firstyear_energy_price4,(setter)0,
	PyDoc_STR("*float*: First year energy price for TOD period 4 [cents/kWh]"),
 	NULL},
{"firstyear_energy_price5", (getter)Outputs_get_firstyear_energy_price5,(setter)0,
	PyDoc_STR("*float*: First year energy price for TOD period 5 [cents/kWh]"),
 	NULL},
{"firstyear_energy_price6", (getter)Outputs_get_firstyear_energy_price6,(setter)0,
	PyDoc_STR("*float*: First year energy price for TOD period 6 [cents/kWh]"),
 	NULL},
{"firstyear_energy_price7", (getter)Outputs_get_firstyear_energy_price7,(setter)0,
	PyDoc_STR("*float*: First year energy price for TOD period 7 [cents/kWh]"),
 	NULL},
{"firstyear_energy_price8", (getter)Outputs_get_firstyear_energy_price8,(setter)0,
	PyDoc_STR("*float*: First year energy price for TOD period 8 [cents/kWh]"),
 	NULL},
{"firstyear_energy_price9", (getter)Outputs_get_firstyear_energy_price9,(setter)0,
	PyDoc_STR("*float*: First year energy price for TOD period 9 [cents/kWh]"),
 	NULL},
{"firstyear_revenue_dispatch1", (getter)Outputs_get_firstyear_revenue_dispatch1,(setter)0,
	PyDoc_STR("*float*: First year revenue from the system in TOD period 1 [$]"),
 	NULL},
{"firstyear_revenue_dispatch2", (getter)Outputs_get_firstyear_revenue_dispatch2,(setter)0,
	PyDoc_STR("*float*: First year revenue from the system in TOD period 2 [$]"),
 	NULL},
{"firstyear_revenue_dispatch3", (getter)Outputs_get_firstyear_revenue_dispatch3,(setter)0,
	PyDoc_STR("*float*: First year revenue from the system in TOD period 3 [$]"),
 	NULL},
{"firstyear_revenue_dispatch4", (getter)Outputs_get_firstyear_revenue_dispatch4,(setter)0,
	PyDoc_STR("*float*: First year revenue from the system in TOD period 4 [$]"),
 	NULL},
{"firstyear_revenue_dispatch5", (getter)Outputs_get_firstyear_revenue_dispatch5,(setter)0,
	PyDoc_STR("*float*: First year revenue from the system in TOD period 5 [$]"),
 	NULL},
{"firstyear_revenue_dispatch6", (getter)Outputs_get_firstyear_revenue_dispatch6,(setter)0,
	PyDoc_STR("*float*: First year revenue from the system in TOD period 6 [$]"),
 	NULL},
{"firstyear_revenue_dispatch7", (getter)Outputs_get_firstyear_revenue_dispatch7,(setter)0,
	PyDoc_STR("*float*: First year revenue from the system in TOD period 7 [$]"),
 	NULL},
{"firstyear_revenue_dispatch8", (getter)Outputs_get_firstyear_revenue_dispatch8,(setter)0,
	PyDoc_STR("*float*: First year revenue from the system in TOD period 8 [$]"),
 	NULL},
{"firstyear_revenue_dispatch9", (getter)Outputs_get_firstyear_revenue_dispatch9,(setter)0,
	PyDoc_STR("*float*: First year revenue from the system in TOD period 9 [$]"),
 	NULL},
{"flip_actual_irr", (getter)Outputs_get_flip_actual_irr,(setter)0,
	PyDoc_STR("*float*: IRR in target year [%]"),
 	NULL},
{"flip_actual_year", (getter)Outputs_get_flip_actual_year,(setter)0,
	PyDoc_STR("*float*: IRR actual year [%]"),
 	NULL},
{"flip_target_irr", (getter)Outputs_get_flip_target_irr,(setter)0,
	PyDoc_STR("*float*: IRR target [%]"),
 	NULL},
{"flip_target_year", (getter)Outputs_get_flip_target_year,(setter)0,
	PyDoc_STR("*float*: IRR target year"),
 	NULL},
{"ibi_fedtax_total", (getter)Outputs_get_ibi_fedtax_total,(setter)0,
	PyDoc_STR("*float*: Federal taxable IBI income [$]"),
 	NULL},
{"ibi_statax_total", (getter)Outputs_get_ibi_statax_total,(setter)0,
	PyDoc_STR("*float*: State taxable IBI income [$]"),
 	NULL},
{"ibi_total", (getter)Outputs_get_ibi_total,(setter)0,
	PyDoc_STR("*float*: Total IBI income [$]"),
 	NULL},
{"ibi_total_fed", (getter)Outputs_get_ibi_total_fed,(setter)0,
	PyDoc_STR("*float*: Federal IBI income [$]"),
 	NULL},
{"ibi_total_oth", (getter)Outputs_get_ibi_total_oth,(setter)0,
	PyDoc_STR("*float*: Other IBI income [$]"),
 	NULL},
{"ibi_total_sta", (getter)Outputs_get_ibi_total_sta,(setter)0,
	PyDoc_STR("*float*: State IBI income [$]"),
 	NULL},
{"ibi_total_uti", (getter)Outputs_get_ibi_total_uti,(setter)0,
	PyDoc_STR("*float*: Utility IBI income [$]"),
 	NULL},
{"issuance_of_equity", (getter)Outputs_get_issuance_of_equity,(setter)0,
	PyDoc_STR("*float*: Issuance of equity [$]"),
 	NULL},
{"itc_disallow_fed_fixed_custom", (getter)Outputs_get_itc_disallow_fed_fixed_custom,(setter)0,
	PyDoc_STR("*float*: Custom straight line depreciation ITC basis disallowance from federal fixed amount [$]"),
 	NULL},
{"itc_disallow_fed_fixed_macrs_15", (getter)Outputs_get_itc_disallow_fed_fixed_macrs_15,(setter)0,
	PyDoc_STR("*float*: 15-yr MACRS depreciation ITC basis disallowance from federal fixed amount [$]"),
 	NULL},
{"itc_disallow_fed_fixed_macrs_5", (getter)Outputs_get_itc_disallow_fed_fixed_macrs_5,(setter)0,
	PyDoc_STR("*float*: 5-yr MACRS depreciation ITC basis disallowance from federal fixed amount [$]"),
 	NULL},
{"itc_disallow_fed_fixed_sl_15", (getter)Outputs_get_itc_disallow_fed_fixed_sl_15,(setter)0,
	PyDoc_STR("*float*: 15-yr straight line depreciation ITC basis disallowance from federal fixed amount [$]"),
 	NULL},
{"itc_disallow_fed_fixed_sl_20", (getter)Outputs_get_itc_disallow_fed_fixed_sl_20,(setter)0,
	PyDoc_STR("*float*: 20-yr straight line depreciation ITC basis disallowance from federal fixed amount [$]"),
 	NULL},
{"itc_disallow_fed_fixed_sl_39", (getter)Outputs_get_itc_disallow_fed_fixed_sl_39,(setter)0,
	PyDoc_STR("*float*: 39-yr straight line depreciation ITC basis disallowance from federal fixed amount [$]"),
 	NULL},
{"itc_disallow_fed_fixed_sl_5", (getter)Outputs_get_itc_disallow_fed_fixed_sl_5,(setter)0,
	PyDoc_STR("*float*: 5-yr straight line depreciation ITC basis disallowance from federal fixed amount [$]"),
 	NULL},
{"itc_disallow_fed_fixed_total", (getter)Outputs_get_itc_disallow_fed_fixed_total,(setter)0,
	PyDoc_STR("*float*: Total depreciation ITC basis disallowance from federal fixed amount [$]"),
 	NULL},
{"itc_disallow_fed_percent_custom", (getter)Outputs_get_itc_disallow_fed_percent_custom,(setter)0,
	PyDoc_STR("*float*: Custom straight line depreciation ITC basis disallowance from federal percentage [$]"),
 	NULL},
{"itc_disallow_fed_percent_macrs_15", (getter)Outputs_get_itc_disallow_fed_percent_macrs_15,(setter)0,
	PyDoc_STR("*float*: 15-yr MACRS depreciation ITC basis disallowance from federal percentage [$]"),
 	NULL},
{"itc_disallow_fed_percent_macrs_5", (getter)Outputs_get_itc_disallow_fed_percent_macrs_5,(setter)0,
	PyDoc_STR("*float*: 5-yr MACRS depreciation ITC basis disallowance from federal percentage [$]"),
 	NULL},
{"itc_disallow_fed_percent_sl_15", (getter)Outputs_get_itc_disallow_fed_percent_sl_15,(setter)0,
	PyDoc_STR("*float*: 15-yr straight line depreciation ITC basis disallowance from federal percentage [$]"),
 	NULL},
{"itc_disallow_fed_percent_sl_20", (getter)Outputs_get_itc_disallow_fed_percent_sl_20,(setter)0,
	PyDoc_STR("*float*: 20-yr straight line depreciation ITC basis disallowance from federal percentage [$]"),
 	NULL},
{"itc_disallow_fed_percent_sl_39", (getter)Outputs_get_itc_disallow_fed_percent_sl_39,(setter)0,
	PyDoc_STR("*float*: 39-yr straight line depreciation ITC basis disallowance from federal percentage [$]"),
 	NULL},
{"itc_disallow_fed_percent_sl_5", (getter)Outputs_get_itc_disallow_fed_percent_sl_5,(setter)0,
	PyDoc_STR("*float*: 5-yr straight line depreciation ITC basis disallowance from federal percentage [$]"),
 	NULL},
{"itc_disallow_fed_percent_total", (getter)Outputs_get_itc_disallow_fed_percent_total,(setter)0,
	PyDoc_STR("*float*: Total depreciation ITC basis disallowance from federal percentage [$]"),
 	NULL},
{"itc_disallow_sta_fixed_custom", (getter)Outputs_get_itc_disallow_sta_fixed_custom,(setter)0,
	PyDoc_STR("*float*: Custom straight line depreciation ITC basis disallowance from state fixed amount [$]"),
 	NULL},
{"itc_disallow_sta_fixed_macrs_15", (getter)Outputs_get_itc_disallow_sta_fixed_macrs_15,(setter)0,
	PyDoc_STR("*float*: 15-yr MACRS depreciation ITC basis disallowance from state fixed amount [$]"),
 	NULL},
{"itc_disallow_sta_fixed_macrs_5", (getter)Outputs_get_itc_disallow_sta_fixed_macrs_5,(setter)0,
	PyDoc_STR("*float*: 5-yr MACRS depreciation ITC basis disallowance from state fixed amount [$]"),
 	NULL},
{"itc_disallow_sta_fixed_sl_15", (getter)Outputs_get_itc_disallow_sta_fixed_sl_15,(setter)0,
	PyDoc_STR("*float*: 15-yr straight line depreciation ITC basis disallowance from state fixed amount [$]"),
 	NULL},
{"itc_disallow_sta_fixed_sl_20", (getter)Outputs_get_itc_disallow_sta_fixed_sl_20,(setter)0,
	PyDoc_STR("*float*: 20-yr straight line depreciation ITC basis disallowance from state fixed amount [$]"),
 	NULL},
{"itc_disallow_sta_fixed_sl_39", (getter)Outputs_get_itc_disallow_sta_fixed_sl_39,(setter)0,
	PyDoc_STR("*float*: 39-yr straight line depreciation ITC basis disallowance from state fixed amount [$]"),
 	NULL},
{"itc_disallow_sta_fixed_sl_5", (getter)Outputs_get_itc_disallow_sta_fixed_sl_5,(setter)0,
	PyDoc_STR("*float*: 5-yr straight line depreciation ITC basis disallowance from state fixed amount [$]"),
 	NULL},
{"itc_disallow_sta_fixed_total", (getter)Outputs_get_itc_disallow_sta_fixed_total,(setter)0,
	PyDoc_STR("*float*: Total depreciation ITC basis disallowance from state fixed amount [$]"),
 	NULL},
{"itc_disallow_sta_percent_custom", (getter)Outputs_get_itc_disallow_sta_percent_custom,(setter)0,
	PyDoc_STR("*float*: Custom straight line depreciation ITC basis disallowance from state percentage [$]"),
 	NULL},
{"itc_disallow_sta_percent_macrs_15", (getter)Outputs_get_itc_disallow_sta_percent_macrs_15,(setter)0,
	PyDoc_STR("*float*: 15-yr MACRS depreciation ITC basis disallowance from state percentage [$]"),
 	NULL},
{"itc_disallow_sta_percent_macrs_5", (getter)Outputs_get_itc_disallow_sta_percent_macrs_5,(setter)0,
	PyDoc_STR("*float*: 5-yr MACRS depreciation ITC basis disallowance from state percentage [$]"),
 	NULL},
{"itc_disallow_sta_percent_sl_15", (getter)Outputs_get_itc_disallow_sta_percent_sl_15,(setter)0,
	PyDoc_STR("*float*: 15-yr straight line depreciation ITC basis disallowance from state percentage [$]"),
 	NULL},
{"itc_disallow_sta_percent_sl_20", (getter)Outputs_get_itc_disallow_sta_percent_sl_20,(setter)0,
	PyDoc_STR("*float*: 20-yr straight line depreciation ITC basis disallowance from state percentage [$]"),
 	NULL},
{"itc_disallow_sta_percent_sl_39", (getter)Outputs_get_itc_disallow_sta_percent_sl_39,(setter)0,
	PyDoc_STR("*float*: 39-yr straight line depreciation ITC basis disallowance from state percentage [$]"),
 	NULL},
{"itc_disallow_sta_percent_sl_5", (getter)Outputs_get_itc_disallow_sta_percent_sl_5,(setter)0,
	PyDoc_STR("*float*: 5-yr straight line depreciation ITC basis disallowance from state percentage [$]"),
 	NULL},
{"itc_disallow_sta_percent_total", (getter)Outputs_get_itc_disallow_sta_percent_total,(setter)0,
	PyDoc_STR("*float*: Total depreciation ITC basis disallowance from state percentage [$]"),
 	NULL},
{"itc_fed_fixed_total", (getter)Outputs_get_itc_fed_fixed_total,(setter)0,
	PyDoc_STR("*float*: Federal ITC fixed total [$]"),
 	NULL},
{"itc_fed_percent_total", (getter)Outputs_get_itc_fed_percent_total,(setter)0,
	PyDoc_STR("*float*: Federal ITC percent total [$]"),
 	NULL},
{"itc_fed_qual_custom", (getter)Outputs_get_itc_fed_qual_custom,(setter)0,
	PyDoc_STR("*float*: Custom straight line depreciation federal ITC adj qualifying costs [$]"),
 	NULL},
{"itc_fed_qual_macrs_15", (getter)Outputs_get_itc_fed_qual_macrs_15,(setter)0,
	PyDoc_STR("*float*: 15-yr MACRS depreciation federal ITC adj qualifying costs [$]"),
 	NULL},
{"itc_fed_qual_macrs_5", (getter)Outputs_get_itc_fed_qual_macrs_5,(setter)0,
	PyDoc_STR("*float*: 5-yr MACRS depreciation federal ITC adj qualifying costs [$]"),
 	NULL},
{"itc_fed_qual_sl_15", (getter)Outputs_get_itc_fed_qual_sl_15,(setter)0,
	PyDoc_STR("*float*: 15-yr straight line depreciation federal ITC adj qualifying costs [$]"),
 	NULL},
{"itc_fed_qual_sl_20", (getter)Outputs_get_itc_fed_qual_sl_20,(setter)0,
	PyDoc_STR("*float*: 20-yr straight line depreciation federal ITC adj qualifying costs [$]"),
 	NULL},
{"itc_fed_qual_sl_39", (getter)Outputs_get_itc_fed_qual_sl_39,(setter)0,
	PyDoc_STR("*float*: 39-yr straight line depreciation federal ITC adj qualifying costs [$]"),
 	NULL},
{"itc_fed_qual_sl_5", (getter)Outputs_get_itc_fed_qual_sl_5,(setter)0,
	PyDoc_STR("*float*: 5-yr straight line depreciation federal ITC adj qualifying costs [$]"),
 	NULL},
{"itc_fed_qual_total", (getter)Outputs_get_itc_fed_qual_total,(setter)0,
	PyDoc_STR("*float*: Total federal ITC adj qualifying costs [$]"),
 	NULL},
{"itc_sta_fixed_total", (getter)Outputs_get_itc_sta_fixed_total,(setter)0,
	PyDoc_STR("*float*: State ITC fixed total [$]"),
 	NULL},
{"itc_sta_percent_total", (getter)Outputs_get_itc_sta_percent_total,(setter)0,
	PyDoc_STR("*float*: State ITC percent total [$]"),
 	NULL},
{"itc_sta_qual_custom", (getter)Outputs_get_itc_sta_qual_custom,(setter)0,
	PyDoc_STR("*float*: Custom straight line depreciation state ITC adj qualifying costs [$]"),
 	NULL},
{"itc_sta_qual_macrs_15", (getter)Outputs_get_itc_sta_qual_macrs_15,(setter)0,
	PyDoc_STR("*float*: 15-yr MACRS depreciation state ITC adj qualifying costs [$]"),
 	NULL},
{"itc_sta_qual_macrs_5", (getter)Outputs_get_itc_sta_qual_macrs_5,(setter)0,
	PyDoc_STR("*float*: 5-yr MACRS depreciation state ITC adj qualifying costs [$]"),
 	NULL},
{"itc_sta_qual_sl_15", (getter)Outputs_get_itc_sta_qual_sl_15,(setter)0,
	PyDoc_STR("*float*: 15-yr straight line depreciation state ITC adj qualifying costs [$]"),
 	NULL},
{"itc_sta_qual_sl_20", (getter)Outputs_get_itc_sta_qual_sl_20,(setter)0,
	PyDoc_STR("*float*: 20-yr straight line depreciation state ITC adj qualifying costs [$]"),
 	NULL},
{"itc_sta_qual_sl_39", (getter)Outputs_get_itc_sta_qual_sl_39,(setter)0,
	PyDoc_STR("*float*: 39-yr straight line depreciation state ITC adj qualifying costs [$]"),
 	NULL},
{"itc_sta_qual_sl_5", (getter)Outputs_get_itc_sta_qual_sl_5,(setter)0,
	PyDoc_STR("*float*: 5-yr straight line depreciation state ITC adj qualifying costs [$]"),
 	NULL},
{"itc_sta_qual_total", (getter)Outputs_get_itc_sta_qual_total,(setter)0,
	PyDoc_STR("*float*: Total state ITC adj qualifying costs [$]"),
 	NULL},
{"itc_total", (getter)Outputs_get_itc_total,(setter)0,
	PyDoc_STR("*float*: Total ITC  [$]"),
 	NULL},
{"itc_total_fed", (getter)Outputs_get_itc_total_fed,(setter)0,
	PyDoc_STR("*float*: Federal ITC income [$]"),
 	NULL},
{"itc_total_sta", (getter)Outputs_get_itc_total_sta,(setter)0,
	PyDoc_STR("*float*: State ITC income [$]"),
 	NULL},
{"lcoe_nom", (getter)Outputs_get_lcoe_nom,(setter)0,
	PyDoc_STR("*float*: Levelized cost (nominal) [cents/kWh]"),
 	NULL},
{"lcoe_real", (getter)Outputs_get_lcoe_real,(setter)0,
	PyDoc_STR("*float*: Levelized cost (real) [cents/kWh]"),
 	NULL},
{"lcoptc_fed_nom", (getter)Outputs_get_lcoptc_fed_nom,(setter)0,
	PyDoc_STR("*float*: Levelized federal PTC (nominal) [cents/kWh]"),
 	NULL},
{"lcoptc_fed_real", (getter)Outputs_get_lcoptc_fed_real,(setter)0,
	PyDoc_STR("*float*: Levelized federal PTC (real) [cents/kWh]"),
 	NULL},
{"lcoptc_sta_nom", (getter)Outputs_get_lcoptc_sta_nom,(setter)0,
	PyDoc_STR("*float*: Levelized state PTC (nominal) [cents/kWh]"),
 	NULL},
{"lcoptc_sta_real", (getter)Outputs_get_lcoptc_sta_real,(setter)0,
	PyDoc_STR("*float*: Levelized state PTC (real) [cents/kWh]"),
 	NULL},
{"lppa_nom", (getter)Outputs_get_lppa_nom,(setter)0,
	PyDoc_STR("*float*: Levelized PPA price (nominal) [cents/kWh]"),
 	NULL},
{"lppa_real", (getter)Outputs_get_lppa_real,(setter)0,
	PyDoc_STR("*float*: Levelized PPA price (real) [cents/kWh]"),
 	NULL},
{"min_dscr", (getter)Outputs_get_min_dscr,(setter)0,
	PyDoc_STR("*float*: Minimum DSCR"),
 	NULL},
{"nominal_discount_rate", (getter)Outputs_get_nominal_discount_rate,(setter)0,
	PyDoc_STR("*float*: Nominal discount rate [%]"),
 	NULL},
{"npv_annual_costs", (getter)Outputs_get_npv_annual_costs,(setter)0,
	PyDoc_STR("*float*: Present value of annual costs [$]"),
 	NULL},
{"npv_energy_nom", (getter)Outputs_get_npv_energy_nom,(setter)0,
	PyDoc_STR("*float*: Present value of annual energy (nominal) [kWh]"),
 	NULL},
{"npv_energy_real", (getter)Outputs_get_npv_energy_real,(setter)0,
	PyDoc_STR("*float*: Present value annual energy (real) [kWh]"),
 	NULL},
{"npv_ppa_revenue", (getter)Outputs_get_npv_ppa_revenue,(setter)0,
	PyDoc_STR("*float*: Present value PPA project revenue [$]"),
 	NULL},
{"ppa", (getter)Outputs_get_ppa,(setter)0,
	PyDoc_STR("*float*: PPA price [cents/kWh]"),
 	NULL},
{"ppa_escalation", (getter)Outputs_get_ppa_escalation,(setter)0,
	PyDoc_STR("*float*: PPA price escalation [%]"),
 	NULL},
{"ppa_multipliers", (getter)Outputs_get_ppa_multipliers,(setter)0,
	PyDoc_STR("*sequence*: TOD factors"),
 	NULL},
{"ppa_price", (getter)Outputs_get_ppa_price,(setter)0,
	PyDoc_STR("*float*: Initial year PPA price [cents/kWh]"),
 	NULL},
{"present_value_fuel", (getter)Outputs_get_present_value_fuel,(setter)0,
	PyDoc_STR("*float*: Present value of fuel O and M [$]"),
 	NULL},
{"present_value_insandproptax", (getter)Outputs_get_present_value_insandproptax,(setter)0,
	PyDoc_STR("*float*: Present value of Insurance and Prop Tax [$]"),
 	NULL},
{"present_value_oandm", (getter)Outputs_get_present_value_oandm,(setter)0,
	PyDoc_STR("*float*: Present value of O and M [$]"),
 	NULL},
{"present_value_oandm_nonfuel", (getter)Outputs_get_present_value_oandm_nonfuel,(setter)0,
	PyDoc_STR("*float*: Present value of non-fuel O and M [$]"),
 	NULL},
{"prop_tax_assessed_value", (getter)Outputs_get_prop_tax_assessed_value,(setter)0,
	PyDoc_STR("*float*: Assessed value of property for tax purposes [$]"),
 	NULL},
{"purchase_of_property", (getter)Outputs_get_purchase_of_property,(setter)0,
	PyDoc_STR("*float*: Purchase of property [$]"),
 	NULL},
{"pv_cafds", (getter)Outputs_get_pv_cafds,(setter)0,
	PyDoc_STR("*float*: Present value of cash available for debt service [$]"),
 	NULL},
{"salvage_value", (getter)Outputs_get_salvage_value,(setter)0,
	PyDoc_STR("*float*: Net pre-tax cash salvage value [$]"),
 	NULL},
{"size_of_debt", (getter)Outputs_get_size_of_debt,(setter)0,
	PyDoc_STR("*float*: Total debt"),
 	NULL},
{"size_of_equity", (getter)Outputs_get_size_of_equity,(setter)0,
	PyDoc_STR("*float*: Total equity"),
 	NULL},
{"sponsor_aftertax_development", (getter)Outputs_get_sponsor_aftertax_development,(setter)0,
	PyDoc_STR("*float*: Developer after-tax development fee [$]"),
 	NULL},
{"sponsor_aftertax_equity", (getter)Outputs_get_sponsor_aftertax_equity,(setter)0,
	PyDoc_STR("*float*: Developer equity investment [$]"),
 	NULL},
{"sponsor_aftertax_irr", (getter)Outputs_get_sponsor_aftertax_irr,(setter)0,
	PyDoc_STR("*float*: Developer after-tax IRR [%]"),
 	NULL},
{"sponsor_aftertax_npv", (getter)Outputs_get_sponsor_aftertax_npv,(setter)0,
	PyDoc_STR("*float*: Developer after-tax NPV [$]"),
 	NULL},
{"sponsor_pretax_development", (getter)Outputs_get_sponsor_pretax_development,(setter)0,
	PyDoc_STR("*float*: Development fee [$]"),
 	NULL},
{"sponsor_pretax_equity", (getter)Outputs_get_sponsor_pretax_equity,(setter)0,
	PyDoc_STR("*float*: Developer equity investment [$]"),
 	NULL},
{"sponsor_pretax_irr", (getter)Outputs_get_sponsor_pretax_irr,(setter)0,
	PyDoc_STR("*float*: Developer pre-tax IRR [%]"),
 	NULL},
{"sponsor_pretax_npv", (getter)Outputs_get_sponsor_pretax_npv,(setter)0,
	PyDoc_STR("*float*: Developer pre-tax NPV [$]"),
 	NULL},
{"tax_investor_aftertax_irr", (getter)Outputs_get_tax_investor_aftertax_irr,(setter)0,
	PyDoc_STR("*float*: Tax investor after-tax IRR [%]"),
 	NULL},
{"tax_investor_aftertax_npv", (getter)Outputs_get_tax_investor_aftertax_npv,(setter)0,
	PyDoc_STR("*float*: Tax investor after-tax NPV [$]"),
 	NULL},
{"tax_investor_pretax_irr", (getter)Outputs_get_tax_investor_pretax_irr,(setter)0,
	PyDoc_STR("*float*: Tax investor pre-tax IRR [%]"),
 	NULL},
{"tax_investor_pretax_npv", (getter)Outputs_get_tax_investor_pretax_npv,(setter)0,
	PyDoc_STR("*float*: Tax investor pre-tax NPV [$]"),
 	NULL},
{"wacc", (getter)Outputs_get_wacc,(setter)0,
	PyDoc_STR("*float*: Weighted average cost of capital (WACC)"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject Outputs_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Levpartflip.Outputs",             /*tp_name*/
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
 * Levpartflip
 */

typedef struct {
	PyObject_HEAD
	PyObject            *x_attr;        /* Attributes dictionary */
	SAM_Levpartflip   data_ptr;
} LevpartflipObject;

static PyTypeObject Levpartflip_Type;

#define LevpartflipObject_Check(v)      (Py_TYPE(v) == &Levpartflip_Type)

static LevpartflipObject *
newLevpartflipObject(void* data_ptr)
{
	LevpartflipObject *self;
	self = PyObject_New(LevpartflipObject, &Levpartflip_Type);

	PySAM_TECH_ATTR("Levpartflip", SAM_Levpartflip_construct)

	PyObject* FinancialParameters_obj = FinancialParameters_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "FinancialParameters", FinancialParameters_obj);
	Py_DECREF(FinancialParameters_obj);

	PyObject* SystemCosts_obj = SystemCosts_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "SystemCosts", SystemCosts_obj);
	Py_DECREF(SystemCosts_obj);

	PyObject* TaxCreditIncentives_obj = TaxCreditIncentives_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "TaxCreditIncentives", TaxCreditIncentives_obj);
	Py_DECREF(TaxCreditIncentives_obj);

	PyObject* PaymentIncentives_obj = PaymentIncentives_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "PaymentIncentives", PaymentIncentives_obj);
	Py_DECREF(PaymentIncentives_obj);

	PyObject* SystemOutput_obj = SystemOutput_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "SystemOutput", SystemOutput_obj);
	Py_DECREF(SystemOutput_obj);

	PyObject* Moratorium_obj = Moratorium_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "Moratorium", Moratorium_obj);
	Py_DECREF(Moratorium_obj);

	PyObject* Recapitalization_obj = Recapitalization_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "Recapitalization", Recapitalization_obj);
	Py_DECREF(Recapitalization_obj);

	PyObject* TimeOfDelivery_obj = TimeOfDelivery_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "TimeOfDelivery", TimeOfDelivery_obj);
	Py_DECREF(TimeOfDelivery_obj);

	PyObject* ReserveAccounts_obj = ReserveAccounts_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "ReserveAccounts", ReserveAccounts_obj);
	Py_DECREF(ReserveAccounts_obj);

	PyObject* Depreciation_obj = Depreciation_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "Depreciation", Depreciation_obj);
	Py_DECREF(Depreciation_obj);

	PyObject* SalvageValue_obj = SalvageValue_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "SalvageValue", SalvageValue_obj);
	Py_DECREF(SalvageValue_obj);

	PyObject* SolutionMode_obj = SolutionMode_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "SolutionMode", SolutionMode_obj);
	Py_DECREF(SolutionMode_obj);

	PyObject* ConstructionFinancing_obj = ConstructionFinancing_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "ConstructionFinancing", ConstructionFinancing_obj);
	Py_DECREF(ConstructionFinancing_obj);

	PyObject* ProjectTermDebt_obj = ProjectTermDebt_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "ProjectTermDebt", ProjectTermDebt_obj);
	Py_DECREF(ProjectTermDebt_obj);

	PyObject* OtherCapitalCosts_obj = OtherCapitalCosts_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "OtherCapitalCosts", OtherCapitalCosts_obj);
	Py_DECREF(OtherCapitalCosts_obj);

	PyObject* IRRTargets_obj = IRRTargets_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "IRRTargets", IRRTargets_obj);
	Py_DECREF(IRRTargets_obj);

	PyObject* CashIncentives_obj = CashIncentives_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "CashIncentives", CashIncentives_obj);
	Py_DECREF(CashIncentives_obj);

	PyObject* Battery_obj = Battery_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "Battery", Battery_obj);
	Py_DECREF(Battery_obj);

	PyObject* Outputs_obj = Outputs_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "Outputs", Outputs_obj);
	Py_DECREF(Outputs_obj);


	return self;
}

/* Levpartflip methods */

static void
Levpartflip_dealloc(LevpartflipObject *self)
{
	Py_XDECREF(self->x_attr);
	SAM_Levpartflip_destruct(self->data_ptr);
	PyObject_Del(self);
}


static PyObject *
Levpartflip_execute(LevpartflipObject *self, PyObject *args)
{
	int verbosity = 0;

	if (!PyArg_ParseTuple(args, "|i", &verbosity))
		return NULL;

	SAM_error error = new_error();
	SAM_Levpartflip_execute(self->data_ptr, verbosity, &error);
	if (PySAM_has_error(error )) return NULL;

	Py_INCREF(Py_None);
	return Py_None;
}


static PyObject *
Levpartflip_assign(LevpartflipObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_nested_dict((PyObject*)self, self->x_attr, self->data_ptr, dict, "Levpartflip"))
		return NULL;

	Py_INCREF(Py_None);
	return Py_None;
}


static PyObject *
Levpartflip_export(LevpartflipObject *self, PyObject *args)
{
	return PySAM_export_to_nested_dict((PyObject *) self, self->x_attr);
}

static PyMethodDef Levpartflip_methods[] = {
		{"execute",            (PyCFunction)Levpartflip_execute,  METH_VARARGS,
				PyDoc_STR("execute(int verbosity) -> None\n Execute simulation with verbosity level 0 (default) or 1")},
		{"assign",            (PyCFunction)Levpartflip_assign,  METH_VARARGS,
				PyDoc_STR("assign(dict) -> None\n Assign attributes from nested dictionary, except for Outputs\n\n``nested_dict = { 'Financial Parameters': { var: val, ...}, ...}``")},
		{"export",            (PyCFunction)Levpartflip_export,  METH_VARARGS,
				PyDoc_STR("export() -> dict\n Export attributes into nested dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
Levpartflip_getattro(LevpartflipObject *self, PyObject *name)
{
	return PySAM_get_attr((PyObject*) self, (PyObject*) self->x_attr, name);
}

static int
Levpartflip_setattr(LevpartflipObject *self, const char *name, PyObject *v)
{
	return PySAM_set_attr((PyObject*)self, (PyObject*)self->x_attr, name, v);
}

static PyTypeObject Levpartflip_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Levpartflip",            /*tp_name*/
		sizeof(LevpartflipObject),/*tp_basicsize*/
		0,                          /*tp_itemsize*/
		/* methods */
		(destructor)Levpartflip_dealloc,    /*tp_dealloc*/
		0,                          /*tp_print*/
		(getattrfunc)0,             /*tp_getattr*/
		(setattrfunc)Levpartflip_setattr,   /*tp_setattr*/
		0,                          /*tp_reserved*/
		0,                          /*tp_repr*/
		0,                          /*tp_as_number*/
		0,                          /*tp_as_sequence*/
		0,                          /*tp_as_mapping*/
		0,                          /*tp_hash*/
		0,                          /*tp_call*/
		0,                          /*tp_str*/
		(getattrofunc)Levpartflip_getattro, /*tp_getattro*/
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
		Levpartflip_methods,      /*tp_methods*/
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


/* Function of no arguments returning new Levpartflip object */

static PyObject *
Levpartflip_new(PyObject *self, PyObject *args)
{
	LevpartflipObject *rv;
	rv = newLevpartflipObject(0);
	if (rv == NULL)
		return NULL;
	return (PyObject *)rv;
}

static PyObject *
Levpartflip_wrap(PyObject *self, PyObject *args)
{
	LevpartflipObject *rv;
	long long int ptr = 0;  // 64 bit arch
	if (!PyArg_ParseTuple(args, "L:wrap", &ptr)){
		PyErr_BadArgument();
		return NULL;
	}
	rv = newLevpartflipObject((void*)ptr);
	if (rv == NULL)
		return NULL;
	return (PyObject *)rv;
}

static PyObject *
Levpartflip_default(PyObject *self, PyObject *args)
{
	LevpartflipObject *rv;
	char* def = 0;
	if (!PyArg_ParseTuple(args, "s:default", &def)){
		PyErr_BadArgument();
		return NULL;
	}
	rv = newLevpartflipObject(0);
	if (rv == NULL)
		return NULL;

	PySAM_load_defaults((PyObject*)rv, rv->x_attr, rv->data_ptr, "Levpartflip", def);

	return (PyObject *)rv;
}

/* ---------- */


/* List of functions defined in the module */

static PyMethodDef LevpartflipModule_methods[] = {
		{"new",             Levpartflip_new,         METH_VARARGS,
				PyDoc_STR("new() -> Levpartflip")},
		{"default",             Levpartflip_default,         METH_VARARGS,
				PyDoc_STR("default(config) -> Levpartflip\n\nUse financial model-specific default attributes\n"
				"config options:\n\n- \"BiopowerLeveragedPartnershipFlip\"\n- \"DSLFLeveragedPartnershipFlip\"\n- \"DSPTLeveragedPartnershipFlip\"\n- \"DishStirlingLeveragedPartnershipFlip\"\n- \"EmpiricalTroughLeveragedPartnershipFlip\"\n- \"FlatPlatePVLeveragedPartnershipFlip\"\n- \"GenericCSPSystemLeveragedPartnershipFlip\"\n- \"GenericSystemLeveragedPartnershipFlip\"\n- \"GeothermalPowerLeveragedPartnershipFlip\"\n- \"HighXConcentratingPVLeveragedPartnershipFlip\"\n- \"MSLFLeveragedPartnershipFlip\"\n- \"MSPTLeveragedPartnershipFlip\"\n- \"PVWattsLeveragedPartnershipFlip\"\n- \"PhysicalTroughLeveragedPartnershipFlip\"\n- \"WindPowerLeveragedPartnershipFlip\"")},
		{"wrap",             Levpartflip_wrap,         METH_VARARGS,
				PyDoc_STR("wrap(ssc_data_t) -> Levpartflip\n\nUse existing PySSC data\n\n.. warning::\n\n	Do not call PySSC.data_free on the ssc_data_t provided to ``wrap``")},
		{NULL,              NULL}           /* sentinel */
};

PyDoc_STRVAR(module_doc,
			 "PPA leveraged partnership flip (with debt) financial model");


static int
LevpartflipModule_exec(PyObject *m)
{
	/* Finalize the type object including setting type of the new type
	 * object; doing it here is required for portability, too. */

	if (PySAM_load_lib(m) < 0) goto fail;
	if (PySAM_init_error(m) < 0) goto fail;

	Levpartflip_Type.tp_dict = PyDict_New();
	if (!Levpartflip_Type.tp_dict) { goto fail; }

	/// Add the FinancialParameters type object to Levpartflip_Type
	if (PyType_Ready(&FinancialParameters_Type) < 0) { goto fail; }
	PyDict_SetItemString(Levpartflip_Type.tp_dict,
				"FinancialParameters",
				(PyObject*)&FinancialParameters_Type);
	Py_DECREF(&FinancialParameters_Type);

	/// Add the SystemCosts type object to Levpartflip_Type
	if (PyType_Ready(&SystemCosts_Type) < 0) { goto fail; }
	PyDict_SetItemString(Levpartflip_Type.tp_dict,
				"SystemCosts",
				(PyObject*)&SystemCosts_Type);
	Py_DECREF(&SystemCosts_Type);

	/// Add the TaxCreditIncentives type object to Levpartflip_Type
	if (PyType_Ready(&TaxCreditIncentives_Type) < 0) { goto fail; }
	PyDict_SetItemString(Levpartflip_Type.tp_dict,
				"TaxCreditIncentives",
				(PyObject*)&TaxCreditIncentives_Type);
	Py_DECREF(&TaxCreditIncentives_Type);

	/// Add the PaymentIncentives type object to Levpartflip_Type
	if (PyType_Ready(&PaymentIncentives_Type) < 0) { goto fail; }
	PyDict_SetItemString(Levpartflip_Type.tp_dict,
				"PaymentIncentives",
				(PyObject*)&PaymentIncentives_Type);
	Py_DECREF(&PaymentIncentives_Type);

	/// Add the SystemOutput type object to Levpartflip_Type
	if (PyType_Ready(&SystemOutput_Type) < 0) { goto fail; }
	PyDict_SetItemString(Levpartflip_Type.tp_dict,
				"SystemOutput",
				(PyObject*)&SystemOutput_Type);
	Py_DECREF(&SystemOutput_Type);

	/// Add the Moratorium type object to Levpartflip_Type
	if (PyType_Ready(&Moratorium_Type) < 0) { goto fail; }
	PyDict_SetItemString(Levpartflip_Type.tp_dict,
				"Moratorium",
				(PyObject*)&Moratorium_Type);
	Py_DECREF(&Moratorium_Type);

	/// Add the Recapitalization type object to Levpartflip_Type
	if (PyType_Ready(&Recapitalization_Type) < 0) { goto fail; }
	PyDict_SetItemString(Levpartflip_Type.tp_dict,
				"Recapitalization",
				(PyObject*)&Recapitalization_Type);
	Py_DECREF(&Recapitalization_Type);

	/// Add the TimeOfDelivery type object to Levpartflip_Type
	if (PyType_Ready(&TimeOfDelivery_Type) < 0) { goto fail; }
	PyDict_SetItemString(Levpartflip_Type.tp_dict,
				"TimeOfDelivery",
				(PyObject*)&TimeOfDelivery_Type);
	Py_DECREF(&TimeOfDelivery_Type);

	/// Add the ReserveAccounts type object to Levpartflip_Type
	if (PyType_Ready(&ReserveAccounts_Type) < 0) { goto fail; }
	PyDict_SetItemString(Levpartflip_Type.tp_dict,
				"ReserveAccounts",
				(PyObject*)&ReserveAccounts_Type);
	Py_DECREF(&ReserveAccounts_Type);

	/// Add the Depreciation type object to Levpartflip_Type
	if (PyType_Ready(&Depreciation_Type) < 0) { goto fail; }
	PyDict_SetItemString(Levpartflip_Type.tp_dict,
				"Depreciation",
				(PyObject*)&Depreciation_Type);
	Py_DECREF(&Depreciation_Type);

	/// Add the SalvageValue type object to Levpartflip_Type
	if (PyType_Ready(&SalvageValue_Type) < 0) { goto fail; }
	PyDict_SetItemString(Levpartflip_Type.tp_dict,
				"SalvageValue",
				(PyObject*)&SalvageValue_Type);
	Py_DECREF(&SalvageValue_Type);

	/// Add the SolutionMode type object to Levpartflip_Type
	if (PyType_Ready(&SolutionMode_Type) < 0) { goto fail; }
	PyDict_SetItemString(Levpartflip_Type.tp_dict,
				"SolutionMode",
				(PyObject*)&SolutionMode_Type);
	Py_DECREF(&SolutionMode_Type);

	/// Add the ConstructionFinancing type object to Levpartflip_Type
	if (PyType_Ready(&ConstructionFinancing_Type) < 0) { goto fail; }
	PyDict_SetItemString(Levpartflip_Type.tp_dict,
				"ConstructionFinancing",
				(PyObject*)&ConstructionFinancing_Type);
	Py_DECREF(&ConstructionFinancing_Type);

	/// Add the ProjectTermDebt type object to Levpartflip_Type
	if (PyType_Ready(&ProjectTermDebt_Type) < 0) { goto fail; }
	PyDict_SetItemString(Levpartflip_Type.tp_dict,
				"ProjectTermDebt",
				(PyObject*)&ProjectTermDebt_Type);
	Py_DECREF(&ProjectTermDebt_Type);

	/// Add the OtherCapitalCosts type object to Levpartflip_Type
	if (PyType_Ready(&OtherCapitalCosts_Type) < 0) { goto fail; }
	PyDict_SetItemString(Levpartflip_Type.tp_dict,
				"OtherCapitalCosts",
				(PyObject*)&OtherCapitalCosts_Type);
	Py_DECREF(&OtherCapitalCosts_Type);

	/// Add the IRRTargets type object to Levpartflip_Type
	if (PyType_Ready(&IRRTargets_Type) < 0) { goto fail; }
	PyDict_SetItemString(Levpartflip_Type.tp_dict,
				"IRRTargets",
				(PyObject*)&IRRTargets_Type);
	Py_DECREF(&IRRTargets_Type);

	/// Add the CashIncentives type object to Levpartflip_Type
	if (PyType_Ready(&CashIncentives_Type) < 0) { goto fail; }
	PyDict_SetItemString(Levpartflip_Type.tp_dict,
				"CashIncentives",
				(PyObject*)&CashIncentives_Type);
	Py_DECREF(&CashIncentives_Type);

	/// Add the Battery type object to Levpartflip_Type
	if (PyType_Ready(&Battery_Type) < 0) { goto fail; }
	PyDict_SetItemString(Levpartflip_Type.tp_dict,
				"Battery",
				(PyObject*)&Battery_Type);
	Py_DECREF(&Battery_Type);

	/// Add the Outputs type object to Levpartflip_Type
	if (PyType_Ready(&Outputs_Type) < 0) { goto fail; }
	PyDict_SetItemString(Levpartflip_Type.tp_dict,
				"Outputs",
				(PyObject*)&Outputs_Type);
	Py_DECREF(&Outputs_Type);

	/// Add the Levpartflip type object to the module
	if (PyType_Ready(&Levpartflip_Type) < 0) { goto fail; }
	PyModule_AddObject(m,
				"Levpartflip",
				(PyObject*)&Levpartflip_Type);

	return 0;
	fail:
	Py_XDECREF(m);
	return -1;
}

static struct PyModuleDef_Slot LevpartflipModule_slots[] = {
		{Py_mod_exec, LevpartflipModule_exec},
		{0, NULL},
};

static struct PyModuleDef LevpartflipModule = {
		PyModuleDef_HEAD_INIT,
		"Levpartflip",
		module_doc,
		0,
		LevpartflipModule_methods,
		LevpartflipModule_slots,
		NULL,
		NULL,
		NULL
};

/* Export function for the module */

PyMODINIT_FUNC
PyInit_Levpartflip(void)
{
	return PyModuleDef_Init(&LevpartflipModule);
}