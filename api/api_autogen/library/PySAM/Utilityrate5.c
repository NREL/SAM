#include <Python.h>

#include <SAM_Utilityrate5.h>
#include <SAM_api.h>

#include "PySAM_utils.h"



	/*
	 * Common Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Utilityrate5   data_ptr;
} CommonObject;

static PyTypeObject Common_Type;

static PyObject *
Common_new(SAM_Utilityrate5 data_ptr)
{
	PyObject* new_obj = Common_Type.tp_alloc(&Common_Type,0);

	CommonObject* Common_obj = (CommonObject*)new_obj;

	Common_obj->data_ptr = data_ptr;

	return new_obj;
}

/* Common methods */

static PyObject *
Common_assign(CommonObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Utilityrate5", "Common")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
Common_export(CommonObject *self, PyObject *args)
{
	PyTypeObject* tp = &Common_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef Common_methods[] = {
		{"assign",            (PyCFunction)Common_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``Common_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)Common_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
Common_get_TOU_demand_single_peak(CommonObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Utilityrate5_Common_TOU_demand_single_peak_nget, self->data_ptr);
}

static int
Common_set_TOU_demand_single_peak(CommonObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Utilityrate5_Common_TOU_demand_single_peak_nset, self->data_ptr);
}

static PyObject *
Common_get_en_electricity_rates(CommonObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Utilityrate5_Common_en_electricity_rates_nget, self->data_ptr);
}

static int
Common_set_en_electricity_rates(CommonObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Utilityrate5_Common_en_electricity_rates_nset, self->data_ptr);
}

static PyObject *
Common_get_ur_sell_eq_buy(CommonObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Utilityrate5_Common_ur_sell_eq_buy_nget, self->data_ptr);
}

static int
Common_set_ur_sell_eq_buy(CommonObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Utilityrate5_Common_ur_sell_eq_buy_nset, self->data_ptr);
}

static PyGetSetDef Common_getset[] = {
{"TOU_demand_single_peak", (getter)Common_get_TOU_demand_single_peak,(setter)Common_set_TOU_demand_single_peak,
	PyDoc_STR("*float*: Use single monthly peak for TOU demand charge [0/1]\n\n*Options*: 0=use TOU peak,1=use flat peak\n\n*Constraints*: INTEGER,MIN=0,MAX=1\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"en_electricity_rates", (getter)Common_get_en_electricity_rates,(setter)Common_set_en_electricity_rates,
	PyDoc_STR("*float*: Optionally enable/disable electricity_rate [years]\n\n*Constraints*: INTEGER,MIN=0,MAX=1"),
 	NULL},
{"ur_sell_eq_buy", (getter)Common_get_ur_sell_eq_buy,(setter)Common_set_ur_sell_eq_buy,
	PyDoc_STR("*float*: Set sell rate equal to buy rate [0/1]\n\n*Info*: Optional override\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject Common_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Utilityrate5.Common",             /*tp_name*/
		sizeof(CommonObject),          /*tp_basicsize*/
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
		Common_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		Common_getset,          /*tp_getset*/
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
	SAM_Utilityrate5   data_ptr;
} LifetimeObject;

static PyTypeObject Lifetime_Type;

static PyObject *
Lifetime_new(SAM_Utilityrate5 data_ptr)
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

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Utilityrate5", "Lifetime")){
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
	return PySAM_double_getter(SAM_Utilityrate5_Lifetime_analysis_period_nget, self->data_ptr);
}

static int
Lifetime_set_analysis_period(LifetimeObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Utilityrate5_Lifetime_analysis_period_nset, self->data_ptr);
}

static PyObject *
Lifetime_get_inflation_rate(LifetimeObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Utilityrate5_Lifetime_inflation_rate_nget, self->data_ptr);
}

static int
Lifetime_set_inflation_rate(LifetimeObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Utilityrate5_Lifetime_inflation_rate_nset, self->data_ptr);
}

static PyObject *
Lifetime_get_system_use_lifetime_output(LifetimeObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Utilityrate5_Lifetime_system_use_lifetime_output_nget, self->data_ptr);
}

static int
Lifetime_set_system_use_lifetime_output(LifetimeObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Utilityrate5_Lifetime_system_use_lifetime_output_nset, self->data_ptr);
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
		"Utilityrate5.Lifetime",             /*tp_name*/
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
	 * SystemOutput Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Utilityrate5   data_ptr;
} SystemOutputObject;

static PyTypeObject SystemOutput_Type;

static PyObject *
SystemOutput_new(SAM_Utilityrate5 data_ptr)
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

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Utilityrate5", "SystemOutput")){
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
	return PySAM_array_getter(SAM_Utilityrate5_SystemOutput_degradation_aget, self->data_ptr);
}

static int
SystemOutput_set_degradation(SystemOutputObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Utilityrate5_SystemOutput_degradation_aset, self->data_ptr);
}

static PyObject *
SystemOutput_get_gen(SystemOutputObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Utilityrate5_SystemOutput_gen_aget, self->data_ptr);
}

static int
SystemOutput_set_gen(SystemOutputObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Utilityrate5_SystemOutput_gen_aset, self->data_ptr);
}

static PyGetSetDef SystemOutput_getset[] = {
{"degradation", (getter)SystemOutput_get_degradation,(setter)SystemOutput_set_degradation,
	PyDoc_STR("*sequence*: Annual energy degradation [%]\n\n*Required*: True"),
 	NULL},
{"gen", (getter)SystemOutput_get_gen,(setter)SystemOutput_set_gen,
	PyDoc_STR("*sequence*: System power generated [kW]\n\n*Required*: True"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject SystemOutput_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Utilityrate5.SystemOutput",             /*tp_name*/
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
	 * TimeSeries Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Utilityrate5   data_ptr;
} TimeSeriesObject;

static PyTypeObject TimeSeries_Type;

static PyObject *
TimeSeries_new(SAM_Utilityrate5 data_ptr)
{
	PyObject* new_obj = TimeSeries_Type.tp_alloc(&TimeSeries_Type,0);

	TimeSeriesObject* TimeSeries_obj = (TimeSeriesObject*)new_obj;

	TimeSeries_obj->data_ptr = data_ptr;

	return new_obj;
}

/* TimeSeries methods */

static PyObject *
TimeSeries_assign(TimeSeriesObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Utilityrate5", "TimeSeries")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
TimeSeries_export(TimeSeriesObject *self, PyObject *args)
{
	PyTypeObject* tp = &TimeSeries_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef TimeSeries_methods[] = {
		{"assign",            (PyCFunction)TimeSeries_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``TimeSeries_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)TimeSeries_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
TimeSeries_get_load(TimeSeriesObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Utilityrate5_TimeSeries_load_aget, self->data_ptr);
}

static int
TimeSeries_set_load(TimeSeriesObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Utilityrate5_TimeSeries_load_aset, self->data_ptr);
}

static PyGetSetDef TimeSeries_getset[] = {
{"load", (getter)TimeSeries_get_load,(setter)TimeSeries_set_load,
	PyDoc_STR("*sequence*: Electricity load (year 1) [kW]"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject TimeSeries_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Utilityrate5.TimeSeries",             /*tp_name*/
		sizeof(TimeSeriesObject),          /*tp_basicsize*/
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
		TimeSeries_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		TimeSeries_getset,          /*tp_getset*/
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
	 * ElectricLoad Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Utilityrate5   data_ptr;
} ElectricLoadObject;

static PyTypeObject ElectricLoad_Type;

static PyObject *
ElectricLoad_new(SAM_Utilityrate5 data_ptr)
{
	PyObject* new_obj = ElectricLoad_Type.tp_alloc(&ElectricLoad_Type,0);

	ElectricLoadObject* ElectricLoad_obj = (ElectricLoadObject*)new_obj;

	ElectricLoad_obj->data_ptr = data_ptr;

	return new_obj;
}

/* ElectricLoad methods */

static PyObject *
ElectricLoad_assign(ElectricLoadObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Utilityrate5", "ElectricLoad")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
ElectricLoad_export(ElectricLoadObject *self, PyObject *args)
{
	PyTypeObject* tp = &ElectricLoad_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef ElectricLoad_methods[] = {
		{"assign",            (PyCFunction)ElectricLoad_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``ElectricLoad_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)ElectricLoad_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
ElectricLoad_get_load_escalation(ElectricLoadObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Utilityrate5_ElectricLoad_load_escalation_aget, self->data_ptr);
}

static int
ElectricLoad_set_load_escalation(ElectricLoadObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Utilityrate5_ElectricLoad_load_escalation_aset, self->data_ptr);
}

static PyGetSetDef ElectricLoad_getset[] = {
{"load_escalation", (getter)ElectricLoad_get_load_escalation,(setter)ElectricLoad_set_load_escalation,
	PyDoc_STR("*sequence*: Annual load escalation [%/year]\n\n*Required*: set to 0 if not provided."),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject ElectricLoad_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Utilityrate5.ElectricLoad",             /*tp_name*/
		sizeof(ElectricLoadObject),          /*tp_basicsize*/
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
		ElectricLoad_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		ElectricLoad_getset,          /*tp_getset*/
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
	 * UtilityRateFlat Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Utilityrate5   data_ptr;
} UtilityRateFlatObject;

static PyTypeObject UtilityRateFlat_Type;

static PyObject *
UtilityRateFlat_new(SAM_Utilityrate5 data_ptr)
{
	PyObject* new_obj = UtilityRateFlat_Type.tp_alloc(&UtilityRateFlat_Type,0);

	UtilityRateFlatObject* UtilityRateFlat_obj = (UtilityRateFlatObject*)new_obj;

	UtilityRateFlat_obj->data_ptr = data_ptr;

	return new_obj;
}

/* UtilityRateFlat methods */

static PyObject *
UtilityRateFlat_assign(UtilityRateFlatObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Utilityrate5", "UtilityRateFlat")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
UtilityRateFlat_export(UtilityRateFlatObject *self, PyObject *args)
{
	PyTypeObject* tp = &UtilityRateFlat_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef UtilityRateFlat_methods[] = {
		{"assign",            (PyCFunction)UtilityRateFlat_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``UtilityRateFlat_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)UtilityRateFlat_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
UtilityRateFlat_get_rate_escalation(UtilityRateFlatObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Utilityrate5_UtilityRateFlat_rate_escalation_aget, self->data_ptr);
}

static int
UtilityRateFlat_set_rate_escalation(UtilityRateFlatObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Utilityrate5_UtilityRateFlat_rate_escalation_aset, self->data_ptr);
}

static PyObject *
UtilityRateFlat_get_ur_annual_min_charge(UtilityRateFlatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Utilityrate5_UtilityRateFlat_ur_annual_min_charge_nget, self->data_ptr);
}

static int
UtilityRateFlat_set_ur_annual_min_charge(UtilityRateFlatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Utilityrate5_UtilityRateFlat_ur_annual_min_charge_nset, self->data_ptr);
}

static PyObject *
UtilityRateFlat_get_ur_en_ts_sell_rate(UtilityRateFlatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Utilityrate5_UtilityRateFlat_ur_en_ts_sell_rate_nget, self->data_ptr);
}

static int
UtilityRateFlat_set_ur_en_ts_sell_rate(UtilityRateFlatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Utilityrate5_UtilityRateFlat_ur_en_ts_sell_rate_nset, self->data_ptr);
}

static PyObject *
UtilityRateFlat_get_ur_metering_option(UtilityRateFlatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Utilityrate5_UtilityRateFlat_ur_metering_option_nget, self->data_ptr);
}

static int
UtilityRateFlat_set_ur_metering_option(UtilityRateFlatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Utilityrate5_UtilityRateFlat_ur_metering_option_nset, self->data_ptr);
}

static PyObject *
UtilityRateFlat_get_ur_monthly_fixed_charge(UtilityRateFlatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Utilityrate5_UtilityRateFlat_ur_monthly_fixed_charge_nget, self->data_ptr);
}

static int
UtilityRateFlat_set_ur_monthly_fixed_charge(UtilityRateFlatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Utilityrate5_UtilityRateFlat_ur_monthly_fixed_charge_nset, self->data_ptr);
}

static PyObject *
UtilityRateFlat_get_ur_monthly_min_charge(UtilityRateFlatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Utilityrate5_UtilityRateFlat_ur_monthly_min_charge_nget, self->data_ptr);
}

static int
UtilityRateFlat_set_ur_monthly_min_charge(UtilityRateFlatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Utilityrate5_UtilityRateFlat_ur_monthly_min_charge_nset, self->data_ptr);
}

static PyObject *
UtilityRateFlat_get_ur_nm_yearend_sell_rate(UtilityRateFlatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Utilityrate5_UtilityRateFlat_ur_nm_yearend_sell_rate_nget, self->data_ptr);
}

static int
UtilityRateFlat_set_ur_nm_yearend_sell_rate(UtilityRateFlatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Utilityrate5_UtilityRateFlat_ur_nm_yearend_sell_rate_nset, self->data_ptr);
}

static PyObject *
UtilityRateFlat_get_ur_ts_sell_rate(UtilityRateFlatObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Utilityrate5_UtilityRateFlat_ur_ts_sell_rate_aget, self->data_ptr);
}

static int
UtilityRateFlat_set_ur_ts_sell_rate(UtilityRateFlatObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Utilityrate5_UtilityRateFlat_ur_ts_sell_rate_aset, self->data_ptr);
}

static PyGetSetDef UtilityRateFlat_getset[] = {
{"rate_escalation", (getter)UtilityRateFlat_get_rate_escalation,(setter)UtilityRateFlat_set_rate_escalation,
	PyDoc_STR("*sequence*: Annual electricity rate escalation [%/year]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"ur_annual_min_charge", (getter)UtilityRateFlat_get_ur_annual_min_charge,(setter)UtilityRateFlat_set_ur_annual_min_charge,
	PyDoc_STR("*float*: Annual minimum charge [$]\n\n*Required*: set to 0.0 if not provided."),
 	NULL},
{"ur_en_ts_sell_rate", (getter)UtilityRateFlat_get_ur_en_ts_sell_rate,(setter)UtilityRateFlat_set_ur_en_ts_sell_rate,
	PyDoc_STR("*float*: Enable time step sell rates [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"ur_metering_option", (getter)UtilityRateFlat_get_ur_metering_option,(setter)UtilityRateFlat_set_ur_metering_option,
	PyDoc_STR("*float*: Metering options [0=Single meter with monthly rollover credits in kWh,1=Single meter with monthly rollover credits in $,2=Single meter with no monthly rollover credits (Net Billing),3=Single meter with monthly rollover credits in $ (Net Billing $),4=Two meters with all generation sold and all load purchased]\n\n*Info*: Net metering monthly excess\n\n*Constraints*: INTEGER,MIN=0,MAX=4\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"ur_monthly_fixed_charge", (getter)UtilityRateFlat_get_ur_monthly_fixed_charge,(setter)UtilityRateFlat_set_ur_monthly_fixed_charge,
	PyDoc_STR("*float*: Monthly fixed charge [$]\n\n*Required*: set to 0.0 if not provided."),
 	NULL},
{"ur_monthly_min_charge", (getter)UtilityRateFlat_get_ur_monthly_min_charge,(setter)UtilityRateFlat_set_ur_monthly_min_charge,
	PyDoc_STR("*float*: Monthly minimum charge [$]\n\n*Required*: set to 0.0 if not provided."),
 	NULL},
{"ur_nm_yearend_sell_rate", (getter)UtilityRateFlat_get_ur_nm_yearend_sell_rate,(setter)UtilityRateFlat_set_ur_nm_yearend_sell_rate,
	PyDoc_STR("*float*: Year end sell rate [$/kWh]\n\n*Required*: set to 0.0 if not provided."),
 	NULL},
{"ur_ts_sell_rate", (getter)UtilityRateFlat_get_ur_ts_sell_rate,(setter)UtilityRateFlat_set_ur_ts_sell_rate,
	PyDoc_STR("*sequence*: Time step sell rates [0/1]"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject UtilityRateFlat_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Utilityrate5.UtilityRateFlat",             /*tp_name*/
		sizeof(UtilityRateFlatObject),          /*tp_basicsize*/
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
		UtilityRateFlat_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		UtilityRateFlat_getset,          /*tp_getset*/
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
	 * UtilityRateEnergyCharge Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Utilityrate5   data_ptr;
} UtilityRateEnergyChargeObject;

static PyTypeObject UtilityRateEnergyCharge_Type;

static PyObject *
UtilityRateEnergyCharge_new(SAM_Utilityrate5 data_ptr)
{
	PyObject* new_obj = UtilityRateEnergyCharge_Type.tp_alloc(&UtilityRateEnergyCharge_Type,0);

	UtilityRateEnergyChargeObject* UtilityRateEnergyCharge_obj = (UtilityRateEnergyChargeObject*)new_obj;

	UtilityRateEnergyCharge_obj->data_ptr = data_ptr;

	return new_obj;
}

/* UtilityRateEnergyCharge methods */

static PyObject *
UtilityRateEnergyCharge_assign(UtilityRateEnergyChargeObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Utilityrate5", "UtilityRateEnergyCharge")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
UtilityRateEnergyCharge_export(UtilityRateEnergyChargeObject *self, PyObject *args)
{
	PyTypeObject* tp = &UtilityRateEnergyCharge_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef UtilityRateEnergyCharge_methods[] = {
		{"assign",            (PyCFunction)UtilityRateEnergyCharge_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``UtilityRateEnergyCharge_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)UtilityRateEnergyCharge_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
UtilityRateEnergyCharge_get_ur_ec_sched_weekday(UtilityRateEnergyChargeObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_UtilityRateEnergyCharge_ur_ec_sched_weekday_mget, self->data_ptr);
}

static int
UtilityRateEnergyCharge_set_ur_ec_sched_weekday(UtilityRateEnergyChargeObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_Utilityrate5_UtilityRateEnergyCharge_ur_ec_sched_weekday_mset, self->data_ptr);
}

static PyObject *
UtilityRateEnergyCharge_get_ur_ec_sched_weekend(UtilityRateEnergyChargeObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_UtilityRateEnergyCharge_ur_ec_sched_weekend_mget, self->data_ptr);
}

static int
UtilityRateEnergyCharge_set_ur_ec_sched_weekend(UtilityRateEnergyChargeObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_Utilityrate5_UtilityRateEnergyCharge_ur_ec_sched_weekend_mset, self->data_ptr);
}

static PyObject *
UtilityRateEnergyCharge_get_ur_ec_tou_mat(UtilityRateEnergyChargeObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_UtilityRateEnergyCharge_ur_ec_tou_mat_mget, self->data_ptr);
}

static int
UtilityRateEnergyCharge_set_ur_ec_tou_mat(UtilityRateEnergyChargeObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_Utilityrate5_UtilityRateEnergyCharge_ur_ec_tou_mat_mset, self->data_ptr);
}

static PyGetSetDef UtilityRateEnergyCharge_getset[] = {
{"ur_ec_sched_weekday", (getter)UtilityRateEnergyCharge_get_ur_ec_sched_weekday,(setter)UtilityRateEnergyCharge_set_ur_ec_sched_weekday,
	PyDoc_STR("*sequence[sequence]*: Energy charge weekday schedule\n\n*Info*: 12x24\n\n*Required*: True"),
 	NULL},
{"ur_ec_sched_weekend", (getter)UtilityRateEnergyCharge_get_ur_ec_sched_weekend,(setter)UtilityRateEnergyCharge_set_ur_ec_sched_weekend,
	PyDoc_STR("*sequence[sequence]*: Energy charge weekend schedule\n\n*Info*: 12x24\n\n*Required*: True"),
 	NULL},
{"ur_ec_tou_mat", (getter)UtilityRateEnergyCharge_get_ur_ec_tou_mat,(setter)UtilityRateEnergyCharge_set_ur_ec_tou_mat,
	PyDoc_STR("*sequence[sequence]*: Energy rates table\n\n*Required*: True"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject UtilityRateEnergyCharge_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Utilityrate5.UtilityRateEnergyCharge",             /*tp_name*/
		sizeof(UtilityRateEnergyChargeObject),          /*tp_basicsize*/
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
		UtilityRateEnergyCharge_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		UtilityRateEnergyCharge_getset,          /*tp_getset*/
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
	 * UtilityRateDemandCharge Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Utilityrate5   data_ptr;
} UtilityRateDemandChargeObject;

static PyTypeObject UtilityRateDemandCharge_Type;

static PyObject *
UtilityRateDemandCharge_new(SAM_Utilityrate5 data_ptr)
{
	PyObject* new_obj = UtilityRateDemandCharge_Type.tp_alloc(&UtilityRateDemandCharge_Type,0);

	UtilityRateDemandChargeObject* UtilityRateDemandCharge_obj = (UtilityRateDemandChargeObject*)new_obj;

	UtilityRateDemandCharge_obj->data_ptr = data_ptr;

	return new_obj;
}

/* UtilityRateDemandCharge methods */

static PyObject *
UtilityRateDemandCharge_assign(UtilityRateDemandChargeObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Utilityrate5", "UtilityRateDemandCharge")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
UtilityRateDemandCharge_export(UtilityRateDemandChargeObject *self, PyObject *args)
{
	PyTypeObject* tp = &UtilityRateDemandCharge_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef UtilityRateDemandCharge_methods[] = {
		{"assign",            (PyCFunction)UtilityRateDemandCharge_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``UtilityRateDemandCharge_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)UtilityRateDemandCharge_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
UtilityRateDemandCharge_get_ur_dc_enable(UtilityRateDemandChargeObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Utilityrate5_UtilityRateDemandCharge_ur_dc_enable_nget, self->data_ptr);
}

static int
UtilityRateDemandCharge_set_ur_dc_enable(UtilityRateDemandChargeObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Utilityrate5_UtilityRateDemandCharge_ur_dc_enable_nset, self->data_ptr);
}

static PyObject *
UtilityRateDemandCharge_get_ur_dc_flat_mat(UtilityRateDemandChargeObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_UtilityRateDemandCharge_ur_dc_flat_mat_mget, self->data_ptr);
}

static int
UtilityRateDemandCharge_set_ur_dc_flat_mat(UtilityRateDemandChargeObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_Utilityrate5_UtilityRateDemandCharge_ur_dc_flat_mat_mset, self->data_ptr);
}

static PyObject *
UtilityRateDemandCharge_get_ur_dc_sched_weekday(UtilityRateDemandChargeObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_UtilityRateDemandCharge_ur_dc_sched_weekday_mget, self->data_ptr);
}

static int
UtilityRateDemandCharge_set_ur_dc_sched_weekday(UtilityRateDemandChargeObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_Utilityrate5_UtilityRateDemandCharge_ur_dc_sched_weekday_mset, self->data_ptr);
}

static PyObject *
UtilityRateDemandCharge_get_ur_dc_sched_weekend(UtilityRateDemandChargeObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_UtilityRateDemandCharge_ur_dc_sched_weekend_mget, self->data_ptr);
}

static int
UtilityRateDemandCharge_set_ur_dc_sched_weekend(UtilityRateDemandChargeObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_Utilityrate5_UtilityRateDemandCharge_ur_dc_sched_weekend_mset, self->data_ptr);
}

static PyObject *
UtilityRateDemandCharge_get_ur_dc_tou_mat(UtilityRateDemandChargeObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_UtilityRateDemandCharge_ur_dc_tou_mat_mget, self->data_ptr);
}

static int
UtilityRateDemandCharge_set_ur_dc_tou_mat(UtilityRateDemandChargeObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_Utilityrate5_UtilityRateDemandCharge_ur_dc_tou_mat_mset, self->data_ptr);
}

static PyGetSetDef UtilityRateDemandCharge_getset[] = {
{"ur_dc_enable", (getter)UtilityRateDemandCharge_get_ur_dc_enable,(setter)UtilityRateDemandCharge_set_ur_dc_enable,
	PyDoc_STR("*float*: Enable demand charge [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"ur_dc_flat_mat", (getter)UtilityRateDemandCharge_get_ur_dc_flat_mat,(setter)UtilityRateDemandCharge_set_ur_dc_flat_mat,
	PyDoc_STR("*sequence[sequence]*: Demand rates (flat) table\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"ur_dc_sched_weekday", (getter)UtilityRateDemandCharge_get_ur_dc_sched_weekday,(setter)UtilityRateDemandCharge_set_ur_dc_sched_weekday,
	PyDoc_STR("*sequence[sequence]*: Demand charge weekday schedule\n\n*Info*: 12x24"),
 	NULL},
{"ur_dc_sched_weekend", (getter)UtilityRateDemandCharge_get_ur_dc_sched_weekend,(setter)UtilityRateDemandCharge_set_ur_dc_sched_weekend,
	PyDoc_STR("*sequence[sequence]*: Demand charge weekend schedule\n\n*Info*: 12x24"),
 	NULL},
{"ur_dc_tou_mat", (getter)UtilityRateDemandCharge_get_ur_dc_tou_mat,(setter)UtilityRateDemandCharge_set_ur_dc_tou_mat,
	PyDoc_STR("*sequence[sequence]*: Demand rates (TOU) table\n\n*Required*: set to 1 if not provided."),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject UtilityRateDemandCharge_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Utilityrate5.UtilityRateDemandCharge",             /*tp_name*/
		sizeof(UtilityRateDemandChargeObject),          /*tp_basicsize*/
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
		UtilityRateDemandCharge_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		UtilityRateDemandCharge_getset,          /*tp_getset*/
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
	SAM_Utilityrate5   data_ptr;
} OutputsObject;

static PyTypeObject Outputs_Type;

static PyObject *
Outputs_new(SAM_Utilityrate5 data_ptr)
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

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Utilityrate5", "Outputs")){
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
Outputs_get_annual_electric_load(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Utilityrate5_Outputs_annual_electric_load_aget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_energy_value(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Utilityrate5_Outputs_annual_energy_value_aget, self->data_ptr);
}

static PyObject *
Outputs_get_bill_load(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Utilityrate5_Outputs_bill_load_aget, self->data_ptr);
}

static PyObject *
Outputs_get_charge_w_sys_dc_fixed(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Utilityrate5_Outputs_charge_w_sys_dc_fixed_aget, self->data_ptr);
}

static PyObject *
Outputs_get_charge_w_sys_dc_fixed_ym(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_charge_w_sys_dc_fixed_ym_mget, self->data_ptr);
}

static PyObject *
Outputs_get_charge_w_sys_dc_tou(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Utilityrate5_Outputs_charge_w_sys_dc_tou_aget, self->data_ptr);
}

static PyObject *
Outputs_get_charge_w_sys_dc_tou_ym(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_charge_w_sys_dc_tou_ym_mget, self->data_ptr);
}

static PyObject *
Outputs_get_charge_w_sys_ec(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Utilityrate5_Outputs_charge_w_sys_ec_aget, self->data_ptr);
}

static PyObject *
Outputs_get_charge_w_sys_ec_apr_tp(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_charge_w_sys_ec_apr_tp_mget, self->data_ptr);
}

static PyObject *
Outputs_get_charge_w_sys_ec_aug_tp(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_charge_w_sys_ec_aug_tp_mget, self->data_ptr);
}

static PyObject *
Outputs_get_charge_w_sys_ec_dec_tp(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_charge_w_sys_ec_dec_tp_mget, self->data_ptr);
}

static PyObject *
Outputs_get_charge_w_sys_ec_feb_tp(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_charge_w_sys_ec_feb_tp_mget, self->data_ptr);
}

static PyObject *
Outputs_get_charge_w_sys_ec_gross_ym(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_charge_w_sys_ec_gross_ym_mget, self->data_ptr);
}

static PyObject *
Outputs_get_charge_w_sys_ec_jan_tp(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_charge_w_sys_ec_jan_tp_mget, self->data_ptr);
}

static PyObject *
Outputs_get_charge_w_sys_ec_jul_tp(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_charge_w_sys_ec_jul_tp_mget, self->data_ptr);
}

static PyObject *
Outputs_get_charge_w_sys_ec_jun_tp(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_charge_w_sys_ec_jun_tp_mget, self->data_ptr);
}

static PyObject *
Outputs_get_charge_w_sys_ec_mar_tp(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_charge_w_sys_ec_mar_tp_mget, self->data_ptr);
}

static PyObject *
Outputs_get_charge_w_sys_ec_may_tp(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_charge_w_sys_ec_may_tp_mget, self->data_ptr);
}

static PyObject *
Outputs_get_charge_w_sys_ec_nov_tp(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_charge_w_sys_ec_nov_tp_mget, self->data_ptr);
}

static PyObject *
Outputs_get_charge_w_sys_ec_oct_tp(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_charge_w_sys_ec_oct_tp_mget, self->data_ptr);
}

static PyObject *
Outputs_get_charge_w_sys_ec_sep_tp(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_charge_w_sys_ec_sep_tp_mget, self->data_ptr);
}

static PyObject *
Outputs_get_charge_w_sys_ec_ym(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_charge_w_sys_ec_ym_mget, self->data_ptr);
}

static PyObject *
Outputs_get_charge_w_sys_fixed(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Utilityrate5_Outputs_charge_w_sys_fixed_aget, self->data_ptr);
}

static PyObject *
Outputs_get_charge_w_sys_fixed_ym(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_charge_w_sys_fixed_ym_mget, self->data_ptr);
}

static PyObject *
Outputs_get_charge_w_sys_minimum(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Utilityrate5_Outputs_charge_w_sys_minimum_aget, self->data_ptr);
}

static PyObject *
Outputs_get_charge_w_sys_minimum_ym(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_charge_w_sys_minimum_ym_mget, self->data_ptr);
}

static PyObject *
Outputs_get_charge_wo_sys_dc_fixed(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Utilityrate5_Outputs_charge_wo_sys_dc_fixed_aget, self->data_ptr);
}

static PyObject *
Outputs_get_charge_wo_sys_dc_fixed_ym(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_charge_wo_sys_dc_fixed_ym_mget, self->data_ptr);
}

static PyObject *
Outputs_get_charge_wo_sys_dc_tou(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Utilityrate5_Outputs_charge_wo_sys_dc_tou_aget, self->data_ptr);
}

static PyObject *
Outputs_get_charge_wo_sys_dc_tou_ym(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_charge_wo_sys_dc_tou_ym_mget, self->data_ptr);
}

static PyObject *
Outputs_get_charge_wo_sys_ec(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Utilityrate5_Outputs_charge_wo_sys_ec_aget, self->data_ptr);
}

static PyObject *
Outputs_get_charge_wo_sys_ec_apr_tp(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_charge_wo_sys_ec_apr_tp_mget, self->data_ptr);
}

static PyObject *
Outputs_get_charge_wo_sys_ec_aug_tp(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_charge_wo_sys_ec_aug_tp_mget, self->data_ptr);
}

static PyObject *
Outputs_get_charge_wo_sys_ec_dec_tp(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_charge_wo_sys_ec_dec_tp_mget, self->data_ptr);
}

static PyObject *
Outputs_get_charge_wo_sys_ec_feb_tp(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_charge_wo_sys_ec_feb_tp_mget, self->data_ptr);
}

static PyObject *
Outputs_get_charge_wo_sys_ec_jan_tp(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_charge_wo_sys_ec_jan_tp_mget, self->data_ptr);
}

static PyObject *
Outputs_get_charge_wo_sys_ec_jul_tp(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_charge_wo_sys_ec_jul_tp_mget, self->data_ptr);
}

static PyObject *
Outputs_get_charge_wo_sys_ec_jun_tp(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_charge_wo_sys_ec_jun_tp_mget, self->data_ptr);
}

static PyObject *
Outputs_get_charge_wo_sys_ec_mar_tp(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_charge_wo_sys_ec_mar_tp_mget, self->data_ptr);
}

static PyObject *
Outputs_get_charge_wo_sys_ec_may_tp(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_charge_wo_sys_ec_may_tp_mget, self->data_ptr);
}

static PyObject *
Outputs_get_charge_wo_sys_ec_nov_tp(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_charge_wo_sys_ec_nov_tp_mget, self->data_ptr);
}

static PyObject *
Outputs_get_charge_wo_sys_ec_oct_tp(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_charge_wo_sys_ec_oct_tp_mget, self->data_ptr);
}

static PyObject *
Outputs_get_charge_wo_sys_ec_sep_tp(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_charge_wo_sys_ec_sep_tp_mget, self->data_ptr);
}

static PyObject *
Outputs_get_charge_wo_sys_ec_ym(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_charge_wo_sys_ec_ym_mget, self->data_ptr);
}

static PyObject *
Outputs_get_charge_wo_sys_fixed(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Utilityrate5_Outputs_charge_wo_sys_fixed_aget, self->data_ptr);
}

static PyObject *
Outputs_get_charge_wo_sys_fixed_ym(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_charge_wo_sys_fixed_ym_mget, self->data_ptr);
}

static PyObject *
Outputs_get_charge_wo_sys_minimum(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Utilityrate5_Outputs_charge_wo_sys_minimum_aget, self->data_ptr);
}

static PyObject *
Outputs_get_charge_wo_sys_minimum_ym(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_charge_wo_sys_minimum_ym_mget, self->data_ptr);
}

static PyObject *
Outputs_get_elec_cost_with_system(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Utilityrate5_Outputs_elec_cost_with_system_aget, self->data_ptr);
}

static PyObject *
Outputs_get_elec_cost_with_system_year1(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Utilityrate5_Outputs_elec_cost_with_system_year1_nget, self->data_ptr);
}

static PyObject *
Outputs_get_elec_cost_without_system(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Utilityrate5_Outputs_elec_cost_without_system_aget, self->data_ptr);
}

static PyObject *
Outputs_get_elec_cost_without_system_year1(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Utilityrate5_Outputs_elec_cost_without_system_year1_nget, self->data_ptr);
}

static PyObject *
Outputs_get_energy_w_sys_ec_apr_tp(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_energy_w_sys_ec_apr_tp_mget, self->data_ptr);
}

static PyObject *
Outputs_get_energy_w_sys_ec_aug_tp(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_energy_w_sys_ec_aug_tp_mget, self->data_ptr);
}

static PyObject *
Outputs_get_energy_w_sys_ec_dec_tp(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_energy_w_sys_ec_dec_tp_mget, self->data_ptr);
}

static PyObject *
Outputs_get_energy_w_sys_ec_feb_tp(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_energy_w_sys_ec_feb_tp_mget, self->data_ptr);
}

static PyObject *
Outputs_get_energy_w_sys_ec_jan_tp(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_energy_w_sys_ec_jan_tp_mget, self->data_ptr);
}

static PyObject *
Outputs_get_energy_w_sys_ec_jul_tp(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_energy_w_sys_ec_jul_tp_mget, self->data_ptr);
}

static PyObject *
Outputs_get_energy_w_sys_ec_jun_tp(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_energy_w_sys_ec_jun_tp_mget, self->data_ptr);
}

static PyObject *
Outputs_get_energy_w_sys_ec_mar_tp(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_energy_w_sys_ec_mar_tp_mget, self->data_ptr);
}

static PyObject *
Outputs_get_energy_w_sys_ec_may_tp(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_energy_w_sys_ec_may_tp_mget, self->data_ptr);
}

static PyObject *
Outputs_get_energy_w_sys_ec_nov_tp(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_energy_w_sys_ec_nov_tp_mget, self->data_ptr);
}

static PyObject *
Outputs_get_energy_w_sys_ec_oct_tp(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_energy_w_sys_ec_oct_tp_mget, self->data_ptr);
}

static PyObject *
Outputs_get_energy_w_sys_ec_sep_tp(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_energy_w_sys_ec_sep_tp_mget, self->data_ptr);
}

static PyObject *
Outputs_get_energy_wo_sys_ec_apr_tp(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_energy_wo_sys_ec_apr_tp_mget, self->data_ptr);
}

static PyObject *
Outputs_get_energy_wo_sys_ec_aug_tp(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_energy_wo_sys_ec_aug_tp_mget, self->data_ptr);
}

static PyObject *
Outputs_get_energy_wo_sys_ec_dec_tp(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_energy_wo_sys_ec_dec_tp_mget, self->data_ptr);
}

static PyObject *
Outputs_get_energy_wo_sys_ec_feb_tp(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_energy_wo_sys_ec_feb_tp_mget, self->data_ptr);
}

static PyObject *
Outputs_get_energy_wo_sys_ec_jan_tp(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_energy_wo_sys_ec_jan_tp_mget, self->data_ptr);
}

static PyObject *
Outputs_get_energy_wo_sys_ec_jul_tp(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_energy_wo_sys_ec_jul_tp_mget, self->data_ptr);
}

static PyObject *
Outputs_get_energy_wo_sys_ec_jun_tp(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_energy_wo_sys_ec_jun_tp_mget, self->data_ptr);
}

static PyObject *
Outputs_get_energy_wo_sys_ec_mar_tp(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_energy_wo_sys_ec_mar_tp_mget, self->data_ptr);
}

static PyObject *
Outputs_get_energy_wo_sys_ec_may_tp(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_energy_wo_sys_ec_may_tp_mget, self->data_ptr);
}

static PyObject *
Outputs_get_energy_wo_sys_ec_nov_tp(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_energy_wo_sys_ec_nov_tp_mget, self->data_ptr);
}

static PyObject *
Outputs_get_energy_wo_sys_ec_oct_tp(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_energy_wo_sys_ec_oct_tp_mget, self->data_ptr);
}

static PyObject *
Outputs_get_energy_wo_sys_ec_sep_tp(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_energy_wo_sys_ec_sep_tp_mget, self->data_ptr);
}

static PyObject *
Outputs_get_excess_dollars_applied_ym(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_excess_dollars_applied_ym_mget, self->data_ptr);
}

static PyObject *
Outputs_get_excess_dollars_earned_ym(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_excess_dollars_earned_ym_mget, self->data_ptr);
}

static PyObject *
Outputs_get_excess_kwhs_applied_ym(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_excess_kwhs_applied_ym_mget, self->data_ptr);
}

static PyObject *
Outputs_get_excess_kwhs_earned_ym(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_excess_kwhs_earned_ym_mget, self->data_ptr);
}

static PyObject *
Outputs_get_lifetime_load(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Utilityrate5_Outputs_lifetime_load_aget, self->data_ptr);
}

static PyObject *
Outputs_get_monthly_tou_demand_charge_w_sys(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_monthly_tou_demand_charge_w_sys_mget, self->data_ptr);
}

static PyObject *
Outputs_get_monthly_tou_demand_charge_wo_sys(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_monthly_tou_demand_charge_wo_sys_mget, self->data_ptr);
}

static PyObject *
Outputs_get_monthly_tou_demand_peak_w_sys(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_monthly_tou_demand_peak_w_sys_mget, self->data_ptr);
}

static PyObject *
Outputs_get_monthly_tou_demand_peak_wo_sys(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_monthly_tou_demand_peak_wo_sys_mget, self->data_ptr);
}

static PyObject *
Outputs_get_savings_year1(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Utilityrate5_Outputs_savings_year1_nget, self->data_ptr);
}

static PyObject *
Outputs_get_surplus_w_sys_ec_apr_tp(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_surplus_w_sys_ec_apr_tp_mget, self->data_ptr);
}

static PyObject *
Outputs_get_surplus_w_sys_ec_aug_tp(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_surplus_w_sys_ec_aug_tp_mget, self->data_ptr);
}

static PyObject *
Outputs_get_surplus_w_sys_ec_dec_tp(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_surplus_w_sys_ec_dec_tp_mget, self->data_ptr);
}

static PyObject *
Outputs_get_surplus_w_sys_ec_feb_tp(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_surplus_w_sys_ec_feb_tp_mget, self->data_ptr);
}

static PyObject *
Outputs_get_surplus_w_sys_ec_jan_tp(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_surplus_w_sys_ec_jan_tp_mget, self->data_ptr);
}

static PyObject *
Outputs_get_surplus_w_sys_ec_jul_tp(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_surplus_w_sys_ec_jul_tp_mget, self->data_ptr);
}

static PyObject *
Outputs_get_surplus_w_sys_ec_jun_tp(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_surplus_w_sys_ec_jun_tp_mget, self->data_ptr);
}

static PyObject *
Outputs_get_surplus_w_sys_ec_mar_tp(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_surplus_w_sys_ec_mar_tp_mget, self->data_ptr);
}

static PyObject *
Outputs_get_surplus_w_sys_ec_may_tp(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_surplus_w_sys_ec_may_tp_mget, self->data_ptr);
}

static PyObject *
Outputs_get_surplus_w_sys_ec_nov_tp(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_surplus_w_sys_ec_nov_tp_mget, self->data_ptr);
}

static PyObject *
Outputs_get_surplus_w_sys_ec_oct_tp(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_surplus_w_sys_ec_oct_tp_mget, self->data_ptr);
}

static PyObject *
Outputs_get_surplus_w_sys_ec_sep_tp(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_surplus_w_sys_ec_sep_tp_mget, self->data_ptr);
}

static PyObject *
Outputs_get_utility_bill_w_sys(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Utilityrate5_Outputs_utility_bill_w_sys_aget, self->data_ptr);
}

static PyObject *
Outputs_get_utility_bill_w_sys_ym(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_utility_bill_w_sys_ym_mget, self->data_ptr);
}

static PyObject *
Outputs_get_utility_bill_wo_sys(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Utilityrate5_Outputs_utility_bill_wo_sys_aget, self->data_ptr);
}

static PyObject *
Outputs_get_utility_bill_wo_sys_ym(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Utilityrate5_Outputs_utility_bill_wo_sys_ym_mget, self->data_ptr);
}

static PyObject *
Outputs_get_year1_electric_load(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Utilityrate5_Outputs_year1_electric_load_nget, self->data_ptr);
}

static PyObject *
Outputs_get_year1_excess_dollars_applied(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Utilityrate5_Outputs_year1_excess_dollars_applied_aget, self->data_ptr);
}

static PyObject *
Outputs_get_year1_excess_dollars_earned(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Utilityrate5_Outputs_year1_excess_dollars_earned_aget, self->data_ptr);
}

static PyObject *
Outputs_get_year1_excess_kwhs_applied(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Utilityrate5_Outputs_year1_excess_kwhs_applied_aget, self->data_ptr);
}

static PyObject *
Outputs_get_year1_excess_kwhs_earned(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Utilityrate5_Outputs_year1_excess_kwhs_earned_aget, self->data_ptr);
}

static PyObject *
Outputs_get_year1_hourly_dc_peak_per_period(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Utilityrate5_Outputs_year1_hourly_dc_peak_per_period_aget, self->data_ptr);
}

static PyObject *
Outputs_get_year1_hourly_dc_tou_schedule(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Utilityrate5_Outputs_year1_hourly_dc_tou_schedule_aget, self->data_ptr);
}

static PyObject *
Outputs_get_year1_hourly_dc_with_system(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Utilityrate5_Outputs_year1_hourly_dc_with_system_aget, self->data_ptr);
}

static PyObject *
Outputs_get_year1_hourly_dc_without_system(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Utilityrate5_Outputs_year1_hourly_dc_without_system_aget, self->data_ptr);
}

static PyObject *
Outputs_get_year1_hourly_e_fromgrid(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Utilityrate5_Outputs_year1_hourly_e_fromgrid_aget, self->data_ptr);
}

static PyObject *
Outputs_get_year1_hourly_e_tofromgrid(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Utilityrate5_Outputs_year1_hourly_e_tofromgrid_aget, self->data_ptr);
}

static PyObject *
Outputs_get_year1_hourly_e_togrid(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Utilityrate5_Outputs_year1_hourly_e_togrid_aget, self->data_ptr);
}

static PyObject *
Outputs_get_year1_hourly_ec_tou_schedule(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Utilityrate5_Outputs_year1_hourly_ec_tou_schedule_aget, self->data_ptr);
}

static PyObject *
Outputs_get_year1_hourly_ec_with_system(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Utilityrate5_Outputs_year1_hourly_ec_with_system_aget, self->data_ptr);
}

static PyObject *
Outputs_get_year1_hourly_ec_without_system(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Utilityrate5_Outputs_year1_hourly_ec_without_system_aget, self->data_ptr);
}

static PyObject *
Outputs_get_year1_hourly_p_system_to_load(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Utilityrate5_Outputs_year1_hourly_p_system_to_load_aget, self->data_ptr);
}

static PyObject *
Outputs_get_year1_hourly_p_tofromgrid(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Utilityrate5_Outputs_year1_hourly_p_tofromgrid_aget, self->data_ptr);
}

static PyObject *
Outputs_get_year1_hourly_salespurchases_with_system(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Utilityrate5_Outputs_year1_hourly_salespurchases_with_system_aget, self->data_ptr);
}

static PyObject *
Outputs_get_year1_hourly_salespurchases_without_system(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Utilityrate5_Outputs_year1_hourly_salespurchases_without_system_aget, self->data_ptr);
}

static PyObject *
Outputs_get_year1_hourly_system_to_load(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Utilityrate5_Outputs_year1_hourly_system_to_load_aget, self->data_ptr);
}

static PyObject *
Outputs_get_year1_monthly_cumulative_excess_dollars(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Utilityrate5_Outputs_year1_monthly_cumulative_excess_dollars_aget, self->data_ptr);
}

static PyObject *
Outputs_get_year1_monthly_cumulative_excess_generation(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Utilityrate5_Outputs_year1_monthly_cumulative_excess_generation_aget, self->data_ptr);
}

static PyObject *
Outputs_get_year1_monthly_dc_fixed_with_system(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Utilityrate5_Outputs_year1_monthly_dc_fixed_with_system_aget, self->data_ptr);
}

static PyObject *
Outputs_get_year1_monthly_dc_fixed_without_system(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Utilityrate5_Outputs_year1_monthly_dc_fixed_without_system_aget, self->data_ptr);
}

static PyObject *
Outputs_get_year1_monthly_dc_tou_with_system(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Utilityrate5_Outputs_year1_monthly_dc_tou_with_system_aget, self->data_ptr);
}

static PyObject *
Outputs_get_year1_monthly_dc_tou_without_system(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Utilityrate5_Outputs_year1_monthly_dc_tou_without_system_aget, self->data_ptr);
}

static PyObject *
Outputs_get_year1_monthly_ec_charge_gross_with_system(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Utilityrate5_Outputs_year1_monthly_ec_charge_gross_with_system_aget, self->data_ptr);
}

static PyObject *
Outputs_get_year1_monthly_ec_charge_with_system(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Utilityrate5_Outputs_year1_monthly_ec_charge_with_system_aget, self->data_ptr);
}

static PyObject *
Outputs_get_year1_monthly_ec_charge_without_system(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Utilityrate5_Outputs_year1_monthly_ec_charge_without_system_aget, self->data_ptr);
}

static PyObject *
Outputs_get_year1_monthly_electricity_to_grid(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Utilityrate5_Outputs_year1_monthly_electricity_to_grid_aget, self->data_ptr);
}

static PyObject *
Outputs_get_year1_monthly_fixed_with_system(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Utilityrate5_Outputs_year1_monthly_fixed_with_system_aget, self->data_ptr);
}

static PyObject *
Outputs_get_year1_monthly_fixed_without_system(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Utilityrate5_Outputs_year1_monthly_fixed_without_system_aget, self->data_ptr);
}

static PyObject *
Outputs_get_year1_monthly_load(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Utilityrate5_Outputs_year1_monthly_load_aget, self->data_ptr);
}

static PyObject *
Outputs_get_year1_monthly_minimum_with_system(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Utilityrate5_Outputs_year1_monthly_minimum_with_system_aget, self->data_ptr);
}

static PyObject *
Outputs_get_year1_monthly_minimum_without_system(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Utilityrate5_Outputs_year1_monthly_minimum_without_system_aget, self->data_ptr);
}

static PyObject *
Outputs_get_year1_monthly_peak_w_system(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Utilityrate5_Outputs_year1_monthly_peak_w_system_aget, self->data_ptr);
}

static PyObject *
Outputs_get_year1_monthly_peak_wo_system(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Utilityrate5_Outputs_year1_monthly_peak_wo_system_aget, self->data_ptr);
}

static PyObject *
Outputs_get_year1_monthly_use_w_system(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Utilityrate5_Outputs_year1_monthly_use_w_system_aget, self->data_ptr);
}

static PyObject *
Outputs_get_year1_monthly_use_wo_system(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Utilityrate5_Outputs_year1_monthly_use_wo_system_aget, self->data_ptr);
}

static PyObject *
Outputs_get_year1_monthly_utility_bill_w_sys(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Utilityrate5_Outputs_year1_monthly_utility_bill_w_sys_aget, self->data_ptr);
}

static PyObject *
Outputs_get_year1_monthly_utility_bill_wo_sys(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Utilityrate5_Outputs_year1_monthly_utility_bill_wo_sys_aget, self->data_ptr);
}

static PyGetSetDef Outputs_getset[] = {
{"annual_electric_load", (getter)Outputs_get_annual_electric_load,(setter)0,
	PyDoc_STR("*sequence*: Electricity load total in each year [kWh]"),
 	NULL},
{"annual_energy_value", (getter)Outputs_get_annual_energy_value,(setter)0,
	PyDoc_STR("*sequence*: Energy value in each year [$]"),
 	NULL},
{"bill_load", (getter)Outputs_get_bill_load,(setter)0,
	PyDoc_STR("*sequence*: Bill load (year 1) [kWh]"),
 	NULL},
{"charge_w_sys_dc_fixed", (getter)Outputs_get_charge_w_sys_dc_fixed,(setter)0,
	PyDoc_STR("*sequence*: Demand charge with system (flat) [$]"),
 	NULL},
{"charge_w_sys_dc_fixed_ym", (getter)Outputs_get_charge_w_sys_dc_fixed_ym,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Demand charge with system (flat) [$]"),
 	NULL},
{"charge_w_sys_dc_tou", (getter)Outputs_get_charge_w_sys_dc_tou,(setter)0,
	PyDoc_STR("*sequence*: Demand charge with system (TOU) [$]"),
 	NULL},
{"charge_w_sys_dc_tou_ym", (getter)Outputs_get_charge_w_sys_dc_tou_ym,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Demand charge with system (TOU) [$]"),
 	NULL},
{"charge_w_sys_ec", (getter)Outputs_get_charge_w_sys_ec,(setter)0,
	PyDoc_STR("*sequence*: Energy charge with system [$]"),
 	NULL},
{"charge_w_sys_ec_apr_tp", (getter)Outputs_get_charge_w_sys_ec_apr_tp,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Energy charge with system Apr [$]"),
 	NULL},
{"charge_w_sys_ec_aug_tp", (getter)Outputs_get_charge_w_sys_ec_aug_tp,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Energy charge with system Aug [$]"),
 	NULL},
{"charge_w_sys_ec_dec_tp", (getter)Outputs_get_charge_w_sys_ec_dec_tp,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Energy charge with system Dec [$]"),
 	NULL},
{"charge_w_sys_ec_feb_tp", (getter)Outputs_get_charge_w_sys_ec_feb_tp,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Energy charge with system Feb [$]"),
 	NULL},
{"charge_w_sys_ec_gross_ym", (getter)Outputs_get_charge_w_sys_ec_gross_ym,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Energy charge with system before credits [$]"),
 	NULL},
{"charge_w_sys_ec_jan_tp", (getter)Outputs_get_charge_w_sys_ec_jan_tp,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Energy charge with system Jan [$]"),
 	NULL},
{"charge_w_sys_ec_jul_tp", (getter)Outputs_get_charge_w_sys_ec_jul_tp,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Energy charge with system Jul [$]"),
 	NULL},
{"charge_w_sys_ec_jun_tp", (getter)Outputs_get_charge_w_sys_ec_jun_tp,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Energy charge with system Jun [$]"),
 	NULL},
{"charge_w_sys_ec_mar_tp", (getter)Outputs_get_charge_w_sys_ec_mar_tp,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Energy charge with system Mar [$]"),
 	NULL},
{"charge_w_sys_ec_may_tp", (getter)Outputs_get_charge_w_sys_ec_may_tp,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Energy charge with system May [$]"),
 	NULL},
{"charge_w_sys_ec_nov_tp", (getter)Outputs_get_charge_w_sys_ec_nov_tp,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Energy charge with system Nov [$]"),
 	NULL},
{"charge_w_sys_ec_oct_tp", (getter)Outputs_get_charge_w_sys_ec_oct_tp,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Energy charge with system Oct [$]"),
 	NULL},
{"charge_w_sys_ec_sep_tp", (getter)Outputs_get_charge_w_sys_ec_sep_tp,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Energy charge with system Sep [$]"),
 	NULL},
{"charge_w_sys_ec_ym", (getter)Outputs_get_charge_w_sys_ec_ym,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Energy charge with system [$]"),
 	NULL},
{"charge_w_sys_fixed", (getter)Outputs_get_charge_w_sys_fixed,(setter)0,
	PyDoc_STR("*sequence*: Fixed monthly charge with system [$]"),
 	NULL},
{"charge_w_sys_fixed_ym", (getter)Outputs_get_charge_w_sys_fixed_ym,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Fixed monthly charge with system [$]"),
 	NULL},
{"charge_w_sys_minimum", (getter)Outputs_get_charge_w_sys_minimum,(setter)0,
	PyDoc_STR("*sequence*: Minimum charge with system [$]"),
 	NULL},
{"charge_w_sys_minimum_ym", (getter)Outputs_get_charge_w_sys_minimum_ym,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Minimum charge with system [$]"),
 	NULL},
{"charge_wo_sys_dc_fixed", (getter)Outputs_get_charge_wo_sys_dc_fixed,(setter)0,
	PyDoc_STR("*sequence*: Demand charge without system (flat) [$]"),
 	NULL},
{"charge_wo_sys_dc_fixed_ym", (getter)Outputs_get_charge_wo_sys_dc_fixed_ym,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Demand charge without system (flat) [$]"),
 	NULL},
{"charge_wo_sys_dc_tou", (getter)Outputs_get_charge_wo_sys_dc_tou,(setter)0,
	PyDoc_STR("*sequence*: Demand charge without system (TOU) [$]"),
 	NULL},
{"charge_wo_sys_dc_tou_ym", (getter)Outputs_get_charge_wo_sys_dc_tou_ym,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Demand charge without system (TOU) [$]"),
 	NULL},
{"charge_wo_sys_ec", (getter)Outputs_get_charge_wo_sys_ec,(setter)0,
	PyDoc_STR("*sequence*: Energy charge without system [$]"),
 	NULL},
{"charge_wo_sys_ec_apr_tp", (getter)Outputs_get_charge_wo_sys_ec_apr_tp,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Energy charge without system Apr [$]"),
 	NULL},
{"charge_wo_sys_ec_aug_tp", (getter)Outputs_get_charge_wo_sys_ec_aug_tp,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Energy charge without system Aug [$]"),
 	NULL},
{"charge_wo_sys_ec_dec_tp", (getter)Outputs_get_charge_wo_sys_ec_dec_tp,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Energy charge without system Dec [$]"),
 	NULL},
{"charge_wo_sys_ec_feb_tp", (getter)Outputs_get_charge_wo_sys_ec_feb_tp,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Energy charge without system Feb [$]"),
 	NULL},
{"charge_wo_sys_ec_jan_tp", (getter)Outputs_get_charge_wo_sys_ec_jan_tp,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Energy charge without system Jan [$]"),
 	NULL},
{"charge_wo_sys_ec_jul_tp", (getter)Outputs_get_charge_wo_sys_ec_jul_tp,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Energy charge without system Jul [$]"),
 	NULL},
{"charge_wo_sys_ec_jun_tp", (getter)Outputs_get_charge_wo_sys_ec_jun_tp,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Energy charge without system Jun [$]"),
 	NULL},
{"charge_wo_sys_ec_mar_tp", (getter)Outputs_get_charge_wo_sys_ec_mar_tp,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Energy charge without system Mar [$]"),
 	NULL},
{"charge_wo_sys_ec_may_tp", (getter)Outputs_get_charge_wo_sys_ec_may_tp,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Energy charge without system May [$]"),
 	NULL},
{"charge_wo_sys_ec_nov_tp", (getter)Outputs_get_charge_wo_sys_ec_nov_tp,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Energy charge without system Nov [$]"),
 	NULL},
{"charge_wo_sys_ec_oct_tp", (getter)Outputs_get_charge_wo_sys_ec_oct_tp,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Energy charge without system Oct [$]"),
 	NULL},
{"charge_wo_sys_ec_sep_tp", (getter)Outputs_get_charge_wo_sys_ec_sep_tp,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Energy charge without system Sep [$]"),
 	NULL},
{"charge_wo_sys_ec_ym", (getter)Outputs_get_charge_wo_sys_ec_ym,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Energy charge without system [$]"),
 	NULL},
{"charge_wo_sys_fixed", (getter)Outputs_get_charge_wo_sys_fixed,(setter)0,
	PyDoc_STR("*sequence*: Fixed monthly charge without system [$]"),
 	NULL},
{"charge_wo_sys_fixed_ym", (getter)Outputs_get_charge_wo_sys_fixed_ym,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Fixed monthly charge without system [$]"),
 	NULL},
{"charge_wo_sys_minimum", (getter)Outputs_get_charge_wo_sys_minimum,(setter)0,
	PyDoc_STR("*sequence*: Minimum charge without system [$]"),
 	NULL},
{"charge_wo_sys_minimum_ym", (getter)Outputs_get_charge_wo_sys_minimum_ym,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Minimum charge without system [$]"),
 	NULL},
{"elec_cost_with_system", (getter)Outputs_get_elec_cost_with_system,(setter)0,
	PyDoc_STR("*sequence*: Electricity bill with system [$/yr]"),
 	NULL},
{"elec_cost_with_system_year1", (getter)Outputs_get_elec_cost_with_system_year1,(setter)0,
	PyDoc_STR("*float*: Electricity bill with system (year 1) [$/yr]"),
 	NULL},
{"elec_cost_without_system", (getter)Outputs_get_elec_cost_without_system,(setter)0,
	PyDoc_STR("*sequence*: Electricity bill without system [$/yr]"),
 	NULL},
{"elec_cost_without_system_year1", (getter)Outputs_get_elec_cost_without_system_year1,(setter)0,
	PyDoc_STR("*float*: Electricity bill without system (year 1) [$/yr]"),
 	NULL},
{"energy_w_sys_ec_apr_tp", (getter)Outputs_get_energy_w_sys_ec_apr_tp,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Electricity usage with system Apr [kWh]"),
 	NULL},
{"energy_w_sys_ec_aug_tp", (getter)Outputs_get_energy_w_sys_ec_aug_tp,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Electricity usage with system Aug [kWh]"),
 	NULL},
{"energy_w_sys_ec_dec_tp", (getter)Outputs_get_energy_w_sys_ec_dec_tp,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Electricity usage with system Dec [kWh]"),
 	NULL},
{"energy_w_sys_ec_feb_tp", (getter)Outputs_get_energy_w_sys_ec_feb_tp,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Electricity usage with system Feb [kWh]"),
 	NULL},
{"energy_w_sys_ec_jan_tp", (getter)Outputs_get_energy_w_sys_ec_jan_tp,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Electricity usage with system Jan [kWh]"),
 	NULL},
{"energy_w_sys_ec_jul_tp", (getter)Outputs_get_energy_w_sys_ec_jul_tp,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Electricity usage with system Jul [kWh]"),
 	NULL},
{"energy_w_sys_ec_jun_tp", (getter)Outputs_get_energy_w_sys_ec_jun_tp,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Electricity usage with system Jun [kWh]"),
 	NULL},
{"energy_w_sys_ec_mar_tp", (getter)Outputs_get_energy_w_sys_ec_mar_tp,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Electricity usage with system Mar [kWh]"),
 	NULL},
{"energy_w_sys_ec_may_tp", (getter)Outputs_get_energy_w_sys_ec_may_tp,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Electricity usage with system May [kWh]"),
 	NULL},
{"energy_w_sys_ec_nov_tp", (getter)Outputs_get_energy_w_sys_ec_nov_tp,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Electricity usage with system Nov [kWh]"),
 	NULL},
{"energy_w_sys_ec_oct_tp", (getter)Outputs_get_energy_w_sys_ec_oct_tp,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Electricity usage with system Oct [kWh]"),
 	NULL},
{"energy_w_sys_ec_sep_tp", (getter)Outputs_get_energy_w_sys_ec_sep_tp,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Electricity usage with system Sep [kWh]"),
 	NULL},
{"energy_wo_sys_ec_apr_tp", (getter)Outputs_get_energy_wo_sys_ec_apr_tp,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Electricity usage without system Apr [kWh]"),
 	NULL},
{"energy_wo_sys_ec_aug_tp", (getter)Outputs_get_energy_wo_sys_ec_aug_tp,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Electricity usage without system Aug [kWh]"),
 	NULL},
{"energy_wo_sys_ec_dec_tp", (getter)Outputs_get_energy_wo_sys_ec_dec_tp,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Electricity usage without system Dec [kWh]"),
 	NULL},
{"energy_wo_sys_ec_feb_tp", (getter)Outputs_get_energy_wo_sys_ec_feb_tp,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Electricity usage without system Feb [kWh]"),
 	NULL},
{"energy_wo_sys_ec_jan_tp", (getter)Outputs_get_energy_wo_sys_ec_jan_tp,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Electricity usage without system Jan [kWh]"),
 	NULL},
{"energy_wo_sys_ec_jul_tp", (getter)Outputs_get_energy_wo_sys_ec_jul_tp,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Electricity usage without system Jul [kWh]"),
 	NULL},
{"energy_wo_sys_ec_jun_tp", (getter)Outputs_get_energy_wo_sys_ec_jun_tp,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Electricity usage without system Jun [kWh]"),
 	NULL},
{"energy_wo_sys_ec_mar_tp", (getter)Outputs_get_energy_wo_sys_ec_mar_tp,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Electricity usage without system Mar [kWh]"),
 	NULL},
{"energy_wo_sys_ec_may_tp", (getter)Outputs_get_energy_wo_sys_ec_may_tp,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Electricity usage without system May [kWh]"),
 	NULL},
{"energy_wo_sys_ec_nov_tp", (getter)Outputs_get_energy_wo_sys_ec_nov_tp,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Electricity usage without system Nov [kWh]"),
 	NULL},
{"energy_wo_sys_ec_oct_tp", (getter)Outputs_get_energy_wo_sys_ec_oct_tp,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Electricity usage without system Oct [kWh]"),
 	NULL},
{"energy_wo_sys_ec_sep_tp", (getter)Outputs_get_energy_wo_sys_ec_sep_tp,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Electricity usage without system Sep [kWh]"),
 	NULL},
{"excess_dollars_applied_ym", (getter)Outputs_get_excess_dollars_applied_ym,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Excess generation $ credit applied [$]"),
 	NULL},
{"excess_dollars_earned_ym", (getter)Outputs_get_excess_dollars_earned_ym,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Excess generation $ credit earned [$]"),
 	NULL},
{"excess_kwhs_applied_ym", (getter)Outputs_get_excess_kwhs_applied_ym,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Excess generation kWh credit applied [kWh]"),
 	NULL},
{"excess_kwhs_earned_ym", (getter)Outputs_get_excess_kwhs_earned_ym,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Excess generation kWh credit earned [kWh]"),
 	NULL},
{"lifetime_load", (getter)Outputs_get_lifetime_load,(setter)0,
	PyDoc_STR("*sequence*: Lifetime electricity load [kW]"),
 	NULL},
{"monthly_tou_demand_charge_w_sys", (getter)Outputs_get_monthly_tou_demand_charge_w_sys,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Demand peak charge with system [$]"),
 	NULL},
{"monthly_tou_demand_charge_wo_sys", (getter)Outputs_get_monthly_tou_demand_charge_wo_sys,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Demand peak charge without system [$]"),
 	NULL},
{"monthly_tou_demand_peak_w_sys", (getter)Outputs_get_monthly_tou_demand_peak_w_sys,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Demand peak with system [kW]"),
 	NULL},
{"monthly_tou_demand_peak_wo_sys", (getter)Outputs_get_monthly_tou_demand_peak_wo_sys,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Demand peak without system [kW]"),
 	NULL},
{"savings_year1", (getter)Outputs_get_savings_year1,(setter)0,
	PyDoc_STR("*float*: Electricity bill savings with system (year 1) [$/yr]"),
 	NULL},
{"surplus_w_sys_ec_apr_tp", (getter)Outputs_get_surplus_w_sys_ec_apr_tp,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Electricity exports with system Apr [kWh]"),
 	NULL},
{"surplus_w_sys_ec_aug_tp", (getter)Outputs_get_surplus_w_sys_ec_aug_tp,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Electricity exports with system Aug [kWh]"),
 	NULL},
{"surplus_w_sys_ec_dec_tp", (getter)Outputs_get_surplus_w_sys_ec_dec_tp,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Electricity exports with system Dec [kWh]"),
 	NULL},
{"surplus_w_sys_ec_feb_tp", (getter)Outputs_get_surplus_w_sys_ec_feb_tp,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Electricity exports with system Feb [kWh]"),
 	NULL},
{"surplus_w_sys_ec_jan_tp", (getter)Outputs_get_surplus_w_sys_ec_jan_tp,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Electricity exports with system Jan [kWh]"),
 	NULL},
{"surplus_w_sys_ec_jul_tp", (getter)Outputs_get_surplus_w_sys_ec_jul_tp,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Electricity exports with system Jul [kWh]"),
 	NULL},
{"surplus_w_sys_ec_jun_tp", (getter)Outputs_get_surplus_w_sys_ec_jun_tp,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Electricity exports with system Jun [kWh]"),
 	NULL},
{"surplus_w_sys_ec_mar_tp", (getter)Outputs_get_surplus_w_sys_ec_mar_tp,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Electricity exports with system Mar [kWh]"),
 	NULL},
{"surplus_w_sys_ec_may_tp", (getter)Outputs_get_surplus_w_sys_ec_may_tp,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Electricity exports with system May [kWh]"),
 	NULL},
{"surplus_w_sys_ec_nov_tp", (getter)Outputs_get_surplus_w_sys_ec_nov_tp,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Electricity exports with system Nov [kWh]"),
 	NULL},
{"surplus_w_sys_ec_oct_tp", (getter)Outputs_get_surplus_w_sys_ec_oct_tp,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Electricity exports with system Oct [kWh]"),
 	NULL},
{"surplus_w_sys_ec_sep_tp", (getter)Outputs_get_surplus_w_sys_ec_sep_tp,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Electricity exports with system Sep [kWh]"),
 	NULL},
{"utility_bill_w_sys", (getter)Outputs_get_utility_bill_w_sys,(setter)0,
	PyDoc_STR("*sequence*: Electricity bill with system [$]"),
 	NULL},
{"utility_bill_w_sys_ym", (getter)Outputs_get_utility_bill_w_sys_ym,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Electricity bill with system [$]"),
 	NULL},
{"utility_bill_wo_sys", (getter)Outputs_get_utility_bill_wo_sys,(setter)0,
	PyDoc_STR("*sequence*: Electricity bill without system [$]"),
 	NULL},
{"utility_bill_wo_sys_ym", (getter)Outputs_get_utility_bill_wo_sys_ym,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Electricity bill without system [$]"),
 	NULL},
{"year1_electric_load", (getter)Outputs_get_year1_electric_load,(setter)0,
	PyDoc_STR("*float*: Electricity load total (year 1) [kWh/yr]"),
 	NULL},
{"year1_excess_dollars_applied", (getter)Outputs_get_year1_excess_dollars_applied,(setter)0,
	PyDoc_STR("*sequence*: Excess generation $ credit applied [$/mo]"),
 	NULL},
{"year1_excess_dollars_earned", (getter)Outputs_get_year1_excess_dollars_earned,(setter)0,
	PyDoc_STR("*sequence*: Excess generation $ credit earned [$/mo]"),
 	NULL},
{"year1_excess_kwhs_applied", (getter)Outputs_get_year1_excess_kwhs_applied,(setter)0,
	PyDoc_STR("*sequence*: Excess generation kWh credit applied [kWh/mo]"),
 	NULL},
{"year1_excess_kwhs_earned", (getter)Outputs_get_year1_excess_kwhs_earned,(setter)0,
	PyDoc_STR("*sequence*: Excess generation kWh credit earned [kWh/mo]"),
 	NULL},
{"year1_hourly_dc_peak_per_period", (getter)Outputs_get_year1_hourly_dc_peak_per_period,(setter)0,
	PyDoc_STR("*sequence*: Electricity peak from grid per TOU period (year 1 hourly) [kW]"),
 	NULL},
{"year1_hourly_dc_tou_schedule", (getter)Outputs_get_year1_hourly_dc_tou_schedule,(setter)0,
	PyDoc_STR("*sequence*: TOU period for demand charges (year 1 hourly)"),
 	NULL},
{"year1_hourly_dc_with_system", (getter)Outputs_get_year1_hourly_dc_with_system,(setter)0,
	PyDoc_STR("*sequence*: Demand charge with system (year 1 hourly) [$]"),
 	NULL},
{"year1_hourly_dc_without_system", (getter)Outputs_get_year1_hourly_dc_without_system,(setter)0,
	PyDoc_STR("*sequence*: Demand charge without system (year 1 hourly) [$]"),
 	NULL},
{"year1_hourly_e_fromgrid", (getter)Outputs_get_year1_hourly_e_fromgrid,(setter)0,
	PyDoc_STR("*sequence*: Electricity from grid (year 1 hourly) [kWh]"),
 	NULL},
{"year1_hourly_e_tofromgrid", (getter)Outputs_get_year1_hourly_e_tofromgrid,(setter)0,
	PyDoc_STR("*sequence*: Electricity to/from grid (year 1 hourly) [kWh]"),
 	NULL},
{"year1_hourly_e_togrid", (getter)Outputs_get_year1_hourly_e_togrid,(setter)0,
	PyDoc_STR("*sequence*: Electricity to grid (year 1 hourly) [kWh]"),
 	NULL},
{"year1_hourly_ec_tou_schedule", (getter)Outputs_get_year1_hourly_ec_tou_schedule,(setter)0,
	PyDoc_STR("*sequence*: TOU period for energy charges (year 1 hourly)"),
 	NULL},
{"year1_hourly_ec_with_system", (getter)Outputs_get_year1_hourly_ec_with_system,(setter)0,
	PyDoc_STR("*sequence*: Energy charge with system (year 1 hourly) [$]"),
 	NULL},
{"year1_hourly_ec_without_system", (getter)Outputs_get_year1_hourly_ec_without_system,(setter)0,
	PyDoc_STR("*sequence*: Energy charge without system (year 1 hourly) [$]"),
 	NULL},
{"year1_hourly_p_system_to_load", (getter)Outputs_get_year1_hourly_p_system_to_load,(setter)0,
	PyDoc_STR("*sequence*: Electricity peak from system to load (year 1 hourly) [kW]"),
 	NULL},
{"year1_hourly_p_tofromgrid", (getter)Outputs_get_year1_hourly_p_tofromgrid,(setter)0,
	PyDoc_STR("*sequence*: Electricity to/from grid peak (year 1 hourly) [kW]"),
 	NULL},
{"year1_hourly_salespurchases_with_system", (getter)Outputs_get_year1_hourly_salespurchases_with_system,(setter)0,
	PyDoc_STR("*sequence*: Electricity sales/purchases with system (year 1 hourly) [$]"),
 	NULL},
{"year1_hourly_salespurchases_without_system", (getter)Outputs_get_year1_hourly_salespurchases_without_system,(setter)0,
	PyDoc_STR("*sequence*: Electricity sales/purchases without system (year 1 hourly) [$]"),
 	NULL},
{"year1_hourly_system_to_load", (getter)Outputs_get_year1_hourly_system_to_load,(setter)0,
	PyDoc_STR("*sequence*: Electricity from system to load (year 1 hourly) [kWh]"),
 	NULL},
{"year1_monthly_cumulative_excess_dollars", (getter)Outputs_get_year1_monthly_cumulative_excess_dollars,(setter)0,
	PyDoc_STR("*sequence*: Excess generation cumulative $ credit earned [$/mo]"),
 	NULL},
{"year1_monthly_cumulative_excess_generation", (getter)Outputs_get_year1_monthly_cumulative_excess_generation,(setter)0,
	PyDoc_STR("*sequence*: Excess generation cumulative kWh credit earned [kWh/mo]"),
 	NULL},
{"year1_monthly_dc_fixed_with_system", (getter)Outputs_get_year1_monthly_dc_fixed_with_system,(setter)0,
	PyDoc_STR("*sequence*: Demand charge (flat) with system [$/mo]"),
 	NULL},
{"year1_monthly_dc_fixed_without_system", (getter)Outputs_get_year1_monthly_dc_fixed_without_system,(setter)0,
	PyDoc_STR("*sequence*: Demand charge (flat) without system [$/mo]"),
 	NULL},
{"year1_monthly_dc_tou_with_system", (getter)Outputs_get_year1_monthly_dc_tou_with_system,(setter)0,
	PyDoc_STR("*sequence*: Demand charge (TOU) with system [$/mo]"),
 	NULL},
{"year1_monthly_dc_tou_without_system", (getter)Outputs_get_year1_monthly_dc_tou_without_system,(setter)0,
	PyDoc_STR("*sequence*: Demand charge (TOU) without system [$/mo]"),
 	NULL},
{"year1_monthly_ec_charge_gross_with_system", (getter)Outputs_get_year1_monthly_ec_charge_gross_with_system,(setter)0,
	PyDoc_STR("*sequence*: Energy charge with system before credits [$/mo]"),
 	NULL},
{"year1_monthly_ec_charge_with_system", (getter)Outputs_get_year1_monthly_ec_charge_with_system,(setter)0,
	PyDoc_STR("*sequence*: Energy charge with system [$/mo]"),
 	NULL},
{"year1_monthly_ec_charge_without_system", (getter)Outputs_get_year1_monthly_ec_charge_without_system,(setter)0,
	PyDoc_STR("*sequence*: Energy charge without system [$/mo]"),
 	NULL},
{"year1_monthly_electricity_to_grid", (getter)Outputs_get_year1_monthly_electricity_to_grid,(setter)0,
	PyDoc_STR("*sequence*: Electricity to/from grid [kWh/mo]"),
 	NULL},
{"year1_monthly_fixed_with_system", (getter)Outputs_get_year1_monthly_fixed_with_system,(setter)0,
	PyDoc_STR("*sequence*: Fixed monthly charge with system [$/mo]"),
 	NULL},
{"year1_monthly_fixed_without_system", (getter)Outputs_get_year1_monthly_fixed_without_system,(setter)0,
	PyDoc_STR("*sequence*: Fixed monthly charge without system [$/mo]"),
 	NULL},
{"year1_monthly_load", (getter)Outputs_get_year1_monthly_load,(setter)0,
	PyDoc_STR("*sequence*: Electricity load [kWh/mo]"),
 	NULL},
{"year1_monthly_minimum_with_system", (getter)Outputs_get_year1_monthly_minimum_with_system,(setter)0,
	PyDoc_STR("*sequence*: Minimum charge with system [$/mo]"),
 	NULL},
{"year1_monthly_minimum_without_system", (getter)Outputs_get_year1_monthly_minimum_without_system,(setter)0,
	PyDoc_STR("*sequence*: Minimum charge without system [$/mo]"),
 	NULL},
{"year1_monthly_peak_w_system", (getter)Outputs_get_year1_monthly_peak_w_system,(setter)0,
	PyDoc_STR("*sequence*: Demand peak with system [kW/mo]"),
 	NULL},
{"year1_monthly_peak_wo_system", (getter)Outputs_get_year1_monthly_peak_wo_system,(setter)0,
	PyDoc_STR("*sequence*: Demand peak without system [kW/mo]"),
 	NULL},
{"year1_monthly_use_w_system", (getter)Outputs_get_year1_monthly_use_w_system,(setter)0,
	PyDoc_STR("*sequence*: Electricity use with system [kWh/mo]"),
 	NULL},
{"year1_monthly_use_wo_system", (getter)Outputs_get_year1_monthly_use_wo_system,(setter)0,
	PyDoc_STR("*sequence*: Electricity use without system [kWh/mo]"),
 	NULL},
{"year1_monthly_utility_bill_w_sys", (getter)Outputs_get_year1_monthly_utility_bill_w_sys,(setter)0,
	PyDoc_STR("*sequence*: Electricity bill with system [$/mo]"),
 	NULL},
{"year1_monthly_utility_bill_wo_sys", (getter)Outputs_get_year1_monthly_utility_bill_wo_sys,(setter)0,
	PyDoc_STR("*sequence*: Electricity bill without system [$/mo]"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject Outputs_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Utilityrate5.Outputs",             /*tp_name*/
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
 * Utilityrate5
 */

typedef struct {
	PyObject_HEAD
	PyObject            *x_attr;        /* Attributes dictionary */
	SAM_Utilityrate5   data_ptr;
} Utilityrate5Object;

static PyTypeObject Utilityrate5_Type;

#define Utilityrate5Object_Check(v)      (Py_TYPE(v) == &Utilityrate5_Type)

static Utilityrate5Object *
newUtilityrate5Object(void* data_ptr)
{
	Utilityrate5Object *self;
	self = PyObject_New(Utilityrate5Object, &Utilityrate5_Type);

	PySAM_TECH_ATTR("Utilityrate5", SAM_Utilityrate5_construct)

	PyObject* Common_obj = Common_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "Common", Common_obj);
	Py_DECREF(Common_obj);

	PyObject* Lifetime_obj = Lifetime_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "Lifetime", Lifetime_obj);
	Py_DECREF(Lifetime_obj);

	PyObject* SystemOutput_obj = SystemOutput_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "SystemOutput", SystemOutput_obj);
	Py_DECREF(SystemOutput_obj);

	PyObject* TimeSeries_obj = TimeSeries_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "TimeSeries", TimeSeries_obj);
	Py_DECREF(TimeSeries_obj);

	PyObject* ElectricLoad_obj = ElectricLoad_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "ElectricLoad", ElectricLoad_obj);
	Py_DECREF(ElectricLoad_obj);

	PyObject* UtilityRateFlat_obj = UtilityRateFlat_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "UtilityRateFlat", UtilityRateFlat_obj);
	Py_DECREF(UtilityRateFlat_obj);

	PyObject* UtilityRateEnergyCharge_obj = UtilityRateEnergyCharge_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "UtilityRateEnergyCharge", UtilityRateEnergyCharge_obj);
	Py_DECREF(UtilityRateEnergyCharge_obj);

	PyObject* UtilityRateDemandCharge_obj = UtilityRateDemandCharge_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "UtilityRateDemandCharge", UtilityRateDemandCharge_obj);
	Py_DECREF(UtilityRateDemandCharge_obj);

	PyObject* Outputs_obj = Outputs_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "Outputs", Outputs_obj);
	Py_DECREF(Outputs_obj);


	return self;
}

/* Utilityrate5 methods */

static void
Utilityrate5_dealloc(Utilityrate5Object *self)
{
	Py_XDECREF(self->x_attr);
	SAM_Utilityrate5_destruct(self->data_ptr);
	PyObject_Del(self);
}


static PyObject *
Utilityrate5_execute(Utilityrate5Object *self, PyObject *args)
{
	int verbosity = 0;

	if (!PyArg_ParseTuple(args, "|i", &verbosity))
		return NULL;

	SAM_error error = new_error();
	SAM_Utilityrate5_execute(self->data_ptr, verbosity, &error);
	if (PySAM_has_error(error )) return NULL;

	Py_INCREF(Py_None);
	return Py_None;
}


static PyObject *
Utilityrate5_assign(Utilityrate5Object *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_nested_dict((PyObject*)self, self->x_attr, self->data_ptr, dict, "Utilityrate5"))
		return NULL;

	Py_INCREF(Py_None);
	return Py_None;
}


static PyObject *
Utilityrate5_export(Utilityrate5Object *self, PyObject *args)
{
	return PySAM_export_to_nested_dict((PyObject *) self, self->x_attr);
}

static PyMethodDef Utilityrate5_methods[] = {
		{"execute",            (PyCFunction)Utilityrate5_execute,  METH_VARARGS,
				PyDoc_STR("execute(int verbosity) -> None\n Execute simulation with verbosity level 0 (default) or 1")},
		{"assign",            (PyCFunction)Utilityrate5_assign,  METH_VARARGS,
				PyDoc_STR("assign(dict) -> None\n Assign attributes from nested dictionary, except for Outputs\n\n``nested_dict = { 'Common': { var: val, ...}, ...}``")},
		{"export",            (PyCFunction)Utilityrate5_export,  METH_VARARGS,
				PyDoc_STR("export() -> dict\n Export attributes into nested dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
Utilityrate5_getattro(Utilityrate5Object *self, PyObject *name)
{
	return PySAM_get_attr((PyObject*) self, (PyObject*) self->x_attr, name);
}

static int
Utilityrate5_setattr(Utilityrate5Object *self, const char *name, PyObject *v)
{
	return PySAM_set_attr((PyObject*)self, (PyObject*)self->x_attr, name, v);
}

static PyTypeObject Utilityrate5_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Utilityrate5",            /*tp_name*/
		sizeof(Utilityrate5Object),/*tp_basicsize*/
		0,                          /*tp_itemsize*/
		/* methods */
		(destructor)Utilityrate5_dealloc,    /*tp_dealloc*/
		0,                          /*tp_print*/
		(getattrfunc)0,             /*tp_getattr*/
		(setattrfunc)Utilityrate5_setattr,   /*tp_setattr*/
		0,                          /*tp_reserved*/
		0,                          /*tp_repr*/
		0,                          /*tp_as_number*/
		0,                          /*tp_as_sequence*/
		0,                          /*tp_as_mapping*/
		0,                          /*tp_hash*/
		0,                          /*tp_call*/
		0,                          /*tp_str*/
		(getattrofunc)Utilityrate5_getattro, /*tp_getattro*/
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
		Utilityrate5_methods,      /*tp_methods*/
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


/* Function of no arguments returning new Utilityrate5 object */

static PyObject *
Utilityrate5_new(PyObject *self, PyObject *args)
{
	Utilityrate5Object *rv;
	rv = newUtilityrate5Object(0);
	if (rv == NULL)
		return NULL;
	return (PyObject *)rv;
}

static PyObject *
Utilityrate5_wrap(PyObject *self, PyObject *args)
{
	Utilityrate5Object *rv;
	long long int ptr = 0;  // 64 bit arch
	if (!PyArg_ParseTuple(args, "L:wrap", &ptr)){
		PyErr_BadArgument();
		return NULL;
	}
	rv = newUtilityrate5Object((void*)ptr);
	if (rv == NULL)
		return NULL;
	return (PyObject *)rv;
}

static PyObject *
Utilityrate5_default(PyObject *self, PyObject *args)
{
	Utilityrate5Object *rv;
	char* def = 0;
	if (!PyArg_ParseTuple(args, "s:default", &def)){
		PyErr_BadArgument();
		return NULL;
	}
	rv = newUtilityrate5Object(0);
	if (rv == NULL)
		return NULL;

	PySAM_load_defaults((PyObject*)rv, rv->x_attr, rv->data_ptr, "Utilityrate5", def);

	return (PyObject *)rv;
}

/* ---------- */


/* List of functions defined in the module */

static PyMethodDef Utilityrate5Module_methods[] = {
		{"new",             Utilityrate5_new,         METH_VARARGS,
				PyDoc_STR("new() -> Utilityrate5")},
		{"default",             Utilityrate5_default,         METH_VARARGS,
				PyDoc_STR("default(config) -> Utilityrate5\n\nUse financial model-specific default attributes\n"
				"config options:\n\n- \"BiopowerCommercial\"\n- \"DSLFCommercial\"\n- \"DishStirlingCommercial\"\n- \"EmpiricalTroughCommercial\"\n- \"FlatPlatePVCommercial\"\n- \"FlatPlatePVHostDeveloper\"\n- \"FlatPlatePVResidential\"\n- \"FlatPlatePVSingleOwner\"\n- \"FlatPlatePVThirdParty\"\n- \"FuelCellCommercial\"\n- \"FuelCellSingleOwner\"\n- \"GenericCSPSystemCommercial\"\n- \"GenericSystemCommercial\"\n- \"GenericSystemHostDeveloper\"\n- \"GenericSystemResidential\"\n- \"GenericSystemThirdParty\"\n- \"MSLFCommercial\"\n- \"PVWattsCommercial\"\n- \"PVWattsHostDeveloper\"\n- \"PVWattsResidential\"\n- \"PVWattsThirdParty\"\n- \"PhysicalTroughCommercial\"\n- \"SolarWaterHeatingCommercial\"\n- \"SolarWaterHeatingResidential\"\n- \"WindPowerCommercial\"\n- \"WindPowerResidential\"")},
		{"wrap",             Utilityrate5_wrap,         METH_VARARGS,
				PyDoc_STR("wrap(ssc_data_t) -> Utilityrate5\n\nUse existing PySSC data\n\n.. warning::\n\n	Do not call PySSC.data_free on the ssc_data_t provided to ``wrap``")},
		{NULL,              NULL}           /* sentinel */
};

PyDoc_STRVAR(module_doc,
			 "Retail electricity bill calculator");


static int
Utilityrate5Module_exec(PyObject *m)
{
	/* Finalize the type object including setting type of the new type
	 * object; doing it here is required for portability, too. */

	if (PySAM_load_lib(m) < 0) goto fail;
	if (PySAM_init_error(m) < 0) goto fail;

	Utilityrate5_Type.tp_dict = PyDict_New();
	if (!Utilityrate5_Type.tp_dict) { goto fail; }

	/// Add the Common type object to Utilityrate5_Type
	if (PyType_Ready(&Common_Type) < 0) { goto fail; }
	PyDict_SetItemString(Utilityrate5_Type.tp_dict,
				"Common",
				(PyObject*)&Common_Type);
	Py_DECREF(&Common_Type);

	/// Add the Lifetime type object to Utilityrate5_Type
	if (PyType_Ready(&Lifetime_Type) < 0) { goto fail; }
	PyDict_SetItemString(Utilityrate5_Type.tp_dict,
				"Lifetime",
				(PyObject*)&Lifetime_Type);
	Py_DECREF(&Lifetime_Type);

	/// Add the SystemOutput type object to Utilityrate5_Type
	if (PyType_Ready(&SystemOutput_Type) < 0) { goto fail; }
	PyDict_SetItemString(Utilityrate5_Type.tp_dict,
				"SystemOutput",
				(PyObject*)&SystemOutput_Type);
	Py_DECREF(&SystemOutput_Type);

	/// Add the TimeSeries type object to Utilityrate5_Type
	if (PyType_Ready(&TimeSeries_Type) < 0) { goto fail; }
	PyDict_SetItemString(Utilityrate5_Type.tp_dict,
				"TimeSeries",
				(PyObject*)&TimeSeries_Type);
	Py_DECREF(&TimeSeries_Type);

	/// Add the ElectricLoad type object to Utilityrate5_Type
	if (PyType_Ready(&ElectricLoad_Type) < 0) { goto fail; }
	PyDict_SetItemString(Utilityrate5_Type.tp_dict,
				"ElectricLoad",
				(PyObject*)&ElectricLoad_Type);
	Py_DECREF(&ElectricLoad_Type);

	/// Add the UtilityRateFlat type object to Utilityrate5_Type
	if (PyType_Ready(&UtilityRateFlat_Type) < 0) { goto fail; }
	PyDict_SetItemString(Utilityrate5_Type.tp_dict,
				"UtilityRateFlat",
				(PyObject*)&UtilityRateFlat_Type);
	Py_DECREF(&UtilityRateFlat_Type);

	/// Add the UtilityRateEnergyCharge type object to Utilityrate5_Type
	if (PyType_Ready(&UtilityRateEnergyCharge_Type) < 0) { goto fail; }
	PyDict_SetItemString(Utilityrate5_Type.tp_dict,
				"UtilityRateEnergyCharge",
				(PyObject*)&UtilityRateEnergyCharge_Type);
	Py_DECREF(&UtilityRateEnergyCharge_Type);

	/// Add the UtilityRateDemandCharge type object to Utilityrate5_Type
	if (PyType_Ready(&UtilityRateDemandCharge_Type) < 0) { goto fail; }
	PyDict_SetItemString(Utilityrate5_Type.tp_dict,
				"UtilityRateDemandCharge",
				(PyObject*)&UtilityRateDemandCharge_Type);
	Py_DECREF(&UtilityRateDemandCharge_Type);

	/// Add the Outputs type object to Utilityrate5_Type
	if (PyType_Ready(&Outputs_Type) < 0) { goto fail; }
	PyDict_SetItemString(Utilityrate5_Type.tp_dict,
				"Outputs",
				(PyObject*)&Outputs_Type);
	Py_DECREF(&Outputs_Type);

	/// Add the Utilityrate5 type object to the module
	if (PyType_Ready(&Utilityrate5_Type) < 0) { goto fail; }
	PyModule_AddObject(m,
				"Utilityrate5",
				(PyObject*)&Utilityrate5_Type);

	return 0;
	fail:
	Py_XDECREF(m);
	return -1;
}

static struct PyModuleDef_Slot Utilityrate5Module_slots[] = {
		{Py_mod_exec, Utilityrate5Module_exec},
		{0, NULL},
};

static struct PyModuleDef Utilityrate5Module = {
		PyModuleDef_HEAD_INIT,
		"Utilityrate5",
		module_doc,
		0,
		Utilityrate5Module_methods,
		Utilityrate5Module_slots,
		NULL,
		NULL,
		NULL
};

/* Export function for the module */

PyMODINIT_FUNC
PyInit_Utilityrate5(void)
{
	return PyModuleDef_Init(&Utilityrate5Module);
}