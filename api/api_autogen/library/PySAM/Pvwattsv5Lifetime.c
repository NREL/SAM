#include <Python.h>

#include <SAM_Pvwattsv5Lifetime.h>
#include <SAM_api.h>

#include "PySAM_utils.h"



	/*
	 * Common Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Pvwattsv5Lifetime   data_ptr;
} CommonObject;

static PyTypeObject Common_Type;

static PyObject *
Common_new(SAM_Pvwattsv5Lifetime data_ptr)
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

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Pvwattsv5Lifetime", "Common")){
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
Common_get_system_use_lifetime_output(CommonObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvwattsv5Lifetime_Common_system_use_lifetime_output_nget, self->data_ptr);
}

static int
Common_set_system_use_lifetime_output(CommonObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvwattsv5Lifetime_Common_system_use_lifetime_output_nset, self->data_ptr);
}

static PyGetSetDef Common_getset[] = {
{"system_use_lifetime_output", (getter)Common_get_system_use_lifetime_output,(setter)Common_set_system_use_lifetime_output,
	PyDoc_STR("*float*: Run lifetime simulation [0/1]\n\n*Required*: True"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject Common_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Pvwattsv5Lifetime.Common",             /*tp_name*/
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
	 * FinancialAnalysisParameters Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Pvwattsv5Lifetime   data_ptr;
} FinancialAnalysisParametersObject;

static PyTypeObject FinancialAnalysisParameters_Type;

static PyObject *
FinancialAnalysisParameters_new(SAM_Pvwattsv5Lifetime data_ptr)
{
	PyObject* new_obj = FinancialAnalysisParameters_Type.tp_alloc(&FinancialAnalysisParameters_Type,0);

	FinancialAnalysisParametersObject* FinancialAnalysisParameters_obj = (FinancialAnalysisParametersObject*)new_obj;

	FinancialAnalysisParameters_obj->data_ptr = data_ptr;

	return new_obj;
}

/* FinancialAnalysisParameters methods */

static PyObject *
FinancialAnalysisParameters_assign(FinancialAnalysisParametersObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Pvwattsv5Lifetime", "FinancialAnalysisParameters")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
FinancialAnalysisParameters_export(FinancialAnalysisParametersObject *self, PyObject *args)
{
	PyTypeObject* tp = &FinancialAnalysisParameters_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef FinancialAnalysisParameters_methods[] = {
		{"assign",            (PyCFunction)FinancialAnalysisParameters_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``FinancialAnalysisParameters_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)FinancialAnalysisParameters_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
FinancialAnalysisParameters_get_analysis_period(FinancialAnalysisParametersObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvwattsv5Lifetime_FinancialAnalysisParameters_analysis_period_nget, self->data_ptr);
}

static int
FinancialAnalysisParameters_set_analysis_period(FinancialAnalysisParametersObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvwattsv5Lifetime_FinancialAnalysisParameters_analysis_period_nset, self->data_ptr);
}

static PyGetSetDef FinancialAnalysisParameters_getset[] = {
{"analysis_period", (getter)FinancialAnalysisParameters_get_analysis_period,(setter)FinancialAnalysisParameters_set_analysis_period,
	PyDoc_STR("*float*: Analysis period [years]\n\n*Required*: set to 1 if not provided."),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject FinancialAnalysisParameters_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Pvwattsv5Lifetime.FinancialAnalysisParameters",             /*tp_name*/
		sizeof(FinancialAnalysisParametersObject),          /*tp_basicsize*/
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
		FinancialAnalysisParameters_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		FinancialAnalysisParameters_getset,          /*tp_getset*/
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
	 * LifetimePV Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Pvwattsv5Lifetime   data_ptr;
} LifetimePVObject;

static PyTypeObject LifetimePV_Type;

static PyObject *
LifetimePV_new(SAM_Pvwattsv5Lifetime data_ptr)
{
	PyObject* new_obj = LifetimePV_Type.tp_alloc(&LifetimePV_Type,0);

	LifetimePVObject* LifetimePV_obj = (LifetimePVObject*)new_obj;

	LifetimePV_obj->data_ptr = data_ptr;

	return new_obj;
}

/* LifetimePV methods */

static PyObject *
LifetimePV_assign(LifetimePVObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Pvwattsv5Lifetime", "LifetimePV")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
LifetimePV_export(LifetimePVObject *self, PyObject *args)
{
	PyTypeObject* tp = &LifetimePV_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef LifetimePV_methods[] = {
		{"assign",            (PyCFunction)LifetimePV_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``LifetimePV_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)LifetimePV_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
LifetimePV_get_dc_degradation(LifetimePVObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvwattsv5Lifetime_LifetimePV_dc_degradation_aget, self->data_ptr);
}

static int
LifetimePV_set_dc_degradation(LifetimePVObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Pvwattsv5Lifetime_LifetimePV_dc_degradation_aset, self->data_ptr);
}

static PyGetSetDef LifetimePV_getset[] = {
{"dc_degradation", (getter)LifetimePV_get_dc_degradation,(setter)LifetimePV_set_dc_degradation,
	PyDoc_STR("*sequence*: Annual AC degradation [%/year]\n\n*Required*: set to 1 if not provided."),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject LifetimePV_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Pvwattsv5Lifetime.LifetimePV",             /*tp_name*/
		sizeof(LifetimePVObject),          /*tp_basicsize*/
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
		LifetimePV_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		LifetimePV_getset,          /*tp_getset*/
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
	 * Weather Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Pvwattsv5Lifetime   data_ptr;
} WeatherObject;

static PyTypeObject Weather_Type;

static PyObject *
Weather_new(SAM_Pvwattsv5Lifetime data_ptr)
{
	PyObject* new_obj = Weather_Type.tp_alloc(&Weather_Type,0);

	WeatherObject* Weather_obj = (WeatherObject*)new_obj;

	Weather_obj->data_ptr = data_ptr;

	return new_obj;
}

/* Weather methods */

static PyObject *
Weather_assign(WeatherObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Pvwattsv5Lifetime", "Weather")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
Weather_export(WeatherObject *self, PyObject *args)
{
	PyTypeObject* tp = &Weather_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef Weather_methods[] = {
		{"assign",            (PyCFunction)Weather_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``Weather_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)Weather_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
Weather_get_solar_resource_data(WeatherObject *self, void *closure)
{
	return PySAM_table_getter(SAM_Pvwattsv5Lifetime_Weather_solar_resource_data_tget, self->data_ptr);
}

static int
Weather_set_solar_resource_data(WeatherObject *self, PyObject *value, void *closure)
{
	return PySAM_table_setter(value, SAM_Pvwattsv5Lifetime_Weather_solar_resource_data_tset, self->data_ptr);
}

static PyObject *
Weather_get_solar_resource_file(WeatherObject *self, void *closure)
{
	return PySAM_string_getter(SAM_Pvwattsv5Lifetime_Weather_solar_resource_file_sget, self->data_ptr);
}

static int
Weather_set_solar_resource_file(WeatherObject *self, PyObject *value, void *closure)
{
	return PySAM_string_setter(value, SAM_Pvwattsv5Lifetime_Weather_solar_resource_file_sset, self->data_ptr);
}

static PyGetSetDef Weather_getset[] = {
{"solar_resource_data", (getter)Weather_get_solar_resource_data,(setter)Weather_set_solar_resource_data,
	PyDoc_STR("*dict*: Weather data\n\n*Info*: dn,df,tdry,wspd,lat,lon,tz\n\n*Required*: False"),
 	NULL},
{"solar_resource_file", (getter)Weather_get_solar_resource_file,(setter)Weather_set_solar_resource_file,
	PyDoc_STR("*str*: Weather file path\n\n*Required*: False"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject Weather_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Pvwattsv5Lifetime.Weather",             /*tp_name*/
		sizeof(WeatherObject),          /*tp_basicsize*/
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
		Weather_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		Weather_getset,          /*tp_getset*/
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
	 * PVWatts Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Pvwattsv5Lifetime   data_ptr;
} PVWattsObject;

static PyTypeObject PVWatts_Type;

static PyObject *
PVWatts_new(SAM_Pvwattsv5Lifetime data_ptr)
{
	PyObject* new_obj = PVWatts_Type.tp_alloc(&PVWatts_Type,0);

	PVWattsObject* PVWatts_obj = (PVWattsObject*)new_obj;

	PVWatts_obj->data_ptr = data_ptr;

	return new_obj;
}

/* PVWatts methods */

static PyObject *
PVWatts_assign(PVWattsObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Pvwattsv5Lifetime", "PVWatts")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
PVWatts_export(PVWattsObject *self, PyObject *args)
{
	PyTypeObject* tp = &PVWatts_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef PVWatts_methods[] = {
		{"assign",            (PyCFunction)PVWatts_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``PVWatts_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)PVWatts_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
PVWatts_get_array_type(PVWattsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvwattsv5Lifetime_PVWatts_array_type_nget, self->data_ptr);
}

static int
PVWatts_set_array_type(PVWattsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvwattsv5Lifetime_PVWatts_array_type_nset, self->data_ptr);
}

static PyObject *
PVWatts_get_azimuth(PVWattsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvwattsv5Lifetime_PVWatts_azimuth_nget, self->data_ptr);
}

static int
PVWatts_set_azimuth(PVWattsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvwattsv5Lifetime_PVWatts_azimuth_nset, self->data_ptr);
}

static PyObject *
PVWatts_get_dc_ac_ratio(PVWattsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvwattsv5Lifetime_PVWatts_dc_ac_ratio_nget, self->data_ptr);
}

static int
PVWatts_set_dc_ac_ratio(PVWattsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvwattsv5Lifetime_PVWatts_dc_ac_ratio_nset, self->data_ptr);
}

static PyObject *
PVWatts_get_gcr(PVWattsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvwattsv5Lifetime_PVWatts_gcr_nget, self->data_ptr);
}

static int
PVWatts_set_gcr(PVWattsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvwattsv5Lifetime_PVWatts_gcr_nset, self->data_ptr);
}

static PyObject *
PVWatts_get_inv_eff(PVWattsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvwattsv5Lifetime_PVWatts_inv_eff_nget, self->data_ptr);
}

static int
PVWatts_set_inv_eff(PVWattsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvwattsv5Lifetime_PVWatts_inv_eff_nset, self->data_ptr);
}

static PyObject *
PVWatts_get_losses(PVWattsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvwattsv5Lifetime_PVWatts_losses_nget, self->data_ptr);
}

static int
PVWatts_set_losses(PVWattsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvwattsv5Lifetime_PVWatts_losses_nset, self->data_ptr);
}

static PyObject *
PVWatts_get_module_type(PVWattsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvwattsv5Lifetime_PVWatts_module_type_nget, self->data_ptr);
}

static int
PVWatts_set_module_type(PVWattsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvwattsv5Lifetime_PVWatts_module_type_nset, self->data_ptr);
}

static PyObject *
PVWatts_get_shading_azal(PVWattsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Pvwattsv5Lifetime_PVWatts_shading_azal_mget, self->data_ptr);
}

static int
PVWatts_set_shading_azal(PVWattsObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_Pvwattsv5Lifetime_PVWatts_shading_azal_mset, self->data_ptr);
}

static PyObject *
PVWatts_get_shading_diff(PVWattsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvwattsv5Lifetime_PVWatts_shading_diff_nget, self->data_ptr);
}

static int
PVWatts_set_shading_diff(PVWattsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvwattsv5Lifetime_PVWatts_shading_diff_nset, self->data_ptr);
}

static PyObject *
PVWatts_get_shading_mxh(PVWattsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Pvwattsv5Lifetime_PVWatts_shading_mxh_mget, self->data_ptr);
}

static int
PVWatts_set_shading_mxh(PVWattsObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_Pvwattsv5Lifetime_PVWatts_shading_mxh_mset, self->data_ptr);
}

static PyObject *
PVWatts_get_shading_timestep(PVWattsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Pvwattsv5Lifetime_PVWatts_shading_timestep_mget, self->data_ptr);
}

static int
PVWatts_set_shading_timestep(PVWattsObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_Pvwattsv5Lifetime_PVWatts_shading_timestep_mset, self->data_ptr);
}

static PyObject *
PVWatts_get_system_capacity(PVWattsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvwattsv5Lifetime_PVWatts_system_capacity_nget, self->data_ptr);
}

static int
PVWatts_set_system_capacity(PVWattsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvwattsv5Lifetime_PVWatts_system_capacity_nset, self->data_ptr);
}

static PyObject *
PVWatts_get_tilt(PVWattsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvwattsv5Lifetime_PVWatts_tilt_nget, self->data_ptr);
}

static int
PVWatts_set_tilt(PVWattsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvwattsv5Lifetime_PVWatts_tilt_nset, self->data_ptr);
}

static PyGetSetDef PVWatts_getset[] = {
{"array_type", (getter)PVWatts_get_array_type,(setter)PVWatts_set_array_type,
	PyDoc_STR("*float*: Array type [0/1/2/3/4]\n\n*Info*: Fixed OR,Fixed Roof,1Axis,Backtracked,2Axis\n\n*Constraints*: MIN=0,MAX=4,INTEGER\n\n*Required*: True"),
 	NULL},
{"azimuth", (getter)PVWatts_get_azimuth,(setter)PVWatts_set_azimuth,
	PyDoc_STR("*float*: Azimuth angle [deg]\n\n*Options*: E=90,S=180,W=270\n\n*Constraints*: MIN=0,MAX=360\n\n*Required*: array_type<4"),
 	NULL},
{"dc_ac_ratio", (getter)PVWatts_get_dc_ac_ratio,(setter)PVWatts_set_dc_ac_ratio,
	PyDoc_STR("*float*: DC to AC ratio [ratio]\n\n*Constraints*: POSITIVE\n\n*Required*: set to 1.1 if not provided."),
 	NULL},
{"gcr", (getter)PVWatts_get_gcr,(setter)PVWatts_set_gcr,
	PyDoc_STR("*float*: Ground coverage ratio [0..1]\n\n*Constraints*: MIN=0,MAX=3\n\n*Required*: set to 0.4 if not provided."),
 	NULL},
{"inv_eff", (getter)PVWatts_get_inv_eff,(setter)PVWatts_set_inv_eff,
	PyDoc_STR("*float*: Inverter efficiency at rated power [%]\n\n*Constraints*: MIN=90,MAX=99.5\n\n*Required*: set to 96 if not provided."),
 	NULL},
{"losses", (getter)PVWatts_get_losses,(setter)PVWatts_set_losses,
	PyDoc_STR("*float*: System losses [%]\n\n*Info*: Total system losses\n\n*Constraints*: MIN=-5,MAX=99\n\n*Required*: True"),
 	NULL},
{"module_type", (getter)PVWatts_get_module_type,(setter)PVWatts_set_module_type,
	PyDoc_STR("*float*: Module type [0/1/2]\n\n*Info*: Standard,Premium,Thin film\n\n*Constraints*: MIN=0,MAX=2,INTEGER\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"shading_azal", (getter)PVWatts_get_shading_azal,(setter)PVWatts_set_shading_azal,
	PyDoc_STR("*sequence[sequence]*: Azimuth x altitude beam shading loss [%]\n\n*Required*: False"),
 	NULL},
{"shading_diff", (getter)PVWatts_get_shading_diff,(setter)PVWatts_set_shading_diff,
	PyDoc_STR("*float*: Diffuse shading loss [%]\n\n*Required*: False"),
 	NULL},
{"shading_mxh", (getter)PVWatts_get_shading_mxh,(setter)PVWatts_set_shading_mxh,
	PyDoc_STR("*sequence[sequence]*: Month x Hour beam shading loss [%]\n\n*Required*: False"),
 	NULL},
{"shading_timestep", (getter)PVWatts_get_shading_timestep,(setter)PVWatts_set_shading_timestep,
	PyDoc_STR("*sequence[sequence]*: Time step beam shading loss [%]\n\n*Required*: False"),
 	NULL},
{"system_capacity", (getter)PVWatts_get_system_capacity,(setter)PVWatts_set_system_capacity,
	PyDoc_STR("*float*: System size (DC nameplate) [kW]\n\n*Required*: True"),
 	NULL},
{"tilt", (getter)PVWatts_get_tilt,(setter)PVWatts_set_tilt,
	PyDoc_STR("*float*: Tilt angle [deg]\n\n*Options*: H=0,V=90\n\n*Constraints*: MIN=0,MAX=90\n\n*Required*: array_type<4"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject PVWatts_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Pvwattsv5Lifetime.PVWatts",             /*tp_name*/
		sizeof(PVWattsObject),          /*tp_basicsize*/
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
		PVWatts_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		PVWatts_getset,          /*tp_getset*/
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
	 * Battwatts Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Pvwattsv5Lifetime   data_ptr;
} BattwattsObject;

static PyTypeObject Battwatts_Type;

static PyObject *
Battwatts_new(SAM_Pvwattsv5Lifetime data_ptr)
{
	PyObject* new_obj = Battwatts_Type.tp_alloc(&Battwatts_Type,0);

	BattwattsObject* Battwatts_obj = (BattwattsObject*)new_obj;

	Battwatts_obj->data_ptr = data_ptr;

	return new_obj;
}

/* Battwatts methods */

static PyObject *
Battwatts_assign(BattwattsObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Pvwattsv5Lifetime", "Battwatts")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
Battwatts_export(BattwattsObject *self, PyObject *args)
{
	PyTypeObject* tp = &Battwatts_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef Battwatts_methods[] = {
		{"assign",            (PyCFunction)Battwatts_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``Battwatts_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)Battwatts_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
Battwatts_get_batt_simple_enable(BattwattsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvwattsv5Lifetime_Battwatts_batt_simple_enable_nget, self->data_ptr);
}

static int
Battwatts_set_batt_simple_enable(BattwattsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvwattsv5Lifetime_Battwatts_batt_simple_enable_nset, self->data_ptr);
}

static PyGetSetDef Battwatts_getset[] = {
{"batt_simple_enable", (getter)Battwatts_get_batt_simple_enable,(setter)Battwatts_set_batt_simple_enable,
	PyDoc_STR("*float*: Enable Battery [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject Battwatts_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Pvwattsv5Lifetime.Battwatts",             /*tp_name*/
		sizeof(BattwattsObject),          /*tp_basicsize*/
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
		Battwatts_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		Battwatts_getset,          /*tp_getset*/
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
	SAM_Pvwattsv5Lifetime   data_ptr;
} OutputsObject;

static PyTypeObject Outputs_Type;

static PyObject *
Outputs_new(SAM_Pvwattsv5Lifetime data_ptr)
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

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Pvwattsv5Lifetime", "Outputs")){
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
Outputs_get_ac(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvwattsv5Lifetime_Outputs_ac_aget, self->data_ptr);
}

static PyObject *
Outputs_get_ac_annual(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvwattsv5Lifetime_Outputs_ac_annual_nget, self->data_ptr);
}

static PyObject *
Outputs_get_ac_monthly(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvwattsv5Lifetime_Outputs_ac_monthly_aget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_energy(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvwattsv5Lifetime_Outputs_annual_energy_nget, self->data_ptr);
}

static PyObject *
Outputs_get_aoi(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvwattsv5Lifetime_Outputs_aoi_aget, self->data_ptr);
}

static PyObject *
Outputs_get_capacity_factor(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvwattsv5Lifetime_Outputs_capacity_factor_nget, self->data_ptr);
}

static PyObject *
Outputs_get_city(OutputsObject *self, void *closure)
{
	return PySAM_string_getter(SAM_Pvwattsv5Lifetime_Outputs_city_sget, self->data_ptr);
}

static PyObject *
Outputs_get_dc(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvwattsv5Lifetime_Outputs_dc_aget, self->data_ptr);
}

static PyObject *
Outputs_get_dc_monthly(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvwattsv5Lifetime_Outputs_dc_monthly_aget, self->data_ptr);
}

static PyObject *
Outputs_get_df(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvwattsv5Lifetime_Outputs_df_aget, self->data_ptr);
}

static PyObject *
Outputs_get_dn(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvwattsv5Lifetime_Outputs_dn_aget, self->data_ptr);
}

static PyObject *
Outputs_get_elev(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvwattsv5Lifetime_Outputs_elev_nget, self->data_ptr);
}

static PyObject *
Outputs_get_gh(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvwattsv5Lifetime_Outputs_gh_aget, self->data_ptr);
}

static PyObject *
Outputs_get_inverter_efficiency(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvwattsv5Lifetime_Outputs_inverter_efficiency_nget, self->data_ptr);
}

static PyObject *
Outputs_get_inverter_model(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvwattsv5Lifetime_Outputs_inverter_model_nget, self->data_ptr);
}

static PyObject *
Outputs_get_kwh_per_kw(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvwattsv5Lifetime_Outputs_kwh_per_kw_nget, self->data_ptr);
}

static PyObject *
Outputs_get_lat(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvwattsv5Lifetime_Outputs_lat_nget, self->data_ptr);
}

static PyObject *
Outputs_get_location(OutputsObject *self, void *closure)
{
	return PySAM_string_getter(SAM_Pvwattsv5Lifetime_Outputs_location_sget, self->data_ptr);
}

static PyObject *
Outputs_get_lon(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvwattsv5Lifetime_Outputs_lon_nget, self->data_ptr);
}

static PyObject *
Outputs_get_monthly_energy(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvwattsv5Lifetime_Outputs_monthly_energy_aget, self->data_ptr);
}

static PyObject *
Outputs_get_percent_complete(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvwattsv5Lifetime_Outputs_percent_complete_nget, self->data_ptr);
}

static PyObject *
Outputs_get_poa(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvwattsv5Lifetime_Outputs_poa_aget, self->data_ptr);
}

static PyObject *
Outputs_get_poa_monthly(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvwattsv5Lifetime_Outputs_poa_monthly_aget, self->data_ptr);
}

static PyObject *
Outputs_get_shad_beam_factor(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvwattsv5Lifetime_Outputs_shad_beam_factor_aget, self->data_ptr);
}

static PyObject *
Outputs_get_solrad_annual(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvwattsv5Lifetime_Outputs_solrad_annual_nget, self->data_ptr);
}

static PyObject *
Outputs_get_solrad_monthly(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvwattsv5Lifetime_Outputs_solrad_monthly_aget, self->data_ptr);
}

static PyObject *
Outputs_get_state(OutputsObject *self, void *closure)
{
	return PySAM_string_getter(SAM_Pvwattsv5Lifetime_Outputs_state_sget, self->data_ptr);
}

static PyObject *
Outputs_get_sunup(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvwattsv5Lifetime_Outputs_sunup_aget, self->data_ptr);
}

static PyObject *
Outputs_get_tamb(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvwattsv5Lifetime_Outputs_tamb_aget, self->data_ptr);
}

static PyObject *
Outputs_get_tcell(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvwattsv5Lifetime_Outputs_tcell_aget, self->data_ptr);
}

static PyObject *
Outputs_get_tpoa(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvwattsv5Lifetime_Outputs_tpoa_aget, self->data_ptr);
}

static PyObject *
Outputs_get_ts_shift_hours(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvwattsv5Lifetime_Outputs_ts_shift_hours_nget, self->data_ptr);
}

static PyObject *
Outputs_get_tz(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvwattsv5Lifetime_Outputs_tz_nget, self->data_ptr);
}

static PyObject *
Outputs_get_wspd(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvwattsv5Lifetime_Outputs_wspd_aget, self->data_ptr);
}

static PyGetSetDef Outputs_getset[] = {
{"ac", (getter)Outputs_get_ac,(setter)0,
	PyDoc_STR("*sequence*: AC inverter power [W]"),
 	NULL},
{"ac_annual", (getter)Outputs_get_ac_annual,(setter)0,
	PyDoc_STR("*float*: Annual AC system output [kWh]"),
 	NULL},
{"ac_monthly", (getter)Outputs_get_ac_monthly,(setter)0,
	PyDoc_STR("*sequence*: AC system output [kWh]"),
 	NULL},
{"annual_energy", (getter)Outputs_get_annual_energy,(setter)0,
	PyDoc_STR("*float*: Annual energy [kWh]"),
 	NULL},
{"aoi", (getter)Outputs_get_aoi,(setter)0,
	PyDoc_STR("*sequence*: Angle of incidence [deg]"),
 	NULL},
{"capacity_factor", (getter)Outputs_get_capacity_factor,(setter)0,
	PyDoc_STR("*float*: Capacity factor [%]"),
 	NULL},
{"city", (getter)Outputs_get_city,(setter)0,
	PyDoc_STR("*str*: City"),
 	NULL},
{"dc", (getter)Outputs_get_dc,(setter)0,
	PyDoc_STR("*sequence*: DC array power [W]"),
 	NULL},
{"dc_monthly", (getter)Outputs_get_dc_monthly,(setter)0,
	PyDoc_STR("*sequence*: DC array output [kWh]"),
 	NULL},
{"df", (getter)Outputs_get_df,(setter)0,
	PyDoc_STR("*sequence*: Diffuse irradiance [W/m2]"),
 	NULL},
{"dn", (getter)Outputs_get_dn,(setter)0,
	PyDoc_STR("*sequence*: Beam irradiance [W/m2]"),
 	NULL},
{"elev", (getter)Outputs_get_elev,(setter)0,
	PyDoc_STR("*float*: Site elevation [m]"),
 	NULL},
{"gh", (getter)Outputs_get_gh,(setter)0,
	PyDoc_STR("*sequence*: Global horizontal irradiance [W/m2]"),
 	NULL},
{"inverter_efficiency", (getter)Outputs_get_inverter_efficiency,(setter)0,
	PyDoc_STR("*float*: Inverter efficiency at rated power [%]"),
 	NULL},
{"inverter_model", (getter)Outputs_get_inverter_model,(setter)0,
	PyDoc_STR("*float*: Inverter model specifier"),
 	NULL},
{"kwh_per_kw", (getter)Outputs_get_kwh_per_kw,(setter)0,
	PyDoc_STR("*float*: First year kWh/kW"),
 	NULL},
{"lat", (getter)Outputs_get_lat,(setter)0,
	PyDoc_STR("*float*: Latitude [deg]"),
 	NULL},
{"location", (getter)Outputs_get_location,(setter)0,
	PyDoc_STR("*str*: Location ID"),
 	NULL},
{"lon", (getter)Outputs_get_lon,(setter)0,
	PyDoc_STR("*float*: Longitude [deg]"),
 	NULL},
{"monthly_energy", (getter)Outputs_get_monthly_energy,(setter)0,
	PyDoc_STR("*sequence*: Monthly energy [kWh]"),
 	NULL},
{"percent_complete", (getter)Outputs_get_percent_complete,(setter)0,
	PyDoc_STR("*float*: Estimated percent of total comleted simulation [%]"),
 	NULL},
{"poa", (getter)Outputs_get_poa,(setter)0,
	PyDoc_STR("*sequence*: Plane of array irradiance [W/m2]"),
 	NULL},
{"poa_monthly", (getter)Outputs_get_poa_monthly,(setter)0,
	PyDoc_STR("*sequence*: Plane of array irradiance [kWh/m2]"),
 	NULL},
{"shad_beam_factor", (getter)Outputs_get_shad_beam_factor,(setter)0,
	PyDoc_STR("*sequence*: Shading factor for beam radiation"),
 	NULL},
{"solrad_annual", (getter)Outputs_get_solrad_annual,(setter)0,
	PyDoc_STR("*float*: Daily average solar irradiance [kWh/m2/day]"),
 	NULL},
{"solrad_monthly", (getter)Outputs_get_solrad_monthly,(setter)0,
	PyDoc_STR("*sequence*: Daily average solar irradiance [kWh/m2/day]"),
 	NULL},
{"state", (getter)Outputs_get_state,(setter)0,
	PyDoc_STR("*str*: State"),
 	NULL},
{"sunup", (getter)Outputs_get_sunup,(setter)0,
	PyDoc_STR("*sequence*: Sun up over horizon [0/1]"),
 	NULL},
{"tamb", (getter)Outputs_get_tamb,(setter)0,
	PyDoc_STR("*sequence*: Ambient temperature [C]"),
 	NULL},
{"tcell", (getter)Outputs_get_tcell,(setter)0,
	PyDoc_STR("*sequence*: Module temperature [C]"),
 	NULL},
{"tpoa", (getter)Outputs_get_tpoa,(setter)0,
	PyDoc_STR("*sequence*: Transmitted plane of array irradiance [W/m2]"),
 	NULL},
{"ts_shift_hours", (getter)Outputs_get_ts_shift_hours,(setter)0,
	PyDoc_STR("*float*: Time offset for interpreting time series outputs [hours]"),
 	NULL},
{"tz", (getter)Outputs_get_tz,(setter)0,
	PyDoc_STR("*float*: Time zone [hr]"),
 	NULL},
{"wspd", (getter)Outputs_get_wspd,(setter)0,
	PyDoc_STR("*sequence*: Wind speed [m/s]"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject Outputs_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Pvwattsv5Lifetime.Outputs",             /*tp_name*/
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
 * Pvwattsv5Lifetime
 */

typedef struct {
	PyObject_HEAD
	PyObject            *x_attr;        /* Attributes dictionary */
	SAM_Pvwattsv5Lifetime   data_ptr;
} Pvwattsv5LifetimeObject;

static PyTypeObject Pvwattsv5Lifetime_Type;

#define Pvwattsv5LifetimeObject_Check(v)      (Py_TYPE(v) == &Pvwattsv5Lifetime_Type)

static Pvwattsv5LifetimeObject *
newPvwattsv5LifetimeObject(void* data_ptr)
{
	Pvwattsv5LifetimeObject *self;
	self = PyObject_New(Pvwattsv5LifetimeObject, &Pvwattsv5Lifetime_Type);

	PySAM_TECH_ATTR("Pvwattsv5Lifetime", SAM_Pvwattsv5Lifetime_construct)

	PyObject* Common_obj = Common_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "Common", Common_obj);
	Py_DECREF(Common_obj);

	PyObject* FinancialAnalysisParameters_obj = FinancialAnalysisParameters_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "FinancialAnalysisParameters", FinancialAnalysisParameters_obj);
	Py_DECREF(FinancialAnalysisParameters_obj);

	PyObject* LifetimePV_obj = LifetimePV_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "LifetimePV", LifetimePV_obj);
	Py_DECREF(LifetimePV_obj);

	PyObject* Weather_obj = Weather_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "Weather", Weather_obj);
	Py_DECREF(Weather_obj);

	PyObject* PVWatts_obj = PVWatts_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "PVWatts", PVWatts_obj);
	Py_DECREF(PVWatts_obj);

	PyObject* Battwatts_obj = Battwatts_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "Battwatts", Battwatts_obj);
	Py_DECREF(Battwatts_obj);

	PyObject* AdjustmentFactorsModule = PyImport_ImportModule("AdjustmentFactors");

	PyObject* data_cap = PyCapsule_New(self->data_ptr, NULL, NULL);
	PyObject* Adjust_obj = PyObject_CallMethod(AdjustmentFactorsModule, "new", "(O)", data_cap);
	Py_XDECREF(data_cap);
	Py_XDECREF(AdjustmentFactorsModule);

	if (!Adjust_obj){
		PyErr_SetString(PySAM_ErrorObject, "Couldn't create AdjustmentFactorsObject\n");
		return NULL;
	}

	PyDict_SetItemString(attr_dict, "AdjustmentFactors", Adjust_obj);
	Py_DECREF(Adjust_obj);

	PyObject* Outputs_obj = Outputs_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "Outputs", Outputs_obj);
	Py_DECREF(Outputs_obj);


	return self;
}

/* Pvwattsv5Lifetime methods */

static void
Pvwattsv5Lifetime_dealloc(Pvwattsv5LifetimeObject *self)
{
	Py_XDECREF(self->x_attr);
	SAM_Pvwattsv5Lifetime_destruct(self->data_ptr);
	PyObject_Del(self);
}


static PyObject *
Pvwattsv5Lifetime_execute(Pvwattsv5LifetimeObject *self, PyObject *args)
{
	int verbosity = 0;

	if (!PyArg_ParseTuple(args, "|i", &verbosity))
		return NULL;

	SAM_error error = new_error();
	SAM_Pvwattsv5Lifetime_execute(self->data_ptr, verbosity, &error);
	if (PySAM_has_error(error )) return NULL;

	Py_INCREF(Py_None);
	return Py_None;
}


static PyObject *
Pvwattsv5Lifetime_assign(Pvwattsv5LifetimeObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_nested_dict((PyObject*)self, self->x_attr, self->data_ptr, dict, "Pvwattsv5Lifetime"))
		return NULL;

	Py_INCREF(Py_None);
	return Py_None;
}


static PyObject *
Pvwattsv5Lifetime_export(Pvwattsv5LifetimeObject *self, PyObject *args)
{
	return PySAM_export_to_nested_dict((PyObject *) self, self->x_attr);
}

static PyMethodDef Pvwattsv5Lifetime_methods[] = {
		{"execute",            (PyCFunction)Pvwattsv5Lifetime_execute,  METH_VARARGS,
				PyDoc_STR("execute(int verbosity) -> None\n Execute simulation with verbosity level 0 (default) or 1")},
		{"assign",            (PyCFunction)Pvwattsv5Lifetime_assign,  METH_VARARGS,
				PyDoc_STR("assign(dict) -> None\n Assign attributes from nested dictionary, except for Outputs\n\n``nested_dict = { 'Common': { var: val, ...}, ...}``")},
		{"export",            (PyCFunction)Pvwattsv5Lifetime_export,  METH_VARARGS,
				PyDoc_STR("export() -> dict\n Export attributes into nested dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
Pvwattsv5Lifetime_getattro(Pvwattsv5LifetimeObject *self, PyObject *name)
{
	return PySAM_get_attr((PyObject*) self, (PyObject*) self->x_attr, name);
}

static int
Pvwattsv5Lifetime_setattr(Pvwattsv5LifetimeObject *self, const char *name, PyObject *v)
{
	return PySAM_set_attr((PyObject*)self, (PyObject*)self->x_attr, name, v);
}

static PyTypeObject Pvwattsv5Lifetime_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Pvwattsv5Lifetime",            /*tp_name*/
		sizeof(Pvwattsv5LifetimeObject),/*tp_basicsize*/
		0,                          /*tp_itemsize*/
		/* methods */
		(destructor)Pvwattsv5Lifetime_dealloc,    /*tp_dealloc*/
		0,                          /*tp_print*/
		(getattrfunc)0,             /*tp_getattr*/
		(setattrfunc)Pvwattsv5Lifetime_setattr,   /*tp_setattr*/
		0,                          /*tp_reserved*/
		0,                          /*tp_repr*/
		0,                          /*tp_as_number*/
		0,                          /*tp_as_sequence*/
		0,                          /*tp_as_mapping*/
		0,                          /*tp_hash*/
		0,                          /*tp_call*/
		0,                          /*tp_str*/
		(getattrofunc)Pvwattsv5Lifetime_getattro, /*tp_getattro*/
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
		Pvwattsv5Lifetime_methods,      /*tp_methods*/
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


/* Function of no arguments returning new Pvwattsv5Lifetime object */

static PyObject *
Pvwattsv5Lifetime_new(PyObject *self, PyObject *args)
{
	Pvwattsv5LifetimeObject *rv;
	rv = newPvwattsv5LifetimeObject(0);
	if (rv == NULL)
		return NULL;
	return (PyObject *)rv;
}

static PyObject *
Pvwattsv5Lifetime_wrap(PyObject *self, PyObject *args)
{
	Pvwattsv5LifetimeObject *rv;
	long long int ptr = 0;  // 64 bit arch
	if (!PyArg_ParseTuple(args, "L:wrap", &ptr)){
		PyErr_BadArgument();
		return NULL;
	}
	rv = newPvwattsv5LifetimeObject((void*)ptr);
	if (rv == NULL)
		return NULL;
	return (PyObject *)rv;
}

static PyObject *
Pvwattsv5Lifetime_default(PyObject *self, PyObject *args)
{
	Pvwattsv5LifetimeObject *rv;
	char* def = 0;
	if (!PyArg_ParseTuple(args, "s:default", &def)){
		PyErr_BadArgument();
		return NULL;
	}
	rv = newPvwattsv5LifetimeObject(0);
	if (rv == NULL)
		return NULL;

	PySAM_load_defaults((PyObject*)rv, rv->x_attr, rv->data_ptr, "Pvwattsv5Lifetime", def);

	return (PyObject *)rv;
}

/* ---------- */


/* List of functions defined in the module */

static PyMethodDef Pvwattsv5LifetimeModule_methods[] = {
		{"new",             Pvwattsv5Lifetime_new,         METH_VARARGS,
				PyDoc_STR("new() -> Pvwattsv5Lifetime")},
		{"default",             Pvwattsv5Lifetime_default,         METH_VARARGS,
				PyDoc_STR("default(config) -> Pvwattsv5Lifetime\n\nUse financial model-specific default attributes\n"
				"config options:\n\n- \"FuelCellCommercial\"\n- \"FuelCellSingleOwner\"")},
		{"wrap",             Pvwattsv5Lifetime_wrap,         METH_VARARGS,
				PyDoc_STR("wrap(ssc_data_t) -> Pvwattsv5Lifetime\n\nUse existing PySSC data\n\n.. warning::\n\n	Do not call PySSC.data_free on the ssc_data_t provided to ``wrap``")},
		{NULL,              NULL}           /* sentinel */
};

PyDoc_STRVAR(module_doc,
			 "PVWatts photovoltaic system model for multi-year lifetime analysis");


static int
Pvwattsv5LifetimeModule_exec(PyObject *m)
{
	/* Finalize the type object including setting type of the new type
	 * object; doing it here is required for portability, too. */

	if (PySAM_load_lib(m) < 0) goto fail;
	if (PySAM_init_error(m) < 0) goto fail;

	Pvwattsv5Lifetime_Type.tp_dict = PyDict_New();
	if (!Pvwattsv5Lifetime_Type.tp_dict) { goto fail; }

	/// Add the AdjustmentFactors type object to Pvwattsv5Lifetime_Type
	PyObject* AdjustmentFactorsModule = PyImport_ImportModule("AdjustmentFactors");
	if (!AdjustmentFactorsModule){
		PyErr_SetImportError(PyUnicode_FromString("Could not import AdjustmentFactors module."), NULL, NULL);
	}

	PyTypeObject* AdjustmentFactors_Type = (PyTypeObject*)PyObject_GetAttrString(AdjustmentFactorsModule, "AdjustmentFactors");
	if (!AdjustmentFactors_Type){
		PyErr_SetImportError(PyUnicode_FromString("Could not import AdjustmentFactors type."), NULL, NULL);
	}
	Py_XDECREF(AdjustmentFactorsModule);

	if (PyType_Ready(AdjustmentFactors_Type) < 0) { goto fail; }
	PyDict_SetItemString(Pvwattsv5Lifetime_Type.tp_dict,
						 "AdjustmentFactors",
						 (PyObject*)AdjustmentFactors_Type);
	Py_DECREF(&AdjustmentFactors_Type);
	Py_XDECREF(AdjustmentFactors_Type);

	/// Add the Common type object to Pvwattsv5Lifetime_Type
	if (PyType_Ready(&Common_Type) < 0) { goto fail; }
	PyDict_SetItemString(Pvwattsv5Lifetime_Type.tp_dict,
				"Common",
				(PyObject*)&Common_Type);
	Py_DECREF(&Common_Type);

	/// Add the FinancialAnalysisParameters type object to Pvwattsv5Lifetime_Type
	if (PyType_Ready(&FinancialAnalysisParameters_Type) < 0) { goto fail; }
	PyDict_SetItemString(Pvwattsv5Lifetime_Type.tp_dict,
				"FinancialAnalysisParameters",
				(PyObject*)&FinancialAnalysisParameters_Type);
	Py_DECREF(&FinancialAnalysisParameters_Type);

	/// Add the LifetimePV type object to Pvwattsv5Lifetime_Type
	if (PyType_Ready(&LifetimePV_Type) < 0) { goto fail; }
	PyDict_SetItemString(Pvwattsv5Lifetime_Type.tp_dict,
				"LifetimePV",
				(PyObject*)&LifetimePV_Type);
	Py_DECREF(&LifetimePV_Type);

	/// Add the Weather type object to Pvwattsv5Lifetime_Type
	if (PyType_Ready(&Weather_Type) < 0) { goto fail; }
	PyDict_SetItemString(Pvwattsv5Lifetime_Type.tp_dict,
				"Weather",
				(PyObject*)&Weather_Type);
	Py_DECREF(&Weather_Type);

	/// Add the PVWatts type object to Pvwattsv5Lifetime_Type
	if (PyType_Ready(&PVWatts_Type) < 0) { goto fail; }
	PyDict_SetItemString(Pvwattsv5Lifetime_Type.tp_dict,
				"PVWatts",
				(PyObject*)&PVWatts_Type);
	Py_DECREF(&PVWatts_Type);

	/// Add the Battwatts type object to Pvwattsv5Lifetime_Type
	if (PyType_Ready(&Battwatts_Type) < 0) { goto fail; }
	PyDict_SetItemString(Pvwattsv5Lifetime_Type.tp_dict,
				"Battwatts",
				(PyObject*)&Battwatts_Type);
	Py_DECREF(&Battwatts_Type);

	/// Add the Outputs type object to Pvwattsv5Lifetime_Type
	if (PyType_Ready(&Outputs_Type) < 0) { goto fail; }
	PyDict_SetItemString(Pvwattsv5Lifetime_Type.tp_dict,
				"Outputs",
				(PyObject*)&Outputs_Type);
	Py_DECREF(&Outputs_Type);

	/// Add the Pvwattsv5Lifetime type object to the module
	if (PyType_Ready(&Pvwattsv5Lifetime_Type) < 0) { goto fail; }
	PyModule_AddObject(m,
				"Pvwattsv5Lifetime",
				(PyObject*)&Pvwattsv5Lifetime_Type);

	return 0;
	fail:
	Py_XDECREF(m);
	return -1;
}

static struct PyModuleDef_Slot Pvwattsv5LifetimeModule_slots[] = {
		{Py_mod_exec, Pvwattsv5LifetimeModule_exec},
		{0, NULL},
};

static struct PyModuleDef Pvwattsv5LifetimeModule = {
		PyModuleDef_HEAD_INIT,
		"Pvwattsv5Lifetime",
		module_doc,
		0,
		Pvwattsv5LifetimeModule_methods,
		Pvwattsv5LifetimeModule_slots,
		NULL,
		NULL,
		NULL
};

/* Export function for the module */

PyMODINIT_FUNC
PyInit_Pvwattsv5Lifetime(void)
{
	return PyModuleDef_Init(&Pvwattsv5LifetimeModule);
}