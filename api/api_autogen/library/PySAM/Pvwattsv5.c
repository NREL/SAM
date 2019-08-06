#include <Python.h>

#include <SAM_Pvwattsv5.h>
#include <SAM_api.h>

#include "PySAM_utils.h"



	/*
	 * LocationAndResource Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Pvwattsv5   data_ptr;
} LocationAndResourceObject;

static PyTypeObject LocationAndResource_Type;

static PyObject *
LocationAndResource_new(SAM_Pvwattsv5 data_ptr)
{
	PyObject* new_obj = LocationAndResource_Type.tp_alloc(&LocationAndResource_Type,0);

	LocationAndResourceObject* LocationAndResource_obj = (LocationAndResourceObject*)new_obj;

	LocationAndResource_obj->data_ptr = data_ptr;

	return new_obj;
}

/* LocationAndResource methods */

static PyObject *
LocationAndResource_assign(LocationAndResourceObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Pvwattsv5", "LocationAndResource")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
LocationAndResource_export(LocationAndResourceObject *self, PyObject *args)
{
	PyTypeObject* tp = &LocationAndResource_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef LocationAndResource_methods[] = {
		{"assign",            (PyCFunction)LocationAndResource_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``LocationAndResource_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)LocationAndResource_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
LocationAndResource_get_solar_resource_data(LocationAndResourceObject *self, void *closure)
{
	return PySAM_table_getter(SAM_Pvwattsv5_LocationAndResource_solar_resource_data_tget, self->data_ptr);
}

static int
LocationAndResource_set_solar_resource_data(LocationAndResourceObject *self, PyObject *value, void *closure)
{
	return PySAM_table_setter(value, SAM_Pvwattsv5_LocationAndResource_solar_resource_data_tset, self->data_ptr);
}

static PyObject *
LocationAndResource_get_solar_resource_file(LocationAndResourceObject *self, void *closure)
{
	return PySAM_string_getter(SAM_Pvwattsv5_LocationAndResource_solar_resource_file_sget, self->data_ptr);
}

static int
LocationAndResource_set_solar_resource_file(LocationAndResourceObject *self, PyObject *value, void *closure)
{
	return PySAM_string_setter(value, SAM_Pvwattsv5_LocationAndResource_solar_resource_file_sset, self->data_ptr);
}

static PyGetSetDef LocationAndResource_getset[] = {
{"solar_resource_data", (getter)LocationAndResource_get_solar_resource_data,(setter)LocationAndResource_set_solar_resource_data,
	PyDoc_STR("*dict*: Weather data\n\n*Info*: dn,df,tdry,wspd,lat,lon,tz\n\n*Required*: False"),
 	NULL},
{"solar_resource_file", (getter)LocationAndResource_get_solar_resource_file,(setter)LocationAndResource_set_solar_resource_file,
	PyDoc_STR("*str*: Weather file path\n\n*Required*: False"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject LocationAndResource_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Pvwattsv5.LocationAndResource",             /*tp_name*/
		sizeof(LocationAndResourceObject),          /*tp_basicsize*/
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
		LocationAndResource_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		LocationAndResource_getset,          /*tp_getset*/
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
	 * SystemDesign Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Pvwattsv5   data_ptr;
} SystemDesignObject;

static PyTypeObject SystemDesign_Type;

static PyObject *
SystemDesign_new(SAM_Pvwattsv5 data_ptr)
{
	PyObject* new_obj = SystemDesign_Type.tp_alloc(&SystemDesign_Type,0);

	SystemDesignObject* SystemDesign_obj = (SystemDesignObject*)new_obj;

	SystemDesign_obj->data_ptr = data_ptr;

	return new_obj;
}

/* SystemDesign methods */

static PyObject *
SystemDesign_assign(SystemDesignObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Pvwattsv5", "SystemDesign")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
SystemDesign_export(SystemDesignObject *self, PyObject *args)
{
	PyTypeObject* tp = &SystemDesign_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef SystemDesign_methods[] = {
		{"assign",            (PyCFunction)SystemDesign_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``SystemDesign_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)SystemDesign_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
SystemDesign_get_array_type(SystemDesignObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvwattsv5_SystemDesign_array_type_nget, self->data_ptr);
}

static int
SystemDesign_set_array_type(SystemDesignObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvwattsv5_SystemDesign_array_type_nset, self->data_ptr);
}

static PyObject *
SystemDesign_get_azimuth(SystemDesignObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvwattsv5_SystemDesign_azimuth_nget, self->data_ptr);
}

static int
SystemDesign_set_azimuth(SystemDesignObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvwattsv5_SystemDesign_azimuth_nset, self->data_ptr);
}

static PyObject *
SystemDesign_get_batt_simple_enable(SystemDesignObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvwattsv5_SystemDesign_batt_simple_enable_nget, self->data_ptr);
}

static int
SystemDesign_set_batt_simple_enable(SystemDesignObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvwattsv5_SystemDesign_batt_simple_enable_nset, self->data_ptr);
}

static PyObject *
SystemDesign_get_dc_ac_ratio(SystemDesignObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvwattsv5_SystemDesign_dc_ac_ratio_nget, self->data_ptr);
}

static int
SystemDesign_set_dc_ac_ratio(SystemDesignObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvwattsv5_SystemDesign_dc_ac_ratio_nset, self->data_ptr);
}

static PyObject *
SystemDesign_get_gcr(SystemDesignObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvwattsv5_SystemDesign_gcr_nget, self->data_ptr);
}

static int
SystemDesign_set_gcr(SystemDesignObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvwattsv5_SystemDesign_gcr_nset, self->data_ptr);
}

static PyObject *
SystemDesign_get_inv_eff(SystemDesignObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvwattsv5_SystemDesign_inv_eff_nget, self->data_ptr);
}

static int
SystemDesign_set_inv_eff(SystemDesignObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvwattsv5_SystemDesign_inv_eff_nset, self->data_ptr);
}

static PyObject *
SystemDesign_get_losses(SystemDesignObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvwattsv5_SystemDesign_losses_nget, self->data_ptr);
}

static int
SystemDesign_set_losses(SystemDesignObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvwattsv5_SystemDesign_losses_nset, self->data_ptr);
}

static PyObject *
SystemDesign_get_module_type(SystemDesignObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvwattsv5_SystemDesign_module_type_nget, self->data_ptr);
}

static int
SystemDesign_set_module_type(SystemDesignObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvwattsv5_SystemDesign_module_type_nset, self->data_ptr);
}

static PyObject *
SystemDesign_get_shading_azal(SystemDesignObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Pvwattsv5_SystemDesign_shading_azal_mget, self->data_ptr);
}

static int
SystemDesign_set_shading_azal(SystemDesignObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_Pvwattsv5_SystemDesign_shading_azal_mset, self->data_ptr);
}

static PyObject *
SystemDesign_get_shading_diff(SystemDesignObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvwattsv5_SystemDesign_shading_diff_nget, self->data_ptr);
}

static int
SystemDesign_set_shading_diff(SystemDesignObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvwattsv5_SystemDesign_shading_diff_nset, self->data_ptr);
}

static PyObject *
SystemDesign_get_shading_mxh(SystemDesignObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Pvwattsv5_SystemDesign_shading_mxh_mget, self->data_ptr);
}

static int
SystemDesign_set_shading_mxh(SystemDesignObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_Pvwattsv5_SystemDesign_shading_mxh_mset, self->data_ptr);
}

static PyObject *
SystemDesign_get_shading_timestep(SystemDesignObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Pvwattsv5_SystemDesign_shading_timestep_mget, self->data_ptr);
}

static int
SystemDesign_set_shading_timestep(SystemDesignObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_Pvwattsv5_SystemDesign_shading_timestep_mset, self->data_ptr);
}

static PyObject *
SystemDesign_get_system_capacity(SystemDesignObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvwattsv5_SystemDesign_system_capacity_nget, self->data_ptr);
}

static int
SystemDesign_set_system_capacity(SystemDesignObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvwattsv5_SystemDesign_system_capacity_nset, self->data_ptr);
}

static PyObject *
SystemDesign_get_tilt(SystemDesignObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvwattsv5_SystemDesign_tilt_nget, self->data_ptr);
}

static int
SystemDesign_set_tilt(SystemDesignObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvwattsv5_SystemDesign_tilt_nset, self->data_ptr);
}

static PyGetSetDef SystemDesign_getset[] = {
{"array_type", (getter)SystemDesign_get_array_type,(setter)SystemDesign_set_array_type,
	PyDoc_STR("*float*: Array type [0/1/2/3/4]\n\n*Info*: Fixed OR,Fixed Roof,1Axis,Backtracked,2Axis\n\n*Constraints*: MIN=0,MAX=4,INTEGER\n\n*Required*: True"),
 	NULL},
{"azimuth", (getter)SystemDesign_get_azimuth,(setter)SystemDesign_set_azimuth,
	PyDoc_STR("*float*: Azimuth angle [deg]\n\n*Options*: E=90,S=180,W=270\n\n*Constraints*: MIN=0,MAX=360\n\n*Required*: array_type<4"),
 	NULL},
{"batt_simple_enable", (getter)SystemDesign_get_batt_simple_enable,(setter)SystemDesign_set_batt_simple_enable,
	PyDoc_STR("*float*: Enable Battery [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"dc_ac_ratio", (getter)SystemDesign_get_dc_ac_ratio,(setter)SystemDesign_set_dc_ac_ratio,
	PyDoc_STR("*float*: DC to AC ratio [ratio]\n\n*Constraints*: POSITIVE\n\n*Required*: set to 1.1 if not provided."),
 	NULL},
{"gcr", (getter)SystemDesign_get_gcr,(setter)SystemDesign_set_gcr,
	PyDoc_STR("*float*: Ground coverage ratio [0..1]\n\n*Constraints*: MIN=0,MAX=3\n\n*Required*: set to 0.4 if not provided."),
 	NULL},
{"inv_eff", (getter)SystemDesign_get_inv_eff,(setter)SystemDesign_set_inv_eff,
	PyDoc_STR("*float*: Inverter efficiency at rated power [%]\n\n*Constraints*: MIN=90,MAX=99.5\n\n*Required*: set to 96 if not provided."),
 	NULL},
{"losses", (getter)SystemDesign_get_losses,(setter)SystemDesign_set_losses,
	PyDoc_STR("*float*: System losses [%]\n\n*Info*: Total system losses\n\n*Constraints*: MIN=-5,MAX=99\n\n*Required*: True"),
 	NULL},
{"module_type", (getter)SystemDesign_get_module_type,(setter)SystemDesign_set_module_type,
	PyDoc_STR("*float*: Module type [0/1/2]\n\n*Info*: Standard,Premium,Thin film\n\n*Constraints*: MIN=0,MAX=2,INTEGER\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"shading_azal", (getter)SystemDesign_get_shading_azal,(setter)SystemDesign_set_shading_azal,
	PyDoc_STR("*sequence[sequence]*: Azimuth x altitude beam shading loss [%]\n\n*Required*: False"),
 	NULL},
{"shading_diff", (getter)SystemDesign_get_shading_diff,(setter)SystemDesign_set_shading_diff,
	PyDoc_STR("*float*: Diffuse shading loss [%]\n\n*Required*: False"),
 	NULL},
{"shading_mxh", (getter)SystemDesign_get_shading_mxh,(setter)SystemDesign_set_shading_mxh,
	PyDoc_STR("*sequence[sequence]*: Month x Hour beam shading loss [%]\n\n*Required*: False"),
 	NULL},
{"shading_timestep", (getter)SystemDesign_get_shading_timestep,(setter)SystemDesign_set_shading_timestep,
	PyDoc_STR("*sequence[sequence]*: Time step beam shading loss [%]\n\n*Required*: False"),
 	NULL},
{"system_capacity", (getter)SystemDesign_get_system_capacity,(setter)SystemDesign_set_system_capacity,
	PyDoc_STR("*float*: System size (DC nameplate) [kW]\n\n*Required*: True"),
 	NULL},
{"tilt", (getter)SystemDesign_get_tilt,(setter)SystemDesign_set_tilt,
	PyDoc_STR("*float*: Tilt angle [deg]\n\n*Options*: H=0,V=90\n\n*Constraints*: MIN=0,MAX=90\n\n*Required*: array_type<4"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject SystemDesign_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Pvwattsv5.SystemDesign",             /*tp_name*/
		sizeof(SystemDesignObject),          /*tp_basicsize*/
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
		SystemDesign_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		SystemDesign_getset,          /*tp_getset*/
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
	SAM_Pvwattsv5   data_ptr;
} OutputsObject;

static PyTypeObject Outputs_Type;

static PyObject *
Outputs_new(SAM_Pvwattsv5 data_ptr)
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

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Pvwattsv5", "Outputs")){
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
	return PySAM_array_getter(SAM_Pvwattsv5_Outputs_ac_aget, self->data_ptr);
}

static PyObject *
Outputs_get_ac_annual(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvwattsv5_Outputs_ac_annual_nget, self->data_ptr);
}

static PyObject *
Outputs_get_ac_monthly(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvwattsv5_Outputs_ac_monthly_aget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_energy(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvwattsv5_Outputs_annual_energy_nget, self->data_ptr);
}

static PyObject *
Outputs_get_aoi(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvwattsv5_Outputs_aoi_aget, self->data_ptr);
}

static PyObject *
Outputs_get_capacity_factor(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvwattsv5_Outputs_capacity_factor_nget, self->data_ptr);
}

static PyObject *
Outputs_get_city(OutputsObject *self, void *closure)
{
	return PySAM_string_getter(SAM_Pvwattsv5_Outputs_city_sget, self->data_ptr);
}

static PyObject *
Outputs_get_dc(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvwattsv5_Outputs_dc_aget, self->data_ptr);
}

static PyObject *
Outputs_get_dc_monthly(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvwattsv5_Outputs_dc_monthly_aget, self->data_ptr);
}

static PyObject *
Outputs_get_df(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvwattsv5_Outputs_df_aget, self->data_ptr);
}

static PyObject *
Outputs_get_dn(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvwattsv5_Outputs_dn_aget, self->data_ptr);
}

static PyObject *
Outputs_get_elev(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvwattsv5_Outputs_elev_nget, self->data_ptr);
}

static PyObject *
Outputs_get_gh(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvwattsv5_Outputs_gh_aget, self->data_ptr);
}

static PyObject *
Outputs_get_inverter_efficiency(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvwattsv5_Outputs_inverter_efficiency_nget, self->data_ptr);
}

static PyObject *
Outputs_get_inverter_model(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvwattsv5_Outputs_inverter_model_nget, self->data_ptr);
}

static PyObject *
Outputs_get_kwh_per_kw(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvwattsv5_Outputs_kwh_per_kw_nget, self->data_ptr);
}

static PyObject *
Outputs_get_lat(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvwattsv5_Outputs_lat_nget, self->data_ptr);
}

static PyObject *
Outputs_get_location(OutputsObject *self, void *closure)
{
	return PySAM_string_getter(SAM_Pvwattsv5_Outputs_location_sget, self->data_ptr);
}

static PyObject *
Outputs_get_lon(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvwattsv5_Outputs_lon_nget, self->data_ptr);
}

static PyObject *
Outputs_get_monthly_energy(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvwattsv5_Outputs_monthly_energy_aget, self->data_ptr);
}

static PyObject *
Outputs_get_poa(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvwattsv5_Outputs_poa_aget, self->data_ptr);
}

static PyObject *
Outputs_get_poa_monthly(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvwattsv5_Outputs_poa_monthly_aget, self->data_ptr);
}

static PyObject *
Outputs_get_shad_beam_factor(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvwattsv5_Outputs_shad_beam_factor_aget, self->data_ptr);
}

static PyObject *
Outputs_get_solrad_annual(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvwattsv5_Outputs_solrad_annual_nget, self->data_ptr);
}

static PyObject *
Outputs_get_solrad_monthly(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvwattsv5_Outputs_solrad_monthly_aget, self->data_ptr);
}

static PyObject *
Outputs_get_state(OutputsObject *self, void *closure)
{
	return PySAM_string_getter(SAM_Pvwattsv5_Outputs_state_sget, self->data_ptr);
}

static PyObject *
Outputs_get_sunup(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvwattsv5_Outputs_sunup_aget, self->data_ptr);
}

static PyObject *
Outputs_get_system_use_lifetime_output(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvwattsv5_Outputs_system_use_lifetime_output_nget, self->data_ptr);
}

static PyObject *
Outputs_get_tamb(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvwattsv5_Outputs_tamb_aget, self->data_ptr);
}

static PyObject *
Outputs_get_tcell(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvwattsv5_Outputs_tcell_aget, self->data_ptr);
}

static PyObject *
Outputs_get_tpoa(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvwattsv5_Outputs_tpoa_aget, self->data_ptr);
}

static PyObject *
Outputs_get_ts_shift_hours(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvwattsv5_Outputs_ts_shift_hours_nget, self->data_ptr);
}

static PyObject *
Outputs_get_tz(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvwattsv5_Outputs_tz_nget, self->data_ptr);
}

static PyObject *
Outputs_get_wspd(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvwattsv5_Outputs_wspd_aget, self->data_ptr);
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
{"system_use_lifetime_output", (getter)Outputs_get_system_use_lifetime_output,(setter)0,
	PyDoc_STR("*float*: Use lifetime output [0/1]"),
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
		"Pvwattsv5.Outputs",             /*tp_name*/
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
 * Pvwattsv5
 */

typedef struct {
	PyObject_HEAD
	PyObject            *x_attr;        /* Attributes dictionary */
	SAM_Pvwattsv5   data_ptr;
} Pvwattsv5Object;

static PyTypeObject Pvwattsv5_Type;

#define Pvwattsv5Object_Check(v)      (Py_TYPE(v) == &Pvwattsv5_Type)

static Pvwattsv5Object *
newPvwattsv5Object(void* data_ptr)
{
	Pvwattsv5Object *self;
	self = PyObject_New(Pvwattsv5Object, &Pvwattsv5_Type);

	PySAM_TECH_ATTR("Pvwattsv5", SAM_Pvwattsv5_construct)

	PyObject* LocationAndResource_obj = LocationAndResource_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "LocationAndResource", LocationAndResource_obj);
	Py_DECREF(LocationAndResource_obj);

	PyObject* SystemDesign_obj = SystemDesign_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "SystemDesign", SystemDesign_obj);
	Py_DECREF(SystemDesign_obj);

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

/* Pvwattsv5 methods */

static void
Pvwattsv5_dealloc(Pvwattsv5Object *self)
{
	Py_XDECREF(self->x_attr);
	SAM_Pvwattsv5_destruct(self->data_ptr);
	PyObject_Del(self);
}


static PyObject *
Pvwattsv5_execute(Pvwattsv5Object *self, PyObject *args)
{
	int verbosity = 0;

	if (!PyArg_ParseTuple(args, "|i", &verbosity))
		return NULL;

	SAM_error error = new_error();
	SAM_Pvwattsv5_execute(self->data_ptr, verbosity, &error);
	if (PySAM_has_error(error )) return NULL;

	Py_INCREF(Py_None);
	return Py_None;
}


static PyObject *
Pvwattsv5_assign(Pvwattsv5Object *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_nested_dict((PyObject*)self, self->x_attr, self->data_ptr, dict, "Pvwattsv5"))
		return NULL;

	Py_INCREF(Py_None);
	return Py_None;
}


static PyObject *
Pvwattsv5_export(Pvwattsv5Object *self, PyObject *args)
{
	return PySAM_export_to_nested_dict((PyObject *) self, self->x_attr);
}

static PyMethodDef Pvwattsv5_methods[] = {
		{"execute",            (PyCFunction)Pvwattsv5_execute,  METH_VARARGS,
				PyDoc_STR("execute(int verbosity) -> None\n Execute simulation with verbosity level 0 (default) or 1")},
		{"assign",            (PyCFunction)Pvwattsv5_assign,  METH_VARARGS,
				PyDoc_STR("assign(dict) -> None\n Assign attributes from nested dictionary, except for Outputs\n\n``nested_dict = { 'Location and Resource': { var: val, ...}, ...}``")},
		{"export",            (PyCFunction)Pvwattsv5_export,  METH_VARARGS,
				PyDoc_STR("export() -> dict\n Export attributes into nested dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
Pvwattsv5_getattro(Pvwattsv5Object *self, PyObject *name)
{
	return PySAM_get_attr((PyObject*) self, (PyObject*) self->x_attr, name);
}

static int
Pvwattsv5_setattr(Pvwattsv5Object *self, const char *name, PyObject *v)
{
	return PySAM_set_attr((PyObject*)self, (PyObject*)self->x_attr, name, v);
}

static PyTypeObject Pvwattsv5_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Pvwattsv5",            /*tp_name*/
		sizeof(Pvwattsv5Object),/*tp_basicsize*/
		0,                          /*tp_itemsize*/
		/* methods */
		(destructor)Pvwattsv5_dealloc,    /*tp_dealloc*/
		0,                          /*tp_print*/
		(getattrfunc)0,             /*tp_getattr*/
		(setattrfunc)Pvwattsv5_setattr,   /*tp_setattr*/
		0,                          /*tp_reserved*/
		0,                          /*tp_repr*/
		0,                          /*tp_as_number*/
		0,                          /*tp_as_sequence*/
		0,                          /*tp_as_mapping*/
		0,                          /*tp_hash*/
		0,                          /*tp_call*/
		0,                          /*tp_str*/
		(getattrofunc)Pvwattsv5_getattro, /*tp_getattro*/
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
		Pvwattsv5_methods,      /*tp_methods*/
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


/* Function of no arguments returning new Pvwattsv5 object */

static PyObject *
Pvwattsv5_new(PyObject *self, PyObject *args)
{
	Pvwattsv5Object *rv;
	rv = newPvwattsv5Object(0);
	if (rv == NULL)
		return NULL;
	return (PyObject *)rv;
}

static PyObject *
Pvwattsv5_wrap(PyObject *self, PyObject *args)
{
	Pvwattsv5Object *rv;
	long long int ptr = 0;  // 64 bit arch
	if (!PyArg_ParseTuple(args, "L:wrap", &ptr)){
		PyErr_BadArgument();
		return NULL;
	}
	rv = newPvwattsv5Object((void*)ptr);
	if (rv == NULL)
		return NULL;
	return (PyObject *)rv;
}

static PyObject *
Pvwattsv5_default(PyObject *self, PyObject *args)
{
	Pvwattsv5Object *rv;
	char* def = 0;
	if (!PyArg_ParseTuple(args, "s:default", &def)){
		PyErr_BadArgument();
		return NULL;
	}
	rv = newPvwattsv5Object(0);
	if (rv == NULL)
		return NULL;

	PySAM_load_defaults((PyObject*)rv, rv->x_attr, rv->data_ptr, "Pvwattsv5", def);

	return (PyObject *)rv;
}

/* ---------- */


/* List of functions defined in the module */

static PyMethodDef Pvwattsv5Module_methods[] = {
		{"new",             Pvwattsv5_new,         METH_VARARGS,
				PyDoc_STR("new() -> Pvwattsv5")},
		{"default",             Pvwattsv5_default,         METH_VARARGS,
				PyDoc_STR("default(config) -> Pvwattsv5\n\nUse financial model-specific default attributes\n"
				"config options:\n\n- \"PVWattsAllEquityPartnershipFlip\"\n- \"PVWattsCommercial\"\n- \"PVWattsCommercialPPA\"\n- \"PVWattsHostDeveloper\"\n- \"PVWattsIndependentPowerProducer\"\n- \"PVWattsLCOECalculator\"\n- \"PVWattsLeveragedPartnershipFlip\"\n- \"PVWattsNone\"\n- \"PVWattsResidential\"\n- \"PVWattsSaleLeaseback\"\n- \"PVWattsSingleOwner\"\n- \"PVWattsThirdParty\"")},
		{"wrap",             Pvwattsv5_wrap,         METH_VARARGS,
				PyDoc_STR("wrap(ssc_data_t) -> Pvwattsv5\n\nUse existing PySSC data\n\n.. warning::\n\n	Do not call PySSC.data_free on the ssc_data_t provided to ``wrap``")},
		{NULL,              NULL}           /* sentinel */
};

PyDoc_STRVAR(module_doc,
			 "PVWatts photovoltaic system model with simple inputs");


static int
Pvwattsv5Module_exec(PyObject *m)
{
	/* Finalize the type object including setting type of the new type
	 * object; doing it here is required for portability, too. */

	if (PySAM_load_lib(m) < 0) goto fail;
	if (PySAM_init_error(m) < 0) goto fail;

	Pvwattsv5_Type.tp_dict = PyDict_New();
	if (!Pvwattsv5_Type.tp_dict) { goto fail; }

	/// Add the AdjustmentFactors type object to Pvwattsv5_Type
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
	PyDict_SetItemString(Pvwattsv5_Type.tp_dict,
						 "AdjustmentFactors",
						 (PyObject*)AdjustmentFactors_Type);
	Py_DECREF(&AdjustmentFactors_Type);
	Py_XDECREF(AdjustmentFactors_Type);

	/// Add the LocationAndResource type object to Pvwattsv5_Type
	if (PyType_Ready(&LocationAndResource_Type) < 0) { goto fail; }
	PyDict_SetItemString(Pvwattsv5_Type.tp_dict,
				"LocationAndResource",
				(PyObject*)&LocationAndResource_Type);
	Py_DECREF(&LocationAndResource_Type);

	/// Add the SystemDesign type object to Pvwattsv5_Type
	if (PyType_Ready(&SystemDesign_Type) < 0) { goto fail; }
	PyDict_SetItemString(Pvwattsv5_Type.tp_dict,
				"SystemDesign",
				(PyObject*)&SystemDesign_Type);
	Py_DECREF(&SystemDesign_Type);

	/// Add the Outputs type object to Pvwattsv5_Type
	if (PyType_Ready(&Outputs_Type) < 0) { goto fail; }
	PyDict_SetItemString(Pvwattsv5_Type.tp_dict,
				"Outputs",
				(PyObject*)&Outputs_Type);
	Py_DECREF(&Outputs_Type);

	/// Add the Pvwattsv5 type object to the module
	if (PyType_Ready(&Pvwattsv5_Type) < 0) { goto fail; }
	PyModule_AddObject(m,
				"Pvwattsv5",
				(PyObject*)&Pvwattsv5_Type);

	return 0;
	fail:
	Py_XDECREF(m);
	return -1;
}

static struct PyModuleDef_Slot Pvwattsv5Module_slots[] = {
		{Py_mod_exec, Pvwattsv5Module_exec},
		{0, NULL},
};

static struct PyModuleDef Pvwattsv5Module = {
		PyModuleDef_HEAD_INIT,
		"Pvwattsv5",
		module_doc,
		0,
		Pvwattsv5Module_methods,
		Pvwattsv5Module_slots,
		NULL,
		NULL,
		NULL
};

/* Export function for the module */

PyMODINIT_FUNC
PyInit_Pvwattsv5(void)
{
	return PyModuleDef_Init(&Pvwattsv5Module);
}