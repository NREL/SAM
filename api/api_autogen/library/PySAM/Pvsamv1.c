#include <Python.h>

#include <SAM_Pvsamv1.h>
#include <SAM_api.h>

#include "PySAM_utils.h"



	/*
	 * SolarResource Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Pvsamv1   data_ptr;
} SolarResourceObject;

static PyTypeObject SolarResource_Type;

static PyObject *
SolarResource_new(SAM_Pvsamv1 data_ptr)
{
	PyObject* new_obj = SolarResource_Type.tp_alloc(&SolarResource_Type,0);

	SolarResourceObject* SolarResource_obj = (SolarResourceObject*)new_obj;

	SolarResource_obj->data_ptr = data_ptr;

	return new_obj;
}

/* SolarResource methods */

static PyObject *
SolarResource_assign(SolarResourceObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Pvsamv1", "SolarResource")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
SolarResource_export(SolarResourceObject *self, PyObject *args)
{
	PyTypeObject* tp = &SolarResource_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef SolarResource_methods[] = {
		{"assign",            (PyCFunction)SolarResource_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``SolarResource_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)SolarResource_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
SolarResource_get_albedo(SolarResourceObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_SolarResource_albedo_aget, self->data_ptr);
}

static int
SolarResource_set_albedo(SolarResourceObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Pvsamv1_SolarResource_albedo_aset, self->data_ptr);
}

static PyObject *
SolarResource_get_irrad_mode(SolarResourceObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SolarResource_irrad_mode_nget, self->data_ptr);
}

static int
SolarResource_set_irrad_mode(SolarResourceObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SolarResource_irrad_mode_nset, self->data_ptr);
}

static PyObject *
SolarResource_get_sky_model(SolarResourceObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SolarResource_sky_model_nget, self->data_ptr);
}

static int
SolarResource_set_sky_model(SolarResourceObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SolarResource_sky_model_nset, self->data_ptr);
}

static PyObject *
SolarResource_get_solar_resource_data(SolarResourceObject *self, void *closure)
{
	return PySAM_table_getter(SAM_Pvsamv1_SolarResource_solar_resource_data_tget, self->data_ptr);
}

static int
SolarResource_set_solar_resource_data(SolarResourceObject *self, PyObject *value, void *closure)
{
	return PySAM_table_setter(value, SAM_Pvsamv1_SolarResource_solar_resource_data_tset, self->data_ptr);
}

static PyObject *
SolarResource_get_solar_resource_file(SolarResourceObject *self, void *closure)
{
	return PySAM_string_getter(SAM_Pvsamv1_SolarResource_solar_resource_file_sget, self->data_ptr);
}

static int
SolarResource_set_solar_resource_file(SolarResourceObject *self, PyObject *value, void *closure)
{
	return PySAM_string_setter(value, SAM_Pvsamv1_SolarResource_solar_resource_file_sset, self->data_ptr);
}

static PyObject *
SolarResource_get_use_wf_albedo(SolarResourceObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SolarResource_use_wf_albedo_nget, self->data_ptr);
}

static int
SolarResource_set_use_wf_albedo(SolarResourceObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SolarResource_use_wf_albedo_nset, self->data_ptr);
}

static PyGetSetDef SolarResource_getset[] = {
{"albedo", (getter)SolarResource_get_albedo,(setter)SolarResource_set_albedo,
	PyDoc_STR("*sequence*: User specified ground albedo [0..1]\n\n*Constraints*: LENGTH=12\n\n*Required*: True"),
 	NULL},
{"irrad_mode", (getter)SolarResource_get_irrad_mode,(setter)SolarResource_set_irrad_mode,
	PyDoc_STR("*float*: Irradiance input translation mode\n\n*Options*: 0=beam&diffuse,1=total&beam,2=total&diffuse,3=poa_reference,4=poa_pyranometer\n\n*Constraints*: INTEGER,MIN=0,MAX=4\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"sky_model", (getter)SolarResource_get_sky_model,(setter)SolarResource_set_sky_model,
	PyDoc_STR("*float*: Diffuse sky model\n\n*Options*: 0=isotropic,1=hkdr,2=perez\n\n*Constraints*: INTEGER,MIN=0,MAX=2\n\n*Required*: set to 2 if not provided."),
 	NULL},
{"solar_resource_data", (getter)SolarResource_get_solar_resource_data,(setter)SolarResource_set_solar_resource_data,
	PyDoc_STR("*dict*: Weather data\n\n*Info*: lat,lon,tz,elev,year,month,hour,minute,gh,dn,df,poa,tdry,twet,tdew,rhum,pres,Snow,alb,aod,wspd,wdir\n\n*Required*: False"),
 	NULL},
{"solar_resource_file", (getter)SolarResource_get_solar_resource_file,(setter)SolarResource_set_solar_resource_file,
	PyDoc_STR("*str*: Weather file in TMY2, TMY3, EPW, or SAM CSV\n\n*Required*: False"),
 	NULL},
{"use_wf_albedo", (getter)SolarResource_get_use_wf_albedo,(setter)SolarResource_set_use_wf_albedo,
	PyDoc_STR("*float*: Use albedo in weather file if provided [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 1 if not provided."),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject SolarResource_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Pvsamv1.SolarResource",             /*tp_name*/
		sizeof(SolarResourceObject),          /*tp_basicsize*/
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
		SolarResource_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		SolarResource_getset,          /*tp_getset*/
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
	 * Losses Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Pvsamv1   data_ptr;
} LossesObject;

static PyTypeObject Losses_Type;

static PyObject *
Losses_new(SAM_Pvsamv1 data_ptr)
{
	PyObject* new_obj = Losses_Type.tp_alloc(&Losses_Type,0);

	LossesObject* Losses_obj = (LossesObject*)new_obj;

	Losses_obj->data_ptr = data_ptr;

	return new_obj;
}

/* Losses methods */

static PyObject *
Losses_assign(LossesObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Pvsamv1", "Losses")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
Losses_export(LossesObject *self, PyObject *args)
{
	PyTypeObject* tp = &Losses_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef Losses_methods[] = {
		{"assign",            (PyCFunction)Losses_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``Losses_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)Losses_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
Losses_get_acwiring_loss(LossesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Losses_acwiring_loss_nget, self->data_ptr);
}

static int
Losses_set_acwiring_loss(LossesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Losses_acwiring_loss_nset, self->data_ptr);
}

static PyObject *
Losses_get_dcoptimizer_loss(LossesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Losses_dcoptimizer_loss_nget, self->data_ptr);
}

static int
Losses_set_dcoptimizer_loss(LossesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Losses_dcoptimizer_loss_nset, self->data_ptr);
}

static PyObject *
Losses_get_en_snow_model(LossesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Losses_en_snow_model_nget, self->data_ptr);
}

static int
Losses_set_en_snow_model(LossesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Losses_en_snow_model_nset, self->data_ptr);
}

static PyObject *
Losses_get_subarray1_dcwiring_loss(LossesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Losses_subarray1_dcwiring_loss_nget, self->data_ptr);
}

static int
Losses_set_subarray1_dcwiring_loss(LossesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Losses_subarray1_dcwiring_loss_nset, self->data_ptr);
}

static PyObject *
Losses_get_subarray1_diodeconn_loss(LossesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Losses_subarray1_diodeconn_loss_nget, self->data_ptr);
}

static int
Losses_set_subarray1_diodeconn_loss(LossesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Losses_subarray1_diodeconn_loss_nset, self->data_ptr);
}

static PyObject *
Losses_get_subarray1_mismatch_loss(LossesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Losses_subarray1_mismatch_loss_nget, self->data_ptr);
}

static int
Losses_set_subarray1_mismatch_loss(LossesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Losses_subarray1_mismatch_loss_nset, self->data_ptr);
}

static PyObject *
Losses_get_subarray1_nameplate_loss(LossesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Losses_subarray1_nameplate_loss_nget, self->data_ptr);
}

static int
Losses_set_subarray1_nameplate_loss(LossesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Losses_subarray1_nameplate_loss_nset, self->data_ptr);
}

static PyObject *
Losses_get_subarray1_rear_irradiance_loss(LossesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Losses_subarray1_rear_irradiance_loss_nget, self->data_ptr);
}

static int
Losses_set_subarray1_rear_irradiance_loss(LossesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Losses_subarray1_rear_irradiance_loss_nset, self->data_ptr);
}

static PyObject *
Losses_get_subarray1_soiling(LossesObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Losses_subarray1_soiling_aget, self->data_ptr);
}

static int
Losses_set_subarray1_soiling(LossesObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Pvsamv1_Losses_subarray1_soiling_aset, self->data_ptr);
}

static PyObject *
Losses_get_subarray1_tracking_loss(LossesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Losses_subarray1_tracking_loss_nget, self->data_ptr);
}

static int
Losses_set_subarray1_tracking_loss(LossesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Losses_subarray1_tracking_loss_nset, self->data_ptr);
}

static PyObject *
Losses_get_subarray2_dcwiring_loss(LossesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Losses_subarray2_dcwiring_loss_nget, self->data_ptr);
}

static int
Losses_set_subarray2_dcwiring_loss(LossesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Losses_subarray2_dcwiring_loss_nset, self->data_ptr);
}

static PyObject *
Losses_get_subarray2_diodeconn_loss(LossesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Losses_subarray2_diodeconn_loss_nget, self->data_ptr);
}

static int
Losses_set_subarray2_diodeconn_loss(LossesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Losses_subarray2_diodeconn_loss_nset, self->data_ptr);
}

static PyObject *
Losses_get_subarray2_mismatch_loss(LossesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Losses_subarray2_mismatch_loss_nget, self->data_ptr);
}

static int
Losses_set_subarray2_mismatch_loss(LossesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Losses_subarray2_mismatch_loss_nset, self->data_ptr);
}

static PyObject *
Losses_get_subarray2_nameplate_loss(LossesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Losses_subarray2_nameplate_loss_nget, self->data_ptr);
}

static int
Losses_set_subarray2_nameplate_loss(LossesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Losses_subarray2_nameplate_loss_nset, self->data_ptr);
}

static PyObject *
Losses_get_subarray2_rear_irradiance_loss(LossesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Losses_subarray2_rear_irradiance_loss_nget, self->data_ptr);
}

static int
Losses_set_subarray2_rear_irradiance_loss(LossesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Losses_subarray2_rear_irradiance_loss_nset, self->data_ptr);
}

static PyObject *
Losses_get_subarray2_soiling(LossesObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Losses_subarray2_soiling_aget, self->data_ptr);
}

static int
Losses_set_subarray2_soiling(LossesObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Pvsamv1_Losses_subarray2_soiling_aset, self->data_ptr);
}

static PyObject *
Losses_get_subarray2_tracking_loss(LossesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Losses_subarray2_tracking_loss_nget, self->data_ptr);
}

static int
Losses_set_subarray2_tracking_loss(LossesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Losses_subarray2_tracking_loss_nset, self->data_ptr);
}

static PyObject *
Losses_get_subarray3_dcwiring_loss(LossesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Losses_subarray3_dcwiring_loss_nget, self->data_ptr);
}

static int
Losses_set_subarray3_dcwiring_loss(LossesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Losses_subarray3_dcwiring_loss_nset, self->data_ptr);
}

static PyObject *
Losses_get_subarray3_diodeconn_loss(LossesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Losses_subarray3_diodeconn_loss_nget, self->data_ptr);
}

static int
Losses_set_subarray3_diodeconn_loss(LossesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Losses_subarray3_diodeconn_loss_nset, self->data_ptr);
}

static PyObject *
Losses_get_subarray3_mismatch_loss(LossesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Losses_subarray3_mismatch_loss_nget, self->data_ptr);
}

static int
Losses_set_subarray3_mismatch_loss(LossesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Losses_subarray3_mismatch_loss_nset, self->data_ptr);
}

static PyObject *
Losses_get_subarray3_nameplate_loss(LossesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Losses_subarray3_nameplate_loss_nget, self->data_ptr);
}

static int
Losses_set_subarray3_nameplate_loss(LossesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Losses_subarray3_nameplate_loss_nset, self->data_ptr);
}

static PyObject *
Losses_get_subarray3_rear_irradiance_loss(LossesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Losses_subarray3_rear_irradiance_loss_nget, self->data_ptr);
}

static int
Losses_set_subarray3_rear_irradiance_loss(LossesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Losses_subarray3_rear_irradiance_loss_nset, self->data_ptr);
}

static PyObject *
Losses_get_subarray3_soiling(LossesObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Losses_subarray3_soiling_aget, self->data_ptr);
}

static int
Losses_set_subarray3_soiling(LossesObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Pvsamv1_Losses_subarray3_soiling_aset, self->data_ptr);
}

static PyObject *
Losses_get_subarray3_tracking_loss(LossesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Losses_subarray3_tracking_loss_nget, self->data_ptr);
}

static int
Losses_set_subarray3_tracking_loss(LossesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Losses_subarray3_tracking_loss_nset, self->data_ptr);
}

static PyObject *
Losses_get_subarray4_dcwiring_loss(LossesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Losses_subarray4_dcwiring_loss_nget, self->data_ptr);
}

static int
Losses_set_subarray4_dcwiring_loss(LossesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Losses_subarray4_dcwiring_loss_nset, self->data_ptr);
}

static PyObject *
Losses_get_subarray4_diodeconn_loss(LossesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Losses_subarray4_diodeconn_loss_nget, self->data_ptr);
}

static int
Losses_set_subarray4_diodeconn_loss(LossesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Losses_subarray4_diodeconn_loss_nset, self->data_ptr);
}

static PyObject *
Losses_get_subarray4_mismatch_loss(LossesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Losses_subarray4_mismatch_loss_nget, self->data_ptr);
}

static int
Losses_set_subarray4_mismatch_loss(LossesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Losses_subarray4_mismatch_loss_nset, self->data_ptr);
}

static PyObject *
Losses_get_subarray4_nameplate_loss(LossesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Losses_subarray4_nameplate_loss_nget, self->data_ptr);
}

static int
Losses_set_subarray4_nameplate_loss(LossesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Losses_subarray4_nameplate_loss_nset, self->data_ptr);
}

static PyObject *
Losses_get_subarray4_rear_irradiance_loss(LossesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Losses_subarray4_rear_irradiance_loss_nget, self->data_ptr);
}

static int
Losses_set_subarray4_rear_irradiance_loss(LossesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Losses_subarray4_rear_irradiance_loss_nset, self->data_ptr);
}

static PyObject *
Losses_get_subarray4_soiling(LossesObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Losses_subarray4_soiling_aget, self->data_ptr);
}

static int
Losses_set_subarray4_soiling(LossesObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Pvsamv1_Losses_subarray4_soiling_aset, self->data_ptr);
}

static PyObject *
Losses_get_subarray4_tracking_loss(LossesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Losses_subarray4_tracking_loss_nget, self->data_ptr);
}

static int
Losses_set_subarray4_tracking_loss(LossesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Losses_subarray4_tracking_loss_nset, self->data_ptr);
}

static PyObject *
Losses_get_transformer_load_loss(LossesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Losses_transformer_load_loss_nget, self->data_ptr);
}

static int
Losses_set_transformer_load_loss(LossesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Losses_transformer_load_loss_nset, self->data_ptr);
}

static PyObject *
Losses_get_transformer_no_load_loss(LossesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Losses_transformer_no_load_loss_nget, self->data_ptr);
}

static int
Losses_set_transformer_no_load_loss(LossesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Losses_transformer_no_load_loss_nset, self->data_ptr);
}

static PyObject *
Losses_get_transmission_loss(LossesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Losses_transmission_loss_nget, self->data_ptr);
}

static int
Losses_set_transmission_loss(LossesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Losses_transmission_loss_nset, self->data_ptr);
}

static PyGetSetDef Losses_getset[] = {
{"acwiring_loss", (getter)Losses_get_acwiring_loss,(setter)Losses_set_acwiring_loss,
	PyDoc_STR("*float*: AC wiring loss [%]\n\n*Constraints*: MIN=0,MAX=100\n\n*Required*: True"),
 	NULL},
{"dcoptimizer_loss", (getter)Losses_get_dcoptimizer_loss,(setter)Losses_set_dcoptimizer_loss,
	PyDoc_STR("*float*: DC power optimizer loss [%]\n\n*Constraints*: MIN=0,MAX=100\n\n*Required*: True"),
 	NULL},
{"en_snow_model", (getter)Losses_get_en_snow_model,(setter)Losses_set_en_snow_model,
	PyDoc_STR("*float*: Toggle snow loss estimation [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"subarray1_dcwiring_loss", (getter)Losses_get_subarray1_dcwiring_loss,(setter)Losses_set_subarray1_dcwiring_loss,
	PyDoc_STR("*float*: Sub-array 1 DC wiring loss [%]\n\n*Constraints*: MIN=0,MAX=100\n\n*Required*: True"),
 	NULL},
{"subarray1_diodeconn_loss", (getter)Losses_get_subarray1_diodeconn_loss,(setter)Losses_set_subarray1_diodeconn_loss,
	PyDoc_STR("*float*: Sub-array 1 DC diodes and connections loss [%]\n\n*Constraints*: MIN=0,MAX=100\n\n*Required*: True"),
 	NULL},
{"subarray1_mismatch_loss", (getter)Losses_get_subarray1_mismatch_loss,(setter)Losses_set_subarray1_mismatch_loss,
	PyDoc_STR("*float*: Sub-array 1 DC mismatch loss [%]\n\n*Constraints*: MIN=0,MAX=100\n\n*Required*: True"),
 	NULL},
{"subarray1_nameplate_loss", (getter)Losses_get_subarray1_nameplate_loss,(setter)Losses_set_subarray1_nameplate_loss,
	PyDoc_STR("*float*: Sub-array 1 DC nameplate loss [%]\n\n*Constraints*: MIN=-5,MAX=100\n\n*Required*: True"),
 	NULL},
{"subarray1_rear_irradiance_loss", (getter)Losses_get_subarray1_rear_irradiance_loss,(setter)Losses_set_subarray1_rear_irradiance_loss,
	PyDoc_STR("*float*: Sub-array 1 rear irradiance loss [%]\n\n*Constraints*: MIN=0,MAX=100\n\n*Required*: True"),
 	NULL},
{"subarray1_soiling", (getter)Losses_get_subarray1_soiling,(setter)Losses_set_subarray1_soiling,
	PyDoc_STR("*sequence*: Sub-array 1 Monthly soiling loss [%]\n\n*Constraints*: LENGTH=12\n\n*Required*: True"),
 	NULL},
{"subarray1_tracking_loss", (getter)Losses_get_subarray1_tracking_loss,(setter)Losses_set_subarray1_tracking_loss,
	PyDoc_STR("*float*: Sub-array 1 DC tracking error loss [%]\n\n*Constraints*: MIN=0,MAX=100\n\n*Required*: True"),
 	NULL},
{"subarray2_dcwiring_loss", (getter)Losses_get_subarray2_dcwiring_loss,(setter)Losses_set_subarray2_dcwiring_loss,
	PyDoc_STR("*float*: Sub-array 2 DC wiring loss [%]\n\n*Constraints*: MIN=0,MAX=100\n\n*Required*: False"),
 	NULL},
{"subarray2_diodeconn_loss", (getter)Losses_get_subarray2_diodeconn_loss,(setter)Losses_set_subarray2_diodeconn_loss,
	PyDoc_STR("*float*: Sub-array 2 DC diodes and connections loss [%]\n\n*Constraints*: MIN=0,MAX=100\n\n*Required*: False"),
 	NULL},
{"subarray2_mismatch_loss", (getter)Losses_get_subarray2_mismatch_loss,(setter)Losses_set_subarray2_mismatch_loss,
	PyDoc_STR("*float*: Sub-array 2 DC mismatch loss [%]\n\n*Constraints*: MIN=0,MAX=100\n\n*Required*: False"),
 	NULL},
{"subarray2_nameplate_loss", (getter)Losses_get_subarray2_nameplate_loss,(setter)Losses_set_subarray2_nameplate_loss,
	PyDoc_STR("*float*: Sub-array 2 DC nameplate loss [%]\n\n*Constraints*: MIN=-5,MAX=100\n\n*Required*: False"),
 	NULL},
{"subarray2_rear_irradiance_loss", (getter)Losses_get_subarray2_rear_irradiance_loss,(setter)Losses_set_subarray2_rear_irradiance_loss,
	PyDoc_STR("*float*: Sub-array 2 rear irradiance loss [%]\n\n*Constraints*: MIN=0,MAX=100\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"subarray2_soiling", (getter)Losses_get_subarray2_soiling,(setter)Losses_set_subarray2_soiling,
	PyDoc_STR("*sequence*: Sub-array 2 Monthly soiling loss [%]\n\n*Constraints*: LENGTH=12\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"subarray2_tracking_loss", (getter)Losses_get_subarray2_tracking_loss,(setter)Losses_set_subarray2_tracking_loss,
	PyDoc_STR("*float*: Sub-array 2 DC tracking error loss [%]\n\n*Constraints*: MIN=0,MAX=100\n\n*Required*: False"),
 	NULL},
{"subarray3_dcwiring_loss", (getter)Losses_get_subarray3_dcwiring_loss,(setter)Losses_set_subarray3_dcwiring_loss,
	PyDoc_STR("*float*: Sub-array 3 DC wiring loss [%]\n\n*Constraints*: MIN=0,MAX=100\n\n*Required*: False"),
 	NULL},
{"subarray3_diodeconn_loss", (getter)Losses_get_subarray3_diodeconn_loss,(setter)Losses_set_subarray3_diodeconn_loss,
	PyDoc_STR("*float*: Sub-array 3 DC diodes and connections loss [%]\n\n*Constraints*: MIN=0,MAX=100\n\n*Required*: False"),
 	NULL},
{"subarray3_mismatch_loss", (getter)Losses_get_subarray3_mismatch_loss,(setter)Losses_set_subarray3_mismatch_loss,
	PyDoc_STR("*float*: Sub-array 3 DC mismatch loss [%]\n\n*Constraints*: MIN=0,MAX=100\n\n*Required*: False"),
 	NULL},
{"subarray3_nameplate_loss", (getter)Losses_get_subarray3_nameplate_loss,(setter)Losses_set_subarray3_nameplate_loss,
	PyDoc_STR("*float*: Sub-array 3 DC nameplate loss [%]\n\n*Constraints*: MIN=-5,MAX=100\n\n*Required*: False"),
 	NULL},
{"subarray3_rear_irradiance_loss", (getter)Losses_get_subarray3_rear_irradiance_loss,(setter)Losses_set_subarray3_rear_irradiance_loss,
	PyDoc_STR("*float*: Sub-array 3 rear irradiance loss [%]\n\n*Constraints*: MIN=0,MAX=100\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"subarray3_soiling", (getter)Losses_get_subarray3_soiling,(setter)Losses_set_subarray3_soiling,
	PyDoc_STR("*sequence*: Sub-array 3 Monthly soiling loss [%]\n\n*Constraints*: LENGTH=12\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"subarray3_tracking_loss", (getter)Losses_get_subarray3_tracking_loss,(setter)Losses_set_subarray3_tracking_loss,
	PyDoc_STR("*float*: Sub-array 3 DC tracking error loss [%]\n\n*Constraints*: MIN=0,MAX=100\n\n*Required*: False"),
 	NULL},
{"subarray4_dcwiring_loss", (getter)Losses_get_subarray4_dcwiring_loss,(setter)Losses_set_subarray4_dcwiring_loss,
	PyDoc_STR("*float*: Sub-array 4 DC wiring loss [%]\n\n*Constraints*: MIN=0,MAX=100\n\n*Required*: False"),
 	NULL},
{"subarray4_diodeconn_loss", (getter)Losses_get_subarray4_diodeconn_loss,(setter)Losses_set_subarray4_diodeconn_loss,
	PyDoc_STR("*float*: Sub-array 4 DC diodes and connections loss [%]\n\n*Info*: ?\n\n*Constraints*: MIN=0,MAX=100\n\n*Required*: False"),
 	NULL},
{"subarray4_mismatch_loss", (getter)Losses_get_subarray4_mismatch_loss,(setter)Losses_set_subarray4_mismatch_loss,
	PyDoc_STR("*float*: Sub-array 4 DC mismatch loss [%]\n\n*Constraints*: MIN=0,MAX=100\n\n*Required*: False"),
 	NULL},
{"subarray4_nameplate_loss", (getter)Losses_get_subarray4_nameplate_loss,(setter)Losses_set_subarray4_nameplate_loss,
	PyDoc_STR("*float*: Sub-array 4 DC nameplate loss [%]\n\n*Constraints*: MIN=-5,MAX=100\n\n*Required*: False"),
 	NULL},
{"subarray4_rear_irradiance_loss", (getter)Losses_get_subarray4_rear_irradiance_loss,(setter)Losses_set_subarray4_rear_irradiance_loss,
	PyDoc_STR("*float*: Sub-array 4 rear irradiance loss [%]\n\n*Constraints*: MIN=0,MAX=100\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"subarray4_soiling", (getter)Losses_get_subarray4_soiling,(setter)Losses_set_subarray4_soiling,
	PyDoc_STR("*sequence*: Sub-array 4 Monthly soiling loss [%]\n\n*Constraints*: LENGTH=12\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"subarray4_tracking_loss", (getter)Losses_get_subarray4_tracking_loss,(setter)Losses_set_subarray4_tracking_loss,
	PyDoc_STR("*float*: Sub-array 4 DC tracking error loss [%]\n\n*Constraints*: MIN=0,MAX=100\n\n*Required*: False"),
 	NULL},
{"transformer_load_loss", (getter)Losses_get_transformer_load_loss,(setter)Losses_set_transformer_load_loss,
	PyDoc_STR("*float*: Power transformer load loss [%]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"transformer_no_load_loss", (getter)Losses_get_transformer_no_load_loss,(setter)Losses_set_transformer_no_load_loss,
	PyDoc_STR("*float*: Power transformer no load loss [%]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"transmission_loss", (getter)Losses_get_transmission_loss,(setter)Losses_set_transmission_loss,
	PyDoc_STR("*float*: Transmission loss [%]\n\n*Constraints*: MIN=0,MAX=100\n\n*Required*: True"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject Losses_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Pvsamv1.Losses",             /*tp_name*/
		sizeof(LossesObject),          /*tp_basicsize*/
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
		Losses_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		Losses_getset,          /*tp_getset*/
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
	SAM_Pvsamv1   data_ptr;
} LifetimeObject;

static PyTypeObject Lifetime_Type;

static PyObject *
Lifetime_new(SAM_Pvsamv1 data_ptr)
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

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Pvsamv1", "Lifetime")){
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
Lifetime_get_ac_lifetime_losses(LifetimeObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Lifetime_ac_lifetime_losses_aget, self->data_ptr);
}

static int
Lifetime_set_ac_lifetime_losses(LifetimeObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Pvsamv1_Lifetime_ac_lifetime_losses_aset, self->data_ptr);
}

static PyObject *
Lifetime_get_analysis_period(LifetimeObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Lifetime_analysis_period_nget, self->data_ptr);
}

static int
Lifetime_set_analysis_period(LifetimeObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Lifetime_analysis_period_nset, self->data_ptr);
}

static PyObject *
Lifetime_get_dc_degradation(LifetimeObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Lifetime_dc_degradation_aget, self->data_ptr);
}

static int
Lifetime_set_dc_degradation(LifetimeObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Pvsamv1_Lifetime_dc_degradation_aset, self->data_ptr);
}

static PyObject *
Lifetime_get_dc_lifetime_losses(LifetimeObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Lifetime_dc_lifetime_losses_aget, self->data_ptr);
}

static int
Lifetime_set_dc_lifetime_losses(LifetimeObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Pvsamv1_Lifetime_dc_lifetime_losses_aset, self->data_ptr);
}

static PyObject *
Lifetime_get_en_ac_lifetime_losses(LifetimeObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Lifetime_en_ac_lifetime_losses_nget, self->data_ptr);
}

static int
Lifetime_set_en_ac_lifetime_losses(LifetimeObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Lifetime_en_ac_lifetime_losses_nset, self->data_ptr);
}

static PyObject *
Lifetime_get_en_dc_lifetime_losses(LifetimeObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Lifetime_en_dc_lifetime_losses_nget, self->data_ptr);
}

static int
Lifetime_set_en_dc_lifetime_losses(LifetimeObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Lifetime_en_dc_lifetime_losses_nset, self->data_ptr);
}

static PyObject *
Lifetime_get_system_use_lifetime_output(LifetimeObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Lifetime_system_use_lifetime_output_nget, self->data_ptr);
}

static int
Lifetime_set_system_use_lifetime_output(LifetimeObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Lifetime_system_use_lifetime_output_nset, self->data_ptr);
}

static PyGetSetDef Lifetime_getset[] = {
{"ac_lifetime_losses", (getter)Lifetime_get_ac_lifetime_losses,(setter)Lifetime_set_ac_lifetime_losses,
	PyDoc_STR("*sequence*: Lifetime daily AC losses [%]\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"analysis_period", (getter)Lifetime_get_analysis_period,(setter)Lifetime_set_analysis_period,
	PyDoc_STR("*float*: Lifetime analysis period [years]\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"dc_degradation", (getter)Lifetime_get_dc_degradation,(setter)Lifetime_set_dc_degradation,
	PyDoc_STR("*sequence*: Annual module degradation [%/year]\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"dc_lifetime_losses", (getter)Lifetime_get_dc_lifetime_losses,(setter)Lifetime_set_dc_lifetime_losses,
	PyDoc_STR("*sequence*: Lifetime daily DC losses [%]\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"en_ac_lifetime_losses", (getter)Lifetime_get_en_ac_lifetime_losses,(setter)Lifetime_set_en_ac_lifetime_losses,
	PyDoc_STR("*float*: Enable lifetime daily AC losses [0/1]\n\n*Constraints*: INTEGER,MIN=0,MAX=1\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"en_dc_lifetime_losses", (getter)Lifetime_get_en_dc_lifetime_losses,(setter)Lifetime_set_en_dc_lifetime_losses,
	PyDoc_STR("*float*: Enable lifetime daily DC losses [0/1]\n\n*Constraints*: INTEGER,MIN=0,MAX=1\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"system_use_lifetime_output", (getter)Lifetime_get_system_use_lifetime_output,(setter)Lifetime_set_system_use_lifetime_output,
	PyDoc_STR("*float*: PV lifetime simulation [0/1]\n\n*Constraints*: INTEGER,MIN=0,MAX=1\n\n*Required*: set to 0 if not provided."),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject Lifetime_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Pvsamv1.Lifetime",             /*tp_name*/
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
	 * SystemDesign Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Pvsamv1   data_ptr;
} SystemDesignObject;

static PyTypeObject SystemDesign_Type;

static PyObject *
SystemDesign_new(SAM_Pvsamv1 data_ptr)
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

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Pvsamv1", "SystemDesign")){
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
SystemDesign_get_enable_mismatch_vmax_calc(SystemDesignObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SystemDesign_enable_mismatch_vmax_calc_nget, self->data_ptr);
}

static int
SystemDesign_set_enable_mismatch_vmax_calc(SystemDesignObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SystemDesign_enable_mismatch_vmax_calc_nset, self->data_ptr);
}

static PyObject *
SystemDesign_get_inverter_count(SystemDesignObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SystemDesign_inverter_count_nget, self->data_ptr);
}

static int
SystemDesign_set_inverter_count(SystemDesignObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SystemDesign_inverter_count_nset, self->data_ptr);
}

static PyObject *
SystemDesign_get_subarray1_azimuth(SystemDesignObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SystemDesign_subarray1_azimuth_nget, self->data_ptr);
}

static int
SystemDesign_set_subarray1_azimuth(SystemDesignObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SystemDesign_subarray1_azimuth_nset, self->data_ptr);
}

static PyObject *
SystemDesign_get_subarray1_backtrack(SystemDesignObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SystemDesign_subarray1_backtrack_nget, self->data_ptr);
}

static int
SystemDesign_set_subarray1_backtrack(SystemDesignObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SystemDesign_subarray1_backtrack_nset, self->data_ptr);
}

static PyObject *
SystemDesign_get_subarray1_gcr(SystemDesignObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SystemDesign_subarray1_gcr_nget, self->data_ptr);
}

static int
SystemDesign_set_subarray1_gcr(SystemDesignObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SystemDesign_subarray1_gcr_nset, self->data_ptr);
}

static PyObject *
SystemDesign_get_subarray1_modules_per_string(SystemDesignObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SystemDesign_subarray1_modules_per_string_nget, self->data_ptr);
}

static int
SystemDesign_set_subarray1_modules_per_string(SystemDesignObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SystemDesign_subarray1_modules_per_string_nset, self->data_ptr);
}

static PyObject *
SystemDesign_get_subarray1_monthly_tilt(SystemDesignObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_SystemDesign_subarray1_monthly_tilt_aget, self->data_ptr);
}

static int
SystemDesign_set_subarray1_monthly_tilt(SystemDesignObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Pvsamv1_SystemDesign_subarray1_monthly_tilt_aset, self->data_ptr);
}

static PyObject *
SystemDesign_get_subarray1_mppt_input(SystemDesignObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SystemDesign_subarray1_mppt_input_nget, self->data_ptr);
}

static int
SystemDesign_set_subarray1_mppt_input(SystemDesignObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SystemDesign_subarray1_mppt_input_nset, self->data_ptr);
}

static PyObject *
SystemDesign_get_subarray1_nstrings(SystemDesignObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SystemDesign_subarray1_nstrings_nget, self->data_ptr);
}

static int
SystemDesign_set_subarray1_nstrings(SystemDesignObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SystemDesign_subarray1_nstrings_nset, self->data_ptr);
}

static PyObject *
SystemDesign_get_subarray1_rotlim(SystemDesignObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SystemDesign_subarray1_rotlim_nget, self->data_ptr);
}

static int
SystemDesign_set_subarray1_rotlim(SystemDesignObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SystemDesign_subarray1_rotlim_nset, self->data_ptr);
}

static PyObject *
SystemDesign_get_subarray1_tilt(SystemDesignObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SystemDesign_subarray1_tilt_nget, self->data_ptr);
}

static int
SystemDesign_set_subarray1_tilt(SystemDesignObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SystemDesign_subarray1_tilt_nset, self->data_ptr);
}

static PyObject *
SystemDesign_get_subarray1_tilt_eq_lat(SystemDesignObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SystemDesign_subarray1_tilt_eq_lat_nget, self->data_ptr);
}

static int
SystemDesign_set_subarray1_tilt_eq_lat(SystemDesignObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SystemDesign_subarray1_tilt_eq_lat_nset, self->data_ptr);
}

static PyObject *
SystemDesign_get_subarray1_track_mode(SystemDesignObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SystemDesign_subarray1_track_mode_nget, self->data_ptr);
}

static int
SystemDesign_set_subarray1_track_mode(SystemDesignObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SystemDesign_subarray1_track_mode_nset, self->data_ptr);
}

static PyObject *
SystemDesign_get_subarray2_azimuth(SystemDesignObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SystemDesign_subarray2_azimuth_nget, self->data_ptr);
}

static int
SystemDesign_set_subarray2_azimuth(SystemDesignObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SystemDesign_subarray2_azimuth_nset, self->data_ptr);
}

static PyObject *
SystemDesign_get_subarray2_backtrack(SystemDesignObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SystemDesign_subarray2_backtrack_nget, self->data_ptr);
}

static int
SystemDesign_set_subarray2_backtrack(SystemDesignObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SystemDesign_subarray2_backtrack_nset, self->data_ptr);
}

static PyObject *
SystemDesign_get_subarray2_enable(SystemDesignObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SystemDesign_subarray2_enable_nget, self->data_ptr);
}

static int
SystemDesign_set_subarray2_enable(SystemDesignObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SystemDesign_subarray2_enable_nset, self->data_ptr);
}

static PyObject *
SystemDesign_get_subarray2_gcr(SystemDesignObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SystemDesign_subarray2_gcr_nget, self->data_ptr);
}

static int
SystemDesign_set_subarray2_gcr(SystemDesignObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SystemDesign_subarray2_gcr_nset, self->data_ptr);
}

static PyObject *
SystemDesign_get_subarray2_modules_per_string(SystemDesignObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SystemDesign_subarray2_modules_per_string_nget, self->data_ptr);
}

static int
SystemDesign_set_subarray2_modules_per_string(SystemDesignObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SystemDesign_subarray2_modules_per_string_nset, self->data_ptr);
}

static PyObject *
SystemDesign_get_subarray2_monthly_tilt(SystemDesignObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_SystemDesign_subarray2_monthly_tilt_aget, self->data_ptr);
}

static int
SystemDesign_set_subarray2_monthly_tilt(SystemDesignObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Pvsamv1_SystemDesign_subarray2_monthly_tilt_aset, self->data_ptr);
}

static PyObject *
SystemDesign_get_subarray2_mppt_input(SystemDesignObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SystemDesign_subarray2_mppt_input_nget, self->data_ptr);
}

static int
SystemDesign_set_subarray2_mppt_input(SystemDesignObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SystemDesign_subarray2_mppt_input_nset, self->data_ptr);
}

static PyObject *
SystemDesign_get_subarray2_nstrings(SystemDesignObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SystemDesign_subarray2_nstrings_nget, self->data_ptr);
}

static int
SystemDesign_set_subarray2_nstrings(SystemDesignObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SystemDesign_subarray2_nstrings_nset, self->data_ptr);
}

static PyObject *
SystemDesign_get_subarray2_rotlim(SystemDesignObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SystemDesign_subarray2_rotlim_nget, self->data_ptr);
}

static int
SystemDesign_set_subarray2_rotlim(SystemDesignObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SystemDesign_subarray2_rotlim_nset, self->data_ptr);
}

static PyObject *
SystemDesign_get_subarray2_tilt(SystemDesignObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SystemDesign_subarray2_tilt_nget, self->data_ptr);
}

static int
SystemDesign_set_subarray2_tilt(SystemDesignObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SystemDesign_subarray2_tilt_nset, self->data_ptr);
}

static PyObject *
SystemDesign_get_subarray2_tilt_eq_lat(SystemDesignObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SystemDesign_subarray2_tilt_eq_lat_nget, self->data_ptr);
}

static int
SystemDesign_set_subarray2_tilt_eq_lat(SystemDesignObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SystemDesign_subarray2_tilt_eq_lat_nset, self->data_ptr);
}

static PyObject *
SystemDesign_get_subarray2_track_mode(SystemDesignObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SystemDesign_subarray2_track_mode_nget, self->data_ptr);
}

static int
SystemDesign_set_subarray2_track_mode(SystemDesignObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SystemDesign_subarray2_track_mode_nset, self->data_ptr);
}

static PyObject *
SystemDesign_get_subarray3_azimuth(SystemDesignObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SystemDesign_subarray3_azimuth_nget, self->data_ptr);
}

static int
SystemDesign_set_subarray3_azimuth(SystemDesignObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SystemDesign_subarray3_azimuth_nset, self->data_ptr);
}

static PyObject *
SystemDesign_get_subarray3_backtrack(SystemDesignObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SystemDesign_subarray3_backtrack_nget, self->data_ptr);
}

static int
SystemDesign_set_subarray3_backtrack(SystemDesignObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SystemDesign_subarray3_backtrack_nset, self->data_ptr);
}

static PyObject *
SystemDesign_get_subarray3_enable(SystemDesignObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SystemDesign_subarray3_enable_nget, self->data_ptr);
}

static int
SystemDesign_set_subarray3_enable(SystemDesignObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SystemDesign_subarray3_enable_nset, self->data_ptr);
}

static PyObject *
SystemDesign_get_subarray3_gcr(SystemDesignObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SystemDesign_subarray3_gcr_nget, self->data_ptr);
}

static int
SystemDesign_set_subarray3_gcr(SystemDesignObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SystemDesign_subarray3_gcr_nset, self->data_ptr);
}

static PyObject *
SystemDesign_get_subarray3_modules_per_string(SystemDesignObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SystemDesign_subarray3_modules_per_string_nget, self->data_ptr);
}

static int
SystemDesign_set_subarray3_modules_per_string(SystemDesignObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SystemDesign_subarray3_modules_per_string_nset, self->data_ptr);
}

static PyObject *
SystemDesign_get_subarray3_monthly_tilt(SystemDesignObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_SystemDesign_subarray3_monthly_tilt_aget, self->data_ptr);
}

static int
SystemDesign_set_subarray3_monthly_tilt(SystemDesignObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Pvsamv1_SystemDesign_subarray3_monthly_tilt_aset, self->data_ptr);
}

static PyObject *
SystemDesign_get_subarray3_mppt_input(SystemDesignObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SystemDesign_subarray3_mppt_input_nget, self->data_ptr);
}

static int
SystemDesign_set_subarray3_mppt_input(SystemDesignObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SystemDesign_subarray3_mppt_input_nset, self->data_ptr);
}

static PyObject *
SystemDesign_get_subarray3_nstrings(SystemDesignObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SystemDesign_subarray3_nstrings_nget, self->data_ptr);
}

static int
SystemDesign_set_subarray3_nstrings(SystemDesignObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SystemDesign_subarray3_nstrings_nset, self->data_ptr);
}

static PyObject *
SystemDesign_get_subarray3_rotlim(SystemDesignObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SystemDesign_subarray3_rotlim_nget, self->data_ptr);
}

static int
SystemDesign_set_subarray3_rotlim(SystemDesignObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SystemDesign_subarray3_rotlim_nset, self->data_ptr);
}

static PyObject *
SystemDesign_get_subarray3_tilt(SystemDesignObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SystemDesign_subarray3_tilt_nget, self->data_ptr);
}

static int
SystemDesign_set_subarray3_tilt(SystemDesignObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SystemDesign_subarray3_tilt_nset, self->data_ptr);
}

static PyObject *
SystemDesign_get_subarray3_tilt_eq_lat(SystemDesignObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SystemDesign_subarray3_tilt_eq_lat_nget, self->data_ptr);
}

static int
SystemDesign_set_subarray3_tilt_eq_lat(SystemDesignObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SystemDesign_subarray3_tilt_eq_lat_nset, self->data_ptr);
}

static PyObject *
SystemDesign_get_subarray3_track_mode(SystemDesignObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SystemDesign_subarray3_track_mode_nget, self->data_ptr);
}

static int
SystemDesign_set_subarray3_track_mode(SystemDesignObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SystemDesign_subarray3_track_mode_nset, self->data_ptr);
}

static PyObject *
SystemDesign_get_subarray4_azimuth(SystemDesignObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SystemDesign_subarray4_azimuth_nget, self->data_ptr);
}

static int
SystemDesign_set_subarray4_azimuth(SystemDesignObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SystemDesign_subarray4_azimuth_nset, self->data_ptr);
}

static PyObject *
SystemDesign_get_subarray4_backtrack(SystemDesignObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SystemDesign_subarray4_backtrack_nget, self->data_ptr);
}

static int
SystemDesign_set_subarray4_backtrack(SystemDesignObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SystemDesign_subarray4_backtrack_nset, self->data_ptr);
}

static PyObject *
SystemDesign_get_subarray4_enable(SystemDesignObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SystemDesign_subarray4_enable_nget, self->data_ptr);
}

static int
SystemDesign_set_subarray4_enable(SystemDesignObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SystemDesign_subarray4_enable_nset, self->data_ptr);
}

static PyObject *
SystemDesign_get_subarray4_gcr(SystemDesignObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SystemDesign_subarray4_gcr_nget, self->data_ptr);
}

static int
SystemDesign_set_subarray4_gcr(SystemDesignObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SystemDesign_subarray4_gcr_nset, self->data_ptr);
}

static PyObject *
SystemDesign_get_subarray4_modules_per_string(SystemDesignObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SystemDesign_subarray4_modules_per_string_nget, self->data_ptr);
}

static int
SystemDesign_set_subarray4_modules_per_string(SystemDesignObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SystemDesign_subarray4_modules_per_string_nset, self->data_ptr);
}

static PyObject *
SystemDesign_get_subarray4_monthly_tilt(SystemDesignObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_SystemDesign_subarray4_monthly_tilt_aget, self->data_ptr);
}

static int
SystemDesign_set_subarray4_monthly_tilt(SystemDesignObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Pvsamv1_SystemDesign_subarray4_monthly_tilt_aset, self->data_ptr);
}

static PyObject *
SystemDesign_get_subarray4_mppt_input(SystemDesignObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SystemDesign_subarray4_mppt_input_nget, self->data_ptr);
}

static int
SystemDesign_set_subarray4_mppt_input(SystemDesignObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SystemDesign_subarray4_mppt_input_nset, self->data_ptr);
}

static PyObject *
SystemDesign_get_subarray4_nstrings(SystemDesignObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SystemDesign_subarray4_nstrings_nget, self->data_ptr);
}

static int
SystemDesign_set_subarray4_nstrings(SystemDesignObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SystemDesign_subarray4_nstrings_nset, self->data_ptr);
}

static PyObject *
SystemDesign_get_subarray4_rotlim(SystemDesignObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SystemDesign_subarray4_rotlim_nget, self->data_ptr);
}

static int
SystemDesign_set_subarray4_rotlim(SystemDesignObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SystemDesign_subarray4_rotlim_nset, self->data_ptr);
}

static PyObject *
SystemDesign_get_subarray4_tilt(SystemDesignObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SystemDesign_subarray4_tilt_nget, self->data_ptr);
}

static int
SystemDesign_set_subarray4_tilt(SystemDesignObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SystemDesign_subarray4_tilt_nset, self->data_ptr);
}

static PyObject *
SystemDesign_get_subarray4_tilt_eq_lat(SystemDesignObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SystemDesign_subarray4_tilt_eq_lat_nget, self->data_ptr);
}

static int
SystemDesign_set_subarray4_tilt_eq_lat(SystemDesignObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SystemDesign_subarray4_tilt_eq_lat_nset, self->data_ptr);
}

static PyObject *
SystemDesign_get_subarray4_track_mode(SystemDesignObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SystemDesign_subarray4_track_mode_nget, self->data_ptr);
}

static int
SystemDesign_set_subarray4_track_mode(SystemDesignObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SystemDesign_subarray4_track_mode_nset, self->data_ptr);
}

static PyObject *
SystemDesign_get_system_capacity(SystemDesignObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SystemDesign_system_capacity_nget, self->data_ptr);
}

static int
SystemDesign_set_system_capacity(SystemDesignObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SystemDesign_system_capacity_nset, self->data_ptr);
}

static PyGetSetDef SystemDesign_getset[] = {
{"enable_mismatch_vmax_calc", (getter)SystemDesign_get_enable_mismatch_vmax_calc,(setter)SystemDesign_set_enable_mismatch_vmax_calc,
	PyDoc_STR("*float*: Enable mismatched subarray Vmax calculation\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"inverter_count", (getter)SystemDesign_get_inverter_count,(setter)SystemDesign_set_inverter_count,
	PyDoc_STR("*float*: Number of inverters\n\n*Constraints*: INTEGER,POSITIVE\n\n*Required*: True"),
 	NULL},
{"subarray1_azimuth", (getter)SystemDesign_get_subarray1_azimuth,(setter)SystemDesign_set_subarray1_azimuth,
	PyDoc_STR("*float*: Sub-array 1 Azimuth [deg]\n\n*Options*: 0=N,90=E,180=S,270=W\n\n*Constraints*: MIN=0,MAX=359.9"),
 	NULL},
{"subarray1_backtrack", (getter)SystemDesign_get_subarray1_backtrack,(setter)SystemDesign_set_subarray1_backtrack,
	PyDoc_STR("*float*: Sub-array 1 Backtracking enabled\n\n*Options*: 0=no backtracking,1=backtrack\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"subarray1_gcr", (getter)SystemDesign_get_subarray1_gcr,(setter)SystemDesign_set_subarray1_gcr,
	PyDoc_STR("*float*: Sub-array 1 Ground coverage ratio [0..1]\n\n*Constraints*: MIN=0,MAX=3\n\n*Required*: set to 0.3 if not provided."),
 	NULL},
{"subarray1_modules_per_string", (getter)SystemDesign_get_subarray1_modules_per_string,(setter)SystemDesign_set_subarray1_modules_per_string,
	PyDoc_STR("*float*: Sub-array 1 Modules per string\n\n*Constraints*: INTEGER,POSITIVE\n\n*Required*: True"),
 	NULL},
{"subarray1_monthly_tilt", (getter)SystemDesign_get_subarray1_monthly_tilt,(setter)SystemDesign_set_subarray1_monthly_tilt,
	PyDoc_STR("*sequence*: Sub-array 1 monthly tilt input [deg]\n\n*Constraints*: LENGTH=12\n\n*Required*: set to 4 if not provided."),
 	NULL},
{"subarray1_mppt_input", (getter)SystemDesign_get_subarray1_mppt_input,(setter)SystemDesign_set_subarray1_mppt_input,
	PyDoc_STR("*float*: Sub-array 1 Inverter MPPT input number\n\n*Constraints*: INTEGER,POSITIVE\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"subarray1_nstrings", (getter)SystemDesign_get_subarray1_nstrings,(setter)SystemDesign_set_subarray1_nstrings,
	PyDoc_STR("*float*: Sub-array 1 Number of parallel strings\n\n*Constraints*: INTEGER"),
 	NULL},
{"subarray1_rotlim", (getter)SystemDesign_get_subarray1_rotlim,(setter)SystemDesign_set_subarray1_rotlim,
	PyDoc_STR("*float*: Sub-array 1 Tracker rotation limit [deg]\n\n*Constraints*: MIN=0,MAX=85\n\n*Required*: set to 45 if not provided."),
 	NULL},
{"subarray1_tilt", (getter)SystemDesign_get_subarray1_tilt,(setter)SystemDesign_set_subarray1_tilt,
	PyDoc_STR("*float*: Sub-array 1 Tilt [deg]\n\n*Options*: 0=horizontal,90=vertical\n\n*Constraints*: MIN=0,MAX=90"),
 	NULL},
{"subarray1_tilt_eq_lat", (getter)SystemDesign_get_subarray1_tilt_eq_lat,(setter)SystemDesign_set_subarray1_tilt_eq_lat,
	PyDoc_STR("*float*: Sub-array 1 Tilt=latitude override [0/1]\n\n*Constraints*: BOOLEAN"),
 	NULL},
{"subarray1_track_mode", (getter)SystemDesign_get_subarray1_track_mode,(setter)SystemDesign_set_subarray1_track_mode,
	PyDoc_STR("*float*: Sub-array 1 Tracking mode\n\n*Options*: 0=fixed,1=1axis,2=2axis,3=azi,4=monthly\n\n*Constraints*: INTEGER,MIN=0,MAX=4\n\n*Required*: True"),
 	NULL},
{"subarray2_azimuth", (getter)SystemDesign_get_subarray2_azimuth,(setter)SystemDesign_set_subarray2_azimuth,
	PyDoc_STR("*float*: Sub-array 2 Azimuth [deg]\n\n*Options*: 0=N,90=E,180=S,270=W\n\n*Constraints*: MIN=0,MAX=359.9"),
 	NULL},
{"subarray2_backtrack", (getter)SystemDesign_get_subarray2_backtrack,(setter)SystemDesign_set_subarray2_backtrack,
	PyDoc_STR("*float*: Sub-array 2 Backtracking enabled\n\n*Options*: 0=no backtracking,1=backtrack\n\n*Constraints*: BOOLEAN"),
 	NULL},
{"subarray2_enable", (getter)SystemDesign_get_subarray2_enable,(setter)SystemDesign_set_subarray2_enable,
	PyDoc_STR("*float*: Sub-array 2 Enable [0/1]\n\n*Options*: 0=disabled,1=enabled\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"subarray2_gcr", (getter)SystemDesign_get_subarray2_gcr,(setter)SystemDesign_set_subarray2_gcr,
	PyDoc_STR("*float*: Sub-array 2 Ground coverage ratio [0..1]\n\n*Constraints*: MIN=0,MAX=3\n\n*Required*: set to 0.3 if not provided."),
 	NULL},
{"subarray2_modules_per_string", (getter)SystemDesign_get_subarray2_modules_per_string,(setter)SystemDesign_set_subarray2_modules_per_string,
	PyDoc_STR("*float*: Sub-array 2 Modules per string\n\n*Constraints*: INTEGER,MIN=1\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"subarray2_monthly_tilt", (getter)SystemDesign_get_subarray2_monthly_tilt,(setter)SystemDesign_set_subarray2_monthly_tilt,
	PyDoc_STR("*sequence*: Sub-array 2 Monthly tilt input [deg]\n\n*Constraints*: LENGTH=12"),
 	NULL},
{"subarray2_mppt_input", (getter)SystemDesign_get_subarray2_mppt_input,(setter)SystemDesign_set_subarray2_mppt_input,
	PyDoc_STR("*float*: Sub-array 2 Inverter MPPT input number\n\n*Constraints*: INTEGER,POSITIVE\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"subarray2_nstrings", (getter)SystemDesign_get_subarray2_nstrings,(setter)SystemDesign_set_subarray2_nstrings,
	PyDoc_STR("*float*: Sub-array 2 Number of parallel strings\n\n*Constraints*: INTEGER,MIN=1\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"subarray2_rotlim", (getter)SystemDesign_get_subarray2_rotlim,(setter)SystemDesign_set_subarray2_rotlim,
	PyDoc_STR("*float*: Sub-array 2 Tracker rotation limit [deg]\n\n*Constraints*: MIN=0,MAX=85\n\n*Required*: set to 45 if not provided."),
 	NULL},
{"subarray2_tilt", (getter)SystemDesign_get_subarray2_tilt,(setter)SystemDesign_set_subarray2_tilt,
	PyDoc_STR("*float*: Sub-array 2 Tilt [deg]\n\n*Options*: 0=horizontal,90=vertical\n\n*Constraints*: MIN=0,MAX=90"),
 	NULL},
{"subarray2_tilt_eq_lat", (getter)SystemDesign_get_subarray2_tilt_eq_lat,(setter)SystemDesign_set_subarray2_tilt_eq_lat,
	PyDoc_STR("*float*: Sub-array 2 Tilt=latitude override [0/1]\n\n*Constraints*: BOOLEAN"),
 	NULL},
{"subarray2_track_mode", (getter)SystemDesign_get_subarray2_track_mode,(setter)SystemDesign_set_subarray2_track_mode,
	PyDoc_STR("*float*: Sub-array 2 Tracking mode\n\n*Options*: 0=fixed,1=1axis,2=2axis,3=azi,4=monthly\n\n*Constraints*: INTEGER,MIN=0,MAX=4\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"subarray3_azimuth", (getter)SystemDesign_get_subarray3_azimuth,(setter)SystemDesign_set_subarray3_azimuth,
	PyDoc_STR("*float*: Sub-array 3 Azimuth [deg]\n\n*Options*: 0=N,90=E,180=S,270=W\n\n*Constraints*: MIN=0,MAX=359.9"),
 	NULL},
{"subarray3_backtrack", (getter)SystemDesign_get_subarray3_backtrack,(setter)SystemDesign_set_subarray3_backtrack,
	PyDoc_STR("*float*: Sub-array 3 Backtracking enabled\n\n*Options*: 0=no backtracking,1=backtrack\n\n*Constraints*: BOOLEAN"),
 	NULL},
{"subarray3_enable", (getter)SystemDesign_get_subarray3_enable,(setter)SystemDesign_set_subarray3_enable,
	PyDoc_STR("*float*: Sub-array 3 Enable [0/1]\n\n*Options*: 0=disabled,1=enabled\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"subarray3_gcr", (getter)SystemDesign_get_subarray3_gcr,(setter)SystemDesign_set_subarray3_gcr,
	PyDoc_STR("*float*: Sub-array 3 Ground coverage ratio [0..1]\n\n*Constraints*: MIN=0,MAX=3\n\n*Required*: set to 0.3 if not provided."),
 	NULL},
{"subarray3_modules_per_string", (getter)SystemDesign_get_subarray3_modules_per_string,(setter)SystemDesign_set_subarray3_modules_per_string,
	PyDoc_STR("*float*: Sub-array 3 Modules per string\n\n*Constraints*: INTEGER,MIN=1\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"subarray3_monthly_tilt", (getter)SystemDesign_get_subarray3_monthly_tilt,(setter)SystemDesign_set_subarray3_monthly_tilt,
	PyDoc_STR("*sequence*: Sub-array 3 Monthly tilt input [deg]\n\n*Constraints*: LENGTH=12"),
 	NULL},
{"subarray3_mppt_input", (getter)SystemDesign_get_subarray3_mppt_input,(setter)SystemDesign_set_subarray3_mppt_input,
	PyDoc_STR("*float*: Sub-array 3 Inverter MPPT input number\n\n*Constraints*: INTEGER,POSITIVE\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"subarray3_nstrings", (getter)SystemDesign_get_subarray3_nstrings,(setter)SystemDesign_set_subarray3_nstrings,
	PyDoc_STR("*float*: Sub-array 3 Number of parallel strings\n\n*Constraints*: INTEGER,MIN=1\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"subarray3_rotlim", (getter)SystemDesign_get_subarray3_rotlim,(setter)SystemDesign_set_subarray3_rotlim,
	PyDoc_STR("*float*: Sub-array 3 Tracker rotation limit [deg]\n\n*Constraints*: MIN=0,MAX=85\n\n*Required*: set to 45 if not provided."),
 	NULL},
{"subarray3_tilt", (getter)SystemDesign_get_subarray3_tilt,(setter)SystemDesign_set_subarray3_tilt,
	PyDoc_STR("*float*: Sub-array 3 Tilt [deg]\n\n*Options*: 0=horizontal,90=vertical\n\n*Constraints*: MIN=0,MAX=90"),
 	NULL},
{"subarray3_tilt_eq_lat", (getter)SystemDesign_get_subarray3_tilt_eq_lat,(setter)SystemDesign_set_subarray3_tilt_eq_lat,
	PyDoc_STR("*float*: Sub-array 3 Tilt=latitude override [0/1]\n\n*Constraints*: BOOLEAN"),
 	NULL},
{"subarray3_track_mode", (getter)SystemDesign_get_subarray3_track_mode,(setter)SystemDesign_set_subarray3_track_mode,
	PyDoc_STR("*float*: Sub-array 3 Tracking mode\n\n*Options*: 0=fixed,1=1axis,2=2axis,3=azi,4=monthly\n\n*Constraints*: INTEGER,MIN=0,MAX=4\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"subarray4_azimuth", (getter)SystemDesign_get_subarray4_azimuth,(setter)SystemDesign_set_subarray4_azimuth,
	PyDoc_STR("*float*: Sub-array 4 Azimuth [deg]\n\n*Options*: 0=N,90=E,180=S,270=W\n\n*Constraints*: MIN=0,MAX=359.9"),
 	NULL},
{"subarray4_backtrack", (getter)SystemDesign_get_subarray4_backtrack,(setter)SystemDesign_set_subarray4_backtrack,
	PyDoc_STR("*float*: Sub-array 4 Backtracking enabled\n\n*Options*: 0=no backtracking,1=backtrack\n\n*Constraints*: BOOLEAN"),
 	NULL},
{"subarray4_enable", (getter)SystemDesign_get_subarray4_enable,(setter)SystemDesign_set_subarray4_enable,
	PyDoc_STR("*float*: Sub-array 4 Enable [0/1]\n\n*Options*: 0=disabled,1=enabled\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"subarray4_gcr", (getter)SystemDesign_get_subarray4_gcr,(setter)SystemDesign_set_subarray4_gcr,
	PyDoc_STR("*float*: Sub-array 4 Ground coverage ratio [0..1]\n\n*Constraints*: MIN=0,MAX=3\n\n*Required*: set to 0.3 if not provided."),
 	NULL},
{"subarray4_modules_per_string", (getter)SystemDesign_get_subarray4_modules_per_string,(setter)SystemDesign_set_subarray4_modules_per_string,
	PyDoc_STR("*float*: Sub-array 4 Modules per string\n\n*Constraints*: INTEGER,MIN=1\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"subarray4_monthly_tilt", (getter)SystemDesign_get_subarray4_monthly_tilt,(setter)SystemDesign_set_subarray4_monthly_tilt,
	PyDoc_STR("*sequence*: Sub-array 4 Monthly tilt input [deg]\n\n*Constraints*: LENGTH=12"),
 	NULL},
{"subarray4_mppt_input", (getter)SystemDesign_get_subarray4_mppt_input,(setter)SystemDesign_set_subarray4_mppt_input,
	PyDoc_STR("*float*: Sub-array 4 Inverter MPPT input number\n\n*Constraints*: INTEGER,POSITIVE\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"subarray4_nstrings", (getter)SystemDesign_get_subarray4_nstrings,(setter)SystemDesign_set_subarray4_nstrings,
	PyDoc_STR("*float*: Sub-array 4 Number of parallel strings\n\n*Constraints*: INTEGER,MIN=1\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"subarray4_rotlim", (getter)SystemDesign_get_subarray4_rotlim,(setter)SystemDesign_set_subarray4_rotlim,
	PyDoc_STR("*float*: Sub-array 4 Tracker rotation limit [deg]\n\n*Constraints*: MIN=0,MAX=85\n\n*Required*: set to 45 if not provided."),
 	NULL},
{"subarray4_tilt", (getter)SystemDesign_get_subarray4_tilt,(setter)SystemDesign_set_subarray4_tilt,
	PyDoc_STR("*float*: Sub-array 4 Tilt [deg]\n\n*Options*: 0=horizontal,90=vertical\n\n*Constraints*: MIN=0,MAX=90"),
 	NULL},
{"subarray4_tilt_eq_lat", (getter)SystemDesign_get_subarray4_tilt_eq_lat,(setter)SystemDesign_set_subarray4_tilt_eq_lat,
	PyDoc_STR("*float*: Sub-array 4 Tilt=latitude override [0/1]\n\n*Constraints*: BOOLEAN"),
 	NULL},
{"subarray4_track_mode", (getter)SystemDesign_get_subarray4_track_mode,(setter)SystemDesign_set_subarray4_track_mode,
	PyDoc_STR("*float*: Sub-array 4 Tracking mode\n\n*Options*: 0=fixed,1=1axis,2=2axis,3=azi,4=monthly\n\n*Constraints*: INTEGER,MIN=0,MAX=4\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"system_capacity", (getter)SystemDesign_get_system_capacity,(setter)SystemDesign_set_system_capacity,
	PyDoc_STR("*float*: DC Nameplate capacity [kWdc]\n\n*Required*: True"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject SystemDesign_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Pvsamv1.SystemDesign",             /*tp_name*/
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
	 * Shading Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Pvsamv1   data_ptr;
} ShadingObject;

static PyTypeObject Shading_Type;

static PyObject *
Shading_new(SAM_Pvsamv1 data_ptr)
{
	PyObject* new_obj = Shading_Type.tp_alloc(&Shading_Type,0);

	ShadingObject* Shading_obj = (ShadingObject*)new_obj;

	Shading_obj->data_ptr = data_ptr;

	return new_obj;
}

/* Shading methods */

static PyObject *
Shading_assign(ShadingObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Pvsamv1", "Shading")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
Shading_export(ShadingObject *self, PyObject *args)
{
	PyTypeObject* tp = &Shading_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef Shading_methods[] = {
		{"assign",            (PyCFunction)Shading_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``Shading_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)Shading_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
Shading_get_subarray1_shade_mode(ShadingObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Shading_subarray1_shade_mode_nget, self->data_ptr);
}

static int
Shading_set_subarray1_shade_mode(ShadingObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Shading_subarray1_shade_mode_nset, self->data_ptr);
}

static PyObject *
Shading_get_subarray1_shading_azal(ShadingObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Pvsamv1_Shading_subarray1_shading_azal_mget, self->data_ptr);
}

static int
Shading_set_subarray1_shading_azal(ShadingObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_Pvsamv1_Shading_subarray1_shading_azal_mset, self->data_ptr);
}

static PyObject *
Shading_get_subarray1_shading_diff(ShadingObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Shading_subarray1_shading_diff_nget, self->data_ptr);
}

static int
Shading_set_subarray1_shading_diff(ShadingObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Shading_subarray1_shading_diff_nset, self->data_ptr);
}

static PyObject *
Shading_get_subarray1_shading_mxh(ShadingObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Pvsamv1_Shading_subarray1_shading_mxh_mget, self->data_ptr);
}

static int
Shading_set_subarray1_shading_mxh(ShadingObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_Pvsamv1_Shading_subarray1_shading_mxh_mset, self->data_ptr);
}

static PyObject *
Shading_get_subarray1_shading_string_option(ShadingObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Shading_subarray1_shading_string_option_nget, self->data_ptr);
}

static int
Shading_set_subarray1_shading_string_option(ShadingObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Shading_subarray1_shading_string_option_nset, self->data_ptr);
}

static PyObject *
Shading_get_subarray1_shading_timestep(ShadingObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Pvsamv1_Shading_subarray1_shading_timestep_mget, self->data_ptr);
}

static int
Shading_set_subarray1_shading_timestep(ShadingObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_Pvsamv1_Shading_subarray1_shading_timestep_mset, self->data_ptr);
}

static PyObject *
Shading_get_subarray2_shade_mode(ShadingObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Shading_subarray2_shade_mode_nget, self->data_ptr);
}

static int
Shading_set_subarray2_shade_mode(ShadingObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Shading_subarray2_shade_mode_nset, self->data_ptr);
}

static PyObject *
Shading_get_subarray2_shading_azal(ShadingObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Pvsamv1_Shading_subarray2_shading_azal_mget, self->data_ptr);
}

static int
Shading_set_subarray2_shading_azal(ShadingObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_Pvsamv1_Shading_subarray2_shading_azal_mset, self->data_ptr);
}

static PyObject *
Shading_get_subarray2_shading_diff(ShadingObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Shading_subarray2_shading_diff_nget, self->data_ptr);
}

static int
Shading_set_subarray2_shading_diff(ShadingObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Shading_subarray2_shading_diff_nset, self->data_ptr);
}

static PyObject *
Shading_get_subarray2_shading_mxh(ShadingObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Pvsamv1_Shading_subarray2_shading_mxh_mget, self->data_ptr);
}

static int
Shading_set_subarray2_shading_mxh(ShadingObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_Pvsamv1_Shading_subarray2_shading_mxh_mset, self->data_ptr);
}

static PyObject *
Shading_get_subarray2_shading_string_option(ShadingObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Shading_subarray2_shading_string_option_nget, self->data_ptr);
}

static int
Shading_set_subarray2_shading_string_option(ShadingObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Shading_subarray2_shading_string_option_nset, self->data_ptr);
}

static PyObject *
Shading_get_subarray2_shading_timestep(ShadingObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Pvsamv1_Shading_subarray2_shading_timestep_mget, self->data_ptr);
}

static int
Shading_set_subarray2_shading_timestep(ShadingObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_Pvsamv1_Shading_subarray2_shading_timestep_mset, self->data_ptr);
}

static PyObject *
Shading_get_subarray3_shade_mode(ShadingObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Shading_subarray3_shade_mode_nget, self->data_ptr);
}

static int
Shading_set_subarray3_shade_mode(ShadingObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Shading_subarray3_shade_mode_nset, self->data_ptr);
}

static PyObject *
Shading_get_subarray3_shading_azal(ShadingObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Pvsamv1_Shading_subarray3_shading_azal_mget, self->data_ptr);
}

static int
Shading_set_subarray3_shading_azal(ShadingObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_Pvsamv1_Shading_subarray3_shading_azal_mset, self->data_ptr);
}

static PyObject *
Shading_get_subarray3_shading_diff(ShadingObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Shading_subarray3_shading_diff_nget, self->data_ptr);
}

static int
Shading_set_subarray3_shading_diff(ShadingObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Shading_subarray3_shading_diff_nset, self->data_ptr);
}

static PyObject *
Shading_get_subarray3_shading_mxh(ShadingObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Pvsamv1_Shading_subarray3_shading_mxh_mget, self->data_ptr);
}

static int
Shading_set_subarray3_shading_mxh(ShadingObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_Pvsamv1_Shading_subarray3_shading_mxh_mset, self->data_ptr);
}

static PyObject *
Shading_get_subarray3_shading_string_option(ShadingObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Shading_subarray3_shading_string_option_nget, self->data_ptr);
}

static int
Shading_set_subarray3_shading_string_option(ShadingObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Shading_subarray3_shading_string_option_nset, self->data_ptr);
}

static PyObject *
Shading_get_subarray3_shading_timestep(ShadingObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Pvsamv1_Shading_subarray3_shading_timestep_mget, self->data_ptr);
}

static int
Shading_set_subarray3_shading_timestep(ShadingObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_Pvsamv1_Shading_subarray3_shading_timestep_mset, self->data_ptr);
}

static PyObject *
Shading_get_subarray4_shade_mode(ShadingObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Shading_subarray4_shade_mode_nget, self->data_ptr);
}

static int
Shading_set_subarray4_shade_mode(ShadingObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Shading_subarray4_shade_mode_nset, self->data_ptr);
}

static PyObject *
Shading_get_subarray4_shading_azal(ShadingObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Pvsamv1_Shading_subarray4_shading_azal_mget, self->data_ptr);
}

static int
Shading_set_subarray4_shading_azal(ShadingObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_Pvsamv1_Shading_subarray4_shading_azal_mset, self->data_ptr);
}

static PyObject *
Shading_get_subarray4_shading_diff(ShadingObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Shading_subarray4_shading_diff_nget, self->data_ptr);
}

static int
Shading_set_subarray4_shading_diff(ShadingObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Shading_subarray4_shading_diff_nset, self->data_ptr);
}

static PyObject *
Shading_get_subarray4_shading_mxh(ShadingObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Pvsamv1_Shading_subarray4_shading_mxh_mget, self->data_ptr);
}

static int
Shading_set_subarray4_shading_mxh(ShadingObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_Pvsamv1_Shading_subarray4_shading_mxh_mset, self->data_ptr);
}

static PyObject *
Shading_get_subarray4_shading_string_option(ShadingObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Shading_subarray4_shading_string_option_nget, self->data_ptr);
}

static int
Shading_set_subarray4_shading_string_option(ShadingObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Shading_subarray4_shading_string_option_nset, self->data_ptr);
}

static PyObject *
Shading_get_subarray4_shading_timestep(ShadingObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Pvsamv1_Shading_subarray4_shading_timestep_mget, self->data_ptr);
}

static int
Shading_set_subarray4_shading_timestep(ShadingObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_Pvsamv1_Shading_subarray4_shading_timestep_mset, self->data_ptr);
}

static PyGetSetDef Shading_getset[] = {
{"subarray1_shade_mode", (getter)Shading_get_subarray1_shade_mode,(setter)Shading_set_subarray1_shade_mode,
	PyDoc_STR("*float*: Sub-array 1 shading mode (fixed tilt or 1x tracking) [0/1/2]\n\n*Options*: 0=none,1=standard(non-linear),2=thin film(linear)\n\n*Constraints*: INTEGER,MIN=0,MAX=2\n\n*Required*: True"),
 	NULL},
{"subarray1_shading_azal", (getter)Shading_get_subarray1_shading_azal,(setter)Shading_set_subarray1_shading_azal,
	PyDoc_STR("*sequence[sequence]*: Sub-array 1 Azimuth x altitude beam shading losses [%]\n\n*Required*: False"),
 	NULL},
{"subarray1_shading_diff", (getter)Shading_get_subarray1_shading_diff,(setter)Shading_set_subarray1_shading_diff,
	PyDoc_STR("*float*: Sub-array 1 Diffuse shading loss [%]\n\n*Required*: False"),
 	NULL},
{"subarray1_shading_mxh", (getter)Shading_get_subarray1_shading_mxh,(setter)Shading_set_subarray1_shading_mxh,
	PyDoc_STR("*sequence[sequence]*: Sub-array 1 Month x Hour beam shading losses [%]\n\n*Required*: False"),
 	NULL},
{"subarray1_shading_string_option", (getter)Shading_get_subarray1_shading_string_option,(setter)Shading_set_subarray1_shading_string_option,
	PyDoc_STR("*float*: Sub-array 1 shading string option\n\n*Options*: 0=shadingdb,1=shadingdb_notc,2=average,3=maximum,4=minimum\n\n*Constraints*: INTEGER,MIN=-1,MAX=4\n\n*Required*: set to -1 if not provided."),
 	NULL},
{"subarray1_shading_timestep", (getter)Shading_get_subarray1_shading_timestep,(setter)Shading_set_subarray1_shading_timestep,
	PyDoc_STR("*sequence[sequence]*: Sub-array 1 timestep beam shading losses [%]\n\n*Required*: False"),
 	NULL},
{"subarray2_shade_mode", (getter)Shading_get_subarray2_shade_mode,(setter)Shading_set_subarray2_shade_mode,
	PyDoc_STR("*float*: Sub-array 2 Shading mode (fixed tilt or 1x tracking) [0/1/2]\n\n*Options*: 0=none,1=standard(non-linear),2=thin film(linear)\n\n*Constraints*: INTEGER,MIN=0,MAX=2\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"subarray2_shading_azal", (getter)Shading_get_subarray2_shading_azal,(setter)Shading_set_subarray2_shading_azal,
	PyDoc_STR("*sequence[sequence]*: Sub-array 2 Azimuth x altitude beam shading losses [%]\n\n*Required*: False"),
 	NULL},
{"subarray2_shading_diff", (getter)Shading_get_subarray2_shading_diff,(setter)Shading_set_subarray2_shading_diff,
	PyDoc_STR("*float*: Sub-array 2 Diffuse shading loss [%]\n\n*Required*: False"),
 	NULL},
{"subarray2_shading_mxh", (getter)Shading_get_subarray2_shading_mxh,(setter)Shading_set_subarray2_shading_mxh,
	PyDoc_STR("*sequence[sequence]*: Sub-array 2 Month x Hour beam shading losses [%]\n\n*Required*: False"),
 	NULL},
{"subarray2_shading_string_option", (getter)Shading_get_subarray2_shading_string_option,(setter)Shading_set_subarray2_shading_string_option,
	PyDoc_STR("*float*: Sub-array 2 Shading string option\n\n*Options*: 0=shadingdb,1=shadingdb_notc,2=average,3=maximum,4=minimum\n\n*Constraints*: INTEGER,MIN=-1,MAX=4\n\n*Required*: set to -1 if not provided."),
 	NULL},
{"subarray2_shading_timestep", (getter)Shading_get_subarray2_shading_timestep,(setter)Shading_set_subarray2_shading_timestep,
	PyDoc_STR("*sequence[sequence]*: Sub-array 2 Timestep beam shading losses [%]\n\n*Required*: False"),
 	NULL},
{"subarray3_shade_mode", (getter)Shading_get_subarray3_shade_mode,(setter)Shading_set_subarray3_shade_mode,
	PyDoc_STR("*float*: Sub-array 3 Shading mode (fixed tilt or 1x tracking) [0/1/2]\n\n*Options*: 0=none,1=standard(non-linear),2=thin film(linear)\n\n*Constraints*: INTEGER,MIN=0,MAX=2\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"subarray3_shading_azal", (getter)Shading_get_subarray3_shading_azal,(setter)Shading_set_subarray3_shading_azal,
	PyDoc_STR("*sequence[sequence]*: Sub-array 3 Azimuth x altitude beam shading losses [%]\n\n*Required*: False"),
 	NULL},
{"subarray3_shading_diff", (getter)Shading_get_subarray3_shading_diff,(setter)Shading_set_subarray3_shading_diff,
	PyDoc_STR("*float*: Sub-array 3 Diffuse shading loss [%]\n\n*Required*: False"),
 	NULL},
{"subarray3_shading_mxh", (getter)Shading_get_subarray3_shading_mxh,(setter)Shading_set_subarray3_shading_mxh,
	PyDoc_STR("*sequence[sequence]*: Sub-array 3 Month x Hour beam shading losses [%]\n\n*Required*: False"),
 	NULL},
{"subarray3_shading_string_option", (getter)Shading_get_subarray3_shading_string_option,(setter)Shading_set_subarray3_shading_string_option,
	PyDoc_STR("*float*: Sub-array 3 Shading string option\n\n*Options*: 0=shadingdb,1=shadingdb_notc,2=average,3=maximum,4=minimum\n\n*Constraints*: INTEGER,MIN=-1,MAX=4\n\n*Required*: set to -1 if not provided."),
 	NULL},
{"subarray3_shading_timestep", (getter)Shading_get_subarray3_shading_timestep,(setter)Shading_set_subarray3_shading_timestep,
	PyDoc_STR("*sequence[sequence]*: Sub-array 3 Timestep beam shading losses [%]\n\n*Required*: False"),
 	NULL},
{"subarray4_shade_mode", (getter)Shading_get_subarray4_shade_mode,(setter)Shading_set_subarray4_shade_mode,
	PyDoc_STR("*float*: Sub-array 4 shading mode (fixed tilt or 1x tracking) [0/1/2]\n\n*Options*: 0=none,1=standard(non-linear),2=thin film(linear)\n\n*Constraints*: INTEGER,MIN=0,MAX=2\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"subarray4_shading_azal", (getter)Shading_get_subarray4_shading_azal,(setter)Shading_set_subarray4_shading_azal,
	PyDoc_STR("*sequence[sequence]*: Sub-array 4 Azimuth x altitude beam shading losses [%]\n\n*Required*: False"),
 	NULL},
{"subarray4_shading_diff", (getter)Shading_get_subarray4_shading_diff,(setter)Shading_set_subarray4_shading_diff,
	PyDoc_STR("*float*: Sub-array 4 Diffuse shading loss [%]\n\n*Required*: False"),
 	NULL},
{"subarray4_shading_mxh", (getter)Shading_get_subarray4_shading_mxh,(setter)Shading_set_subarray4_shading_mxh,
	PyDoc_STR("*sequence[sequence]*: Sub-array 4 Month x Hour beam shading losses [%]\n\n*Required*: False"),
 	NULL},
{"subarray4_shading_string_option", (getter)Shading_get_subarray4_shading_string_option,(setter)Shading_set_subarray4_shading_string_option,
	PyDoc_STR("*float*: Sub-array 4 Shading string option\n\n*Options*: 0=shadingdb,1=shadingdb_notc,2=average,3=maximum,4=minimum\n\n*Constraints*: INTEGER,MIN=-1,MAX=4\n\n*Required*: set to -1 if not provided."),
 	NULL},
{"subarray4_shading_timestep", (getter)Shading_get_subarray4_shading_timestep,(setter)Shading_set_subarray4_shading_timestep,
	PyDoc_STR("*sequence[sequence]*: Sub-array 4 Timestep beam shading losses [%]\n\n*Required*: False"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject Shading_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Pvsamv1.Shading",             /*tp_name*/
		sizeof(ShadingObject),          /*tp_basicsize*/
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
		Shading_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		Shading_getset,          /*tp_getset*/
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
	 * Layout Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Pvsamv1   data_ptr;
} LayoutObject;

static PyTypeObject Layout_Type;

static PyObject *
Layout_new(SAM_Pvsamv1 data_ptr)
{
	PyObject* new_obj = Layout_Type.tp_alloc(&Layout_Type,0);

	LayoutObject* Layout_obj = (LayoutObject*)new_obj;

	Layout_obj->data_ptr = data_ptr;

	return new_obj;
}

/* Layout methods */

static PyObject *
Layout_assign(LayoutObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Pvsamv1", "Layout")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
Layout_export(LayoutObject *self, PyObject *args)
{
	PyTypeObject* tp = &Layout_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef Layout_methods[] = {
		{"assign",            (PyCFunction)Layout_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``Layout_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)Layout_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
Layout_get_module_aspect_ratio(LayoutObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Layout_module_aspect_ratio_nget, self->data_ptr);
}

static int
Layout_set_module_aspect_ratio(LayoutObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Layout_module_aspect_ratio_nset, self->data_ptr);
}

static PyObject *
Layout_get_subarray1_mod_orient(LayoutObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Layout_subarray1_mod_orient_nget, self->data_ptr);
}

static int
Layout_set_subarray1_mod_orient(LayoutObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Layout_subarray1_mod_orient_nset, self->data_ptr);
}

static PyObject *
Layout_get_subarray1_nmodx(LayoutObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Layout_subarray1_nmodx_nget, self->data_ptr);
}

static int
Layout_set_subarray1_nmodx(LayoutObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Layout_subarray1_nmodx_nset, self->data_ptr);
}

static PyObject *
Layout_get_subarray1_nmody(LayoutObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Layout_subarray1_nmody_nget, self->data_ptr);
}

static int
Layout_set_subarray1_nmody(LayoutObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Layout_subarray1_nmody_nset, self->data_ptr);
}

static PyObject *
Layout_get_subarray2_mod_orient(LayoutObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Layout_subarray2_mod_orient_nget, self->data_ptr);
}

static int
Layout_set_subarray2_mod_orient(LayoutObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Layout_subarray2_mod_orient_nset, self->data_ptr);
}

static PyObject *
Layout_get_subarray2_nmodx(LayoutObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Layout_subarray2_nmodx_nget, self->data_ptr);
}

static int
Layout_set_subarray2_nmodx(LayoutObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Layout_subarray2_nmodx_nset, self->data_ptr);
}

static PyObject *
Layout_get_subarray2_nmody(LayoutObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Layout_subarray2_nmody_nget, self->data_ptr);
}

static int
Layout_set_subarray2_nmody(LayoutObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Layout_subarray2_nmody_nset, self->data_ptr);
}

static PyObject *
Layout_get_subarray3_mod_orient(LayoutObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Layout_subarray3_mod_orient_nget, self->data_ptr);
}

static int
Layout_set_subarray3_mod_orient(LayoutObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Layout_subarray3_mod_orient_nset, self->data_ptr);
}

static PyObject *
Layout_get_subarray3_nmodx(LayoutObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Layout_subarray3_nmodx_nget, self->data_ptr);
}

static int
Layout_set_subarray3_nmodx(LayoutObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Layout_subarray3_nmodx_nset, self->data_ptr);
}

static PyObject *
Layout_get_subarray3_nmody(LayoutObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Layout_subarray3_nmody_nget, self->data_ptr);
}

static int
Layout_set_subarray3_nmody(LayoutObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Layout_subarray3_nmody_nset, self->data_ptr);
}

static PyObject *
Layout_get_subarray4_mod_orient(LayoutObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Layout_subarray4_mod_orient_nget, self->data_ptr);
}

static int
Layout_set_subarray4_mod_orient(LayoutObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Layout_subarray4_mod_orient_nset, self->data_ptr);
}

static PyObject *
Layout_get_subarray4_nmodx(LayoutObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Layout_subarray4_nmodx_nget, self->data_ptr);
}

static int
Layout_set_subarray4_nmodx(LayoutObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Layout_subarray4_nmodx_nset, self->data_ptr);
}

static PyObject *
Layout_get_subarray4_nmody(LayoutObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Layout_subarray4_nmody_nget, self->data_ptr);
}

static int
Layout_set_subarray4_nmody(LayoutObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Layout_subarray4_nmody_nset, self->data_ptr);
}

static PyGetSetDef Layout_getset[] = {
{"module_aspect_ratio", (getter)Layout_get_module_aspect_ratio,(setter)Layout_set_module_aspect_ratio,
	PyDoc_STR("*float*: Module aspect ratio\n\n*Constraints*: POSITIVE\n\n*Required*: set to 1.7 if not provided."),
 	NULL},
{"subarray1_mod_orient", (getter)Layout_get_subarray1_mod_orient,(setter)Layout_set_subarray1_mod_orient,
	PyDoc_STR("*float*: Sub-array 1 Module orientation [0/1]\n\n*Options*: 0=portrait,1=landscape\n\n*Constraints*: INTEGER,MIN=0,MAX=1\n\n*Required*: True"),
 	NULL},
{"subarray1_nmodx", (getter)Layout_get_subarray1_nmodx,(setter)Layout_set_subarray1_nmodx,
	PyDoc_STR("*float*: Sub-array 1 Number of modules along bottom of row\n\n*Constraints*: INTEGER,POSITIVE\n\n*Required*: True"),
 	NULL},
{"subarray1_nmody", (getter)Layout_get_subarray1_nmody,(setter)Layout_set_subarray1_nmody,
	PyDoc_STR("*float*: Sub-array 1 Number of modules along side of row\n\n*Constraints*: INTEGER,POSITIVE\n\n*Required*: True"),
 	NULL},
{"subarray2_mod_orient", (getter)Layout_get_subarray2_mod_orient,(setter)Layout_set_subarray2_mod_orient,
	PyDoc_STR("*float*: Sub-array 2 Module orientation [0/1]\n\n*Options*: 0=portrait,1=landscape\n\n*Constraints*: INTEGER,MIN=0,MAX=1\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"subarray2_nmodx", (getter)Layout_get_subarray2_nmodx,(setter)Layout_set_subarray2_nmodx,
	PyDoc_STR("*float*: Sub-array 2 Number of modules along bottom of row\n\n*Constraints*: INTEGER,POSITIVE\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"subarray2_nmody", (getter)Layout_get_subarray2_nmody,(setter)Layout_set_subarray2_nmody,
	PyDoc_STR("*float*: Sub-array 2 Number of modules along side of row\n\n*Constraints*: INTEGER,POSITIVE\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"subarray3_mod_orient", (getter)Layout_get_subarray3_mod_orient,(setter)Layout_set_subarray3_mod_orient,
	PyDoc_STR("*float*: Sub-array 3 Module orientation [0/1]\n\n*Options*: 0=portrait,1=landscape\n\n*Constraints*: INTEGER,MIN=0,MAX=1\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"subarray3_nmodx", (getter)Layout_get_subarray3_nmodx,(setter)Layout_set_subarray3_nmodx,
	PyDoc_STR("*float*: Sub-array 3 Number of modules along bottom of row\n\n*Constraints*: INTEGER,POSITIVE\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"subarray3_nmody", (getter)Layout_get_subarray3_nmody,(setter)Layout_set_subarray3_nmody,
	PyDoc_STR("*float*: Sub-array 3 Number of modules along side of row\n\n*Constraints*: INTEGER,POSITIVE\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"subarray4_mod_orient", (getter)Layout_get_subarray4_mod_orient,(setter)Layout_set_subarray4_mod_orient,
	PyDoc_STR("*float*: Sub-array 4 Module orientation [0/1]\n\n*Options*: 0=portrait,1=landscape\n\n*Constraints*: INTEGER,MIN=0,MAX=1\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"subarray4_nmodx", (getter)Layout_get_subarray4_nmodx,(setter)Layout_set_subarray4_nmodx,
	PyDoc_STR("*float*: Sub-array 4 Number of modules along bottom of row\n\n*Constraints*: INTEGER,POSITIVE\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"subarray4_nmody", (getter)Layout_get_subarray4_nmody,(setter)Layout_set_subarray4_nmody,
	PyDoc_STR("*float*: Sub-array 4 Number of modules along side of row\n\n*Constraints*: INTEGER,POSITIVE\n\n*Required*: set to 1 if not provided."),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject Layout_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Pvsamv1.Layout",             /*tp_name*/
		sizeof(LayoutObject),          /*tp_basicsize*/
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
		Layout_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		Layout_getset,          /*tp_getset*/
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
	 * Module Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Pvsamv1   data_ptr;
} ModuleObject;

static PyTypeObject Module_Type;

static PyObject *
Module_new(SAM_Pvsamv1 data_ptr)
{
	PyObject* new_obj = Module_Type.tp_alloc(&Module_Type,0);

	ModuleObject* Module_obj = (ModuleObject*)new_obj;

	Module_obj->data_ptr = data_ptr;

	return new_obj;
}

/* Module methods */

static PyObject *
Module_assign(ModuleObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Pvsamv1", "Module")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
Module_export(ModuleObject *self, PyObject *args)
{
	PyTypeObject* tp = &Module_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef Module_methods[] = {
		{"assign",            (PyCFunction)Module_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``Module_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)Module_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
Module_get_module_model(ModuleObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Module_module_model_nget, self->data_ptr);
}

static int
Module_set_module_model(ModuleObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Module_module_model_nset, self->data_ptr);
}

static PyGetSetDef Module_getset[] = {
{"module_model", (getter)Module_get_module_model,(setter)Module_set_module_model,
	PyDoc_STR("*float*: Photovoltaic module model specifier\n\n*Options*: 0=spe,1=cec,2=6par_user,3=snl,4=sd11-iec61853,5=PVYield\n\n*Constraints*: INTEGER,MIN=0,MAX=5\n\n*Required*: True"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject Module_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Pvsamv1.Module",             /*tp_name*/
		sizeof(ModuleObject),          /*tp_basicsize*/
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
		Module_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		Module_getset,          /*tp_getset*/
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
	 * SimpleEfficiencyModuleModel Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Pvsamv1   data_ptr;
} SimpleEfficiencyModuleModelObject;

static PyTypeObject SimpleEfficiencyModuleModel_Type;

static PyObject *
SimpleEfficiencyModuleModel_new(SAM_Pvsamv1 data_ptr)
{
	PyObject* new_obj = SimpleEfficiencyModuleModel_Type.tp_alloc(&SimpleEfficiencyModuleModel_Type,0);

	SimpleEfficiencyModuleModelObject* SimpleEfficiencyModuleModel_obj = (SimpleEfficiencyModuleModelObject*)new_obj;

	SimpleEfficiencyModuleModel_obj->data_ptr = data_ptr;

	return new_obj;
}

/* SimpleEfficiencyModuleModel methods */

static PyObject *
SimpleEfficiencyModuleModel_assign(SimpleEfficiencyModuleModelObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Pvsamv1", "SimpleEfficiencyModuleModel")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
SimpleEfficiencyModuleModel_export(SimpleEfficiencyModuleModelObject *self, PyObject *args)
{
	PyTypeObject* tp = &SimpleEfficiencyModuleModel_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef SimpleEfficiencyModuleModel_methods[] = {
		{"assign",            (PyCFunction)SimpleEfficiencyModuleModel_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``SimpleEfficiencyModuleModel_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)SimpleEfficiencyModuleModel_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
SimpleEfficiencyModuleModel_get_spe_a(SimpleEfficiencyModuleModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_a_nget, self->data_ptr);
}

static int
SimpleEfficiencyModuleModel_set_spe_a(SimpleEfficiencyModuleModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_a_nset, self->data_ptr);
}

static PyObject *
SimpleEfficiencyModuleModel_get_spe_area(SimpleEfficiencyModuleModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_area_nget, self->data_ptr);
}

static int
SimpleEfficiencyModuleModel_set_spe_area(SimpleEfficiencyModuleModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_area_nset, self->data_ptr);
}

static PyObject *
SimpleEfficiencyModuleModel_get_spe_b(SimpleEfficiencyModuleModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_b_nget, self->data_ptr);
}

static int
SimpleEfficiencyModuleModel_set_spe_b(SimpleEfficiencyModuleModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_b_nset, self->data_ptr);
}

static PyObject *
SimpleEfficiencyModuleModel_get_spe_bifacial_ground_clearance_height(SimpleEfficiencyModuleModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_bifacial_ground_clearance_height_nget, self->data_ptr);
}

static int
SimpleEfficiencyModuleModel_set_spe_bifacial_ground_clearance_height(SimpleEfficiencyModuleModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_bifacial_ground_clearance_height_nset, self->data_ptr);
}

static PyObject *
SimpleEfficiencyModuleModel_get_spe_bifacial_transmission_factor(SimpleEfficiencyModuleModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_bifacial_transmission_factor_nget, self->data_ptr);
}

static int
SimpleEfficiencyModuleModel_set_spe_bifacial_transmission_factor(SimpleEfficiencyModuleModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_bifacial_transmission_factor_nset, self->data_ptr);
}

static PyObject *
SimpleEfficiencyModuleModel_get_spe_bifaciality(SimpleEfficiencyModuleModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_bifaciality_nget, self->data_ptr);
}

static int
SimpleEfficiencyModuleModel_set_spe_bifaciality(SimpleEfficiencyModuleModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_bifaciality_nset, self->data_ptr);
}

static PyObject *
SimpleEfficiencyModuleModel_get_spe_dT(SimpleEfficiencyModuleModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_dT_nget, self->data_ptr);
}

static int
SimpleEfficiencyModuleModel_set_spe_dT(SimpleEfficiencyModuleModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_dT_nset, self->data_ptr);
}

static PyObject *
SimpleEfficiencyModuleModel_get_spe_eff0(SimpleEfficiencyModuleModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_eff0_nget, self->data_ptr);
}

static int
SimpleEfficiencyModuleModel_set_spe_eff0(SimpleEfficiencyModuleModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_eff0_nset, self->data_ptr);
}

static PyObject *
SimpleEfficiencyModuleModel_get_spe_eff1(SimpleEfficiencyModuleModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_eff1_nget, self->data_ptr);
}

static int
SimpleEfficiencyModuleModel_set_spe_eff1(SimpleEfficiencyModuleModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_eff1_nset, self->data_ptr);
}

static PyObject *
SimpleEfficiencyModuleModel_get_spe_eff2(SimpleEfficiencyModuleModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_eff2_nget, self->data_ptr);
}

static int
SimpleEfficiencyModuleModel_set_spe_eff2(SimpleEfficiencyModuleModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_eff2_nset, self->data_ptr);
}

static PyObject *
SimpleEfficiencyModuleModel_get_spe_eff3(SimpleEfficiencyModuleModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_eff3_nget, self->data_ptr);
}

static int
SimpleEfficiencyModuleModel_set_spe_eff3(SimpleEfficiencyModuleModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_eff3_nset, self->data_ptr);
}

static PyObject *
SimpleEfficiencyModuleModel_get_spe_eff4(SimpleEfficiencyModuleModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_eff4_nget, self->data_ptr);
}

static int
SimpleEfficiencyModuleModel_set_spe_eff4(SimpleEfficiencyModuleModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_eff4_nset, self->data_ptr);
}

static PyObject *
SimpleEfficiencyModuleModel_get_spe_fd(SimpleEfficiencyModuleModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_fd_nget, self->data_ptr);
}

static int
SimpleEfficiencyModuleModel_set_spe_fd(SimpleEfficiencyModuleModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_fd_nset, self->data_ptr);
}

static PyObject *
SimpleEfficiencyModuleModel_get_spe_is_bifacial(SimpleEfficiencyModuleModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_is_bifacial_nget, self->data_ptr);
}

static int
SimpleEfficiencyModuleModel_set_spe_is_bifacial(SimpleEfficiencyModuleModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_is_bifacial_nset, self->data_ptr);
}

static PyObject *
SimpleEfficiencyModuleModel_get_spe_module_structure(SimpleEfficiencyModuleModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_module_structure_nget, self->data_ptr);
}

static int
SimpleEfficiencyModuleModel_set_spe_module_structure(SimpleEfficiencyModuleModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_module_structure_nset, self->data_ptr);
}

static PyObject *
SimpleEfficiencyModuleModel_get_spe_rad0(SimpleEfficiencyModuleModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_rad0_nget, self->data_ptr);
}

static int
SimpleEfficiencyModuleModel_set_spe_rad0(SimpleEfficiencyModuleModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_rad0_nset, self->data_ptr);
}

static PyObject *
SimpleEfficiencyModuleModel_get_spe_rad1(SimpleEfficiencyModuleModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_rad1_nget, self->data_ptr);
}

static int
SimpleEfficiencyModuleModel_set_spe_rad1(SimpleEfficiencyModuleModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_rad1_nset, self->data_ptr);
}

static PyObject *
SimpleEfficiencyModuleModel_get_spe_rad2(SimpleEfficiencyModuleModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_rad2_nget, self->data_ptr);
}

static int
SimpleEfficiencyModuleModel_set_spe_rad2(SimpleEfficiencyModuleModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_rad2_nset, self->data_ptr);
}

static PyObject *
SimpleEfficiencyModuleModel_get_spe_rad3(SimpleEfficiencyModuleModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_rad3_nget, self->data_ptr);
}

static int
SimpleEfficiencyModuleModel_set_spe_rad3(SimpleEfficiencyModuleModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_rad3_nset, self->data_ptr);
}

static PyObject *
SimpleEfficiencyModuleModel_get_spe_rad4(SimpleEfficiencyModuleModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_rad4_nget, self->data_ptr);
}

static int
SimpleEfficiencyModuleModel_set_spe_rad4(SimpleEfficiencyModuleModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_rad4_nset, self->data_ptr);
}

static PyObject *
SimpleEfficiencyModuleModel_get_spe_reference(SimpleEfficiencyModuleModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_reference_nget, self->data_ptr);
}

static int
SimpleEfficiencyModuleModel_set_spe_reference(SimpleEfficiencyModuleModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_reference_nset, self->data_ptr);
}

static PyObject *
SimpleEfficiencyModuleModel_get_spe_temp_coeff(SimpleEfficiencyModuleModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_temp_coeff_nget, self->data_ptr);
}

static int
SimpleEfficiencyModuleModel_set_spe_temp_coeff(SimpleEfficiencyModuleModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_temp_coeff_nset, self->data_ptr);
}

static PyObject *
SimpleEfficiencyModuleModel_get_spe_vmp(SimpleEfficiencyModuleModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_vmp_nget, self->data_ptr);
}

static int
SimpleEfficiencyModuleModel_set_spe_vmp(SimpleEfficiencyModuleModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_vmp_nset, self->data_ptr);
}

static PyObject *
SimpleEfficiencyModuleModel_get_spe_voc(SimpleEfficiencyModuleModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_voc_nget, self->data_ptr);
}

static int
SimpleEfficiencyModuleModel_set_spe_voc(SimpleEfficiencyModuleModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_voc_nset, self->data_ptr);
}

static PyGetSetDef SimpleEfficiencyModuleModel_getset[] = {
{"spe_a", (getter)SimpleEfficiencyModuleModel_get_spe_a,(setter)SimpleEfficiencyModuleModel_set_spe_a,
	PyDoc_STR("*float*: Cell temp parameter a\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"spe_area", (getter)SimpleEfficiencyModuleModel_get_spe_area,(setter)SimpleEfficiencyModuleModel_set_spe_area,
	PyDoc_STR("*float*: Module area [m2]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"spe_b", (getter)SimpleEfficiencyModuleModel_get_spe_b,(setter)SimpleEfficiencyModuleModel_set_spe_b,
	PyDoc_STR("*float*: Cell temp parameter b\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"spe_bifacial_ground_clearance_height", (getter)SimpleEfficiencyModuleModel_get_spe_bifacial_ground_clearance_height,(setter)SimpleEfficiencyModuleModel_set_spe_bifacial_ground_clearance_height,
	PyDoc_STR("*float*: Module ground clearance height [m]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"spe_bifacial_transmission_factor", (getter)SimpleEfficiencyModuleModel_get_spe_bifacial_transmission_factor,(setter)SimpleEfficiencyModuleModel_set_spe_bifacial_transmission_factor,
	PyDoc_STR("*float*: Bifacial transmission factor [0-1]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"spe_bifaciality", (getter)SimpleEfficiencyModuleModel_get_spe_bifaciality,(setter)SimpleEfficiencyModuleModel_set_spe_bifaciality,
	PyDoc_STR("*float*: Bifaciality factor [%]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"spe_dT", (getter)SimpleEfficiencyModuleModel_get_spe_dT,(setter)SimpleEfficiencyModuleModel_set_spe_dT,
	PyDoc_STR("*float*: Cell temp parameter dT\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"spe_eff0", (getter)SimpleEfficiencyModuleModel_get_spe_eff0,(setter)SimpleEfficiencyModuleModel_set_spe_eff0,
	PyDoc_STR("*float*: Efficiency at irradiance level 0 [%]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"spe_eff1", (getter)SimpleEfficiencyModuleModel_get_spe_eff1,(setter)SimpleEfficiencyModuleModel_set_spe_eff1,
	PyDoc_STR("*float*: Efficiency at irradiance level 1 [%]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"spe_eff2", (getter)SimpleEfficiencyModuleModel_get_spe_eff2,(setter)SimpleEfficiencyModuleModel_set_spe_eff2,
	PyDoc_STR("*float*: Efficiency at irradiance level 2 [%]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"spe_eff3", (getter)SimpleEfficiencyModuleModel_get_spe_eff3,(setter)SimpleEfficiencyModuleModel_set_spe_eff3,
	PyDoc_STR("*float*: Efficiency at irradiance level 3 [%]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"spe_eff4", (getter)SimpleEfficiencyModuleModel_get_spe_eff4,(setter)SimpleEfficiencyModuleModel_set_spe_eff4,
	PyDoc_STR("*float*: Efficiency at irradiance level 4 [%]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"spe_fd", (getter)SimpleEfficiencyModuleModel_get_spe_fd,(setter)SimpleEfficiencyModuleModel_set_spe_fd,
	PyDoc_STR("*float*: Diffuse fraction [0..1]\n\n*Constraints*: MIN=0,MAX=1\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"spe_is_bifacial", (getter)SimpleEfficiencyModuleModel_get_spe_is_bifacial,(setter)SimpleEfficiencyModuleModel_set_spe_is_bifacial,
	PyDoc_STR("*float*: Modules are bifacial [0/1]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"spe_module_structure", (getter)SimpleEfficiencyModuleModel_get_spe_module_structure,(setter)SimpleEfficiencyModuleModel_set_spe_module_structure,
	PyDoc_STR("*float*: Mounting and module structure\n\n*Options*: 0=glass/cell/polymer sheet - open rack,1=glass/cell/glass - open rack,2=polymer/thin film/steel - open rack,3=Insulated back, building-integrated PV,4=close roof mount,5=user-defined\n\n*Constraints*: INTEGER,MIN=0,MAX=5\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"spe_rad0", (getter)SimpleEfficiencyModuleModel_get_spe_rad0,(setter)SimpleEfficiencyModuleModel_set_spe_rad0,
	PyDoc_STR("*float*: Irradiance level 0 [W/m2]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"spe_rad1", (getter)SimpleEfficiencyModuleModel_get_spe_rad1,(setter)SimpleEfficiencyModuleModel_set_spe_rad1,
	PyDoc_STR("*float*: Irradiance level 1 [W/m2]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"spe_rad2", (getter)SimpleEfficiencyModuleModel_get_spe_rad2,(setter)SimpleEfficiencyModuleModel_set_spe_rad2,
	PyDoc_STR("*float*: Irradiance level 2 [W/m2]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"spe_rad3", (getter)SimpleEfficiencyModuleModel_get_spe_rad3,(setter)SimpleEfficiencyModuleModel_set_spe_rad3,
	PyDoc_STR("*float*: Irradiance level 3 [W/m2]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"spe_rad4", (getter)SimpleEfficiencyModuleModel_get_spe_rad4,(setter)SimpleEfficiencyModuleModel_set_spe_rad4,
	PyDoc_STR("*float*: Irradiance level 4 [W/m2]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"spe_reference", (getter)SimpleEfficiencyModuleModel_get_spe_reference,(setter)SimpleEfficiencyModuleModel_set_spe_reference,
	PyDoc_STR("*float*: Reference irradiance level\n\n*Constraints*: INTEGER,MIN=0,MAX=4\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"spe_temp_coeff", (getter)SimpleEfficiencyModuleModel_get_spe_temp_coeff,(setter)SimpleEfficiencyModuleModel_set_spe_temp_coeff,
	PyDoc_STR("*float*: Temperature coefficient [%/C]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"spe_vmp", (getter)SimpleEfficiencyModuleModel_get_spe_vmp,(setter)SimpleEfficiencyModuleModel_set_spe_vmp,
	PyDoc_STR("*float*: Nominal max power voltage [V]\n\n*Constraints*: POSITIVE\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"spe_voc", (getter)SimpleEfficiencyModuleModel_get_spe_voc,(setter)SimpleEfficiencyModuleModel_set_spe_voc,
	PyDoc_STR("*float*: Nominal open circuit voltage [V]\n\n*Constraints*: POSITIVE\n\n*Required*: set to 0 if not provided."),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject SimpleEfficiencyModuleModel_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Pvsamv1.SimpleEfficiencyModuleModel",             /*tp_name*/
		sizeof(SimpleEfficiencyModuleModelObject),          /*tp_basicsize*/
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
		SimpleEfficiencyModuleModel_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		SimpleEfficiencyModuleModel_getset,          /*tp_getset*/
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
	 * CECPerformanceModelWithModuleDatabase Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Pvsamv1   data_ptr;
} CECPerformanceModelWithModuleDatabaseObject;

static PyTypeObject CECPerformanceModelWithModuleDatabase_Type;

static PyObject *
CECPerformanceModelWithModuleDatabase_new(SAM_Pvsamv1 data_ptr)
{
	PyObject* new_obj = CECPerformanceModelWithModuleDatabase_Type.tp_alloc(&CECPerformanceModelWithModuleDatabase_Type,0);

	CECPerformanceModelWithModuleDatabaseObject* CECPerformanceModelWithModuleDatabase_obj = (CECPerformanceModelWithModuleDatabaseObject*)new_obj;

	CECPerformanceModelWithModuleDatabase_obj->data_ptr = data_ptr;

	return new_obj;
}

/* CECPerformanceModelWithModuleDatabase methods */

static PyObject *
CECPerformanceModelWithModuleDatabase_assign(CECPerformanceModelWithModuleDatabaseObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Pvsamv1", "CECPerformanceModelWithModuleDatabase")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
CECPerformanceModelWithModuleDatabase_export(CECPerformanceModelWithModuleDatabaseObject *self, PyObject *args)
{
	PyTypeObject* tp = &CECPerformanceModelWithModuleDatabase_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef CECPerformanceModelWithModuleDatabase_methods[] = {
		{"assign",            (PyCFunction)CECPerformanceModelWithModuleDatabase_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``CECPerformanceModelWithModuleDatabase_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)CECPerformanceModelWithModuleDatabase_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
CECPerformanceModelWithModuleDatabase_get_cec_a_ref(CECPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_a_ref_nget, self->data_ptr);
}

static int
CECPerformanceModelWithModuleDatabase_set_cec_a_ref(CECPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_a_ref_nset, self->data_ptr);
}

static PyObject *
CECPerformanceModelWithModuleDatabase_get_cec_adjust(CECPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_adjust_nget, self->data_ptr);
}

static int
CECPerformanceModelWithModuleDatabase_set_cec_adjust(CECPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_adjust_nset, self->data_ptr);
}

static PyObject *
CECPerformanceModelWithModuleDatabase_get_cec_alpha_sc(CECPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_alpha_sc_nget, self->data_ptr);
}

static int
CECPerformanceModelWithModuleDatabase_set_cec_alpha_sc(CECPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_alpha_sc_nset, self->data_ptr);
}

static PyObject *
CECPerformanceModelWithModuleDatabase_get_cec_area(CECPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_area_nget, self->data_ptr);
}

static int
CECPerformanceModelWithModuleDatabase_set_cec_area(CECPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_area_nset, self->data_ptr);
}

static PyObject *
CECPerformanceModelWithModuleDatabase_get_cec_array_cols(CECPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_array_cols_nget, self->data_ptr);
}

static int
CECPerformanceModelWithModuleDatabase_set_cec_array_cols(CECPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_array_cols_nset, self->data_ptr);
}

static PyObject *
CECPerformanceModelWithModuleDatabase_get_cec_array_rows(CECPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_array_rows_nget, self->data_ptr);
}

static int
CECPerformanceModelWithModuleDatabase_set_cec_array_rows(CECPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_array_rows_nset, self->data_ptr);
}

static PyObject *
CECPerformanceModelWithModuleDatabase_get_cec_backside_temp(CECPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_backside_temp_nget, self->data_ptr);
}

static int
CECPerformanceModelWithModuleDatabase_set_cec_backside_temp(CECPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_backside_temp_nset, self->data_ptr);
}

static PyObject *
CECPerformanceModelWithModuleDatabase_get_cec_beta_oc(CECPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_beta_oc_nget, self->data_ptr);
}

static int
CECPerformanceModelWithModuleDatabase_set_cec_beta_oc(CECPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_beta_oc_nset, self->data_ptr);
}

static PyObject *
CECPerformanceModelWithModuleDatabase_get_cec_bifacial_ground_clearance_height(CECPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_bifacial_ground_clearance_height_nget, self->data_ptr);
}

static int
CECPerformanceModelWithModuleDatabase_set_cec_bifacial_ground_clearance_height(CECPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_bifacial_ground_clearance_height_nset, self->data_ptr);
}

static PyObject *
CECPerformanceModelWithModuleDatabase_get_cec_bifacial_transmission_factor(CECPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_bifacial_transmission_factor_nget, self->data_ptr);
}

static int
CECPerformanceModelWithModuleDatabase_set_cec_bifacial_transmission_factor(CECPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_bifacial_transmission_factor_nset, self->data_ptr);
}

static PyObject *
CECPerformanceModelWithModuleDatabase_get_cec_bifaciality(CECPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_bifaciality_nget, self->data_ptr);
}

static int
CECPerformanceModelWithModuleDatabase_set_cec_bifaciality(CECPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_bifaciality_nset, self->data_ptr);
}

static PyObject *
CECPerformanceModelWithModuleDatabase_get_cec_gamma_r(CECPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_gamma_r_nget, self->data_ptr);
}

static int
CECPerformanceModelWithModuleDatabase_set_cec_gamma_r(CECPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_gamma_r_nset, self->data_ptr);
}

static PyObject *
CECPerformanceModelWithModuleDatabase_get_cec_gap_spacing(CECPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_gap_spacing_nget, self->data_ptr);
}

static int
CECPerformanceModelWithModuleDatabase_set_cec_gap_spacing(CECPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_gap_spacing_nset, self->data_ptr);
}

static PyObject *
CECPerformanceModelWithModuleDatabase_get_cec_heat_transfer(CECPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_heat_transfer_nget, self->data_ptr);
}

static int
CECPerformanceModelWithModuleDatabase_set_cec_heat_transfer(CECPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_heat_transfer_nset, self->data_ptr);
}

static PyObject *
CECPerformanceModelWithModuleDatabase_get_cec_height(CECPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_height_nget, self->data_ptr);
}

static int
CECPerformanceModelWithModuleDatabase_set_cec_height(CECPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_height_nset, self->data_ptr);
}

static PyObject *
CECPerformanceModelWithModuleDatabase_get_cec_i_l_ref(CECPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_i_l_ref_nget, self->data_ptr);
}

static int
CECPerformanceModelWithModuleDatabase_set_cec_i_l_ref(CECPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_i_l_ref_nset, self->data_ptr);
}

static PyObject *
CECPerformanceModelWithModuleDatabase_get_cec_i_mp_ref(CECPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_i_mp_ref_nget, self->data_ptr);
}

static int
CECPerformanceModelWithModuleDatabase_set_cec_i_mp_ref(CECPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_i_mp_ref_nset, self->data_ptr);
}

static PyObject *
CECPerformanceModelWithModuleDatabase_get_cec_i_o_ref(CECPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_i_o_ref_nget, self->data_ptr);
}

static int
CECPerformanceModelWithModuleDatabase_set_cec_i_o_ref(CECPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_i_o_ref_nset, self->data_ptr);
}

static PyObject *
CECPerformanceModelWithModuleDatabase_get_cec_i_sc_ref(CECPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_i_sc_ref_nget, self->data_ptr);
}

static int
CECPerformanceModelWithModuleDatabase_set_cec_i_sc_ref(CECPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_i_sc_ref_nset, self->data_ptr);
}

static PyObject *
CECPerformanceModelWithModuleDatabase_get_cec_is_bifacial(CECPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_is_bifacial_nget, self->data_ptr);
}

static int
CECPerformanceModelWithModuleDatabase_set_cec_is_bifacial(CECPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_is_bifacial_nset, self->data_ptr);
}

static PyObject *
CECPerformanceModelWithModuleDatabase_get_cec_module_length(CECPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_module_length_nget, self->data_ptr);
}

static int
CECPerformanceModelWithModuleDatabase_set_cec_module_length(CECPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_module_length_nset, self->data_ptr);
}

static PyObject *
CECPerformanceModelWithModuleDatabase_get_cec_module_width(CECPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_module_width_nget, self->data_ptr);
}

static int
CECPerformanceModelWithModuleDatabase_set_cec_module_width(CECPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_module_width_nset, self->data_ptr);
}

static PyObject *
CECPerformanceModelWithModuleDatabase_get_cec_mounting_config(CECPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_mounting_config_nget, self->data_ptr);
}

static int
CECPerformanceModelWithModuleDatabase_set_cec_mounting_config(CECPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_mounting_config_nset, self->data_ptr);
}

static PyObject *
CECPerformanceModelWithModuleDatabase_get_cec_mounting_orientation(CECPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_mounting_orientation_nget, self->data_ptr);
}

static int
CECPerformanceModelWithModuleDatabase_set_cec_mounting_orientation(CECPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_mounting_orientation_nset, self->data_ptr);
}

static PyObject *
CECPerformanceModelWithModuleDatabase_get_cec_n_s(CECPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_n_s_nget, self->data_ptr);
}

static int
CECPerformanceModelWithModuleDatabase_set_cec_n_s(CECPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_n_s_nset, self->data_ptr);
}

static PyObject *
CECPerformanceModelWithModuleDatabase_get_cec_r_s(CECPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_r_s_nget, self->data_ptr);
}

static int
CECPerformanceModelWithModuleDatabase_set_cec_r_s(CECPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_r_s_nset, self->data_ptr);
}

static PyObject *
CECPerformanceModelWithModuleDatabase_get_cec_r_sh_ref(CECPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_r_sh_ref_nget, self->data_ptr);
}

static int
CECPerformanceModelWithModuleDatabase_set_cec_r_sh_ref(CECPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_r_sh_ref_nset, self->data_ptr);
}

static PyObject *
CECPerformanceModelWithModuleDatabase_get_cec_standoff(CECPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_standoff_nget, self->data_ptr);
}

static int
CECPerformanceModelWithModuleDatabase_set_cec_standoff(CECPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_standoff_nset, self->data_ptr);
}

static PyObject *
CECPerformanceModelWithModuleDatabase_get_cec_t_noct(CECPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_t_noct_nget, self->data_ptr);
}

static int
CECPerformanceModelWithModuleDatabase_set_cec_t_noct(CECPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_t_noct_nset, self->data_ptr);
}

static PyObject *
CECPerformanceModelWithModuleDatabase_get_cec_temp_corr_mode(CECPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_temp_corr_mode_nget, self->data_ptr);
}

static int
CECPerformanceModelWithModuleDatabase_set_cec_temp_corr_mode(CECPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_temp_corr_mode_nset, self->data_ptr);
}

static PyObject *
CECPerformanceModelWithModuleDatabase_get_cec_v_mp_ref(CECPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_v_mp_ref_nget, self->data_ptr);
}

static int
CECPerformanceModelWithModuleDatabase_set_cec_v_mp_ref(CECPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_v_mp_ref_nset, self->data_ptr);
}

static PyObject *
CECPerformanceModelWithModuleDatabase_get_cec_v_oc_ref(CECPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_v_oc_ref_nget, self->data_ptr);
}

static int
CECPerformanceModelWithModuleDatabase_set_cec_v_oc_ref(CECPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_v_oc_ref_nset, self->data_ptr);
}

static PyGetSetDef CECPerformanceModelWithModuleDatabase_getset[] = {
{"cec_a_ref", (getter)CECPerformanceModelWithModuleDatabase_get_cec_a_ref,(setter)CECPerformanceModelWithModuleDatabase_set_cec_a_ref,
	PyDoc_STR("*float*: Nonideality factor a\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"cec_adjust", (getter)CECPerformanceModelWithModuleDatabase_get_cec_adjust,(setter)CECPerformanceModelWithModuleDatabase_set_cec_adjust,
	PyDoc_STR("*float*: Temperature coefficient adjustment [%]\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"cec_alpha_sc", (getter)CECPerformanceModelWithModuleDatabase_get_cec_alpha_sc,(setter)CECPerformanceModelWithModuleDatabase_set_cec_alpha_sc,
	PyDoc_STR("*float*: Short circuit current temperature coefficient [A/C]\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"cec_area", (getter)CECPerformanceModelWithModuleDatabase_get_cec_area,(setter)CECPerformanceModelWithModuleDatabase_set_cec_area,
	PyDoc_STR("*float*: Module area [m2]\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"cec_array_cols", (getter)CECPerformanceModelWithModuleDatabase_get_cec_array_cols,(setter)CECPerformanceModelWithModuleDatabase_set_cec_array_cols,
	PyDoc_STR("*float*: Columns of modules in array\n\n*Required*: set to 1&cec_temp_corr_mode=1 if not provided."),
 	NULL},
{"cec_array_rows", (getter)CECPerformanceModelWithModuleDatabase_get_cec_array_rows,(setter)CECPerformanceModelWithModuleDatabase_set_cec_array_rows,
	PyDoc_STR("*float*: Rows of modules in array\n\n*Required*: set to 1&cec_temp_corr_mode=1 if not provided."),
 	NULL},
{"cec_backside_temp", (getter)CECPerformanceModelWithModuleDatabase_get_cec_backside_temp,(setter)CECPerformanceModelWithModuleDatabase_set_cec_backside_temp,
	PyDoc_STR("*float*: Module backside temperature [C]\n\n*Constraints*: POSITIVE\n\n*Required*: set to 1&cec_temp_corr_mode=1 if not provided."),
 	NULL},
{"cec_beta_oc", (getter)CECPerformanceModelWithModuleDatabase_get_cec_beta_oc,(setter)CECPerformanceModelWithModuleDatabase_set_cec_beta_oc,
	PyDoc_STR("*float*: Open circuit voltage temperature coefficient [V/C]\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"cec_bifacial_ground_clearance_height", (getter)CECPerformanceModelWithModuleDatabase_get_cec_bifacial_ground_clearance_height,(setter)CECPerformanceModelWithModuleDatabase_set_cec_bifacial_ground_clearance_height,
	PyDoc_STR("*float*: Module ground clearance height [m]\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"cec_bifacial_transmission_factor", (getter)CECPerformanceModelWithModuleDatabase_get_cec_bifacial_transmission_factor,(setter)CECPerformanceModelWithModuleDatabase_set_cec_bifacial_transmission_factor,
	PyDoc_STR("*float*: Bifacial transmission factor [0-1]\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"cec_bifaciality", (getter)CECPerformanceModelWithModuleDatabase_get_cec_bifaciality,(setter)CECPerformanceModelWithModuleDatabase_set_cec_bifaciality,
	PyDoc_STR("*float*: Bifaciality factor [%]\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"cec_gamma_r", (getter)CECPerformanceModelWithModuleDatabase_get_cec_gamma_r,(setter)CECPerformanceModelWithModuleDatabase_set_cec_gamma_r,
	PyDoc_STR("*float*: Maximum power point temperature coefficient [%/C]\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"cec_gap_spacing", (getter)CECPerformanceModelWithModuleDatabase_get_cec_gap_spacing,(setter)CECPerformanceModelWithModuleDatabase_set_cec_gap_spacing,
	PyDoc_STR("*float*: Gap spacing [m]\n\n*Required*: set to 1&cec_temp_corr_mode=1 if not provided."),
 	NULL},
{"cec_heat_transfer", (getter)CECPerformanceModelWithModuleDatabase_get_cec_heat_transfer,(setter)CECPerformanceModelWithModuleDatabase_set_cec_heat_transfer,
	PyDoc_STR("*float*: Heat transfer dimensions\n\n*Options*: 0=module,1=array\n\n*Constraints*: INTEGER,MIN=0,MAX=1\n\n*Required*: set to 1&cec_temp_corr_mode=1 if not provided."),
 	NULL},
{"cec_height", (getter)CECPerformanceModelWithModuleDatabase_get_cec_height,(setter)CECPerformanceModelWithModuleDatabase_set_cec_height,
	PyDoc_STR("*float*: Array mounting height\n\n*Options*: 0=one story,1=two story\n\n*Constraints*: INTEGER,MIN=0,MAX=1\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"cec_i_l_ref", (getter)CECPerformanceModelWithModuleDatabase_get_cec_i_l_ref,(setter)CECPerformanceModelWithModuleDatabase_set_cec_i_l_ref,
	PyDoc_STR("*float*: Light current [A]\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"cec_i_mp_ref", (getter)CECPerformanceModelWithModuleDatabase_get_cec_i_mp_ref,(setter)CECPerformanceModelWithModuleDatabase_set_cec_i_mp_ref,
	PyDoc_STR("*float*: Maximum power point current [A]\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"cec_i_o_ref", (getter)CECPerformanceModelWithModuleDatabase_get_cec_i_o_ref,(setter)CECPerformanceModelWithModuleDatabase_set_cec_i_o_ref,
	PyDoc_STR("*float*: Saturation current [A]\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"cec_i_sc_ref", (getter)CECPerformanceModelWithModuleDatabase_get_cec_i_sc_ref,(setter)CECPerformanceModelWithModuleDatabase_set_cec_i_sc_ref,
	PyDoc_STR("*float*: Short circuit current [A]\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"cec_is_bifacial", (getter)CECPerformanceModelWithModuleDatabase_get_cec_is_bifacial,(setter)CECPerformanceModelWithModuleDatabase_set_cec_is_bifacial,
	PyDoc_STR("*float*: Modules are bifacial [0/1]\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"cec_module_length", (getter)CECPerformanceModelWithModuleDatabase_get_cec_module_length,(setter)CECPerformanceModelWithModuleDatabase_set_cec_module_length,
	PyDoc_STR("*float*: Module height [m]\n\n*Required*: set to 1&cec_temp_corr_mode=1 if not provided."),
 	NULL},
{"cec_module_width", (getter)CECPerformanceModelWithModuleDatabase_get_cec_module_width,(setter)CECPerformanceModelWithModuleDatabase_set_cec_module_width,
	PyDoc_STR("*float*: Module width [m]\n\n*Required*: set to 1&cec_temp_corr_mode=1 if not provided."),
 	NULL},
{"cec_mounting_config", (getter)CECPerformanceModelWithModuleDatabase_get_cec_mounting_config,(setter)CECPerformanceModelWithModuleDatabase_set_cec_mounting_config,
	PyDoc_STR("*float*: Mounting configuration\n\n*Options*: 0=rack,1=flush,2=integrated,3=gap\n\n*Constraints*: INTEGER,MIN=0,MAX=3\n\n*Required*: set to 1&cec_temp_corr_mode=1 if not provided."),
 	NULL},
{"cec_mounting_orientation", (getter)CECPerformanceModelWithModuleDatabase_get_cec_mounting_orientation,(setter)CECPerformanceModelWithModuleDatabase_set_cec_mounting_orientation,
	PyDoc_STR("*float*: Mounting structure orientation\n\n*Options*: 0=do not impede flow,1=vertical supports,2=horizontal supports\n\n*Constraints*: INTEGER,MIN=0,MAX=2\n\n*Required*: set to 1&cec_temp_corr_mode=1 if not provided."),
 	NULL},
{"cec_n_s", (getter)CECPerformanceModelWithModuleDatabase_get_cec_n_s,(setter)CECPerformanceModelWithModuleDatabase_set_cec_n_s,
	PyDoc_STR("*float*: Number of cells in series\n\n*Constraints*: POSITIVE\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"cec_r_s", (getter)CECPerformanceModelWithModuleDatabase_get_cec_r_s,(setter)CECPerformanceModelWithModuleDatabase_set_cec_r_s,
	PyDoc_STR("*float*: Series resistance [ohm]\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"cec_r_sh_ref", (getter)CECPerformanceModelWithModuleDatabase_get_cec_r_sh_ref,(setter)CECPerformanceModelWithModuleDatabase_set_cec_r_sh_ref,
	PyDoc_STR("*float*: Shunt resistance [ohm]\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"cec_standoff", (getter)CECPerformanceModelWithModuleDatabase_get_cec_standoff,(setter)CECPerformanceModelWithModuleDatabase_set_cec_standoff,
	PyDoc_STR("*float*: Standoff mode\n\n*Options*: 0=bipv,1=>3.5in,2=2.5-3.5in,3=1.5-2.5in,4=0.5-1.5in,5=<0.5in,6=ground/rack\n\n*Constraints*: INTEGER,MIN=0,MAX=6\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"cec_t_noct", (getter)CECPerformanceModelWithModuleDatabase_get_cec_t_noct,(setter)CECPerformanceModelWithModuleDatabase_set_cec_t_noct,
	PyDoc_STR("*float*: Nominal operating cell temperature [C]\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"cec_temp_corr_mode", (getter)CECPerformanceModelWithModuleDatabase_get_cec_temp_corr_mode,(setter)CECPerformanceModelWithModuleDatabase_set_cec_temp_corr_mode,
	PyDoc_STR("*float*: Cell temperature model selection\n\n*Options*: 0=noct,1=mc\n\n*Constraints*: INTEGER,MIN=0,MAX=1\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"cec_v_mp_ref", (getter)CECPerformanceModelWithModuleDatabase_get_cec_v_mp_ref,(setter)CECPerformanceModelWithModuleDatabase_set_cec_v_mp_ref,
	PyDoc_STR("*float*: Maximum power point voltage [V]\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"cec_v_oc_ref", (getter)CECPerformanceModelWithModuleDatabase_get_cec_v_oc_ref,(setter)CECPerformanceModelWithModuleDatabase_set_cec_v_oc_ref,
	PyDoc_STR("*float*: Open circuit voltage [V]\n\n*Required*: set to 1 if not provided."),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject CECPerformanceModelWithModuleDatabase_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Pvsamv1.CECPerformanceModelWithModuleDatabase",             /*tp_name*/
		sizeof(CECPerformanceModelWithModuleDatabaseObject),          /*tp_basicsize*/
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
		CECPerformanceModelWithModuleDatabase_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		CECPerformanceModelWithModuleDatabase_getset,          /*tp_getset*/
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
	 * CECPerformanceModelWithUserEnteredSpecifications Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Pvsamv1   data_ptr;
} CECPerformanceModelWithUserEnteredSpecificationsObject;

static PyTypeObject CECPerformanceModelWithUserEnteredSpecifications_Type;

static PyObject *
CECPerformanceModelWithUserEnteredSpecifications_new(SAM_Pvsamv1 data_ptr)
{
	PyObject* new_obj = CECPerformanceModelWithUserEnteredSpecifications_Type.tp_alloc(&CECPerformanceModelWithUserEnteredSpecifications_Type,0);

	CECPerformanceModelWithUserEnteredSpecificationsObject* CECPerformanceModelWithUserEnteredSpecifications_obj = (CECPerformanceModelWithUserEnteredSpecificationsObject*)new_obj;

	CECPerformanceModelWithUserEnteredSpecifications_obj->data_ptr = data_ptr;

	return new_obj;
}

/* CECPerformanceModelWithUserEnteredSpecifications methods */

static PyObject *
CECPerformanceModelWithUserEnteredSpecifications_assign(CECPerformanceModelWithUserEnteredSpecificationsObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Pvsamv1", "CECPerformanceModelWithUserEnteredSpecifications")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
CECPerformanceModelWithUserEnteredSpecifications_export(CECPerformanceModelWithUserEnteredSpecificationsObject *self, PyObject *args)
{
	PyTypeObject* tp = &CECPerformanceModelWithUserEnteredSpecifications_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef CECPerformanceModelWithUserEnteredSpecifications_methods[] = {
		{"assign",            (PyCFunction)CECPerformanceModelWithUserEnteredSpecifications_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``CECPerformanceModelWithUserEnteredSpecifications_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)CECPerformanceModelWithUserEnteredSpecifications_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
CECPerformanceModelWithUserEnteredSpecifications_get_6par_aisc(CECPerformanceModelWithUserEnteredSpecificationsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_6par_aisc_nget, self->data_ptr);
}

static int
CECPerformanceModelWithUserEnteredSpecifications_set_6par_aisc(CECPerformanceModelWithUserEnteredSpecificationsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_6par_aisc_nset, self->data_ptr);
}

static PyObject *
CECPerformanceModelWithUserEnteredSpecifications_get_6par_area(CECPerformanceModelWithUserEnteredSpecificationsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_6par_area_nget, self->data_ptr);
}

static int
CECPerformanceModelWithUserEnteredSpecifications_set_6par_area(CECPerformanceModelWithUserEnteredSpecificationsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_6par_area_nset, self->data_ptr);
}

static PyObject *
CECPerformanceModelWithUserEnteredSpecifications_get_6par_bifacial_ground_clearance_height(CECPerformanceModelWithUserEnteredSpecificationsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_6par_bifacial_ground_clearance_height_nget, self->data_ptr);
}

static int
CECPerformanceModelWithUserEnteredSpecifications_set_6par_bifacial_ground_clearance_height(CECPerformanceModelWithUserEnteredSpecificationsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_6par_bifacial_ground_clearance_height_nset, self->data_ptr);
}

static PyObject *
CECPerformanceModelWithUserEnteredSpecifications_get_6par_bifacial_transmission_factor(CECPerformanceModelWithUserEnteredSpecificationsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_6par_bifacial_transmission_factor_nget, self->data_ptr);
}

static int
CECPerformanceModelWithUserEnteredSpecifications_set_6par_bifacial_transmission_factor(CECPerformanceModelWithUserEnteredSpecificationsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_6par_bifacial_transmission_factor_nset, self->data_ptr);
}

static PyObject *
CECPerformanceModelWithUserEnteredSpecifications_get_6par_bifaciality(CECPerformanceModelWithUserEnteredSpecificationsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_6par_bifaciality_nget, self->data_ptr);
}

static int
CECPerformanceModelWithUserEnteredSpecifications_set_6par_bifaciality(CECPerformanceModelWithUserEnteredSpecificationsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_6par_bifaciality_nset, self->data_ptr);
}

static PyObject *
CECPerformanceModelWithUserEnteredSpecifications_get_6par_bvoc(CECPerformanceModelWithUserEnteredSpecificationsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_6par_bvoc_nget, self->data_ptr);
}

static int
CECPerformanceModelWithUserEnteredSpecifications_set_6par_bvoc(CECPerformanceModelWithUserEnteredSpecificationsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_6par_bvoc_nset, self->data_ptr);
}

static PyObject *
CECPerformanceModelWithUserEnteredSpecifications_get_6par_celltech(CECPerformanceModelWithUserEnteredSpecificationsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_6par_celltech_nget, self->data_ptr);
}

static int
CECPerformanceModelWithUserEnteredSpecifications_set_6par_celltech(CECPerformanceModelWithUserEnteredSpecificationsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_6par_celltech_nset, self->data_ptr);
}

static PyObject *
CECPerformanceModelWithUserEnteredSpecifications_get_6par_gpmp(CECPerformanceModelWithUserEnteredSpecificationsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_6par_gpmp_nget, self->data_ptr);
}

static int
CECPerformanceModelWithUserEnteredSpecifications_set_6par_gpmp(CECPerformanceModelWithUserEnteredSpecificationsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_6par_gpmp_nset, self->data_ptr);
}

static PyObject *
CECPerformanceModelWithUserEnteredSpecifications_get_6par_imp(CECPerformanceModelWithUserEnteredSpecificationsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_6par_imp_nget, self->data_ptr);
}

static int
CECPerformanceModelWithUserEnteredSpecifications_set_6par_imp(CECPerformanceModelWithUserEnteredSpecificationsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_6par_imp_nset, self->data_ptr);
}

static PyObject *
CECPerformanceModelWithUserEnteredSpecifications_get_6par_is_bifacial(CECPerformanceModelWithUserEnteredSpecificationsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_6par_is_bifacial_nget, self->data_ptr);
}

static int
CECPerformanceModelWithUserEnteredSpecifications_set_6par_is_bifacial(CECPerformanceModelWithUserEnteredSpecificationsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_6par_is_bifacial_nset, self->data_ptr);
}

static PyObject *
CECPerformanceModelWithUserEnteredSpecifications_get_6par_isc(CECPerformanceModelWithUserEnteredSpecificationsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_6par_isc_nget, self->data_ptr);
}

static int
CECPerformanceModelWithUserEnteredSpecifications_set_6par_isc(CECPerformanceModelWithUserEnteredSpecificationsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_6par_isc_nset, self->data_ptr);
}

static PyObject *
CECPerformanceModelWithUserEnteredSpecifications_get_6par_mounting(CECPerformanceModelWithUserEnteredSpecificationsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_6par_mounting_nget, self->data_ptr);
}

static int
CECPerformanceModelWithUserEnteredSpecifications_set_6par_mounting(CECPerformanceModelWithUserEnteredSpecificationsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_6par_mounting_nset, self->data_ptr);
}

static PyObject *
CECPerformanceModelWithUserEnteredSpecifications_get_6par_nser(CECPerformanceModelWithUserEnteredSpecificationsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_6par_nser_nget, self->data_ptr);
}

static int
CECPerformanceModelWithUserEnteredSpecifications_set_6par_nser(CECPerformanceModelWithUserEnteredSpecificationsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_6par_nser_nset, self->data_ptr);
}

static PyObject *
CECPerformanceModelWithUserEnteredSpecifications_get_6par_standoff(CECPerformanceModelWithUserEnteredSpecificationsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_6par_standoff_nget, self->data_ptr);
}

static int
CECPerformanceModelWithUserEnteredSpecifications_set_6par_standoff(CECPerformanceModelWithUserEnteredSpecificationsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_6par_standoff_nset, self->data_ptr);
}

static PyObject *
CECPerformanceModelWithUserEnteredSpecifications_get_6par_tnoct(CECPerformanceModelWithUserEnteredSpecificationsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_6par_tnoct_nget, self->data_ptr);
}

static int
CECPerformanceModelWithUserEnteredSpecifications_set_6par_tnoct(CECPerformanceModelWithUserEnteredSpecificationsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_6par_tnoct_nset, self->data_ptr);
}

static PyObject *
CECPerformanceModelWithUserEnteredSpecifications_get_6par_vmp(CECPerformanceModelWithUserEnteredSpecificationsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_6par_vmp_nget, self->data_ptr);
}

static int
CECPerformanceModelWithUserEnteredSpecifications_set_6par_vmp(CECPerformanceModelWithUserEnteredSpecificationsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_6par_vmp_nset, self->data_ptr);
}

static PyObject *
CECPerformanceModelWithUserEnteredSpecifications_get_6par_voc(CECPerformanceModelWithUserEnteredSpecificationsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_6par_voc_nget, self->data_ptr);
}

static int
CECPerformanceModelWithUserEnteredSpecifications_set_6par_voc(CECPerformanceModelWithUserEnteredSpecificationsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_6par_voc_nset, self->data_ptr);
}

static PyGetSetDef CECPerformanceModelWithUserEnteredSpecifications_getset[] = {
{"6par_aisc", (getter)CECPerformanceModelWithUserEnteredSpecifications_get_6par_aisc,(setter)CECPerformanceModelWithUserEnteredSpecifications_set_6par_aisc,
	PyDoc_STR("*float*: Short circuit current temperature coefficient [A/C]\n\n*Required*: set to 2 if not provided."),
 	NULL},
{"6par_area", (getter)CECPerformanceModelWithUserEnteredSpecifications_get_6par_area,(setter)CECPerformanceModelWithUserEnteredSpecifications_set_6par_area,
	PyDoc_STR("*float*: Module area [m2]\n\n*Required*: set to 2 if not provided."),
 	NULL},
{"6par_bifacial_ground_clearance_height", (getter)CECPerformanceModelWithUserEnteredSpecifications_get_6par_bifacial_ground_clearance_height,(setter)CECPerformanceModelWithUserEnteredSpecifications_set_6par_bifacial_ground_clearance_height,
	PyDoc_STR("*float*: Module ground clearance height [m]\n\n*Required*: set to 2 if not provided."),
 	NULL},
{"6par_bifacial_transmission_factor", (getter)CECPerformanceModelWithUserEnteredSpecifications_get_6par_bifacial_transmission_factor,(setter)CECPerformanceModelWithUserEnteredSpecifications_set_6par_bifacial_transmission_factor,
	PyDoc_STR("*float*: Bifacial transmission factor [0-1]\n\n*Required*: set to 2 if not provided."),
 	NULL},
{"6par_bifaciality", (getter)CECPerformanceModelWithUserEnteredSpecifications_get_6par_bifaciality,(setter)CECPerformanceModelWithUserEnteredSpecifications_set_6par_bifaciality,
	PyDoc_STR("*float*: Bifaciality factor [%]\n\n*Required*: set to 2 if not provided."),
 	NULL},
{"6par_bvoc", (getter)CECPerformanceModelWithUserEnteredSpecifications_get_6par_bvoc,(setter)CECPerformanceModelWithUserEnteredSpecifications_set_6par_bvoc,
	PyDoc_STR("*float*: Open circuit voltage temperature coefficient [V/C]\n\n*Required*: set to 2 if not provided."),
 	NULL},
{"6par_celltech", (getter)CECPerformanceModelWithUserEnteredSpecifications_get_6par_celltech,(setter)CECPerformanceModelWithUserEnteredSpecifications_set_6par_celltech,
	PyDoc_STR("*float*: Solar cell technology type\n\n*Options*: monoSi=0,multiSi=1,CdTe=2,CIS=3,CIGS=4,Amorphous=5\n\n*Constraints*: INTEGER,MIN=0,MAX=5\n\n*Required*: set to 2 if not provided."),
 	NULL},
{"6par_gpmp", (getter)CECPerformanceModelWithUserEnteredSpecifications_get_6par_gpmp,(setter)CECPerformanceModelWithUserEnteredSpecifications_set_6par_gpmp,
	PyDoc_STR("*float*: Maximum power point temperature coefficient [%/C]\n\n*Required*: set to 2 if not provided."),
 	NULL},
{"6par_imp", (getter)CECPerformanceModelWithUserEnteredSpecifications_get_6par_imp,(setter)CECPerformanceModelWithUserEnteredSpecifications_set_6par_imp,
	PyDoc_STR("*float*: Imp [A]\n\n*Required*: set to 2 if not provided."),
 	NULL},
{"6par_is_bifacial", (getter)CECPerformanceModelWithUserEnteredSpecifications_get_6par_is_bifacial,(setter)CECPerformanceModelWithUserEnteredSpecifications_set_6par_is_bifacial,
	PyDoc_STR("*float*: Modules are bifacial [0/1]\n\n*Required*: set to 2 if not provided."),
 	NULL},
{"6par_isc", (getter)CECPerformanceModelWithUserEnteredSpecifications_get_6par_isc,(setter)CECPerformanceModelWithUserEnteredSpecifications_set_6par_isc,
	PyDoc_STR("*float*: Isc [A]\n\n*Required*: set to 2 if not provided."),
 	NULL},
{"6par_mounting", (getter)CECPerformanceModelWithUserEnteredSpecifications_get_6par_mounting,(setter)CECPerformanceModelWithUserEnteredSpecifications_set_6par_mounting,
	PyDoc_STR("*float*: Array mounting height\n\n*Options*: 0=one story,1=two story\n\n*Constraints*: INTEGER,MIN=0,MAX=1\n\n*Required*: set to 2 if not provided."),
 	NULL},
{"6par_nser", (getter)CECPerformanceModelWithUserEnteredSpecifications_get_6par_nser,(setter)CECPerformanceModelWithUserEnteredSpecifications_set_6par_nser,
	PyDoc_STR("*float*: Nseries\n\n*Constraints*: INTEGER,POSITIVE\n\n*Required*: set to 2 if not provided."),
 	NULL},
{"6par_standoff", (getter)CECPerformanceModelWithUserEnteredSpecifications_get_6par_standoff,(setter)CECPerformanceModelWithUserEnteredSpecifications_set_6par_standoff,
	PyDoc_STR("*float*: Standoff mode\n\n*Options*: 0=bipv,1=>3.5in,2=2.5-3.5in,3=1.5-2.5in,4=0.5-1.5in,5=<0.5in,6=ground/rack\n\n*Constraints*: INTEGER,MIN=0,MAX=6\n\n*Required*: set to 2 if not provided."),
 	NULL},
{"6par_tnoct", (getter)CECPerformanceModelWithUserEnteredSpecifications_get_6par_tnoct,(setter)CECPerformanceModelWithUserEnteredSpecifications_set_6par_tnoct,
	PyDoc_STR("*float*: Nominal operating cell temperature [C]\n\n*Required*: set to 2 if not provided."),
 	NULL},
{"6par_vmp", (getter)CECPerformanceModelWithUserEnteredSpecifications_get_6par_vmp,(setter)CECPerformanceModelWithUserEnteredSpecifications_set_6par_vmp,
	PyDoc_STR("*float*: Maximum power point voltage [V]\n\n*Required*: set to 2 if not provided."),
 	NULL},
{"6par_voc", (getter)CECPerformanceModelWithUserEnteredSpecifications_get_6par_voc,(setter)CECPerformanceModelWithUserEnteredSpecifications_set_6par_voc,
	PyDoc_STR("*float*: Voc [V]\n\n*Required*: set to 2 if not provided."),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject CECPerformanceModelWithUserEnteredSpecifications_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Pvsamv1.CECPerformanceModelWithUserEnteredSpecifications",             /*tp_name*/
		sizeof(CECPerformanceModelWithUserEnteredSpecificationsObject),          /*tp_basicsize*/
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
		CECPerformanceModelWithUserEnteredSpecifications_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		CECPerformanceModelWithUserEnteredSpecifications_getset,          /*tp_getset*/
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
	 * SandiaPVArrayPerformanceModelWithModuleDatabase Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Pvsamv1   data_ptr;
} SandiaPVArrayPerformanceModelWithModuleDatabaseObject;

static PyTypeObject SandiaPVArrayPerformanceModelWithModuleDatabase_Type;

static PyObject *
SandiaPVArrayPerformanceModelWithModuleDatabase_new(SAM_Pvsamv1 data_ptr)
{
	PyObject* new_obj = SandiaPVArrayPerformanceModelWithModuleDatabase_Type.tp_alloc(&SandiaPVArrayPerformanceModelWithModuleDatabase_Type,0);

	SandiaPVArrayPerformanceModelWithModuleDatabaseObject* SandiaPVArrayPerformanceModelWithModuleDatabase_obj = (SandiaPVArrayPerformanceModelWithModuleDatabaseObject*)new_obj;

	SandiaPVArrayPerformanceModelWithModuleDatabase_obj->data_ptr = data_ptr;

	return new_obj;
}

/* SandiaPVArrayPerformanceModelWithModuleDatabase methods */

static PyObject *
SandiaPVArrayPerformanceModelWithModuleDatabase_assign(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Pvsamv1", "SandiaPVArrayPerformanceModelWithModuleDatabase")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
SandiaPVArrayPerformanceModelWithModuleDatabase_export(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, PyObject *args)
{
	PyTypeObject* tp = &SandiaPVArrayPerformanceModelWithModuleDatabase_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef SandiaPVArrayPerformanceModelWithModuleDatabase_methods[] = {
		{"assign",            (PyCFunction)SandiaPVArrayPerformanceModelWithModuleDatabase_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``SandiaPVArrayPerformanceModelWithModuleDatabase_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)SandiaPVArrayPerformanceModelWithModuleDatabase_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_a(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_a_nget, self->data_ptr);
}

static int
SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_a(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_a_nset, self->data_ptr);
}

static PyObject *
SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_a0(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_a0_nget, self->data_ptr);
}

static int
SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_a0(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_a0_nset, self->data_ptr);
}

static PyObject *
SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_a1(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_a1_nget, self->data_ptr);
}

static int
SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_a1(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_a1_nset, self->data_ptr);
}

static PyObject *
SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_a2(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_a2_nget, self->data_ptr);
}

static int
SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_a2(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_a2_nset, self->data_ptr);
}

static PyObject *
SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_a3(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_a3_nget, self->data_ptr);
}

static int
SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_a3(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_a3_nset, self->data_ptr);
}

static PyObject *
SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_a4(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_a4_nget, self->data_ptr);
}

static int
SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_a4(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_a4_nset, self->data_ptr);
}

static PyObject *
SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_aimp(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_aimp_nget, self->data_ptr);
}

static int
SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_aimp(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_aimp_nset, self->data_ptr);
}

static PyObject *
SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_aisc(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_aisc_nget, self->data_ptr);
}

static int
SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_aisc(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_aisc_nset, self->data_ptr);
}

static PyObject *
SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_area(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_area_nget, self->data_ptr);
}

static int
SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_area(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_area_nset, self->data_ptr);
}

static PyObject *
SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_b(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_b_nget, self->data_ptr);
}

static int
SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_b(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_b_nset, self->data_ptr);
}

static PyObject *
SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_b0(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_b0_nget, self->data_ptr);
}

static int
SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_b0(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_b0_nset, self->data_ptr);
}

static PyObject *
SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_b1(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_b1_nget, self->data_ptr);
}

static int
SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_b1(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_b1_nset, self->data_ptr);
}

static PyObject *
SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_b2(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_b2_nget, self->data_ptr);
}

static int
SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_b2(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_b2_nset, self->data_ptr);
}

static PyObject *
SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_b3(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_b3_nget, self->data_ptr);
}

static int
SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_b3(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_b3_nset, self->data_ptr);
}

static PyObject *
SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_b4(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_b4_nget, self->data_ptr);
}

static int
SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_b4(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_b4_nset, self->data_ptr);
}

static PyObject *
SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_b5(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_b5_nget, self->data_ptr);
}

static int
SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_b5(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_b5_nset, self->data_ptr);
}

static PyObject *
SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_bvmpo(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_bvmpo_nget, self->data_ptr);
}

static int
SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_bvmpo(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_bvmpo_nset, self->data_ptr);
}

static PyObject *
SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_bvoco(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_bvoco_nget, self->data_ptr);
}

static int
SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_bvoco(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_bvoco_nset, self->data_ptr);
}

static PyObject *
SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_c0(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_c0_nget, self->data_ptr);
}

static int
SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_c0(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_c0_nset, self->data_ptr);
}

static PyObject *
SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_c1(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_c1_nget, self->data_ptr);
}

static int
SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_c1(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_c1_nset, self->data_ptr);
}

static PyObject *
SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_c2(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_c2_nget, self->data_ptr);
}

static int
SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_c2(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_c2_nset, self->data_ptr);
}

static PyObject *
SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_c3(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_c3_nget, self->data_ptr);
}

static int
SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_c3(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_c3_nset, self->data_ptr);
}

static PyObject *
SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_c4(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_c4_nget, self->data_ptr);
}

static int
SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_c4(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_c4_nset, self->data_ptr);
}

static PyObject *
SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_c5(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_c5_nget, self->data_ptr);
}

static int
SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_c5(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_c5_nset, self->data_ptr);
}

static PyObject *
SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_c6(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_c6_nget, self->data_ptr);
}

static int
SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_c6(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_c6_nset, self->data_ptr);
}

static PyObject *
SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_c7(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_c7_nget, self->data_ptr);
}

static int
SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_c7(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_c7_nset, self->data_ptr);
}

static PyObject *
SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_dtc(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_dtc_nget, self->data_ptr);
}

static int
SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_dtc(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_dtc_nset, self->data_ptr);
}

static PyObject *
SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_fd(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_fd_nget, self->data_ptr);
}

static int
SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_fd(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_fd_nset, self->data_ptr);
}

static PyObject *
SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_impo(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_impo_nget, self->data_ptr);
}

static int
SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_impo(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_impo_nset, self->data_ptr);
}

static PyObject *
SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_isco(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_isco_nget, self->data_ptr);
}

static int
SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_isco(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_isco_nset, self->data_ptr);
}

static PyObject *
SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_ixo(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_ixo_nget, self->data_ptr);
}

static int
SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_ixo(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_ixo_nset, self->data_ptr);
}

static PyObject *
SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_ixxo(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_ixxo_nget, self->data_ptr);
}

static int
SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_ixxo(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_ixxo_nset, self->data_ptr);
}

static PyObject *
SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_mbvmp(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_mbvmp_nget, self->data_ptr);
}

static int
SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_mbvmp(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_mbvmp_nset, self->data_ptr);
}

static PyObject *
SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_mbvoc(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_mbvoc_nget, self->data_ptr);
}

static int
SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_mbvoc(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_mbvoc_nset, self->data_ptr);
}

static PyObject *
SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_module_structure(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_module_structure_nget, self->data_ptr);
}

static int
SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_module_structure(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_module_structure_nset, self->data_ptr);
}

static PyObject *
SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_n(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_n_nget, self->data_ptr);
}

static int
SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_n(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_n_nset, self->data_ptr);
}

static PyObject *
SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_ref_a(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_ref_a_nget, self->data_ptr);
}

static int
SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_ref_a(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_ref_a_nset, self->data_ptr);
}

static PyObject *
SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_ref_b(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_ref_b_nget, self->data_ptr);
}

static int
SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_ref_b(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_ref_b_nset, self->data_ptr);
}

static PyObject *
SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_ref_dT(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_ref_dT_nget, self->data_ptr);
}

static int
SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_ref_dT(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_ref_dT_nset, self->data_ptr);
}

static PyObject *
SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_series_cells(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_series_cells_nget, self->data_ptr);
}

static int
SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_series_cells(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_series_cells_nset, self->data_ptr);
}

static PyObject *
SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_vmpo(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_vmpo_nget, self->data_ptr);
}

static int
SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_vmpo(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_vmpo_nset, self->data_ptr);
}

static PyObject *
SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_voco(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_voco_nget, self->data_ptr);
}

static int
SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_voco(SandiaPVArrayPerformanceModelWithModuleDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_voco_nset, self->data_ptr);
}

static PyGetSetDef SandiaPVArrayPerformanceModelWithModuleDatabase_getset[] = {
{"snl_a", (getter)SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_a,(setter)SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_a,
	PyDoc_STR("*float*: Temperature coefficient a\n\n*Required*: set to 3 if not provided."),
 	NULL},
{"snl_a0", (getter)SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_a0,(setter)SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_a0,
	PyDoc_STR("*float*: Air mass polynomial coeff 0\n\n*Required*: set to 3 if not provided."),
 	NULL},
{"snl_a1", (getter)SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_a1,(setter)SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_a1,
	PyDoc_STR("*float*: Air mass polynomial coeff 1\n\n*Required*: set to 3 if not provided."),
 	NULL},
{"snl_a2", (getter)SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_a2,(setter)SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_a2,
	PyDoc_STR("*float*: Air mass polynomial coeff 2\n\n*Required*: set to 3 if not provided."),
 	NULL},
{"snl_a3", (getter)SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_a3,(setter)SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_a3,
	PyDoc_STR("*float*: Air mass polynomial coeff 3\n\n*Required*: set to 3 if not provided."),
 	NULL},
{"snl_a4", (getter)SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_a4,(setter)SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_a4,
	PyDoc_STR("*float*: Air mass polynomial coeff 4\n\n*Required*: set to 3 if not provided."),
 	NULL},
{"snl_aimp", (getter)SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_aimp,(setter)SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_aimp,
	PyDoc_STR("*float*: Max power point current temperature coefficient\n\n*Required*: set to 3 if not provided."),
 	NULL},
{"snl_aisc", (getter)SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_aisc,(setter)SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_aisc,
	PyDoc_STR("*float*: Short circuit current temperature coefficient\n\n*Required*: set to 3 if not provided."),
 	NULL},
{"snl_area", (getter)SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_area,(setter)SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_area,
	PyDoc_STR("*float*: Module area\n\n*Required*: set to 3 if not provided."),
 	NULL},
{"snl_b", (getter)SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_b,(setter)SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_b,
	PyDoc_STR("*float*: Temperature coefficient b\n\n*Required*: set to 3 if not provided."),
 	NULL},
{"snl_b0", (getter)SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_b0,(setter)SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_b0,
	PyDoc_STR("*float*: Incidence angle modifier polynomial coeff 0\n\n*Required*: set to 3 if not provided."),
 	NULL},
{"snl_b1", (getter)SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_b1,(setter)SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_b1,
	PyDoc_STR("*float*: Incidence angle modifier polynomial coeff 1\n\n*Required*: set to 3 if not provided."),
 	NULL},
{"snl_b2", (getter)SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_b2,(setter)SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_b2,
	PyDoc_STR("*float*: Incidence angle modifier polynomial coeff 2\n\n*Required*: set to 3 if not provided."),
 	NULL},
{"snl_b3", (getter)SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_b3,(setter)SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_b3,
	PyDoc_STR("*float*: Incidence angle modifier polynomial coeff 3\n\n*Required*: set to 3 if not provided."),
 	NULL},
{"snl_b4", (getter)SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_b4,(setter)SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_b4,
	PyDoc_STR("*float*: Incidence angle modifier polynomial coeff 4\n\n*Required*: set to 3 if not provided."),
 	NULL},
{"snl_b5", (getter)SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_b5,(setter)SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_b5,
	PyDoc_STR("*float*: Incidence angle modifier polynomial coeff 5\n\n*Required*: set to 3 if not provided."),
 	NULL},
{"snl_bvmpo", (getter)SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_bvmpo,(setter)SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_bvmpo,
	PyDoc_STR("*float*: Max power point voltage temperature coefficient\n\n*Required*: set to 3 if not provided."),
 	NULL},
{"snl_bvoco", (getter)SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_bvoco,(setter)SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_bvoco,
	PyDoc_STR("*float*: Open circuit voltage temperature coefficient\n\n*Required*: set to 3 if not provided."),
 	NULL},
{"snl_c0", (getter)SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_c0,(setter)SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_c0,
	PyDoc_STR("*float*: C0\n\n*Required*: set to 3 if not provided."),
 	NULL},
{"snl_c1", (getter)SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_c1,(setter)SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_c1,
	PyDoc_STR("*float*: C1\n\n*Required*: set to 3 if not provided."),
 	NULL},
{"snl_c2", (getter)SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_c2,(setter)SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_c2,
	PyDoc_STR("*float*: C2\n\n*Required*: set to 3 if not provided."),
 	NULL},
{"snl_c3", (getter)SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_c3,(setter)SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_c3,
	PyDoc_STR("*float*: C3\n\n*Required*: set to 3 if not provided."),
 	NULL},
{"snl_c4", (getter)SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_c4,(setter)SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_c4,
	PyDoc_STR("*float*: C4\n\n*Required*: set to 3 if not provided."),
 	NULL},
{"snl_c5", (getter)SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_c5,(setter)SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_c5,
	PyDoc_STR("*float*: C5\n\n*Required*: set to 3 if not provided."),
 	NULL},
{"snl_c6", (getter)SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_c6,(setter)SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_c6,
	PyDoc_STR("*float*: C6\n\n*Required*: set to 3 if not provided."),
 	NULL},
{"snl_c7", (getter)SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_c7,(setter)SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_c7,
	PyDoc_STR("*float*: C7\n\n*Required*: set to 3 if not provided."),
 	NULL},
{"snl_dtc", (getter)SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_dtc,(setter)SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_dtc,
	PyDoc_STR("*float*: Temperature coefficient dT\n\n*Required*: set to 3 if not provided."),
 	NULL},
{"snl_fd", (getter)SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_fd,(setter)SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_fd,
	PyDoc_STR("*float*: Diffuse fraction\n\n*Required*: set to 3 if not provided."),
 	NULL},
{"snl_impo", (getter)SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_impo,(setter)SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_impo,
	PyDoc_STR("*float*: Max power point current\n\n*Required*: set to 3 if not provided."),
 	NULL},
{"snl_isco", (getter)SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_isco,(setter)SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_isco,
	PyDoc_STR("*float*: Short circuit current\n\n*Required*: set to 3 if not provided."),
 	NULL},
{"snl_ixo", (getter)SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_ixo,(setter)SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_ixo,
	PyDoc_STR("*float*: Ix midpoint current\n\n*Required*: set to 3 if not provided."),
 	NULL},
{"snl_ixxo", (getter)SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_ixxo,(setter)SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_ixxo,
	PyDoc_STR("*float*: Ixx midpoint current\n\n*Required*: set to 3 if not provided."),
 	NULL},
{"snl_mbvmp", (getter)SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_mbvmp,(setter)SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_mbvmp,
	PyDoc_STR("*float*: Irradiance dependence of Vmp temperature coefficient\n\n*Required*: set to 3 if not provided."),
 	NULL},
{"snl_mbvoc", (getter)SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_mbvoc,(setter)SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_mbvoc,
	PyDoc_STR("*float*: Irradiance dependence of Voc temperature coefficient\n\n*Required*: set to 3 if not provided."),
 	NULL},
{"snl_module_structure", (getter)SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_module_structure,(setter)SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_module_structure,
	PyDoc_STR("*float*: Module and mounting structure configuration\n\n*Options*: 0=Use Database Values,1=glass/cell/polymer sheet-open rack,2=glass/cell/glass-open rack,3=polymer/thin film/steel-open rack,4=Insulated back BIPV,5=close roof mount,6=user-defined\n\n*Constraints*: INTEGER,MIN=0,MAX=6\n\n*Required*: set to 3 if not provided."),
 	NULL},
{"snl_n", (getter)SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_n,(setter)SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_n,
	PyDoc_STR("*float*: Diode factor\n\n*Required*: set to 3 if not provided."),
 	NULL},
{"snl_ref_a", (getter)SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_ref_a,(setter)SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_ref_a,
	PyDoc_STR("*float*: User-specified a\n\n*Required*: set to 3 if not provided."),
 	NULL},
{"snl_ref_b", (getter)SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_ref_b,(setter)SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_ref_b,
	PyDoc_STR("*float*: User-specified b\n\n*Required*: set to 3 if not provided."),
 	NULL},
{"snl_ref_dT", (getter)SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_ref_dT,(setter)SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_ref_dT,
	PyDoc_STR("*float*: User-specified dT\n\n*Required*: set to 3 if not provided."),
 	NULL},
{"snl_series_cells", (getter)SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_series_cells,(setter)SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_series_cells,
	PyDoc_STR("*float*: Number of cells in series\n\n*Constraints*: INTEGER\n\n*Required*: set to 3 if not provided."),
 	NULL},
{"snl_vmpo", (getter)SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_vmpo,(setter)SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_vmpo,
	PyDoc_STR("*float*: Max power point voltage\n\n*Required*: set to 3 if not provided."),
 	NULL},
{"snl_voco", (getter)SandiaPVArrayPerformanceModelWithModuleDatabase_get_snl_voco,(setter)SandiaPVArrayPerformanceModelWithModuleDatabase_set_snl_voco,
	PyDoc_STR("*float*: Open circuit voltage\n\n*Required*: set to 3 if not provided."),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject SandiaPVArrayPerformanceModelWithModuleDatabase_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Pvsamv1.SandiaPVArrayPerformanceModelWithModuleDatabase",             /*tp_name*/
		sizeof(SandiaPVArrayPerformanceModelWithModuleDatabaseObject),          /*tp_basicsize*/
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
		SandiaPVArrayPerformanceModelWithModuleDatabase_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		SandiaPVArrayPerformanceModelWithModuleDatabase_getset,          /*tp_getset*/
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
	 * IEC61853SingleDiodeModel Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Pvsamv1   data_ptr;
} IEC61853SingleDiodeModelObject;

static PyTypeObject IEC61853SingleDiodeModel_Type;

static PyObject *
IEC61853SingleDiodeModel_new(SAM_Pvsamv1 data_ptr)
{
	PyObject* new_obj = IEC61853SingleDiodeModel_Type.tp_alloc(&IEC61853SingleDiodeModel_Type,0);

	IEC61853SingleDiodeModelObject* IEC61853SingleDiodeModel_obj = (IEC61853SingleDiodeModelObject*)new_obj;

	IEC61853SingleDiodeModel_obj->data_ptr = data_ptr;

	return new_obj;
}

/* IEC61853SingleDiodeModel methods */

static PyObject *
IEC61853SingleDiodeModel_assign(IEC61853SingleDiodeModelObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Pvsamv1", "IEC61853SingleDiodeModel")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
IEC61853SingleDiodeModel_export(IEC61853SingleDiodeModelObject *self, PyObject *args)
{
	PyTypeObject* tp = &IEC61853SingleDiodeModel_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef IEC61853SingleDiodeModel_methods[] = {
		{"assign",            (PyCFunction)IEC61853SingleDiodeModel_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``IEC61853SingleDiodeModel_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)IEC61853SingleDiodeModel_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
IEC61853SingleDiodeModel_get_sd11par_AMa0(IEC61853SingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_AMa0_nget, self->data_ptr);
}

static int
IEC61853SingleDiodeModel_set_sd11par_AMa0(IEC61853SingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_AMa0_nset, self->data_ptr);
}

static PyObject *
IEC61853SingleDiodeModel_get_sd11par_AMa1(IEC61853SingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_AMa1_nget, self->data_ptr);
}

static int
IEC61853SingleDiodeModel_set_sd11par_AMa1(IEC61853SingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_AMa1_nset, self->data_ptr);
}

static PyObject *
IEC61853SingleDiodeModel_get_sd11par_AMa2(IEC61853SingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_AMa2_nget, self->data_ptr);
}

static int
IEC61853SingleDiodeModel_set_sd11par_AMa2(IEC61853SingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_AMa2_nset, self->data_ptr);
}

static PyObject *
IEC61853SingleDiodeModel_get_sd11par_AMa3(IEC61853SingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_AMa3_nget, self->data_ptr);
}

static int
IEC61853SingleDiodeModel_set_sd11par_AMa3(IEC61853SingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_AMa3_nset, self->data_ptr);
}

static PyObject *
IEC61853SingleDiodeModel_get_sd11par_AMa4(IEC61853SingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_AMa4_nget, self->data_ptr);
}

static int
IEC61853SingleDiodeModel_set_sd11par_AMa4(IEC61853SingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_AMa4_nset, self->data_ptr);
}

static PyObject *
IEC61853SingleDiodeModel_get_sd11par_Egref(IEC61853SingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_Egref_nget, self->data_ptr);
}

static int
IEC61853SingleDiodeModel_set_sd11par_Egref(IEC61853SingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_Egref_nset, self->data_ptr);
}

static PyObject *
IEC61853SingleDiodeModel_get_sd11par_Il(IEC61853SingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_Il_nget, self->data_ptr);
}

static int
IEC61853SingleDiodeModel_set_sd11par_Il(IEC61853SingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_Il_nset, self->data_ptr);
}

static PyObject *
IEC61853SingleDiodeModel_get_sd11par_Imp0(IEC61853SingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_Imp0_nget, self->data_ptr);
}

static int
IEC61853SingleDiodeModel_set_sd11par_Imp0(IEC61853SingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_Imp0_nset, self->data_ptr);
}

static PyObject *
IEC61853SingleDiodeModel_get_sd11par_Io(IEC61853SingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_Io_nget, self->data_ptr);
}

static int
IEC61853SingleDiodeModel_set_sd11par_Io(IEC61853SingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_Io_nset, self->data_ptr);
}

static PyObject *
IEC61853SingleDiodeModel_get_sd11par_Isc0(IEC61853SingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_Isc0_nget, self->data_ptr);
}

static int
IEC61853SingleDiodeModel_set_sd11par_Isc0(IEC61853SingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_Isc0_nset, self->data_ptr);
}

static PyObject *
IEC61853SingleDiodeModel_get_sd11par_Vmp0(IEC61853SingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_Vmp0_nget, self->data_ptr);
}

static int
IEC61853SingleDiodeModel_set_sd11par_Vmp0(IEC61853SingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_Vmp0_nset, self->data_ptr);
}

static PyObject *
IEC61853SingleDiodeModel_get_sd11par_Voc0(IEC61853SingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_Voc0_nget, self->data_ptr);
}

static int
IEC61853SingleDiodeModel_set_sd11par_Voc0(IEC61853SingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_Voc0_nset, self->data_ptr);
}

static PyObject *
IEC61853SingleDiodeModel_get_sd11par_alphaIsc(IEC61853SingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_alphaIsc_nget, self->data_ptr);
}

static int
IEC61853SingleDiodeModel_set_sd11par_alphaIsc(IEC61853SingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_alphaIsc_nset, self->data_ptr);
}

static PyObject *
IEC61853SingleDiodeModel_get_sd11par_area(IEC61853SingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_area_nget, self->data_ptr);
}

static int
IEC61853SingleDiodeModel_set_sd11par_area(IEC61853SingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_area_nset, self->data_ptr);
}

static PyObject *
IEC61853SingleDiodeModel_get_sd11par_c1(IEC61853SingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_c1_nget, self->data_ptr);
}

static int
IEC61853SingleDiodeModel_set_sd11par_c1(IEC61853SingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_c1_nset, self->data_ptr);
}

static PyObject *
IEC61853SingleDiodeModel_get_sd11par_c2(IEC61853SingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_c2_nget, self->data_ptr);
}

static int
IEC61853SingleDiodeModel_set_sd11par_c2(IEC61853SingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_c2_nset, self->data_ptr);
}

static PyObject *
IEC61853SingleDiodeModel_get_sd11par_c3(IEC61853SingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_c3_nget, self->data_ptr);
}

static int
IEC61853SingleDiodeModel_set_sd11par_c3(IEC61853SingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_c3_nset, self->data_ptr);
}

static PyObject *
IEC61853SingleDiodeModel_get_sd11par_d1(IEC61853SingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_d1_nget, self->data_ptr);
}

static int
IEC61853SingleDiodeModel_set_sd11par_d1(IEC61853SingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_d1_nset, self->data_ptr);
}

static PyObject *
IEC61853SingleDiodeModel_get_sd11par_d2(IEC61853SingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_d2_nget, self->data_ptr);
}

static int
IEC61853SingleDiodeModel_set_sd11par_d2(IEC61853SingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_d2_nset, self->data_ptr);
}

static PyObject *
IEC61853SingleDiodeModel_get_sd11par_d3(IEC61853SingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_d3_nget, self->data_ptr);
}

static int
IEC61853SingleDiodeModel_set_sd11par_d3(IEC61853SingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_d3_nset, self->data_ptr);
}

static PyObject *
IEC61853SingleDiodeModel_get_sd11par_glass(IEC61853SingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_glass_nget, self->data_ptr);
}

static int
IEC61853SingleDiodeModel_set_sd11par_glass(IEC61853SingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_glass_nset, self->data_ptr);
}

static PyObject *
IEC61853SingleDiodeModel_get_sd11par_mounting(IEC61853SingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_mounting_nget, self->data_ptr);
}

static int
IEC61853SingleDiodeModel_set_sd11par_mounting(IEC61853SingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_mounting_nset, self->data_ptr);
}

static PyObject *
IEC61853SingleDiodeModel_get_sd11par_n(IEC61853SingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_n_nget, self->data_ptr);
}

static int
IEC61853SingleDiodeModel_set_sd11par_n(IEC61853SingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_n_nset, self->data_ptr);
}

static PyObject *
IEC61853SingleDiodeModel_get_sd11par_nser(IEC61853SingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_nser_nget, self->data_ptr);
}

static int
IEC61853SingleDiodeModel_set_sd11par_nser(IEC61853SingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_nser_nset, self->data_ptr);
}

static PyObject *
IEC61853SingleDiodeModel_get_sd11par_standoff(IEC61853SingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_standoff_nget, self->data_ptr);
}

static int
IEC61853SingleDiodeModel_set_sd11par_standoff(IEC61853SingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_standoff_nset, self->data_ptr);
}

static PyObject *
IEC61853SingleDiodeModel_get_sd11par_tnoct(IEC61853SingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_tnoct_nget, self->data_ptr);
}

static int
IEC61853SingleDiodeModel_set_sd11par_tnoct(IEC61853SingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_tnoct_nset, self->data_ptr);
}

static PyGetSetDef IEC61853SingleDiodeModel_getset[] = {
{"sd11par_AMa0", (getter)IEC61853SingleDiodeModel_get_sd11par_AMa0,(setter)IEC61853SingleDiodeModel_set_sd11par_AMa0,
	PyDoc_STR("*float*: Air mass modifier coeff 0\n\n*Required*: set to 4 if not provided."),
 	NULL},
{"sd11par_AMa1", (getter)IEC61853SingleDiodeModel_get_sd11par_AMa1,(setter)IEC61853SingleDiodeModel_set_sd11par_AMa1,
	PyDoc_STR("*float*: Air mass modifier coeff 1\n\n*Required*: set to 4 if not provided."),
 	NULL},
{"sd11par_AMa2", (getter)IEC61853SingleDiodeModel_get_sd11par_AMa2,(setter)IEC61853SingleDiodeModel_set_sd11par_AMa2,
	PyDoc_STR("*float*: Air mass modifier coeff 2\n\n*Required*: set to 4 if not provided."),
 	NULL},
{"sd11par_AMa3", (getter)IEC61853SingleDiodeModel_get_sd11par_AMa3,(setter)IEC61853SingleDiodeModel_set_sd11par_AMa3,
	PyDoc_STR("*float*: Air mass modifier coeff 3\n\n*Required*: set to 4 if not provided."),
 	NULL},
{"sd11par_AMa4", (getter)IEC61853SingleDiodeModel_get_sd11par_AMa4,(setter)IEC61853SingleDiodeModel_set_sd11par_AMa4,
	PyDoc_STR("*float*: Air mass modifier coeff 4\n\n*Required*: set to 4 if not provided."),
 	NULL},
{"sd11par_Egref", (getter)IEC61853SingleDiodeModel_get_sd11par_Egref,(setter)IEC61853SingleDiodeModel_set_sd11par_Egref,
	PyDoc_STR("*float*: Bandgap voltage [eV]\n\n*Required*: set to 4 if not provided."),
 	NULL},
{"sd11par_Il", (getter)IEC61853SingleDiodeModel_get_sd11par_Il,(setter)IEC61853SingleDiodeModel_set_sd11par_Il,
	PyDoc_STR("*float*: Light current [A]\n\n*Required*: set to 4 if not provided."),
 	NULL},
{"sd11par_Imp0", (getter)IEC61853SingleDiodeModel_get_sd11par_Imp0,(setter)IEC61853SingleDiodeModel_set_sd11par_Imp0,
	PyDoc_STR("*float*: Imp (STC) [A]\n\n*Required*: set to 4 if not provided."),
 	NULL},
{"sd11par_Io", (getter)IEC61853SingleDiodeModel_get_sd11par_Io,(setter)IEC61853SingleDiodeModel_set_sd11par_Io,
	PyDoc_STR("*float*: Saturation current [A]\n\n*Required*: set to 4 if not provided."),
 	NULL},
{"sd11par_Isc0", (getter)IEC61853SingleDiodeModel_get_sd11par_Isc0,(setter)IEC61853SingleDiodeModel_set_sd11par_Isc0,
	PyDoc_STR("*float*: Isc (STC) [A]\n\n*Required*: set to 4 if not provided."),
 	NULL},
{"sd11par_Vmp0", (getter)IEC61853SingleDiodeModel_get_sd11par_Vmp0,(setter)IEC61853SingleDiodeModel_set_sd11par_Vmp0,
	PyDoc_STR("*float*: Vmp (STC) [V]\n\n*Required*: set to 4 if not provided."),
 	NULL},
{"sd11par_Voc0", (getter)IEC61853SingleDiodeModel_get_sd11par_Voc0,(setter)IEC61853SingleDiodeModel_set_sd11par_Voc0,
	PyDoc_STR("*float*: Voc (STC) [V]\n\n*Required*: set to 4 if not provided."),
 	NULL},
{"sd11par_alphaIsc", (getter)IEC61853SingleDiodeModel_get_sd11par_alphaIsc,(setter)IEC61853SingleDiodeModel_set_sd11par_alphaIsc,
	PyDoc_STR("*float*: Short curcuit current temperature coefficient [A/C]\n\n*Required*: set to 4 if not provided."),
 	NULL},
{"sd11par_area", (getter)IEC61853SingleDiodeModel_get_sd11par_area,(setter)IEC61853SingleDiodeModel_set_sd11par_area,
	PyDoc_STR("*float*: Module area [m2]\n\n*Required*: set to 4 if not provided."),
 	NULL},
{"sd11par_c1", (getter)IEC61853SingleDiodeModel_get_sd11par_c1,(setter)IEC61853SingleDiodeModel_set_sd11par_c1,
	PyDoc_STR("*float*: Rsh fit parameter 1\n\n*Required*: set to 4 if not provided."),
 	NULL},
{"sd11par_c2", (getter)IEC61853SingleDiodeModel_get_sd11par_c2,(setter)IEC61853SingleDiodeModel_set_sd11par_c2,
	PyDoc_STR("*float*: Rsh fit parameter 2\n\n*Required*: set to 4 if not provided."),
 	NULL},
{"sd11par_c3", (getter)IEC61853SingleDiodeModel_get_sd11par_c3,(setter)IEC61853SingleDiodeModel_set_sd11par_c3,
	PyDoc_STR("*float*: Rsh fit parameter 3\n\n*Required*: set to 4 if not provided."),
 	NULL},
{"sd11par_d1", (getter)IEC61853SingleDiodeModel_get_sd11par_d1,(setter)IEC61853SingleDiodeModel_set_sd11par_d1,
	PyDoc_STR("*float*: Rs fit parameter 1\n\n*Required*: set to 4 if not provided."),
 	NULL},
{"sd11par_d2", (getter)IEC61853SingleDiodeModel_get_sd11par_d2,(setter)IEC61853SingleDiodeModel_set_sd11par_d2,
	PyDoc_STR("*float*: Rs fit parameter 2\n\n*Required*: set to 4 if not provided."),
 	NULL},
{"sd11par_d3", (getter)IEC61853SingleDiodeModel_get_sd11par_d3,(setter)IEC61853SingleDiodeModel_set_sd11par_d3,
	PyDoc_STR("*float*: Rs fit parameter 3\n\n*Required*: set to 4 if not provided."),
 	NULL},
{"sd11par_glass", (getter)IEC61853SingleDiodeModel_get_sd11par_glass,(setter)IEC61853SingleDiodeModel_set_sd11par_glass,
	PyDoc_STR("*float*: Module cover glass type\n\n*Options*: 0=normal,1=AR glass\n\n*Required*: set to 4 if not provided."),
 	NULL},
{"sd11par_mounting", (getter)IEC61853SingleDiodeModel_get_sd11par_mounting,(setter)IEC61853SingleDiodeModel_set_sd11par_mounting,
	PyDoc_STR("*float*: Array mounting height\n\n*Options*: 0=one story,1=two story\n\n*Constraints*: INTEGER,MIN=0,MAX=1\n\n*Required*: set to 4 if not provided."),
 	NULL},
{"sd11par_n", (getter)IEC61853SingleDiodeModel_get_sd11par_n,(setter)IEC61853SingleDiodeModel_set_sd11par_n,
	PyDoc_STR("*float*: Diode nonideality factor\n\n*Required*: set to 4 if not provided."),
 	NULL},
{"sd11par_nser", (getter)IEC61853SingleDiodeModel_get_sd11par_nser,(setter)IEC61853SingleDiodeModel_set_sd11par_nser,
	PyDoc_STR("*float*: Nseries\n\n*Constraints*: INTEGER,POSITIVE\n\n*Required*: set to 4 if not provided."),
 	NULL},
{"sd11par_standoff", (getter)IEC61853SingleDiodeModel_get_sd11par_standoff,(setter)IEC61853SingleDiodeModel_set_sd11par_standoff,
	PyDoc_STR("*float*: Standoff mode\n\n*Options*: 0=bipv,1=>3.5in,2=2.5-3.5in,3=1.5-2.5in,4=0.5-1.5in,5=<0.5in,6=ground/rack\n\n*Constraints*: INTEGER,MIN=0,MAX=6\n\n*Required*: set to 4 if not provided."),
 	NULL},
{"sd11par_tnoct", (getter)IEC61853SingleDiodeModel_get_sd11par_tnoct,(setter)IEC61853SingleDiodeModel_set_sd11par_tnoct,
	PyDoc_STR("*float*: Nominal operating cell temperature [C]\n\n*Required*: set to 4 if not provided."),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject IEC61853SingleDiodeModel_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Pvsamv1.IEC61853SingleDiodeModel",             /*tp_name*/
		sizeof(IEC61853SingleDiodeModelObject),          /*tp_basicsize*/
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
		IEC61853SingleDiodeModel_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		IEC61853SingleDiodeModel_getset,          /*tp_getset*/
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
	 * MermoudLejeuneSingleDiodeModel Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Pvsamv1   data_ptr;
} MermoudLejeuneSingleDiodeModelObject;

static PyTypeObject MermoudLejeuneSingleDiodeModel_Type;

static PyObject *
MermoudLejeuneSingleDiodeModel_new(SAM_Pvsamv1 data_ptr)
{
	PyObject* new_obj = MermoudLejeuneSingleDiodeModel_Type.tp_alloc(&MermoudLejeuneSingleDiodeModel_Type,0);

	MermoudLejeuneSingleDiodeModelObject* MermoudLejeuneSingleDiodeModel_obj = (MermoudLejeuneSingleDiodeModelObject*)new_obj;

	MermoudLejeuneSingleDiodeModel_obj->data_ptr = data_ptr;

	return new_obj;
}

/* MermoudLejeuneSingleDiodeModel methods */

static PyObject *
MermoudLejeuneSingleDiodeModel_assign(MermoudLejeuneSingleDiodeModelObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Pvsamv1", "MermoudLejeuneSingleDiodeModel")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
MermoudLejeuneSingleDiodeModel_export(MermoudLejeuneSingleDiodeModelObject *self, PyObject *args)
{
	PyTypeObject* tp = &MermoudLejeuneSingleDiodeModel_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef MermoudLejeuneSingleDiodeModel_methods[] = {
		{"assign",            (PyCFunction)MermoudLejeuneSingleDiodeModel_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``MermoudLejeuneSingleDiodeModel_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)MermoudLejeuneSingleDiodeModel_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
MermoudLejeuneSingleDiodeModel_get_mlm_AM_c_lp0(MermoudLejeuneSingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_c_lp0_nget, self->data_ptr);
}

static int
MermoudLejeuneSingleDiodeModel_set_mlm_AM_c_lp0(MermoudLejeuneSingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_c_lp0_nset, self->data_ptr);
}

static PyObject *
MermoudLejeuneSingleDiodeModel_get_mlm_AM_c_lp1(MermoudLejeuneSingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_c_lp1_nget, self->data_ptr);
}

static int
MermoudLejeuneSingleDiodeModel_set_mlm_AM_c_lp1(MermoudLejeuneSingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_c_lp1_nset, self->data_ptr);
}

static PyObject *
MermoudLejeuneSingleDiodeModel_get_mlm_AM_c_lp2(MermoudLejeuneSingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_c_lp2_nget, self->data_ptr);
}

static int
MermoudLejeuneSingleDiodeModel_set_mlm_AM_c_lp2(MermoudLejeuneSingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_c_lp2_nset, self->data_ptr);
}

static PyObject *
MermoudLejeuneSingleDiodeModel_get_mlm_AM_c_lp3(MermoudLejeuneSingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_c_lp3_nget, self->data_ptr);
}

static int
MermoudLejeuneSingleDiodeModel_set_mlm_AM_c_lp3(MermoudLejeuneSingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_c_lp3_nset, self->data_ptr);
}

static PyObject *
MermoudLejeuneSingleDiodeModel_get_mlm_AM_c_lp4(MermoudLejeuneSingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_c_lp4_nget, self->data_ptr);
}

static int
MermoudLejeuneSingleDiodeModel_set_mlm_AM_c_lp4(MermoudLejeuneSingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_c_lp4_nset, self->data_ptr);
}

static PyObject *
MermoudLejeuneSingleDiodeModel_get_mlm_AM_c_lp5(MermoudLejeuneSingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_c_lp5_nget, self->data_ptr);
}

static int
MermoudLejeuneSingleDiodeModel_set_mlm_AM_c_lp5(MermoudLejeuneSingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_c_lp5_nset, self->data_ptr);
}

static PyObject *
MermoudLejeuneSingleDiodeModel_get_mlm_AM_c_sa0(MermoudLejeuneSingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_c_sa0_nget, self->data_ptr);
}

static int
MermoudLejeuneSingleDiodeModel_set_mlm_AM_c_sa0(MermoudLejeuneSingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_c_sa0_nset, self->data_ptr);
}

static PyObject *
MermoudLejeuneSingleDiodeModel_get_mlm_AM_c_sa1(MermoudLejeuneSingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_c_sa1_nget, self->data_ptr);
}

static int
MermoudLejeuneSingleDiodeModel_set_mlm_AM_c_sa1(MermoudLejeuneSingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_c_sa1_nset, self->data_ptr);
}

static PyObject *
MermoudLejeuneSingleDiodeModel_get_mlm_AM_c_sa2(MermoudLejeuneSingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_c_sa2_nget, self->data_ptr);
}

static int
MermoudLejeuneSingleDiodeModel_set_mlm_AM_c_sa2(MermoudLejeuneSingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_c_sa2_nset, self->data_ptr);
}

static PyObject *
MermoudLejeuneSingleDiodeModel_get_mlm_AM_c_sa3(MermoudLejeuneSingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_c_sa3_nget, self->data_ptr);
}

static int
MermoudLejeuneSingleDiodeModel_set_mlm_AM_c_sa3(MermoudLejeuneSingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_c_sa3_nset, self->data_ptr);
}

static PyObject *
MermoudLejeuneSingleDiodeModel_get_mlm_AM_c_sa4(MermoudLejeuneSingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_c_sa4_nget, self->data_ptr);
}

static int
MermoudLejeuneSingleDiodeModel_set_mlm_AM_c_sa4(MermoudLejeuneSingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_c_sa4_nset, self->data_ptr);
}

static PyObject *
MermoudLejeuneSingleDiodeModel_get_mlm_AM_mode(MermoudLejeuneSingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_mode_nget, self->data_ptr);
}

static int
MermoudLejeuneSingleDiodeModel_set_mlm_AM_mode(MermoudLejeuneSingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_mode_nset, self->data_ptr);
}

static PyObject *
MermoudLejeuneSingleDiodeModel_get_mlm_D2MuTau(MermoudLejeuneSingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_D2MuTau_nget, self->data_ptr);
}

static int
MermoudLejeuneSingleDiodeModel_set_mlm_D2MuTau(MermoudLejeuneSingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_D2MuTau_nset, self->data_ptr);
}

static PyObject *
MermoudLejeuneSingleDiodeModel_get_mlm_E_g(MermoudLejeuneSingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_E_g_nget, self->data_ptr);
}

static int
MermoudLejeuneSingleDiodeModel_set_mlm_E_g(MermoudLejeuneSingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_E_g_nset, self->data_ptr);
}

static PyObject *
MermoudLejeuneSingleDiodeModel_get_mlm_IAM_c_as(MermoudLejeuneSingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_IAM_c_as_nget, self->data_ptr);
}

static int
MermoudLejeuneSingleDiodeModel_set_mlm_IAM_c_as(MermoudLejeuneSingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_IAM_c_as_nset, self->data_ptr);
}

static PyObject *
MermoudLejeuneSingleDiodeModel_get_mlm_IAM_c_cs_iamValue(MermoudLejeuneSingleDiodeModelObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_IAM_c_cs_iamValue_aget, self->data_ptr);
}

static int
MermoudLejeuneSingleDiodeModel_set_mlm_IAM_c_cs_iamValue(MermoudLejeuneSingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_IAM_c_cs_iamValue_aset, self->data_ptr);
}

static PyObject *
MermoudLejeuneSingleDiodeModel_get_mlm_IAM_c_cs_incAngle(MermoudLejeuneSingleDiodeModelObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_IAM_c_cs_incAngle_aget, self->data_ptr);
}

static int
MermoudLejeuneSingleDiodeModel_set_mlm_IAM_c_cs_incAngle(MermoudLejeuneSingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_IAM_c_cs_incAngle_aset, self->data_ptr);
}

static PyObject *
MermoudLejeuneSingleDiodeModel_get_mlm_IAM_c_sa0(MermoudLejeuneSingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_IAM_c_sa0_nget, self->data_ptr);
}

static int
MermoudLejeuneSingleDiodeModel_set_mlm_IAM_c_sa0(MermoudLejeuneSingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_IAM_c_sa0_nset, self->data_ptr);
}

static PyObject *
MermoudLejeuneSingleDiodeModel_get_mlm_IAM_c_sa1(MermoudLejeuneSingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_IAM_c_sa1_nget, self->data_ptr);
}

static int
MermoudLejeuneSingleDiodeModel_set_mlm_IAM_c_sa1(MermoudLejeuneSingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_IAM_c_sa1_nset, self->data_ptr);
}

static PyObject *
MermoudLejeuneSingleDiodeModel_get_mlm_IAM_c_sa2(MermoudLejeuneSingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_IAM_c_sa2_nget, self->data_ptr);
}

static int
MermoudLejeuneSingleDiodeModel_set_mlm_IAM_c_sa2(MermoudLejeuneSingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_IAM_c_sa2_nset, self->data_ptr);
}

static PyObject *
MermoudLejeuneSingleDiodeModel_get_mlm_IAM_c_sa3(MermoudLejeuneSingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_IAM_c_sa3_nget, self->data_ptr);
}

static int
MermoudLejeuneSingleDiodeModel_set_mlm_IAM_c_sa3(MermoudLejeuneSingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_IAM_c_sa3_nset, self->data_ptr);
}

static PyObject *
MermoudLejeuneSingleDiodeModel_get_mlm_IAM_c_sa4(MermoudLejeuneSingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_IAM_c_sa4_nget, self->data_ptr);
}

static int
MermoudLejeuneSingleDiodeModel_set_mlm_IAM_c_sa4(MermoudLejeuneSingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_IAM_c_sa4_nset, self->data_ptr);
}

static PyObject *
MermoudLejeuneSingleDiodeModel_get_mlm_IAM_c_sa5(MermoudLejeuneSingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_IAM_c_sa5_nget, self->data_ptr);
}

static int
MermoudLejeuneSingleDiodeModel_set_mlm_IAM_c_sa5(MermoudLejeuneSingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_IAM_c_sa5_nset, self->data_ptr);
}

static PyObject *
MermoudLejeuneSingleDiodeModel_get_mlm_IAM_mode(MermoudLejeuneSingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_IAM_mode_nget, self->data_ptr);
}

static int
MermoudLejeuneSingleDiodeModel_set_mlm_IAM_mode(MermoudLejeuneSingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_IAM_mode_nset, self->data_ptr);
}

static PyObject *
MermoudLejeuneSingleDiodeModel_get_mlm_I_mp_ref(MermoudLejeuneSingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_I_mp_ref_nget, self->data_ptr);
}

static int
MermoudLejeuneSingleDiodeModel_set_mlm_I_mp_ref(MermoudLejeuneSingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_I_mp_ref_nset, self->data_ptr);
}

static PyObject *
MermoudLejeuneSingleDiodeModel_get_mlm_I_sc_ref(MermoudLejeuneSingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_I_sc_ref_nget, self->data_ptr);
}

static int
MermoudLejeuneSingleDiodeModel_set_mlm_I_sc_ref(MermoudLejeuneSingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_I_sc_ref_nset, self->data_ptr);
}

static PyObject *
MermoudLejeuneSingleDiodeModel_get_mlm_Length(MermoudLejeuneSingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_Length_nget, self->data_ptr);
}

static int
MermoudLejeuneSingleDiodeModel_set_mlm_Length(MermoudLejeuneSingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_Length_nset, self->data_ptr);
}

static PyObject *
MermoudLejeuneSingleDiodeModel_get_mlm_N_diodes(MermoudLejeuneSingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_N_diodes_nget, self->data_ptr);
}

static int
MermoudLejeuneSingleDiodeModel_set_mlm_N_diodes(MermoudLejeuneSingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_N_diodes_nset, self->data_ptr);
}

static PyObject *
MermoudLejeuneSingleDiodeModel_get_mlm_N_parallel(MermoudLejeuneSingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_N_parallel_nget, self->data_ptr);
}

static int
MermoudLejeuneSingleDiodeModel_set_mlm_N_parallel(MermoudLejeuneSingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_N_parallel_nset, self->data_ptr);
}

static PyObject *
MermoudLejeuneSingleDiodeModel_get_mlm_N_series(MermoudLejeuneSingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_N_series_nget, self->data_ptr);
}

static int
MermoudLejeuneSingleDiodeModel_set_mlm_N_series(MermoudLejeuneSingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_N_series_nset, self->data_ptr);
}

static PyObject *
MermoudLejeuneSingleDiodeModel_get_mlm_R_s(MermoudLejeuneSingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_R_s_nget, self->data_ptr);
}

static int
MermoudLejeuneSingleDiodeModel_set_mlm_R_s(MermoudLejeuneSingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_R_s_nset, self->data_ptr);
}

static PyObject *
MermoudLejeuneSingleDiodeModel_get_mlm_R_sh0(MermoudLejeuneSingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_R_sh0_nget, self->data_ptr);
}

static int
MermoudLejeuneSingleDiodeModel_set_mlm_R_sh0(MermoudLejeuneSingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_R_sh0_nset, self->data_ptr);
}

static PyObject *
MermoudLejeuneSingleDiodeModel_get_mlm_R_shexp(MermoudLejeuneSingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_R_shexp_nget, self->data_ptr);
}

static int
MermoudLejeuneSingleDiodeModel_set_mlm_R_shexp(MermoudLejeuneSingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_R_shexp_nset, self->data_ptr);
}

static PyObject *
MermoudLejeuneSingleDiodeModel_get_mlm_R_shref(MermoudLejeuneSingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_R_shref_nget, self->data_ptr);
}

static int
MermoudLejeuneSingleDiodeModel_set_mlm_R_shref(MermoudLejeuneSingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_R_shref_nset, self->data_ptr);
}

static PyObject *
MermoudLejeuneSingleDiodeModel_get_mlm_S_ref(MermoudLejeuneSingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_S_ref_nget, self->data_ptr);
}

static int
MermoudLejeuneSingleDiodeModel_set_mlm_S_ref(MermoudLejeuneSingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_S_ref_nset, self->data_ptr);
}

static PyObject *
MermoudLejeuneSingleDiodeModel_get_mlm_T_c_fa_U0(MermoudLejeuneSingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_T_c_fa_U0_nget, self->data_ptr);
}

static int
MermoudLejeuneSingleDiodeModel_set_mlm_T_c_fa_U0(MermoudLejeuneSingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_T_c_fa_U0_nset, self->data_ptr);
}

static PyObject *
MermoudLejeuneSingleDiodeModel_get_mlm_T_c_fa_U1(MermoudLejeuneSingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_T_c_fa_U1_nget, self->data_ptr);
}

static int
MermoudLejeuneSingleDiodeModel_set_mlm_T_c_fa_U1(MermoudLejeuneSingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_T_c_fa_U1_nset, self->data_ptr);
}

static PyObject *
MermoudLejeuneSingleDiodeModel_get_mlm_T_c_fa_alpha(MermoudLejeuneSingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_T_c_fa_alpha_nget, self->data_ptr);
}

static int
MermoudLejeuneSingleDiodeModel_set_mlm_T_c_fa_alpha(MermoudLejeuneSingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_T_c_fa_alpha_nset, self->data_ptr);
}

static PyObject *
MermoudLejeuneSingleDiodeModel_get_mlm_T_c_no_mounting(MermoudLejeuneSingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_T_c_no_mounting_nget, self->data_ptr);
}

static int
MermoudLejeuneSingleDiodeModel_set_mlm_T_c_no_mounting(MermoudLejeuneSingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_T_c_no_mounting_nset, self->data_ptr);
}

static PyObject *
MermoudLejeuneSingleDiodeModel_get_mlm_T_c_no_standoff(MermoudLejeuneSingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_T_c_no_standoff_nget, self->data_ptr);
}

static int
MermoudLejeuneSingleDiodeModel_set_mlm_T_c_no_standoff(MermoudLejeuneSingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_T_c_no_standoff_nset, self->data_ptr);
}

static PyObject *
MermoudLejeuneSingleDiodeModel_get_mlm_T_c_no_tnoct(MermoudLejeuneSingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_T_c_no_tnoct_nget, self->data_ptr);
}

static int
MermoudLejeuneSingleDiodeModel_set_mlm_T_c_no_tnoct(MermoudLejeuneSingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_T_c_no_tnoct_nset, self->data_ptr);
}

static PyObject *
MermoudLejeuneSingleDiodeModel_get_mlm_T_mode(MermoudLejeuneSingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_T_mode_nget, self->data_ptr);
}

static int
MermoudLejeuneSingleDiodeModel_set_mlm_T_mode(MermoudLejeuneSingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_T_mode_nset, self->data_ptr);
}

static PyObject *
MermoudLejeuneSingleDiodeModel_get_mlm_T_ref(MermoudLejeuneSingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_T_ref_nget, self->data_ptr);
}

static int
MermoudLejeuneSingleDiodeModel_set_mlm_T_ref(MermoudLejeuneSingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_T_ref_nset, self->data_ptr);
}

static PyObject *
MermoudLejeuneSingleDiodeModel_get_mlm_V_mp_ref(MermoudLejeuneSingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_V_mp_ref_nget, self->data_ptr);
}

static int
MermoudLejeuneSingleDiodeModel_set_mlm_V_mp_ref(MermoudLejeuneSingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_V_mp_ref_nset, self->data_ptr);
}

static PyObject *
MermoudLejeuneSingleDiodeModel_get_mlm_V_oc_ref(MermoudLejeuneSingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_V_oc_ref_nget, self->data_ptr);
}

static int
MermoudLejeuneSingleDiodeModel_set_mlm_V_oc_ref(MermoudLejeuneSingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_V_oc_ref_nset, self->data_ptr);
}

static PyObject *
MermoudLejeuneSingleDiodeModel_get_mlm_Width(MermoudLejeuneSingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_Width_nget, self->data_ptr);
}

static int
MermoudLejeuneSingleDiodeModel_set_mlm_Width(MermoudLejeuneSingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_Width_nset, self->data_ptr);
}

static PyObject *
MermoudLejeuneSingleDiodeModel_get_mlm_alpha_isc(MermoudLejeuneSingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_alpha_isc_nget, self->data_ptr);
}

static int
MermoudLejeuneSingleDiodeModel_set_mlm_alpha_isc(MermoudLejeuneSingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_alpha_isc_nset, self->data_ptr);
}

static PyObject *
MermoudLejeuneSingleDiodeModel_get_mlm_beta_voc_spec(MermoudLejeuneSingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_beta_voc_spec_nget, self->data_ptr);
}

static int
MermoudLejeuneSingleDiodeModel_set_mlm_beta_voc_spec(MermoudLejeuneSingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_beta_voc_spec_nset, self->data_ptr);
}

static PyObject *
MermoudLejeuneSingleDiodeModel_get_mlm_groundRelfectionFraction(MermoudLejeuneSingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_groundRelfectionFraction_nget, self->data_ptr);
}

static int
MermoudLejeuneSingleDiodeModel_set_mlm_groundRelfectionFraction(MermoudLejeuneSingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_groundRelfectionFraction_nset, self->data_ptr);
}

static PyObject *
MermoudLejeuneSingleDiodeModel_get_mlm_mu_n(MermoudLejeuneSingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_mu_n_nget, self->data_ptr);
}

static int
MermoudLejeuneSingleDiodeModel_set_mlm_mu_n(MermoudLejeuneSingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_mu_n_nset, self->data_ptr);
}

static PyObject *
MermoudLejeuneSingleDiodeModel_get_mlm_n_0(MermoudLejeuneSingleDiodeModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_n_0_nget, self->data_ptr);
}

static int
MermoudLejeuneSingleDiodeModel_set_mlm_n_0(MermoudLejeuneSingleDiodeModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_n_0_nset, self->data_ptr);
}

static PyGetSetDef MermoudLejeuneSingleDiodeModel_getset[] = {
{"mlm_AM_c_lp0", (getter)MermoudLejeuneSingleDiodeModel_get_mlm_AM_c_lp0,(setter)MermoudLejeuneSingleDiodeModel_set_mlm_AM_c_lp0,
	PyDoc_STR("*float*: Coefficient 0 for Lee/Panchula Air Mass Modifier [-]\n\n*Required*: set to 5 if not provided."),
 	NULL},
{"mlm_AM_c_lp1", (getter)MermoudLejeuneSingleDiodeModel_get_mlm_AM_c_lp1,(setter)MermoudLejeuneSingleDiodeModel_set_mlm_AM_c_lp1,
	PyDoc_STR("*float*: Coefficient 1 for Lee/Panchula Air Mass Modifier [-]\n\n*Required*: set to 5 if not provided."),
 	NULL},
{"mlm_AM_c_lp2", (getter)MermoudLejeuneSingleDiodeModel_get_mlm_AM_c_lp2,(setter)MermoudLejeuneSingleDiodeModel_set_mlm_AM_c_lp2,
	PyDoc_STR("*float*: Coefficient 2 for Lee/Panchula Air Mass Modifier [-]\n\n*Required*: set to 5 if not provided."),
 	NULL},
{"mlm_AM_c_lp3", (getter)MermoudLejeuneSingleDiodeModel_get_mlm_AM_c_lp3,(setter)MermoudLejeuneSingleDiodeModel_set_mlm_AM_c_lp3,
	PyDoc_STR("*float*: Coefficient 3 for Lee/Panchula Air Mass Modifier [-]\n\n*Required*: set to 5 if not provided."),
 	NULL},
{"mlm_AM_c_lp4", (getter)MermoudLejeuneSingleDiodeModel_get_mlm_AM_c_lp4,(setter)MermoudLejeuneSingleDiodeModel_set_mlm_AM_c_lp4,
	PyDoc_STR("*float*: Coefficient 4 for Lee/Panchula Air Mass Modifier [-]\n\n*Required*: set to 5 if not provided."),
 	NULL},
{"mlm_AM_c_lp5", (getter)MermoudLejeuneSingleDiodeModel_get_mlm_AM_c_lp5,(setter)MermoudLejeuneSingleDiodeModel_set_mlm_AM_c_lp5,
	PyDoc_STR("*float*: Coefficient 5 for Lee/Panchula Air Mass Modifier [-]\n\n*Required*: set to 5 if not provided."),
 	NULL},
{"mlm_AM_c_sa0", (getter)MermoudLejeuneSingleDiodeModel_get_mlm_AM_c_sa0,(setter)MermoudLejeuneSingleDiodeModel_set_mlm_AM_c_sa0,
	PyDoc_STR("*float*: Coefficient 0 for Sandia Air Mass Modifier [-]\n\n*Required*: set to 5 if not provided."),
 	NULL},
{"mlm_AM_c_sa1", (getter)MermoudLejeuneSingleDiodeModel_get_mlm_AM_c_sa1,(setter)MermoudLejeuneSingleDiodeModel_set_mlm_AM_c_sa1,
	PyDoc_STR("*float*: Coefficient 1 for Sandia Air Mass Modifier [-]\n\n*Required*: set to 5 if not provided."),
 	NULL},
{"mlm_AM_c_sa2", (getter)MermoudLejeuneSingleDiodeModel_get_mlm_AM_c_sa2,(setter)MermoudLejeuneSingleDiodeModel_set_mlm_AM_c_sa2,
	PyDoc_STR("*float*: Coefficient 2 for Sandia Air Mass Modifier [-]\n\n*Required*: set to 5 if not provided."),
 	NULL},
{"mlm_AM_c_sa3", (getter)MermoudLejeuneSingleDiodeModel_get_mlm_AM_c_sa3,(setter)MermoudLejeuneSingleDiodeModel_set_mlm_AM_c_sa3,
	PyDoc_STR("*float*: Coefficient 3 for Sandia Air Mass Modifier [-]\n\n*Required*: set to 5 if not provided."),
 	NULL},
{"mlm_AM_c_sa4", (getter)MermoudLejeuneSingleDiodeModel_get_mlm_AM_c_sa4,(setter)MermoudLejeuneSingleDiodeModel_set_mlm_AM_c_sa4,
	PyDoc_STR("*float*: Coefficient 4 for Sandia Air Mass Modifier [-]\n\n*Required*: set to 5 if not provided."),
 	NULL},
{"mlm_AM_mode", (getter)MermoudLejeuneSingleDiodeModel_get_mlm_AM_mode,(setter)MermoudLejeuneSingleDiodeModel_set_mlm_AM_mode,
	PyDoc_STR("*float*: Air-mass modifier mode [-]\n\n*Options*: 1: Do not consider AM effects, 2: Use Sandia polynomial [corr=f(AM)], 3: Use standard coefficients from DeSoto model [corr=f(AM)], 4: Use First Solar polynomial [corr=f(AM, p_wat)]\n\n*Required*: set to 5 if not provided."),
 	NULL},
{"mlm_D2MuTau", (getter)MermoudLejeuneSingleDiodeModel_get_mlm_D2MuTau,(setter)MermoudLejeuneSingleDiodeModel_set_mlm_D2MuTau,
	PyDoc_STR("*float*: Coefficient for recombination losses [V]\n\n*Required*: set to 5 if not provided."),
 	NULL},
{"mlm_E_g", (getter)MermoudLejeuneSingleDiodeModel_get_mlm_E_g,(setter)MermoudLejeuneSingleDiodeModel_set_mlm_E_g,
	PyDoc_STR("*float*: Reference bandgap energy [eV]\n\n*Required*: set to 5 if not provided."),
 	NULL},
{"mlm_IAM_c_as", (getter)MermoudLejeuneSingleDiodeModel_get_mlm_IAM_c_as,(setter)MermoudLejeuneSingleDiodeModel_set_mlm_IAM_c_as,
	PyDoc_STR("*float*: ASHRAE incidence modifier coefficient b_0 [-]\n\n*Required*: set to 5 if not provided."),
 	NULL},
{"mlm_IAM_c_cs_iamValue", (getter)MermoudLejeuneSingleDiodeModel_get_mlm_IAM_c_cs_iamValue,(setter)MermoudLejeuneSingleDiodeModel_set_mlm_IAM_c_cs_iamValue,
	PyDoc_STR("*sequence*: Spline IAM - IAM values [-]\n\n*Required*: set to 5 if not provided."),
 	NULL},
{"mlm_IAM_c_cs_incAngle", (getter)MermoudLejeuneSingleDiodeModel_get_mlm_IAM_c_cs_incAngle,(setter)MermoudLejeuneSingleDiodeModel_set_mlm_IAM_c_cs_incAngle,
	PyDoc_STR("*sequence*: Spline IAM - Incidence angles [deg]\n\n*Required*: set to 5 if not provided."),
 	NULL},
{"mlm_IAM_c_sa0", (getter)MermoudLejeuneSingleDiodeModel_get_mlm_IAM_c_sa0,(setter)MermoudLejeuneSingleDiodeModel_set_mlm_IAM_c_sa0,
	PyDoc_STR("*float*: Sandia IAM coefficient 0 [-]\n\n*Required*: set to 5 if not provided."),
 	NULL},
{"mlm_IAM_c_sa1", (getter)MermoudLejeuneSingleDiodeModel_get_mlm_IAM_c_sa1,(setter)MermoudLejeuneSingleDiodeModel_set_mlm_IAM_c_sa1,
	PyDoc_STR("*float*: Sandia IAM coefficient 1 [-]\n\n*Required*: set to 5 if not provided."),
 	NULL},
{"mlm_IAM_c_sa2", (getter)MermoudLejeuneSingleDiodeModel_get_mlm_IAM_c_sa2,(setter)MermoudLejeuneSingleDiodeModel_set_mlm_IAM_c_sa2,
	PyDoc_STR("*float*: Sandia IAM coefficient 2 [-]\n\n*Required*: set to 5 if not provided."),
 	NULL},
{"mlm_IAM_c_sa3", (getter)MermoudLejeuneSingleDiodeModel_get_mlm_IAM_c_sa3,(setter)MermoudLejeuneSingleDiodeModel_set_mlm_IAM_c_sa3,
	PyDoc_STR("*float*: Sandia IAM coefficient 3 [-]\n\n*Required*: set to 5 if not provided."),
 	NULL},
{"mlm_IAM_c_sa4", (getter)MermoudLejeuneSingleDiodeModel_get_mlm_IAM_c_sa4,(setter)MermoudLejeuneSingleDiodeModel_set_mlm_IAM_c_sa4,
	PyDoc_STR("*float*: Sandia IAM coefficient 4 [-]\n\n*Required*: set to 5 if not provided."),
 	NULL},
{"mlm_IAM_c_sa5", (getter)MermoudLejeuneSingleDiodeModel_get_mlm_IAM_c_sa5,(setter)MermoudLejeuneSingleDiodeModel_set_mlm_IAM_c_sa5,
	PyDoc_STR("*float*: Sandia IAM coefficient 5 [-]\n\n*Required*: set to 5 if not provided."),
 	NULL},
{"mlm_IAM_mode", (getter)MermoudLejeuneSingleDiodeModel_get_mlm_IAM_mode,(setter)MermoudLejeuneSingleDiodeModel_set_mlm_IAM_mode,
	PyDoc_STR("*float*: Incidence Angle Modifier mode [-]\n\n*Info*: 1: Use ASHRAE formula, 2: Use Sandia polynomial, 3: Use cubic spline with user-supplied data\n\n*Required*: set to 5 if not provided."),
 	NULL},
{"mlm_I_mp_ref", (getter)MermoudLejeuneSingleDiodeModel_get_mlm_I_mp_ref,(setter)MermoudLejeuneSingleDiodeModel_set_mlm_I_mp_ref,
	PyDoc_STR("*float*: I_mp at STC [A]\n\n*Required*: set to 5 if not provided."),
 	NULL},
{"mlm_I_sc_ref", (getter)MermoudLejeuneSingleDiodeModel_get_mlm_I_sc_ref,(setter)MermoudLejeuneSingleDiodeModel_set_mlm_I_sc_ref,
	PyDoc_STR("*float*: I_sc at STC [A]\n\n*Required*: set to 5 if not provided."),
 	NULL},
{"mlm_Length", (getter)MermoudLejeuneSingleDiodeModel_get_mlm_Length,(setter)MermoudLejeuneSingleDiodeModel_set_mlm_Length,
	PyDoc_STR("*float*: Module length (long side) [m]\n\n*Required*: set to 5 if not provided."),
 	NULL},
{"mlm_N_diodes", (getter)MermoudLejeuneSingleDiodeModel_get_mlm_N_diodes,(setter)MermoudLejeuneSingleDiodeModel_set_mlm_N_diodes,
	PyDoc_STR("*float*: Number of diodes [-]\n\n*Required*: set to 5 if not provided."),
 	NULL},
{"mlm_N_parallel", (getter)MermoudLejeuneSingleDiodeModel_get_mlm_N_parallel,(setter)MermoudLejeuneSingleDiodeModel_set_mlm_N_parallel,
	PyDoc_STR("*float*: Number of cells in parallel [-]\n\n*Required*: set to 5 if not provided."),
 	NULL},
{"mlm_N_series", (getter)MermoudLejeuneSingleDiodeModel_get_mlm_N_series,(setter)MermoudLejeuneSingleDiodeModel_set_mlm_N_series,
	PyDoc_STR("*float*: Number of cells in series [-]\n\n*Required*: set to 5 if not provided."),
 	NULL},
{"mlm_R_s", (getter)MermoudLejeuneSingleDiodeModel_get_mlm_R_s,(setter)MermoudLejeuneSingleDiodeModel_set_mlm_R_s,
	PyDoc_STR("*float*: Series resistance [V/A]\n\n*Required*: set to 5 if not provided."),
 	NULL},
{"mlm_R_sh0", (getter)MermoudLejeuneSingleDiodeModel_get_mlm_R_sh0,(setter)MermoudLejeuneSingleDiodeModel_set_mlm_R_sh0,
	PyDoc_STR("*float*: Rsh,0 [V/A]\n\n*Required*: set to 5 if not provided."),
 	NULL},
{"mlm_R_shexp", (getter)MermoudLejeuneSingleDiodeModel_get_mlm_R_shexp,(setter)MermoudLejeuneSingleDiodeModel_set_mlm_R_shexp,
	PyDoc_STR("*float*: Rsh exponential coefficient [-]\n\n*Required*: set to 5 if not provided."),
 	NULL},
{"mlm_R_shref", (getter)MermoudLejeuneSingleDiodeModel_get_mlm_R_shref,(setter)MermoudLejeuneSingleDiodeModel_set_mlm_R_shref,
	PyDoc_STR("*float*: Reference shunt resistance [V/A]\n\n*Required*: set to 5 if not provided."),
 	NULL},
{"mlm_S_ref", (getter)MermoudLejeuneSingleDiodeModel_get_mlm_S_ref,(setter)MermoudLejeuneSingleDiodeModel_set_mlm_S_ref,
	PyDoc_STR("*float*: Reference irradiance (Typically 1000W/m²) [W/m²]\n\n*Required*: set to 5 if not provided."),
 	NULL},
{"mlm_T_c_fa_U0", (getter)MermoudLejeuneSingleDiodeModel_get_mlm_T_c_fa_U0,(setter)MermoudLejeuneSingleDiodeModel_set_mlm_T_c_fa_U0,
	PyDoc_STR("*float*: Extended Faiman model U_0 [W/m²K]\n\n*Required*: set to 5 if not provided."),
 	NULL},
{"mlm_T_c_fa_U1", (getter)MermoudLejeuneSingleDiodeModel_get_mlm_T_c_fa_U1,(setter)MermoudLejeuneSingleDiodeModel_set_mlm_T_c_fa_U1,
	PyDoc_STR("*float*: Extended Faiman model U_1 [W/m³sK]\n\n*Required*: set to 5 if not provided."),
 	NULL},
{"mlm_T_c_fa_alpha", (getter)MermoudLejeuneSingleDiodeModel_get_mlm_T_c_fa_alpha,(setter)MermoudLejeuneSingleDiodeModel_set_mlm_T_c_fa_alpha,
	PyDoc_STR("*float*: Extended Faiman model absorptivity [-]\n\n*Required*: set to 5 if not provided."),
 	NULL},
{"mlm_T_c_no_mounting", (getter)MermoudLejeuneSingleDiodeModel_get_mlm_T_c_no_mounting,(setter)MermoudLejeuneSingleDiodeModel_set_mlm_T_c_no_mounting,
	PyDoc_STR("*float*: NOCT Array mounting height [-]\n\n*Options*: 0=one story,1=two story\n\n*Required*: set to 5 if not provided."),
 	NULL},
{"mlm_T_c_no_standoff", (getter)MermoudLejeuneSingleDiodeModel_get_mlm_T_c_no_standoff,(setter)MermoudLejeuneSingleDiodeModel_set_mlm_T_c_no_standoff,
	PyDoc_STR("*float*: NOCT standoff mode [-]\n\n*Options*: 0=bipv,1=>3.5in,2=2.5-3.5in,3=1.5-2.5in,4=0.5-1.5in,5=<0.5in,6=ground/rack\n\n*Required*: set to 5 if not provided."),
 	NULL},
{"mlm_T_c_no_tnoct", (getter)MermoudLejeuneSingleDiodeModel_get_mlm_T_c_no_tnoct,(setter)MermoudLejeuneSingleDiodeModel_set_mlm_T_c_no_tnoct,
	PyDoc_STR("*float*: NOCT cell temperature [°C]\n\n*Required*: set to 5 if not provided."),
 	NULL},
{"mlm_T_mode", (getter)MermoudLejeuneSingleDiodeModel_get_mlm_T_mode,(setter)MermoudLejeuneSingleDiodeModel_set_mlm_T_mode,
	PyDoc_STR("*float*: Cell temperature model mode [-]\n\n*Info*: 1: NOCT\n\n*Required*: set to 5 if not provided."),
 	NULL},
{"mlm_T_ref", (getter)MermoudLejeuneSingleDiodeModel_get_mlm_T_ref,(setter)MermoudLejeuneSingleDiodeModel_set_mlm_T_ref,
	PyDoc_STR("*float*: Reference temperature (Typically 25°C) [°C]\n\n*Required*: set to 5 if not provided."),
 	NULL},
{"mlm_V_mp_ref", (getter)MermoudLejeuneSingleDiodeModel_get_mlm_V_mp_ref,(setter)MermoudLejeuneSingleDiodeModel_set_mlm_V_mp_ref,
	PyDoc_STR("*float*: V_mp at STC [V]\n\n*Required*: set to 5 if not provided."),
 	NULL},
{"mlm_V_oc_ref", (getter)MermoudLejeuneSingleDiodeModel_get_mlm_V_oc_ref,(setter)MermoudLejeuneSingleDiodeModel_set_mlm_V_oc_ref,
	PyDoc_STR("*float*: V_oc at STC [V]\n\n*Required*: set to 5 if not provided."),
 	NULL},
{"mlm_Width", (getter)MermoudLejeuneSingleDiodeModel_get_mlm_Width,(setter)MermoudLejeuneSingleDiodeModel_set_mlm_Width,
	PyDoc_STR("*float*: Module width (short side) [m]\n\n*Required*: set to 5 if not provided."),
 	NULL},
{"mlm_alpha_isc", (getter)MermoudLejeuneSingleDiodeModel_get_mlm_alpha_isc,(setter)MermoudLejeuneSingleDiodeModel_set_mlm_alpha_isc,
	PyDoc_STR("*float*: Temperature coefficient for I_sc [A/K]\n\n*Required*: set to 5 if not provided."),
 	NULL},
{"mlm_beta_voc_spec", (getter)MermoudLejeuneSingleDiodeModel_get_mlm_beta_voc_spec,(setter)MermoudLejeuneSingleDiodeModel_set_mlm_beta_voc_spec,
	PyDoc_STR("*float*: Temperature coefficient for V_oc [V/K]\n\n*Required*: set to 5 if not provided."),
 	NULL},
{"mlm_groundRelfectionFraction", (getter)MermoudLejeuneSingleDiodeModel_get_mlm_groundRelfectionFraction,(setter)MermoudLejeuneSingleDiodeModel_set_mlm_groundRelfectionFraction,
	PyDoc_STR("*float*: Ground reflection fraction [-]\n\n*Required*: set to 5 if not provided."),
 	NULL},
{"mlm_mu_n", (getter)MermoudLejeuneSingleDiodeModel_get_mlm_mu_n,(setter)MermoudLejeuneSingleDiodeModel_set_mlm_mu_n,
	PyDoc_STR("*float*: Temperature coefficient of gamma [1/K]\n\n*Required*: set to 5 if not provided."),
 	NULL},
{"mlm_n_0", (getter)MermoudLejeuneSingleDiodeModel_get_mlm_n_0,(setter)MermoudLejeuneSingleDiodeModel_set_mlm_n_0,
	PyDoc_STR("*float*: Gamma [-]\n\n*Required*: set to 5 if not provided."),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject MermoudLejeuneSingleDiodeModel_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Pvsamv1.MermoudLejeuneSingleDiodeModel",             /*tp_name*/
		sizeof(MermoudLejeuneSingleDiodeModelObject),          /*tp_basicsize*/
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
		MermoudLejeuneSingleDiodeModel_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		MermoudLejeuneSingleDiodeModel_getset,          /*tp_getset*/
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
	 * Inverter Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Pvsamv1   data_ptr;
} InverterObject;

static PyTypeObject Inverter_Type;

static PyObject *
Inverter_new(SAM_Pvsamv1 data_ptr)
{
	PyObject* new_obj = Inverter_Type.tp_alloc(&Inverter_Type,0);

	InverterObject* Inverter_obj = (InverterObject*)new_obj;

	Inverter_obj->data_ptr = data_ptr;

	return new_obj;
}

/* Inverter methods */

static PyObject *
Inverter_assign(InverterObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Pvsamv1", "Inverter")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
Inverter_export(InverterObject *self, PyObject *args)
{
	PyTypeObject* tp = &Inverter_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef Inverter_methods[] = {
		{"assign",            (PyCFunction)Inverter_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``Inverter_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)Inverter_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
Inverter_get_inv_cec_cg_eff_cec(InverterObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Inverter_inv_cec_cg_eff_cec_nget, self->data_ptr);
}

static int
Inverter_set_inv_cec_cg_eff_cec(InverterObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Inverter_inv_cec_cg_eff_cec_nset, self->data_ptr);
}

static PyObject *
Inverter_get_inv_cec_cg_paco(InverterObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Inverter_inv_cec_cg_paco_nget, self->data_ptr);
}

static int
Inverter_set_inv_cec_cg_paco(InverterObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Inverter_inv_cec_cg_paco_nset, self->data_ptr);
}

static PyObject *
Inverter_get_inv_ds_eff(InverterObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Inverter_inv_ds_eff_nget, self->data_ptr);
}

static int
Inverter_set_inv_ds_eff(InverterObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Inverter_inv_ds_eff_nset, self->data_ptr);
}

static PyObject *
Inverter_get_inv_ds_paco(InverterObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Inverter_inv_ds_paco_nget, self->data_ptr);
}

static int
Inverter_set_inv_ds_paco(InverterObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Inverter_inv_ds_paco_nset, self->data_ptr);
}

static PyObject *
Inverter_get_inv_num_mppt(InverterObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Inverter_inv_num_mppt_nget, self->data_ptr);
}

static int
Inverter_set_inv_num_mppt(InverterObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Inverter_inv_num_mppt_nset, self->data_ptr);
}

static PyObject *
Inverter_get_inv_pd_eff(InverterObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Inverter_inv_pd_eff_nget, self->data_ptr);
}

static int
Inverter_set_inv_pd_eff(InverterObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Inverter_inv_pd_eff_nset, self->data_ptr);
}

static PyObject *
Inverter_get_inv_pd_paco(InverterObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Inverter_inv_pd_paco_nget, self->data_ptr);
}

static int
Inverter_set_inv_pd_paco(InverterObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Inverter_inv_pd_paco_nset, self->data_ptr);
}

static PyObject *
Inverter_get_inv_snl_eff_cec(InverterObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Inverter_inv_snl_eff_cec_nget, self->data_ptr);
}

static int
Inverter_set_inv_snl_eff_cec(InverterObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Inverter_inv_snl_eff_cec_nset, self->data_ptr);
}

static PyObject *
Inverter_get_inv_snl_paco(InverterObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Inverter_inv_snl_paco_nget, self->data_ptr);
}

static int
Inverter_set_inv_snl_paco(InverterObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Inverter_inv_snl_paco_nset, self->data_ptr);
}

static PyObject *
Inverter_get_inverter_count(InverterObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Inverter_inverter_count_nget, self->data_ptr);
}

static int
Inverter_set_inverter_count(InverterObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Inverter_inverter_count_nset, self->data_ptr);
}

static PyObject *
Inverter_get_inverter_model(InverterObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Inverter_inverter_model_nget, self->data_ptr);
}

static int
Inverter_set_inverter_model(InverterObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Inverter_inverter_model_nset, self->data_ptr);
}

static PyObject *
Inverter_get_mppt_hi_inverter(InverterObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Inverter_mppt_hi_inverter_nget, self->data_ptr);
}

static int
Inverter_set_mppt_hi_inverter(InverterObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Inverter_mppt_hi_inverter_nset, self->data_ptr);
}

static PyObject *
Inverter_get_mppt_low_inverter(InverterObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Inverter_mppt_low_inverter_nget, self->data_ptr);
}

static int
Inverter_set_mppt_low_inverter(InverterObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Inverter_mppt_low_inverter_nset, self->data_ptr);
}

static PyGetSetDef Inverter_getset[] = {
{"inv_cec_cg_eff_cec", (getter)Inverter_get_inv_cec_cg_eff_cec,(setter)Inverter_set_inv_cec_cg_eff_cec,
	PyDoc_STR("*float*: Inverter Coefficient Generator CEC Efficiency [%]"),
 	NULL},
{"inv_cec_cg_paco", (getter)Inverter_get_inv_cec_cg_paco,(setter)Inverter_set_inv_cec_cg_paco,
	PyDoc_STR("*float*: Inverter Coefficient Generator Max AC Power [Wac]"),
 	NULL},
{"inv_ds_eff", (getter)Inverter_get_inv_ds_eff,(setter)Inverter_set_inv_ds_eff,
	PyDoc_STR("*float*: Inverter Datasheet Efficiency [%]"),
 	NULL},
{"inv_ds_paco", (getter)Inverter_get_inv_ds_paco,(setter)Inverter_set_inv_ds_paco,
	PyDoc_STR("*float*: Inverter Datasheet Maximum AC Power [Wac]"),
 	NULL},
{"inv_num_mppt", (getter)Inverter_get_inv_num_mppt,(setter)Inverter_set_inv_num_mppt,
	PyDoc_STR("*float*: Number of MPPT inputs\n\n*Constraints*: INTEGER,MIN=0,MAX=4\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"inv_pd_eff", (getter)Inverter_get_inv_pd_eff,(setter)Inverter_set_inv_pd_eff,
	PyDoc_STR("*float*: Inverter Partload Efficiency [%]"),
 	NULL},
{"inv_pd_paco", (getter)Inverter_get_inv_pd_paco,(setter)Inverter_set_inv_pd_paco,
	PyDoc_STR("*float*: Inverter Partload Maximum AC Power [Wac]"),
 	NULL},
{"inv_snl_eff_cec", (getter)Inverter_get_inv_snl_eff_cec,(setter)Inverter_set_inv_snl_eff_cec,
	PyDoc_STR("*float*: Inverter Sandia CEC Efficiency [%]"),
 	NULL},
{"inv_snl_paco", (getter)Inverter_get_inv_snl_paco,(setter)Inverter_set_inv_snl_paco,
	PyDoc_STR("*float*: Inverter Sandia Maximum AC Power [Wac]"),
 	NULL},
{"inverter_count", (getter)Inverter_get_inverter_count,(setter)Inverter_set_inverter_count,
	PyDoc_STR("*float*: Number of inverters"),
 	NULL},
{"inverter_model", (getter)Inverter_get_inverter_model,(setter)Inverter_set_inverter_model,
	PyDoc_STR("*float*: Inverter model specifier\n\n*Options*: 0=cec,1=datasheet,2=partload,3=coefficientgenerator,4=PVYield\n\n*Constraints*: INTEGER,MIN=0,MAX=4\n\n*Required*: True"),
 	NULL},
{"mppt_hi_inverter", (getter)Inverter_get_mppt_hi_inverter,(setter)Inverter_set_mppt_hi_inverter,
	PyDoc_STR("*float*: Maximum inverter MPPT voltage window [Vdc]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"mppt_low_inverter", (getter)Inverter_get_mppt_low_inverter,(setter)Inverter_set_mppt_low_inverter,
	PyDoc_STR("*float*: Minimum inverter MPPT voltage window [Vdc]\n\n*Required*: set to 0 if not provided."),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject Inverter_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Pvsamv1.Inverter",             /*tp_name*/
		sizeof(InverterObject),          /*tp_basicsize*/
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
		Inverter_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		Inverter_getset,          /*tp_getset*/
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
	 * InverterCECDatabase Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Pvsamv1   data_ptr;
} InverterCECDatabaseObject;

static PyTypeObject InverterCECDatabase_Type;

static PyObject *
InverterCECDatabase_new(SAM_Pvsamv1 data_ptr)
{
	PyObject* new_obj = InverterCECDatabase_Type.tp_alloc(&InverterCECDatabase_Type,0);

	InverterCECDatabaseObject* InverterCECDatabase_obj = (InverterCECDatabaseObject*)new_obj;

	InverterCECDatabase_obj->data_ptr = data_ptr;

	return new_obj;
}

/* InverterCECDatabase methods */

static PyObject *
InverterCECDatabase_assign(InverterCECDatabaseObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Pvsamv1", "InverterCECDatabase")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
InverterCECDatabase_export(InverterCECDatabaseObject *self, PyObject *args)
{
	PyTypeObject* tp = &InverterCECDatabase_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef InverterCECDatabase_methods[] = {
		{"assign",            (PyCFunction)InverterCECDatabase_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``InverterCECDatabase_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)InverterCECDatabase_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
InverterCECDatabase_get_inv_snl_c0(InverterCECDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_InverterCECDatabase_inv_snl_c0_nget, self->data_ptr);
}

static int
InverterCECDatabase_set_inv_snl_c0(InverterCECDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_InverterCECDatabase_inv_snl_c0_nset, self->data_ptr);
}

static PyObject *
InverterCECDatabase_get_inv_snl_c1(InverterCECDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_InverterCECDatabase_inv_snl_c1_nget, self->data_ptr);
}

static int
InverterCECDatabase_set_inv_snl_c1(InverterCECDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_InverterCECDatabase_inv_snl_c1_nset, self->data_ptr);
}

static PyObject *
InverterCECDatabase_get_inv_snl_c2(InverterCECDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_InverterCECDatabase_inv_snl_c2_nget, self->data_ptr);
}

static int
InverterCECDatabase_set_inv_snl_c2(InverterCECDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_InverterCECDatabase_inv_snl_c2_nset, self->data_ptr);
}

static PyObject *
InverterCECDatabase_get_inv_snl_c3(InverterCECDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_InverterCECDatabase_inv_snl_c3_nget, self->data_ptr);
}

static int
InverterCECDatabase_set_inv_snl_c3(InverterCECDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_InverterCECDatabase_inv_snl_c3_nset, self->data_ptr);
}

static PyObject *
InverterCECDatabase_get_inv_snl_paco(InverterCECDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_InverterCECDatabase_inv_snl_paco_nget, self->data_ptr);
}

static int
InverterCECDatabase_set_inv_snl_paco(InverterCECDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_InverterCECDatabase_inv_snl_paco_nset, self->data_ptr);
}

static PyObject *
InverterCECDatabase_get_inv_snl_pdco(InverterCECDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_InverterCECDatabase_inv_snl_pdco_nget, self->data_ptr);
}

static int
InverterCECDatabase_set_inv_snl_pdco(InverterCECDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_InverterCECDatabase_inv_snl_pdco_nset, self->data_ptr);
}

static PyObject *
InverterCECDatabase_get_inv_snl_pnt(InverterCECDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_InverterCECDatabase_inv_snl_pnt_nget, self->data_ptr);
}

static int
InverterCECDatabase_set_inv_snl_pnt(InverterCECDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_InverterCECDatabase_inv_snl_pnt_nset, self->data_ptr);
}

static PyObject *
InverterCECDatabase_get_inv_snl_pso(InverterCECDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_InverterCECDatabase_inv_snl_pso_nget, self->data_ptr);
}

static int
InverterCECDatabase_set_inv_snl_pso(InverterCECDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_InverterCECDatabase_inv_snl_pso_nset, self->data_ptr);
}

static PyObject *
InverterCECDatabase_get_inv_snl_vdcmax(InverterCECDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_InverterCECDatabase_inv_snl_vdcmax_nget, self->data_ptr);
}

static int
InverterCECDatabase_set_inv_snl_vdcmax(InverterCECDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_InverterCECDatabase_inv_snl_vdcmax_nset, self->data_ptr);
}

static PyObject *
InverterCECDatabase_get_inv_snl_vdco(InverterCECDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_InverterCECDatabase_inv_snl_vdco_nget, self->data_ptr);
}

static int
InverterCECDatabase_set_inv_snl_vdco(InverterCECDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_InverterCECDatabase_inv_snl_vdco_nset, self->data_ptr);
}

static PyObject *
InverterCECDatabase_get_inv_tdc_cec_db(InverterCECDatabaseObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Pvsamv1_InverterCECDatabase_inv_tdc_cec_db_mget, self->data_ptr);
}

static int
InverterCECDatabase_set_inv_tdc_cec_db(InverterCECDatabaseObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_Pvsamv1_InverterCECDatabase_inv_tdc_cec_db_mset, self->data_ptr);
}

static PyGetSetDef InverterCECDatabase_getset[] = {
{"inv_snl_c0", (getter)InverterCECDatabase_get_inv_snl_c0,(setter)InverterCECDatabase_set_inv_snl_c0,
	PyDoc_STR("*float*: Curvature between AC power and DC power at ref [1/W]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"inv_snl_c1", (getter)InverterCECDatabase_get_inv_snl_c1,(setter)InverterCECDatabase_set_inv_snl_c1,
	PyDoc_STR("*float*: Coefficient of Pdco variation with DC input voltage [1/V]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"inv_snl_c2", (getter)InverterCECDatabase_get_inv_snl_c2,(setter)InverterCECDatabase_set_inv_snl_c2,
	PyDoc_STR("*float*: Coefficient of Pso variation with DC input voltage [1/V]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"inv_snl_c3", (getter)InverterCECDatabase_get_inv_snl_c3,(setter)InverterCECDatabase_set_inv_snl_c3,
	PyDoc_STR("*float*: Coefficient of Co variation with DC input voltage [1/V]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"inv_snl_paco", (getter)InverterCECDatabase_get_inv_snl_paco,(setter)InverterCECDatabase_set_inv_snl_paco,
	PyDoc_STR("*float*: AC maximum power rating [Wac]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"inv_snl_pdco", (getter)InverterCECDatabase_get_inv_snl_pdco,(setter)InverterCECDatabase_set_inv_snl_pdco,
	PyDoc_STR("*float*: DC input power at which AC power rating is achieved [Wdc]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"inv_snl_pnt", (getter)InverterCECDatabase_get_inv_snl_pnt,(setter)InverterCECDatabase_set_inv_snl_pnt,
	PyDoc_STR("*float*: AC power consumed by inverter at night [Wac]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"inv_snl_pso", (getter)InverterCECDatabase_get_inv_snl_pso,(setter)InverterCECDatabase_set_inv_snl_pso,
	PyDoc_STR("*float*: DC power required to enable the inversion process [Wdc]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"inv_snl_vdcmax", (getter)InverterCECDatabase_get_inv_snl_vdcmax,(setter)InverterCECDatabase_set_inv_snl_vdcmax,
	PyDoc_STR("*float*: Maximum DC input operating voltage [Vdc]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"inv_snl_vdco", (getter)InverterCECDatabase_get_inv_snl_vdco,(setter)InverterCECDatabase_set_inv_snl_vdco,
	PyDoc_STR("*float*: DC input voltage for the rated AC power rating [Vdc]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"inv_tdc_cec_db", (getter)InverterCECDatabase_get_inv_tdc_cec_db,(setter)InverterCECDatabase_set_inv_tdc_cec_db,
	PyDoc_STR("*sequence[sequence]*: Temperature derate curves for CEC Database [Vdc]\n\n*Required*: set to 0 if not provided."),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject InverterCECDatabase_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Pvsamv1.InverterCECDatabase",             /*tp_name*/
		sizeof(InverterCECDatabaseObject),          /*tp_basicsize*/
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
		InverterCECDatabase_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		InverterCECDatabase_getset,          /*tp_getset*/
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
	 * InverterCECCoefficientGenerator Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Pvsamv1   data_ptr;
} InverterCECCoefficientGeneratorObject;

static PyTypeObject InverterCECCoefficientGenerator_Type;

static PyObject *
InverterCECCoefficientGenerator_new(SAM_Pvsamv1 data_ptr)
{
	PyObject* new_obj = InverterCECCoefficientGenerator_Type.tp_alloc(&InverterCECCoefficientGenerator_Type,0);

	InverterCECCoefficientGeneratorObject* InverterCECCoefficientGenerator_obj = (InverterCECCoefficientGeneratorObject*)new_obj;

	InverterCECCoefficientGenerator_obj->data_ptr = data_ptr;

	return new_obj;
}

/* InverterCECCoefficientGenerator methods */

static PyObject *
InverterCECCoefficientGenerator_assign(InverterCECCoefficientGeneratorObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Pvsamv1", "InverterCECCoefficientGenerator")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
InverterCECCoefficientGenerator_export(InverterCECCoefficientGeneratorObject *self, PyObject *args)
{
	PyTypeObject* tp = &InverterCECCoefficientGenerator_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef InverterCECCoefficientGenerator_methods[] = {
		{"assign",            (PyCFunction)InverterCECCoefficientGenerator_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``InverterCECCoefficientGenerator_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)InverterCECCoefficientGenerator_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
InverterCECCoefficientGenerator_get_inv_cec_cg_c0(InverterCECCoefficientGeneratorObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_c0_nget, self->data_ptr);
}

static int
InverterCECCoefficientGenerator_set_inv_cec_cg_c0(InverterCECCoefficientGeneratorObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_c0_nset, self->data_ptr);
}

static PyObject *
InverterCECCoefficientGenerator_get_inv_cec_cg_c1(InverterCECCoefficientGeneratorObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_c1_nget, self->data_ptr);
}

static int
InverterCECCoefficientGenerator_set_inv_cec_cg_c1(InverterCECCoefficientGeneratorObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_c1_nset, self->data_ptr);
}

static PyObject *
InverterCECCoefficientGenerator_get_inv_cec_cg_c2(InverterCECCoefficientGeneratorObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_c2_nget, self->data_ptr);
}

static int
InverterCECCoefficientGenerator_set_inv_cec_cg_c2(InverterCECCoefficientGeneratorObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_c2_nset, self->data_ptr);
}

static PyObject *
InverterCECCoefficientGenerator_get_inv_cec_cg_c3(InverterCECCoefficientGeneratorObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_c3_nget, self->data_ptr);
}

static int
InverterCECCoefficientGenerator_set_inv_cec_cg_c3(InverterCECCoefficientGeneratorObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_c3_nset, self->data_ptr);
}

static PyObject *
InverterCECCoefficientGenerator_get_inv_cec_cg_paco(InverterCECCoefficientGeneratorObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_paco_nget, self->data_ptr);
}

static int
InverterCECCoefficientGenerator_set_inv_cec_cg_paco(InverterCECCoefficientGeneratorObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_paco_nset, self->data_ptr);
}

static PyObject *
InverterCECCoefficientGenerator_get_inv_cec_cg_pdco(InverterCECCoefficientGeneratorObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_pdco_nget, self->data_ptr);
}

static int
InverterCECCoefficientGenerator_set_inv_cec_cg_pdco(InverterCECCoefficientGeneratorObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_pdco_nset, self->data_ptr);
}

static PyObject *
InverterCECCoefficientGenerator_get_inv_cec_cg_pnt(InverterCECCoefficientGeneratorObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_pnt_nget, self->data_ptr);
}

static int
InverterCECCoefficientGenerator_set_inv_cec_cg_pnt(InverterCECCoefficientGeneratorObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_pnt_nset, self->data_ptr);
}

static PyObject *
InverterCECCoefficientGenerator_get_inv_cec_cg_psco(InverterCECCoefficientGeneratorObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_psco_nget, self->data_ptr);
}

static int
InverterCECCoefficientGenerator_set_inv_cec_cg_psco(InverterCECCoefficientGeneratorObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_psco_nset, self->data_ptr);
}

static PyObject *
InverterCECCoefficientGenerator_get_inv_cec_cg_vdcmax(InverterCECCoefficientGeneratorObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_vdcmax_nget, self->data_ptr);
}

static int
InverterCECCoefficientGenerator_set_inv_cec_cg_vdcmax(InverterCECCoefficientGeneratorObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_vdcmax_nset, self->data_ptr);
}

static PyObject *
InverterCECCoefficientGenerator_get_inv_cec_cg_vdco(InverterCECCoefficientGeneratorObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_vdco_nget, self->data_ptr);
}

static int
InverterCECCoefficientGenerator_set_inv_cec_cg_vdco(InverterCECCoefficientGeneratorObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_vdco_nset, self->data_ptr);
}

static PyObject *
InverterCECCoefficientGenerator_get_inv_tdc_cec_cg(InverterCECCoefficientGeneratorObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_tdc_cec_cg_mget, self->data_ptr);
}

static int
InverterCECCoefficientGenerator_set_inv_tdc_cec_cg(InverterCECCoefficientGeneratorObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_tdc_cec_cg_mset, self->data_ptr);
}

static PyGetSetDef InverterCECCoefficientGenerator_getset[] = {
{"inv_cec_cg_c0", (getter)InverterCECCoefficientGenerator_get_inv_cec_cg_c0,(setter)InverterCECCoefficientGenerator_set_inv_cec_cg_c0,
	PyDoc_STR("*float*: Curvature between AC power and DC power at ref [1/W]\n\n*Required*: set to 3 if not provided."),
 	NULL},
{"inv_cec_cg_c1", (getter)InverterCECCoefficientGenerator_get_inv_cec_cg_c1,(setter)InverterCECCoefficientGenerator_set_inv_cec_cg_c1,
	PyDoc_STR("*float*: Coefficient of Pdco variation with DC input voltage [1/V]\n\n*Required*: set to 3 if not provided."),
 	NULL},
{"inv_cec_cg_c2", (getter)InverterCECCoefficientGenerator_get_inv_cec_cg_c2,(setter)InverterCECCoefficientGenerator_set_inv_cec_cg_c2,
	PyDoc_STR("*float*: Coefficient of Pso variation with DC input voltage [1/V]\n\n*Required*: set to 3 if not provided."),
 	NULL},
{"inv_cec_cg_c3", (getter)InverterCECCoefficientGenerator_get_inv_cec_cg_c3,(setter)InverterCECCoefficientGenerator_set_inv_cec_cg_c3,
	PyDoc_STR("*float*: Coefficient of Co variation with DC input voltage [1/V]\n\n*Required*: set to 3 if not provided."),
 	NULL},
{"inv_cec_cg_paco", (getter)InverterCECCoefficientGenerator_get_inv_cec_cg_paco,(setter)InverterCECCoefficientGenerator_set_inv_cec_cg_paco,
	PyDoc_STR("*float*: AC maximum power rating [Wac]\n\n*Required*: set to 3 if not provided."),
 	NULL},
{"inv_cec_cg_pdco", (getter)InverterCECCoefficientGenerator_get_inv_cec_cg_pdco,(setter)InverterCECCoefficientGenerator_set_inv_cec_cg_pdco,
	PyDoc_STR("*float*: DC input power at which AC power rating is achieved [Wdc]\n\n*Required*: set to 3 if not provided."),
 	NULL},
{"inv_cec_cg_pnt", (getter)InverterCECCoefficientGenerator_get_inv_cec_cg_pnt,(setter)InverterCECCoefficientGenerator_set_inv_cec_cg_pnt,
	PyDoc_STR("*float*: AC power consumed by inverter at night [Wac]\n\n*Required*: set to 3 if not provided."),
 	NULL},
{"inv_cec_cg_psco", (getter)InverterCECCoefficientGenerator_get_inv_cec_cg_psco,(setter)InverterCECCoefficientGenerator_set_inv_cec_cg_psco,
	PyDoc_STR("*float*: DC power required to enable the inversion process [Wdc]\n\n*Required*: set to 3 if not provided."),
 	NULL},
{"inv_cec_cg_vdcmax", (getter)InverterCECCoefficientGenerator_get_inv_cec_cg_vdcmax,(setter)InverterCECCoefficientGenerator_set_inv_cec_cg_vdcmax,
	PyDoc_STR("*float*: Maximum DC input operating voltage [Vdc]\n\n*Required*: set to 3 if not provided."),
 	NULL},
{"inv_cec_cg_vdco", (getter)InverterCECCoefficientGenerator_get_inv_cec_cg_vdco,(setter)InverterCECCoefficientGenerator_set_inv_cec_cg_vdco,
	PyDoc_STR("*float*: DC input voltage for the rated AC power rating [Vdc]\n\n*Required*: set to 3 if not provided."),
 	NULL},
{"inv_tdc_cec_cg", (getter)InverterCECCoefficientGenerator_get_inv_tdc_cec_cg,(setter)InverterCECCoefficientGenerator_set_inv_tdc_cec_cg,
	PyDoc_STR("*sequence[sequence]*: Temperature derate curves for CEC Coef Gen [Vdc]\n\n*Required*: set to 3 if not provided."),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject InverterCECCoefficientGenerator_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Pvsamv1.InverterCECCoefficientGenerator",             /*tp_name*/
		sizeof(InverterCECCoefficientGeneratorObject),          /*tp_basicsize*/
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
		InverterCECCoefficientGenerator_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		InverterCECCoefficientGenerator_getset,          /*tp_getset*/
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
	 * InverterDatasheet Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Pvsamv1   data_ptr;
} InverterDatasheetObject;

static PyTypeObject InverterDatasheet_Type;

static PyObject *
InverterDatasheet_new(SAM_Pvsamv1 data_ptr)
{
	PyObject* new_obj = InverterDatasheet_Type.tp_alloc(&InverterDatasheet_Type,0);

	InverterDatasheetObject* InverterDatasheet_obj = (InverterDatasheetObject*)new_obj;

	InverterDatasheet_obj->data_ptr = data_ptr;

	return new_obj;
}

/* InverterDatasheet methods */

static PyObject *
InverterDatasheet_assign(InverterDatasheetObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Pvsamv1", "InverterDatasheet")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
InverterDatasheet_export(InverterDatasheetObject *self, PyObject *args)
{
	PyTypeObject* tp = &InverterDatasheet_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef InverterDatasheet_methods[] = {
		{"assign",            (PyCFunction)InverterDatasheet_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``InverterDatasheet_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)InverterDatasheet_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
InverterDatasheet_get_inv_ds_eff(InverterDatasheetObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_InverterDatasheet_inv_ds_eff_nget, self->data_ptr);
}

static int
InverterDatasheet_set_inv_ds_eff(InverterDatasheetObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_InverterDatasheet_inv_ds_eff_nset, self->data_ptr);
}

static PyObject *
InverterDatasheet_get_inv_ds_paco(InverterDatasheetObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_InverterDatasheet_inv_ds_paco_nget, self->data_ptr);
}

static int
InverterDatasheet_set_inv_ds_paco(InverterDatasheetObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_InverterDatasheet_inv_ds_paco_nset, self->data_ptr);
}

static PyObject *
InverterDatasheet_get_inv_ds_pnt(InverterDatasheetObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_InverterDatasheet_inv_ds_pnt_nget, self->data_ptr);
}

static int
InverterDatasheet_set_inv_ds_pnt(InverterDatasheetObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_InverterDatasheet_inv_ds_pnt_nset, self->data_ptr);
}

static PyObject *
InverterDatasheet_get_inv_ds_pso(InverterDatasheetObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_InverterDatasheet_inv_ds_pso_nget, self->data_ptr);
}

static int
InverterDatasheet_set_inv_ds_pso(InverterDatasheetObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_InverterDatasheet_inv_ds_pso_nset, self->data_ptr);
}

static PyObject *
InverterDatasheet_get_inv_ds_vdcmax(InverterDatasheetObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_InverterDatasheet_inv_ds_vdcmax_nget, self->data_ptr);
}

static int
InverterDatasheet_set_inv_ds_vdcmax(InverterDatasheetObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_InverterDatasheet_inv_ds_vdcmax_nset, self->data_ptr);
}

static PyObject *
InverterDatasheet_get_inv_ds_vdco(InverterDatasheetObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_InverterDatasheet_inv_ds_vdco_nget, self->data_ptr);
}

static int
InverterDatasheet_set_inv_ds_vdco(InverterDatasheetObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_InverterDatasheet_inv_ds_vdco_nset, self->data_ptr);
}

static PyObject *
InverterDatasheet_get_inv_tdc_ds(InverterDatasheetObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Pvsamv1_InverterDatasheet_inv_tdc_ds_mget, self->data_ptr);
}

static int
InverterDatasheet_set_inv_tdc_ds(InverterDatasheetObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_Pvsamv1_InverterDatasheet_inv_tdc_ds_mset, self->data_ptr);
}

static PyGetSetDef InverterDatasheet_getset[] = {
{"inv_ds_eff", (getter)InverterDatasheet_get_inv_ds_eff,(setter)InverterDatasheet_set_inv_ds_eff,
	PyDoc_STR("*float*: Weighted or Peak or Nominal Efficiency [Wdc]\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"inv_ds_paco", (getter)InverterDatasheet_get_inv_ds_paco,(setter)InverterDatasheet_set_inv_ds_paco,
	PyDoc_STR("*float*: AC maximum power rating [Wac]\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"inv_ds_pnt", (getter)InverterDatasheet_get_inv_ds_pnt,(setter)InverterDatasheet_set_inv_ds_pnt,
	PyDoc_STR("*float*: AC power consumed by inverter at night [Wac]\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"inv_ds_pso", (getter)InverterDatasheet_get_inv_ds_pso,(setter)InverterDatasheet_set_inv_ds_pso,
	PyDoc_STR("*float*: DC power required to enable the inversion process [Wdc]\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"inv_ds_vdcmax", (getter)InverterDatasheet_get_inv_ds_vdcmax,(setter)InverterDatasheet_set_inv_ds_vdcmax,
	PyDoc_STR("*float*: Maximum DC input operating voltage [Vdc]\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"inv_ds_vdco", (getter)InverterDatasheet_get_inv_ds_vdco,(setter)InverterDatasheet_set_inv_ds_vdco,
	PyDoc_STR("*float*: DC input voltage for the rated AC power rating [Vdc]\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"inv_tdc_ds", (getter)InverterDatasheet_get_inv_tdc_ds,(setter)InverterDatasheet_set_inv_tdc_ds,
	PyDoc_STR("*sequence[sequence]*: Temperature derate curves for Inv Datasheet [Vdc]\n\n*Required*: set to 1 if not provided."),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject InverterDatasheet_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Pvsamv1.InverterDatasheet",             /*tp_name*/
		sizeof(InverterDatasheetObject),          /*tp_basicsize*/
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
		InverterDatasheet_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		InverterDatasheet_getset,          /*tp_getset*/
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
	 * InverterPartLoadCurve Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Pvsamv1   data_ptr;
} InverterPartLoadCurveObject;

static PyTypeObject InverterPartLoadCurve_Type;

static PyObject *
InverterPartLoadCurve_new(SAM_Pvsamv1 data_ptr)
{
	PyObject* new_obj = InverterPartLoadCurve_Type.tp_alloc(&InverterPartLoadCurve_Type,0);

	InverterPartLoadCurveObject* InverterPartLoadCurve_obj = (InverterPartLoadCurveObject*)new_obj;

	InverterPartLoadCurve_obj->data_ptr = data_ptr;

	return new_obj;
}

/* InverterPartLoadCurve methods */

static PyObject *
InverterPartLoadCurve_assign(InverterPartLoadCurveObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Pvsamv1", "InverterPartLoadCurve")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
InverterPartLoadCurve_export(InverterPartLoadCurveObject *self, PyObject *args)
{
	PyTypeObject* tp = &InverterPartLoadCurve_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef InverterPartLoadCurve_methods[] = {
		{"assign",            (PyCFunction)InverterPartLoadCurve_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``InverterPartLoadCurve_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)InverterPartLoadCurve_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
InverterPartLoadCurve_get_inv_pd_efficiency(InverterPartLoadCurveObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_InverterPartLoadCurve_inv_pd_efficiency_aget, self->data_ptr);
}

static int
InverterPartLoadCurve_set_inv_pd_efficiency(InverterPartLoadCurveObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Pvsamv1_InverterPartLoadCurve_inv_pd_efficiency_aset, self->data_ptr);
}

static PyObject *
InverterPartLoadCurve_get_inv_pd_paco(InverterPartLoadCurveObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_InverterPartLoadCurve_inv_pd_paco_nget, self->data_ptr);
}

static int
InverterPartLoadCurve_set_inv_pd_paco(InverterPartLoadCurveObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_InverterPartLoadCurve_inv_pd_paco_nset, self->data_ptr);
}

static PyObject *
InverterPartLoadCurve_get_inv_pd_partload(InverterPartLoadCurveObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_InverterPartLoadCurve_inv_pd_partload_aget, self->data_ptr);
}

static int
InverterPartLoadCurve_set_inv_pd_partload(InverterPartLoadCurveObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Pvsamv1_InverterPartLoadCurve_inv_pd_partload_aset, self->data_ptr);
}

static PyObject *
InverterPartLoadCurve_get_inv_pd_pdco(InverterPartLoadCurveObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_InverterPartLoadCurve_inv_pd_pdco_nget, self->data_ptr);
}

static int
InverterPartLoadCurve_set_inv_pd_pdco(InverterPartLoadCurveObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_InverterPartLoadCurve_inv_pd_pdco_nset, self->data_ptr);
}

static PyObject *
InverterPartLoadCurve_get_inv_pd_pnt(InverterPartLoadCurveObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_InverterPartLoadCurve_inv_pd_pnt_nget, self->data_ptr);
}

static int
InverterPartLoadCurve_set_inv_pd_pnt(InverterPartLoadCurveObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_InverterPartLoadCurve_inv_pd_pnt_nset, self->data_ptr);
}

static PyObject *
InverterPartLoadCurve_get_inv_pd_vdcmax(InverterPartLoadCurveObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_InverterPartLoadCurve_inv_pd_vdcmax_nget, self->data_ptr);
}

static int
InverterPartLoadCurve_set_inv_pd_vdcmax(InverterPartLoadCurveObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_InverterPartLoadCurve_inv_pd_vdcmax_nset, self->data_ptr);
}

static PyObject *
InverterPartLoadCurve_get_inv_pd_vdco(InverterPartLoadCurveObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_InverterPartLoadCurve_inv_pd_vdco_nget, self->data_ptr);
}

static int
InverterPartLoadCurve_set_inv_pd_vdco(InverterPartLoadCurveObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_InverterPartLoadCurve_inv_pd_vdco_nset, self->data_ptr);
}

static PyObject *
InverterPartLoadCurve_get_inv_tdc_plc(InverterPartLoadCurveObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Pvsamv1_InverterPartLoadCurve_inv_tdc_plc_mget, self->data_ptr);
}

static int
InverterPartLoadCurve_set_inv_tdc_plc(InverterPartLoadCurveObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_Pvsamv1_InverterPartLoadCurve_inv_tdc_plc_mset, self->data_ptr);
}

static PyGetSetDef InverterPartLoadCurve_getset[] = {
{"inv_pd_efficiency", (getter)InverterPartLoadCurve_get_inv_pd_efficiency,(setter)InverterPartLoadCurve_set_inv_pd_efficiency,
	PyDoc_STR("*sequence*: Partload curve efficiency values [%]\n\n*Required*: set to 2 if not provided."),
 	NULL},
{"inv_pd_paco", (getter)InverterPartLoadCurve_get_inv_pd_paco,(setter)InverterPartLoadCurve_set_inv_pd_paco,
	PyDoc_STR("*float*: AC maximum power rating [Wac]\n\n*Required*: set to 2 if not provided."),
 	NULL},
{"inv_pd_partload", (getter)InverterPartLoadCurve_get_inv_pd_partload,(setter)InverterPartLoadCurve_set_inv_pd_partload,
	PyDoc_STR("*sequence*: Partload curve partload values [%]\n\n*Required*: set to 2 if not provided."),
 	NULL},
{"inv_pd_pdco", (getter)InverterPartLoadCurve_get_inv_pd_pdco,(setter)InverterPartLoadCurve_set_inv_pd_pdco,
	PyDoc_STR("*float*: DC input power at which AC power rating is achieved [Wdc]\n\n*Required*: set to 2 if not provided."),
 	NULL},
{"inv_pd_pnt", (getter)InverterPartLoadCurve_get_inv_pd_pnt,(setter)InverterPartLoadCurve_set_inv_pd_pnt,
	PyDoc_STR("*float*: AC power consumed by inverter at night [Wac]\n\n*Required*: set to 2 if not provided."),
 	NULL},
{"inv_pd_vdcmax", (getter)InverterPartLoadCurve_get_inv_pd_vdcmax,(setter)InverterPartLoadCurve_set_inv_pd_vdcmax,
	PyDoc_STR("*float*: Maximum DC input operating voltage [Vdc]\n\n*Required*: set to 2 if not provided."),
 	NULL},
{"inv_pd_vdco", (getter)InverterPartLoadCurve_get_inv_pd_vdco,(setter)InverterPartLoadCurve_set_inv_pd_vdco,
	PyDoc_STR("*float*: DC input voltage for the rated AC power rating [Vdc]\n\n*Required*: set to 2 if not provided."),
 	NULL},
{"inv_tdc_plc", (getter)InverterPartLoadCurve_get_inv_tdc_plc,(setter)InverterPartLoadCurve_set_inv_tdc_plc,
	PyDoc_STR("*sequence[sequence]*: Temperature derate curves for Part Load Curve [C]\n\n*Required*: set to 2 if not provided."),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject InverterPartLoadCurve_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Pvsamv1.InverterPartLoadCurve",             /*tp_name*/
		sizeof(InverterPartLoadCurveObject),          /*tp_basicsize*/
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
		InverterPartLoadCurve_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		InverterPartLoadCurve_getset,          /*tp_getset*/
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
	 * InverterMermoudLejeuneModel Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Pvsamv1   data_ptr;
} InverterMermoudLejeuneModelObject;

static PyTypeObject InverterMermoudLejeuneModel_Type;

static PyObject *
InverterMermoudLejeuneModel_new(SAM_Pvsamv1 data_ptr)
{
	PyObject* new_obj = InverterMermoudLejeuneModel_Type.tp_alloc(&InverterMermoudLejeuneModel_Type,0);

	InverterMermoudLejeuneModelObject* InverterMermoudLejeuneModel_obj = (InverterMermoudLejeuneModelObject*)new_obj;

	InverterMermoudLejeuneModel_obj->data_ptr = data_ptr;

	return new_obj;
}

/* InverterMermoudLejeuneModel methods */

static PyObject *
InverterMermoudLejeuneModel_assign(InverterMermoudLejeuneModelObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Pvsamv1", "InverterMermoudLejeuneModel")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
InverterMermoudLejeuneModel_export(InverterMermoudLejeuneModelObject *self, PyObject *args)
{
	PyTypeObject* tp = &InverterMermoudLejeuneModel_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef InverterMermoudLejeuneModel_methods[] = {
		{"assign",            (PyCFunction)InverterMermoudLejeuneModel_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``InverterMermoudLejeuneModel_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)InverterMermoudLejeuneModel_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
InverterMermoudLejeuneModel_get_ond_Aux_Loss(InverterMermoudLejeuneModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_Aux_Loss_nget, self->data_ptr);
}

static int
InverterMermoudLejeuneModel_set_ond_Aux_Loss(InverterMermoudLejeuneModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_Aux_Loss_nset, self->data_ptr);
}

static PyObject *
InverterMermoudLejeuneModel_get_ond_CompPMax(InverterMermoudLejeuneModelObject *self, void *closure)
{
	return PySAM_string_getter(SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_CompPMax_sget, self->data_ptr);
}

static int
InverterMermoudLejeuneModel_set_ond_CompPMax(InverterMermoudLejeuneModelObject *self, PyObject *value, void *closure)
{
	return PySAM_string_setter(value, SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_CompPMax_sset, self->data_ptr);
}

static PyObject *
InverterMermoudLejeuneModel_get_ond_CompVMax(InverterMermoudLejeuneModelObject *self, void *closure)
{
	return PySAM_string_getter(SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_CompVMax_sget, self->data_ptr);
}

static int
InverterMermoudLejeuneModel_set_ond_CompVMax(InverterMermoudLejeuneModelObject *self, PyObject *value, void *closure)
{
	return PySAM_string_setter(value, SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_CompVMax_sset, self->data_ptr);
}

static PyObject *
InverterMermoudLejeuneModel_get_ond_IMaxAC(InverterMermoudLejeuneModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_IMaxAC_nget, self->data_ptr);
}

static int
InverterMermoudLejeuneModel_set_ond_IMaxAC(InverterMermoudLejeuneModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_IMaxAC_nset, self->data_ptr);
}

static PyObject *
InverterMermoudLejeuneModel_get_ond_IMaxDC(InverterMermoudLejeuneModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_IMaxDC_nget, self->data_ptr);
}

static int
InverterMermoudLejeuneModel_set_ond_IMaxDC(InverterMermoudLejeuneModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_IMaxDC_nset, self->data_ptr);
}

static PyObject *
InverterMermoudLejeuneModel_get_ond_INomAC(InverterMermoudLejeuneModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_INomAC_nget, self->data_ptr);
}

static int
InverterMermoudLejeuneModel_set_ond_INomAC(InverterMermoudLejeuneModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_INomAC_nset, self->data_ptr);
}

static PyObject *
InverterMermoudLejeuneModel_get_ond_INomDC(InverterMermoudLejeuneModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_INomDC_nget, self->data_ptr);
}

static int
InverterMermoudLejeuneModel_set_ond_INomDC(InverterMermoudLejeuneModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_INomDC_nset, self->data_ptr);
}

static PyObject *
InverterMermoudLejeuneModel_get_ond_ModeAffEnum(InverterMermoudLejeuneModelObject *self, void *closure)
{
	return PySAM_string_getter(SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_ModeAffEnum_sget, self->data_ptr);
}

static int
InverterMermoudLejeuneModel_set_ond_ModeAffEnum(InverterMermoudLejeuneModelObject *self, PyObject *value, void *closure)
{
	return PySAM_string_setter(value, SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_ModeAffEnum_sset, self->data_ptr);
}

static PyObject *
InverterMermoudLejeuneModel_get_ond_ModeOper(InverterMermoudLejeuneModelObject *self, void *closure)
{
	return PySAM_string_getter(SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_ModeOper_sget, self->data_ptr);
}

static int
InverterMermoudLejeuneModel_set_ond_ModeOper(InverterMermoudLejeuneModelObject *self, PyObject *value, void *closure)
{
	return PySAM_string_setter(value, SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_ModeOper_sset, self->data_ptr);
}

static PyObject *
InverterMermoudLejeuneModel_get_ond_NbInputs(InverterMermoudLejeuneModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_NbInputs_nget, self->data_ptr);
}

static int
InverterMermoudLejeuneModel_set_ond_NbInputs(InverterMermoudLejeuneModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_NbInputs_nset, self->data_ptr);
}

static PyObject *
InverterMermoudLejeuneModel_get_ond_NbMPPT(InverterMermoudLejeuneModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_NbMPPT_nget, self->data_ptr);
}

static int
InverterMermoudLejeuneModel_set_ond_NbMPPT(InverterMermoudLejeuneModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_NbMPPT_nset, self->data_ptr);
}

static PyObject *
InverterMermoudLejeuneModel_get_ond_Night_Loss(InverterMermoudLejeuneModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_Night_Loss_nget, self->data_ptr);
}

static int
InverterMermoudLejeuneModel_set_ond_Night_Loss(InverterMermoudLejeuneModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_Night_Loss_nset, self->data_ptr);
}

static PyObject *
InverterMermoudLejeuneModel_get_ond_PLim1(InverterMermoudLejeuneModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_PLim1_nget, self->data_ptr);
}

static int
InverterMermoudLejeuneModel_set_ond_PLim1(InverterMermoudLejeuneModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_PLim1_nset, self->data_ptr);
}

static PyObject *
InverterMermoudLejeuneModel_get_ond_PLimAbs(InverterMermoudLejeuneModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_PLimAbs_nget, self->data_ptr);
}

static int
InverterMermoudLejeuneModel_set_ond_PLimAbs(InverterMermoudLejeuneModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_PLimAbs_nset, self->data_ptr);
}

static PyObject *
InverterMermoudLejeuneModel_get_ond_PMaxDC(InverterMermoudLejeuneModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_PMaxDC_nget, self->data_ptr);
}

static int
InverterMermoudLejeuneModel_set_ond_PMaxDC(InverterMermoudLejeuneModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_PMaxDC_nset, self->data_ptr);
}

static PyObject *
InverterMermoudLejeuneModel_get_ond_PMaxOUT(InverterMermoudLejeuneModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_PMaxOUT_nget, self->data_ptr);
}

static int
InverterMermoudLejeuneModel_set_ond_PMaxOUT(InverterMermoudLejeuneModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_PMaxOUT_nset, self->data_ptr);
}

static PyObject *
InverterMermoudLejeuneModel_get_ond_PNomConv(InverterMermoudLejeuneModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_PNomConv_nget, self->data_ptr);
}

static int
InverterMermoudLejeuneModel_set_ond_PNomConv(InverterMermoudLejeuneModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_PNomConv_nset, self->data_ptr);
}

static PyObject *
InverterMermoudLejeuneModel_get_ond_PNomDC(InverterMermoudLejeuneModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_PNomDC_nget, self->data_ptr);
}

static int
InverterMermoudLejeuneModel_set_ond_PNomDC(InverterMermoudLejeuneModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_PNomDC_nset, self->data_ptr);
}

static PyObject *
InverterMermoudLejeuneModel_get_ond_PSeuil(InverterMermoudLejeuneModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_PSeuil_nget, self->data_ptr);
}

static int
InverterMermoudLejeuneModel_set_ond_PSeuil(InverterMermoudLejeuneModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_PSeuil_nset, self->data_ptr);
}

static PyObject *
InverterMermoudLejeuneModel_get_ond_TPLim1(InverterMermoudLejeuneModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_TPLim1_nget, self->data_ptr);
}

static int
InverterMermoudLejeuneModel_set_ond_TPLim1(InverterMermoudLejeuneModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_TPLim1_nset, self->data_ptr);
}

static PyObject *
InverterMermoudLejeuneModel_get_ond_TPLimAbs(InverterMermoudLejeuneModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_TPLimAbs_nget, self->data_ptr);
}

static int
InverterMermoudLejeuneModel_set_ond_TPLimAbs(InverterMermoudLejeuneModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_TPLimAbs_nset, self->data_ptr);
}

static PyObject *
InverterMermoudLejeuneModel_get_ond_TPMax(InverterMermoudLejeuneModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_TPMax_nget, self->data_ptr);
}

static int
InverterMermoudLejeuneModel_set_ond_TPMax(InverterMermoudLejeuneModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_TPMax_nset, self->data_ptr);
}

static PyObject *
InverterMermoudLejeuneModel_get_ond_TPNom(InverterMermoudLejeuneModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_TPNom_nget, self->data_ptr);
}

static int
InverterMermoudLejeuneModel_set_ond_TPNom(InverterMermoudLejeuneModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_TPNom_nset, self->data_ptr);
}

static PyObject *
InverterMermoudLejeuneModel_get_ond_VAbsMax(InverterMermoudLejeuneModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_VAbsMax_nget, self->data_ptr);
}

static int
InverterMermoudLejeuneModel_set_ond_VAbsMax(InverterMermoudLejeuneModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_VAbsMax_nset, self->data_ptr);
}

static PyObject *
InverterMermoudLejeuneModel_get_ond_VMPPMax(InverterMermoudLejeuneModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_VMPPMax_nget, self->data_ptr);
}

static int
InverterMermoudLejeuneModel_set_ond_VMPPMax(InverterMermoudLejeuneModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_VMPPMax_nset, self->data_ptr);
}

static PyObject *
InverterMermoudLejeuneModel_get_ond_VMppMin(InverterMermoudLejeuneModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_VMppMin_nget, self->data_ptr);
}

static int
InverterMermoudLejeuneModel_set_ond_VMppMin(InverterMermoudLejeuneModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_VMppMin_nset, self->data_ptr);
}

static PyObject *
InverterMermoudLejeuneModel_get_ond_VNomEff(InverterMermoudLejeuneModelObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_VNomEff_aget, self->data_ptr);
}

static int
InverterMermoudLejeuneModel_set_ond_VNomEff(InverterMermoudLejeuneModelObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_VNomEff_aset, self->data_ptr);
}

static PyObject *
InverterMermoudLejeuneModel_get_ond_VOutConv(InverterMermoudLejeuneModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_VOutConv_nget, self->data_ptr);
}

static int
InverterMermoudLejeuneModel_set_ond_VOutConv(InverterMermoudLejeuneModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_VOutConv_nset, self->data_ptr);
}

static PyObject *
InverterMermoudLejeuneModel_get_ond_doAllowOverpower(InverterMermoudLejeuneModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_doAllowOverpower_nget, self->data_ptr);
}

static int
InverterMermoudLejeuneModel_set_ond_doAllowOverpower(InverterMermoudLejeuneModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_doAllowOverpower_nset, self->data_ptr);
}

static PyObject *
InverterMermoudLejeuneModel_get_ond_doUseTemperatureLimit(InverterMermoudLejeuneModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_doUseTemperatureLimit_nget, self->data_ptr);
}

static int
InverterMermoudLejeuneModel_set_ond_doUseTemperatureLimit(InverterMermoudLejeuneModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_doUseTemperatureLimit_nset, self->data_ptr);
}

static PyObject *
InverterMermoudLejeuneModel_get_ond_effCurve_Pac(InverterMermoudLejeuneModelObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_effCurve_Pac_mget, self->data_ptr);
}

static int
InverterMermoudLejeuneModel_set_ond_effCurve_Pac(InverterMermoudLejeuneModelObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_effCurve_Pac_mset, self->data_ptr);
}

static PyObject *
InverterMermoudLejeuneModel_get_ond_effCurve_Pdc(InverterMermoudLejeuneModelObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_effCurve_Pdc_mget, self->data_ptr);
}

static int
InverterMermoudLejeuneModel_set_ond_effCurve_Pdc(InverterMermoudLejeuneModelObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_effCurve_Pdc_mset, self->data_ptr);
}

static PyObject *
InverterMermoudLejeuneModel_get_ond_effCurve_elements(InverterMermoudLejeuneModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_effCurve_elements_nget, self->data_ptr);
}

static int
InverterMermoudLejeuneModel_set_ond_effCurve_elements(InverterMermoudLejeuneModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_effCurve_elements_nset, self->data_ptr);
}

static PyObject *
InverterMermoudLejeuneModel_get_ond_effCurve_eta(InverterMermoudLejeuneModelObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_effCurve_eta_mget, self->data_ptr);
}

static int
InverterMermoudLejeuneModel_set_ond_effCurve_eta(InverterMermoudLejeuneModelObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_effCurve_eta_mset, self->data_ptr);
}

static PyObject *
InverterMermoudLejeuneModel_get_ond_lossRAc(InverterMermoudLejeuneModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_lossRAc_nget, self->data_ptr);
}

static int
InverterMermoudLejeuneModel_set_ond_lossRAc(InverterMermoudLejeuneModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_lossRAc_nset, self->data_ptr);
}

static PyObject *
InverterMermoudLejeuneModel_get_ond_lossRDc(InverterMermoudLejeuneModelObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_lossRDc_nget, self->data_ptr);
}

static int
InverterMermoudLejeuneModel_set_ond_lossRDc(InverterMermoudLejeuneModelObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_lossRDc_nset, self->data_ptr);
}

static PyGetSetDef InverterMermoudLejeuneModel_getset[] = {
{"ond_Aux_Loss", (getter)InverterMermoudLejeuneModel_get_ond_Aux_Loss,(setter)InverterMermoudLejeuneModel_set_ond_Aux_Loss,
	PyDoc_STR("*float*:  [W]\n\n*Required*: set to 4 if not provided."),
 	NULL},
{"ond_CompPMax", (getter)InverterMermoudLejeuneModel_get_ond_CompPMax,(setter)InverterMermoudLejeuneModel_set_ond_CompPMax,
	PyDoc_STR("*str*:  [-]\n\n*Required*: set to 4 if not provided."),
 	NULL},
{"ond_CompVMax", (getter)InverterMermoudLejeuneModel_get_ond_CompVMax,(setter)InverterMermoudLejeuneModel_set_ond_CompVMax,
	PyDoc_STR("*str*:  [-]\n\n*Required*: set to 4 if not provided."),
 	NULL},
{"ond_IMaxAC", (getter)InverterMermoudLejeuneModel_get_ond_IMaxAC,(setter)InverterMermoudLejeuneModel_set_ond_IMaxAC,
	PyDoc_STR("*float*:  [A]\n\n*Required*: set to 4 if not provided."),
 	NULL},
{"ond_IMaxDC", (getter)InverterMermoudLejeuneModel_get_ond_IMaxDC,(setter)InverterMermoudLejeuneModel_set_ond_IMaxDC,
	PyDoc_STR("*float*:  [A]\n\n*Required*: set to 4 if not provided."),
 	NULL},
{"ond_INomAC", (getter)InverterMermoudLejeuneModel_get_ond_INomAC,(setter)InverterMermoudLejeuneModel_set_ond_INomAC,
	PyDoc_STR("*float*:  [A]\n\n*Required*: set to 4 if not provided."),
 	NULL},
{"ond_INomDC", (getter)InverterMermoudLejeuneModel_get_ond_INomDC,(setter)InverterMermoudLejeuneModel_set_ond_INomDC,
	PyDoc_STR("*float*:  [A]\n\n*Required*: set to 4 if not provided."),
 	NULL},
{"ond_ModeAffEnum", (getter)InverterMermoudLejeuneModel_get_ond_ModeAffEnum,(setter)InverterMermoudLejeuneModel_set_ond_ModeAffEnum,
	PyDoc_STR("*str*:  [-]\n\n*Required*: set to 4 if not provided."),
 	NULL},
{"ond_ModeOper", (getter)InverterMermoudLejeuneModel_get_ond_ModeOper,(setter)InverterMermoudLejeuneModel_set_ond_ModeOper,
	PyDoc_STR("*str*:  [-]\n\n*Required*: set to 4 if not provided."),
 	NULL},
{"ond_NbInputs", (getter)InverterMermoudLejeuneModel_get_ond_NbInputs,(setter)InverterMermoudLejeuneModel_set_ond_NbInputs,
	PyDoc_STR("*float*:  [-]\n\n*Required*: set to 4 if not provided."),
 	NULL},
{"ond_NbMPPT", (getter)InverterMermoudLejeuneModel_get_ond_NbMPPT,(setter)InverterMermoudLejeuneModel_set_ond_NbMPPT,
	PyDoc_STR("*float*:  [-]\n\n*Required*: set to 4 if not provided."),
 	NULL},
{"ond_Night_Loss", (getter)InverterMermoudLejeuneModel_get_ond_Night_Loss,(setter)InverterMermoudLejeuneModel_set_ond_Night_Loss,
	PyDoc_STR("*float*:  [W]\n\n*Required*: set to 4 if not provided."),
 	NULL},
{"ond_PLim1", (getter)InverterMermoudLejeuneModel_get_ond_PLim1,(setter)InverterMermoudLejeuneModel_set_ond_PLim1,
	PyDoc_STR("*float*:  [W]\n\n*Required*: set to 4 if not provided."),
 	NULL},
{"ond_PLimAbs", (getter)InverterMermoudLejeuneModel_get_ond_PLimAbs,(setter)InverterMermoudLejeuneModel_set_ond_PLimAbs,
	PyDoc_STR("*float*:  [W]\n\n*Required*: set to 4 if not provided."),
 	NULL},
{"ond_PMaxDC", (getter)InverterMermoudLejeuneModel_get_ond_PMaxDC,(setter)InverterMermoudLejeuneModel_set_ond_PMaxDC,
	PyDoc_STR("*float*:  [W]\n\n*Required*: set to 4 if not provided."),
 	NULL},
{"ond_PMaxOUT", (getter)InverterMermoudLejeuneModel_get_ond_PMaxOUT,(setter)InverterMermoudLejeuneModel_set_ond_PMaxOUT,
	PyDoc_STR("*float*:  [W]\n\n*Required*: set to 4 if not provided."),
 	NULL},
{"ond_PNomConv", (getter)InverterMermoudLejeuneModel_get_ond_PNomConv,(setter)InverterMermoudLejeuneModel_set_ond_PNomConv,
	PyDoc_STR("*float*:  [W]\n\n*Required*: set to 4 if not provided."),
 	NULL},
{"ond_PNomDC", (getter)InverterMermoudLejeuneModel_get_ond_PNomDC,(setter)InverterMermoudLejeuneModel_set_ond_PNomDC,
	PyDoc_STR("*float*:  [W]\n\n*Required*: set to 4 if not provided."),
 	NULL},
{"ond_PSeuil", (getter)InverterMermoudLejeuneModel_get_ond_PSeuil,(setter)InverterMermoudLejeuneModel_set_ond_PSeuil,
	PyDoc_STR("*float*:  [W]\n\n*Required*: set to 4 if not provided."),
 	NULL},
{"ond_TPLim1", (getter)InverterMermoudLejeuneModel_get_ond_TPLim1,(setter)InverterMermoudLejeuneModel_set_ond_TPLim1,
	PyDoc_STR("*float*:  [°C]\n\n*Required*: set to 4 if not provided."),
 	NULL},
{"ond_TPLimAbs", (getter)InverterMermoudLejeuneModel_get_ond_TPLimAbs,(setter)InverterMermoudLejeuneModel_set_ond_TPLimAbs,
	PyDoc_STR("*float*:  [°C]\n\n*Required*: set to 4 if not provided."),
 	NULL},
{"ond_TPMax", (getter)InverterMermoudLejeuneModel_get_ond_TPMax,(setter)InverterMermoudLejeuneModel_set_ond_TPMax,
	PyDoc_STR("*float*:  [°C]\n\n*Required*: set to 4 if not provided."),
 	NULL},
{"ond_TPNom", (getter)InverterMermoudLejeuneModel_get_ond_TPNom,(setter)InverterMermoudLejeuneModel_set_ond_TPNom,
	PyDoc_STR("*float*:  [°C]\n\n*Required*: set to 4 if not provided."),
 	NULL},
{"ond_VAbsMax", (getter)InverterMermoudLejeuneModel_get_ond_VAbsMax,(setter)InverterMermoudLejeuneModel_set_ond_VAbsMax,
	PyDoc_STR("*float*:  [V]\n\n*Required*: set to 4 if not provided."),
 	NULL},
{"ond_VMPPMax", (getter)InverterMermoudLejeuneModel_get_ond_VMPPMax,(setter)InverterMermoudLejeuneModel_set_ond_VMPPMax,
	PyDoc_STR("*float*:  [V]\n\n*Required*: set to 4 if not provided."),
 	NULL},
{"ond_VMppMin", (getter)InverterMermoudLejeuneModel_get_ond_VMppMin,(setter)InverterMermoudLejeuneModel_set_ond_VMppMin,
	PyDoc_STR("*float*:  [V]\n\n*Required*: set to 4 if not provided."),
 	NULL},
{"ond_VNomEff", (getter)InverterMermoudLejeuneModel_get_ond_VNomEff,(setter)InverterMermoudLejeuneModel_set_ond_VNomEff,
	PyDoc_STR("*sequence*:  [V]\n\n*Required*: set to 4 if not provided."),
 	NULL},
{"ond_VOutConv", (getter)InverterMermoudLejeuneModel_get_ond_VOutConv,(setter)InverterMermoudLejeuneModel_set_ond_VOutConv,
	PyDoc_STR("*float*:  [W]\n\n*Required*: set to 4 if not provided."),
 	NULL},
{"ond_doAllowOverpower", (getter)InverterMermoudLejeuneModel_get_ond_doAllowOverpower,(setter)InverterMermoudLejeuneModel_set_ond_doAllowOverpower,
	PyDoc_STR("*float*:  [-]\n\n*Required*: set to 4 if not provided."),
 	NULL},
{"ond_doUseTemperatureLimit", (getter)InverterMermoudLejeuneModel_get_ond_doUseTemperatureLimit,(setter)InverterMermoudLejeuneModel_set_ond_doUseTemperatureLimit,
	PyDoc_STR("*float*:  [-]\n\n*Required*: set to 4 if not provided."),
 	NULL},
{"ond_effCurve_Pac", (getter)InverterMermoudLejeuneModel_get_ond_effCurve_Pac,(setter)InverterMermoudLejeuneModel_set_ond_effCurve_Pac,
	PyDoc_STR("*sequence[sequence]*:  [W]\n\n*Required*: set to 4 if not provided."),
 	NULL},
{"ond_effCurve_Pdc", (getter)InverterMermoudLejeuneModel_get_ond_effCurve_Pdc,(setter)InverterMermoudLejeuneModel_set_ond_effCurve_Pdc,
	PyDoc_STR("*sequence[sequence]*:  [W]\n\n*Required*: set to 4 if not provided."),
 	NULL},
{"ond_effCurve_elements", (getter)InverterMermoudLejeuneModel_get_ond_effCurve_elements,(setter)InverterMermoudLejeuneModel_set_ond_effCurve_elements,
	PyDoc_STR("*float*:  [-]\n\n*Required*: set to 4 if not provided."),
 	NULL},
{"ond_effCurve_eta", (getter)InverterMermoudLejeuneModel_get_ond_effCurve_eta,(setter)InverterMermoudLejeuneModel_set_ond_effCurve_eta,
	PyDoc_STR("*sequence[sequence]*:  [-]\n\n*Required*: set to 4 if not provided."),
 	NULL},
{"ond_lossRAc", (getter)InverterMermoudLejeuneModel_get_ond_lossRAc,(setter)InverterMermoudLejeuneModel_set_ond_lossRAc,
	PyDoc_STR("*float*:  [A]\n\n*Required*: set to 4 if not provided."),
 	NULL},
{"ond_lossRDc", (getter)InverterMermoudLejeuneModel_get_ond_lossRDc,(setter)InverterMermoudLejeuneModel_set_ond_lossRDc,
	PyDoc_STR("*float*:  [V/A]\n\n*Required*: set to 4 if not provided."),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject InverterMermoudLejeuneModel_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Pvsamv1.InverterMermoudLejeuneModel",             /*tp_name*/
		sizeof(InverterMermoudLejeuneModelObject),          /*tp_basicsize*/
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
		InverterMermoudLejeuneModel_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		InverterMermoudLejeuneModel_getset,          /*tp_getset*/
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
	SAM_Pvsamv1   data_ptr;
} BatteryObject;

static PyTypeObject Battery_Type;

static PyObject *
Battery_new(SAM_Pvsamv1 data_ptr)
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

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Pvsamv1", "Battery")){
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
Battery_get_LeadAcid_q10_computed(BatteryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Battery_LeadAcid_q10_computed_nget, self->data_ptr);
}

static int
Battery_set_LeadAcid_q10_computed(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Battery_LeadAcid_q10_computed_nset, self->data_ptr);
}

static PyObject *
Battery_get_LeadAcid_q20_computed(BatteryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Battery_LeadAcid_q20_computed_nget, self->data_ptr);
}

static int
Battery_set_LeadAcid_q20_computed(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Battery_LeadAcid_q20_computed_nset, self->data_ptr);
}

static PyObject *
Battery_get_LeadAcid_qn_computed(BatteryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Battery_LeadAcid_qn_computed_nget, self->data_ptr);
}

static int
Battery_set_LeadAcid_qn_computed(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Battery_LeadAcid_qn_computed_nset, self->data_ptr);
}

static PyObject *
Battery_get_LeadAcid_tn(BatteryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Battery_LeadAcid_tn_nget, self->data_ptr);
}

static int
Battery_set_LeadAcid_tn(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Battery_LeadAcid_tn_nset, self->data_ptr);
}

static PyObject *
Battery_get_batt_C_rate(BatteryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Battery_batt_C_rate_nget, self->data_ptr);
}

static int
Battery_set_batt_C_rate(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Battery_batt_C_rate_nset, self->data_ptr);
}

static PyObject *
Battery_get_batt_Cp(BatteryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Battery_batt_Cp_nget, self->data_ptr);
}

static int
Battery_set_batt_Cp(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Battery_batt_Cp_nset, self->data_ptr);
}

static PyObject *
Battery_get_batt_Qexp(BatteryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Battery_batt_Qexp_nget, self->data_ptr);
}

static int
Battery_set_batt_Qexp(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Battery_batt_Qexp_nset, self->data_ptr);
}

static PyObject *
Battery_get_batt_Qfull(BatteryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Battery_batt_Qfull_nget, self->data_ptr);
}

static int
Battery_set_batt_Qfull(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Battery_batt_Qfull_nset, self->data_ptr);
}

static PyObject *
Battery_get_batt_Qfull_flow(BatteryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Battery_batt_Qfull_flow_nget, self->data_ptr);
}

static int
Battery_set_batt_Qfull_flow(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Battery_batt_Qfull_flow_nset, self->data_ptr);
}

static PyObject *
Battery_get_batt_Qnom(BatteryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Battery_batt_Qnom_nget, self->data_ptr);
}

static int
Battery_set_batt_Qnom(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Battery_batt_Qnom_nset, self->data_ptr);
}

static PyObject *
Battery_get_batt_Vexp(BatteryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Battery_batt_Vexp_nget, self->data_ptr);
}

static int
Battery_set_batt_Vexp(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Battery_batt_Vexp_nset, self->data_ptr);
}

static PyObject *
Battery_get_batt_Vfull(BatteryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Battery_batt_Vfull_nget, self->data_ptr);
}

static int
Battery_set_batt_Vfull(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Battery_batt_Vfull_nset, self->data_ptr);
}

static PyObject *
Battery_get_batt_Vnom(BatteryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Battery_batt_Vnom_nget, self->data_ptr);
}

static int
Battery_set_batt_Vnom(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Battery_batt_Vnom_nset, self->data_ptr);
}

static PyObject *
Battery_get_batt_Vnom_default(BatteryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Battery_batt_Vnom_default_nget, self->data_ptr);
}

static int
Battery_set_batt_Vnom_default(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Battery_batt_Vnom_default_nset, self->data_ptr);
}

static PyObject *
Battery_get_batt_ac_dc_efficiency(BatteryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Battery_batt_ac_dc_efficiency_nget, self->data_ptr);
}

static int
Battery_set_batt_ac_dc_efficiency(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Battery_batt_ac_dc_efficiency_nset, self->data_ptr);
}

static PyObject *
Battery_get_batt_ac_or_dc(BatteryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Battery_batt_ac_or_dc_nget, self->data_ptr);
}

static int
Battery_set_batt_ac_or_dc(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Battery_batt_ac_or_dc_nset, self->data_ptr);
}

static PyObject *
Battery_get_batt_auto_gridcharge_max_daily(BatteryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Battery_batt_auto_gridcharge_max_daily_nget, self->data_ptr);
}

static int
Battery_set_batt_auto_gridcharge_max_daily(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Battery_batt_auto_gridcharge_max_daily_nset, self->data_ptr);
}

static PyObject *
Battery_get_batt_calendar_a(BatteryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Battery_batt_calendar_a_nget, self->data_ptr);
}

static int
Battery_set_batt_calendar_a(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Battery_batt_calendar_a_nset, self->data_ptr);
}

static PyObject *
Battery_get_batt_calendar_b(BatteryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Battery_batt_calendar_b_nget, self->data_ptr);
}

static int
Battery_set_batt_calendar_b(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Battery_batt_calendar_b_nset, self->data_ptr);
}

static PyObject *
Battery_get_batt_calendar_c(BatteryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Battery_batt_calendar_c_nget, self->data_ptr);
}

static int
Battery_set_batt_calendar_c(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Battery_batt_calendar_c_nset, self->data_ptr);
}

static PyObject *
Battery_get_batt_calendar_choice(BatteryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Battery_batt_calendar_choice_nget, self->data_ptr);
}

static int
Battery_set_batt_calendar_choice(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Battery_batt_calendar_choice_nset, self->data_ptr);
}

static PyObject *
Battery_get_batt_calendar_lifetime_matrix(BatteryObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Pvsamv1_Battery_batt_calendar_lifetime_matrix_mget, self->data_ptr);
}

static int
Battery_set_batt_calendar_lifetime_matrix(BatteryObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_Pvsamv1_Battery_batt_calendar_lifetime_matrix_mset, self->data_ptr);
}

static PyObject *
Battery_get_batt_calendar_q0(BatteryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Battery_batt_calendar_q0_nget, self->data_ptr);
}

static int
Battery_set_batt_calendar_q0(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Battery_batt_calendar_q0_nset, self->data_ptr);
}

static PyObject *
Battery_get_batt_chem(BatteryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Battery_batt_chem_nget, self->data_ptr);
}

static int
Battery_set_batt_chem(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Battery_batt_chem_nset, self->data_ptr);
}

static PyObject *
Battery_get_batt_computed_bank_capacity(BatteryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Battery_batt_computed_bank_capacity_nget, self->data_ptr);
}

static int
Battery_set_batt_computed_bank_capacity(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Battery_batt_computed_bank_capacity_nset, self->data_ptr);
}

static PyObject *
Battery_get_batt_computed_series(BatteryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Battery_batt_computed_series_nget, self->data_ptr);
}

static int
Battery_set_batt_computed_series(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Battery_batt_computed_series_nset, self->data_ptr);
}

static PyObject *
Battery_get_batt_computed_strings(BatteryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Battery_batt_computed_strings_nget, self->data_ptr);
}

static int
Battery_set_batt_computed_strings(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Battery_batt_computed_strings_nset, self->data_ptr);
}

static PyObject *
Battery_get_batt_current_charge_max(BatteryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Battery_batt_current_charge_max_nget, self->data_ptr);
}

static int
Battery_set_batt_current_charge_max(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Battery_batt_current_charge_max_nset, self->data_ptr);
}

static PyObject *
Battery_get_batt_current_choice(BatteryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Battery_batt_current_choice_nget, self->data_ptr);
}

static int
Battery_set_batt_current_choice(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Battery_batt_current_choice_nset, self->data_ptr);
}

static PyObject *
Battery_get_batt_current_discharge_max(BatteryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Battery_batt_current_discharge_max_nget, self->data_ptr);
}

static int
Battery_set_batt_current_discharge_max(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Battery_batt_current_discharge_max_nset, self->data_ptr);
}

static PyObject *
Battery_get_batt_custom_dispatch(BatteryObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Battery_batt_custom_dispatch_aget, self->data_ptr);
}

static int
Battery_set_batt_custom_dispatch(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Pvsamv1_Battery_batt_custom_dispatch_aset, self->data_ptr);
}

static PyObject *
Battery_get_batt_cycle_cost(BatteryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Battery_batt_cycle_cost_nget, self->data_ptr);
}

static int
Battery_set_batt_cycle_cost(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Battery_batt_cycle_cost_nset, self->data_ptr);
}

static PyObject *
Battery_get_batt_cycle_cost_choice(BatteryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Battery_batt_cycle_cost_choice_nget, self->data_ptr);
}

static int
Battery_set_batt_cycle_cost_choice(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Battery_batt_cycle_cost_choice_nset, self->data_ptr);
}

static PyObject *
Battery_get_batt_dc_ac_efficiency(BatteryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Battery_batt_dc_ac_efficiency_nget, self->data_ptr);
}

static int
Battery_set_batt_dc_ac_efficiency(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Battery_batt_dc_ac_efficiency_nset, self->data_ptr);
}

static PyObject *
Battery_get_batt_dc_dc_efficiency(BatteryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Battery_batt_dc_dc_efficiency_nget, self->data_ptr);
}

static int
Battery_set_batt_dc_dc_efficiency(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Battery_batt_dc_dc_efficiency_nset, self->data_ptr);
}

static PyObject *
Battery_get_batt_dispatch_auto_can_charge(BatteryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Battery_batt_dispatch_auto_can_charge_nget, self->data_ptr);
}

static int
Battery_set_batt_dispatch_auto_can_charge(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Battery_batt_dispatch_auto_can_charge_nset, self->data_ptr);
}

static PyObject *
Battery_get_batt_dispatch_auto_can_clipcharge(BatteryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Battery_batt_dispatch_auto_can_clipcharge_nget, self->data_ptr);
}

static int
Battery_set_batt_dispatch_auto_can_clipcharge(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Battery_batt_dispatch_auto_can_clipcharge_nset, self->data_ptr);
}

static PyObject *
Battery_get_batt_dispatch_auto_can_fuelcellcharge(BatteryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Battery_batt_dispatch_auto_can_fuelcellcharge_nget, self->data_ptr);
}

static int
Battery_set_batt_dispatch_auto_can_fuelcellcharge(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Battery_batt_dispatch_auto_can_fuelcellcharge_nset, self->data_ptr);
}

static PyObject *
Battery_get_batt_dispatch_auto_can_gridcharge(BatteryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Battery_batt_dispatch_auto_can_gridcharge_nget, self->data_ptr);
}

static int
Battery_set_batt_dispatch_auto_can_gridcharge(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Battery_batt_dispatch_auto_can_gridcharge_nset, self->data_ptr);
}

static PyObject *
Battery_get_batt_dispatch_choice(BatteryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Battery_batt_dispatch_choice_nget, self->data_ptr);
}

static int
Battery_set_batt_dispatch_choice(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Battery_batt_dispatch_choice_nset, self->data_ptr);
}

static PyObject *
Battery_get_batt_dispatch_update_frequency_hours(BatteryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Battery_batt_dispatch_update_frequency_hours_nget, self->data_ptr);
}

static int
Battery_set_batt_dispatch_update_frequency_hours(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Battery_batt_dispatch_update_frequency_hours_nset, self->data_ptr);
}

static PyObject *
Battery_get_batt_h_to_ambient(BatteryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Battery_batt_h_to_ambient_nget, self->data_ptr);
}

static int
Battery_set_batt_h_to_ambient(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Battery_batt_h_to_ambient_nset, self->data_ptr);
}

static PyObject *
Battery_get_batt_height(BatteryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Battery_batt_height_nget, self->data_ptr);
}

static int
Battery_set_batt_height(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Battery_batt_height_nset, self->data_ptr);
}

static PyObject *
Battery_get_batt_initial_SOC(BatteryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Battery_batt_initial_SOC_nget, self->data_ptr);
}

static int
Battery_set_batt_initial_SOC(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Battery_batt_initial_SOC_nset, self->data_ptr);
}

static PyObject *
Battery_get_batt_length(BatteryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Battery_batt_length_nget, self->data_ptr);
}

static int
Battery_set_batt_length(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Battery_batt_length_nset, self->data_ptr);
}

static PyObject *
Battery_get_batt_lifetime_matrix(BatteryObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Pvsamv1_Battery_batt_lifetime_matrix_mget, self->data_ptr);
}

static int
Battery_set_batt_lifetime_matrix(BatteryObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_Pvsamv1_Battery_batt_lifetime_matrix_mset, self->data_ptr);
}

static PyObject *
Battery_get_batt_look_ahead_hours(BatteryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Battery_batt_look_ahead_hours_nget, self->data_ptr);
}

static int
Battery_set_batt_look_ahead_hours(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Battery_batt_look_ahead_hours_nset, self->data_ptr);
}

static PyObject *
Battery_get_batt_loss_choice(BatteryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Battery_batt_loss_choice_nget, self->data_ptr);
}

static int
Battery_set_batt_loss_choice(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Battery_batt_loss_choice_nset, self->data_ptr);
}

static PyObject *
Battery_get_batt_losses(BatteryObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Battery_batt_losses_aget, self->data_ptr);
}

static int
Battery_set_batt_losses(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Pvsamv1_Battery_batt_losses_aset, self->data_ptr);
}

static PyObject *
Battery_get_batt_losses_charging(BatteryObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Battery_batt_losses_charging_aget, self->data_ptr);
}

static int
Battery_set_batt_losses_charging(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Pvsamv1_Battery_batt_losses_charging_aset, self->data_ptr);
}

static PyObject *
Battery_get_batt_losses_discharging(BatteryObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Battery_batt_losses_discharging_aget, self->data_ptr);
}

static int
Battery_set_batt_losses_discharging(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Pvsamv1_Battery_batt_losses_discharging_aset, self->data_ptr);
}

static PyObject *
Battery_get_batt_losses_idle(BatteryObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Battery_batt_losses_idle_aget, self->data_ptr);
}

static int
Battery_set_batt_losses_idle(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Pvsamv1_Battery_batt_losses_idle_aset, self->data_ptr);
}

static PyObject *
Battery_get_batt_mass(BatteryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Battery_batt_mass_nget, self->data_ptr);
}

static int
Battery_set_batt_mass(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Battery_batt_mass_nset, self->data_ptr);
}

static PyObject *
Battery_get_batt_maximum_SOC(BatteryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Battery_batt_maximum_SOC_nget, self->data_ptr);
}

static int
Battery_set_batt_maximum_SOC(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Battery_batt_maximum_SOC_nset, self->data_ptr);
}

static PyObject *
Battery_get_batt_meter_position(BatteryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Battery_batt_meter_position_nget, self->data_ptr);
}

static int
Battery_set_batt_meter_position(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Battery_batt_meter_position_nset, self->data_ptr);
}

static PyObject *
Battery_get_batt_minimum_SOC(BatteryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Battery_batt_minimum_SOC_nget, self->data_ptr);
}

static int
Battery_set_batt_minimum_SOC(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Battery_batt_minimum_SOC_nset, self->data_ptr);
}

static PyObject *
Battery_get_batt_minimum_modetime(BatteryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Battery_batt_minimum_modetime_nget, self->data_ptr);
}

static int
Battery_set_batt_minimum_modetime(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Battery_batt_minimum_modetime_nset, self->data_ptr);
}

static PyObject *
Battery_get_batt_power_charge_max(BatteryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Battery_batt_power_charge_max_nget, self->data_ptr);
}

static int
Battery_set_batt_power_charge_max(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Battery_batt_power_charge_max_nset, self->data_ptr);
}

static PyObject *
Battery_get_batt_power_discharge_max(BatteryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Battery_batt_power_discharge_max_nget, self->data_ptr);
}

static int
Battery_set_batt_power_discharge_max(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Battery_batt_power_discharge_max_nset, self->data_ptr);
}

static PyObject *
Battery_get_batt_pv_clipping_forecast(BatteryObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Battery_batt_pv_clipping_forecast_aget, self->data_ptr);
}

static int
Battery_set_batt_pv_clipping_forecast(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Pvsamv1_Battery_batt_pv_clipping_forecast_aset, self->data_ptr);
}

static PyObject *
Battery_get_batt_pv_dc_forecast(BatteryObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Battery_batt_pv_dc_forecast_aget, self->data_ptr);
}

static int
Battery_set_batt_pv_dc_forecast(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Pvsamv1_Battery_batt_pv_dc_forecast_aset, self->data_ptr);
}

static PyObject *
Battery_get_batt_replacement_capacity(BatteryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Battery_batt_replacement_capacity_nget, self->data_ptr);
}

static int
Battery_set_batt_replacement_capacity(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Battery_batt_replacement_capacity_nset, self->data_ptr);
}

static PyObject *
Battery_get_batt_replacement_option(BatteryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Battery_batt_replacement_option_nget, self->data_ptr);
}

static int
Battery_set_batt_replacement_option(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Battery_batt_replacement_option_nset, self->data_ptr);
}

static PyObject *
Battery_get_batt_replacement_schedule(BatteryObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Battery_batt_replacement_schedule_aget, self->data_ptr);
}

static int
Battery_set_batt_replacement_schedule(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Pvsamv1_Battery_batt_replacement_schedule_aset, self->data_ptr);
}

static PyObject *
Battery_get_batt_resistance(BatteryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Battery_batt_resistance_nget, self->data_ptr);
}

static int
Battery_set_batt_resistance(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Battery_batt_resistance_nset, self->data_ptr);
}

static PyObject *
Battery_get_batt_room_temperature_celsius(BatteryObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Battery_batt_room_temperature_celsius_aget, self->data_ptr);
}

static int
Battery_set_batt_room_temperature_celsius(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Pvsamv1_Battery_batt_room_temperature_celsius_aset, self->data_ptr);
}

static PyObject *
Battery_get_batt_target_choice(BatteryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Battery_batt_target_choice_nget, self->data_ptr);
}

static int
Battery_set_batt_target_choice(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Battery_batt_target_choice_nset, self->data_ptr);
}

static PyObject *
Battery_get_batt_target_power(BatteryObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Battery_batt_target_power_aget, self->data_ptr);
}

static int
Battery_set_batt_target_power(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Pvsamv1_Battery_batt_target_power_aset, self->data_ptr);
}

static PyObject *
Battery_get_batt_target_power_monthly(BatteryObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Battery_batt_target_power_monthly_aget, self->data_ptr);
}

static int
Battery_set_batt_target_power_monthly(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Pvsamv1_Battery_batt_target_power_monthly_aset, self->data_ptr);
}

static PyObject *
Battery_get_batt_voltage_choice(BatteryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Battery_batt_voltage_choice_nget, self->data_ptr);
}

static int
Battery_set_batt_voltage_choice(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Battery_batt_voltage_choice_nset, self->data_ptr);
}

static PyObject *
Battery_get_batt_voltage_matrix(BatteryObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Pvsamv1_Battery_batt_voltage_matrix_mget, self->data_ptr);
}

static int
Battery_set_batt_voltage_matrix(BatteryObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_Pvsamv1_Battery_batt_voltage_matrix_mset, self->data_ptr);
}

static PyObject *
Battery_get_batt_width(BatteryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Battery_batt_width_nget, self->data_ptr);
}

static int
Battery_set_batt_width(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Battery_batt_width_nset, self->data_ptr);
}

static PyObject *
Battery_get_cap_vs_temp(BatteryObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Pvsamv1_Battery_cap_vs_temp_mget, self->data_ptr);
}

static int
Battery_set_cap_vs_temp(BatteryObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_Pvsamv1_Battery_cap_vs_temp_mset, self->data_ptr);
}

static PyObject *
Battery_get_dispatch_manual_charge(BatteryObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Battery_dispatch_manual_charge_aget, self->data_ptr);
}

static int
Battery_set_dispatch_manual_charge(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Pvsamv1_Battery_dispatch_manual_charge_aset, self->data_ptr);
}

static PyObject *
Battery_get_dispatch_manual_discharge(BatteryObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Battery_dispatch_manual_discharge_aget, self->data_ptr);
}

static int
Battery_set_dispatch_manual_discharge(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Pvsamv1_Battery_dispatch_manual_discharge_aset, self->data_ptr);
}

static PyObject *
Battery_get_dispatch_manual_gridcharge(BatteryObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Battery_dispatch_manual_gridcharge_aget, self->data_ptr);
}

static int
Battery_set_dispatch_manual_gridcharge(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Pvsamv1_Battery_dispatch_manual_gridcharge_aset, self->data_ptr);
}

static PyObject *
Battery_get_dispatch_manual_percent_discharge(BatteryObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Battery_dispatch_manual_percent_discharge_aget, self->data_ptr);
}

static int
Battery_set_dispatch_manual_percent_discharge(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Pvsamv1_Battery_dispatch_manual_percent_discharge_aset, self->data_ptr);
}

static PyObject *
Battery_get_dispatch_manual_percent_gridcharge(BatteryObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Battery_dispatch_manual_percent_gridcharge_aget, self->data_ptr);
}

static int
Battery_set_dispatch_manual_percent_gridcharge(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Pvsamv1_Battery_dispatch_manual_percent_gridcharge_aset, self->data_ptr);
}

static PyObject *
Battery_get_dispatch_manual_sched(BatteryObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Pvsamv1_Battery_dispatch_manual_sched_mget, self->data_ptr);
}

static int
Battery_set_dispatch_manual_sched(BatteryObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_Pvsamv1_Battery_dispatch_manual_sched_mset, self->data_ptr);
}

static PyObject *
Battery_get_dispatch_manual_sched_weekend(BatteryObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Pvsamv1_Battery_dispatch_manual_sched_weekend_mget, self->data_ptr);
}

static int
Battery_set_dispatch_manual_sched_weekend(BatteryObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_Pvsamv1_Battery_dispatch_manual_sched_weekend_mset, self->data_ptr);
}

static PyObject *
Battery_get_en_batt(BatteryObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Battery_en_batt_nget, self->data_ptr);
}

static int
Battery_set_en_batt(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Battery_en_batt_nset, self->data_ptr);
}

static PyObject *
Battery_get_load(BatteryObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Battery_load_aget, self->data_ptr);
}

static int
Battery_set_load(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Pvsamv1_Battery_load_aset, self->data_ptr);
}

static PyObject *
Battery_get_om_replacement_cost1(BatteryObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Battery_om_replacement_cost1_aget, self->data_ptr);
}

static int
Battery_set_om_replacement_cost1(BatteryObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Pvsamv1_Battery_om_replacement_cost1_aset, self->data_ptr);
}

static PyGetSetDef Battery_getset[] = {
{"LeadAcid_q10_computed", (getter)Battery_get_LeadAcid_q10_computed,(setter)Battery_set_LeadAcid_q10_computed,
	PyDoc_STR("*float*: Capacity at 10-hour discharge rate [Ah]"),
 	NULL},
{"LeadAcid_q20_computed", (getter)Battery_get_LeadAcid_q20_computed,(setter)Battery_set_LeadAcid_q20_computed,
	PyDoc_STR("*float*: Capacity at 20-hour discharge rate [Ah]"),
 	NULL},
{"LeadAcid_qn_computed", (getter)Battery_get_LeadAcid_qn_computed,(setter)Battery_set_LeadAcid_qn_computed,
	PyDoc_STR("*float*: Capacity at discharge rate for n-hour rate [Ah]"),
 	NULL},
{"LeadAcid_tn", (getter)Battery_get_LeadAcid_tn,(setter)Battery_set_LeadAcid_tn,
	PyDoc_STR("*float*: Time to discharge [h]"),
 	NULL},
{"batt_C_rate", (getter)Battery_get_batt_C_rate,(setter)Battery_set_batt_C_rate,
	PyDoc_STR("*float*: Rate at which voltage vs. capacity curve input"),
 	NULL},
{"batt_Cp", (getter)Battery_get_batt_Cp,(setter)Battery_set_batt_Cp,
	PyDoc_STR("*float*: Battery specific heat capacity [J/KgK]"),
 	NULL},
{"batt_Qexp", (getter)Battery_get_batt_Qexp,(setter)Battery_set_batt_Qexp,
	PyDoc_STR("*float*: Cell capacity at end of exponential zone [Ah]"),
 	NULL},
{"batt_Qfull", (getter)Battery_get_batt_Qfull,(setter)Battery_set_batt_Qfull,
	PyDoc_STR("*float*: Fully charged cell capacity [Ah]"),
 	NULL},
{"batt_Qfull_flow", (getter)Battery_get_batt_Qfull_flow,(setter)Battery_set_batt_Qfull_flow,
	PyDoc_STR("*float*: Fully charged flow battery capacity [Ah]"),
 	NULL},
{"batt_Qnom", (getter)Battery_get_batt_Qnom,(setter)Battery_set_batt_Qnom,
	PyDoc_STR("*float*: Cell capacity at end of nominal zone [Ah]"),
 	NULL},
{"batt_Vexp", (getter)Battery_get_batt_Vexp,(setter)Battery_set_batt_Vexp,
	PyDoc_STR("*float*: Cell voltage at end of exponential zone [V]"),
 	NULL},
{"batt_Vfull", (getter)Battery_get_batt_Vfull,(setter)Battery_set_batt_Vfull,
	PyDoc_STR("*float*: Fully charged cell voltage [V]"),
 	NULL},
{"batt_Vnom", (getter)Battery_get_batt_Vnom,(setter)Battery_set_batt_Vnom,
	PyDoc_STR("*float*: Cell voltage at end of nominal zone [V]"),
 	NULL},
{"batt_Vnom_default", (getter)Battery_get_batt_Vnom_default,(setter)Battery_set_batt_Vnom_default,
	PyDoc_STR("*float*: Default nominal cell voltage [V]"),
 	NULL},
{"batt_ac_dc_efficiency", (getter)Battery_get_batt_ac_dc_efficiency,(setter)Battery_set_batt_ac_dc_efficiency,
	PyDoc_STR("*float*: Inverter AC to battery DC efficiency"),
 	NULL},
{"batt_ac_or_dc", (getter)Battery_get_batt_ac_or_dc,(setter)Battery_set_batt_ac_or_dc,
	PyDoc_STR("*float*: Battery interconnection (AC or DC)\n\n*Options*: 0=DC_Connected,1=AC_Connected"),
 	NULL},
{"batt_auto_gridcharge_max_daily", (getter)Battery_get_batt_auto_gridcharge_max_daily,(setter)Battery_set_batt_auto_gridcharge_max_daily,
	PyDoc_STR("*float*: Allowed grid charging percent per day for automated dispatch [kW]"),
 	NULL},
{"batt_calendar_a", (getter)Battery_get_batt_calendar_a,(setter)Battery_set_batt_calendar_a,
	PyDoc_STR("*float*: Calendar life model coefficient [1/sqrt(day)]"),
 	NULL},
{"batt_calendar_b", (getter)Battery_get_batt_calendar_b,(setter)Battery_set_batt_calendar_b,
	PyDoc_STR("*float*: Calendar life model coefficient [K]"),
 	NULL},
{"batt_calendar_c", (getter)Battery_get_batt_calendar_c,(setter)Battery_set_batt_calendar_c,
	PyDoc_STR("*float*: Calendar life model coefficient [K]"),
 	NULL},
{"batt_calendar_choice", (getter)Battery_get_batt_calendar_choice,(setter)Battery_set_batt_calendar_choice,
	PyDoc_STR("*float*: Calendar life degradation input option [0/1/2]\n\n*Options*: 0=NoCalendarDegradation,1=LithiomIonModel,2=InputLossTable"),
 	NULL},
{"batt_calendar_lifetime_matrix", (getter)Battery_get_batt_calendar_lifetime_matrix,(setter)Battery_set_batt_calendar_lifetime_matrix,
	PyDoc_STR("*sequence[sequence]*: Days vs capacity"),
 	NULL},
{"batt_calendar_q0", (getter)Battery_get_batt_calendar_q0,(setter)Battery_set_batt_calendar_q0,
	PyDoc_STR("*float*: Calendar life model initial capacity cofficient"),
 	NULL},
{"batt_chem", (getter)Battery_get_batt_chem,(setter)Battery_set_batt_chem,
	PyDoc_STR("*float*: Battery chemistry\n\n*Options*: 0=LeadAcid,1=LiIon"),
 	NULL},
{"batt_computed_bank_capacity", (getter)Battery_get_batt_computed_bank_capacity,(setter)Battery_set_batt_computed_bank_capacity,
	PyDoc_STR("*float*: Computed bank capacity [kWh]"),
 	NULL},
{"batt_computed_series", (getter)Battery_get_batt_computed_series,(setter)Battery_set_batt_computed_series,
	PyDoc_STR("*float*: Number of cells in series"),
 	NULL},
{"batt_computed_strings", (getter)Battery_get_batt_computed_strings,(setter)Battery_set_batt_computed_strings,
	PyDoc_STR("*float*: Number of strings of cells"),
 	NULL},
{"batt_current_charge_max", (getter)Battery_get_batt_current_charge_max,(setter)Battery_set_batt_current_charge_max,
	PyDoc_STR("*float*: Maximum charge current [A]"),
 	NULL},
{"batt_current_choice", (getter)Battery_get_batt_current_choice,(setter)Battery_set_batt_current_choice,
	PyDoc_STR("*float*: Limit cells by current or power"),
 	NULL},
{"batt_current_discharge_max", (getter)Battery_get_batt_current_discharge_max,(setter)Battery_set_batt_current_discharge_max,
	PyDoc_STR("*float*: Maximum discharge current [A]"),
 	NULL},
{"batt_custom_dispatch", (getter)Battery_get_batt_custom_dispatch,(setter)Battery_set_batt_custom_dispatch,
	PyDoc_STR("*sequence*: Custom battery power for every time step [kW]\n\n*Required*: set to 1&batt_dispatch_choice=3 if not provided."),
 	NULL},
{"batt_cycle_cost", (getter)Battery_get_batt_cycle_cost,(setter)Battery_set_batt_cycle_cost,
	PyDoc_STR("*float*: Input battery cycle costs [$/cycle-kWh]"),
 	NULL},
{"batt_cycle_cost_choice", (getter)Battery_get_batt_cycle_cost_choice,(setter)Battery_set_batt_cycle_cost_choice,
	PyDoc_STR("*float*: Use SAM model for cycle costs or input custom [0/1]\n\n*Options*: 0=UseCostModel,1=InputCost"),
 	NULL},
{"batt_dc_ac_efficiency", (getter)Battery_get_batt_dc_ac_efficiency,(setter)Battery_set_batt_dc_ac_efficiency,
	PyDoc_STR("*float*: Battery DC to AC efficiency"),
 	NULL},
{"batt_dc_dc_efficiency", (getter)Battery_get_batt_dc_dc_efficiency,(setter)Battery_set_batt_dc_dc_efficiency,
	PyDoc_STR("*float*: PV DC to battery DC efficiency"),
 	NULL},
{"batt_dispatch_auto_can_charge", (getter)Battery_get_batt_dispatch_auto_can_charge,(setter)Battery_set_batt_dispatch_auto_can_charge,
	PyDoc_STR("*float*: PV charging allowed for automated dispatch? [kW]"),
 	NULL},
{"batt_dispatch_auto_can_clipcharge", (getter)Battery_get_batt_dispatch_auto_can_clipcharge,(setter)Battery_set_batt_dispatch_auto_can_clipcharge,
	PyDoc_STR("*float*: Battery can charge from clipped PV for automated dispatch? [kW]"),
 	NULL},
{"batt_dispatch_auto_can_fuelcellcharge", (getter)Battery_get_batt_dispatch_auto_can_fuelcellcharge,(setter)Battery_set_batt_dispatch_auto_can_fuelcellcharge,
	PyDoc_STR("*float*: Charging from fuel cell allowed for automated dispatch? [kW]"),
 	NULL},
{"batt_dispatch_auto_can_gridcharge", (getter)Battery_get_batt_dispatch_auto_can_gridcharge,(setter)Battery_set_batt_dispatch_auto_can_gridcharge,
	PyDoc_STR("*float*: Grid charging allowed for automated dispatch? [kW]"),
 	NULL},
{"batt_dispatch_choice", (getter)Battery_get_batt_dispatch_choice,(setter)Battery_set_batt_dispatch_choice,
	PyDoc_STR("*float*: Battery dispatch algorithm [0/1/2/3/4]\n\n*Options*: If behind the meter: 0=PeakShavingLookAhead,1=PeakShavingLookBehind,2=InputGridTarget,3=InputBatteryPower,4=ManualDispatch, if front of meter: 0=AutomatedLookAhead,1=AutomatedLookBehind,2=AutomatedInputForecast,3=InputBatteryPower,4=ManualDispatch\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"batt_dispatch_update_frequency_hours", (getter)Battery_get_batt_dispatch_update_frequency_hours,(setter)Battery_set_batt_dispatch_update_frequency_hours,
	PyDoc_STR("*float*: Frequency to update the look-ahead dispatch [hours]"),
 	NULL},
{"batt_h_to_ambient", (getter)Battery_get_batt_h_to_ambient,(setter)Battery_set_batt_h_to_ambient,
	PyDoc_STR("*float*: Heat transfer between battery and environment [W/m2K]"),
 	NULL},
{"batt_height", (getter)Battery_get_batt_height,(setter)Battery_set_batt_height,
	PyDoc_STR("*float*: Battery height [m]"),
 	NULL},
{"batt_initial_SOC", (getter)Battery_get_batt_initial_SOC,(setter)Battery_set_batt_initial_SOC,
	PyDoc_STR("*float*: Initial state-of-charge [%]"),
 	NULL},
{"batt_length", (getter)Battery_get_batt_length,(setter)Battery_set_batt_length,
	PyDoc_STR("*float*: Battery length [m]"),
 	NULL},
{"batt_lifetime_matrix", (getter)Battery_get_batt_lifetime_matrix,(setter)Battery_set_batt_lifetime_matrix,
	PyDoc_STR("*sequence[sequence]*: Cycles vs capacity at different depths-of-discharge"),
 	NULL},
{"batt_look_ahead_hours", (getter)Battery_get_batt_look_ahead_hours,(setter)Battery_set_batt_look_ahead_hours,
	PyDoc_STR("*float*: Hours to look ahead in automated dispatch [hours]"),
 	NULL},
{"batt_loss_choice", (getter)Battery_get_batt_loss_choice,(setter)Battery_set_batt_loss_choice,
	PyDoc_STR("*float*: Loss power input option [0/1]\n\n*Options*: 0=Monthly,1=TimeSeries\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"batt_losses", (getter)Battery_get_batt_losses,(setter)Battery_set_batt_losses,
	PyDoc_STR("*sequence*: Battery system losses at each timestep [kW]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"batt_losses_charging", (getter)Battery_get_batt_losses_charging,(setter)Battery_set_batt_losses_charging,
	PyDoc_STR("*sequence*: Battery system losses when charging [kW]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"batt_losses_discharging", (getter)Battery_get_batt_losses_discharging,(setter)Battery_set_batt_losses_discharging,
	PyDoc_STR("*sequence*: Battery system losses when discharging [kW]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"batt_losses_idle", (getter)Battery_get_batt_losses_idle,(setter)Battery_set_batt_losses_idle,
	PyDoc_STR("*sequence*: Battery system losses when idle [kW]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"batt_mass", (getter)Battery_get_batt_mass,(setter)Battery_set_batt_mass,
	PyDoc_STR("*float*: Battery mass [kg]"),
 	NULL},
{"batt_maximum_SOC", (getter)Battery_get_batt_maximum_SOC,(setter)Battery_set_batt_maximum_SOC,
	PyDoc_STR("*float*: Maximum allowed state-of-charge [%]"),
 	NULL},
{"batt_meter_position", (getter)Battery_get_batt_meter_position,(setter)Battery_set_batt_meter_position,
	PyDoc_STR("*float*: Position of battery relative to electric meter\n\n*Options*: 0=BehindTheMeter,1=FrontOfMeter"),
 	NULL},
{"batt_minimum_SOC", (getter)Battery_get_batt_minimum_SOC,(setter)Battery_set_batt_minimum_SOC,
	PyDoc_STR("*float*: Minimum allowed state-of-charge [%]"),
 	NULL},
{"batt_minimum_modetime", (getter)Battery_get_batt_minimum_modetime,(setter)Battery_set_batt_minimum_modetime,
	PyDoc_STR("*float*: Minimum time at charge state [min]"),
 	NULL},
{"batt_power_charge_max", (getter)Battery_get_batt_power_charge_max,(setter)Battery_set_batt_power_charge_max,
	PyDoc_STR("*float*: Maximum charge power [kW]"),
 	NULL},
{"batt_power_discharge_max", (getter)Battery_get_batt_power_discharge_max,(setter)Battery_set_batt_power_discharge_max,
	PyDoc_STR("*float*: Maximum discharge power [kW]"),
 	NULL},
{"batt_pv_clipping_forecast", (getter)Battery_get_batt_pv_clipping_forecast,(setter)Battery_set_batt_pv_clipping_forecast,
	PyDoc_STR("*sequence*: PV clipping forecast [kW]\n\n*Required*: set to 1&batt_meter_position=1&batt_dispatch_choice=2 if not provided."),
 	NULL},
{"batt_pv_dc_forecast", (getter)Battery_get_batt_pv_dc_forecast,(setter)Battery_set_batt_pv_dc_forecast,
	PyDoc_STR("*sequence*: PV dc power forecast [kW]\n\n*Required*: set to 1&batt_meter_position=1&batt_dispatch_choice=2 if not provided."),
 	NULL},
{"batt_replacement_capacity", (getter)Battery_get_batt_replacement_capacity,(setter)Battery_set_batt_replacement_capacity,
	PyDoc_STR("*float*: Capacity degradation at which to replace battery [%]"),
 	NULL},
{"batt_replacement_option", (getter)Battery_get_batt_replacement_option,(setter)Battery_set_batt_replacement_option,
	PyDoc_STR("*float*: Enable battery replacement? [0=none,1=capacity based,2=user schedule]\n\n*Constraints*: INTEGER,MIN=0,MAX=2\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"batt_replacement_schedule", (getter)Battery_get_batt_replacement_schedule,(setter)Battery_set_batt_replacement_schedule,
	PyDoc_STR("*sequence*: Battery bank replacements per year (user specified) [number/year]\n\n*Required*: set to 2 if not provided."),
 	NULL},
{"batt_resistance", (getter)Battery_get_batt_resistance,(setter)Battery_set_batt_resistance,
	PyDoc_STR("*float*: Internal resistance [Ohm]"),
 	NULL},
{"batt_room_temperature_celsius", (getter)Battery_get_batt_room_temperature_celsius,(setter)Battery_set_batt_room_temperature_celsius,
	PyDoc_STR("*sequence*: Temperature of storage room [C]"),
 	NULL},
{"batt_target_choice", (getter)Battery_get_batt_target_choice,(setter)Battery_set_batt_target_choice,
	PyDoc_STR("*float*: Target power input option [0/1]\n\n*Options*: 0=InputMonthlyTarget,1=InputFullTimeSeries\n\n*Required*: set to 1&batt_meter_position=0&batt_dispatch_choice=2 if not provided."),
 	NULL},
{"batt_target_power", (getter)Battery_get_batt_target_power,(setter)Battery_set_batt_target_power,
	PyDoc_STR("*sequence*: Grid target power for every time step [kW]\n\n*Required*: set to 1&batt_meter_position=0&batt_dispatch_choice=2 if not provided."),
 	NULL},
{"batt_target_power_monthly", (getter)Battery_get_batt_target_power_monthly,(setter)Battery_set_batt_target_power_monthly,
	PyDoc_STR("*sequence*: Grid target power on monthly basis [kW]\n\n*Required*: set to 1&batt_meter_position=0&batt_dispatch_choice=2 if not provided."),
 	NULL},
{"batt_voltage_choice", (getter)Battery_get_batt_voltage_choice,(setter)Battery_set_batt_voltage_choice,
	PyDoc_STR("*float*: Battery voltage input option [0/1]\n\n*Options*: 0=UseVoltageModel,1=InputVoltageTable\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"batt_voltage_matrix", (getter)Battery_get_batt_voltage_matrix,(setter)Battery_set_batt_voltage_matrix,
	PyDoc_STR("*sequence[sequence]*: Battery voltage vs. depth-of-discharge"),
 	NULL},
{"batt_width", (getter)Battery_get_batt_width,(setter)Battery_set_batt_width,
	PyDoc_STR("*float*: Battery width [m]"),
 	NULL},
{"cap_vs_temp", (getter)Battery_get_cap_vs_temp,(setter)Battery_set_cap_vs_temp,
	PyDoc_STR("*sequence[sequence]*: Effective capacity as function of temperature [C,%]"),
 	NULL},
{"dispatch_manual_charge", (getter)Battery_get_dispatch_manual_charge,(setter)Battery_set_dispatch_manual_charge,
	PyDoc_STR("*sequence*: Periods 1-6 charging from system allowed?\n\n*Required*: set to 1&batt_dispatch_choice=4 if not provided."),
 	NULL},
{"dispatch_manual_discharge", (getter)Battery_get_dispatch_manual_discharge,(setter)Battery_set_dispatch_manual_discharge,
	PyDoc_STR("*sequence*: Periods 1-6 discharging allowed?\n\n*Required*: set to 1&batt_dispatch_choice=4 if not provided."),
 	NULL},
{"dispatch_manual_gridcharge", (getter)Battery_get_dispatch_manual_gridcharge,(setter)Battery_set_dispatch_manual_gridcharge,
	PyDoc_STR("*sequence*: Periods 1-6 grid charging allowed?\n\n*Required*: set to 1&batt_dispatch_choice=4 if not provided."),
 	NULL},
{"dispatch_manual_percent_discharge", (getter)Battery_get_dispatch_manual_percent_discharge,(setter)Battery_set_dispatch_manual_percent_discharge,
	PyDoc_STR("*sequence*: Periods 1-6 discharge percent [%]\n\n*Required*: set to 1&batt_dispatch_choice=4 if not provided."),
 	NULL},
{"dispatch_manual_percent_gridcharge", (getter)Battery_get_dispatch_manual_percent_gridcharge,(setter)Battery_set_dispatch_manual_percent_gridcharge,
	PyDoc_STR("*sequence*: Periods 1-6 gridcharge percent [%]\n\n*Required*: set to 1&batt_dispatch_choice=4 if not provided."),
 	NULL},
{"dispatch_manual_sched", (getter)Battery_get_dispatch_manual_sched,(setter)Battery_set_dispatch_manual_sched,
	PyDoc_STR("*sequence[sequence]*: Battery dispatch schedule for weekday\n\n*Required*: set to 1&batt_dispatch_choice=4 if not provided."),
 	NULL},
{"dispatch_manual_sched_weekend", (getter)Battery_get_dispatch_manual_sched_weekend,(setter)Battery_set_dispatch_manual_sched_weekend,
	PyDoc_STR("*sequence[sequence]*: Battery dispatch schedule for weekend\n\n*Required*: set to 1&batt_dispatch_choice=4 if not provided."),
 	NULL},
{"en_batt", (getter)Battery_get_en_batt,(setter)Battery_set_en_batt,
	PyDoc_STR("*float*: Enable battery storage model [0/1]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"load", (getter)Battery_get_load,(setter)Battery_set_load,
	PyDoc_STR("*sequence*: Electricity load (year 1) [kW]\n\n*Required*: False"),
 	NULL},
{"om_replacement_cost1", (getter)Battery_get_om_replacement_cost1,(setter)Battery_set_om_replacement_cost1,
	PyDoc_STR("*sequence*: Cost to replace battery per kWh [$/kWh]"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject Battery_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Pvsamv1.Battery",             /*tp_name*/
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
	 * Simulation Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Pvsamv1   data_ptr;
} SimulationObject;

static PyTypeObject Simulation_Type;

static PyObject *
Simulation_new(SAM_Pvsamv1 data_ptr)
{
	PyObject* new_obj = Simulation_Type.tp_alloc(&Simulation_Type,0);

	SimulationObject* Simulation_obj = (SimulationObject*)new_obj;

	Simulation_obj->data_ptr = data_ptr;

	return new_obj;
}

/* Simulation methods */

static PyObject *
Simulation_assign(SimulationObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Pvsamv1", "Simulation")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
Simulation_export(SimulationObject *self, PyObject *args)
{
	PyTypeObject* tp = &Simulation_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef Simulation_methods[] = {
		{"assign",            (PyCFunction)Simulation_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``Simulation_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)Simulation_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
Simulation_get_analysis_period(SimulationObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Simulation_analysis_period_nget, self->data_ptr);
}

static int
Simulation_set_analysis_period(SimulationObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Simulation_analysis_period_nset, self->data_ptr);
}

static PyObject *
Simulation_get_system_use_lifetime_output(SimulationObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Simulation_system_use_lifetime_output_nget, self->data_ptr);
}

static int
Simulation_set_system_use_lifetime_output(SimulationObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Simulation_system_use_lifetime_output_nset, self->data_ptr);
}

static PyGetSetDef Simulation_getset[] = {
{"analysis_period", (getter)Simulation_get_analysis_period,(setter)Simulation_set_analysis_period,
	PyDoc_STR("*float*: Lifetime analysis period [years]\n\n*Info*: The number of years in the simulation\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"system_use_lifetime_output", (getter)Simulation_get_system_use_lifetime_output,(setter)Simulation_set_system_use_lifetime_output,
	PyDoc_STR("*float*: PV lifetime simulation [0/1]\n\n*Options*: 0=SingleYearRepeated,1=RunEveryYear\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject Simulation_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Pvsamv1.Simulation",             /*tp_name*/
		sizeof(SimulationObject),          /*tp_basicsize*/
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
		Simulation_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		Simulation_getset,          /*tp_getset*/
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
	 * Common Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Pvsamv1   data_ptr;
} CommonObject;

static PyTypeObject Common_Type;

static PyObject *
Common_new(SAM_Pvsamv1 data_ptr)
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

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Pvsamv1", "Common")){
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
Common_get_inverter_model(CommonObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Common_inverter_model_nget, self->data_ptr);
}

static int
Common_set_inverter_model(CommonObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_Common_inverter_model_nset, self->data_ptr);
}

static PyGetSetDef Common_getset[] = {
{"inverter_model", (getter)Common_get_inverter_model,(setter)Common_set_inverter_model,
	PyDoc_STR("*float*: Inverter model specifier\n\n*Options*: 0=cec,1=datasheet,2=partload,3=coefficientgenerator,4=generic\n\n*Constraints*: INTEGER,MIN=0,MAX=4"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject Common_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Pvsamv1.Common",             /*tp_name*/
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
	 * PV Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Pvsamv1   data_ptr;
} PVObject;

static PyTypeObject PV_Type;

static PyObject *
PV_new(SAM_Pvsamv1 data_ptr)
{
	PyObject* new_obj = PV_Type.tp_alloc(&PV_Type,0);

	PVObject* PV_obj = (PVObject*)new_obj;

	PV_obj->data_ptr = data_ptr;

	return new_obj;
}

/* PV methods */

static PyObject *
PV_assign(PVObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Pvsamv1", "PV")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
PV_export(PVObject *self, PyObject *args)
{
	PyTypeObject* tp = &PV_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef PV_methods[] = {
		{"assign",            (PyCFunction)PV_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``PV_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)PV_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
PV_get_dcoptimizer_loss(PVObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_PV_dcoptimizer_loss_nget, self->data_ptr);
}

static int
PV_set_dcoptimizer_loss(PVObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_PV_dcoptimizer_loss_nset, self->data_ptr);
}

static PyGetSetDef PV_getset[] = {
{"dcoptimizer_loss", (getter)PV_get_dcoptimizer_loss,(setter)PV_set_dcoptimizer_loss,
	PyDoc_STR("*float*: PV loss in DC/DC w/MPPT conversion"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject PV_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Pvsamv1.PV",             /*tp_name*/
		sizeof(PVObject),          /*tp_basicsize*/
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
		PV_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		PV_getset,          /*tp_getset*/
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
	 * FuelCell Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Pvsamv1   data_ptr;
} FuelCellObject;

static PyTypeObject FuelCell_Type;

static PyObject *
FuelCell_new(SAM_Pvsamv1 data_ptr)
{
	PyObject* new_obj = FuelCell_Type.tp_alloc(&FuelCell_Type,0);

	FuelCellObject* FuelCell_obj = (FuelCellObject*)new_obj;

	FuelCell_obj->data_ptr = data_ptr;

	return new_obj;
}

/* FuelCell methods */

static PyObject *
FuelCell_assign(FuelCellObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Pvsamv1", "FuelCell")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
FuelCell_export(FuelCellObject *self, PyObject *args)
{
	PyTypeObject* tp = &FuelCell_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef FuelCell_methods[] = {
		{"assign",            (PyCFunction)FuelCell_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``FuelCell_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)FuelCell_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
FuelCell_get_dispatch_manual_fuelcellcharge(FuelCellObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_FuelCell_dispatch_manual_fuelcellcharge_aget, self->data_ptr);
}

static int
FuelCell_set_dispatch_manual_fuelcellcharge(FuelCellObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Pvsamv1_FuelCell_dispatch_manual_fuelcellcharge_aset, self->data_ptr);
}

static PyObject *
FuelCell_get_fuelcell_power(FuelCellObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_FuelCell_fuelcell_power_aget, self->data_ptr);
}

static int
FuelCell_set_fuelcell_power(FuelCellObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Pvsamv1_FuelCell_fuelcell_power_aset, self->data_ptr);
}

static PyGetSetDef FuelCell_getset[] = {
{"dispatch_manual_fuelcellcharge", (getter)FuelCell_get_dispatch_manual_fuelcellcharge,(setter)FuelCell_set_dispatch_manual_fuelcellcharge,
	PyDoc_STR("*sequence*: Periods 1-6 charging from fuel cell allowed?"),
 	NULL},
{"fuelcell_power", (getter)FuelCell_get_fuelcell_power,(setter)FuelCell_set_fuelcell_power,
	PyDoc_STR("*sequence*: Electricity from fuel cell [kW]"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject FuelCell_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Pvsamv1.FuelCell",             /*tp_name*/
		sizeof(FuelCellObject),          /*tp_basicsize*/
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
		FuelCell_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		FuelCell_getset,          /*tp_getset*/
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
	 * ElectricityRate Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Pvsamv1   data_ptr;
} ElectricityRateObject;

static PyTypeObject ElectricityRate_Type;

static PyObject *
ElectricityRate_new(SAM_Pvsamv1 data_ptr)
{
	PyObject* new_obj = ElectricityRate_Type.tp_alloc(&ElectricityRate_Type,0);

	ElectricityRateObject* ElectricityRate_obj = (ElectricityRateObject*)new_obj;

	ElectricityRate_obj->data_ptr = data_ptr;

	return new_obj;
}

/* ElectricityRate methods */

static PyObject *
ElectricityRate_assign(ElectricityRateObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Pvsamv1", "ElectricityRate")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
ElectricityRate_export(ElectricityRateObject *self, PyObject *args)
{
	PyTypeObject* tp = &ElectricityRate_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef ElectricityRate_methods[] = {
		{"assign",            (PyCFunction)ElectricityRate_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``ElectricityRate_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)ElectricityRate_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
ElectricityRate_get_en_electricity_rates(ElectricityRateObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_ElectricityRate_en_electricity_rates_nget, self->data_ptr);
}

static int
ElectricityRate_set_en_electricity_rates(ElectricityRateObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_ElectricityRate_en_electricity_rates_nset, self->data_ptr);
}

static PyObject *
ElectricityRate_get_ur_ec_sched_weekday(ElectricityRateObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Pvsamv1_ElectricityRate_ur_ec_sched_weekday_mget, self->data_ptr);
}

static int
ElectricityRate_set_ur_ec_sched_weekday(ElectricityRateObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_Pvsamv1_ElectricityRate_ur_ec_sched_weekday_mset, self->data_ptr);
}

static PyObject *
ElectricityRate_get_ur_ec_sched_weekend(ElectricityRateObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Pvsamv1_ElectricityRate_ur_ec_sched_weekend_mget, self->data_ptr);
}

static int
ElectricityRate_set_ur_ec_sched_weekend(ElectricityRateObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_Pvsamv1_ElectricityRate_ur_ec_sched_weekend_mset, self->data_ptr);
}

static PyObject *
ElectricityRate_get_ur_ec_tou_mat(ElectricityRateObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Pvsamv1_ElectricityRate_ur_ec_tou_mat_mget, self->data_ptr);
}

static int
ElectricityRate_set_ur_ec_tou_mat(ElectricityRateObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_Pvsamv1_ElectricityRate_ur_ec_tou_mat_mset, self->data_ptr);
}

static PyGetSetDef ElectricityRate_getset[] = {
{"en_electricity_rates", (getter)ElectricityRate_get_en_electricity_rates,(setter)ElectricityRate_set_en_electricity_rates,
	PyDoc_STR("*float*: Enable Electricity Rates [0/1]\n\n*Options*: 0=EnableElectricityRates,1=NoRates"),
 	NULL},
{"ur_ec_sched_weekday", (getter)ElectricityRate_get_ur_ec_sched_weekday,(setter)ElectricityRate_set_ur_ec_sched_weekday,
	PyDoc_STR("*sequence[sequence]*: Energy charge weekday schedule\n\n*Info*: 12 x 24 matrix\n\n*Required*: set to 1&batt_meter_position=1&batt_dispatch_choice=2 if not provided."),
 	NULL},
{"ur_ec_sched_weekend", (getter)ElectricityRate_get_ur_ec_sched_weekend,(setter)ElectricityRate_set_ur_ec_sched_weekend,
	PyDoc_STR("*sequence[sequence]*: Energy charge weekend schedule\n\n*Info*: 12 x 24 matrix\n\n*Required*: set to 1&batt_meter_position=1&batt_dispatch_choice=2 if not provided."),
 	NULL},
{"ur_ec_tou_mat", (getter)ElectricityRate_get_ur_ec_tou_mat,(setter)ElectricityRate_set_ur_ec_tou_mat,
	PyDoc_STR("*sequence[sequence]*: Energy rates table\n\n*Required*: set to 1&batt_meter_position=1&batt_dispatch_choice=2 if not provided."),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject ElectricityRate_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Pvsamv1.ElectricityRate",             /*tp_name*/
		sizeof(ElectricityRateObject),          /*tp_basicsize*/
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
		ElectricityRate_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		ElectricityRate_getset,          /*tp_getset*/
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
	 * EnergyMarket Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Pvsamv1   data_ptr;
} EnergyMarketObject;

static PyTypeObject EnergyMarket_Type;

static PyObject *
EnergyMarket_new(SAM_Pvsamv1 data_ptr)
{
	PyObject* new_obj = EnergyMarket_Type.tp_alloc(&EnergyMarket_Type,0);

	EnergyMarketObject* EnergyMarket_obj = (EnergyMarketObject*)new_obj;

	EnergyMarket_obj->data_ptr = data_ptr;

	return new_obj;
}

/* EnergyMarket methods */

static PyObject *
EnergyMarket_assign(EnergyMarketObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Pvsamv1", "EnergyMarket")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
EnergyMarket_export(EnergyMarketObject *self, PyObject *args)
{
	PyTypeObject* tp = &EnergyMarket_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef EnergyMarket_methods[] = {
		{"assign",            (PyCFunction)EnergyMarket_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``EnergyMarket_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)EnergyMarket_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
EnergyMarket_get_dispatch_sched_weekday(EnergyMarketObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Pvsamv1_EnergyMarket_dispatch_sched_weekday_mget, self->data_ptr);
}

static int
EnergyMarket_set_dispatch_sched_weekday(EnergyMarketObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_Pvsamv1_EnergyMarket_dispatch_sched_weekday_mset, self->data_ptr);
}

static PyObject *
EnergyMarket_get_dispatch_sched_weekend(EnergyMarketObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Pvsamv1_EnergyMarket_dispatch_sched_weekend_mget, self->data_ptr);
}

static int
EnergyMarket_set_dispatch_sched_weekend(EnergyMarketObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_Pvsamv1_EnergyMarket_dispatch_sched_weekend_mset, self->data_ptr);
}

static PyObject *
EnergyMarket_get_dispatch_tod_factors(EnergyMarketObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_EnergyMarket_dispatch_tod_factors_aget, self->data_ptr);
}

static int
EnergyMarket_set_dispatch_tod_factors(EnergyMarketObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Pvsamv1_EnergyMarket_dispatch_tod_factors_aset, self->data_ptr);
}

static PyObject *
EnergyMarket_get_ppa_price_input(EnergyMarketObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_EnergyMarket_ppa_price_input_nget, self->data_ptr);
}

static int
EnergyMarket_set_ppa_price_input(EnergyMarketObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Pvsamv1_EnergyMarket_ppa_price_input_nset, self->data_ptr);
}

static PyGetSetDef EnergyMarket_getset[] = {
{"dispatch_sched_weekday", (getter)EnergyMarket_get_dispatch_sched_weekday,(setter)EnergyMarket_set_dispatch_sched_weekday,
	PyDoc_STR("*sequence[sequence]*: Diurnal weekday TOD periods [1..9]\n\n*Info*: 12 x 24 matrix\n\n*Required*: set to 1&batt_meter_position=1&batt_dispatch_choice=2 if not provided."),
 	NULL},
{"dispatch_sched_weekend", (getter)EnergyMarket_get_dispatch_sched_weekend,(setter)EnergyMarket_set_dispatch_sched_weekend,
	PyDoc_STR("*sequence[sequence]*: Diurnal weekend TOD periods [1..9]\n\n*Info*: 12 x 24 matrix\n\n*Required*: set to 1&batt_meter_position=1&batt_dispatch_choice=2 if not provided."),
 	NULL},
{"dispatch_tod_factors", (getter)EnergyMarket_get_dispatch_tod_factors,(setter)EnergyMarket_set_dispatch_tod_factors,
	PyDoc_STR("*sequence*: TOD factors for periods 1-9\n\n*Required*: set to 1&batt_meter_position=1&batt_dispatch_choice=2 if not provided."),
 	NULL},
{"ppa_price_input", (getter)EnergyMarket_get_ppa_price_input,(setter)EnergyMarket_set_ppa_price_input,
	PyDoc_STR("*float*: PPA Price Input\n\n*Required*: set to 1&batt_meter_position=1&batt_dispatch_choice=2 if not provided."),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject EnergyMarket_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Pvsamv1.EnergyMarket",             /*tp_name*/
		sizeof(EnergyMarketObject),          /*tp_basicsize*/
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
		EnergyMarket_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		EnergyMarket_getset,          /*tp_getset*/
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
	SAM_Pvsamv1   data_ptr;
} OutputsObject;

static PyTypeObject Outputs_Type;

static PyObject *
Outputs_new(SAM_Pvsamv1 data_ptr)
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

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Pvsamv1", "Outputs")){
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
Outputs_get_6par_Adj(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_6par_Adj_nget, self->data_ptr);
}

static PyObject *
Outputs_get_6par_Il(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_6par_Il_nget, self->data_ptr);
}

static PyObject *
Outputs_get_6par_Io(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_6par_Io_nget, self->data_ptr);
}

static PyObject *
Outputs_get_6par_Rs(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_6par_Rs_nget, self->data_ptr);
}

static PyObject *
Outputs_get_6par_Rsh(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_6par_Rsh_nget, self->data_ptr);
}

static PyObject *
Outputs_get_6par_a(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_6par_a_nget, self->data_ptr);
}

static PyObject *
Outputs_get_ac_loss(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_ac_loss_nget, self->data_ptr);
}

static PyObject *
Outputs_get_ac_transmission_loss(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_ac_transmission_loss_aget, self->data_ptr);
}

static PyObject *
Outputs_get_ac_wiring_loss(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_ac_wiring_loss_aget, self->data_ptr);
}

static PyObject *
Outputs_get_airmass(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_airmass_aget, self->data_ptr);
}

static PyObject *
Outputs_get_alb(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_alb_aget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_ac_battery_loss_percent(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_ac_battery_loss_percent_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_ac_gross(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_ac_gross_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_ac_inv_clip_loss_percent(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_ac_inv_clip_loss_percent_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_ac_inv_eff_loss_percent(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_ac_inv_eff_loss_percent_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_ac_inv_pnt_loss_percent(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_ac_inv_pnt_loss_percent_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_ac_inv_pso_loss_percent(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_ac_inv_pso_loss_percent_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_ac_inv_tdc_loss_percent(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_ac_inv_tdc_loss_percent_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_ac_lifetime_loss_percent(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_ac_lifetime_loss_percent_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_ac_loss_ond(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_ac_loss_ond_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_ac_perf_adj_loss_percent(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_ac_perf_adj_loss_percent_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_ac_wiring_loss(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_ac_wiring_loss_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_ac_wiring_loss_percent(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_ac_wiring_loss_percent_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_dc_battery_loss_percent(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_dc_battery_loss_percent_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_dc_diodes_loss(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_dc_diodes_loss_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_dc_diodes_loss_percent(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_dc_diodes_loss_percent_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_dc_gross(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_dc_gross_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_dc_invmppt_loss(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_dc_invmppt_loss_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_dc_lifetime_loss_percent(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_dc_lifetime_loss_percent_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_dc_loss_ond(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_dc_loss_ond_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_dc_mismatch_loss(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_dc_mismatch_loss_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_dc_mismatch_loss_percent(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_dc_mismatch_loss_percent_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_dc_module_loss_percent(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_dc_module_loss_percent_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_dc_mppt_clip_loss_percent(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_dc_mppt_clip_loss_percent_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_dc_nameplate_loss(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_dc_nameplate_loss_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_dc_nameplate_loss_percent(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_dc_nameplate_loss_percent_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_dc_net(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_dc_net_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_dc_nominal(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_dc_nominal_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_dc_optimizer_loss(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_dc_optimizer_loss_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_dc_optimizer_loss_percent(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_dc_optimizer_loss_percent_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_dc_perf_adj_loss_percent(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_dc_perf_adj_loss_percent_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_dc_snow_loss_percent(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_dc_snow_loss_percent_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_dc_tracking_loss(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_dc_tracking_loss_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_dc_tracking_loss_percent(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_dc_tracking_loss_percent_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_dc_wiring_loss(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_dc_wiring_loss_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_dc_wiring_loss_percent(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_dc_wiring_loss_percent_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_energy(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_energy_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_export_to_grid_energy(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_annual_export_to_grid_energy_aget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_gh(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_gh_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_import_to_grid_energy(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_annual_import_to_grid_energy_aget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_inv_cliploss(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_inv_cliploss_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_inv_pntloss(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_inv_pntloss_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_inv_psoloss(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_inv_psoloss_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_inv_tdcloss(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_inv_tdcloss_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_poa_beam_eff(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_poa_beam_eff_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_poa_beam_nom(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_poa_beam_nom_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_poa_cover_loss_percent(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_poa_cover_loss_percent_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_poa_eff(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_poa_eff_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_poa_front(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_poa_front_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_poa_nom(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_poa_nom_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_poa_rear(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_poa_rear_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_poa_rear_gain_percent(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_poa_rear_gain_percent_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_poa_shaded(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_poa_shaded_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_poa_shaded_soiled(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_poa_shaded_soiled_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_poa_shading_loss_percent(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_poa_shading_loss_percent_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_poa_soiling_loss_percent(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_poa_soiling_loss_percent_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_snow_loss(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_snow_loss_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_subarray1_dc_diodes_loss(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_subarray1_dc_diodes_loss_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_subarray1_dc_gross(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_subarray1_dc_gross_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_subarray1_dc_mismatch_loss(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_subarray1_dc_mismatch_loss_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_subarray1_dc_nameplate_loss(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_subarray1_dc_nameplate_loss_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_subarray1_dc_tracking_loss(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_subarray1_dc_tracking_loss_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_subarray1_dc_wiring_loss(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_subarray1_dc_wiring_loss_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_subarray2_dc_diodes_loss(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_subarray2_dc_diodes_loss_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_subarray2_dc_gross(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_subarray2_dc_gross_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_subarray2_dc_mismatch_loss(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_subarray2_dc_mismatch_loss_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_subarray2_dc_nameplate_loss(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_subarray2_dc_nameplate_loss_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_subarray2_dc_tracking_loss(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_subarray2_dc_tracking_loss_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_subarray2_dc_wiring_loss(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_subarray2_dc_wiring_loss_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_subarray3_dc_diodes_loss(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_subarray3_dc_diodes_loss_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_subarray3_dc_gross(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_subarray3_dc_gross_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_subarray3_dc_mismatch_loss(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_subarray3_dc_mismatch_loss_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_subarray3_dc_nameplate_loss(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_subarray3_dc_nameplate_loss_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_subarray3_dc_tracking_loss(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_subarray3_dc_tracking_loss_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_subarray3_dc_wiring_loss(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_subarray3_dc_wiring_loss_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_subarray4_dc_diodes_loss(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_subarray4_dc_diodes_loss_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_subarray4_dc_gross(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_subarray4_dc_gross_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_subarray4_dc_mismatch_loss(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_subarray4_dc_mismatch_loss_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_subarray4_dc_nameplate_loss(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_subarray4_dc_nameplate_loss_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_subarray4_dc_tracking_loss(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_subarray4_dc_tracking_loss_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_subarray4_dc_wiring_loss(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_subarray4_dc_wiring_loss_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_transmission_loss(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_transmission_loss_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_transmission_loss_percent(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_transmission_loss_percent_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_xfmr_loss_percent(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_annual_xfmr_loss_percent_nget, self->data_ptr);
}

static PyObject *
Outputs_get_average_battery_conversion_efficiency(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_average_battery_conversion_efficiency_nget, self->data_ptr);
}

static PyObject *
Outputs_get_average_battery_roundtrip_efficiency(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_average_battery_roundtrip_efficiency_nget, self->data_ptr);
}

static PyObject *
Outputs_get_batt_DOD(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_batt_DOD_aget, self->data_ptr);
}

static PyObject *
Outputs_get_batt_I(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_batt_I_aget, self->data_ptr);
}

static PyObject *
Outputs_get_batt_SOC(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_batt_SOC_aget, self->data_ptr);
}

static PyObject *
Outputs_get_batt_annual_charge_energy(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_batt_annual_charge_energy_aget, self->data_ptr);
}

static PyObject *
Outputs_get_batt_annual_charge_from_grid(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_batt_annual_charge_from_grid_aget, self->data_ptr);
}

static PyObject *
Outputs_get_batt_annual_charge_from_pv(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_batt_annual_charge_from_pv_aget, self->data_ptr);
}

static PyObject *
Outputs_get_batt_annual_discharge_energy(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_batt_annual_discharge_energy_aget, self->data_ptr);
}

static PyObject *
Outputs_get_batt_annual_energy_loss(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_batt_annual_energy_loss_aget, self->data_ptr);
}

static PyObject *
Outputs_get_batt_annual_energy_system_loss(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_batt_annual_energy_system_loss_aget, self->data_ptr);
}

static PyObject *
Outputs_get_batt_bank_installed_capacity(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_batt_bank_installed_capacity_nget, self->data_ptr);
}

static PyObject *
Outputs_get_batt_bank_replacement(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_batt_bank_replacement_aget, self->data_ptr);
}

static PyObject *
Outputs_get_batt_capacity_percent(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_batt_capacity_percent_aget, self->data_ptr);
}

static PyObject *
Outputs_get_batt_capacity_thermal_percent(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_batt_capacity_thermal_percent_aget, self->data_ptr);
}

static PyObject *
Outputs_get_batt_conversion_loss(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_batt_conversion_loss_aget, self->data_ptr);
}

static PyObject *
Outputs_get_batt_cost_to_cycle(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_batt_cost_to_cycle_aget, self->data_ptr);
}

static PyObject *
Outputs_get_batt_cycles(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_batt_cycles_aget, self->data_ptr);
}

static PyObject *
Outputs_get_batt_dispatch_sched(OutputsObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Pvsamv1_Outputs_batt_dispatch_sched_mget, self->data_ptr);
}

static PyObject *
Outputs_get_batt_power(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_batt_power_aget, self->data_ptr);
}

static PyObject *
Outputs_get_batt_power_target(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_batt_power_target_aget, self->data_ptr);
}

static PyObject *
Outputs_get_batt_pv_charge_percent(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_batt_pv_charge_percent_nget, self->data_ptr);
}

static PyObject *
Outputs_get_batt_q0(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_batt_q0_aget, self->data_ptr);
}

static PyObject *
Outputs_get_batt_q1(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_batt_q1_aget, self->data_ptr);
}

static PyObject *
Outputs_get_batt_q2(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_batt_q2_aget, self->data_ptr);
}

static PyObject *
Outputs_get_batt_qmax(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_batt_qmax_aget, self->data_ptr);
}

static PyObject *
Outputs_get_batt_qmaxI(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_batt_qmaxI_aget, self->data_ptr);
}

static PyObject *
Outputs_get_batt_qmax_thermal(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_batt_qmax_thermal_aget, self->data_ptr);
}

static PyObject *
Outputs_get_batt_system_loss(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_batt_system_loss_aget, self->data_ptr);
}

static PyObject *
Outputs_get_batt_temperature(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_batt_temperature_aget, self->data_ptr);
}

static PyObject *
Outputs_get_batt_to_grid(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_batt_to_grid_aget, self->data_ptr);
}

static PyObject *
Outputs_get_batt_to_load(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_batt_to_load_aget, self->data_ptr);
}

static PyObject *
Outputs_get_batt_voltage(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_batt_voltage_aget, self->data_ptr);
}

static PyObject *
Outputs_get_batt_voltage_cell(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_batt_voltage_cell_aget, self->data_ptr);
}

static PyObject *
Outputs_get_capacity_factor(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_capacity_factor_nget, self->data_ptr);
}

static PyObject *
Outputs_get_capacity_factor_ac(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_capacity_factor_ac_nget, self->data_ptr);
}

static PyObject *
Outputs_get_dc_degrade_factor(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_dc_degrade_factor_aget, self->data_ptr);
}

static PyObject *
Outputs_get_dc_invmppt_loss(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_dc_invmppt_loss_aget, self->data_ptr);
}

static PyObject *
Outputs_get_dc_net(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_dc_net_aget, self->data_ptr);
}

static PyObject *
Outputs_get_dc_snow_loss(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_dc_snow_loss_aget, self->data_ptr);
}

static PyObject *
Outputs_get_df(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_df_aget, self->data_ptr);
}

static PyObject *
Outputs_get_df_calc(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_df_calc_aget, self->data_ptr);
}

static PyObject *
Outputs_get_dn(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_dn_aget, self->data_ptr);
}

static PyObject *
Outputs_get_dn_calc(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_dn_calc_aget, self->data_ptr);
}

static PyObject *
Outputs_get_fuelcell_to_batt(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_fuelcell_to_batt_aget, self->data_ptr);
}

static PyObject *
Outputs_get_gen(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_gen_aget, self->data_ptr);
}

static PyObject *
Outputs_get_gh(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_gh_aget, self->data_ptr);
}

static PyObject *
Outputs_get_gh_calc(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_gh_calc_aget, self->data_ptr);
}

static PyObject *
Outputs_get_grid_power(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_grid_power_aget, self->data_ptr);
}

static PyObject *
Outputs_get_grid_power_target(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_grid_power_target_aget, self->data_ptr);
}

static PyObject *
Outputs_get_grid_to_batt(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_grid_to_batt_aget, self->data_ptr);
}

static PyObject *
Outputs_get_grid_to_load(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_grid_to_load_aget, self->data_ptr);
}

static PyObject *
Outputs_get_inv_cliploss(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_inv_cliploss_aget, self->data_ptr);
}

static PyObject *
Outputs_get_inv_eff(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_inv_eff_aget, self->data_ptr);
}

static PyObject *
Outputs_get_inv_pntloss(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_inv_pntloss_aget, self->data_ptr);
}

static PyObject *
Outputs_get_inv_psoloss(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_inv_psoloss_aget, self->data_ptr);
}

static PyObject *
Outputs_get_inv_tdcloss(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_inv_tdcloss_aget, self->data_ptr);
}

static PyObject *
Outputs_get_inv_total_loss(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_inv_total_loss_aget, self->data_ptr);
}

static PyObject *
Outputs_get_inverterMPPT1_DCVoltage(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_inverterMPPT1_DCVoltage_aget, self->data_ptr);
}

static PyObject *
Outputs_get_inverterMPPT2_DCVoltage(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_inverterMPPT2_DCVoltage_aget, self->data_ptr);
}

static PyObject *
Outputs_get_inverterMPPT3_DCVoltage(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_inverterMPPT3_DCVoltage_aget, self->data_ptr);
}

static PyObject *
Outputs_get_inverterMPPT4_DCVoltage(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_inverterMPPT4_DCVoltage_aget, self->data_ptr);
}

static PyObject *
Outputs_get_kwh_per_kw(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_kwh_per_kw_nget, self->data_ptr);
}

static PyObject *
Outputs_get_monthly_batt_to_grid(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_monthly_batt_to_grid_aget, self->data_ptr);
}

static PyObject *
Outputs_get_monthly_batt_to_load(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_monthly_batt_to_load_aget, self->data_ptr);
}

static PyObject *
Outputs_get_monthly_dc(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_monthly_dc_aget, self->data_ptr);
}

static PyObject *
Outputs_get_monthly_energy(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_monthly_energy_aget, self->data_ptr);
}

static PyObject *
Outputs_get_monthly_grid_to_batt(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_monthly_grid_to_batt_aget, self->data_ptr);
}

static PyObject *
Outputs_get_monthly_grid_to_load(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_monthly_grid_to_load_aget, self->data_ptr);
}

static PyObject *
Outputs_get_monthly_poa_beam_eff(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_monthly_poa_beam_eff_aget, self->data_ptr);
}

static PyObject *
Outputs_get_monthly_poa_beam_nom(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_monthly_poa_beam_nom_aget, self->data_ptr);
}

static PyObject *
Outputs_get_monthly_poa_eff(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_monthly_poa_eff_aget, self->data_ptr);
}

static PyObject *
Outputs_get_monthly_poa_front(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_monthly_poa_front_aget, self->data_ptr);
}

static PyObject *
Outputs_get_monthly_poa_nom(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_monthly_poa_nom_aget, self->data_ptr);
}

static PyObject *
Outputs_get_monthly_poa_rear(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_monthly_poa_rear_aget, self->data_ptr);
}

static PyObject *
Outputs_get_monthly_pv_to_batt(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_monthly_pv_to_batt_aget, self->data_ptr);
}

static PyObject *
Outputs_get_monthly_pv_to_grid(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_monthly_pv_to_grid_aget, self->data_ptr);
}

static PyObject *
Outputs_get_monthly_pv_to_load(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_monthly_pv_to_load_aget, self->data_ptr);
}

static PyObject *
Outputs_get_monthly_snow_loss(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_monthly_snow_loss_aget, self->data_ptr);
}

static PyObject *
Outputs_get_nameplate_dc_rating(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_nameplate_dc_rating_nget, self->data_ptr);
}

static PyObject *
Outputs_get_performance_ratio(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_performance_ratio_nget, self->data_ptr);
}

static PyObject *
Outputs_get_poa_beam_eff(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_poa_beam_eff_aget, self->data_ptr);
}

static PyObject *
Outputs_get_poa_beam_nom(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_poa_beam_nom_aget, self->data_ptr);
}

static PyObject *
Outputs_get_poa_eff(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_poa_eff_aget, self->data_ptr);
}

static PyObject *
Outputs_get_poa_front(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_poa_front_aget, self->data_ptr);
}

static PyObject *
Outputs_get_poa_nom(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_poa_nom_aget, self->data_ptr);
}

static PyObject *
Outputs_get_poa_rear(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_poa_rear_aget, self->data_ptr);
}

static PyObject *
Outputs_get_poa_shaded(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_poa_shaded_aget, self->data_ptr);
}

static PyObject *
Outputs_get_poa_shaded_soiled(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_poa_shaded_soiled_aget, self->data_ptr);
}

static PyObject *
Outputs_get_pv_to_batt(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_pv_to_batt_aget, self->data_ptr);
}

static PyObject *
Outputs_get_pv_to_grid(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_pv_to_grid_aget, self->data_ptr);
}

static PyObject *
Outputs_get_pv_to_load(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_pv_to_load_aget, self->data_ptr);
}

static PyObject *
Outputs_get_shadedb_subarray1_shade_frac(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_shadedb_subarray1_shade_frac_aget, self->data_ptr);
}

static PyObject *
Outputs_get_shadedb_subarray2_shade_frac(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_shadedb_subarray2_shade_frac_aget, self->data_ptr);
}

static PyObject *
Outputs_get_shadedb_subarray3_shade_frac(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_shadedb_subarray3_shade_frac_aget, self->data_ptr);
}

static PyObject *
Outputs_get_shadedb_subarray4_shade_frac(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_shadedb_subarray4_shade_frac_aget, self->data_ptr);
}

static PyObject *
Outputs_get_snowdepth(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_snowdepth_aget, self->data_ptr);
}

static PyObject *
Outputs_get_sol_alt(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_sol_alt_aget, self->data_ptr);
}

static PyObject *
Outputs_get_sol_azi(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_sol_azi_aget, self->data_ptr);
}

static PyObject *
Outputs_get_sol_zen(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_sol_zen_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray1_aoi(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray1_aoi_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray1_aoi_modifier(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray1_aoi_modifier_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray1_axisrot(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray1_axisrot_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray1_beam_shading_factor(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray1_beam_shading_factor_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray1_celltemp(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray1_celltemp_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray1_dc_gross(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray1_dc_gross_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray1_dc_voltage(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray1_dc_voltage_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray1_dcloss(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_subarray1_dcloss_nget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray1_idealrot(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray1_idealrot_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray1_isc(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray1_isc_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray1_linear_derate(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray1_linear_derate_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray1_modeff(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray1_modeff_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray1_poa_eff(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray1_poa_eff_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray1_poa_eff_beam(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray1_poa_eff_beam_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray1_poa_eff_diff(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray1_poa_eff_diff_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray1_poa_front(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray1_poa_front_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray1_poa_nom(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray1_poa_nom_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray1_poa_rear(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray1_poa_rear_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray1_poa_shaded(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray1_poa_shaded_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray1_poa_shaded_soiled(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray1_poa_shaded_soiled_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray1_snow_coverage(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray1_snow_coverage_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray1_snow_loss(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray1_snow_loss_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray1_soiling_derate(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray1_soiling_derate_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray1_ss_derate(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray1_ss_derate_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray1_ss_diffuse_derate(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray1_ss_diffuse_derate_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray1_ss_reflected_derate(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray1_ss_reflected_derate_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray1_surf_azi(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray1_surf_azi_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray1_surf_tilt(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray1_surf_tilt_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray1_voc(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray1_voc_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray2_aoi(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray2_aoi_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray2_aoi_modifier(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray2_aoi_modifier_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray2_axisrot(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray2_axisrot_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray2_beam_shading_factor(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray2_beam_shading_factor_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray2_celltemp(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray2_celltemp_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray2_dc_gross(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray2_dc_gross_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray2_dc_voltage(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray2_dc_voltage_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray2_dcloss(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_subarray2_dcloss_nget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray2_idealrot(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray2_idealrot_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray2_isc(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray2_isc_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray2_linear_derate(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray2_linear_derate_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray2_modeff(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray2_modeff_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray2_poa_eff(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray2_poa_eff_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray2_poa_eff_beam(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray2_poa_eff_beam_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray2_poa_eff_diff(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray2_poa_eff_diff_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray2_poa_front(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray2_poa_front_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray2_poa_nom(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray2_poa_nom_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray2_poa_rear(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray2_poa_rear_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray2_poa_shaded(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray2_poa_shaded_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray2_poa_shaded_soiled(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray2_poa_shaded_soiled_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray2_snow_coverage(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray2_snow_coverage_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray2_snow_loss(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray2_snow_loss_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray2_soiling_derate(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray2_soiling_derate_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray2_ss_derate(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray2_ss_derate_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray2_ss_diffuse_derate(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray2_ss_diffuse_derate_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray2_ss_reflected_derate(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray2_ss_reflected_derate_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray2_surf_azi(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray2_surf_azi_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray2_surf_tilt(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray2_surf_tilt_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray2_voc(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray2_voc_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray3_aoi(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray3_aoi_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray3_aoi_modifier(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray3_aoi_modifier_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray3_axisrot(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray3_axisrot_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray3_beam_shading_factor(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray3_beam_shading_factor_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray3_celltemp(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray3_celltemp_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray3_dc_gross(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray3_dc_gross_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray3_dc_voltage(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray3_dc_voltage_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray3_dcloss(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_subarray3_dcloss_nget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray3_idealrot(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray3_idealrot_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray3_isc(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray3_isc_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray3_linear_derate(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray3_linear_derate_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray3_modeff(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray3_modeff_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray3_poa_eff(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray3_poa_eff_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray3_poa_eff_beam(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray3_poa_eff_beam_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray3_poa_eff_diff(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray3_poa_eff_diff_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray3_poa_front(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray3_poa_front_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray3_poa_nom(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray3_poa_nom_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray3_poa_rear(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray3_poa_rear_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray3_poa_shaded(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray3_poa_shaded_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray3_poa_shaded_soiled(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray3_poa_shaded_soiled_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray3_snow_coverage(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray3_snow_coverage_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray3_snow_loss(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray3_snow_loss_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray3_soiling_derate(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray3_soiling_derate_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray3_ss_derate(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray3_ss_derate_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray3_ss_diffuse_derate(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray3_ss_diffuse_derate_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray3_ss_reflected_derate(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray3_ss_reflected_derate_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray3_surf_azi(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray3_surf_azi_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray3_surf_tilt(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray3_surf_tilt_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray3_voc(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray3_voc_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray4_aoi(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray4_aoi_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray4_aoi_modifier(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray4_aoi_modifier_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray4_axisrot(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray4_axisrot_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray4_beam_shading_factor(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray4_beam_shading_factor_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray4_celltemp(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray4_celltemp_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray4_dc_gross(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray4_dc_gross_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray4_dc_voltage(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray4_dc_voltage_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray4_dcloss(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_subarray4_dcloss_nget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray4_idealrot(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray4_idealrot_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray4_isc(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray4_isc_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray4_linear_derate(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray4_linear_derate_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray4_modeff(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray4_modeff_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray4_poa_eff(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray4_poa_eff_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray4_poa_eff_beam(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray4_poa_eff_beam_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray4_poa_eff_diff(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray4_poa_eff_diff_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray4_poa_front(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray4_poa_front_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray4_poa_nom(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray4_poa_nom_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray4_poa_rear(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray4_poa_rear_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray4_poa_shaded(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray4_poa_shaded_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray4_poa_shaded_soiled(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray4_poa_shaded_soiled_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray4_snow_coverage(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray4_snow_coverage_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray4_snow_loss(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray4_snow_loss_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray4_soiling_derate(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray4_soiling_derate_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray4_ss_derate(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray4_ss_derate_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray4_ss_diffuse_derate(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray4_ss_diffuse_derate_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray4_ss_reflected_derate(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray4_ss_reflected_derate_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray4_surf_azi(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray4_surf_azi_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray4_surf_tilt(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray4_surf_tilt_aget, self->data_ptr);
}

static PyObject *
Outputs_get_subarray4_voc(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_subarray4_voc_aget, self->data_ptr);
}

static PyObject *
Outputs_get_sunpos_hour(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_sunpos_hour_aget, self->data_ptr);
}

static PyObject *
Outputs_get_sunup(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_sunup_aget, self->data_ptr);
}

static PyObject *
Outputs_get_tdry(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_tdry_aget, self->data_ptr);
}

static PyObject *
Outputs_get_ts_shift_hours(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_ts_shift_hours_nget, self->data_ptr);
}

static PyObject *
Outputs_get_wfpoa(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_wfpoa_aget, self->data_ptr);
}

static PyObject *
Outputs_get_wspd(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_wspd_aget, self->data_ptr);
}

static PyObject *
Outputs_get_xfmr_ll_ts(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_xfmr_ll_ts_aget, self->data_ptr);
}

static PyObject *
Outputs_get_xfmr_ll_year1(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_xfmr_ll_year1_nget, self->data_ptr);
}

static PyObject *
Outputs_get_xfmr_loss_ts(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_xfmr_loss_ts_aget, self->data_ptr);
}

static PyObject *
Outputs_get_xfmr_loss_year1(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_xfmr_loss_year1_nget, self->data_ptr);
}

static PyObject *
Outputs_get_xfmr_nll_ts(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Pvsamv1_Outputs_xfmr_nll_ts_aget, self->data_ptr);
}

static PyObject *
Outputs_get_xfmr_nll_year1(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Pvsamv1_Outputs_xfmr_nll_year1_nget, self->data_ptr);
}

static PyGetSetDef Outputs_getset[] = {
{"6par_Adj", (getter)Outputs_get_6par_Adj,(setter)0,
	PyDoc_STR("*float*: CEC 6-parameter: Adj"),
 	NULL},
{"6par_Il", (getter)Outputs_get_6par_Il,(setter)0,
	PyDoc_STR("*float*: CEC 6-parameter: Il"),
 	NULL},
{"6par_Io", (getter)Outputs_get_6par_Io,(setter)0,
	PyDoc_STR("*float*: CEC 6-parameter: Io"),
 	NULL},
{"6par_Rs", (getter)Outputs_get_6par_Rs,(setter)0,
	PyDoc_STR("*float*: CEC 6-parameter: Rs"),
 	NULL},
{"6par_Rsh", (getter)Outputs_get_6par_Rsh,(setter)0,
	PyDoc_STR("*float*: CEC 6-parameter: Rsh"),
 	NULL},
{"6par_a", (getter)Outputs_get_6par_a,(setter)0,
	PyDoc_STR("*float*: CEC 6-parameter: a"),
 	NULL},
{"ac_loss", (getter)Outputs_get_ac_loss,(setter)0,
	PyDoc_STR("*float*: AC wiring loss [%]"),
 	NULL},
{"ac_transmission_loss", (getter)Outputs_get_ac_transmission_loss,(setter)0,
	PyDoc_STR("*sequence*: Transmission loss [kW]"),
 	NULL},
{"ac_wiring_loss", (getter)Outputs_get_ac_wiring_loss,(setter)0,
	PyDoc_STR("*sequence*: AC wiring loss [kW]"),
 	NULL},
{"airmass", (getter)Outputs_get_airmass,(setter)0,
	PyDoc_STR("*sequence*: Absolute air mass"),
 	NULL},
{"alb", (getter)Outputs_get_alb,(setter)0,
	PyDoc_STR("*sequence*: Weather file albedo"),
 	NULL},
{"annual_ac_battery_loss_percent", (getter)Outputs_get_annual_ac_battery_loss_percent,(setter)0,
	PyDoc_STR("*float*: AC connected battery loss- year 1 [%]"),
 	NULL},
{"annual_ac_gross", (getter)Outputs_get_annual_ac_gross,(setter)0,
	PyDoc_STR("*float*: Annual AC energy gross [kWh/yr]"),
 	NULL},
{"annual_ac_inv_clip_loss_percent", (getter)Outputs_get_annual_ac_inv_clip_loss_percent,(setter)0,
	PyDoc_STR("*float*: AC inverter power clipping loss [%]"),
 	NULL},
{"annual_ac_inv_eff_loss_percent", (getter)Outputs_get_annual_ac_inv_eff_loss_percent,(setter)0,
	PyDoc_STR("*float*: AC inverter efficiency loss [%]"),
 	NULL},
{"annual_ac_inv_pnt_loss_percent", (getter)Outputs_get_annual_ac_inv_pnt_loss_percent,(setter)0,
	PyDoc_STR("*float*: AC inverter night tare loss [%]"),
 	NULL},
{"annual_ac_inv_pso_loss_percent", (getter)Outputs_get_annual_ac_inv_pso_loss_percent,(setter)0,
	PyDoc_STR("*float*: AC inverter power consumption loss [%]"),
 	NULL},
{"annual_ac_inv_tdc_loss_percent", (getter)Outputs_get_annual_ac_inv_tdc_loss_percent,(setter)0,
	PyDoc_STR("*float*: AC inverter thermal derate loss [%]"),
 	NULL},
{"annual_ac_lifetime_loss_percent", (getter)Outputs_get_annual_ac_lifetime_loss_percent,(setter)0,
	PyDoc_STR("*float*: Lifetime daily AC loss- year 1 [%]"),
 	NULL},
{"annual_ac_loss_ond", (getter)Outputs_get_annual_ac_loss_ond,(setter)0,
	PyDoc_STR("*float*: Annual AC loss OND model [kWh/yr]"),
 	NULL},
{"annual_ac_perf_adj_loss_percent", (getter)Outputs_get_annual_ac_perf_adj_loss_percent,(setter)0,
	PyDoc_STR("*float*: AC performance adjustment loss [%]"),
 	NULL},
{"annual_ac_wiring_loss", (getter)Outputs_get_annual_ac_wiring_loss,(setter)0,
	PyDoc_STR("*float*: AC wiring loss [kWh]"),
 	NULL},
{"annual_ac_wiring_loss_percent", (getter)Outputs_get_annual_ac_wiring_loss_percent,(setter)0,
	PyDoc_STR("*float*: AC wiring loss [%]"),
 	NULL},
{"annual_dc_battery_loss_percent", (getter)Outputs_get_annual_dc_battery_loss_percent,(setter)0,
	PyDoc_STR("*float*: DC connected battery loss- year 1 [%]"),
 	NULL},
{"annual_dc_diodes_loss", (getter)Outputs_get_annual_dc_diodes_loss,(setter)0,
	PyDoc_STR("*float*: DC diodes and connections loss [kWh]"),
 	NULL},
{"annual_dc_diodes_loss_percent", (getter)Outputs_get_annual_dc_diodes_loss_percent,(setter)0,
	PyDoc_STR("*float*: DC diodes and connections loss [%]"),
 	NULL},
{"annual_dc_gross", (getter)Outputs_get_annual_dc_gross,(setter)0,
	PyDoc_STR("*float*: Annual DC energy gross [kWh/yr]"),
 	NULL},
{"annual_dc_invmppt_loss", (getter)Outputs_get_annual_dc_invmppt_loss,(setter)0,
	PyDoc_STR("*float*: Inverter clipping loss DC MPPT voltage limits [kWh/yr]"),
 	NULL},
{"annual_dc_lifetime_loss_percent", (getter)Outputs_get_annual_dc_lifetime_loss_percent,(setter)0,
	PyDoc_STR("*float*: Lifetime daily DC loss- year 1 [%]"),
 	NULL},
{"annual_dc_loss_ond", (getter)Outputs_get_annual_dc_loss_ond,(setter)0,
	PyDoc_STR("*float*: Annual DC loss OND model [kWh/yr]"),
 	NULL},
{"annual_dc_mismatch_loss", (getter)Outputs_get_annual_dc_mismatch_loss,(setter)0,
	PyDoc_STR("*float*: DC mismatch loss [kWh]"),
 	NULL},
{"annual_dc_mismatch_loss_percent", (getter)Outputs_get_annual_dc_mismatch_loss_percent,(setter)0,
	PyDoc_STR("*float*: DC mismatch loss [%]"),
 	NULL},
{"annual_dc_module_loss_percent", (getter)Outputs_get_annual_dc_module_loss_percent,(setter)0,
	PyDoc_STR("*float*: DC module modeled loss [%]"),
 	NULL},
{"annual_dc_mppt_clip_loss_percent", (getter)Outputs_get_annual_dc_mppt_clip_loss_percent,(setter)0,
	PyDoc_STR("*float*: DC inverter MPPT clipping loss [%]"),
 	NULL},
{"annual_dc_nameplate_loss", (getter)Outputs_get_annual_dc_nameplate_loss,(setter)0,
	PyDoc_STR("*float*: DC nameplate loss [kWh]"),
 	NULL},
{"annual_dc_nameplate_loss_percent", (getter)Outputs_get_annual_dc_nameplate_loss_percent,(setter)0,
	PyDoc_STR("*float*: DC nameplate loss [%]"),
 	NULL},
{"annual_dc_net", (getter)Outputs_get_annual_dc_net,(setter)0,
	PyDoc_STR("*float*: Annual DC energy [kWh/yr]"),
 	NULL},
{"annual_dc_nominal", (getter)Outputs_get_annual_dc_nominal,(setter)0,
	PyDoc_STR("*float*: Annual DC energy nominal [kWh/yr]"),
 	NULL},
{"annual_dc_optimizer_loss", (getter)Outputs_get_annual_dc_optimizer_loss,(setter)0,
	PyDoc_STR("*float*: DC power optimizer loss [kWh]"),
 	NULL},
{"annual_dc_optimizer_loss_percent", (getter)Outputs_get_annual_dc_optimizer_loss_percent,(setter)0,
	PyDoc_STR("*float*: DC power optimizer loss [%]"),
 	NULL},
{"annual_dc_perf_adj_loss_percent", (getter)Outputs_get_annual_dc_perf_adj_loss_percent,(setter)0,
	PyDoc_STR("*float*: DC performance adjustment loss [%]"),
 	NULL},
{"annual_dc_snow_loss_percent", (getter)Outputs_get_annual_dc_snow_loss_percent,(setter)0,
	PyDoc_STR("*float*: DC snow loss [%]"),
 	NULL},
{"annual_dc_tracking_loss", (getter)Outputs_get_annual_dc_tracking_loss,(setter)0,
	PyDoc_STR("*float*: DC tracking loss [kWh]"),
 	NULL},
{"annual_dc_tracking_loss_percent", (getter)Outputs_get_annual_dc_tracking_loss_percent,(setter)0,
	PyDoc_STR("*float*: DC tracking loss [%]"),
 	NULL},
{"annual_dc_wiring_loss", (getter)Outputs_get_annual_dc_wiring_loss,(setter)0,
	PyDoc_STR("*float*: DC wiring loss [kWh]"),
 	NULL},
{"annual_dc_wiring_loss_percent", (getter)Outputs_get_annual_dc_wiring_loss_percent,(setter)0,
	PyDoc_STR("*float*: DC wiring loss [%]"),
 	NULL},
{"annual_energy", (getter)Outputs_get_annual_energy,(setter)0,
	PyDoc_STR("*float*: Annual AC energy [kWh]"),
 	NULL},
{"annual_export_to_grid_energy", (getter)Outputs_get_annual_export_to_grid_energy,(setter)0,
	PyDoc_STR("*sequence*: Annual energy exported to grid [kWh]"),
 	NULL},
{"annual_gh", (getter)Outputs_get_annual_gh,(setter)0,
	PyDoc_STR("*float*: Annual GHI [Wh/m2/yr]"),
 	NULL},
{"annual_import_to_grid_energy", (getter)Outputs_get_annual_import_to_grid_energy,(setter)0,
	PyDoc_STR("*sequence*: Annual energy imported from grid [kWh]"),
 	NULL},
{"annual_inv_cliploss", (getter)Outputs_get_annual_inv_cliploss,(setter)0,
	PyDoc_STR("*float*: Inverter clipping loss AC power limit [kWh/yr]"),
 	NULL},
{"annual_inv_pntloss", (getter)Outputs_get_annual_inv_pntloss,(setter)0,
	PyDoc_STR("*float*: Inverter night time loss [kWh/yr]"),
 	NULL},
{"annual_inv_psoloss", (getter)Outputs_get_annual_inv_psoloss,(setter)0,
	PyDoc_STR("*float*: Inverter power consumption loss [kWh/yr]"),
 	NULL},
{"annual_inv_tdcloss", (getter)Outputs_get_annual_inv_tdcloss,(setter)0,
	PyDoc_STR("*float*: Inverter thermal derate loss [kWh/yr]"),
 	NULL},
{"annual_poa_beam_eff", (getter)Outputs_get_annual_poa_beam_eff,(setter)0,
	PyDoc_STR("*float*: POA front-side irradiance beam after shading and soiling [kWh/yr]"),
 	NULL},
{"annual_poa_beam_nom", (getter)Outputs_get_annual_poa_beam_nom,(setter)0,
	PyDoc_STR("*float*: POA front-side irradiance beam nominal [kWh/yr]"),
 	NULL},
{"annual_poa_cover_loss_percent", (getter)Outputs_get_annual_poa_cover_loss_percent,(setter)0,
	PyDoc_STR("*float*: POA front-side reflection (IAM) loss [%]"),
 	NULL},
{"annual_poa_eff", (getter)Outputs_get_annual_poa_eff,(setter)0,
	PyDoc_STR("*float*: POA irradiance total after reflection (IAM) [kWh/yr]"),
 	NULL},
{"annual_poa_front", (getter)Outputs_get_annual_poa_front,(setter)0,
	PyDoc_STR("*float*: POA front-side irradiance total after reflection (IAM) [kWh/yr]"),
 	NULL},
{"annual_poa_nom", (getter)Outputs_get_annual_poa_nom,(setter)0,
	PyDoc_STR("*float*: POA front-side irradiance total nominal [kWh/yr]"),
 	NULL},
{"annual_poa_rear", (getter)Outputs_get_annual_poa_rear,(setter)0,
	PyDoc_STR("*float*: POA rear-side irradiance total after reflection (IAM) [kWh/yr]"),
 	NULL},
{"annual_poa_rear_gain_percent", (getter)Outputs_get_annual_poa_rear_gain_percent,(setter)0,
	PyDoc_STR("*float*: POA rear-side bifacial gain [%]"),
 	NULL},
{"annual_poa_shaded", (getter)Outputs_get_annual_poa_shaded,(setter)0,
	PyDoc_STR("*float*: POA front-side irradiance total after shading [kWh/yr]"),
 	NULL},
{"annual_poa_shaded_soiled", (getter)Outputs_get_annual_poa_shaded_soiled,(setter)0,
	PyDoc_STR("*float*: POA front-side irradiance total after shading and soiling [kWh/yr]"),
 	NULL},
{"annual_poa_shading_loss_percent", (getter)Outputs_get_annual_poa_shading_loss_percent,(setter)0,
	PyDoc_STR("*float*: POA front-side shading loss [%]"),
 	NULL},
{"annual_poa_soiling_loss_percent", (getter)Outputs_get_annual_poa_soiling_loss_percent,(setter)0,
	PyDoc_STR("*float*: POA front-side soiling loss [%]"),
 	NULL},
{"annual_snow_loss", (getter)Outputs_get_annual_snow_loss,(setter)0,
	PyDoc_STR("*float*: Snow DC energy loss [kWh/yr]"),
 	NULL},
{"annual_subarray1_dc_diodes_loss", (getter)Outputs_get_annual_subarray1_dc_diodes_loss,(setter)0,
	PyDoc_STR("*float*: Subarray 1 DC diodes and connections loss [kWh]"),
 	NULL},
{"annual_subarray1_dc_gross", (getter)Outputs_get_annual_subarray1_dc_gross,(setter)0,
	PyDoc_STR("*float*: Subarray 1 Gross DC energy [kWh]"),
 	NULL},
{"annual_subarray1_dc_mismatch_loss", (getter)Outputs_get_annual_subarray1_dc_mismatch_loss,(setter)0,
	PyDoc_STR("*float*: Subarray 1 DC mismatch loss [kWh]"),
 	NULL},
{"annual_subarray1_dc_nameplate_loss", (getter)Outputs_get_annual_subarray1_dc_nameplate_loss,(setter)0,
	PyDoc_STR("*float*: Subarray 1 DC nameplate loss [kWh]"),
 	NULL},
{"annual_subarray1_dc_tracking_loss", (getter)Outputs_get_annual_subarray1_dc_tracking_loss,(setter)0,
	PyDoc_STR("*float*: Subarray 1 DC tracking loss [kWh]"),
 	NULL},
{"annual_subarray1_dc_wiring_loss", (getter)Outputs_get_annual_subarray1_dc_wiring_loss,(setter)0,
	PyDoc_STR("*float*: Subarray 1 DC wiring loss [kWh]"),
 	NULL},
{"annual_subarray2_dc_diodes_loss", (getter)Outputs_get_annual_subarray2_dc_diodes_loss,(setter)0,
	PyDoc_STR("*float*: Subarray 2 DC diodes and connections loss [kWh]"),
 	NULL},
{"annual_subarray2_dc_gross", (getter)Outputs_get_annual_subarray2_dc_gross,(setter)0,
	PyDoc_STR("*float*: Subarray 2 Gross DC energy [kWh]"),
 	NULL},
{"annual_subarray2_dc_mismatch_loss", (getter)Outputs_get_annual_subarray2_dc_mismatch_loss,(setter)0,
	PyDoc_STR("*float*: Subarray 2 DC mismatch loss [kWh]"),
 	NULL},
{"annual_subarray2_dc_nameplate_loss", (getter)Outputs_get_annual_subarray2_dc_nameplate_loss,(setter)0,
	PyDoc_STR("*float*: Subarray 2 DC nameplate loss [kWh]"),
 	NULL},
{"annual_subarray2_dc_tracking_loss", (getter)Outputs_get_annual_subarray2_dc_tracking_loss,(setter)0,
	PyDoc_STR("*float*: Subarray 2 DC tracking loss [kWh]"),
 	NULL},
{"annual_subarray2_dc_wiring_loss", (getter)Outputs_get_annual_subarray2_dc_wiring_loss,(setter)0,
	PyDoc_STR("*float*: Subarray 2 DC wiring loss [kWh]"),
 	NULL},
{"annual_subarray3_dc_diodes_loss", (getter)Outputs_get_annual_subarray3_dc_diodes_loss,(setter)0,
	PyDoc_STR("*float*: Subarray 3 DC diodes and connections loss [kWh]"),
 	NULL},
{"annual_subarray3_dc_gross", (getter)Outputs_get_annual_subarray3_dc_gross,(setter)0,
	PyDoc_STR("*float*: Subarray 3 Gross DC energy [kWh]"),
 	NULL},
{"annual_subarray3_dc_mismatch_loss", (getter)Outputs_get_annual_subarray3_dc_mismatch_loss,(setter)0,
	PyDoc_STR("*float*: Subarray 3 DC mismatch loss [kWh]"),
 	NULL},
{"annual_subarray3_dc_nameplate_loss", (getter)Outputs_get_annual_subarray3_dc_nameplate_loss,(setter)0,
	PyDoc_STR("*float*: Subarray 3 DC nameplate loss [kWh]"),
 	NULL},
{"annual_subarray3_dc_tracking_loss", (getter)Outputs_get_annual_subarray3_dc_tracking_loss,(setter)0,
	PyDoc_STR("*float*: Subarray 3 DC tracking loss [kWh]"),
 	NULL},
{"annual_subarray3_dc_wiring_loss", (getter)Outputs_get_annual_subarray3_dc_wiring_loss,(setter)0,
	PyDoc_STR("*float*: Subarray 3 DC wiring loss [kWh]"),
 	NULL},
{"annual_subarray4_dc_diodes_loss", (getter)Outputs_get_annual_subarray4_dc_diodes_loss,(setter)0,
	PyDoc_STR("*float*: Subarray 4 DC diodes and connections loss [kWh]"),
 	NULL},
{"annual_subarray4_dc_gross", (getter)Outputs_get_annual_subarray4_dc_gross,(setter)0,
	PyDoc_STR("*float*: Subarray 4 Gross DC energy [kWh]"),
 	NULL},
{"annual_subarray4_dc_mismatch_loss", (getter)Outputs_get_annual_subarray4_dc_mismatch_loss,(setter)0,
	PyDoc_STR("*float*: Subarray 4 DC mismatch loss [kWh]"),
 	NULL},
{"annual_subarray4_dc_nameplate_loss", (getter)Outputs_get_annual_subarray4_dc_nameplate_loss,(setter)0,
	PyDoc_STR("*float*: Subarray 4 DC nameplate loss [kWh]"),
 	NULL},
{"annual_subarray4_dc_tracking_loss", (getter)Outputs_get_annual_subarray4_dc_tracking_loss,(setter)0,
	PyDoc_STR("*float*: Subarray 4 DC tracking loss [kWh]"),
 	NULL},
{"annual_subarray4_dc_wiring_loss", (getter)Outputs_get_annual_subarray4_dc_wiring_loss,(setter)0,
	PyDoc_STR("*float*: Subarray 4 DC wiring loss [kWh]"),
 	NULL},
{"annual_transmission_loss", (getter)Outputs_get_annual_transmission_loss,(setter)0,
	PyDoc_STR("*float*: Transmission loss [kWh]"),
 	NULL},
{"annual_transmission_loss_percent", (getter)Outputs_get_annual_transmission_loss_percent,(setter)0,
	PyDoc_STR("*float*: Transmission loss [%]"),
 	NULL},
{"annual_xfmr_loss_percent", (getter)Outputs_get_annual_xfmr_loss_percent,(setter)0,
	PyDoc_STR("*float*: Transformer loss percent [%]"),
 	NULL},
{"average_battery_conversion_efficiency", (getter)Outputs_get_average_battery_conversion_efficiency,(setter)0,
	PyDoc_STR("*float*: Battery average cycle conversion efficiency [%]"),
 	NULL},
{"average_battery_roundtrip_efficiency", (getter)Outputs_get_average_battery_roundtrip_efficiency,(setter)0,
	PyDoc_STR("*float*: Battery average roundtrip efficiency [%]"),
 	NULL},
{"batt_DOD", (getter)Outputs_get_batt_DOD,(setter)0,
	PyDoc_STR("*sequence*: Battery cycle depth of discharge [%]"),
 	NULL},
{"batt_I", (getter)Outputs_get_batt_I,(setter)0,
	PyDoc_STR("*sequence*: Battery current [A]"),
 	NULL},
{"batt_SOC", (getter)Outputs_get_batt_SOC,(setter)0,
	PyDoc_STR("*sequence*: Battery state of charge [%]"),
 	NULL},
{"batt_annual_charge_energy", (getter)Outputs_get_batt_annual_charge_energy,(setter)0,
	PyDoc_STR("*sequence*: Battery annual energy charged [kWh]"),
 	NULL},
{"batt_annual_charge_from_grid", (getter)Outputs_get_batt_annual_charge_from_grid,(setter)0,
	PyDoc_STR("*sequence*: Battery annual energy charged from grid [kWh]"),
 	NULL},
{"batt_annual_charge_from_pv", (getter)Outputs_get_batt_annual_charge_from_pv,(setter)0,
	PyDoc_STR("*sequence*: Battery annual energy charged from PV [kWh]"),
 	NULL},
{"batt_annual_discharge_energy", (getter)Outputs_get_batt_annual_discharge_energy,(setter)0,
	PyDoc_STR("*sequence*: Battery annual energy discharged [kWh]"),
 	NULL},
{"batt_annual_energy_loss", (getter)Outputs_get_batt_annual_energy_loss,(setter)0,
	PyDoc_STR("*sequence*: Battery annual energy loss [kWh]"),
 	NULL},
{"batt_annual_energy_system_loss", (getter)Outputs_get_batt_annual_energy_system_loss,(setter)0,
	PyDoc_STR("*sequence*: Battery annual system energy loss [kWh]"),
 	NULL},
{"batt_bank_installed_capacity", (getter)Outputs_get_batt_bank_installed_capacity,(setter)0,
	PyDoc_STR("*float*: Battery bank installed capacity [kWh]"),
 	NULL},
{"batt_bank_replacement", (getter)Outputs_get_batt_bank_replacement,(setter)0,
	PyDoc_STR("*sequence*: Battery bank replacements per year [number/year]"),
 	NULL},
{"batt_capacity_percent", (getter)Outputs_get_batt_capacity_percent,(setter)0,
	PyDoc_STR("*sequence*: Battery capacity percent for lifetime [%]"),
 	NULL},
{"batt_capacity_thermal_percent", (getter)Outputs_get_batt_capacity_thermal_percent,(setter)0,
	PyDoc_STR("*sequence*: Battery capacity percent for temperature [%]"),
 	NULL},
{"batt_conversion_loss", (getter)Outputs_get_batt_conversion_loss,(setter)0,
	PyDoc_STR("*sequence*: Electricity loss in battery power electronics [kW]"),
 	NULL},
{"batt_cost_to_cycle", (getter)Outputs_get_batt_cost_to_cycle,(setter)0,
	PyDoc_STR("*sequence*: Computed cost to cycle [$/cycle]"),
 	NULL},
{"batt_cycles", (getter)Outputs_get_batt_cycles,(setter)0,
	PyDoc_STR("*sequence*: Battery number of cycles"),
 	NULL},
{"batt_dispatch_sched", (getter)Outputs_get_batt_dispatch_sched,(setter)0,
	PyDoc_STR("*sequence[sequence]*: Battery dispatch schedule"),
 	NULL},
{"batt_power", (getter)Outputs_get_batt_power,(setter)0,
	PyDoc_STR("*sequence*: Electricity to/from battery [kW]"),
 	NULL},
{"batt_power_target", (getter)Outputs_get_batt_power_target,(setter)0,
	PyDoc_STR("*sequence*: Electricity battery power target for automated dispatch [kW]"),
 	NULL},
{"batt_pv_charge_percent", (getter)Outputs_get_batt_pv_charge_percent,(setter)0,
	PyDoc_STR("*float*: Battery percent energy charged from PV [%]"),
 	NULL},
{"batt_q0", (getter)Outputs_get_batt_q0,(setter)0,
	PyDoc_STR("*sequence*: Battery total charge [Ah]"),
 	NULL},
{"batt_q1", (getter)Outputs_get_batt_q1,(setter)0,
	PyDoc_STR("*sequence*: Battery available charge [Ah]"),
 	NULL},
{"batt_q2", (getter)Outputs_get_batt_q2,(setter)0,
	PyDoc_STR("*sequence*: Battery bound charge [Ah]"),
 	NULL},
{"batt_qmax", (getter)Outputs_get_batt_qmax,(setter)0,
	PyDoc_STR("*sequence*: Battery maximum charge with degradation [Ah]"),
 	NULL},
{"batt_qmaxI", (getter)Outputs_get_batt_qmaxI,(setter)0,
	PyDoc_STR("*sequence*: Battery maximum capacity at current [Ah]"),
 	NULL},
{"batt_qmax_thermal", (getter)Outputs_get_batt_qmax_thermal,(setter)0,
	PyDoc_STR("*sequence*: Battery maximum charge at temperature [Ah]"),
 	NULL},
{"batt_system_loss", (getter)Outputs_get_batt_system_loss,(setter)0,
	PyDoc_STR("*sequence*: Electricity loss from battery ancillary equipment [kW]"),
 	NULL},
{"batt_temperature", (getter)Outputs_get_batt_temperature,(setter)0,
	PyDoc_STR("*sequence*: Battery temperature [C]"),
 	NULL},
{"batt_to_grid", (getter)Outputs_get_batt_to_grid,(setter)0,
	PyDoc_STR("*sequence*: Electricity to grid from battery [kW]"),
 	NULL},
{"batt_to_load", (getter)Outputs_get_batt_to_load,(setter)0,
	PyDoc_STR("*sequence*: Electricity to load from battery [kW]"),
 	NULL},
{"batt_voltage", (getter)Outputs_get_batt_voltage,(setter)0,
	PyDoc_STR("*sequence*: Battery voltage [V]"),
 	NULL},
{"batt_voltage_cell", (getter)Outputs_get_batt_voltage_cell,(setter)0,
	PyDoc_STR("*sequence*: Battery cell voltage [V]"),
 	NULL},
{"capacity_factor", (getter)Outputs_get_capacity_factor,(setter)0,
	PyDoc_STR("*float*: Capacity factor [%]"),
 	NULL},
{"capacity_factor_ac", (getter)Outputs_get_capacity_factor_ac,(setter)0,
	PyDoc_STR("*float*: Capacity factor based on AC system capacity [%]"),
 	NULL},
{"dc_degrade_factor", (getter)Outputs_get_dc_degrade_factor,(setter)0,
	PyDoc_STR("*sequence*: Annual module degrade factor"),
 	NULL},
{"dc_invmppt_loss", (getter)Outputs_get_dc_invmppt_loss,(setter)0,
	PyDoc_STR("*sequence*: Inverter clipping loss DC MPPT voltage limits [kW]"),
 	NULL},
{"dc_net", (getter)Outputs_get_dc_net,(setter)0,
	PyDoc_STR("*sequence*: Array DC power [kW]"),
 	NULL},
{"dc_snow_loss", (getter)Outputs_get_dc_snow_loss,(setter)0,
	PyDoc_STR("*sequence*: Array DC power loss due to snow [kW]"),
 	NULL},
{"df", (getter)Outputs_get_df,(setter)0,
	PyDoc_STR("*sequence*: Irradiance DHI from weather file [W/m2]"),
 	NULL},
{"df_calc", (getter)Outputs_get_df_calc,(setter)0,
	PyDoc_STR("*sequence*: Irradiance DHI calculated [W/m2]"),
 	NULL},
{"dn", (getter)Outputs_get_dn,(setter)0,
	PyDoc_STR("*sequence*: Irradiance DNI from weather file [W/m2]"),
 	NULL},
{"dn_calc", (getter)Outputs_get_dn_calc,(setter)0,
	PyDoc_STR("*sequence*: Irradiance DNI calculated [W/m2]"),
 	NULL},
{"fuelcell_to_batt", (getter)Outputs_get_fuelcell_to_batt,(setter)0,
	PyDoc_STR("*sequence*: Electricity to battery from fuel cell [kW]"),
 	NULL},
{"gen", (getter)Outputs_get_gen,(setter)0,
	PyDoc_STR("*sequence*: System power generated [kW]"),
 	NULL},
{"gh", (getter)Outputs_get_gh,(setter)0,
	PyDoc_STR("*sequence*: Irradiance GHI from weather file [W/m2]"),
 	NULL},
{"gh_calc", (getter)Outputs_get_gh_calc,(setter)0,
	PyDoc_STR("*sequence*: Irradiance GHI calculated [W/m2]"),
 	NULL},
{"grid_power", (getter)Outputs_get_grid_power,(setter)0,
	PyDoc_STR("*sequence*: Electricity to/from grid [kW]"),
 	NULL},
{"grid_power_target", (getter)Outputs_get_grid_power_target,(setter)0,
	PyDoc_STR("*sequence*: Electricity grid power target for automated dispatch [kW]"),
 	NULL},
{"grid_to_batt", (getter)Outputs_get_grid_to_batt,(setter)0,
	PyDoc_STR("*sequence*: Electricity to battery from grid [kW]"),
 	NULL},
{"grid_to_load", (getter)Outputs_get_grid_to_load,(setter)0,
	PyDoc_STR("*sequence*: Electricity to load from grid [kW]"),
 	NULL},
{"inv_cliploss", (getter)Outputs_get_inv_cliploss,(setter)0,
	PyDoc_STR("*sequence*: Inverter clipping loss AC power limit [kW]"),
 	NULL},
{"inv_eff", (getter)Outputs_get_inv_eff,(setter)0,
	PyDoc_STR("*sequence*: Inverter efficiency [%]"),
 	NULL},
{"inv_pntloss", (getter)Outputs_get_inv_pntloss,(setter)0,
	PyDoc_STR("*sequence*: Inverter night time loss [kW]"),
 	NULL},
{"inv_psoloss", (getter)Outputs_get_inv_psoloss,(setter)0,
	PyDoc_STR("*sequence*: Inverter power consumption loss [kW]"),
 	NULL},
{"inv_tdcloss", (getter)Outputs_get_inv_tdcloss,(setter)0,
	PyDoc_STR("*sequence*: Inverter thermal derate loss [kW]"),
 	NULL},
{"inv_total_loss", (getter)Outputs_get_inv_total_loss,(setter)0,
	PyDoc_STR("*sequence*: Inverter total power loss [kW]"),
 	NULL},
{"inverterMPPT1_DCVoltage", (getter)Outputs_get_inverterMPPT1_DCVoltage,(setter)0,
	PyDoc_STR("*sequence*: Inverter MPPT 1 Nominal DC voltage [V]"),
 	NULL},
{"inverterMPPT2_DCVoltage", (getter)Outputs_get_inverterMPPT2_DCVoltage,(setter)0,
	PyDoc_STR("*sequence*: Inverter MPPT 2 Nominal DC voltage [V]"),
 	NULL},
{"inverterMPPT3_DCVoltage", (getter)Outputs_get_inverterMPPT3_DCVoltage,(setter)0,
	PyDoc_STR("*sequence*: Inverter MPPT 3 Nominal DC voltage [V]"),
 	NULL},
{"inverterMPPT4_DCVoltage", (getter)Outputs_get_inverterMPPT4_DCVoltage,(setter)0,
	PyDoc_STR("*sequence*: Inverter MPPT 4 Nominal DC voltage [V]"),
 	NULL},
{"kwh_per_kw", (getter)Outputs_get_kwh_per_kw,(setter)0,
	PyDoc_STR("*float*: First year kWh(AC)/kW(DC) [kWh/kW]"),
 	NULL},
{"monthly_batt_to_grid", (getter)Outputs_get_monthly_batt_to_grid,(setter)0,
	PyDoc_STR("*sequence*: Energy to grid from battery [kWh]"),
 	NULL},
{"monthly_batt_to_load", (getter)Outputs_get_monthly_batt_to_load,(setter)0,
	PyDoc_STR("*sequence*: Energy to load from battery [kWh]"),
 	NULL},
{"monthly_dc", (getter)Outputs_get_monthly_dc,(setter)0,
	PyDoc_STR("*sequence*: PV array DC energy [kWh/mo]"),
 	NULL},
{"monthly_energy", (getter)Outputs_get_monthly_energy,(setter)0,
	PyDoc_STR("*sequence*: System AC energy [kWh/mo]"),
 	NULL},
{"monthly_grid_to_batt", (getter)Outputs_get_monthly_grid_to_batt,(setter)0,
	PyDoc_STR("*sequence*: Energy to battery from grid [kWh]"),
 	NULL},
{"monthly_grid_to_load", (getter)Outputs_get_monthly_grid_to_load,(setter)0,
	PyDoc_STR("*sequence*: Energy to load from grid [kWh]"),
 	NULL},
{"monthly_poa_beam_eff", (getter)Outputs_get_monthly_poa_beam_eff,(setter)0,
	PyDoc_STR("*sequence*: POA front-side irradiance beam after shading and soiling [kWh/mo]"),
 	NULL},
{"monthly_poa_beam_nom", (getter)Outputs_get_monthly_poa_beam_nom,(setter)0,
	PyDoc_STR("*sequence*: POA front-side irradiance beam nominal [kWh/mo]"),
 	NULL},
{"monthly_poa_eff", (getter)Outputs_get_monthly_poa_eff,(setter)0,
	PyDoc_STR("*sequence*: POA irradiance total after shading and soiling [kWh/mo]"),
 	NULL},
{"monthly_poa_front", (getter)Outputs_get_monthly_poa_front,(setter)0,
	PyDoc_STR("*sequence*: POA front-side irradiance total [kWh/mo]"),
 	NULL},
{"monthly_poa_nom", (getter)Outputs_get_monthly_poa_nom,(setter)0,
	PyDoc_STR("*sequence*: POA front-side irradiance total nominal [kWh/mo]"),
 	NULL},
{"monthly_poa_rear", (getter)Outputs_get_monthly_poa_rear,(setter)0,
	PyDoc_STR("*sequence*: POA rear-side irradiance total [kWh/mo]"),
 	NULL},
{"monthly_pv_to_batt", (getter)Outputs_get_monthly_pv_to_batt,(setter)0,
	PyDoc_STR("*sequence*: Energy to battery from PV [kWh]"),
 	NULL},
{"monthly_pv_to_grid", (getter)Outputs_get_monthly_pv_to_grid,(setter)0,
	PyDoc_STR("*sequence*: Energy to grid from PV [kWh]"),
 	NULL},
{"monthly_pv_to_load", (getter)Outputs_get_monthly_pv_to_load,(setter)0,
	PyDoc_STR("*sequence*: Energy to load from PV [kWh]"),
 	NULL},
{"monthly_snow_loss", (getter)Outputs_get_monthly_snow_loss,(setter)0,
	PyDoc_STR("*sequence*: Snow DC energy loss [kWh/mo]"),
 	NULL},
{"nameplate_dc_rating", (getter)Outputs_get_nameplate_dc_rating,(setter)0,
	PyDoc_STR("*float*: System nameplate DC rating [kW]"),
 	NULL},
{"performance_ratio", (getter)Outputs_get_performance_ratio,(setter)0,
	PyDoc_STR("*float*: Performance ratio"),
 	NULL},
{"poa_beam_eff", (getter)Outputs_get_poa_beam_eff,(setter)0,
	PyDoc_STR("*sequence*: Array POA beam radiation after shading and soiling [kW]"),
 	NULL},
{"poa_beam_nom", (getter)Outputs_get_poa_beam_nom,(setter)0,
	PyDoc_STR("*sequence*: Array POA front-side beam radiation nominal [kW]"),
 	NULL},
{"poa_eff", (getter)Outputs_get_poa_eff,(setter)0,
	PyDoc_STR("*sequence*: Array POA radiation total after reflection (IAM) [kW]"),
 	NULL},
{"poa_front", (getter)Outputs_get_poa_front,(setter)0,
	PyDoc_STR("*sequence*: Array POA front-side total radiation after reflection (IAM) [kW]"),
 	NULL},
{"poa_nom", (getter)Outputs_get_poa_nom,(setter)0,
	PyDoc_STR("*sequence*: Array POA front-side total radiation nominal [kW]"),
 	NULL},
{"poa_rear", (getter)Outputs_get_poa_rear,(setter)0,
	PyDoc_STR("*sequence*: Array POA rear-side total radiation after reflection (IAM) [kW]"),
 	NULL},
{"poa_shaded", (getter)Outputs_get_poa_shaded,(setter)0,
	PyDoc_STR("*sequence*: Array POA front-side total radiation after shading only [kW]"),
 	NULL},
{"poa_shaded_soiled", (getter)Outputs_get_poa_shaded_soiled,(setter)0,
	PyDoc_STR("*sequence*: Array POA front-side total radiation after shading and soiling [kW]"),
 	NULL},
{"pv_to_batt", (getter)Outputs_get_pv_to_batt,(setter)0,
	PyDoc_STR("*sequence*: Electricity to battery from PV [kW]"),
 	NULL},
{"pv_to_grid", (getter)Outputs_get_pv_to_grid,(setter)0,
	PyDoc_STR("*sequence*: Electricity to grid from PV [kW]"),
 	NULL},
{"pv_to_load", (getter)Outputs_get_pv_to_load,(setter)0,
	PyDoc_STR("*sequence*: Electricity to load from PV [kW]"),
 	NULL},
{"shadedb_subarray1_shade_frac", (getter)Outputs_get_shadedb_subarray1_shade_frac,(setter)0,
	PyDoc_STR("*sequence*: Subarray 1 Partial external shading DC factor [frac]"),
 	NULL},
{"shadedb_subarray2_shade_frac", (getter)Outputs_get_shadedb_subarray2_shade_frac,(setter)0,
	PyDoc_STR("*sequence*: Subarray 2 Partial shading DC factor [frac]"),
 	NULL},
{"shadedb_subarray3_shade_frac", (getter)Outputs_get_shadedb_subarray3_shade_frac,(setter)0,
	PyDoc_STR("*sequence*: Subarray 3 Partial external shading DC factor [frac]"),
 	NULL},
{"shadedb_subarray4_shade_frac", (getter)Outputs_get_shadedb_subarray4_shade_frac,(setter)0,
	PyDoc_STR("*sequence*: Subarray 4 Partial external shading DC factor [frac]"),
 	NULL},
{"snowdepth", (getter)Outputs_get_snowdepth,(setter)0,
	PyDoc_STR("*sequence*: Weather file snow depth [cm]"),
 	NULL},
{"sol_alt", (getter)Outputs_get_sol_alt,(setter)0,
	PyDoc_STR("*sequence*: Sun altitude angle [deg]"),
 	NULL},
{"sol_azi", (getter)Outputs_get_sol_azi,(setter)0,
	PyDoc_STR("*sequence*: Sun azimuth angle [deg]"),
 	NULL},
{"sol_zen", (getter)Outputs_get_sol_zen,(setter)0,
	PyDoc_STR("*sequence*: Sun zenith angle [deg]"),
 	NULL},
{"subarray1_aoi", (getter)Outputs_get_subarray1_aoi,(setter)0,
	PyDoc_STR("*sequence*: Subarray 1 Angle of incidence [deg]"),
 	NULL},
{"subarray1_aoi_modifier", (getter)Outputs_get_subarray1_aoi_modifier,(setter)0,
	PyDoc_STR("*sequence*: Subarray 1 Angle of incidence Modifier [0-1]"),
 	NULL},
{"subarray1_axisrot", (getter)Outputs_get_subarray1_axisrot,(setter)0,
	PyDoc_STR("*sequence*: Subarray 1 Axis rotation for 1 axis trackers [deg]"),
 	NULL},
{"subarray1_beam_shading_factor", (getter)Outputs_get_subarray1_beam_shading_factor,(setter)0,
	PyDoc_STR("*sequence*: Subarray 1 External shading and soiling beam irradiance factor [frac]"),
 	NULL},
{"subarray1_celltemp", (getter)Outputs_get_subarray1_celltemp,(setter)0,
	PyDoc_STR("*sequence*: Subarray 1 Cell temperature [C]"),
 	NULL},
{"subarray1_dc_gross", (getter)Outputs_get_subarray1_dc_gross,(setter)0,
	PyDoc_STR("*sequence*: Subarray 1 DC power gross [kW]"),
 	NULL},
{"subarray1_dc_voltage", (getter)Outputs_get_subarray1_dc_voltage,(setter)0,
	PyDoc_STR("*sequence*: Subarray 1 Operating DC voltage [V]"),
 	NULL},
{"subarray1_dcloss", (getter)Outputs_get_subarray1_dcloss,(setter)0,
	PyDoc_STR("*float*: Subarray 1 Total DC power loss [%]"),
 	NULL},
{"subarray1_idealrot", (getter)Outputs_get_subarray1_idealrot,(setter)0,
	PyDoc_STR("*sequence*: Subarray 1 Axis rotation ideal for 1 axis trackers [deg]"),
 	NULL},
{"subarray1_isc", (getter)Outputs_get_subarray1_isc,(setter)0,
	PyDoc_STR("*sequence*: Subarray 1 Short circuit DC current [A]"),
 	NULL},
{"subarray1_linear_derate", (getter)Outputs_get_subarray1_linear_derate,(setter)0,
	PyDoc_STR("*sequence*: Subarray 1 Self-shading linear beam irradiance factor [frac]"),
 	NULL},
{"subarray1_modeff", (getter)Outputs_get_subarray1_modeff,(setter)0,
	PyDoc_STR("*sequence*: Subarray 1 Module efficiency [%]"),
 	NULL},
{"subarray1_poa_eff", (getter)Outputs_get_subarray1_poa_eff,(setter)0,
	PyDoc_STR("*sequence*: Subarray 1 POA total irradiance after reflection (IAM) [W/m2]"),
 	NULL},
{"subarray1_poa_eff_beam", (getter)Outputs_get_subarray1_poa_eff_beam,(setter)0,
	PyDoc_STR("*sequence*: Subarray 1 POA front beam irradiance after shading and soiling [W/m2]"),
 	NULL},
{"subarray1_poa_eff_diff", (getter)Outputs_get_subarray1_poa_eff_diff,(setter)0,
	PyDoc_STR("*sequence*: Subarray 1 POA front diffuse irradiance after shading and soiling [W/m2]"),
 	NULL},
{"subarray1_poa_front", (getter)Outputs_get_subarray1_poa_front,(setter)0,
	PyDoc_STR("*sequence*: Subarray 1 POA front total irradiance after reflection (IAM) [W/m2]"),
 	NULL},
{"subarray1_poa_nom", (getter)Outputs_get_subarray1_poa_nom,(setter)0,
	PyDoc_STR("*sequence*: Subarray 1 POA front total irradiance nominal [W/m2]"),
 	NULL},
{"subarray1_poa_rear", (getter)Outputs_get_subarray1_poa_rear,(setter)0,
	PyDoc_STR("*sequence*: Subarray 1 POA rear total irradiance after reflection (IAM) [W/m2]"),
 	NULL},
{"subarray1_poa_shaded", (getter)Outputs_get_subarray1_poa_shaded,(setter)0,
	PyDoc_STR("*sequence*: Subarray 1 POA front total irradiance after shading only [W/m2]"),
 	NULL},
{"subarray1_poa_shaded_soiled", (getter)Outputs_get_subarray1_poa_shaded_soiled,(setter)0,
	PyDoc_STR("*sequence*: Subarray 1 POA front total irradiance after shading and soiling [W/m2]"),
 	NULL},
{"subarray1_snow_coverage", (getter)Outputs_get_subarray1_snow_coverage,(setter)0,
	PyDoc_STR("*sequence*: Subarray 1 Snow cover [0..1]"),
 	NULL},
{"subarray1_snow_loss", (getter)Outputs_get_subarray1_snow_loss,(setter)0,
	PyDoc_STR("*sequence*: Subarray 1 Snow cover DC power loss [kW]"),
 	NULL},
{"subarray1_soiling_derate", (getter)Outputs_get_subarray1_soiling_derate,(setter)0,
	PyDoc_STR("*sequence*: Subarray 1 Soiling beam irradiance factor [frac]"),
 	NULL},
{"subarray1_ss_derate", (getter)Outputs_get_subarray1_ss_derate,(setter)0,
	PyDoc_STR("*sequence*: Subarray 1 Self-shading non-linear DC factor [frac]"),
 	NULL},
{"subarray1_ss_diffuse_derate", (getter)Outputs_get_subarray1_ss_diffuse_derate,(setter)0,
	PyDoc_STR("*sequence*: Subarray 1 Self-shading non-linear sky diffuse irradiance factor [frac]"),
 	NULL},
{"subarray1_ss_reflected_derate", (getter)Outputs_get_subarray1_ss_reflected_derate,(setter)0,
	PyDoc_STR("*sequence*: Subarray 1 Self-shading non-linear ground diffuse irradiance factor [frac]"),
 	NULL},
{"subarray1_surf_azi", (getter)Outputs_get_subarray1_surf_azi,(setter)0,
	PyDoc_STR("*sequence*: Subarray 1 Surface azimuth [deg]"),
 	NULL},
{"subarray1_surf_tilt", (getter)Outputs_get_subarray1_surf_tilt,(setter)0,
	PyDoc_STR("*sequence*: Subarray 1 Surface tilt [deg]"),
 	NULL},
{"subarray1_voc", (getter)Outputs_get_subarray1_voc,(setter)0,
	PyDoc_STR("*sequence*: Subarray 1 Open circuit DC voltage [V]"),
 	NULL},
{"subarray2_aoi", (getter)Outputs_get_subarray2_aoi,(setter)0,
	PyDoc_STR("*sequence*: Subarray 2 Angle of incidence [deg]"),
 	NULL},
{"subarray2_aoi_modifier", (getter)Outputs_get_subarray2_aoi_modifier,(setter)0,
	PyDoc_STR("*sequence*: Subarray 2 Angle of incidence Modifier [0-1]"),
 	NULL},
{"subarray2_axisrot", (getter)Outputs_get_subarray2_axisrot,(setter)0,
	PyDoc_STR("*sequence*: Subarray 2 Axis rotation for 1 axis trackers [deg]"),
 	NULL},
{"subarray2_beam_shading_factor", (getter)Outputs_get_subarray2_beam_shading_factor,(setter)0,
	PyDoc_STR("*sequence*: Subarray 2 External shading and soiling beam irradiance factor [frac]"),
 	NULL},
{"subarray2_celltemp", (getter)Outputs_get_subarray2_celltemp,(setter)0,
	PyDoc_STR("*sequence*: Subarray 2 Cell temperature [C]"),
 	NULL},
{"subarray2_dc_gross", (getter)Outputs_get_subarray2_dc_gross,(setter)0,
	PyDoc_STR("*sequence*: Subarray 2 DC power gross [kW]"),
 	NULL},
{"subarray2_dc_voltage", (getter)Outputs_get_subarray2_dc_voltage,(setter)0,
	PyDoc_STR("*sequence*: Subarray 2 Operating DC voltage [V]"),
 	NULL},
{"subarray2_dcloss", (getter)Outputs_get_subarray2_dcloss,(setter)0,
	PyDoc_STR("*float*: Subarray 2 Total DC power loss [%]"),
 	NULL},
{"subarray2_idealrot", (getter)Outputs_get_subarray2_idealrot,(setter)0,
	PyDoc_STR("*sequence*: Subarray 2 Axis rotation ideal for 1 axis trackers [deg]"),
 	NULL},
{"subarray2_isc", (getter)Outputs_get_subarray2_isc,(setter)0,
	PyDoc_STR("*sequence*: Subarray 2 Short circuit DC current [A]"),
 	NULL},
{"subarray2_linear_derate", (getter)Outputs_get_subarray2_linear_derate,(setter)0,
	PyDoc_STR("*sequence*: Subarray 2 Self-shading linear beam irradiance factor [frac]"),
 	NULL},
{"subarray2_modeff", (getter)Outputs_get_subarray2_modeff,(setter)0,
	PyDoc_STR("*sequence*: Subarray 2 Module efficiency [%]"),
 	NULL},
{"subarray2_poa_eff", (getter)Outputs_get_subarray2_poa_eff,(setter)0,
	PyDoc_STR("*sequence*: Subarray 2 POA total irradiance after module reflection (IAM) [W/m2]"),
 	NULL},
{"subarray2_poa_eff_beam", (getter)Outputs_get_subarray2_poa_eff_beam,(setter)0,
	PyDoc_STR("*sequence*: Subarray 2 POA front beam irradiance after shading and soiling [W/m2]"),
 	NULL},
{"subarray2_poa_eff_diff", (getter)Outputs_get_subarray2_poa_eff_diff,(setter)0,
	PyDoc_STR("*sequence*: Subarray 2 POA front diffuse irradiance after shading and soiling [W/m2]"),
 	NULL},
{"subarray2_poa_front", (getter)Outputs_get_subarray2_poa_front,(setter)0,
	PyDoc_STR("*sequence*: Subarray 2 POA front total irradiance after reflection (IAM) [W/m2]"),
 	NULL},
{"subarray2_poa_nom", (getter)Outputs_get_subarray2_poa_nom,(setter)0,
	PyDoc_STR("*sequence*: Subarray 2 POA front total irradiance nominal [W/m2]"),
 	NULL},
{"subarray2_poa_rear", (getter)Outputs_get_subarray2_poa_rear,(setter)0,
	PyDoc_STR("*sequence*: Subarray 2 POA rear irradiance after reflection (IAM) [W/m2]"),
 	NULL},
{"subarray2_poa_shaded", (getter)Outputs_get_subarray2_poa_shaded,(setter)0,
	PyDoc_STR("*sequence*: Subarray 2 POA front total irradiance after shading only [W/m2]"),
 	NULL},
{"subarray2_poa_shaded_soiled", (getter)Outputs_get_subarray2_poa_shaded_soiled,(setter)0,
	PyDoc_STR("*sequence*: Subarray 2 POA front total irradiance after shading and soiling [W/m2]"),
 	NULL},
{"subarray2_snow_coverage", (getter)Outputs_get_subarray2_snow_coverage,(setter)0,
	PyDoc_STR("*sequence*: Subarray 2 Snow cover [0..1]"),
 	NULL},
{"subarray2_snow_loss", (getter)Outputs_get_subarray2_snow_loss,(setter)0,
	PyDoc_STR("*sequence*: Subarray 2 Snow cover DC power loss [kW]"),
 	NULL},
{"subarray2_soiling_derate", (getter)Outputs_get_subarray2_soiling_derate,(setter)0,
	PyDoc_STR("*sequence*: Subarray 2 Soiling beam irradiance factor [frac]"),
 	NULL},
{"subarray2_ss_derate", (getter)Outputs_get_subarray2_ss_derate,(setter)0,
	PyDoc_STR("*sequence*: Subarray 2 Self-shading non-linear DC factor [frac]"),
 	NULL},
{"subarray2_ss_diffuse_derate", (getter)Outputs_get_subarray2_ss_diffuse_derate,(setter)0,
	PyDoc_STR("*sequence*: Subarray 2 Self-shading non-linear sky diffuse irradiance factor [frac]"),
 	NULL},
{"subarray2_ss_reflected_derate", (getter)Outputs_get_subarray2_ss_reflected_derate,(setter)0,
	PyDoc_STR("*sequence*: Subarray 2 Self-shading non-linear ground diffuse irradiance factor [frac]"),
 	NULL},
{"subarray2_surf_azi", (getter)Outputs_get_subarray2_surf_azi,(setter)0,
	PyDoc_STR("*sequence*: Subarray 2 Surface azimuth [deg]"),
 	NULL},
{"subarray2_surf_tilt", (getter)Outputs_get_subarray2_surf_tilt,(setter)0,
	PyDoc_STR("*sequence*: Subarray 2 Surface tilt [deg]"),
 	NULL},
{"subarray2_voc", (getter)Outputs_get_subarray2_voc,(setter)0,
	PyDoc_STR("*sequence*: Subarray 2 Open circuit DC voltage [V]"),
 	NULL},
{"subarray3_aoi", (getter)Outputs_get_subarray3_aoi,(setter)0,
	PyDoc_STR("*sequence*: Subarray 3 Angle of incidence [deg]"),
 	NULL},
{"subarray3_aoi_modifier", (getter)Outputs_get_subarray3_aoi_modifier,(setter)0,
	PyDoc_STR("*sequence*: Subarray 3 Angle of incidence Modifier [0-1]"),
 	NULL},
{"subarray3_axisrot", (getter)Outputs_get_subarray3_axisrot,(setter)0,
	PyDoc_STR("*sequence*: Subarray 3 Axis rotation for 1 axis trackers [deg]"),
 	NULL},
{"subarray3_beam_shading_factor", (getter)Outputs_get_subarray3_beam_shading_factor,(setter)0,
	PyDoc_STR("*sequence*: Subarray 3 External shading and soiling beam irradiance factor [frac]"),
 	NULL},
{"subarray3_celltemp", (getter)Outputs_get_subarray3_celltemp,(setter)0,
	PyDoc_STR("*sequence*: Subarray 3 Cell temperature [C]"),
 	NULL},
{"subarray3_dc_gross", (getter)Outputs_get_subarray3_dc_gross,(setter)0,
	PyDoc_STR("*sequence*: Subarray 3 DC power gross [kW]"),
 	NULL},
{"subarray3_dc_voltage", (getter)Outputs_get_subarray3_dc_voltage,(setter)0,
	PyDoc_STR("*sequence*: Subarray 3 Operating DC voltage [V]"),
 	NULL},
{"subarray3_dcloss", (getter)Outputs_get_subarray3_dcloss,(setter)0,
	PyDoc_STR("*float*: Subarray 3 Total DC power loss [%]"),
 	NULL},
{"subarray3_idealrot", (getter)Outputs_get_subarray3_idealrot,(setter)0,
	PyDoc_STR("*sequence*: Subarray 3 Axis rotation ideal for 1 axis trackers [deg]"),
 	NULL},
{"subarray3_isc", (getter)Outputs_get_subarray3_isc,(setter)0,
	PyDoc_STR("*sequence*: Subarray 3 Short circuit DC current [A]"),
 	NULL},
{"subarray3_linear_derate", (getter)Outputs_get_subarray3_linear_derate,(setter)0,
	PyDoc_STR("*sequence*: Subarray 3 Self-shading linear beam irradiance factor [frac]"),
 	NULL},
{"subarray3_modeff", (getter)Outputs_get_subarray3_modeff,(setter)0,
	PyDoc_STR("*sequence*: Subarray 3 Module efficiency [%]"),
 	NULL},
{"subarray3_poa_eff", (getter)Outputs_get_subarray3_poa_eff,(setter)0,
	PyDoc_STR("*sequence*: Subarray 3 POA total irradiance after reflection (IAM) [W/m2]"),
 	NULL},
{"subarray3_poa_eff_beam", (getter)Outputs_get_subarray3_poa_eff_beam,(setter)0,
	PyDoc_STR("*sequence*: Subarray 3 POA front beam irradiance after shading and soiling [W/m2]"),
 	NULL},
{"subarray3_poa_eff_diff", (getter)Outputs_get_subarray3_poa_eff_diff,(setter)0,
	PyDoc_STR("*sequence*: Subarray 3 POA front diffuse irradiance after shading and soiling [W/m2]"),
 	NULL},
{"subarray3_poa_front", (getter)Outputs_get_subarray3_poa_front,(setter)0,
	PyDoc_STR("*sequence*: Subarray 3 POA front total irradiance after reflection (IAM) [W/m2]"),
 	NULL},
{"subarray3_poa_nom", (getter)Outputs_get_subarray3_poa_nom,(setter)0,
	PyDoc_STR("*sequence*: Subarray 3 POA font total irradiance nominal [W/m2]"),
 	NULL},
{"subarray3_poa_rear", (getter)Outputs_get_subarray3_poa_rear,(setter)0,
	PyDoc_STR("*sequence*: Subarray 3 POA rear irradiance after reflection (IAM) [W/m2]"),
 	NULL},
{"subarray3_poa_shaded", (getter)Outputs_get_subarray3_poa_shaded,(setter)0,
	PyDoc_STR("*sequence*: Subarray 3 POA front total irradiance after shading only [W/m2]"),
 	NULL},
{"subarray3_poa_shaded_soiled", (getter)Outputs_get_subarray3_poa_shaded_soiled,(setter)0,
	PyDoc_STR("*sequence*: Subarray 3 POA front total irradiance after shading and soiling [W/m2]"),
 	NULL},
{"subarray3_snow_coverage", (getter)Outputs_get_subarray3_snow_coverage,(setter)0,
	PyDoc_STR("*sequence*: Subarray 3 Snow cover [0..1]"),
 	NULL},
{"subarray3_snow_loss", (getter)Outputs_get_subarray3_snow_loss,(setter)0,
	PyDoc_STR("*sequence*: Subarray 3 Snow cover DC power loss [kW]"),
 	NULL},
{"subarray3_soiling_derate", (getter)Outputs_get_subarray3_soiling_derate,(setter)0,
	PyDoc_STR("*sequence*: Subarray 3 Soiling beam irradiance factor [frac]"),
 	NULL},
{"subarray3_ss_derate", (getter)Outputs_get_subarray3_ss_derate,(setter)0,
	PyDoc_STR("*sequence*: Subarray 3 Self-shading non-linear DC factor [frac]"),
 	NULL},
{"subarray3_ss_diffuse_derate", (getter)Outputs_get_subarray3_ss_diffuse_derate,(setter)0,
	PyDoc_STR("*sequence*: Subarray 3 Self-shading non-linear sky diffuse irradiance factor [frac]"),
 	NULL},
{"subarray3_ss_reflected_derate", (getter)Outputs_get_subarray3_ss_reflected_derate,(setter)0,
	PyDoc_STR("*sequence*: Subarray 3 Self-shading non-linear ground diffuse irradiance factor [frac]"),
 	NULL},
{"subarray3_surf_azi", (getter)Outputs_get_subarray3_surf_azi,(setter)0,
	PyDoc_STR("*sequence*: Subarray 3 Surface azimuth [deg]"),
 	NULL},
{"subarray3_surf_tilt", (getter)Outputs_get_subarray3_surf_tilt,(setter)0,
	PyDoc_STR("*sequence*: Subarray 3 Surface tilt [deg]"),
 	NULL},
{"subarray3_voc", (getter)Outputs_get_subarray3_voc,(setter)0,
	PyDoc_STR("*sequence*: Subarray 3 Open circuit DC voltage [V]"),
 	NULL},
{"subarray4_aoi", (getter)Outputs_get_subarray4_aoi,(setter)0,
	PyDoc_STR("*sequence*: Subarray 4 Angle of incidence [deg]"),
 	NULL},
{"subarray4_aoi_modifier", (getter)Outputs_get_subarray4_aoi_modifier,(setter)0,
	PyDoc_STR("*sequence*: Subarray 4 Angle of incidence Modifier [0-1]"),
 	NULL},
{"subarray4_axisrot", (getter)Outputs_get_subarray4_axisrot,(setter)0,
	PyDoc_STR("*sequence*: Subarray 4 Axis rotation for 1 axis trackers [deg]"),
 	NULL},
{"subarray4_beam_shading_factor", (getter)Outputs_get_subarray4_beam_shading_factor,(setter)0,
	PyDoc_STR("*sequence*: Subarray 4 External shading and soiling beam irradiance factor [frac]"),
 	NULL},
{"subarray4_celltemp", (getter)Outputs_get_subarray4_celltemp,(setter)0,
	PyDoc_STR("*sequence*: Subarray 4 Cell temperature [C]"),
 	NULL},
{"subarray4_dc_gross", (getter)Outputs_get_subarray4_dc_gross,(setter)0,
	PyDoc_STR("*sequence*: Subarray 4 DC power gross [kW]"),
 	NULL},
{"subarray4_dc_voltage", (getter)Outputs_get_subarray4_dc_voltage,(setter)0,
	PyDoc_STR("*sequence*: Subarray 4 Operating DC voltage [V]"),
 	NULL},
{"subarray4_dcloss", (getter)Outputs_get_subarray4_dcloss,(setter)0,
	PyDoc_STR("*float*: Subarray 4 Total DC power loss [%]"),
 	NULL},
{"subarray4_idealrot", (getter)Outputs_get_subarray4_idealrot,(setter)0,
	PyDoc_STR("*sequence*: Subarray 4 Axis rotation ideal for 1 axis trackers [deg]"),
 	NULL},
{"subarray4_isc", (getter)Outputs_get_subarray4_isc,(setter)0,
	PyDoc_STR("*sequence*: Subarray 4 Short circuit DC current [A]"),
 	NULL},
{"subarray4_linear_derate", (getter)Outputs_get_subarray4_linear_derate,(setter)0,
	PyDoc_STR("*sequence*: Subarray 4 Self-shading linear beam irradiance factor [frac]"),
 	NULL},
{"subarray4_modeff", (getter)Outputs_get_subarray4_modeff,(setter)0,
	PyDoc_STR("*sequence*: Subarray 4 Module efficiency [%]"),
 	NULL},
{"subarray4_poa_eff", (getter)Outputs_get_subarray4_poa_eff,(setter)0,
	PyDoc_STR("*sequence*: Subarray 4 POA total irradiance after reflection (IAM) [W/m2]"),
 	NULL},
{"subarray4_poa_eff_beam", (getter)Outputs_get_subarray4_poa_eff_beam,(setter)0,
	PyDoc_STR("*sequence*: Subarray 4 POA front beam irradiance after shading and soiling [W/m2]"),
 	NULL},
{"subarray4_poa_eff_diff", (getter)Outputs_get_subarray4_poa_eff_diff,(setter)0,
	PyDoc_STR("*sequence*: Subarray 4 POA front diffuse irradiance after shading and soiling [W/m2]"),
 	NULL},
{"subarray4_poa_front", (getter)Outputs_get_subarray4_poa_front,(setter)0,
	PyDoc_STR("*sequence*: Subarray 4 POA front total irradiance after reflection (IAM) [W/m2]"),
 	NULL},
{"subarray4_poa_nom", (getter)Outputs_get_subarray4_poa_nom,(setter)0,
	PyDoc_STR("*sequence*: Subarray 4 POA front total irradiance nominal [W/m2]"),
 	NULL},
{"subarray4_poa_rear", (getter)Outputs_get_subarray4_poa_rear,(setter)0,
	PyDoc_STR("*sequence*: Subarray 4 POA rear irradiance after reflection (IAM) [W/m2]"),
 	NULL},
{"subarray4_poa_shaded", (getter)Outputs_get_subarray4_poa_shaded,(setter)0,
	PyDoc_STR("*sequence*: Subarray 4 POA front total irradiance after shading only [W/m2]"),
 	NULL},
{"subarray4_poa_shaded_soiled", (getter)Outputs_get_subarray4_poa_shaded_soiled,(setter)0,
	PyDoc_STR("*sequence*: Subarray 4 POA front total irradiance after shading and soiling [W/m2]"),
 	NULL},
{"subarray4_snow_coverage", (getter)Outputs_get_subarray4_snow_coverage,(setter)0,
	PyDoc_STR("*sequence*: Subarray 4 Snow cover [0..1]"),
 	NULL},
{"subarray4_snow_loss", (getter)Outputs_get_subarray4_snow_loss,(setter)0,
	PyDoc_STR("*sequence*: Subarray 4 Snow cover DC power loss [kW]"),
 	NULL},
{"subarray4_soiling_derate", (getter)Outputs_get_subarray4_soiling_derate,(setter)0,
	PyDoc_STR("*sequence*: Subarray 4 Soiling beam irradiance factor [frac]"),
 	NULL},
{"subarray4_ss_derate", (getter)Outputs_get_subarray4_ss_derate,(setter)0,
	PyDoc_STR("*sequence*: Subarray 4 Self-shading non-linear DC factor [frac]"),
 	NULL},
{"subarray4_ss_diffuse_derate", (getter)Outputs_get_subarray4_ss_diffuse_derate,(setter)0,
	PyDoc_STR("*sequence*: Subarray 4 Self-shading non-linear sky diffuse irradiance factor [frac]"),
 	NULL},
{"subarray4_ss_reflected_derate", (getter)Outputs_get_subarray4_ss_reflected_derate,(setter)0,
	PyDoc_STR("*sequence*: Subarray 4 Self-shading non-linear ground diffuse irradiance factor [frac]"),
 	NULL},
{"subarray4_surf_azi", (getter)Outputs_get_subarray4_surf_azi,(setter)0,
	PyDoc_STR("*sequence*: Subarray 4 Surface azimuth [deg]"),
 	NULL},
{"subarray4_surf_tilt", (getter)Outputs_get_subarray4_surf_tilt,(setter)0,
	PyDoc_STR("*sequence*: Subarray 4 Surface tilt [deg]"),
 	NULL},
{"subarray4_voc", (getter)Outputs_get_subarray4_voc,(setter)0,
	PyDoc_STR("*sequence*: Subarray 4 Open circuit DC voltage [V]"),
 	NULL},
{"sunpos_hour", (getter)Outputs_get_sunpos_hour,(setter)0,
	PyDoc_STR("*sequence*: Sun position time [hour]"),
 	NULL},
{"sunup", (getter)Outputs_get_sunup,(setter)0,
	PyDoc_STR("*sequence*: Sun up over horizon [0/1/2/3]"),
 	NULL},
{"tdry", (getter)Outputs_get_tdry,(setter)0,
	PyDoc_STR("*sequence*: Weather file ambient temperature [C]"),
 	NULL},
{"ts_shift_hours", (getter)Outputs_get_ts_shift_hours,(setter)0,
	PyDoc_STR("*float*: Sun position time offset [hours]"),
 	NULL},
{"wfpoa", (getter)Outputs_get_wfpoa,(setter)0,
	PyDoc_STR("*sequence*: Irradiance POA from weather file [W/m2]"),
 	NULL},
{"wspd", (getter)Outputs_get_wspd,(setter)0,
	PyDoc_STR("*sequence*: Weather file wind speed [m/s]"),
 	NULL},
{"xfmr_ll_ts", (getter)Outputs_get_xfmr_ll_ts,(setter)0,
	PyDoc_STR("*sequence*: Transformer load loss [kW]"),
 	NULL},
{"xfmr_ll_year1", (getter)Outputs_get_xfmr_ll_year1,(setter)0,
	PyDoc_STR("*float*: Transformer load loss [kWh/yr]"),
 	NULL},
{"xfmr_loss_ts", (getter)Outputs_get_xfmr_loss_ts,(setter)0,
	PyDoc_STR("*sequence*: Transformer total loss [kW]"),
 	NULL},
{"xfmr_loss_year1", (getter)Outputs_get_xfmr_loss_year1,(setter)0,
	PyDoc_STR("*float*: Transformer total loss [kWh/yr]"),
 	NULL},
{"xfmr_nll_ts", (getter)Outputs_get_xfmr_nll_ts,(setter)0,
	PyDoc_STR("*sequence*: Transformer no load loss [kW]"),
 	NULL},
{"xfmr_nll_year1", (getter)Outputs_get_xfmr_nll_year1,(setter)0,
	PyDoc_STR("*float*: Transformer no load loss [kWh/yr]"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject Outputs_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Pvsamv1.Outputs",             /*tp_name*/
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
 * Pvsamv1
 */

typedef struct {
	PyObject_HEAD
	PyObject            *x_attr;        /* Attributes dictionary */
	SAM_Pvsamv1   data_ptr;
} Pvsamv1Object;

static PyTypeObject Pvsamv1_Type;

#define Pvsamv1Object_Check(v)      (Py_TYPE(v) == &Pvsamv1_Type)

static Pvsamv1Object *
newPvsamv1Object(void* data_ptr)
{
	Pvsamv1Object *self;
	self = PyObject_New(Pvsamv1Object, &Pvsamv1_Type);

	PySAM_TECH_ATTR("Pvsamv1", SAM_Pvsamv1_construct)

	PyObject* SolarResource_obj = SolarResource_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "SolarResource", SolarResource_obj);
	Py_DECREF(SolarResource_obj);

	PyObject* Losses_obj = Losses_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "Losses", Losses_obj);
	Py_DECREF(Losses_obj);

	PyObject* Lifetime_obj = Lifetime_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "Lifetime", Lifetime_obj);
	Py_DECREF(Lifetime_obj);

	PyObject* SystemDesign_obj = SystemDesign_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "SystemDesign", SystemDesign_obj);
	Py_DECREF(SystemDesign_obj);

	PyObject* Shading_obj = Shading_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "Shading", Shading_obj);
	Py_DECREF(Shading_obj);

	PyObject* Layout_obj = Layout_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "Layout", Layout_obj);
	Py_DECREF(Layout_obj);

	PyObject* Module_obj = Module_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "Module", Module_obj);
	Py_DECREF(Module_obj);

	PyObject* SimpleEfficiencyModuleModel_obj = SimpleEfficiencyModuleModel_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "SimpleEfficiencyModuleModel", SimpleEfficiencyModuleModel_obj);
	Py_DECREF(SimpleEfficiencyModuleModel_obj);

	PyObject* CECPerformanceModelWithModuleDatabase_obj = CECPerformanceModelWithModuleDatabase_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "CECPerformanceModelWithModuleDatabase", CECPerformanceModelWithModuleDatabase_obj);
	Py_DECREF(CECPerformanceModelWithModuleDatabase_obj);

	PyObject* CECPerformanceModelWithUserEnteredSpecifications_obj = CECPerformanceModelWithUserEnteredSpecifications_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "CECPerformanceModelWithUserEnteredSpecifications", CECPerformanceModelWithUserEnteredSpecifications_obj);
	Py_DECREF(CECPerformanceModelWithUserEnteredSpecifications_obj);

	PyObject* SandiaPVArrayPerformanceModelWithModuleDatabase_obj = SandiaPVArrayPerformanceModelWithModuleDatabase_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "SandiaPVArrayPerformanceModelWithModuleDatabase", SandiaPVArrayPerformanceModelWithModuleDatabase_obj);
	Py_DECREF(SandiaPVArrayPerformanceModelWithModuleDatabase_obj);

	PyObject* IEC61853SingleDiodeModel_obj = IEC61853SingleDiodeModel_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "IEC61853SingleDiodeModel", IEC61853SingleDiodeModel_obj);
	Py_DECREF(IEC61853SingleDiodeModel_obj);

	PyObject* MermoudLejeuneSingleDiodeModel_obj = MermoudLejeuneSingleDiodeModel_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "MermoudLejeuneSingleDiodeModel", MermoudLejeuneSingleDiodeModel_obj);
	Py_DECREF(MermoudLejeuneSingleDiodeModel_obj);

	PyObject* Inverter_obj = Inverter_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "Inverter", Inverter_obj);
	Py_DECREF(Inverter_obj);

	PyObject* InverterCECDatabase_obj = InverterCECDatabase_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "InverterCECDatabase", InverterCECDatabase_obj);
	Py_DECREF(InverterCECDatabase_obj);

	PyObject* InverterCECCoefficientGenerator_obj = InverterCECCoefficientGenerator_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "InverterCECCoefficientGenerator", InverterCECCoefficientGenerator_obj);
	Py_DECREF(InverterCECCoefficientGenerator_obj);

	PyObject* InverterDatasheet_obj = InverterDatasheet_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "InverterDatasheet", InverterDatasheet_obj);
	Py_DECREF(InverterDatasheet_obj);

	PyObject* InverterPartLoadCurve_obj = InverterPartLoadCurve_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "InverterPartLoadCurve", InverterPartLoadCurve_obj);
	Py_DECREF(InverterPartLoadCurve_obj);

	PyObject* InverterMermoudLejeuneModel_obj = InverterMermoudLejeuneModel_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "InverterMermoudLejeuneModel", InverterMermoudLejeuneModel_obj);
	Py_DECREF(InverterMermoudLejeuneModel_obj);

	PyObject* Battery_obj = Battery_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "Battery", Battery_obj);
	Py_DECREF(Battery_obj);

	PyObject* Simulation_obj = Simulation_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "Simulation", Simulation_obj);
	Py_DECREF(Simulation_obj);

	PyObject* Common_obj = Common_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "Common", Common_obj);
	Py_DECREF(Common_obj);

	PyObject* PV_obj = PV_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "PV", PV_obj);
	Py_DECREF(PV_obj);

	PyObject* FuelCell_obj = FuelCell_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "FuelCell", FuelCell_obj);
	Py_DECREF(FuelCell_obj);

	PyObject* ElectricityRate_obj = ElectricityRate_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "ElectricityRate", ElectricityRate_obj);
	Py_DECREF(ElectricityRate_obj);

	PyObject* EnergyMarket_obj = EnergyMarket_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "EnergyMarket", EnergyMarket_obj);
	Py_DECREF(EnergyMarket_obj);

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

/* Pvsamv1 methods */

static void
Pvsamv1_dealloc(Pvsamv1Object *self)
{
	Py_XDECREF(self->x_attr);
	SAM_Pvsamv1_destruct(self->data_ptr);
	PyObject_Del(self);
}


static PyObject *
Pvsamv1_execute(Pvsamv1Object *self, PyObject *args)
{
	int verbosity = 0;

	if (!PyArg_ParseTuple(args, "|i", &verbosity))
		return NULL;

	SAM_error error = new_error();
	SAM_Pvsamv1_execute(self->data_ptr, verbosity, &error);
	if (PySAM_has_error(error )) return NULL;

	Py_INCREF(Py_None);
	return Py_None;
}


static PyObject *
Pvsamv1_assign(Pvsamv1Object *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_nested_dict((PyObject*)self, self->x_attr, self->data_ptr, dict, "Pvsamv1"))
		return NULL;

	Py_INCREF(Py_None);
	return Py_None;
}


static PyObject *
Pvsamv1_export(Pvsamv1Object *self, PyObject *args)
{
	return PySAM_export_to_nested_dict((PyObject *) self, self->x_attr);
}

static PyMethodDef Pvsamv1_methods[] = {
		{"execute",            (PyCFunction)Pvsamv1_execute,  METH_VARARGS,
				PyDoc_STR("execute(int verbosity) -> None\n Execute simulation with verbosity level 0 (default) or 1")},
		{"assign",            (PyCFunction)Pvsamv1_assign,  METH_VARARGS,
				PyDoc_STR("assign(dict) -> None\n Assign attributes from nested dictionary, except for Outputs\n\n``nested_dict = { 'Solar Resource': { var: val, ...}, ...}``")},
		{"export",            (PyCFunction)Pvsamv1_export,  METH_VARARGS,
				PyDoc_STR("export() -> dict\n Export attributes into nested dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
Pvsamv1_getattro(Pvsamv1Object *self, PyObject *name)
{
	return PySAM_get_attr((PyObject*) self, (PyObject*) self->x_attr, name);
}

static int
Pvsamv1_setattr(Pvsamv1Object *self, const char *name, PyObject *v)
{
	return PySAM_set_attr((PyObject*)self, (PyObject*)self->x_attr, name, v);
}

static PyTypeObject Pvsamv1_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Pvsamv1",            /*tp_name*/
		sizeof(Pvsamv1Object),/*tp_basicsize*/
		0,                          /*tp_itemsize*/
		/* methods */
		(destructor)Pvsamv1_dealloc,    /*tp_dealloc*/
		0,                          /*tp_print*/
		(getattrfunc)0,             /*tp_getattr*/
		(setattrfunc)Pvsamv1_setattr,   /*tp_setattr*/
		0,                          /*tp_reserved*/
		0,                          /*tp_repr*/
		0,                          /*tp_as_number*/
		0,                          /*tp_as_sequence*/
		0,                          /*tp_as_mapping*/
		0,                          /*tp_hash*/
		0,                          /*tp_call*/
		0,                          /*tp_str*/
		(getattrofunc)Pvsamv1_getattro, /*tp_getattro*/
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
		Pvsamv1_methods,      /*tp_methods*/
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


/* Function of no arguments returning new Pvsamv1 object */

static PyObject *
Pvsamv1_new(PyObject *self, PyObject *args)
{
	Pvsamv1Object *rv;
	rv = newPvsamv1Object(0);
	if (rv == NULL)
		return NULL;
	return (PyObject *)rv;
}

static PyObject *
Pvsamv1_wrap(PyObject *self, PyObject *args)
{
	Pvsamv1Object *rv;
	long long int ptr = 0;  // 64 bit arch
	if (!PyArg_ParseTuple(args, "L:wrap", &ptr)){
		PyErr_BadArgument();
		return NULL;
	}
	rv = newPvsamv1Object((void*)ptr);
	if (rv == NULL)
		return NULL;
	return (PyObject *)rv;
}

static PyObject *
Pvsamv1_default(PyObject *self, PyObject *args)
{
	Pvsamv1Object *rv;
	char* def = 0;
	if (!PyArg_ParseTuple(args, "s:default", &def)){
		PyErr_BadArgument();
		return NULL;
	}
	rv = newPvsamv1Object(0);
	if (rv == NULL)
		return NULL;

	PySAM_load_defaults((PyObject*)rv, rv->x_attr, rv->data_ptr, "Pvsamv1", def);

	return (PyObject *)rv;
}

/* ---------- */


/* List of functions defined in the module */

static PyMethodDef Pvsamv1Module_methods[] = {
		{"new",             Pvsamv1_new,         METH_VARARGS,
				PyDoc_STR("new() -> Pvsamv1")},
		{"default",             Pvsamv1_default,         METH_VARARGS,
				PyDoc_STR("default(config) -> Pvsamv1\n\nUse financial model-specific default attributes\n"
				"config options:\n\n- \"FlatPlatePVAllEquityPartnershipFlip\"\n- \"FlatPlatePVCommercial\"\n- \"FlatPlatePVCommercialPPA\"\n- \"FlatPlatePVHostDeveloper\"\n- \"FlatPlatePVIndependentPowerProducer\"\n- \"FlatPlatePVLCOECalculator\"\n- \"FlatPlatePVLeveragedPartnershipFlip\"\n- \"FlatPlatePVNone\"\n- \"FlatPlatePVResidential\"\n- \"FlatPlatePVSaleLeaseback\"\n- \"FlatPlatePVSingleOwner\"\n- \"FlatPlatePVThirdParty\"")},
		{"wrap",             Pvsamv1_wrap,         METH_VARARGS,
				PyDoc_STR("wrap(ssc_data_t) -> Pvsamv1\n\nUse existing PySSC data\n\n.. warning::\n\n	Do not call PySSC.data_free on the ssc_data_t provided to ``wrap``")},
		{NULL,              NULL}           /* sentinel */
};

PyDoc_STRVAR(module_doc,
			 "Detailed photovoltaic system model with separate components for module and inverter");


static int
Pvsamv1Module_exec(PyObject *m)
{
	/* Finalize the type object including setting type of the new type
	 * object; doing it here is required for portability, too. */

	if (PySAM_load_lib(m) < 0) goto fail;
	if (PySAM_init_error(m) < 0) goto fail;

	Pvsamv1_Type.tp_dict = PyDict_New();
	if (!Pvsamv1_Type.tp_dict) { goto fail; }

	/// Add the AdjustmentFactors type object to Pvsamv1_Type
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
	PyDict_SetItemString(Pvsamv1_Type.tp_dict,
						 "AdjustmentFactors",
						 (PyObject*)AdjustmentFactors_Type);
	Py_DECREF(&AdjustmentFactors_Type);
	Py_XDECREF(AdjustmentFactors_Type);

	/// Add the SolarResource type object to Pvsamv1_Type
	if (PyType_Ready(&SolarResource_Type) < 0) { goto fail; }
	PyDict_SetItemString(Pvsamv1_Type.tp_dict,
				"SolarResource",
				(PyObject*)&SolarResource_Type);
	Py_DECREF(&SolarResource_Type);

	/// Add the Losses type object to Pvsamv1_Type
	if (PyType_Ready(&Losses_Type) < 0) { goto fail; }
	PyDict_SetItemString(Pvsamv1_Type.tp_dict,
				"Losses",
				(PyObject*)&Losses_Type);
	Py_DECREF(&Losses_Type);

	/// Add the Lifetime type object to Pvsamv1_Type
	if (PyType_Ready(&Lifetime_Type) < 0) { goto fail; }
	PyDict_SetItemString(Pvsamv1_Type.tp_dict,
				"Lifetime",
				(PyObject*)&Lifetime_Type);
	Py_DECREF(&Lifetime_Type);

	/// Add the SystemDesign type object to Pvsamv1_Type
	if (PyType_Ready(&SystemDesign_Type) < 0) { goto fail; }
	PyDict_SetItemString(Pvsamv1_Type.tp_dict,
				"SystemDesign",
				(PyObject*)&SystemDesign_Type);
	Py_DECREF(&SystemDesign_Type);

	/// Add the Shading type object to Pvsamv1_Type
	if (PyType_Ready(&Shading_Type) < 0) { goto fail; }
	PyDict_SetItemString(Pvsamv1_Type.tp_dict,
				"Shading",
				(PyObject*)&Shading_Type);
	Py_DECREF(&Shading_Type);

	/// Add the Layout type object to Pvsamv1_Type
	if (PyType_Ready(&Layout_Type) < 0) { goto fail; }
	PyDict_SetItemString(Pvsamv1_Type.tp_dict,
				"Layout",
				(PyObject*)&Layout_Type);
	Py_DECREF(&Layout_Type);

	/// Add the Module type object to Pvsamv1_Type
	if (PyType_Ready(&Module_Type) < 0) { goto fail; }
	PyDict_SetItemString(Pvsamv1_Type.tp_dict,
				"Module",
				(PyObject*)&Module_Type);
	Py_DECREF(&Module_Type);

	/// Add the SimpleEfficiencyModuleModel type object to Pvsamv1_Type
	if (PyType_Ready(&SimpleEfficiencyModuleModel_Type) < 0) { goto fail; }
	PyDict_SetItemString(Pvsamv1_Type.tp_dict,
				"SimpleEfficiencyModuleModel",
				(PyObject*)&SimpleEfficiencyModuleModel_Type);
	Py_DECREF(&SimpleEfficiencyModuleModel_Type);

	/// Add the CECPerformanceModelWithModuleDatabase type object to Pvsamv1_Type
	if (PyType_Ready(&CECPerformanceModelWithModuleDatabase_Type) < 0) { goto fail; }
	PyDict_SetItemString(Pvsamv1_Type.tp_dict,
				"CECPerformanceModelWithModuleDatabase",
				(PyObject*)&CECPerformanceModelWithModuleDatabase_Type);
	Py_DECREF(&CECPerformanceModelWithModuleDatabase_Type);

	/// Add the CECPerformanceModelWithUserEnteredSpecifications type object to Pvsamv1_Type
	if (PyType_Ready(&CECPerformanceModelWithUserEnteredSpecifications_Type) < 0) { goto fail; }
	PyDict_SetItemString(Pvsamv1_Type.tp_dict,
				"CECPerformanceModelWithUserEnteredSpecifications",
				(PyObject*)&CECPerformanceModelWithUserEnteredSpecifications_Type);
	Py_DECREF(&CECPerformanceModelWithUserEnteredSpecifications_Type);

	/// Add the SandiaPVArrayPerformanceModelWithModuleDatabase type object to Pvsamv1_Type
	if (PyType_Ready(&SandiaPVArrayPerformanceModelWithModuleDatabase_Type) < 0) { goto fail; }
	PyDict_SetItemString(Pvsamv1_Type.tp_dict,
				"SandiaPVArrayPerformanceModelWithModuleDatabase",
				(PyObject*)&SandiaPVArrayPerformanceModelWithModuleDatabase_Type);
	Py_DECREF(&SandiaPVArrayPerformanceModelWithModuleDatabase_Type);

	/// Add the IEC61853SingleDiodeModel type object to Pvsamv1_Type
	if (PyType_Ready(&IEC61853SingleDiodeModel_Type) < 0) { goto fail; }
	PyDict_SetItemString(Pvsamv1_Type.tp_dict,
				"IEC61853SingleDiodeModel",
				(PyObject*)&IEC61853SingleDiodeModel_Type);
	Py_DECREF(&IEC61853SingleDiodeModel_Type);

	/// Add the MermoudLejeuneSingleDiodeModel type object to Pvsamv1_Type
	if (PyType_Ready(&MermoudLejeuneSingleDiodeModel_Type) < 0) { goto fail; }
	PyDict_SetItemString(Pvsamv1_Type.tp_dict,
				"MermoudLejeuneSingleDiodeModel",
				(PyObject*)&MermoudLejeuneSingleDiodeModel_Type);
	Py_DECREF(&MermoudLejeuneSingleDiodeModel_Type);

	/// Add the Inverter type object to Pvsamv1_Type
	if (PyType_Ready(&Inverter_Type) < 0) { goto fail; }
	PyDict_SetItemString(Pvsamv1_Type.tp_dict,
				"Inverter",
				(PyObject*)&Inverter_Type);
	Py_DECREF(&Inverter_Type);

	/// Add the InverterCECDatabase type object to Pvsamv1_Type
	if (PyType_Ready(&InverterCECDatabase_Type) < 0) { goto fail; }
	PyDict_SetItemString(Pvsamv1_Type.tp_dict,
				"InverterCECDatabase",
				(PyObject*)&InverterCECDatabase_Type);
	Py_DECREF(&InverterCECDatabase_Type);

	/// Add the InverterCECCoefficientGenerator type object to Pvsamv1_Type
	if (PyType_Ready(&InverterCECCoefficientGenerator_Type) < 0) { goto fail; }
	PyDict_SetItemString(Pvsamv1_Type.tp_dict,
				"InverterCECCoefficientGenerator",
				(PyObject*)&InverterCECCoefficientGenerator_Type);
	Py_DECREF(&InverterCECCoefficientGenerator_Type);

	/// Add the InverterDatasheet type object to Pvsamv1_Type
	if (PyType_Ready(&InverterDatasheet_Type) < 0) { goto fail; }
	PyDict_SetItemString(Pvsamv1_Type.tp_dict,
				"InverterDatasheet",
				(PyObject*)&InverterDatasheet_Type);
	Py_DECREF(&InverterDatasheet_Type);

	/// Add the InverterPartLoadCurve type object to Pvsamv1_Type
	if (PyType_Ready(&InverterPartLoadCurve_Type) < 0) { goto fail; }
	PyDict_SetItemString(Pvsamv1_Type.tp_dict,
				"InverterPartLoadCurve",
				(PyObject*)&InverterPartLoadCurve_Type);
	Py_DECREF(&InverterPartLoadCurve_Type);

	/// Add the InverterMermoudLejeuneModel type object to Pvsamv1_Type
	if (PyType_Ready(&InverterMermoudLejeuneModel_Type) < 0) { goto fail; }
	PyDict_SetItemString(Pvsamv1_Type.tp_dict,
				"InverterMermoudLejeuneModel",
				(PyObject*)&InverterMermoudLejeuneModel_Type);
	Py_DECREF(&InverterMermoudLejeuneModel_Type);

	/// Add the Battery type object to Pvsamv1_Type
	if (PyType_Ready(&Battery_Type) < 0) { goto fail; }
	PyDict_SetItemString(Pvsamv1_Type.tp_dict,
				"Battery",
				(PyObject*)&Battery_Type);
	Py_DECREF(&Battery_Type);

	/// Add the Simulation type object to Pvsamv1_Type
	if (PyType_Ready(&Simulation_Type) < 0) { goto fail; }
	PyDict_SetItemString(Pvsamv1_Type.tp_dict,
				"Simulation",
				(PyObject*)&Simulation_Type);
	Py_DECREF(&Simulation_Type);

	/// Add the Common type object to Pvsamv1_Type
	if (PyType_Ready(&Common_Type) < 0) { goto fail; }
	PyDict_SetItemString(Pvsamv1_Type.tp_dict,
				"Common",
				(PyObject*)&Common_Type);
	Py_DECREF(&Common_Type);

	/// Add the PV type object to Pvsamv1_Type
	if (PyType_Ready(&PV_Type) < 0) { goto fail; }
	PyDict_SetItemString(Pvsamv1_Type.tp_dict,
				"PV",
				(PyObject*)&PV_Type);
	Py_DECREF(&PV_Type);

	/// Add the FuelCell type object to Pvsamv1_Type
	if (PyType_Ready(&FuelCell_Type) < 0) { goto fail; }
	PyDict_SetItemString(Pvsamv1_Type.tp_dict,
				"FuelCell",
				(PyObject*)&FuelCell_Type);
	Py_DECREF(&FuelCell_Type);

	/// Add the ElectricityRate type object to Pvsamv1_Type
	if (PyType_Ready(&ElectricityRate_Type) < 0) { goto fail; }
	PyDict_SetItemString(Pvsamv1_Type.tp_dict,
				"ElectricityRate",
				(PyObject*)&ElectricityRate_Type);
	Py_DECREF(&ElectricityRate_Type);

	/// Add the EnergyMarket type object to Pvsamv1_Type
	if (PyType_Ready(&EnergyMarket_Type) < 0) { goto fail; }
	PyDict_SetItemString(Pvsamv1_Type.tp_dict,
				"EnergyMarket",
				(PyObject*)&EnergyMarket_Type);
	Py_DECREF(&EnergyMarket_Type);

	/// Add the Outputs type object to Pvsamv1_Type
	if (PyType_Ready(&Outputs_Type) < 0) { goto fail; }
	PyDict_SetItemString(Pvsamv1_Type.tp_dict,
				"Outputs",
				(PyObject*)&Outputs_Type);
	Py_DECREF(&Outputs_Type);

	/// Add the Pvsamv1 type object to the module
	if (PyType_Ready(&Pvsamv1_Type) < 0) { goto fail; }
	PyModule_AddObject(m,
				"Pvsamv1",
				(PyObject*)&Pvsamv1_Type);

	return 0;
	fail:
	Py_XDECREF(m);
	return -1;
}

static struct PyModuleDef_Slot Pvsamv1Module_slots[] = {
		{Py_mod_exec, Pvsamv1Module_exec},
		{0, NULL},
};

static struct PyModuleDef Pvsamv1Module = {
		PyModuleDef_HEAD_INIT,
		"Pvsamv1",
		module_doc,
		0,
		Pvsamv1Module_methods,
		Pvsamv1Module_slots,
		NULL,
		NULL,
		NULL
};

/* Export function for the module */

PyMODINIT_FUNC
PyInit_Pvsamv1(void)
{
	return PyModuleDef_Init(&Pvsamv1Module);
}