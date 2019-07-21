#include <Python.h>

#include <SAM_TroughPhysicalProcessHeat.h>
#include <SAM_api.h>

#include "PySAM_utils.h"



	/*
	 * Weather Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_TroughPhysicalProcessHeat   data_ptr;
} WeatherObject;

static PyTypeObject Weather_Type;

static PyObject *
Weather_new(SAM_TroughPhysicalProcessHeat data_ptr)
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

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "TroughPhysicalProcessHeat", "Weather")){
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
Weather_get_azimuth(WeatherObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_Weather_azimuth_nget, self->data_ptr);
}

static int
Weather_set_azimuth(WeatherObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TroughPhysicalProcessHeat_Weather_azimuth_nset, self->data_ptr);
}

static PyObject *
Weather_get_file_name(WeatherObject *self, void *closure)
{
	return PySAM_string_getter(SAM_TroughPhysicalProcessHeat_Weather_file_name_sget, self->data_ptr);
}

static int
Weather_set_file_name(WeatherObject *self, PyObject *value, void *closure)
{
	return PySAM_string_setter(value, SAM_TroughPhysicalProcessHeat_Weather_file_name_sset, self->data_ptr);
}

static PyObject *
Weather_get_tilt(WeatherObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_Weather_tilt_nget, self->data_ptr);
}

static int
Weather_set_tilt(WeatherObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TroughPhysicalProcessHeat_Weather_tilt_nset, self->data_ptr);
}

static PyObject *
Weather_get_track_mode(WeatherObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_Weather_track_mode_nget, self->data_ptr);
}

static int
Weather_set_track_mode(WeatherObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TroughPhysicalProcessHeat_Weather_track_mode_nset, self->data_ptr);
}

static PyGetSetDef Weather_getset[] = {
{"azimuth", (getter)Weather_get_azimuth,(setter)Weather_set_azimuth,
	PyDoc_STR("*float*: Azimuth angle of surface/axis [none]\n\n*Required*: True"),
 	NULL},
{"file_name", (getter)Weather_get_file_name,(setter)Weather_set_file_name,
	PyDoc_STR("*str*: Local weather file with path [none]\n\n*Constraints*: LOCAL_FILE\n\n*Required*: True"),
 	NULL},
{"tilt", (getter)Weather_get_tilt,(setter)Weather_set_tilt,
	PyDoc_STR("*float*: Tilt angle of surface/axis [none]\n\n*Required*: True"),
 	NULL},
{"track_mode", (getter)Weather_get_track_mode,(setter)Weather_set_track_mode,
	PyDoc_STR("*float*: Tracking mode [none]\n\n*Required*: True"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject Weather_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"TroughPhysicalProcessHeat.Weather",             /*tp_name*/
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
	 * SolarField Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_TroughPhysicalProcessHeat   data_ptr;
} SolarFieldObject;

static PyTypeObject SolarField_Type;

static PyObject *
SolarField_new(SAM_TroughPhysicalProcessHeat data_ptr)
{
	PyObject* new_obj = SolarField_Type.tp_alloc(&SolarField_Type,0);

	SolarFieldObject* SolarField_obj = (SolarFieldObject*)new_obj;

	SolarField_obj->data_ptr = data_ptr;

	return new_obj;
}

/* SolarField methods */

static PyObject *
SolarField_assign(SolarFieldObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "TroughPhysicalProcessHeat", "SolarField")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
SolarField_export(SolarFieldObject *self, PyObject *args)
{
	PyTypeObject* tp = &SolarField_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef SolarField_methods[] = {
		{"assign",            (PyCFunction)SolarField_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``SolarField_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)SolarField_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
SolarField_get_A_aperture(SolarFieldObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_SolarField_A_aperture_aget, self->data_ptr);
}

static int
SolarField_set_A_aperture(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_A_aperture_aset, self->data_ptr);
}

static PyObject *
SolarField_get_AbsorberMaterial(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TroughPhysicalProcessHeat_SolarField_AbsorberMaterial_mget, self->data_ptr);
}

static int
SolarField_set_AbsorberMaterial(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_AbsorberMaterial_mset, self->data_ptr);
}

static PyObject *
SolarField_get_AnnulusGas(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TroughPhysicalProcessHeat_SolarField_AnnulusGas_mget, self->data_ptr);
}

static int
SolarField_set_AnnulusGas(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_AnnulusGas_mset, self->data_ptr);
}

static PyObject *
SolarField_get_Ave_Focal_Length(SolarFieldObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_SolarField_Ave_Focal_Length_aget, self->data_ptr);
}

static int
SolarField_set_Ave_Focal_Length(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_Ave_Focal_Length_aset, self->data_ptr);
}

static PyObject *
SolarField_get_ColperSCA(SolarFieldObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_SolarField_ColperSCA_aget, self->data_ptr);
}

static int
SolarField_set_ColperSCA(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_ColperSCA_aset, self->data_ptr);
}

static PyObject *
SolarField_get_D_2(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TroughPhysicalProcessHeat_SolarField_D_2_mget, self->data_ptr);
}

static int
SolarField_set_D_2(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_D_2_mset, self->data_ptr);
}

static PyObject *
SolarField_get_D_3(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TroughPhysicalProcessHeat_SolarField_D_3_mget, self->data_ptr);
}

static int
SolarField_set_D_3(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_D_3_mset, self->data_ptr);
}

static PyObject *
SolarField_get_D_4(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TroughPhysicalProcessHeat_SolarField_D_4_mget, self->data_ptr);
}

static int
SolarField_set_D_4(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_D_4_mset, self->data_ptr);
}

static PyObject *
SolarField_get_D_5(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TroughPhysicalProcessHeat_SolarField_D_5_mget, self->data_ptr);
}

static int
SolarField_set_D_5(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_D_5_mset, self->data_ptr);
}

static PyObject *
SolarField_get_D_cpnt(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TroughPhysicalProcessHeat_SolarField_D_cpnt_mget, self->data_ptr);
}

static int
SolarField_set_D_cpnt(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_D_cpnt_mset, self->data_ptr);
}

static PyObject *
SolarField_get_D_p(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TroughPhysicalProcessHeat_SolarField_D_p_mget, self->data_ptr);
}

static int
SolarField_set_D_p(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_D_p_mset, self->data_ptr);
}

static PyObject *
SolarField_get_Design_loss(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TroughPhysicalProcessHeat_SolarField_Design_loss_mget, self->data_ptr);
}

static int
SolarField_set_Design_loss(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_Design_loss_mset, self->data_ptr);
}

static PyObject *
SolarField_get_Dirt_HCE(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TroughPhysicalProcessHeat_SolarField_Dirt_HCE_mget, self->data_ptr);
}

static int
SolarField_set_Dirt_HCE(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_Dirt_HCE_mset, self->data_ptr);
}

static PyObject *
SolarField_get_Dirt_mirror(SolarFieldObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_SolarField_Dirt_mirror_aget, self->data_ptr);
}

static int
SolarField_set_Dirt_mirror(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_Dirt_mirror_aset, self->data_ptr);
}

static PyObject *
SolarField_get_Distance_SCA(SolarFieldObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_SolarField_Distance_SCA_aget, self->data_ptr);
}

static int
SolarField_set_Distance_SCA(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_Distance_SCA_aset, self->data_ptr);
}

static PyObject *
SolarField_get_EPSILON_4(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TroughPhysicalProcessHeat_SolarField_EPSILON_4_mget, self->data_ptr);
}

static int
SolarField_set_EPSILON_4(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_EPSILON_4_mset, self->data_ptr);
}

static PyObject *
SolarField_get_EPSILON_5(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TroughPhysicalProcessHeat_SolarField_EPSILON_5_mget, self->data_ptr);
}

static int
SolarField_set_EPSILON_5(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_EPSILON_5_mset, self->data_ptr);
}

static PyObject *
SolarField_get_Error(SolarFieldObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_SolarField_Error_aget, self->data_ptr);
}

static int
SolarField_set_Error(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_Error_aset, self->data_ptr);
}

static PyObject *
SolarField_get_FieldConfig(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_SolarField_FieldConfig_nget, self->data_ptr);
}

static int
SolarField_set_FieldConfig(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_FieldConfig_nset, self->data_ptr);
}

static PyObject *
SolarField_get_Flow_type(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TroughPhysicalProcessHeat_SolarField_Flow_type_mget, self->data_ptr);
}

static int
SolarField_set_Flow_type(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_Flow_type_mset, self->data_ptr);
}

static PyObject *
SolarField_get_Fluid(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_SolarField_Fluid_nget, self->data_ptr);
}

static int
SolarField_set_Fluid(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_Fluid_nset, self->data_ptr);
}

static PyObject *
SolarField_get_GeomEffects(SolarFieldObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_SolarField_GeomEffects_aget, self->data_ptr);
}

static int
SolarField_set_GeomEffects(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_GeomEffects_aset, self->data_ptr);
}

static PyObject *
SolarField_get_GlazingIntactIn(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TroughPhysicalProcessHeat_SolarField_GlazingIntactIn_mget, self->data_ptr);
}

static int
SolarField_set_GlazingIntactIn(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_GlazingIntactIn_mset, self->data_ptr);
}

static PyObject *
SolarField_get_HCE_FieldFrac(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TroughPhysicalProcessHeat_SolarField_HCE_FieldFrac_mget, self->data_ptr);
}

static int
SolarField_set_HCE_FieldFrac(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_HCE_FieldFrac_mset, self->data_ptr);
}

static PyObject *
SolarField_get_HDR_rough(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_SolarField_HDR_rough_nget, self->data_ptr);
}

static int
SolarField_set_HDR_rough(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_HDR_rough_nset, self->data_ptr);
}

static PyObject *
SolarField_get_IAM_matrix(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TroughPhysicalProcessHeat_SolarField_IAM_matrix_mget, self->data_ptr);
}

static int
SolarField_set_IAM_matrix(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_IAM_matrix_mset, self->data_ptr);
}

static PyObject *
SolarField_get_I_bn_des(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_SolarField_I_bn_des_nget, self->data_ptr);
}

static int
SolarField_set_I_bn_des(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_I_bn_des_nset, self->data_ptr);
}

static PyObject *
SolarField_get_K_cpnt(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TroughPhysicalProcessHeat_SolarField_K_cpnt_mget, self->data_ptr);
}

static int
SolarField_set_K_cpnt(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_K_cpnt_mset, self->data_ptr);
}

static PyObject *
SolarField_get_L_SCA(SolarFieldObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_SolarField_L_SCA_aget, self->data_ptr);
}

static int
SolarField_set_L_SCA(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_L_SCA_aset, self->data_ptr);
}

static PyObject *
SolarField_get_L_aperture(SolarFieldObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_SolarField_L_aperture_aget, self->data_ptr);
}

static int
SolarField_set_L_aperture(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_L_aperture_aset, self->data_ptr);
}

static PyObject *
SolarField_get_L_cpnt(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TroughPhysicalProcessHeat_SolarField_L_cpnt_mget, self->data_ptr);
}

static int
SolarField_set_L_cpnt(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_L_cpnt_mset, self->data_ptr);
}

static PyObject *
SolarField_get_L_heat_sink_piping(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_SolarField_L_heat_sink_piping_nget, self->data_ptr);
}

static int
SolarField_set_L_heat_sink_piping(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_L_heat_sink_piping_nset, self->data_ptr);
}

static PyObject *
SolarField_get_L_rnr_per_xpan(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_SolarField_L_rnr_per_xpan_nget, self->data_ptr);
}

static int
SolarField_set_L_rnr_per_xpan(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_L_rnr_per_xpan_nset, self->data_ptr);
}

static PyObject *
SolarField_get_L_xpan_hdr(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_SolarField_L_xpan_hdr_nget, self->data_ptr);
}

static int
SolarField_set_L_xpan_hdr(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_L_xpan_hdr_nset, self->data_ptr);
}

static PyObject *
SolarField_get_L_xpan_rnr(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_SolarField_L_xpan_rnr_nget, self->data_ptr);
}

static int
SolarField_set_L_xpan_rnr(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_L_xpan_rnr_nset, self->data_ptr);
}

static PyObject *
SolarField_get_Min_rnr_xpans(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_SolarField_Min_rnr_xpans_nget, self->data_ptr);
}

static int
SolarField_set_Min_rnr_xpans(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_Min_rnr_xpans_nset, self->data_ptr);
}

static PyObject *
SolarField_get_N_hdr_per_xpan(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_SolarField_N_hdr_per_xpan_nget, self->data_ptr);
}

static int
SolarField_set_N_hdr_per_xpan(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_N_hdr_per_xpan_nset, self->data_ptr);
}

static PyObject *
SolarField_get_N_max_hdr_diams(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_SolarField_N_max_hdr_diams_nget, self->data_ptr);
}

static int
SolarField_set_N_max_hdr_diams(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_N_max_hdr_diams_nset, self->data_ptr);
}

static PyObject *
SolarField_get_P_a(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TroughPhysicalProcessHeat_SolarField_P_a_mget, self->data_ptr);
}

static int
SolarField_set_P_a(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_P_a_mset, self->data_ptr);
}

static PyObject *
SolarField_get_Pipe_hl_coef(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_SolarField_Pipe_hl_coef_nget, self->data_ptr);
}

static int
SolarField_set_Pipe_hl_coef(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_Pipe_hl_coef_nset, self->data_ptr);
}

static PyObject *
SolarField_get_Rho_mirror_clean(SolarFieldObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_SolarField_Rho_mirror_clean_aget, self->data_ptr);
}

static int
SolarField_set_Rho_mirror_clean(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_Rho_mirror_clean_aset, self->data_ptr);
}

static PyObject *
SolarField_get_Rough(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TroughPhysicalProcessHeat_SolarField_Rough_mget, self->data_ptr);
}

static int
SolarField_set_Rough(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_Rough_mset, self->data_ptr);
}

static PyObject *
SolarField_get_Row_Distance(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_SolarField_Row_Distance_nget, self->data_ptr);
}

static int
SolarField_set_Row_Distance(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_Row_Distance_nset, self->data_ptr);
}

static PyObject *
SolarField_get_SCADefocusArray(SolarFieldObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_SolarField_SCADefocusArray_aget, self->data_ptr);
}

static int
SolarField_set_SCADefocusArray(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_SCADefocusArray_aset, self->data_ptr);
}

static PyObject *
SolarField_get_SCAInfoArray(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TroughPhysicalProcessHeat_SolarField_SCAInfoArray_mget, self->data_ptr);
}

static int
SolarField_set_SCAInfoArray(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_SCAInfoArray_mset, self->data_ptr);
}

static PyObject *
SolarField_get_SCA_drives_elec(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_SolarField_SCA_drives_elec_nget, self->data_ptr);
}

static int
SolarField_set_SCA_drives_elec(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_SCA_drives_elec_nset, self->data_ptr);
}

static PyObject *
SolarField_get_Shadowing(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TroughPhysicalProcessHeat_SolarField_Shadowing_mget, self->data_ptr);
}

static int
SolarField_set_Shadowing(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_Shadowing_mset, self->data_ptr);
}

static PyObject *
SolarField_get_T_fp(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_SolarField_T_fp_nget, self->data_ptr);
}

static int
SolarField_set_T_fp(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_T_fp_nset, self->data_ptr);
}

static PyObject *
SolarField_get_T_loop_in_des(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_SolarField_T_loop_in_des_nget, self->data_ptr);
}

static int
SolarField_set_T_loop_in_des(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_T_loop_in_des_nset, self->data_ptr);
}

static PyObject *
SolarField_get_T_loop_out(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_SolarField_T_loop_out_nget, self->data_ptr);
}

static int
SolarField_set_T_loop_out(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_T_loop_out_nset, self->data_ptr);
}

static PyObject *
SolarField_get_Tau_envelope(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TroughPhysicalProcessHeat_SolarField_Tau_envelope_mget, self->data_ptr);
}

static int
SolarField_set_Tau_envelope(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_Tau_envelope_mset, self->data_ptr);
}

static PyObject *
SolarField_get_TrackingError(SolarFieldObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_SolarField_TrackingError_aget, self->data_ptr);
}

static int
SolarField_set_TrackingError(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_TrackingError_aset, self->data_ptr);
}

static PyObject *
SolarField_get_Type_cpnt(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TroughPhysicalProcessHeat_SolarField_Type_cpnt_mget, self->data_ptr);
}

static int
SolarField_set_Type_cpnt(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_Type_cpnt_mset, self->data_ptr);
}

static PyObject *
SolarField_get_V_hdr_max(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_SolarField_V_hdr_max_nget, self->data_ptr);
}

static int
SolarField_set_V_hdr_max(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_V_hdr_max_nset, self->data_ptr);
}

static PyObject *
SolarField_get_V_hdr_min(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_SolarField_V_hdr_min_nget, self->data_ptr);
}

static int
SolarField_set_V_hdr_min(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_V_hdr_min_nset, self->data_ptr);
}

static PyObject *
SolarField_get_W_aperture(SolarFieldObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_SolarField_W_aperture_aget, self->data_ptr);
}

static int
SolarField_set_W_aperture(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_W_aperture_aset, self->data_ptr);
}

static PyObject *
SolarField_get_accept_init(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_SolarField_accept_init_nget, self->data_ptr);
}

static int
SolarField_set_accept_init(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_accept_init_nset, self->data_ptr);
}

static PyObject *
SolarField_get_accept_loc(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_SolarField_accept_loc_nget, self->data_ptr);
}

static int
SolarField_set_accept_loc(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_accept_loc_nset, self->data_ptr);
}

static PyObject *
SolarField_get_accept_mode(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_SolarField_accept_mode_nget, self->data_ptr);
}

static int
SolarField_set_accept_mode(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_accept_mode_nset, self->data_ptr);
}

static PyObject *
SolarField_get_alpha_abs(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TroughPhysicalProcessHeat_SolarField_alpha_abs_mget, self->data_ptr);
}

static int
SolarField_set_alpha_abs(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_alpha_abs_mset, self->data_ptr);
}

static PyObject *
SolarField_get_alpha_env(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TroughPhysicalProcessHeat_SolarField_alpha_env_mget, self->data_ptr);
}

static int
SolarField_set_alpha_env(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_alpha_env_mset, self->data_ptr);
}

static PyObject *
SolarField_get_calc_design_pipe_vals(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_SolarField_calc_design_pipe_vals_nget, self->data_ptr);
}

static int
SolarField_set_calc_design_pipe_vals(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_calc_design_pipe_vals_nset, self->data_ptr);
}

static PyObject *
SolarField_get_custom_sf_pipe_sizes(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_SolarField_custom_sf_pipe_sizes_nget, self->data_ptr);
}

static int
SolarField_set_custom_sf_pipe_sizes(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_custom_sf_pipe_sizes_nset, self->data_ptr);
}

static PyObject *
SolarField_get_epsilon_3_11(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TroughPhysicalProcessHeat_SolarField_epsilon_3_11_mget, self->data_ptr);
}

static int
SolarField_set_epsilon_3_11(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_epsilon_3_11_mset, self->data_ptr);
}

static PyObject *
SolarField_get_epsilon_3_12(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TroughPhysicalProcessHeat_SolarField_epsilon_3_12_mget, self->data_ptr);
}

static int
SolarField_set_epsilon_3_12(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_epsilon_3_12_mset, self->data_ptr);
}

static PyObject *
SolarField_get_epsilon_3_13(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TroughPhysicalProcessHeat_SolarField_epsilon_3_13_mget, self->data_ptr);
}

static int
SolarField_set_epsilon_3_13(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_epsilon_3_13_mset, self->data_ptr);
}

static PyObject *
SolarField_get_epsilon_3_14(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TroughPhysicalProcessHeat_SolarField_epsilon_3_14_mget, self->data_ptr);
}

static int
SolarField_set_epsilon_3_14(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_epsilon_3_14_mset, self->data_ptr);
}

static PyObject *
SolarField_get_epsilon_3_21(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TroughPhysicalProcessHeat_SolarField_epsilon_3_21_mget, self->data_ptr);
}

static int
SolarField_set_epsilon_3_21(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_epsilon_3_21_mset, self->data_ptr);
}

static PyObject *
SolarField_get_epsilon_3_22(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TroughPhysicalProcessHeat_SolarField_epsilon_3_22_mget, self->data_ptr);
}

static int
SolarField_set_epsilon_3_22(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_epsilon_3_22_mset, self->data_ptr);
}

static PyObject *
SolarField_get_epsilon_3_23(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TroughPhysicalProcessHeat_SolarField_epsilon_3_23_mget, self->data_ptr);
}

static int
SolarField_set_epsilon_3_23(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_epsilon_3_23_mset, self->data_ptr);
}

static PyObject *
SolarField_get_epsilon_3_24(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TroughPhysicalProcessHeat_SolarField_epsilon_3_24_mget, self->data_ptr);
}

static int
SolarField_set_epsilon_3_24(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_epsilon_3_24_mset, self->data_ptr);
}

static PyObject *
SolarField_get_epsilon_3_31(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TroughPhysicalProcessHeat_SolarField_epsilon_3_31_mget, self->data_ptr);
}

static int
SolarField_set_epsilon_3_31(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_epsilon_3_31_mset, self->data_ptr);
}

static PyObject *
SolarField_get_epsilon_3_32(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TroughPhysicalProcessHeat_SolarField_epsilon_3_32_mget, self->data_ptr);
}

static int
SolarField_set_epsilon_3_32(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_epsilon_3_32_mset, self->data_ptr);
}

static PyObject *
SolarField_get_epsilon_3_33(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TroughPhysicalProcessHeat_SolarField_epsilon_3_33_mget, self->data_ptr);
}

static int
SolarField_set_epsilon_3_33(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_epsilon_3_33_mset, self->data_ptr);
}

static PyObject *
SolarField_get_epsilon_3_34(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TroughPhysicalProcessHeat_SolarField_epsilon_3_34_mget, self->data_ptr);
}

static int
SolarField_set_epsilon_3_34(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_epsilon_3_34_mset, self->data_ptr);
}

static PyObject *
SolarField_get_epsilon_3_41(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TroughPhysicalProcessHeat_SolarField_epsilon_3_41_mget, self->data_ptr);
}

static int
SolarField_set_epsilon_3_41(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_epsilon_3_41_mset, self->data_ptr);
}

static PyObject *
SolarField_get_epsilon_3_42(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TroughPhysicalProcessHeat_SolarField_epsilon_3_42_mget, self->data_ptr);
}

static int
SolarField_set_epsilon_3_42(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_epsilon_3_42_mset, self->data_ptr);
}

static PyObject *
SolarField_get_epsilon_3_43(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TroughPhysicalProcessHeat_SolarField_epsilon_3_43_mget, self->data_ptr);
}

static int
SolarField_set_epsilon_3_43(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_epsilon_3_43_mset, self->data_ptr);
}

static PyObject *
SolarField_get_epsilon_3_44(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TroughPhysicalProcessHeat_SolarField_epsilon_3_44_mget, self->data_ptr);
}

static int
SolarField_set_epsilon_3_44(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_epsilon_3_44_mset, self->data_ptr);
}

static PyObject *
SolarField_get_eta_pump(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_SolarField_eta_pump_nget, self->data_ptr);
}

static int
SolarField_set_eta_pump(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_eta_pump_nset, self->data_ptr);
}

static PyObject *
SolarField_get_is_model_heat_sink_piping(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_SolarField_is_model_heat_sink_piping_nget, self->data_ptr);
}

static int
SolarField_set_is_model_heat_sink_piping(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_is_model_heat_sink_piping_nset, self->data_ptr);
}

static PyObject *
SolarField_get_m_dot_htfmax(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_SolarField_m_dot_htfmax_nget, self->data_ptr);
}

static int
SolarField_set_m_dot_htfmax(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_m_dot_htfmax_nset, self->data_ptr);
}

static PyObject *
SolarField_get_m_dot_htfmin(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_SolarField_m_dot_htfmin_nget, self->data_ptr);
}

static int
SolarField_set_m_dot_htfmin(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_m_dot_htfmin_nset, self->data_ptr);
}

static PyObject *
SolarField_get_mc_bal_cold(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_SolarField_mc_bal_cold_nget, self->data_ptr);
}

static int
SolarField_set_mc_bal_cold(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_mc_bal_cold_nset, self->data_ptr);
}

static PyObject *
SolarField_get_mc_bal_hot(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_SolarField_mc_bal_hot_nget, self->data_ptr);
}

static int
SolarField_set_mc_bal_hot(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_mc_bal_hot_nset, self->data_ptr);
}

static PyObject *
SolarField_get_mc_bal_sca(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_SolarField_mc_bal_sca_nget, self->data_ptr);
}

static int
SolarField_set_mc_bal_sca(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_mc_bal_sca_nset, self->data_ptr);
}

static PyObject *
SolarField_get_nColt(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_SolarField_nColt_nget, self->data_ptr);
}

static int
SolarField_set_nColt(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_nColt_nset, self->data_ptr);
}

static PyObject *
SolarField_get_nHCEVar(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_SolarField_nHCEVar_nget, self->data_ptr);
}

static int
SolarField_set_nHCEVar(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_nHCEVar_nset, self->data_ptr);
}

static PyObject *
SolarField_get_nHCEt(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_SolarField_nHCEt_nget, self->data_ptr);
}

static int
SolarField_set_nHCEt(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_nHCEt_nset, self->data_ptr);
}

static PyObject *
SolarField_get_nLoops(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_SolarField_nLoops_nget, self->data_ptr);
}

static int
SolarField_set_nLoops(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_nLoops_nset, self->data_ptr);
}

static PyObject *
SolarField_get_nSCA(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_SolarField_nSCA_nget, self->data_ptr);
}

static int
SolarField_set_nSCA(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_nSCA_nset, self->data_ptr);
}

static PyObject *
SolarField_get_northsouth_field_sep(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_SolarField_northsouth_field_sep_nget, self->data_ptr);
}

static int
SolarField_set_northsouth_field_sep(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_northsouth_field_sep_nset, self->data_ptr);
}

static PyObject *
SolarField_get_offset_xpan_hdr(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_SolarField_offset_xpan_hdr_nget, self->data_ptr);
}

static int
SolarField_set_offset_xpan_hdr(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_offset_xpan_hdr_nset, self->data_ptr);
}

static PyObject *
SolarField_get_sf_hdr_diams(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TroughPhysicalProcessHeat_SolarField_sf_hdr_diams_mget, self->data_ptr);
}

static int
SolarField_set_sf_hdr_diams(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_sf_hdr_diams_mset, self->data_ptr);
}

static PyObject *
SolarField_get_sf_hdr_lengths(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TroughPhysicalProcessHeat_SolarField_sf_hdr_lengths_mget, self->data_ptr);
}

static int
SolarField_set_sf_hdr_lengths(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_sf_hdr_lengths_mset, self->data_ptr);
}

static PyObject *
SolarField_get_sf_hdr_wallthicks(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TroughPhysicalProcessHeat_SolarField_sf_hdr_wallthicks_mget, self->data_ptr);
}

static int
SolarField_set_sf_hdr_wallthicks(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_sf_hdr_wallthicks_mset, self->data_ptr);
}

static PyObject *
SolarField_get_sf_rnr_diams(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TroughPhysicalProcessHeat_SolarField_sf_rnr_diams_mget, self->data_ptr);
}

static int
SolarField_set_sf_rnr_diams(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_sf_rnr_diams_mset, self->data_ptr);
}

static PyObject *
SolarField_get_sf_rnr_lengths(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TroughPhysicalProcessHeat_SolarField_sf_rnr_lengths_mget, self->data_ptr);
}

static int
SolarField_set_sf_rnr_lengths(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_sf_rnr_lengths_mset, self->data_ptr);
}

static PyObject *
SolarField_get_sf_rnr_wallthicks(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TroughPhysicalProcessHeat_SolarField_sf_rnr_wallthicks_mget, self->data_ptr);
}

static int
SolarField_set_sf_rnr_wallthicks(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_sf_rnr_wallthicks_mset, self->data_ptr);
}

static PyObject *
SolarField_get_solar_mult(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_SolarField_solar_mult_nget, self->data_ptr);
}

static int
SolarField_set_solar_mult(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_solar_mult_nset, self->data_ptr);
}

static PyObject *
SolarField_get_theta_dep(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_SolarField_theta_dep_nget, self->data_ptr);
}

static int
SolarField_set_theta_dep(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_theta_dep_nset, self->data_ptr);
}

static PyObject *
SolarField_get_theta_stow(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_SolarField_theta_stow_nget, self->data_ptr);
}

static int
SolarField_set_theta_stow(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_theta_stow_nset, self->data_ptr);
}

static PyObject *
SolarField_get_washing_frequency(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_SolarField_washing_frequency_nget, self->data_ptr);
}

static int
SolarField_set_washing_frequency(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_washing_frequency_nset, self->data_ptr);
}

static PyObject *
SolarField_get_water_usage_per_wash(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_SolarField_water_usage_per_wash_nget, self->data_ptr);
}

static int
SolarField_set_water_usage_per_wash(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_water_usage_per_wash_nset, self->data_ptr);
}

static PyObject *
SolarField_get_wind_stow_speed(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_SolarField_wind_stow_speed_nget, self->data_ptr);
}

static int
SolarField_set_wind_stow_speed(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TroughPhysicalProcessHeat_SolarField_wind_stow_speed_nset, self->data_ptr);
}

static PyGetSetDef SolarField_getset[] = {
{"A_aperture", (getter)SolarField_get_A_aperture,(setter)SolarField_set_A_aperture,
	PyDoc_STR("*sequence*: Reflective aperture area of the collector [m2]\n\n*Required*: True"),
 	NULL},
{"AbsorberMaterial", (getter)SolarField_get_AbsorberMaterial,(setter)SolarField_set_AbsorberMaterial,
	PyDoc_STR("*sequence[sequence]*: Absorber material type [none]\n\n*Required*: True"),
 	NULL},
{"AnnulusGas", (getter)SolarField_get_AnnulusGas,(setter)SolarField_set_AnnulusGas,
	PyDoc_STR("*sequence[sequence]*: Annulus gas type (1=air, 26=Ar, 27=H2) [none]\n\n*Required*: True"),
 	NULL},
{"Ave_Focal_Length", (getter)SolarField_get_Ave_Focal_Length,(setter)SolarField_set_Ave_Focal_Length,
	PyDoc_STR("*sequence*: Average focal length of the collector  [m]\n\n*Required*: True"),
 	NULL},
{"ColperSCA", (getter)SolarField_get_ColperSCA,(setter)SolarField_set_ColperSCA,
	PyDoc_STR("*sequence*: Number of individual collector sections in an SCA  [none]\n\n*Required*: True"),
 	NULL},
{"D_2", (getter)SolarField_get_D_2,(setter)SolarField_set_D_2,
	PyDoc_STR("*sequence[sequence]*: Inner absorber tube diameter [m]\n\n*Required*: True"),
 	NULL},
{"D_3", (getter)SolarField_get_D_3,(setter)SolarField_set_D_3,
	PyDoc_STR("*sequence[sequence]*: Outer absorber tube diameter [m]\n\n*Required*: True"),
 	NULL},
{"D_4", (getter)SolarField_get_D_4,(setter)SolarField_set_D_4,
	PyDoc_STR("*sequence[sequence]*: Inner glass envelope diameter  [m]\n\n*Required*: True"),
 	NULL},
{"D_5", (getter)SolarField_get_D_5,(setter)SolarField_set_D_5,
	PyDoc_STR("*sequence[sequence]*: Outer glass envelope diameter  [m]\n\n*Required*: True"),
 	NULL},
{"D_cpnt", (getter)SolarField_get_D_cpnt,(setter)SolarField_set_D_cpnt,
	PyDoc_STR("*sequence[sequence]*: Interconnect component diameters, row=intc, col=cpnt [none]\n\n*Required*: True"),
 	NULL},
{"D_p", (getter)SolarField_get_D_p,(setter)SolarField_set_D_p,
	PyDoc_STR("*sequence[sequence]*: Diameter of the absorber flow plug (optional)  [m]\n\n*Required*: True"),
 	NULL},
{"Design_loss", (getter)SolarField_get_Design_loss,(setter)SolarField_set_Design_loss,
	PyDoc_STR("*sequence[sequence]*: Receiver heat loss at design [W/m]\n\n*Required*: True"),
 	NULL},
{"Dirt_HCE", (getter)SolarField_get_Dirt_HCE,(setter)SolarField_set_Dirt_HCE,
	PyDoc_STR("*sequence[sequence]*: Loss due to dirt on the receiver envelope [none]\n\n*Required*: True"),
 	NULL},
{"Dirt_mirror", (getter)SolarField_get_Dirt_mirror,(setter)SolarField_set_Dirt_mirror,
	PyDoc_STR("*sequence*: User-defined dirt on mirror derate [none]\n\n*Required*: True"),
 	NULL},
{"Distance_SCA", (getter)SolarField_get_Distance_SCA,(setter)SolarField_set_Distance_SCA,
	PyDoc_STR("*sequence*: Piping distance between SCA's in the field [m]\n\n*Required*: True"),
 	NULL},
{"EPSILON_4", (getter)SolarField_get_EPSILON_4,(setter)SolarField_set_EPSILON_4,
	PyDoc_STR("*sequence[sequence]*: Inner glass envelope emissivities (Pyrex)  [none]\n\n*Required*: True"),
 	NULL},
{"EPSILON_5", (getter)SolarField_get_EPSILON_5,(setter)SolarField_set_EPSILON_5,
	PyDoc_STR("*sequence[sequence]*: Outer glass envelope emissivities (Pyrex)  [none]\n\n*Required*: True"),
 	NULL},
{"Error", (getter)SolarField_get_Error,(setter)SolarField_set_Error,
	PyDoc_STR("*sequence*: User-defined general optical error derate  [none]\n\n*Required*: True"),
 	NULL},
{"FieldConfig", (getter)SolarField_get_FieldConfig,(setter)SolarField_set_FieldConfig,
	PyDoc_STR("*float*: Number of subfield headers [none]\n\n*Required*: True"),
 	NULL},
{"Flow_type", (getter)SolarField_get_Flow_type,(setter)SolarField_set_Flow_type,
	PyDoc_STR("*sequence[sequence]*: Flow type through the absorber [none]\n\n*Required*: True"),
 	NULL},
{"Fluid", (getter)SolarField_get_Fluid,(setter)SolarField_set_Fluid,
	PyDoc_STR("*float*: Field HTF fluid ID number [none]\n\n*Required*: True"),
 	NULL},
{"GeomEffects", (getter)SolarField_get_GeomEffects,(setter)SolarField_set_GeomEffects,
	PyDoc_STR("*sequence*: User-defined geometry effects derate [none]\n\n*Required*: True"),
 	NULL},
{"GlazingIntactIn", (getter)SolarField_get_GlazingIntactIn,(setter)SolarField_set_GlazingIntactIn,
	PyDoc_STR("*sequence[sequence]*: Glazing intact (broken glass) flag {1=true, else=false} [none]\n\n*Required*: True"),
 	NULL},
{"HCE_FieldFrac", (getter)SolarField_get_HCE_FieldFrac,(setter)SolarField_set_HCE_FieldFrac,
	PyDoc_STR("*sequence[sequence]*: Fraction of the field occupied by this HCE type  [none]\n\n*Required*: True"),
 	NULL},
{"HDR_rough", (getter)SolarField_get_HDR_rough,(setter)SolarField_set_HDR_rough,
	PyDoc_STR("*float*: Header pipe roughness [m]\n\n*Required*: True"),
 	NULL},
{"IAM_matrix", (getter)SolarField_get_IAM_matrix,(setter)SolarField_set_IAM_matrix,
	PyDoc_STR("*sequence[sequence]*: IAM coefficients, matrix for 4 collectors [none]\n\n*Required*: True"),
 	NULL},
{"I_bn_des", (getter)SolarField_get_I_bn_des,(setter)SolarField_set_I_bn_des,
	PyDoc_STR("*float*: Solar irradiation at design [C]\n\n*Required*: True"),
 	NULL},
{"K_cpnt", (getter)SolarField_get_K_cpnt,(setter)SolarField_set_K_cpnt,
	PyDoc_STR("*sequence[sequence]*: Interconnect component minor loss coefficients, row=intc, col=cpnt [none]\n\n*Required*: True"),
 	NULL},
{"L_SCA", (getter)SolarField_get_L_SCA,(setter)SolarField_set_L_SCA,
	PyDoc_STR("*sequence*: Length of the SCA  [m]\n\n*Required*: True"),
 	NULL},
{"L_aperture", (getter)SolarField_get_L_aperture,(setter)SolarField_set_L_aperture,
	PyDoc_STR("*sequence*: Length of a single mirror/HCE unit [m]\n\n*Required*: True"),
 	NULL},
{"L_cpnt", (getter)SolarField_get_L_cpnt,(setter)SolarField_set_L_cpnt,
	PyDoc_STR("*sequence[sequence]*: Interconnect component lengths, row=intc, col=cpnt [none]\n\n*Required*: True"),
 	NULL},
{"L_heat_sink_piping", (getter)SolarField_get_L_heat_sink_piping,(setter)SolarField_set_L_heat_sink_piping,
	PyDoc_STR("*float*: Length of piping (full mass flow) through heat sink (if applicable) [none]\n\n*Required*: True"),
 	NULL},
{"L_rnr_per_xpan", (getter)SolarField_get_L_rnr_per_xpan,(setter)SolarField_set_L_rnr_per_xpan,
	PyDoc_STR("*float*: Threshold length of straight runner pipe without an expansion loop [m]\n\n*Required*: True"),
 	NULL},
{"L_xpan_hdr", (getter)SolarField_get_L_xpan_hdr,(setter)SolarField_set_L_xpan_hdr,
	PyDoc_STR("*float*: Compined perpendicular lengths of each header expansion loop [m]\n\n*Required*: True"),
 	NULL},
{"L_xpan_rnr", (getter)SolarField_get_L_xpan_rnr,(setter)SolarField_set_L_xpan_rnr,
	PyDoc_STR("*float*: Compined perpendicular lengths of each runner expansion loop [m]\n\n*Required*: True"),
 	NULL},
{"Min_rnr_xpans", (getter)SolarField_get_Min_rnr_xpans,(setter)SolarField_set_Min_rnr_xpans,
	PyDoc_STR("*float*: Minimum number of expansion loops per single-diameter runner section [none]\n\n*Required*: True"),
 	NULL},
{"N_hdr_per_xpan", (getter)SolarField_get_N_hdr_per_xpan,(setter)SolarField_set_N_hdr_per_xpan,
	PyDoc_STR("*float*: Number of collector loops per expansion loop [none]\n\n*Required*: True"),
 	NULL},
{"N_max_hdr_diams", (getter)SolarField_get_N_max_hdr_diams,(setter)SolarField_set_N_max_hdr_diams,
	PyDoc_STR("*float*: Maximum number of diameters in each of the hot and cold headers [none]\n\n*Required*: True"),
 	NULL},
{"P_a", (getter)SolarField_get_P_a,(setter)SolarField_set_P_a,
	PyDoc_STR("*sequence[sequence]*: Annulus gas pressure [torr]\n\n*Required*: True"),
 	NULL},
{"Pipe_hl_coef", (getter)SolarField_get_Pipe_hl_coef,(setter)SolarField_set_Pipe_hl_coef,
	PyDoc_STR("*float*: Loss coefficient from the header, runner pipe, and non-HCE piping [m/s]\n\n*Required*: True"),
 	NULL},
{"Rho_mirror_clean", (getter)SolarField_get_Rho_mirror_clean,(setter)SolarField_set_Rho_mirror_clean,
	PyDoc_STR("*sequence*: User-defined clean mirror reflectivity [none]\n\n*Required*: True"),
 	NULL},
{"Rough", (getter)SolarField_get_Rough,(setter)SolarField_set_Rough,
	PyDoc_STR("*sequence[sequence]*: Roughness of the internal surface  [m]\n\n*Required*: True"),
 	NULL},
{"Row_Distance", (getter)SolarField_get_Row_Distance,(setter)SolarField_set_Row_Distance,
	PyDoc_STR("*float*: Spacing between rows (centerline to centerline) [m]\n\n*Required*: True"),
 	NULL},
{"SCADefocusArray", (getter)SolarField_get_SCADefocusArray,(setter)SolarField_set_SCADefocusArray,
	PyDoc_STR("*sequence*: Collector defocus order [none]\n\n*Required*: True"),
 	NULL},
{"SCAInfoArray", (getter)SolarField_get_SCAInfoArray,(setter)SolarField_set_SCAInfoArray,
	PyDoc_STR("*sequence[sequence]*: Receiver (,1) and collector (,2) type for each assembly in loop [none]\n\n*Required*: True"),
 	NULL},
{"SCA_drives_elec", (getter)SolarField_get_SCA_drives_elec,(setter)SolarField_set_SCA_drives_elec,
	PyDoc_STR("*float*: Tracking power, in Watts per SCA drive [W/m2-K]\n\n*Required*: True"),
 	NULL},
{"Shadowing", (getter)SolarField_get_Shadowing,(setter)SolarField_set_Shadowing,
	PyDoc_STR("*sequence[sequence]*: Receiver bellows shadowing loss factor [none]\n\n*Required*: True"),
 	NULL},
{"T_fp", (getter)SolarField_get_T_fp,(setter)SolarField_set_T_fp,
	PyDoc_STR("*float*: Freeze protection temperature (heat trace activation temperature) [none]\n\n*Required*: True"),
 	NULL},
{"T_loop_in_des", (getter)SolarField_get_T_loop_in_des,(setter)SolarField_set_T_loop_in_des,
	PyDoc_STR("*float*: Design loop inlet temperature [C]\n\n*Required*: True"),
 	NULL},
{"T_loop_out", (getter)SolarField_get_T_loop_out,(setter)SolarField_set_T_loop_out,
	PyDoc_STR("*float*: Target loop outlet temperature [C]\n\n*Required*: True"),
 	NULL},
{"Tau_envelope", (getter)SolarField_get_Tau_envelope,(setter)SolarField_set_Tau_envelope,
	PyDoc_STR("*sequence[sequence]*: Envelope transmittance [none]\n\n*Required*: True"),
 	NULL},
{"TrackingError", (getter)SolarField_get_TrackingError,(setter)SolarField_set_TrackingError,
	PyDoc_STR("*sequence*: User-defined tracking error derate [none]\n\n*Required*: True"),
 	NULL},
{"Type_cpnt", (getter)SolarField_get_Type_cpnt,(setter)SolarField_set_Type_cpnt,
	PyDoc_STR("*sequence[sequence]*: Interconnect component type, row=intc, col=cpnt [none]\n\n*Required*: True"),
 	NULL},
{"V_hdr_max", (getter)SolarField_get_V_hdr_max,(setter)SolarField_set_V_hdr_max,
	PyDoc_STR("*float*: Maximum HTF velocity in the header at design [W/m2]\n\n*Required*: True"),
 	NULL},
{"V_hdr_min", (getter)SolarField_get_V_hdr_min,(setter)SolarField_set_V_hdr_min,
	PyDoc_STR("*float*: Minimum HTF velocity in the header at design [m/s]\n\n*Required*: True"),
 	NULL},
{"W_aperture", (getter)SolarField_get_W_aperture,(setter)SolarField_set_W_aperture,
	PyDoc_STR("*sequence*: The collector aperture width (Total structural area used for shadowing) [m]\n\n*Required*: True"),
 	NULL},
{"accept_init", (getter)SolarField_get_accept_init,(setter)SolarField_set_accept_init,
	PyDoc_STR("*float*: In acceptance testing mode - require steady-state startup [none]\n\n*Required*: True"),
 	NULL},
{"accept_loc", (getter)SolarField_get_accept_loc,(setter)SolarField_set_accept_loc,
	PyDoc_STR("*float*: In acceptance testing mode - temperature sensor location [1/2]\n\n*Info*: hx/loop\n\n*Required*: True"),
 	NULL},
{"accept_mode", (getter)SolarField_get_accept_mode,(setter)SolarField_set_accept_mode,
	PyDoc_STR("*float*: Acceptance testing mode? [0/1]\n\n*Info*: no/yes\n\n*Required*: True"),
 	NULL},
{"alpha_abs", (getter)SolarField_get_alpha_abs,(setter)SolarField_set_alpha_abs,
	PyDoc_STR("*sequence[sequence]*: Absorber absorptance  [none]\n\n*Required*: True"),
 	NULL},
{"alpha_env", (getter)SolarField_get_alpha_env,(setter)SolarField_set_alpha_env,
	PyDoc_STR("*sequence[sequence]*: Envelope absorptance  [none]\n\n*Required*: True"),
 	NULL},
{"calc_design_pipe_vals", (getter)SolarField_get_calc_design_pipe_vals,(setter)SolarField_set_calc_design_pipe_vals,
	PyDoc_STR("*float*: Calculate temps and pressures at design conditions for runners and headers [none]\n\n*Required*: True"),
 	NULL},
{"custom_sf_pipe_sizes", (getter)SolarField_get_custom_sf_pipe_sizes,(setter)SolarField_set_custom_sf_pipe_sizes,
	PyDoc_STR("*float*: Use custom solar field pipe diams, wallthks, and lengths [none]\n\n*Required*: True"),
 	NULL},
{"epsilon_3_11", (getter)SolarField_get_epsilon_3_11,(setter)SolarField_set_epsilon_3_11,
	PyDoc_STR("*sequence[sequence]*: Absorber emittance for receiver type 1 variation 1 [none]\n\n*Required*: True"),
 	NULL},
{"epsilon_3_12", (getter)SolarField_get_epsilon_3_12,(setter)SolarField_set_epsilon_3_12,
	PyDoc_STR("*sequence[sequence]*: Absorber emittance for receiver type 1 variation 2 [none]\n\n*Required*: True"),
 	NULL},
{"epsilon_3_13", (getter)SolarField_get_epsilon_3_13,(setter)SolarField_set_epsilon_3_13,
	PyDoc_STR("*sequence[sequence]*: Absorber emittance for receiver type 1 variation 3 [none]\n\n*Required*: True"),
 	NULL},
{"epsilon_3_14", (getter)SolarField_get_epsilon_3_14,(setter)SolarField_set_epsilon_3_14,
	PyDoc_STR("*sequence[sequence]*: Absorber emittance for receiver type 1 variation 4 [none]\n\n*Required*: True"),
 	NULL},
{"epsilon_3_21", (getter)SolarField_get_epsilon_3_21,(setter)SolarField_set_epsilon_3_21,
	PyDoc_STR("*sequence[sequence]*: Absorber emittance for receiver type 2 variation 1 [none]\n\n*Required*: True"),
 	NULL},
{"epsilon_3_22", (getter)SolarField_get_epsilon_3_22,(setter)SolarField_set_epsilon_3_22,
	PyDoc_STR("*sequence[sequence]*: Absorber emittance for receiver type 2 variation 2 [none]\n\n*Required*: True"),
 	NULL},
{"epsilon_3_23", (getter)SolarField_get_epsilon_3_23,(setter)SolarField_set_epsilon_3_23,
	PyDoc_STR("*sequence[sequence]*: Absorber emittance for receiver type 2 variation 3 [none]\n\n*Required*: True"),
 	NULL},
{"epsilon_3_24", (getter)SolarField_get_epsilon_3_24,(setter)SolarField_set_epsilon_3_24,
	PyDoc_STR("*sequence[sequence]*: Absorber emittance for receiver type 2 variation 4 [none]\n\n*Required*: True"),
 	NULL},
{"epsilon_3_31", (getter)SolarField_get_epsilon_3_31,(setter)SolarField_set_epsilon_3_31,
	PyDoc_STR("*sequence[sequence]*: Absorber emittance for receiver type 3 variation 1 [none]\n\n*Required*: True"),
 	NULL},
{"epsilon_3_32", (getter)SolarField_get_epsilon_3_32,(setter)SolarField_set_epsilon_3_32,
	PyDoc_STR("*sequence[sequence]*: Absorber emittance for receiver type 3 variation 2 [none]\n\n*Required*: True"),
 	NULL},
{"epsilon_3_33", (getter)SolarField_get_epsilon_3_33,(setter)SolarField_set_epsilon_3_33,
	PyDoc_STR("*sequence[sequence]*: Absorber emittance for receiver type 3 variation 3 [none]\n\n*Required*: True"),
 	NULL},
{"epsilon_3_34", (getter)SolarField_get_epsilon_3_34,(setter)SolarField_set_epsilon_3_34,
	PyDoc_STR("*sequence[sequence]*: Absorber emittance for receiver type 3 variation 4 [none]\n\n*Required*: True"),
 	NULL},
{"epsilon_3_41", (getter)SolarField_get_epsilon_3_41,(setter)SolarField_set_epsilon_3_41,
	PyDoc_STR("*sequence[sequence]*: Absorber emittance for receiver type 4 variation 1 [none]\n\n*Required*: True"),
 	NULL},
{"epsilon_3_42", (getter)SolarField_get_epsilon_3_42,(setter)SolarField_set_epsilon_3_42,
	PyDoc_STR("*sequence[sequence]*: Absorber emittance for receiver type 4 variation 2 [none]\n\n*Required*: True"),
 	NULL},
{"epsilon_3_43", (getter)SolarField_get_epsilon_3_43,(setter)SolarField_set_epsilon_3_43,
	PyDoc_STR("*sequence[sequence]*: Absorber emittance for receiver type 4 variation 3 [none]\n\n*Required*: True"),
 	NULL},
{"epsilon_3_44", (getter)SolarField_get_epsilon_3_44,(setter)SolarField_set_epsilon_3_44,
	PyDoc_STR("*sequence[sequence]*: Absorber emittance for receiver type 4 variation 4 [none]\n\n*Required*: True"),
 	NULL},
{"eta_pump", (getter)SolarField_get_eta_pump,(setter)SolarField_set_eta_pump,
	PyDoc_STR("*float*: HTF pump efficiency [none]\n\n*Required*: True"),
 	NULL},
{"is_model_heat_sink_piping", (getter)SolarField_get_is_model_heat_sink_piping,(setter)SolarField_set_is_model_heat_sink_piping,
	PyDoc_STR("*float*: Should model consider piping through heat sink? [none]\n\n*Required*: True"),
 	NULL},
{"m_dot_htfmax", (getter)SolarField_get_m_dot_htfmax,(setter)SolarField_set_m_dot_htfmax,
	PyDoc_STR("*float*: Maximum loop HTF flow rate [kg/s]\n\n*Required*: True"),
 	NULL},
{"m_dot_htfmin", (getter)SolarField_get_m_dot_htfmin,(setter)SolarField_set_m_dot_htfmin,
	PyDoc_STR("*float*: Minimum loop HTF flow rate [kg/s]\n\n*Required*: True"),
 	NULL},
{"mc_bal_cold", (getter)SolarField_get_mc_bal_cold,(setter)SolarField_set_mc_bal_cold,
	PyDoc_STR("*float*: Heat capacity of the balance of plant on the cold side [kWht/K-MWt]\n\n*Required*: True"),
 	NULL},
{"mc_bal_hot", (getter)SolarField_get_mc_bal_hot,(setter)SolarField_set_mc_bal_hot,
	PyDoc_STR("*float*: Heat capacity of the balance of plant on the hot side [kWht/K-MWt]\n\n*Info*: none\n\n*Required*: True"),
 	NULL},
{"mc_bal_sca", (getter)SolarField_get_mc_bal_sca,(setter)SolarField_set_mc_bal_sca,
	PyDoc_STR("*float*: Non-HTF heat capacity associated with each SCA - per meter basis [Wht/K-m]\n\n*Required*: True"),
 	NULL},
{"nColt", (getter)SolarField_get_nColt,(setter)SolarField_set_nColt,
	PyDoc_STR("*float*: Number of collector types [none]\n\n*Options*: constant=4\n\n*Required*: True"),
 	NULL},
{"nHCEVar", (getter)SolarField_get_nHCEVar,(setter)SolarField_set_nHCEVar,
	PyDoc_STR("*float*: Number of HCE variants per type [none]\n\n*Required*: True"),
 	NULL},
{"nHCEt", (getter)SolarField_get_nHCEt,(setter)SolarField_set_nHCEt,
	PyDoc_STR("*float*: Number of HCE types [none]\n\n*Required*: True"),
 	NULL},
{"nLoops", (getter)SolarField_get_nLoops,(setter)SolarField_set_nLoops,
	PyDoc_STR("*float*: Number of loops in the field [none]\n\n*Required*: True"),
 	NULL},
{"nSCA", (getter)SolarField_get_nSCA,(setter)SolarField_set_nSCA,
	PyDoc_STR("*float*: Number of SCAs in a loop [none]\n\n*Required*: True"),
 	NULL},
{"northsouth_field_sep", (getter)SolarField_get_northsouth_field_sep,(setter)SolarField_set_northsouth_field_sep,
	PyDoc_STR("*float*: North/south separation between subfields. 0 = SCAs are touching [m]\n\n*Required*: True"),
 	NULL},
{"offset_xpan_hdr", (getter)SolarField_get_offset_xpan_hdr,(setter)SolarField_set_offset_xpan_hdr,
	PyDoc_STR("*float*: Location of first header expansion loop. 1 = after first collector loop [none]\n\n*Required*: True"),
 	NULL},
{"sf_hdr_diams", (getter)SolarField_get_sf_hdr_diams,(setter)SolarField_set_sf_hdr_diams,
	PyDoc_STR("*sequence[sequence]*: Custom header diameters [m]\n\n*Required*: True"),
 	NULL},
{"sf_hdr_lengths", (getter)SolarField_get_sf_hdr_lengths,(setter)SolarField_set_sf_hdr_lengths,
	PyDoc_STR("*sequence[sequence]*: Custom header lengths [m]\n\n*Required*: True"),
 	NULL},
{"sf_hdr_wallthicks", (getter)SolarField_get_sf_hdr_wallthicks,(setter)SolarField_set_sf_hdr_wallthicks,
	PyDoc_STR("*sequence[sequence]*: Custom header wall thicknesses [m]\n\n*Required*: True"),
 	NULL},
{"sf_rnr_diams", (getter)SolarField_get_sf_rnr_diams,(setter)SolarField_set_sf_rnr_diams,
	PyDoc_STR("*sequence[sequence]*: Custom runner diameters [m]\n\n*Required*: True"),
 	NULL},
{"sf_rnr_lengths", (getter)SolarField_get_sf_rnr_lengths,(setter)SolarField_set_sf_rnr_lengths,
	PyDoc_STR("*sequence[sequence]*: Custom runner lengths [m]\n\n*Required*: True"),
 	NULL},
{"sf_rnr_wallthicks", (getter)SolarField_get_sf_rnr_wallthicks,(setter)SolarField_set_sf_rnr_wallthicks,
	PyDoc_STR("*sequence[sequence]*: Custom runner wall thicknesses [m]\n\n*Required*: True"),
 	NULL},
{"solar_mult", (getter)SolarField_get_solar_mult,(setter)SolarField_set_solar_mult,
	PyDoc_STR("*float*: Solar multiple [none]\n\n*Required*: True"),
 	NULL},
{"theta_dep", (getter)SolarField_get_theta_dep,(setter)SolarField_set_theta_dep,
	PyDoc_STR("*float*: Deploy angle [deg]\n\n*Required*: True"),
 	NULL},
{"theta_stow", (getter)SolarField_get_theta_stow,(setter)SolarField_set_theta_stow,
	PyDoc_STR("*float*: Stow angle [deg]\n\n*Required*: True"),
 	NULL},
{"washing_frequency", (getter)SolarField_get_washing_frequency,(setter)SolarField_set_washing_frequency,
	PyDoc_STR("*float*: Mirror washing frequency [-/year]\n\n*Required*: True"),
 	NULL},
{"water_usage_per_wash", (getter)SolarField_get_water_usage_per_wash,(setter)SolarField_set_water_usage_per_wash,
	PyDoc_STR("*float*: Water usage per wash [L/m2_aper]\n\n*Required*: True"),
 	NULL},
{"wind_stow_speed", (getter)SolarField_get_wind_stow_speed,(setter)SolarField_set_wind_stow_speed,
	PyDoc_STR("*float*: Trough wind stow speed [m/s]\n\n*Required*: set to 50 if not provided."),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject SolarField_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"TroughPhysicalProcessHeat.SolarField",             /*tp_name*/
		sizeof(SolarFieldObject),          /*tp_basicsize*/
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
		SolarField_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		SolarField_getset,          /*tp_getset*/
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
	 * Controller Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_TroughPhysicalProcessHeat   data_ptr;
} ControllerObject;

static PyTypeObject Controller_Type;

static PyObject *
Controller_new(SAM_TroughPhysicalProcessHeat data_ptr)
{
	PyObject* new_obj = Controller_Type.tp_alloc(&Controller_Type,0);

	ControllerObject* Controller_obj = (ControllerObject*)new_obj;

	Controller_obj->data_ptr = data_ptr;

	return new_obj;
}

/* Controller methods */

static PyObject *
Controller_assign(ControllerObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "TroughPhysicalProcessHeat", "Controller")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
Controller_export(ControllerObject *self, PyObject *args)
{
	PyTypeObject* tp = &Controller_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef Controller_methods[] = {
		{"assign",            (PyCFunction)Controller_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``Controller_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)Controller_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
Controller_get_field_fl_props(ControllerObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TroughPhysicalProcessHeat_Controller_field_fl_props_mget, self->data_ptr);
}

static int
Controller_set_field_fl_props(ControllerObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TroughPhysicalProcessHeat_Controller_field_fl_props_mset, self->data_ptr);
}

static PyObject *
Controller_get_pb_pump_coef(ControllerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_Controller_pb_pump_coef_nget, self->data_ptr);
}

static int
Controller_set_pb_pump_coef(ControllerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TroughPhysicalProcessHeat_Controller_pb_pump_coef_nset, self->data_ptr);
}

static PyObject *
Controller_get_q_pb_design(ControllerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_Controller_q_pb_design_nget, self->data_ptr);
}

static int
Controller_set_q_pb_design(ControllerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TroughPhysicalProcessHeat_Controller_q_pb_design_nset, self->data_ptr);
}

static PyGetSetDef Controller_getset[] = {
{"field_fl_props", (getter)Controller_get_field_fl_props,(setter)Controller_set_field_fl_props,
	PyDoc_STR("*sequence[sequence]*: User defined field fluid property data [-]\n\n*Required*: True"),
 	NULL},
{"pb_pump_coef", (getter)Controller_get_pb_pump_coef,(setter)Controller_set_pb_pump_coef,
	PyDoc_STR("*float*: Pumping power to move 1kg of HTF through PB loop [kW/kg]\n\n*Required*: True"),
 	NULL},
{"q_pb_design", (getter)Controller_get_q_pb_design,(setter)Controller_set_q_pb_design,
	PyDoc_STR("*float*: Design heat input to power block [MWt]\n\n*Required*: True"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject Controller_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"TroughPhysicalProcessHeat.Controller",             /*tp_name*/
		sizeof(ControllerObject),          /*tp_basicsize*/
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
		Controller_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		Controller_getset,          /*tp_getset*/
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
	SAM_TroughPhysicalProcessHeat   data_ptr;
} SystemDesignObject;

static PyTypeObject SystemDesign_Type;

static PyObject *
SystemDesign_new(SAM_TroughPhysicalProcessHeat data_ptr)
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

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "TroughPhysicalProcessHeat", "SystemDesign")){
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
SystemDesign_get_tshours(SystemDesignObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_SystemDesign_tshours_nget, self->data_ptr);
}

static int
SystemDesign_set_tshours(SystemDesignObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TroughPhysicalProcessHeat_SystemDesign_tshours_nset, self->data_ptr);
}

static PyGetSetDef SystemDesign_getset[] = {
{"tshours", (getter)SystemDesign_get_tshours,(setter)SystemDesign_set_tshours,
	PyDoc_STR("*float*: Equivalent full-load thermal storage hours [hr]\n\n*Required*: True"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject SystemDesign_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"TroughPhysicalProcessHeat.SystemDesign",             /*tp_name*/
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
	 * TES Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_TroughPhysicalProcessHeat   data_ptr;
} TESObject;

static PyTypeObject TES_Type;

static PyObject *
TES_new(SAM_TroughPhysicalProcessHeat data_ptr)
{
	PyObject* new_obj = TES_Type.tp_alloc(&TES_Type,0);

	TESObject* TES_obj = (TESObject*)new_obj;

	TES_obj->data_ptr = data_ptr;

	return new_obj;
}

/* TES methods */

static PyObject *
TES_assign(TESObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "TroughPhysicalProcessHeat", "TES")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
TES_export(TESObject *self, PyObject *args)
{
	PyTypeObject* tp = &TES_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef TES_methods[] = {
		{"assign",            (PyCFunction)TES_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``TES_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)TES_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
TES_get_cold_tank_Thtr(TESObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_TES_cold_tank_Thtr_nget, self->data_ptr);
}

static int
TES_set_cold_tank_Thtr(TESObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TroughPhysicalProcessHeat_TES_cold_tank_Thtr_nset, self->data_ptr);
}

static PyObject *
TES_get_cold_tank_max_heat(TESObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_TES_cold_tank_max_heat_nget, self->data_ptr);
}

static int
TES_set_cold_tank_max_heat(TESObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TroughPhysicalProcessHeat_TES_cold_tank_max_heat_nset, self->data_ptr);
}

static PyObject *
TES_get_h_tank(TESObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_TES_h_tank_nget, self->data_ptr);
}

static int
TES_set_h_tank(TESObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TroughPhysicalProcessHeat_TES_h_tank_nset, self->data_ptr);
}

static PyObject *
TES_get_init_hot_htf_percent(TESObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_TES_init_hot_htf_percent_nget, self->data_ptr);
}

static int
TES_set_init_hot_htf_percent(TESObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TroughPhysicalProcessHeat_TES_init_hot_htf_percent_nset, self->data_ptr);
}

static PyObject *
TES_get_tank_pairs(TESObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_TES_tank_pairs_nget, self->data_ptr);
}

static int
TES_set_tank_pairs(TESObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TroughPhysicalProcessHeat_TES_tank_pairs_nset, self->data_ptr);
}

static PyObject *
TES_get_u_tank(TESObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_TES_u_tank_nget, self->data_ptr);
}

static int
TES_set_u_tank(TESObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TroughPhysicalProcessHeat_TES_u_tank_nset, self->data_ptr);
}

static PyGetSetDef TES_getset[] = {
{"cold_tank_Thtr", (getter)TES_get_cold_tank_Thtr,(setter)TES_set_cold_tank_Thtr,
	PyDoc_STR("*float*: Minimum allowable cold tank HTF temp [C]\n\n*Required*: True"),
 	NULL},
{"cold_tank_max_heat", (getter)TES_get_cold_tank_max_heat,(setter)TES_set_cold_tank_max_heat,
	PyDoc_STR("*float*: Rated heater capacity for cold tank heating [MW]\n\n*Required*: True"),
 	NULL},
{"h_tank", (getter)TES_get_h_tank,(setter)TES_set_h_tank,
	PyDoc_STR("*float*: Total height of tank (height of HTF when tank is full [m]\n\n*Required*: True"),
 	NULL},
{"init_hot_htf_percent", (getter)TES_get_init_hot_htf_percent,(setter)TES_set_init_hot_htf_percent,
	PyDoc_STR("*float*: Initial fraction of avail. vol that is hot [%]\n\n*Required*: True"),
 	NULL},
{"tank_pairs", (getter)TES_get_tank_pairs,(setter)TES_set_tank_pairs,
	PyDoc_STR("*float*: Number of equivalent tank pairs [-]\n\n*Constraints*: INTEGER\n\n*Required*: True"),
 	NULL},
{"u_tank", (getter)TES_get_u_tank,(setter)TES_set_u_tank,
	PyDoc_STR("*float*: Loss coefficient from the tank [W/m2-K]\n\n*Required*: True"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject TES_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"TroughPhysicalProcessHeat.TES",             /*tp_name*/
		sizeof(TESObject),          /*tp_basicsize*/
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
		TES_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		TES_getset,          /*tp_getset*/
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
	 * TES2tank Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_TroughPhysicalProcessHeat   data_ptr;
} TES2tankObject;

static PyTypeObject TES2tank_Type;

static PyObject *
TES2tank_new(SAM_TroughPhysicalProcessHeat data_ptr)
{
	PyObject* new_obj = TES2tank_Type.tp_alloc(&TES2tank_Type,0);

	TES2tankObject* TES2tank_obj = (TES2tankObject*)new_obj;

	TES2tank_obj->data_ptr = data_ptr;

	return new_obj;
}

/* TES2tank methods */

static PyObject *
TES2tank_assign(TES2tankObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "TroughPhysicalProcessHeat", "TES2tank")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
TES2tank_export(TES2tankObject *self, PyObject *args)
{
	PyTypeObject* tp = &TES2tank_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef TES2tank_methods[] = {
		{"assign",            (PyCFunction)TES2tank_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``TES2tank_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)TES2tank_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
TES2tank_get_h_tank_min(TES2tankObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_TES2tank_h_tank_min_nget, self->data_ptr);
}

static int
TES2tank_set_h_tank_min(TES2tankObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TroughPhysicalProcessHeat_TES2tank_h_tank_min_nset, self->data_ptr);
}

static PyObject *
TES2tank_get_hot_tank_Thtr(TES2tankObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_TES2tank_hot_tank_Thtr_nget, self->data_ptr);
}

static int
TES2tank_set_hot_tank_Thtr(TES2tankObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TroughPhysicalProcessHeat_TES2tank_hot_tank_Thtr_nset, self->data_ptr);
}

static PyObject *
TES2tank_get_hot_tank_max_heat(TES2tankObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_TES2tank_hot_tank_max_heat_nget, self->data_ptr);
}

static int
TES2tank_set_hot_tank_max_heat(TES2tankObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TroughPhysicalProcessHeat_TES2tank_hot_tank_max_heat_nset, self->data_ptr);
}

static PyGetSetDef TES2tank_getset[] = {
{"h_tank_min", (getter)TES2tank_get_h_tank_min,(setter)TES2tank_set_h_tank_min,
	PyDoc_STR("*float*: Minimum allowable HTF height in storage tank [m]\n\n*Required*: True"),
 	NULL},
{"hot_tank_Thtr", (getter)TES2tank_get_hot_tank_Thtr,(setter)TES2tank_set_hot_tank_Thtr,
	PyDoc_STR("*float*: Minimum allowable hot tank HTF temp [C]\n\n*Required*: True"),
 	NULL},
{"hot_tank_max_heat", (getter)TES2tank_get_hot_tank_max_heat,(setter)TES2tank_set_hot_tank_max_heat,
	PyDoc_STR("*float*: Rated heater capacity for hot tank heating [MW]\n\n*Required*: True"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject TES2tank_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"TroughPhysicalProcessHeat.TES2tank",             /*tp_name*/
		sizeof(TES2tankObject),          /*tp_basicsize*/
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
		TES2tank_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		TES2tank_getset,          /*tp_getset*/
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
	 * Powerblock Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_TroughPhysicalProcessHeat   data_ptr;
} PowerblockObject;

static PyTypeObject Powerblock_Type;

static PyObject *
Powerblock_new(SAM_TroughPhysicalProcessHeat data_ptr)
{
	PyObject* new_obj = Powerblock_Type.tp_alloc(&Powerblock_Type,0);

	PowerblockObject* Powerblock_obj = (PowerblockObject*)new_obj;

	Powerblock_obj->data_ptr = data_ptr;

	return new_obj;
}

/* Powerblock methods */

static PyObject *
Powerblock_assign(PowerblockObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "TroughPhysicalProcessHeat", "Powerblock")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
Powerblock_export(PowerblockObject *self, PyObject *args)
{
	PyTypeObject* tp = &Powerblock_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef Powerblock_methods[] = {
		{"assign",            (PyCFunction)Powerblock_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``Powerblock_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)Powerblock_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
Powerblock_get_L_rnr_pb(PowerblockObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_Powerblock_L_rnr_pb_nget, self->data_ptr);
}

static int
Powerblock_set_L_rnr_pb(PowerblockObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TroughPhysicalProcessHeat_Powerblock_L_rnr_pb_nset, self->data_ptr);
}

static PyGetSetDef Powerblock_getset[] = {
{"L_rnr_pb", (getter)Powerblock_get_L_rnr_pb,(setter)Powerblock_set_L_rnr_pb,
	PyDoc_STR("*float*: Length of runner pipe in power block [m]\n\n*Required*: True"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject Powerblock_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"TroughPhysicalProcessHeat.Powerblock",             /*tp_name*/
		sizeof(PowerblockObject),          /*tp_basicsize*/
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
		Powerblock_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		Powerblock_getset,          /*tp_getset*/
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
	SAM_TroughPhysicalProcessHeat   data_ptr;
} OutputsObject;

static PyTypeObject Outputs_Type;

static PyObject *
Outputs_new(SAM_TroughPhysicalProcessHeat data_ptr)
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

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "TroughPhysicalProcessHeat", "Outputs")){
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
Outputs_get_CosTh_ave(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_Outputs_CosTh_ave_aget, self->data_ptr);
}

static PyObject *
Outputs_get_EndLoss_ave(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_Outputs_EndLoss_ave_aget, self->data_ptr);
}

static PyObject *
Outputs_get_EqOpteff(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_Outputs_EqOpteff_aget, self->data_ptr);
}

static PyObject *
Outputs_get_IAM_ave(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_Outputs_IAM_ave_aget, self->data_ptr);
}

static PyObject *
Outputs_get_RowShadow_ave(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_Outputs_RowShadow_ave_aget, self->data_ptr);
}

static PyObject *
Outputs_get_SCAs_def(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_Outputs_SCAs_def_aget, self->data_ptr);
}

static PyObject *
Outputs_get_T_field_cold_in(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_Outputs_T_field_cold_in_aget, self->data_ptr);
}

static PyObject *
Outputs_get_T_field_hot_out(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_Outputs_T_field_hot_out_aget, self->data_ptr);
}

static PyObject *
Outputs_get_T_heat_sink_in(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_Outputs_T_heat_sink_in_aget, self->data_ptr);
}

static PyObject *
Outputs_get_T_heat_sink_out(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_Outputs_T_heat_sink_out_aget, self->data_ptr);
}

static PyObject *
Outputs_get_T_rec_cold_in(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_Outputs_T_rec_cold_in_aget, self->data_ptr);
}

static PyObject *
Outputs_get_T_rec_hot_out(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_Outputs_T_rec_hot_out_aget, self->data_ptr);
}

static PyObject *
Outputs_get_T_tes_cold(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_Outputs_T_tes_cold_aget, self->data_ptr);
}

static PyObject *
Outputs_get_T_tes_hot(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_Outputs_T_tes_hot_aget, self->data_ptr);
}

static PyObject *
Outputs_get_Theta_ave(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_Outputs_Theta_ave_aget, self->data_ptr);
}

static PyObject *
Outputs_get_W_dot_field_pump(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_Outputs_W_dot_field_pump_aget, self->data_ptr);
}

static PyObject *
Outputs_get_W_dot_parasitic_tot(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_Outputs_W_dot_parasitic_tot_aget, self->data_ptr);
}

static PyObject *
Outputs_get_W_dot_pc_pump(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_Outputs_W_dot_pc_pump_aget, self->data_ptr);
}

static PyObject *
Outputs_get_W_dot_sca_track(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_Outputs_W_dot_sca_track_aget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_electricity_consumption(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_Outputs_annual_electricity_consumption_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_energy(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_Outputs_annual_energy_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_field_freeze_protection(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_Outputs_annual_field_freeze_protection_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_gross_energy(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_Outputs_annual_gross_energy_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_tes_freeze_protection(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_Outputs_annual_tes_freeze_protection_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_thermal_consumption(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_Outputs_annual_thermal_consumption_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_total_water_use(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TroughPhysicalProcessHeat_Outputs_annual_total_water_use_nget, self->data_ptr);
}

static PyObject *
Outputs_get_beam(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_Outputs_beam_aget, self->data_ptr);
}

static PyObject *
Outputs_get_deltaP_field(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_Outputs_deltaP_field_aget, self->data_ptr);
}

static PyObject *
Outputs_get_dni_costh(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_Outputs_dni_costh_aget, self->data_ptr);
}

static PyObject *
Outputs_get_e_ch_tes(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_Outputs_e_ch_tes_aget, self->data_ptr);
}

static PyObject *
Outputs_get_e_dot_field_int_energy(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_Outputs_e_dot_field_int_energy_aget, self->data_ptr);
}

static PyObject *
Outputs_get_hour_day(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_Outputs_hour_day_aget, self->data_ptr);
}

static PyObject *
Outputs_get_m_dot_balance(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_Outputs_m_dot_balance_aget, self->data_ptr);
}

static PyObject *
Outputs_get_m_dot_field_delivered(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_Outputs_m_dot_field_delivered_aget, self->data_ptr);
}

static PyObject *
Outputs_get_m_dot_field_recirc(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_Outputs_m_dot_field_recirc_aget, self->data_ptr);
}

static PyObject *
Outputs_get_m_dot_htf_heat_sink(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_Outputs_m_dot_htf_heat_sink_aget, self->data_ptr);
}

static PyObject *
Outputs_get_m_dot_loop(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_Outputs_m_dot_loop_aget, self->data_ptr);
}

static PyObject *
Outputs_get_m_dot_tes_ch(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_Outputs_m_dot_tes_ch_aget, self->data_ptr);
}

static PyObject *
Outputs_get_m_dot_tes_dc(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_Outputs_m_dot_tes_dc_aget, self->data_ptr);
}

static PyObject *
Outputs_get_month(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_Outputs_month_aget, self->data_ptr);
}

static PyObject *
Outputs_get_op_mode_1(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_Outputs_op_mode_1_aget, self->data_ptr);
}

static PyObject *
Outputs_get_op_mode_2(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_Outputs_op_mode_2_aget, self->data_ptr);
}

static PyObject *
Outputs_get_op_mode_3(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_Outputs_op_mode_3_aget, self->data_ptr);
}

static PyObject *
Outputs_get_pres(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_Outputs_pres_aget, self->data_ptr);
}

static PyObject *
Outputs_get_q_balance(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_Outputs_q_balance_aget, self->data_ptr);
}

static PyObject *
Outputs_get_q_ch_tes(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_Outputs_q_ch_tes_aget, self->data_ptr);
}

static PyObject *
Outputs_get_q_dc_tes(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_Outputs_q_dc_tes_aget, self->data_ptr);
}

static PyObject *
Outputs_get_q_dot_freeze_prot(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_Outputs_q_dot_freeze_prot_aget, self->data_ptr);
}

static PyObject *
Outputs_get_q_dot_htf_sf_out(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_Outputs_q_dot_htf_sf_out_aget, self->data_ptr);
}

static PyObject *
Outputs_get_q_dot_piping_loss(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_Outputs_q_dot_piping_loss_aget, self->data_ptr);
}

static PyObject *
Outputs_get_q_dot_rec_abs(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_Outputs_q_dot_rec_abs_aget, self->data_ptr);
}

static PyObject *
Outputs_get_q_dot_rec_inc(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_Outputs_q_dot_rec_inc_aget, self->data_ptr);
}

static PyObject *
Outputs_get_q_dot_rec_thermal_loss(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_Outputs_q_dot_rec_thermal_loss_aget, self->data_ptr);
}

static PyObject *
Outputs_get_q_dot_to_heat_sink(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_Outputs_q_dot_to_heat_sink_aget, self->data_ptr);
}

static PyObject *
Outputs_get_q_inc_sf_tot(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_Outputs_q_inc_sf_tot_aget, self->data_ptr);
}

static PyObject *
Outputs_get_q_tes_heater(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_Outputs_q_tes_heater_aget, self->data_ptr);
}

static PyObject *
Outputs_get_qinc_costh(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_Outputs_qinc_costh_aget, self->data_ptr);
}

static PyObject *
Outputs_get_solazi(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_Outputs_solazi_aget, self->data_ptr);
}

static PyObject *
Outputs_get_solzen(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_Outputs_solzen_aget, self->data_ptr);
}

static PyObject *
Outputs_get_tank_losses(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_Outputs_tank_losses_aget, self->data_ptr);
}

static PyObject *
Outputs_get_tdry(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_Outputs_tdry_aget, self->data_ptr);
}

static PyObject *
Outputs_get_time_hr(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_Outputs_time_hr_aget, self->data_ptr);
}

static PyObject *
Outputs_get_twet(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_Outputs_twet_aget, self->data_ptr);
}

static PyObject *
Outputs_get_wspd(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TroughPhysicalProcessHeat_Outputs_wspd_aget, self->data_ptr);
}

static PyGetSetDef Outputs_getset[] = {
{"CosTh_ave", (getter)Outputs_get_CosTh_ave,(setter)0,
	PyDoc_STR("*sequence*: Field collector cosine efficiency"),
 	NULL},
{"EndLoss_ave", (getter)Outputs_get_EndLoss_ave,(setter)0,
	PyDoc_STR("*sequence*: Field collector optical end loss"),
 	NULL},
{"EqOpteff", (getter)Outputs_get_EqOpteff,(setter)0,
	PyDoc_STR("*sequence*: Field optical efficiency before defocus"),
 	NULL},
{"IAM_ave", (getter)Outputs_get_IAM_ave,(setter)0,
	PyDoc_STR("*sequence*: Field collector incidence angle modifier"),
 	NULL},
{"RowShadow_ave", (getter)Outputs_get_RowShadow_ave,(setter)0,
	PyDoc_STR("*sequence*: Field collector row shadowing loss"),
 	NULL},
{"SCAs_def", (getter)Outputs_get_SCAs_def,(setter)0,
	PyDoc_STR("*sequence*: Field fraction of focused SCAs"),
 	NULL},
{"T_field_cold_in", (getter)Outputs_get_T_field_cold_in,(setter)0,
	PyDoc_STR("*sequence*: Field timestep-averaged inlet temperature [C]"),
 	NULL},
{"T_field_hot_out", (getter)Outputs_get_T_field_hot_out,(setter)0,
	PyDoc_STR("*sequence*: Field timestep-averaged outlet temperature [C]"),
 	NULL},
{"T_heat_sink_in", (getter)Outputs_get_T_heat_sink_in,(setter)0,
	PyDoc_STR("*sequence*: Heat sink HTF inlet temp [C]"),
 	NULL},
{"T_heat_sink_out", (getter)Outputs_get_T_heat_sink_out,(setter)0,
	PyDoc_STR("*sequence*: Heat sink HTF outlet temp [C]"),
 	NULL},
{"T_rec_cold_in", (getter)Outputs_get_T_rec_cold_in,(setter)0,
	PyDoc_STR("*sequence*: Loop timestep-averaged inlet temperature [C]"),
 	NULL},
{"T_rec_hot_out", (getter)Outputs_get_T_rec_hot_out,(setter)0,
	PyDoc_STR("*sequence*: Loop timestep-averaged outlet temperature [C]"),
 	NULL},
{"T_tes_cold", (getter)Outputs_get_T_tes_cold,(setter)0,
	PyDoc_STR("*sequence*: TES cold temperature [C]"),
 	NULL},
{"T_tes_hot", (getter)Outputs_get_T_tes_hot,(setter)0,
	PyDoc_STR("*sequence*: TES hot temperature [C]"),
 	NULL},
{"Theta_ave", (getter)Outputs_get_Theta_ave,(setter)0,
	PyDoc_STR("*sequence*: Field collector solar incidence angle [deg]"),
 	NULL},
{"W_dot_field_pump", (getter)Outputs_get_W_dot_field_pump,(setter)0,
	PyDoc_STR("*sequence*: Field htf pumping power [MWe]"),
 	NULL},
{"W_dot_parasitic_tot", (getter)Outputs_get_W_dot_parasitic_tot,(setter)0,
	PyDoc_STR("*sequence*: System total electrical parasitic [MWe]"),
 	NULL},
{"W_dot_pc_pump", (getter)Outputs_get_W_dot_pc_pump,(setter)0,
	PyDoc_STR("*sequence*: Heat sink pumping power [MWe]"),
 	NULL},
{"W_dot_sca_track", (getter)Outputs_get_W_dot_sca_track,(setter)0,
	PyDoc_STR("*sequence*: Field collector tracking power [MWe]"),
 	NULL},
{"annual_electricity_consumption", (getter)Outputs_get_annual_electricity_consumption,(setter)0,
	PyDoc_STR("*float*: Annual electricity consumption w/ avail derate [kWe-hr]"),
 	NULL},
{"annual_energy", (getter)Outputs_get_annual_energy,(setter)0,
	PyDoc_STR("*float*: Annual Net Thermal Energy Production w/ avail derate [kWt-hr]"),
 	NULL},
{"annual_field_freeze_protection", (getter)Outputs_get_annual_field_freeze_protection,(setter)0,
	PyDoc_STR("*float*: Annual thermal power for field freeze protection [kWt-hr]"),
 	NULL},
{"annual_gross_energy", (getter)Outputs_get_annual_gross_energy,(setter)0,
	PyDoc_STR("*float*: Annual Gross Thermal Energy Production w/ avail derate [kWt-hr]"),
 	NULL},
{"annual_tes_freeze_protection", (getter)Outputs_get_annual_tes_freeze_protection,(setter)0,
	PyDoc_STR("*float*: Annual thermal power for TES freeze protection [kWt-hr]"),
 	NULL},
{"annual_thermal_consumption", (getter)Outputs_get_annual_thermal_consumption,(setter)0,
	PyDoc_STR("*float*: Annual thermal freeze protection required [kWt-hr]"),
 	NULL},
{"annual_total_water_use", (getter)Outputs_get_annual_total_water_use,(setter)0,
	PyDoc_STR("*float*: Total Annual Water Usage [m^3]"),
 	NULL},
{"beam", (getter)Outputs_get_beam,(setter)0,
	PyDoc_STR("*sequence*: Resource Beam normal irradiance [W/m2]"),
 	NULL},
{"deltaP_field", (getter)Outputs_get_deltaP_field,(setter)0,
	PyDoc_STR("*sequence*: Field pressure drop [bar]"),
 	NULL},
{"dni_costh", (getter)Outputs_get_dni_costh,(setter)0,
	PyDoc_STR("*sequence*: Field collector DNI-cosine product [W/m2]"),
 	NULL},
{"e_ch_tes", (getter)Outputs_get_e_ch_tes,(setter)0,
	PyDoc_STR("*sequence*: TES charge state [MWht]"),
 	NULL},
{"e_dot_field_int_energy", (getter)Outputs_get_e_dot_field_int_energy,(setter)0,
	PyDoc_STR("*sequence*: Field change in material/htf internal energy [MWt]"),
 	NULL},
{"hour_day", (getter)Outputs_get_hour_day,(setter)0,
	PyDoc_STR("*sequence*: Resource Hour of Day"),
 	NULL},
{"m_dot_balance", (getter)Outputs_get_m_dot_balance,(setter)0,
	PyDoc_STR("*sequence*: Relative mass flow balance error"),
 	NULL},
{"m_dot_field_delivered", (getter)Outputs_get_m_dot_field_delivered,(setter)0,
	PyDoc_STR("*sequence*: Field total mass flow delivered [kg/s]"),
 	NULL},
{"m_dot_field_recirc", (getter)Outputs_get_m_dot_field_recirc,(setter)0,
	PyDoc_STR("*sequence*: Field total mass flow recirculated [kg/s]"),
 	NULL},
{"m_dot_htf_heat_sink", (getter)Outputs_get_m_dot_htf_heat_sink,(setter)0,
	PyDoc_STR("*sequence*: Heat sink HTF mass flow [kg/s]"),
 	NULL},
{"m_dot_loop", (getter)Outputs_get_m_dot_loop,(setter)0,
	PyDoc_STR("*sequence*: Receiver mass flow rate [kg/s]"),
 	NULL},
{"m_dot_tes_ch", (getter)Outputs_get_m_dot_tes_ch,(setter)0,
	PyDoc_STR("*sequence*: TES charge mass flow rate [kg/s]"),
 	NULL},
{"m_dot_tes_dc", (getter)Outputs_get_m_dot_tes_dc,(setter)0,
	PyDoc_STR("*sequence*: TES discharge mass flow rate [kg/s]"),
 	NULL},
{"month", (getter)Outputs_get_month,(setter)0,
	PyDoc_STR("*sequence*: Resource Month"),
 	NULL},
{"op_mode_1", (getter)Outputs_get_op_mode_1,(setter)0,
	PyDoc_STR("*sequence*: 1st operating mode"),
 	NULL},
{"op_mode_2", (getter)Outputs_get_op_mode_2,(setter)0,
	PyDoc_STR("*sequence*: 2nd op. mode, if applicable"),
 	NULL},
{"op_mode_3", (getter)Outputs_get_op_mode_3,(setter)0,
	PyDoc_STR("*sequence*: 3rd op. mode, if applicable"),
 	NULL},
{"pres", (getter)Outputs_get_pres,(setter)0,
	PyDoc_STR("*sequence*: Resource Pressure [mbar]"),
 	NULL},
{"q_balance", (getter)Outputs_get_q_balance,(setter)0,
	PyDoc_STR("*sequence*: Relative energy balance error"),
 	NULL},
{"q_ch_tes", (getter)Outputs_get_q_ch_tes,(setter)0,
	PyDoc_STR("*sequence*: TES charge thermal power [MWt]"),
 	NULL},
{"q_dc_tes", (getter)Outputs_get_q_dc_tes,(setter)0,
	PyDoc_STR("*sequence*: TES discharge thermal power [MWt]"),
 	NULL},
{"q_dot_freeze_prot", (getter)Outputs_get_q_dot_freeze_prot,(setter)0,
	PyDoc_STR("*sequence*: Field freeze protection required [MWt]"),
 	NULL},
{"q_dot_htf_sf_out", (getter)Outputs_get_q_dot_htf_sf_out,(setter)0,
	PyDoc_STR("*sequence*: Field thermal power leaving in HTF [MWt]"),
 	NULL},
{"q_dot_piping_loss", (getter)Outputs_get_q_dot_piping_loss,(setter)0,
	PyDoc_STR("*sequence*: Field piping thermal losses [MWt]"),
 	NULL},
{"q_dot_rec_abs", (getter)Outputs_get_q_dot_rec_abs,(setter)0,
	PyDoc_STR("*sequence*: Receiver thermal power absorbed [MWt]"),
 	NULL},
{"q_dot_rec_inc", (getter)Outputs_get_q_dot_rec_inc,(setter)0,
	PyDoc_STR("*sequence*: Receiver thermal power incident [MWt]"),
 	NULL},
{"q_dot_rec_thermal_loss", (getter)Outputs_get_q_dot_rec_thermal_loss,(setter)0,
	PyDoc_STR("*sequence*: Receiver thermal losses [MWt]"),
 	NULL},
{"q_dot_to_heat_sink", (getter)Outputs_get_q_dot_to_heat_sink,(setter)0,
	PyDoc_STR("*sequence*: Heat sink thermal power [MWt]"),
 	NULL},
{"q_inc_sf_tot", (getter)Outputs_get_q_inc_sf_tot,(setter)0,
	PyDoc_STR("*sequence*: Field thermal power incident [MWt]"),
 	NULL},
{"q_tes_heater", (getter)Outputs_get_q_tes_heater,(setter)0,
	PyDoc_STR("*sequence*: TES freeze protection power [MWe]"),
 	NULL},
{"qinc_costh", (getter)Outputs_get_qinc_costh,(setter)0,
	PyDoc_STR("*sequence*: Field thermal power incident after cosine [MWt]"),
 	NULL},
{"solazi", (getter)Outputs_get_solazi,(setter)0,
	PyDoc_STR("*sequence*: Resource Solar Azimuth [deg]"),
 	NULL},
{"solzen", (getter)Outputs_get_solzen,(setter)0,
	PyDoc_STR("*sequence*: Resource Solar Zenith [deg]"),
 	NULL},
{"tank_losses", (getter)Outputs_get_tank_losses,(setter)0,
	PyDoc_STR("*sequence*: TES thermal losses [MWt]"),
 	NULL},
{"tdry", (getter)Outputs_get_tdry,(setter)0,
	PyDoc_STR("*sequence*: Resource Dry bulb temperature [C]"),
 	NULL},
{"time_hr", (getter)Outputs_get_time_hr,(setter)0,
	PyDoc_STR("*sequence*: Time at end of timestep [hr]"),
 	NULL},
{"twet", (getter)Outputs_get_twet,(setter)0,
	PyDoc_STR("*sequence*: Resource Wet bulb temperature [C]"),
 	NULL},
{"wspd", (getter)Outputs_get_wspd,(setter)0,
	PyDoc_STR("*sequence*: Resource Wind Speed [m/s]"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject Outputs_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"TroughPhysicalProcessHeat.Outputs",             /*tp_name*/
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
 * TroughPhysicalProcessHeat
 */

typedef struct {
	PyObject_HEAD
	PyObject            *x_attr;        /* Attributes dictionary */
	SAM_TroughPhysicalProcessHeat   data_ptr;
} TroughPhysicalProcessHeatObject;

static PyTypeObject TroughPhysicalProcessHeat_Type;

#define TroughPhysicalProcessHeatObject_Check(v)      (Py_TYPE(v) == &TroughPhysicalProcessHeat_Type)

static TroughPhysicalProcessHeatObject *
newTroughPhysicalProcessHeatObject(void* data_ptr)
{
	TroughPhysicalProcessHeatObject *self;
	self = PyObject_New(TroughPhysicalProcessHeatObject, &TroughPhysicalProcessHeat_Type);

	PySAM_TECH_ATTR("TroughPhysicalProcessHeat", SAM_TroughPhysicalProcessHeat_construct)

	PyObject* Weather_obj = Weather_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "Weather", Weather_obj);
	Py_DECREF(Weather_obj);

	PyObject* SolarField_obj = SolarField_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "SolarField", SolarField_obj);
	Py_DECREF(SolarField_obj);

	PyObject* Controller_obj = Controller_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "Controller", Controller_obj);
	Py_DECREF(Controller_obj);

	PyObject* SystemDesign_obj = SystemDesign_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "SystemDesign", SystemDesign_obj);
	Py_DECREF(SystemDesign_obj);

	PyObject* TES_obj = TES_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "TES", TES_obj);
	Py_DECREF(TES_obj);

	PyObject* TES2tank_obj = TES2tank_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "TES2tank", TES2tank_obj);
	Py_DECREF(TES2tank_obj);

	PyObject* Powerblock_obj = Powerblock_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "Powerblock", Powerblock_obj);
	Py_DECREF(Powerblock_obj);

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

/* TroughPhysicalProcessHeat methods */

static void
TroughPhysicalProcessHeat_dealloc(TroughPhysicalProcessHeatObject *self)
{
	Py_XDECREF(self->x_attr);
	SAM_TroughPhysicalProcessHeat_destruct(self->data_ptr);
	PyObject_Del(self);
}


static PyObject *
TroughPhysicalProcessHeat_execute(TroughPhysicalProcessHeatObject *self, PyObject *args)
{
	int verbosity = 0;

	if (!PyArg_ParseTuple(args, "|i", &verbosity))
		return NULL;

	SAM_error error = new_error();
	SAM_TroughPhysicalProcessHeat_execute(self->data_ptr, verbosity, &error);
	if (PySAM_has_error(error )) return NULL;

	Py_INCREF(Py_None);
	return Py_None;
}


static PyObject *
TroughPhysicalProcessHeat_assign(TroughPhysicalProcessHeatObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_nested_dict((PyObject*)self, self->x_attr, self->data_ptr, dict, "TroughPhysicalProcessHeat"))
		return NULL;

	Py_INCREF(Py_None);
	return Py_None;
}


static PyObject *
TroughPhysicalProcessHeat_export(TroughPhysicalProcessHeatObject *self, PyObject *args)
{
	return PySAM_export_to_nested_dict((PyObject *) self, self->x_attr);
}

static PyMethodDef TroughPhysicalProcessHeat_methods[] = {
		{"execute",            (PyCFunction)TroughPhysicalProcessHeat_execute,  METH_VARARGS,
				PyDoc_STR("execute(int verbosity) -> None\n Execute simulation with verbosity level 0 (default) or 1")},
		{"assign",            (PyCFunction)TroughPhysicalProcessHeat_assign,  METH_VARARGS,
				PyDoc_STR("assign(dict) -> None\n Assign attributes from nested dictionary, except for Outputs\n\n``nested_dict = { 'Weather': { var: val, ...}, ...}``")},
		{"export",            (PyCFunction)TroughPhysicalProcessHeat_export,  METH_VARARGS,
				PyDoc_STR("export() -> dict\n Export attributes into nested dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
TroughPhysicalProcessHeat_getattro(TroughPhysicalProcessHeatObject *self, PyObject *name)
{
	return PySAM_get_attr((PyObject*) self, (PyObject*) self->x_attr, name);
}

static int
TroughPhysicalProcessHeat_setattr(TroughPhysicalProcessHeatObject *self, const char *name, PyObject *v)
{
	return PySAM_set_attr((PyObject*)self, (PyObject*)self->x_attr, name, v);
}

static PyTypeObject TroughPhysicalProcessHeat_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"TroughPhysicalProcessHeat",            /*tp_name*/
		sizeof(TroughPhysicalProcessHeatObject),/*tp_basicsize*/
		0,                          /*tp_itemsize*/
		/* methods */
		(destructor)TroughPhysicalProcessHeat_dealloc,    /*tp_dealloc*/
		0,                          /*tp_print*/
		(getattrfunc)0,             /*tp_getattr*/
		(setattrfunc)TroughPhysicalProcessHeat_setattr,   /*tp_setattr*/
		0,                          /*tp_reserved*/
		0,                          /*tp_repr*/
		0,                          /*tp_as_number*/
		0,                          /*tp_as_sequence*/
		0,                          /*tp_as_mapping*/
		0,                          /*tp_hash*/
		0,                          /*tp_call*/
		0,                          /*tp_str*/
		(getattrofunc)TroughPhysicalProcessHeat_getattro, /*tp_getattro*/
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
		TroughPhysicalProcessHeat_methods,      /*tp_methods*/
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


/* Function of no arguments returning new TroughPhysicalProcessHeat object */

static PyObject *
TroughPhysicalProcessHeat_new(PyObject *self, PyObject *args)
{
	TroughPhysicalProcessHeatObject *rv;
	rv = newTroughPhysicalProcessHeatObject(0);
	if (rv == NULL)
		return NULL;
	return (PyObject *)rv;
}

static PyObject *
TroughPhysicalProcessHeat_wrap(PyObject *self, PyObject *args)
{
	TroughPhysicalProcessHeatObject *rv;
	long long int ptr = 0;  // 64 bit arch
	if (!PyArg_ParseTuple(args, "L:wrap", &ptr)){
		PyErr_BadArgument();
		return NULL;
	}
	rv = newTroughPhysicalProcessHeatObject((void*)ptr);
	if (rv == NULL)
		return NULL;
	return (PyObject *)rv;
}

static PyObject *
TroughPhysicalProcessHeat_default(PyObject *self, PyObject *args)
{
	TroughPhysicalProcessHeatObject *rv;
	char* def = 0;
	if (!PyArg_ParseTuple(args, "s:default", &def)){
		PyErr_BadArgument();
		return NULL;
	}
	rv = newTroughPhysicalProcessHeatObject(0);
	if (rv == NULL)
		return NULL;

	PySAM_load_defaults((PyObject*)rv, rv->x_attr, rv->data_ptr, "TroughPhysicalProcessHeat", def);

	return (PyObject *)rv;
}

/* ---------- */


/* List of functions defined in the module */

static PyMethodDef TroughPhysicalProcessHeatModule_methods[] = {
		{"new",             TroughPhysicalProcessHeat_new,         METH_VARARGS,
				PyDoc_STR("new() -> TroughPhysicalProcessHeat")},
		{"default",             TroughPhysicalProcessHeat_default,         METH_VARARGS,
				PyDoc_STR("default(config) -> TroughPhysicalProcessHeat\n\nUse financial model-specific default attributes\n"
				"config options:\n\n- \"PhysicalTroughIPHLCOHCalculator\"\n- \"PhysicalTroughIPHNone\"")},
		{"wrap",             TroughPhysicalProcessHeat_wrap,         METH_VARARGS,
				PyDoc_STR("wrap(ssc_data_t) -> TroughPhysicalProcessHeat\n\nUse existing PySSC data\n\n.. warning::\n\n	Do not call PySSC.data_free on the ssc_data_t provided to ``wrap``")},
		{NULL,              NULL}           /* sentinel */
};

PyDoc_STRVAR(module_doc,
			 "Parabolic trough for industrial process heat applications");


static int
TroughPhysicalProcessHeatModule_exec(PyObject *m)
{
	/* Finalize the type object including setting type of the new type
	 * object; doing it here is required for portability, too. */

	if (PySAM_load_lib(m) < 0) goto fail;
	if (PySAM_init_error(m) < 0) goto fail;

	TroughPhysicalProcessHeat_Type.tp_dict = PyDict_New();
	if (!TroughPhysicalProcessHeat_Type.tp_dict) { goto fail; }

	/// Add the AdjustmentFactors type object to TroughPhysicalProcessHeat_Type
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
	PyDict_SetItemString(TroughPhysicalProcessHeat_Type.tp_dict,
						 "AdjustmentFactors",
						 (PyObject*)AdjustmentFactors_Type);
	Py_DECREF(&AdjustmentFactors_Type);
	Py_XDECREF(AdjustmentFactors_Type);

	/// Add the Weather type object to TroughPhysicalProcessHeat_Type
	if (PyType_Ready(&Weather_Type) < 0) { goto fail; }
	PyDict_SetItemString(TroughPhysicalProcessHeat_Type.tp_dict,
				"Weather",
				(PyObject*)&Weather_Type);
	Py_DECREF(&Weather_Type);

	/// Add the SolarField type object to TroughPhysicalProcessHeat_Type
	if (PyType_Ready(&SolarField_Type) < 0) { goto fail; }
	PyDict_SetItemString(TroughPhysicalProcessHeat_Type.tp_dict,
				"SolarField",
				(PyObject*)&SolarField_Type);
	Py_DECREF(&SolarField_Type);

	/// Add the Controller type object to TroughPhysicalProcessHeat_Type
	if (PyType_Ready(&Controller_Type) < 0) { goto fail; }
	PyDict_SetItemString(TroughPhysicalProcessHeat_Type.tp_dict,
				"Controller",
				(PyObject*)&Controller_Type);
	Py_DECREF(&Controller_Type);

	/// Add the SystemDesign type object to TroughPhysicalProcessHeat_Type
	if (PyType_Ready(&SystemDesign_Type) < 0) { goto fail; }
	PyDict_SetItemString(TroughPhysicalProcessHeat_Type.tp_dict,
				"SystemDesign",
				(PyObject*)&SystemDesign_Type);
	Py_DECREF(&SystemDesign_Type);

	/// Add the TES type object to TroughPhysicalProcessHeat_Type
	if (PyType_Ready(&TES_Type) < 0) { goto fail; }
	PyDict_SetItemString(TroughPhysicalProcessHeat_Type.tp_dict,
				"TES",
				(PyObject*)&TES_Type);
	Py_DECREF(&TES_Type);

	/// Add the TES2tank type object to TroughPhysicalProcessHeat_Type
	if (PyType_Ready(&TES2tank_Type) < 0) { goto fail; }
	PyDict_SetItemString(TroughPhysicalProcessHeat_Type.tp_dict,
				"TES2tank",
				(PyObject*)&TES2tank_Type);
	Py_DECREF(&TES2tank_Type);

	/// Add the Powerblock type object to TroughPhysicalProcessHeat_Type
	if (PyType_Ready(&Powerblock_Type) < 0) { goto fail; }
	PyDict_SetItemString(TroughPhysicalProcessHeat_Type.tp_dict,
				"Powerblock",
				(PyObject*)&Powerblock_Type);
	Py_DECREF(&Powerblock_Type);

	/// Add the Outputs type object to TroughPhysicalProcessHeat_Type
	if (PyType_Ready(&Outputs_Type) < 0) { goto fail; }
	PyDict_SetItemString(TroughPhysicalProcessHeat_Type.tp_dict,
				"Outputs",
				(PyObject*)&Outputs_Type);
	Py_DECREF(&Outputs_Type);

	/// Add the TroughPhysicalProcessHeat type object to the module
	if (PyType_Ready(&TroughPhysicalProcessHeat_Type) < 0) { goto fail; }
	PyModule_AddObject(m,
				"TroughPhysicalProcessHeat",
				(PyObject*)&TroughPhysicalProcessHeat_Type);

	return 0;
	fail:
	Py_XDECREF(m);
	return -1;
}

static struct PyModuleDef_Slot TroughPhysicalProcessHeatModule_slots[] = {
		{Py_mod_exec, TroughPhysicalProcessHeatModule_exec},
		{0, NULL},
};

static struct PyModuleDef TroughPhysicalProcessHeatModule = {
		PyModuleDef_HEAD_INIT,
		"TroughPhysicalProcessHeat",
		module_doc,
		0,
		TroughPhysicalProcessHeatModule_methods,
		TroughPhysicalProcessHeatModule_slots,
		NULL,
		NULL,
		NULL
};

/* Export function for the module */

PyMODINIT_FUNC
PyInit_TroughPhysicalProcessHeat(void)
{
	return PyModuleDef_Init(&TroughPhysicalProcessHeatModule);
}