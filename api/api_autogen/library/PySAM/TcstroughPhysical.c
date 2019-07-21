#include <Python.h>

#include <SAM_TcstroughPhysical.h>
#include <SAM_api.h>

#include "PySAM_utils.h"



	/*
	 * Weather Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_TcstroughPhysical   data_ptr;
} WeatherObject;

static PyTypeObject Weather_Type;

static PyObject *
Weather_new(SAM_TcstroughPhysical data_ptr)
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

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "TcstroughPhysical", "Weather")){
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
	return PySAM_double_getter(SAM_TcstroughPhysical_Weather_azimuth_nget, self->data_ptr);
}

static int
Weather_set_azimuth(WeatherObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_Weather_azimuth_nset, self->data_ptr);
}

static PyObject *
Weather_get_file_name(WeatherObject *self, void *closure)
{
	return PySAM_string_getter(SAM_TcstroughPhysical_Weather_file_name_sget, self->data_ptr);
}

static int
Weather_set_file_name(WeatherObject *self, PyObject *value, void *closure)
{
	return PySAM_string_setter(value, SAM_TcstroughPhysical_Weather_file_name_sset, self->data_ptr);
}

static PyObject *
Weather_get_tilt(WeatherObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Weather_tilt_nget, self->data_ptr);
}

static int
Weather_set_tilt(WeatherObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_Weather_tilt_nset, self->data_ptr);
}

static PyObject *
Weather_get_track_mode(WeatherObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Weather_track_mode_nget, self->data_ptr);
}

static int
Weather_set_track_mode(WeatherObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_Weather_track_mode_nset, self->data_ptr);
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
		"TcstroughPhysical.Weather",             /*tp_name*/
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
	 * Trough Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_TcstroughPhysical   data_ptr;
} TroughObject;

static PyTypeObject Trough_Type;

static PyObject *
Trough_new(SAM_TcstroughPhysical data_ptr)
{
	PyObject* new_obj = Trough_Type.tp_alloc(&Trough_Type,0);

	TroughObject* Trough_obj = (TroughObject*)new_obj;

	Trough_obj->data_ptr = data_ptr;

	return new_obj;
}

/* Trough methods */

static PyObject *
Trough_assign(TroughObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "TcstroughPhysical", "Trough")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
Trough_export(TroughObject *self, PyObject *args)
{
	PyTypeObject* tp = &Trough_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef Trough_methods[] = {
		{"assign",            (PyCFunction)Trough_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``Trough_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)Trough_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
Trough_get_system_capacity(TroughObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Trough_system_capacity_nget, self->data_ptr);
}

static int
Trough_set_system_capacity(TroughObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_Trough_system_capacity_nset, self->data_ptr);
}

static PyGetSetDef Trough_getset[] = {
{"system_capacity", (getter)Trough_get_system_capacity,(setter)Trough_set_system_capacity,
	PyDoc_STR("*float*: Nameplate capacity [kW]\n\n*Required*: True"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject Trough_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"TcstroughPhysical.Trough",             /*tp_name*/
		sizeof(TroughObject),          /*tp_basicsize*/
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
		Trough_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		Trough_getset,          /*tp_getset*/
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
	SAM_TcstroughPhysical   data_ptr;
} SolarFieldObject;

static PyTypeObject SolarField_Type;

static PyObject *
SolarField_new(SAM_TcstroughPhysical data_ptr)
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

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "TcstroughPhysical", "SolarField")){
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
	return PySAM_array_getter(SAM_TcstroughPhysical_SolarField_A_aperture_aget, self->data_ptr);
}

static int
SolarField_set_A_aperture(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_TcstroughPhysical_SolarField_A_aperture_aset, self->data_ptr);
}

static PyObject *
SolarField_get_AbsorberMaterial(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TcstroughPhysical_SolarField_AbsorberMaterial_mget, self->data_ptr);
}

static int
SolarField_set_AbsorberMaterial(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TcstroughPhysical_SolarField_AbsorberMaterial_mset, self->data_ptr);
}

static PyObject *
SolarField_get_AnnulusGas(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TcstroughPhysical_SolarField_AnnulusGas_mget, self->data_ptr);
}

static int
SolarField_set_AnnulusGas(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TcstroughPhysical_SolarField_AnnulusGas_mset, self->data_ptr);
}

static PyObject *
SolarField_get_Ave_Focal_Length(SolarFieldObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_SolarField_Ave_Focal_Length_aget, self->data_ptr);
}

static int
SolarField_set_Ave_Focal_Length(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_TcstroughPhysical_SolarField_Ave_Focal_Length_aset, self->data_ptr);
}

static PyObject *
SolarField_get_ColperSCA(SolarFieldObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_SolarField_ColperSCA_aget, self->data_ptr);
}

static int
SolarField_set_ColperSCA(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_TcstroughPhysical_SolarField_ColperSCA_aset, self->data_ptr);
}

static PyObject *
SolarField_get_D_2(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TcstroughPhysical_SolarField_D_2_mget, self->data_ptr);
}

static int
SolarField_set_D_2(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TcstroughPhysical_SolarField_D_2_mset, self->data_ptr);
}

static PyObject *
SolarField_get_D_3(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TcstroughPhysical_SolarField_D_3_mget, self->data_ptr);
}

static int
SolarField_set_D_3(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TcstroughPhysical_SolarField_D_3_mset, self->data_ptr);
}

static PyObject *
SolarField_get_D_4(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TcstroughPhysical_SolarField_D_4_mget, self->data_ptr);
}

static int
SolarField_set_D_4(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TcstroughPhysical_SolarField_D_4_mset, self->data_ptr);
}

static PyObject *
SolarField_get_D_5(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TcstroughPhysical_SolarField_D_5_mget, self->data_ptr);
}

static int
SolarField_set_D_5(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TcstroughPhysical_SolarField_D_5_mset, self->data_ptr);
}

static PyObject *
SolarField_get_D_cpnt(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TcstroughPhysical_SolarField_D_cpnt_mget, self->data_ptr);
}

static int
SolarField_set_D_cpnt(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TcstroughPhysical_SolarField_D_cpnt_mset, self->data_ptr);
}

static PyObject *
SolarField_get_D_p(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TcstroughPhysical_SolarField_D_p_mget, self->data_ptr);
}

static int
SolarField_set_D_p(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TcstroughPhysical_SolarField_D_p_mset, self->data_ptr);
}

static PyObject *
SolarField_get_Design_loss(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TcstroughPhysical_SolarField_Design_loss_mget, self->data_ptr);
}

static int
SolarField_set_Design_loss(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TcstroughPhysical_SolarField_Design_loss_mset, self->data_ptr);
}

static PyObject *
SolarField_get_Dirt_HCE(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TcstroughPhysical_SolarField_Dirt_HCE_mget, self->data_ptr);
}

static int
SolarField_set_Dirt_HCE(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TcstroughPhysical_SolarField_Dirt_HCE_mset, self->data_ptr);
}

static PyObject *
SolarField_get_Dirt_mirror(SolarFieldObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_SolarField_Dirt_mirror_aget, self->data_ptr);
}

static int
SolarField_set_Dirt_mirror(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_TcstroughPhysical_SolarField_Dirt_mirror_aset, self->data_ptr);
}

static PyObject *
SolarField_get_Distance_SCA(SolarFieldObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_SolarField_Distance_SCA_aget, self->data_ptr);
}

static int
SolarField_set_Distance_SCA(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_TcstroughPhysical_SolarField_Distance_SCA_aset, self->data_ptr);
}

static PyObject *
SolarField_get_EPSILON_4(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TcstroughPhysical_SolarField_EPSILON_4_mget, self->data_ptr);
}

static int
SolarField_set_EPSILON_4(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TcstroughPhysical_SolarField_EPSILON_4_mset, self->data_ptr);
}

static PyObject *
SolarField_get_EPSILON_5(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TcstroughPhysical_SolarField_EPSILON_5_mget, self->data_ptr);
}

static int
SolarField_set_EPSILON_5(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TcstroughPhysical_SolarField_EPSILON_5_mset, self->data_ptr);
}

static PyObject *
SolarField_get_Error(SolarFieldObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_SolarField_Error_aget, self->data_ptr);
}

static int
SolarField_set_Error(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_TcstroughPhysical_SolarField_Error_aset, self->data_ptr);
}

static PyObject *
SolarField_get_FieldConfig(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_SolarField_FieldConfig_nget, self->data_ptr);
}

static int
SolarField_set_FieldConfig(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_SolarField_FieldConfig_nset, self->data_ptr);
}

static PyObject *
SolarField_get_Flow_type(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TcstroughPhysical_SolarField_Flow_type_mget, self->data_ptr);
}

static int
SolarField_set_Flow_type(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TcstroughPhysical_SolarField_Flow_type_mset, self->data_ptr);
}

static PyObject *
SolarField_get_Fluid(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_SolarField_Fluid_nget, self->data_ptr);
}

static int
SolarField_set_Fluid(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_SolarField_Fluid_nset, self->data_ptr);
}

static PyObject *
SolarField_get_GeomEffects(SolarFieldObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_SolarField_GeomEffects_aget, self->data_ptr);
}

static int
SolarField_set_GeomEffects(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_TcstroughPhysical_SolarField_GeomEffects_aset, self->data_ptr);
}

static PyObject *
SolarField_get_GlazingIntactIn(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TcstroughPhysical_SolarField_GlazingIntactIn_mget, self->data_ptr);
}

static int
SolarField_set_GlazingIntactIn(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TcstroughPhysical_SolarField_GlazingIntactIn_mset, self->data_ptr);
}

static PyObject *
SolarField_get_HCE_FieldFrac(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TcstroughPhysical_SolarField_HCE_FieldFrac_mget, self->data_ptr);
}

static int
SolarField_set_HCE_FieldFrac(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TcstroughPhysical_SolarField_HCE_FieldFrac_mset, self->data_ptr);
}

static PyObject *
SolarField_get_HDR_rough(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_SolarField_HDR_rough_nget, self->data_ptr);
}

static int
SolarField_set_HDR_rough(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_SolarField_HDR_rough_nset, self->data_ptr);
}

static PyObject *
SolarField_get_IAM_matrix(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TcstroughPhysical_SolarField_IAM_matrix_mget, self->data_ptr);
}

static int
SolarField_set_IAM_matrix(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TcstroughPhysical_SolarField_IAM_matrix_mset, self->data_ptr);
}

static PyObject *
SolarField_get_I_bn_des(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_SolarField_I_bn_des_nget, self->data_ptr);
}

static int
SolarField_set_I_bn_des(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_SolarField_I_bn_des_nset, self->data_ptr);
}

static PyObject *
SolarField_get_K_cpnt(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TcstroughPhysical_SolarField_K_cpnt_mget, self->data_ptr);
}

static int
SolarField_set_K_cpnt(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TcstroughPhysical_SolarField_K_cpnt_mset, self->data_ptr);
}

static PyObject *
SolarField_get_L_SCA(SolarFieldObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_SolarField_L_SCA_aget, self->data_ptr);
}

static int
SolarField_set_L_SCA(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_TcstroughPhysical_SolarField_L_SCA_aset, self->data_ptr);
}

static PyObject *
SolarField_get_L_aperture(SolarFieldObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_SolarField_L_aperture_aget, self->data_ptr);
}

static int
SolarField_set_L_aperture(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_TcstroughPhysical_SolarField_L_aperture_aset, self->data_ptr);
}

static PyObject *
SolarField_get_L_cpnt(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TcstroughPhysical_SolarField_L_cpnt_mget, self->data_ptr);
}

static int
SolarField_set_L_cpnt(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TcstroughPhysical_SolarField_L_cpnt_mset, self->data_ptr);
}

static PyObject *
SolarField_get_L_rnr_pb(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_SolarField_L_rnr_pb_nget, self->data_ptr);
}

static int
SolarField_set_L_rnr_pb(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_SolarField_L_rnr_pb_nset, self->data_ptr);
}

static PyObject *
SolarField_get_L_rnr_per_xpan(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_SolarField_L_rnr_per_xpan_nget, self->data_ptr);
}

static int
SolarField_set_L_rnr_per_xpan(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_SolarField_L_rnr_per_xpan_nset, self->data_ptr);
}

static PyObject *
SolarField_get_L_xpan_hdr(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_SolarField_L_xpan_hdr_nget, self->data_ptr);
}

static int
SolarField_set_L_xpan_hdr(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_SolarField_L_xpan_hdr_nset, self->data_ptr);
}

static PyObject *
SolarField_get_L_xpan_rnr(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_SolarField_L_xpan_rnr_nget, self->data_ptr);
}

static int
SolarField_set_L_xpan_rnr(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_SolarField_L_xpan_rnr_nset, self->data_ptr);
}

static PyObject *
SolarField_get_Min_rnr_xpans(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_SolarField_Min_rnr_xpans_nget, self->data_ptr);
}

static int
SolarField_set_Min_rnr_xpans(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_SolarField_Min_rnr_xpans_nset, self->data_ptr);
}

static PyObject *
SolarField_get_N_hdr_per_xpan(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_SolarField_N_hdr_per_xpan_nget, self->data_ptr);
}

static int
SolarField_set_N_hdr_per_xpan(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_SolarField_N_hdr_per_xpan_nset, self->data_ptr);
}

static PyObject *
SolarField_get_N_max_hdr_diams(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_SolarField_N_max_hdr_diams_nget, self->data_ptr);
}

static int
SolarField_set_N_max_hdr_diams(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_SolarField_N_max_hdr_diams_nset, self->data_ptr);
}

static PyObject *
SolarField_get_P_a(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TcstroughPhysical_SolarField_P_a_mget, self->data_ptr);
}

static int
SolarField_set_P_a(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TcstroughPhysical_SolarField_P_a_mset, self->data_ptr);
}

static PyObject *
SolarField_get_P_ref(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_SolarField_P_ref_nget, self->data_ptr);
}

static int
SolarField_set_P_ref(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_SolarField_P_ref_nset, self->data_ptr);
}

static PyObject *
SolarField_get_Pipe_hl_coef(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_SolarField_Pipe_hl_coef_nget, self->data_ptr);
}

static int
SolarField_set_Pipe_hl_coef(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_SolarField_Pipe_hl_coef_nset, self->data_ptr);
}

static PyObject *
SolarField_get_Rho_mirror_clean(SolarFieldObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_SolarField_Rho_mirror_clean_aget, self->data_ptr);
}

static int
SolarField_set_Rho_mirror_clean(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_TcstroughPhysical_SolarField_Rho_mirror_clean_aset, self->data_ptr);
}

static PyObject *
SolarField_get_Rough(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TcstroughPhysical_SolarField_Rough_mget, self->data_ptr);
}

static int
SolarField_set_Rough(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TcstroughPhysical_SolarField_Rough_mset, self->data_ptr);
}

static PyObject *
SolarField_get_Row_Distance(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_SolarField_Row_Distance_nget, self->data_ptr);
}

static int
SolarField_set_Row_Distance(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_SolarField_Row_Distance_nset, self->data_ptr);
}

static PyObject *
SolarField_get_SCADefocusArray(SolarFieldObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_SolarField_SCADefocusArray_aget, self->data_ptr);
}

static int
SolarField_set_SCADefocusArray(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_TcstroughPhysical_SolarField_SCADefocusArray_aset, self->data_ptr);
}

static PyObject *
SolarField_get_SCAInfoArray(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TcstroughPhysical_SolarField_SCAInfoArray_mget, self->data_ptr);
}

static int
SolarField_set_SCAInfoArray(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TcstroughPhysical_SolarField_SCAInfoArray_mset, self->data_ptr);
}

static PyObject *
SolarField_get_SCA_drives_elec(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_SolarField_SCA_drives_elec_nget, self->data_ptr);
}

static int
SolarField_set_SCA_drives_elec(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_SolarField_SCA_drives_elec_nset, self->data_ptr);
}

static PyObject *
SolarField_get_Shadowing(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TcstroughPhysical_SolarField_Shadowing_mget, self->data_ptr);
}

static int
SolarField_set_Shadowing(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TcstroughPhysical_SolarField_Shadowing_mset, self->data_ptr);
}

static PyObject *
SolarField_get_T_fp(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_SolarField_T_fp_nget, self->data_ptr);
}

static int
SolarField_set_T_fp(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_SolarField_T_fp_nset, self->data_ptr);
}

static PyObject *
SolarField_get_T_loop_in_des(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_SolarField_T_loop_in_des_nget, self->data_ptr);
}

static int
SolarField_set_T_loop_in_des(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_SolarField_T_loop_in_des_nset, self->data_ptr);
}

static PyObject *
SolarField_get_T_loop_out(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_SolarField_T_loop_out_nget, self->data_ptr);
}

static int
SolarField_set_T_loop_out(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_SolarField_T_loop_out_nset, self->data_ptr);
}

static PyObject *
SolarField_get_T_startup(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_SolarField_T_startup_nget, self->data_ptr);
}

static int
SolarField_set_T_startup(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_SolarField_T_startup_nset, self->data_ptr);
}

static PyObject *
SolarField_get_Tau_envelope(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TcstroughPhysical_SolarField_Tau_envelope_mget, self->data_ptr);
}

static int
SolarField_set_Tau_envelope(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TcstroughPhysical_SolarField_Tau_envelope_mset, self->data_ptr);
}

static PyObject *
SolarField_get_TrackingError(SolarFieldObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_SolarField_TrackingError_aget, self->data_ptr);
}

static int
SolarField_set_TrackingError(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_TcstroughPhysical_SolarField_TrackingError_aset, self->data_ptr);
}

static PyObject *
SolarField_get_Type_cpnt(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TcstroughPhysical_SolarField_Type_cpnt_mget, self->data_ptr);
}

static int
SolarField_set_Type_cpnt(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TcstroughPhysical_SolarField_Type_cpnt_mset, self->data_ptr);
}

static PyObject *
SolarField_get_V_hdr_cold_max(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_SolarField_V_hdr_cold_max_nget, self->data_ptr);
}

static int
SolarField_set_V_hdr_cold_max(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_SolarField_V_hdr_cold_max_nset, self->data_ptr);
}

static PyObject *
SolarField_get_V_hdr_cold_min(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_SolarField_V_hdr_cold_min_nget, self->data_ptr);
}

static int
SolarField_set_V_hdr_cold_min(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_SolarField_V_hdr_cold_min_nset, self->data_ptr);
}

static PyObject *
SolarField_get_V_hdr_hot_max(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_SolarField_V_hdr_hot_max_nget, self->data_ptr);
}

static int
SolarField_set_V_hdr_hot_max(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_SolarField_V_hdr_hot_max_nset, self->data_ptr);
}

static PyObject *
SolarField_get_V_hdr_hot_min(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_SolarField_V_hdr_hot_min_nget, self->data_ptr);
}

static int
SolarField_set_V_hdr_hot_min(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_SolarField_V_hdr_hot_min_nset, self->data_ptr);
}

static PyObject *
SolarField_get_W_aperture(SolarFieldObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_SolarField_W_aperture_aget, self->data_ptr);
}

static int
SolarField_set_W_aperture(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_TcstroughPhysical_SolarField_W_aperture_aset, self->data_ptr);
}

static PyObject *
SolarField_get_accept_init(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_SolarField_accept_init_nget, self->data_ptr);
}

static int
SolarField_set_accept_init(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_SolarField_accept_init_nset, self->data_ptr);
}

static PyObject *
SolarField_get_accept_loc(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_SolarField_accept_loc_nget, self->data_ptr);
}

static int
SolarField_set_accept_loc(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_SolarField_accept_loc_nset, self->data_ptr);
}

static PyObject *
SolarField_get_accept_mode(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_SolarField_accept_mode_nget, self->data_ptr);
}

static int
SolarField_set_accept_mode(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_SolarField_accept_mode_nset, self->data_ptr);
}

static PyObject *
SolarField_get_alpha_abs(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TcstroughPhysical_SolarField_alpha_abs_mget, self->data_ptr);
}

static int
SolarField_set_alpha_abs(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TcstroughPhysical_SolarField_alpha_abs_mset, self->data_ptr);
}

static PyObject *
SolarField_get_alpha_env(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TcstroughPhysical_SolarField_alpha_env_mget, self->data_ptr);
}

static int
SolarField_set_alpha_env(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TcstroughPhysical_SolarField_alpha_env_mset, self->data_ptr);
}

static PyObject *
SolarField_get_calc_design_pipe_vals(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_SolarField_calc_design_pipe_vals_nget, self->data_ptr);
}

static int
SolarField_set_calc_design_pipe_vals(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_SolarField_calc_design_pipe_vals_nset, self->data_ptr);
}

static PyObject *
SolarField_get_custom_sf_pipe_sizes(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_SolarField_custom_sf_pipe_sizes_nget, self->data_ptr);
}

static int
SolarField_set_custom_sf_pipe_sizes(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_SolarField_custom_sf_pipe_sizes_nset, self->data_ptr);
}

static PyObject *
SolarField_get_epsilon_3_11(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TcstroughPhysical_SolarField_epsilon_3_11_mget, self->data_ptr);
}

static int
SolarField_set_epsilon_3_11(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TcstroughPhysical_SolarField_epsilon_3_11_mset, self->data_ptr);
}

static PyObject *
SolarField_get_epsilon_3_12(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TcstroughPhysical_SolarField_epsilon_3_12_mget, self->data_ptr);
}

static int
SolarField_set_epsilon_3_12(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TcstroughPhysical_SolarField_epsilon_3_12_mset, self->data_ptr);
}

static PyObject *
SolarField_get_epsilon_3_13(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TcstroughPhysical_SolarField_epsilon_3_13_mget, self->data_ptr);
}

static int
SolarField_set_epsilon_3_13(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TcstroughPhysical_SolarField_epsilon_3_13_mset, self->data_ptr);
}

static PyObject *
SolarField_get_epsilon_3_14(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TcstroughPhysical_SolarField_epsilon_3_14_mget, self->data_ptr);
}

static int
SolarField_set_epsilon_3_14(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TcstroughPhysical_SolarField_epsilon_3_14_mset, self->data_ptr);
}

static PyObject *
SolarField_get_epsilon_3_21(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TcstroughPhysical_SolarField_epsilon_3_21_mget, self->data_ptr);
}

static int
SolarField_set_epsilon_3_21(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TcstroughPhysical_SolarField_epsilon_3_21_mset, self->data_ptr);
}

static PyObject *
SolarField_get_epsilon_3_22(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TcstroughPhysical_SolarField_epsilon_3_22_mget, self->data_ptr);
}

static int
SolarField_set_epsilon_3_22(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TcstroughPhysical_SolarField_epsilon_3_22_mset, self->data_ptr);
}

static PyObject *
SolarField_get_epsilon_3_23(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TcstroughPhysical_SolarField_epsilon_3_23_mget, self->data_ptr);
}

static int
SolarField_set_epsilon_3_23(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TcstroughPhysical_SolarField_epsilon_3_23_mset, self->data_ptr);
}

static PyObject *
SolarField_get_epsilon_3_24(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TcstroughPhysical_SolarField_epsilon_3_24_mget, self->data_ptr);
}

static int
SolarField_set_epsilon_3_24(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TcstroughPhysical_SolarField_epsilon_3_24_mset, self->data_ptr);
}

static PyObject *
SolarField_get_epsilon_3_31(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TcstroughPhysical_SolarField_epsilon_3_31_mget, self->data_ptr);
}

static int
SolarField_set_epsilon_3_31(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TcstroughPhysical_SolarField_epsilon_3_31_mset, self->data_ptr);
}

static PyObject *
SolarField_get_epsilon_3_32(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TcstroughPhysical_SolarField_epsilon_3_32_mget, self->data_ptr);
}

static int
SolarField_set_epsilon_3_32(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TcstroughPhysical_SolarField_epsilon_3_32_mset, self->data_ptr);
}

static PyObject *
SolarField_get_epsilon_3_33(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TcstroughPhysical_SolarField_epsilon_3_33_mget, self->data_ptr);
}

static int
SolarField_set_epsilon_3_33(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TcstroughPhysical_SolarField_epsilon_3_33_mset, self->data_ptr);
}

static PyObject *
SolarField_get_epsilon_3_34(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TcstroughPhysical_SolarField_epsilon_3_34_mget, self->data_ptr);
}

static int
SolarField_set_epsilon_3_34(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TcstroughPhysical_SolarField_epsilon_3_34_mset, self->data_ptr);
}

static PyObject *
SolarField_get_epsilon_3_41(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TcstroughPhysical_SolarField_epsilon_3_41_mget, self->data_ptr);
}

static int
SolarField_set_epsilon_3_41(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TcstroughPhysical_SolarField_epsilon_3_41_mset, self->data_ptr);
}

static PyObject *
SolarField_get_epsilon_3_42(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TcstroughPhysical_SolarField_epsilon_3_42_mget, self->data_ptr);
}

static int
SolarField_set_epsilon_3_42(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TcstroughPhysical_SolarField_epsilon_3_42_mset, self->data_ptr);
}

static PyObject *
SolarField_get_epsilon_3_43(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TcstroughPhysical_SolarField_epsilon_3_43_mget, self->data_ptr);
}

static int
SolarField_set_epsilon_3_43(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TcstroughPhysical_SolarField_epsilon_3_43_mset, self->data_ptr);
}

static PyObject *
SolarField_get_epsilon_3_44(SolarFieldObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TcstroughPhysical_SolarField_epsilon_3_44_mget, self->data_ptr);
}

static int
SolarField_set_epsilon_3_44(SolarFieldObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TcstroughPhysical_SolarField_epsilon_3_44_mset, self->data_ptr);
}

static PyObject *
SolarField_get_eta_pump(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_SolarField_eta_pump_nget, self->data_ptr);
}

static int
SolarField_set_eta_pump(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_SolarField_eta_pump_nset, self->data_ptr);
}

static PyObject *
SolarField_get_fthrctrl(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_SolarField_fthrctrl_nget, self->data_ptr);
}

static int
SolarField_set_fthrctrl(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_SolarField_fthrctrl_nset, self->data_ptr);
}

static PyObject *
SolarField_get_fthrok(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_SolarField_fthrok_nget, self->data_ptr);
}

static int
SolarField_set_fthrok(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_SolarField_fthrok_nset, self->data_ptr);
}

static PyObject *
SolarField_get_m_dot_htfmax(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_SolarField_m_dot_htfmax_nget, self->data_ptr);
}

static int
SolarField_set_m_dot_htfmax(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_SolarField_m_dot_htfmax_nset, self->data_ptr);
}

static PyObject *
SolarField_get_m_dot_htfmin(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_SolarField_m_dot_htfmin_nget, self->data_ptr);
}

static int
SolarField_set_m_dot_htfmin(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_SolarField_m_dot_htfmin_nset, self->data_ptr);
}

static PyObject *
SolarField_get_mc_bal_cold(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_SolarField_mc_bal_cold_nget, self->data_ptr);
}

static int
SolarField_set_mc_bal_cold(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_SolarField_mc_bal_cold_nset, self->data_ptr);
}

static PyObject *
SolarField_get_mc_bal_hot(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_SolarField_mc_bal_hot_nget, self->data_ptr);
}

static int
SolarField_set_mc_bal_hot(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_SolarField_mc_bal_hot_nset, self->data_ptr);
}

static PyObject *
SolarField_get_mc_bal_sca(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_SolarField_mc_bal_sca_nget, self->data_ptr);
}

static int
SolarField_set_mc_bal_sca(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_SolarField_mc_bal_sca_nset, self->data_ptr);
}

static PyObject *
SolarField_get_nColt(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_SolarField_nColt_nget, self->data_ptr);
}

static int
SolarField_set_nColt(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_SolarField_nColt_nset, self->data_ptr);
}

static PyObject *
SolarField_get_nHCEVar(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_SolarField_nHCEVar_nget, self->data_ptr);
}

static int
SolarField_set_nHCEVar(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_SolarField_nHCEVar_nset, self->data_ptr);
}

static PyObject *
SolarField_get_nHCEt(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_SolarField_nHCEt_nget, self->data_ptr);
}

static int
SolarField_set_nHCEt(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_SolarField_nHCEt_nset, self->data_ptr);
}

static PyObject *
SolarField_get_nLoops(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_SolarField_nLoops_nget, self->data_ptr);
}

static int
SolarField_set_nLoops(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_SolarField_nLoops_nset, self->data_ptr);
}

static PyObject *
SolarField_get_nSCA(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_SolarField_nSCA_nget, self->data_ptr);
}

static int
SolarField_set_nSCA(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_SolarField_nSCA_nset, self->data_ptr);
}

static PyObject *
SolarField_get_northsouth_field_sep(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_SolarField_northsouth_field_sep_nget, self->data_ptr);
}

static int
SolarField_set_northsouth_field_sep(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_SolarField_northsouth_field_sep_nset, self->data_ptr);
}

static PyObject *
SolarField_get_offset_xpan_hdr(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_SolarField_offset_xpan_hdr_nget, self->data_ptr);
}

static int
SolarField_set_offset_xpan_hdr(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_SolarField_offset_xpan_hdr_nset, self->data_ptr);
}

static PyObject *
SolarField_get_sf_hdr_diams(SolarFieldObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_SolarField_sf_hdr_diams_aget, self->data_ptr);
}

static int
SolarField_set_sf_hdr_diams(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_TcstroughPhysical_SolarField_sf_hdr_diams_aset, self->data_ptr);
}

static PyObject *
SolarField_get_sf_hdr_lengths(SolarFieldObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_SolarField_sf_hdr_lengths_aget, self->data_ptr);
}

static int
SolarField_set_sf_hdr_lengths(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_TcstroughPhysical_SolarField_sf_hdr_lengths_aset, self->data_ptr);
}

static PyObject *
SolarField_get_sf_hdr_wallthicks(SolarFieldObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_SolarField_sf_hdr_wallthicks_aget, self->data_ptr);
}

static int
SolarField_set_sf_hdr_wallthicks(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_TcstroughPhysical_SolarField_sf_hdr_wallthicks_aset, self->data_ptr);
}

static PyObject *
SolarField_get_sf_rnr_diams(SolarFieldObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_SolarField_sf_rnr_diams_aget, self->data_ptr);
}

static int
SolarField_set_sf_rnr_diams(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_TcstroughPhysical_SolarField_sf_rnr_diams_aset, self->data_ptr);
}

static PyObject *
SolarField_get_sf_rnr_lengths(SolarFieldObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_SolarField_sf_rnr_lengths_aget, self->data_ptr);
}

static int
SolarField_set_sf_rnr_lengths(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_TcstroughPhysical_SolarField_sf_rnr_lengths_aset, self->data_ptr);
}

static PyObject *
SolarField_get_sf_rnr_wallthicks(SolarFieldObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_SolarField_sf_rnr_wallthicks_aget, self->data_ptr);
}

static int
SolarField_set_sf_rnr_wallthicks(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_TcstroughPhysical_SolarField_sf_rnr_wallthicks_aset, self->data_ptr);
}

static PyObject *
SolarField_get_solar_mult(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_SolarField_solar_mult_nget, self->data_ptr);
}

static int
SolarField_set_solar_mult(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_SolarField_solar_mult_nset, self->data_ptr);
}

static PyObject *
SolarField_get_theta_dep(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_SolarField_theta_dep_nget, self->data_ptr);
}

static int
SolarField_set_theta_dep(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_SolarField_theta_dep_nset, self->data_ptr);
}

static PyObject *
SolarField_get_theta_stow(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_SolarField_theta_stow_nget, self->data_ptr);
}

static int
SolarField_set_theta_stow(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_SolarField_theta_stow_nset, self->data_ptr);
}

static PyObject *
SolarField_get_washing_frequency(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_SolarField_washing_frequency_nget, self->data_ptr);
}

static int
SolarField_set_washing_frequency(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_SolarField_washing_frequency_nset, self->data_ptr);
}

static PyObject *
SolarField_get_water_usage_per_wash(SolarFieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_SolarField_water_usage_per_wash_nget, self->data_ptr);
}

static int
SolarField_set_water_usage_per_wash(SolarFieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_SolarField_water_usage_per_wash_nset, self->data_ptr);
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
	PyDoc_STR("*float*: Solar irradiation at design [W/m2]\n\n*Required*: True"),
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
{"L_rnr_pb", (getter)SolarField_get_L_rnr_pb,(setter)SolarField_set_L_rnr_pb,
	PyDoc_STR("*float*: Length of runner pipe in power block [m]\n\n*Required*: True"),
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
{"P_ref", (getter)SolarField_get_P_ref,(setter)SolarField_set_P_ref,
	PyDoc_STR("*float*: Rated plant capacity [MWe]\n\n*Required*: True"),
 	NULL},
{"Pipe_hl_coef", (getter)SolarField_get_Pipe_hl_coef,(setter)SolarField_set_Pipe_hl_coef,
	PyDoc_STR("*float*: Loss coefficient from the header, runner pipe, and non-HCE piping [W/m2-K]\n\n*Required*: True"),
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
	PyDoc_STR("*float*: Tracking power, in Watts per SCA drive [W/SCA]\n\n*Required*: True"),
 	NULL},
{"Shadowing", (getter)SolarField_get_Shadowing,(setter)SolarField_set_Shadowing,
	PyDoc_STR("*sequence[sequence]*: Receiver bellows shadowing loss factor [none]\n\n*Required*: True"),
 	NULL},
{"T_fp", (getter)SolarField_get_T_fp,(setter)SolarField_set_T_fp,
	PyDoc_STR("*float*: Freeze protection temperature (heat trace activation temperature) [C]\n\n*Required*: True"),
 	NULL},
{"T_loop_in_des", (getter)SolarField_get_T_loop_in_des,(setter)SolarField_set_T_loop_in_des,
	PyDoc_STR("*float*: Design loop inlet temperature [C]\n\n*Required*: True"),
 	NULL},
{"T_loop_out", (getter)SolarField_get_T_loop_out,(setter)SolarField_set_T_loop_out,
	PyDoc_STR("*float*: Target loop outlet temperature [C]\n\n*Required*: True"),
 	NULL},
{"T_startup", (getter)SolarField_get_T_startup,(setter)SolarField_set_T_startup,
	PyDoc_STR("*float*: Required temperature of the system before the power block can be switched on [C]\n\n*Required*: True"),
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
{"V_hdr_cold_max", (getter)SolarField_get_V_hdr_cold_max,(setter)SolarField_set_V_hdr_cold_max,
	PyDoc_STR("*float*: Maximum HTF velocity in the cold headers at design [m/s]\n\n*Required*: True"),
 	NULL},
{"V_hdr_cold_min", (getter)SolarField_get_V_hdr_cold_min,(setter)SolarField_set_V_hdr_cold_min,
	PyDoc_STR("*float*: Minimum HTF velocity in the cold headers at design [m/s]\n\n*Required*: True"),
 	NULL},
{"V_hdr_hot_max", (getter)SolarField_get_V_hdr_hot_max,(setter)SolarField_set_V_hdr_hot_max,
	PyDoc_STR("*float*: Maximum HTF velocity in the hot headers at design [m/s]\n\n*Required*: True"),
 	NULL},
{"V_hdr_hot_min", (getter)SolarField_get_V_hdr_hot_min,(setter)SolarField_set_V_hdr_hot_min,
	PyDoc_STR("*float*: Minimum HTF velocity in the hot headers at design [m/s]\n\n*Required*: True"),
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
{"fthrctrl", (getter)SolarField_get_fthrctrl,(setter)SolarField_set_fthrctrl,
	PyDoc_STR("*float*: Defocusing strategy [none]\n\n*Required*: True"),
 	NULL},
{"fthrok", (getter)SolarField_get_fthrok,(setter)SolarField_set_fthrok,
	PyDoc_STR("*float*: Flag to allow partial defocusing of the collectors\n\n*Constraints*: INTEGER\n\n*Required*: True"),
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
	PyDoc_STR("*sequence*: Custom header diameters [m]\n\n*Required*: True"),
 	NULL},
{"sf_hdr_lengths", (getter)SolarField_get_sf_hdr_lengths,(setter)SolarField_set_sf_hdr_lengths,
	PyDoc_STR("*sequence*: Custom header lengths [m]\n\n*Required*: True"),
 	NULL},
{"sf_hdr_wallthicks", (getter)SolarField_get_sf_hdr_wallthicks,(setter)SolarField_set_sf_hdr_wallthicks,
	PyDoc_STR("*sequence*: Custom header wall thicknesses [m]\n\n*Required*: True"),
 	NULL},
{"sf_rnr_diams", (getter)SolarField_get_sf_rnr_diams,(setter)SolarField_set_sf_rnr_diams,
	PyDoc_STR("*sequence*: Custom runner diameters [m]\n\n*Required*: True"),
 	NULL},
{"sf_rnr_lengths", (getter)SolarField_get_sf_rnr_lengths,(setter)SolarField_set_sf_rnr_lengths,
	PyDoc_STR("*sequence*: Custom runner lengths [m]\n\n*Required*: True"),
 	NULL},
{"sf_rnr_wallthicks", (getter)SolarField_get_sf_rnr_wallthicks,(setter)SolarField_set_sf_rnr_wallthicks,
	PyDoc_STR("*sequence*: Custom runner wall thicknesses [m]\n\n*Required*: True"),
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
	PyDoc_STR("*float*: Mirror washing frequency [none]\n\n*Required*: True"),
 	NULL},
{"water_usage_per_wash", (getter)SolarField_get_water_usage_per_wash,(setter)SolarField_set_water_usage_per_wash,
	PyDoc_STR("*float*: Water usage per wash [L/m2_aper]\n\n*Required*: True"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject SolarField_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"TcstroughPhysical.SolarField",             /*tp_name*/
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
	SAM_TcstroughPhysical   data_ptr;
} ControllerObject;

static PyTypeObject Controller_Type;

static PyObject *
Controller_new(SAM_TcstroughPhysical data_ptr)
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

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "TcstroughPhysical", "Controller")){
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
Controller_get_DP_SGS(ControllerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Controller_DP_SGS_nget, self->data_ptr);
}

static int
Controller_set_DP_SGS(ControllerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_Controller_DP_SGS_nset, self->data_ptr);
}

static PyObject *
Controller_get_HDR_rough(ControllerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Controller_HDR_rough_nget, self->data_ptr);
}

static int
Controller_set_HDR_rough(ControllerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_Controller_HDR_rough_nset, self->data_ptr);
}

static PyObject *
Controller_get_T_set_aux(ControllerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Controller_T_set_aux_nget, self->data_ptr);
}

static int
Controller_set_T_set_aux(ControllerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_Controller_T_set_aux_nset, self->data_ptr);
}

static PyObject *
Controller_get_T_tank_cold_ini(ControllerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Controller_T_tank_cold_ini_nget, self->data_ptr);
}

static int
Controller_set_T_tank_cold_ini(ControllerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_Controller_T_tank_cold_ini_nset, self->data_ptr);
}

static PyObject *
Controller_get_T_tank_hot_inlet_min(ControllerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Controller_T_tank_hot_inlet_min_nget, self->data_ptr);
}

static int
Controller_set_T_tank_hot_inlet_min(ControllerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_Controller_T_tank_hot_inlet_min_nset, self->data_ptr);
}

static PyObject *
Controller_get_V_tank_hot_ini(ControllerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Controller_V_tank_hot_ini_nget, self->data_ptr);
}

static int
Controller_set_V_tank_hot_ini(ControllerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_Controller_V_tank_hot_ini_nset, self->data_ptr);
}

static PyObject *
Controller_get_V_tes_des(ControllerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Controller_V_tes_des_nget, self->data_ptr);
}

static int
Controller_set_V_tes_des(ControllerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_Controller_V_tes_des_nset, self->data_ptr);
}

static PyObject *
Controller_get_W_pb_design(ControllerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Controller_W_pb_design_nget, self->data_ptr);
}

static int
Controller_set_W_pb_design(ControllerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_Controller_W_pb_design_nset, self->data_ptr);
}

static PyObject *
Controller_get_aux_array(ControllerObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Controller_aux_array_aget, self->data_ptr);
}

static int
Controller_set_aux_array(ControllerObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_TcstroughPhysical_Controller_aux_array_aset, self->data_ptr);
}

static PyObject *
Controller_get_bop_array(ControllerObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Controller_bop_array_aget, self->data_ptr);
}

static int
Controller_set_bop_array(ControllerObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_TcstroughPhysical_Controller_bop_array_aset, self->data_ptr);
}

static PyObject *
Controller_get_cold_tank_Thtr(ControllerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Controller_cold_tank_Thtr_nget, self->data_ptr);
}

static int
Controller_set_cold_tank_Thtr(ControllerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_Controller_cold_tank_Thtr_nset, self->data_ptr);
}

static PyObject *
Controller_get_custom_sgs_pipe_sizes(ControllerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Controller_custom_sgs_pipe_sizes_nget, self->data_ptr);
}

static int
Controller_set_custom_sgs_pipe_sizes(ControllerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_Controller_custom_sgs_pipe_sizes_nset, self->data_ptr);
}

static PyObject *
Controller_get_custom_tes_p_loss(ControllerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Controller_custom_tes_p_loss_nget, self->data_ptr);
}

static int
Controller_set_custom_tes_p_loss(ControllerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_Controller_custom_tes_p_loss_nset, self->data_ptr);
}

static PyObject *
Controller_get_cycle_cutoff_frac(ControllerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Controller_cycle_cutoff_frac_nget, self->data_ptr);
}

static int
Controller_set_cycle_cutoff_frac(ControllerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_Controller_cycle_cutoff_frac_nset, self->data_ptr);
}

static PyObject *
Controller_get_cycle_max_frac(ControllerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Controller_cycle_max_frac_nget, self->data_ptr);
}

static int
Controller_set_cycle_max_frac(ControllerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_Controller_cycle_max_frac_nset, self->data_ptr);
}

static PyObject *
Controller_get_dt_cold(ControllerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Controller_dt_cold_nget, self->data_ptr);
}

static int
Controller_set_dt_cold(ControllerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_Controller_dt_cold_nset, self->data_ptr);
}

static PyObject *
Controller_get_dt_hot(ControllerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Controller_dt_hot_nget, self->data_ptr);
}

static int
Controller_set_dt_hot(ControllerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_Controller_dt_hot_nset, self->data_ptr);
}

static PyObject *
Controller_get_eta_pump(ControllerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Controller_eta_pump_nget, self->data_ptr);
}

static int
Controller_set_eta_pump(ControllerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_Controller_eta_pump_nset, self->data_ptr);
}

static PyObject *
Controller_get_f_tc_cold(ControllerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Controller_f_tc_cold_nget, self->data_ptr);
}

static int
Controller_set_f_tc_cold(ControllerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_Controller_f_tc_cold_nset, self->data_ptr);
}

static PyObject *
Controller_get_ffrac(ControllerObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Controller_ffrac_aget, self->data_ptr);
}

static int
Controller_set_ffrac(ControllerObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_TcstroughPhysical_Controller_ffrac_aset, self->data_ptr);
}

static PyObject *
Controller_get_field_fl_props(ControllerObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TcstroughPhysical_Controller_field_fl_props_mget, self->data_ptr);
}

static int
Controller_set_field_fl_props(ControllerObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TcstroughPhysical_Controller_field_fl_props_mset, self->data_ptr);
}

static PyObject *
Controller_get_fossil_mode(ControllerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Controller_fossil_mode_nget, self->data_ptr);
}

static int
Controller_set_fossil_mode(ControllerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_Controller_fossil_mode_nset, self->data_ptr);
}

static PyObject *
Controller_get_h_tank(ControllerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Controller_h_tank_nget, self->data_ptr);
}

static int
Controller_set_h_tank(ControllerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_Controller_h_tank_nset, self->data_ptr);
}

static PyObject *
Controller_get_h_tank_min(ControllerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Controller_h_tank_min_nget, self->data_ptr);
}

static int
Controller_set_h_tank_min(ControllerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_Controller_h_tank_min_nset, self->data_ptr);
}

static PyObject *
Controller_get_has_hot_tank_bypass(ControllerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Controller_has_hot_tank_bypass_nget, self->data_ptr);
}

static int
Controller_set_has_hot_tank_bypass(ControllerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_Controller_has_hot_tank_bypass_nset, self->data_ptr);
}

static PyObject *
Controller_get_hot_tank_Thtr(ControllerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Controller_hot_tank_Thtr_nget, self->data_ptr);
}

static int
Controller_set_hot_tank_Thtr(ControllerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_Controller_hot_tank_Thtr_nset, self->data_ptr);
}

static PyObject *
Controller_get_hx_config(ControllerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Controller_hx_config_nget, self->data_ptr);
}

static int
Controller_set_hx_config(ControllerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_Controller_hx_config_nset, self->data_ptr);
}

static PyObject *
Controller_get_is_hx(ControllerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Controller_is_hx_nget, self->data_ptr);
}

static int
Controller_set_is_hx(ControllerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_Controller_is_hx_nset, self->data_ptr);
}

static PyObject *
Controller_get_k_tes_loss_coeffs(ControllerObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Controller_k_tes_loss_coeffs_aget, self->data_ptr);
}

static int
Controller_set_k_tes_loss_coeffs(ControllerObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_TcstroughPhysical_Controller_k_tes_loss_coeffs_aset, self->data_ptr);
}

static PyObject *
Controller_get_nodes(ControllerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Controller_nodes_nget, self->data_ptr);
}

static int
Controller_set_nodes(ControllerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_Controller_nodes_nset, self->data_ptr);
}

static PyObject *
Controller_get_pb_fixed_par(ControllerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Controller_pb_fixed_par_nget, self->data_ptr);
}

static int
Controller_set_pb_fixed_par(ControllerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_Controller_pb_fixed_par_nset, self->data_ptr);
}

static PyObject *
Controller_get_pb_pump_coef(ControllerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Controller_pb_pump_coef_nget, self->data_ptr);
}

static int
Controller_set_pb_pump_coef(ControllerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_Controller_pb_pump_coef_nset, self->data_ptr);
}

static PyObject *
Controller_get_q_max_aux(ControllerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Controller_q_max_aux_nget, self->data_ptr);
}

static int
Controller_set_q_max_aux(ControllerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_Controller_q_max_aux_nset, self->data_ptr);
}

static PyObject *
Controller_get_q_pb_design(ControllerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Controller_q_pb_design_nget, self->data_ptr);
}

static int
Controller_set_q_pb_design(ControllerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_Controller_q_pb_design_nset, self->data_ptr);
}

static PyObject *
Controller_get_sf_type(ControllerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Controller_sf_type_nget, self->data_ptr);
}

static int
Controller_set_sf_type(ControllerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_Controller_sf_type_nset, self->data_ptr);
}

static PyObject *
Controller_get_sgs_diams(ControllerObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Controller_sgs_diams_aget, self->data_ptr);
}

static int
Controller_set_sgs_diams(ControllerObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_TcstroughPhysical_Controller_sgs_diams_aset, self->data_ptr);
}

static PyObject *
Controller_get_sgs_lengths(ControllerObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Controller_sgs_lengths_aget, self->data_ptr);
}

static int
Controller_set_sgs_lengths(ControllerObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_TcstroughPhysical_Controller_sgs_lengths_aset, self->data_ptr);
}

static PyObject *
Controller_get_sgs_wallthicks(ControllerObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Controller_sgs_wallthicks_aget, self->data_ptr);
}

static int
Controller_set_sgs_wallthicks(ControllerObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_TcstroughPhysical_Controller_sgs_wallthicks_aset, self->data_ptr);
}

static PyObject *
Controller_get_store_fl_props(ControllerObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TcstroughPhysical_Controller_store_fl_props_mget, self->data_ptr);
}

static int
Controller_set_store_fl_props(ControllerObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TcstroughPhysical_Controller_store_fl_props_mset, self->data_ptr);
}

static PyObject *
Controller_get_store_fluid(ControllerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Controller_store_fluid_nget, self->data_ptr);
}

static int
Controller_set_store_fluid(ControllerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_Controller_store_fluid_nset, self->data_ptr);
}

static PyObject *
Controller_get_t_ch_out_max(ControllerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Controller_t_ch_out_max_nget, self->data_ptr);
}

static int
Controller_set_t_ch_out_max(ControllerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_Controller_t_ch_out_max_nset, self->data_ptr);
}

static PyObject *
Controller_get_t_dis_out_min(ControllerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Controller_t_dis_out_min_nget, self->data_ptr);
}

static int
Controller_set_t_dis_out_min(ControllerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_Controller_t_dis_out_min_nset, self->data_ptr);
}

static PyObject *
Controller_get_t_standby_reset(ControllerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Controller_t_standby_reset_nget, self->data_ptr);
}

static int
Controller_set_t_standby_reset(ControllerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_Controller_t_standby_reset_nset, self->data_ptr);
}

static PyObject *
Controller_get_tank_max_heat(ControllerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Controller_tank_max_heat_nget, self->data_ptr);
}

static int
Controller_set_tank_max_heat(ControllerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_Controller_tank_max_heat_nset, self->data_ptr);
}

static PyObject *
Controller_get_tank_pairs(ControllerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Controller_tank_pairs_nget, self->data_ptr);
}

static int
Controller_set_tank_pairs(ControllerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_Controller_tank_pairs_nset, self->data_ptr);
}

static PyObject *
Controller_get_tanks_in_parallel(ControllerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Controller_tanks_in_parallel_nget, self->data_ptr);
}

static int
Controller_set_tanks_in_parallel(ControllerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_Controller_tanks_in_parallel_nset, self->data_ptr);
}

static PyObject *
Controller_get_tc_fill(ControllerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Controller_tc_fill_nget, self->data_ptr);
}

static int
Controller_set_tc_fill(ControllerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_Controller_tc_fill_nset, self->data_ptr);
}

static PyObject *
Controller_get_tc_void(ControllerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Controller_tc_void_nget, self->data_ptr);
}

static int
Controller_set_tc_void(ControllerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_Controller_tc_void_nset, self->data_ptr);
}

static PyObject *
Controller_get_tes_pump_coef(ControllerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Controller_tes_pump_coef_nget, self->data_ptr);
}

static int
Controller_set_tes_pump_coef(ControllerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_Controller_tes_pump_coef_nset, self->data_ptr);
}

static PyObject *
Controller_get_tes_type(ControllerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Controller_tes_type_nget, self->data_ptr);
}

static int
Controller_set_tes_type(ControllerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_Controller_tes_type_nset, self->data_ptr);
}

static PyObject *
Controller_get_tshours(ControllerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Controller_tshours_nget, self->data_ptr);
}

static int
Controller_set_tshours(ControllerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_Controller_tshours_nset, self->data_ptr);
}

static PyObject *
Controller_get_tslogic_a(ControllerObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Controller_tslogic_a_aget, self->data_ptr);
}

static int
Controller_set_tslogic_a(ControllerObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_TcstroughPhysical_Controller_tslogic_a_aset, self->data_ptr);
}

static PyObject *
Controller_get_tslogic_b(ControllerObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Controller_tslogic_b_aget, self->data_ptr);
}

static int
Controller_set_tslogic_b(ControllerObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_TcstroughPhysical_Controller_tslogic_b_aset, self->data_ptr);
}

static PyObject *
Controller_get_tslogic_c(ControllerObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Controller_tslogic_c_aget, self->data_ptr);
}

static int
Controller_set_tslogic_c(ControllerObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_TcstroughPhysical_Controller_tslogic_c_aset, self->data_ptr);
}

static PyObject *
Controller_get_u_tank(ControllerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Controller_u_tank_nget, self->data_ptr);
}

static int
Controller_set_u_tank(ControllerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_Controller_u_tank_nset, self->data_ptr);
}

static PyObject *
Controller_get_vol_tank(ControllerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Controller_vol_tank_nget, self->data_ptr);
}

static int
Controller_set_vol_tank(ControllerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_Controller_vol_tank_nset, self->data_ptr);
}

static PyGetSetDef Controller_getset[] = {
{"DP_SGS", (getter)Controller_get_DP_SGS,(setter)Controller_set_DP_SGS,
	PyDoc_STR("*float*: Pressure drop within the steam generator [bar]\n\n*Required*: True"),
 	NULL},
{"HDR_rough", (getter)Controller_get_HDR_rough,(setter)Controller_set_HDR_rough,
	PyDoc_STR("*float*: Header pipe roughness - used as general pipe roughness [m]\n\n*Required*: True"),
 	NULL},
{"T_set_aux", (getter)Controller_get_T_set_aux,(setter)Controller_set_T_set_aux,
	PyDoc_STR("*float*: Aux heater outlet temp set point [C]\n\n*Required*: True"),
 	NULL},
{"T_tank_cold_ini", (getter)Controller_get_T_tank_cold_ini,(setter)Controller_set_T_tank_cold_ini,
	PyDoc_STR("*float*: Initial cold tank fluid tmeperature [C]\n\n*Required*: True"),
 	NULL},
{"T_tank_hot_inlet_min", (getter)Controller_get_T_tank_hot_inlet_min,(setter)Controller_set_T_tank_hot_inlet_min,
	PyDoc_STR("*float*: Minimum hot tank htf inlet temperature [C]\n\n*Required*: True"),
 	NULL},
{"V_tank_hot_ini", (getter)Controller_get_V_tank_hot_ini,(setter)Controller_set_V_tank_hot_ini,
	PyDoc_STR("*float*: Initial hot tank fluid volume [m3]\n\n*Required*: True"),
 	NULL},
{"V_tes_des", (getter)Controller_get_V_tes_des,(setter)Controller_set_V_tes_des,
	PyDoc_STR("*float*: Design-point velocity to size the TES pipe diameters [m/s]\n\n*Required*: True"),
 	NULL},
{"W_pb_design", (getter)Controller_get_W_pb_design,(setter)Controller_set_W_pb_design,
	PyDoc_STR("*float*: Rated plant capacity [MWe]\n\n*Required*: True"),
 	NULL},
{"aux_array", (getter)Controller_get_aux_array,(setter)Controller_set_aux_array,
	PyDoc_STR("*sequence*: Coefficients for auxiliary heater parasitics calcs [-]\n\n*Required*: True"),
 	NULL},
{"bop_array", (getter)Controller_get_bop_array,(setter)Controller_set_bop_array,
	PyDoc_STR("*sequence*: Coefficients for balance of plant parasitics calcs [-]\n\n*Required*: True"),
 	NULL},
{"cold_tank_Thtr", (getter)Controller_get_cold_tank_Thtr,(setter)Controller_set_cold_tank_Thtr,
	PyDoc_STR("*float*: Minimum allowable cold tank HTF temp [C]\n\n*Required*: True"),
 	NULL},
{"custom_sgs_pipe_sizes", (getter)Controller_get_custom_sgs_pipe_sizes,(setter)Controller_set_custom_sgs_pipe_sizes,
	PyDoc_STR("*float*: Use custom SGS pipe diams, wallthks, and lengths [-]\n\n*Required*: True"),
 	NULL},
{"custom_tes_p_loss", (getter)Controller_get_custom_tes_p_loss,(setter)Controller_set_custom_tes_p_loss,
	PyDoc_STR("*float*: TES pipe losses are based on custom lengths and coeffs [-]\n\n*Required*: True"),
 	NULL},
{"cycle_cutoff_frac", (getter)Controller_get_cycle_cutoff_frac,(setter)Controller_set_cycle_cutoff_frac,
	PyDoc_STR("*float*: Minimum turbine operation fraction before shutdown [-]\n\n*Required*: True"),
 	NULL},
{"cycle_max_frac", (getter)Controller_get_cycle_max_frac,(setter)Controller_set_cycle_max_frac,
	PyDoc_STR("*float*: Maximum turbine over design operation fraction [-]\n\n*Required*: True"),
 	NULL},
{"dt_cold", (getter)Controller_get_dt_cold,(setter)Controller_set_dt_cold,
	PyDoc_STR("*float*: Cold side HX approach temp [C]\n\n*Required*: True"),
 	NULL},
{"dt_hot", (getter)Controller_get_dt_hot,(setter)Controller_set_dt_hot,
	PyDoc_STR("*float*: Hot side HX approach temp [C]\n\n*Required*: True"),
 	NULL},
{"eta_pump", (getter)Controller_get_eta_pump,(setter)Controller_set_eta_pump,
	PyDoc_STR("*float*: HTF pump efficiency [none]\n\n*Required*: True"),
 	NULL},
{"f_tc_cold", (getter)Controller_get_f_tc_cold,(setter)Controller_set_f_tc_cold,
	PyDoc_STR("*float*: 0=entire tank is hot, 1=entire tank is cold [-]\n\n*Required*: True"),
 	NULL},
{"ffrac", (getter)Controller_get_ffrac,(setter)Controller_set_ffrac,
	PyDoc_STR("*sequence*: Fossil dispatch logic [-]\n\n*Required*: True"),
 	NULL},
{"field_fl_props", (getter)Controller_get_field_fl_props,(setter)Controller_set_field_fl_props,
	PyDoc_STR("*sequence[sequence]*: User defined field fluid property data [-]\n\n*Required*: True"),
 	NULL},
{"fossil_mode", (getter)Controller_get_fossil_mode,(setter)Controller_set_fossil_mode,
	PyDoc_STR("*float*: Fossil backup mode 1=Normal 2=Topping [-]\n\n*Constraints*: INTEGER\n\n*Required*: True"),
 	NULL},
{"h_tank", (getter)Controller_get_h_tank,(setter)Controller_set_h_tank,
	PyDoc_STR("*float*: Total height of tank (height of HTF when tank is full [m]\n\n*Required*: True"),
 	NULL},
{"h_tank_min", (getter)Controller_get_h_tank_min,(setter)Controller_set_h_tank_min,
	PyDoc_STR("*float*: Minimum allowable HTF height in storage tank [m]\n\n*Required*: True"),
 	NULL},
{"has_hot_tank_bypass", (getter)Controller_get_has_hot_tank_bypass,(setter)Controller_set_has_hot_tank_bypass,
	PyDoc_STR("*float*: Bypass valve connects field outlet to cold tank [-]\n\n*Required*: True"),
 	NULL},
{"hot_tank_Thtr", (getter)Controller_get_hot_tank_Thtr,(setter)Controller_set_hot_tank_Thtr,
	PyDoc_STR("*float*: Minimum allowable hot tank HTF temp [C]\n\n*Required*: True"),
 	NULL},
{"hx_config", (getter)Controller_get_hx_config,(setter)Controller_set_hx_config,
	PyDoc_STR("*float*: HX configuration [-]\n\n*Required*: True"),
 	NULL},
{"is_hx", (getter)Controller_get_is_hx,(setter)Controller_set_is_hx,
	PyDoc_STR("*float*: Heat exchanger (HX) exists (1=yes, 0=no) [-]\n\n*Required*: True"),
 	NULL},
{"k_tes_loss_coeffs", (getter)Controller_get_k_tes_loss_coeffs,(setter)Controller_set_k_tes_loss_coeffs,
	PyDoc_STR("*sequence*: Minor loss coeffs for the coll, gen, and bypass loops [-]\n\n*Required*: True"),
 	NULL},
{"nodes", (getter)Controller_get_nodes,(setter)Controller_set_nodes,
	PyDoc_STR("*float*: Nodes modeled in the flow path [-]\n\n*Required*: True"),
 	NULL},
{"pb_fixed_par", (getter)Controller_get_pb_fixed_par,(setter)Controller_set_pb_fixed_par,
	PyDoc_STR("*float*: Fraction of rated gross power constantly consumed [-]\n\n*Required*: True"),
 	NULL},
{"pb_pump_coef", (getter)Controller_get_pb_pump_coef,(setter)Controller_set_pb_pump_coef,
	PyDoc_STR("*float*: Pumping power to move 1kg of HTF through PB loop [kW/(kg/s)]\n\n*Required*: True"),
 	NULL},
{"q_max_aux", (getter)Controller_get_q_max_aux,(setter)Controller_set_q_max_aux,
	PyDoc_STR("*float*: Max heat rate of auxiliary heater [MWt]\n\n*Required*: True"),
 	NULL},
{"q_pb_design", (getter)Controller_get_q_pb_design,(setter)Controller_set_q_pb_design,
	PyDoc_STR("*float*: Design heat input to power block [MWt]\n\n*Required*: True"),
 	NULL},
{"sf_type", (getter)Controller_get_sf_type,(setter)Controller_set_sf_type,
	PyDoc_STR("*float*: Solar field type, 1 = trough, 2 = tower [-]\n\n*Required*: True"),
 	NULL},
{"sgs_diams", (getter)Controller_get_sgs_diams,(setter)Controller_set_sgs_diams,
	PyDoc_STR("*sequence*: Custom SGS diameters [m]\n\n*Required*: True"),
 	NULL},
{"sgs_lengths", (getter)Controller_get_sgs_lengths,(setter)Controller_set_sgs_lengths,
	PyDoc_STR("*sequence*: Custom SGS lengths [m]\n\n*Required*: True"),
 	NULL},
{"sgs_wallthicks", (getter)Controller_get_sgs_wallthicks,(setter)Controller_set_sgs_wallthicks,
	PyDoc_STR("*sequence*: Custom SGS wall thicknesses [m]\n\n*Required*: True"),
 	NULL},
{"store_fl_props", (getter)Controller_get_store_fl_props,(setter)Controller_set_store_fl_props,
	PyDoc_STR("*sequence[sequence]*: User defined storage fluid property data [-]\n\n*Required*: True"),
 	NULL},
{"store_fluid", (getter)Controller_get_store_fluid,(setter)Controller_set_store_fluid,
	PyDoc_STR("*float*: Material number for storage fluid [-]\n\n*Required*: True"),
 	NULL},
{"t_ch_out_max", (getter)Controller_get_t_ch_out_max,(setter)Controller_set_t_ch_out_max,
	PyDoc_STR("*float*: Max allowable cold side outlet temp during charge [C]\n\n*Required*: True"),
 	NULL},
{"t_dis_out_min", (getter)Controller_get_t_dis_out_min,(setter)Controller_set_t_dis_out_min,
	PyDoc_STR("*float*: Min allowable hot side outlet temp during discharge [C]\n\n*Required*: True"),
 	NULL},
{"t_standby_reset", (getter)Controller_get_t_standby_reset,(setter)Controller_set_t_standby_reset,
	PyDoc_STR("*float*: Maximum allowable time for PB standby operation [hr]\n\n*Required*: True"),
 	NULL},
{"tank_max_heat", (getter)Controller_get_tank_max_heat,(setter)Controller_set_tank_max_heat,
	PyDoc_STR("*float*: Rated heater capacity for tank heating [MW]\n\n*Required*: True"),
 	NULL},
{"tank_pairs", (getter)Controller_get_tank_pairs,(setter)Controller_set_tank_pairs,
	PyDoc_STR("*float*: Number of equivalent tank pairs [-]\n\n*Constraints*: INTEGER\n\n*Required*: True"),
 	NULL},
{"tanks_in_parallel", (getter)Controller_get_tanks_in_parallel,(setter)Controller_set_tanks_in_parallel,
	PyDoc_STR("*float*: Tanks are in parallel, not in series, with solar field [-]\n\n*Required*: True"),
 	NULL},
{"tc_fill", (getter)Controller_get_tc_fill,(setter)Controller_set_tc_fill,
	PyDoc_STR("*float*: Thermocline fill material [-]\n\n*Required*: True"),
 	NULL},
{"tc_void", (getter)Controller_get_tc_void,(setter)Controller_set_tc_void,
	PyDoc_STR("*float*: Thermocline void fraction [-]\n\n*Required*: True"),
 	NULL},
{"tes_pump_coef", (getter)Controller_get_tes_pump_coef,(setter)Controller_set_tes_pump_coef,
	PyDoc_STR("*float*: Pumping power to move 1kg of HTF through tes loop [kW/(kg/s)]\n\n*Required*: True"),
 	NULL},
{"tes_type", (getter)Controller_get_tes_type,(setter)Controller_set_tes_type,
	PyDoc_STR("*float*: 1=2-tank, 2=thermocline [-]\n\n*Required*: True"),
 	NULL},
{"tshours", (getter)Controller_get_tshours,(setter)Controller_set_tshours,
	PyDoc_STR("*float*: Equivalent full-load thermal storage hours [hr]\n\n*Required*: True"),
 	NULL},
{"tslogic_a", (getter)Controller_get_tslogic_a,(setter)Controller_set_tslogic_a,
	PyDoc_STR("*sequence*: Dispatch logic without solar [-]\n\n*Required*: True"),
 	NULL},
{"tslogic_b", (getter)Controller_get_tslogic_b,(setter)Controller_set_tslogic_b,
	PyDoc_STR("*sequence*: Dispatch logic with solar [-]\n\n*Required*: True"),
 	NULL},
{"tslogic_c", (getter)Controller_get_tslogic_c,(setter)Controller_set_tslogic_c,
	PyDoc_STR("*sequence*: Dispatch logic for turbine load fraction [-]\n\n*Required*: True"),
 	NULL},
{"u_tank", (getter)Controller_get_u_tank,(setter)Controller_set_u_tank,
	PyDoc_STR("*float*: Loss coefficient from the tank [W/m2-K]\n\n*Required*: True"),
 	NULL},
{"vol_tank", (getter)Controller_get_vol_tank,(setter)Controller_set_vol_tank,
	PyDoc_STR("*float*: Total tank volume, including unusable HTF at bottom [m3]\n\n*Required*: True"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject Controller_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"TcstroughPhysical.Controller",             /*tp_name*/
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
	 * TouTranslator Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_TcstroughPhysical   data_ptr;
} TouTranslatorObject;

static PyTypeObject TouTranslator_Type;

static PyObject *
TouTranslator_new(SAM_TcstroughPhysical data_ptr)
{
	PyObject* new_obj = TouTranslator_Type.tp_alloc(&TouTranslator_Type,0);

	TouTranslatorObject* TouTranslator_obj = (TouTranslatorObject*)new_obj;

	TouTranslator_obj->data_ptr = data_ptr;

	return new_obj;
}

/* TouTranslator methods */

static PyObject *
TouTranslator_assign(TouTranslatorObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "TcstroughPhysical", "TouTranslator")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
TouTranslator_export(TouTranslatorObject *self, PyObject *args)
{
	PyTypeObject* tp = &TouTranslator_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef TouTranslator_methods[] = {
		{"assign",            (PyCFunction)TouTranslator_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``TouTranslator_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)TouTranslator_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
TouTranslator_get_weekday_schedule(TouTranslatorObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TcstroughPhysical_TouTranslator_weekday_schedule_mget, self->data_ptr);
}

static int
TouTranslator_set_weekday_schedule(TouTranslatorObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TcstroughPhysical_TouTranslator_weekday_schedule_mset, self->data_ptr);
}

static PyObject *
TouTranslator_get_weekend_schedule(TouTranslatorObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TcstroughPhysical_TouTranslator_weekend_schedule_mget, self->data_ptr);
}

static int
TouTranslator_set_weekend_schedule(TouTranslatorObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TcstroughPhysical_TouTranslator_weekend_schedule_mset, self->data_ptr);
}

static PyGetSetDef TouTranslator_getset[] = {
{"weekday_schedule", (getter)TouTranslator_get_weekday_schedule,(setter)TouTranslator_set_weekday_schedule,
	PyDoc_STR("*sequence[sequence]*: Dispatch 12mx24h schedule for week days\n\n*Required*: True"),
 	NULL},
{"weekend_schedule", (getter)TouTranslator_get_weekend_schedule,(setter)TouTranslator_set_weekend_schedule,
	PyDoc_STR("*sequence[sequence]*: Dispatch 12mx24h schedule for weekends\n\n*Required*: True"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject TouTranslator_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"TcstroughPhysical.TouTranslator",             /*tp_name*/
		sizeof(TouTranslatorObject),          /*tp_basicsize*/
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
		TouTranslator_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		TouTranslator_getset,          /*tp_getset*/
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
	SAM_TcstroughPhysical   data_ptr;
} PowerblockObject;

static PyTypeObject Powerblock_Type;

static PyObject *
Powerblock_new(SAM_TcstroughPhysical data_ptr)
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

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "TcstroughPhysical", "Powerblock")){
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
Powerblock_get_CT(PowerblockObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Powerblock_CT_nget, self->data_ptr);
}

static int
Powerblock_set_CT(PowerblockObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_Powerblock_CT_nset, self->data_ptr);
}

static PyObject *
Powerblock_get_F_wc(PowerblockObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Powerblock_F_wc_aget, self->data_ptr);
}

static int
Powerblock_set_F_wc(PowerblockObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_TcstroughPhysical_Powerblock_F_wc_aset, self->data_ptr);
}

static PyObject *
Powerblock_get_P_boil(PowerblockObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Powerblock_P_boil_nget, self->data_ptr);
}

static int
Powerblock_set_P_boil(PowerblockObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_Powerblock_P_boil_nset, self->data_ptr);
}

static PyObject *
Powerblock_get_P_cond_min(PowerblockObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Powerblock_P_cond_min_nget, self->data_ptr);
}

static int
Powerblock_set_P_cond_min(PowerblockObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_Powerblock_P_cond_min_nset, self->data_ptr);
}

static PyObject *
Powerblock_get_P_cond_ratio(PowerblockObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Powerblock_P_cond_ratio_nget, self->data_ptr);
}

static int
Powerblock_set_P_cond_ratio(PowerblockObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_Powerblock_P_cond_ratio_nset, self->data_ptr);
}

static PyObject *
Powerblock_get_T_ITD_des(PowerblockObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Powerblock_T_ITD_des_nget, self->data_ptr);
}

static int
Powerblock_set_T_ITD_des(PowerblockObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_Powerblock_T_ITD_des_nset, self->data_ptr);
}

static PyObject *
Powerblock_get_T_amb_des(PowerblockObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Powerblock_T_amb_des_nget, self->data_ptr);
}

static int
Powerblock_set_T_amb_des(PowerblockObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_Powerblock_T_amb_des_nset, self->data_ptr);
}

static PyObject *
Powerblock_get_T_approach(PowerblockObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Powerblock_T_approach_nget, self->data_ptr);
}

static int
Powerblock_set_T_approach(PowerblockObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_Powerblock_T_approach_nset, self->data_ptr);
}

static PyObject *
Powerblock_get_dT_cw_ref(PowerblockObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Powerblock_dT_cw_ref_nget, self->data_ptr);
}

static int
Powerblock_set_dT_cw_ref(PowerblockObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_Powerblock_dT_cw_ref_nset, self->data_ptr);
}

static PyObject *
Powerblock_get_eta_ref(PowerblockObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Powerblock_eta_ref_nget, self->data_ptr);
}

static int
Powerblock_set_eta_ref(PowerblockObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_Powerblock_eta_ref_nset, self->data_ptr);
}

static PyObject *
Powerblock_get_n_pl_inc(PowerblockObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Powerblock_n_pl_inc_nget, self->data_ptr);
}

static int
Powerblock_set_n_pl_inc(PowerblockObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_Powerblock_n_pl_inc_nset, self->data_ptr);
}

static PyObject *
Powerblock_get_pb_bd_frac(PowerblockObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Powerblock_pb_bd_frac_nget, self->data_ptr);
}

static int
Powerblock_set_pb_bd_frac(PowerblockObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_Powerblock_pb_bd_frac_nset, self->data_ptr);
}

static PyObject *
Powerblock_get_pc_config(PowerblockObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Powerblock_pc_config_nget, self->data_ptr);
}

static int
Powerblock_set_pc_config(PowerblockObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_Powerblock_pc_config_nset, self->data_ptr);
}

static PyObject *
Powerblock_get_q_sby_frac(PowerblockObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Powerblock_q_sby_frac_nget, self->data_ptr);
}

static int
Powerblock_set_q_sby_frac(PowerblockObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_Powerblock_q_sby_frac_nset, self->data_ptr);
}

static PyObject *
Powerblock_get_startup_frac(PowerblockObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Powerblock_startup_frac_nget, self->data_ptr);
}

static int
Powerblock_set_startup_frac(PowerblockObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_Powerblock_startup_frac_nset, self->data_ptr);
}

static PyObject *
Powerblock_get_startup_time(PowerblockObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Powerblock_startup_time_nget, self->data_ptr);
}

static int
Powerblock_set_startup_time(PowerblockObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_Powerblock_startup_time_nset, self->data_ptr);
}

static PyObject *
Powerblock_get_tech_type(PowerblockObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Powerblock_tech_type_nget, self->data_ptr);
}

static int
Powerblock_set_tech_type(PowerblockObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_Powerblock_tech_type_nset, self->data_ptr);
}

static PyGetSetDef Powerblock_getset[] = {
{"CT", (getter)Powerblock_get_CT,(setter)Powerblock_set_CT,
	PyDoc_STR("*float*: Flag for using dry cooling or wet cooling system [none]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"F_wc", (getter)Powerblock_get_F_wc,(setter)Powerblock_set_F_wc,
	PyDoc_STR("*sequence*: Fraction indicating wet cooling use for hybrid system [none]\n\n*Options*: constant=[0,0,0,0,0,0,0,0,0]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"P_boil", (getter)Powerblock_get_P_boil,(setter)Powerblock_set_P_boil,
	PyDoc_STR("*float*: Boiler operating pressure [bar]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"P_cond_min", (getter)Powerblock_get_P_cond_min,(setter)Powerblock_set_P_cond_min,
	PyDoc_STR("*float*: Minimum condenser pressure [inHg]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"P_cond_ratio", (getter)Powerblock_get_P_cond_ratio,(setter)Powerblock_set_P_cond_ratio,
	PyDoc_STR("*float*: Condenser pressure ratio [none]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"T_ITD_des", (getter)Powerblock_get_T_ITD_des,(setter)Powerblock_set_T_ITD_des,
	PyDoc_STR("*float*: ITD at design for dry system [C]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"T_amb_des", (getter)Powerblock_get_T_amb_des,(setter)Powerblock_set_T_amb_des,
	PyDoc_STR("*float*: Reference ambient temperature at design point [C]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"T_approach", (getter)Powerblock_get_T_approach,(setter)Powerblock_set_T_approach,
	PyDoc_STR("*float*: Cooling tower approach temperature [C]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"dT_cw_ref", (getter)Powerblock_get_dT_cw_ref,(setter)Powerblock_set_dT_cw_ref,
	PyDoc_STR("*float*: Reference condenser cooling water inlet/outlet T diff [C]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"eta_ref", (getter)Powerblock_get_eta_ref,(setter)Powerblock_set_eta_ref,
	PyDoc_STR("*float*: Reference conversion efficiency at design condition [none]\n\n*Required*: True"),
 	NULL},
{"n_pl_inc", (getter)Powerblock_get_n_pl_inc,(setter)Powerblock_set_n_pl_inc,
	PyDoc_STR("*float*: Number of part-load increments for the heat rejection system [none]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"pb_bd_frac", (getter)Powerblock_get_pb_bd_frac,(setter)Powerblock_set_pb_bd_frac,
	PyDoc_STR("*float*: Power block blowdown steam fraction  [none]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"pc_config", (getter)Powerblock_get_pc_config,(setter)Powerblock_set_pc_config,
	PyDoc_STR("*float*: 0: Steam Rankine (224), 1: user defined [-]\n\n*Constraints*: INTEGER\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"q_sby_frac", (getter)Powerblock_get_q_sby_frac,(setter)Powerblock_set_q_sby_frac,
	PyDoc_STR("*float*: Fraction of thermal power required for standby mode [none]\n\n*Required*: True"),
 	NULL},
{"startup_frac", (getter)Powerblock_get_startup_frac,(setter)Powerblock_set_startup_frac,
	PyDoc_STR("*float*: Fraction of design thermal power needed for startup [none]\n\n*Required*: True"),
 	NULL},
{"startup_time", (getter)Powerblock_get_startup_time,(setter)Powerblock_set_startup_time,
	PyDoc_STR("*float*: Time needed for power block startup [hr]\n\n*Required*: True"),
 	NULL},
{"tech_type", (getter)Powerblock_get_tech_type,(setter)Powerblock_set_tech_type,
	PyDoc_STR("*float*: Turbine inlet pressure control flag (sliding=user, fixed=trough) [1/2/3]\n\n*Info*: tower/trough/user\n\n*Required*: set to 0 if not provided."),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject Powerblock_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"TcstroughPhysical.Powerblock",             /*tp_name*/
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
	 * UserDefinedPC Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_TcstroughPhysical   data_ptr;
} UserDefinedPCObject;

static PyTypeObject UserDefinedPC_Type;

static PyObject *
UserDefinedPC_new(SAM_TcstroughPhysical data_ptr)
{
	PyObject* new_obj = UserDefinedPC_Type.tp_alloc(&UserDefinedPC_Type,0);

	UserDefinedPCObject* UserDefinedPC_obj = (UserDefinedPCObject*)new_obj;

	UserDefinedPC_obj->data_ptr = data_ptr;

	return new_obj;
}

/* UserDefinedPC methods */

static PyObject *
UserDefinedPC_assign(UserDefinedPCObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "TcstroughPhysical", "UserDefinedPC")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
UserDefinedPC_export(UserDefinedPCObject *self, PyObject *args)
{
	PyTypeObject* tp = &UserDefinedPC_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef UserDefinedPC_methods[] = {
		{"assign",            (PyCFunction)UserDefinedPC_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``UserDefinedPC_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)UserDefinedPC_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
UserDefinedPC_get_ud_T_amb_des(UserDefinedPCObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_UserDefinedPC_ud_T_amb_des_nget, self->data_ptr);
}

static int
UserDefinedPC_set_ud_T_amb_des(UserDefinedPCObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_UserDefinedPC_ud_T_amb_des_nset, self->data_ptr);
}

static PyObject *
UserDefinedPC_get_ud_T_amb_high(UserDefinedPCObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_UserDefinedPC_ud_T_amb_high_nget, self->data_ptr);
}

static int
UserDefinedPC_set_ud_T_amb_high(UserDefinedPCObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_UserDefinedPC_ud_T_amb_high_nset, self->data_ptr);
}

static PyObject *
UserDefinedPC_get_ud_T_amb_ind_od(UserDefinedPCObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TcstroughPhysical_UserDefinedPC_ud_T_amb_ind_od_mget, self->data_ptr);
}

static int
UserDefinedPC_set_ud_T_amb_ind_od(UserDefinedPCObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TcstroughPhysical_UserDefinedPC_ud_T_amb_ind_od_mset, self->data_ptr);
}

static PyObject *
UserDefinedPC_get_ud_T_amb_low(UserDefinedPCObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_UserDefinedPC_ud_T_amb_low_nget, self->data_ptr);
}

static int
UserDefinedPC_set_ud_T_amb_low(UserDefinedPCObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_UserDefinedPC_ud_T_amb_low_nset, self->data_ptr);
}

static PyObject *
UserDefinedPC_get_ud_T_htf_high(UserDefinedPCObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_UserDefinedPC_ud_T_htf_high_nget, self->data_ptr);
}

static int
UserDefinedPC_set_ud_T_htf_high(UserDefinedPCObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_UserDefinedPC_ud_T_htf_high_nset, self->data_ptr);
}

static PyObject *
UserDefinedPC_get_ud_T_htf_ind_od(UserDefinedPCObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TcstroughPhysical_UserDefinedPC_ud_T_htf_ind_od_mget, self->data_ptr);
}

static int
UserDefinedPC_set_ud_T_htf_ind_od(UserDefinedPCObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TcstroughPhysical_UserDefinedPC_ud_T_htf_ind_od_mset, self->data_ptr);
}

static PyObject *
UserDefinedPC_get_ud_T_htf_low(UserDefinedPCObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_UserDefinedPC_ud_T_htf_low_nget, self->data_ptr);
}

static int
UserDefinedPC_set_ud_T_htf_low(UserDefinedPCObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_UserDefinedPC_ud_T_htf_low_nset, self->data_ptr);
}

static PyObject *
UserDefinedPC_get_ud_f_W_dot_cool_des(UserDefinedPCObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_UserDefinedPC_ud_f_W_dot_cool_des_nget, self->data_ptr);
}

static int
UserDefinedPC_set_ud_f_W_dot_cool_des(UserDefinedPCObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_UserDefinedPC_ud_f_W_dot_cool_des_nset, self->data_ptr);
}

static PyObject *
UserDefinedPC_get_ud_ind_od(UserDefinedPCObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TcstroughPhysical_UserDefinedPC_ud_ind_od_mget, self->data_ptr);
}

static int
UserDefinedPC_set_ud_ind_od(UserDefinedPCObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TcstroughPhysical_UserDefinedPC_ud_ind_od_mset, self->data_ptr);
}

static PyObject *
UserDefinedPC_get_ud_m_dot_htf_high(UserDefinedPCObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_UserDefinedPC_ud_m_dot_htf_high_nget, self->data_ptr);
}

static int
UserDefinedPC_set_ud_m_dot_htf_high(UserDefinedPCObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_UserDefinedPC_ud_m_dot_htf_high_nset, self->data_ptr);
}

static PyObject *
UserDefinedPC_get_ud_m_dot_htf_ind_od(UserDefinedPCObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TcstroughPhysical_UserDefinedPC_ud_m_dot_htf_ind_od_mget, self->data_ptr);
}

static int
UserDefinedPC_set_ud_m_dot_htf_ind_od(UserDefinedPCObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TcstroughPhysical_UserDefinedPC_ud_m_dot_htf_ind_od_mset, self->data_ptr);
}

static PyObject *
UserDefinedPC_get_ud_m_dot_htf_low(UserDefinedPCObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_UserDefinedPC_ud_m_dot_htf_low_nget, self->data_ptr);
}

static int
UserDefinedPC_set_ud_m_dot_htf_low(UserDefinedPCObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_UserDefinedPC_ud_m_dot_htf_low_nset, self->data_ptr);
}

static PyObject *
UserDefinedPC_get_ud_m_dot_water_cool_des(UserDefinedPCObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_UserDefinedPC_ud_m_dot_water_cool_des_nget, self->data_ptr);
}

static int
UserDefinedPC_set_ud_m_dot_water_cool_des(UserDefinedPCObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_UserDefinedPC_ud_m_dot_water_cool_des_nset, self->data_ptr);
}

static PyGetSetDef UserDefinedPC_getset[] = {
{"ud_T_amb_des", (getter)UserDefinedPC_get_ud_T_amb_des,(setter)UserDefinedPC_set_ud_T_amb_des,
	PyDoc_STR("*float*: Ambient temperature at user-defined power cycle design point [C]\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"ud_T_amb_high", (getter)UserDefinedPC_get_ud_T_amb_high,(setter)UserDefinedPC_set_ud_T_amb_high,
	PyDoc_STR("*float*: High level ambient temperature for HTF mass flow rate parametric [C]\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"ud_T_amb_ind_od", (getter)UserDefinedPC_get_ud_T_amb_ind_od,(setter)UserDefinedPC_set_ud_T_amb_ind_od,
	PyDoc_STR("*sequence[sequence]*: Off design table of user-defined power cycle performance formed from parametric on T_amb [C]\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"ud_T_amb_low", (getter)UserDefinedPC_get_ud_T_amb_low,(setter)UserDefinedPC_set_ud_T_amb_low,
	PyDoc_STR("*float*: Low level ambient temperature for HTF mass flow rate parametric [C]\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"ud_T_htf_high", (getter)UserDefinedPC_get_ud_T_htf_high,(setter)UserDefinedPC_set_ud_T_htf_high,
	PyDoc_STR("*float*: High level HTF inlet temperature for T_amb parametric [C]\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"ud_T_htf_ind_od", (getter)UserDefinedPC_get_ud_T_htf_ind_od,(setter)UserDefinedPC_set_ud_T_htf_ind_od,
	PyDoc_STR("*sequence[sequence]*: Off design table of user-defined power cycle performance formed from parametric on T_htf_hot [C]\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"ud_T_htf_low", (getter)UserDefinedPC_get_ud_T_htf_low,(setter)UserDefinedPC_set_ud_T_htf_low,
	PyDoc_STR("*float*: Low level HTF inlet temperature for T_amb parametric [C]\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"ud_f_W_dot_cool_des", (getter)UserDefinedPC_get_ud_f_W_dot_cool_des,(setter)UserDefinedPC_set_ud_f_W_dot_cool_des,
	PyDoc_STR("*float*: Percent of user-defined power cycle design gross output consumed by cooling [%]\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"ud_ind_od", (getter)UserDefinedPC_get_ud_ind_od,(setter)UserDefinedPC_set_ud_ind_od,
	PyDoc_STR("*sequence[sequence]*: Off design user-defined power cycle performance as function of T_htf, m_dot_htf [ND], and T_amb\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"ud_m_dot_htf_high", (getter)UserDefinedPC_get_ud_m_dot_htf_high,(setter)UserDefinedPC_set_ud_m_dot_htf_high,
	PyDoc_STR("*float*: High level normalized HTF mass flow rate for T_HTF parametric [-]\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"ud_m_dot_htf_ind_od", (getter)UserDefinedPC_get_ud_m_dot_htf_ind_od,(setter)UserDefinedPC_set_ud_m_dot_htf_ind_od,
	PyDoc_STR("*sequence[sequence]*: Off design table of user-defined power cycle performance formed from parametric on m_dot_htf [ND]\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"ud_m_dot_htf_low", (getter)UserDefinedPC_get_ud_m_dot_htf_low,(setter)UserDefinedPC_set_ud_m_dot_htf_low,
	PyDoc_STR("*float*: Low level normalized HTF mass flow rate for T_HTF parametric [-]\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"ud_m_dot_water_cool_des", (getter)UserDefinedPC_get_ud_m_dot_water_cool_des,(setter)UserDefinedPC_set_ud_m_dot_water_cool_des,
	PyDoc_STR("*float*: Mass flow rate of water required at user-defined power cycle design point [kg/s]\n\n*Required*: set to 1 if not provided."),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject UserDefinedPC_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"TcstroughPhysical.UserDefinedPC",             /*tp_name*/
		sizeof(UserDefinedPCObject),          /*tp_basicsize*/
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
		UserDefinedPC_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		UserDefinedPC_getset,          /*tp_getset*/
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
	 * Enet Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_TcstroughPhysical   data_ptr;
} EnetObject;

static PyTypeObject Enet_Type;

static PyObject *
Enet_new(SAM_TcstroughPhysical data_ptr)
{
	PyObject* new_obj = Enet_Type.tp_alloc(&Enet_Type,0);

	EnetObject* Enet_obj = (EnetObject*)new_obj;

	Enet_obj->data_ptr = data_ptr;

	return new_obj;
}

/* Enet methods */

static PyObject *
Enet_assign(EnetObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "TcstroughPhysical", "Enet")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
Enet_export(EnetObject *self, PyObject *args)
{
	PyTypeObject* tp = &Enet_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef Enet_methods[] = {
		{"assign",            (PyCFunction)Enet_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``Enet_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)Enet_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
Enet_get_eta_lhv(EnetObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Enet_eta_lhv_nget, self->data_ptr);
}

static int
Enet_set_eta_lhv(EnetObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_Enet_eta_lhv_nset, self->data_ptr);
}

static PyObject *
Enet_get_eta_tes_htr(EnetObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Enet_eta_tes_htr_nget, self->data_ptr);
}

static int
Enet_set_eta_tes_htr(EnetObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughPhysical_Enet_eta_tes_htr_nset, self->data_ptr);
}

static PyGetSetDef Enet_getset[] = {
{"eta_lhv", (getter)Enet_get_eta_lhv,(setter)Enet_set_eta_lhv,
	PyDoc_STR("*float*: Fossil fuel lower heating value - Thermal power generated per unit fuel [MW/MMBTU]\n\n*Required*: True"),
 	NULL},
{"eta_tes_htr", (getter)Enet_get_eta_tes_htr,(setter)Enet_set_eta_tes_htr,
	PyDoc_STR("*float*: Thermal storage tank heater efficiency (fp_mode=1 only) [none]\n\n*Required*: True"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject Enet_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"TcstroughPhysical.Enet",             /*tp_name*/
		sizeof(EnetObject),          /*tp_basicsize*/
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
		Enet_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		Enet_getset,          /*tp_getset*/
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
	SAM_TcstroughPhysical   data_ptr;
} OutputsObject;

static PyTypeObject Outputs_Type;

static PyObject *
Outputs_new(SAM_TcstroughPhysical data_ptr)
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

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "TcstroughPhysical", "Outputs")){
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
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_CosTh_ave_aget, self->data_ptr);
}

static PyObject *
Outputs_get_DP_tot(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_DP_tot_aget, self->data_ptr);
}

static PyObject *
Outputs_get_E_bal_startup(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_E_bal_startup_aget, self->data_ptr);
}

static PyObject *
Outputs_get_EndLoss_ave(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_EndLoss_ave_aget, self->data_ptr);
}

static PyObject *
Outputs_get_EqOpteff(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_EqOpteff_aget, self->data_ptr);
}

static PyObject *
Outputs_get_Fuel_usage(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_Fuel_usage_aget, self->data_ptr);
}

static PyObject *
Outputs_get_IAM_ave(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_IAM_ave_aget, self->data_ptr);
}

static PyObject *
Outputs_get_Pipe_hl(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_Pipe_hl_aget, self->data_ptr);
}

static PyObject *
Outputs_get_Q_aux_backup(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_Q_aux_backup_aget, self->data_ptr);
}

static PyObject *
Outputs_get_Q_par_sf_fp(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_Q_par_sf_fp_aget, self->data_ptr);
}

static PyObject *
Outputs_get_Q_par_tes_fp(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_Q_par_tes_fp_aget, self->data_ptr);
}

static PyObject *
Outputs_get_RowShadow_ave(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_RowShadow_ave_aget, self->data_ptr);
}

static PyObject *
Outputs_get_SCA_par_tot(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_SCA_par_tot_aget, self->data_ptr);
}

static PyObject *
Outputs_get_SCAs_def(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_SCAs_def_aget, self->data_ptr);
}

static PyObject *
Outputs_get_T_field_in(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_T_field_in_aget, self->data_ptr);
}

static PyObject *
Outputs_get_T_pb_in(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_T_pb_in_aget, self->data_ptr);
}

static PyObject *
Outputs_get_T_pb_out(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_T_pb_out_aget, self->data_ptr);
}

static PyObject *
Outputs_get_T_sys_c(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_T_sys_c_aget, self->data_ptr);
}

static PyObject *
Outputs_get_T_sys_h(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_T_sys_h_aget, self->data_ptr);
}

static PyObject *
Outputs_get_T_tank_cold_fin(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_T_tank_cold_fin_aget, self->data_ptr);
}

static PyObject *
Outputs_get_T_tank_cold_in(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_T_tank_cold_in_aget, self->data_ptr);
}

static PyObject *
Outputs_get_T_tank_hot_fin(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_T_tank_hot_fin_aget, self->data_ptr);
}

static PyObject *
Outputs_get_T_tank_hot_in(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_T_tank_hot_in_aget, self->data_ptr);
}

static PyObject *
Outputs_get_Theta_ave(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_Theta_ave_aget, self->data_ptr);
}

static PyObject *
Outputs_get_Ts_cold(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_Ts_cold_aget, self->data_ptr);
}

static PyObject *
Outputs_get_Ts_hot(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_Ts_hot_aget, self->data_ptr);
}

static PyObject *
Outputs_get_W_cool_par(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_W_cool_par_aget, self->data_ptr);
}

static PyObject *
Outputs_get_W_cycle_gross(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_W_cycle_gross_aget, self->data_ptr);
}

static PyObject *
Outputs_get_W_dot_pump(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_W_dot_pump_aget, self->data_ptr);
}

static PyObject *
Outputs_get_W_net(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_W_net_aget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_W_cycle_gross(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Outputs_annual_W_cycle_gross_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_energy(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Outputs_annual_energy_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_fuel_usage(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Outputs_annual_fuel_usage_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_q_abs_tot(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Outputs_annual_q_abs_tot_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_q_aux(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Outputs_annual_q_aux_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_q_avail(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Outputs_annual_q_avail_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_q_dump(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Outputs_annual_q_dump_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_q_inc_sf_tot(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Outputs_annual_q_inc_sf_tot_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_q_pb(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Outputs_annual_q_pb_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_q_to_tes(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Outputs_annual_q_to_tes_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_total_water_use(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Outputs_annual_total_water_use_nget, self->data_ptr);
}

static PyObject *
Outputs_get_aux_par(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_aux_par_aget, self->data_ptr);
}

static PyObject *
Outputs_get_beam(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_beam_aget, self->data_ptr);
}

static PyObject *
Outputs_get_bop_par(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_bop_par_aget, self->data_ptr);
}

static PyObject *
Outputs_get_capacity_factor(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Outputs_capacity_factor_nget, self->data_ptr);
}

static PyObject *
Outputs_get_conversion_factor(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Outputs_conversion_factor_nget, self->data_ptr);
}

static PyObject *
Outputs_get_dni_costh(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_dni_costh_aget, self->data_ptr);
}

static PyObject *
Outputs_get_eta(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_eta_aget, self->data_ptr);
}

static PyObject *
Outputs_get_fixed_par(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_fixed_par_aget, self->data_ptr);
}

static PyObject *
Outputs_get_gen(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_gen_aget, self->data_ptr);
}

static PyObject *
Outputs_get_hour(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_hour_aget, self->data_ptr);
}

static PyObject *
Outputs_get_htf_pump_power(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_htf_pump_power_aget, self->data_ptr);
}

static PyObject *
Outputs_get_kwh_per_kw(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Outputs_kwh_per_kw_nget, self->data_ptr);
}

static PyObject *
Outputs_get_m_dot_aux(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_m_dot_aux_aget, self->data_ptr);
}

static PyObject *
Outputs_get_m_dot_avail(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_m_dot_avail_aget, self->data_ptr);
}

static PyObject *
Outputs_get_m_dot_charge_field(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_m_dot_charge_field_aget, self->data_ptr);
}

static PyObject *
Outputs_get_m_dot_discharge_tank(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_m_dot_discharge_tank_aget, self->data_ptr);
}

static PyObject *
Outputs_get_m_dot_htf2(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_m_dot_htf2_aget, self->data_ptr);
}

static PyObject *
Outputs_get_m_dot_makeup(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_m_dot_makeup_aget, self->data_ptr);
}

static PyObject *
Outputs_get_m_dot_pb(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_m_dot_pb_aget, self->data_ptr);
}

static PyObject *
Outputs_get_mass_tank_cold(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_mass_tank_cold_aget, self->data_ptr);
}

static PyObject *
Outputs_get_mass_tank_hot(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_mass_tank_hot_aget, self->data_ptr);
}

static PyObject *
Outputs_get_month(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_month_aget, self->data_ptr);
}

static PyObject *
Outputs_get_monthly_Fuel_usage(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_monthly_Fuel_usage_aget, self->data_ptr);
}

static PyObject *
Outputs_get_monthly_W_cycle_gross(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_monthly_W_cycle_gross_aget, self->data_ptr);
}

static PyObject *
Outputs_get_monthly_energy(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_monthly_energy_aget, self->data_ptr);
}

static PyObject *
Outputs_get_monthly_m_dot_makeup(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_monthly_m_dot_makeup_aget, self->data_ptr);
}

static PyObject *
Outputs_get_monthly_q_abs_tot(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_monthly_q_abs_tot_aget, self->data_ptr);
}

static PyObject *
Outputs_get_monthly_q_avail(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_monthly_q_avail_aget, self->data_ptr);
}

static PyObject *
Outputs_get_monthly_q_dump(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_monthly_q_dump_aget, self->data_ptr);
}

static PyObject *
Outputs_get_monthly_q_inc_sf_tot(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_monthly_q_inc_sf_tot_aget, self->data_ptr);
}

static PyObject *
Outputs_get_monthly_q_pb(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_monthly_q_pb_aget, self->data_ptr);
}

static PyObject *
Outputs_get_monthly_q_to_tes(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_monthly_q_to_tes_aget, self->data_ptr);
}

static PyObject *
Outputs_get_pipe_header_P_dsn(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_pipe_header_P_dsn_aget, self->data_ptr);
}

static PyObject *
Outputs_get_pipe_header_T_dsn(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_pipe_header_T_dsn_aget, self->data_ptr);
}

static PyObject *
Outputs_get_pipe_header_diams(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_pipe_header_diams_aget, self->data_ptr);
}

static PyObject *
Outputs_get_pipe_header_expansions(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_pipe_header_expansions_aget, self->data_ptr);
}

static PyObject *
Outputs_get_pipe_header_lengths(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_pipe_header_lengths_aget, self->data_ptr);
}

static PyObject *
Outputs_get_pipe_header_mdot_dsn(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_pipe_header_mdot_dsn_aget, self->data_ptr);
}

static PyObject *
Outputs_get_pipe_header_vel_dsn(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_pipe_header_vel_dsn_aget, self->data_ptr);
}

static PyObject *
Outputs_get_pipe_header_wallthk(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_pipe_header_wallthk_aget, self->data_ptr);
}

static PyObject *
Outputs_get_pipe_loop_P_dsn(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_pipe_loop_P_dsn_aget, self->data_ptr);
}

static PyObject *
Outputs_get_pipe_loop_T_dsn(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_pipe_loop_T_dsn_aget, self->data_ptr);
}

static PyObject *
Outputs_get_pipe_runner_P_dsn(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_pipe_runner_P_dsn_aget, self->data_ptr);
}

static PyObject *
Outputs_get_pipe_runner_T_dsn(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_pipe_runner_T_dsn_aget, self->data_ptr);
}

static PyObject *
Outputs_get_pipe_runner_diams(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_pipe_runner_diams_aget, self->data_ptr);
}

static PyObject *
Outputs_get_pipe_runner_expansions(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_pipe_runner_expansions_aget, self->data_ptr);
}

static PyObject *
Outputs_get_pipe_runner_lengths(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_pipe_runner_lengths_aget, self->data_ptr);
}

static PyObject *
Outputs_get_pipe_runner_mdot_dsn(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_pipe_runner_mdot_dsn_aget, self->data_ptr);
}

static PyObject *
Outputs_get_pipe_runner_vel_dsn(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_pipe_runner_vel_dsn_aget, self->data_ptr);
}

static PyObject *
Outputs_get_pipe_runner_wallthk(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_pipe_runner_wallthk_aget, self->data_ptr);
}

static PyObject *
Outputs_get_pipe_sgs_P_dsn(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_pipe_sgs_P_dsn_aget, self->data_ptr);
}

static PyObject *
Outputs_get_pipe_sgs_T_dsn(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_pipe_sgs_T_dsn_aget, self->data_ptr);
}

static PyObject *
Outputs_get_pipe_sgs_diams(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_pipe_sgs_diams_aget, self->data_ptr);
}

static PyObject *
Outputs_get_pipe_sgs_mdot_dsn(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_pipe_sgs_mdot_dsn_aget, self->data_ptr);
}

static PyObject *
Outputs_get_pipe_sgs_vel_dsn(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_pipe_sgs_vel_dsn_aget, self->data_ptr);
}

static PyObject *
Outputs_get_pipe_sgs_wallthk(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_pipe_sgs_wallthk_aget, self->data_ptr);
}

static PyObject *
Outputs_get_pres(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_pres_aget, self->data_ptr);
}

static PyObject *
Outputs_get_q_abs_tot(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_q_abs_tot_aget, self->data_ptr);
}

static PyObject *
Outputs_get_q_avail(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_q_avail_aget, self->data_ptr);
}

static PyObject *
Outputs_get_q_dump(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_q_dump_aget, self->data_ptr);
}

static PyObject *
Outputs_get_q_inc_sf_tot(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_q_inc_sf_tot_aget, self->data_ptr);
}

static PyObject *
Outputs_get_q_loss_spec_tot(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_q_loss_spec_tot_aget, self->data_ptr);
}

static PyObject *
Outputs_get_q_loss_tot(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_q_loss_tot_aget, self->data_ptr);
}

static PyObject *
Outputs_get_q_pb(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_q_pb_aget, self->data_ptr);
}

static PyObject *
Outputs_get_q_to_tes(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_q_to_tes_aget, self->data_ptr);
}

static PyObject *
Outputs_get_qinc_costh(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_qinc_costh_aget, self->data_ptr);
}

static PyObject *
Outputs_get_recirculating(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_recirculating_aget, self->data_ptr);
}

static PyObject *
Outputs_get_solazi(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_solazi_aget, self->data_ptr);
}

static PyObject *
Outputs_get_solzen(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_solzen_aget, self->data_ptr);
}

static PyObject *
Outputs_get_system_heat_rate(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughPhysical_Outputs_system_heat_rate_nget, self->data_ptr);
}

static PyObject *
Outputs_get_tank_losses(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_tank_losses_aget, self->data_ptr);
}

static PyObject *
Outputs_get_tdry(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_tdry_aget, self->data_ptr);
}

static PyObject *
Outputs_get_tou_value(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_tou_value_aget, self->data_ptr);
}

static PyObject *
Outputs_get_twet(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_twet_aget, self->data_ptr);
}

static PyObject *
Outputs_get_vol_tank_cold_fin(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_vol_tank_cold_fin_aget, self->data_ptr);
}

static PyObject *
Outputs_get_vol_tank_hot_fin(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_vol_tank_hot_fin_aget, self->data_ptr);
}

static PyObject *
Outputs_get_vol_tank_total(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_vol_tank_total_aget, self->data_ptr);
}

static PyObject *
Outputs_get_wspd(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughPhysical_Outputs_wspd_aget, self->data_ptr);
}

static PyGetSetDef Outputs_getset[] = {
{"CosTh_ave", (getter)Outputs_get_CosTh_ave,(setter)0,
	PyDoc_STR("*sequence*: Field collector cosine efficiency"),
 	NULL},
{"DP_tot", (getter)Outputs_get_DP_tot,(setter)0,
	PyDoc_STR("*sequence*: Field HTF pressure drop total [bar]"),
 	NULL},
{"E_bal_startup", (getter)Outputs_get_E_bal_startup,(setter)0,
	PyDoc_STR("*sequence*: Field HTF energy inertial (consumed) [MWht]"),
 	NULL},
{"EndLoss_ave", (getter)Outputs_get_EndLoss_ave,(setter)0,
	PyDoc_STR("*sequence*: Field collector optical end loss"),
 	NULL},
{"EqOpteff", (getter)Outputs_get_EqOpteff,(setter)0,
	PyDoc_STR("*sequence*: Field collector optical efficiency"),
 	NULL},
{"Fuel_usage", (getter)Outputs_get_Fuel_usage,(setter)0,
	PyDoc_STR("*sequence*: Fossil fuel usage (all subsystems) [MMBTU]"),
 	NULL},
{"IAM_ave", (getter)Outputs_get_IAM_ave,(setter)0,
	PyDoc_STR("*sequence*: Field collector incidence angle modifier"),
 	NULL},
{"Pipe_hl", (getter)Outputs_get_Pipe_hl,(setter)0,
	PyDoc_STR("*sequence*: Field thermal power header pipe losses [MWt]"),
 	NULL},
{"Q_aux_backup", (getter)Outputs_get_Q_aux_backup,(setter)0,
	PyDoc_STR("*sequence*: Fossil thermal power produced [MWt]"),
 	NULL},
{"Q_par_sf_fp", (getter)Outputs_get_Q_par_sf_fp,(setter)0,
	PyDoc_STR("*sequence*: Parasitic thermal field freeze protection [MWt]"),
 	NULL},
{"Q_par_tes_fp", (getter)Outputs_get_Q_par_tes_fp,(setter)0,
	PyDoc_STR("*sequence*: Parasitic thermal TES freeze protection [MWt]"),
 	NULL},
{"RowShadow_ave", (getter)Outputs_get_RowShadow_ave,(setter)0,
	PyDoc_STR("*sequence*: Field collector row shadowing loss"),
 	NULL},
{"SCA_par_tot", (getter)Outputs_get_SCA_par_tot,(setter)0,
	PyDoc_STR("*sequence*: Parasitic power field collector drives [MWe]"),
 	NULL},
{"SCAs_def", (getter)Outputs_get_SCAs_def,(setter)0,
	PyDoc_STR("*sequence*: Field collector fraction of focused SCA's"),
 	NULL},
{"T_field_in", (getter)Outputs_get_T_field_in,(setter)0,
	PyDoc_STR("*sequence*: Field HTF temperature collector inlet [C]"),
 	NULL},
{"T_pb_in", (getter)Outputs_get_T_pb_in,(setter)0,
	PyDoc_STR("*sequence*: Cycle HTF temperature in (hot) [C]"),
 	NULL},
{"T_pb_out", (getter)Outputs_get_T_pb_out,(setter)0,
	PyDoc_STR("*sequence*: Cycle HTF temperature out (cold) [C]"),
 	NULL},
{"T_sys_c", (getter)Outputs_get_T_sys_c,(setter)0,
	PyDoc_STR("*sequence*: Field HTF temperature cold header inlet [C]"),
 	NULL},
{"T_sys_h", (getter)Outputs_get_T_sys_h,(setter)0,
	PyDoc_STR("*sequence*: Field HTF temperature hot header outlet [C]"),
 	NULL},
{"T_tank_cold_fin", (getter)Outputs_get_T_tank_cold_fin,(setter)0,
	PyDoc_STR("*sequence*: TES HTF temperature in cold tank [C]"),
 	NULL},
{"T_tank_cold_in", (getter)Outputs_get_T_tank_cold_in,(setter)0,
	PyDoc_STR("*sequence*: TES HTF temperature cold tank inlet [C]"),
 	NULL},
{"T_tank_hot_fin", (getter)Outputs_get_T_tank_hot_fin,(setter)0,
	PyDoc_STR("*sequence*: TES HTF temperature in hot tank [C]"),
 	NULL},
{"T_tank_hot_in", (getter)Outputs_get_T_tank_hot_in,(setter)0,
	PyDoc_STR("*sequence*: TES HTF temperature hot tank inlet [C]"),
 	NULL},
{"Theta_ave", (getter)Outputs_get_Theta_ave,(setter)0,
	PyDoc_STR("*sequence*: Field collector solar incidence angle [deg]"),
 	NULL},
{"Ts_cold", (getter)Outputs_get_Ts_cold,(setter)0,
	PyDoc_STR("*sequence*: TES HTF temperature HX field side cold [C]"),
 	NULL},
{"Ts_hot", (getter)Outputs_get_Ts_hot,(setter)0,
	PyDoc_STR("*sequence*: TES HTF temperature HX field side hot [C]"),
 	NULL},
{"W_cool_par", (getter)Outputs_get_W_cool_par,(setter)0,
	PyDoc_STR("*sequence*: Parasitic power condenser operation [MWe]"),
 	NULL},
{"W_cycle_gross", (getter)Outputs_get_W_cycle_gross,(setter)0,
	PyDoc_STR("*sequence*: Cycle electrical power output (gross) [MWe]"),
 	NULL},
{"W_dot_pump", (getter)Outputs_get_W_dot_pump,(setter)0,
	PyDoc_STR("*sequence*: Parasitic power solar field HTF pump [MWe]"),
 	NULL},
{"W_net", (getter)Outputs_get_W_net,(setter)0,
	PyDoc_STR("*sequence*: Cycle electrical power output (net) [MWe]"),
 	NULL},
{"annual_W_cycle_gross", (getter)Outputs_get_annual_W_cycle_gross,(setter)0,
	PyDoc_STR("*float*: Electrical source - Power cycle gross output [MWhe]"),
 	NULL},
{"annual_energy", (getter)Outputs_get_annual_energy,(setter)0,
	PyDoc_STR("*float*: Annual Energy [kWh]"),
 	NULL},
{"annual_fuel_usage", (getter)Outputs_get_annual_fuel_usage,(setter)0,
	PyDoc_STR("*float*: Annual fuel usage [kWht]"),
 	NULL},
{"annual_q_abs_tot", (getter)Outputs_get_annual_q_abs_tot,(setter)0,
	PyDoc_STR("*float*: Total absorbed energy [MWht]"),
 	NULL},
{"annual_q_aux", (getter)Outputs_get_annual_q_aux,(setter)0,
	PyDoc_STR("*float*: Total fossil fuel usage by all plant subsystems [MMBTU]"),
 	NULL},
{"annual_q_avail", (getter)Outputs_get_annual_q_avail,(setter)0,
	PyDoc_STR("*float*: Thermal power produced by the field [MWht]"),
 	NULL},
{"annual_q_dump", (getter)Outputs_get_annual_q_dump,(setter)0,
	PyDoc_STR("*float*: Dumped thermal energy [MWht]"),
 	NULL},
{"annual_q_inc_sf_tot", (getter)Outputs_get_annual_q_inc_sf_tot,(setter)0,
	PyDoc_STR("*float*: Total power incident on the field [MWht]"),
 	NULL},
{"annual_q_pb", (getter)Outputs_get_annual_q_pb,(setter)0,
	PyDoc_STR("*float*: Thermal energy to the power block [MWht]"),
 	NULL},
{"annual_q_to_tes", (getter)Outputs_get_annual_q_to_tes,(setter)0,
	PyDoc_STR("*float*: Thermal energy into storage [MWht]"),
 	NULL},
{"annual_total_water_use", (getter)Outputs_get_annual_total_water_use,(setter)0,
	PyDoc_STR("*float*: Total Annual Water Usage: cycle + mirror washing [m3]"),
 	NULL},
{"aux_par", (getter)Outputs_get_aux_par,(setter)0,
	PyDoc_STR("*sequence*: Parasitic power auxiliary heater operation [MWe]"),
 	NULL},
{"beam", (getter)Outputs_get_beam,(setter)0,
	PyDoc_STR("*sequence*: Resource Beam normal irradiance [W/m2]"),
 	NULL},
{"bop_par", (getter)Outputs_get_bop_par,(setter)0,
	PyDoc_STR("*sequence*: Parasitic power generation-dependent load [MWe]"),
 	NULL},
{"capacity_factor", (getter)Outputs_get_capacity_factor,(setter)0,
	PyDoc_STR("*float*: Capacity factor [%]"),
 	NULL},
{"conversion_factor", (getter)Outputs_get_conversion_factor,(setter)0,
	PyDoc_STR("*float*: Gross to Net Conversion Factor [%]"),
 	NULL},
{"dni_costh", (getter)Outputs_get_dni_costh,(setter)0,
	PyDoc_STR("*sequence*: Field collector DNI-cosine product [W/m2]"),
 	NULL},
{"eta", (getter)Outputs_get_eta,(setter)0,
	PyDoc_STR("*sequence*: Cycle efficiency (gross)"),
 	NULL},
{"fixed_par", (getter)Outputs_get_fixed_par,(setter)0,
	PyDoc_STR("*sequence*: Parasitic power fixed load [MWe]"),
 	NULL},
{"gen", (getter)Outputs_get_gen,(setter)0,
	PyDoc_STR("*sequence*: System power generated [kW]"),
 	NULL},
{"hour", (getter)Outputs_get_hour,(setter)0,
	PyDoc_STR("*sequence*: Resource Hour of Day"),
 	NULL},
{"htf_pump_power", (getter)Outputs_get_htf_pump_power,(setter)0,
	PyDoc_STR("*sequence*: Parasitic power TES and Cycle HTF pump [MWe]"),
 	NULL},
{"kwh_per_kw", (getter)Outputs_get_kwh_per_kw,(setter)0,
	PyDoc_STR("*float*: First year kWh/kW [kWh/kW]"),
 	NULL},
{"m_dot_aux", (getter)Outputs_get_m_dot_aux,(setter)0,
	PyDoc_STR("*sequence*: Fossil HTF mass flow rate [kg/hr]"),
 	NULL},
{"m_dot_avail", (getter)Outputs_get_m_dot_avail,(setter)0,
	PyDoc_STR("*sequence*: Field HTF mass flow rate total [kg/hr]"),
 	NULL},
{"m_dot_charge_field", (getter)Outputs_get_m_dot_charge_field,(setter)0,
	PyDoc_STR("*sequence*: TES HTF mass flow rate - field side of HX [kg/hr]"),
 	NULL},
{"m_dot_discharge_tank", (getter)Outputs_get_m_dot_discharge_tank,(setter)0,
	PyDoc_STR("*sequence*: TES HTF mass flow rate - storage side of HX [kg/hr]"),
 	NULL},
{"m_dot_htf2", (getter)Outputs_get_m_dot_htf2,(setter)0,
	PyDoc_STR("*sequence*: Field HTF mass flow rate loop [kg/s]"),
 	NULL},
{"m_dot_makeup", (getter)Outputs_get_m_dot_makeup,(setter)0,
	PyDoc_STR("*sequence*: Cycle cooling water mass flow rate - makeup [kg/hr]"),
 	NULL},
{"m_dot_pb", (getter)Outputs_get_m_dot_pb,(setter)0,
	PyDoc_STR("*sequence*: Cycle HTF mass flow rate [kg/hr]"),
 	NULL},
{"mass_tank_cold", (getter)Outputs_get_mass_tank_cold,(setter)0,
	PyDoc_STR("*sequence*: TES HTF mass in cold tank [kg]"),
 	NULL},
{"mass_tank_hot", (getter)Outputs_get_mass_tank_hot,(setter)0,
	PyDoc_STR("*sequence*: TES HTF mass in hot tank [kg]"),
 	NULL},
{"month", (getter)Outputs_get_month,(setter)0,
	PyDoc_STR("*sequence*: Resource Month"),
 	NULL},
{"monthly_Fuel_usage", (getter)Outputs_get_monthly_Fuel_usage,(setter)0,
	PyDoc_STR("*sequence*: Total fossil fuel usage by all plant subsystems [MMBTU]"),
 	NULL},
{"monthly_W_cycle_gross", (getter)Outputs_get_monthly_W_cycle_gross,(setter)0,
	PyDoc_STR("*sequence*: Electrical source - Power cycle gross output [MWhe]"),
 	NULL},
{"monthly_energy", (getter)Outputs_get_monthly_energy,(setter)0,
	PyDoc_STR("*sequence*: Monthly Energy [kWh]"),
 	NULL},
{"monthly_m_dot_makeup", (getter)Outputs_get_monthly_m_dot_makeup,(setter)0,
	PyDoc_STR("*sequence*: Cooling water makeup flow rate [kg/hr]"),
 	NULL},
{"monthly_q_abs_tot", (getter)Outputs_get_monthly_q_abs_tot,(setter)0,
	PyDoc_STR("*sequence*: Total absorbed energy [MWht]"),
 	NULL},
{"monthly_q_avail", (getter)Outputs_get_monthly_q_avail,(setter)0,
	PyDoc_STR("*sequence*: Thermal power produced by the field [MWht]"),
 	NULL},
{"monthly_q_dump", (getter)Outputs_get_monthly_q_dump,(setter)0,
	PyDoc_STR("*sequence*: Dumped thermal energy [MWht]"),
 	NULL},
{"monthly_q_inc_sf_tot", (getter)Outputs_get_monthly_q_inc_sf_tot,(setter)0,
	PyDoc_STR("*sequence*: Total power incident on the field [MWht]"),
 	NULL},
{"monthly_q_pb", (getter)Outputs_get_monthly_q_pb,(setter)0,
	PyDoc_STR("*sequence*: Thermal energy to the power block [MWht]"),
 	NULL},
{"monthly_q_to_tes", (getter)Outputs_get_monthly_q_to_tes,(setter)0,
	PyDoc_STR("*sequence*: Thermal energy into storage [MWht]"),
 	NULL},
{"pipe_header_P_dsn", (getter)Outputs_get_pipe_header_P_dsn,(setter)0,
	PyDoc_STR("*sequence*: Field piping header pressure at design [bar]"),
 	NULL},
{"pipe_header_T_dsn", (getter)Outputs_get_pipe_header_T_dsn,(setter)0,
	PyDoc_STR("*sequence*: Field piping header temperature at design [C]"),
 	NULL},
{"pipe_header_diams", (getter)Outputs_get_pipe_header_diams,(setter)0,
	PyDoc_STR("*sequence*: Field piping header diameters [m]"),
 	NULL},
{"pipe_header_expansions", (getter)Outputs_get_pipe_header_expansions,(setter)0,
	PyDoc_STR("*sequence*: Number of field piping header expansions [-]"),
 	NULL},
{"pipe_header_lengths", (getter)Outputs_get_pipe_header_lengths,(setter)0,
	PyDoc_STR("*sequence*: Field piping header lengths [m]"),
 	NULL},
{"pipe_header_mdot_dsn", (getter)Outputs_get_pipe_header_mdot_dsn,(setter)0,
	PyDoc_STR("*sequence*: Field piping header mass flow at design [kg/s]"),
 	NULL},
{"pipe_header_vel_dsn", (getter)Outputs_get_pipe_header_vel_dsn,(setter)0,
	PyDoc_STR("*sequence*: Field piping header velocity at design [m/s]"),
 	NULL},
{"pipe_header_wallthk", (getter)Outputs_get_pipe_header_wallthk,(setter)0,
	PyDoc_STR("*sequence*: Field piping header wall thicknesses [m]"),
 	NULL},
{"pipe_loop_P_dsn", (getter)Outputs_get_pipe_loop_P_dsn,(setter)0,
	PyDoc_STR("*sequence*: Field piping loop pressure at design [bar]"),
 	NULL},
{"pipe_loop_T_dsn", (getter)Outputs_get_pipe_loop_T_dsn,(setter)0,
	PyDoc_STR("*sequence*: Field piping loop temperature at design [C]"),
 	NULL},
{"pipe_runner_P_dsn", (getter)Outputs_get_pipe_runner_P_dsn,(setter)0,
	PyDoc_STR("*sequence*: Field piping runner pressure at design [bar]"),
 	NULL},
{"pipe_runner_T_dsn", (getter)Outputs_get_pipe_runner_T_dsn,(setter)0,
	PyDoc_STR("*sequence*: Field piping runner temperature at design [C]"),
 	NULL},
{"pipe_runner_diams", (getter)Outputs_get_pipe_runner_diams,(setter)0,
	PyDoc_STR("*sequence*: Field piping runner diameters [m]"),
 	NULL},
{"pipe_runner_expansions", (getter)Outputs_get_pipe_runner_expansions,(setter)0,
	PyDoc_STR("*sequence*: Number of field piping runner expansions [-]"),
 	NULL},
{"pipe_runner_lengths", (getter)Outputs_get_pipe_runner_lengths,(setter)0,
	PyDoc_STR("*sequence*: Field piping runner lengths [m]"),
 	NULL},
{"pipe_runner_mdot_dsn", (getter)Outputs_get_pipe_runner_mdot_dsn,(setter)0,
	PyDoc_STR("*sequence*: Field piping runner mass flow at design [kg/s]"),
 	NULL},
{"pipe_runner_vel_dsn", (getter)Outputs_get_pipe_runner_vel_dsn,(setter)0,
	PyDoc_STR("*sequence*: Field piping runner velocity at design [m/s]"),
 	NULL},
{"pipe_runner_wallthk", (getter)Outputs_get_pipe_runner_wallthk,(setter)0,
	PyDoc_STR("*sequence*: Field piping runner wall thicknesses [m]"),
 	NULL},
{"pipe_sgs_P_dsn", (getter)Outputs_get_pipe_sgs_P_dsn,(setter)0,
	PyDoc_STR("*sequence*: Pressure in SGS pipes at design conditions [bar]"),
 	NULL},
{"pipe_sgs_T_dsn", (getter)Outputs_get_pipe_sgs_T_dsn,(setter)0,
	PyDoc_STR("*sequence*: Temperature in SGS pipes at design conditions [C]"),
 	NULL},
{"pipe_sgs_diams", (getter)Outputs_get_pipe_sgs_diams,(setter)0,
	PyDoc_STR("*sequence*: Pipe diameters in SGS [m]"),
 	NULL},
{"pipe_sgs_mdot_dsn", (getter)Outputs_get_pipe_sgs_mdot_dsn,(setter)0,
	PyDoc_STR("*sequence*: Mass flow SGS pipes at design conditions [kg/s]"),
 	NULL},
{"pipe_sgs_vel_dsn", (getter)Outputs_get_pipe_sgs_vel_dsn,(setter)0,
	PyDoc_STR("*sequence*: Velocity in SGS pipes at design conditions [m/s]"),
 	NULL},
{"pipe_sgs_wallthk", (getter)Outputs_get_pipe_sgs_wallthk,(setter)0,
	PyDoc_STR("*sequence*: Pipe wall thickness in SGS [m]"),
 	NULL},
{"pres", (getter)Outputs_get_pres,(setter)0,
	PyDoc_STR("*sequence*: Resource Pressure [mbar]"),
 	NULL},
{"q_abs_tot", (getter)Outputs_get_q_abs_tot,(setter)0,
	PyDoc_STR("*sequence*: Field thermal power absorbed [MWt]"),
 	NULL},
{"q_avail", (getter)Outputs_get_q_avail,(setter)0,
	PyDoc_STR("*sequence*: Field thermal power produced [MWt]"),
 	NULL},
{"q_dump", (getter)Outputs_get_q_dump,(setter)0,
	PyDoc_STR("*sequence*: Field thermal power dumped [MWt]"),
 	NULL},
{"q_inc_sf_tot", (getter)Outputs_get_q_inc_sf_tot,(setter)0,
	PyDoc_STR("*sequence*: Field thermal power incident [MWt]"),
 	NULL},
{"q_loss_spec_tot", (getter)Outputs_get_q_loss_spec_tot,(setter)0,
	PyDoc_STR("*sequence*: Field thermal power avg. receiver loss [W/m]"),
 	NULL},
{"q_loss_tot", (getter)Outputs_get_q_loss_tot,(setter)0,
	PyDoc_STR("*sequence*: Field thermal power receiver loss [MWt]"),
 	NULL},
{"q_pb", (getter)Outputs_get_q_pb,(setter)0,
	PyDoc_STR("*sequence*: Cycle thermal power input [MWt]"),
 	NULL},
{"q_to_tes", (getter)Outputs_get_q_to_tes,(setter)0,
	PyDoc_STR("*sequence*: TES thermal energy into storage [MWt]"),
 	NULL},
{"qinc_costh", (getter)Outputs_get_qinc_costh,(setter)0,
	PyDoc_STR("*sequence*: Field thermal power incident after cosine [MWt]"),
 	NULL},
{"recirculating", (getter)Outputs_get_recirculating,(setter)0,
	PyDoc_STR("*sequence*: Field recirculating (bypass valve open) [-]"),
 	NULL},
{"solazi", (getter)Outputs_get_solazi,(setter)0,
	PyDoc_STR("*sequence*: Resource Solar Azimuth [deg]"),
 	NULL},
{"solzen", (getter)Outputs_get_solzen,(setter)0,
	PyDoc_STR("*sequence*: Resource Solar Zenith [deg]"),
 	NULL},
{"system_heat_rate", (getter)Outputs_get_system_heat_rate,(setter)0,
	PyDoc_STR("*float*: System heat rate [MMBtu/MWh]"),
 	NULL},
{"tank_losses", (getter)Outputs_get_tank_losses,(setter)0,
	PyDoc_STR("*sequence*: TES thermal losses from tank(s) [MWt]"),
 	NULL},
{"tdry", (getter)Outputs_get_tdry,(setter)0,
	PyDoc_STR("*sequence*: Resource Dry bulb temperature [C]"),
 	NULL},
{"tou_value", (getter)Outputs_get_tou_value,(setter)0,
	PyDoc_STR("*sequence*: Resource Time-of-use value"),
 	NULL},
{"twet", (getter)Outputs_get_twet,(setter)0,
	PyDoc_STR("*sequence*: Resource Wet bulb temperature [C]"),
 	NULL},
{"vol_tank_cold_fin", (getter)Outputs_get_vol_tank_cold_fin,(setter)0,
	PyDoc_STR("*sequence*: TES HTF volume in cold tank [m3]"),
 	NULL},
{"vol_tank_hot_fin", (getter)Outputs_get_vol_tank_hot_fin,(setter)0,
	PyDoc_STR("*sequence*: TES HTF volume in hot tank [m3]"),
 	NULL},
{"vol_tank_total", (getter)Outputs_get_vol_tank_total,(setter)0,
	PyDoc_STR("*sequence*: TES HTF volume total [m3]"),
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
		"TcstroughPhysical.Outputs",             /*tp_name*/
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
 * TcstroughPhysical
 */

typedef struct {
	PyObject_HEAD
	PyObject            *x_attr;        /* Attributes dictionary */
	SAM_TcstroughPhysical   data_ptr;
} TcstroughPhysicalObject;

static PyTypeObject TcstroughPhysical_Type;

#define TcstroughPhysicalObject_Check(v)      (Py_TYPE(v) == &TcstroughPhysical_Type)

static TcstroughPhysicalObject *
newTcstroughPhysicalObject(void* data_ptr)
{
	TcstroughPhysicalObject *self;
	self = PyObject_New(TcstroughPhysicalObject, &TcstroughPhysical_Type);

	PySAM_TECH_ATTR("TcstroughPhysical", SAM_TcstroughPhysical_construct)

	PyObject* Weather_obj = Weather_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "Weather", Weather_obj);
	Py_DECREF(Weather_obj);

	PyObject* Trough_obj = Trough_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "Trough", Trough_obj);
	Py_DECREF(Trough_obj);

	PyObject* SolarField_obj = SolarField_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "SolarField", SolarField_obj);
	Py_DECREF(SolarField_obj);

	PyObject* Controller_obj = Controller_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "Controller", Controller_obj);
	Py_DECREF(Controller_obj);

	PyObject* TouTranslator_obj = TouTranslator_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "TouTranslator", TouTranslator_obj);
	Py_DECREF(TouTranslator_obj);

	PyObject* Powerblock_obj = Powerblock_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "Powerblock", Powerblock_obj);
	Py_DECREF(Powerblock_obj);

	PyObject* UserDefinedPC_obj = UserDefinedPC_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "UserDefinedPC", UserDefinedPC_obj);
	Py_DECREF(UserDefinedPC_obj);

	PyObject* Enet_obj = Enet_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "Enet", Enet_obj);
	Py_DECREF(Enet_obj);

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

/* TcstroughPhysical methods */

static void
TcstroughPhysical_dealloc(TcstroughPhysicalObject *self)
{
	Py_XDECREF(self->x_attr);
	SAM_TcstroughPhysical_destruct(self->data_ptr);
	PyObject_Del(self);
}


static PyObject *
TcstroughPhysical_execute(TcstroughPhysicalObject *self, PyObject *args)
{
	int verbosity = 0;

	if (!PyArg_ParseTuple(args, "|i", &verbosity))
		return NULL;

	SAM_error error = new_error();
	SAM_TcstroughPhysical_execute(self->data_ptr, verbosity, &error);
	if (PySAM_has_error(error )) return NULL;

	Py_INCREF(Py_None);
	return Py_None;
}


static PyObject *
TcstroughPhysical_assign(TcstroughPhysicalObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_nested_dict((PyObject*)self, self->x_attr, self->data_ptr, dict, "TcstroughPhysical"))
		return NULL;

	Py_INCREF(Py_None);
	return Py_None;
}


static PyObject *
TcstroughPhysical_export(TcstroughPhysicalObject *self, PyObject *args)
{
	return PySAM_export_to_nested_dict((PyObject *) self, self->x_attr);
}

static PyMethodDef TcstroughPhysical_methods[] = {
		{"execute",            (PyCFunction)TcstroughPhysical_execute,  METH_VARARGS,
				PyDoc_STR("execute(int verbosity) -> None\n Execute simulation with verbosity level 0 (default) or 1")},
		{"assign",            (PyCFunction)TcstroughPhysical_assign,  METH_VARARGS,
				PyDoc_STR("assign(dict) -> None\n Assign attributes from nested dictionary, except for Outputs\n\n``nested_dict = { 'Weather': { var: val, ...}, ...}``")},
		{"export",            (PyCFunction)TcstroughPhysical_export,  METH_VARARGS,
				PyDoc_STR("export() -> dict\n Export attributes into nested dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
TcstroughPhysical_getattro(TcstroughPhysicalObject *self, PyObject *name)
{
	return PySAM_get_attr((PyObject*) self, (PyObject*) self->x_attr, name);
}

static int
TcstroughPhysical_setattr(TcstroughPhysicalObject *self, const char *name, PyObject *v)
{
	return PySAM_set_attr((PyObject*)self, (PyObject*)self->x_attr, name, v);
}

static PyTypeObject TcstroughPhysical_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"TcstroughPhysical",            /*tp_name*/
		sizeof(TcstroughPhysicalObject),/*tp_basicsize*/
		0,                          /*tp_itemsize*/
		/* methods */
		(destructor)TcstroughPhysical_dealloc,    /*tp_dealloc*/
		0,                          /*tp_print*/
		(getattrfunc)0,             /*tp_getattr*/
		(setattrfunc)TcstroughPhysical_setattr,   /*tp_setattr*/
		0,                          /*tp_reserved*/
		0,                          /*tp_repr*/
		0,                          /*tp_as_number*/
		0,                          /*tp_as_sequence*/
		0,                          /*tp_as_mapping*/
		0,                          /*tp_hash*/
		0,                          /*tp_call*/
		0,                          /*tp_str*/
		(getattrofunc)TcstroughPhysical_getattro, /*tp_getattro*/
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
		TcstroughPhysical_methods,      /*tp_methods*/
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


/* Function of no arguments returning new TcstroughPhysical object */

static PyObject *
TcstroughPhysical_new(PyObject *self, PyObject *args)
{
	TcstroughPhysicalObject *rv;
	rv = newTcstroughPhysicalObject(0);
	if (rv == NULL)
		return NULL;
	return (PyObject *)rv;
}

static PyObject *
TcstroughPhysical_wrap(PyObject *self, PyObject *args)
{
	TcstroughPhysicalObject *rv;
	long long int ptr = 0;  // 64 bit arch
	if (!PyArg_ParseTuple(args, "L:wrap", &ptr)){
		PyErr_BadArgument();
		return NULL;
	}
	rv = newTcstroughPhysicalObject((void*)ptr);
	if (rv == NULL)
		return NULL;
	return (PyObject *)rv;
}

static PyObject *
TcstroughPhysical_default(PyObject *self, PyObject *args)
{
	TcstroughPhysicalObject *rv;
	char* def = 0;
	if (!PyArg_ParseTuple(args, "s:default", &def)){
		PyErr_BadArgument();
		return NULL;
	}
	rv = newTcstroughPhysicalObject(0);
	if (rv == NULL)
		return NULL;

	PySAM_load_defaults((PyObject*)rv, rv->x_attr, rv->data_ptr, "TcstroughPhysical", def);

	return (PyObject *)rv;
}

/* ---------- */


/* List of functions defined in the module */

static PyMethodDef TcstroughPhysicalModule_methods[] = {
		{"new",             TcstroughPhysical_new,         METH_VARARGS,
				PyDoc_STR("new() -> TcstroughPhysical")},
		{"default",             TcstroughPhysical_default,         METH_VARARGS,
				PyDoc_STR("default(config) -> TcstroughPhysical\n\nUse financial model-specific default attributes\n"
				"config options:\n\n- \"PhysicalTroughAllEquityPartnershipFlip\"\n- \"PhysicalTroughCommercial\"\n- \"PhysicalTroughIndependentPowerProducer\"\n- \"PhysicalTroughLCOECalculator\"\n- \"PhysicalTroughLeveragedPartnershipFlip\"\n- \"PhysicalTroughNone\"\n- \"PhysicalTroughSaleLeaseback\"\n- \"PhysicalTroughSingleOwner\"")},
		{"wrap",             TcstroughPhysical_wrap,         METH_VARARGS,
				PyDoc_STR("wrap(ssc_data_t) -> TcstroughPhysical\n\nUse existing PySSC data\n\n.. warning::\n\n	Do not call PySSC.data_free on the ssc_data_t provided to ``wrap``")},
		{NULL,              NULL}           /* sentinel */
};

PyDoc_STRVAR(module_doc,
			 "CSP parabolic trough model based on heat transfer and thermodynamic principles for power generation");


static int
TcstroughPhysicalModule_exec(PyObject *m)
{
	/* Finalize the type object including setting type of the new type
	 * object; doing it here is required for portability, too. */

	if (PySAM_load_lib(m) < 0) goto fail;
	if (PySAM_init_error(m) < 0) goto fail;

	TcstroughPhysical_Type.tp_dict = PyDict_New();
	if (!TcstroughPhysical_Type.tp_dict) { goto fail; }

	/// Add the AdjustmentFactors type object to TcstroughPhysical_Type
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
	PyDict_SetItemString(TcstroughPhysical_Type.tp_dict,
						 "AdjustmentFactors",
						 (PyObject*)AdjustmentFactors_Type);
	Py_DECREF(&AdjustmentFactors_Type);
	Py_XDECREF(AdjustmentFactors_Type);

	/// Add the Weather type object to TcstroughPhysical_Type
	if (PyType_Ready(&Weather_Type) < 0) { goto fail; }
	PyDict_SetItemString(TcstroughPhysical_Type.tp_dict,
				"Weather",
				(PyObject*)&Weather_Type);
	Py_DECREF(&Weather_Type);

	/// Add the Trough type object to TcstroughPhysical_Type
	if (PyType_Ready(&Trough_Type) < 0) { goto fail; }
	PyDict_SetItemString(TcstroughPhysical_Type.tp_dict,
				"Trough",
				(PyObject*)&Trough_Type);
	Py_DECREF(&Trough_Type);

	/// Add the SolarField type object to TcstroughPhysical_Type
	if (PyType_Ready(&SolarField_Type) < 0) { goto fail; }
	PyDict_SetItemString(TcstroughPhysical_Type.tp_dict,
				"SolarField",
				(PyObject*)&SolarField_Type);
	Py_DECREF(&SolarField_Type);

	/// Add the Controller type object to TcstroughPhysical_Type
	if (PyType_Ready(&Controller_Type) < 0) { goto fail; }
	PyDict_SetItemString(TcstroughPhysical_Type.tp_dict,
				"Controller",
				(PyObject*)&Controller_Type);
	Py_DECREF(&Controller_Type);

	/// Add the TouTranslator type object to TcstroughPhysical_Type
	if (PyType_Ready(&TouTranslator_Type) < 0) { goto fail; }
	PyDict_SetItemString(TcstroughPhysical_Type.tp_dict,
				"TouTranslator",
				(PyObject*)&TouTranslator_Type);
	Py_DECREF(&TouTranslator_Type);

	/// Add the Powerblock type object to TcstroughPhysical_Type
	if (PyType_Ready(&Powerblock_Type) < 0) { goto fail; }
	PyDict_SetItemString(TcstroughPhysical_Type.tp_dict,
				"Powerblock",
				(PyObject*)&Powerblock_Type);
	Py_DECREF(&Powerblock_Type);

	/// Add the UserDefinedPC type object to TcstroughPhysical_Type
	if (PyType_Ready(&UserDefinedPC_Type) < 0) { goto fail; }
	PyDict_SetItemString(TcstroughPhysical_Type.tp_dict,
				"UserDefinedPC",
				(PyObject*)&UserDefinedPC_Type);
	Py_DECREF(&UserDefinedPC_Type);

	/// Add the Enet type object to TcstroughPhysical_Type
	if (PyType_Ready(&Enet_Type) < 0) { goto fail; }
	PyDict_SetItemString(TcstroughPhysical_Type.tp_dict,
				"Enet",
				(PyObject*)&Enet_Type);
	Py_DECREF(&Enet_Type);

	/// Add the Outputs type object to TcstroughPhysical_Type
	if (PyType_Ready(&Outputs_Type) < 0) { goto fail; }
	PyDict_SetItemString(TcstroughPhysical_Type.tp_dict,
				"Outputs",
				(PyObject*)&Outputs_Type);
	Py_DECREF(&Outputs_Type);

	/// Add the TcstroughPhysical type object to the module
	if (PyType_Ready(&TcstroughPhysical_Type) < 0) { goto fail; }
	PyModule_AddObject(m,
				"TcstroughPhysical",
				(PyObject*)&TcstroughPhysical_Type);

	return 0;
	fail:
	Py_XDECREF(m);
	return -1;
}

static struct PyModuleDef_Slot TcstroughPhysicalModule_slots[] = {
		{Py_mod_exec, TcstroughPhysicalModule_exec},
		{0, NULL},
};

static struct PyModuleDef TcstroughPhysicalModule = {
		PyModuleDef_HEAD_INIT,
		"TcstroughPhysical",
		module_doc,
		0,
		TcstroughPhysicalModule_methods,
		TcstroughPhysicalModule_slots,
		NULL,
		NULL,
		NULL
};

/* Export function for the module */

PyMODINIT_FUNC
PyInit_TcstroughPhysical(void)
{
	return PyModuleDef_Init(&TcstroughPhysicalModule);
}