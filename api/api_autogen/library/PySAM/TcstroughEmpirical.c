#include <Python.h>

#include <SAM_TcstroughEmpirical.h>
#include <SAM_api.h>

#include "PySAM_utils.h"



	/*
	 * Weather Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_TcstroughEmpirical   data_ptr;
} WeatherObject;

static PyTypeObject Weather_Type;

static PyObject *
Weather_new(SAM_TcstroughEmpirical data_ptr)
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

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "TcstroughEmpirical", "Weather")){
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
	return PySAM_double_getter(SAM_TcstroughEmpirical_Weather_azimuth_nget, self->data_ptr);
}

static int
Weather_set_azimuth(WeatherObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Weather_azimuth_nset, self->data_ptr);
}

static PyObject *
Weather_get_file_name(WeatherObject *self, void *closure)
{
	return PySAM_string_getter(SAM_TcstroughEmpirical_Weather_file_name_sget, self->data_ptr);
}

static int
Weather_set_file_name(WeatherObject *self, PyObject *value, void *closure)
{
	return PySAM_string_setter(value, SAM_TcstroughEmpirical_Weather_file_name_sset, self->data_ptr);
}

static PyObject *
Weather_get_tilt(WeatherObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Weather_tilt_nget, self->data_ptr);
}

static int
Weather_set_tilt(WeatherObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Weather_tilt_nset, self->data_ptr);
}

static PyObject *
Weather_get_track_mode(WeatherObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Weather_track_mode_nget, self->data_ptr);
}

static int
Weather_set_track_mode(WeatherObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Weather_track_mode_nset, self->data_ptr);
}

static PyGetSetDef Weather_getset[] = {
{"azimuth", (getter)Weather_get_azimuth,(setter)Weather_set_azimuth,
	PyDoc_STR("*float*: Azimuth angle of surface/axis\n\n*Required*: True"),
 	NULL},
{"file_name", (getter)Weather_get_file_name,(setter)Weather_set_file_name,
	PyDoc_STR("*str*: local weather file path\n\n*Constraints*: LOCAL_FILE\n\n*Required*: True"),
 	NULL},
{"tilt", (getter)Weather_get_tilt,(setter)Weather_set_tilt,
	PyDoc_STR("*float*: Tilt angle of surface/axis\n\n*Required*: True"),
 	NULL},
{"track_mode", (getter)Weather_get_track_mode,(setter)Weather_set_track_mode,
	PyDoc_STR("*float*: Tracking mode\n\n*Required*: True"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject Weather_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"TcstroughEmpirical.Weather",             /*tp_name*/
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
	SAM_TcstroughEmpirical   data_ptr;
} TroughObject;

static PyTypeObject Trough_Type;

static PyObject *
Trough_new(SAM_TcstroughEmpirical data_ptr)
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

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "TcstroughEmpirical", "Trough")){
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
	return PySAM_double_getter(SAM_TcstroughEmpirical_Trough_system_capacity_nget, self->data_ptr);
}

static int
Trough_set_system_capacity(TroughObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Trough_system_capacity_nset, self->data_ptr);
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
		"TcstroughEmpirical.Trough",             /*tp_name*/
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
	 * TouTranslator Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_TcstroughEmpirical   data_ptr;
} TouTranslatorObject;

static PyTypeObject TouTranslator_Type;

static PyObject *
TouTranslator_new(SAM_TcstroughEmpirical data_ptr)
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

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "TcstroughEmpirical", "TouTranslator")){
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
	return PySAM_matrix_getter(SAM_TcstroughEmpirical_TouTranslator_weekday_schedule_mget, self->data_ptr);
}

static int
TouTranslator_set_weekday_schedule(TouTranslatorObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TcstroughEmpirical_TouTranslator_weekday_schedule_mset, self->data_ptr);
}

static PyObject *
TouTranslator_get_weekend_schedule(TouTranslatorObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TcstroughEmpirical_TouTranslator_weekend_schedule_mget, self->data_ptr);
}

static int
TouTranslator_set_weekend_schedule(TouTranslatorObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TcstroughEmpirical_TouTranslator_weekend_schedule_mset, self->data_ptr);
}

static PyGetSetDef TouTranslator_getset[] = {
{"weekday_schedule", (getter)TouTranslator_get_weekday_schedule,(setter)TouTranslator_set_weekday_schedule,
	PyDoc_STR("*sequence[sequence]*: 12x24 Time of Use Values for week days\n\n*Required*: True"),
 	NULL},
{"weekend_schedule", (getter)TouTranslator_get_weekend_schedule,(setter)TouTranslator_set_weekend_schedule,
	PyDoc_STR("*sequence[sequence]*: 12x24 Time of Use Values for week end days\n\n*Required*: True"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject TouTranslator_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"TcstroughEmpirical.TouTranslator",             /*tp_name*/
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
	 * Solarfield Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_TcstroughEmpirical   data_ptr;
} SolarfieldObject;

static PyTypeObject Solarfield_Type;

static PyObject *
Solarfield_new(SAM_TcstroughEmpirical data_ptr)
{
	PyObject* new_obj = Solarfield_Type.tp_alloc(&Solarfield_Type,0);

	SolarfieldObject* Solarfield_obj = (SolarfieldObject*)new_obj;

	Solarfield_obj->data_ptr = data_ptr;

	return new_obj;
}

/* Solarfield methods */

static PyObject *
Solarfield_assign(SolarfieldObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "TcstroughEmpirical", "Solarfield")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
Solarfield_export(SolarfieldObject *self, PyObject *args)
{
	PyTypeObject* tp = &Solarfield_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef Solarfield_methods[] = {
		{"assign",            (PyCFunction)Solarfield_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``Solarfield_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)Solarfield_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
Solarfield_get_DepAngle(SolarfieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Solarfield_DepAngle_nget, self->data_ptr);
}

static int
Solarfield_set_DepAngle(SolarfieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Solarfield_DepAngle_nset, self->data_ptr);
}

static PyObject *
Solarfield_get_Distance_SCA(SolarfieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Solarfield_Distance_SCA_nget, self->data_ptr);
}

static int
Solarfield_set_Distance_SCA(SolarfieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Solarfield_Distance_SCA_nset, self->data_ptr);
}

static PyObject *
Solarfield_get_HTFFluid(SolarfieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Solarfield_HTFFluid_nget, self->data_ptr);
}

static int
Solarfield_set_HTFFluid(SolarfieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Solarfield_HTFFluid_nset, self->data_ptr);
}

static PyObject *
Solarfield_get_HtfGalArea(SolarfieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Solarfield_HtfGalArea_nget, self->data_ptr);
}

static int
Solarfield_set_HtfGalArea(SolarfieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Solarfield_HtfGalArea_nset, self->data_ptr);
}

static PyObject *
Solarfield_get_MinHtfTemp(SolarfieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Solarfield_MinHtfTemp_nget, self->data_ptr);
}

static int
Solarfield_set_MinHtfTemp(SolarfieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Solarfield_MinHtfTemp_nset, self->data_ptr);
}

static PyObject *
Solarfield_get_NumScas(SolarfieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Solarfield_NumScas_nget, self->data_ptr);
}

static int
Solarfield_set_NumScas(SolarfieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Solarfield_NumScas_nset, self->data_ptr);
}

static PyObject *
Solarfield_get_Row_Distance(SolarfieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Solarfield_Row_Distance_nget, self->data_ptr);
}

static int
Solarfield_set_Row_Distance(SolarfieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Solarfield_Row_Distance_nset, self->data_ptr);
}

static PyObject *
Solarfield_get_SFTempInit(SolarfieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Solarfield_SFTempInit_nget, self->data_ptr);
}

static int
Solarfield_set_SFTempInit(SolarfieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Solarfield_SFTempInit_nset, self->data_ptr);
}

static PyObject *
Solarfield_get_SfInTempD(SolarfieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Solarfield_SfInTempD_nget, self->data_ptr);
}

static int
Solarfield_set_SfInTempD(SolarfieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Solarfield_SfInTempD_nset, self->data_ptr);
}

static PyObject *
Solarfield_get_SfOutTempD(SolarfieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Solarfield_SfOutTempD_nget, self->data_ptr);
}

static int
Solarfield_set_SfOutTempD(SolarfieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Solarfield_SfOutTempD_nset, self->data_ptr);
}

static PyObject *
Solarfield_get_SfPipeHl1(SolarfieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Solarfield_SfPipeHl1_nget, self->data_ptr);
}

static int
Solarfield_set_SfPipeHl1(SolarfieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Solarfield_SfPipeHl1_nset, self->data_ptr);
}

static PyObject *
Solarfield_get_SfPipeHl2(SolarfieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Solarfield_SfPipeHl2_nget, self->data_ptr);
}

static int
Solarfield_set_SfPipeHl2(SolarfieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Solarfield_SfPipeHl2_nset, self->data_ptr);
}

static PyObject *
Solarfield_get_SfPipeHl3(SolarfieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Solarfield_SfPipeHl3_nget, self->data_ptr);
}

static int
Solarfield_set_SfPipeHl3(SolarfieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Solarfield_SfPipeHl3_nset, self->data_ptr);
}

static PyObject *
Solarfield_get_SfPipeHl300(SolarfieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Solarfield_SfPipeHl300_nget, self->data_ptr);
}

static int
Solarfield_set_SfPipeHl300(SolarfieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Solarfield_SfPipeHl300_nset, self->data_ptr);
}

static PyObject *
Solarfield_get_Solar_Field_Area(SolarfieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Solarfield_Solar_Field_Area_nget, self->data_ptr);
}

static int
Solarfield_set_Solar_Field_Area(SolarfieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Solarfield_Solar_Field_Area_nset, self->data_ptr);
}

static PyObject *
Solarfield_get_Solar_Field_Mult(SolarfieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Solarfield_Solar_Field_Mult_nget, self->data_ptr);
}

static int
Solarfield_set_Solar_Field_Mult(SolarfieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Solarfield_Solar_Field_Mult_nset, self->data_ptr);
}

static PyObject *
Solarfield_get_Stow_Angle(SolarfieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Solarfield_Stow_Angle_nget, self->data_ptr);
}

static int
Solarfield_set_Stow_Angle(SolarfieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Solarfield_Stow_Angle_nset, self->data_ptr);
}

static PyObject *
Solarfield_get_i_SfTi(SolarfieldObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Solarfield_i_SfTi_nget, self->data_ptr);
}

static int
Solarfield_set_i_SfTi(SolarfieldObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Solarfield_i_SfTi_nset, self->data_ptr);
}

static PyGetSetDef Solarfield_getset[] = {
{"DepAngle", (getter)Solarfield_get_DepAngle,(setter)Solarfield_set_DepAngle,
	PyDoc_STR("*float*: Deployment Angle [deg]\n\n*Required*: True"),
 	NULL},
{"Distance_SCA", (getter)Solarfield_get_Distance_SCA,(setter)Solarfield_set_Distance_SCA,
	PyDoc_STR("*float*: Distance between SCAs in Row [m]\n\n*Required*: True"),
 	NULL},
{"HTFFluid", (getter)Solarfield_get_HTFFluid,(setter)Solarfield_set_HTFFluid,
	PyDoc_STR("*float*: Type of Heat Transfer Fluid used\n\n*Constraints*: INTEGER\n\n*Required*: True"),
 	NULL},
{"HtfGalArea", (getter)Solarfield_get_HtfGalArea,(setter)Solarfield_set_HtfGalArea,
	PyDoc_STR("*float*: HTF Fluids in Gallons per Field Area [gal/m2]\n\n*Required*: True"),
 	NULL},
{"MinHtfTemp", (getter)Solarfield_get_MinHtfTemp,(setter)Solarfield_set_MinHtfTemp,
	PyDoc_STR("*float*: Minimum Heat Transfer Fluid Temperature [C]\n\n*Required*: True"),
 	NULL},
{"NumScas", (getter)Solarfield_get_NumScas,(setter)Solarfield_set_NumScas,
	PyDoc_STR("*float*: Number of SCAs per Row\n\n*Required*: True"),
 	NULL},
{"Row_Distance", (getter)Solarfield_get_Row_Distance,(setter)Solarfield_set_Row_Distance,
	PyDoc_STR("*float*: Distance between Rows of SCAs [m]\n\n*Required*: True"),
 	NULL},
{"SFTempInit", (getter)Solarfield_get_SFTempInit,(setter)Solarfield_set_SFTempInit,
	PyDoc_STR("*float*: Solar Field Initial Temperature [C]\n\n*Required*: True"),
 	NULL},
{"SfInTempD", (getter)Solarfield_get_SfInTempD,(setter)Solarfield_set_SfInTempD,
	PyDoc_STR("*float*: Solar Field Design Inlet Temperature [C]\n\n*Required*: True"),
 	NULL},
{"SfOutTempD", (getter)Solarfield_get_SfOutTempD,(setter)Solarfield_set_SfOutTempD,
	PyDoc_STR("*float*: Solar Field Design Outlet Temperature [C]\n\n*Required*: True"),
 	NULL},
{"SfPipeHl1", (getter)Solarfield_get_SfPipeHl1,(setter)Solarfield_set_SfPipeHl1,
	PyDoc_STR("*float*: Solar field piping heat loss at reduced temp. - linear term [C^(-1)]\n\n*Required*: True"),
 	NULL},
{"SfPipeHl2", (getter)Solarfield_get_SfPipeHl2,(setter)Solarfield_set_SfPipeHl2,
	PyDoc_STR("*float*: Solar field piping heat loss at reduced temp. - quadratic term [C^(-2)]\n\n*Required*: True"),
 	NULL},
{"SfPipeHl3", (getter)Solarfield_get_SfPipeHl3,(setter)Solarfield_set_SfPipeHl3,
	PyDoc_STR("*float*: Solar field piping heat loss at reduced temp. - cubic term [C^(-3)]\n\n*Required*: True"),
 	NULL},
{"SfPipeHl300", (getter)Solarfield_get_SfPipeHl300,(setter)Solarfield_set_SfPipeHl300,
	PyDoc_STR("*float*: Solar field piping heat loss at design [W/m2]\n\n*Required*: True"),
 	NULL},
{"Solar_Field_Area", (getter)Solarfield_get_Solar_Field_Area,(setter)Solarfield_set_Solar_Field_Area,
	PyDoc_STR("*float*: Solar Field Area [m2]\n\n*Required*: True"),
 	NULL},
{"Solar_Field_Mult", (getter)Solarfield_get_Solar_Field_Mult,(setter)Solarfield_set_Solar_Field_Mult,
	PyDoc_STR("*float*: Solar Field Multiple\n\n*Required*: True"),
 	NULL},
{"Stow_Angle", (getter)Solarfield_get_Stow_Angle,(setter)Solarfield_set_Stow_Angle,
	PyDoc_STR("*float*: Night-Time Trough Stow Angle [deg]\n\n*Required*: True"),
 	NULL},
{"i_SfTi", (getter)Solarfield_get_i_SfTi,(setter)Solarfield_set_i_SfTi,
	PyDoc_STR("*float*: Solar Field HTF inlet Temperature (if -999, calculated) [C]\n\n*Required*: True"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject Solarfield_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"TcstroughEmpirical.Solarfield",             /*tp_name*/
		sizeof(SolarfieldObject),          /*tp_basicsize*/
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
		Solarfield_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		Solarfield_getset,          /*tp_getset*/
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
	 * Sca Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_TcstroughEmpirical   data_ptr;
} ScaObject;

static PyTypeObject Sca_Type;

static PyObject *
Sca_new(SAM_TcstroughEmpirical data_ptr)
{
	PyObject* new_obj = Sca_Type.tp_alloc(&Sca_Type,0);

	ScaObject* Sca_obj = (ScaObject*)new_obj;

	Sca_obj->data_ptr = data_ptr;

	return new_obj;
}

/* Sca methods */

static PyObject *
Sca_assign(ScaObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "TcstroughEmpirical", "Sca")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
Sca_export(ScaObject *self, PyObject *args)
{
	PyTypeObject* tp = &Sca_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef Sca_methods[] = {
		{"assign",            (PyCFunction)Sca_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``Sca_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)Sca_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
Sca_get_Ave_Focal_Length(ScaObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Sca_Ave_Focal_Length_nget, self->data_ptr);
}

static int
Sca_set_Ave_Focal_Length(ScaObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Sca_Ave_Focal_Length_nset, self->data_ptr);
}

static PyObject *
Sca_get_ConcFac(ScaObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Sca_ConcFac_nget, self->data_ptr);
}

static int
Sca_set_ConcFac(ScaObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Sca_ConcFac_nset, self->data_ptr);
}

static PyObject *
Sca_get_GeoAcc(ScaObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Sca_GeoAcc_nget, self->data_ptr);
}

static int
Sca_set_GeoAcc(ScaObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Sca_GeoAcc_nset, self->data_ptr);
}

static PyObject *
Sca_get_IamF0(ScaObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Sca_IamF0_nget, self->data_ptr);
}

static int
Sca_set_IamF0(ScaObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Sca_IamF0_nset, self->data_ptr);
}

static PyObject *
Sca_get_IamF1(ScaObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Sca_IamF1_nget, self->data_ptr);
}

static int
Sca_set_IamF1(ScaObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Sca_IamF1_nset, self->data_ptr);
}

static PyObject *
Sca_get_IamF2(ScaObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Sca_IamF2_nget, self->data_ptr);
}

static int
Sca_set_IamF2(ScaObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Sca_IamF2_nset, self->data_ptr);
}

static PyObject *
Sca_get_MirCln(ScaObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Sca_MirCln_nget, self->data_ptr);
}

static int
Sca_set_MirCln(ScaObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Sca_MirCln_nset, self->data_ptr);
}

static PyObject *
Sca_get_MirRef(ScaObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Sca_MirRef_nget, self->data_ptr);
}

static int
Sca_set_MirRef(ScaObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Sca_MirRef_nset, self->data_ptr);
}

static PyObject *
Sca_get_SCA_aper(ScaObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Sca_SCA_aper_nget, self->data_ptr);
}

static int
Sca_set_SCA_aper(ScaObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Sca_SCA_aper_nset, self->data_ptr);
}

static PyObject *
Sca_get_ScaLen(ScaObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Sca_ScaLen_nget, self->data_ptr);
}

static int
Sca_set_ScaLen(ScaObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Sca_ScaLen_nset, self->data_ptr);
}

static PyObject *
Sca_get_SfAvail(ScaObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Sca_SfAvail_nget, self->data_ptr);
}

static int
Sca_set_SfAvail(ScaObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Sca_SfAvail_nset, self->data_ptr);
}

static PyObject *
Sca_get_TrkTwstErr(ScaObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Sca_TrkTwstErr_nget, self->data_ptr);
}

static int
Sca_set_TrkTwstErr(ScaObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Sca_TrkTwstErr_nset, self->data_ptr);
}

static PyGetSetDef Sca_getset[] = {
{"Ave_Focal_Length", (getter)Sca_get_Ave_Focal_Length,(setter)Sca_set_Ave_Focal_Length,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"ConcFac", (getter)Sca_get_ConcFac,(setter)Sca_set_ConcFac,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"GeoAcc", (getter)Sca_get_GeoAcc,(setter)Sca_set_GeoAcc,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"IamF0", (getter)Sca_get_IamF0,(setter)Sca_set_IamF0,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"IamF1", (getter)Sca_get_IamF1,(setter)Sca_set_IamF1,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"IamF2", (getter)Sca_get_IamF2,(setter)Sca_set_IamF2,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"MirCln", (getter)Sca_get_MirCln,(setter)Sca_set_MirCln,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"MirRef", (getter)Sca_get_MirRef,(setter)Sca_set_MirRef,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"SCA_aper", (getter)Sca_get_SCA_aper,(setter)Sca_set_SCA_aper,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"ScaLen", (getter)Sca_get_ScaLen,(setter)Sca_set_ScaLen,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"SfAvail", (getter)Sca_get_SfAvail,(setter)Sca_set_SfAvail,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"TrkTwstErr", (getter)Sca_get_TrkTwstErr,(setter)Sca_set_TrkTwstErr,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject Sca_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"TcstroughEmpirical.Sca",             /*tp_name*/
		sizeof(ScaObject),          /*tp_basicsize*/
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
		Sca_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		Sca_getset,          /*tp_getset*/
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
	 * Hce Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_TcstroughEmpirical   data_ptr;
} HceObject;

static PyTypeObject Hce_Type;

static PyObject *
Hce_new(SAM_TcstroughEmpirical data_ptr)
{
	PyObject* new_obj = Hce_Type.tp_alloc(&Hce_Type,0);

	HceObject* Hce_obj = (HceObject*)new_obj;

	Hce_obj->data_ptr = data_ptr;

	return new_obj;
}

/* Hce methods */

static PyObject *
Hce_assign(HceObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "TcstroughEmpirical", "Hce")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
Hce_export(HceObject *self, PyObject *args)
{
	PyTypeObject* tp = &Hce_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef Hce_methods[] = {
		{"assign",            (PyCFunction)Hce_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``Hce_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)Hce_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
Hce_get_HCEBelShad(HceObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Hce_HCEBelShad_aget, self->data_ptr);
}

static int
Hce_set_HCEBelShad(HceObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_TcstroughEmpirical_Hce_HCEBelShad_aset, self->data_ptr);
}

static PyObject *
Hce_get_HCEEnvTrans(HceObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Hce_HCEEnvTrans_aget, self->data_ptr);
}

static int
Hce_set_HCEEnvTrans(HceObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_TcstroughEmpirical_Hce_HCEEnvTrans_aset, self->data_ptr);
}

static PyObject *
Hce_get_HCEFrac(HceObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Hce_HCEFrac_aget, self->data_ptr);
}

static int
Hce_set_HCEFrac(HceObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_TcstroughEmpirical_Hce_HCEFrac_aset, self->data_ptr);
}

static PyObject *
Hce_get_HCE_A0(HceObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Hce_HCE_A0_aget, self->data_ptr);
}

static int
Hce_set_HCE_A0(HceObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_TcstroughEmpirical_Hce_HCE_A0_aset, self->data_ptr);
}

static PyObject *
Hce_get_HCE_A1(HceObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Hce_HCE_A1_aget, self->data_ptr);
}

static int
Hce_set_HCE_A1(HceObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_TcstroughEmpirical_Hce_HCE_A1_aset, self->data_ptr);
}

static PyObject *
Hce_get_HCE_A2(HceObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Hce_HCE_A2_aget, self->data_ptr);
}

static int
Hce_set_HCE_A2(HceObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_TcstroughEmpirical_Hce_HCE_A2_aset, self->data_ptr);
}

static PyObject *
Hce_get_HCE_A3(HceObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Hce_HCE_A3_aget, self->data_ptr);
}

static int
Hce_set_HCE_A3(HceObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_TcstroughEmpirical_Hce_HCE_A3_aset, self->data_ptr);
}

static PyObject *
Hce_get_HCE_A4(HceObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Hce_HCE_A4_aget, self->data_ptr);
}

static int
Hce_set_HCE_A4(HceObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_TcstroughEmpirical_Hce_HCE_A4_aset, self->data_ptr);
}

static PyObject *
Hce_get_HCE_A5(HceObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Hce_HCE_A5_aget, self->data_ptr);
}

static int
Hce_set_HCE_A5(HceObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_TcstroughEmpirical_Hce_HCE_A5_aset, self->data_ptr);
}

static PyObject *
Hce_get_HCE_A6(HceObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Hce_HCE_A6_aget, self->data_ptr);
}

static int
Hce_set_HCE_A6(HceObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_TcstroughEmpirical_Hce_HCE_A6_aset, self->data_ptr);
}

static PyObject *
Hce_get_HCEabs(HceObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Hce_HCEabs_aget, self->data_ptr);
}

static int
Hce_set_HCEabs(HceObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_TcstroughEmpirical_Hce_HCEabs_aset, self->data_ptr);
}

static PyObject *
Hce_get_HCEdust(HceObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Hce_HCEdust_aget, self->data_ptr);
}

static int
Hce_set_HCEdust(HceObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_TcstroughEmpirical_Hce_HCEdust_aset, self->data_ptr);
}

static PyObject *
Hce_get_HCEmisc(HceObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Hce_HCEmisc_aget, self->data_ptr);
}

static int
Hce_set_HCEmisc(HceObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_TcstroughEmpirical_Hce_HCEmisc_aset, self->data_ptr);
}

static PyObject *
Hce_get_HCEtype(HceObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Hce_HCEtype_aget, self->data_ptr);
}

static int
Hce_set_HCEtype(HceObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_TcstroughEmpirical_Hce_HCEtype_aset, self->data_ptr);
}

static PyObject *
Hce_get_NumHCETypes(HceObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Hce_NumHCETypes_nget, self->data_ptr);
}

static int
Hce_set_NumHCETypes(HceObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Hce_NumHCETypes_nset, self->data_ptr);
}

static PyObject *
Hce_get_PerfFac(HceObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Hce_PerfFac_aget, self->data_ptr);
}

static int
Hce_set_PerfFac(HceObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_TcstroughEmpirical_Hce_PerfFac_aset, self->data_ptr);
}

static PyObject *
Hce_get_RefMirrAper(HceObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Hce_RefMirrAper_aget, self->data_ptr);
}

static int
Hce_set_RefMirrAper(HceObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_TcstroughEmpirical_Hce_RefMirrAper_aset, self->data_ptr);
}

static PyGetSetDef Hce_getset[] = {
{"HCEBelShad", (getter)Hce_get_HCEBelShad,(setter)Hce_set_HCEBelShad,
	PyDoc_STR("*sequence*: label\n\n*Required*: True"),
 	NULL},
{"HCEEnvTrans", (getter)Hce_get_HCEEnvTrans,(setter)Hce_set_HCEEnvTrans,
	PyDoc_STR("*sequence*: label\n\n*Required*: True"),
 	NULL},
{"HCEFrac", (getter)Hce_get_HCEFrac,(setter)Hce_set_HCEFrac,
	PyDoc_STR("*sequence*: Fraction of field that is this type of HCE\n\n*Required*: True"),
 	NULL},
{"HCE_A0", (getter)Hce_get_HCE_A0,(setter)Hce_set_HCE_A0,
	PyDoc_STR("*sequence*: label\n\n*Required*: True"),
 	NULL},
{"HCE_A1", (getter)Hce_get_HCE_A1,(setter)Hce_set_HCE_A1,
	PyDoc_STR("*sequence*: label\n\n*Required*: True"),
 	NULL},
{"HCE_A2", (getter)Hce_get_HCE_A2,(setter)Hce_set_HCE_A2,
	PyDoc_STR("*sequence*: label\n\n*Required*: True"),
 	NULL},
{"HCE_A3", (getter)Hce_get_HCE_A3,(setter)Hce_set_HCE_A3,
	PyDoc_STR("*sequence*: label\n\n*Required*: True"),
 	NULL},
{"HCE_A4", (getter)Hce_get_HCE_A4,(setter)Hce_set_HCE_A4,
	PyDoc_STR("*sequence*: label\n\n*Required*: True"),
 	NULL},
{"HCE_A5", (getter)Hce_get_HCE_A5,(setter)Hce_set_HCE_A5,
	PyDoc_STR("*sequence*: label\n\n*Required*: True"),
 	NULL},
{"HCE_A6", (getter)Hce_get_HCE_A6,(setter)Hce_set_HCE_A6,
	PyDoc_STR("*sequence*: label\n\n*Required*: True"),
 	NULL},
{"HCEabs", (getter)Hce_get_HCEabs,(setter)Hce_set_HCEabs,
	PyDoc_STR("*sequence*: label\n\n*Required*: True"),
 	NULL},
{"HCEdust", (getter)Hce_get_HCEdust,(setter)Hce_set_HCEdust,
	PyDoc_STR("*sequence*: label\n\n*Required*: True"),
 	NULL},
{"HCEmisc", (getter)Hce_get_HCEmisc,(setter)Hce_set_HCEmisc,
	PyDoc_STR("*sequence*: label\n\n*Required*: True"),
 	NULL},
{"HCEtype", (getter)Hce_get_HCEtype,(setter)Hce_set_HCEtype,
	PyDoc_STR("*sequence*: Number indicating the receiver type\n\n*Required*: True"),
 	NULL},
{"NumHCETypes", (getter)Hce_get_NumHCETypes,(setter)Hce_set_NumHCETypes,
	PyDoc_STR("*float*: Number of HCE types\n\n*Constraints*: INTEGER\n\n*Required*: True"),
 	NULL},
{"PerfFac", (getter)Hce_get_PerfFac,(setter)Hce_set_PerfFac,
	PyDoc_STR("*sequence*: label\n\n*Required*: True"),
 	NULL},
{"RefMirrAper", (getter)Hce_get_RefMirrAper,(setter)Hce_set_RefMirrAper,
	PyDoc_STR("*sequence*: label\n\n*Required*: True"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject Hce_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"TcstroughEmpirical.Hce",             /*tp_name*/
		sizeof(HceObject),          /*tp_basicsize*/
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
		Hce_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		Hce_getset,          /*tp_getset*/
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
	 * Pwrb Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_TcstroughEmpirical   data_ptr;
} PwrbObject;

static PyTypeObject Pwrb_Type;

static PyObject *
Pwrb_new(SAM_TcstroughEmpirical data_ptr)
{
	PyObject* new_obj = Pwrb_Type.tp_alloc(&Pwrb_Type,0);

	PwrbObject* Pwrb_obj = (PwrbObject*)new_obj;

	Pwrb_obj->data_ptr = data_ptr;

	return new_obj;
}

/* Pwrb methods */

static PyObject *
Pwrb_assign(PwrbObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "TcstroughEmpirical", "Pwrb")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
Pwrb_export(PwrbObject *self, PyObject *args)
{
	PyTypeObject* tp = &Pwrb_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef Pwrb_methods[] = {
		{"assign",            (PyCFunction)Pwrb_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``Pwrb_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)Pwrb_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
Pwrb_get_E2TPLF0(PwrbObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Pwrb_E2TPLF0_nget, self->data_ptr);
}

static int
Pwrb_set_E2TPLF0(PwrbObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Pwrb_E2TPLF0_nset, self->data_ptr);
}

static PyObject *
Pwrb_get_E2TPLF1(PwrbObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Pwrb_E2TPLF1_nget, self->data_ptr);
}

static int
Pwrb_set_E2TPLF1(PwrbObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Pwrb_E2TPLF1_nset, self->data_ptr);
}

static PyObject *
Pwrb_get_E2TPLF2(PwrbObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Pwrb_E2TPLF2_nget, self->data_ptr);
}

static int
Pwrb_set_E2TPLF2(PwrbObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Pwrb_E2TPLF2_nset, self->data_ptr);
}

static PyObject *
Pwrb_get_E2TPLF3(PwrbObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Pwrb_E2TPLF3_nget, self->data_ptr);
}

static int
Pwrb_set_E2TPLF3(PwrbObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Pwrb_E2TPLF3_nset, self->data_ptr);
}

static PyObject *
Pwrb_get_E2TPLF4(PwrbObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Pwrb_E2TPLF4_nget, self->data_ptr);
}

static int
Pwrb_set_E2TPLF4(PwrbObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Pwrb_E2TPLF4_nset, self->data_ptr);
}

static PyObject *
Pwrb_get_LHVBoilEff(PwrbObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Pwrb_LHVBoilEff_nget, self->data_ptr);
}

static int
Pwrb_set_LHVBoilEff(PwrbObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Pwrb_LHVBoilEff_nset, self->data_ptr);
}

static PyObject *
Pwrb_get_MaxGrOut(PwrbObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Pwrb_MaxGrOut_nget, self->data_ptr);
}

static int
Pwrb_set_MaxGrOut(PwrbObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Pwrb_MaxGrOut_nset, self->data_ptr);
}

static PyObject *
Pwrb_get_MinGrOut(PwrbObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Pwrb_MinGrOut_nget, self->data_ptr);
}

static int
Pwrb_set_MinGrOut(PwrbObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Pwrb_MinGrOut_nset, self->data_ptr);
}

static PyObject *
Pwrb_get_PTTMAX(PwrbObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Pwrb_PTTMAX_nget, self->data_ptr);
}

static int
Pwrb_set_PTTMAX(PwrbObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Pwrb_PTTMAX_nset, self->data_ptr);
}

static PyObject *
Pwrb_get_PTTMIN(PwrbObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Pwrb_PTTMIN_nget, self->data_ptr);
}

static int
Pwrb_set_PTTMIN(PwrbObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Pwrb_PTTMIN_nset, self->data_ptr);
}

static PyObject *
Pwrb_get_T2EPLF0(PwrbObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Pwrb_T2EPLF0_nget, self->data_ptr);
}

static int
Pwrb_set_T2EPLF0(PwrbObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Pwrb_T2EPLF0_nset, self->data_ptr);
}

static PyObject *
Pwrb_get_T2EPLF1(PwrbObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Pwrb_T2EPLF1_nget, self->data_ptr);
}

static int
Pwrb_set_T2EPLF1(PwrbObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Pwrb_T2EPLF1_nset, self->data_ptr);
}

static PyObject *
Pwrb_get_T2EPLF2(PwrbObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Pwrb_T2EPLF2_nget, self->data_ptr);
}

static int
Pwrb_set_T2EPLF2(PwrbObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Pwrb_T2EPLF2_nset, self->data_ptr);
}

static PyObject *
Pwrb_get_T2EPLF3(PwrbObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Pwrb_T2EPLF3_nget, self->data_ptr);
}

static int
Pwrb_set_T2EPLF3(PwrbObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Pwrb_T2EPLF3_nset, self->data_ptr);
}

static PyObject *
Pwrb_get_T2EPLF4(PwrbObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Pwrb_T2EPLF4_nget, self->data_ptr);
}

static int
Pwrb_set_T2EPLF4(PwrbObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Pwrb_T2EPLF4_nset, self->data_ptr);
}

static PyObject *
Pwrb_get_TempCorr0(PwrbObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Pwrb_TempCorr0_nget, self->data_ptr);
}

static int
Pwrb_set_TempCorr0(PwrbObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Pwrb_TempCorr0_nset, self->data_ptr);
}

static PyObject *
Pwrb_get_TempCorr1(PwrbObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Pwrb_TempCorr1_nget, self->data_ptr);
}

static int
Pwrb_set_TempCorr1(PwrbObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Pwrb_TempCorr1_nset, self->data_ptr);
}

static PyObject *
Pwrb_get_TempCorr2(PwrbObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Pwrb_TempCorr2_nget, self->data_ptr);
}

static int
Pwrb_set_TempCorr2(PwrbObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Pwrb_TempCorr2_nset, self->data_ptr);
}

static PyObject *
Pwrb_get_TempCorr3(PwrbObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Pwrb_TempCorr3_nget, self->data_ptr);
}

static int
Pwrb_set_TempCorr3(PwrbObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Pwrb_TempCorr3_nset, self->data_ptr);
}

static PyObject *
Pwrb_get_TempCorr4(PwrbObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Pwrb_TempCorr4_nget, self->data_ptr);
}

static int
Pwrb_set_TempCorr4(PwrbObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Pwrb_TempCorr4_nset, self->data_ptr);
}

static PyObject *
Pwrb_get_TempCorrF(PwrbObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Pwrb_TempCorrF_nget, self->data_ptr);
}

static int
Pwrb_set_TempCorrF(PwrbObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Pwrb_TempCorrF_nset, self->data_ptr);
}

static PyObject *
Pwrb_get_TurSUE(PwrbObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Pwrb_TurSUE_nget, self->data_ptr);
}

static int
Pwrb_set_TurSUE(PwrbObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Pwrb_TurSUE_nset, self->data_ptr);
}

static PyObject *
Pwrb_get_TurbEffG(PwrbObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Pwrb_TurbEffG_nget, self->data_ptr);
}

static int
Pwrb_set_TurbEffG(PwrbObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Pwrb_TurbEffG_nset, self->data_ptr);
}

static PyObject *
Pwrb_get_TurbOutG(PwrbObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Pwrb_TurbOutG_nget, self->data_ptr);
}

static int
Pwrb_set_TurbOutG(PwrbObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Pwrb_TurbOutG_nset, self->data_ptr);
}

static PyGetSetDef Pwrb_getset[] = {
{"E2TPLF0", (getter)Pwrb_get_E2TPLF0,(setter)Pwrb_set_E2TPLF0,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"E2TPLF1", (getter)Pwrb_get_E2TPLF1,(setter)Pwrb_set_E2TPLF1,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"E2TPLF2", (getter)Pwrb_get_E2TPLF2,(setter)Pwrb_set_E2TPLF2,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"E2TPLF3", (getter)Pwrb_get_E2TPLF3,(setter)Pwrb_set_E2TPLF3,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"E2TPLF4", (getter)Pwrb_get_E2TPLF4,(setter)Pwrb_set_E2TPLF4,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"LHVBoilEff", (getter)Pwrb_get_LHVBoilEff,(setter)Pwrb_set_LHVBoilEff,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"MaxGrOut", (getter)Pwrb_get_MaxGrOut,(setter)Pwrb_set_MaxGrOut,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"MinGrOut", (getter)Pwrb_get_MinGrOut,(setter)Pwrb_set_MinGrOut,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"PTTMAX", (getter)Pwrb_get_PTTMAX,(setter)Pwrb_set_PTTMAX,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"PTTMIN", (getter)Pwrb_get_PTTMIN,(setter)Pwrb_set_PTTMIN,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"T2EPLF0", (getter)Pwrb_get_T2EPLF0,(setter)Pwrb_set_T2EPLF0,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"T2EPLF1", (getter)Pwrb_get_T2EPLF1,(setter)Pwrb_set_T2EPLF1,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"T2EPLF2", (getter)Pwrb_get_T2EPLF2,(setter)Pwrb_set_T2EPLF2,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"T2EPLF3", (getter)Pwrb_get_T2EPLF3,(setter)Pwrb_set_T2EPLF3,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"T2EPLF4", (getter)Pwrb_get_T2EPLF4,(setter)Pwrb_set_T2EPLF4,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"TempCorr0", (getter)Pwrb_get_TempCorr0,(setter)Pwrb_set_TempCorr0,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"TempCorr1", (getter)Pwrb_get_TempCorr1,(setter)Pwrb_set_TempCorr1,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"TempCorr2", (getter)Pwrb_get_TempCorr2,(setter)Pwrb_set_TempCorr2,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"TempCorr3", (getter)Pwrb_get_TempCorr3,(setter)Pwrb_set_TempCorr3,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"TempCorr4", (getter)Pwrb_get_TempCorr4,(setter)Pwrb_set_TempCorr4,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"TempCorrF", (getter)Pwrb_get_TempCorrF,(setter)Pwrb_set_TempCorrF,
	PyDoc_STR("*float*: Temp Correction Mode (0=wetbulb 1=drybulb basis)\n\n*Constraints*: INTEGER\n\n*Required*: True"),
 	NULL},
{"TurSUE", (getter)Pwrb_get_TurSUE,(setter)Pwrb_set_TurSUE,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"TurbEffG", (getter)Pwrb_get_TurbEffG,(setter)Pwrb_set_TurbEffG,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"TurbOutG", (getter)Pwrb_get_TurbOutG,(setter)Pwrb_set_TurbOutG,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject Pwrb_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"TcstroughEmpirical.Pwrb",             /*tp_name*/
		sizeof(PwrbObject),          /*tp_basicsize*/
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
		Pwrb_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		Pwrb_getset,          /*tp_getset*/
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
	 * Tes Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_TcstroughEmpirical   data_ptr;
} TesObject;

static PyTypeObject Tes_Type;

static PyObject *
Tes_new(SAM_TcstroughEmpirical data_ptr)
{
	PyObject* new_obj = Tes_Type.tp_alloc(&Tes_Type,0);

	TesObject* Tes_obj = (TesObject*)new_obj;

	Tes_obj->data_ptr = data_ptr;

	return new_obj;
}

/* Tes methods */

static PyObject *
Tes_assign(TesObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "TcstroughEmpirical", "Tes")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
Tes_export(TesObject *self, PyObject *args)
{
	PyTypeObject* tp = &Tes_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef Tes_methods[] = {
		{"assign",            (PyCFunction)Tes_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``Tes_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)Tes_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
Tes_get_E_tes_ini(TesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Tes_E_tes_ini_nget, self->data_ptr);
}

static int
Tes_set_E_tes_ini(TesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Tes_E_tes_ini_nset, self->data_ptr);
}

static PyObject *
Tes_get_FossilFill(TesObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Tes_FossilFill_aget, self->data_ptr);
}

static int
Tes_set_FossilFill(TesObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_TcstroughEmpirical_Tes_FossilFill_aset, self->data_ptr);
}

static PyObject *
Tes_get_NUMTOU(TesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Tes_NUMTOU_nget, self->data_ptr);
}

static int
Tes_set_NUMTOU(TesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Tes_NUMTOU_nset, self->data_ptr);
}

static PyObject *
Tes_get_PFSmax(TesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Tes_PFSmax_nget, self->data_ptr);
}

static int
Tes_set_PFSmax(TesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Tes_PFSmax_nset, self->data_ptr);
}

static PyObject *
Tes_get_PTSmax(TesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Tes_PTSmax_nget, self->data_ptr);
}

static int
Tes_set_PTSmax(TesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Tes_PTSmax_nset, self->data_ptr);
}

static PyObject *
Tes_get_TSHOURS(TesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Tes_TSHOURS_nget, self->data_ptr);
}

static int
Tes_set_TSHOURS(TesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Tes_TSHOURS_nset, self->data_ptr);
}

static PyObject *
Tes_get_TSLogic(TesObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_TcstroughEmpirical_Tes_TSLogic_mget, self->data_ptr);
}

static int
Tes_set_TSLogic(TesObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_TcstroughEmpirical_Tes_TSLogic_mset, self->data_ptr);
}

static PyObject *
Tes_get_TnkHL(TesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Tes_TnkHL_nget, self->data_ptr);
}

static int
Tes_set_TnkHL(TesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Tes_TnkHL_nset, self->data_ptr);
}

static PyObject *
Tes_get_TurTesEffAdj(TesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Tes_TurTesEffAdj_nget, self->data_ptr);
}

static int
Tes_set_TurTesEffAdj(TesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Tes_TurTesEffAdj_nset, self->data_ptr);
}

static PyObject *
Tes_get_TurTesOutAdj(TesObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Tes_TurTesOutAdj_nget, self->data_ptr);
}

static int
Tes_set_TurTesOutAdj(TesObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Tes_TurTesOutAdj_nset, self->data_ptr);
}

static PyGetSetDef Tes_getset[] = {
{"E_tes_ini", (getter)Tes_get_E_tes_ini,(setter)Tes_set_E_tes_ini,
	PyDoc_STR("*float*: Initial TES energy - fraction of max\n\n*Required*: True"),
 	NULL},
{"FossilFill", (getter)Tes_get_FossilFill,(setter)Tes_set_FossilFill,
	PyDoc_STR("*sequence*: Label\n\n*Required*: True"),
 	NULL},
{"NUMTOU", (getter)Tes_get_NUMTOU,(setter)Tes_set_NUMTOU,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"PFSmax", (getter)Tes_get_PFSmax,(setter)Tes_set_PFSmax,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"PTSmax", (getter)Tes_get_PTSmax,(setter)Tes_set_PTSmax,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"TSHOURS", (getter)Tes_get_TSHOURS,(setter)Tes_set_TSHOURS,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"TSLogic", (getter)Tes_get_TSLogic,(setter)Tes_set_TSLogic,
	PyDoc_STR("*sequence[sequence]*: Label\n\n*Required*: True"),
 	NULL},
{"TnkHL", (getter)Tes_get_TnkHL,(setter)Tes_set_TnkHL,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"TurTesEffAdj", (getter)Tes_get_TurTesEffAdj,(setter)Tes_set_TurTesEffAdj,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"TurTesOutAdj", (getter)Tes_get_TurTesOutAdj,(setter)Tes_set_TurTesOutAdj,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject Tes_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"TcstroughEmpirical.Tes",             /*tp_name*/
		sizeof(TesObject),          /*tp_basicsize*/
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
		Tes_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		Tes_getset,          /*tp_getset*/
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
	 * Parasitic Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_TcstroughEmpirical   data_ptr;
} ParasiticObject;

static PyTypeObject Parasitic_Type;

static PyObject *
Parasitic_new(SAM_TcstroughEmpirical data_ptr)
{
	PyObject* new_obj = Parasitic_Type.tp_alloc(&Parasitic_Type,0);

	ParasiticObject* Parasitic_obj = (ParasiticObject*)new_obj;

	Parasitic_obj->data_ptr = data_ptr;

	return new_obj;
}

/* Parasitic methods */

static PyObject *
Parasitic_assign(ParasiticObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "TcstroughEmpirical", "Parasitic")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
Parasitic_export(ParasiticObject *self, PyObject *args)
{
	PyTypeObject* tp = &Parasitic_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef Parasitic_methods[] = {
		{"assign",            (PyCFunction)Parasitic_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``Parasitic_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)Parasitic_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
Parasitic_get_AntiFrPar(ParasiticObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Parasitic_AntiFrPar_nget, self->data_ptr);
}

static int
Parasitic_set_AntiFrPar(ParasiticObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Parasitic_AntiFrPar_nset, self->data_ptr);
}

static PyObject *
Parasitic_get_BOPPar(ParasiticObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Parasitic_BOPPar_nget, self->data_ptr);
}

static int
Parasitic_set_BOPPar(ParasiticObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Parasitic_BOPPar_nset, self->data_ptr);
}

static PyObject *
Parasitic_get_BOPParF0(ParasiticObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Parasitic_BOPParF0_nget, self->data_ptr);
}

static int
Parasitic_set_BOPParF0(ParasiticObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Parasitic_BOPParF0_nset, self->data_ptr);
}

static PyObject *
Parasitic_get_BOPParF1(ParasiticObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Parasitic_BOPParF1_nget, self->data_ptr);
}

static int
Parasitic_set_BOPParF1(ParasiticObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Parasitic_BOPParF1_nset, self->data_ptr);
}

static PyObject *
Parasitic_get_BOPParF2(ParasiticObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Parasitic_BOPParF2_nget, self->data_ptr);
}

static int
Parasitic_set_BOPParF2(ParasiticObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Parasitic_BOPParF2_nset, self->data_ptr);
}

static PyObject *
Parasitic_get_BOPParPF(ParasiticObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Parasitic_BOPParPF_nget, self->data_ptr);
}

static int
Parasitic_set_BOPParPF(ParasiticObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Parasitic_BOPParPF_nset, self->data_ptr);
}

static PyObject *
Parasitic_get_CHTFParF0(ParasiticObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Parasitic_CHTFParF0_nget, self->data_ptr);
}

static int
Parasitic_set_CHTFParF0(ParasiticObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Parasitic_CHTFParF0_nset, self->data_ptr);
}

static PyObject *
Parasitic_get_CHTFParF1(ParasiticObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Parasitic_CHTFParF1_nget, self->data_ptr);
}

static int
Parasitic_set_CHTFParF1(ParasiticObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Parasitic_CHTFParF1_nset, self->data_ptr);
}

static PyObject *
Parasitic_get_CHTFParF2(ParasiticObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Parasitic_CHTFParF2_nget, self->data_ptr);
}

static int
Parasitic_set_CHTFParF2(ParasiticObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Parasitic_CHTFParF2_nset, self->data_ptr);
}

static PyObject *
Parasitic_get_ChtfPar(ParasiticObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Parasitic_ChtfPar_nget, self->data_ptr);
}

static int
Parasitic_set_ChtfPar(ParasiticObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Parasitic_ChtfPar_nset, self->data_ptr);
}

static PyObject *
Parasitic_get_ChtfParPF(ParasiticObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Parasitic_ChtfParPF_nget, self->data_ptr);
}

static int
Parasitic_set_ChtfParPF(ParasiticObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Parasitic_ChtfParPF_nset, self->data_ptr);
}

static PyObject *
Parasitic_get_CtOpF(ParasiticObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Parasitic_CtOpF_nget, self->data_ptr);
}

static int
Parasitic_set_CtOpF(ParasiticObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Parasitic_CtOpF_nset, self->data_ptr);
}

static PyObject *
Parasitic_get_CtPar(ParasiticObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Parasitic_CtPar_nget, self->data_ptr);
}

static int
Parasitic_set_CtPar(ParasiticObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Parasitic_CtPar_nset, self->data_ptr);
}

static PyObject *
Parasitic_get_CtParF0(ParasiticObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Parasitic_CtParF0_nget, self->data_ptr);
}

static int
Parasitic_set_CtParF0(ParasiticObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Parasitic_CtParF0_nset, self->data_ptr);
}

static PyObject *
Parasitic_get_CtParF1(ParasiticObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Parasitic_CtParF1_nget, self->data_ptr);
}

static int
Parasitic_set_CtParF1(ParasiticObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Parasitic_CtParF1_nset, self->data_ptr);
}

static PyObject *
Parasitic_get_CtParF2(ParasiticObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Parasitic_CtParF2_nget, self->data_ptr);
}

static int
Parasitic_set_CtParF2(ParasiticObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Parasitic_CtParF2_nset, self->data_ptr);
}

static PyObject *
Parasitic_get_CtParPF(ParasiticObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Parasitic_CtParPF_nget, self->data_ptr);
}

static int
Parasitic_set_CtParPF(ParasiticObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Parasitic_CtParPF_nset, self->data_ptr);
}

static PyObject *
Parasitic_get_HhtfPar(ParasiticObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Parasitic_HhtfPar_nget, self->data_ptr);
}

static int
Parasitic_set_HhtfPar(ParasiticObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Parasitic_HhtfPar_nset, self->data_ptr);
}

static PyObject *
Parasitic_get_HhtfParF0(ParasiticObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Parasitic_HhtfParF0_nget, self->data_ptr);
}

static int
Parasitic_set_HhtfParF0(ParasiticObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Parasitic_HhtfParF0_nset, self->data_ptr);
}

static PyObject *
Parasitic_get_HhtfParF1(ParasiticObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Parasitic_HhtfParF1_nget, self->data_ptr);
}

static int
Parasitic_set_HhtfParF1(ParasiticObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Parasitic_HhtfParF1_nset, self->data_ptr);
}

static PyObject *
Parasitic_get_HhtfParF2(ParasiticObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Parasitic_HhtfParF2_nget, self->data_ptr);
}

static int
Parasitic_set_HhtfParF2(ParasiticObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Parasitic_HhtfParF2_nset, self->data_ptr);
}

static PyObject *
Parasitic_get_HhtfParPF(ParasiticObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Parasitic_HhtfParPF_nget, self->data_ptr);
}

static int
Parasitic_set_HhtfParPF(ParasiticObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Parasitic_HhtfParPF_nset, self->data_ptr);
}

static PyObject *
Parasitic_get_HtrPar(ParasiticObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Parasitic_HtrPar_nget, self->data_ptr);
}

static int
Parasitic_set_HtrPar(ParasiticObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Parasitic_HtrPar_nset, self->data_ptr);
}

static PyObject *
Parasitic_get_HtrParF0(ParasiticObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Parasitic_HtrParF0_nget, self->data_ptr);
}

static int
Parasitic_set_HtrParF0(ParasiticObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Parasitic_HtrParF0_nset, self->data_ptr);
}

static PyObject *
Parasitic_get_HtrParF1(ParasiticObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Parasitic_HtrParF1_nget, self->data_ptr);
}

static int
Parasitic_set_HtrParF1(ParasiticObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Parasitic_HtrParF1_nset, self->data_ptr);
}

static PyObject *
Parasitic_get_HtrParF2(ParasiticObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Parasitic_HtrParF2_nget, self->data_ptr);
}

static int
Parasitic_set_HtrParF2(ParasiticObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Parasitic_HtrParF2_nset, self->data_ptr);
}

static PyObject *
Parasitic_get_HtrParPF(ParasiticObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Parasitic_HtrParPF_nget, self->data_ptr);
}

static int
Parasitic_set_HtrParPF(ParasiticObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Parasitic_HtrParPF_nset, self->data_ptr);
}

static PyObject *
Parasitic_get_PbFixPar(ParasiticObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Parasitic_PbFixPar_nget, self->data_ptr);
}

static int
Parasitic_set_PbFixPar(ParasiticObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Parasitic_PbFixPar_nset, self->data_ptr);
}

static PyObject *
Parasitic_get_SfPar(ParasiticObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Parasitic_SfPar_nget, self->data_ptr);
}

static int
Parasitic_set_SfPar(ParasiticObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Parasitic_SfPar_nset, self->data_ptr);
}

static PyObject *
Parasitic_get_SfParPF(ParasiticObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Parasitic_SfParPF_nget, self->data_ptr);
}

static int
Parasitic_set_SfParPF(ParasiticObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_TcstroughEmpirical_Parasitic_SfParPF_nset, self->data_ptr);
}

static PyGetSetDef Parasitic_getset[] = {
{"AntiFrPar", (getter)Parasitic_get_AntiFrPar,(setter)Parasitic_set_AntiFrPar,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"BOPPar", (getter)Parasitic_get_BOPPar,(setter)Parasitic_set_BOPPar,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"BOPParF0", (getter)Parasitic_get_BOPParF0,(setter)Parasitic_set_BOPParF0,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"BOPParF1", (getter)Parasitic_get_BOPParF1,(setter)Parasitic_set_BOPParF1,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"BOPParF2", (getter)Parasitic_get_BOPParF2,(setter)Parasitic_set_BOPParF2,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"BOPParPF", (getter)Parasitic_get_BOPParPF,(setter)Parasitic_set_BOPParPF,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"CHTFParF0", (getter)Parasitic_get_CHTFParF0,(setter)Parasitic_set_CHTFParF0,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"CHTFParF1", (getter)Parasitic_get_CHTFParF1,(setter)Parasitic_set_CHTFParF1,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"CHTFParF2", (getter)Parasitic_get_CHTFParF2,(setter)Parasitic_set_CHTFParF2,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"ChtfPar", (getter)Parasitic_get_ChtfPar,(setter)Parasitic_set_ChtfPar,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"ChtfParPF", (getter)Parasitic_get_ChtfParPF,(setter)Parasitic_set_ChtfParPF,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"CtOpF", (getter)Parasitic_get_CtOpF,(setter)Parasitic_set_CtOpF,
	PyDoc_STR("*float*: Label\n\n*Constraints*: INTEGER\n\n*Required*: True"),
 	NULL},
{"CtPar", (getter)Parasitic_get_CtPar,(setter)Parasitic_set_CtPar,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"CtParF0", (getter)Parasitic_get_CtParF0,(setter)Parasitic_set_CtParF0,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"CtParF1", (getter)Parasitic_get_CtParF1,(setter)Parasitic_set_CtParF1,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"CtParF2", (getter)Parasitic_get_CtParF2,(setter)Parasitic_set_CtParF2,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"CtParPF", (getter)Parasitic_get_CtParPF,(setter)Parasitic_set_CtParPF,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"HhtfPar", (getter)Parasitic_get_HhtfPar,(setter)Parasitic_set_HhtfPar,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"HhtfParF0", (getter)Parasitic_get_HhtfParF0,(setter)Parasitic_set_HhtfParF0,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"HhtfParF1", (getter)Parasitic_get_HhtfParF1,(setter)Parasitic_set_HhtfParF1,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"HhtfParF2", (getter)Parasitic_get_HhtfParF2,(setter)Parasitic_set_HhtfParF2,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"HhtfParPF", (getter)Parasitic_get_HhtfParPF,(setter)Parasitic_set_HhtfParPF,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"HtrPar", (getter)Parasitic_get_HtrPar,(setter)Parasitic_set_HtrPar,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"HtrParF0", (getter)Parasitic_get_HtrParF0,(setter)Parasitic_set_HtrParF0,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"HtrParF1", (getter)Parasitic_get_HtrParF1,(setter)Parasitic_set_HtrParF1,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"HtrParF2", (getter)Parasitic_get_HtrParF2,(setter)Parasitic_set_HtrParF2,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"HtrParPF", (getter)Parasitic_get_HtrParPF,(setter)Parasitic_set_HtrParPF,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"PbFixPar", (getter)Parasitic_get_PbFixPar,(setter)Parasitic_set_PbFixPar,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"SfPar", (getter)Parasitic_get_SfPar,(setter)Parasitic_set_SfPar,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
{"SfParPF", (getter)Parasitic_get_SfParPF,(setter)Parasitic_set_SfParPF,
	PyDoc_STR("*float*: Label\n\n*Required*: True"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject Parasitic_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"TcstroughEmpirical.Parasitic",             /*tp_name*/
		sizeof(ParasiticObject),          /*tp_basicsize*/
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
		Parasitic_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		Parasitic_getset,          /*tp_getset*/
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
	SAM_TcstroughEmpirical   data_ptr;
} OutputsObject;

static PyTypeObject Outputs_Type;

static PyObject *
Outputs_new(SAM_TcstroughEmpirical data_ptr)
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

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "TcstroughEmpirical", "Outputs")){
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
Outputs_get_AveSfTemp(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Outputs_AveSfTemp_aget, self->data_ptr);
}

static PyObject *
Outputs_get_ColEff(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Outputs_ColEff_aget, self->data_ptr);
}

static PyObject *
Outputs_get_CosTheta(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Outputs_CosTheta_aget, self->data_ptr);
}

static PyObject *
Outputs_get_Egr(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Outputs_Egr_aget, self->data_ptr);
}

static PyObject *
Outputs_get_EgrFos(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Outputs_EgrFos_aget, self->data_ptr);
}

static PyObject *
Outputs_get_EgrSol(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Outputs_EgrSol_aget, self->data_ptr);
}

static PyObject *
Outputs_get_EndLoss(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Outputs_EndLoss_aget, self->data_ptr);
}

static PyObject *
Outputs_get_Enet(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Outputs_Enet_aget, self->data_ptr);
}

static PyObject *
Outputs_get_Epar(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Outputs_Epar_aget, self->data_ptr);
}

static PyObject *
Outputs_get_EparAnti(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Outputs_EparAnti_aget, self->data_ptr);
}

static PyObject *
Outputs_get_EparBOP(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Outputs_EparBOP_aget, self->data_ptr);
}

static PyObject *
Outputs_get_EparCHTF(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Outputs_EparCHTF_aget, self->data_ptr);
}

static PyObject *
Outputs_get_EparCT(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Outputs_EparCT_aget, self->data_ptr);
}

static PyObject *
Outputs_get_EparHhtf(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Outputs_EparHhtf_aget, self->data_ptr);
}

static PyObject *
Outputs_get_EparHtr(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Outputs_EparHtr_aget, self->data_ptr);
}

static PyObject *
Outputs_get_EparOffLine(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Outputs_EparOffLine_aget, self->data_ptr);
}

static PyObject *
Outputs_get_EparOnLine(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Outputs_EparOnLine_aget, self->data_ptr);
}

static PyObject *
Outputs_get_EparPB(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Outputs_EparPB_aget, self->data_ptr);
}

static PyObject *
Outputs_get_EparSf(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Outputs_EparSf_aget, self->data_ptr);
}

static PyObject *
Outputs_get_Ets(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Outputs_Ets_aget, self->data_ptr);
}

static PyObject *
Outputs_get_Ftrack(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Outputs_Ftrack_aget, self->data_ptr);
}

static PyObject *
Outputs_get_IAM(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Outputs_IAM_aget, self->data_ptr);
}

static PyObject *
Outputs_get_QTsFull(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Outputs_QTsFull_aget, self->data_ptr);
}

static PyObject *
Outputs_get_QTsHl(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Outputs_QTsHl_aget, self->data_ptr);
}

static PyObject *
Outputs_get_QTurSu(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Outputs_QTurSu_aget, self->data_ptr);
}

static PyObject *
Outputs_get_Qdni(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Outputs_Qdni_aget, self->data_ptr);
}

static PyObject *
Outputs_get_Qdump(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Outputs_Qdump_aget, self->data_ptr);
}

static PyObject *
Outputs_get_Qfts(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Outputs_Qfts_aget, self->data_ptr);
}

static PyObject *
Outputs_get_Qgas(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Outputs_Qgas_aget, self->data_ptr);
}

static PyObject *
Outputs_get_QhtfFpHtr(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Outputs_QhtfFpHtr_aget, self->data_ptr);
}

static PyObject *
Outputs_get_QhtfFpTES(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Outputs_QhtfFpTES_aget, self->data_ptr);
}

static PyObject *
Outputs_get_QhtfFreezeProt(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Outputs_QhtfFreezeProt_aget, self->data_ptr);
}

static PyObject *
Outputs_get_Qmin(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Outputs_Qmin_aget, self->data_ptr);
}

static PyObject *
Outputs_get_QnipCosTh(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Outputs_QnipCosTh_aget, self->data_ptr);
}

static PyObject *
Outputs_get_Qsf(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Outputs_Qsf_aget, self->data_ptr);
}

static PyObject *
Outputs_get_QsfAbs(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Outputs_QsfAbs_aget, self->data_ptr);
}

static PyObject *
Outputs_get_QsfHceHL(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Outputs_QsfHceHL_aget, self->data_ptr);
}

static PyObject *
Outputs_get_QsfPipeHL(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Outputs_QsfPipeHL_aget, self->data_ptr);
}

static PyObject *
Outputs_get_QsfWarmup(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Outputs_QsfWarmup_aget, self->data_ptr);
}

static PyObject *
Outputs_get_Qsfnipcosth(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Outputs_Qsfnipcosth_aget, self->data_ptr);
}

static PyObject *
Outputs_get_Qtpb(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Outputs_Qtpb_aget, self->data_ptr);
}

static PyObject *
Outputs_get_Qtts(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Outputs_Qtts_aget, self->data_ptr);
}

static PyObject *
Outputs_get_RecHl(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Outputs_RecHl_aget, self->data_ptr);
}

static PyObject *
Outputs_get_RowShadow(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Outputs_RowShadow_aget, self->data_ptr);
}

static PyObject *
Outputs_get_SfMassFlow(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Outputs_SfMassFlow_aget, self->data_ptr);
}

static PyObject *
Outputs_get_SfTo(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Outputs_SfTo_aget, self->data_ptr);
}

static PyObject *
Outputs_get_Theta(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Outputs_Theta_aget, self->data_ptr);
}

static PyObject *
Outputs_get_TrackAngle(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Outputs_TrackAngle_aget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_W_cycle_gross(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Outputs_annual_W_cycle_gross_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_energy(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Outputs_annual_energy_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_fuel_usage(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Outputs_annual_fuel_usage_nget, self->data_ptr);
}

static PyObject *
Outputs_get_beam(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Outputs_beam_aget, self->data_ptr);
}

static PyObject *
Outputs_get_capacity_factor(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Outputs_capacity_factor_nget, self->data_ptr);
}

static PyObject *
Outputs_get_conversion_factor(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Outputs_conversion_factor_nget, self->data_ptr);
}

static PyObject *
Outputs_get_gen(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Outputs_gen_aget, self->data_ptr);
}

static PyObject *
Outputs_get_hour(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Outputs_hour_aget, self->data_ptr);
}

static PyObject *
Outputs_get_kwh_per_kw(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Outputs_kwh_per_kw_nget, self->data_ptr);
}

static PyObject *
Outputs_get_month(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Outputs_month_aget, self->data_ptr);
}

static PyObject *
Outputs_get_o_SfTi(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Outputs_o_SfTi_aget, self->data_ptr);
}

static PyObject *
Outputs_get_pres(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Outputs_pres_aget, self->data_ptr);
}

static PyObject *
Outputs_get_solazi(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Outputs_solazi_aget, self->data_ptr);
}

static PyObject *
Outputs_get_solzen(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Outputs_solzen_aget, self->data_ptr);
}

static PyObject *
Outputs_get_system_heat_rate(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Outputs_system_heat_rate_nget, self->data_ptr);
}

static PyObject *
Outputs_get_system_use_lifetime_output(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_TcstroughEmpirical_Outputs_system_use_lifetime_output_nget, self->data_ptr);
}

static PyObject *
Outputs_get_tdry(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Outputs_tdry_aget, self->data_ptr);
}

static PyObject *
Outputs_get_tou_value(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Outputs_tou_value_aget, self->data_ptr);
}

static PyObject *
Outputs_get_twet(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Outputs_twet_aget, self->data_ptr);
}

static PyObject *
Outputs_get_wspd(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_TcstroughEmpirical_Outputs_wspd_aget, self->data_ptr);
}

static PyGetSetDef Outputs_getset[] = {
{"AveSfTemp", (getter)Outputs_get_AveSfTemp,(setter)0,
	PyDoc_STR("*sequence*: Field HTF temperature average [C]"),
 	NULL},
{"ColEff", (getter)Outputs_get_ColEff,(setter)0,
	PyDoc_STR("*sequence*: Field collector thermal and optical efficiency"),
 	NULL},
{"CosTheta", (getter)Outputs_get_CosTheta,(setter)0,
	PyDoc_STR("*sequence*: Field collector cosine efficiency"),
 	NULL},
{"Egr", (getter)Outputs_get_Egr,(setter)0,
	PyDoc_STR("*sequence*: Cycle electrical power output (gross) [MWe]"),
 	NULL},
{"EgrFos", (getter)Outputs_get_EgrFos,(setter)0,
	PyDoc_STR("*sequence*: Cycle electrical power output (gross, fossil share) [MWe]"),
 	NULL},
{"EgrSol", (getter)Outputs_get_EgrSol,(setter)0,
	PyDoc_STR("*sequence*: Cycle electrical power output (gross, solar share) [MWe]"),
 	NULL},
{"EndLoss", (getter)Outputs_get_EndLoss,(setter)0,
	PyDoc_STR("*sequence*: Field collector optical end loss"),
 	NULL},
{"Enet", (getter)Outputs_get_Enet,(setter)0,
	PyDoc_STR("*sequence*: Cycle electrical power output (net) [MWe]"),
 	NULL},
{"Epar", (getter)Outputs_get_Epar,(setter)0,
	PyDoc_STR("*sequence*: Parasitic power total consumption [MWe]"),
 	NULL},
{"EparAnti", (getter)Outputs_get_EparAnti,(setter)0,
	PyDoc_STR("*sequence*: Parasitic power freeze protection pump [MWe]"),
 	NULL},
{"EparBOP", (getter)Outputs_get_EparBOP,(setter)0,
	PyDoc_STR("*sequence*: Parasitic power generation-dependent load [MWe]"),
 	NULL},
{"EparCHTF", (getter)Outputs_get_EparCHTF,(setter)0,
	PyDoc_STR("*sequence*: Parasitic power solar field HTF pump [MWe]"),
 	NULL},
{"EparCT", (getter)Outputs_get_EparCT,(setter)0,
	PyDoc_STR("*sequence*: Parasitic power condenser operation [MWe]"),
 	NULL},
{"EparHhtf", (getter)Outputs_get_EparHhtf,(setter)0,
	PyDoc_STR("*sequence*: Parasitic power TES and Cycle HTF pump [MWe]"),
 	NULL},
{"EparHtr", (getter)Outputs_get_EparHtr,(setter)0,
	PyDoc_STR("*sequence*: Parasitic power auxiliary heater operation [MWe]"),
 	NULL},
{"EparOffLine", (getter)Outputs_get_EparOffLine,(setter)0,
	PyDoc_STR("*sequence*: Parasitic power - offline total [MWe]"),
 	NULL},
{"EparOnLine", (getter)Outputs_get_EparOnLine,(setter)0,
	PyDoc_STR("*sequence*: Parasitic power - online total [MWe]"),
 	NULL},
{"EparPB", (getter)Outputs_get_EparPB,(setter)0,
	PyDoc_STR("*sequence*: Parasitic power fixed load [MWe]"),
 	NULL},
{"EparSf", (getter)Outputs_get_EparSf,(setter)0,
	PyDoc_STR("*sequence*: Parasitic power field collector drives [MWe]"),
 	NULL},
{"Ets", (getter)Outputs_get_Ets,(setter)0,
	PyDoc_STR("*sequence*: TES thermal energy available [MWht]"),
 	NULL},
{"Ftrack", (getter)Outputs_get_Ftrack,(setter)0,
	PyDoc_STR("*sequence*: Field collector fraction of time period tracking"),
 	NULL},
{"IAM", (getter)Outputs_get_IAM,(setter)0,
	PyDoc_STR("*sequence*: Field collector incidence angle modifier"),
 	NULL},
{"QTsFull", (getter)Outputs_get_QTsFull,(setter)0,
	PyDoc_STR("*sequence*: Cycle thermal energy dumped - TES is full [MWt]"),
 	NULL},
{"QTsHl", (getter)Outputs_get_QTsHl,(setter)0,
	PyDoc_STR("*sequence*: TES thermal losses from tank(s) [MWt]"),
 	NULL},
{"QTurSu", (getter)Outputs_get_QTurSu,(setter)0,
	PyDoc_STR("*sequence*: Cycle thermal startup energy [MWt]"),
 	NULL},
{"Qdni", (getter)Outputs_get_Qdni,(setter)0,
	PyDoc_STR("*sequence*: Field thermal power total incident [MWt]"),
 	NULL},
{"Qdump", (getter)Outputs_get_Qdump,(setter)0,
	PyDoc_STR("*sequence*: Cycle thermal energy dumped - solar field [MWt]"),
 	NULL},
{"Qfts", (getter)Outputs_get_Qfts,(setter)0,
	PyDoc_STR("*sequence*: TES thermal energy from storage [MWt]"),
 	NULL},
{"Qgas", (getter)Outputs_get_Qgas,(setter)0,
	PyDoc_STR("*sequence*: Fossil thermal power produced [MWt]"),
 	NULL},
{"QhtfFpHtr", (getter)Outputs_get_QhtfFpHtr,(setter)0,
	PyDoc_STR("*sequence*: Fossil freeze protection provided [MWt]"),
 	NULL},
{"QhtfFpTES", (getter)Outputs_get_QhtfFpTES,(setter)0,
	PyDoc_STR("*sequence*: Parasitic thermal TES freeze protection [MWt]"),
 	NULL},
{"QhtfFreezeProt", (getter)Outputs_get_QhtfFreezeProt,(setter)0,
	PyDoc_STR("*sequence*: Parasitic thermal field freeze protection [MWt]"),
 	NULL},
{"Qmin", (getter)Outputs_get_Qmin,(setter)0,
	PyDoc_STR("*sequence*: Cycle thermal energy dumped - min. load requirement [MWt]"),
 	NULL},
{"QnipCosTh", (getter)Outputs_get_QnipCosTh,(setter)0,
	PyDoc_STR("*sequence*: Field collector DNI-cosine product [W/m2]"),
 	NULL},
{"Qsf", (getter)Outputs_get_Qsf,(setter)0,
	PyDoc_STR("*sequence*: Field thermal power total produced [MWt]"),
 	NULL},
{"QsfAbs", (getter)Outputs_get_QsfAbs,(setter)0,
	PyDoc_STR("*sequence*: Field thermal power absorbed [MWt]"),
 	NULL},
{"QsfHceHL", (getter)Outputs_get_QsfHceHL,(setter)0,
	PyDoc_STR("*sequence*: Field thermal power receiver total loss [MWt]"),
 	NULL},
{"QsfPipeHL", (getter)Outputs_get_QsfPipeHL,(setter)0,
	PyDoc_STR("*sequence*: Field thermal power pipe losses [MWt]"),
 	NULL},
{"QsfWarmup", (getter)Outputs_get_QsfWarmup,(setter)0,
	PyDoc_STR("*sequence*: Field HTF energy inertial (consumed) [MWht]"),
 	NULL},
{"Qsfnipcosth", (getter)Outputs_get_Qsfnipcosth,(setter)0,
	PyDoc_STR("*sequence*: Field thermal power incident after cosine [MWt]"),
 	NULL},
{"Qtpb", (getter)Outputs_get_Qtpb,(setter)0,
	PyDoc_STR("*sequence*: Cycle thermal power input [MWt]"),
 	NULL},
{"Qtts", (getter)Outputs_get_Qtts,(setter)0,
	PyDoc_STR("*sequence*: TES thermal energy into storage [MWt]"),
 	NULL},
{"RecHl", (getter)Outputs_get_RecHl,(setter)0,
	PyDoc_STR("*sequence*: Field thermal power receiver heat loss [kJ/hr-m2]"),
 	NULL},
{"RowShadow", (getter)Outputs_get_RowShadow,(setter)0,
	PyDoc_STR("*sequence*: Field collector row shadowing loss"),
 	NULL},
{"SfMassFlow", (getter)Outputs_get_SfMassFlow,(setter)0,
	PyDoc_STR("*sequence*: Field HTF mass flow rate total [kg/s]"),
 	NULL},
{"SfTo", (getter)Outputs_get_SfTo,(setter)0,
	PyDoc_STR("*sequence*: Field HTF temperature hot header outlet [C]"),
 	NULL},
{"Theta", (getter)Outputs_get_Theta,(setter)0,
	PyDoc_STR("*sequence*: Field collector solar incidence angle [deg]"),
 	NULL},
{"TrackAngle", (getter)Outputs_get_TrackAngle,(setter)0,
	PyDoc_STR("*sequence*: Field collector tracking angle [deg]"),
 	NULL},
{"annual_W_cycle_gross", (getter)Outputs_get_annual_W_cycle_gross,(setter)0,
	PyDoc_STR("*float*: Electrical source - Power cycle gross output [kWh]"),
 	NULL},
{"annual_energy", (getter)Outputs_get_annual_energy,(setter)0,
	PyDoc_STR("*float*: Annual energy [kWh]"),
 	NULL},
{"annual_fuel_usage", (getter)Outputs_get_annual_fuel_usage,(setter)0,
	PyDoc_STR("*float*: Annual fuel usage [kWh]"),
 	NULL},
{"beam", (getter)Outputs_get_beam,(setter)0,
	PyDoc_STR("*sequence*: Resource Beam normal irradiance [W/m2]"),
 	NULL},
{"capacity_factor", (getter)Outputs_get_capacity_factor,(setter)0,
	PyDoc_STR("*float*: Capacity factor [%]"),
 	NULL},
{"conversion_factor", (getter)Outputs_get_conversion_factor,(setter)0,
	PyDoc_STR("*float*: Gross to Net Conversion Factor [%]"),
 	NULL},
{"gen", (getter)Outputs_get_gen,(setter)0,
	PyDoc_STR("*sequence*: System power generated [kW]"),
 	NULL},
{"hour", (getter)Outputs_get_hour,(setter)0,
	PyDoc_STR("*sequence*: Resource Hour of Day"),
 	NULL},
{"kwh_per_kw", (getter)Outputs_get_kwh_per_kw,(setter)0,
	PyDoc_STR("*float*: First year kWh/kW [kWh/kW]"),
 	NULL},
{"month", (getter)Outputs_get_month,(setter)0,
	PyDoc_STR("*sequence*: Resource Month"),
 	NULL},
{"o_SfTi", (getter)Outputs_get_o_SfTi,(setter)0,
	PyDoc_STR("*sequence*: Field HTF temperature cold header inlet [C]"),
 	NULL},
{"pres", (getter)Outputs_get_pres,(setter)0,
	PyDoc_STR("*sequence*: Resource Pressure [mbar]"),
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
{"system_use_lifetime_output", (getter)Outputs_get_system_use_lifetime_output,(setter)0,
	PyDoc_STR("*float*: Use lifetime output [0/1]"),
 	NULL},
{"tdry", (getter)Outputs_get_tdry,(setter)0,
	PyDoc_STR("*sequence*: Resource Dry bulb temperature [C]"),
 	NULL},
{"tou_value", (getter)Outputs_get_tou_value,(setter)0,
	PyDoc_STR("*sequence*: Resource time-of-use value"),
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
		"TcstroughEmpirical.Outputs",             /*tp_name*/
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
 * TcstroughEmpirical
 */

typedef struct {
	PyObject_HEAD
	PyObject            *x_attr;        /* Attributes dictionary */
	SAM_TcstroughEmpirical   data_ptr;
} TcstroughEmpiricalObject;

static PyTypeObject TcstroughEmpirical_Type;

#define TcstroughEmpiricalObject_Check(v)      (Py_TYPE(v) == &TcstroughEmpirical_Type)

static TcstroughEmpiricalObject *
newTcstroughEmpiricalObject(void* data_ptr)
{
	TcstroughEmpiricalObject *self;
	self = PyObject_New(TcstroughEmpiricalObject, &TcstroughEmpirical_Type);

	PySAM_TECH_ATTR("TcstroughEmpirical", SAM_TcstroughEmpirical_construct)

	PyObject* Weather_obj = Weather_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "Weather", Weather_obj);
	Py_DECREF(Weather_obj);

	PyObject* Trough_obj = Trough_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "Trough", Trough_obj);
	Py_DECREF(Trough_obj);

	PyObject* TouTranslator_obj = TouTranslator_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "TouTranslator", TouTranslator_obj);
	Py_DECREF(TouTranslator_obj);

	PyObject* Solarfield_obj = Solarfield_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "Solarfield", Solarfield_obj);
	Py_DECREF(Solarfield_obj);

	PyObject* Sca_obj = Sca_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "Sca", Sca_obj);
	Py_DECREF(Sca_obj);

	PyObject* Hce_obj = Hce_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "Hce", Hce_obj);
	Py_DECREF(Hce_obj);

	PyObject* Pwrb_obj = Pwrb_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "Pwrb", Pwrb_obj);
	Py_DECREF(Pwrb_obj);

	PyObject* Tes_obj = Tes_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "Tes", Tes_obj);
	Py_DECREF(Tes_obj);

	PyObject* Parasitic_obj = Parasitic_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "Parasitic", Parasitic_obj);
	Py_DECREF(Parasitic_obj);

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

/* TcstroughEmpirical methods */

static void
TcstroughEmpirical_dealloc(TcstroughEmpiricalObject *self)
{
	Py_XDECREF(self->x_attr);
	SAM_TcstroughEmpirical_destruct(self->data_ptr);
	PyObject_Del(self);
}


static PyObject *
TcstroughEmpirical_execute(TcstroughEmpiricalObject *self, PyObject *args)
{
	int verbosity = 0;

	if (!PyArg_ParseTuple(args, "|i", &verbosity))
		return NULL;

	SAM_error error = new_error();
	SAM_TcstroughEmpirical_execute(self->data_ptr, verbosity, &error);
	if (PySAM_has_error(error )) return NULL;

	Py_INCREF(Py_None);
	return Py_None;
}


static PyObject *
TcstroughEmpirical_assign(TcstroughEmpiricalObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_nested_dict((PyObject*)self, self->x_attr, self->data_ptr, dict, "TcstroughEmpirical"))
		return NULL;

	Py_INCREF(Py_None);
	return Py_None;
}


static PyObject *
TcstroughEmpirical_export(TcstroughEmpiricalObject *self, PyObject *args)
{
	return PySAM_export_to_nested_dict((PyObject *) self, self->x_attr);
}

static PyMethodDef TcstroughEmpirical_methods[] = {
		{"execute",            (PyCFunction)TcstroughEmpirical_execute,  METH_VARARGS,
				PyDoc_STR("execute(int verbosity) -> None\n Execute simulation with verbosity level 0 (default) or 1")},
		{"assign",            (PyCFunction)TcstroughEmpirical_assign,  METH_VARARGS,
				PyDoc_STR("assign(dict) -> None\n Assign attributes from nested dictionary, except for Outputs\n\n``nested_dict = { 'Weather': { var: val, ...}, ...}``")},
		{"export",            (PyCFunction)TcstroughEmpirical_export,  METH_VARARGS,
				PyDoc_STR("export() -> dict\n Export attributes into nested dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
TcstroughEmpirical_getattro(TcstroughEmpiricalObject *self, PyObject *name)
{
	return PySAM_get_attr((PyObject*) self, (PyObject*) self->x_attr, name);
}

static int
TcstroughEmpirical_setattr(TcstroughEmpiricalObject *self, const char *name, PyObject *v)
{
	return PySAM_set_attr((PyObject*)self, (PyObject*)self->x_attr, name, v);
}

static PyTypeObject TcstroughEmpirical_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"TcstroughEmpirical",            /*tp_name*/
		sizeof(TcstroughEmpiricalObject),/*tp_basicsize*/
		0,                          /*tp_itemsize*/
		/* methods */
		(destructor)TcstroughEmpirical_dealloc,    /*tp_dealloc*/
		0,                          /*tp_print*/
		(getattrfunc)0,             /*tp_getattr*/
		(setattrfunc)TcstroughEmpirical_setattr,   /*tp_setattr*/
		0,                          /*tp_reserved*/
		0,                          /*tp_repr*/
		0,                          /*tp_as_number*/
		0,                          /*tp_as_sequence*/
		0,                          /*tp_as_mapping*/
		0,                          /*tp_hash*/
		0,                          /*tp_call*/
		0,                          /*tp_str*/
		(getattrofunc)TcstroughEmpirical_getattro, /*tp_getattro*/
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
		TcstroughEmpirical_methods,      /*tp_methods*/
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


/* Function of no arguments returning new TcstroughEmpirical object */

static PyObject *
TcstroughEmpirical_new(PyObject *self, PyObject *args)
{
	TcstroughEmpiricalObject *rv;
	rv = newTcstroughEmpiricalObject(0);
	if (rv == NULL)
		return NULL;
	return (PyObject *)rv;
}

static PyObject *
TcstroughEmpirical_wrap(PyObject *self, PyObject *args)
{
	TcstroughEmpiricalObject *rv;
	long long int ptr = 0;  // 64 bit arch
	if (!PyArg_ParseTuple(args, "L:wrap", &ptr)){
		PyErr_BadArgument();
		return NULL;
	}
	rv = newTcstroughEmpiricalObject((void*)ptr);
	if (rv == NULL)
		return NULL;
	return (PyObject *)rv;
}

static PyObject *
TcstroughEmpirical_default(PyObject *self, PyObject *args)
{
	TcstroughEmpiricalObject *rv;
	char* def = 0;
	if (!PyArg_ParseTuple(args, "s:default", &def)){
		PyErr_BadArgument();
		return NULL;
	}
	rv = newTcstroughEmpiricalObject(0);
	if (rv == NULL)
		return NULL;

	PySAM_load_defaults((PyObject*)rv, rv->x_attr, rv->data_ptr, "TcstroughEmpirical", def);

	return (PyObject *)rv;
}

/* ---------- */


/* List of functions defined in the module */

static PyMethodDef TcstroughEmpiricalModule_methods[] = {
		{"new",             TcstroughEmpirical_new,         METH_VARARGS,
				PyDoc_STR("new() -> TcstroughEmpirical")},
		{"default",             TcstroughEmpirical_default,         METH_VARARGS,
				PyDoc_STR("default(config) -> TcstroughEmpirical\n\nUse financial model-specific default attributes\n"
				"config options:\n\n- \"EmpiricalTroughAllEquityPartnershipFlip\"\n- \"EmpiricalTroughCommercial\"\n- \"EmpiricalTroughCommercialPPA\"\n- \"EmpiricalTroughIndependentPowerProducer\"\n- \"EmpiricalTroughLCOECalculator\"\n- \"EmpiricalTroughLeveragedPartnershipFlip\"\n- \"EmpiricalTroughNone\"\n- \"EmpiricalTroughSaleLeaseback\"\n- \"EmpiricalTroughSingleOwner\"")},
		{"wrap",             TcstroughEmpirical_wrap,         METH_VARARGS,
				PyDoc_STR("wrap(ssc_data_t) -> TcstroughEmpirical\n\nUse existing PySSC data\n\n.. warning::\n\n	Do not call PySSC.data_free on the ssc_data_t provided to ``wrap``")},
		{NULL,              NULL}           /* sentinel */
};

PyDoc_STRVAR(module_doc,
			 "CSP parabolic trough model based on empirically-derived coefficients and equations for power generation");


static int
TcstroughEmpiricalModule_exec(PyObject *m)
{
	/* Finalize the type object including setting type of the new type
	 * object; doing it here is required for portability, too. */

	if (PySAM_load_lib(m) < 0) goto fail;
	if (PySAM_init_error(m) < 0) goto fail;

	TcstroughEmpirical_Type.tp_dict = PyDict_New();
	if (!TcstroughEmpirical_Type.tp_dict) { goto fail; }

	/// Add the AdjustmentFactors type object to TcstroughEmpirical_Type
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
	PyDict_SetItemString(TcstroughEmpirical_Type.tp_dict,
						 "AdjustmentFactors",
						 (PyObject*)AdjustmentFactors_Type);
	Py_DECREF(&AdjustmentFactors_Type);
	Py_XDECREF(AdjustmentFactors_Type);

	/// Add the Weather type object to TcstroughEmpirical_Type
	if (PyType_Ready(&Weather_Type) < 0) { goto fail; }
	PyDict_SetItemString(TcstroughEmpirical_Type.tp_dict,
				"Weather",
				(PyObject*)&Weather_Type);
	Py_DECREF(&Weather_Type);

	/// Add the Trough type object to TcstroughEmpirical_Type
	if (PyType_Ready(&Trough_Type) < 0) { goto fail; }
	PyDict_SetItemString(TcstroughEmpirical_Type.tp_dict,
				"Trough",
				(PyObject*)&Trough_Type);
	Py_DECREF(&Trough_Type);

	/// Add the TouTranslator type object to TcstroughEmpirical_Type
	if (PyType_Ready(&TouTranslator_Type) < 0) { goto fail; }
	PyDict_SetItemString(TcstroughEmpirical_Type.tp_dict,
				"TouTranslator",
				(PyObject*)&TouTranslator_Type);
	Py_DECREF(&TouTranslator_Type);

	/// Add the Solarfield type object to TcstroughEmpirical_Type
	if (PyType_Ready(&Solarfield_Type) < 0) { goto fail; }
	PyDict_SetItemString(TcstroughEmpirical_Type.tp_dict,
				"Solarfield",
				(PyObject*)&Solarfield_Type);
	Py_DECREF(&Solarfield_Type);

	/// Add the Sca type object to TcstroughEmpirical_Type
	if (PyType_Ready(&Sca_Type) < 0) { goto fail; }
	PyDict_SetItemString(TcstroughEmpirical_Type.tp_dict,
				"Sca",
				(PyObject*)&Sca_Type);
	Py_DECREF(&Sca_Type);

	/// Add the Hce type object to TcstroughEmpirical_Type
	if (PyType_Ready(&Hce_Type) < 0) { goto fail; }
	PyDict_SetItemString(TcstroughEmpirical_Type.tp_dict,
				"Hce",
				(PyObject*)&Hce_Type);
	Py_DECREF(&Hce_Type);

	/// Add the Pwrb type object to TcstroughEmpirical_Type
	if (PyType_Ready(&Pwrb_Type) < 0) { goto fail; }
	PyDict_SetItemString(TcstroughEmpirical_Type.tp_dict,
				"Pwrb",
				(PyObject*)&Pwrb_Type);
	Py_DECREF(&Pwrb_Type);

	/// Add the Tes type object to TcstroughEmpirical_Type
	if (PyType_Ready(&Tes_Type) < 0) { goto fail; }
	PyDict_SetItemString(TcstroughEmpirical_Type.tp_dict,
				"Tes",
				(PyObject*)&Tes_Type);
	Py_DECREF(&Tes_Type);

	/// Add the Parasitic type object to TcstroughEmpirical_Type
	if (PyType_Ready(&Parasitic_Type) < 0) { goto fail; }
	PyDict_SetItemString(TcstroughEmpirical_Type.tp_dict,
				"Parasitic",
				(PyObject*)&Parasitic_Type);
	Py_DECREF(&Parasitic_Type);

	/// Add the Outputs type object to TcstroughEmpirical_Type
	if (PyType_Ready(&Outputs_Type) < 0) { goto fail; }
	PyDict_SetItemString(TcstroughEmpirical_Type.tp_dict,
				"Outputs",
				(PyObject*)&Outputs_Type);
	Py_DECREF(&Outputs_Type);

	/// Add the TcstroughEmpirical type object to the module
	if (PyType_Ready(&TcstroughEmpirical_Type) < 0) { goto fail; }
	PyModule_AddObject(m,
				"TcstroughEmpirical",
				(PyObject*)&TcstroughEmpirical_Type);

	return 0;
	fail:
	Py_XDECREF(m);
	return -1;
}

static struct PyModuleDef_Slot TcstroughEmpiricalModule_slots[] = {
		{Py_mod_exec, TcstroughEmpiricalModule_exec},
		{0, NULL},
};

static struct PyModuleDef TcstroughEmpiricalModule = {
		PyModuleDef_HEAD_INIT,
		"TcstroughEmpirical",
		module_doc,
		0,
		TcstroughEmpiricalModule_methods,
		TcstroughEmpiricalModule_slots,
		NULL,
		NULL,
		NULL
};

/* Export function for the module */

PyMODINIT_FUNC
PyInit_TcstroughEmpirical(void)
{
	return PyModuleDef_Init(&TcstroughEmpiricalModule);
}