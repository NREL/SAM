#include <Python.h>

#include <SAM_Tcsdish.h>
#include <SAM_api.h>

#include "PySAM_utils.h"



	/*
	 * Weather Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Tcsdish   data_ptr;
} WeatherObject;

static PyTypeObject Weather_Type;

static PyObject *
Weather_new(SAM_Tcsdish data_ptr)
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

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Tcsdish", "Weather")){
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
Weather_get_file_name(WeatherObject *self, void *closure)
{
	return PySAM_string_getter(SAM_Tcsdish_Weather_file_name_sget, self->data_ptr);
}

static int
Weather_set_file_name(WeatherObject *self, PyObject *value, void *closure)
{
	return PySAM_string_setter(value, SAM_Tcsdish_Weather_file_name_sset, self->data_ptr);
}

static PyGetSetDef Weather_getset[] = {
{"file_name", (getter)Weather_get_file_name,(setter)Weather_set_file_name,
	PyDoc_STR("*str*: local weather file path\n\n*Constraints*: LOCAL_FILE\n\n*Required*: True"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject Weather_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Tcsdish.Weather",             /*tp_name*/
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
	 * Dish Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Tcsdish   data_ptr;
} DishObject;

static PyTypeObject Dish_Type;

static PyObject *
Dish_new(SAM_Tcsdish data_ptr)
{
	PyObject* new_obj = Dish_Type.tp_alloc(&Dish_Type,0);

	DishObject* Dish_obj = (DishObject*)new_obj;

	Dish_obj->data_ptr = data_ptr;

	return new_obj;
}

/* Dish methods */

static PyObject *
Dish_assign(DishObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Tcsdish", "Dish")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
Dish_export(DishObject *self, PyObject *args)
{
	PyTypeObject* tp = &Dish_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef Dish_methods[] = {
		{"assign",            (PyCFunction)Dish_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``Dish_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)Dish_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
Dish_get_system_capacity(DishObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Dish_system_capacity_nget, self->data_ptr);
}

static int
Dish_set_system_capacity(DishObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Dish_system_capacity_nset, self->data_ptr);
}

static PyGetSetDef Dish_getset[] = {
{"system_capacity", (getter)Dish_get_system_capacity,(setter)Dish_set_system_capacity,
	PyDoc_STR("*float*: Nameplate capacity [kW]\n\n*Required*: True"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject Dish_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Tcsdish.Dish",             /*tp_name*/
		sizeof(DishObject),          /*tp_basicsize*/
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
		Dish_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		Dish_getset,          /*tp_getset*/
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
	 * Type295 Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Tcsdish   data_ptr;
} Type295Object;

static PyTypeObject Type295_Type;

static PyObject *
Type295_new(SAM_Tcsdish data_ptr)
{
	PyObject* new_obj = Type295_Type.tp_alloc(&Type295_Type,0);

	Type295Object* Type295_obj = (Type295Object*)new_obj;

	Type295_obj->data_ptr = data_ptr;

	return new_obj;
}

/* Type295 methods */

static PyObject *
Type295_assign(Type295Object *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Tcsdish", "Type295")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
Type295_export(Type295Object *self, PyObject *args)
{
	PyTypeObject* tp = &Type295_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef Type295_methods[] = {
		{"assign",            (PyCFunction)Type295_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``Type295_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)Type295_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
Type295_get_A_proj(Type295Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type295_A_proj_nget, self->data_ptr);
}

static int
Type295_set_A_proj(Type295Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type295_A_proj_nset, self->data_ptr);
}

static PyObject *
Type295_get_A_total(Type295Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type295_A_total_nget, self->data_ptr);
}

static int
Type295_set_A_total(Type295Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type295_A_total_nset, self->data_ptr);
}

static PyObject *
Type295_get_I_cut_in(Type295Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type295_I_cut_in_nget, self->data_ptr);
}

static int
Type295_set_I_cut_in(Type295Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type295_I_cut_in_nset, self->data_ptr);
}

static PyObject *
Type295_get_d_ap(Type295Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type295_d_ap_nget, self->data_ptr);
}

static int
Type295_set_d_ap(Type295Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type295_d_ap_nset, self->data_ptr);
}

static PyObject *
Type295_get_d_ap_test(Type295Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type295_d_ap_test_nget, self->data_ptr);
}

static int
Type295_set_d_ap_test(Type295Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type295_d_ap_test_nset, self->data_ptr);
}

static PyObject *
Type295_get_ew_dish_sep(Type295Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type295_ew_dish_sep_nget, self->data_ptr);
}

static int
Type295_set_ew_dish_sep(Type295Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type295_ew_dish_sep_nset, self->data_ptr);
}

static PyObject *
Type295_get_h_slot_gap(Type295Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type295_h_slot_gap_nget, self->data_ptr);
}

static int
Type295_set_h_slot_gap(Type295Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type295_h_slot_gap_nset, self->data_ptr);
}

static PyObject *
Type295_get_n_ew(Type295Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type295_n_ew_nget, self->data_ptr);
}

static int
Type295_set_n_ew(Type295Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type295_n_ew_nset, self->data_ptr);
}

static PyObject *
Type295_get_n_ns(Type295Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type295_n_ns_nget, self->data_ptr);
}

static int
Type295_set_n_ns(Type295Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type295_n_ns_nset, self->data_ptr);
}

static PyObject *
Type295_get_ns_dish_sep(Type295Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type295_ns_dish_sep_nget, self->data_ptr);
}

static int
Type295_set_ns_dish_sep(Type295Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type295_ns_dish_sep_nset, self->data_ptr);
}

static PyObject *
Type295_get_rho(Type295Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type295_rho_nget, self->data_ptr);
}

static int
Type295_set_rho(Type295Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type295_rho_nset, self->data_ptr);
}

static PyObject *
Type295_get_slope_ew(Type295Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type295_slope_ew_nget, self->data_ptr);
}

static int
Type295_set_slope_ew(Type295Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type295_slope_ew_nset, self->data_ptr);
}

static PyObject *
Type295_get_slope_ns(Type295Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type295_slope_ns_nget, self->data_ptr);
}

static int
Type295_set_slope_ns(Type295Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type295_slope_ns_nset, self->data_ptr);
}

static PyObject *
Type295_get_test_L_focal(Type295Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type295_test_L_focal_nget, self->data_ptr);
}

static int
Type295_set_test_L_focal(Type295Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type295_test_L_focal_nset, self->data_ptr);
}

static PyObject *
Type295_get_test_if(Type295Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type295_test_if_nget, self->data_ptr);
}

static int
Type295_set_test_if(Type295Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type295_test_if_nset, self->data_ptr);
}

static PyObject *
Type295_get_w_slot_gap(Type295Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type295_w_slot_gap_nget, self->data_ptr);
}

static int
Type295_set_w_slot_gap(Type295Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type295_w_slot_gap_nset, self->data_ptr);
}

static PyObject *
Type295_get_wind_stow_speed(Type295Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type295_wind_stow_speed_nget, self->data_ptr);
}

static int
Type295_set_wind_stow_speed(Type295Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type295_wind_stow_speed_nset, self->data_ptr);
}

static PyGetSetDef Type295_getset[] = {
{"A_proj", (getter)Type295_get_A_proj,(setter)Type295_set_A_proj,
	PyDoc_STR("*float*: Projected mirror area [m^2]\n\n*Required*: True"),
 	NULL},
{"A_total", (getter)Type295_get_A_total,(setter)Type295_set_A_total,
	PyDoc_STR("*float*: Total Area [m^2]\n\n*Required*: True"),
 	NULL},
{"I_cut_in", (getter)Type295_get_I_cut_in,(setter)Type295_set_I_cut_in,
	PyDoc_STR("*float*: Insolation cut in value [W/m^2]\n\n*Required*: True"),
 	NULL},
{"d_ap", (getter)Type295_get_d_ap,(setter)Type295_set_d_ap,
	PyDoc_STR("*float*: Dish aperture diameter [m]\n\n*Required*: True"),
 	NULL},
{"d_ap_test", (getter)Type295_get_d_ap_test,(setter)Type295_set_d_ap_test,
	PyDoc_STR("*float*: Receiver aperture diameter during test [m]\n\n*Required*: True"),
 	NULL},
{"ew_dish_sep", (getter)Type295_get_ew_dish_sep,(setter)Type295_set_ew_dish_sep,
	PyDoc_STR("*float*: Collector separation East-West [m]\n\n*Required*: True"),
 	NULL},
{"h_slot_gap", (getter)Type295_get_h_slot_gap,(setter)Type295_set_h_slot_gap,
	PyDoc_STR("*float*: Slot gap height [m]\n\n*Required*: True"),
 	NULL},
{"n_ew", (getter)Type295_get_n_ew,(setter)Type295_set_n_ew,
	PyDoc_STR("*float*: Number of collectors East-West [-]\n\n*Constraints*: INTEGER\n\n*Required*: True"),
 	NULL},
{"n_ns", (getter)Type295_get_n_ns,(setter)Type295_set_n_ns,
	PyDoc_STR("*float*: Number of collectors North-South [-]\n\n*Constraints*: INTEGER\n\n*Required*: True"),
 	NULL},
{"ns_dish_sep", (getter)Type295_get_ns_dish_sep,(setter)Type295_set_ns_dish_sep,
	PyDoc_STR("*float*: Collector separation North-South [m]\n\n*Required*: True"),
 	NULL},
{"rho", (getter)Type295_get_rho,(setter)Type295_set_rho,
	PyDoc_STR("*float*: Mirror surface reflectivity [-]\n\n*Required*: True"),
 	NULL},
{"slope_ew", (getter)Type295_get_slope_ew,(setter)Type295_set_slope_ew,
	PyDoc_STR("*float*: East-West ground slope [%]\n\n*Required*: True"),
 	NULL},
{"slope_ns", (getter)Type295_get_slope_ns,(setter)Type295_set_slope_ns,
	PyDoc_STR("*float*: North-South ground slope [%]\n\n*Required*: True"),
 	NULL},
{"test_L_focal", (getter)Type295_get_test_L_focal,(setter)Type295_set_test_L_focal,
	PyDoc_STR("*float*: Focal length of mirror system [m]\n\n*Required*: True"),
 	NULL},
{"test_if", (getter)Type295_get_test_if,(setter)Type295_set_test_if,
	PyDoc_STR("*float*: Test intercept factor [-]\n\n*Required*: True"),
 	NULL},
{"w_slot_gap", (getter)Type295_get_w_slot_gap,(setter)Type295_set_w_slot_gap,
	PyDoc_STR("*float*: Slot gap width [m]\n\n*Required*: True"),
 	NULL},
{"wind_stow_speed", (getter)Type295_get_wind_stow_speed,(setter)Type295_set_wind_stow_speed,
	PyDoc_STR("*float*: Wind stow speed [m/s]\n\n*Required*: True"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject Type295_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Tcsdish.Type295",             /*tp_name*/
		sizeof(Type295Object),          /*tp_basicsize*/
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
		Type295_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		Type295_getset,          /*tp_getset*/
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
	 * Type296 Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Tcsdish   data_ptr;
} Type296Object;

static PyTypeObject Type296_Type;

static PyObject *
Type296_new(SAM_Tcsdish data_ptr)
{
	PyObject* new_obj = Type296_Type.tp_alloc(&Type296_Type,0);

	Type296Object* Type296_obj = (Type296Object*)new_obj;

	Type296_obj->data_ptr = data_ptr;

	return new_obj;
}

/* Type296 methods */

static PyObject *
Type296_assign(Type296Object *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Tcsdish", "Type296")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
Type296_export(Type296Object *self, PyObject *args)
{
	PyTypeObject* tp = &Type296_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef Type296_methods[] = {
		{"assign",            (PyCFunction)Type296_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``Type296_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)Type296_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
Type296_get_A_absorber(Type296Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type296_A_absorber_nget, self->data_ptr);
}

static int
Type296_set_A_absorber(Type296Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type296_A_absorber_nset, self->data_ptr);
}

static PyObject *
Type296_get_A_wall(Type296Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type296_A_wall_nget, self->data_ptr);
}

static int
Type296_set_A_wall(Type296Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type296_A_wall_nset, self->data_ptr);
}

static PyObject *
Type296_get_DELTA_T_DIR(Type296Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type296_DELTA_T_DIR_nget, self->data_ptr);
}

static int
Type296_set_DELTA_T_DIR(Type296Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type296_DELTA_T_DIR_nset, self->data_ptr);
}

static PyObject *
Type296_get_DELTA_T_REFLUX(Type296Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type296_DELTA_T_REFLUX_nget, self->data_ptr);
}

static int
Type296_set_DELTA_T_REFLUX(Type296Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type296_DELTA_T_REFLUX_nset, self->data_ptr);
}

static PyObject *
Type296_get_L_cav(Type296Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type296_L_cav_nget, self->data_ptr);
}

static int
Type296_set_L_cav(Type296Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type296_L_cav_nset, self->data_ptr);
}

static PyObject *
Type296_get_L_insulation(Type296Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type296_L_insulation_nget, self->data_ptr);
}

static int
Type296_set_L_insulation(Type296Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type296_L_insulation_nset, self->data_ptr);
}

static PyObject *
Type296_get_P_cav(Type296Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type296_P_cav_nget, self->data_ptr);
}

static int
Type296_set_P_cav(Type296Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type296_P_cav_nset, self->data_ptr);
}

static PyObject *
Type296_get_T_heater_head_high(Type296Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type296_T_heater_head_high_nget, self->data_ptr);
}

static int
Type296_set_T_heater_head_high(Type296Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type296_T_heater_head_high_nset, self->data_ptr);
}

static PyObject *
Type296_get_T_heater_head_low(Type296Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type296_T_heater_head_low_nget, self->data_ptr);
}

static int
Type296_set_T_heater_head_low(Type296Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type296_T_heater_head_low_nset, self->data_ptr);
}

static PyObject *
Type296_get_alpha_absorber(Type296Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type296_alpha_absorber_nget, self->data_ptr);
}

static int
Type296_set_alpha_absorber(Type296Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type296_alpha_absorber_nset, self->data_ptr);
}

static PyObject *
Type296_get_alpha_wall(Type296Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type296_alpha_wall_nget, self->data_ptr);
}

static int
Type296_set_alpha_wall(Type296Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type296_alpha_wall_nset, self->data_ptr);
}

static PyObject *
Type296_get_d_cav(Type296Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type296_d_cav_nget, self->data_ptr);
}

static int
Type296_set_d_cav(Type296Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type296_d_cav_nset, self->data_ptr);
}

static PyObject *
Type296_get_k_insulation(Type296Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type296_k_insulation_nget, self->data_ptr);
}

static int
Type296_set_k_insulation(Type296Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type296_k_insulation_nset, self->data_ptr);
}

static PyObject *
Type296_get_rec_type(Type296Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type296_rec_type_nget, self->data_ptr);
}

static int
Type296_set_rec_type(Type296Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type296_rec_type_nset, self->data_ptr);
}

static PyObject *
Type296_get_transmittance_cover(Type296Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type296_transmittance_cover_nget, self->data_ptr);
}

static int
Type296_set_transmittance_cover(Type296Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type296_transmittance_cover_nset, self->data_ptr);
}

static PyGetSetDef Type296_getset[] = {
{"A_absorber", (getter)Type296_get_A_absorber,(setter)Type296_set_A_absorber,
	PyDoc_STR("*float*: Absorber surface area [m^2]\n\n*Required*: True"),
 	NULL},
{"A_wall", (getter)Type296_get_A_wall,(setter)Type296_set_A_wall,
	PyDoc_STR("*float*: Cavity surface area [m^2]\n\n*Required*: True"),
 	NULL},
{"DELTA_T_DIR", (getter)Type296_get_DELTA_T_DIR,(setter)Type296_set_DELTA_T_DIR,
	PyDoc_STR("*float*: Delta temperature for DIR receiver [K]\n\n*Required*: True"),
 	NULL},
{"DELTA_T_REFLUX", (getter)Type296_get_DELTA_T_REFLUX,(setter)Type296_set_DELTA_T_REFLUX,
	PyDoc_STR("*float*: Delta temp for REFLUX receiver (always = 40) [K]\n\n*Required*: True"),
 	NULL},
{"L_cav", (getter)Type296_get_L_cav,(setter)Type296_set_L_cav,
	PyDoc_STR("*float*: Internal depth of cavity perp to aperture [m]\n\n*Required*: True"),
 	NULL},
{"L_insulation", (getter)Type296_get_L_insulation,(setter)Type296_set_L_insulation,
	PyDoc_STR("*float*: Insulation thickness [m]\n\n*Required*: True"),
 	NULL},
{"P_cav", (getter)Type296_get_P_cav,(setter)Type296_set_P_cav,
	PyDoc_STR("*float*: Internal cavity pressure with aperture covered [kPa]\n\n*Required*: True"),
 	NULL},
{"T_heater_head_high", (getter)Type296_get_T_heater_head_high,(setter)Type296_set_T_heater_head_high,
	PyDoc_STR("*float*: Heater Head Set Temperature [K]\n\n*Required*: True"),
 	NULL},
{"T_heater_head_low", (getter)Type296_get_T_heater_head_low,(setter)Type296_set_T_heater_head_low,
	PyDoc_STR("*float*: Header Head Lowest Temperature [K]\n\n*Required*: True"),
 	NULL},
{"alpha_absorber", (getter)Type296_get_alpha_absorber,(setter)Type296_set_alpha_absorber,
	PyDoc_STR("*float*: Absorber absorptance [-]\n\n*Required*: True"),
 	NULL},
{"alpha_wall", (getter)Type296_get_alpha_wall,(setter)Type296_set_alpha_wall,
	PyDoc_STR("*float*: Cavity absorptance [-]\n\n*Required*: True"),
 	NULL},
{"d_cav", (getter)Type296_get_d_cav,(setter)Type296_set_d_cav,
	PyDoc_STR("*float*: Internal diameter of cavity perp to aperture [m]\n\n*Required*: True"),
 	NULL},
{"k_insulation", (getter)Type296_get_k_insulation,(setter)Type296_set_k_insulation,
	PyDoc_STR("*float*: Insulation thermal conductivity [W/m-K]\n\n*Required*: True"),
 	NULL},
{"rec_type", (getter)Type296_get_rec_type,(setter)Type296_set_rec_type,
	PyDoc_STR("*float*: Receiver type (always = 1) [-]\n\n*Required*: True"),
 	NULL},
{"transmittance_cover", (getter)Type296_get_transmittance_cover,(setter)Type296_set_transmittance_cover,
	PyDoc_STR("*float*: Transmittance cover (always = 1) [-]\n\n*Required*: True"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject Type296_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Tcsdish.Type296",             /*tp_name*/
		sizeof(Type296Object),          /*tp_basicsize*/
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
		Type296_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		Type296_getset,          /*tp_getset*/
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
	 * Type297 Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Tcsdish   data_ptr;
} Type297Object;

static PyTypeObject Type297_Type;

static PyObject *
Type297_new(SAM_Tcsdish data_ptr)
{
	PyObject* new_obj = Type297_Type.tp_alloc(&Type297_Type,0);

	Type297Object* Type297_obj = (Type297Object*)new_obj;

	Type297_obj->data_ptr = data_ptr;

	return new_obj;
}

/* Type297 methods */

static PyObject *
Type297_assign(Type297Object *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Tcsdish", "Type297")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
Type297_export(Type297Object *self, PyObject *args)
{
	PyTypeObject* tp = &Type297_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef Type297_methods[] = {
		{"assign",            (PyCFunction)Type297_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``Type297_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)Type297_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
Type297_get_Beale_const_coef(Type297Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type297_Beale_const_coef_nget, self->data_ptr);
}

static int
Type297_set_Beale_const_coef(Type297Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type297_Beale_const_coef_nset, self->data_ptr);
}

static PyObject *
Type297_get_Beale_first_coef(Type297Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type297_Beale_first_coef_nget, self->data_ptr);
}

static int
Type297_set_Beale_first_coef(Type297Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type297_Beale_first_coef_nset, self->data_ptr);
}

static PyObject *
Type297_get_Beale_fourth_coef(Type297Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type297_Beale_fourth_coef_nget, self->data_ptr);
}

static int
Type297_set_Beale_fourth_coef(Type297Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type297_Beale_fourth_coef_nset, self->data_ptr);
}

static PyObject *
Type297_get_Beale_square_coef(Type297Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type297_Beale_square_coef_nget, self->data_ptr);
}

static int
Type297_set_Beale_square_coef(Type297Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type297_Beale_square_coef_nset, self->data_ptr);
}

static PyObject *
Type297_get_Beale_third_coef(Type297Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type297_Beale_third_coef_nget, self->data_ptr);
}

static int
Type297_set_Beale_third_coef(Type297Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type297_Beale_third_coef_nset, self->data_ptr);
}

static PyObject *
Type297_get_Pressure_coef(Type297Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type297_Pressure_coef_nget, self->data_ptr);
}

static int
Type297_set_Pressure_coef(Type297Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type297_Pressure_coef_nset, self->data_ptr);
}

static PyObject *
Type297_get_Pressure_first(Type297Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type297_Pressure_first_nget, self->data_ptr);
}

static int
Type297_set_Pressure_first(Type297Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type297_Pressure_first_nset, self->data_ptr);
}

static PyObject *
Type297_get_T_compression_in(Type297Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type297_T_compression_in_nget, self->data_ptr);
}

static int
Type297_set_T_compression_in(Type297Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type297_T_compression_in_nset, self->data_ptr);
}

static PyObject *
Type297_get_V_displaced(Type297Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type297_V_displaced_nget, self->data_ptr);
}

static int
Type297_set_V_displaced(Type297Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type297_V_displaced_nset, self->data_ptr);
}

static PyObject *
Type297_get_engine_speed(Type297Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type297_engine_speed_nget, self->data_ptr);
}

static int
Type297_set_engine_speed(Type297Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type297_engine_speed_nset, self->data_ptr);
}

static PyGetSetDef Type297_getset[] = {
{"Beale_const_coef", (getter)Type297_get_Beale_const_coef,(setter)Type297_set_Beale_const_coef,
	PyDoc_STR("*float*: Beale Constant Coefficient [-]\n\n*Required*: True"),
 	NULL},
{"Beale_first_coef", (getter)Type297_get_Beale_first_coef,(setter)Type297_set_Beale_first_coef,
	PyDoc_STR("*float*: Beale first-order coefficient [1/W]\n\n*Required*: True"),
 	NULL},
{"Beale_fourth_coef", (getter)Type297_get_Beale_fourth_coef,(setter)Type297_set_Beale_fourth_coef,
	PyDoc_STR("*float*: Beale fourth-order coefficient [1/W^4]\n\n*Required*: True"),
 	NULL},
{"Beale_square_coef", (getter)Type297_get_Beale_square_coef,(setter)Type297_set_Beale_square_coef,
	PyDoc_STR("*float*: Beale second-order coefficient [1/W^2]\n\n*Required*: True"),
 	NULL},
{"Beale_third_coef", (getter)Type297_get_Beale_third_coef,(setter)Type297_set_Beale_third_coef,
	PyDoc_STR("*float*: Beale third-order coefficient [1/W^3]\n\n*Required*: True"),
 	NULL},
{"Pressure_coef", (getter)Type297_get_Pressure_coef,(setter)Type297_set_Pressure_coef,
	PyDoc_STR("*float*: Pressure constant coefficient [MPa]\n\n*Required*: True"),
 	NULL},
{"Pressure_first", (getter)Type297_get_Pressure_first,(setter)Type297_set_Pressure_first,
	PyDoc_STR("*float*: Pressure first-order coefficient [MPa/W]\n\n*Required*: True"),
 	NULL},
{"T_compression_in", (getter)Type297_get_T_compression_in,(setter)Type297_set_T_compression_in,
	PyDoc_STR("*float*: Receiver efficiency [C]\n\n*Required*: True"),
 	NULL},
{"V_displaced", (getter)Type297_get_V_displaced,(setter)Type297_set_V_displaced,
	PyDoc_STR("*float*: Displaced engine volume [m3]\n\n*Required*: True"),
 	NULL},
{"engine_speed", (getter)Type297_get_engine_speed,(setter)Type297_set_engine_speed,
	PyDoc_STR("*float*: Engine operating speed [rpm]\n\n*Required*: True"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject Type297_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Tcsdish.Type297",             /*tp_name*/
		sizeof(Type297Object),          /*tp_basicsize*/
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
		Type297_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		Type297_getset,          /*tp_getset*/
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
	 * Type298 Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Tcsdish   data_ptr;
} Type298Object;

static PyTypeObject Type298_Type;

static PyObject *
Type298_new(SAM_Tcsdish data_ptr)
{
	PyObject* new_obj = Type298_Type.tp_alloc(&Type298_Type,0);

	Type298Object* Type298_obj = (Type298Object*)new_obj;

	Type298_obj->data_ptr = data_ptr;

	return new_obj;
}

/* Type298 methods */

static PyObject *
Type298_assign(Type298Object *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Tcsdish", "Type298")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
Type298_export(Type298Object *self, PyObject *args)
{
	PyTypeObject* tp = &Type298_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef Type298_methods[] = {
		{"assign",            (PyCFunction)Type298_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``Type298_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)Type298_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
Type298_get_P_controls(Type298Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type298_P_controls_nget, self->data_ptr);
}

static int
Type298_set_P_controls(Type298Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type298_P_controls_nset, self->data_ptr);
}

static PyObject *
Type298_get_P_tower_fan(Type298Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type298_P_tower_fan_nget, self->data_ptr);
}

static int
Type298_set_P_tower_fan(Type298Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type298_P_tower_fan_nset, self->data_ptr);
}

static PyObject *
Type298_get_T_cool_speed2(Type298Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type298_T_cool_speed2_nget, self->data_ptr);
}

static int
Type298_set_T_cool_speed2(Type298Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type298_T_cool_speed2_nset, self->data_ptr);
}

static PyObject *
Type298_get_T_cool_speed3(Type298Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type298_T_cool_speed3_nget, self->data_ptr);
}

static int
Type298_set_T_cool_speed3(Type298Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type298_T_cool_speed3_nset, self->data_ptr);
}

static PyObject *
Type298_get_Tower_water_outlet_temp(Type298Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type298_Tower_water_outlet_temp_nget, self->data_ptr);
}

static int
Type298_set_Tower_water_outlet_temp(Type298Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type298_Tower_water_outlet_temp_nset, self->data_ptr);
}

static PyObject *
Type298_get_b_cooler(Type298Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type298_b_cooler_nget, self->data_ptr);
}

static int
Type298_set_b_cooler(Type298Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type298_b_cooler_nset, self->data_ptr);
}

static PyObject *
Type298_get_b_radiator(Type298Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type298_b_radiator_nget, self->data_ptr);
}

static int
Type298_set_b_radiator(Type298Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type298_b_radiator_nset, self->data_ptr);
}

static PyObject *
Type298_get_cooling_fluid(Type298Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type298_cooling_fluid_nget, self->data_ptr);
}

static int
Type298_set_cooling_fluid(Type298Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type298_cooling_fluid_nset, self->data_ptr);
}

static PyObject *
Type298_get_cooling_tower_on(Type298Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type298_cooling_tower_on_nget, self->data_ptr);
}

static int
Type298_set_cooling_tower_on(Type298Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type298_cooling_tower_on_nset, self->data_ptr);
}

static PyObject *
Type298_get_d_pipe_tower(Type298Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type298_d_pipe_tower_nget, self->data_ptr);
}

static int
Type298_set_d_pipe_tower(Type298Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type298_d_pipe_tower_nset, self->data_ptr);
}

static PyObject *
Type298_get_epsilon_cooler_test(Type298Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type298_epsilon_cooler_test_nget, self->data_ptr);
}

static int
Type298_set_epsilon_cooler_test(Type298Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type298_epsilon_cooler_test_nset, self->data_ptr);
}

static PyObject *
Type298_get_epsilon_power_test(Type298Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type298_epsilon_power_test_nget, self->data_ptr);
}

static int
Type298_set_epsilon_power_test(Type298Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type298_epsilon_power_test_nset, self->data_ptr);
}

static PyObject *
Type298_get_epsilon_radiator_test(Type298Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type298_epsilon_radiator_test_nget, self->data_ptr);
}

static int
Type298_set_epsilon_radiator_test(Type298Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type298_epsilon_radiator_test_nset, self->data_ptr);
}

static PyObject *
Type298_get_eta_tower_pump(Type298Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type298_eta_tower_pump_nget, self->data_ptr);
}

static int
Type298_set_eta_tower_pump(Type298Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type298_eta_tower_pump_nset, self->data_ptr);
}

static PyObject *
Type298_get_ew_dish_separation(Type298Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type298_ew_dish_separation_nget, self->data_ptr);
}

static int
Type298_set_ew_dish_separation(Type298Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type298_ew_dish_separation_nset, self->data_ptr);
}

static PyObject *
Type298_get_fan_control_signal(Type298Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type298_fan_control_signal_nget, self->data_ptr);
}

static int
Type298_set_fan_control_signal(Type298Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type298_fan_control_signal_nset, self->data_ptr);
}

static PyObject *
Type298_get_fan_speed1(Type298Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type298_fan_speed1_nget, self->data_ptr);
}

static int
Type298_set_fan_speed1(Type298Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type298_fan_speed1_nset, self->data_ptr);
}

static PyObject *
Type298_get_fan_speed2(Type298Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type298_fan_speed2_nget, self->data_ptr);
}

static int
Type298_set_fan_speed2(Type298Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type298_fan_speed2_nset, self->data_ptr);
}

static PyObject *
Type298_get_fan_speed3(Type298Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type298_fan_speed3_nget, self->data_ptr);
}

static int
Type298_set_fan_speed3(Type298Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type298_fan_speed3_nset, self->data_ptr);
}

static PyObject *
Type298_get_ns_dish_separation(Type298Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type298_ns_dish_separation_nget, self->data_ptr);
}

static int
Type298_set_ns_dish_separation(Type298Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type298_ns_dish_separation_nset, self->data_ptr);
}

static PyObject *
Type298_get_pump_speed(Type298Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type298_pump_speed_nget, self->data_ptr);
}

static int
Type298_set_pump_speed(Type298Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type298_pump_speed_nset, self->data_ptr);
}

static PyObject *
Type298_get_system_availability(Type298Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type298_system_availability_nget, self->data_ptr);
}

static int
Type298_set_system_availability(Type298Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type298_system_availability_nset, self->data_ptr);
}

static PyObject *
Type298_get_test_P_fan(Type298Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type298_test_P_fan_nget, self->data_ptr);
}

static int
Type298_set_test_P_fan(Type298Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type298_test_P_fan_nset, self->data_ptr);
}

static PyObject *
Type298_get_test_P_pump(Type298Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type298_test_P_pump_nget, self->data_ptr);
}

static int
Type298_set_test_P_pump(Type298Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type298_test_P_pump_nset, self->data_ptr);
}

static PyObject *
Type298_get_test_T_fluid(Type298Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type298_test_T_fluid_nget, self->data_ptr);
}

static int
Type298_set_test_T_fluid(Type298Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type298_test_T_fluid_nset, self->data_ptr);
}

static PyObject *
Type298_get_test_V_dot_fluid(Type298Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type298_test_V_dot_fluid_nget, self->data_ptr);
}

static int
Type298_set_test_V_dot_fluid(Type298Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type298_test_V_dot_fluid_nset, self->data_ptr);
}

static PyObject *
Type298_get_test_cooling_fluid(Type298Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type298_test_cooling_fluid_nget, self->data_ptr);
}

static int
Type298_set_test_cooling_fluid(Type298Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type298_test_cooling_fluid_nset, self->data_ptr);
}

static PyObject *
Type298_get_test_fan_cfm(Type298Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type298_test_fan_cfm_nget, self->data_ptr);
}

static int
Type298_set_test_fan_cfm(Type298Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type298_test_fan_cfm_nset, self->data_ptr);
}

static PyObject *
Type298_get_test_fan_rho_air(Type298Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type298_test_fan_rho_air_nget, self->data_ptr);
}

static int
Type298_set_test_fan_rho_air(Type298Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type298_test_fan_rho_air_nset, self->data_ptr);
}

static PyObject *
Type298_get_test_fan_speed(Type298Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type298_test_fan_speed_nget, self->data_ptr);
}

static int
Type298_set_test_fan_speed(Type298Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type298_test_fan_speed_nset, self->data_ptr);
}

static PyObject *
Type298_get_test_pump_speed(Type298Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type298_test_pump_speed_nget, self->data_ptr);
}

static int
Type298_set_test_pump_speed(Type298Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type298_test_pump_speed_nset, self->data_ptr);
}

static PyObject *
Type298_get_tower_m_dot_water(Type298Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type298_tower_m_dot_water_nget, self->data_ptr);
}

static int
Type298_set_tower_m_dot_water(Type298Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type298_tower_m_dot_water_nset, self->data_ptr);
}

static PyObject *
Type298_get_tower_m_dot_water_test(Type298Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type298_tower_m_dot_water_test_nget, self->data_ptr);
}

static int
Type298_set_tower_m_dot_water_test(Type298Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type298_tower_m_dot_water_test_nset, self->data_ptr);
}

static PyObject *
Type298_get_tower_mode(Type298Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type298_tower_mode_nget, self->data_ptr);
}

static int
Type298_set_tower_mode(Type298Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type298_tower_mode_nset, self->data_ptr);
}

static PyObject *
Type298_get_tower_pipe_material(Type298Object *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Type298_tower_pipe_material_nget, self->data_ptr);
}

static int
Type298_set_tower_pipe_material(Type298Object *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsdish_Type298_tower_pipe_material_nset, self->data_ptr);
}

static PyGetSetDef Type298_getset[] = {
{"P_controls", (getter)Type298_get_P_controls,(setter)Type298_set_P_controls,
	PyDoc_STR("*float*: Control System Parasitic Power, Avg. [W]\n\n*Required*: True"),
 	NULL},
{"P_tower_fan", (getter)Type298_get_P_tower_fan,(setter)Type298_set_P_tower_fan,
	PyDoc_STR("*float*: Tower fan power (set to 0) [kJ/hr]\n\n*Required*: True"),
 	NULL},
{"T_cool_speed2", (getter)Type298_get_T_cool_speed2,(setter)Type298_set_T_cool_speed2,
	PyDoc_STR("*float*: Cooling Fluid Temp. For Fan Speed 2 Cut-In [C]\n\n*Required*: True"),
 	NULL},
{"T_cool_speed3", (getter)Type298_get_T_cool_speed3,(setter)Type298_set_T_cool_speed3,
	PyDoc_STR("*float*: Cooling Fluid Temp. For Fan Speed 3 Cut-In [C]\n\n*Required*: True"),
 	NULL},
{"Tower_water_outlet_temp", (getter)Type298_get_Tower_water_outlet_temp,(setter)Type298_set_Tower_water_outlet_temp,
	PyDoc_STR("*float*: Tower water outlet temperature (set to 20) [C]\n\n*Required*: True"),
 	NULL},
{"b_cooler", (getter)Type298_get_b_cooler,(setter)Type298_set_b_cooler,
	PyDoc_STR("*float*: b_cooler parameter [-]\n\n*Required*: True"),
 	NULL},
{"b_radiator", (getter)Type298_get_b_radiator,(setter)Type298_set_b_radiator,
	PyDoc_STR("*float*: b_radiator parameter [-]\n\n*Required*: True"),
 	NULL},
{"cooling_fluid", (getter)Type298_get_cooling_fluid,(setter)Type298_set_cooling_fluid,
	PyDoc_STR("*float*: Reference Condition Cooling Fluid: 1=Water,2=V50%EG,3=V25%EG,4=V40%PG,5=V25%PG [-]\n\n*Constraints*: INTEGER\n\n*Required*: True"),
 	NULL},
{"cooling_tower_on", (getter)Type298_get_cooling_tower_on,(setter)Type298_set_cooling_tower_on,
	PyDoc_STR("*float*: Option to use a cooling tower (set to 0=off) [-]\n\n*Required*: True"),
 	NULL},
{"d_pipe_tower", (getter)Type298_get_d_pipe_tower,(setter)Type298_set_d_pipe_tower,
	PyDoc_STR("*float*: Runner pipe diameter to the cooling tower (set to 0.4m) [m]\n\n*Required*: True"),
 	NULL},
{"epsilon_cooler_test", (getter)Type298_get_epsilon_cooler_test,(setter)Type298_set_epsilon_cooler_test,
	PyDoc_STR("*float*: Cooler effectiveness [-]\n\n*Required*: True"),
 	NULL},
{"epsilon_power_test", (getter)Type298_get_epsilon_power_test,(setter)Type298_set_epsilon_power_test,
	PyDoc_STR("*float*: Test value for cooling tower effectiveness (set to 0.7) [-]\n\n*Required*: True"),
 	NULL},
{"epsilon_radiator_test", (getter)Type298_get_epsilon_radiator_test,(setter)Type298_set_epsilon_radiator_test,
	PyDoc_STR("*float*: Radiator effectiveness [-]\n\n*Required*: True"),
 	NULL},
{"eta_tower_pump", (getter)Type298_get_eta_tower_pump,(setter)Type298_set_eta_tower_pump,
	PyDoc_STR("*float*: Tower pump efficiency (set to 0.6) [-]\n\n*Required*: True"),
 	NULL},
{"ew_dish_separation", (getter)Type298_get_ew_dish_separation,(setter)Type298_set_ew_dish_separation,
	PyDoc_STR("*float*: East-West dish separation used in the simulation [m]\n\n*Required*: True"),
 	NULL},
{"fan_control_signal", (getter)Type298_get_fan_control_signal,(setter)Type298_set_fan_control_signal,
	PyDoc_STR("*float*: Fan control signal (set to 1, not used in this model) [-]\n\n*Required*: True"),
 	NULL},
{"fan_speed1", (getter)Type298_get_fan_speed1,(setter)Type298_set_fan_speed1,
	PyDoc_STR("*float*: Cooling system fan speed 1 [rpm]\n\n*Required*: True"),
 	NULL},
{"fan_speed2", (getter)Type298_get_fan_speed2,(setter)Type298_set_fan_speed2,
	PyDoc_STR("*float*: Cooling system fan speed 2 [rpm]\n\n*Required*: True"),
 	NULL},
{"fan_speed3", (getter)Type298_get_fan_speed3,(setter)Type298_set_fan_speed3,
	PyDoc_STR("*float*: Cooling system fan speed 3 [rpm]\n\n*Required*: True"),
 	NULL},
{"ns_dish_separation", (getter)Type298_get_ns_dish_separation,(setter)Type298_set_ns_dish_separation,
	PyDoc_STR("*float*: North-South dish separation used in the simulation [m]\n\n*Required*: True"),
 	NULL},
{"pump_speed", (getter)Type298_get_pump_speed,(setter)Type298_set_pump_speed,
	PyDoc_STR("*float*: Reference Condition Pump Speed [rpm]\n\n*Required*: True"),
 	NULL},
{"system_availability", (getter)Type298_get_system_availability,(setter)Type298_set_system_availability,
	PyDoc_STR("*float*: System availability (set to 1.0) [-]\n\n*Required*: True"),
 	NULL},
{"test_P_fan", (getter)Type298_get_test_P_fan,(setter)Type298_set_test_P_fan,
	PyDoc_STR("*float*: Reference Condition Cooling System Fan Power [W]\n\n*Required*: True"),
 	NULL},
{"test_P_pump", (getter)Type298_get_test_P_pump,(setter)Type298_set_test_P_pump,
	PyDoc_STR("*float*: Reference Condition Pump Parasitic Power [W]\n\n*Required*: True"),
 	NULL},
{"test_T_fluid", (getter)Type298_get_test_T_fluid,(setter)Type298_set_test_T_fluid,
	PyDoc_STR("*float*: Reference Condition Cooling Fluid Temperature [K]\n\n*Required*: True"),
 	NULL},
{"test_V_dot_fluid", (getter)Type298_get_test_V_dot_fluid,(setter)Type298_set_test_V_dot_fluid,
	PyDoc_STR("*float*: Reference Condition Cooling Fluid Volumetric Flow Rate [gpm]\n\n*Required*: True"),
 	NULL},
{"test_cooling_fluid", (getter)Type298_get_test_cooling_fluid,(setter)Type298_set_test_cooling_fluid,
	PyDoc_STR("*float*: Reference Condition Cooling Fluid [-]\n\n*Constraints*: INTEGER\n\n*Required*: True"),
 	NULL},
{"test_fan_cfm", (getter)Type298_get_test_fan_cfm,(setter)Type298_set_test_fan_cfm,
	PyDoc_STR("*float*: Reference condition van volumentric flow rate [cfm]\n\n*Required*: True"),
 	NULL},
{"test_fan_rho_air", (getter)Type298_get_test_fan_rho_air,(setter)Type298_set_test_fan_rho_air,
	PyDoc_STR("*float*: Reference condition fan air density [kg/m^3]\n\n*Required*: True"),
 	NULL},
{"test_fan_speed", (getter)Type298_get_test_fan_speed,(setter)Type298_set_test_fan_speed,
	PyDoc_STR("*float*: Reference Condition Cooling System Fan Speed [rpm]\n\n*Required*: True"),
 	NULL},
{"test_pump_speed", (getter)Type298_get_test_pump_speed,(setter)Type298_set_test_pump_speed,
	PyDoc_STR("*float*: Reference Condition Pump Speed [rpm]\n\n*Required*: True"),
 	NULL},
{"tower_m_dot_water", (getter)Type298_get_tower_m_dot_water,(setter)Type298_set_tower_m_dot_water,
	PyDoc_STR("*float*: Tower cooling water flow rate (set to 134,000 kg/hr) [kg/s]\n\n*Required*: True"),
 	NULL},
{"tower_m_dot_water_test", (getter)Type298_get_tower_m_dot_water_test,(setter)Type298_set_tower_m_dot_water_test,
	PyDoc_STR("*float*: Test value for the cooling water flow rate (set to 134,000 kg/hr) [kg/s]\n\n*Required*: True"),
 	NULL},
{"tower_mode", (getter)Type298_get_tower_mode,(setter)Type298_set_tower_mode,
	PyDoc_STR("*float*: Cooling tower type (natural or forced draft) [-]\n\n*Required*: True"),
 	NULL},
{"tower_pipe_material", (getter)Type298_get_tower_pipe_material,(setter)Type298_set_tower_pipe_material,
	PyDoc_STR("*float*: Tower pipe material (1=plastic, 2=new cast iron, 3=riveted steel) [-]\n\n*Required*: True"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject Type298_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Tcsdish.Type298",             /*tp_name*/
		sizeof(Type298Object),          /*tp_basicsize*/
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
		Type298_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		Type298_getset,          /*tp_getset*/
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
	SAM_Tcsdish   data_ptr;
} OutputsObject;

static PyTypeObject Outputs_Type;

static PyObject *
Outputs_new(SAM_Tcsdish data_ptr)
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

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Tcsdish", "Outputs")){
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
Outputs_get_Collector_Losses(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsdish_Outputs_Collector_Losses_aget, self->data_ptr);
}

static PyObject *
Outputs_get_P_SE_losses(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsdish_Outputs_P_SE_losses_aget, self->data_ptr);
}

static PyObject *
Outputs_get_P_out_SE(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsdish_Outputs_P_out_SE_aget, self->data_ptr);
}

static PyObject *
Outputs_get_P_out_rec(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsdish_Outputs_P_out_rec_aget, self->data_ptr);
}

static PyObject *
Outputs_get_P_parasitic(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsdish_Outputs_P_parasitic_aget, self->data_ptr);
}

static PyObject *
Outputs_get_Phi_shade(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsdish_Outputs_Phi_shade_aget, self->data_ptr);
}

static PyObject *
Outputs_get_Power_in_collector(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsdish_Outputs_Power_in_collector_aget, self->data_ptr);
}

static PyObject *
Outputs_get_Power_in_rec(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsdish_Outputs_Power_in_rec_aget, self->data_ptr);
}

static PyObject *
Outputs_get_Power_out_col(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsdish_Outputs_Power_out_col_aget, self->data_ptr);
}

static PyObject *
Outputs_get_Q_rec_losses(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsdish_Outputs_Q_rec_losses_aget, self->data_ptr);
}

static PyObject *
Outputs_get_T_compression(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsdish_Outputs_T_compression_aget, self->data_ptr);
}

static PyObject *
Outputs_get_T_heater_head_operate(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsdish_Outputs_T_heater_head_operate_aget, self->data_ptr);
}

static PyObject *
Outputs_get_T_tower_in(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsdish_Outputs_T_tower_in_aget, self->data_ptr);
}

static PyObject *
Outputs_get_T_tower_out(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsdish_Outputs_T_tower_out_aget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_Collector_Losses(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Outputs_annual_Collector_Losses_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_P_out_SE(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Outputs_annual_P_out_SE_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_P_out_rec(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Outputs_annual_P_out_rec_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_P_parasitic(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Outputs_annual_P_parasitic_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_Power_in_collector(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Outputs_annual_Power_in_collector_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_Power_in_rec(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Outputs_annual_Power_in_rec_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_Power_out_col(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Outputs_annual_Power_out_col_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_Q_rec_losses(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Outputs_annual_Q_rec_losses_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_energy(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Outputs_annual_energy_nget, self->data_ptr);
}

static PyObject *
Outputs_get_beam(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsdish_Outputs_beam_aget, self->data_ptr);
}

static PyObject *
Outputs_get_capacity_factor(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Outputs_capacity_factor_nget, self->data_ptr);
}

static PyObject *
Outputs_get_conversion_factor(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Outputs_conversion_factor_nget, self->data_ptr);
}

static PyObject *
Outputs_get_engine_pressure(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsdish_Outputs_engine_pressure_aget, self->data_ptr);
}

static PyObject *
Outputs_get_eta_SE(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsdish_Outputs_eta_SE_aget, self->data_ptr);
}

static PyObject *
Outputs_get_eta_collector(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsdish_Outputs_eta_collector_aget, self->data_ptr);
}

static PyObject *
Outputs_get_eta_net(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsdish_Outputs_eta_net_aget, self->data_ptr);
}

static PyObject *
Outputs_get_eta_rec(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsdish_Outputs_eta_rec_aget, self->data_ptr);
}

static PyObject *
Outputs_get_gen(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsdish_Outputs_gen_aget, self->data_ptr);
}

static PyObject *
Outputs_get_hour(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsdish_Outputs_hour_aget, self->data_ptr);
}

static PyObject *
Outputs_get_hourly_Collector_Losses(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsdish_Outputs_hourly_Collector_Losses_aget, self->data_ptr);
}

static PyObject *
Outputs_get_hourly_P_out_SE(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsdish_Outputs_hourly_P_out_SE_aget, self->data_ptr);
}

static PyObject *
Outputs_get_hourly_P_out_rec(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsdish_Outputs_hourly_P_out_rec_aget, self->data_ptr);
}

static PyObject *
Outputs_get_hourly_P_parasitic(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsdish_Outputs_hourly_P_parasitic_aget, self->data_ptr);
}

static PyObject *
Outputs_get_hourly_Power_in_collector(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsdish_Outputs_hourly_Power_in_collector_aget, self->data_ptr);
}

static PyObject *
Outputs_get_hourly_Power_in_rec(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsdish_Outputs_hourly_Power_in_rec_aget, self->data_ptr);
}

static PyObject *
Outputs_get_hourly_Power_out_col(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsdish_Outputs_hourly_Power_out_col_aget, self->data_ptr);
}

static PyObject *
Outputs_get_hourly_Q_rec_losses(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsdish_Outputs_hourly_Q_rec_losses_aget, self->data_ptr);
}

static PyObject *
Outputs_get_kwh_per_kw(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsdish_Outputs_kwh_per_kw_nget, self->data_ptr);
}

static PyObject *
Outputs_get_month(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsdish_Outputs_month_aget, self->data_ptr);
}

static PyObject *
Outputs_get_monthly_Collector_Losses(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsdish_Outputs_monthly_Collector_Losses_aget, self->data_ptr);
}

static PyObject *
Outputs_get_monthly_P_out_SE(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsdish_Outputs_monthly_P_out_SE_aget, self->data_ptr);
}

static PyObject *
Outputs_get_monthly_P_out_rec(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsdish_Outputs_monthly_P_out_rec_aget, self->data_ptr);
}

static PyObject *
Outputs_get_monthly_P_parasitic(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsdish_Outputs_monthly_P_parasitic_aget, self->data_ptr);
}

static PyObject *
Outputs_get_monthly_Power_in_collector(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsdish_Outputs_monthly_Power_in_collector_aget, self->data_ptr);
}

static PyObject *
Outputs_get_monthly_Power_in_rec(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsdish_Outputs_monthly_Power_in_rec_aget, self->data_ptr);
}

static PyObject *
Outputs_get_monthly_Power_out_col(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsdish_Outputs_monthly_Power_out_col_aget, self->data_ptr);
}

static PyObject *
Outputs_get_monthly_Q_rec_losses(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsdish_Outputs_monthly_Q_rec_losses_aget, self->data_ptr);
}

static PyObject *
Outputs_get_monthly_energy(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsdish_Outputs_monthly_energy_aget, self->data_ptr);
}

static PyObject *
Outputs_get_net_power(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsdish_Outputs_net_power_aget, self->data_ptr);
}

static PyObject *
Outputs_get_pres(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsdish_Outputs_pres_aget, self->data_ptr);
}

static PyObject *
Outputs_get_solazi(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsdish_Outputs_solazi_aget, self->data_ptr);
}

static PyObject *
Outputs_get_solzen(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsdish_Outputs_solzen_aget, self->data_ptr);
}

static PyObject *
Outputs_get_tdry(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsdish_Outputs_tdry_aget, self->data_ptr);
}

static PyObject *
Outputs_get_twet(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsdish_Outputs_twet_aget, self->data_ptr);
}

static PyObject *
Outputs_get_wspd(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsdish_Outputs_wspd_aget, self->data_ptr);
}

static PyGetSetDef Outputs_getset[] = {
{"Collector_Losses", (getter)Outputs_get_Collector_Losses,(setter)0,
	PyDoc_STR("*sequence*: Collector loss total [kWt]"),
 	NULL},
{"P_SE_losses", (getter)Outputs_get_P_SE_losses,(setter)0,
	PyDoc_STR("*sequence*: Engine power loss [kWt]"),
 	NULL},
{"P_out_SE", (getter)Outputs_get_P_out_SE,(setter)0,
	PyDoc_STR("*sequence*: Engine power output (gross) [kWe]"),
 	NULL},
{"P_out_rec", (getter)Outputs_get_P_out_rec,(setter)0,
	PyDoc_STR("*sequence*: Receiver thermal power output [kWt]"),
 	NULL},
{"P_parasitic", (getter)Outputs_get_P_parasitic,(setter)0,
	PyDoc_STR("*sequence*: Parasitic power [We]"),
 	NULL},
{"Phi_shade", (getter)Outputs_get_Phi_shade,(setter)0,
	PyDoc_STR("*sequence*: Collector shading efficiency"),
 	NULL},
{"Power_in_collector", (getter)Outputs_get_Power_in_collector,(setter)0,
	PyDoc_STR("*sequence*: Collector thermal power incident [kWt]"),
 	NULL},
{"Power_in_rec", (getter)Outputs_get_Power_in_rec,(setter)0,
	PyDoc_STR("*sequence*: Receiver thermal power input [kWt]"),
 	NULL},
{"Power_out_col", (getter)Outputs_get_Power_out_col,(setter)0,
	PyDoc_STR("*sequence*: Collector thermal power produced [kWt]"),
 	NULL},
{"Q_rec_losses", (getter)Outputs_get_Q_rec_losses,(setter)0,
	PyDoc_STR("*sequence*: Receiver thermal power loss [kWt]"),
 	NULL},
{"T_compression", (getter)Outputs_get_T_compression,(setter)0,
	PyDoc_STR("*sequence*: Engine compression temperature [K]"),
 	NULL},
{"T_heater_head_operate", (getter)Outputs_get_T_heater_head_operate,(setter)0,
	PyDoc_STR("*sequence*: Receiver temperature - head operating [K]"),
 	NULL},
{"T_tower_in", (getter)Outputs_get_T_tower_in,(setter)0,
	PyDoc_STR("*sequence*: Cooling fluid temperature - cooler out/tower in [C]"),
 	NULL},
{"T_tower_out", (getter)Outputs_get_T_tower_out,(setter)0,
	PyDoc_STR("*sequence*: Cooling fluid temperature - cooler in/tower out [C]"),
 	NULL},
{"annual_Collector_Losses", (getter)Outputs_get_annual_Collector_Losses,(setter)0,
	PyDoc_STR("*float*: Total collector losses (Incident - P_out) [MWh]"),
 	NULL},
{"annual_P_out_SE", (getter)Outputs_get_annual_P_out_SE,(setter)0,
	PyDoc_STR("*float*: Stirling engine gross output [MWh]"),
 	NULL},
{"annual_P_out_rec", (getter)Outputs_get_annual_P_out_rec,(setter)0,
	PyDoc_STR("*float*: Receiver output power [MWh]"),
 	NULL},
{"annual_P_parasitic", (getter)Outputs_get_annual_P_parasitic,(setter)0,
	PyDoc_STR("*float*: Total parasitic power load [MWh]"),
 	NULL},
{"annual_Power_in_collector", (getter)Outputs_get_annual_Power_in_collector,(setter)0,
	PyDoc_STR("*float*: Power incident on the collector [MWh]"),
 	NULL},
{"annual_Power_in_rec", (getter)Outputs_get_annual_Power_in_rec,(setter)0,
	PyDoc_STR("*float*: Power entering the receiver from the collector [MWh]"),
 	NULL},
{"annual_Power_out_col", (getter)Outputs_get_annual_Power_out_col,(setter)0,
	PyDoc_STR("*float*: Total power from the collector dish [MWh]"),
 	NULL},
{"annual_Q_rec_losses", (getter)Outputs_get_annual_Q_rec_losses,(setter)0,
	PyDoc_STR("*float*: Receiver thermal losses [MWh]"),
 	NULL},
{"annual_energy", (getter)Outputs_get_annual_energy,(setter)0,
	PyDoc_STR("*float*: Annual Energy [kWh]"),
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
{"engine_pressure", (getter)Outputs_get_engine_pressure,(setter)0,
	PyDoc_STR("*sequence*: Engine pressure [Pa]"),
 	NULL},
{"eta_SE", (getter)Outputs_get_eta_SE,(setter)0,
	PyDoc_STR("*sequence*: Engine efficiency"),
 	NULL},
{"eta_collector", (getter)Outputs_get_eta_collector,(setter)0,
	PyDoc_STR("*sequence*: Collector efficiency"),
 	NULL},
{"eta_net", (getter)Outputs_get_eta_net,(setter)0,
	PyDoc_STR("*sequence*: System total: Net efficiency"),
 	NULL},
{"eta_rec", (getter)Outputs_get_eta_rec,(setter)0,
	PyDoc_STR("*sequence*: Receiver efficiency"),
 	NULL},
{"gen", (getter)Outputs_get_gen,(setter)0,
	PyDoc_STR("*sequence*: System power generated [kW]"),
 	NULL},
{"hour", (getter)Outputs_get_hour,(setter)0,
	PyDoc_STR("*sequence*: Resource Hour of Day"),
 	NULL},
{"hourly_Collector_Losses", (getter)Outputs_get_hourly_Collector_Losses,(setter)0,
	PyDoc_STR("*sequence*: System total: Collector loss total [MWt]"),
 	NULL},
{"hourly_P_out_SE", (getter)Outputs_get_hourly_P_out_SE,(setter)0,
	PyDoc_STR("*sequence*: System total: Engine power output (gross) [MWe]"),
 	NULL},
{"hourly_P_out_rec", (getter)Outputs_get_hourly_P_out_rec,(setter)0,
	PyDoc_STR("*sequence*: System total: Receiver thermal power output [MWt]"),
 	NULL},
{"hourly_P_parasitic", (getter)Outputs_get_hourly_P_parasitic,(setter)0,
	PyDoc_STR("*sequence*: System total: Parasitic power [MWe]"),
 	NULL},
{"hourly_Power_in_collector", (getter)Outputs_get_hourly_Power_in_collector,(setter)0,
	PyDoc_STR("*sequence*: System total: Collector thermal power incident [MWt]"),
 	NULL},
{"hourly_Power_in_rec", (getter)Outputs_get_hourly_Power_in_rec,(setter)0,
	PyDoc_STR("*sequence*: System total: Receiver thermal power input [MWt]"),
 	NULL},
{"hourly_Power_out_col", (getter)Outputs_get_hourly_Power_out_col,(setter)0,
	PyDoc_STR("*sequence*: System total: Collector thermal power produced [MWt]"),
 	NULL},
{"hourly_Q_rec_losses", (getter)Outputs_get_hourly_Q_rec_losses,(setter)0,
	PyDoc_STR("*sequence*: System total: Receiver thermal loss [MWt]"),
 	NULL},
{"kwh_per_kw", (getter)Outputs_get_kwh_per_kw,(setter)0,
	PyDoc_STR("*float*: First year kWh/kW [kWh/kW]"),
 	NULL},
{"month", (getter)Outputs_get_month,(setter)0,
	PyDoc_STR("*sequence*: Resource Month"),
 	NULL},
{"monthly_Collector_Losses", (getter)Outputs_get_monthly_Collector_Losses,(setter)0,
	PyDoc_STR("*sequence*: Total collector losses (Incident - P_out) [MWh]"),
 	NULL},
{"monthly_P_out_SE", (getter)Outputs_get_monthly_P_out_SE,(setter)0,
	PyDoc_STR("*sequence*: Stirling engine gross output [MWh]"),
 	NULL},
{"monthly_P_out_rec", (getter)Outputs_get_monthly_P_out_rec,(setter)0,
	PyDoc_STR("*sequence*: Receiver output power [MWh]"),
 	NULL},
{"monthly_P_parasitic", (getter)Outputs_get_monthly_P_parasitic,(setter)0,
	PyDoc_STR("*sequence*: Total parasitic power load [MWh]"),
 	NULL},
{"monthly_Power_in_collector", (getter)Outputs_get_monthly_Power_in_collector,(setter)0,
	PyDoc_STR("*sequence*: Power incident on the collector [MWh]"),
 	NULL},
{"monthly_Power_in_rec", (getter)Outputs_get_monthly_Power_in_rec,(setter)0,
	PyDoc_STR("*sequence*: Power entering the receiver from the collector [MWh]"),
 	NULL},
{"monthly_Power_out_col", (getter)Outputs_get_monthly_Power_out_col,(setter)0,
	PyDoc_STR("*sequence*: Total power from the collector dish [MWh]"),
 	NULL},
{"monthly_Q_rec_losses", (getter)Outputs_get_monthly_Q_rec_losses,(setter)0,
	PyDoc_STR("*sequence*: Receiver thermal losses [MWh]"),
 	NULL},
{"monthly_energy", (getter)Outputs_get_monthly_energy,(setter)0,
	PyDoc_STR("*sequence*: Monthly Energy [kWh]"),
 	NULL},
{"net_power", (getter)Outputs_get_net_power,(setter)0,
	PyDoc_STR("*sequence*: Engine power output (net) [kWe]"),
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
{"tdry", (getter)Outputs_get_tdry,(setter)0,
	PyDoc_STR("*sequence*: Resource Dry bulb temperature [C]"),
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
		"Tcsdish.Outputs",             /*tp_name*/
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
 * Tcsdish
 */

typedef struct {
	PyObject_HEAD
	PyObject            *x_attr;        /* Attributes dictionary */
	SAM_Tcsdish   data_ptr;
} TcsdishObject;

static PyTypeObject Tcsdish_Type;

#define TcsdishObject_Check(v)      (Py_TYPE(v) == &Tcsdish_Type)

static TcsdishObject *
newTcsdishObject(void* data_ptr)
{
	TcsdishObject *self;
	self = PyObject_New(TcsdishObject, &Tcsdish_Type);

	PySAM_TECH_ATTR("Tcsdish", SAM_Tcsdish_construct)

	PyObject* Weather_obj = Weather_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "Weather", Weather_obj);
	Py_DECREF(Weather_obj);

	PyObject* Dish_obj = Dish_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "Dish", Dish_obj);
	Py_DECREF(Dish_obj);

	PyObject* Type295_obj = Type295_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "Type295", Type295_obj);
	Py_DECREF(Type295_obj);

	PyObject* Type296_obj = Type296_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "Type296", Type296_obj);
	Py_DECREF(Type296_obj);

	PyObject* Type297_obj = Type297_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "Type297", Type297_obj);
	Py_DECREF(Type297_obj);

	PyObject* Type298_obj = Type298_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "Type298", Type298_obj);
	Py_DECREF(Type298_obj);

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

/* Tcsdish methods */

static void
Tcsdish_dealloc(TcsdishObject *self)
{
	Py_XDECREF(self->x_attr);
	SAM_Tcsdish_destruct(self->data_ptr);
	PyObject_Del(self);
}


static PyObject *
Tcsdish_execute(TcsdishObject *self, PyObject *args)
{
	int verbosity = 0;

	if (!PyArg_ParseTuple(args, "|i", &verbosity))
		return NULL;

	SAM_error error = new_error();
	SAM_Tcsdish_execute(self->data_ptr, verbosity, &error);
	if (PySAM_has_error(error )) return NULL;

	Py_INCREF(Py_None);
	return Py_None;
}


static PyObject *
Tcsdish_assign(TcsdishObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_nested_dict((PyObject*)self, self->x_attr, self->data_ptr, dict, "Tcsdish"))
		return NULL;

	Py_INCREF(Py_None);
	return Py_None;
}


static PyObject *
Tcsdish_export(TcsdishObject *self, PyObject *args)
{
	return PySAM_export_to_nested_dict((PyObject *) self, self->x_attr);
}

static PyMethodDef Tcsdish_methods[] = {
		{"execute",            (PyCFunction)Tcsdish_execute,  METH_VARARGS,
				PyDoc_STR("execute(int verbosity) -> None\n Execute simulation with verbosity level 0 (default) or 1")},
		{"assign",            (PyCFunction)Tcsdish_assign,  METH_VARARGS,
				PyDoc_STR("assign(dict) -> None\n Assign attributes from nested dictionary, except for Outputs\n\n``nested_dict = { 'Weather': { var: val, ...}, ...}``")},
		{"export",            (PyCFunction)Tcsdish_export,  METH_VARARGS,
				PyDoc_STR("export() -> dict\n Export attributes into nested dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
Tcsdish_getattro(TcsdishObject *self, PyObject *name)
{
	return PySAM_get_attr((PyObject*) self, (PyObject*) self->x_attr, name);
}

static int
Tcsdish_setattr(TcsdishObject *self, const char *name, PyObject *v)
{
	return PySAM_set_attr((PyObject*)self, (PyObject*)self->x_attr, name, v);
}

static PyTypeObject Tcsdish_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Tcsdish",            /*tp_name*/
		sizeof(TcsdishObject),/*tp_basicsize*/
		0,                          /*tp_itemsize*/
		/* methods */
		(destructor)Tcsdish_dealloc,    /*tp_dealloc*/
		0,                          /*tp_print*/
		(getattrfunc)0,             /*tp_getattr*/
		(setattrfunc)Tcsdish_setattr,   /*tp_setattr*/
		0,                          /*tp_reserved*/
		0,                          /*tp_repr*/
		0,                          /*tp_as_number*/
		0,                          /*tp_as_sequence*/
		0,                          /*tp_as_mapping*/
		0,                          /*tp_hash*/
		0,                          /*tp_call*/
		0,                          /*tp_str*/
		(getattrofunc)Tcsdish_getattro, /*tp_getattro*/
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
		Tcsdish_methods,      /*tp_methods*/
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


/* Function of no arguments returning new Tcsdish object */

static PyObject *
Tcsdish_new(PyObject *self, PyObject *args)
{
	TcsdishObject *rv;
	rv = newTcsdishObject(0);
	if (rv == NULL)
		return NULL;
	return (PyObject *)rv;
}

static PyObject *
Tcsdish_wrap(PyObject *self, PyObject *args)
{
	TcsdishObject *rv;
	long long int ptr = 0;  // 64 bit arch
	if (!PyArg_ParseTuple(args, "L:wrap", &ptr)){
		PyErr_BadArgument();
		return NULL;
	}
	rv = newTcsdishObject((void*)ptr);
	if (rv == NULL)
		return NULL;
	return (PyObject *)rv;
}

static PyObject *
Tcsdish_default(PyObject *self, PyObject *args)
{
	TcsdishObject *rv;
	char* def = 0;
	if (!PyArg_ParseTuple(args, "s:default", &def)){
		PyErr_BadArgument();
		return NULL;
	}
	rv = newTcsdishObject(0);
	if (rv == NULL)
		return NULL;

	PySAM_load_defaults((PyObject*)rv, rv->x_attr, rv->data_ptr, "Tcsdish", def);

	return (PyObject *)rv;
}

/* ---------- */


/* List of functions defined in the module */

static PyMethodDef TcsdishModule_methods[] = {
		{"new",             Tcsdish_new,         METH_VARARGS,
				PyDoc_STR("new() -> Tcsdish")},
		{"default",             Tcsdish_default,         METH_VARARGS,
				PyDoc_STR("default(config) -> Tcsdish\n\nUse financial model-specific default attributes\n"
				"config options:\n\n- \"DishStirlingAllEquityPartnershipFlip\"\n- \"DishStirlingCommercial\"\n- \"DishStirlingCommercialPPA\"\n- \"DishStirlingIndependentPowerProducer\"\n- \"DishStirlingLCOECalculator\"\n- \"DishStirlingLeveragedPartnershipFlip\"\n- \"DishStirlingNone\"\n- \"DishStirlingSaleLeaseback\"\n- \"DishStirlingSingleOwner\"")},
		{"wrap",             Tcsdish_wrap,         METH_VARARGS,
				PyDoc_STR("wrap(ssc_data_t) -> Tcsdish\n\nUse existing PySSC data\n\n.. warning::\n\n	Do not call PySSC.data_free on the ssc_data_t provided to ``wrap``")},
		{NULL,              NULL}           /* sentinel */
};

PyDoc_STRVAR(module_doc,
			 "CSP dish-Stirling model with parameters for SES and WGA-ADDS systems for power generation");


static int
TcsdishModule_exec(PyObject *m)
{
	/* Finalize the type object including setting type of the new type
	 * object; doing it here is required for portability, too. */

	if (PySAM_load_lib(m) < 0) goto fail;
	if (PySAM_init_error(m) < 0) goto fail;

	Tcsdish_Type.tp_dict = PyDict_New();
	if (!Tcsdish_Type.tp_dict) { goto fail; }

	/// Add the AdjustmentFactors type object to Tcsdish_Type
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
	PyDict_SetItemString(Tcsdish_Type.tp_dict,
						 "AdjustmentFactors",
						 (PyObject*)AdjustmentFactors_Type);
	Py_DECREF(&AdjustmentFactors_Type);
	Py_XDECREF(AdjustmentFactors_Type);

	/// Add the Weather type object to Tcsdish_Type
	if (PyType_Ready(&Weather_Type) < 0) { goto fail; }
	PyDict_SetItemString(Tcsdish_Type.tp_dict,
				"Weather",
				(PyObject*)&Weather_Type);
	Py_DECREF(&Weather_Type);

	/// Add the Dish type object to Tcsdish_Type
	if (PyType_Ready(&Dish_Type) < 0) { goto fail; }
	PyDict_SetItemString(Tcsdish_Type.tp_dict,
				"Dish",
				(PyObject*)&Dish_Type);
	Py_DECREF(&Dish_Type);

	/// Add the Type295 type object to Tcsdish_Type
	if (PyType_Ready(&Type295_Type) < 0) { goto fail; }
	PyDict_SetItemString(Tcsdish_Type.tp_dict,
				"Type295",
				(PyObject*)&Type295_Type);
	Py_DECREF(&Type295_Type);

	/// Add the Type296 type object to Tcsdish_Type
	if (PyType_Ready(&Type296_Type) < 0) { goto fail; }
	PyDict_SetItemString(Tcsdish_Type.tp_dict,
				"Type296",
				(PyObject*)&Type296_Type);
	Py_DECREF(&Type296_Type);

	/// Add the Type297 type object to Tcsdish_Type
	if (PyType_Ready(&Type297_Type) < 0) { goto fail; }
	PyDict_SetItemString(Tcsdish_Type.tp_dict,
				"Type297",
				(PyObject*)&Type297_Type);
	Py_DECREF(&Type297_Type);

	/// Add the Type298 type object to Tcsdish_Type
	if (PyType_Ready(&Type298_Type) < 0) { goto fail; }
	PyDict_SetItemString(Tcsdish_Type.tp_dict,
				"Type298",
				(PyObject*)&Type298_Type);
	Py_DECREF(&Type298_Type);

	/// Add the Outputs type object to Tcsdish_Type
	if (PyType_Ready(&Outputs_Type) < 0) { goto fail; }
	PyDict_SetItemString(Tcsdish_Type.tp_dict,
				"Outputs",
				(PyObject*)&Outputs_Type);
	Py_DECREF(&Outputs_Type);

	/// Add the Tcsdish type object to the module
	if (PyType_Ready(&Tcsdish_Type) < 0) { goto fail; }
	PyModule_AddObject(m,
				"Tcsdish",
				(PyObject*)&Tcsdish_Type);

	return 0;
	fail:
	Py_XDECREF(m);
	return -1;
}

static struct PyModuleDef_Slot TcsdishModule_slots[] = {
		{Py_mod_exec, TcsdishModule_exec},
		{0, NULL},
};

static struct PyModuleDef TcsdishModule = {
		PyModuleDef_HEAD_INIT,
		"Tcsdish",
		module_doc,
		0,
		TcsdishModule_methods,
		TcsdishModule_slots,
		NULL,
		NULL,
		NULL
};

/* Export function for the module */

PyMODINIT_FUNC
PyInit_Tcsdish(void)
{
	return PyModuleDef_Init(&TcsdishModule);
}