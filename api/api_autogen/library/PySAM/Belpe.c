#include <Python.h>

#include <SAM_Belpe.h>
#include <SAM_api.h>

#include "PySAM_utils.h"



	/*
	 * LoadProfileEstimator Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Belpe   data_ptr;
} LoadProfileEstimatorObject;

static PyTypeObject LoadProfileEstimator_Type;

static PyObject *
LoadProfileEstimator_new(SAM_Belpe data_ptr)
{
	PyObject* new_obj = LoadProfileEstimator_Type.tp_alloc(&LoadProfileEstimator_Type,0);

	LoadProfileEstimatorObject* LoadProfileEstimator_obj = (LoadProfileEstimatorObject*)new_obj;

	LoadProfileEstimator_obj->data_ptr = data_ptr;

	return new_obj;
}

/* LoadProfileEstimator methods */

static PyObject *
LoadProfileEstimator_assign(LoadProfileEstimatorObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Belpe", "LoadProfileEstimator")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
LoadProfileEstimator_export(LoadProfileEstimatorObject *self, PyObject *args)
{
	PyTypeObject* tp = &LoadProfileEstimator_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef LoadProfileEstimator_methods[] = {
		{"assign",            (PyCFunction)LoadProfileEstimator_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``LoadProfileEstimator_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)LoadProfileEstimator_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
LoadProfileEstimator_get_Monthly_util(LoadProfileEstimatorObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Belpe_LoadProfileEstimator_Monthly_util_aget, self->data_ptr);
}

static int
LoadProfileEstimator_set_Monthly_util(LoadProfileEstimatorObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Belpe_LoadProfileEstimator_Monthly_util_aset, self->data_ptr);
}

static PyObject *
LoadProfileEstimator_get_Occ_Schedule(LoadProfileEstimatorObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Belpe_LoadProfileEstimator_Occ_Schedule_aget, self->data_ptr);
}

static int
LoadProfileEstimator_set_Occ_Schedule(LoadProfileEstimatorObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Belpe_LoadProfileEstimator_Occ_Schedule_aset, self->data_ptr);
}

static PyObject *
LoadProfileEstimator_get_Occupants(LoadProfileEstimatorObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Belpe_LoadProfileEstimator_Occupants_nget, self->data_ptr);
}

static int
LoadProfileEstimator_set_Occupants(LoadProfileEstimatorObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Belpe_LoadProfileEstimator_Occupants_nset, self->data_ptr);
}

static PyObject *
LoadProfileEstimator_get_Retrofits(LoadProfileEstimatorObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Belpe_LoadProfileEstimator_Retrofits_nget, self->data_ptr);
}

static int
LoadProfileEstimator_set_Retrofits(LoadProfileEstimatorObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Belpe_LoadProfileEstimator_Retrofits_nset, self->data_ptr);
}

static PyObject *
LoadProfileEstimator_get_Stories(LoadProfileEstimatorObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Belpe_LoadProfileEstimator_Stories_nget, self->data_ptr);
}

static int
LoadProfileEstimator_set_Stories(LoadProfileEstimatorObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Belpe_LoadProfileEstimator_Stories_nset, self->data_ptr);
}

static PyObject *
LoadProfileEstimator_get_TCool(LoadProfileEstimatorObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Belpe_LoadProfileEstimator_TCool_nget, self->data_ptr);
}

static int
LoadProfileEstimator_set_TCool(LoadProfileEstimatorObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Belpe_LoadProfileEstimator_TCool_nset, self->data_ptr);
}

static PyObject *
LoadProfileEstimator_get_TCoolSB(LoadProfileEstimatorObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Belpe_LoadProfileEstimator_TCoolSB_nget, self->data_ptr);
}

static int
LoadProfileEstimator_set_TCoolSB(LoadProfileEstimatorObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Belpe_LoadProfileEstimator_TCoolSB_nset, self->data_ptr);
}

static PyObject *
LoadProfileEstimator_get_THeat(LoadProfileEstimatorObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Belpe_LoadProfileEstimator_THeat_nget, self->data_ptr);
}

static int
LoadProfileEstimator_set_THeat(LoadProfileEstimatorObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Belpe_LoadProfileEstimator_THeat_nset, self->data_ptr);
}

static PyObject *
LoadProfileEstimator_get_THeatSB(LoadProfileEstimatorObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Belpe_LoadProfileEstimator_THeatSB_nget, self->data_ptr);
}

static int
LoadProfileEstimator_set_THeatSB(LoadProfileEstimatorObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Belpe_LoadProfileEstimator_THeatSB_nset, self->data_ptr);
}

static PyObject *
LoadProfileEstimator_get_T_Sched(LoadProfileEstimatorObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Belpe_LoadProfileEstimator_T_Sched_aget, self->data_ptr);
}

static int
LoadProfileEstimator_set_T_Sched(LoadProfileEstimatorObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Belpe_LoadProfileEstimator_T_Sched_aset, self->data_ptr);
}

static PyObject *
LoadProfileEstimator_get_YrBuilt(LoadProfileEstimatorObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Belpe_LoadProfileEstimator_YrBuilt_nget, self->data_ptr);
}

static int
LoadProfileEstimator_set_YrBuilt(LoadProfileEstimatorObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Belpe_LoadProfileEstimator_YrBuilt_nset, self->data_ptr);
}

static PyObject *
LoadProfileEstimator_get_en_belpe(LoadProfileEstimatorObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Belpe_LoadProfileEstimator_en_belpe_nget, self->data_ptr);
}

static int
LoadProfileEstimator_set_en_belpe(LoadProfileEstimatorObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Belpe_LoadProfileEstimator_en_belpe_nset, self->data_ptr);
}

static PyObject *
LoadProfileEstimator_get_en_cool(LoadProfileEstimatorObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Belpe_LoadProfileEstimator_en_cool_nget, self->data_ptr);
}

static int
LoadProfileEstimator_set_en_cool(LoadProfileEstimatorObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Belpe_LoadProfileEstimator_en_cool_nset, self->data_ptr);
}

static PyObject *
LoadProfileEstimator_get_en_dish(LoadProfileEstimatorObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Belpe_LoadProfileEstimator_en_dish_nget, self->data_ptr);
}

static int
LoadProfileEstimator_set_en_dish(LoadProfileEstimatorObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Belpe_LoadProfileEstimator_en_dish_nset, self->data_ptr);
}

static PyObject *
LoadProfileEstimator_get_en_dry(LoadProfileEstimatorObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Belpe_LoadProfileEstimator_en_dry_nget, self->data_ptr);
}

static int
LoadProfileEstimator_set_en_dry(LoadProfileEstimatorObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Belpe_LoadProfileEstimator_en_dry_nset, self->data_ptr);
}

static PyObject *
LoadProfileEstimator_get_en_fridge(LoadProfileEstimatorObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Belpe_LoadProfileEstimator_en_fridge_nget, self->data_ptr);
}

static int
LoadProfileEstimator_set_en_fridge(LoadProfileEstimatorObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Belpe_LoadProfileEstimator_en_fridge_nset, self->data_ptr);
}

static PyObject *
LoadProfileEstimator_get_en_heat(LoadProfileEstimatorObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Belpe_LoadProfileEstimator_en_heat_nget, self->data_ptr);
}

static int
LoadProfileEstimator_set_en_heat(LoadProfileEstimatorObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Belpe_LoadProfileEstimator_en_heat_nset, self->data_ptr);
}

static PyObject *
LoadProfileEstimator_get_en_mels(LoadProfileEstimatorObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Belpe_LoadProfileEstimator_en_mels_nget, self->data_ptr);
}

static int
LoadProfileEstimator_set_en_mels(LoadProfileEstimatorObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Belpe_LoadProfileEstimator_en_mels_nset, self->data_ptr);
}

static PyObject *
LoadProfileEstimator_get_en_range(LoadProfileEstimatorObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Belpe_LoadProfileEstimator_en_range_nget, self->data_ptr);
}

static int
LoadProfileEstimator_set_en_range(LoadProfileEstimatorObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Belpe_LoadProfileEstimator_en_range_nset, self->data_ptr);
}

static PyObject *
LoadProfileEstimator_get_en_wash(LoadProfileEstimatorObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Belpe_LoadProfileEstimator_en_wash_nget, self->data_ptr);
}

static int
LoadProfileEstimator_set_en_wash(LoadProfileEstimatorObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Belpe_LoadProfileEstimator_en_wash_nset, self->data_ptr);
}

static PyObject *
LoadProfileEstimator_get_floor_area(LoadProfileEstimatorObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Belpe_LoadProfileEstimator_floor_area_nget, self->data_ptr);
}

static int
LoadProfileEstimator_set_floor_area(LoadProfileEstimatorObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Belpe_LoadProfileEstimator_floor_area_nset, self->data_ptr);
}

static PyObject *
LoadProfileEstimator_get_load(LoadProfileEstimatorObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Belpe_LoadProfileEstimator_load_aget, self->data_ptr);
}

static int
LoadProfileEstimator_set_load(LoadProfileEstimatorObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Belpe_LoadProfileEstimator_load_aset, self->data_ptr);
}

static PyObject *
LoadProfileEstimator_get_solar_resource_file(LoadProfileEstimatorObject *self, void *closure)
{
	return PySAM_string_getter(SAM_Belpe_LoadProfileEstimator_solar_resource_file_sget, self->data_ptr);
}

static int
LoadProfileEstimator_set_solar_resource_file(LoadProfileEstimatorObject *self, PyObject *value, void *closure)
{
	return PySAM_string_setter(value, SAM_Belpe_LoadProfileEstimator_solar_resource_file_sset, self->data_ptr);
}

static PyGetSetDef LoadProfileEstimator_getset[] = {
{"Monthly_util", (getter)LoadProfileEstimator_get_Monthly_util,(setter)LoadProfileEstimator_set_Monthly_util,
	PyDoc_STR("*sequence*: Monthly consumption from utility bill [kWh]\n\n*Constraints*: LENGTH=12\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"Occ_Schedule", (getter)LoadProfileEstimator_get_Occ_Schedule,(setter)LoadProfileEstimator_set_Occ_Schedule,
	PyDoc_STR("*sequence*: Hourly occupant schedule [frac/hr]\n\n*Constraints*: LENGTH=24\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"Occupants", (getter)LoadProfileEstimator_get_Occupants,(setter)LoadProfileEstimator_set_Occupants,
	PyDoc_STR("*float*: Occupants [#]\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"Retrofits", (getter)LoadProfileEstimator_get_Retrofits,(setter)LoadProfileEstimator_set_Retrofits,
	PyDoc_STR("*float*: Energy retrofitted [0/1]\n\n*Options*: 0=No, 1=Yes\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"Stories", (getter)LoadProfileEstimator_get_Stories,(setter)LoadProfileEstimator_set_Stories,
	PyDoc_STR("*float*: Number of stories [#]\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"TCool", (getter)LoadProfileEstimator_get_TCool,(setter)LoadProfileEstimator_set_TCool,
	PyDoc_STR("*float*: Cooling setpoint [degF]\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"TCoolSB", (getter)LoadProfileEstimator_get_TCoolSB,(setter)LoadProfileEstimator_set_TCoolSB,
	PyDoc_STR("*float*: Cooling setpoint setback [degF]\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"THeat", (getter)LoadProfileEstimator_get_THeat,(setter)LoadProfileEstimator_set_THeat,
	PyDoc_STR("*float*: Heating setpoint [degF]\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"THeatSB", (getter)LoadProfileEstimator_get_THeatSB,(setter)LoadProfileEstimator_set_THeatSB,
	PyDoc_STR("*float*: Heating setpoint setback [degf]\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"T_Sched", (getter)LoadProfileEstimator_get_T_Sched,(setter)LoadProfileEstimator_set_T_Sched,
	PyDoc_STR("*sequence*: Temperature schedule [0/1]\n\n*Constraints*: LENGTH=24\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"YrBuilt", (getter)LoadProfileEstimator_get_YrBuilt,(setter)LoadProfileEstimator_set_YrBuilt,
	PyDoc_STR("*float*: Year built [yr]\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"en_belpe", (getter)LoadProfileEstimator_get_en_belpe,(setter)LoadProfileEstimator_set_en_belpe,
	PyDoc_STR("*float*: Enable building load calculator [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: True"),
 	NULL},
{"en_cool", (getter)LoadProfileEstimator_get_en_cool,(setter)LoadProfileEstimator_set_en_cool,
	PyDoc_STR("*float*: Enable electric cool [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"en_dish", (getter)LoadProfileEstimator_get_en_dish,(setter)LoadProfileEstimator_set_en_dish,
	PyDoc_STR("*float*: Enable electric dishwasher [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"en_dry", (getter)LoadProfileEstimator_get_en_dry,(setter)LoadProfileEstimator_set_en_dry,
	PyDoc_STR("*float*: Enable electric dryer [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"en_fridge", (getter)LoadProfileEstimator_get_en_fridge,(setter)LoadProfileEstimator_set_en_fridge,
	PyDoc_STR("*float*: Enable electric fridge [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"en_heat", (getter)LoadProfileEstimator_get_en_heat,(setter)LoadProfileEstimator_set_en_heat,
	PyDoc_STR("*float*: Enable electric heat [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"en_mels", (getter)LoadProfileEstimator_get_en_mels,(setter)LoadProfileEstimator_set_en_mels,
	PyDoc_STR("*float*: Enable misc electric loads [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"en_range", (getter)LoadProfileEstimator_get_en_range,(setter)LoadProfileEstimator_set_en_range,
	PyDoc_STR("*float*: Enable electric range [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"en_wash", (getter)LoadProfileEstimator_get_en_wash,(setter)LoadProfileEstimator_set_en_wash,
	PyDoc_STR("*float*: Enable electric washer [0/1]\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"floor_area", (getter)LoadProfileEstimator_get_floor_area,(setter)LoadProfileEstimator_set_floor_area,
	PyDoc_STR("*float*: Building floor area [m2]\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"load", (getter)LoadProfileEstimator_get_load,(setter)LoadProfileEstimator_set_load,
	PyDoc_STR("*sequence*: Electricity load (year 1) [kW]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"solar_resource_file", (getter)LoadProfileEstimator_get_solar_resource_file,(setter)LoadProfileEstimator_set_solar_resource_file,
	PyDoc_STR("*str*: Weather Data file [n/a]\n\n*Constraints*: LOCAL_FILE\n\n*Required*: set to 1 if not provided."),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject LoadProfileEstimator_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Belpe.LoadProfileEstimator",             /*tp_name*/
		sizeof(LoadProfileEstimatorObject),          /*tp_basicsize*/
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
		LoadProfileEstimator_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		LoadProfileEstimator_getset,          /*tp_getset*/
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
	SAM_Belpe   data_ptr;
} OutputsObject;

static PyTypeObject Outputs_Type;

static PyObject *
Outputs_new(SAM_Belpe data_ptr)
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

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Belpe", "Outputs")){
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

static PyGetSetDef Outputs_getset[] = {
	{NULL}  /* Sentinel */
};

static PyTypeObject Outputs_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Belpe.Outputs",             /*tp_name*/
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
 * Belpe
 */

typedef struct {
	PyObject_HEAD
	PyObject            *x_attr;        /* Attributes dictionary */
	SAM_Belpe   data_ptr;
} BelpeObject;

static PyTypeObject Belpe_Type;

#define BelpeObject_Check(v)      (Py_TYPE(v) == &Belpe_Type)

static BelpeObject *
newBelpeObject(void* data_ptr)
{
	BelpeObject *self;
	self = PyObject_New(BelpeObject, &Belpe_Type);

	PySAM_TECH_ATTR("Belpe", SAM_Belpe_construct)

	PyObject* LoadProfileEstimator_obj = LoadProfileEstimator_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "LoadProfileEstimator", LoadProfileEstimator_obj);
	Py_DECREF(LoadProfileEstimator_obj);

	PyObject* Outputs_obj = Outputs_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "Outputs", Outputs_obj);
	Py_DECREF(Outputs_obj);


	return self;
}

/* Belpe methods */

static void
Belpe_dealloc(BelpeObject *self)
{
	Py_XDECREF(self->x_attr);
	SAM_Belpe_destruct(self->data_ptr);
	PyObject_Del(self);
}


static PyObject *
Belpe_execute(BelpeObject *self, PyObject *args)
{
	int verbosity = 0;

	if (!PyArg_ParseTuple(args, "|i", &verbosity))
		return NULL;

	SAM_error error = new_error();
	SAM_Belpe_execute(self->data_ptr, verbosity, &error);
	if (PySAM_has_error(error )) return NULL;

	Py_INCREF(Py_None);
	return Py_None;
}


static PyObject *
Belpe_assign(BelpeObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_nested_dict((PyObject*)self, self->x_attr, self->data_ptr, dict, "Belpe"))
		return NULL;

	Py_INCREF(Py_None);
	return Py_None;
}


static PyObject *
Belpe_export(BelpeObject *self, PyObject *args)
{
	return PySAM_export_to_nested_dict((PyObject *) self, self->x_attr);
}

static PyMethodDef Belpe_methods[] = {
		{"execute",            (PyCFunction)Belpe_execute,  METH_VARARGS,
				PyDoc_STR("execute(int verbosity) -> None\n Execute simulation with verbosity level 0 (default) or 1")},
		{"assign",            (PyCFunction)Belpe_assign,  METH_VARARGS,
				PyDoc_STR("assign(dict) -> None\n Assign attributes from nested dictionary, except for Outputs\n\n``nested_dict = { 'Load Profile Estimator': { var: val, ...}, ...}``")},
		{"export",            (PyCFunction)Belpe_export,  METH_VARARGS,
				PyDoc_STR("export() -> dict\n Export attributes into nested dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
Belpe_getattro(BelpeObject *self, PyObject *name)
{
	return PySAM_get_attr((PyObject*) self, (PyObject*) self->x_attr, name);
}

static int
Belpe_setattr(BelpeObject *self, const char *name, PyObject *v)
{
	return PySAM_set_attr((PyObject*)self, (PyObject*)self->x_attr, name, v);
}

static PyTypeObject Belpe_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Belpe",            /*tp_name*/
		sizeof(BelpeObject),/*tp_basicsize*/
		0,                          /*tp_itemsize*/
		/* methods */
		(destructor)Belpe_dealloc,    /*tp_dealloc*/
		0,                          /*tp_print*/
		(getattrfunc)0,             /*tp_getattr*/
		(setattrfunc)Belpe_setattr,   /*tp_setattr*/
		0,                          /*tp_reserved*/
		0,                          /*tp_repr*/
		0,                          /*tp_as_number*/
		0,                          /*tp_as_sequence*/
		0,                          /*tp_as_mapping*/
		0,                          /*tp_hash*/
		0,                          /*tp_call*/
		0,                          /*tp_str*/
		(getattrofunc)Belpe_getattro, /*tp_getattro*/
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
		Belpe_methods,      /*tp_methods*/
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


/* Function of no arguments returning new Belpe object */

static PyObject *
Belpe_new(PyObject *self, PyObject *args)
{
	BelpeObject *rv;
	rv = newBelpeObject(0);
	if (rv == NULL)
		return NULL;
	return (PyObject *)rv;
}

static PyObject *
Belpe_wrap(PyObject *self, PyObject *args)
{
	BelpeObject *rv;
	long long int ptr = 0;  // 64 bit arch
	if (!PyArg_ParseTuple(args, "L:wrap", &ptr)){
		PyErr_BadArgument();
		return NULL;
	}
	rv = newBelpeObject((void*)ptr);
	if (rv == NULL)
		return NULL;
	return (PyObject *)rv;
}

static PyObject *
Belpe_default(PyObject *self, PyObject *args)
{
	BelpeObject *rv;
	char* def = 0;
	if (!PyArg_ParseTuple(args, "s:default", &def)){
		PyErr_BadArgument();
		return NULL;
	}
	rv = newBelpeObject(0);
	if (rv == NULL)
		return NULL;

	PySAM_load_defaults((PyObject*)rv, rv->x_attr, rv->data_ptr, "Belpe", def);

	return (PyObject *)rv;
}

/* ---------- */


/* List of functions defined in the module */

static PyMethodDef BelpeModule_methods[] = {
		{"new",             Belpe_new,         METH_VARARGS,
				PyDoc_STR("new() -> Belpe")},
		{"default",             Belpe_default,         METH_VARARGS,
				PyDoc_STR("default(config) -> Belpe\n\nUse financial model-specific default attributes\n"
				"config options:\n\n- \"FlatPlatePVResidential\"\n- \"FlatPlatePVThirdParty\"\n- \"PVWattsResidential\"\n- \"PVWattsThirdParty\"\n- \"SolarWaterHeatingResidential\"")},
		{"wrap",             Belpe_wrap,         METH_VARARGS,
				PyDoc_STR("wrap(ssc_data_t) -> Belpe\n\nUse existing PySSC data\n\n.. warning::\n\n	Do not call PySSC.data_free on the ssc_data_t provided to ``wrap``")},
		{NULL,              NULL}           /* sentinel */
};

PyDoc_STRVAR(module_doc,
			 "Electric load calculator for residential buildings");


static int
BelpeModule_exec(PyObject *m)
{
	/* Finalize the type object including setting type of the new type
	 * object; doing it here is required for portability, too. */

	if (PySAM_load_lib(m) < 0) goto fail;
	if (PySAM_init_error(m) < 0) goto fail;

	Belpe_Type.tp_dict = PyDict_New();
	if (!Belpe_Type.tp_dict) { goto fail; }

	/// Add the LoadProfileEstimator type object to Belpe_Type
	if (PyType_Ready(&LoadProfileEstimator_Type) < 0) { goto fail; }
	PyDict_SetItemString(Belpe_Type.tp_dict,
				"LoadProfileEstimator",
				(PyObject*)&LoadProfileEstimator_Type);
	Py_DECREF(&LoadProfileEstimator_Type);

	/// Add the Outputs type object to Belpe_Type
	if (PyType_Ready(&Outputs_Type) < 0) { goto fail; }
	PyDict_SetItemString(Belpe_Type.tp_dict,
				"Outputs",
				(PyObject*)&Outputs_Type);
	Py_DECREF(&Outputs_Type);

	/// Add the Belpe type object to the module
	if (PyType_Ready(&Belpe_Type) < 0) { goto fail; }
	PyModule_AddObject(m,
				"Belpe",
				(PyObject*)&Belpe_Type);

	return 0;
	fail:
	Py_XDECREF(m);
	return -1;
}

static struct PyModuleDef_Slot BelpeModule_slots[] = {
		{Py_mod_exec, BelpeModule_exec},
		{0, NULL},
};

static struct PyModuleDef BelpeModule = {
		PyModuleDef_HEAD_INIT,
		"Belpe",
		module_doc,
		0,
		BelpeModule_methods,
		BelpeModule_slots,
		NULL,
		NULL,
		NULL
};

/* Export function for the module */

PyMODINIT_FUNC
PyInit_Belpe(void)
{
	return PyModuleDef_Init(&BelpeModule);
}