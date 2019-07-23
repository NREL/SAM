#include <Python.h>

#include <SAM_IphToLcoefcr.h>
#include <SAM_api.h>

#include "PySAM_utils.h"



	/*
	 * IPHLCOH Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_IphToLcoefcr   data_ptr;
} IPHLCOHObject;

static PyTypeObject IPHLCOH_Type;

static PyObject *
IPHLCOH_new(SAM_IphToLcoefcr data_ptr)
{
	PyObject* new_obj = IPHLCOH_Type.tp_alloc(&IPHLCOH_Type,0);

	IPHLCOHObject* IPHLCOH_obj = (IPHLCOHObject*)new_obj;

	IPHLCOH_obj->data_ptr = data_ptr;

	return new_obj;
}

/* IPHLCOH methods */

static PyObject *
IPHLCOH_assign(IPHLCOHObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "IphToLcoefcr", "IPHLCOH")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
IPHLCOH_export(IPHLCOHObject *self, PyObject *args)
{
	PyTypeObject* tp = &IPHLCOH_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef IPHLCOH_methods[] = {
		{"assign",            (PyCFunction)IPHLCOH_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``IPHLCOH_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)IPHLCOH_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
IPHLCOH_get_annual_electricity_consumption(IPHLCOHObject *self, void *closure)
{
	return PySAM_double_getter(SAM_IphToLcoefcr_IPHLCOH_annual_electricity_consumption_nget, self->data_ptr);
}

static int
IPHLCOH_set_annual_electricity_consumption(IPHLCOHObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_IphToLcoefcr_IPHLCOH_annual_electricity_consumption_nset, self->data_ptr);
}

static PyObject *
IPHLCOH_get_electricity_rate(IPHLCOHObject *self, void *closure)
{
	return PySAM_double_getter(SAM_IphToLcoefcr_IPHLCOH_electricity_rate_nget, self->data_ptr);
}

static int
IPHLCOH_set_electricity_rate(IPHLCOHObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_IphToLcoefcr_IPHLCOH_electricity_rate_nset, self->data_ptr);
}

static PyGetSetDef IPHLCOH_getset[] = {
{"annual_electricity_consumption", (getter)IPHLCOH_get_annual_electricity_consumption,(setter)IPHLCOH_set_annual_electricity_consumption,
	PyDoc_STR("*float*: Annual electricity consumptoin w/ avail derate [kWe-hr]\n\n*Required*: True"),
 	NULL},
{"electricity_rate", (getter)IPHLCOH_get_electricity_rate,(setter)IPHLCOH_set_electricity_rate,
	PyDoc_STR("*float*: Cost of electricity used to operate pumps/trackers [$/kWe]\n\n*Required*: True"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject IPHLCOH_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"IphToLcoefcr.IPHLCOH",             /*tp_name*/
		sizeof(IPHLCOHObject),          /*tp_basicsize*/
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
		IPHLCOH_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		IPHLCOH_getset,          /*tp_getset*/
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
	 * SimpleLCOE Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_IphToLcoefcr   data_ptr;
} SimpleLCOEObject;

static PyTypeObject SimpleLCOE_Type;

static PyObject *
SimpleLCOE_new(SAM_IphToLcoefcr data_ptr)
{
	PyObject* new_obj = SimpleLCOE_Type.tp_alloc(&SimpleLCOE_Type,0);

	SimpleLCOEObject* SimpleLCOE_obj = (SimpleLCOEObject*)new_obj;

	SimpleLCOE_obj->data_ptr = data_ptr;

	return new_obj;
}

/* SimpleLCOE methods */

static PyObject *
SimpleLCOE_assign(SimpleLCOEObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "IphToLcoefcr", "SimpleLCOE")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
SimpleLCOE_export(SimpleLCOEObject *self, PyObject *args)
{
	PyTypeObject* tp = &SimpleLCOE_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef SimpleLCOE_methods[] = {
		{"assign",            (PyCFunction)SimpleLCOE_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``SimpleLCOE_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)SimpleLCOE_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
SimpleLCOE_get_fixed_operating_cost(SimpleLCOEObject *self, void *closure)
{
	return PySAM_double_getter(SAM_IphToLcoefcr_SimpleLCOE_fixed_operating_cost_nget, self->data_ptr);
}

static int
SimpleLCOE_set_fixed_operating_cost(SimpleLCOEObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_IphToLcoefcr_SimpleLCOE_fixed_operating_cost_nset, self->data_ptr);
}

static PyGetSetDef SimpleLCOE_getset[] = {
{"fixed_operating_cost", (getter)SimpleLCOE_get_fixed_operating_cost,(setter)SimpleLCOE_set_fixed_operating_cost,
	PyDoc_STR("*float*: Annual fixed operating cost [$/kW]\n\n*Required*: True"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject SimpleLCOE_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"IphToLcoefcr.SimpleLCOE",             /*tp_name*/
		sizeof(SimpleLCOEObject),          /*tp_basicsize*/
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
		SimpleLCOE_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		SimpleLCOE_getset,          /*tp_getset*/
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
	SAM_IphToLcoefcr   data_ptr;
} OutputsObject;

static PyTypeObject Outputs_Type;

static PyObject *
Outputs_new(SAM_IphToLcoefcr data_ptr)
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

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "IphToLcoefcr", "Outputs")){
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
		"IphToLcoefcr.Outputs",             /*tp_name*/
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
 * IphToLcoefcr
 */

typedef struct {
	PyObject_HEAD
	PyObject            *x_attr;        /* Attributes dictionary */
	SAM_IphToLcoefcr   data_ptr;
} IphToLcoefcrObject;

static PyTypeObject IphToLcoefcr_Type;

#define IphToLcoefcrObject_Check(v)      (Py_TYPE(v) == &IphToLcoefcr_Type)

static IphToLcoefcrObject *
newIphToLcoefcrObject(void* data_ptr)
{
	IphToLcoefcrObject *self;
	self = PyObject_New(IphToLcoefcrObject, &IphToLcoefcr_Type);

	PySAM_TECH_ATTR("IphToLcoefcr", SAM_IphToLcoefcr_construct)

	PyObject* IPHLCOH_obj = IPHLCOH_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "IPHLCOH", IPHLCOH_obj);
	Py_DECREF(IPHLCOH_obj);

	PyObject* SimpleLCOE_obj = SimpleLCOE_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "SimpleLCOE", SimpleLCOE_obj);
	Py_DECREF(SimpleLCOE_obj);

	PyObject* Outputs_obj = Outputs_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "Outputs", Outputs_obj);
	Py_DECREF(Outputs_obj);


	return self;
}

/* IphToLcoefcr methods */

static void
IphToLcoefcr_dealloc(IphToLcoefcrObject *self)
{
	Py_XDECREF(self->x_attr);
	SAM_IphToLcoefcr_destruct(self->data_ptr);
	PyObject_Del(self);
}


static PyObject *
IphToLcoefcr_execute(IphToLcoefcrObject *self, PyObject *args)
{
	int verbosity = 0;

	if (!PyArg_ParseTuple(args, "|i", &verbosity))
		return NULL;

	SAM_error error = new_error();
	SAM_IphToLcoefcr_execute(self->data_ptr, verbosity, &error);
	if (PySAM_has_error(error )) return NULL;

	Py_INCREF(Py_None);
	return Py_None;
}


static PyObject *
IphToLcoefcr_assign(IphToLcoefcrObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_nested_dict((PyObject*)self, self->x_attr, self->data_ptr, dict, "IphToLcoefcr"))
		return NULL;

	Py_INCREF(Py_None);
	return Py_None;
}


static PyObject *
IphToLcoefcr_export(IphToLcoefcrObject *self, PyObject *args)
{
	return PySAM_export_to_nested_dict((PyObject *) self, self->x_attr);
}

static PyMethodDef IphToLcoefcr_methods[] = {
		{"execute",            (PyCFunction)IphToLcoefcr_execute,  METH_VARARGS,
				PyDoc_STR("execute(int verbosity) -> None\n Execute simulation with verbosity level 0 (default) or 1")},
		{"assign",            (PyCFunction)IphToLcoefcr_assign,  METH_VARARGS,
				PyDoc_STR("assign(dict) -> None\n Assign attributes from nested dictionary, except for Outputs\n\n``nested_dict = { 'IPH LCOH': { var: val, ...}, ...}``")},
		{"export",            (PyCFunction)IphToLcoefcr_export,  METH_VARARGS,
				PyDoc_STR("export() -> dict\n Export attributes into nested dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
IphToLcoefcr_getattro(IphToLcoefcrObject *self, PyObject *name)
{
	return PySAM_get_attr((PyObject*) self, (PyObject*) self->x_attr, name);
}

static int
IphToLcoefcr_setattr(IphToLcoefcrObject *self, const char *name, PyObject *v)
{
	return PySAM_set_attr((PyObject*)self, (PyObject*)self->x_attr, name, v);
}

static PyTypeObject IphToLcoefcr_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"IphToLcoefcr",            /*tp_name*/
		sizeof(IphToLcoefcrObject),/*tp_basicsize*/
		0,                          /*tp_itemsize*/
		/* methods */
		(destructor)IphToLcoefcr_dealloc,    /*tp_dealloc*/
		0,                          /*tp_print*/
		(getattrfunc)0,             /*tp_getattr*/
		(setattrfunc)IphToLcoefcr_setattr,   /*tp_setattr*/
		0,                          /*tp_reserved*/
		0,                          /*tp_repr*/
		0,                          /*tp_as_number*/
		0,                          /*tp_as_sequence*/
		0,                          /*tp_as_mapping*/
		0,                          /*tp_hash*/
		0,                          /*tp_call*/
		0,                          /*tp_str*/
		(getattrofunc)IphToLcoefcr_getattro, /*tp_getattro*/
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
		IphToLcoefcr_methods,      /*tp_methods*/
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


/* Function of no arguments returning new IphToLcoefcr object */

static PyObject *
IphToLcoefcr_new(PyObject *self, PyObject *args)
{
	IphToLcoefcrObject *rv;
	rv = newIphToLcoefcrObject(0);
	if (rv == NULL)
		return NULL;
	return (PyObject *)rv;
}

static PyObject *
IphToLcoefcr_wrap(PyObject *self, PyObject *args)
{
	IphToLcoefcrObject *rv;
	long long int ptr = 0;  // 64 bit arch
	if (!PyArg_ParseTuple(args, "L:wrap", &ptr)){
		PyErr_BadArgument();
		return NULL;
	}
	rv = newIphToLcoefcrObject((void*)ptr);
	if (rv == NULL)
		return NULL;
	return (PyObject *)rv;
}

static PyObject *
IphToLcoefcr_default(PyObject *self, PyObject *args)
{
	IphToLcoefcrObject *rv;
	char* def = 0;
	if (!PyArg_ParseTuple(args, "s:default", &def)){
		PyErr_BadArgument();
		return NULL;
	}
	rv = newIphToLcoefcrObject(0);
	if (rv == NULL)
		return NULL;

	PySAM_load_defaults((PyObject*)rv, rv->x_attr, rv->data_ptr, "IphToLcoefcr", def);

	return (PyObject *)rv;
}

/* ---------- */


/* List of functions defined in the module */

static PyMethodDef IphToLcoefcrModule_methods[] = {
		{"new",             IphToLcoefcr_new,         METH_VARARGS,
				PyDoc_STR("new() -> IphToLcoefcr")},
		{"default",             IphToLcoefcr_default,         METH_VARARGS,
				PyDoc_STR("default(config) -> IphToLcoefcr\n\nUse financial model-specific default attributes\n"
				"config options:\n\n- \"DSGLIPHLCOHCalculator\"\n- \"PhysicalTroughIPHLCOHCalculator\"")},
		{"wrap",             IphToLcoefcr_wrap,         METH_VARARGS,
				PyDoc_STR("wrap(ssc_data_t) -> IphToLcoefcr\n\nUse existing PySSC data\n\n.. warning::\n\n	Do not call PySSC.data_free on the ssc_data_t provided to ``wrap``")},
		{NULL,              NULL}           /* sentinel */
};

PyDoc_STRVAR(module_doc,
			 "Calculate levelized cost of heat using fixed charge rate method for industrial process heat models");


static int
IphToLcoefcrModule_exec(PyObject *m)
{
	/* Finalize the type object including setting type of the new type
	 * object; doing it here is required for portability, too. */

	if (PySAM_load_lib(m) < 0) goto fail;
	if (PySAM_init_error(m) < 0) goto fail;

	IphToLcoefcr_Type.tp_dict = PyDict_New();
	if (!IphToLcoefcr_Type.tp_dict) { goto fail; }

	/// Add the IPHLCOH type object to IphToLcoefcr_Type
	if (PyType_Ready(&IPHLCOH_Type) < 0) { goto fail; }
	PyDict_SetItemString(IphToLcoefcr_Type.tp_dict,
				"IPHLCOH",
				(PyObject*)&IPHLCOH_Type);
	Py_DECREF(&IPHLCOH_Type);

	/// Add the SimpleLCOE type object to IphToLcoefcr_Type
	if (PyType_Ready(&SimpleLCOE_Type) < 0) { goto fail; }
	PyDict_SetItemString(IphToLcoefcr_Type.tp_dict,
				"SimpleLCOE",
				(PyObject*)&SimpleLCOE_Type);
	Py_DECREF(&SimpleLCOE_Type);

	/// Add the Outputs type object to IphToLcoefcr_Type
	if (PyType_Ready(&Outputs_Type) < 0) { goto fail; }
	PyDict_SetItemString(IphToLcoefcr_Type.tp_dict,
				"Outputs",
				(PyObject*)&Outputs_Type);
	Py_DECREF(&Outputs_Type);

	/// Add the IphToLcoefcr type object to the module
	if (PyType_Ready(&IphToLcoefcr_Type) < 0) { goto fail; }
	PyModule_AddObject(m,
				"IphToLcoefcr",
				(PyObject*)&IphToLcoefcr_Type);

	return 0;
	fail:
	Py_XDECREF(m);
	return -1;
}

static struct PyModuleDef_Slot IphToLcoefcrModule_slots[] = {
		{Py_mod_exec, IphToLcoefcrModule_exec},
		{0, NULL},
};

static struct PyModuleDef IphToLcoefcrModule = {
		PyModuleDef_HEAD_INIT,
		"IphToLcoefcr",
		module_doc,
		0,
		IphToLcoefcrModule_methods,
		IphToLcoefcrModule_slots,
		NULL,
		NULL,
		NULL
};

/* Export function for the module */

PyMODINIT_FUNC
PyInit_IphToLcoefcr(void)
{
	return PyModuleDef_Init(&IphToLcoefcrModule);
}