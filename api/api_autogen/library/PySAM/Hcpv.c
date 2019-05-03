#include <Python.h>

#include <SAM_Hcpv.h>
#include <SAM_api.h>

#include "PySAM_utils.h"



	/*
	 * SolarResourceData Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Hcpv   data_ptr;
} SolarResourceDataObject;

static PyTypeObject SolarResourceData_Type;

static PyObject *
SolarResourceData_new(SAM_Hcpv data_ptr)
{
	PyObject* new_obj = SolarResourceData_Type.tp_alloc(&SolarResourceData_Type,0);

	SolarResourceDataObject* SolarResourceData_obj = (SolarResourceDataObject*)new_obj;

	SolarResourceData_obj->data_ptr = data_ptr;

	return new_obj;
}

/* SolarResourceData methods */

static PyObject *
SolarResourceData_assign(SolarResourceDataObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Hcpv", "SolarResourceData")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
SolarResourceData_export(SolarResourceDataObject *self, PyObject *args)
{
	PyTypeObject* tp = &SolarResourceData_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef SolarResourceData_methods[] = {
		{"assign",            (PyCFunction)SolarResourceData_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``SolarResourceData_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)SolarResourceData_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
SolarResourceData_get_file_name(SolarResourceDataObject *self, void *closure)
{
	return PySAM_string_getter(SAM_Hcpv_SolarResourceData_file_name_sget, self->data_ptr);
}

static int
SolarResourceData_set_file_name(SolarResourceDataObject *self, PyObject *value, void *closure)
{
	return PySAM_string_setter(value, SAM_Hcpv_SolarResourceData_file_name_sset, self->data_ptr);
}

static PyGetSetDef SolarResourceData_getset[] = {
{"file_name", (getter)SolarResourceData_get_file_name,(setter)SolarResourceData_set_file_name,
	PyDoc_STR("*str*: Weather file in TMY2, TMY3, EPW, or SMW.\n\n*Constraints*: LOCAL_FILE\n\n*Required*: True"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject SolarResourceData_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Hcpv.SolarResourceData",             /*tp_name*/
		sizeof(SolarResourceDataObject),          /*tp_basicsize*/
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
		SolarResourceData_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		SolarResourceData_getset,          /*tp_getset*/
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
	SAM_Hcpv   data_ptr;
} PVWattsObject;

static PyTypeObject PVWatts_Type;

static PyObject *
PVWatts_new(SAM_Hcpv data_ptr)
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

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Hcpv", "PVWatts")){
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
PVWatts_get_system_capacity(PVWattsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Hcpv_PVWatts_system_capacity_nget, self->data_ptr);
}

static int
PVWatts_set_system_capacity(PVWattsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Hcpv_PVWatts_system_capacity_nset, self->data_ptr);
}

static PyGetSetDef PVWatts_getset[] = {
{"system_capacity", (getter)PVWatts_get_system_capacity,(setter)PVWatts_set_system_capacity,
	PyDoc_STR("*float*: Nameplate capacity [kW]\n\n*Required*: True"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject PVWatts_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Hcpv.PVWatts",             /*tp_name*/
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
	 * HCPVModule Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Hcpv   data_ptr;
} HCPVModuleObject;

static PyTypeObject HCPVModule_Type;

static PyObject *
HCPVModule_new(SAM_Hcpv data_ptr)
{
	PyObject* new_obj = HCPVModule_Type.tp_alloc(&HCPVModule_Type,0);

	HCPVModuleObject* HCPVModule_obj = (HCPVModuleObject*)new_obj;

	HCPVModule_obj->data_ptr = data_ptr;

	return new_obj;
}

/* HCPVModule methods */

static PyObject *
HCPVModule_assign(HCPVModuleObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Hcpv", "HCPVModule")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
HCPVModule_export(HCPVModuleObject *self, PyObject *args)
{
	PyTypeObject* tp = &HCPVModule_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef HCPVModule_methods[] = {
		{"assign",            (PyCFunction)HCPVModule_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``HCPVModule_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)HCPVModule_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
HCPVModule_get_module_a(HCPVModuleObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Hcpv_HCPVModule_module_a_nget, self->data_ptr);
}

static int
HCPVModule_set_module_a(HCPVModuleObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Hcpv_HCPVModule_module_a_nset, self->data_ptr);
}

static PyObject *
HCPVModule_get_module_a0(HCPVModuleObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Hcpv_HCPVModule_module_a0_nget, self->data_ptr);
}

static int
HCPVModule_set_module_a0(HCPVModuleObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Hcpv_HCPVModule_module_a0_nset, self->data_ptr);
}

static PyObject *
HCPVModule_get_module_a1(HCPVModuleObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Hcpv_HCPVModule_module_a1_nget, self->data_ptr);
}

static int
HCPVModule_set_module_a1(HCPVModuleObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Hcpv_HCPVModule_module_a1_nset, self->data_ptr);
}

static PyObject *
HCPVModule_get_module_a2(HCPVModuleObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Hcpv_HCPVModule_module_a2_nget, self->data_ptr);
}

static int
HCPVModule_set_module_a2(HCPVModuleObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Hcpv_HCPVModule_module_a2_nset, self->data_ptr);
}

static PyObject *
HCPVModule_get_module_a3(HCPVModuleObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Hcpv_HCPVModule_module_a3_nget, self->data_ptr);
}

static int
HCPVModule_set_module_a3(HCPVModuleObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Hcpv_HCPVModule_module_a3_nset, self->data_ptr);
}

static PyObject *
HCPVModule_get_module_a4(HCPVModuleObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Hcpv_HCPVModule_module_a4_nget, self->data_ptr);
}

static int
HCPVModule_set_module_a4(HCPVModuleObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Hcpv_HCPVModule_module_a4_nset, self->data_ptr);
}

static PyObject *
HCPVModule_get_module_alignment_error(HCPVModuleObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Hcpv_HCPVModule_module_alignment_error_nget, self->data_ptr);
}

static int
HCPVModule_set_module_alignment_error(HCPVModuleObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Hcpv_HCPVModule_module_alignment_error_nset, self->data_ptr);
}

static PyObject *
HCPVModule_get_module_b(HCPVModuleObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Hcpv_HCPVModule_module_b_nget, self->data_ptr);
}

static int
HCPVModule_set_module_b(HCPVModuleObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Hcpv_HCPVModule_module_b_nset, self->data_ptr);
}

static PyObject *
HCPVModule_get_module_cell_area(HCPVModuleObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Hcpv_HCPVModule_module_cell_area_nget, self->data_ptr);
}

static int
HCPVModule_set_module_cell_area(HCPVModuleObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Hcpv_HCPVModule_module_cell_area_nset, self->data_ptr);
}

static PyObject *
HCPVModule_get_module_concentration(HCPVModuleObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Hcpv_HCPVModule_module_concentration_nget, self->data_ptr);
}

static int
HCPVModule_set_module_concentration(HCPVModuleObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Hcpv_HCPVModule_module_concentration_nset, self->data_ptr);
}

static PyObject *
HCPVModule_get_module_dT(HCPVModuleObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Hcpv_HCPVModule_module_dT_nget, self->data_ptr);
}

static int
HCPVModule_set_module_dT(HCPVModuleObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Hcpv_HCPVModule_module_dT_nset, self->data_ptr);
}

static PyObject *
HCPVModule_get_module_flutter_loss_coeff(HCPVModuleObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Hcpv_HCPVModule_module_flutter_loss_coeff_nget, self->data_ptr);
}

static int
HCPVModule_set_module_flutter_loss_coeff(HCPVModuleObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Hcpv_HCPVModule_module_flutter_loss_coeff_nset, self->data_ptr);
}

static PyObject *
HCPVModule_get_module_mjeff(HCPVModuleObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Hcpv_HCPVModule_module_mjeff_aget, self->data_ptr);
}

static int
HCPVModule_set_module_mjeff(HCPVModuleObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Hcpv_HCPVModule_module_mjeff_aset, self->data_ptr);
}

static PyObject *
HCPVModule_get_module_ncells(HCPVModuleObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Hcpv_HCPVModule_module_ncells_nget, self->data_ptr);
}

static int
HCPVModule_set_module_ncells(HCPVModuleObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Hcpv_HCPVModule_module_ncells_nset, self->data_ptr);
}

static PyObject *
HCPVModule_get_module_optical_error(HCPVModuleObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Hcpv_HCPVModule_module_optical_error_nget, self->data_ptr);
}

static int
HCPVModule_set_module_optical_error(HCPVModuleObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Hcpv_HCPVModule_module_optical_error_nset, self->data_ptr);
}

static PyObject *
HCPVModule_get_module_rad(HCPVModuleObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Hcpv_HCPVModule_module_rad_aget, self->data_ptr);
}

static int
HCPVModule_set_module_rad(HCPVModuleObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Hcpv_HCPVModule_module_rad_aset, self->data_ptr);
}

static PyObject *
HCPVModule_get_module_reference(HCPVModuleObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Hcpv_HCPVModule_module_reference_nget, self->data_ptr);
}

static int
HCPVModule_set_module_reference(HCPVModuleObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Hcpv_HCPVModule_module_reference_nset, self->data_ptr);
}

static PyObject *
HCPVModule_get_module_temp_coeff(HCPVModuleObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Hcpv_HCPVModule_module_temp_coeff_nget, self->data_ptr);
}

static int
HCPVModule_set_module_temp_coeff(HCPVModuleObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Hcpv_HCPVModule_module_temp_coeff_nset, self->data_ptr);
}

static PyGetSetDef HCPVModule_getset[] = {
{"module_a", (getter)HCPVModule_get_module_a,(setter)HCPVModule_set_module_a,
	PyDoc_STR("*float*: Equation variable (a), at high irradiance & low wind speed [none]\n\n*Required*: True"),
 	NULL},
{"module_a0", (getter)HCPVModule_get_module_a0,(setter)HCPVModule_set_module_a0,
	PyDoc_STR("*float*: Air mass modifier coefficient 0 [none]\n\n*Required*: True"),
 	NULL},
{"module_a1", (getter)HCPVModule_get_module_a1,(setter)HCPVModule_set_module_a1,
	PyDoc_STR("*float*: Air mass modifier coefficient 1 [none]\n\n*Required*: True"),
 	NULL},
{"module_a2", (getter)HCPVModule_get_module_a2,(setter)HCPVModule_set_module_a2,
	PyDoc_STR("*float*: Air mass modifier coefficient 2 [none]\n\n*Required*: True"),
 	NULL},
{"module_a3", (getter)HCPVModule_get_module_a3,(setter)HCPVModule_set_module_a3,
	PyDoc_STR("*float*: Air mass modifier coefficient 3 [none]\n\n*Required*: True"),
 	NULL},
{"module_a4", (getter)HCPVModule_get_module_a4,(setter)HCPVModule_set_module_a4,
	PyDoc_STR("*float*: Air mass modifier coefficient 4 [none]\n\n*Required*: True"),
 	NULL},
{"module_alignment_error", (getter)HCPVModule_get_module_alignment_error,(setter)HCPVModule_set_module_alignment_error,
	PyDoc_STR("*float*: Alignment loss factor [0..1]\n\n*Required*: True"),
 	NULL},
{"module_b", (getter)HCPVModule_get_module_b,(setter)HCPVModule_set_module_b,
	PyDoc_STR("*float*: Equation variable (b), rate at which module temp drops [none]\n\n*Required*: True"),
 	NULL},
{"module_cell_area", (getter)HCPVModule_get_module_cell_area,(setter)HCPVModule_set_module_cell_area,
	PyDoc_STR("*float*: Single cell area [cm^2]\n\n*Required*: True"),
 	NULL},
{"module_concentration", (getter)HCPVModule_get_module_concentration,(setter)HCPVModule_set_module_concentration,
	PyDoc_STR("*float*: Concentration ratio [none]\n\n*Required*: True"),
 	NULL},
{"module_dT", (getter)HCPVModule_get_module_dT,(setter)HCPVModule_set_module_dT,
	PyDoc_STR("*float*: Equation variable (dT), temp diff between heat sink & cell [C]\n\n*Required*: True"),
 	NULL},
{"module_flutter_loss_coeff", (getter)HCPVModule_get_module_flutter_loss_coeff,(setter)HCPVModule_set_module_flutter_loss_coeff,
	PyDoc_STR("*float*: Wind flutter loss factor [0..1 per m/s]\n\n*Required*: True"),
 	NULL},
{"module_mjeff", (getter)HCPVModule_get_module_mjeff,(setter)HCPVModule_set_module_mjeff,
	PyDoc_STR("*sequence*: Module junction efficiency array [percent]\n\n*Required*: True"),
 	NULL},
{"module_ncells", (getter)HCPVModule_get_module_ncells,(setter)HCPVModule_set_module_ncells,
	PyDoc_STR("*float*: Number of cells [none]\n\n*Constraints*: INTEGER\n\n*Required*: True"),
 	NULL},
{"module_optical_error", (getter)HCPVModule_get_module_optical_error,(setter)HCPVModule_set_module_optical_error,
	PyDoc_STR("*float*: Optical error factor [0..1]\n\n*Required*: True"),
 	NULL},
{"module_rad", (getter)HCPVModule_get_module_rad,(setter)HCPVModule_set_module_rad,
	PyDoc_STR("*sequence*: POA irradiance array [W/m^2]\n\n*Required*: True"),
 	NULL},
{"module_reference", (getter)HCPVModule_get_module_reference,(setter)HCPVModule_set_module_reference,
	PyDoc_STR("*float*: Index in arrays of the reference condition [none]\n\n*Constraints*: INTEGER\n\n*Required*: True"),
 	NULL},
{"module_temp_coeff", (getter)HCPVModule_get_module_temp_coeff,(setter)HCPVModule_set_module_temp_coeff,
	PyDoc_STR("*float*: Temperature coefficient [%/C]\n\n*Required*: True"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject HCPVModule_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Hcpv.HCPVModule",             /*tp_name*/
		sizeof(HCPVModuleObject),          /*tp_basicsize*/
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
		HCPVModule_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		HCPVModule_getset,          /*tp_getset*/
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
	SAM_Hcpv   data_ptr;
} InverterCECDatabaseObject;

static PyTypeObject InverterCECDatabase_Type;

static PyObject *
InverterCECDatabase_new(SAM_Hcpv data_ptr)
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

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Hcpv", "InverterCECDatabase")){
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
	return PySAM_double_getter(SAM_Hcpv_InverterCECDatabase_inv_snl_c0_nget, self->data_ptr);
}

static int
InverterCECDatabase_set_inv_snl_c0(InverterCECDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Hcpv_InverterCECDatabase_inv_snl_c0_nset, self->data_ptr);
}

static PyObject *
InverterCECDatabase_get_inv_snl_c1(InverterCECDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Hcpv_InverterCECDatabase_inv_snl_c1_nget, self->data_ptr);
}

static int
InverterCECDatabase_set_inv_snl_c1(InverterCECDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Hcpv_InverterCECDatabase_inv_snl_c1_nset, self->data_ptr);
}

static PyObject *
InverterCECDatabase_get_inv_snl_c2(InverterCECDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Hcpv_InverterCECDatabase_inv_snl_c2_nget, self->data_ptr);
}

static int
InverterCECDatabase_set_inv_snl_c2(InverterCECDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Hcpv_InverterCECDatabase_inv_snl_c2_nset, self->data_ptr);
}

static PyObject *
InverterCECDatabase_get_inv_snl_c3(InverterCECDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Hcpv_InverterCECDatabase_inv_snl_c3_nget, self->data_ptr);
}

static int
InverterCECDatabase_set_inv_snl_c3(InverterCECDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Hcpv_InverterCECDatabase_inv_snl_c3_nset, self->data_ptr);
}

static PyObject *
InverterCECDatabase_get_inv_snl_paco(InverterCECDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Hcpv_InverterCECDatabase_inv_snl_paco_nget, self->data_ptr);
}

static int
InverterCECDatabase_set_inv_snl_paco(InverterCECDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Hcpv_InverterCECDatabase_inv_snl_paco_nset, self->data_ptr);
}

static PyObject *
InverterCECDatabase_get_inv_snl_pdco(InverterCECDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Hcpv_InverterCECDatabase_inv_snl_pdco_nget, self->data_ptr);
}

static int
InverterCECDatabase_set_inv_snl_pdco(InverterCECDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Hcpv_InverterCECDatabase_inv_snl_pdco_nset, self->data_ptr);
}

static PyObject *
InverterCECDatabase_get_inv_snl_pnt(InverterCECDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Hcpv_InverterCECDatabase_inv_snl_pnt_nget, self->data_ptr);
}

static int
InverterCECDatabase_set_inv_snl_pnt(InverterCECDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Hcpv_InverterCECDatabase_inv_snl_pnt_nset, self->data_ptr);
}

static PyObject *
InverterCECDatabase_get_inv_snl_pso(InverterCECDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Hcpv_InverterCECDatabase_inv_snl_pso_nget, self->data_ptr);
}

static int
InverterCECDatabase_set_inv_snl_pso(InverterCECDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Hcpv_InverterCECDatabase_inv_snl_pso_nset, self->data_ptr);
}

static PyObject *
InverterCECDatabase_get_inv_snl_vdcmax(InverterCECDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Hcpv_InverterCECDatabase_inv_snl_vdcmax_nget, self->data_ptr);
}

static int
InverterCECDatabase_set_inv_snl_vdcmax(InverterCECDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Hcpv_InverterCECDatabase_inv_snl_vdcmax_nset, self->data_ptr);
}

static PyObject *
InverterCECDatabase_get_inv_snl_vdco(InverterCECDatabaseObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Hcpv_InverterCECDatabase_inv_snl_vdco_nget, self->data_ptr);
}

static int
InverterCECDatabase_set_inv_snl_vdco(InverterCECDatabaseObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Hcpv_InverterCECDatabase_inv_snl_vdco_nset, self->data_ptr);
}

static PyGetSetDef InverterCECDatabase_getset[] = {
{"inv_snl_c0", (getter)InverterCECDatabase_get_inv_snl_c0,(setter)InverterCECDatabase_set_inv_snl_c0,
	PyDoc_STR("*float*: Parameter defining the curvature (parabolic) of the relationship between ac-power and dc-power at the reference operating condition, default value of zero gives a linear relationship, (1/W) [xxx]\n\n*Required*: True"),
 	NULL},
{"inv_snl_c1", (getter)InverterCECDatabase_get_inv_snl_c1,(setter)InverterCECDatabase_set_inv_snl_c1,
	PyDoc_STR("*float*: Empirical coefficient allowing Pdco to vary linearly with dc-voltage input, default value is zero, (1/V) [xxx]\n\n*Required*: True"),
 	NULL},
{"inv_snl_c2", (getter)InverterCECDatabase_get_inv_snl_c2,(setter)InverterCECDatabase_set_inv_snl_c2,
	PyDoc_STR("*float*: Empirical coefficient allowing Pso to vary linearly with dc-voltage input, default value is zero, (1/V) [xxx]\n\n*Required*: True"),
 	NULL},
{"inv_snl_c3", (getter)InverterCECDatabase_get_inv_snl_c3,(setter)InverterCECDatabase_set_inv_snl_c3,
	PyDoc_STR("*float*: Empirical coefficient allowing Co to vary linearly with dc-voltage input, default value is zero, (1/V) [xxx]\n\n*Required*: True"),
 	NULL},
{"inv_snl_paco", (getter)InverterCECDatabase_get_inv_snl_paco,(setter)InverterCECDatabase_set_inv_snl_paco,
	PyDoc_STR("*float*: W maximum ac-power rating for inverter at reference or nominal operating condition, assumed to be an upper limit value, (W) [xxx]\n\n*Required*: True"),
 	NULL},
{"inv_snl_pdco", (getter)InverterCECDatabase_get_inv_snl_pdco,(setter)InverterCECDatabase_set_inv_snl_pdco,
	PyDoc_STR("*float*: W dc-power level at which the ac-power rating is achieved at the reference operating condition, (W) [xxx]\n\n*Required*: True"),
 	NULL},
{"inv_snl_pnt", (getter)InverterCECDatabase_get_inv_snl_pnt,(setter)InverterCECDatabase_set_inv_snl_pnt,
	PyDoc_STR("*float*: W ac-power consumed by inverter at night (night tare) to maintain circuitry required to sense PV array voltage, (W) [xxx]\n\n*Required*: True"),
 	NULL},
{"inv_snl_pso", (getter)InverterCECDatabase_get_inv_snl_pso,(setter)InverterCECDatabase_set_inv_snl_pso,
	PyDoc_STR("*float*: W dc-power required to start the inversion process, or self-consumption by inverter, strongly influences inverter efficiency at low power levels, (W) [xxx]\n\n*Required*: True"),
 	NULL},
{"inv_snl_vdcmax", (getter)InverterCECDatabase_get_inv_snl_vdcmax,(setter)InverterCECDatabase_set_inv_snl_vdcmax,
	PyDoc_STR("*float*: V (Vdcmax) dc-voltage maximum operating voltage, (V) [xxx]\n\n*Required*: True"),
 	NULL},
{"inv_snl_vdco", (getter)InverterCECDatabase_get_inv_snl_vdco,(setter)InverterCECDatabase_set_inv_snl_vdco,
	PyDoc_STR("*float*: V (Vnom) dc-voltage level at which the ac-power rating is achieved at the reference operating condition, (V) [xxx]\n\n*Required*: True"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject InverterCECDatabase_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Hcpv.InverterCECDatabase",             /*tp_name*/
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
	 * HCPVArray Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Hcpv   data_ptr;
} HCPVArrayObject;

static PyTypeObject HCPVArray_Type;

static PyObject *
HCPVArray_new(SAM_Hcpv data_ptr)
{
	PyObject* new_obj = HCPVArray_Type.tp_alloc(&HCPVArray_Type,0);

	HCPVArrayObject* HCPVArray_obj = (HCPVArrayObject*)new_obj;

	HCPVArray_obj->data_ptr = data_ptr;

	return new_obj;
}

/* HCPVArray methods */

static PyObject *
HCPVArray_assign(HCPVArrayObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Hcpv", "HCPVArray")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
HCPVArray_export(HCPVArrayObject *self, PyObject *args)
{
	PyTypeObject* tp = &HCPVArray_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef HCPVArray_methods[] = {
		{"assign",            (PyCFunction)HCPVArray_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``HCPVArray_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)HCPVArray_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
HCPVArray_get_array_ac_wiring_loss(HCPVArrayObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Hcpv_HCPVArray_array_ac_wiring_loss_nget, self->data_ptr);
}

static int
HCPVArray_set_array_ac_wiring_loss(HCPVArrayObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Hcpv_HCPVArray_array_ac_wiring_loss_nset, self->data_ptr);
}

static PyObject *
HCPVArray_get_array_dc_mismatch_loss(HCPVArrayObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Hcpv_HCPVArray_array_dc_mismatch_loss_nget, self->data_ptr);
}

static int
HCPVArray_set_array_dc_mismatch_loss(HCPVArrayObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Hcpv_HCPVArray_array_dc_mismatch_loss_nset, self->data_ptr);
}

static PyObject *
HCPVArray_get_array_dc_wiring_loss(HCPVArrayObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Hcpv_HCPVArray_array_dc_wiring_loss_nget, self->data_ptr);
}

static int
HCPVArray_set_array_dc_wiring_loss(HCPVArrayObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Hcpv_HCPVArray_array_dc_wiring_loss_nset, self->data_ptr);
}

static PyObject *
HCPVArray_get_array_diode_conn_loss(HCPVArrayObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Hcpv_HCPVArray_array_diode_conn_loss_nget, self->data_ptr);
}

static int
HCPVArray_set_array_diode_conn_loss(HCPVArrayObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Hcpv_HCPVArray_array_diode_conn_loss_nset, self->data_ptr);
}

static PyObject *
HCPVArray_get_array_enable_azalt_sf(HCPVArrayObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Hcpv_HCPVArray_array_enable_azalt_sf_nget, self->data_ptr);
}

static int
HCPVArray_set_array_enable_azalt_sf(HCPVArrayObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Hcpv_HCPVArray_array_enable_azalt_sf_nset, self->data_ptr);
}

static PyObject *
HCPVArray_get_array_modules_per_tracker(HCPVArrayObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Hcpv_HCPVArray_array_modules_per_tracker_nget, self->data_ptr);
}

static int
HCPVArray_set_array_modules_per_tracker(HCPVArrayObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Hcpv_HCPVArray_array_modules_per_tracker_nset, self->data_ptr);
}

static PyObject *
HCPVArray_get_array_monthly_soiling(HCPVArrayObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Hcpv_HCPVArray_array_monthly_soiling_aget, self->data_ptr);
}

static int
HCPVArray_set_array_monthly_soiling(HCPVArrayObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Hcpv_HCPVArray_array_monthly_soiling_aset, self->data_ptr);
}

static PyObject *
HCPVArray_get_array_num_inverters(HCPVArrayObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Hcpv_HCPVArray_array_num_inverters_nget, self->data_ptr);
}

static int
HCPVArray_set_array_num_inverters(HCPVArrayObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Hcpv_HCPVArray_array_num_inverters_nset, self->data_ptr);
}

static PyObject *
HCPVArray_get_array_num_trackers(HCPVArrayObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Hcpv_HCPVArray_array_num_trackers_nget, self->data_ptr);
}

static int
HCPVArray_set_array_num_trackers(HCPVArrayObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Hcpv_HCPVArray_array_num_trackers_nset, self->data_ptr);
}

static PyObject *
HCPVArray_get_array_rlim_az_max(HCPVArrayObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Hcpv_HCPVArray_array_rlim_az_max_nget, self->data_ptr);
}

static int
HCPVArray_set_array_rlim_az_max(HCPVArrayObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Hcpv_HCPVArray_array_rlim_az_max_nset, self->data_ptr);
}

static PyObject *
HCPVArray_get_array_rlim_az_min(HCPVArrayObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Hcpv_HCPVArray_array_rlim_az_min_nget, self->data_ptr);
}

static int
HCPVArray_set_array_rlim_az_min(HCPVArrayObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Hcpv_HCPVArray_array_rlim_az_min_nset, self->data_ptr);
}

static PyObject *
HCPVArray_get_array_rlim_el_max(HCPVArrayObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Hcpv_HCPVArray_array_rlim_el_max_nget, self->data_ptr);
}

static int
HCPVArray_set_array_rlim_el_max(HCPVArrayObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Hcpv_HCPVArray_array_rlim_el_max_nset, self->data_ptr);
}

static PyObject *
HCPVArray_get_array_rlim_el_min(HCPVArrayObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Hcpv_HCPVArray_array_rlim_el_min_nget, self->data_ptr);
}

static int
HCPVArray_set_array_rlim_el_min(HCPVArrayObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Hcpv_HCPVArray_array_rlim_el_min_nset, self->data_ptr);
}

static PyObject *
HCPVArray_get_array_tracker_power_fraction(HCPVArrayObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Hcpv_HCPVArray_array_tracker_power_fraction_nget, self->data_ptr);
}

static int
HCPVArray_set_array_tracker_power_fraction(HCPVArrayObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Hcpv_HCPVArray_array_tracker_power_fraction_nset, self->data_ptr);
}

static PyObject *
HCPVArray_get_array_tracking_error(HCPVArrayObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Hcpv_HCPVArray_array_tracking_error_nget, self->data_ptr);
}

static int
HCPVArray_set_array_tracking_error(HCPVArrayObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Hcpv_HCPVArray_array_tracking_error_nset, self->data_ptr);
}

static PyObject *
HCPVArray_get_array_wind_stow_speed(HCPVArrayObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Hcpv_HCPVArray_array_wind_stow_speed_nget, self->data_ptr);
}

static int
HCPVArray_set_array_wind_stow_speed(HCPVArrayObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Hcpv_HCPVArray_array_wind_stow_speed_nset, self->data_ptr);
}

static PyObject *
HCPVArray_get_azaltsf(HCPVArrayObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Hcpv_HCPVArray_azaltsf_mget, self->data_ptr);
}

static int
HCPVArray_set_azaltsf(HCPVArrayObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_Hcpv_HCPVArray_azaltsf_mset, self->data_ptr);
}

static PyGetSetDef HCPVArray_getset[] = {
{"array_ac_wiring_loss", (getter)HCPVArray_get_array_ac_wiring_loss,(setter)HCPVArray_set_array_ac_wiring_loss,
	PyDoc_STR("*float*: AC wiring loss factor [0..1]\n\n*Required*: True"),
 	NULL},
{"array_dc_mismatch_loss", (getter)HCPVArray_get_array_dc_mismatch_loss,(setter)HCPVArray_set_array_dc_mismatch_loss,
	PyDoc_STR("*float*: DC module mismatch loss factor [0..1]\n\n*Required*: True"),
 	NULL},
{"array_dc_wiring_loss", (getter)HCPVArray_get_array_dc_wiring_loss,(setter)HCPVArray_set_array_dc_wiring_loss,
	PyDoc_STR("*float*: DC Wiring loss factor [0..1]\n\n*Required*: True"),
 	NULL},
{"array_diode_conn_loss", (getter)HCPVArray_get_array_diode_conn_loss,(setter)HCPVArray_set_array_diode_conn_loss,
	PyDoc_STR("*float*: Diodes and connections loss factor [0..1]\n\n*Required*: True"),
 	NULL},
{"array_enable_azalt_sf", (getter)HCPVArray_get_array_enable_azalt_sf,(setter)HCPVArray_set_array_enable_azalt_sf,
	PyDoc_STR("*float*: Boolean for irradiance derate [0-1]\n\n*Constraints*: INTEGER\n\n*Required*: True"),
 	NULL},
{"array_modules_per_tracker", (getter)HCPVArray_get_array_modules_per_tracker,(setter)HCPVArray_set_array_modules_per_tracker,
	PyDoc_STR("*float*: Modules on each tracker [none]\n\n*Constraints*: INTEGER\n\n*Required*: True"),
 	NULL},
{"array_monthly_soiling", (getter)HCPVArray_get_array_monthly_soiling,(setter)HCPVArray_set_array_monthly_soiling,
	PyDoc_STR("*sequence*: Monthly soiling factors array [0..1]\n\n*Required*: True"),
 	NULL},
{"array_num_inverters", (getter)HCPVArray_get_array_num_inverters,(setter)HCPVArray_set_array_num_inverters,
	PyDoc_STR("*float*: Number of inverters [none]\n\n*Required*: True"),
 	NULL},
{"array_num_trackers", (getter)HCPVArray_get_array_num_trackers,(setter)HCPVArray_set_array_num_trackers,
	PyDoc_STR("*float*: Number of trackers [none]\n\n*Constraints*: INTEGER\n\n*Required*: True"),
 	NULL},
{"array_rlim_az_max", (getter)HCPVArray_get_array_rlim_az_max,(setter)HCPVArray_set_array_rlim_az_max,
	PyDoc_STR("*float*: Tracker maximum azimuth angle [deg]\n\n*Required*: True"),
 	NULL},
{"array_rlim_az_min", (getter)HCPVArray_get_array_rlim_az_min,(setter)HCPVArray_set_array_rlim_az_min,
	PyDoc_STR("*float*: Tracker minimum azimuth angle [deg]\n\n*Required*: True"),
 	NULL},
{"array_rlim_el_max", (getter)HCPVArray_get_array_rlim_el_max,(setter)HCPVArray_set_array_rlim_el_max,
	PyDoc_STR("*float*: Tracker maximum elevation angle [deg]\n\n*Required*: True"),
 	NULL},
{"array_rlim_el_min", (getter)HCPVArray_get_array_rlim_el_min,(setter)HCPVArray_set_array_rlim_el_min,
	PyDoc_STR("*float*: Tracker minimum elevation angle [deg]\n\n*Required*: True"),
 	NULL},
{"array_tracker_power_fraction", (getter)HCPVArray_get_array_tracker_power_fraction,(setter)HCPVArray_set_array_tracker_power_fraction,
	PyDoc_STR("*float*: Single tracker power fraction [0..1]\n\n*Required*: True"),
 	NULL},
{"array_tracking_error", (getter)HCPVArray_get_array_tracking_error,(setter)HCPVArray_set_array_tracking_error,
	PyDoc_STR("*float*: General racking error [0..1]\n\n*Required*: True"),
 	NULL},
{"array_wind_stow_speed", (getter)HCPVArray_get_array_wind_stow_speed,(setter)HCPVArray_set_array_wind_stow_speed,
	PyDoc_STR("*float*: Allowed wind speed before stowing [m/s]\n\n*Required*: True"),
 	NULL},
{"azaltsf", (getter)HCPVArray_get_azaltsf,(setter)HCPVArray_set_azaltsf,
	PyDoc_STR("*sequence[sequence]*: Azimuth-Altitude Shading Table\n\n*Required*: True"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject HCPVArray_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Hcpv.HCPVArray",             /*tp_name*/
		sizeof(HCPVArrayObject),          /*tp_basicsize*/
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
		HCPVArray_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		HCPVArray_getset,          /*tp_getset*/
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
	SAM_Hcpv   data_ptr;
} OutputsObject;

static PyTypeObject Outputs_Type;

static PyObject *
Outputs_new(SAM_Hcpv data_ptr)
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

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Hcpv", "Outputs")){
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
Outputs_get_ac_loss_tracker_kwh(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Hcpv_Outputs_ac_loss_tracker_kwh_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_ac(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Hcpv_Outputs_annual_ac_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_beam(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Hcpv_Outputs_annual_beam_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_dc(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Hcpv_Outputs_annual_dc_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_dc_net(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Hcpv_Outputs_annual_dc_net_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_energy(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Hcpv_Outputs_annual_energy_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_input_radiation(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Hcpv_Outputs_annual_input_radiation_nget, self->data_ptr);
}

static PyObject *
Outputs_get_capacity_factor(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Hcpv_Outputs_capacity_factor_nget, self->data_ptr);
}

static PyObject *
Outputs_get_dc_loss_stowing_kwh(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Hcpv_Outputs_dc_loss_stowing_kwh_nget, self->data_ptr);
}

static PyObject *
Outputs_get_dc_nominal(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Hcpv_Outputs_dc_nominal_nget, self->data_ptr);
}

static PyObject *
Outputs_get_gen(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Hcpv_Outputs_gen_aget, self->data_ptr);
}

static PyObject *
Outputs_get_hourly_ac(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Hcpv_Outputs_hourly_ac_aget, self->data_ptr);
}

static PyObject *
Outputs_get_hourly_airmass(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Hcpv_Outputs_hourly_airmass_aget, self->data_ptr);
}

static PyObject *
Outputs_get_hourly_beam(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Hcpv_Outputs_hourly_beam_aget, self->data_ptr);
}

static PyObject *
Outputs_get_hourly_celleff(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Hcpv_Outputs_hourly_celleff_aget, self->data_ptr);
}

static PyObject *
Outputs_get_hourly_dc(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Hcpv_Outputs_hourly_dc_aget, self->data_ptr);
}

static PyObject *
Outputs_get_hourly_dc_net(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Hcpv_Outputs_hourly_dc_net_aget, self->data_ptr);
}

static PyObject *
Outputs_get_hourly_input_radiation(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Hcpv_Outputs_hourly_input_radiation_aget, self->data_ptr);
}

static PyObject *
Outputs_get_hourly_modeff(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Hcpv_Outputs_hourly_modeff_aget, self->data_ptr);
}

static PyObject *
Outputs_get_hourly_poa(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Hcpv_Outputs_hourly_poa_aget, self->data_ptr);
}

static PyObject *
Outputs_get_hourly_sazi(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Hcpv_Outputs_hourly_sazi_aget, self->data_ptr);
}

static PyObject *
Outputs_get_hourly_shading_derate(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Hcpv_Outputs_hourly_shading_derate_aget, self->data_ptr);
}

static PyObject *
Outputs_get_hourly_solazi(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Hcpv_Outputs_hourly_solazi_aget, self->data_ptr);
}

static PyObject *
Outputs_get_hourly_solzen(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Hcpv_Outputs_hourly_solzen_aget, self->data_ptr);
}

static PyObject *
Outputs_get_hourly_stilt(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Hcpv_Outputs_hourly_stilt_aget, self->data_ptr);
}

static PyObject *
Outputs_get_hourly_sunup(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Hcpv_Outputs_hourly_sunup_aget, self->data_ptr);
}

static PyObject *
Outputs_get_hourly_tcell(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Hcpv_Outputs_hourly_tcell_aget, self->data_ptr);
}

static PyObject *
Outputs_get_hourly_tdry(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Hcpv_Outputs_hourly_tdry_aget, self->data_ptr);
}

static PyObject *
Outputs_get_hourly_tmod(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Hcpv_Outputs_hourly_tmod_aget, self->data_ptr);
}

static PyObject *
Outputs_get_hourly_windspd(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Hcpv_Outputs_hourly_windspd_aget, self->data_ptr);
}

static PyObject *
Outputs_get_kwh_per_kw(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Hcpv_Outputs_kwh_per_kw_nget, self->data_ptr);
}

static PyObject *
Outputs_get_modeff_ref(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Hcpv_Outputs_modeff_ref_nget, self->data_ptr);
}

static PyObject *
Outputs_get_monthly_beam(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Hcpv_Outputs_monthly_beam_aget, self->data_ptr);
}

static PyObject *
Outputs_get_monthly_dc_net(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Hcpv_Outputs_monthly_dc_net_aget, self->data_ptr);
}

static PyObject *
Outputs_get_monthly_energy(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Hcpv_Outputs_monthly_energy_aget, self->data_ptr);
}

static PyObject *
Outputs_get_monthly_input_radiation(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Hcpv_Outputs_monthly_input_radiation_aget, self->data_ptr);
}

static PyObject *
Outputs_get_tracker_nameplate_watts(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Hcpv_Outputs_tracker_nameplate_watts_nget, self->data_ptr);
}

static PyGetSetDef Outputs_getset[] = {
{"ac_loss_tracker_kwh", (getter)Outputs_get_ac_loss_tracker_kwh,(setter)0,
	PyDoc_STR("*float*: Annual tracker power loss [kWh]"),
 	NULL},
{"annual_ac", (getter)Outputs_get_annual_ac,(setter)0,
	PyDoc_STR("*float*: AC gross [kWh]"),
 	NULL},
{"annual_beam", (getter)Outputs_get_annual_beam,(setter)0,
	PyDoc_STR("*float*: Beam irradiance [kW/m2]"),
 	NULL},
{"annual_dc", (getter)Outputs_get_annual_dc,(setter)0,
	PyDoc_STR("*float*: DC gross [kWh]"),
 	NULL},
{"annual_dc_net", (getter)Outputs_get_annual_dc_net,(setter)0,
	PyDoc_STR("*float*: DC net [kWh]"),
 	NULL},
{"annual_energy", (getter)Outputs_get_annual_energy,(setter)0,
	PyDoc_STR("*float*: Annual Energy [kWh]"),
 	NULL},
{"annual_input_radiation", (getter)Outputs_get_annual_input_radiation,(setter)0,
	PyDoc_STR("*float*: Input radiation [kWh]"),
 	NULL},
{"capacity_factor", (getter)Outputs_get_capacity_factor,(setter)0,
	PyDoc_STR("*float*: Capacity factor [%]"),
 	NULL},
{"dc_loss_stowing_kwh", (getter)Outputs_get_dc_loss_stowing_kwh,(setter)0,
	PyDoc_STR("*float*: Annual stowing power loss [kWh]"),
 	NULL},
{"dc_nominal", (getter)Outputs_get_dc_nominal,(setter)0,
	PyDoc_STR("*float*: Annual DC nominal [kWh]"),
 	NULL},
{"gen", (getter)Outputs_get_gen,(setter)0,
	PyDoc_STR("*sequence*: System power generated [kW]"),
 	NULL},
{"hourly_ac", (getter)Outputs_get_hourly_ac,(setter)0,
	PyDoc_STR("*sequence*: AC gross [kWh]"),
 	NULL},
{"hourly_airmass", (getter)Outputs_get_hourly_airmass,(setter)0,
	PyDoc_STR("*sequence*: Relative air mass [none]"),
 	NULL},
{"hourly_beam", (getter)Outputs_get_hourly_beam,(setter)0,
	PyDoc_STR("*sequence*: Beam irradiance [kW/m2]"),
 	NULL},
{"hourly_celleff", (getter)Outputs_get_hourly_celleff,(setter)0,
	PyDoc_STR("*sequence*: Cell efficiency [%]"),
 	NULL},
{"hourly_dc", (getter)Outputs_get_hourly_dc,(setter)0,
	PyDoc_STR("*sequence*: DC gross [kWh]"),
 	NULL},
{"hourly_dc_net", (getter)Outputs_get_hourly_dc_net,(setter)0,
	PyDoc_STR("*sequence*: DC net [kWh]"),
 	NULL},
{"hourly_input_radiation", (getter)Outputs_get_hourly_input_radiation,(setter)0,
	PyDoc_STR("*sequence*: Input radiation [kWh]"),
 	NULL},
{"hourly_modeff", (getter)Outputs_get_hourly_modeff,(setter)0,
	PyDoc_STR("*sequence*: Module efficiency [%]"),
 	NULL},
{"hourly_poa", (getter)Outputs_get_hourly_poa,(setter)0,
	PyDoc_STR("*sequence*: POA on cell [W/m2]"),
 	NULL},
{"hourly_sazi", (getter)Outputs_get_hourly_sazi,(setter)0,
	PyDoc_STR("*sequence*: Tracker azimuth [deg]"),
 	NULL},
{"hourly_shading_derate", (getter)Outputs_get_hourly_shading_derate,(setter)0,
	PyDoc_STR("*sequence*: Shading derate [none]"),
 	NULL},
{"hourly_solazi", (getter)Outputs_get_hourly_solazi,(setter)0,
	PyDoc_STR("*sequence*: Hourly solar azimuth [deg]"),
 	NULL},
{"hourly_solzen", (getter)Outputs_get_hourly_solzen,(setter)0,
	PyDoc_STR("*sequence*: Hourly solar zenith [deg]"),
 	NULL},
{"hourly_stilt", (getter)Outputs_get_hourly_stilt,(setter)0,
	PyDoc_STR("*sequence*: Tracker tilt [deg]"),
 	NULL},
{"hourly_sunup", (getter)Outputs_get_hourly_sunup,(setter)0,
	PyDoc_STR("*sequence*: Sun up? (0/1) [0 or 1]"),
 	NULL},
{"hourly_tcell", (getter)Outputs_get_hourly_tcell,(setter)0,
	PyDoc_STR("*sequence*: Cell temperature [C]"),
 	NULL},
{"hourly_tdry", (getter)Outputs_get_hourly_tdry,(setter)0,
	PyDoc_STR("*sequence*: Ambient dry bulb temperature [C]"),
 	NULL},
{"hourly_tmod", (getter)Outputs_get_hourly_tmod,(setter)0,
	PyDoc_STR("*sequence*: Module backplate temp [C]"),
 	NULL},
{"hourly_windspd", (getter)Outputs_get_hourly_windspd,(setter)0,
	PyDoc_STR("*sequence*: Wind speed [m/s]"),
 	NULL},
{"kwh_per_kw", (getter)Outputs_get_kwh_per_kw,(setter)0,
	PyDoc_STR("*float*: First year kWh/kW [kWh/kW]"),
 	NULL},
{"modeff_ref", (getter)Outputs_get_modeff_ref,(setter)0,
	PyDoc_STR("*float*: Module efficiency [-]"),
 	NULL},
{"monthly_beam", (getter)Outputs_get_monthly_beam,(setter)0,
	PyDoc_STR("*sequence*: Beam irradiance [kW/m2]"),
 	NULL},
{"monthly_dc_net", (getter)Outputs_get_monthly_dc_net,(setter)0,
	PyDoc_STR("*sequence*: DC net [kWh]"),
 	NULL},
{"monthly_energy", (getter)Outputs_get_monthly_energy,(setter)0,
	PyDoc_STR("*sequence*: Monthly Energy [kWh]"),
 	NULL},
{"monthly_input_radiation", (getter)Outputs_get_monthly_input_radiation,(setter)0,
	PyDoc_STR("*sequence*: Input radiation [kWh]"),
 	NULL},
{"tracker_nameplate_watts", (getter)Outputs_get_tracker_nameplate_watts,(setter)0,
	PyDoc_STR("*float*: Tracker nameplate [watts]"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject Outputs_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Hcpv.Outputs",             /*tp_name*/
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
 * Hcpv
 */

typedef struct {
	PyObject_HEAD
	PyObject            *x_attr;        /* Attributes dictionary */
	SAM_Hcpv   data_ptr;
} HcpvObject;

static PyTypeObject Hcpv_Type;

#define HcpvObject_Check(v)      (Py_TYPE(v) == &Hcpv_Type)

static HcpvObject *
newHcpvObject(void* data_ptr)
{
	HcpvObject *self;
	self = PyObject_New(HcpvObject, &Hcpv_Type);

	PySAM_TECH_ATTR("Hcpv", SAM_Hcpv_construct)

	PyObject* SolarResourceData_obj = SolarResourceData_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "SolarResourceData", SolarResourceData_obj);
	Py_DECREF(SolarResourceData_obj);

	PyObject* PVWatts_obj = PVWatts_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "PVWatts", PVWatts_obj);
	Py_DECREF(PVWatts_obj);

	PyObject* HCPVModule_obj = HCPVModule_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "HCPVModule", HCPVModule_obj);
	Py_DECREF(HCPVModule_obj);

	PyObject* InverterCECDatabase_obj = InverterCECDatabase_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "InverterCECDatabase", InverterCECDatabase_obj);
	Py_DECREF(InverterCECDatabase_obj);

	PyObject* HCPVArray_obj = HCPVArray_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "HCPVArray", HCPVArray_obj);
	Py_DECREF(HCPVArray_obj);

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

/* Hcpv methods */

static void
Hcpv_dealloc(HcpvObject *self)
{
	Py_XDECREF(self->x_attr);
	SAM_Hcpv_destruct(self->data_ptr);
	PyObject_Del(self);
}


static PyObject *
Hcpv_execute(HcpvObject *self, PyObject *args)
{
	int verbosity = 0;

	if (!PyArg_ParseTuple(args, "|i", &verbosity))
		return NULL;

	SAM_error error = new_error();
	SAM_Hcpv_execute(self->data_ptr, verbosity, &error);
	if (PySAM_has_error(error )) return NULL;

	Py_INCREF(Py_None);
	return Py_None;
}


static PyObject *
Hcpv_assign(HcpvObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_nested_dict((PyObject*)self, self->x_attr, self->data_ptr, dict, "Hcpv"))
		return NULL;

	Py_INCREF(Py_None);
	return Py_None;
}


static PyObject *
Hcpv_export(HcpvObject *self, PyObject *args)
{
	return PySAM_export_to_nested_dict((PyObject *) self, self->x_attr);
}

static PyMethodDef Hcpv_methods[] = {
		{"execute",            (PyCFunction)Hcpv_execute,  METH_VARARGS,
				PyDoc_STR("execute(int verbosity) -> None\n Execute simulation with verbosity level 0 (default) or 1")},
		{"assign",            (PyCFunction)Hcpv_assign,  METH_VARARGS,
				PyDoc_STR("assign(dict) -> None\n Assign attributes from nested dictionary, except for Outputs\n\n``nested_dict = { 'SolarResourceData': { var: val, ...}, ...}``")},
		{"export",            (PyCFunction)Hcpv_export,  METH_VARARGS,
				PyDoc_STR("export() -> dict\n Export attributes into nested dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
Hcpv_getattro(HcpvObject *self, PyObject *name)
{
	return PySAM_get_attr((PyObject*) self, (PyObject*) self->x_attr, name);
}

static int
Hcpv_setattr(HcpvObject *self, const char *name, PyObject *v)
{
	return PySAM_set_attr((PyObject*)self, (PyObject*)self->x_attr, name, v);
}

static PyTypeObject Hcpv_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Hcpv",            /*tp_name*/
		sizeof(HcpvObject),/*tp_basicsize*/
		0,                          /*tp_itemsize*/
		/* methods */
		(destructor)Hcpv_dealloc,    /*tp_dealloc*/
		0,                          /*tp_print*/
		(getattrfunc)0,             /*tp_getattr*/
		(setattrfunc)Hcpv_setattr,   /*tp_setattr*/
		0,                          /*tp_reserved*/
		0,                          /*tp_repr*/
		0,                          /*tp_as_number*/
		0,                          /*tp_as_sequence*/
		0,                          /*tp_as_mapping*/
		0,                          /*tp_hash*/
		0,                          /*tp_call*/
		0,                          /*tp_str*/
		(getattrofunc)Hcpv_getattro, /*tp_getattro*/
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
		Hcpv_methods,      /*tp_methods*/
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


/* Function of no arguments returning new Hcpv object */

static PyObject *
Hcpv_new(PyObject *self, PyObject *args)
{
	HcpvObject *rv;
	rv = newHcpvObject(0);
	if (rv == NULL)
		return NULL;
	return (PyObject *)rv;
}

static PyObject *
Hcpv_wrap(PyObject *self, PyObject *args)
{
	HcpvObject *rv;
	long long int ptr = 0;  // 64 bit arch
	if (!PyArg_ParseTuple(args, "L:wrap", &ptr)){
		PyErr_BadArgument();
		return NULL;
	}
	rv = newHcpvObject((void*)ptr);
	if (rv == NULL)
		return NULL;
	return (PyObject *)rv;
}

static PyObject *
Hcpv_default(PyObject *self, PyObject *args)
{
	HcpvObject *rv;
	char* def = 0;
	if (!PyArg_ParseTuple(args, "s:default", &def)){
		PyErr_BadArgument();
		return NULL;
	}
	rv = newHcpvObject(0);
	if (rv == NULL)
		return NULL;

	PySAM_load_defaults((PyObject*)rv, rv->x_attr, rv->data_ptr, "Hcpv", def);

	return (PyObject *)rv;
}

/* ---------- */


/* List of functions defined in the module */

static PyMethodDef HcpvModule_methods[] = {
		{"new",             Hcpv_new,         METH_VARARGS,
				PyDoc_STR("new() -> Hcpv")},
		{"default",             Hcpv_default,         METH_VARARGS,
				PyDoc_STR("default(config) -> Hcpv\n\nUse financial model-specific default attributes\n"
				"config options:\n\n- \"HighXConcentratingPVAllEquityPartnershipFlip\"\n- \"HighXConcentratingPVIndependentPowerProducer\"\n- \"HighXConcentratingPVLCOECalculator\"\n- \"HighXConcentratingPVLeveragedPartnershipFlip\"\n- \"HighXConcentratingPVNone\"\n- \"HighXConcentratingPVSaleLeaseback\"\n- \"HighXConcentratingPVSingleOwner\"")},
		{"wrap",             Hcpv_wrap,         METH_VARARGS,
				PyDoc_STR("wrap(ssc_data_t) -> Hcpv\n\nUse existing PySSC data\n\n.. warning::\n\n	Do not call PySSC.data_free on the ssc_data_t provided to ``wrap``")},
		{NULL,              NULL}           /* sentinel */
};

PyDoc_STRVAR(module_doc,
			 "Concentrating photovoltaic system with a high concentration photovoltaic module model and separate inverter model");


static int
HcpvModule_exec(PyObject *m)
{
	/* Finalize the type object including setting type of the new type
	 * object; doing it here is required for portability, too. */

	if (PySAM_load_lib(m) < 0) goto fail;
	if (PySAM_init_error(m) < 0) goto fail;

	Hcpv_Type.tp_dict = PyDict_New();
	if (!Hcpv_Type.tp_dict) { goto fail; }

	/// Add the AdjustmentFactors type object to Hcpv_Type
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
	PyDict_SetItemString(Hcpv_Type.tp_dict,
						 "AdjustmentFactors",
						 (PyObject*)AdjustmentFactors_Type);
	Py_DECREF(&AdjustmentFactors_Type);
	Py_XDECREF(AdjustmentFactors_Type);

	/// Add the SolarResourceData type object to Hcpv_Type
	if (PyType_Ready(&SolarResourceData_Type) < 0) { goto fail; }
	PyDict_SetItemString(Hcpv_Type.tp_dict,
				"SolarResourceData",
				(PyObject*)&SolarResourceData_Type);
	Py_DECREF(&SolarResourceData_Type);

	/// Add the PVWatts type object to Hcpv_Type
	if (PyType_Ready(&PVWatts_Type) < 0) { goto fail; }
	PyDict_SetItemString(Hcpv_Type.tp_dict,
				"PVWatts",
				(PyObject*)&PVWatts_Type);
	Py_DECREF(&PVWatts_Type);

	/// Add the HCPVModule type object to Hcpv_Type
	if (PyType_Ready(&HCPVModule_Type) < 0) { goto fail; }
	PyDict_SetItemString(Hcpv_Type.tp_dict,
				"HCPVModule",
				(PyObject*)&HCPVModule_Type);
	Py_DECREF(&HCPVModule_Type);

	/// Add the InverterCECDatabase type object to Hcpv_Type
	if (PyType_Ready(&InverterCECDatabase_Type) < 0) { goto fail; }
	PyDict_SetItemString(Hcpv_Type.tp_dict,
				"InverterCECDatabase",
				(PyObject*)&InverterCECDatabase_Type);
	Py_DECREF(&InverterCECDatabase_Type);

	/// Add the HCPVArray type object to Hcpv_Type
	if (PyType_Ready(&HCPVArray_Type) < 0) { goto fail; }
	PyDict_SetItemString(Hcpv_Type.tp_dict,
				"HCPVArray",
				(PyObject*)&HCPVArray_Type);
	Py_DECREF(&HCPVArray_Type);

	/// Add the Outputs type object to Hcpv_Type
	if (PyType_Ready(&Outputs_Type) < 0) { goto fail; }
	PyDict_SetItemString(Hcpv_Type.tp_dict,
				"Outputs",
				(PyObject*)&Outputs_Type);
	Py_DECREF(&Outputs_Type);

	/// Add the Hcpv type object to the module
	if (PyType_Ready(&Hcpv_Type) < 0) { goto fail; }
	PyModule_AddObject(m,
				"Hcpv",
				(PyObject*)&Hcpv_Type);

	return 0;
	fail:
	Py_XDECREF(m);
	return -1;
}

static struct PyModuleDef_Slot HcpvModule_slots[] = {
		{Py_mod_exec, HcpvModule_exec},
		{0, NULL},
};

static struct PyModuleDef HcpvModule = {
		PyModuleDef_HEAD_INIT,
		"Hcpv",
		module_doc,
		0,
		HcpvModule_methods,
		HcpvModule_slots,
		NULL,
		NULL,
		NULL
};

/* Export function for the module */

PyMODINIT_FUNC
PyInit_Hcpv(void)
{
	return PyModuleDef_Init(&HcpvModule);
}