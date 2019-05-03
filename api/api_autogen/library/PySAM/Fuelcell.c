#include <Python.h>

#include <SAM_Fuelcell.h>
#include <SAM_api.h>

#include "PySAM_utils.h"



	/*
	 * Common Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Fuelcell   data_ptr;
} CommonObject;

static PyTypeObject Common_Type;

static PyObject *
Common_new(SAM_Fuelcell data_ptr)
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

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Fuelcell", "Common")){
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
Common_get_annual_energy(CommonObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Fuelcell_Common_annual_energy_nget, self->data_ptr);
}

static int
Common_set_annual_energy(CommonObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Fuelcell_Common_annual_energy_nset, self->data_ptr);
}

static PyObject *
Common_get_capacity_factor(CommonObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Fuelcell_Common_capacity_factor_nget, self->data_ptr);
}

static int
Common_set_capacity_factor(CommonObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Fuelcell_Common_capacity_factor_nset, self->data_ptr);
}

static PyObject *
Common_get_gen(CommonObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Fuelcell_Common_gen_aget, self->data_ptr);
}

static int
Common_set_gen(CommonObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Fuelcell_Common_gen_aset, self->data_ptr);
}

static PyObject *
Common_get_percent_complete(CommonObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Fuelcell_Common_percent_complete_nget, self->data_ptr);
}

static int
Common_set_percent_complete(CommonObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Fuelcell_Common_percent_complete_nset, self->data_ptr);
}

static PyGetSetDef Common_getset[] = {
{"annual_energy", (getter)Common_get_annual_energy,(setter)Common_set_annual_energy,
	PyDoc_STR("*float*: Annual Energy [kWh]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"capacity_factor", (getter)Common_get_capacity_factor,(setter)Common_set_capacity_factor,
	PyDoc_STR("*float*: Capacity factor [%]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"gen", (getter)Common_get_gen,(setter)Common_set_gen,
	PyDoc_STR("*sequence*: System power generated [kW]\n\n*Info*: Lifetime system generation"),
 	NULL},
{"percent_complete", (getter)Common_get_percent_complete,(setter)Common_set_percent_complete,
	PyDoc_STR("*float*: Estimated simulation status [%]"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject Common_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Fuelcell.Common",             /*tp_name*/
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
	 * Lifetime Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Fuelcell   data_ptr;
} LifetimeObject;

static PyTypeObject Lifetime_Type;

static PyObject *
Lifetime_new(SAM_Fuelcell data_ptr)
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

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Fuelcell", "Lifetime")){
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
Lifetime_get_analysis_period(LifetimeObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Fuelcell_Lifetime_analysis_period_nget, self->data_ptr);
}

static int
Lifetime_set_analysis_period(LifetimeObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Fuelcell_Lifetime_analysis_period_nset, self->data_ptr);
}

static PyObject *
Lifetime_get_system_use_lifetime_output(LifetimeObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Fuelcell_Lifetime_system_use_lifetime_output_nget, self->data_ptr);
}

static int
Lifetime_set_system_use_lifetime_output(LifetimeObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Fuelcell_Lifetime_system_use_lifetime_output_nset, self->data_ptr);
}

static PyGetSetDef Lifetime_getset[] = {
{"analysis_period", (getter)Lifetime_get_analysis_period,(setter)Lifetime_set_analysis_period,
	PyDoc_STR("*float*: Lifetime analysis period [years]\n\n*Info*: The number of years in the simulation\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"system_use_lifetime_output", (getter)Lifetime_get_system_use_lifetime_output,(setter)Lifetime_set_system_use_lifetime_output,
	PyDoc_STR("*float*: Lifetime simulation [0/1]\n\n*Options*: 0=SingleYearRepeated,1=RunEveryYear\n\n*Constraints*: BOOLEAN\n\n*Required*: set to 0 if not provided."),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject Lifetime_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Fuelcell.Lifetime",             /*tp_name*/
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
	 * Load Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Fuelcell   data_ptr;
} LoadObject;

static PyTypeObject Load_Type;

static PyObject *
Load_new(SAM_Fuelcell data_ptr)
{
	PyObject* new_obj = Load_Type.tp_alloc(&Load_Type,0);

	LoadObject* Load_obj = (LoadObject*)new_obj;

	Load_obj->data_ptr = data_ptr;

	return new_obj;
}

/* Load methods */

static PyObject *
Load_assign(LoadObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Fuelcell", "Load")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
Load_export(LoadObject *self, PyObject *args)
{
	PyTypeObject* tp = &Load_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef Load_methods[] = {
		{"assign",            (PyCFunction)Load_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``Load_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)Load_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
Load_get_load(LoadObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Fuelcell_Load_load_aget, self->data_ptr);
}

static int
Load_set_load(LoadObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Fuelcell_Load_load_aset, self->data_ptr);
}

static PyGetSetDef Load_getset[] = {
{"load", (getter)Load_get_load,(setter)Load_set_load,
	PyDoc_STR("*sequence*: Electricity load (year 1) [kW]"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject Load_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Fuelcell.Load",             /*tp_name*/
		sizeof(LoadObject),          /*tp_basicsize*/
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
		Load_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		Load_getset,          /*tp_getset*/
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
	SAM_Fuelcell   data_ptr;
} FuelCellObject;

static PyTypeObject FuelCell_Type;

static PyObject *
FuelCell_new(SAM_Fuelcell data_ptr)
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

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Fuelcell", "FuelCell")){
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
	return PySAM_array_getter(SAM_Fuelcell_FuelCell_dispatch_manual_fuelcellcharge_aget, self->data_ptr);
}

static int
FuelCell_set_dispatch_manual_fuelcellcharge(FuelCellObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Fuelcell_FuelCell_dispatch_manual_fuelcellcharge_aset, self->data_ptr);
}

static PyObject *
FuelCell_get_dispatch_manual_fuelcelldischarge(FuelCellObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Fuelcell_FuelCell_dispatch_manual_fuelcelldischarge_aget, self->data_ptr);
}

static int
FuelCell_set_dispatch_manual_fuelcelldischarge(FuelCellObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Fuelcell_FuelCell_dispatch_manual_fuelcelldischarge_aset, self->data_ptr);
}

static PyObject *
FuelCell_get_dispatch_manual_percent_fc_discharge(FuelCellObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Fuelcell_FuelCell_dispatch_manual_percent_fc_discharge_aget, self->data_ptr);
}

static int
FuelCell_set_dispatch_manual_percent_fc_discharge(FuelCellObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Fuelcell_FuelCell_dispatch_manual_percent_fc_discharge_aset, self->data_ptr);
}

static PyObject *
FuelCell_get_dispatch_manual_sched(FuelCellObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Fuelcell_FuelCell_dispatch_manual_sched_mget, self->data_ptr);
}

static int
FuelCell_set_dispatch_manual_sched(FuelCellObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_Fuelcell_FuelCell_dispatch_manual_sched_mset, self->data_ptr);
}

static PyObject *
FuelCell_get_dispatch_manual_sched_weekend(FuelCellObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Fuelcell_FuelCell_dispatch_manual_sched_weekend_mget, self->data_ptr);
}

static int
FuelCell_set_dispatch_manual_sched_weekend(FuelCellObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_Fuelcell_FuelCell_dispatch_manual_sched_weekend_mset, self->data_ptr);
}

static PyObject *
FuelCell_get_dispatch_manual_units_fc_discharge(FuelCellObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Fuelcell_FuelCell_dispatch_manual_units_fc_discharge_aget, self->data_ptr);
}

static int
FuelCell_set_dispatch_manual_units_fc_discharge(FuelCellObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Fuelcell_FuelCell_dispatch_manual_units_fc_discharge_aset, self->data_ptr);
}

static PyObject *
FuelCell_get_fuelcell_availability_schedule(FuelCellObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Fuelcell_FuelCell_fuelcell_availability_schedule_mget, self->data_ptr);
}

static int
FuelCell_set_fuelcell_availability_schedule(FuelCellObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_Fuelcell_FuelCell_fuelcell_availability_schedule_mset, self->data_ptr);
}

static PyObject *
FuelCell_get_fuelcell_degradation(FuelCellObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Fuelcell_FuelCell_fuelcell_degradation_nget, self->data_ptr);
}

static int
FuelCell_set_fuelcell_degradation(FuelCellObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Fuelcell_FuelCell_fuelcell_degradation_nset, self->data_ptr);
}

static PyObject *
FuelCell_get_fuelcell_degradation_restart(FuelCellObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Fuelcell_FuelCell_fuelcell_degradation_restart_nget, self->data_ptr);
}

static int
FuelCell_set_fuelcell_degradation_restart(FuelCellObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Fuelcell_FuelCell_fuelcell_degradation_restart_nset, self->data_ptr);
}

static PyObject *
FuelCell_get_fuelcell_degradation_restart_schedule(FuelCellObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Fuelcell_FuelCell_fuelcell_degradation_restart_schedule_nget, self->data_ptr);
}

static int
FuelCell_set_fuelcell_degradation_restart_schedule(FuelCellObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Fuelcell_FuelCell_fuelcell_degradation_restart_schedule_nset, self->data_ptr);
}

static PyObject *
FuelCell_get_fuelcell_degradation_restarts_per_year(FuelCellObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Fuelcell_FuelCell_fuelcell_degradation_restarts_per_year_nget, self->data_ptr);
}

static int
FuelCell_set_fuelcell_degradation_restarts_per_year(FuelCellObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Fuelcell_FuelCell_fuelcell_degradation_restarts_per_year_nset, self->data_ptr);
}

static PyObject *
FuelCell_get_fuelcell_dispatch(FuelCellObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Fuelcell_FuelCell_fuelcell_dispatch_aget, self->data_ptr);
}

static int
FuelCell_set_fuelcell_dispatch(FuelCellObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Fuelcell_FuelCell_fuelcell_dispatch_aset, self->data_ptr);
}

static PyObject *
FuelCell_get_fuelcell_dispatch_choice(FuelCellObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Fuelcell_FuelCell_fuelcell_dispatch_choice_nget, self->data_ptr);
}

static int
FuelCell_set_fuelcell_dispatch_choice(FuelCellObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Fuelcell_FuelCell_fuelcell_dispatch_choice_nset, self->data_ptr);
}

static PyObject *
FuelCell_get_fuelcell_dynamic_response_down(FuelCellObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Fuelcell_FuelCell_fuelcell_dynamic_response_down_nget, self->data_ptr);
}

static int
FuelCell_set_fuelcell_dynamic_response_down(FuelCellObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Fuelcell_FuelCell_fuelcell_dynamic_response_down_nset, self->data_ptr);
}

static PyObject *
FuelCell_get_fuelcell_dynamic_response_up(FuelCellObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Fuelcell_FuelCell_fuelcell_dynamic_response_up_nget, self->data_ptr);
}

static int
FuelCell_set_fuelcell_dynamic_response_up(FuelCellObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Fuelcell_FuelCell_fuelcell_dynamic_response_up_nset, self->data_ptr);
}

static PyObject *
FuelCell_get_fuelcell_efficiency(FuelCellObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Fuelcell_FuelCell_fuelcell_efficiency_mget, self->data_ptr);
}

static int
FuelCell_set_fuelcell_efficiency(FuelCellObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_Fuelcell_FuelCell_fuelcell_efficiency_mset, self->data_ptr);
}

static PyObject *
FuelCell_get_fuelcell_efficiency_choice(FuelCellObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Fuelcell_FuelCell_fuelcell_efficiency_choice_nget, self->data_ptr);
}

static int
FuelCell_set_fuelcell_efficiency_choice(FuelCellObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Fuelcell_FuelCell_fuelcell_efficiency_choice_nset, self->data_ptr);
}

static PyObject *
FuelCell_get_fuelcell_fixed_pct(FuelCellObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Fuelcell_FuelCell_fuelcell_fixed_pct_nget, self->data_ptr);
}

static int
FuelCell_set_fuelcell_fixed_pct(FuelCellObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Fuelcell_FuelCell_fuelcell_fixed_pct_nset, self->data_ptr);
}

static PyObject *
FuelCell_get_fuelcell_fuel_available(FuelCellObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Fuelcell_FuelCell_fuelcell_fuel_available_nget, self->data_ptr);
}

static int
FuelCell_set_fuelcell_fuel_available(FuelCellObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Fuelcell_FuelCell_fuelcell_fuel_available_nset, self->data_ptr);
}

static PyObject *
FuelCell_get_fuelcell_fuel_price(FuelCellObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Fuelcell_FuelCell_fuelcell_fuel_price_nget, self->data_ptr);
}

static int
FuelCell_set_fuelcell_fuel_price(FuelCellObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Fuelcell_FuelCell_fuelcell_fuel_price_nset, self->data_ptr);
}

static PyObject *
FuelCell_get_fuelcell_fuel_type(FuelCellObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Fuelcell_FuelCell_fuelcell_fuel_type_nget, self->data_ptr);
}

static int
FuelCell_set_fuelcell_fuel_type(FuelCellObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Fuelcell_FuelCell_fuelcell_fuel_type_nset, self->data_ptr);
}

static PyObject *
FuelCell_get_fuelcell_lhv(FuelCellObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Fuelcell_FuelCell_fuelcell_lhv_nget, self->data_ptr);
}

static int
FuelCell_set_fuelcell_lhv(FuelCellObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Fuelcell_FuelCell_fuelcell_lhv_nset, self->data_ptr);
}

static PyObject *
FuelCell_get_fuelcell_number_of_units(FuelCellObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Fuelcell_FuelCell_fuelcell_number_of_units_nget, self->data_ptr);
}

static int
FuelCell_set_fuelcell_number_of_units(FuelCellObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Fuelcell_FuelCell_fuelcell_number_of_units_nset, self->data_ptr);
}

static PyObject *
FuelCell_get_fuelcell_operation_options(FuelCellObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Fuelcell_FuelCell_fuelcell_operation_options_nget, self->data_ptr);
}

static int
FuelCell_set_fuelcell_operation_options(FuelCellObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Fuelcell_FuelCell_fuelcell_operation_options_nset, self->data_ptr);
}

static PyObject *
FuelCell_get_fuelcell_replacement_option(FuelCellObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Fuelcell_FuelCell_fuelcell_replacement_option_nget, self->data_ptr);
}

static int
FuelCell_set_fuelcell_replacement_option(FuelCellObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Fuelcell_FuelCell_fuelcell_replacement_option_nset, self->data_ptr);
}

static PyObject *
FuelCell_get_fuelcell_replacement_percent(FuelCellObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Fuelcell_FuelCell_fuelcell_replacement_percent_nget, self->data_ptr);
}

static int
FuelCell_set_fuelcell_replacement_percent(FuelCellObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Fuelcell_FuelCell_fuelcell_replacement_percent_nset, self->data_ptr);
}

static PyObject *
FuelCell_get_fuelcell_replacement_schedule(FuelCellObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Fuelcell_FuelCell_fuelcell_replacement_schedule_aget, self->data_ptr);
}

static int
FuelCell_set_fuelcell_replacement_schedule(FuelCellObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Fuelcell_FuelCell_fuelcell_replacement_schedule_aset, self->data_ptr);
}

static PyObject *
FuelCell_get_fuelcell_shutdown_time(FuelCellObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Fuelcell_FuelCell_fuelcell_shutdown_time_nget, self->data_ptr);
}

static int
FuelCell_set_fuelcell_shutdown_time(FuelCellObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Fuelcell_FuelCell_fuelcell_shutdown_time_nset, self->data_ptr);
}

static PyObject *
FuelCell_get_fuelcell_startup_time(FuelCellObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Fuelcell_FuelCell_fuelcell_startup_time_nget, self->data_ptr);
}

static int
FuelCell_set_fuelcell_startup_time(FuelCellObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Fuelcell_FuelCell_fuelcell_startup_time_nset, self->data_ptr);
}

static PyObject *
FuelCell_get_fuelcell_type(FuelCellObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Fuelcell_FuelCell_fuelcell_type_nget, self->data_ptr);
}

static int
FuelCell_set_fuelcell_type(FuelCellObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Fuelcell_FuelCell_fuelcell_type_nset, self->data_ptr);
}

static PyObject *
FuelCell_get_fuelcell_unit_max_power(FuelCellObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Fuelcell_FuelCell_fuelcell_unit_max_power_nget, self->data_ptr);
}

static int
FuelCell_set_fuelcell_unit_max_power(FuelCellObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Fuelcell_FuelCell_fuelcell_unit_max_power_nset, self->data_ptr);
}

static PyObject *
FuelCell_get_fuelcell_unit_min_power(FuelCellObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Fuelcell_FuelCell_fuelcell_unit_min_power_nget, self->data_ptr);
}

static int
FuelCell_set_fuelcell_unit_min_power(FuelCellObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Fuelcell_FuelCell_fuelcell_unit_min_power_nset, self->data_ptr);
}

static PyGetSetDef FuelCell_getset[] = {
{"dispatch_manual_fuelcellcharge", (getter)FuelCell_get_dispatch_manual_fuelcellcharge,(setter)FuelCell_set_dispatch_manual_fuelcellcharge,
	PyDoc_STR("*sequence*: Periods 1-6 charging allowed?"),
 	NULL},
{"dispatch_manual_fuelcelldischarge", (getter)FuelCell_get_dispatch_manual_fuelcelldischarge,(setter)FuelCell_set_dispatch_manual_fuelcelldischarge,
	PyDoc_STR("*sequence*: Periods 1-6 discharging allowed?"),
 	NULL},
{"dispatch_manual_percent_fc_discharge", (getter)FuelCell_get_dispatch_manual_percent_fc_discharge,(setter)FuelCell_set_dispatch_manual_percent_fc_discharge,
	PyDoc_STR("*sequence*: Periods 1-6 percent of max fuelcell output"),
 	NULL},
{"dispatch_manual_sched", (getter)FuelCell_get_dispatch_manual_sched,(setter)FuelCell_set_dispatch_manual_sched,
	PyDoc_STR("*sequence[sequence]*: Dispatch schedule for weekday"),
 	NULL},
{"dispatch_manual_sched_weekend", (getter)FuelCell_get_dispatch_manual_sched_weekend,(setter)FuelCell_set_dispatch_manual_sched_weekend,
	PyDoc_STR("*sequence[sequence]*: Dispatch schedule for weekend"),
 	NULL},
{"dispatch_manual_units_fc_discharge", (getter)FuelCell_get_dispatch_manual_units_fc_discharge,(setter)FuelCell_set_dispatch_manual_units_fc_discharge,
	PyDoc_STR("*sequence*: Periods 1-6 number of fuel cell units?"),
 	NULL},
{"fuelcell_availability_schedule", (getter)FuelCell_get_fuelcell_availability_schedule,(setter)FuelCell_set_fuelcell_availability_schedule,
	PyDoc_STR("*sequence[sequence]*: Fuel cell availability schedule  [Column 1: Hour of year start shutdown/Column 2: Hours duration of shutdown ]"),
 	NULL},
{"fuelcell_degradation", (getter)FuelCell_get_fuelcell_degradation,(setter)FuelCell_set_fuelcell_degradation,
	PyDoc_STR("*float*: Fuel cell degradation per hour [kW/h]"),
 	NULL},
{"fuelcell_degradation_restart", (getter)FuelCell_get_fuelcell_degradation_restart,(setter)FuelCell_set_fuelcell_degradation_restart,
	PyDoc_STR("*float*: Fuel cell degradation at restart [kW]"),
 	NULL},
{"fuelcell_degradation_restart_schedule", (getter)FuelCell_get_fuelcell_degradation_restart_schedule,(setter)FuelCell_set_fuelcell_degradation_restart_schedule,
	PyDoc_STR("*float*: Fuel cell enable scheduled restarts [0/1]"),
 	NULL},
{"fuelcell_degradation_restarts_per_year", (getter)FuelCell_get_fuelcell_degradation_restarts_per_year,(setter)FuelCell_set_fuelcell_degradation_restarts_per_year,
	PyDoc_STR("*float*: Fuel cell scheduled restarts per year"),
 	NULL},
{"fuelcell_dispatch", (getter)FuelCell_get_fuelcell_dispatch,(setter)FuelCell_set_fuelcell_dispatch,
	PyDoc_STR("*sequence*: Fuel cell dispatch input per unit [kW]"),
 	NULL},
{"fuelcell_dispatch_choice", (getter)FuelCell_get_fuelcell_dispatch_choice,(setter)FuelCell_set_fuelcell_dispatch_choice,
	PyDoc_STR("*float*: Fuel cell dispatch choice [0/1/2]"),
 	NULL},
{"fuelcell_dynamic_response_down", (getter)FuelCell_get_fuelcell_dynamic_response_down,(setter)FuelCell_set_fuelcell_dynamic_response_down,
	PyDoc_STR("*float*: Fuel cell ramp rate limit down [kW/h]"),
 	NULL},
{"fuelcell_dynamic_response_up", (getter)FuelCell_get_fuelcell_dynamic_response_up,(setter)FuelCell_set_fuelcell_dynamic_response_up,
	PyDoc_STR("*float*: Fuel cell ramp rate limit up [kW/h]"),
 	NULL},
{"fuelcell_efficiency", (getter)FuelCell_get_fuelcell_efficiency,(setter)FuelCell_set_fuelcell_efficiency,
	PyDoc_STR("*sequence[sequence]*: Fuel cell efficiency table "),
 	NULL},
{"fuelcell_efficiency_choice", (getter)FuelCell_get_fuelcell_efficiency_choice,(setter)FuelCell_set_fuelcell_efficiency_choice,
	PyDoc_STR("*float*: Fuel cell efficiency definition choice  [0/1]\n\n*Options*: 0=OriginalNameplate,1=DegradedNameplate"),
 	NULL},
{"fuelcell_fixed_pct", (getter)FuelCell_get_fuelcell_fixed_pct,(setter)FuelCell_set_fuelcell_fixed_pct,
	PyDoc_STR("*float*: Fuel cell fixed operation percent [%]"),
 	NULL},
{"fuelcell_fuel_available", (getter)FuelCell_get_fuelcell_fuel_available,(setter)FuelCell_set_fuelcell_fuel_available,
	PyDoc_STR("*float*: Fuel cell available fuel quantity [MCf]"),
 	NULL},
{"fuelcell_fuel_price", (getter)FuelCell_get_fuelcell_fuel_price,(setter)FuelCell_set_fuelcell_fuel_price,
	PyDoc_STR("*float*: Fuel cell price [$/MCf]"),
 	NULL},
{"fuelcell_fuel_type", (getter)FuelCell_get_fuelcell_fuel_type,(setter)FuelCell_set_fuelcell_fuel_type,
	PyDoc_STR("*float*: Fuel cell type [0/1]"),
 	NULL},
{"fuelcell_lhv", (getter)FuelCell_get_fuelcell_lhv,(setter)FuelCell_set_fuelcell_lhv,
	PyDoc_STR("*float*: Fuel cell lower heating value [Btu/ft3]"),
 	NULL},
{"fuelcell_number_of_units", (getter)FuelCell_get_fuelcell_number_of_units,(setter)FuelCell_set_fuelcell_number_of_units,
	PyDoc_STR("*float*: Fuel cell number of units"),
 	NULL},
{"fuelcell_operation_options", (getter)FuelCell_get_fuelcell_operation_options,(setter)FuelCell_set_fuelcell_operation_options,
	PyDoc_STR("*float*: Fuel cell turn off options [0/1]"),
 	NULL},
{"fuelcell_replacement_option", (getter)FuelCell_get_fuelcell_replacement_option,(setter)FuelCell_set_fuelcell_replacement_option,
	PyDoc_STR("*float*: Fuel cell replacement option [0/1/2]"),
 	NULL},
{"fuelcell_replacement_percent", (getter)FuelCell_get_fuelcell_replacement_percent,(setter)FuelCell_set_fuelcell_replacement_percent,
	PyDoc_STR("*float*: Fuel cell replace at percentage"),
 	NULL},
{"fuelcell_replacement_schedule", (getter)FuelCell_get_fuelcell_replacement_schedule,(setter)FuelCell_set_fuelcell_replacement_schedule,
	PyDoc_STR("*sequence*: Fuel cell replace on schedule"),
 	NULL},
{"fuelcell_shutdown_time", (getter)FuelCell_get_fuelcell_shutdown_time,(setter)FuelCell_set_fuelcell_shutdown_time,
	PyDoc_STR("*float*: Fuel cell shutdown hours [hours]"),
 	NULL},
{"fuelcell_startup_time", (getter)FuelCell_get_fuelcell_startup_time,(setter)FuelCell_set_fuelcell_startup_time,
	PyDoc_STR("*float*: Fuel cell startup hours [hours]"),
 	NULL},
{"fuelcell_type", (getter)FuelCell_get_fuelcell_type,(setter)FuelCell_set_fuelcell_type,
	PyDoc_STR("*float*: Fuel cell type [0/1/2]"),
 	NULL},
{"fuelcell_unit_max_power", (getter)FuelCell_get_fuelcell_unit_max_power,(setter)FuelCell_set_fuelcell_unit_max_power,
	PyDoc_STR("*float*: Fuel cell max power per unit [kW]"),
 	NULL},
{"fuelcell_unit_min_power", (getter)FuelCell_get_fuelcell_unit_min_power,(setter)FuelCell_set_fuelcell_unit_min_power,
	PyDoc_STR("*float*: Fuel cell min power per unit [kW]"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject FuelCell_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Fuelcell.FuelCell",             /*tp_name*/
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
	 * Outputs Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Fuelcell   data_ptr;
} OutputsObject;

static PyTypeObject Outputs_Type;

static PyObject *
Outputs_new(SAM_Fuelcell data_ptr)
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

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Fuelcell", "Outputs")){
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
Outputs_get_annual_fuel_usage(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Fuelcell_Outputs_annual_fuel_usage_nget, self->data_ptr);
}

static PyObject *
Outputs_get_fuelcell_electrical_efficiency(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Fuelcell_Outputs_fuelcell_electrical_efficiency_aget, self->data_ptr);
}

static PyObject *
Outputs_get_fuelcell_fuel_consumption_mcf(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Fuelcell_Outputs_fuelcell_fuel_consumption_mcf_aget, self->data_ptr);
}

static PyObject *
Outputs_get_fuelcell_percent_load(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Fuelcell_Outputs_fuelcell_percent_load_aget, self->data_ptr);
}

static PyObject *
Outputs_get_fuelcell_power(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Fuelcell_Outputs_fuelcell_power_aget, self->data_ptr);
}

static PyObject *
Outputs_get_fuelcell_power_max_percent(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Fuelcell_Outputs_fuelcell_power_max_percent_aget, self->data_ptr);
}

static PyObject *
Outputs_get_fuelcell_power_thermal(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Fuelcell_Outputs_fuelcell_power_thermal_aget, self->data_ptr);
}

static PyObject *
Outputs_get_fuelcell_replacement(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Fuelcell_Outputs_fuelcell_replacement_aget, self->data_ptr);
}

static PyObject *
Outputs_get_fuelcell_to_grid(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Fuelcell_Outputs_fuelcell_to_grid_aget, self->data_ptr);
}

static PyObject *
Outputs_get_fuelcell_to_load(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Fuelcell_Outputs_fuelcell_to_load_aget, self->data_ptr);
}

static PyObject *
Outputs_get_gen(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Fuelcell_Outputs_gen_aget, self->data_ptr);
}

static PyObject *
Outputs_get_system_heat_rate(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Fuelcell_Outputs_system_heat_rate_nget, self->data_ptr);
}

static PyGetSetDef Outputs_getset[] = {
{"annual_fuel_usage", (getter)Outputs_get_annual_fuel_usage,(setter)0,
	PyDoc_STR("*float*: Annual Fuel Usage [kWht]"),
 	NULL},
{"fuelcell_electrical_efficiency", (getter)Outputs_get_fuelcell_electrical_efficiency,(setter)0,
	PyDoc_STR("*sequence*: Fuel cell electrical efficiency [%]"),
 	NULL},
{"fuelcell_fuel_consumption_mcf", (getter)Outputs_get_fuelcell_fuel_consumption_mcf,(setter)0,
	PyDoc_STR("*sequence*: Fuel consumption of fuel cell [MCf]"),
 	NULL},
{"fuelcell_percent_load", (getter)Outputs_get_fuelcell_percent_load,(setter)0,
	PyDoc_STR("*sequence*: Fuel cell percent load [%]"),
 	NULL},
{"fuelcell_power", (getter)Outputs_get_fuelcell_power,(setter)0,
	PyDoc_STR("*sequence*: Electricity from fuel cell [kW]"),
 	NULL},
{"fuelcell_power_max_percent", (getter)Outputs_get_fuelcell_power_max_percent,(setter)0,
	PyDoc_STR("*sequence*: Fuel cell max power percent available [%]"),
 	NULL},
{"fuelcell_power_thermal", (getter)Outputs_get_fuelcell_power_thermal,(setter)0,
	PyDoc_STR("*sequence*: Heat from fuel cell [kWt]"),
 	NULL},
{"fuelcell_replacement", (getter)Outputs_get_fuelcell_replacement,(setter)0,
	PyDoc_STR("*sequence*: Fuel cell replacements per year [number/year]"),
 	NULL},
{"fuelcell_to_grid", (getter)Outputs_get_fuelcell_to_grid,(setter)0,
	PyDoc_STR("*sequence*: Electricity to grid from fuel cell [kW]"),
 	NULL},
{"fuelcell_to_load", (getter)Outputs_get_fuelcell_to_load,(setter)0,
	PyDoc_STR("*sequence*: Electricity to load from fuel cell [kW]"),
 	NULL},
{"gen", (getter)Outputs_get_gen,(setter)0,
	PyDoc_STR("*sequence*: System power generated [kW]"),
 	NULL},
{"system_heat_rate", (getter)Outputs_get_system_heat_rate,(setter)0,
	PyDoc_STR("*float*: Heat rate conversion factor (MMBTUs/MWhe) [MMBTUs/MWhe]"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject Outputs_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Fuelcell.Outputs",             /*tp_name*/
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
 * Fuelcell
 */

typedef struct {
	PyObject_HEAD
	PyObject            *x_attr;        /* Attributes dictionary */
	SAM_Fuelcell   data_ptr;
} FuelcellObject;

static PyTypeObject Fuelcell_Type;

#define FuelcellObject_Check(v)      (Py_TYPE(v) == &Fuelcell_Type)

static FuelcellObject *
newFuelcellObject(void* data_ptr)
{
	FuelcellObject *self;
	self = PyObject_New(FuelcellObject, &Fuelcell_Type);

	PySAM_TECH_ATTR("Fuelcell", SAM_Fuelcell_construct)

	PyObject* Common_obj = Common_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "Common", Common_obj);
	Py_DECREF(Common_obj);

	PyObject* Lifetime_obj = Lifetime_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "Lifetime", Lifetime_obj);
	Py_DECREF(Lifetime_obj);

	PyObject* Load_obj = Load_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "Load", Load_obj);
	Py_DECREF(Load_obj);

	PyObject* FuelCell_obj = FuelCell_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "FuelCell", FuelCell_obj);
	Py_DECREF(FuelCell_obj);

	PyObject* Outputs_obj = Outputs_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "Outputs", Outputs_obj);
	Py_DECREF(Outputs_obj);


	return self;
}

/* Fuelcell methods */

static void
Fuelcell_dealloc(FuelcellObject *self)
{
	Py_XDECREF(self->x_attr);
	SAM_Fuelcell_destruct(self->data_ptr);
	PyObject_Del(self);
}


static PyObject *
Fuelcell_execute(FuelcellObject *self, PyObject *args)
{
	int verbosity = 0;

	if (!PyArg_ParseTuple(args, "|i", &verbosity))
		return NULL;

	SAM_error error = new_error();
	SAM_Fuelcell_execute(self->data_ptr, verbosity, &error);
	if (PySAM_has_error(error )) return NULL;

	Py_INCREF(Py_None);
	return Py_None;
}


static PyObject *
Fuelcell_assign(FuelcellObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_nested_dict((PyObject*)self, self->x_attr, self->data_ptr, dict, "Fuelcell"))
		return NULL;

	Py_INCREF(Py_None);
	return Py_None;
}


static PyObject *
Fuelcell_export(FuelcellObject *self, PyObject *args)
{
	return PySAM_export_to_nested_dict((PyObject *) self, self->x_attr);
}

static PyMethodDef Fuelcell_methods[] = {
		{"execute",            (PyCFunction)Fuelcell_execute,  METH_VARARGS,
				PyDoc_STR("execute(int verbosity) -> None\n Execute simulation with verbosity level 0 (default) or 1")},
		{"assign",            (PyCFunction)Fuelcell_assign,  METH_VARARGS,
				PyDoc_STR("assign(dict) -> None\n Assign attributes from nested dictionary, except for Outputs\n\n``nested_dict = { 'Common': { var: val, ...}, ...}``")},
		{"export",            (PyCFunction)Fuelcell_export,  METH_VARARGS,
				PyDoc_STR("export() -> dict\n Export attributes into nested dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
Fuelcell_getattro(FuelcellObject *self, PyObject *name)
{
	return PySAM_get_attr((PyObject*) self, (PyObject*) self->x_attr, name);
}

static int
Fuelcell_setattr(FuelcellObject *self, const char *name, PyObject *v)
{
	return PySAM_set_attr((PyObject*)self, (PyObject*)self->x_attr, name, v);
}

static PyTypeObject Fuelcell_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Fuelcell",            /*tp_name*/
		sizeof(FuelcellObject),/*tp_basicsize*/
		0,                          /*tp_itemsize*/
		/* methods */
		(destructor)Fuelcell_dealloc,    /*tp_dealloc*/
		0,                          /*tp_print*/
		(getattrfunc)0,             /*tp_getattr*/
		(setattrfunc)Fuelcell_setattr,   /*tp_setattr*/
		0,                          /*tp_reserved*/
		0,                          /*tp_repr*/
		0,                          /*tp_as_number*/
		0,                          /*tp_as_sequence*/
		0,                          /*tp_as_mapping*/
		0,                          /*tp_hash*/
		0,                          /*tp_call*/
		0,                          /*tp_str*/
		(getattrofunc)Fuelcell_getattro, /*tp_getattro*/
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
		Fuelcell_methods,      /*tp_methods*/
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


/* Function of no arguments returning new Fuelcell object */

static PyObject *
Fuelcell_new(PyObject *self, PyObject *args)
{
	FuelcellObject *rv;
	rv = newFuelcellObject(0);
	if (rv == NULL)
		return NULL;
	return (PyObject *)rv;
}

static PyObject *
Fuelcell_wrap(PyObject *self, PyObject *args)
{
	FuelcellObject *rv;
	long long int ptr = 0;  // 64 bit arch
	if (!PyArg_ParseTuple(args, "L:wrap", &ptr)){
		PyErr_BadArgument();
		return NULL;
	}
	rv = newFuelcellObject((void*)ptr);
	if (rv == NULL)
		return NULL;
	return (PyObject *)rv;
}

static PyObject *
Fuelcell_default(PyObject *self, PyObject *args)
{
	FuelcellObject *rv;
	char* def = 0;
	if (!PyArg_ParseTuple(args, "s:default", &def)){
		PyErr_BadArgument();
		return NULL;
	}
	rv = newFuelcellObject(0);
	if (rv == NULL)
		return NULL;

	PySAM_load_defaults((PyObject*)rv, rv->x_attr, rv->data_ptr, "Fuelcell", def);

	return (PyObject *)rv;
}

/* ---------- */


/* List of functions defined in the module */

static PyMethodDef FuelcellModule_methods[] = {
		{"new",             Fuelcell_new,         METH_VARARGS,
				PyDoc_STR("new() -> Fuelcell")},
		{"default",             Fuelcell_default,         METH_VARARGS,
				PyDoc_STR("default(config) -> Fuelcell\n\nUse financial model-specific default attributes\n"
				"config options:\n\n- \"FuelCellCommercial\"\n- \"FuelCellSingleOwner\"")},
		{"wrap",             Fuelcell_wrap,         METH_VARARGS,
				PyDoc_STR("wrap(ssc_data_t) -> Fuelcell\n\nUse existing PySSC data\n\n.. warning::\n\n	Do not call PySSC.data_free on the ssc_data_t provided to ``wrap``")},
		{NULL,              NULL}           /* sentinel */
};

PyDoc_STRVAR(module_doc,
			 "Fuel cell model");


static int
FuelcellModule_exec(PyObject *m)
{
	/* Finalize the type object including setting type of the new type
	 * object; doing it here is required for portability, too. */

	if (PySAM_load_lib(m) < 0) goto fail;
	if (PySAM_init_error(m) < 0) goto fail;

	Fuelcell_Type.tp_dict = PyDict_New();
	if (!Fuelcell_Type.tp_dict) { goto fail; }

	/// Add the Common type object to Fuelcell_Type
	if (PyType_Ready(&Common_Type) < 0) { goto fail; }
	PyDict_SetItemString(Fuelcell_Type.tp_dict,
				"Common",
				(PyObject*)&Common_Type);
	Py_DECREF(&Common_Type);

	/// Add the Lifetime type object to Fuelcell_Type
	if (PyType_Ready(&Lifetime_Type) < 0) { goto fail; }
	PyDict_SetItemString(Fuelcell_Type.tp_dict,
				"Lifetime",
				(PyObject*)&Lifetime_Type);
	Py_DECREF(&Lifetime_Type);

	/// Add the Load type object to Fuelcell_Type
	if (PyType_Ready(&Load_Type) < 0) { goto fail; }
	PyDict_SetItemString(Fuelcell_Type.tp_dict,
				"Load",
				(PyObject*)&Load_Type);
	Py_DECREF(&Load_Type);

	/// Add the FuelCell type object to Fuelcell_Type
	if (PyType_Ready(&FuelCell_Type) < 0) { goto fail; }
	PyDict_SetItemString(Fuelcell_Type.tp_dict,
				"FuelCell",
				(PyObject*)&FuelCell_Type);
	Py_DECREF(&FuelCell_Type);

	/// Add the Outputs type object to Fuelcell_Type
	if (PyType_Ready(&Outputs_Type) < 0) { goto fail; }
	PyDict_SetItemString(Fuelcell_Type.tp_dict,
				"Outputs",
				(PyObject*)&Outputs_Type);
	Py_DECREF(&Outputs_Type);

	/// Add the Fuelcell type object to the module
	if (PyType_Ready(&Fuelcell_Type) < 0) { goto fail; }
	PyModule_AddObject(m,
				"Fuelcell",
				(PyObject*)&Fuelcell_Type);

	return 0;
	fail:
	Py_XDECREF(m);
	return -1;
}

static struct PyModuleDef_Slot FuelcellModule_slots[] = {
		{Py_mod_exec, FuelcellModule_exec},
		{0, NULL},
};

static struct PyModuleDef FuelcellModule = {
		PyModuleDef_HEAD_INIT,
		"Fuelcell",
		module_doc,
		0,
		FuelcellModule_methods,
		FuelcellModule_slots,
		NULL,
		NULL,
		NULL
};

/* Export function for the module */

PyMODINIT_FUNC
PyInit_Fuelcell(void)
{
	return PyModuleDef_Init(&FuelcellModule);
}