#include <Python.h>

#include <SAM_Tcsiscc.h>
#include <SAM_api.h>

#include "PySAM_utils.h"



	/*
	 * Weather Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Tcsiscc   data_ptr;
} WeatherObject;

static PyTypeObject Weather_Type;

static PyObject *
Weather_new(SAM_Tcsiscc data_ptr)
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

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Tcsiscc", "Weather")){
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
Weather_get_solar_resource_file(WeatherObject *self, void *closure)
{
	return PySAM_string_getter(SAM_Tcsiscc_Weather_solar_resource_file_sget, self->data_ptr);
}

static int
Weather_set_solar_resource_file(WeatherObject *self, PyObject *value, void *closure)
{
	return PySAM_string_setter(value, SAM_Tcsiscc_Weather_solar_resource_file_sset, self->data_ptr);
}

static PyGetSetDef Weather_getset[] = {
{"solar_resource_file", (getter)Weather_get_solar_resource_file,(setter)Weather_set_solar_resource_file,
	PyDoc_STR("*str*: local weather file path\n\n*Constraints*: LOCAL_FILE\n\n*Required*: True"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject Weather_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Tcsiscc.Weather",             /*tp_name*/
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
	 * MoltenSaltTower Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Tcsiscc   data_ptr;
} MoltenSaltTowerObject;

static PyTypeObject MoltenSaltTower_Type;

static PyObject *
MoltenSaltTower_new(SAM_Tcsiscc data_ptr)
{
	PyObject* new_obj = MoltenSaltTower_Type.tp_alloc(&MoltenSaltTower_Type,0);

	MoltenSaltTowerObject* MoltenSaltTower_obj = (MoltenSaltTowerObject*)new_obj;

	MoltenSaltTower_obj->data_ptr = data_ptr;

	return new_obj;
}

/* MoltenSaltTower methods */

static PyObject *
MoltenSaltTower_assign(MoltenSaltTowerObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Tcsiscc", "MoltenSaltTower")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
MoltenSaltTower_export(MoltenSaltTowerObject *self, PyObject *args)
{
	PyTypeObject* tp = &MoltenSaltTower_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef MoltenSaltTower_methods[] = {
		{"assign",            (PyCFunction)MoltenSaltTower_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``MoltenSaltTower_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)MoltenSaltTower_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
MoltenSaltTower_get_system_capacity(MoltenSaltTowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_MoltenSaltTower_system_capacity_nget, self->data_ptr);
}

static int
MoltenSaltTower_set_system_capacity(MoltenSaltTowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_MoltenSaltTower_system_capacity_nset, self->data_ptr);
}

static PyGetSetDef MoltenSaltTower_getset[] = {
{"system_capacity", (getter)MoltenSaltTower_get_system_capacity,(setter)MoltenSaltTower_set_system_capacity,
	PyDoc_STR("*float*: Nameplate capacity [kW]\n\n*Required*: True"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject MoltenSaltTower_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Tcsiscc.MoltenSaltTower",             /*tp_name*/
		sizeof(MoltenSaltTowerObject),          /*tp_basicsize*/
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
		MoltenSaltTower_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		MoltenSaltTower_getset,          /*tp_getset*/
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
	 * Heliostat Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Tcsiscc   data_ptr;
} HeliostatObject;

static PyTypeObject Heliostat_Type;

static PyObject *
Heliostat_new(SAM_Tcsiscc data_ptr)
{
	PyObject* new_obj = Heliostat_Type.tp_alloc(&Heliostat_Type,0);

	HeliostatObject* Heliostat_obj = (HeliostatObject*)new_obj;

	Heliostat_obj->data_ptr = data_ptr;

	return new_obj;
}

/* Heliostat methods */

static PyObject *
Heliostat_assign(HeliostatObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Tcsiscc", "Heliostat")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
Heliostat_export(HeliostatObject *self, PyObject *args)
{
	PyTypeObject* tp = &Heliostat_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef Heliostat_methods[] = {
		{"assign",            (PyCFunction)Heliostat_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``Heliostat_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)Heliostat_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
Heliostat_get_N_hel(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_N_hel_nget, self->data_ptr);
}

static int
Heliostat_set_N_hel(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_N_hel_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_bop_spec_cost(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_bop_spec_cost_nget, self->data_ptr);
}

static int
Heliostat_set_bop_spec_cost(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_bop_spec_cost_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_c_atm_0(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_c_atm_0_nget, self->data_ptr);
}

static int
Heliostat_set_c_atm_0(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_c_atm_0_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_c_atm_1(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_c_atm_1_nget, self->data_ptr);
}

static int
Heliostat_set_c_atm_1(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_c_atm_1_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_c_atm_2(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_c_atm_2_nget, self->data_ptr);
}

static int
Heliostat_set_c_atm_2(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_c_atm_2_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_c_atm_3(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_c_atm_3_nget, self->data_ptr);
}

static int
Heliostat_set_c_atm_3(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_c_atm_3_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_calc_fluxmaps(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_calc_fluxmaps_nget, self->data_ptr);
}

static int
Heliostat_set_calc_fluxmaps(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_calc_fluxmaps_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_cant_type(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_cant_type_nget, self->data_ptr);
}

static int
Heliostat_set_cant_type(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_cant_type_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_check_max_flux(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_check_max_flux_nget, self->data_ptr);
}

static int
Heliostat_set_check_max_flux(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_check_max_flux_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_contingency_rate(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_contingency_rate_nget, self->data_ptr);
}

static int
Heliostat_set_contingency_rate(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_contingency_rate_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_cost_sf_fixed(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_cost_sf_fixed_nget, self->data_ptr);
}

static int
Heliostat_set_cost_sf_fixed(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_cost_sf_fixed_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_csp_pt_cost_epc_fixed(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_csp_pt_cost_epc_fixed_nget, self->data_ptr);
}

static int
Heliostat_set_csp_pt_cost_epc_fixed(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_csp_pt_cost_epc_fixed_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_csp_pt_cost_epc_per_acre(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_csp_pt_cost_epc_per_acre_nget, self->data_ptr);
}

static int
Heliostat_set_csp_pt_cost_epc_per_acre(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_csp_pt_cost_epc_per_acre_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_csp_pt_cost_epc_per_watt(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_csp_pt_cost_epc_per_watt_nget, self->data_ptr);
}

static int
Heliostat_set_csp_pt_cost_epc_per_watt(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_csp_pt_cost_epc_per_watt_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_csp_pt_cost_epc_percent(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_csp_pt_cost_epc_percent_nget, self->data_ptr);
}

static int
Heliostat_set_csp_pt_cost_epc_percent(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_csp_pt_cost_epc_percent_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_csp_pt_cost_plm_fixed(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_csp_pt_cost_plm_fixed_nget, self->data_ptr);
}

static int
Heliostat_set_csp_pt_cost_plm_fixed(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_csp_pt_cost_plm_fixed_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_csp_pt_cost_plm_per_acre(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_csp_pt_cost_plm_per_acre_nget, self->data_ptr);
}

static int
Heliostat_set_csp_pt_cost_plm_per_acre(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_csp_pt_cost_plm_per_acre_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_csp_pt_cost_plm_per_watt(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_csp_pt_cost_plm_per_watt_nget, self->data_ptr);
}

static int
Heliostat_set_csp_pt_cost_plm_per_watt(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_csp_pt_cost_plm_per_watt_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_csp_pt_cost_plm_percent(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_csp_pt_cost_plm_percent_nget, self->data_ptr);
}

static int
Heliostat_set_csp_pt_cost_plm_percent(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_csp_pt_cost_plm_percent_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_csp_pt_sf_fixed_land_area(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_csp_pt_sf_fixed_land_area_nget, self->data_ptr);
}

static int
Heliostat_set_csp_pt_sf_fixed_land_area(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_csp_pt_sf_fixed_land_area_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_csp_pt_sf_land_overhead_factor(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_csp_pt_sf_land_overhead_factor_nget, self->data_ptr);
}

static int
Heliostat_set_csp_pt_sf_land_overhead_factor(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_csp_pt_sf_land_overhead_factor_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_delta_flux_hrs(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_delta_flux_hrs_nget, self->data_ptr);
}

static int
Heliostat_set_delta_flux_hrs(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_delta_flux_hrs_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_dens_mirror(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_dens_mirror_nget, self->data_ptr);
}

static int
Heliostat_set_dens_mirror(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_dens_mirror_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_dni_des(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_dni_des_nget, self->data_ptr);
}

static int
Heliostat_set_dni_des(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_dni_des_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_eta_map(HeliostatObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Tcsiscc_Heliostat_eta_map_mget, self->data_ptr);
}

static int
Heliostat_set_eta_map(HeliostatObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_Tcsiscc_Heliostat_eta_map_mset, self->data_ptr);
}

static PyObject *
Heliostat_get_flux_maps(HeliostatObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Tcsiscc_Heliostat_flux_maps_mget, self->data_ptr);
}

static int
Heliostat_set_flux_maps(HeliostatObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_Tcsiscc_Heliostat_flux_maps_mset, self->data_ptr);
}

static PyObject *
Heliostat_get_flux_max(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_flux_max_nget, self->data_ptr);
}

static int
Heliostat_set_flux_max(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_flux_max_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_flux_positions(HeliostatObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Tcsiscc_Heliostat_flux_positions_mget, self->data_ptr);
}

static int
Heliostat_set_flux_positions(HeliostatObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_Tcsiscc_Heliostat_flux_positions_mset, self->data_ptr);
}

static PyObject *
Heliostat_get_focus_type(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_focus_type_nget, self->data_ptr);
}

static int
Heliostat_set_focus_type(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_focus_type_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_fossil_spec_cost(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_fossil_spec_cost_nget, self->data_ptr);
}

static int
Heliostat_set_fossil_spec_cost(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_fossil_spec_cost_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_h_tower(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_h_tower_nget, self->data_ptr);
}

static int
Heliostat_set_h_tower(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_h_tower_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_hel_stow_deploy(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_hel_stow_deploy_nget, self->data_ptr);
}

static int
Heliostat_set_hel_stow_deploy(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_hel_stow_deploy_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_helio_active_fraction(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_helio_active_fraction_nget, self->data_ptr);
}

static int
Heliostat_set_helio_active_fraction(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_helio_active_fraction_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_helio_aim_points(HeliostatObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Tcsiscc_Heliostat_helio_aim_points_mget, self->data_ptr);
}

static int
Heliostat_set_helio_aim_points(HeliostatObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_Tcsiscc_Heliostat_helio_aim_points_mset, self->data_ptr);
}

static PyObject *
Heliostat_get_helio_height(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_helio_height_nget, self->data_ptr);
}

static int
Heliostat_set_helio_height(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_helio_height_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_helio_optical_error(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_helio_optical_error_nget, self->data_ptr);
}

static int
Heliostat_set_helio_optical_error(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_helio_optical_error_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_helio_positions(HeliostatObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Tcsiscc_Heliostat_helio_positions_mget, self->data_ptr);
}

static int
Heliostat_set_helio_positions(HeliostatObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_Tcsiscc_Heliostat_helio_positions_mset, self->data_ptr);
}

static PyObject *
Heliostat_get_helio_reflectance(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_helio_reflectance_nget, self->data_ptr);
}

static int
Heliostat_set_helio_reflectance(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_helio_reflectance_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_helio_width(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_helio_width_nget, self->data_ptr);
}

static int
Heliostat_set_helio_width(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_helio_width_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_heliostat_spec_cost(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_heliostat_spec_cost_nget, self->data_ptr);
}

static int
Heliostat_set_heliostat_spec_cost(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_heliostat_spec_cost_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_interp_beta(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_interp_beta_nget, self->data_ptr);
}

static int
Heliostat_set_interp_beta(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_interp_beta_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_interp_nug(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_interp_nug_nget, self->data_ptr);
}

static int
Heliostat_set_interp_nug(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_interp_nug_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_is_optimize(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_is_optimize_nget, self->data_ptr);
}

static int
Heliostat_set_is_optimize(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_is_optimize_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_land_bound_list(HeliostatObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsiscc_Heliostat_land_bound_list_aget, self->data_ptr);
}

static int
Heliostat_set_land_bound_list(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Tcsiscc_Heliostat_land_bound_list_aset, self->data_ptr);
}

static PyObject *
Heliostat_get_land_bound_table(HeliostatObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Tcsiscc_Heliostat_land_bound_table_mget, self->data_ptr);
}

static int
Heliostat_set_land_bound_table(HeliostatObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_Tcsiscc_Heliostat_land_bound_table_mset, self->data_ptr);
}

static PyObject *
Heliostat_get_land_bound_type(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_land_bound_type_nget, self->data_ptr);
}

static int
Heliostat_set_land_bound_type(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_land_bound_type_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_land_max(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_land_max_nget, self->data_ptr);
}

static int
Heliostat_set_land_max(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_land_max_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_land_min(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_land_min_nget, self->data_ptr);
}

static int
Heliostat_set_land_min(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_land_min_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_land_spec_cost(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_land_spec_cost_nget, self->data_ptr);
}

static int
Heliostat_set_land_spec_cost(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_land_spec_cost_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_n_facet_x(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_n_facet_x_nget, self->data_ptr);
}

static int
Heliostat_set_n_facet_x(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_n_facet_x_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_n_facet_y(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_n_facet_y_nget, self->data_ptr);
}

static int
Heliostat_set_n_facet_y(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_n_facet_y_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_n_flux_days(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_n_flux_days_nget, self->data_ptr);
}

static int
Heliostat_set_n_flux_days(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_n_flux_days_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_n_flux_x(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_n_flux_x_nget, self->data_ptr);
}

static int
Heliostat_set_n_flux_x(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_n_flux_x_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_n_flux_y(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_n_flux_y_nget, self->data_ptr);
}

static int
Heliostat_set_n_flux_y(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_n_flux_y_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_opt_algorithm(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_opt_algorithm_nget, self->data_ptr);
}

static int
Heliostat_set_opt_algorithm(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_opt_algorithm_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_opt_conv_tol(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_opt_conv_tol_nget, self->data_ptr);
}

static int
Heliostat_set_opt_conv_tol(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_opt_conv_tol_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_opt_flux_penalty(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_opt_flux_penalty_nget, self->data_ptr);
}

static int
Heliostat_set_opt_flux_penalty(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_opt_flux_penalty_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_opt_init_step(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_opt_init_step_nget, self->data_ptr);
}

static int
Heliostat_set_opt_init_step(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_opt_init_step_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_opt_max_iter(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_opt_max_iter_nget, self->data_ptr);
}

static int
Heliostat_set_opt_max_iter(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_opt_max_iter_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_p_start(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_p_start_nget, self->data_ptr);
}

static int
Heliostat_set_p_start(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_p_start_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_p_track(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_p_track_nget, self->data_ptr);
}

static int
Heliostat_set_p_track(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_p_track_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_plant_spec_cost(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_plant_spec_cost_nget, self->data_ptr);
}

static int
Heliostat_set_plant_spec_cost(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_plant_spec_cost_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_q_design(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_q_design_nget, self->data_ptr);
}

static int
Heliostat_set_q_design(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_q_design_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_rec_absorptance(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_rec_absorptance_nget, self->data_ptr);
}

static int
Heliostat_set_rec_absorptance(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_rec_absorptance_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_rec_aspect(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_rec_aspect_nget, self->data_ptr);
}

static int
Heliostat_set_rec_aspect(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_rec_aspect_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_rec_cost_exp(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_rec_cost_exp_nget, self->data_ptr);
}

static int
Heliostat_set_rec_cost_exp(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_rec_cost_exp_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_rec_height(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_rec_height_nget, self->data_ptr);
}

static int
Heliostat_set_rec_height(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_rec_height_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_rec_hl_perm2(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_rec_hl_perm2_nget, self->data_ptr);
}

static int
Heliostat_set_rec_hl_perm2(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_rec_hl_perm2_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_rec_ref_area(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_rec_ref_area_nget, self->data_ptr);
}

static int
Heliostat_set_rec_ref_area(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_rec_ref_area_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_rec_ref_cost(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_rec_ref_cost_nget, self->data_ptr);
}

static int
Heliostat_set_rec_ref_cost(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_rec_ref_cost_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_run_type(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_run_type_nget, self->data_ptr);
}

static int
Heliostat_set_run_type(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_run_type_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_sales_tax_frac(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_sales_tax_frac_nget, self->data_ptr);
}

static int
Heliostat_set_sales_tax_frac(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_sales_tax_frac_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_sales_tax_rate(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_sales_tax_rate_nget, self->data_ptr);
}

static int
Heliostat_set_sales_tax_rate(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_sales_tax_rate_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_site_spec_cost(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_site_spec_cost_nget, self->data_ptr);
}

static int
Heliostat_set_site_spec_cost(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_site_spec_cost_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_tes_spec_cost(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_tes_spec_cost_nget, self->data_ptr);
}

static int
Heliostat_set_tes_spec_cost(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_tes_spec_cost_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_total_installed_cost(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_total_installed_cost_nget, self->data_ptr);
}

static int
Heliostat_set_total_installed_cost(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_total_installed_cost_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_tower_exp(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_tower_exp_nget, self->data_ptr);
}

static int
Heliostat_set_tower_exp(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_tower_exp_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_tower_fixed_cost(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_tower_fixed_cost_nget, self->data_ptr);
}

static int
Heliostat_set_tower_fixed_cost(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_tower_fixed_cost_nset, self->data_ptr);
}

static PyObject *
Heliostat_get_v_wind_max(HeliostatObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Heliostat_v_wind_max_nget, self->data_ptr);
}

static int
Heliostat_set_v_wind_max(HeliostatObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Heliostat_v_wind_max_nset, self->data_ptr);
}

static PyGetSetDef Heliostat_getset[] = {
{"N_hel", (getter)Heliostat_get_N_hel,(setter)Heliostat_set_N_hel,
	PyDoc_STR("*float*: Number of heliostats [-]\n\n*Required*: False"),
 	NULL},
{"bop_spec_cost", (getter)Heliostat_get_bop_spec_cost,(setter)Heliostat_set_bop_spec_cost,
	PyDoc_STR("*float*: BOS specific cost [$/kWe]\n\n*Required*: True"),
 	NULL},
{"c_atm_0", (getter)Heliostat_get_c_atm_0,(setter)Heliostat_set_c_atm_0,
	PyDoc_STR("*float*: Attenuation coefficient 0\n\n*Required*: set to 0.006789 if not provided."),
 	NULL},
{"c_atm_1", (getter)Heliostat_get_c_atm_1,(setter)Heliostat_set_c_atm_1,
	PyDoc_STR("*float*: Attenuation coefficient 1\n\n*Required*: set to 0.1046 if not provided."),
 	NULL},
{"c_atm_2", (getter)Heliostat_get_c_atm_2,(setter)Heliostat_set_c_atm_2,
	PyDoc_STR("*float*: Attenuation coefficient 2\n\n*Required*: set to -0.0107 if not provided."),
 	NULL},
{"c_atm_3", (getter)Heliostat_get_c_atm_3,(setter)Heliostat_set_c_atm_3,
	PyDoc_STR("*float*: Attenuation coefficient 3\n\n*Required*: set to 0.002845 if not provided."),
 	NULL},
{"calc_fluxmaps", (getter)Heliostat_get_calc_fluxmaps,(setter)Heliostat_set_calc_fluxmaps,
	PyDoc_STR("*float*: Include fluxmap calculations\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"cant_type", (getter)Heliostat_get_cant_type,(setter)Heliostat_set_cant_type,
	PyDoc_STR("*float*: Heliostat cant method\n\n*Required*: True"),
 	NULL},
{"check_max_flux", (getter)Heliostat_get_check_max_flux,(setter)Heliostat_set_check_max_flux,
	PyDoc_STR("*float*: Check max flux at design point\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"contingency_rate", (getter)Heliostat_get_contingency_rate,(setter)Heliostat_set_contingency_rate,
	PyDoc_STR("*float*: Contingency for cost overrun [%]\n\n*Required*: True"),
 	NULL},
{"cost_sf_fixed", (getter)Heliostat_get_cost_sf_fixed,(setter)Heliostat_set_cost_sf_fixed,
	PyDoc_STR("*float*: Solar field fixed cost [$]\n\n*Required*: True"),
 	NULL},
{"csp_pt_cost_epc_fixed", (getter)Heliostat_get_csp_pt_cost_epc_fixed,(setter)Heliostat_set_csp_pt_cost_epc_fixed,
	PyDoc_STR("*float*: EPC fixed [$]\n\n*Required*: True"),
 	NULL},
{"csp_pt_cost_epc_per_acre", (getter)Heliostat_get_csp_pt_cost_epc_per_acre,(setter)Heliostat_set_csp_pt_cost_epc_per_acre,
	PyDoc_STR("*float*: EPC cost per acre [$/acre]\n\n*Required*: True"),
 	NULL},
{"csp_pt_cost_epc_per_watt", (getter)Heliostat_get_csp_pt_cost_epc_per_watt,(setter)Heliostat_set_csp_pt_cost_epc_per_watt,
	PyDoc_STR("*float*: EPC cost per watt [$/W]\n\n*Required*: True"),
 	NULL},
{"csp_pt_cost_epc_percent", (getter)Heliostat_get_csp_pt_cost_epc_percent,(setter)Heliostat_set_csp_pt_cost_epc_percent,
	PyDoc_STR("*float*: EPC cost percent of direct\n\n*Required*: True"),
 	NULL},
{"csp_pt_cost_plm_fixed", (getter)Heliostat_get_csp_pt_cost_plm_fixed,(setter)Heliostat_set_csp_pt_cost_plm_fixed,
	PyDoc_STR("*float*: PLM fixed [$]\n\n*Required*: True"),
 	NULL},
{"csp_pt_cost_plm_per_acre", (getter)Heliostat_get_csp_pt_cost_plm_per_acre,(setter)Heliostat_set_csp_pt_cost_plm_per_acre,
	PyDoc_STR("*float*: PLM cost per acre [$/acre]\n\n*Required*: True"),
 	NULL},
{"csp_pt_cost_plm_per_watt", (getter)Heliostat_get_csp_pt_cost_plm_per_watt,(setter)Heliostat_set_csp_pt_cost_plm_per_watt,
	PyDoc_STR("*float*: PLM cost per watt [$/W]\n\n*Required*: True"),
 	NULL},
{"csp_pt_cost_plm_percent", (getter)Heliostat_get_csp_pt_cost_plm_percent,(setter)Heliostat_set_csp_pt_cost_plm_percent,
	PyDoc_STR("*float*: PLM cost percent of direct\n\n*Required*: True"),
 	NULL},
{"csp_pt_sf_fixed_land_area", (getter)Heliostat_get_csp_pt_sf_fixed_land_area,(setter)Heliostat_set_csp_pt_sf_fixed_land_area,
	PyDoc_STR("*float*: Fixed land area [acre]\n\n*Required*: True"),
 	NULL},
{"csp_pt_sf_land_overhead_factor", (getter)Heliostat_get_csp_pt_sf_land_overhead_factor,(setter)Heliostat_set_csp_pt_sf_land_overhead_factor,
	PyDoc_STR("*float*: Land overhead factor\n\n*Required*: True"),
 	NULL},
{"delta_flux_hrs", (getter)Heliostat_get_delta_flux_hrs,(setter)Heliostat_set_delta_flux_hrs,
	PyDoc_STR("*float*: Hourly frequency in flux map lookup\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"dens_mirror", (getter)Heliostat_get_dens_mirror,(setter)Heliostat_set_dens_mirror,
	PyDoc_STR("*float*: Ratio of Reflective Area to Profile [-]\n\n*Required*: True"),
 	NULL},
{"dni_des", (getter)Heliostat_get_dni_des,(setter)Heliostat_set_dni_des,
	PyDoc_STR("*float*: Design-point DNI [W/m2]\n\n*Required*: True"),
 	NULL},
{"eta_map", (getter)Heliostat_get_eta_map,(setter)Heliostat_set_eta_map,
	PyDoc_STR("*sequence[sequence]*: Field efficiency array [-]\n\n*Required*: False"),
 	NULL},
{"flux_maps", (getter)Heliostat_get_flux_maps,(setter)Heliostat_set_flux_maps,
	PyDoc_STR("*sequence[sequence]*: Flux map intensities [-]\n\n*Required*: False"),
 	NULL},
{"flux_max", (getter)Heliostat_get_flux_max,(setter)Heliostat_set_flux_max,
	PyDoc_STR("*float*: Maximum allowable flux\n\n*Required*: set to 1000 if not provided."),
 	NULL},
{"flux_positions", (getter)Heliostat_get_flux_positions,(setter)Heliostat_set_flux_positions,
	PyDoc_STR("*sequence[sequence]*: Flux map sun positions [deg]\n\n*Required*: False"),
 	NULL},
{"focus_type", (getter)Heliostat_get_focus_type,(setter)Heliostat_set_focus_type,
	PyDoc_STR("*float*: Heliostat focus method\n\n*Required*: True"),
 	NULL},
{"fossil_spec_cost", (getter)Heliostat_get_fossil_spec_cost,(setter)Heliostat_set_fossil_spec_cost,
	PyDoc_STR("*float*: Fossil system specific cost [$/kWe]\n\n*Required*: True"),
 	NULL},
{"h_tower", (getter)Heliostat_get_h_tower,(setter)Heliostat_set_h_tower,
	PyDoc_STR("*float*: Tower height [m]\n\n*Required*: True"),
 	NULL},
{"hel_stow_deploy", (getter)Heliostat_get_hel_stow_deploy,(setter)Heliostat_set_hel_stow_deploy,
	PyDoc_STR("*float*: Stow/deploy elevation [deg]\n\n*Required*: True"),
 	NULL},
{"helio_active_fraction", (getter)Heliostat_get_helio_active_fraction,(setter)Heliostat_set_helio_active_fraction,
	PyDoc_STR("*float*: Heliostat active frac. [-]\n\n*Required*: True"),
 	NULL},
{"helio_aim_points", (getter)Heliostat_get_helio_aim_points,(setter)Heliostat_set_helio_aim_points,
	PyDoc_STR("*sequence[sequence]*: Heliostat aim point table [m]\n\n*Required*: False"),
 	NULL},
{"helio_height", (getter)Heliostat_get_helio_height,(setter)Heliostat_set_helio_height,
	PyDoc_STR("*float*: Heliostat height [m]\n\n*Required*: True"),
 	NULL},
{"helio_optical_error", (getter)Heliostat_get_helio_optical_error,(setter)Heliostat_set_helio_optical_error,
	PyDoc_STR("*float*: Heliostat optical error [rad]\n\n*Required*: True"),
 	NULL},
{"helio_positions", (getter)Heliostat_get_helio_positions,(setter)Heliostat_set_helio_positions,
	PyDoc_STR("*sequence[sequence]*: Heliostat position table [m]\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"helio_reflectance", (getter)Heliostat_get_helio_reflectance,(setter)Heliostat_set_helio_reflectance,
	PyDoc_STR("*float*: Heliostat reflectance [-]\n\n*Required*: True"),
 	NULL},
{"helio_width", (getter)Heliostat_get_helio_width,(setter)Heliostat_set_helio_width,
	PyDoc_STR("*float*: Heliostat width [m]\n\n*Required*: True"),
 	NULL},
{"heliostat_spec_cost", (getter)Heliostat_get_heliostat_spec_cost,(setter)Heliostat_set_heliostat_spec_cost,
	PyDoc_STR("*float*: Heliostat field cost [$/m2]\n\n*Required*: True"),
 	NULL},
{"interp_beta", (getter)Heliostat_get_interp_beta,(setter)Heliostat_set_interp_beta,
	PyDoc_STR("*float*: Interpolation beta coef. [-]\n\n*Required*: set to 1.99 if not provided."),
 	NULL},
{"interp_nug", (getter)Heliostat_get_interp_nug,(setter)Heliostat_set_interp_nug,
	PyDoc_STR("*float*: Interpolation nugget [-]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"is_optimize", (getter)Heliostat_get_is_optimize,(setter)Heliostat_set_is_optimize,
	PyDoc_STR("*float*: Do SolarPILOT optimization\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"land_bound_list", (getter)Heliostat_get_land_bound_list,(setter)Heliostat_set_land_bound_list,
	PyDoc_STR("*sequence*: Boundary table listing [-]\n\n*Required*: False"),
 	NULL},
{"land_bound_table", (getter)Heliostat_get_land_bound_table,(setter)Heliostat_set_land_bound_table,
	PyDoc_STR("*sequence[sequence]*: Land boundary table [m]\n\n*Required*: False"),
 	NULL},
{"land_bound_type", (getter)Heliostat_get_land_bound_type,(setter)Heliostat_set_land_bound_type,
	PyDoc_STR("*float*: Land boundary type [-]\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"land_max", (getter)Heliostat_get_land_max,(setter)Heliostat_set_land_max,
	PyDoc_STR("*float*: Land max boundary [-ORm]\n\n*Required*: set to 7.5 if not provided."),
 	NULL},
{"land_min", (getter)Heliostat_get_land_min,(setter)Heliostat_set_land_min,
	PyDoc_STR("*float*: Land min boundary [-ORm]\n\n*Required*: set to 0.75 if not provided."),
 	NULL},
{"land_spec_cost", (getter)Heliostat_get_land_spec_cost,(setter)Heliostat_set_land_spec_cost,
	PyDoc_STR("*float*: Total land area cost [$/acre]\n\n*Required*: True"),
 	NULL},
{"n_facet_x", (getter)Heliostat_get_n_facet_x,(setter)Heliostat_set_n_facet_x,
	PyDoc_STR("*float*: Number of heliostat facets - X\n\n*Required*: True"),
 	NULL},
{"n_facet_y", (getter)Heliostat_get_n_facet_y,(setter)Heliostat_set_n_facet_y,
	PyDoc_STR("*float*: Number of heliostat facets - Y\n\n*Required*: True"),
 	NULL},
{"n_flux_days", (getter)Heliostat_get_n_flux_days,(setter)Heliostat_set_n_flux_days,
	PyDoc_STR("*float*: No. days in flux map lookup\n\n*Required*: set to 8 if not provided."),
 	NULL},
{"n_flux_x", (getter)Heliostat_get_n_flux_x,(setter)Heliostat_set_n_flux_x,
	PyDoc_STR("*float*: Flux map X resolution [-]\n\n*Required*: set to 12 if not provided."),
 	NULL},
{"n_flux_y", (getter)Heliostat_get_n_flux_y,(setter)Heliostat_set_n_flux_y,
	PyDoc_STR("*float*: Flux map Y resolution [-]\n\n*Required*: set to 1 if not provided."),
 	NULL},
{"opt_algorithm", (getter)Heliostat_get_opt_algorithm,(setter)Heliostat_set_opt_algorithm,
	PyDoc_STR("*float*: Optimization algorithm\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"opt_conv_tol", (getter)Heliostat_get_opt_conv_tol,(setter)Heliostat_set_opt_conv_tol,
	PyDoc_STR("*float*: Optimization convergence tol\n\n*Required*: set to 0.001 if not provided."),
 	NULL},
{"opt_flux_penalty", (getter)Heliostat_get_opt_flux_penalty,(setter)Heliostat_set_opt_flux_penalty,
	PyDoc_STR("*float*: Optimization flux overage penalty\n\n*Required*: True"),
 	NULL},
{"opt_init_step", (getter)Heliostat_get_opt_init_step,(setter)Heliostat_set_opt_init_step,
	PyDoc_STR("*float*: Optimization initial step size\n\n*Required*: set to 0.05 if not provided."),
 	NULL},
{"opt_max_iter", (getter)Heliostat_get_opt_max_iter,(setter)Heliostat_set_opt_max_iter,
	PyDoc_STR("*float*: Max. number iteration steps\n\n*Required*: set to 200 if not provided."),
 	NULL},
{"p_start", (getter)Heliostat_get_p_start,(setter)Heliostat_set_p_start,
	PyDoc_STR("*float*: Heliostat startup energy [kWe-hr]\n\n*Required*: True"),
 	NULL},
{"p_track", (getter)Heliostat_get_p_track,(setter)Heliostat_set_p_track,
	PyDoc_STR("*float*: Heliostat tracking energy [kWe]\n\n*Required*: True"),
 	NULL},
{"plant_spec_cost", (getter)Heliostat_get_plant_spec_cost,(setter)Heliostat_set_plant_spec_cost,
	PyDoc_STR("*float*: Power cycle specific cost [$/kWe]\n\n*Required*: True"),
 	NULL},
{"q_design", (getter)Heliostat_get_q_design,(setter)Heliostat_set_q_design,
	PyDoc_STR("*float*: Receiver thermal design power [MW]\n\n*Required*: True"),
 	NULL},
{"rec_absorptance", (getter)Heliostat_get_rec_absorptance,(setter)Heliostat_set_rec_absorptance,
	PyDoc_STR("*float*: Receiver absorptance [-]\n\n*Required*: True"),
 	NULL},
{"rec_aspect", (getter)Heliostat_get_rec_aspect,(setter)Heliostat_set_rec_aspect,
	PyDoc_STR("*float*: Receiver aspect ratio [-]\n\n*Required*: True"),
 	NULL},
{"rec_cost_exp", (getter)Heliostat_get_rec_cost_exp,(setter)Heliostat_set_rec_cost_exp,
	PyDoc_STR("*float*: Receiver cost scaling exponent\n\n*Required*: True"),
 	NULL},
{"rec_height", (getter)Heliostat_get_rec_height,(setter)Heliostat_set_rec_height,
	PyDoc_STR("*float*: Receiver height [m]\n\n*Required*: True"),
 	NULL},
{"rec_hl_perm2", (getter)Heliostat_get_rec_hl_perm2,(setter)Heliostat_set_rec_hl_perm2,
	PyDoc_STR("*float*: Receiver design heatloss [kW/m2]\n\n*Required*: True"),
 	NULL},
{"rec_ref_area", (getter)Heliostat_get_rec_ref_area,(setter)Heliostat_set_rec_ref_area,
	PyDoc_STR("*float*: Receiver reference area for cost scale\n\n*Required*: True"),
 	NULL},
{"rec_ref_cost", (getter)Heliostat_get_rec_ref_cost,(setter)Heliostat_set_rec_ref_cost,
	PyDoc_STR("*float*: Receiver reference cost [$]\n\n*Required*: True"),
 	NULL},
{"run_type", (getter)Heliostat_get_run_type,(setter)Heliostat_set_run_type,
	PyDoc_STR("*float*: Run type [-]\n\n*Required*: True"),
 	NULL},
{"sales_tax_frac", (getter)Heliostat_get_sales_tax_frac,(setter)Heliostat_set_sales_tax_frac,
	PyDoc_STR("*float*: Percent of cost to which sales tax applies [%]\n\n*Required*: True"),
 	NULL},
{"sales_tax_rate", (getter)Heliostat_get_sales_tax_rate,(setter)Heliostat_set_sales_tax_rate,
	PyDoc_STR("*float*: Sales tax rate [%]\n\n*Required*: True"),
 	NULL},
{"site_spec_cost", (getter)Heliostat_get_site_spec_cost,(setter)Heliostat_set_site_spec_cost,
	PyDoc_STR("*float*: Site improvement cost [$/m2]\n\n*Required*: True"),
 	NULL},
{"tes_spec_cost", (getter)Heliostat_get_tes_spec_cost,(setter)Heliostat_set_tes_spec_cost,
	PyDoc_STR("*float*: Thermal energy storage cost [$/kWht]\n\n*Required*: True"),
 	NULL},
{"total_installed_cost", (getter)Heliostat_get_total_installed_cost,(setter)Heliostat_set_total_installed_cost,
	PyDoc_STR("*float*: Total installed cost [$]\n\n*Required*: True"),
 	NULL},
{"tower_exp", (getter)Heliostat_get_tower_exp,(setter)Heliostat_set_tower_exp,
	PyDoc_STR("*float*: Tower cost scaling exponent\n\n*Required*: True"),
 	NULL},
{"tower_fixed_cost", (getter)Heliostat_get_tower_fixed_cost,(setter)Heliostat_set_tower_fixed_cost,
	PyDoc_STR("*float*: Tower fixed cost [$]\n\n*Required*: True"),
 	NULL},
{"v_wind_max", (getter)Heliostat_get_v_wind_max,(setter)Heliostat_set_v_wind_max,
	PyDoc_STR("*float*: Max. wind velocity [m/s]\n\n*Required*: True"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject Heliostat_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Tcsiscc.Heliostat",             /*tp_name*/
		sizeof(HeliostatObject),          /*tp_basicsize*/
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
		Heliostat_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		Heliostat_getset,          /*tp_getset*/
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
	 * Receiver Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Tcsiscc   data_ptr;
} ReceiverObject;

static PyTypeObject Receiver_Type;

static PyObject *
Receiver_new(SAM_Tcsiscc data_ptr)
{
	PyObject* new_obj = Receiver_Type.tp_alloc(&Receiver_Type,0);

	ReceiverObject* Receiver_obj = (ReceiverObject*)new_obj;

	Receiver_obj->data_ptr = data_ptr;

	return new_obj;
}

/* Receiver methods */

static PyObject *
Receiver_assign(ReceiverObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Tcsiscc", "Receiver")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
Receiver_export(ReceiverObject *self, PyObject *args)
{
	PyTypeObject* tp = &Receiver_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef Receiver_methods[] = {
		{"assign",            (PyCFunction)Receiver_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``Receiver_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)Receiver_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
Receiver_get_A_sf(ReceiverObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Receiver_A_sf_nget, self->data_ptr);
}

static int
Receiver_set_A_sf(ReceiverObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Receiver_A_sf_nset, self->data_ptr);
}

static PyObject *
Receiver_get_D_rec(ReceiverObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Receiver_D_rec_nget, self->data_ptr);
}

static int
Receiver_set_D_rec(ReceiverObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Receiver_D_rec_nset, self->data_ptr);
}

static PyObject *
Receiver_get_Flow_type(ReceiverObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Receiver_Flow_type_nget, self->data_ptr);
}

static int
Receiver_set_Flow_type(ReceiverObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Receiver_Flow_type_nset, self->data_ptr);
}

static PyObject *
Receiver_get_H_rec(ReceiverObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Receiver_H_rec_nget, self->data_ptr);
}

static int
Receiver_set_H_rec(ReceiverObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Receiver_H_rec_nset, self->data_ptr);
}

static PyObject *
Receiver_get_N_panels(ReceiverObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Receiver_N_panels_nget, self->data_ptr);
}

static int
Receiver_set_N_panels(ReceiverObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Receiver_N_panels_nset, self->data_ptr);
}

static PyObject *
Receiver_get_Q_rec_des(ReceiverObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Receiver_Q_rec_des_nget, self->data_ptr);
}

static int
Receiver_set_Q_rec_des(ReceiverObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Receiver_Q_rec_des_nset, self->data_ptr);
}

static PyObject *
Receiver_get_THT(ReceiverObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Receiver_THT_nget, self->data_ptr);
}

static int
Receiver_set_THT(ReceiverObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Receiver_THT_nset, self->data_ptr);
}

static PyObject *
Receiver_get_T_htf_cold_des(ReceiverObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Receiver_T_htf_cold_des_nget, self->data_ptr);
}

static int
Receiver_set_T_htf_cold_des(ReceiverObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Receiver_T_htf_cold_des_nset, self->data_ptr);
}

static PyObject *
Receiver_get_T_htf_hot_des(ReceiverObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Receiver_T_htf_hot_des_nget, self->data_ptr);
}

static int
Receiver_set_T_htf_hot_des(ReceiverObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Receiver_T_htf_hot_des_nset, self->data_ptr);
}

static PyObject *
Receiver_get_crossover_shift(ReceiverObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Receiver_crossover_shift_nget, self->data_ptr);
}

static int
Receiver_set_crossover_shift(ReceiverObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Receiver_crossover_shift_nset, self->data_ptr);
}

static PyObject *
Receiver_get_d_tube_out(ReceiverObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Receiver_d_tube_out_nget, self->data_ptr);
}

static int
Receiver_set_d_tube_out(ReceiverObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Receiver_d_tube_out_nset, self->data_ptr);
}

static PyObject *
Receiver_get_epsilon(ReceiverObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Receiver_epsilon_nget, self->data_ptr);
}

static int
Receiver_set_epsilon(ReceiverObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Receiver_epsilon_nset, self->data_ptr);
}

static PyObject *
Receiver_get_eta_pump(ReceiverObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Receiver_eta_pump_nget, self->data_ptr);
}

static int
Receiver_set_eta_pump(ReceiverObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Receiver_eta_pump_nset, self->data_ptr);
}

static PyObject *
Receiver_get_f_rec_min(ReceiverObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Receiver_f_rec_min_nget, self->data_ptr);
}

static int
Receiver_set_f_rec_min(ReceiverObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Receiver_f_rec_min_nset, self->data_ptr);
}

static PyObject *
Receiver_get_field_fl_props(ReceiverObject *self, void *closure)
{
	return PySAM_matrix_getter(SAM_Tcsiscc_Receiver_field_fl_props_mget, self->data_ptr);
}

static int
Receiver_set_field_fl_props(ReceiverObject *self, PyObject *value, void *closure)
{
		return PySAM_matrix_setter(value, SAM_Tcsiscc_Receiver_field_fl_props_mset, self->data_ptr);
}

static PyObject *
Receiver_get_hl_ffact(ReceiverObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Receiver_hl_ffact_nget, self->data_ptr);
}

static int
Receiver_set_hl_ffact(ReceiverObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Receiver_hl_ffact_nset, self->data_ptr);
}

static PyObject *
Receiver_get_m_dot_htf_max(ReceiverObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Receiver_m_dot_htf_max_nget, self->data_ptr);
}

static int
Receiver_set_m_dot_htf_max(ReceiverObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Receiver_m_dot_htf_max_nset, self->data_ptr);
}

static PyObject *
Receiver_get_mat_tube(ReceiverObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Receiver_mat_tube_nget, self->data_ptr);
}

static int
Receiver_set_mat_tube(ReceiverObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Receiver_mat_tube_nset, self->data_ptr);
}

static PyObject *
Receiver_get_rec_htf(ReceiverObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Receiver_rec_htf_nget, self->data_ptr);
}

static int
Receiver_set_rec_htf(ReceiverObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Receiver_rec_htf_nset, self->data_ptr);
}

static PyObject *
Receiver_get_rec_qf_delay(ReceiverObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Receiver_rec_qf_delay_nget, self->data_ptr);
}

static int
Receiver_set_rec_qf_delay(ReceiverObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Receiver_rec_qf_delay_nset, self->data_ptr);
}

static PyObject *
Receiver_get_rec_su_delay(ReceiverObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Receiver_rec_su_delay_nget, self->data_ptr);
}

static int
Receiver_set_rec_su_delay(ReceiverObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Receiver_rec_su_delay_nset, self->data_ptr);
}

static PyObject *
Receiver_get_receiver_type(ReceiverObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Receiver_receiver_type_nget, self->data_ptr);
}

static int
Receiver_set_receiver_type(ReceiverObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Receiver_receiver_type_nset, self->data_ptr);
}

static PyObject *
Receiver_get_th_tube(ReceiverObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Receiver_th_tube_nget, self->data_ptr);
}

static int
Receiver_set_th_tube(ReceiverObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Receiver_th_tube_nset, self->data_ptr);
}

static PyGetSetDef Receiver_getset[] = {
{"A_sf", (getter)Receiver_get_A_sf,(setter)Receiver_set_A_sf,
	PyDoc_STR("*float*: Solar Field Area [m^2]\n\n*Required*: True"),
 	NULL},
{"D_rec", (getter)Receiver_get_D_rec,(setter)Receiver_set_D_rec,
	PyDoc_STR("*float*: The overall outer diameter of the receiver [m]\n\n*Required*: True"),
 	NULL},
{"Flow_type", (getter)Receiver_get_Flow_type,(setter)Receiver_set_Flow_type,
	PyDoc_STR("*float*: A flag indicating which flow pattern is used\n\n*Required*: True"),
 	NULL},
{"H_rec", (getter)Receiver_get_H_rec,(setter)Receiver_set_H_rec,
	PyDoc_STR("*float*: The height of the receiver [m]\n\n*Required*: True"),
 	NULL},
{"N_panels", (getter)Receiver_get_N_panels,(setter)Receiver_set_N_panels,
	PyDoc_STR("*float*: Number of individual panels on the receiver\n\n*Constraints*: INTEGER\n\n*Required*: True"),
 	NULL},
{"Q_rec_des", (getter)Receiver_get_Q_rec_des,(setter)Receiver_set_Q_rec_des,
	PyDoc_STR("*float*: Design-point receiver thermal power output [MWt]\n\n*Required*: True"),
 	NULL},
{"THT", (getter)Receiver_get_THT,(setter)Receiver_set_THT,
	PyDoc_STR("*float*: The height of the tower (hel. pivot to rec equator) [m]\n\n*Required*: True"),
 	NULL},
{"T_htf_cold_des", (getter)Receiver_get_T_htf_cold_des,(setter)Receiver_set_T_htf_cold_des,
	PyDoc_STR("*float*: Cold HTF inlet temperature at design conditions [C]\n\n*Required*: True"),
 	NULL},
{"T_htf_hot_des", (getter)Receiver_get_T_htf_hot_des,(setter)Receiver_set_T_htf_hot_des,
	PyDoc_STR("*float*: Hot HTF outlet temperature at design conditions [C]\n\n*Required*: True"),
 	NULL},
{"crossover_shift", (getter)Receiver_get_crossover_shift,(setter)Receiver_set_crossover_shift,
	PyDoc_STR("*float*: No. panels shift in receiver crossover position\n\n*Required*: set to 0 if not provided."),
 	NULL},
{"d_tube_out", (getter)Receiver_get_d_tube_out,(setter)Receiver_set_d_tube_out,
	PyDoc_STR("*float*: The outer diameter of an individual receiver tube [mm]\n\n*Required*: True"),
 	NULL},
{"epsilon", (getter)Receiver_get_epsilon,(setter)Receiver_set_epsilon,
	PyDoc_STR("*float*: The emissivity of the receiver surface coating\n\n*Required*: True"),
 	NULL},
{"eta_pump", (getter)Receiver_get_eta_pump,(setter)Receiver_set_eta_pump,
	PyDoc_STR("*float*: Receiver HTF pump efficiency\n\n*Required*: True"),
 	NULL},
{"f_rec_min", (getter)Receiver_get_f_rec_min,(setter)Receiver_set_f_rec_min,
	PyDoc_STR("*float*: Minimum receiver mass flow rate turn down fraction\n\n*Required*: True"),
 	NULL},
{"field_fl_props", (getter)Receiver_get_field_fl_props,(setter)Receiver_set_field_fl_props,
	PyDoc_STR("*sequence[sequence]*: User defined field fluid property data [-]\n\n*Required*: True"),
 	NULL},
{"hl_ffact", (getter)Receiver_get_hl_ffact,(setter)Receiver_set_hl_ffact,
	PyDoc_STR("*float*: The heat loss factor (thermal loss fudge factor)\n\n*Required*: True"),
 	NULL},
{"m_dot_htf_max", (getter)Receiver_get_m_dot_htf_max,(setter)Receiver_set_m_dot_htf_max,
	PyDoc_STR("*float*: Maximum receiver mass flow rate [kg/hr]\n\n*Required*: True"),
 	NULL},
{"mat_tube", (getter)Receiver_get_mat_tube,(setter)Receiver_set_mat_tube,
	PyDoc_STR("*float*: The material name of the receiver tubes\n\n*Required*: True"),
 	NULL},
{"rec_htf", (getter)Receiver_get_rec_htf,(setter)Receiver_set_rec_htf,
	PyDoc_STR("*float*: The name of the HTF used in the receiver\n\n*Required*: True"),
 	NULL},
{"rec_qf_delay", (getter)Receiver_get_rec_qf_delay,(setter)Receiver_set_rec_qf_delay,
	PyDoc_STR("*float*: Energy-based rcvr startup delay (fraction of rated thermal power)\n\n*Required*: True"),
 	NULL},
{"rec_su_delay", (getter)Receiver_get_rec_su_delay,(setter)Receiver_set_rec_su_delay,
	PyDoc_STR("*float*: Fixed startup delay time for the receiver [hr]\n\n*Required*: True"),
 	NULL},
{"receiver_type", (getter)Receiver_get_receiver_type,(setter)Receiver_set_receiver_type,
	PyDoc_STR("*float*: External=0, Cavity=1\n\n*Constraints*: INTEGER\n\n*Required*: True"),
 	NULL},
{"th_tube", (getter)Receiver_get_th_tube,(setter)Receiver_set_th_tube,
	PyDoc_STR("*float*: The wall thickness of a single receiver tube [mm]\n\n*Required*: True"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject Receiver_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Tcsiscc.Receiver",             /*tp_name*/
		sizeof(ReceiverObject),          /*tp_basicsize*/
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
		Receiver_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		Receiver_getset,          /*tp_getset*/
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
	SAM_Tcsiscc   data_ptr;
} PowerblockObject;

static PyTypeObject Powerblock_Type;

static PyObject *
Powerblock_new(SAM_Tcsiscc data_ptr)
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

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Tcsiscc", "Powerblock")){
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
Powerblock_get_elev(PowerblockObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Powerblock_elev_nget, self->data_ptr);
}

static int
Powerblock_set_elev(PowerblockObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Powerblock_elev_nset, self->data_ptr);
}

static PyObject *
Powerblock_get_ngcc_model(PowerblockObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Powerblock_ngcc_model_nget, self->data_ptr);
}

static int
Powerblock_set_ngcc_model(PowerblockObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Powerblock_ngcc_model_nset, self->data_ptr);
}

static PyObject *
Powerblock_get_pinch_point_coldside(PowerblockObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Powerblock_pinch_point_coldside_nget, self->data_ptr);
}

static int
Powerblock_set_pinch_point_coldside(PowerblockObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Powerblock_pinch_point_coldside_nset, self->data_ptr);
}

static PyObject *
Powerblock_get_pinch_point_hotside(PowerblockObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Powerblock_pinch_point_hotside_nget, self->data_ptr);
}

static int
Powerblock_set_pinch_point_hotside(PowerblockObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Powerblock_pinch_point_hotside_nset, self->data_ptr);
}

static PyObject *
Powerblock_get_q_pb_design(PowerblockObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Powerblock_q_pb_design_nget, self->data_ptr);
}

static int
Powerblock_set_q_pb_design(PowerblockObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Powerblock_q_pb_design_nset, self->data_ptr);
}

static PyGetSetDef Powerblock_getset[] = {
{"elev", (getter)Powerblock_get_elev,(setter)Powerblock_set_elev,
	PyDoc_STR("*float*: Plant elevation [m]\n\n*Required*: True"),
 	NULL},
{"ngcc_model", (getter)Powerblock_get_ngcc_model,(setter)Powerblock_set_ngcc_model,
	PyDoc_STR("*float*: 1: NREL, 2: GE\n\n*Required*: True"),
 	NULL},
{"pinch_point_coldside", (getter)Powerblock_get_pinch_point_coldside,(setter)Powerblock_set_pinch_point_coldside,
	PyDoc_STR("*float*: Cold side HX pinch point [C]\n\n*Required*: True"),
 	NULL},
{"pinch_point_hotside", (getter)Powerblock_get_pinch_point_hotside,(setter)Powerblock_set_pinch_point_hotside,
	PyDoc_STR("*float*: Hot side temperature HX temperature difference [C]\n\n*Required*: True"),
 	NULL},
{"q_pb_design", (getter)Powerblock_get_q_pb_design,(setter)Powerblock_set_q_pb_design,
	PyDoc_STR("*float*: Design point power block thermal power [MWt]\n\n*Required*: True"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject Powerblock_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Tcsiscc.Powerblock",             /*tp_name*/
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
	 * Parasitics Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Tcsiscc   data_ptr;
} ParasiticsObject;

static PyTypeObject Parasitics_Type;

static PyObject *
Parasitics_new(SAM_Tcsiscc data_ptr)
{
	PyObject* new_obj = Parasitics_Type.tp_alloc(&Parasitics_Type,0);

	ParasiticsObject* Parasitics_obj = (ParasiticsObject*)new_obj;

	Parasitics_obj->data_ptr = data_ptr;

	return new_obj;
}

/* Parasitics methods */

static PyObject *
Parasitics_assign(ParasiticsObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Tcsiscc", "Parasitics")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
Parasitics_export(ParasiticsObject *self, PyObject *args)
{
	PyTypeObject* tp = &Parasitics_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef Parasitics_methods[] = {
		{"assign",            (PyCFunction)Parasitics_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``Parasitics_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)Parasitics_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
Parasitics_get_Q_rec_des(ParasiticsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Parasitics_Q_rec_des_nget, self->data_ptr);
}

static int
Parasitics_set_Q_rec_des(ParasiticsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Parasitics_Q_rec_des_nset, self->data_ptr);
}

static PyObject *
Parasitics_get_W_dot_solar_des(ParasiticsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Parasitics_W_dot_solar_des_nget, self->data_ptr);
}

static int
Parasitics_set_W_dot_solar_des(ParasiticsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Parasitics_W_dot_solar_des_nset, self->data_ptr);
}

static PyObject *
Parasitics_get_bop_par(ParasiticsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Parasitics_bop_par_nget, self->data_ptr);
}

static int
Parasitics_set_bop_par(ParasiticsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Parasitics_bop_par_nset, self->data_ptr);
}

static PyObject *
Parasitics_get_bop_par_0(ParasiticsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Parasitics_bop_par_0_nget, self->data_ptr);
}

static int
Parasitics_set_bop_par_0(ParasiticsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Parasitics_bop_par_0_nset, self->data_ptr);
}

static PyObject *
Parasitics_get_bop_par_1(ParasiticsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Parasitics_bop_par_1_nget, self->data_ptr);
}

static int
Parasitics_set_bop_par_1(ParasiticsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Parasitics_bop_par_1_nset, self->data_ptr);
}

static PyObject *
Parasitics_get_bop_par_2(ParasiticsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Parasitics_bop_par_2_nget, self->data_ptr);
}

static int
Parasitics_set_bop_par_2(ParasiticsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Parasitics_bop_par_2_nset, self->data_ptr);
}

static PyObject *
Parasitics_get_bop_par_f(ParasiticsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Parasitics_bop_par_f_nget, self->data_ptr);
}

static int
Parasitics_set_bop_par_f(ParasiticsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Parasitics_bop_par_f_nset, self->data_ptr);
}

static PyObject *
Parasitics_get_fossil_output(ParasiticsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Parasitics_fossil_output_nget, self->data_ptr);
}

static int
Parasitics_set_fossil_output(ParasiticsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Parasitics_fossil_output_nset, self->data_ptr);
}

static PyObject *
Parasitics_get_pb_fixed_par(ParasiticsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Parasitics_pb_fixed_par_nget, self->data_ptr);
}

static int
Parasitics_set_pb_fixed_par(ParasiticsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Parasitics_pb_fixed_par_nset, self->data_ptr);
}

static PyObject *
Parasitics_get_pb_pump_coef(ParasiticsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Parasitics_pb_pump_coef_nget, self->data_ptr);
}

static int
Parasitics_set_pb_pump_coef(ParasiticsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Parasitics_pb_pump_coef_nset, self->data_ptr);
}

static PyObject *
Parasitics_get_piping_length(ParasiticsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Parasitics_piping_length_nget, self->data_ptr);
}

static int
Parasitics_set_piping_length(ParasiticsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Parasitics_piping_length_nset, self->data_ptr);
}

static PyObject *
Parasitics_get_piping_length_const(ParasiticsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Parasitics_piping_length_const_nget, self->data_ptr);
}

static int
Parasitics_set_piping_length_const(ParasiticsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Parasitics_piping_length_const_nset, self->data_ptr);
}

static PyObject *
Parasitics_get_piping_length_mult(ParasiticsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Parasitics_piping_length_mult_nget, self->data_ptr);
}

static int
Parasitics_set_piping_length_mult(ParasiticsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Parasitics_piping_length_mult_nset, self->data_ptr);
}

static PyObject *
Parasitics_get_piping_loss(ParasiticsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Parasitics_piping_loss_nget, self->data_ptr);
}

static int
Parasitics_set_piping_loss(ParasiticsObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Tcsiscc_Parasitics_piping_loss_nset, self->data_ptr);
}

static PyGetSetDef Parasitics_getset[] = {
{"Q_rec_des", (getter)Parasitics_get_Q_rec_des,(setter)Parasitics_set_Q_rec_des,
	PyDoc_STR("*float*: Design point solar field thermal output [MW]\n\n*Required*: True"),
 	NULL},
{"W_dot_solar_des", (getter)Parasitics_get_W_dot_solar_des,(setter)Parasitics_set_W_dot_solar_des,
	PyDoc_STR("*float*: Solar contribution to cycle output at design [MWe]\n\n*Required*: True"),
 	NULL},
{"bop_par", (getter)Parasitics_get_bop_par,(setter)Parasitics_set_bop_par,
	PyDoc_STR("*float*: Balance of plant parasitic power fraction [MWe/MWcap]\n\n*Required*: True"),
 	NULL},
{"bop_par_0", (getter)Parasitics_get_bop_par_0,(setter)Parasitics_set_bop_par_0,
	PyDoc_STR("*float*: Balance of plant parasitic power fraction - const coeff [none]\n\n*Required*: True"),
 	NULL},
{"bop_par_1", (getter)Parasitics_get_bop_par_1,(setter)Parasitics_set_bop_par_1,
	PyDoc_STR("*float*: Balance of plant parasitic power fraction - linear coeff [none]\n\n*Required*: True"),
 	NULL},
{"bop_par_2", (getter)Parasitics_get_bop_par_2,(setter)Parasitics_set_bop_par_2,
	PyDoc_STR("*float*: Balance of plant parasitic power fraction - quadratic coeff [none]\n\n*Required*: True"),
 	NULL},
{"bop_par_f", (getter)Parasitics_get_bop_par_f,(setter)Parasitics_set_bop_par_f,
	PyDoc_STR("*float*: Balance of plant parasitic power fraction - mult frac [none]\n\n*Required*: True"),
 	NULL},
{"fossil_output", (getter)Parasitics_get_fossil_output,(setter)Parasitics_set_fossil_output,
	PyDoc_STR("*float*: Fossil-only cycle output at design [MWe]\n\n*Required*: True"),
 	NULL},
{"pb_fixed_par", (getter)Parasitics_get_pb_fixed_par,(setter)Parasitics_set_pb_fixed_par,
	PyDoc_STR("*float*: Fixed parasitic load - runs at all times [MWe/MWcap]\n\n*Required*: True"),
 	NULL},
{"pb_pump_coef", (getter)Parasitics_get_pb_pump_coef,(setter)Parasitics_set_pb_pump_coef,
	PyDoc_STR("*float*: Required pumping power for HTF through power block [kJ/kg]\n\n*Required*: True"),
 	NULL},
{"piping_length", (getter)Parasitics_get_piping_length,(setter)Parasitics_set_piping_length,
	PyDoc_STR("*float*: Total length of exposed piping [m]\n\n*Required*: True"),
 	NULL},
{"piping_length_const", (getter)Parasitics_get_piping_length_const,(setter)Parasitics_set_piping_length_const,
	PyDoc_STR("*float*: Piping constant length [m]\n\n*Required*: True"),
 	NULL},
{"piping_length_mult", (getter)Parasitics_get_piping_length_mult,(setter)Parasitics_set_piping_length_mult,
	PyDoc_STR("*float*: Piping length multiplier\n\n*Required*: True"),
 	NULL},
{"piping_loss", (getter)Parasitics_get_piping_loss,(setter)Parasitics_set_piping_loss,
	PyDoc_STR("*float*: Thermal loss per meter of piping [Wt/m]\n\n*Required*: True"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject Parasitics_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Tcsiscc.Parasitics",             /*tp_name*/
		sizeof(ParasiticsObject),          /*tp_basicsize*/
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
		Parasitics_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		Parasitics_getset,          /*tp_getset*/
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
	SAM_Tcsiscc   data_ptr;
} OutputsObject;

static PyTypeObject Outputs_Type;

static PyObject *
Outputs_new(SAM_Tcsiscc data_ptr)
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

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Tcsiscc", "Outputs")){
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
Outputs_get_P_fixed(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsiscc_Outputs_P_fixed_aget, self->data_ptr);
}

static PyObject *
Outputs_get_P_plant_balance_tot(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsiscc_Outputs_P_plant_balance_tot_aget, self->data_ptr);
}

static PyObject *
Outputs_get_Q_dot_max(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsiscc_Outputs_Q_dot_max_aget, self->data_ptr);
}

static PyObject *
Outputs_get_Q_solar_total(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsiscc_Outputs_Q_solar_total_aget, self->data_ptr);
}

static PyObject *
Outputs_get_Q_thermal(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsiscc_Outputs_Q_thermal_aget, self->data_ptr);
}

static PyObject *
Outputs_get_T_htf_cold(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsiscc_Outputs_T_htf_cold_aget, self->data_ptr);
}

static PyObject *
Outputs_get_T_salt_hot(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsiscc_Outputs_T_salt_hot_aget, self->data_ptr);
}

static PyObject *
Outputs_get_T_st_cold(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsiscc_Outputs_T_st_cold_aget, self->data_ptr);
}

static PyObject *
Outputs_get_T_st_hot(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsiscc_Outputs_T_st_hot_aget, self->data_ptr);
}

static PyObject *
Outputs_get_W_dot_pc_fossil(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsiscc_Outputs_W_dot_pc_fossil_aget, self->data_ptr);
}

static PyObject *
Outputs_get_W_dot_pc_hybrid(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsiscc_Outputs_W_dot_pc_hybrid_aget, self->data_ptr);
}

static PyObject *
Outputs_get_W_dot_plant_fossil(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsiscc_Outputs_W_dot_plant_fossil_aget, self->data_ptr);
}

static PyObject *
Outputs_get_W_dot_plant_hybrid(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsiscc_Outputs_W_dot_plant_hybrid_aget, self->data_ptr);
}

static PyObject *
Outputs_get_W_dot_plant_solar(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsiscc_Outputs_W_dot_plant_solar_aget, self->data_ptr);
}

static PyObject *
Outputs_get_W_dot_pump(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsiscc_Outputs_W_dot_pump_aget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_energy(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Outputs_annual_energy_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_fuel_usage(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Outputs_annual_fuel_usage_nget, self->data_ptr);
}

static PyObject *
Outputs_get_beam(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsiscc_Outputs_beam_aget, self->data_ptr);
}

static PyObject *
Outputs_get_capacity_factor(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Outputs_capacity_factor_nget, self->data_ptr);
}

static PyObject *
Outputs_get_eta_field(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsiscc_Outputs_eta_field_aget, self->data_ptr);
}

static PyObject *
Outputs_get_eta_fuel(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsiscc_Outputs_eta_fuel_aget, self->data_ptr);
}

static PyObject *
Outputs_get_eta_solar_use(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsiscc_Outputs_eta_solar_use_aget, self->data_ptr);
}

static PyObject *
Outputs_get_eta_therm(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsiscc_Outputs_eta_therm_aget, self->data_ptr);
}

static PyObject *
Outputs_get_f_timestep(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsiscc_Outputs_f_timestep_aget, self->data_ptr);
}

static PyObject *
Outputs_get_field_eff_adj(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsiscc_Outputs_field_eff_adj_aget, self->data_ptr);
}

static PyObject *
Outputs_get_fuel_use(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsiscc_Outputs_fuel_use_aget, self->data_ptr);
}

static PyObject *
Outputs_get_gen(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsiscc_Outputs_gen_aget, self->data_ptr);
}

static PyObject *
Outputs_get_hour(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsiscc_Outputs_hour_aget, self->data_ptr);
}

static PyObject *
Outputs_get_kwh_per_kw(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Outputs_kwh_per_kw_nget, self->data_ptr);
}

static PyObject *
Outputs_get_m_dot_salt_tot(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsiscc_Outputs_m_dot_salt_tot_aget, self->data_ptr);
}

static PyObject *
Outputs_get_m_dot_ss(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsiscc_Outputs_m_dot_ss_aget, self->data_ptr);
}

static PyObject *
Outputs_get_m_dot_steam(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsiscc_Outputs_m_dot_steam_aget, self->data_ptr);
}

static PyObject *
Outputs_get_month(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsiscc_Outputs_month_aget, self->data_ptr);
}

static PyObject *
Outputs_get_pparasi(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsiscc_Outputs_pparasi_aget, self->data_ptr);
}

static PyObject *
Outputs_get_pres(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsiscc_Outputs_pres_aget, self->data_ptr);
}

static PyObject *
Outputs_get_q_conv_sum(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsiscc_Outputs_q_conv_sum_aget, self->data_ptr);
}

static PyObject *
Outputs_get_q_rad_sum(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsiscc_Outputs_q_rad_sum_aget, self->data_ptr);
}

static PyObject *
Outputs_get_q_startup(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsiscc_Outputs_q_startup_aget, self->data_ptr);
}

static PyObject *
Outputs_get_solar_fraction(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsiscc_Outputs_solar_fraction_aget, self->data_ptr);
}

static PyObject *
Outputs_get_solazi(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsiscc_Outputs_solazi_aget, self->data_ptr);
}

static PyObject *
Outputs_get_solzen(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsiscc_Outputs_solzen_aget, self->data_ptr);
}

static PyObject *
Outputs_get_system_heat_rate(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Tcsiscc_Outputs_system_heat_rate_nget, self->data_ptr);
}

static PyObject *
Outputs_get_tdry(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsiscc_Outputs_tdry_aget, self->data_ptr);
}

static PyObject *
Outputs_get_twet(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsiscc_Outputs_twet_aget, self->data_ptr);
}

static PyObject *
Outputs_get_wspd(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Tcsiscc_Outputs_wspd_aget, self->data_ptr);
}

static PyGetSetDef Outputs_getset[] = {
{"P_fixed", (getter)Outputs_get_P_fixed,(setter)0,
	PyDoc_STR("*sequence*: Parasitic power fixed load [MWe]"),
 	NULL},
{"P_plant_balance_tot", (getter)Outputs_get_P_plant_balance_tot,(setter)0,
	PyDoc_STR("*sequence*: Parasitic power generation-dependent load [MWe]"),
 	NULL},
{"Q_dot_max", (getter)Outputs_get_Q_dot_max,(setter)0,
	PyDoc_STR("*sequence*: Cycle max allowable thermal power to NGCC [MWt]"),
 	NULL},
{"Q_solar_total", (getter)Outputs_get_Q_solar_total,(setter)0,
	PyDoc_STR("*sequence*: Receiver thermal power absorbed [MWt]"),
 	NULL},
{"Q_thermal", (getter)Outputs_get_Q_thermal,(setter)0,
	PyDoc_STR("*sequence*: Receiver thermal power to HTF [MWt]"),
 	NULL},
{"T_htf_cold", (getter)Outputs_get_T_htf_cold,(setter)0,
	PyDoc_STR("*sequence*: Receiver HTF temperature in [C]"),
 	NULL},
{"T_salt_hot", (getter)Outputs_get_T_salt_hot,(setter)0,
	PyDoc_STR("*sequence*: Receiver HTF temperature out [C]"),
 	NULL},
{"T_st_cold", (getter)Outputs_get_T_st_cold,(setter)0,
	PyDoc_STR("*sequence*: Cycle steam temp from NGCC to HX [C]"),
 	NULL},
{"T_st_hot", (getter)Outputs_get_T_st_hot,(setter)0,
	PyDoc_STR("*sequence*: Cycle steam temp from HX back to NGCC [C]"),
 	NULL},
{"W_dot_pc_fossil", (getter)Outputs_get_W_dot_pc_fossil,(setter)0,
	PyDoc_STR("*sequence*: Cycle net output only considering fossil power [MWe]"),
 	NULL},
{"W_dot_pc_hybrid", (getter)Outputs_get_W_dot_pc_hybrid,(setter)0,
	PyDoc_STR("*sequence*: Cycle net output including solar power [MWe]"),
 	NULL},
{"W_dot_plant_fossil", (getter)Outputs_get_W_dot_plant_fossil,(setter)0,
	PyDoc_STR("*sequence*: Plant net output only considering fossil power & parasitics [MWe]"),
 	NULL},
{"W_dot_plant_hybrid", (getter)Outputs_get_W_dot_plant_hybrid,(setter)0,
	PyDoc_STR("*sequence*: Plant net output including solar power & parasitics [MWe]"),
 	NULL},
{"W_dot_plant_solar", (getter)Outputs_get_W_dot_plant_solar,(setter)0,
	PyDoc_STR("*sequence*: Plant net output attributable to solar [MWe]"),
 	NULL},
{"W_dot_pump", (getter)Outputs_get_W_dot_pump,(setter)0,
	PyDoc_STR("*sequence*: Parasitic power receiver HTF pump [MWe]"),
 	NULL},
{"annual_energy", (getter)Outputs_get_annual_energy,(setter)0,
	PyDoc_STR("*float*: Annual Energy [kW]"),
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
{"eta_field", (getter)Outputs_get_eta_field,(setter)0,
	PyDoc_STR("*sequence*: Field optical efficiency"),
 	NULL},
{"eta_fuel", (getter)Outputs_get_eta_fuel,(setter)0,
	PyDoc_STR("*sequence*: Plant efficiency of fossil only operation (LHV basis) [%]"),
 	NULL},
{"eta_solar_use", (getter)Outputs_get_eta_solar_use,(setter)0,
	PyDoc_STR("*sequence*: Plant solar use efficiency considering parasitics [-]"),
 	NULL},
{"eta_therm", (getter)Outputs_get_eta_therm,(setter)0,
	PyDoc_STR("*sequence*: Receiver thermal efficiency"),
 	NULL},
{"f_timestep", (getter)Outputs_get_f_timestep,(setter)0,
	PyDoc_STR("*sequence*: Receiver operating fraction after startup"),
 	NULL},
{"field_eff_adj", (getter)Outputs_get_field_eff_adj,(setter)0,
	PyDoc_STR("*sequence*: Solar field efficiency w/ defocusing"),
 	NULL},
{"fuel_use", (getter)Outputs_get_fuel_use,(setter)0,
	PyDoc_STR("*sequence*: Cycle natural gas used during timestep [MMBTU]"),
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
{"m_dot_salt_tot", (getter)Outputs_get_m_dot_salt_tot,(setter)0,
	PyDoc_STR("*sequence*: Receiver mass flow rate, derated for startup [kg/s]"),
 	NULL},
{"m_dot_ss", (getter)Outputs_get_m_dot_ss,(setter)0,
	PyDoc_STR("*sequence*: Receiver mass flow rate, steady state [kg/s]"),
 	NULL},
{"m_dot_steam", (getter)Outputs_get_m_dot_steam,(setter)0,
	PyDoc_STR("*sequence*: Cycle solar steam mass flow rate [kg/hr]"),
 	NULL},
{"month", (getter)Outputs_get_month,(setter)0,
	PyDoc_STR("*sequence*: Resource Month"),
 	NULL},
{"pparasi", (getter)Outputs_get_pparasi,(setter)0,
	PyDoc_STR("*sequence*: Parasitic power heliostat drives [MWe]"),
 	NULL},
{"pres", (getter)Outputs_get_pres,(setter)0,
	PyDoc_STR("*sequence*: Resource Pressure [mbar]"),
 	NULL},
{"q_conv_sum", (getter)Outputs_get_q_conv_sum,(setter)0,
	PyDoc_STR("*sequence*: Receiver thermal power loss to convection [MWt]"),
 	NULL},
{"q_rad_sum", (getter)Outputs_get_q_rad_sum,(setter)0,
	PyDoc_STR("*sequence*: Receiver thermal power loss to radiation [MWt]"),
 	NULL},
{"q_startup", (getter)Outputs_get_q_startup,(setter)0,
	PyDoc_STR("*sequence*: Receiver startup power [MWt]"),
 	NULL},
{"solar_fraction", (getter)Outputs_get_solar_fraction,(setter)0,
	PyDoc_STR("*sequence*: Plant solar fraction [-]"),
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
		"Tcsiscc.Outputs",             /*tp_name*/
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
 * Tcsiscc
 */

typedef struct {
	PyObject_HEAD
	PyObject            *x_attr;        /* Attributes dictionary */
	SAM_Tcsiscc   data_ptr;
} TcsisccObject;

static PyTypeObject Tcsiscc_Type;

#define TcsisccObject_Check(v)      (Py_TYPE(v) == &Tcsiscc_Type)

static TcsisccObject *
newTcsisccObject(void* data_ptr)
{
	TcsisccObject *self;
	self = PyObject_New(TcsisccObject, &Tcsiscc_Type);

	PySAM_TECH_ATTR("Tcsiscc", SAM_Tcsiscc_construct)

	PyObject* Weather_obj = Weather_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "Weather", Weather_obj);
	Py_DECREF(Weather_obj);

	PyObject* MoltenSaltTower_obj = MoltenSaltTower_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "MoltenSaltTower", MoltenSaltTower_obj);
	Py_DECREF(MoltenSaltTower_obj);

	PyObject* Heliostat_obj = Heliostat_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "Heliostat", Heliostat_obj);
	Py_DECREF(Heliostat_obj);

	PyObject* Receiver_obj = Receiver_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "Receiver", Receiver_obj);
	Py_DECREF(Receiver_obj);

	PyObject* Powerblock_obj = Powerblock_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "Powerblock", Powerblock_obj);
	Py_DECREF(Powerblock_obj);

	PyObject* Parasitics_obj = Parasitics_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "Parasitics", Parasitics_obj);
	Py_DECREF(Parasitics_obj);

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

/* Tcsiscc methods */

static void
Tcsiscc_dealloc(TcsisccObject *self)
{
	Py_XDECREF(self->x_attr);
	SAM_Tcsiscc_destruct(self->data_ptr);
	PyObject_Del(self);
}


static PyObject *
Tcsiscc_execute(TcsisccObject *self, PyObject *args)
{
	int verbosity = 0;

	if (!PyArg_ParseTuple(args, "|i", &verbosity))
		return NULL;

	SAM_error error = new_error();
	SAM_Tcsiscc_execute(self->data_ptr, verbosity, &error);
	if (PySAM_has_error(error )) return NULL;

	Py_INCREF(Py_None);
	return Py_None;
}


static PyObject *
Tcsiscc_assign(TcsisccObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_nested_dict((PyObject*)self, self->x_attr, self->data_ptr, dict, "Tcsiscc"))
		return NULL;

	Py_INCREF(Py_None);
	return Py_None;
}


static PyObject *
Tcsiscc_export(TcsisccObject *self, PyObject *args)
{
	return PySAM_export_to_nested_dict((PyObject *) self, self->x_attr);
}

static PyMethodDef Tcsiscc_methods[] = {
		{"execute",            (PyCFunction)Tcsiscc_execute,  METH_VARARGS,
				PyDoc_STR("execute(int verbosity) -> None\n Execute simulation with verbosity level 0 (default) or 1")},
		{"assign",            (PyCFunction)Tcsiscc_assign,  METH_VARARGS,
				PyDoc_STR("assign(dict) -> None\n Assign attributes from nested dictionary, except for Outputs\n\n``nested_dict = { 'Weather': { var: val, ...}, ...}``")},
		{"export",            (PyCFunction)Tcsiscc_export,  METH_VARARGS,
				PyDoc_STR("export() -> dict\n Export attributes into nested dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
Tcsiscc_getattro(TcsisccObject *self, PyObject *name)
{
	return PySAM_get_attr((PyObject*) self, (PyObject*) self->x_attr, name);
}

static int
Tcsiscc_setattr(TcsisccObject *self, const char *name, PyObject *v)
{
	return PySAM_set_attr((PyObject*)self, (PyObject*)self->x_attr, name, v);
}

static PyTypeObject Tcsiscc_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Tcsiscc",            /*tp_name*/
		sizeof(TcsisccObject),/*tp_basicsize*/
		0,                          /*tp_itemsize*/
		/* methods */
		(destructor)Tcsiscc_dealloc,    /*tp_dealloc*/
		0,                          /*tp_print*/
		(getattrfunc)0,             /*tp_getattr*/
		(setattrfunc)Tcsiscc_setattr,   /*tp_setattr*/
		0,                          /*tp_reserved*/
		0,                          /*tp_repr*/
		0,                          /*tp_as_number*/
		0,                          /*tp_as_sequence*/
		0,                          /*tp_as_mapping*/
		0,                          /*tp_hash*/
		0,                          /*tp_call*/
		0,                          /*tp_str*/
		(getattrofunc)Tcsiscc_getattro, /*tp_getattro*/
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
		Tcsiscc_methods,      /*tp_methods*/
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


/* Function of no arguments returning new Tcsiscc object */

static PyObject *
Tcsiscc_new(PyObject *self, PyObject *args)
{
	TcsisccObject *rv;
	rv = newTcsisccObject(0);
	if (rv == NULL)
		return NULL;
	return (PyObject *)rv;
}

static PyObject *
Tcsiscc_wrap(PyObject *self, PyObject *args)
{
	TcsisccObject *rv;
	long long int ptr = 0;  // 64 bit arch
	if (!PyArg_ParseTuple(args, "L:wrap", &ptr)){
		PyErr_BadArgument();
		return NULL;
	}
	rv = newTcsisccObject((void*)ptr);
	if (rv == NULL)
		return NULL;
	return (PyObject *)rv;
}

static PyObject *
Tcsiscc_default(PyObject *self, PyObject *args)
{
	TcsisccObject *rv;
	char* def = 0;
	if (!PyArg_ParseTuple(args, "s:default", &def)){
		PyErr_BadArgument();
		return NULL;
	}
	rv = newTcsisccObject(0);
	if (rv == NULL)
		return NULL;

	PySAM_load_defaults((PyObject*)rv, rv->x_attr, rv->data_ptr, "Tcsiscc", def);

	return (PyObject *)rv;
}

/* ---------- */


/* List of functions defined in the module */

static PyMethodDef TcsisccModule_methods[] = {
		{"new",             Tcsiscc_new,         METH_VARARGS,
				PyDoc_STR("new() -> Tcsiscc")},
		{"default",             Tcsiscc_default,         METH_VARARGS,
				PyDoc_STR("default(config) -> Tcsiscc\n\nUse financial model-specific default attributes\n"
				"config options:\n\n- \"ISCCSingleOwner\"")},
		{"wrap",             Tcsiscc_wrap,         METH_VARARGS,
				PyDoc_STR("wrap(ssc_data_t) -> Tcsiscc\n\nUse existing PySSC data\n\n.. warning::\n\n	Do not call PySSC.data_free on the ssc_data_t provided to ``wrap``")},
		{NULL,              NULL}           /* sentinel */
};

PyDoc_STRVAR(module_doc,
			 "CSP molten salt power tower system with a natural gas combined cycle power plant");


static int
TcsisccModule_exec(PyObject *m)
{
	/* Finalize the type object including setting type of the new type
	 * object; doing it here is required for portability, too. */

	if (PySAM_load_lib(m) < 0) goto fail;
	if (PySAM_init_error(m) < 0) goto fail;

	Tcsiscc_Type.tp_dict = PyDict_New();
	if (!Tcsiscc_Type.tp_dict) { goto fail; }

	/// Add the AdjustmentFactors type object to Tcsiscc_Type
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
	PyDict_SetItemString(Tcsiscc_Type.tp_dict,
						 "AdjustmentFactors",
						 (PyObject*)AdjustmentFactors_Type);
	Py_DECREF(&AdjustmentFactors_Type);
	Py_XDECREF(AdjustmentFactors_Type);

	/// Add the Weather type object to Tcsiscc_Type
	if (PyType_Ready(&Weather_Type) < 0) { goto fail; }
	PyDict_SetItemString(Tcsiscc_Type.tp_dict,
				"Weather",
				(PyObject*)&Weather_Type);
	Py_DECREF(&Weather_Type);

	/// Add the MoltenSaltTower type object to Tcsiscc_Type
	if (PyType_Ready(&MoltenSaltTower_Type) < 0) { goto fail; }
	PyDict_SetItemString(Tcsiscc_Type.tp_dict,
				"MoltenSaltTower",
				(PyObject*)&MoltenSaltTower_Type);
	Py_DECREF(&MoltenSaltTower_Type);

	/// Add the Heliostat type object to Tcsiscc_Type
	if (PyType_Ready(&Heliostat_Type) < 0) { goto fail; }
	PyDict_SetItemString(Tcsiscc_Type.tp_dict,
				"Heliostat",
				(PyObject*)&Heliostat_Type);
	Py_DECREF(&Heliostat_Type);

	/// Add the Receiver type object to Tcsiscc_Type
	if (PyType_Ready(&Receiver_Type) < 0) { goto fail; }
	PyDict_SetItemString(Tcsiscc_Type.tp_dict,
				"Receiver",
				(PyObject*)&Receiver_Type);
	Py_DECREF(&Receiver_Type);

	/// Add the Powerblock type object to Tcsiscc_Type
	if (PyType_Ready(&Powerblock_Type) < 0) { goto fail; }
	PyDict_SetItemString(Tcsiscc_Type.tp_dict,
				"Powerblock",
				(PyObject*)&Powerblock_Type);
	Py_DECREF(&Powerblock_Type);

	/// Add the Parasitics type object to Tcsiscc_Type
	if (PyType_Ready(&Parasitics_Type) < 0) { goto fail; }
	PyDict_SetItemString(Tcsiscc_Type.tp_dict,
				"Parasitics",
				(PyObject*)&Parasitics_Type);
	Py_DECREF(&Parasitics_Type);

	/// Add the Outputs type object to Tcsiscc_Type
	if (PyType_Ready(&Outputs_Type) < 0) { goto fail; }
	PyDict_SetItemString(Tcsiscc_Type.tp_dict,
				"Outputs",
				(PyObject*)&Outputs_Type);
	Py_DECREF(&Outputs_Type);

	/// Add the Tcsiscc type object to the module
	if (PyType_Ready(&Tcsiscc_Type) < 0) { goto fail; }
	PyModule_AddObject(m,
				"Tcsiscc",
				(PyObject*)&Tcsiscc_Type);

	return 0;
	fail:
	Py_XDECREF(m);
	return -1;
}

static struct PyModuleDef_Slot TcsisccModule_slots[] = {
		{Py_mod_exec, TcsisccModule_exec},
		{0, NULL},
};

static struct PyModuleDef TcsisccModule = {
		PyModuleDef_HEAD_INIT,
		"Tcsiscc",
		module_doc,
		0,
		TcsisccModule_methods,
		TcsisccModule_slots,
		NULL,
		NULL,
		NULL
};

/* Export function for the module */

PyMODINIT_FUNC
PyInit_Tcsiscc(void)
{
	return PyModuleDef_Init(&TcsisccModule);
}