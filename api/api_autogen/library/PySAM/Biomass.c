#include <Python.h>

#include <SAM_Biomass.h>
#include <SAM_api.h>

#include "PySAM_utils.h"



	/*
	 * Biopower Group
	 */ 

typedef struct {
	PyObject_HEAD
	SAM_Biomass   data_ptr;
} BiopowerObject;

static PyTypeObject Biopower_Type;

static PyObject *
Biopower_new(SAM_Biomass data_ptr)
{
	PyObject* new_obj = Biopower_Type.tp_alloc(&Biopower_Type,0);

	BiopowerObject* Biopower_obj = (BiopowerObject*)new_obj;

	Biopower_obj->data_ptr = data_ptr;

	return new_obj;
}

/* Biopower methods */

static PyObject *
Biopower_assign(BiopowerObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Biomass", "Biopower")){
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
Biopower_export(BiopowerObject *self, PyObject *args)
{
	PyTypeObject* tp = &Biopower_Type;
	PyObject* dict = PySAM_export_to_dict((PyObject *) self, tp);
	return dict;
}

static PyMethodDef Biopower_methods[] = {
		{"assign",            (PyCFunction)Biopower_assign,  METH_VARARGS,
			PyDoc_STR("assign() -> None\n Assign attributes from dictionary\n\n``Biopower_vals = { var: val, ...}``")},
		{"export",            (PyCFunction)Biopower_export,  METH_VARARGS,
			PyDoc_STR("export() -> dict\n Export attributes into dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
Biopower_get_biopwr_emissions_avoided_cred(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_emissions_avoided_cred_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_emissions_avoided_cred(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_emissions_avoided_cred_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_emissions_collection_fuel(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_emissions_collection_fuel_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_emissions_collection_fuel(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_emissions_collection_fuel_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_emissions_grid_intensity(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_emissions_grid_intensity_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_emissions_grid_intensity(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_emissions_grid_intensity_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_emissions_pre_chipopt(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_emissions_pre_chipopt_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_emissions_pre_chipopt(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_emissions_pre_chipopt_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_emissions_pre_grindopt(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_emissions_pre_grindopt_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_emissions_pre_grindopt(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_emissions_pre_grindopt_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_emissions_pre_pelletopt(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_emissions_pre_pelletopt_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_emissions_pre_pelletopt(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_emissions_pre_pelletopt_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_emissions_transport_fuel(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_emissions_transport_fuel_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_emissions_transport_fuel(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_emissions_transport_fuel_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_emissions_transport_legs(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_emissions_transport_legs_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_emissions_transport_legs(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_emissions_transport_legs_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_emissions_transport_long(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_emissions_transport_long_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_emissions_transport_long(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_emissions_transport_long_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_emissions_transport_longmiles(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_emissions_transport_longmiles_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_emissions_transport_longmiles(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_emissions_transport_longmiles_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_emissions_transport_longopt(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_emissions_transport_longopt_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_emissions_transport_longopt(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_emissions_transport_longopt_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_emissions_transport_predist(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_emissions_transport_predist_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_emissions_transport_predist(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_emissions_transport_predist_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_feedstock_additional_opt(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_feedstock_additional_opt_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_feedstock_additional_opt(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_feedstock_additional_opt_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_feedstock_bagasse_frac(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_feedstock_bagasse_frac_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_feedstock_bagasse_frac(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_feedstock_bagasse_frac_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_feedstock_bagasse_moisture(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_feedstock_bagasse_moisture_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_feedstock_bagasse_moisture(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_feedstock_bagasse_moisture_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_feedstock_barley_frac(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_feedstock_barley_frac_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_feedstock_barley_frac(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_feedstock_barley_frac_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_feedstock_barley_moisture(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_feedstock_barley_moisture_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_feedstock_barley_moisture(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_feedstock_barley_moisture_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_feedstock_bit_frac(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_feedstock_bit_frac_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_feedstock_bit_frac(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_feedstock_bit_frac_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_feedstock_bit_moisture(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_feedstock_bit_moisture_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_feedstock_bit_moisture(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_feedstock_bit_moisture_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_feedstock_collection_radius(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_feedstock_collection_radius_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_feedstock_collection_radius(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_feedstock_collection_radius_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_feedstock_feedstock1_c(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_feedstock_feedstock1_c_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_feedstock_feedstock1_c(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_feedstock_feedstock1_c_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_feedstock_feedstock1_frac(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_feedstock_feedstock1_frac_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_feedstock_feedstock1_frac(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_feedstock_feedstock1_frac_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_feedstock_feedstock1_h(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_feedstock_feedstock1_h_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_feedstock_feedstock1_h(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_feedstock_feedstock1_h_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_feedstock_feedstock1_hhv(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_feedstock_feedstock1_hhv_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_feedstock_feedstock1_hhv(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_feedstock_feedstock1_hhv_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_feedstock_feedstock1_moisture(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_feedstock_feedstock1_moisture_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_feedstock_feedstock1_moisture(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_feedstock_feedstock1_moisture_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_feedstock_feedstock1_resource(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_feedstock_feedstock1_resource_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_feedstock_feedstock1_resource(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_feedstock_feedstock1_resource_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_feedstock_feedstock2_c(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_feedstock_feedstock2_c_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_feedstock_feedstock2_c(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_feedstock_feedstock2_c_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_feedstock_feedstock2_frac(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_feedstock_feedstock2_frac_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_feedstock_feedstock2_frac(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_feedstock_feedstock2_frac_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_feedstock_feedstock2_h(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_feedstock_feedstock2_h_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_feedstock_feedstock2_h(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_feedstock_feedstock2_h_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_feedstock_feedstock2_hhv(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_feedstock_feedstock2_hhv_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_feedstock_feedstock2_hhv(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_feedstock_feedstock2_hhv_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_feedstock_feedstock2_moisture(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_feedstock_feedstock2_moisture_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_feedstock_feedstock2_moisture(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_feedstock_feedstock2_moisture_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_feedstock_feedstock2_resource(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_feedstock_feedstock2_resource_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_feedstock_feedstock2_resource(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_feedstock_feedstock2_resource_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_feedstock_forest_frac(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_feedstock_forest_frac_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_feedstock_forest_frac(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_feedstock_forest_frac_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_feedstock_forest_moisture(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_feedstock_forest_moisture_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_feedstock_forest_moisture(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_feedstock_forest_moisture_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_feedstock_herb_c(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_feedstock_herb_c_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_feedstock_herb_c(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_feedstock_herb_c_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_feedstock_herb_frac(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_feedstock_herb_frac_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_feedstock_herb_frac(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_feedstock_herb_frac_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_feedstock_herb_moisture(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_feedstock_herb_moisture_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_feedstock_herb_moisture(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_feedstock_herb_moisture_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_feedstock_lig_frac(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_feedstock_lig_frac_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_feedstock_lig_frac(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_feedstock_lig_frac_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_feedstock_lig_moisture(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_feedstock_lig_moisture_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_feedstock_lig_moisture(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_feedstock_lig_moisture_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_feedstock_mill_c(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_feedstock_mill_c_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_feedstock_mill_c(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_feedstock_mill_c_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_feedstock_mill_frac(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_feedstock_mill_frac_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_feedstock_mill_frac(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_feedstock_mill_frac_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_feedstock_mill_moisture(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_feedstock_mill_moisture_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_feedstock_mill_moisture(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_feedstock_mill_moisture_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_feedstock_rice_frac(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_feedstock_rice_frac_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_feedstock_rice_frac(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_feedstock_rice_frac_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_feedstock_rice_moisture(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_feedstock_rice_moisture_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_feedstock_rice_moisture(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_feedstock_rice_moisture_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_feedstock_stover_frac(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_feedstock_stover_frac_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_feedstock_stover_frac(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_feedstock_stover_frac_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_feedstock_stover_moisture(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_feedstock_stover_moisture_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_feedstock_stover_moisture(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_feedstock_stover_moisture_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_feedstock_subbit_frac(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_feedstock_subbit_frac_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_feedstock_subbit_frac(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_feedstock_subbit_frac_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_feedstock_subbit_moisture(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_feedstock_subbit_moisture_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_feedstock_subbit_moisture(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_feedstock_subbit_moisture_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_feedstock_total(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_feedstock_total_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_feedstock_total(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_feedstock_total_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_feedstock_total_biomass(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_feedstock_total_biomass_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_feedstock_total_biomass(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_feedstock_total_biomass_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_feedstock_total_biomass_c(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_feedstock_total_biomass_c_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_feedstock_total_biomass_c(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_feedstock_total_biomass_c_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_feedstock_total_c(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_feedstock_total_c_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_feedstock_total_c(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_feedstock_total_c_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_feedstock_total_coal(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_feedstock_total_coal_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_feedstock_total_coal(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_feedstock_total_coal_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_feedstock_total_h(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_feedstock_total_h_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_feedstock_total_h(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_feedstock_total_h_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_feedstock_total_hhv(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_feedstock_total_hhv_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_feedstock_total_hhv(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_feedstock_total_hhv_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_feedstock_total_lhv(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_feedstock_total_lhv_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_feedstock_total_lhv(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_feedstock_total_lhv_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_feedstock_total_moisture(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_feedstock_total_moisture_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_feedstock_total_moisture(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_feedstock_total_moisture_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_feedstock_urban_c(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_feedstock_urban_c_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_feedstock_urban_c(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_feedstock_urban_c_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_feedstock_urban_frac(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_feedstock_urban_frac_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_feedstock_urban_frac(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_feedstock_urban_frac_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_feedstock_urban_moisture(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_feedstock_urban_moisture_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_feedstock_urban_moisture(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_feedstock_urban_moisture_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_feedstock_wheat_frac(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_feedstock_wheat_frac_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_feedstock_wheat_frac(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_feedstock_wheat_frac_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_feedstock_wheat_moisture(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_feedstock_wheat_moisture_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_feedstock_wheat_moisture(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_feedstock_wheat_moisture_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_feedstock_woody_c(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_feedstock_woody_c_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_feedstock_woody_c(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_feedstock_woody_c_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_feedstock_woody_frac(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_feedstock_woody_frac_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_feedstock_woody_frac(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_feedstock_woody_frac_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_feedstock_woody_moisture(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_feedstock_woody_moisture_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_feedstock_woody_moisture(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_feedstock_woody_moisture_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_plant_boiler_air_feed(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_plant_boiler_air_feed_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_plant_boiler_air_feed(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_plant_boiler_air_feed_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_plant_boiler_cap_per_boiler(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_plant_boiler_cap_per_boiler_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_plant_boiler_cap_per_boiler(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_plant_boiler_cap_per_boiler_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_plant_boiler_flue_temp(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_plant_boiler_flue_temp_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_plant_boiler_flue_temp(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_plant_boiler_flue_temp_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_plant_boiler_num(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_plant_boiler_num_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_plant_boiler_num(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_plant_boiler_num_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_plant_boiler_over_design(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_plant_boiler_over_design_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_plant_boiler_over_design(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_plant_boiler_over_design_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_plant_boiler_steam_enthalpy(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_plant_boiler_steam_enthalpy_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_plant_boiler_steam_enthalpy(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_plant_boiler_steam_enthalpy_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_plant_boiler_steam_pressure(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_plant_boiler_steam_pressure_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_plant_boiler_steam_pressure(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_plant_boiler_steam_pressure_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_plant_combustor_type(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_plant_combustor_type_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_plant_combustor_type(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_plant_combustor_type_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_plant_cycle_design_temp(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_plant_cycle_design_temp_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_plant_cycle_design_temp(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_plant_cycle_design_temp_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_plant_disp_power(BiopowerObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Biomass_Biopower_biopwr_plant_disp_power_aget, self->data_ptr);
}

static int
Biopower_set_biopwr_plant_disp_power(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_array_setter(value, SAM_Biomass_Biopower_biopwr_plant_disp_power_aset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_plant_drying_method(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_plant_drying_method_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_plant_drying_method(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_plant_drying_method_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_plant_drying_spec(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_plant_drying_spec_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_plant_drying_spec(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_plant_drying_spec_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_plant_max_over_design(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_plant_max_over_design_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_plant_max_over_design(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_plant_max_over_design_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_plant_min_load(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_plant_min_load_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_plant_min_load(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_plant_min_load_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_plant_nameplate(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_plant_nameplate_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_plant_nameplate(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_plant_nameplate_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_plant_par_percent(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_plant_par_percent_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_plant_par_percent(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_plant_par_percent_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_plant_pl_eff_f0(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_plant_pl_eff_f0_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_plant_pl_eff_f0(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_plant_pl_eff_f0_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_plant_pl_eff_f1(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_plant_pl_eff_f1_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_plant_pl_eff_f1(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_plant_pl_eff_f1_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_plant_pl_eff_f2(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_plant_pl_eff_f2_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_plant_pl_eff_f2(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_plant_pl_eff_f2_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_plant_pl_eff_f3(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_plant_pl_eff_f3_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_plant_pl_eff_f3(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_plant_pl_eff_f3_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_plant_pl_eff_f4(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_plant_pl_eff_f4_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_plant_pl_eff_f4(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_plant_pl_eff_f4_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_plant_ramp_rate(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_plant_ramp_rate_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_plant_ramp_rate(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_plant_ramp_rate_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_plant_rated_eff(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_plant_rated_eff_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_plant_rated_eff(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_plant_rated_eff_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_plant_temp_corr_mode(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_plant_temp_corr_mode_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_plant_temp_corr_mode(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_plant_temp_corr_mode_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_plant_temp_eff_f0(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_plant_temp_eff_f0_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_plant_temp_eff_f0(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_plant_temp_eff_f0_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_plant_temp_eff_f1(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_plant_temp_eff_f1_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_plant_temp_eff_f1(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_plant_temp_eff_f1_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_plant_temp_eff_f2(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_plant_temp_eff_f2_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_plant_temp_eff_f2(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_plant_temp_eff_f2_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_plant_temp_eff_f3(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_plant_temp_eff_f3_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_plant_temp_eff_f3(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_plant_temp_eff_f3_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_plant_temp_eff_f4(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_plant_temp_eff_f4_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_plant_temp_eff_f4(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_plant_temp_eff_f4_nset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_plant_tou_grid(BiopowerObject *self, void *closure)
{
	return PySAM_string_getter(SAM_Biomass_Biopower_biopwr_plant_tou_grid_sget, self->data_ptr);
}

static int
Biopower_set_biopwr_plant_tou_grid(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_string_setter(value, SAM_Biomass_Biopower_biopwr_plant_tou_grid_sset, self->data_ptr);
}

static PyObject *
Biopower_get_biopwr_plant_tou_option(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_biopwr_plant_tou_option_nget, self->data_ptr);
}

static int
Biopower_set_biopwr_plant_tou_option(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_biopwr_plant_tou_option_nset, self->data_ptr);
}

static PyObject *
Biopower_get_file_name(BiopowerObject *self, void *closure)
{
	return PySAM_string_getter(SAM_Biomass_Biopower_file_name_sget, self->data_ptr);
}

static int
Biopower_set_file_name(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_string_setter(value, SAM_Biomass_Biopower_file_name_sset, self->data_ptr);
}

static PyObject *
Biopower_get_system_capacity(BiopowerObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Biopower_system_capacity_nget, self->data_ptr);
}

static int
Biopower_set_system_capacity(BiopowerObject *self, PyObject *value, void *closure)
{
	return PySAM_double_setter(value, SAM_Biomass_Biopower_system_capacity_nset, self->data_ptr);
}

static PyGetSetDef Biopower_getset[] = {
{"biopwr_emissions_avoided_cred", (getter)Biopower_get_biopwr_emissions_avoided_cred,(setter)Biopower_set_biopwr_emissions_avoided_cred,
	PyDoc_STR("*float*\n\n*Constraints*: INTEGER\n\n*Required*: True"),
 	NULL},
{"biopwr_emissions_collection_fuel", (getter)Biopower_get_biopwr_emissions_collection_fuel,(setter)Biopower_set_biopwr_emissions_collection_fuel,
	PyDoc_STR("*float*\n\n*Constraints*: INTEGER\n\n*Required*: True"),
 	NULL},
{"biopwr_emissions_grid_intensity", (getter)Biopower_get_biopwr_emissions_grid_intensity,(setter)Biopower_set_biopwr_emissions_grid_intensity,
	PyDoc_STR("*float*\n\n*Required*: True"),
 	NULL},
{"biopwr_emissions_pre_chipopt", (getter)Biopower_get_biopwr_emissions_pre_chipopt,(setter)Biopower_set_biopwr_emissions_pre_chipopt,
	PyDoc_STR("*float*\n\n*Constraints*: INTEGER\n\n*Required*: True"),
 	NULL},
{"biopwr_emissions_pre_grindopt", (getter)Biopower_get_biopwr_emissions_pre_grindopt,(setter)Biopower_set_biopwr_emissions_pre_grindopt,
	PyDoc_STR("*float*\n\n*Constraints*: INTEGER\n\n*Required*: True"),
 	NULL},
{"biopwr_emissions_pre_pelletopt", (getter)Biopower_get_biopwr_emissions_pre_pelletopt,(setter)Biopower_set_biopwr_emissions_pre_pelletopt,
	PyDoc_STR("*float*\n\n*Constraints*: INTEGER\n\n*Required*: True"),
 	NULL},
{"biopwr_emissions_transport_fuel", (getter)Biopower_get_biopwr_emissions_transport_fuel,(setter)Biopower_set_biopwr_emissions_transport_fuel,
	PyDoc_STR("*float*\n\n*Constraints*: INTEGER\n\n*Required*: True"),
 	NULL},
{"biopwr_emissions_transport_legs", (getter)Biopower_get_biopwr_emissions_transport_legs,(setter)Biopower_set_biopwr_emissions_transport_legs,
	PyDoc_STR("*float*\n\n*Constraints*: INTEGER\n\n*Required*: True"),
 	NULL},
{"biopwr_emissions_transport_long", (getter)Biopower_get_biopwr_emissions_transport_long,(setter)Biopower_set_biopwr_emissions_transport_long,
	PyDoc_STR("*float*\n\n*Constraints*: INTEGER\n\n*Required*: True"),
 	NULL},
{"biopwr_emissions_transport_longmiles", (getter)Biopower_get_biopwr_emissions_transport_longmiles,(setter)Biopower_set_biopwr_emissions_transport_longmiles,
	PyDoc_STR("*float*\n\n*Required*: True"),
 	NULL},
{"biopwr_emissions_transport_longopt", (getter)Biopower_get_biopwr_emissions_transport_longopt,(setter)Biopower_set_biopwr_emissions_transport_longopt,
	PyDoc_STR("*float*\n\n*Constraints*: INTEGER\n\n*Required*: True"),
 	NULL},
{"biopwr_emissions_transport_predist", (getter)Biopower_get_biopwr_emissions_transport_predist,(setter)Biopower_set_biopwr_emissions_transport_predist,
	PyDoc_STR("*float*\n\n*Required*: True"),
 	NULL},
{"biopwr_feedstock_additional_opt", (getter)Biopower_get_biopwr_feedstock_additional_opt,(setter)Biopower_set_biopwr_feedstock_additional_opt,
	PyDoc_STR("*float*\n\n*Constraints*: INTEGER\n\n*Required*: True"),
 	NULL},
{"biopwr_feedstock_bagasse_frac", (getter)Biopower_get_biopwr_feedstock_bagasse_frac,(setter)Biopower_set_biopwr_feedstock_bagasse_frac,
	PyDoc_STR("*float*: Bagasse feedstock fraction\n\n*Required*: True"),
 	NULL},
{"biopwr_feedstock_bagasse_moisture", (getter)Biopower_get_biopwr_feedstock_bagasse_moisture,(setter)Biopower_set_biopwr_feedstock_bagasse_moisture,
	PyDoc_STR("*float*\n\n*Required*: True"),
 	NULL},
{"biopwr_feedstock_barley_frac", (getter)Biopower_get_biopwr_feedstock_barley_frac,(setter)Biopower_set_biopwr_feedstock_barley_frac,
	PyDoc_STR("*float*: Barley feedstock fraction\n\n*Required*: True"),
 	NULL},
{"biopwr_feedstock_barley_moisture", (getter)Biopower_get_biopwr_feedstock_barley_moisture,(setter)Biopower_set_biopwr_feedstock_barley_moisture,
	PyDoc_STR("*float*\n\n*Required*: True"),
 	NULL},
{"biopwr_feedstock_bit_frac", (getter)Biopower_get_biopwr_feedstock_bit_frac,(setter)Biopower_set_biopwr_feedstock_bit_frac,
	PyDoc_STR("*float*: Bituminos coal feedstock fraction\n\n*Required*: True"),
 	NULL},
{"biopwr_feedstock_bit_moisture", (getter)Biopower_get_biopwr_feedstock_bit_moisture,(setter)Biopower_set_biopwr_feedstock_bit_moisture,
	PyDoc_STR("*float*\n\n*Required*: True"),
 	NULL},
{"biopwr_feedstock_collection_radius", (getter)Biopower_get_biopwr_feedstock_collection_radius,(setter)Biopower_set_biopwr_feedstock_collection_radius,
	PyDoc_STR("*float*\n\n*Required*: True"),
 	NULL},
{"biopwr_feedstock_feedstock1_c", (getter)Biopower_get_biopwr_feedstock_feedstock1_c,(setter)Biopower_set_biopwr_feedstock_feedstock1_c,
	PyDoc_STR("*float*\n\n*Required*: True"),
 	NULL},
{"biopwr_feedstock_feedstock1_frac", (getter)Biopower_get_biopwr_feedstock_feedstock1_frac,(setter)Biopower_set_biopwr_feedstock_feedstock1_frac,
	PyDoc_STR("*float*\n\n*Required*: True"),
 	NULL},
{"biopwr_feedstock_feedstock1_h", (getter)Biopower_get_biopwr_feedstock_feedstock1_h,(setter)Biopower_set_biopwr_feedstock_feedstock1_h,
	PyDoc_STR("*float*\n\n*Required*: True"),
 	NULL},
{"biopwr_feedstock_feedstock1_hhv", (getter)Biopower_get_biopwr_feedstock_feedstock1_hhv,(setter)Biopower_set_biopwr_feedstock_feedstock1_hhv,
	PyDoc_STR("*float*\n\n*Required*: True"),
 	NULL},
{"biopwr_feedstock_feedstock1_moisture", (getter)Biopower_get_biopwr_feedstock_feedstock1_moisture,(setter)Biopower_set_biopwr_feedstock_feedstock1_moisture,
	PyDoc_STR("*float*\n\n*Required*: True"),
 	NULL},
{"biopwr_feedstock_feedstock1_resource", (getter)Biopower_get_biopwr_feedstock_feedstock1_resource,(setter)Biopower_set_biopwr_feedstock_feedstock1_resource,
	PyDoc_STR("*float*: Opt feedstock 1 (dt/yr)\n\n*Required*: True"),
 	NULL},
{"biopwr_feedstock_feedstock2_c", (getter)Biopower_get_biopwr_feedstock_feedstock2_c,(setter)Biopower_set_biopwr_feedstock_feedstock2_c,
	PyDoc_STR("*float*\n\n*Required*: True"),
 	NULL},
{"biopwr_feedstock_feedstock2_frac", (getter)Biopower_get_biopwr_feedstock_feedstock2_frac,(setter)Biopower_set_biopwr_feedstock_feedstock2_frac,
	PyDoc_STR("*float*\n\n*Required*: True"),
 	NULL},
{"biopwr_feedstock_feedstock2_h", (getter)Biopower_get_biopwr_feedstock_feedstock2_h,(setter)Biopower_set_biopwr_feedstock_feedstock2_h,
	PyDoc_STR("*float*\n\n*Required*: True"),
 	NULL},
{"biopwr_feedstock_feedstock2_hhv", (getter)Biopower_get_biopwr_feedstock_feedstock2_hhv,(setter)Biopower_set_biopwr_feedstock_feedstock2_hhv,
	PyDoc_STR("*float*\n\n*Required*: True"),
 	NULL},
{"biopwr_feedstock_feedstock2_moisture", (getter)Biopower_get_biopwr_feedstock_feedstock2_moisture,(setter)Biopower_set_biopwr_feedstock_feedstock2_moisture,
	PyDoc_STR("*float*\n\n*Required*: True"),
 	NULL},
{"biopwr_feedstock_feedstock2_resource", (getter)Biopower_get_biopwr_feedstock_feedstock2_resource,(setter)Biopower_set_biopwr_feedstock_feedstock2_resource,
	PyDoc_STR("*float*: Opt feedstock 2 (dt/yr)\n\n*Required*: True"),
 	NULL},
{"biopwr_feedstock_forest_frac", (getter)Biopower_get_biopwr_feedstock_forest_frac,(setter)Biopower_set_biopwr_feedstock_forest_frac,
	PyDoc_STR("*float*: Forest residue feedstock fraction\n\n*Required*: True"),
 	NULL},
{"biopwr_feedstock_forest_moisture", (getter)Biopower_get_biopwr_feedstock_forest_moisture,(setter)Biopower_set_biopwr_feedstock_forest_moisture,
	PyDoc_STR("*float*\n\n*Required*: True"),
 	NULL},
{"biopwr_feedstock_herb_c", (getter)Biopower_get_biopwr_feedstock_herb_c,(setter)Biopower_set_biopwr_feedstock_herb_c,
	PyDoc_STR("*float*: Carbon fraction in herbaceous energy crop\n\n*Required*: True"),
 	NULL},
{"biopwr_feedstock_herb_frac", (getter)Biopower_get_biopwr_feedstock_herb_frac,(setter)Biopower_set_biopwr_feedstock_herb_frac,
	PyDoc_STR("*float*: Herbaceous energy crop feedstock fraction\n\n*Required*: True"),
 	NULL},
{"biopwr_feedstock_herb_moisture", (getter)Biopower_get_biopwr_feedstock_herb_moisture,(setter)Biopower_set_biopwr_feedstock_herb_moisture,
	PyDoc_STR("*float*\n\n*Required*: True"),
 	NULL},
{"biopwr_feedstock_lig_frac", (getter)Biopower_get_biopwr_feedstock_lig_frac,(setter)Biopower_set_biopwr_feedstock_lig_frac,
	PyDoc_STR("*float*: Lignite coal feedstock fraction\n\n*Required*: True"),
 	NULL},
{"biopwr_feedstock_lig_moisture", (getter)Biopower_get_biopwr_feedstock_lig_moisture,(setter)Biopower_set_biopwr_feedstock_lig_moisture,
	PyDoc_STR("*float*\n\n*Required*: True"),
 	NULL},
{"biopwr_feedstock_mill_c", (getter)Biopower_get_biopwr_feedstock_mill_c,(setter)Biopower_set_biopwr_feedstock_mill_c,
	PyDoc_STR("*float*: Carbon fraction in mill residue\n\n*Required*: True"),
 	NULL},
{"biopwr_feedstock_mill_frac", (getter)Biopower_get_biopwr_feedstock_mill_frac,(setter)Biopower_set_biopwr_feedstock_mill_frac,
	PyDoc_STR("*float*: Mill residue feedstock fraction\n\n*Required*: True"),
 	NULL},
{"biopwr_feedstock_mill_moisture", (getter)Biopower_get_biopwr_feedstock_mill_moisture,(setter)Biopower_set_biopwr_feedstock_mill_moisture,
	PyDoc_STR("*float*\n\n*Required*: True"),
 	NULL},
{"biopwr_feedstock_rice_frac", (getter)Biopower_get_biopwr_feedstock_rice_frac,(setter)Biopower_set_biopwr_feedstock_rice_frac,
	PyDoc_STR("*float*: Rice straw feedstock fraction\n\n*Required*: True"),
 	NULL},
{"biopwr_feedstock_rice_moisture", (getter)Biopower_get_biopwr_feedstock_rice_moisture,(setter)Biopower_set_biopwr_feedstock_rice_moisture,
	PyDoc_STR("*float*\n\n*Required*: True"),
 	NULL},
{"biopwr_feedstock_stover_frac", (getter)Biopower_get_biopwr_feedstock_stover_frac,(setter)Biopower_set_biopwr_feedstock_stover_frac,
	PyDoc_STR("*float*: Stover feedstock fraction\n\n*Required*: True"),
 	NULL},
{"biopwr_feedstock_stover_moisture", (getter)Biopower_get_biopwr_feedstock_stover_moisture,(setter)Biopower_set_biopwr_feedstock_stover_moisture,
	PyDoc_STR("*float*\n\n*Required*: True"),
 	NULL},
{"biopwr_feedstock_subbit_frac", (getter)Biopower_get_biopwr_feedstock_subbit_frac,(setter)Biopower_set_biopwr_feedstock_subbit_frac,
	PyDoc_STR("*float*: Sub-bituminous coal feedstock fraction\n\n*Required*: True"),
 	NULL},
{"biopwr_feedstock_subbit_moisture", (getter)Biopower_get_biopwr_feedstock_subbit_moisture,(setter)Biopower_set_biopwr_feedstock_subbit_moisture,
	PyDoc_STR("*float*\n\n*Required*: True"),
 	NULL},
{"biopwr_feedstock_total", (getter)Biopower_get_biopwr_feedstock_total,(setter)Biopower_set_biopwr_feedstock_total,
	PyDoc_STR("*float*: Total fuel resource (dt/yr)\n\n*Required*: True"),
 	NULL},
{"biopwr_feedstock_total_biomass", (getter)Biopower_get_biopwr_feedstock_total_biomass,(setter)Biopower_set_biopwr_feedstock_total_biomass,
	PyDoc_STR("*float*: Total biomass resource (dt/yr)\n\n*Required*: True"),
 	NULL},
{"biopwr_feedstock_total_biomass_c", (getter)Biopower_get_biopwr_feedstock_total_biomass_c,(setter)Biopower_set_biopwr_feedstock_total_biomass_c,
	PyDoc_STR("*float*: Biomass fraction carbon\n\n*Required*: True"),
 	NULL},
{"biopwr_feedstock_total_c", (getter)Biopower_get_biopwr_feedstock_total_c,(setter)Biopower_set_biopwr_feedstock_total_c,
	PyDoc_STR("*float*: Mass fraction carbon\n\n*Required*: True"),
 	NULL},
{"biopwr_feedstock_total_coal", (getter)Biopower_get_biopwr_feedstock_total_coal,(setter)Biopower_set_biopwr_feedstock_total_coal,
	PyDoc_STR("*float*: Total coal resource (dt/yr)\n\n*Required*: True"),
 	NULL},
{"biopwr_feedstock_total_h", (getter)Biopower_get_biopwr_feedstock_total_h,(setter)Biopower_set_biopwr_feedstock_total_h,
	PyDoc_STR("*float*: Mass fraction hydrogen\n\n*Required*: True"),
 	NULL},
{"biopwr_feedstock_total_hhv", (getter)Biopower_get_biopwr_feedstock_total_hhv,(setter)Biopower_set_biopwr_feedstock_total_hhv,
	PyDoc_STR("*float*: Dry feedstock HHV (Btu/lb)\n\n*Required*: True"),
 	NULL},
{"biopwr_feedstock_total_lhv", (getter)Biopower_get_biopwr_feedstock_total_lhv,(setter)Biopower_set_biopwr_feedstock_total_lhv,
	PyDoc_STR("*float*: Dry feedstock LHV (Btu/lb)\n\n*Required*: True"),
 	NULL},
{"biopwr_feedstock_total_moisture", (getter)Biopower_get_biopwr_feedstock_total_moisture,(setter)Biopower_set_biopwr_feedstock_total_moisture,
	PyDoc_STR("*float*: Overall Moisture Content (dry %)\n\n*Required*: True"),
 	NULL},
{"biopwr_feedstock_urban_c", (getter)Biopower_get_biopwr_feedstock_urban_c,(setter)Biopower_set_biopwr_feedstock_urban_c,
	PyDoc_STR("*float*: Carbon fraction in urban residue\n\n*Required*: True"),
 	NULL},
{"biopwr_feedstock_urban_frac", (getter)Biopower_get_biopwr_feedstock_urban_frac,(setter)Biopower_set_biopwr_feedstock_urban_frac,
	PyDoc_STR("*float*: Urban wood residue feedstock fraction\n\n*Required*: True"),
 	NULL},
{"biopwr_feedstock_urban_moisture", (getter)Biopower_get_biopwr_feedstock_urban_moisture,(setter)Biopower_set_biopwr_feedstock_urban_moisture,
	PyDoc_STR("*float*\n\n*Required*: True"),
 	NULL},
{"biopwr_feedstock_wheat_frac", (getter)Biopower_get_biopwr_feedstock_wheat_frac,(setter)Biopower_set_biopwr_feedstock_wheat_frac,
	PyDoc_STR("*float*: Wheat straw feedstock fraction\n\n*Required*: True"),
 	NULL},
{"biopwr_feedstock_wheat_moisture", (getter)Biopower_get_biopwr_feedstock_wheat_moisture,(setter)Biopower_set_biopwr_feedstock_wheat_moisture,
	PyDoc_STR("*float*\n\n*Required*: True"),
 	NULL},
{"biopwr_feedstock_woody_c", (getter)Biopower_get_biopwr_feedstock_woody_c,(setter)Biopower_set_biopwr_feedstock_woody_c,
	PyDoc_STR("*float*: Carbon fraction in woody energy crop\n\n*Required*: True"),
 	NULL},
{"biopwr_feedstock_woody_frac", (getter)Biopower_get_biopwr_feedstock_woody_frac,(setter)Biopower_set_biopwr_feedstock_woody_frac,
	PyDoc_STR("*float*: Woody energy crop feedstock fraction\n\n*Required*: True"),
 	NULL},
{"biopwr_feedstock_woody_moisture", (getter)Biopower_get_biopwr_feedstock_woody_moisture,(setter)Biopower_set_biopwr_feedstock_woody_moisture,
	PyDoc_STR("*float*\n\n*Required*: True"),
 	NULL},
{"biopwr_plant_boiler_air_feed", (getter)Biopower_get_biopwr_plant_boiler_air_feed,(setter)Biopower_set_biopwr_plant_boiler_air_feed,
	PyDoc_STR("*float*\n\n*Required*: True"),
 	NULL},
{"biopwr_plant_boiler_cap_per_boiler", (getter)Biopower_get_biopwr_plant_boiler_cap_per_boiler,(setter)Biopower_set_biopwr_plant_boiler_cap_per_boiler,
	PyDoc_STR("*float*\n\n*Required*: True"),
 	NULL},
{"biopwr_plant_boiler_flue_temp", (getter)Biopower_get_biopwr_plant_boiler_flue_temp,(setter)Biopower_set_biopwr_plant_boiler_flue_temp,
	PyDoc_STR("*float*\n\n*Required*: True"),
 	NULL},
{"biopwr_plant_boiler_num", (getter)Biopower_get_biopwr_plant_boiler_num,(setter)Biopower_set_biopwr_plant_boiler_num,
	PyDoc_STR("*float*\n\n*Required*: True"),
 	NULL},
{"biopwr_plant_boiler_over_design", (getter)Biopower_get_biopwr_plant_boiler_over_design,(setter)Biopower_set_biopwr_plant_boiler_over_design,
	PyDoc_STR("*float*\n\n*Required*: True"),
 	NULL},
{"biopwr_plant_boiler_steam_enthalpy", (getter)Biopower_get_biopwr_plant_boiler_steam_enthalpy,(setter)Biopower_set_biopwr_plant_boiler_steam_enthalpy,
	PyDoc_STR("*float*\n\n*Required*: True"),
 	NULL},
{"biopwr_plant_boiler_steam_pressure", (getter)Biopower_get_biopwr_plant_boiler_steam_pressure,(setter)Biopower_set_biopwr_plant_boiler_steam_pressure,
	PyDoc_STR("*float*\n\n*Required*: True"),
 	NULL},
{"biopwr_plant_combustor_type", (getter)Biopower_get_biopwr_plant_combustor_type,(setter)Biopower_set_biopwr_plant_combustor_type,
	PyDoc_STR("*float*\n\n*Constraints*: INTEGER\n\n*Required*: True"),
 	NULL},
{"biopwr_plant_cycle_design_temp", (getter)Biopower_get_biopwr_plant_cycle_design_temp,(setter)Biopower_set_biopwr_plant_cycle_design_temp,
	PyDoc_STR("*float*\n\n*Required*: True"),
 	NULL},
{"biopwr_plant_disp_power", (getter)Biopower_get_biopwr_plant_disp_power,(setter)Biopower_set_biopwr_plant_disp_power,
	PyDoc_STR("*sequence*\n\n*Constraints*: LENGTH=9\n\n*Required*: True"),
 	NULL},
{"biopwr_plant_drying_method", (getter)Biopower_get_biopwr_plant_drying_method,(setter)Biopower_set_biopwr_plant_drying_method,
	PyDoc_STR("*float*\n\n*Constraints*: INTEGER\n\n*Required*: True"),
 	NULL},
{"biopwr_plant_drying_spec", (getter)Biopower_get_biopwr_plant_drying_spec,(setter)Biopower_set_biopwr_plant_drying_spec,
	PyDoc_STR("*float*\n\n*Required*: True"),
 	NULL},
{"biopwr_plant_max_over_design", (getter)Biopower_get_biopwr_plant_max_over_design,(setter)Biopower_set_biopwr_plant_max_over_design,
	PyDoc_STR("*float*\n\n*Required*: True"),
 	NULL},
{"biopwr_plant_min_load", (getter)Biopower_get_biopwr_plant_min_load,(setter)Biopower_set_biopwr_plant_min_load,
	PyDoc_STR("*float*\n\n*Required*: True"),
 	NULL},
{"biopwr_plant_nameplate", (getter)Biopower_get_biopwr_plant_nameplate,(setter)Biopower_set_biopwr_plant_nameplate,
	PyDoc_STR("*float*\n\n*Required*: True"),
 	NULL},
{"biopwr_plant_par_percent", (getter)Biopower_get_biopwr_plant_par_percent,(setter)Biopower_set_biopwr_plant_par_percent,
	PyDoc_STR("*float*\n\n*Required*: True"),
 	NULL},
{"biopwr_plant_pl_eff_f0", (getter)Biopower_get_biopwr_plant_pl_eff_f0,(setter)Biopower_set_biopwr_plant_pl_eff_f0,
	PyDoc_STR("*float*\n\n*Required*: True"),
 	NULL},
{"biopwr_plant_pl_eff_f1", (getter)Biopower_get_biopwr_plant_pl_eff_f1,(setter)Biopower_set_biopwr_plant_pl_eff_f1,
	PyDoc_STR("*float*\n\n*Required*: True"),
 	NULL},
{"biopwr_plant_pl_eff_f2", (getter)Biopower_get_biopwr_plant_pl_eff_f2,(setter)Biopower_set_biopwr_plant_pl_eff_f2,
	PyDoc_STR("*float*\n\n*Required*: True"),
 	NULL},
{"biopwr_plant_pl_eff_f3", (getter)Biopower_get_biopwr_plant_pl_eff_f3,(setter)Biopower_set_biopwr_plant_pl_eff_f3,
	PyDoc_STR("*float*\n\n*Required*: True"),
 	NULL},
{"biopwr_plant_pl_eff_f4", (getter)Biopower_get_biopwr_plant_pl_eff_f4,(setter)Biopower_set_biopwr_plant_pl_eff_f4,
	PyDoc_STR("*float*\n\n*Required*: True"),
 	NULL},
{"biopwr_plant_ramp_rate", (getter)Biopower_get_biopwr_plant_ramp_rate,(setter)Biopower_set_biopwr_plant_ramp_rate,
	PyDoc_STR("*float*\n\n*Required*: True"),
 	NULL},
{"biopwr_plant_rated_eff", (getter)Biopower_get_biopwr_plant_rated_eff,(setter)Biopower_set_biopwr_plant_rated_eff,
	PyDoc_STR("*float*\n\n*Required*: True"),
 	NULL},
{"biopwr_plant_temp_corr_mode", (getter)Biopower_get_biopwr_plant_temp_corr_mode,(setter)Biopower_set_biopwr_plant_temp_corr_mode,
	PyDoc_STR("*float*\n\n*Constraints*: INTEGER\n\n*Required*: True"),
 	NULL},
{"biopwr_plant_temp_eff_f0", (getter)Biopower_get_biopwr_plant_temp_eff_f0,(setter)Biopower_set_biopwr_plant_temp_eff_f0,
	PyDoc_STR("*float*\n\n*Required*: True"),
 	NULL},
{"biopwr_plant_temp_eff_f1", (getter)Biopower_get_biopwr_plant_temp_eff_f1,(setter)Biopower_set_biopwr_plant_temp_eff_f1,
	PyDoc_STR("*float*\n\n*Required*: True"),
 	NULL},
{"biopwr_plant_temp_eff_f2", (getter)Biopower_get_biopwr_plant_temp_eff_f2,(setter)Biopower_set_biopwr_plant_temp_eff_f2,
	PyDoc_STR("*float*\n\n*Required*: True"),
 	NULL},
{"biopwr_plant_temp_eff_f3", (getter)Biopower_get_biopwr_plant_temp_eff_f3,(setter)Biopower_set_biopwr_plant_temp_eff_f3,
	PyDoc_STR("*float*\n\n*Required*: True"),
 	NULL},
{"biopwr_plant_temp_eff_f4", (getter)Biopower_get_biopwr_plant_temp_eff_f4,(setter)Biopower_set_biopwr_plant_temp_eff_f4,
	PyDoc_STR("*float*\n\n*Required*: True"),
 	NULL},
{"biopwr_plant_tou_grid", (getter)Biopower_get_biopwr_plant_tou_grid,(setter)Biopower_set_biopwr_plant_tou_grid,
	PyDoc_STR("*str*\n\n*Required*: True"),
 	NULL},
{"biopwr_plant_tou_option", (getter)Biopower_get_biopwr_plant_tou_option,(setter)Biopower_set_biopwr_plant_tou_option,
	PyDoc_STR("*float*\n\n*Constraints*: INTEGER\n\n*Required*: True"),
 	NULL},
{"file_name", (getter)Biopower_get_file_name,(setter)Biopower_set_file_name,
	PyDoc_STR("*str*: Local weather file path\n\n*Constraints*: LOCAL_FILE\n\n*Required*: True"),
 	NULL},
{"system_capacity", (getter)Biopower_get_system_capacity,(setter)Biopower_set_system_capacity,
	PyDoc_STR("*float*: Nameplate capacity [kW]\n\n*Required*: True"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject Biopower_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Biomass.Biopower",             /*tp_name*/
		sizeof(BiopowerObject),          /*tp_basicsize*/
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
		Biopower_methods,         /*tp_methods*/
		0,                          /*tp_members*/
		Biopower_getset,          /*tp_getset*/
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
	SAM_Biomass   data_ptr;
} OutputsObject;

static PyTypeObject Outputs_Type;

static PyObject *
Outputs_new(SAM_Biomass data_ptr)
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

	if (!PySAM_assign_from_dict(self->data_ptr, dict, "Biomass", "Outputs")){
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
Outputs_get_annual_energy(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Outputs_annual_energy_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_fuel_usage(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Outputs_annual_fuel_usage_nget, self->data_ptr);
}

static PyObject *
Outputs_get_annual_watter_usage(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Outputs_annual_watter_usage_nget, self->data_ptr);
}

static PyObject *
Outputs_get_capacity_factor(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Outputs_capacity_factor_nget, self->data_ptr);
}

static PyObject *
Outputs_get_gen(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Biomass_Outputs_gen_aget, self->data_ptr);
}

static PyObject *
Outputs_get_hourly_boiler_eff(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Biomass_Outputs_hourly_boiler_eff_aget, self->data_ptr);
}

static PyObject *
Outputs_get_hourly_pbeta(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Biomass_Outputs_hourly_pbeta_aget, self->data_ptr);
}

static PyObject *
Outputs_get_hourly_q_to_pb(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Biomass_Outputs_hourly_q_to_pb_aget, self->data_ptr);
}

static PyObject *
Outputs_get_kwh_per_kw(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Outputs_kwh_per_kw_nget, self->data_ptr);
}

static PyObject *
Outputs_get_monthly_bagasse_emc(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Biomass_Outputs_monthly_bagasse_emc_aget, self->data_ptr);
}

static PyObject *
Outputs_get_monthly_barley_emc(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Biomass_Outputs_monthly_barley_emc_aget, self->data_ptr);
}

static PyObject *
Outputs_get_monthly_boiler_eff(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Biomass_Outputs_monthly_boiler_eff_aget, self->data_ptr);
}

static PyObject *
Outputs_get_monthly_energy(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Biomass_Outputs_monthly_energy_aget, self->data_ptr);
}

static PyObject *
Outputs_get_monthly_forest_emc(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Biomass_Outputs_monthly_forest_emc_aget, self->data_ptr);
}

static PyObject *
Outputs_get_monthly_herb_emc(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Biomass_Outputs_monthly_herb_emc_aget, self->data_ptr);
}

static PyObject *
Outputs_get_monthly_hhv_heatrate(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Biomass_Outputs_monthly_hhv_heatrate_aget, self->data_ptr);
}

static PyObject *
Outputs_get_monthly_lhv_heatrate(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Biomass_Outputs_monthly_lhv_heatrate_aget, self->data_ptr);
}

static PyObject *
Outputs_get_monthly_mill_emc(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Biomass_Outputs_monthly_mill_emc_aget, self->data_ptr);
}

static PyObject *
Outputs_get_monthly_moist(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Biomass_Outputs_monthly_moist_aget, self->data_ptr);
}

static PyObject *
Outputs_get_monthly_pb_eta(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Biomass_Outputs_monthly_pb_eta_aget, self->data_ptr);
}

static PyObject *
Outputs_get_monthly_q_to_pb(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Biomass_Outputs_monthly_q_to_pb_aget, self->data_ptr);
}

static PyObject *
Outputs_get_monthly_rh(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Biomass_Outputs_monthly_rh_aget, self->data_ptr);
}

static PyObject *
Outputs_get_monthly_rice_emc(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Biomass_Outputs_monthly_rice_emc_aget, self->data_ptr);
}

static PyObject *
Outputs_get_monthly_stover_emc(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Biomass_Outputs_monthly_stover_emc_aget, self->data_ptr);
}

static PyObject *
Outputs_get_monthly_temp_c(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Biomass_Outputs_monthly_temp_c_aget, self->data_ptr);
}

static PyObject *
Outputs_get_monthly_urban_emc(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Biomass_Outputs_monthly_urban_emc_aget, self->data_ptr);
}

static PyObject *
Outputs_get_monthly_wheat_emc(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Biomass_Outputs_monthly_wheat_emc_aget, self->data_ptr);
}

static PyObject *
Outputs_get_monthly_woody_emc(OutputsObject *self, void *closure)
{
	return PySAM_array_getter(SAM_Biomass_Outputs_monthly_woody_emc_aget, self->data_ptr);
}

static PyObject *
Outputs_get_system_annual_ash(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Outputs_system_annual_ash_nget, self->data_ptr);
}

static PyObject *
Outputs_get_system_annual_biomass(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Outputs_system_annual_biomass_nget, self->data_ptr);
}

static PyObject *
Outputs_get_system_annual_boiler_loss_dry(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Outputs_system_annual_boiler_loss_dry_nget, self->data_ptr);
}

static PyObject *
Outputs_get_system_annual_boiler_loss_dry_kwh(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Outputs_system_annual_boiler_loss_dry_kwh_nget, self->data_ptr);
}

static PyObject *
Outputs_get_system_annual_boiler_loss_fuel(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Outputs_system_annual_boiler_loss_fuel_nget, self->data_ptr);
}

static PyObject *
Outputs_get_system_annual_boiler_loss_fuel_kwh(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Outputs_system_annual_boiler_loss_fuel_kwh_nget, self->data_ptr);
}

static PyObject *
Outputs_get_system_annual_boiler_loss_manu(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Outputs_system_annual_boiler_loss_manu_nget, self->data_ptr);
}

static PyObject *
Outputs_get_system_annual_boiler_loss_manu_kwh(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Outputs_system_annual_boiler_loss_manu_kwh_nget, self->data_ptr);
}

static PyObject *
Outputs_get_system_annual_boiler_loss_rad(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Outputs_system_annual_boiler_loss_rad_nget, self->data_ptr);
}

static PyObject *
Outputs_get_system_annual_boiler_loss_rad_kwh(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Outputs_system_annual_boiler_loss_rad_kwh_nget, self->data_ptr);
}

static PyObject *
Outputs_get_system_annual_boiler_loss_total(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Outputs_system_annual_boiler_loss_total_nget, self->data_ptr);
}

static PyObject *
Outputs_get_system_annual_boiler_loss_total_kwh(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Outputs_system_annual_boiler_loss_total_kwh_nget, self->data_ptr);
}

static PyObject *
Outputs_get_system_annual_boiler_loss_unburn(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Outputs_system_annual_boiler_loss_unburn_nget, self->data_ptr);
}

static PyObject *
Outputs_get_system_annual_boiler_loss_unburn_kwh(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Outputs_system_annual_boiler_loss_unburn_kwh_nget, self->data_ptr);
}

static PyObject *
Outputs_get_system_annual_boiler_loss_wet(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Outputs_system_annual_boiler_loss_wet_nget, self->data_ptr);
}

static PyObject *
Outputs_get_system_annual_boiler_loss_wet_kwh(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Outputs_system_annual_boiler_loss_wet_kwh_nget, self->data_ptr);
}

static PyObject *
Outputs_get_system_annual_boiler_output(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Outputs_system_annual_boiler_output_nget, self->data_ptr);
}

static PyObject *
Outputs_get_system_annual_coal(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Outputs_system_annual_coal_nget, self->data_ptr);
}

static PyObject *
Outputs_get_system_annual_e_net(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Outputs_system_annual_e_net_nget, self->data_ptr);
}

static PyObject *
Outputs_get_system_annual_par_loss(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Outputs_system_annual_par_loss_nget, self->data_ptr);
}

static PyObject *
Outputs_get_system_annual_par_loss_kwh(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Outputs_system_annual_par_loss_kwh_nget, self->data_ptr);
}

static PyObject *
Outputs_get_system_annual_pb_eta(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Outputs_system_annual_pb_eta_nget, self->data_ptr);
}

static PyObject *
Outputs_get_system_annual_pb_eta_kwh(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Outputs_system_annual_pb_eta_kwh_nget, self->data_ptr);
}

static PyObject *
Outputs_get_system_annual_qtoboil_tot(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Outputs_system_annual_qtoboil_tot_nget, self->data_ptr);
}

static PyObject *
Outputs_get_system_annual_qtopb_tot(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Outputs_system_annual_qtopb_tot_nget, self->data_ptr);
}

static PyObject *
Outputs_get_system_annual_turbine_output(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Outputs_system_annual_turbine_output_nget, self->data_ptr);
}

static PyObject *
Outputs_get_system_capfactor(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Outputs_system_capfactor_nget, self->data_ptr);
}

static PyObject *
Outputs_get_system_emissions_avoided(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Outputs_system_emissions_avoided_nget, self->data_ptr);
}

static PyObject *
Outputs_get_system_emissions_biodiesel(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Outputs_system_emissions_biodiesel_nget, self->data_ptr);
}

static PyObject *
Outputs_get_system_emissions_bunker(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Outputs_system_emissions_bunker_nget, self->data_ptr);
}

static PyObject *
Outputs_get_system_emissions_combustion(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Outputs_system_emissions_combustion_nget, self->data_ptr);
}

static PyObject *
Outputs_get_system_emissions_diesel(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Outputs_system_emissions_diesel_nget, self->data_ptr);
}

static PyObject *
Outputs_get_system_emissions_drying(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Outputs_system_emissions_drying_nget, self->data_ptr);
}

static PyObject *
Outputs_get_system_emissions_ems_per_lb(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Outputs_system_emissions_ems_per_lb_nget, self->data_ptr);
}

static PyObject *
Outputs_get_system_emissions_growth(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Outputs_system_emissions_growth_nget, self->data_ptr);
}

static PyObject *
Outputs_get_system_emissions_lime(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Outputs_system_emissions_lime_nget, self->data_ptr);
}

static PyObject *
Outputs_get_system_emissions_naturalgas(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Outputs_system_emissions_naturalgas_nget, self->data_ptr);
}

static PyObject *
Outputs_get_system_emissions_nitrogen(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Outputs_system_emissions_nitrogen_nget, self->data_ptr);
}

static PyObject *
Outputs_get_system_emissions_oil(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Outputs_system_emissions_oil_nget, self->data_ptr);
}

static PyObject *
Outputs_get_system_emissions_phosphorus(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Outputs_system_emissions_phosphorus_nget, self->data_ptr);
}

static PyObject *
Outputs_get_system_emissions_potassium(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Outputs_system_emissions_potassium_nget, self->data_ptr);
}

static PyObject *
Outputs_get_system_emissions_preprocessing(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Outputs_system_emissions_preprocessing_nget, self->data_ptr);
}

static PyObject *
Outputs_get_system_emissions_total_sum(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Outputs_system_emissions_total_sum_nget, self->data_ptr);
}

static PyObject *
Outputs_get_system_emissions_transport(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Outputs_system_emissions_transport_nget, self->data_ptr);
}

static PyObject *
Outputs_get_system_emissions_uptake(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Outputs_system_emissions_uptake_nget, self->data_ptr);
}

static PyObject *
Outputs_get_system_heat_rate(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Outputs_system_heat_rate_nget, self->data_ptr);
}

static PyObject *
Outputs_get_system_hhv_heatrate(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Outputs_system_hhv_heatrate_nget, self->data_ptr);
}

static PyObject *
Outputs_get_system_hhv_thermeff(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Outputs_system_hhv_thermeff_nget, self->data_ptr);
}

static PyObject *
Outputs_get_system_lhv_heatrate(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Outputs_system_lhv_heatrate_nget, self->data_ptr);
}

static PyObject *
Outputs_get_system_lhv_thermeff(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Outputs_system_lhv_thermeff_nget, self->data_ptr);
}

static PyObject *
Outputs_get_system_total_moisture(OutputsObject *self, void *closure)
{
	return PySAM_double_getter(SAM_Biomass_Outputs_system_total_moisture_nget, self->data_ptr);
}

static PyGetSetDef Outputs_getset[] = {
{"annual_energy", (getter)Outputs_get_annual_energy,(setter)0,
	PyDoc_STR("*float*: Annual Energy [kWh]"),
 	NULL},
{"annual_fuel_usage", (getter)Outputs_get_annual_fuel_usage,(setter)0,
	PyDoc_STR("*float*: Annual Fuel Usage [kWht]"),
 	NULL},
{"annual_watter_usage", (getter)Outputs_get_annual_watter_usage,(setter)0,
	PyDoc_STR("*float*: Annual Water Usage [m3]"),
 	NULL},
{"capacity_factor", (getter)Outputs_get_capacity_factor,(setter)0,
	PyDoc_STR("*float*: Capacity factor [%]"),
 	NULL},
{"gen", (getter)Outputs_get_gen,(setter)0,
	PyDoc_STR("*sequence*: System power generated [kW]"),
 	NULL},
{"hourly_boiler_eff", (getter)Outputs_get_hourly_boiler_eff,(setter)0,
	PyDoc_STR("*sequence*: Boiler Efficiency"),
 	NULL},
{"hourly_pbeta", (getter)Outputs_get_hourly_pbeta,(setter)0,
	PyDoc_STR("*sequence*: Power Block Efficiency"),
 	NULL},
{"hourly_q_to_pb", (getter)Outputs_get_hourly_q_to_pb,(setter)0,
	PyDoc_STR("*sequence*: Q To Power Block [kW]"),
 	NULL},
{"kwh_per_kw", (getter)Outputs_get_kwh_per_kw,(setter)0,
	PyDoc_STR("*float*: First year kWh/kW [kWh/kW]"),
 	NULL},
{"monthly_bagasse_emc", (getter)Outputs_get_monthly_bagasse_emc,(setter)0,
	PyDoc_STR("*sequence*: Monthly bagasse EMC (dry)"),
 	NULL},
{"monthly_barley_emc", (getter)Outputs_get_monthly_barley_emc,(setter)0,
	PyDoc_STR("*sequence*: Monthly barley EMC (dry)"),
 	NULL},
{"monthly_boiler_eff", (getter)Outputs_get_monthly_boiler_eff,(setter)0,
	PyDoc_STR("*sequence*: Total Boiler Efficiency - HHV (%) [%]"),
 	NULL},
{"monthly_energy", (getter)Outputs_get_monthly_energy,(setter)0,
	PyDoc_STR("*sequence*: Monthly Energy [kWh]"),
 	NULL},
{"monthly_forest_emc", (getter)Outputs_get_monthly_forest_emc,(setter)0,
	PyDoc_STR("*sequence*: Monthly forest EMC (dry)"),
 	NULL},
{"monthly_herb_emc", (getter)Outputs_get_monthly_herb_emc,(setter)0,
	PyDoc_STR("*sequence*: Monthly herbaceous crop EMC (dry)"),
 	NULL},
{"monthly_hhv_heatrate", (getter)Outputs_get_monthly_hhv_heatrate,(setter)0,
	PyDoc_STR("*sequence*: Gross Monthly Heat Rate (MMBtu/MWh) [MMBtu/MWh]"),
 	NULL},
{"monthly_lhv_heatrate", (getter)Outputs_get_monthly_lhv_heatrate,(setter)0,
	PyDoc_STR("*sequence*: Net Monthly Heat Rate (MMBtu/MWh) [MMBtu/MWh]"),
 	NULL},
{"monthly_mill_emc", (getter)Outputs_get_monthly_mill_emc,(setter)0,
	PyDoc_STR("*sequence*: Monthly mill waste EMC (dry)"),
 	NULL},
{"monthly_moist", (getter)Outputs_get_monthly_moist,(setter)0,
	PyDoc_STR("*sequence*: Monthly biomass moisture fraction (dry)"),
 	NULL},
{"monthly_pb_eta", (getter)Outputs_get_monthly_pb_eta,(setter)0,
	PyDoc_STR("*sequence*: Power Block Effiency [%]"),
 	NULL},
{"monthly_q_to_pb", (getter)Outputs_get_monthly_q_to_pb,(setter)0,
	PyDoc_STR("*sequence*: Q To Power Block [kWh]"),
 	NULL},
{"monthly_rh", (getter)Outputs_get_monthly_rh,(setter)0,
	PyDoc_STR("*sequence*: Relative humidity"),
 	NULL},
{"monthly_rice_emc", (getter)Outputs_get_monthly_rice_emc,(setter)0,
	PyDoc_STR("*sequence*: Monthly rice straw EMC (dry)"),
 	NULL},
{"monthly_stover_emc", (getter)Outputs_get_monthly_stover_emc,(setter)0,
	PyDoc_STR("*sequence*: Monthly stover EMC (dry)"),
 	NULL},
{"monthly_temp_c", (getter)Outputs_get_monthly_temp_c,(setter)0,
	PyDoc_STR("*sequence*: Temperature"),
 	NULL},
{"monthly_urban_emc", (getter)Outputs_get_monthly_urban_emc,(setter)0,
	PyDoc_STR("*sequence*: Monthly urban wood waste EMC (dry)"),
 	NULL},
{"monthly_wheat_emc", (getter)Outputs_get_monthly_wheat_emc,(setter)0,
	PyDoc_STR("*sequence*: Monthly wheat straw EMC (dry)"),
 	NULL},
{"monthly_woody_emc", (getter)Outputs_get_monthly_woody_emc,(setter)0,
	PyDoc_STR("*sequence*: Monthly woody crop EMC (dry)"),
 	NULL},
{"system_annual_ash", (getter)Outputs_get_system_annual_ash,(setter)0,
	PyDoc_STR("*float*: Ash produced [tons/yr]"),
 	NULL},
{"system_annual_biomass", (getter)Outputs_get_system_annual_biomass,(setter)0,
	PyDoc_STR("*float*: Annual biomass usage [dry tons/yr]"),
 	NULL},
{"system_annual_boiler_loss_dry", (getter)Outputs_get_system_annual_boiler_loss_dry,(setter)0,
	PyDoc_STR("*float*: Energy lost in hot flue gas [%]"),
 	NULL},
{"system_annual_boiler_loss_dry_kwh", (getter)Outputs_get_system_annual_boiler_loss_dry_kwh,(setter)0,
	PyDoc_STR("*float*: Energy lost in hot flue gas [kWh]"),
 	NULL},
{"system_annual_boiler_loss_fuel", (getter)Outputs_get_system_annual_boiler_loss_fuel,(setter)0,
	PyDoc_STR("*float*: Energy lost in fuel out of boiler [%]"),
 	NULL},
{"system_annual_boiler_loss_fuel_kwh", (getter)Outputs_get_system_annual_boiler_loss_fuel_kwh,(setter)0,
	PyDoc_STR("*float*: Energy lost in fuel out of boiler [kWh]"),
 	NULL},
{"system_annual_boiler_loss_manu", (getter)Outputs_get_system_annual_boiler_loss_manu,(setter)0,
	PyDoc_STR("*float*: Energy loss included in manufacturer's margin [%]"),
 	NULL},
{"system_annual_boiler_loss_manu_kwh", (getter)Outputs_get_system_annual_boiler_loss_manu_kwh,(setter)0,
	PyDoc_STR("*float*: Energy loss included in manufacturer's margin [kWh]"),
 	NULL},
{"system_annual_boiler_loss_rad", (getter)Outputs_get_system_annual_boiler_loss_rad,(setter)0,
	PyDoc_STR("*float*: Energy loss due to boiler radiation [%]"),
 	NULL},
{"system_annual_boiler_loss_rad_kwh", (getter)Outputs_get_system_annual_boiler_loss_rad_kwh,(setter)0,
	PyDoc_STR("*float*: Energy loss due to boiler radiation [kWh]"),
 	NULL},
{"system_annual_boiler_loss_total", (getter)Outputs_get_system_annual_boiler_loss_total,(setter)0,
	PyDoc_STR("*float*: Energy lost in boiler - total [%]"),
 	NULL},
{"system_annual_boiler_loss_total_kwh", (getter)Outputs_get_system_annual_boiler_loss_total_kwh,(setter)0,
	PyDoc_STR("*float*: Energy lost in boiler - total [kWh]"),
 	NULL},
{"system_annual_boiler_loss_unburn", (getter)Outputs_get_system_annual_boiler_loss_unburn,(setter)0,
	PyDoc_STR("*float*: Energy lost in unburned fuel [%]"),
 	NULL},
{"system_annual_boiler_loss_unburn_kwh", (getter)Outputs_get_system_annual_boiler_loss_unburn_kwh,(setter)0,
	PyDoc_STR("*float*: Energy lost in unburned fuel [kWh]"),
 	NULL},
{"system_annual_boiler_loss_wet", (getter)Outputs_get_system_annual_boiler_loss_wet,(setter)0,
	PyDoc_STR("*float*: Energy lost to moisture in air [%]"),
 	NULL},
{"system_annual_boiler_loss_wet_kwh", (getter)Outputs_get_system_annual_boiler_loss_wet_kwh,(setter)0,
	PyDoc_STR("*float*: Energy lost to moisture in air [kWh]"),
 	NULL},
{"system_annual_boiler_output", (getter)Outputs_get_system_annual_boiler_output,(setter)0,
	PyDoc_STR("*float*: Boiler output [kWh]"),
 	NULL},
{"system_annual_coal", (getter)Outputs_get_system_annual_coal,(setter)0,
	PyDoc_STR("*float*: Annual coal usage [dry tons/yr]"),
 	NULL},
{"system_annual_e_net", (getter)Outputs_get_system_annual_e_net,(setter)0,
	PyDoc_STR("*float*: Gross Annual Energy [kWh]"),
 	NULL},
{"system_annual_par_loss", (getter)Outputs_get_system_annual_par_loss,(setter)0,
	PyDoc_STR("*float*: Energy consumed within plant - parasitic load [%]"),
 	NULL},
{"system_annual_par_loss_kwh", (getter)Outputs_get_system_annual_par_loss_kwh,(setter)0,
	PyDoc_STR("*float*: Energy consumed within plant - parasitic load [kWh]"),
 	NULL},
{"system_annual_pb_eta", (getter)Outputs_get_system_annual_pb_eta,(setter)0,
	PyDoc_STR("*float*: Energy lost in steam turbine and generator [%]"),
 	NULL},
{"system_annual_pb_eta_kwh", (getter)Outputs_get_system_annual_pb_eta_kwh,(setter)0,
	PyDoc_STR("*float*: Energy lost in steam turbine and generator [kWh]"),
 	NULL},
{"system_annual_qtoboil_tot", (getter)Outputs_get_system_annual_qtoboil_tot,(setter)0,
	PyDoc_STR("*float*: Q to Boiler [kWh]"),
 	NULL},
{"system_annual_qtopb_tot", (getter)Outputs_get_system_annual_qtopb_tot,(setter)0,
	PyDoc_STR("*float*: Q to Power Block [kWh]"),
 	NULL},
{"system_annual_turbine_output", (getter)Outputs_get_system_annual_turbine_output,(setter)0,
	PyDoc_STR("*float*: Turbine output [kWh]"),
 	NULL},
{"system_capfactor", (getter)Outputs_get_system_capfactor,(setter)0,
	PyDoc_STR("*float*: Annual Capacity Factor (%) [%]"),
 	NULL},
{"system_emissions_avoided", (getter)Outputs_get_system_emissions_avoided,(setter)0,
	PyDoc_STR("*float*: Biomass Avoided Use [kWh]"),
 	NULL},
{"system_emissions_biodiesel", (getter)Outputs_get_system_emissions_biodiesel,(setter)0,
	PyDoc_STR("*float*: Life Cycle Biodiesel use [Btu/kWh]"),
 	NULL},
{"system_emissions_bunker", (getter)Outputs_get_system_emissions_bunker,(setter)0,
	PyDoc_STR("*float*: Life Cycle Bunker fuel use [Btu/kWh]"),
 	NULL},
{"system_emissions_combustion", (getter)Outputs_get_system_emissions_combustion,(setter)0,
	PyDoc_STR("*float*: Combustion [kWh]"),
 	NULL},
{"system_emissions_diesel", (getter)Outputs_get_system_emissions_diesel,(setter)0,
	PyDoc_STR("*float*: Life Cycle Diesel use [Btu/kWh]"),
 	NULL},
{"system_emissions_drying", (getter)Outputs_get_system_emissions_drying,(setter)0,
	PyDoc_STR("*float*: Biomass Drying [kWh]"),
 	NULL},
{"system_emissions_ems_per_lb", (getter)Outputs_get_system_emissions_ems_per_lb,(setter)0,
	PyDoc_STR("*float*: Life Cycle g CO2eq released/lb dry biomass"),
 	NULL},
{"system_emissions_growth", (getter)Outputs_get_system_emissions_growth,(setter)0,
	PyDoc_STR("*float*: Biomass Collection [kWh]"),
 	NULL},
{"system_emissions_lime", (getter)Outputs_get_system_emissions_lime,(setter)0,
	PyDoc_STR("*float*: Life Cycle Lime fertilizer use [lb Lime/kWh]"),
 	NULL},
{"system_emissions_naturalgas", (getter)Outputs_get_system_emissions_naturalgas,(setter)0,
	PyDoc_STR("*float*: Life Cycle Natural gas use [Btu/kWh]"),
 	NULL},
{"system_emissions_nitrogen", (getter)Outputs_get_system_emissions_nitrogen,(setter)0,
	PyDoc_STR("*float*: Life Cycle Nitrogen fertilizer use [lb N/kWh]"),
 	NULL},
{"system_emissions_oil", (getter)Outputs_get_system_emissions_oil,(setter)0,
	PyDoc_STR("*float*: Life Cycle Oil use [Btu/kWh]"),
 	NULL},
{"system_emissions_phosphorus", (getter)Outputs_get_system_emissions_phosphorus,(setter)0,
	PyDoc_STR("*float*: Life Cycle Phosphorus fertilizer use [lb K2O/kWh]"),
 	NULL},
{"system_emissions_potassium", (getter)Outputs_get_system_emissions_potassium,(setter)0,
	PyDoc_STR("*float*: Life Cycle Potassium fertilizer use [lb P2O5/kWh]"),
 	NULL},
{"system_emissions_preprocessing", (getter)Outputs_get_system_emissions_preprocessing,(setter)0,
	PyDoc_STR("*float*: Biomass Preprocessing [kWh]"),
 	NULL},
{"system_emissions_total_sum", (getter)Outputs_get_system_emissions_total_sum,(setter)0,
	PyDoc_STR("*float*: Biomass Life Cycle CO2 [kWh]"),
 	NULL},
{"system_emissions_transport", (getter)Outputs_get_system_emissions_transport,(setter)0,
	PyDoc_STR("*float*: Biomass Transport [kWh]"),
 	NULL},
{"system_emissions_uptake", (getter)Outputs_get_system_emissions_uptake,(setter)0,
	PyDoc_STR("*float*: Biomass CO2 Uptake [kWh]"),
 	NULL},
{"system_heat_rate", (getter)Outputs_get_system_heat_rate,(setter)0,
	PyDoc_STR("*float*: Heat Rate Conversion Factor (MMBTUs/MWhe) [MMBTUs/MWhe]"),
 	NULL},
{"system_hhv_heatrate", (getter)Outputs_get_system_hhv_heatrate,(setter)0,
	PyDoc_STR("*float*: Gross Heat Rate (MMBtu/MWh) [MMBtu/MWh]"),
 	NULL},
{"system_hhv_thermeff", (getter)Outputs_get_system_hhv_thermeff,(setter)0,
	PyDoc_STR("*float*: Thermal efficiency, HHV (%) [%]"),
 	NULL},
{"system_lhv_heatrate", (getter)Outputs_get_system_lhv_heatrate,(setter)0,
	PyDoc_STR("*float*: Net Heat Rate (MMBtu/MWh) [MMBtu/MWh]"),
 	NULL},
{"system_lhv_thermeff", (getter)Outputs_get_system_lhv_thermeff,(setter)0,
	PyDoc_STR("*float*: Thermal efficiency, LHV (%) [%]"),
 	NULL},
{"system_total_moisture", (getter)Outputs_get_system_total_moisture,(setter)0,
	PyDoc_STR("*float*: Overall Moisture Content (dry %) [%]"),
 	NULL},
	{NULL}  /* Sentinel */
};

static PyTypeObject Outputs_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Biomass.Outputs",             /*tp_name*/
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
 * Biomass
 */

typedef struct {
	PyObject_HEAD
	PyObject            *x_attr;        /* Attributes dictionary */
	SAM_Biomass   data_ptr;
} BiomassObject;

static PyTypeObject Biomass_Type;

#define BiomassObject_Check(v)      (Py_TYPE(v) == &Biomass_Type)

static BiomassObject *
newBiomassObject(void* data_ptr)
{
	BiomassObject *self;
	self = PyObject_New(BiomassObject, &Biomass_Type);

	PySAM_TECH_ATTR("Biomass", SAM_Biomass_construct)

	PyObject* Biopower_obj = Biopower_new(self->data_ptr);
	PyDict_SetItemString(attr_dict, "Biopower", Biopower_obj);
	Py_DECREF(Biopower_obj);

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

/* Biomass methods */

static void
Biomass_dealloc(BiomassObject *self)
{
	Py_XDECREF(self->x_attr);
	SAM_Biomass_destruct(self->data_ptr);
	PyObject_Del(self);
}


static PyObject *
Biomass_execute(BiomassObject *self, PyObject *args)
{
	int verbosity = 0;

	if (!PyArg_ParseTuple(args, "|i", &verbosity))
		return NULL;

	SAM_error error = new_error();
	SAM_Biomass_execute(self->data_ptr, verbosity, &error);
	if (PySAM_has_error(error )) return NULL;

	Py_INCREF(Py_None);
	return Py_None;
}


static PyObject *
Biomass_assign(BiomassObject *self, PyObject *args)
{
	PyObject* dict;
	if (!PyArg_ParseTuple(args, "O:assign", &dict)){
		return NULL;
	}

	if (!PySAM_assign_from_nested_dict((PyObject*)self, self->x_attr, self->data_ptr, dict, "Biomass"))
		return NULL;

	Py_INCREF(Py_None);
	return Py_None;
}


static PyObject *
Biomass_export(BiomassObject *self, PyObject *args)
{
	return PySAM_export_to_nested_dict((PyObject *) self, self->x_attr);
}

static PyMethodDef Biomass_methods[] = {
		{"execute",            (PyCFunction)Biomass_execute,  METH_VARARGS,
				PyDoc_STR("execute(int verbosity) -> None\n Execute simulation with verbosity level 0 (default) or 1")},
		{"assign",            (PyCFunction)Biomass_assign,  METH_VARARGS,
				PyDoc_STR("assign(dict) -> None\n Assign attributes from nested dictionary, except for Outputs\n\n``nested_dict = { 'biopower': { var: val, ...}, ...}``")},
		{"export",            (PyCFunction)Biomass_export,  METH_VARARGS,
				PyDoc_STR("export() -> dict\n Export attributes into nested dictionary")},
		{NULL,              NULL}           /* sentinel */
};

static PyObject *
Biomass_getattro(BiomassObject *self, PyObject *name)
{
	return PySAM_get_attr((PyObject*) self, (PyObject*) self->x_attr, name);
}

static int
Biomass_setattr(BiomassObject *self, const char *name, PyObject *v)
{
	return PySAM_set_attr((PyObject*)self, (PyObject*)self->x_attr, name, v);
}

static PyTypeObject Biomass_Type = {
		/* The ob_type field must be initialized in the module init function
		 * to be portable to Windows without using C++. */
		PyVarObject_HEAD_INIT(NULL, 0)
		"Biomass",            /*tp_name*/
		sizeof(BiomassObject),/*tp_basicsize*/
		0,                          /*tp_itemsize*/
		/* methods */
		(destructor)Biomass_dealloc,    /*tp_dealloc*/
		0,                          /*tp_print*/
		(getattrfunc)0,             /*tp_getattr*/
		(setattrfunc)Biomass_setattr,   /*tp_setattr*/
		0,                          /*tp_reserved*/
		0,                          /*tp_repr*/
		0,                          /*tp_as_number*/
		0,                          /*tp_as_sequence*/
		0,                          /*tp_as_mapping*/
		0,                          /*tp_hash*/
		0,                          /*tp_call*/
		0,                          /*tp_str*/
		(getattrofunc)Biomass_getattro, /*tp_getattro*/
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
		Biomass_methods,      /*tp_methods*/
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


/* Function of no arguments returning new Biomass object */

static PyObject *
Biomass_new(PyObject *self, PyObject *args)
{
	BiomassObject *rv;
	rv = newBiomassObject(0);
	if (rv == NULL)
		return NULL;
	return (PyObject *)rv;
}

static PyObject *
Biomass_wrap(PyObject *self, PyObject *args)
{
	BiomassObject *rv;
	long long int ptr = 0;  // 64 bit arch
	if (!PyArg_ParseTuple(args, "L:wrap", &ptr)){
		PyErr_BadArgument();
		return NULL;
	}
	rv = newBiomassObject((void*)ptr);
	if (rv == NULL)
		return NULL;
	return (PyObject *)rv;
}

static PyObject *
Biomass_default(PyObject *self, PyObject *args)
{
	BiomassObject *rv;
	char* def = 0;
	if (!PyArg_ParseTuple(args, "s:default", &def)){
		PyErr_BadArgument();
		return NULL;
	}
	rv = newBiomassObject(0);
	if (rv == NULL)
		return NULL;

	PySAM_load_defaults((PyObject*)rv, rv->x_attr, rv->data_ptr, "Biomass", def);

	return (PyObject *)rv;
}

/* ---------- */


/* List of functions defined in the module */

static PyMethodDef BiomassModule_methods[] = {
		{"new",             Biomass_new,         METH_VARARGS,
				PyDoc_STR("new() -> Biomass")},
		{"default",             Biomass_default,         METH_VARARGS,
				PyDoc_STR("default(config) -> Biomass\n\nUse financial model-specific default attributes\n"
				"config options:\n\n- \"BiopowerAllEquityPartnershipFlip\"\n- \"BiopowerCommercial\"\n- \"BiopowerCommercialPPA\"\n- \"BiopowerIndependentPowerProducer\"\n- \"BiopowerLCOECalculator\"\n- \"BiopowerLeveragedPartnershipFlip\"\n- \"BiopowerNone\"\n- \"BiopowerSaleLeaseback\"\n- \"BiopowerSingleOwner\"")},
		{"wrap",             Biomass_wrap,         METH_VARARGS,
				PyDoc_STR("wrap(ssc_data_t) -> Biomass\n\nUse existing PySSC data\n\n.. warning::\n\n	Do not call PySSC.data_free on the ssc_data_t provided to ``wrap``")},
		{NULL,              NULL}           /* sentinel */
};

PyDoc_STRVAR(module_doc,
			 "Biomass combustion for electricity generation");


static int
BiomassModule_exec(PyObject *m)
{
	/* Finalize the type object including setting type of the new type
	 * object; doing it here is required for portability, too. */

	if (PySAM_load_lib(m) < 0) goto fail;
	if (PySAM_init_error(m) < 0) goto fail;

	Biomass_Type.tp_dict = PyDict_New();
	if (!Biomass_Type.tp_dict) { goto fail; }

	/// Add the AdjustmentFactors type object to Biomass_Type
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
	PyDict_SetItemString(Biomass_Type.tp_dict,
						 "AdjustmentFactors",
						 (PyObject*)AdjustmentFactors_Type);
	Py_DECREF(&AdjustmentFactors_Type);
	Py_XDECREF(AdjustmentFactors_Type);

	/// Add the Biopower type object to Biomass_Type
	if (PyType_Ready(&Biopower_Type) < 0) { goto fail; }
	PyDict_SetItemString(Biomass_Type.tp_dict,
				"Biopower",
				(PyObject*)&Biopower_Type);
	Py_DECREF(&Biopower_Type);

	/// Add the Outputs type object to Biomass_Type
	if (PyType_Ready(&Outputs_Type) < 0) { goto fail; }
	PyDict_SetItemString(Biomass_Type.tp_dict,
				"Outputs",
				(PyObject*)&Outputs_Type);
	Py_DECREF(&Outputs_Type);

	/// Add the Biomass type object to the module
	if (PyType_Ready(&Biomass_Type) < 0) { goto fail; }
	PyModule_AddObject(m,
				"Biomass",
				(PyObject*)&Biomass_Type);

	return 0;
	fail:
	Py_XDECREF(m);
	return -1;
}

static struct PyModuleDef_Slot BiomassModule_slots[] = {
		{Py_mod_exec, BiomassModule_exec},
		{0, NULL},
};

static struct PyModuleDef BiomassModule = {
		PyModuleDef_HEAD_INIT,
		"Biomass",
		module_doc,
		0,
		BiomassModule_methods,
		BiomassModule_slots,
		NULL,
		NULL,
		NULL
};

/* Export function for the module */

PyMODINIT_FUNC
PyInit_Biomass(void)
{
	return PyModuleDef_Init(&BiomassModule);
}