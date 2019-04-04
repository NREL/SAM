#ifndef SAM_BIOMASS_FUNCTIONS_H_
#define SAM_BIOMASS_FUNCTIONS_H_

#include "Biomass-data.h"

#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	/** 
	 * Create a Feedstock variable table for a BiopowerNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_Biomass_Feedstock SAM_Biomass_Feedstock_create(const char* def, SAM_error* err);


	/**
	 * Set biopwr.feedstock.additional_opt: 
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: INTEGER
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.additional_opt_set(SAM_Biomass_Feedstock ptr, float number, SAM_error* err);

	/**
	 * Set biopwr.feedstock.bagasse_c: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.bagasse_c_set(SAM_Biomass_Feedstock ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.feedstock.bagasse_h: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.bagasse_h_set(SAM_Biomass_Feedstock ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.feedstock.bagasse_moisture_wet: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.bagasse_moisture_wet_set(SAM_Biomass_Feedstock ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.feedstock.bagasse_obtainable: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.bagasse_obtainable_set(SAM_Biomass_Feedstock ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.feedstock.bagasse_resource: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.bagasse_resource_set(SAM_Biomass_Feedstock ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.feedstock.barley_c: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.barley_c_set(SAM_Biomass_Feedstock ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.feedstock.barley_h: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.barley_h_set(SAM_Biomass_Feedstock ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.feedstock.barley_moisture_wet: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.barley_moisture_wet_set(SAM_Biomass_Feedstock ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.feedstock.barley_obtainable: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.barley_obtainable_set(SAM_Biomass_Feedstock ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.feedstock.barley_resource: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.barley_resource_set(SAM_Biomass_Feedstock ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.feedstock.bit_c: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.bit_c_set(SAM_Biomass_Feedstock ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.feedstock.bit_h: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.bit_h_set(SAM_Biomass_Feedstock ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.feedstock.bit_moisture_wet: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.bit_moisture_wet_set(SAM_Biomass_Feedstock ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.feedstock.bit_resource: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.bit_resource_set(SAM_Biomass_Feedstock ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.feedstock.coal_opt: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.coal_opt_set(SAM_Biomass_Feedstock ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.feedstock.collection_radius: 
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.collection_radius_set(SAM_Biomass_Feedstock ptr, float number, SAM_error* err);

	/**
	 * Set biopwr.feedstock.feedstock1_c: 
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.feedstock1_c_set(SAM_Biomass_Feedstock ptr, float number, SAM_error* err);

	/**
	 * Set biopwr.feedstock.feedstock1_h: 
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.feedstock1_h_set(SAM_Biomass_Feedstock ptr, float number, SAM_error* err);

	/**
	 * Set biopwr.feedstock.feedstock1_moisture_wet: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.feedstock1_moisture_wet_set(SAM_Biomass_Feedstock ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.feedstock.feedstock1_opt: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.feedstock1_opt_set(SAM_Biomass_Feedstock ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.feedstock.feedstock1_resource: Opt feedstock 1 (dt/yr)
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.feedstock1_resource_set(SAM_Biomass_Feedstock ptr, float number, SAM_error* err);

	/**
	 * Set biopwr.feedstock.feedstock1_user_hhv: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.feedstock1_user_hhv_set(SAM_Biomass_Feedstock ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.feedstock.feedstock2_c: 
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.feedstock2_c_set(SAM_Biomass_Feedstock ptr, float number, SAM_error* err);

	/**
	 * Set biopwr.feedstock.feedstock2_h: 
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.feedstock2_h_set(SAM_Biomass_Feedstock ptr, float number, SAM_error* err);

	/**
	 * Set biopwr.feedstock.feedstock2_moisture_wet: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.feedstock2_moisture_wet_set(SAM_Biomass_Feedstock ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.feedstock.feedstock2_opt: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.feedstock2_opt_set(SAM_Biomass_Feedstock ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.feedstock.feedstock2_resource: Opt feedstock 2 (dt/yr)
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.feedstock2_resource_set(SAM_Biomass_Feedstock ptr, float number, SAM_error* err);

	/**
	 * Set biopwr.feedstock.feedstock2_user_hhv: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.feedstock2_user_hhv_set(SAM_Biomass_Feedstock ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.feedstock.forest_c: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.forest_c_set(SAM_Biomass_Feedstock ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.feedstock.forest_h: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.forest_h_set(SAM_Biomass_Feedstock ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.feedstock.forest_moisture_wet: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.forest_moisture_wet_set(SAM_Biomass_Feedstock ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.feedstock.forest_obtainable: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.forest_obtainable_set(SAM_Biomass_Feedstock ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.feedstock.forest_resource: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.forest_resource_set(SAM_Biomass_Feedstock ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.feedstock.herb_c: Carbon fraction in herbaceous energy crop
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.herb_c_set(SAM_Biomass_Feedstock ptr, float number, SAM_error* err);

	/**
	 * Set biopwr.feedstock.herb_h: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.herb_h_set(SAM_Biomass_Feedstock ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.feedstock.herb_moisture_wet: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.herb_moisture_wet_set(SAM_Biomass_Feedstock ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.feedstock.herb_obtainable: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.herb_obtainable_set(SAM_Biomass_Feedstock ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.feedstock.herb_resource: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.herb_resource_set(SAM_Biomass_Feedstock ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.feedstock.lig_c: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.lig_c_set(SAM_Biomass_Feedstock ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.feedstock.lig_h: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.lig_h_set(SAM_Biomass_Feedstock ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.feedstock.lig_moisture_wet: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.lig_moisture_wet_set(SAM_Biomass_Feedstock ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.feedstock.lig_resource: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.lig_resource_set(SAM_Biomass_Feedstock ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.feedstock.mill_c: Carbon fraction in mill residue
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.mill_c_set(SAM_Biomass_Feedstock ptr, float number, SAM_error* err);

	/**
	 * Set biopwr.feedstock.mill_h: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.mill_h_set(SAM_Biomass_Feedstock ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.feedstock.mill_moisture_wet: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.mill_moisture_wet_set(SAM_Biomass_Feedstock ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.feedstock.mill_obtainable: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.mill_obtainable_set(SAM_Biomass_Feedstock ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.feedstock.mill_resource: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.mill_resource_set(SAM_Biomass_Feedstock ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.feedstock.rice_c: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.rice_c_set(SAM_Biomass_Feedstock ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.feedstock.rice_h: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.rice_h_set(SAM_Biomass_Feedstock ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.feedstock.rice_moisture_wet: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.rice_moisture_wet_set(SAM_Biomass_Feedstock ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.feedstock.rice_obtainable: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.rice_obtainable_set(SAM_Biomass_Feedstock ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.feedstock.rice_resource: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.rice_resource_set(SAM_Biomass_Feedstock ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.feedstock.stover_c: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.stover_c_set(SAM_Biomass_Feedstock ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.feedstock.stover_h: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.stover_h_set(SAM_Biomass_Feedstock ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.feedstock.stover_moisture_wet: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.stover_moisture_wet_set(SAM_Biomass_Feedstock ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.feedstock.stover_obtainable: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.stover_obtainable_set(SAM_Biomass_Feedstock ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.feedstock.stover_resource: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.stover_resource_set(SAM_Biomass_Feedstock ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.feedstock.subbit_c: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.subbit_c_set(SAM_Biomass_Feedstock ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.feedstock.subbit_h: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.subbit_h_set(SAM_Biomass_Feedstock ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.feedstock.subbit_moisture_wet: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.subbit_moisture_wet_set(SAM_Biomass_Feedstock ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.feedstock.subbit_resource: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.subbit_resource_set(SAM_Biomass_Feedstock ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.feedstock.urban_c: Carbon fraction in urban residue
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.urban_c_set(SAM_Biomass_Feedstock ptr, float number, SAM_error* err);

	/**
	 * Set biopwr.feedstock.urban_h: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.urban_h_set(SAM_Biomass_Feedstock ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.feedstock.urban_moisture_wet: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.urban_moisture_wet_set(SAM_Biomass_Feedstock ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.feedstock.urban_obtainable: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.urban_obtainable_set(SAM_Biomass_Feedstock ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.feedstock.urban_resource: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.urban_resource_set(SAM_Biomass_Feedstock ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.feedstock.wheat_c: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.wheat_c_set(SAM_Biomass_Feedstock ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.feedstock.wheat_h: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.wheat_h_set(SAM_Biomass_Feedstock ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.feedstock.wheat_moisture_wet: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.wheat_moisture_wet_set(SAM_Biomass_Feedstock ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.feedstock.wheat_obtainable: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.wheat_obtainable_set(SAM_Biomass_Feedstock ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.feedstock.wheat_resource: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.wheat_resource_set(SAM_Biomass_Feedstock ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.feedstock.woody_c: Carbon fraction in woody energy crop
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.woody_c_set(SAM_Biomass_Feedstock ptr, float number, SAM_error* err);

	/**
	 * Set biopwr.feedstock.woody_h: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.woody_h_set(SAM_Biomass_Feedstock ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.feedstock.woody_moisture_wet: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.woody_moisture_wet_set(SAM_Biomass_Feedstock ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.feedstock.woody_obtainable: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.woody_obtainable_set(SAM_Biomass_Feedstock ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.feedstock.woody_resource: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Feedstock_biopwr.feedstock.woody_resource_set(SAM_Biomass_Feedstock ptr, const char* string, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_Biomass_Feedstock_biopwr.feedstock.additional_opt_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_Feedstock_biopwr.feedstock.bagasse_c_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_Feedstock_biopwr.feedstock.bagasse_h_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_Feedstock_biopwr.feedstock.bagasse_moisture_wet_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_Feedstock_biopwr.feedstock.bagasse_obtainable_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_Feedstock_biopwr.feedstock.bagasse_resource_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_Feedstock_biopwr.feedstock.barley_c_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_Feedstock_biopwr.feedstock.barley_h_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_Feedstock_biopwr.feedstock.barley_moisture_wet_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_Feedstock_biopwr.feedstock.barley_obtainable_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_Feedstock_biopwr.feedstock.barley_resource_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_Feedstock_biopwr.feedstock.bit_c_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_Feedstock_biopwr.feedstock.bit_h_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_Feedstock_biopwr.feedstock.bit_moisture_wet_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_Feedstock_biopwr.feedstock.bit_resource_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_Feedstock_biopwr.feedstock.coal_opt_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT float SAM_Biomass_Feedstock_biopwr.feedstock.collection_radius_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT float SAM_Biomass_Feedstock_biopwr.feedstock.feedstock1_c_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT float SAM_Biomass_Feedstock_biopwr.feedstock.feedstock1_h_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_Feedstock_biopwr.feedstock.feedstock1_moisture_wet_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_Feedstock_biopwr.feedstock.feedstock1_opt_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT float SAM_Biomass_Feedstock_biopwr.feedstock.feedstock1_resource_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_Feedstock_biopwr.feedstock.feedstock1_user_hhv_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT float SAM_Biomass_Feedstock_biopwr.feedstock.feedstock2_c_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT float SAM_Biomass_Feedstock_biopwr.feedstock.feedstock2_h_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_Feedstock_biopwr.feedstock.feedstock2_moisture_wet_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_Feedstock_biopwr.feedstock.feedstock2_opt_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT float SAM_Biomass_Feedstock_biopwr.feedstock.feedstock2_resource_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_Feedstock_biopwr.feedstock.feedstock2_user_hhv_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_Feedstock_biopwr.feedstock.forest_c_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_Feedstock_biopwr.feedstock.forest_h_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_Feedstock_biopwr.feedstock.forest_moisture_wet_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_Feedstock_biopwr.feedstock.forest_obtainable_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_Feedstock_biopwr.feedstock.forest_resource_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT float SAM_Biomass_Feedstock_biopwr.feedstock.herb_c_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_Feedstock_biopwr.feedstock.herb_h_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_Feedstock_biopwr.feedstock.herb_moisture_wet_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_Feedstock_biopwr.feedstock.herb_obtainable_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_Feedstock_biopwr.feedstock.herb_resource_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_Feedstock_biopwr.feedstock.lig_c_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_Feedstock_biopwr.feedstock.lig_h_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_Feedstock_biopwr.feedstock.lig_moisture_wet_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_Feedstock_biopwr.feedstock.lig_resource_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT float SAM_Biomass_Feedstock_biopwr.feedstock.mill_c_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_Feedstock_biopwr.feedstock.mill_h_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_Feedstock_biopwr.feedstock.mill_moisture_wet_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_Feedstock_biopwr.feedstock.mill_obtainable_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_Feedstock_biopwr.feedstock.mill_resource_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_Feedstock_biopwr.feedstock.rice_c_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_Feedstock_biopwr.feedstock.rice_h_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_Feedstock_biopwr.feedstock.rice_moisture_wet_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_Feedstock_biopwr.feedstock.rice_obtainable_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_Feedstock_biopwr.feedstock.rice_resource_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_Feedstock_biopwr.feedstock.stover_c_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_Feedstock_biopwr.feedstock.stover_h_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_Feedstock_biopwr.feedstock.stover_moisture_wet_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_Feedstock_biopwr.feedstock.stover_obtainable_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_Feedstock_biopwr.feedstock.stover_resource_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_Feedstock_biopwr.feedstock.subbit_c_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_Feedstock_biopwr.feedstock.subbit_h_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_Feedstock_biopwr.feedstock.subbit_moisture_wet_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_Feedstock_biopwr.feedstock.subbit_resource_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT float SAM_Biomass_Feedstock_biopwr.feedstock.urban_c_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_Feedstock_biopwr.feedstock.urban_h_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_Feedstock_biopwr.feedstock.urban_moisture_wet_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_Feedstock_biopwr.feedstock.urban_obtainable_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_Feedstock_biopwr.feedstock.urban_resource_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_Feedstock_biopwr.feedstock.wheat_c_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_Feedstock_biopwr.feedstock.wheat_h_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_Feedstock_biopwr.feedstock.wheat_moisture_wet_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_Feedstock_biopwr.feedstock.wheat_obtainable_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_Feedstock_biopwr.feedstock.wheat_resource_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT float SAM_Biomass_Feedstock_biopwr.feedstock.woody_c_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_Feedstock_biopwr.feedstock.woody_h_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_Feedstock_biopwr.feedstock.woody_moisture_wet_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_Feedstock_biopwr.feedstock.woody_obtainable_get(SAM_Biomass_Feedstock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_Feedstock_biopwr.feedstock.woody_resource_get(SAM_Biomass_Feedstock ptr, SAM_error* err);



	/** 
	 * Create a PlantSpecs variable table for a BiopowerNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_Biomass_PlantSpecs SAM_Biomass_PlantSpecs_create(const char* def, SAM_error* err);


	/**
	 * Set biopwr.plant.boiler.excess_air: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_PlantSpecs_biopwr.plant.boiler.excess_air_set(SAM_Biomass_PlantSpecs ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.plant.boiler.flue_temp: 
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_PlantSpecs_biopwr.plant.boiler.flue_temp_set(SAM_Biomass_PlantSpecs ptr, float number, SAM_error* err);

	/**
	 * Set biopwr.plant.boiler.num: 
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_PlantSpecs_biopwr.plant.boiler.num_set(SAM_Biomass_PlantSpecs ptr, float number, SAM_error* err);

	/**
	 * Set biopwr.plant.boiler.over_design: 
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_PlantSpecs_biopwr.plant.boiler.over_design_set(SAM_Biomass_PlantSpecs ptr, float number, SAM_error* err);

	/**
	 * Set biopwr.plant.boiler.steam_grade: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_PlantSpecs_biopwr.plant.boiler.steam_grade_set(SAM_Biomass_PlantSpecs ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.plant.combustor_type: 
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: INTEGER
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_PlantSpecs_biopwr.plant.combustor_type_set(SAM_Biomass_PlantSpecs ptr, float number, SAM_error* err);

	/**
	 * Set biopwr.plant.drying_method: 
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: INTEGER
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_PlantSpecs_biopwr.plant.drying_method_set(SAM_Biomass_PlantSpecs ptr, float number, SAM_error* err);

	/**
	 * Set biopwr.plant.drying_spec_wet: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_PlantSpecs_biopwr.plant.drying_spec_wet_set(SAM_Biomass_PlantSpecs ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.plant.par_percent: 
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_PlantSpecs_biopwr.plant.par_percent_set(SAM_Biomass_PlantSpecs ptr, float number, SAM_error* err);

	/**
	 * Set biopwr.plant.ramp_opt: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_PlantSpecs_biopwr.plant.ramp_opt_set(SAM_Biomass_PlantSpecs ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.plant.ramp_opt1: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_PlantSpecs_biopwr.plant.ramp_opt1_set(SAM_Biomass_PlantSpecs ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.plant.ramp_opt2: Local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_PlantSpecs_biopwr.plant.ramp_opt2_set(SAM_Biomass_PlantSpecs ptr, const char* string, SAM_error* err);

	/**
	 * Set biopwr.plant.rated_eff: 
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_PlantSpecs_biopwr.plant.rated_eff_set(SAM_Biomass_PlantSpecs ptr, float number, SAM_error* err);

	/**
	 * Set biopwr.plant.tou_option: 
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: INTEGER
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_PlantSpecs_biopwr.plant.tou_option_set(SAM_Biomass_PlantSpecs ptr, float number, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT const char* SAM_Biomass_PlantSpecs_biopwr.plant.boiler.excess_air_get(SAM_Biomass_PlantSpecs ptr, SAM_error* err);

	SAM_EXPORT float SAM_Biomass_PlantSpecs_biopwr.plant.boiler.flue_temp_get(SAM_Biomass_PlantSpecs ptr, SAM_error* err);

	SAM_EXPORT float SAM_Biomass_PlantSpecs_biopwr.plant.boiler.num_get(SAM_Biomass_PlantSpecs ptr, SAM_error* err);

	SAM_EXPORT float SAM_Biomass_PlantSpecs_biopwr.plant.boiler.over_design_get(SAM_Biomass_PlantSpecs ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_PlantSpecs_biopwr.plant.boiler.steam_grade_get(SAM_Biomass_PlantSpecs ptr, SAM_error* err);

	SAM_EXPORT float SAM_Biomass_PlantSpecs_biopwr.plant.combustor_type_get(SAM_Biomass_PlantSpecs ptr, SAM_error* err);

	SAM_EXPORT float SAM_Biomass_PlantSpecs_biopwr.plant.drying_method_get(SAM_Biomass_PlantSpecs ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_PlantSpecs_biopwr.plant.drying_spec_wet_get(SAM_Biomass_PlantSpecs ptr, SAM_error* err);

	SAM_EXPORT float SAM_Biomass_PlantSpecs_biopwr.plant.par_percent_get(SAM_Biomass_PlantSpecs ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_PlantSpecs_biopwr.plant.ramp_opt_get(SAM_Biomass_PlantSpecs ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_PlantSpecs_biopwr.plant.ramp_opt1_get(SAM_Biomass_PlantSpecs ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Biomass_PlantSpecs_biopwr.plant.ramp_opt2_get(SAM_Biomass_PlantSpecs ptr, SAM_error* err);

	SAM_EXPORT float SAM_Biomass_PlantSpecs_biopwr.plant.rated_eff_get(SAM_Biomass_PlantSpecs ptr, SAM_error* err);

	SAM_EXPORT float SAM_Biomass_PlantSpecs_biopwr.plant.tou_option_get(SAM_Biomass_PlantSpecs ptr, SAM_error* err);



	/** 
	 * Create a Emissions variable table for a BiopowerNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_Biomass_Emissions SAM_Biomass_Emissions_create(const char* def, SAM_error* err);


	/**
	 * Set biopwr.emissions.transport_legs: 
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: INTEGER
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Emissions_biopwr.emissions.transport_legs_set(SAM_Biomass_Emissions ptr, float number, SAM_error* err);

	/**
	 * Set biopwr.emissions.transport_long: 
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: INTEGER
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Emissions_biopwr.emissions.transport_long_set(SAM_Biomass_Emissions ptr, float number, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_Biomass_Emissions_biopwr.emissions.transport_legs_get(SAM_Biomass_Emissions ptr, SAM_error* err);

	SAM_EXPORT float SAM_Biomass_Emissions_biopwr.emissions.transport_long_get(SAM_Biomass_Emissions ptr, SAM_error* err);



	/** 
	 * Create a Common variable table for a BiopowerNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_Biomass_Common SAM_Biomass_Common_create(const char* def, SAM_error* err);


	/**
	 * Set adjust:constant: Constant loss adjustment
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: MAX=100
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Common_adjust:constant_set(SAM_Biomass_Common ptr, float number, SAM_error* err);

	/**
	 * Set adjust:hourly: Hourly loss adjustments
	 * type: array
	 * units: %
	 * options: None
	 * constraints: LENGTH=8760
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Common_adjust:hourly_set(SAM_Biomass_Common ptr, float* array, int length, SAM_error* err);

	/**
	 * Set adjust:periods: Period-based loss adjustments
	 * type: matrix
	 * units: %
	 * options: n x 3 matrix [ start, end, loss ]
	 * constraints: COLS=3
	 * required if: None
	 */
	SAM_EXPORT void SAM_Biomass_Common_adjust:periods_set(SAM_Biomass_Common ptr, float* matrix, int nr, int nc, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_Biomass_Common_adjust:constant_get(SAM_Biomass_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Biomass_Common_adjust:hourly_get(SAM_Biomass_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Biomass_Common_adjust:periods_get(SAM_Biomass_Common ptr, SAM_error* err);



#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif