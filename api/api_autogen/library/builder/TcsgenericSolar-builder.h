#ifndef SAM_TCSGENERICSOLAR_FUNCTIONS_H_
#define SAM_TCSGENERICSOLAR_FUNCTIONS_H_

#include "TcsgenericSolar-data.h"

#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	/** 
	 * Create a SolarField variable table for a GenericCSPSystemNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_TcsgenericSolar_SolarField SAM_TcsgenericSolar_SolarField_create(const char* def, SAM_error* err);


	/**
	 * Set OpticalTable: Optical table
	 * type: matrix
	 * units: none
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsgenericSolar_SolarField_OpticalTable_set(SAM_TcsgenericSolar_SolarField ptr, float* matrix, int nr, int nc, SAM_error* err);

	/**
	 * Set eta_opt_gen: General/other optical derate
	 * type: numeric
	 * units: none
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsgenericSolar_SolarField_eta_opt_gen_set(SAM_TcsgenericSolar_SolarField ptr, float number, SAM_error* err);

	/**
	 * Set eta_opt_soil: Soiling optical derate factor
	 * type: numeric
	 * units: none
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsgenericSolar_SolarField_eta_opt_soil_set(SAM_TcsgenericSolar_SolarField ptr, float number, SAM_error* err);

	/**
	 * Set f_sfhl_ref: Reference solar field thermal loss fraction
	 * type: numeric
	 * units: MW/MWcap
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsgenericSolar_SolarField_f_sfhl_ref_set(SAM_TcsgenericSolar_SolarField ptr, float number, SAM_error* err);

	/**
	 * Set irr_des: Irradiation design point
	 * type: numeric
	 * units: W/m2
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsgenericSolar_SolarField_irr_des_set(SAM_TcsgenericSolar_SolarField ptr, float number, SAM_error* err);

	/**
	 * Set istableunsorted: Is optical table unsorted format?none
	 * type: numeric
	 * units: None
	 * options: type_260
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsgenericSolar_SolarField_istableunsorted_set(SAM_TcsgenericSolar_SolarField ptr, float number, SAM_error* err);

	/**
	 * Set solarm: Solar multiple
	 * type: numeric
	 * units: none
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsgenericSolar_SolarField_solarm_set(SAM_TcsgenericSolar_SolarField ptr, float number, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float* SAM_TcsgenericSolar_SolarField_OpticalTable_get(SAM_TcsgenericSolar_SolarField ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsgenericSolar_SolarField_eta_opt_gen_get(SAM_TcsgenericSolar_SolarField ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsgenericSolar_SolarField_eta_opt_soil_get(SAM_TcsgenericSolar_SolarField ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsgenericSolar_SolarField_f_sfhl_ref_get(SAM_TcsgenericSolar_SolarField ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsgenericSolar_SolarField_irr_des_get(SAM_TcsgenericSolar_SolarField ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsgenericSolar_SolarField_istableunsorted_get(SAM_TcsgenericSolar_SolarField ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsgenericSolar_SolarField_solarm_get(SAM_TcsgenericSolar_SolarField ptr, SAM_error* err);



	/** 
	 * Create a PowerBlock variable table for a GenericCSPSystemNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_TcsgenericSolar_PowerBlock SAM_TcsgenericSolar_PowerBlock_create(const char* def, SAM_error* err);


	/**
	 * Set eta_des: Design power cycle gross efficiency
	 * type: numeric
	 * units: none
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsgenericSolar_PowerBlock_eta_des_set(SAM_TcsgenericSolar_PowerBlock ptr, float number, SAM_error* err);

	/**
	 * Set f_Wpar_fixed: Fixed capacity-based parasitic loss fraction
	 * type: numeric
	 * units: MWe/MWcap
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsgenericSolar_PowerBlock_f_Wpar_fixed_set(SAM_TcsgenericSolar_PowerBlock ptr, float number, SAM_error* err);

	/**
	 * Set f_Wpar_prod: Production-based parasitic loss fraction
	 * type: numeric
	 * units: MWe/MWe
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsgenericSolar_PowerBlock_f_Wpar_prod_set(SAM_TcsgenericSolar_PowerBlock ptr, float number, SAM_error* err);

	/**
	 * Set w_des: Design power cycle gross output
	 * type: numeric
	 * units: MWe
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsgenericSolar_PowerBlock_w_des_set(SAM_TcsgenericSolar_PowerBlock ptr, float number, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_TcsgenericSolar_PowerBlock_eta_des_get(SAM_TcsgenericSolar_PowerBlock ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsgenericSolar_PowerBlock_f_Wpar_fixed_get(SAM_TcsgenericSolar_PowerBlock ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsgenericSolar_PowerBlock_f_Wpar_prod_get(SAM_TcsgenericSolar_PowerBlock ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsgenericSolar_PowerBlock_w_des_get(SAM_TcsgenericSolar_PowerBlock ptr, SAM_error* err);



	/** 
	 * Create a ThermalStorage variable table for a GenericCSPSystemNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_TcsgenericSolar_ThermalStorage SAM_TcsgenericSolar_ThermalStorage_create(const char* def, SAM_error* err);


	/**
	 * Set hrs_tes: Equivalent full-load hours of storage
	 * type: numeric
	 * units: hours
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsgenericSolar_ThermalStorage_hrs_tes_set(SAM_TcsgenericSolar_ThermalStorage ptr, float number, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_TcsgenericSolar_ThermalStorage_hrs_tes_get(SAM_TcsgenericSolar_ThermalStorage ptr, SAM_error* err);



	/** 
	 * Create a Common variable table for a GenericCSPSystemNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_TcsgenericSolar_Common SAM_TcsgenericSolar_Common_create(const char* def, SAM_error* err);


	/**
	 * Set adjust:constant: Constant loss adjustment
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: MAX=100
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsgenericSolar_Common_adjust:constant_set(SAM_TcsgenericSolar_Common ptr, float number, SAM_error* err);

	/**
	 * Set adjust:hourly: Hourly loss adjustments
	 * type: array
	 * units: %
	 * options: None
	 * constraints: LENGTH=8760
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsgenericSolar_Common_adjust:hourly_set(SAM_TcsgenericSolar_Common ptr, float* array, int length, SAM_error* err);

	/**
	 * Set adjust:periods: Period-based loss adjustments
	 * type: matrix
	 * units: %
	 * options: n x 3 matrix [ start, end, loss ]
	 * constraints: COLS=3
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsgenericSolar_Common_adjust:periods_set(SAM_TcsgenericSolar_Common ptr, float* matrix, int nr, int nc, SAM_error* err);

	/**
	 * Set sf_adjust:constant: SF Constant loss adjustment
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: MAX=100
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsgenericSolar_Common_sf_adjust:constant_set(SAM_TcsgenericSolar_Common ptr, float number, SAM_error* err);

	/**
	 * Set sf_adjust:hourly: SF Hourly loss adjustments
	 * type: array
	 * units: %
	 * options: None
	 * constraints: LENGTH=8760
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsgenericSolar_Common_sf_adjust:hourly_set(SAM_TcsgenericSolar_Common ptr, float* array, int length, SAM_error* err);

	/**
	 * Set sf_adjust:periods: SF Period-based loss adjustments
	 * type: matrix
	 * units: %
	 * options: n x 3 matrix [ start, end, loss ]
	 * constraints: COLS=3
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcsgenericSolar_Common_sf_adjust:periods_set(SAM_TcsgenericSolar_Common ptr, float* matrix, int nr, int nc, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_TcsgenericSolar_Common_adjust:constant_get(SAM_TcsgenericSolar_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_TcsgenericSolar_Common_adjust:hourly_get(SAM_TcsgenericSolar_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_TcsgenericSolar_Common_adjust:periods_get(SAM_TcsgenericSolar_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcsgenericSolar_Common_sf_adjust:constant_get(SAM_TcsgenericSolar_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_TcsgenericSolar_Common_sf_adjust:hourly_get(SAM_TcsgenericSolar_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_TcsgenericSolar_Common_sf_adjust:periods_get(SAM_TcsgenericSolar_Common ptr, SAM_error* err);



#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif