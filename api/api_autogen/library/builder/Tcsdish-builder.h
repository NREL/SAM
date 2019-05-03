#ifndef SAM_TCSDISH_FUNCTIONS_H_
#define SAM_TCSDISH_FUNCTIONS_H_

#include "Tcsdish-data.h"

#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	/** 
	 * Create a SolarField variable table for a DishStirlingNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_Tcsdish_SolarField SAM_Tcsdish_SolarField_create(const char* def, SAM_error* err);


	/**
	 * Set ew_dish_sep: Collector separation East-West
	 * type: numeric
	 * units: m
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Tcsdish_SolarField_ew_dish_sep_set(SAM_Tcsdish_SolarField ptr, float number, SAM_error* err);

	/**
	 * Set n_ew: Number of collectors East-West
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: INTEGER
	 * required if: None
	 */
	SAM_EXPORT void SAM_Tcsdish_SolarField_n_ew_set(SAM_Tcsdish_SolarField ptr, float number, SAM_error* err);

	/**
	 * Set n_ns: Number of collectors North-South
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: INTEGER
	 * required if: None
	 */
	SAM_EXPORT void SAM_Tcsdish_SolarField_n_ns_set(SAM_Tcsdish_SolarField ptr, float number, SAM_error* err);

	/**
	 * Set ns_dish_sep: Collector separation North-South
	 * type: numeric
	 * units: m
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Tcsdish_SolarField_ns_dish_sep_set(SAM_Tcsdish_SolarField ptr, float number, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_Tcsdish_SolarField_ew_dish_sep_get(SAM_Tcsdish_SolarField ptr, SAM_error* err);

	SAM_EXPORT float SAM_Tcsdish_SolarField_n_ew_get(SAM_Tcsdish_SolarField ptr, SAM_error* err);

	SAM_EXPORT float SAM_Tcsdish_SolarField_n_ns_get(SAM_Tcsdish_SolarField ptr, SAM_error* err);

	SAM_EXPORT float SAM_Tcsdish_SolarField_ns_dish_sep_get(SAM_Tcsdish_SolarField ptr, SAM_error* err);



	/** 
	 * Create a Common variable table for a DishStirlingNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_Tcsdish_Common SAM_Tcsdish_Common_create(const char* def, SAM_error* err);


	/**
	 * Set adjust:constant: Constant loss adjustment
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: MAX=100
	 * required if: None
	 */
	SAM_EXPORT void SAM_Tcsdish_Common_adjust:constant_set(SAM_Tcsdish_Common ptr, float number, SAM_error* err);

	/**
	 * Set adjust:hourly: Hourly loss adjustments
	 * type: array
	 * units: %
	 * options: None
	 * constraints: LENGTH=8760
	 * required if: None
	 */
	SAM_EXPORT void SAM_Tcsdish_Common_adjust:hourly_set(SAM_Tcsdish_Common ptr, float* array, int length, SAM_error* err);

	/**
	 * Set adjust:periods: Period-based loss adjustments
	 * type: matrix
	 * units: %
	 * options: n x 3 matrix [ start, end, loss ]
	 * constraints: COLS=3
	 * required if: None
	 */
	SAM_EXPORT void SAM_Tcsdish_Common_adjust:periods_set(SAM_Tcsdish_Common ptr, float* matrix, int nr, int nc, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_Tcsdish_Common_adjust:constant_get(SAM_Tcsdish_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Tcsdish_Common_adjust:hourly_get(SAM_Tcsdish_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Tcsdish_Common_adjust:periods_get(SAM_Tcsdish_Common ptr, SAM_error* err);



#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif