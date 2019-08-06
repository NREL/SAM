#ifndef SAM_GENERICSYSTEM_FUNCTIONS_H_
#define SAM_GENERICSYSTEM_FUNCTIONS_H_

#include "GenericSystem-data.h"

#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	/** 
	 * Create a PowerPlant variable table for a GenericSystemNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_GenericSystem_PowerPlant SAM_GenericSystem_PowerPlant_create(const char* def, SAM_error* err);


	/**
	 * Set derate: Derate
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_GenericSystem_PowerPlant_derate_set(SAM_GenericSystem_PowerPlant ptr, float number, SAM_error* err);

	/**
	 * Set energy_output_array: Array of Energy Output Profile
	 * type: array
	 * units: kW
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_GenericSystem_PowerPlant_energy_output_array_set(SAM_GenericSystem_PowerPlant ptr, float* array, int length, SAM_error* err);

	/**
	 * Set heat_rate: Heat Rate
	 * type: numeric
	 * units: MMBTUs/MWhe
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_GenericSystem_PowerPlant_heat_rate_set(SAM_GenericSystem_PowerPlant ptr, float number, SAM_error* err);

	/**
	 * Set spec_mode: Spec mode: 0=constant CF,1=profile
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_GenericSystem_PowerPlant_spec_mode_set(SAM_GenericSystem_PowerPlant ptr, float number, SAM_error* err);

	/**
	 * Set system_capacity: Nameplace Capcity
	 * type: numeric
	 * units: kW
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_GenericSystem_PowerPlant_system_capacity_set(SAM_GenericSystem_PowerPlant ptr, float number, SAM_error* err);

	/**
	 * Set user_capacity_factor: Capacity Factor
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_GenericSystem_PowerPlant_user_capacity_factor_set(SAM_GenericSystem_PowerPlant ptr, float number, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_GenericSystem_PowerPlant_derate_get(SAM_GenericSystem_PowerPlant ptr, SAM_error* err);

	SAM_EXPORT float* SAM_GenericSystem_PowerPlant_energy_output_array_get(SAM_GenericSystem_PowerPlant ptr, SAM_error* err);

	SAM_EXPORT float SAM_GenericSystem_PowerPlant_heat_rate_get(SAM_GenericSystem_PowerPlant ptr, SAM_error* err);

	SAM_EXPORT float SAM_GenericSystem_PowerPlant_spec_mode_get(SAM_GenericSystem_PowerPlant ptr, SAM_error* err);

	SAM_EXPORT float SAM_GenericSystem_PowerPlant_system_capacity_get(SAM_GenericSystem_PowerPlant ptr, SAM_error* err);

	SAM_EXPORT float SAM_GenericSystem_PowerPlant_user_capacity_factor_get(SAM_GenericSystem_PowerPlant ptr, SAM_error* err);



	/** 
	 * Create a Common variable table for a GenericSystemNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_GenericSystem_Common SAM_GenericSystem_Common_create(const char* def, SAM_error* err);


	/**
	 * Set adjust:constant: Constant loss adjustment
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: MAX=100
	 * required if: None
	 */
	SAM_EXPORT void SAM_GenericSystem_Common_adjust:constant_set(SAM_GenericSystem_Common ptr, float number, SAM_error* err);

	/**
	 * Set adjust:hourly: Hourly loss adjustments
	 * type: array
	 * units: %
	 * options: None
	 * constraints: LENGTH=8760
	 * required if: None
	 */
	SAM_EXPORT void SAM_GenericSystem_Common_adjust:hourly_set(SAM_GenericSystem_Common ptr, float* array, int length, SAM_error* err);

	/**
	 * Set adjust:periods: Period-based loss adjustments
	 * type: matrix
	 * units: %
	 * options: n x 3 matrix [ start, end, loss ]
	 * constraints: COLS=3
	 * required if: None
	 */
	SAM_EXPORT void SAM_GenericSystem_Common_adjust:periods_set(SAM_GenericSystem_Common ptr, float* matrix, int nr, int nc, SAM_error* err);

	/**
	 * Set analysis_period: Lifetime analysis period
	 * type: numeric
	 * units: years
	 * options: None
	 * constraints: None
	 * required if: system_use_lifetime_output=1
	 */
	SAM_EXPORT void SAM_GenericSystem_Common_analysis_period_set(SAM_GenericSystem_Common ptr, float number, SAM_error* err);

	/**
	 * Set dc_adjust:constant: DC Constant loss adjustment
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: MAX=100
	 * required if: None
	 */
	SAM_EXPORT void SAM_GenericSystem_Common_dc_adjust:constant_set(SAM_GenericSystem_Common ptr, float number, SAM_error* err);

	/**
	 * Set dc_adjust:hourly: DC Hourly loss adjustments
	 * type: array
	 * units: %
	 * options: None
	 * constraints: LENGTH=8760
	 * required if: None
	 */
	SAM_EXPORT void SAM_GenericSystem_Common_dc_adjust:hourly_set(SAM_GenericSystem_Common ptr, float* array, int length, SAM_error* err);

	/**
	 * Set dc_adjust:periods: DC Period-based loss adjustments
	 * type: matrix
	 * units: %
	 * options: n x 3 matrix [ start, end, loss ]
	 * constraints: COLS=3
	 * required if: None
	 */
	SAM_EXPORT void SAM_GenericSystem_Common_dc_adjust:periods_set(SAM_GenericSystem_Common ptr, float* matrix, int nr, int nc, SAM_error* err);

	/**
	 * Set generic_degradation: Annual module degradation
	 * type: array
	 * units: %/year
	 * options: None
	 * constraints: None
	 * required if: system_use_lifetime_output=1
	 */
	SAM_EXPORT void SAM_GenericSystem_Common_generic_degradation_set(SAM_GenericSystem_Common ptr, float* array, int length, SAM_error* err);

	/**
	 * Set system_use_lifetime_output: Generic lifetime simulation
	 * type: numeric
	 * units: 0/1
	 * options: None
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_GenericSystem_Common_system_use_lifetime_output_set(SAM_GenericSystem_Common ptr, float number, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_GenericSystem_Common_adjust:constant_get(SAM_GenericSystem_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_GenericSystem_Common_adjust:hourly_get(SAM_GenericSystem_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_GenericSystem_Common_adjust:periods_get(SAM_GenericSystem_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_GenericSystem_Common_analysis_period_get(SAM_GenericSystem_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_GenericSystem_Common_dc_adjust:constant_get(SAM_GenericSystem_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_GenericSystem_Common_dc_adjust:hourly_get(SAM_GenericSystem_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_GenericSystem_Common_dc_adjust:periods_get(SAM_GenericSystem_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_GenericSystem_Common_generic_degradation_get(SAM_GenericSystem_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_GenericSystem_Common_system_use_lifetime_output_get(SAM_GenericSystem_Common ptr, SAM_error* err);



#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif