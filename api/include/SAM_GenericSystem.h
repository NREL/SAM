#ifndef SAM_GENERICSYSTEM_H_
#define SAM_GENERICSYSTEM_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// GenericSystem Technology Model
	//

	/** 
	 * Create a GenericSystem variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_GenericSystem;

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_GenericSystem_execute(SAM_table data, int verbosity, SAM_error* err);


	//
	// Plant parameters
	//

	/**
	 * Set conv_eff: Conversion Efficiency [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_GenericSystem_Plant_conv_eff_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set derate: Derate [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_GenericSystem_Plant_derate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set energy_output_array: Array of Energy Output Profile [kW]
	 * options: None
	 * constraints: None
	 * required if: spec_mode=1
	 */
	SAM_EXPORT void SAM_GenericSystem_Plant_energy_output_array_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set heat_rate: Heat Rate [MMBTUs/MWhe]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_GenericSystem_Plant_heat_rate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set spec_mode: Spec mode: 0=constant CF,1=profile
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_GenericSystem_Plant_spec_mode_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set system_capacity: Nameplace Capcity [kW]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_GenericSystem_Plant_system_capacity_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set user_capacity_factor: Capacity Factor [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_GenericSystem_Plant_user_capacity_factor_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// Lifetime parameters
	//

	/**
	 * Set analysis_period: Lifetime analysis period [years]
	 * options: None
	 * constraints: None
	 * required if: system_use_lifetime_output=1
	 */
	SAM_EXPORT void SAM_GenericSystem_Lifetime_analysis_period_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set generic_degradation: Annual AC degradation [%/year]
	 * options: None
	 * constraints: None
	 * required if: system_use_lifetime_output=1
	 */
	SAM_EXPORT void SAM_GenericSystem_Lifetime_generic_degradation_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set system_use_lifetime_output: Generic lifetime simulation [0/1]
	 * options: None
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_GenericSystem_Lifetime_system_use_lifetime_output_nset(SAM_table ptr, double number, SAM_error *err);


	/**
	 * Plant Getters
	 */

	SAM_EXPORT double SAM_GenericSystem_Plant_conv_eff_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GenericSystem_Plant_derate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_GenericSystem_Plant_energy_output_array_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_GenericSystem_Plant_heat_rate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GenericSystem_Plant_spec_mode_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GenericSystem_Plant_system_capacity_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GenericSystem_Plant_user_capacity_factor_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Lifetime Getters
	 */

	SAM_EXPORT double SAM_GenericSystem_Lifetime_analysis_period_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_GenericSystem_Lifetime_generic_degradation_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_GenericSystem_Lifetime_system_use_lifetime_output_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double SAM_GenericSystem_Outputs_annual_energy_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_GenericSystem_Outputs_annual_energy_distribution_time_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_GenericSystem_Outputs_annual_fuel_usage_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GenericSystem_Outputs_capacity_factor_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_GenericSystem_Outputs_gen_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_GenericSystem_Outputs_kwh_per_kw_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_GenericSystem_Outputs_monthly_energy_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_GenericSystem_Outputs_system_heat_rate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_GenericSystem_Outputs_water_usage_nget(SAM_table ptr, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif