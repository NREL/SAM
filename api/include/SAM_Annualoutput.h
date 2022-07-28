#ifndef SAM_ANNUALOUTPUT_H_
#define SAM_ANNUALOUTPUT_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// Annualoutput Technology Model
	//

	/** 
	 * Create a Annualoutput variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_Annualoutput;

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_Annualoutput_execute(SAM_table data, int verbosity, SAM_error* err);


	//
	// AnnualOutput parameters
	//

	/**
	 * Set analysis_period: Analyis period [years]
	 * options: None
	 * constraints: INTEGER,MIN=0,MAX=50
	 * required if: ?=30
	 */
	SAM_EXPORT void SAM_Annualoutput_AnnualOutput_analysis_period_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set energy_availability: Annual energy availability [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Annualoutput_AnnualOutput_energy_availability_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set energy_curtailment: First year energy curtailment
	 * options: (0..1)
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Annualoutput_AnnualOutput_energy_curtailment_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set energy_degradation: Annual energy degradation [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Annualoutput_AnnualOutput_energy_degradation_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set system_hourly_energy: Hourly energy produced by the system [kW]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Annualoutput_AnnualOutput_system_hourly_energy_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set system_use_lifetime_output: Lifetime hourly system outputs [0/1]
	 * options: 0=hourly first year,1=hourly lifetime
	 * constraints: INTEGER,MIN=0
	 * required if: *
	 */
	SAM_EXPORT void SAM_Annualoutput_AnnualOutput_system_use_lifetime_output_nset(SAM_table ptr, double number, SAM_error *err);


	/**
	 * AnnualOutput Getters
	 */

	SAM_EXPORT double SAM_Annualoutput_AnnualOutput_analysis_period_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Annualoutput_AnnualOutput_energy_availability_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Annualoutput_AnnualOutput_energy_curtailment_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Annualoutput_AnnualOutput_energy_degradation_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Annualoutput_AnnualOutput_system_hourly_energy_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Annualoutput_AnnualOutput_system_use_lifetime_output_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double* SAM_Annualoutput_Outputs_annual_availability_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Annualoutput_Outputs_annual_degradation_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Annualoutput_Outputs_annual_energy_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Annualoutput_Outputs_hourly_energy_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Annualoutput_Outputs_monthly_energy_aget(SAM_table ptr, int* length, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif