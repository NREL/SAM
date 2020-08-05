#ifndef SAM_GRID_H_
#define SAM_GRID_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// Grid Technology Model
	//

	/** 
	 * Create a Grid variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_Grid;

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_Grid_execute(SAM_table data, int verbosity, SAM_error* err);


	//
	// Lifetime parameters
	//

	/**
	 * Set analysis_period: Lifetime analysis period [years]
	 * options: The number of years in the simulation
	 * constraints: None
	 * required if: system_use_lifetime_output=1
	 */
	SAM_EXPORT void SAM_Grid_Lifetime_analysis_period_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set system_use_lifetime_output: Lifetime simulation [0/1]
	 * options: 0=SingleYearRepeated,1=RunEveryYear
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Grid_Lifetime_system_use_lifetime_output_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// GridLimits parameters
	//

	/**
	 * Set enable_interconnection_limit: Enable grid interconnection limit [0/1]
	 * options: Enable a grid interconnection limit
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Grid_GridLimits_enable_interconnection_limit_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set grid_curtailment: Grid curtailment as energy delivery limit (first year) [MW]
	 * options: None
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Grid_GridLimits_grid_curtailment_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set grid_interconnection_limit_kwac: Grid interconnection limit [kWac]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Grid_GridLimits_grid_interconnection_limit_kwac_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// SystemOutput parameters
	//

	/**
	 * Set annual_energy: Annual Energy AC (year 1) [kWh]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Grid_SystemOutput_annual_energy_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set gen: System power generated [kW]
	 * options: Lifetime system generation
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Grid_SystemOutput_gen_aset(SAM_table ptr, double* arr, int length, SAM_error *err);


	//
	// Load parameters
	//

	/**
	 * Set load: Electricity load (year 1) [kW]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Grid_Load_load_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set load_escalation: Annual load escalation [%/year]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Grid_Load_load_escalation_aset(SAM_table ptr, double* arr, int length, SAM_error *err);


	/**
	 * Lifetime Getters
	 */

	SAM_EXPORT double SAM_Grid_Lifetime_analysis_period_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Grid_Lifetime_system_use_lifetime_output_nget(SAM_table ptr, SAM_error *err);


	/**
	 * GridLimits Getters
	 */

	SAM_EXPORT double SAM_Grid_GridLimits_enable_interconnection_limit_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Grid_GridLimits_grid_curtailment_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Grid_GridLimits_grid_interconnection_limit_kwac_nget(SAM_table ptr, SAM_error *err);


	/**
	 * SystemOutput Getters
	 */

	SAM_EXPORT double SAM_Grid_SystemOutput_annual_energy_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Grid_SystemOutput_gen_aget(SAM_table ptr, int* length, SAM_error *err);


	/**
	 * Load Getters
	 */

	SAM_EXPORT double* SAM_Grid_Load_load_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Grid_Load_load_escalation_aget(SAM_table ptr, int* length, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double SAM_Grid_Outputs_annual_ac_curtailment_loss_kwh_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Grid_Outputs_annual_ac_curtailment_loss_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Grid_Outputs_annual_ac_interconnect_loss_kwh_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Grid_Outputs_annual_ac_interconnect_loss_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Grid_Outputs_annual_energy_pre_curtailment_ac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Grid_Outputs_annual_energy_pre_interconnect_ac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Grid_Outputs_capacity_factor_curtailment_ac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Grid_Outputs_capacity_factor_interconnect_ac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Grid_Outputs_gen_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Grid_Outputs_system_pre_curtailment_kwac_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Grid_Outputs_system_pre_interconnect_kwac_aget(SAM_table ptr, int* length, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif