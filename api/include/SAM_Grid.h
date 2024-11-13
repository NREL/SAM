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
	// SystemOutput parameters
	//

	/**
	 * Set annual_energy: Annual AC energy in Year 1 [kWh]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Grid_SystemOutput_annual_energy_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set energy_hourly_kW: Power output of array [kW]
	 * options: Lifetime system generation
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Grid_SystemOutput_energy_hourly_kW_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

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
	 * Set crit_load: Critical electricity load (year 1) [kW]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Grid_Load_crit_load_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set grid_outage: Grid outage in this time step [0/1]
	 * options: 0=GridAvailable,1=GridUnavailable,Length=load
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Grid_Load_grid_outage_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

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


	//
	// Monthly parameters
	//

	/**
	 * Set monthly_energy: Monthly AC energy in Year 1 [kWh/mo]
	 * options: None
	 * constraints: LENGTH=12
	 * required if: None
	 */
	SAM_EXPORT void SAM_Grid_Monthly_monthly_energy_aset(SAM_table ptr, double* arr, int length, SAM_error *err);


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
	// HybridCosts parameters
	//

	/**
	 * Set degradation: Annual AC degradation [%]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Grid_HybridCosts_degradation_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set land_area: Total land area [acres]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Grid_HybridCosts_land_area_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set om_capacity: Capacity-based O&M amount [$/kWcap]
	 * options: !battery,!fuelcell
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Grid_HybridCosts_om_capacity_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set om_capacity_escal: Capacity-based O&M escalation [%/year]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Grid_HybridCosts_om_capacity_escal_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set om_fixed: Fixed O&M annual amount [$/year]
	 * options: !battery,!fuelcell
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Grid_HybridCosts_om_fixed_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set om_fixed_escal: Fixed O&M escalation [%/year]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Grid_HybridCosts_om_fixed_escal_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set om_land_lease: Land lease cost [$/acre]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Grid_HybridCosts_om_land_lease_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set om_land_lease_escal: Land lease cost escalation [%/yr]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Grid_HybridCosts_om_land_lease_escal_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set om_production: Production-based O&M amount [$/MWh]
	 * options: !battery,!fuelcell
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Grid_HybridCosts_om_production_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set om_production_escal: Production-based O&M escalation [%/year]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Grid_HybridCosts_om_production_escal_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set total_installed_cost: Total installed cost [$]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Grid_HybridCosts_total_installed_cost_nset(SAM_table ptr, double number, SAM_error *err);


	/**
	 * Lifetime Getters
	 */

	SAM_EXPORT double SAM_Grid_Lifetime_analysis_period_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Grid_Lifetime_system_use_lifetime_output_nget(SAM_table ptr, SAM_error *err);


	/**
	 * SystemOutput Getters
	 */

	SAM_EXPORT double SAM_Grid_SystemOutput_annual_energy_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Grid_SystemOutput_energy_hourly_kW_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Grid_SystemOutput_gen_aget(SAM_table ptr, int* length, SAM_error *err);


	/**
	 * Load Getters
	 */

	SAM_EXPORT double* SAM_Grid_Load_crit_load_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Grid_Load_grid_outage_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Grid_Load_load_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Grid_Load_load_escalation_aget(SAM_table ptr, int* length, SAM_error *err);


	/**
	 * Monthly Getters
	 */

	SAM_EXPORT double* SAM_Grid_Monthly_monthly_energy_aget(SAM_table ptr, int* length, SAM_error *err);


	/**
	 * GridLimits Getters
	 */

	SAM_EXPORT double SAM_Grid_GridLimits_enable_interconnection_limit_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Grid_GridLimits_grid_curtailment_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Grid_GridLimits_grid_interconnection_limit_kwac_nget(SAM_table ptr, SAM_error *err);


	/**
	 * HybridCosts Getters
	 */

	SAM_EXPORT double* SAM_Grid_HybridCosts_degradation_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Grid_HybridCosts_land_area_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Grid_HybridCosts_om_capacity_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Grid_HybridCosts_om_capacity_escal_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Grid_HybridCosts_om_fixed_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Grid_HybridCosts_om_fixed_escal_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Grid_HybridCosts_om_land_lease_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Grid_HybridCosts_om_land_lease_escal_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Grid_HybridCosts_om_production_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Grid_HybridCosts_om_production_escal_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Grid_HybridCosts_total_installed_cost_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double SAM_Grid_Outputs_annual_ac_curtailment_loss_kwh_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Grid_Outputs_annual_ac_curtailment_loss_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Grid_Outputs_annual_ac_interconnect_loss_kwh_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Grid_Outputs_annual_ac_interconnect_loss_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Grid_Outputs_annual_energy_distribution_time_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Grid_Outputs_annual_energy_pre_curtailment_ac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Grid_Outputs_annual_energy_pre_interconnect_ac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Grid_Outputs_capacity_factor_curtailment_ac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Grid_Outputs_capacity_factor_interconnect_ac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Grid_Outputs_cf_land_lease_expense_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Grid_Outputs_full_load_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Grid_Outputs_gen_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Grid_Outputs_system_pre_curtailment_kwac_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Grid_Outputs_system_pre_interconnect_kwac_aget(SAM_table ptr, int* length, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif