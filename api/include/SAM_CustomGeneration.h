#ifndef SAM_CUSTOMGENERATION_H_
#define SAM_CUSTOMGENERATION_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// CustomGeneration Technology Model
	//

	/** 
	 * Create a CustomGeneration variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_CustomGeneration;

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_CustomGeneration_execute(SAM_table data, int verbosity, SAM_error* err);


	//
	// Plant parameters
	//

	/**
	 * Set conv_eff: Conversion Efficiency [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CustomGeneration_Plant_conv_eff_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set derate: Derate [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CustomGeneration_Plant_derate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set energy_output_array: Array of Energy Output Profile [kW]
	 * options: None
	 * constraints: None
	 * required if: spec_mode=1
	 */
	SAM_EXPORT void SAM_CustomGeneration_Plant_energy_output_array_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set heat_rate: Heat Rate [MMBTUs/MWhe]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CustomGeneration_Plant_heat_rate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set spec_mode: Spec mode: 0=constant CF,1=profile
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CustomGeneration_Plant_spec_mode_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set system_capacity: Nameplace Capcity [kW]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CustomGeneration_Plant_system_capacity_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set user_capacity_factor: Capacity Factor [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CustomGeneration_Plant_user_capacity_factor_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// Lifetime parameters
	//

	/**
	 * Set analysis_period: Lifetime analysis period [years]
	 * options: None
	 * constraints: None
	 * required if: system_use_lifetime_output=1
	 */
	SAM_EXPORT void SAM_CustomGeneration_Lifetime_analysis_period_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set generic_degradation: Annual AC degradation [%/year]
	 * options: None
	 * constraints: None
	 * required if: system_use_lifetime_output=1
	 */
	SAM_EXPORT void SAM_CustomGeneration_Lifetime_generic_degradation_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set system_use_lifetime_output: Custom generation profile lifetime simulation [0/1]
	 * options: None
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_CustomGeneration_Lifetime_system_use_lifetime_output_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// AdjustmentFactors parameters
	//

	/**
	 * Set adjust_constant: Constant loss adjustment [%]
	 * options: 'adjust' and 'constant' separated by _ instead of : after SAM 2022.12.21
	 * constraints: MAX=100
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_CustomGeneration_AdjustmentFactors_adjust_constant_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set adjust_en_periods: Enable period-based adjustment factors [0/1]
	 * options: 'adjust' and 'en_periods' separated by _ instead of : after SAM 2022.12.21
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_CustomGeneration_AdjustmentFactors_adjust_en_periods_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set adjust_en_timeindex: Enable lifetime adjustment factors [0/1]
	 * options: 'adjust' and 'en_timeindex' separated by _ instead of : after SAM 2022.12.21
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_CustomGeneration_AdjustmentFactors_adjust_en_timeindex_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set adjust_periods: Period-based adjustment factors [%]
	 * options: Syntax: n x 3 matrix [ start, end, loss ]; Version upgrade: 'adjust' and 'periods' separated by _ instead of : after SAM 2022.12.21
	 * constraints: COLS=3
	 * required if: adjust_en_periods=1
	 */
	SAM_EXPORT void SAM_CustomGeneration_AdjustmentFactors_adjust_periods_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set adjust_timeindex: Lifetime adjustment factors [%]
	 * options: 'adjust' and 'timeindex' separated by _ instead of : after SAM 2022.12.21
	 * constraints: None
	 * required if: adjust_en_timeindex=1
	 */
	SAM_EXPORT void SAM_CustomGeneration_AdjustmentFactors_adjust_timeindex_aset(SAM_table ptr, double* arr, int length, SAM_error *err);


	//
	// HybridCosts parameters
	//

	/**
	 * Set annual_fuel_usage_lifetime: Fuel usage (lifetime) [kWht]
	 * options: custom_generation,fuelcell,tcslinearfresnel,tcstroughempirical,tcsgenericsolar,fresnelphysical
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_CustomGeneration_HybridCosts_annual_fuel_usage_lifetime_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set degradation: Annual AC degradation [%]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_CustomGeneration_HybridCosts_degradation_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set land_area: Total land area [acres]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_CustomGeneration_HybridCosts_land_area_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set om_capacity: Capacity-based O&M amount [$/kWcap]
	 * options: !battery,!fuelcell
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_CustomGeneration_HybridCosts_om_capacity_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set om_capacity_escal: Capacity-based O&M escalation [%/year]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_CustomGeneration_HybridCosts_om_capacity_escal_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set om_fixed: Fixed O&M annual amount [$/year]
	 * options: !battery,!fuelcell
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_CustomGeneration_HybridCosts_om_fixed_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set om_fixed_escal: Fixed O&M escalation [%/year]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_CustomGeneration_HybridCosts_om_fixed_escal_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set om_fuel_cost: Fuel cost [$/MMBtu]
	 * options: custom_generation,fuelcell,tcslinearfresnel,tcstroughempirical,tcsgenericsolar,fresnelphysical
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_CustomGeneration_HybridCosts_om_fuel_cost_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set om_fuel_cost_escal: Fuel cost escalation [%/year]
	 * options: custom_generation,fuelcell,tcslinearfresnel,tcstroughempirical,tcsgenericsolar,fresnelphysical
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_CustomGeneration_HybridCosts_om_fuel_cost_escal_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set om_land_lease: Land lease cost [$/acre]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_CustomGeneration_HybridCosts_om_land_lease_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set om_land_lease_escal: Land lease cost escalation [%/yr]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_CustomGeneration_HybridCosts_om_land_lease_escal_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set om_production: Production-based O&M amount [$/MWh]
	 * options: !battery,!fuelcell
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_CustomGeneration_HybridCosts_om_production_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set om_production_escal: Production-based O&M escalation [%/year]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_CustomGeneration_HybridCosts_om_production_escal_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set total_installed_cost: Total installed cost [$]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CustomGeneration_HybridCosts_total_installed_cost_nset(SAM_table ptr, double number, SAM_error *err);


	/**
	 * Plant Getters
	 */

	SAM_EXPORT double SAM_CustomGeneration_Plant_conv_eff_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_CustomGeneration_Plant_derate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_CustomGeneration_Plant_energy_output_array_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_CustomGeneration_Plant_heat_rate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_CustomGeneration_Plant_spec_mode_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_CustomGeneration_Plant_system_capacity_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_CustomGeneration_Plant_user_capacity_factor_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Lifetime Getters
	 */

	SAM_EXPORT double SAM_CustomGeneration_Lifetime_analysis_period_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_CustomGeneration_Lifetime_generic_degradation_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_CustomGeneration_Lifetime_system_use_lifetime_output_nget(SAM_table ptr, SAM_error *err);


	/**
	 * AdjustmentFactors Getters
	 */

	SAM_EXPORT double SAM_CustomGeneration_AdjustmentFactors_adjust_constant_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_CustomGeneration_AdjustmentFactors_adjust_en_periods_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_CustomGeneration_AdjustmentFactors_adjust_en_timeindex_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_CustomGeneration_AdjustmentFactors_adjust_periods_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_CustomGeneration_AdjustmentFactors_adjust_timeindex_aget(SAM_table ptr, int* length, SAM_error *err);


	/**
	 * HybridCosts Getters
	 */

	SAM_EXPORT double* SAM_CustomGeneration_HybridCosts_annual_fuel_usage_lifetime_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_CustomGeneration_HybridCosts_degradation_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_CustomGeneration_HybridCosts_land_area_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_CustomGeneration_HybridCosts_om_capacity_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_CustomGeneration_HybridCosts_om_capacity_escal_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_CustomGeneration_HybridCosts_om_fixed_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_CustomGeneration_HybridCosts_om_fixed_escal_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_CustomGeneration_HybridCosts_om_fuel_cost_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_CustomGeneration_HybridCosts_om_fuel_cost_escal_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_CustomGeneration_HybridCosts_om_land_lease_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_CustomGeneration_HybridCosts_om_land_lease_escal_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_CustomGeneration_HybridCosts_om_production_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_CustomGeneration_HybridCosts_om_production_escal_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_CustomGeneration_HybridCosts_total_installed_cost_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double SAM_CustomGeneration_Outputs_annual_energy_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_CustomGeneration_Outputs_annual_energy_distribution_time_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_CustomGeneration_Outputs_annual_fuel_usage_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_CustomGeneration_Outputs_capacity_factor_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_CustomGeneration_Outputs_cf_battery_replacement_cost_schedule_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_CustomGeneration_Outputs_cf_energy_net_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_CustomGeneration_Outputs_cf_fuelcell_replacement_cost_schedule_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_CustomGeneration_Outputs_cf_land_lease_expense_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_CustomGeneration_Outputs_cf_om_capacity_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_CustomGeneration_Outputs_cf_om_fixed_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_CustomGeneration_Outputs_cf_om_fuel_cost_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_CustomGeneration_Outputs_cf_om_land_lease_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_CustomGeneration_Outputs_cf_om_production_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_CustomGeneration_Outputs_gen_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_CustomGeneration_Outputs_kwh_per_kw_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_CustomGeneration_Outputs_monthly_energy_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_CustomGeneration_Outputs_system_heat_rate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_CustomGeneration_Outputs_water_usage_nget(SAM_table ptr, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif