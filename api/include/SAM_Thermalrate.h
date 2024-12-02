#ifndef SAM_THERMALRATE_H_
#define SAM_THERMALRATE_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// Thermalrate Technology Model
	//

	/** 
	 * Create a Thermalrate variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_Thermalrate;

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_Thermalrate_execute(SAM_table data, int verbosity, SAM_error* err);


	//
	// ThermalRate parameters
	//

	/**
	 * Set en_thermal_rates: Optionally enable/disable thermal_rate [years]
	 * options: None
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: None
	 */
	SAM_EXPORT void SAM_Thermalrate_ThermalRate_en_thermal_rates_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set fuelcell_power_thermal: Fuel cell power generated [kW-t]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Thermalrate_ThermalRate_fuelcell_power_thermal_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set thermal_buy_rate: Thermal buy rate [$/kW-t]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Thermalrate_ThermalRate_thermal_buy_rate_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set thermal_buy_rate_flat: Thermal buy rate flat [$/kW-t]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Thermalrate_ThermalRate_thermal_buy_rate_flat_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set thermal_buy_rate_option: Thermal buy rate option [0/1]
	 * options: 0=flat,1=timestep
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Thermalrate_ThermalRate_thermal_buy_rate_option_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set thermal_degradation: Annual energy degradation [%]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Thermalrate_ThermalRate_thermal_degradation_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set thermal_load: Thermal load (year 1) [kW-t]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Thermalrate_ThermalRate_thermal_load_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set thermal_load_escalation: Annual load escalation [%/year]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Thermalrate_ThermalRate_thermal_load_escalation_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set thermal_rate_escalation: Annual thermal rate escalation [%/year]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Thermalrate_ThermalRate_thermal_rate_escalation_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set thermal_sell_rate: Thermal sell rate [$/kW-t]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Thermalrate_ThermalRate_thermal_sell_rate_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set thermal_sell_rate_flat: Thermal sell rate flat [$/kW-t]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Thermalrate_ThermalRate_thermal_sell_rate_flat_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set thermal_sell_rate_option: Thermal sell rate option [0/1]
	 * options: 0=flat,1=timestep
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Thermalrate_ThermalRate_thermal_sell_rate_option_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// Lifetime parameters
	//

	/**
	 * Set analysis_period: Number of years in analysis [years]
	 * options: None
	 * constraints: INTEGER,POSITIVE
	 * required if: *
	 */
	SAM_EXPORT void SAM_Thermalrate_Lifetime_analysis_period_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set inflation_rate: Inflation rate [%]
	 * options: None
	 * constraints: MIN=-99
	 * required if: *
	 */
	SAM_EXPORT void SAM_Thermalrate_Lifetime_inflation_rate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set system_use_lifetime_output: Lifetime hourly system outputs [0/1]
	 * options: 0=hourly first year,1=hourly lifetime
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: *
	 */
	SAM_EXPORT void SAM_Thermalrate_Lifetime_system_use_lifetime_output_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// HybridCosts parameters
	//

	/**
	 * Set degradation: Annual AC degradation [%]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Thermalrate_HybridCosts_degradation_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set land_area: Total land area [acres]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Thermalrate_HybridCosts_land_area_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set om_capacity: Capacity-based O&M amount [$/kWcap]
	 * options: !battery,!fuelcell
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Thermalrate_HybridCosts_om_capacity_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set om_capacity_escal: Capacity-based O&M escalation [%/year]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Thermalrate_HybridCosts_om_capacity_escal_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set om_fixed: Fixed O&M annual amount [$/year]
	 * options: !battery,!fuelcell
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Thermalrate_HybridCosts_om_fixed_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set om_fixed_escal: Fixed O&M escalation [%/year]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Thermalrate_HybridCosts_om_fixed_escal_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set om_land_lease: Land lease cost [$/acre]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Thermalrate_HybridCosts_om_land_lease_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set om_land_lease_escal: Land lease cost escalation [%/yr]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Thermalrate_HybridCosts_om_land_lease_escal_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set om_production: Production-based O&M amount [$/MWh]
	 * options: !battery,!fuelcell
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Thermalrate_HybridCosts_om_production_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set om_production_escal: Production-based O&M escalation [%/year]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Thermalrate_HybridCosts_om_production_escal_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set total_installed_cost: Total installed cost [$]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Thermalrate_HybridCosts_total_installed_cost_nset(SAM_table ptr, double number, SAM_error *err);


	/**
	 * ThermalRate Getters
	 */

	SAM_EXPORT double SAM_Thermalrate_ThermalRate_en_thermal_rates_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Thermalrate_ThermalRate_fuelcell_power_thermal_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Thermalrate_ThermalRate_thermal_buy_rate_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Thermalrate_ThermalRate_thermal_buy_rate_flat_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Thermalrate_ThermalRate_thermal_buy_rate_option_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Thermalrate_ThermalRate_thermal_degradation_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Thermalrate_ThermalRate_thermal_load_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Thermalrate_ThermalRate_thermal_load_escalation_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Thermalrate_ThermalRate_thermal_rate_escalation_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Thermalrate_ThermalRate_thermal_sell_rate_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Thermalrate_ThermalRate_thermal_sell_rate_flat_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Thermalrate_ThermalRate_thermal_sell_rate_option_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Lifetime Getters
	 */

	SAM_EXPORT double SAM_Thermalrate_Lifetime_analysis_period_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Thermalrate_Lifetime_inflation_rate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Thermalrate_Lifetime_system_use_lifetime_output_nget(SAM_table ptr, SAM_error *err);


	/**
	 * HybridCosts Getters
	 */

	SAM_EXPORT double* SAM_Thermalrate_HybridCosts_degradation_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Thermalrate_HybridCosts_land_area_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Thermalrate_HybridCosts_om_capacity_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Thermalrate_HybridCosts_om_capacity_escal_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Thermalrate_HybridCosts_om_fixed_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Thermalrate_HybridCosts_om_fixed_escal_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Thermalrate_HybridCosts_om_land_lease_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Thermalrate_HybridCosts_om_land_lease_escal_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Thermalrate_HybridCosts_om_production_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Thermalrate_HybridCosts_om_production_escal_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Thermalrate_HybridCosts_total_installed_cost_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double* SAM_Thermalrate_Outputs_annual_thermal_value_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Thermalrate_Outputs_cf_land_lease_expense_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Thermalrate_Outputs_thermal_cost_with_system_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Thermalrate_Outputs_thermal_cost_with_system_year1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Thermalrate_Outputs_thermal_cost_without_system_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Thermalrate_Outputs_thermal_cost_without_system_year1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Thermalrate_Outputs_thermal_load_year1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Thermalrate_Outputs_thermal_revenue_with_system_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Thermalrate_Outputs_thermal_revenue_without_system_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Thermalrate_Outputs_thermal_savings_year1_nget(SAM_table ptr, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif