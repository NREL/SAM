#ifndef SAM_THERMALRATEIPH_H_
#define SAM_THERMALRATEIPH_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// ThermalrateIph Technology Model
	//

	/** 
	 * Create a ThermalrateIph variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_ThermalrateIph;

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_ThermalrateIph_execute(SAM_table data, int verbosity, SAM_error* err);


	//
	// ThermalRate parameters
	//

	/**
	 * Set en_thermal_rates: Optionally enable/disable thermal_rate [years]
	 * options: None
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: None
	 */
	SAM_EXPORT void SAM_ThermalrateIph_ThermalRate_en_thermal_rates_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set gen_heat: Thermal power generated [kWt]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_ThermalrateIph_ThermalRate_gen_heat_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set thermal_buy_rate_flat_heat_btu: Thermal buy rate flat [$/MMBtu]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_ThermalrateIph_ThermalRate_thermal_buy_rate_flat_heat_btu_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set thermal_buy_rate_option: Thermal buy rate option [0-2]
	 * options: 0=flat,1=timestep,2=monthly
	 * constraints: INTEGER,MIN=0,MAX=2
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_ThermalrateIph_ThermalRate_thermal_buy_rate_option_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set thermal_conversion_efficiency: Heat conversion efficiency (buy) [%]
	 * options: None
	 * constraints: None
	 * required if: ?=100
	 */
	SAM_EXPORT void SAM_ThermalrateIph_ThermalRate_thermal_conversion_efficiency_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set thermal_degradation: Annual energy degradation [%]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_ThermalrateIph_ThermalRate_thermal_degradation_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set thermal_load_escalation: Annual load escalation [%/year]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_ThermalrateIph_ThermalRate_thermal_load_escalation_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set thermal_load_heat_btu: Thermal load (year 1) [MMBtu/hr]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_ThermalrateIph_ThermalRate_thermal_load_heat_btu_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set thermal_monthly_buy_rate_heat_btu: Monthly thermal buy rate [$/MMBtu]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_ThermalrateIph_ThermalRate_thermal_monthly_buy_rate_heat_btu_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set thermal_monthly_sell_rate_heat_btu: Thermal sell rate monthly [$/MMBtu]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_ThermalrateIph_ThermalRate_thermal_monthly_sell_rate_heat_btu_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set thermal_rate_escalation: Annual thermal rate escalation [%/year]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_ThermalrateIph_ThermalRate_thermal_rate_escalation_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set thermal_sell_rate_flat_heat_btu: Thermal sell rate flat [$/MMBtu]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_ThermalrateIph_ThermalRate_thermal_sell_rate_flat_heat_btu_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set thermal_sell_rate_option: Thermal sell rate option [0-2]
	 * options: 0=flat,1=timestep,2=monthly
	 * constraints: INTEGER,MIN=0,MAX=2
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_ThermalrateIph_ThermalRate_thermal_sell_rate_option_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set thermal_timestep_buy_rate_heat_btu: Thermal buy rate [$/MMBtu]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_ThermalrateIph_ThermalRate_thermal_timestep_buy_rate_heat_btu_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set thermal_timestep_sell_rate_heat_btu: Thermal sell rate timestep [$/MMBtu]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_ThermalrateIph_ThermalRate_thermal_timestep_sell_rate_heat_btu_aset(SAM_table ptr, double* arr, int length, SAM_error *err);


	//
	// Lifetime parameters
	//

	/**
	 * Set analysis_period: Number of years in analysis [years]
	 * options: None
	 * constraints: INTEGER,POSITIVE
	 * required if: *
	 */
	SAM_EXPORT void SAM_ThermalrateIph_Lifetime_analysis_period_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set inflation_rate: Inflation rate [%]
	 * options: None
	 * constraints: MIN=-99
	 * required if: *
	 */
	SAM_EXPORT void SAM_ThermalrateIph_Lifetime_inflation_rate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set system_use_lifetime_output: Lifetime hourly system outputs [0/1]
	 * options: 0=hourly first year,1=hourly lifetime
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: *
	 */
	SAM_EXPORT void SAM_ThermalrateIph_Lifetime_system_use_lifetime_output_nset(SAM_table ptr, double number, SAM_error *err);


	/**
	 * ThermalRate Getters
	 */

	SAM_EXPORT double SAM_ThermalrateIph_ThermalRate_en_thermal_rates_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_ThermalrateIph_ThermalRate_gen_heat_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_ThermalrateIph_ThermalRate_thermal_buy_rate_flat_heat_btu_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ThermalrateIph_ThermalRate_thermal_buy_rate_option_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ThermalrateIph_ThermalRate_thermal_conversion_efficiency_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_ThermalrateIph_ThermalRate_thermal_degradation_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_ThermalrateIph_ThermalRate_thermal_load_escalation_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_ThermalrateIph_ThermalRate_thermal_load_heat_btu_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_ThermalrateIph_ThermalRate_thermal_monthly_buy_rate_heat_btu_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_ThermalrateIph_ThermalRate_thermal_monthly_sell_rate_heat_btu_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_ThermalrateIph_ThermalRate_thermal_rate_escalation_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_ThermalrateIph_ThermalRate_thermal_sell_rate_flat_heat_btu_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ThermalrateIph_ThermalRate_thermal_sell_rate_option_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_ThermalrateIph_ThermalRate_thermal_timestep_buy_rate_heat_btu_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_ThermalrateIph_ThermalRate_thermal_timestep_sell_rate_heat_btu_aget(SAM_table ptr, int* length, SAM_error *err);


	/**
	 * Lifetime Getters
	 */

	SAM_EXPORT double SAM_ThermalrateIph_Lifetime_analysis_period_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ThermalrateIph_Lifetime_inflation_rate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ThermalrateIph_Lifetime_system_use_lifetime_output_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double* SAM_ThermalrateIph_Outputs_annual_thermal_value_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_ThermalrateIph_Outputs_thermal_cost_with_system_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_ThermalrateIph_Outputs_thermal_cost_with_system_year1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_ThermalrateIph_Outputs_thermal_cost_without_system_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_ThermalrateIph_Outputs_thermal_cost_without_system_year1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ThermalrateIph_Outputs_thermal_load_year1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_ThermalrateIph_Outputs_thermal_revenue_with_system_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_ThermalrateIph_Outputs_thermal_revenue_without_system_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_ThermalrateIph_Outputs_thermal_savings_year1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_ThermalrateIph_Outputs_year1_monthly_load_heat_aget(SAM_table ptr, int* length, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif