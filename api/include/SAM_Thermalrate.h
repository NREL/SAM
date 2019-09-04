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

	SAM_EXPORT SAM_Thermalrate SAM_Thermalrate_construct(const char* def, SAM_error* err);

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_Thermalrate_execute(SAM_Thermalrate data, int verbosity, SAM_error* err);

	SAM_EXPORT void SAM_Thermalrate_destruct(SAM_Thermalrate system);


	//
	// ThermalRate parameters
	//

	/**
	 * Set en_thermal_rates: Optionally enable/disable thermal_rate [years]
	 * options: None
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: None
	 */
	SAM_EXPORT void SAM_Thermalrate_ThermalRate_en_thermal_rates_nset(SAM_Thermalrate ptr, double number, SAM_error *err);

	/**
	 * Set fuelcell_power_thermal: Fuel cell power generated [kW-t]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Thermalrate_ThermalRate_fuelcell_power_thermal_aset(SAM_Thermalrate ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set thermal_buy_rate: Thermal buy rate [$/kW-t]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Thermalrate_ThermalRate_thermal_buy_rate_aset(SAM_Thermalrate ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set thermal_buy_rate_flat: Thermal buy rate flat [$/kW-t]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Thermalrate_ThermalRate_thermal_buy_rate_flat_nset(SAM_Thermalrate ptr, double number, SAM_error *err);

	/**
	 * Set thermal_buy_rate_option: Thermal buy rate option [0/1]
	 * options: 0=flat,1=timestep
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Thermalrate_ThermalRate_thermal_buy_rate_option_nset(SAM_Thermalrate ptr, double number, SAM_error *err);

	/**
	 * Set thermal_degradation: Annual energy degradation [%]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Thermalrate_ThermalRate_thermal_degradation_aset(SAM_Thermalrate ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set thermal_load: Thermal load (year 1) [kW-t]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Thermalrate_ThermalRate_thermal_load_aset(SAM_Thermalrate ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set thermal_load_escalation: Annual load escalation [%/year]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Thermalrate_ThermalRate_thermal_load_escalation_aset(SAM_Thermalrate ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set thermal_rate_escalation: Annual thermal rate escalation [%/year]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Thermalrate_ThermalRate_thermal_rate_escalation_aset(SAM_Thermalrate ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set thermal_sell_rate: Thermal sell rate [$/kW-t]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Thermalrate_ThermalRate_thermal_sell_rate_aset(SAM_Thermalrate ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set thermal_sell_rate_flat: Thermal sell rate flat [$/kW-t]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Thermalrate_ThermalRate_thermal_sell_rate_flat_nset(SAM_Thermalrate ptr, double number, SAM_error *err);

	/**
	 * Set thermal_sell_rate_option: Thermal sell rate option [0/1]
	 * options: 0=flat,1=timestep
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Thermalrate_ThermalRate_thermal_sell_rate_option_nset(SAM_Thermalrate ptr, double number, SAM_error *err);


	//
	// Lifetime parameters
	//

	/**
	 * Set analysis_period: Number of years in analysis [years]
	 * options: None
	 * constraints: INTEGER,POSITIVE
	 * required if: *
	 */
	SAM_EXPORT void SAM_Thermalrate_Lifetime_analysis_period_nset(SAM_Thermalrate ptr, double number, SAM_error *err);

	/**
	 * Set inflation_rate: Inflation rate [%]
	 * options: None
	 * constraints: MIN=-99
	 * required if: *
	 */
	SAM_EXPORT void SAM_Thermalrate_Lifetime_inflation_rate_nset(SAM_Thermalrate ptr, double number, SAM_error *err);

	/**
	 * Set system_use_lifetime_output: Lifetime hourly system outputs [0/1]
	 * options: 0=hourly first year,1=hourly lifetime
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: *
	 */
	SAM_EXPORT void SAM_Thermalrate_Lifetime_system_use_lifetime_output_nset(SAM_Thermalrate ptr, double number, SAM_error *err);


	/**
	 * ThermalRate Getters
	 */

	SAM_EXPORT double SAM_Thermalrate_ThermalRate_en_thermal_rates_nget(SAM_Thermalrate ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Thermalrate_ThermalRate_fuelcell_power_thermal_aget(SAM_Thermalrate ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Thermalrate_ThermalRate_thermal_buy_rate_aget(SAM_Thermalrate ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Thermalrate_ThermalRate_thermal_buy_rate_flat_nget(SAM_Thermalrate ptr, SAM_error *err);

	SAM_EXPORT double SAM_Thermalrate_ThermalRate_thermal_buy_rate_option_nget(SAM_Thermalrate ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Thermalrate_ThermalRate_thermal_degradation_aget(SAM_Thermalrate ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Thermalrate_ThermalRate_thermal_load_aget(SAM_Thermalrate ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Thermalrate_ThermalRate_thermal_load_escalation_aget(SAM_Thermalrate ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Thermalrate_ThermalRate_thermal_rate_escalation_aget(SAM_Thermalrate ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Thermalrate_ThermalRate_thermal_sell_rate_aget(SAM_Thermalrate ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Thermalrate_ThermalRate_thermal_sell_rate_flat_nget(SAM_Thermalrate ptr, SAM_error *err);

	SAM_EXPORT double SAM_Thermalrate_ThermalRate_thermal_sell_rate_option_nget(SAM_Thermalrate ptr, SAM_error *err);


	/**
	 * Lifetime Getters
	 */

	SAM_EXPORT double SAM_Thermalrate_Lifetime_analysis_period_nget(SAM_Thermalrate ptr, SAM_error *err);

	SAM_EXPORT double SAM_Thermalrate_Lifetime_inflation_rate_nget(SAM_Thermalrate ptr, SAM_error *err);

	SAM_EXPORT double SAM_Thermalrate_Lifetime_system_use_lifetime_output_nget(SAM_Thermalrate ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double SAM_Thermalrate_Outputs_thermal_cost_with_system_year1_nget(SAM_Thermalrate ptr, SAM_error *err);

	SAM_EXPORT double SAM_Thermalrate_Outputs_thermal_cost_without_system_year1_nget(SAM_Thermalrate ptr, SAM_error *err);

	SAM_EXPORT double SAM_Thermalrate_Outputs_thermal_load_year1_nget(SAM_Thermalrate ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Thermalrate_Outputs_thermal_revenue_with_system_aget(SAM_Thermalrate ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Thermalrate_Outputs_thermal_revenue_without_system_aget(SAM_Thermalrate ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Thermalrate_Outputs_thermal_savings_year1_nget(SAM_Thermalrate ptr, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif