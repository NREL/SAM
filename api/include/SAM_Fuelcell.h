#ifndef SAM_FUELCELL_H_
#define SAM_FUELCELL_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// Fuelcell Technology Model
	//

	/** 
	 * Create a Fuelcell variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_Fuelcell;

	SAM_EXPORT SAM_Fuelcell SAM_Fuelcell_construct(const char* def, SAM_error* err);

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_Fuelcell_execute(SAM_Fuelcell data, int verbosity, SAM_error* err);

	SAM_EXPORT void SAM_Fuelcell_destruct(SAM_Fuelcell system);


	//
	// Common parameters
	//

	/**
	 * Set annual_energy: Annual Energy [kWh]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Fuelcell_Common_annual_energy_fset(SAM_Fuelcell ptr, float number, SAM_error *err);

	/**
	 * Set capacity_factor: Capacity factor [%]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Fuelcell_Common_capacity_factor_fset(SAM_Fuelcell ptr, float number, SAM_error *err);

	/**
	 * Set gen: System power generated [kW]
	 * options: Lifetime system generation
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Fuelcell_Common_gen_aset(SAM_Fuelcell ptr, float* arr, int length, SAM_error *err);

	/**
	 * Set load: Electricity load (year 1) [kW]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Fuelcell_Common_load_aset(SAM_Fuelcell ptr, float* arr, int length, SAM_error *err);

	/**
	 * Set percent_complete: Estimated simulation status [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Fuelcell_Common_percent_complete_fset(SAM_Fuelcell ptr, float number, SAM_error *err);

	/**
	 * Set system_use_lifetime_output: Lifetime simulation [0/1]
	 * options: 0=SingleYearRepeated,1=RunEveryYear
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Fuelcell_Common_system_use_lifetime_output_fset(SAM_Fuelcell ptr, float number, SAM_error *err);


	//
	// FinancialAnalysisParameters parameters
	//

	/**
	 * Set analysis_period: Lifetime analysis period [years]
	 * options: The number of years in the simulation
	 * constraints: None
	 * required if: system_use_lifetime_output=1
	 */
	SAM_EXPORT void SAM_Fuelcell_FinancialAnalysisParameters_analysis_period_fset(SAM_Fuelcell ptr, float number, SAM_error *err);


	//
	// FuelCell parameters
	//

	/**
	 * Set dispatch_manual_fuelcellcharge: Periods 1-6 charging allowed? []
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Fuelcell_FuelCell_dispatch_manual_fuelcellcharge_aset(SAM_Fuelcell ptr, float* arr, int length, SAM_error *err);

	/**
	 * Set dispatch_manual_fuelcelldischarge: Periods 1-6 discharging allowed? []
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Fuelcell_FuelCell_dispatch_manual_fuelcelldischarge_aset(SAM_Fuelcell ptr, float* arr, int length, SAM_error *err);

	/**
	 * Set dispatch_manual_percent_fc_discharge: Periods 1-6 percent of max fuelcell output []
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Fuelcell_FuelCell_dispatch_manual_percent_fc_discharge_aset(SAM_Fuelcell ptr, float* arr, int length, SAM_error *err);

	/**
	 * Set dispatch_manual_sched: Dispatch schedule for weekday []
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Fuelcell_FuelCell_dispatch_manual_sched_mset(SAM_Fuelcell ptr, float* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set dispatch_manual_sched_weekend: Dispatch schedule for weekend []
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Fuelcell_FuelCell_dispatch_manual_sched_weekend_mset(SAM_Fuelcell ptr, float* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set dispatch_manual_units_fc_discharge: Periods 1-6 number of fuel cell units? []
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Fuelcell_FuelCell_dispatch_manual_units_fc_discharge_aset(SAM_Fuelcell ptr, float* arr, int length, SAM_error *err);

	/**
	 * Set fuelcell_availability_schedule: Fuel cell availability schedule  [Column 1: Hour of year start shutdown/Column 2: Hours duration of shutdown ]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Fuelcell_FuelCell_fuelcell_availability_schedule_mset(SAM_Fuelcell ptr, float* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set fuelcell_degradation: Fuel cell degradation per hour [kW/h]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Fuelcell_FuelCell_fuelcell_degradation_fset(SAM_Fuelcell ptr, float number, SAM_error *err);

	/**
	 * Set fuelcell_degradation_restart: Fuel cell degradation at restart [kW]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Fuelcell_FuelCell_fuelcell_degradation_restart_fset(SAM_Fuelcell ptr, float number, SAM_error *err);

	/**
	 * Set fuelcell_degradation_restart_schedule: Fuel cell enable scheduled restarts [0/1]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Fuelcell_FuelCell_fuelcell_degradation_restart_schedule_fset(SAM_Fuelcell ptr, float number, SAM_error *err);

	/**
	 * Set fuelcell_degradation_restarts_per_year: Fuel cell scheduled restarts per year []
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Fuelcell_FuelCell_fuelcell_degradation_restarts_per_year_fset(SAM_Fuelcell ptr, float number, SAM_error *err);

	/**
	 * Set fuelcell_dispatch: Fuel cell dispatch input per unit [kW]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Fuelcell_FuelCell_fuelcell_dispatch_aset(SAM_Fuelcell ptr, float* arr, int length, SAM_error *err);

	/**
	 * Set fuelcell_dispatch_choice: Fuel cell dispatch choice [0/1/2]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Fuelcell_FuelCell_fuelcell_dispatch_choice_fset(SAM_Fuelcell ptr, float number, SAM_error *err);

	/**
	 * Set fuelcell_dynamic_response_down: Fuel cell ramp rate limit down [kW/h]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Fuelcell_FuelCell_fuelcell_dynamic_response_down_fset(SAM_Fuelcell ptr, float number, SAM_error *err);

	/**
	 * Set fuelcell_dynamic_response_up: Fuel cell ramp rate limit up [kW/h]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Fuelcell_FuelCell_fuelcell_dynamic_response_up_fset(SAM_Fuelcell ptr, float number, SAM_error *err);

	/**
	 * Set fuelcell_efficiency: Fuel cell efficiency table  []
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Fuelcell_FuelCell_fuelcell_efficiency_mset(SAM_Fuelcell ptr, float* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set fuelcell_efficiency_choice: Fuel cell efficiency definition choice  [0/1]
	 * options: 0=OriginalNameplate,1=DegradedNameplate
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Fuelcell_FuelCell_fuelcell_efficiency_choice_fset(SAM_Fuelcell ptr, float number, SAM_error *err);

	/**
	 * Set fuelcell_fixed_pct: Fuel cell fixed operation percent [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Fuelcell_FuelCell_fuelcell_fixed_pct_fset(SAM_Fuelcell ptr, float number, SAM_error *err);

	/**
	 * Set fuelcell_fuel_available: Fuel cell available fuel quantity [MCf]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Fuelcell_FuelCell_fuelcell_fuel_available_fset(SAM_Fuelcell ptr, float number, SAM_error *err);

	/**
	 * Set fuelcell_fuel_price: Fuel cell price [$/MCf]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Fuelcell_FuelCell_fuelcell_fuel_price_fset(SAM_Fuelcell ptr, float number, SAM_error *err);

	/**
	 * Set fuelcell_fuel_type: Fuel cell type [0/1]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Fuelcell_FuelCell_fuelcell_fuel_type_fset(SAM_Fuelcell ptr, float number, SAM_error *err);

	/**
	 * Set fuelcell_lhv: Fuel cell lower heating value [Btu/ft3]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Fuelcell_FuelCell_fuelcell_lhv_fset(SAM_Fuelcell ptr, float number, SAM_error *err);

	/**
	 * Set fuelcell_number_of_units: Fuel cell number of units []
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Fuelcell_FuelCell_fuelcell_number_of_units_fset(SAM_Fuelcell ptr, float number, SAM_error *err);

	/**
	 * Set fuelcell_operation_options: Fuel cell turn off options [0/1]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Fuelcell_FuelCell_fuelcell_operation_options_fset(SAM_Fuelcell ptr, float number, SAM_error *err);

	/**
	 * Set fuelcell_replacement_option: Fuel cell replacement option [0/1/2]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Fuelcell_FuelCell_fuelcell_replacement_option_fset(SAM_Fuelcell ptr, float number, SAM_error *err);

	/**
	 * Set fuelcell_replacement_percent: Fuel cell replace at percentage []
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Fuelcell_FuelCell_fuelcell_replacement_percent_fset(SAM_Fuelcell ptr, float number, SAM_error *err);

	/**
	 * Set fuelcell_replacement_schedule: Fuel cell replace on schedule []
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Fuelcell_FuelCell_fuelcell_replacement_schedule_aset(SAM_Fuelcell ptr, float* arr, int length, SAM_error *err);

	/**
	 * Set fuelcell_shutdown_time: Fuel cell shutdown hours [hours]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Fuelcell_FuelCell_fuelcell_shutdown_time_fset(SAM_Fuelcell ptr, float number, SAM_error *err);

	/**
	 * Set fuelcell_startup_time: Fuel cell startup hours [hours]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Fuelcell_FuelCell_fuelcell_startup_time_fset(SAM_Fuelcell ptr, float number, SAM_error *err);

	/**
	 * Set fuelcell_type: Fuel cell type [0/1/2]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Fuelcell_FuelCell_fuelcell_type_fset(SAM_Fuelcell ptr, float number, SAM_error *err);

	/**
	 * Set fuelcell_unit_max_power: Fuel cell max power per unit [kW]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Fuelcell_FuelCell_fuelcell_unit_max_power_fset(SAM_Fuelcell ptr, float number, SAM_error *err);

	/**
	 * Set fuelcell_unit_min_power: Fuel cell min power per unit [kW]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Fuelcell_FuelCell_fuelcell_unit_min_power_fset(SAM_Fuelcell ptr, float number, SAM_error *err);


	/**
	 * Common Getters
	 */

	SAM_EXPORT float SAM_Fuelcell_Common_annual_energy_fget(SAM_Fuelcell ptr, SAM_error *err);

	SAM_EXPORT float SAM_Fuelcell_Common_capacity_factor_fget(SAM_Fuelcell ptr, SAM_error *err);

	SAM_EXPORT float* SAM_Fuelcell_Common_gen_aget(SAM_Fuelcell ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_Fuelcell_Common_load_aget(SAM_Fuelcell ptr, int* length, SAM_error *err);

	SAM_EXPORT float SAM_Fuelcell_Common_percent_complete_fget(SAM_Fuelcell ptr, SAM_error *err);

	SAM_EXPORT float SAM_Fuelcell_Common_system_use_lifetime_output_fget(SAM_Fuelcell ptr, SAM_error *err);


	/**
	 * FinancialAnalysisParameters Getters
	 */

	SAM_EXPORT float SAM_Fuelcell_FinancialAnalysisParameters_analysis_period_fget(SAM_Fuelcell ptr, SAM_error *err);


	/**
	 * FuelCell Getters
	 */

	SAM_EXPORT float* SAM_Fuelcell_FuelCell_dispatch_manual_fuelcellcharge_aget(SAM_Fuelcell ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_Fuelcell_FuelCell_dispatch_manual_fuelcelldischarge_aget(SAM_Fuelcell ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_Fuelcell_FuelCell_dispatch_manual_percent_fc_discharge_aget(SAM_Fuelcell ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_Fuelcell_FuelCell_dispatch_manual_sched_mget(SAM_Fuelcell ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT float* SAM_Fuelcell_FuelCell_dispatch_manual_sched_weekend_mget(SAM_Fuelcell ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT float* SAM_Fuelcell_FuelCell_dispatch_manual_units_fc_discharge_aget(SAM_Fuelcell ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_Fuelcell_FuelCell_fuelcell_availability_schedule_mget(SAM_Fuelcell ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT float SAM_Fuelcell_FuelCell_fuelcell_degradation_fget(SAM_Fuelcell ptr, SAM_error *err);

	SAM_EXPORT float SAM_Fuelcell_FuelCell_fuelcell_degradation_restart_fget(SAM_Fuelcell ptr, SAM_error *err);

	SAM_EXPORT float SAM_Fuelcell_FuelCell_fuelcell_degradation_restart_schedule_fget(SAM_Fuelcell ptr, SAM_error *err);

	SAM_EXPORT float SAM_Fuelcell_FuelCell_fuelcell_degradation_restarts_per_year_fget(SAM_Fuelcell ptr, SAM_error *err);

	SAM_EXPORT float* SAM_Fuelcell_FuelCell_fuelcell_dispatch_aget(SAM_Fuelcell ptr, int* length, SAM_error *err);

	SAM_EXPORT float SAM_Fuelcell_FuelCell_fuelcell_dispatch_choice_fget(SAM_Fuelcell ptr, SAM_error *err);

	SAM_EXPORT float SAM_Fuelcell_FuelCell_fuelcell_dynamic_response_down_fget(SAM_Fuelcell ptr, SAM_error *err);

	SAM_EXPORT float SAM_Fuelcell_FuelCell_fuelcell_dynamic_response_up_fget(SAM_Fuelcell ptr, SAM_error *err);

	SAM_EXPORT float* SAM_Fuelcell_FuelCell_fuelcell_efficiency_mget(SAM_Fuelcell ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT float SAM_Fuelcell_FuelCell_fuelcell_efficiency_choice_fget(SAM_Fuelcell ptr, SAM_error *err);

	SAM_EXPORT float SAM_Fuelcell_FuelCell_fuelcell_fixed_pct_fget(SAM_Fuelcell ptr, SAM_error *err);

	SAM_EXPORT float SAM_Fuelcell_FuelCell_fuelcell_fuel_available_fget(SAM_Fuelcell ptr, SAM_error *err);

	SAM_EXPORT float SAM_Fuelcell_FuelCell_fuelcell_fuel_price_fget(SAM_Fuelcell ptr, SAM_error *err);

	SAM_EXPORT float SAM_Fuelcell_FuelCell_fuelcell_fuel_type_fget(SAM_Fuelcell ptr, SAM_error *err);

	SAM_EXPORT float SAM_Fuelcell_FuelCell_fuelcell_lhv_fget(SAM_Fuelcell ptr, SAM_error *err);

	SAM_EXPORT float SAM_Fuelcell_FuelCell_fuelcell_number_of_units_fget(SAM_Fuelcell ptr, SAM_error *err);

	SAM_EXPORT float SAM_Fuelcell_FuelCell_fuelcell_operation_options_fget(SAM_Fuelcell ptr, SAM_error *err);

	SAM_EXPORT float SAM_Fuelcell_FuelCell_fuelcell_replacement_option_fget(SAM_Fuelcell ptr, SAM_error *err);

	SAM_EXPORT float SAM_Fuelcell_FuelCell_fuelcell_replacement_percent_fget(SAM_Fuelcell ptr, SAM_error *err);

	SAM_EXPORT float* SAM_Fuelcell_FuelCell_fuelcell_replacement_schedule_aget(SAM_Fuelcell ptr, int* length, SAM_error *err);

	SAM_EXPORT float SAM_Fuelcell_FuelCell_fuelcell_shutdown_time_fget(SAM_Fuelcell ptr, SAM_error *err);

	SAM_EXPORT float SAM_Fuelcell_FuelCell_fuelcell_startup_time_fget(SAM_Fuelcell ptr, SAM_error *err);

	SAM_EXPORT float SAM_Fuelcell_FuelCell_fuelcell_type_fget(SAM_Fuelcell ptr, SAM_error *err);

	SAM_EXPORT float SAM_Fuelcell_FuelCell_fuelcell_unit_max_power_fget(SAM_Fuelcell ptr, SAM_error *err);

	SAM_EXPORT float SAM_Fuelcell_FuelCell_fuelcell_unit_min_power_fget(SAM_Fuelcell ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT float SAM_Fuelcell_Outputs_annual_fuel_usage_fget(SAM_Fuelcell ptr, SAM_error *err);

	SAM_EXPORT float* SAM_Fuelcell_Outputs_fuelcell_fuel_consumption_mcf_aget(SAM_Fuelcell ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_Fuelcell_Outputs_fuelcell_percent_load_aget(SAM_Fuelcell ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_Fuelcell_Outputs_fuelcell_power_aget(SAM_Fuelcell ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_Fuelcell_Outputs_fuelcell_power_max_percent_aget(SAM_Fuelcell ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_Fuelcell_Outputs_fuelcell_power_thermal_aget(SAM_Fuelcell ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_Fuelcell_Outputs_fuelcell_replacement_aget(SAM_Fuelcell ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_Fuelcell_Outputs_fuelcell_to_grid_aget(SAM_Fuelcell ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_Fuelcell_Outputs_fuelcell_to_load_aget(SAM_Fuelcell ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_Fuelcell_Outputs_gen_aget(SAM_Fuelcell ptr, int* length, SAM_error *err);

	SAM_EXPORT float SAM_Fuelcell_Outputs_system_heat_rate_fget(SAM_Fuelcell ptr, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif