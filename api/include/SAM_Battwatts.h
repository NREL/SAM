#ifndef SAM_BATTWATTS_H_
#define SAM_BATTWATTS_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// Battwatts Technology Model
	//

	/** 
	 * Create a Battwatts variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_Battwatts;

	SAM_EXPORT SAM_Battwatts SAM_Battwatts_construct(const char* def, SAM_error* err);

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_Battwatts_execute(SAM_Battwatts data, int verbosity, SAM_error* err);

	SAM_EXPORT void SAM_Battwatts_destruct(SAM_Battwatts system);


	//
	// PVWatts parameters
	//

	/**
	 * Set system_use_lifetime_output: PV lifetime simulation [0/1]
	 * options: 0=SingleYearRepeated,1=RunEveryYear
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Battwatts_PVWatts_system_use_lifetime_output_nset(SAM_Battwatts ptr, double number, SAM_error *err);


	//
	// FinancialAnalysisParameters parameters
	//

	/**
	 * Set analysis_period: Lifetime analysis period [years]
	 * options: The number of years in the simulation
	 * constraints: None
	 * required if: system_use_lifetime_output=1
	 */
	SAM_EXPORT void SAM_Battwatts_FinancialAnalysisParameters_analysis_period_nset(SAM_Battwatts ptr, double number, SAM_error *err);


	//
	// BatteryModelSimple parameters
	//

	/**
	 * Set batt_simple_chemistry: Battery Chemistry [0=lead acid/1=Li-ion/2]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Battwatts_BatteryModelSimple_batt_simple_chemistry_nset(SAM_Battwatts ptr, double number, SAM_error *err);

	/**
	 * Set batt_simple_dispatch: Battery Dispatch [0=peak shaving look ahead/1=peak shaving look behind]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Battwatts_BatteryModelSimple_batt_simple_dispatch_nset(SAM_Battwatts ptr, double number, SAM_error *err);

	/**
	 * Set batt_simple_enable: Enable Battery [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Battwatts_BatteryModelSimple_batt_simple_enable_nset(SAM_Battwatts ptr, double number, SAM_error *err);

	/**
	 * Set batt_simple_kw: Battery Power [kW]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Battwatts_BatteryModelSimple_batt_simple_kw_nset(SAM_Battwatts ptr, double number, SAM_error *err);

	/**
	 * Set batt_simple_kwh: Battery Capacity [kWh]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Battwatts_BatteryModelSimple_batt_simple_kwh_nset(SAM_Battwatts ptr, double number, SAM_error *err);

	/**
	 * Set batt_simple_meter_position: Battery Meter Position [0=behind meter/1=front of meter]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Battwatts_BatteryModelSimple_batt_simple_meter_position_nset(SAM_Battwatts ptr, double number, SAM_error *err);


	//
	// Common parameters
	//

	/**
	 * Set ac: AC inverter power [W]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battwatts_Common_ac_aset(SAM_Battwatts ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set dc: DC array power [W]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battwatts_Common_dc_aset(SAM_Battwatts ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set inverter_efficiency: Inverter Efficiency [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battwatts_Common_inverter_efficiency_nset(SAM_Battwatts ptr, double number, SAM_error *err);

	/**
	 * Set inverter_model: Inverter model specifier
	 * options: 0=cec,1=datasheet,2=partload,3=coefficientgenerator,4=generic
	 * constraints: INTEGER,MIN=0,MAX=4
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battwatts_Common_inverter_model_nset(SAM_Battwatts ptr, double number, SAM_error *err);


	//
	// ElectricLoadOther parameters
	//

	/**
	 * Set load: Electricity load (year 1) [kW]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battwatts_ElectricLoadOther_load_aset(SAM_Battwatts ptr, double* arr, int length, SAM_error *err);


	/**
	 * PVWatts Getters
	 */

	SAM_EXPORT double SAM_Battwatts_PVWatts_system_use_lifetime_output_nget(SAM_Battwatts ptr, SAM_error *err);


	/**
	 * FinancialAnalysisParameters Getters
	 */

	SAM_EXPORT double SAM_Battwatts_FinancialAnalysisParameters_analysis_period_nget(SAM_Battwatts ptr, SAM_error *err);


	/**
	 * BatteryModelSimple Getters
	 */

	SAM_EXPORT double SAM_Battwatts_BatteryModelSimple_batt_simple_chemistry_nget(SAM_Battwatts ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battwatts_BatteryModelSimple_batt_simple_dispatch_nget(SAM_Battwatts ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battwatts_BatteryModelSimple_batt_simple_enable_nget(SAM_Battwatts ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battwatts_BatteryModelSimple_batt_simple_kw_nget(SAM_Battwatts ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battwatts_BatteryModelSimple_batt_simple_kwh_nget(SAM_Battwatts ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battwatts_BatteryModelSimple_batt_simple_meter_position_nget(SAM_Battwatts ptr, SAM_error *err);


	/**
	 * Common Getters
	 */

	SAM_EXPORT double* SAM_Battwatts_Common_ac_aget(SAM_Battwatts ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Common_dc_aget(SAM_Battwatts ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Battwatts_Common_inverter_efficiency_nget(SAM_Battwatts ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battwatts_Common_inverter_model_nget(SAM_Battwatts ptr, SAM_error *err);


	/**
	 * ElectricLoadOther Getters
	 */

	SAM_EXPORT double* SAM_Battwatts_ElectricLoadOther_load_aget(SAM_Battwatts ptr, int* length, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double* SAM_Battwatts_Outputs_annual_export_to_grid_energy_aget(SAM_Battwatts ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_annual_import_to_grid_energy_aget(SAM_Battwatts ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Battwatts_Outputs_average_battery_conversion_efficiency_nget(SAM_Battwatts ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battwatts_Outputs_average_battery_roundtrip_efficiency_nget(SAM_Battwatts ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_DOD_aget(SAM_Battwatts ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_DOD_cycle_average_aget(SAM_Battwatts ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_I_aget(SAM_Battwatts ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_SOC_aget(SAM_Battwatts ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_annual_charge_energy_aget(SAM_Battwatts ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_annual_charge_from_grid_aget(SAM_Battwatts ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_annual_charge_from_pv_aget(SAM_Battwatts ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_annual_discharge_energy_aget(SAM_Battwatts ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_annual_energy_loss_aget(SAM_Battwatts ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_annual_energy_system_loss_aget(SAM_Battwatts ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Battwatts_Outputs_batt_bank_installed_capacity_nget(SAM_Battwatts ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_bank_replacement_aget(SAM_Battwatts ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_capacity_percent_aget(SAM_Battwatts ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_capacity_percent_calendar_aget(SAM_Battwatts ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_capacity_percent_cycle_aget(SAM_Battwatts ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_capacity_thermal_percent_aget(SAM_Battwatts ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_conversion_loss_aget(SAM_Battwatts ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_cost_to_cycle_aget(SAM_Battwatts ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_cycles_aget(SAM_Battwatts ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_dispatch_sched_mget(SAM_Battwatts ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_power_aget(SAM_Battwatts ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_power_target_aget(SAM_Battwatts ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Battwatts_Outputs_batt_pv_charge_percent_nget(SAM_Battwatts ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_q0_aget(SAM_Battwatts ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_q1_aget(SAM_Battwatts ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_q2_aget(SAM_Battwatts ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_qmax_aget(SAM_Battwatts ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_qmaxI_aget(SAM_Battwatts ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_qmax_thermal_aget(SAM_Battwatts ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_revenue_charge_aget(SAM_Battwatts ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_revenue_clipcharge_aget(SAM_Battwatts ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_revenue_discharge_aget(SAM_Battwatts ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_revenue_gridcharge_aget(SAM_Battwatts ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_system_loss_aget(SAM_Battwatts ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_temperature_aget(SAM_Battwatts ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_to_grid_aget(SAM_Battwatts ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_to_load_aget(SAM_Battwatts ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_voltage_aget(SAM_Battwatts ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_voltage_cell_aget(SAM_Battwatts ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_fuelcell_to_batt_aget(SAM_Battwatts ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_gen_aget(SAM_Battwatts ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_grid_power_aget(SAM_Battwatts ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_grid_power_target_aget(SAM_Battwatts ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_grid_to_batt_aget(SAM_Battwatts ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_grid_to_load_aget(SAM_Battwatts ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_market_sell_rate_series_yr1_aget(SAM_Battwatts ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_monthly_batt_to_grid_aget(SAM_Battwatts ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_monthly_batt_to_load_aget(SAM_Battwatts ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_monthly_grid_to_batt_aget(SAM_Battwatts ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_monthly_grid_to_load_aget(SAM_Battwatts ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_monthly_pv_to_batt_aget(SAM_Battwatts ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_monthly_pv_to_grid_aget(SAM_Battwatts ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_monthly_pv_to_load_aget(SAM_Battwatts ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_pv_to_batt_aget(SAM_Battwatts ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_pv_to_grid_aget(SAM_Battwatts ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_pv_to_load_aget(SAM_Battwatts ptr, int* length, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif