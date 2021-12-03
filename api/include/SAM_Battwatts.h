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

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_Battwatts_execute(SAM_table data, int verbosity, SAM_error* err);


	//
	// Lifetime parameters
	//

	/**
	 * Set analysis_period: Lifetime analysis period [years]
	 * options: The number of years in the simulation
	 * constraints: None
	 * required if: system_use_lifetime_output=1
	 */
	SAM_EXPORT void SAM_Battwatts_Lifetime_analysis_period_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set system_use_lifetime_output: Enable lifetime simulation [0/1]
	 * options: 0=SingleYearRepeated,1=RunEveryYear
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Battwatts_Lifetime_system_use_lifetime_output_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// Battery parameters
	//

	/**
	 * Set ac: AC inverter power [W]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battwatts_Battery_ac_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set batt_custom_dispatch: Battery Dispatch [kW]
	 * options: None
	 * constraints: None
	 * required if: batt_simple_dispatch=2
	 */
	SAM_EXPORT void SAM_Battwatts_Battery_batt_custom_dispatch_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set batt_simple_chemistry: Battery Chemistry [0=LeadAcid,1=Li-ion/2]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Battwatts_Battery_batt_simple_chemistry_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_simple_dispatch: Battery Dispatch [0=PeakShavingLookAhead,1=PeakShavingLookBehind,2=Custom]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Battwatts_Battery_batt_simple_dispatch_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_simple_enable: Enable Battery [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Battwatts_Battery_batt_simple_enable_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_simple_kw: Battery Power [kW]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Battwatts_Battery_batt_simple_kw_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_simple_kwh: Battery Capacity [kWh]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Battwatts_Battery_batt_simple_kwh_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_simple_meter_position: Battery Meter Position [0=BehindTheMeter,1=FrontOfMeter]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Battwatts_Battery_batt_simple_meter_position_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set crit_load: Critical electricity load (year 1) [kW]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battwatts_Battery_crit_load_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set dc: DC array power [W]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battwatts_Battery_dc_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set inverter_efficiency: Inverter Efficiency [%]
	 * options: None
	 * constraints: MIN=0,MAX=100
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battwatts_Battery_inverter_efficiency_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set load: Electricity load (year 1) [kW]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battwatts_Battery_load_aset(SAM_table ptr, double* arr, int length, SAM_error *err);


	//
	// Load parameters
	//

	/**
	 * Set grid_outage: Timesteps with grid outage [0/1]
	 * options: 0=GridAvailable,1=GridUnavailable,Length=load
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battwatts_Load_grid_outage_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set load_escalation: Annual load escalation [%/year]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Battwatts_Load_load_escalation_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set run_resiliency_calcs: Enable resilence calculations for every timestep [0/1]
	 * options: 0=DisableCalcs,1=EnableCalcs
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Battwatts_Load_run_resiliency_calcs_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// GridLimits parameters
	//

	/**
	 * Set enable_interconnection_limit: Enable grid interconnection limit [0/1]
	 * options: Enable a grid interconnection limit
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battwatts_GridLimits_enable_interconnection_limit_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set grid_curtailment: Grid curtailment as energy delivery limit (first year) [MW]
	 * options: None
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Battwatts_GridLimits_grid_curtailment_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set grid_interconnection_limit_kwac: Grid interconnection limit [kWac]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battwatts_GridLimits_grid_interconnection_limit_kwac_nset(SAM_table ptr, double number, SAM_error *err);


	/**
	 * Lifetime Getters
	 */

	SAM_EXPORT double SAM_Battwatts_Lifetime_analysis_period_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battwatts_Lifetime_system_use_lifetime_output_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Battery Getters
	 */

	SAM_EXPORT double* SAM_Battwatts_Battery_ac_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Battery_batt_custom_dispatch_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Battwatts_Battery_batt_simple_chemistry_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battwatts_Battery_batt_simple_dispatch_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battwatts_Battery_batt_simple_enable_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battwatts_Battery_batt_simple_kw_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battwatts_Battery_batt_simple_kwh_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battwatts_Battery_batt_simple_meter_position_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Battery_crit_load_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Battery_dc_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Battwatts_Battery_inverter_efficiency_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Battery_load_aget(SAM_table ptr, int* length, SAM_error *err);


	/**
	 * Load Getters
	 */

	SAM_EXPORT double* SAM_Battwatts_Load_grid_outage_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Load_load_escalation_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Battwatts_Load_run_resiliency_calcs_nget(SAM_table ptr, SAM_error *err);


	/**
	 * GridLimits Getters
	 */

	SAM_EXPORT double SAM_Battwatts_GridLimits_enable_interconnection_limit_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_GridLimits_grid_curtailment_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Battwatts_GridLimits_grid_interconnection_limit_kwac_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double SAM_Battwatts_Outputs_annual_crit_load_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battwatts_Outputs_annual_crit_load_unmet_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battwatts_Outputs_annual_crit_load_unmet_percentage_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_annual_energy_distribution_time_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_annual_export_to_grid_energy_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_annual_import_to_grid_energy_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Battwatts_Outputs_annual_outage_losses_unmet_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battwatts_Outputs_average_battery_conversion_efficiency_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battwatts_Outputs_average_battery_roundtrip_efficiency_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battwatts_Outputs_avg_critical_load_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_DOD_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_DOD_cycle_average_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_I_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_SOC_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_annual_charge_energy_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_annual_charge_from_grid_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_annual_charge_from_system_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_annual_discharge_energy_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_annual_energy_loss_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_annual_energy_system_loss_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Battwatts_Outputs_batt_bank_installed_capacity_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_bank_replacement_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_capacity_percent_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_capacity_percent_calendar_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_capacity_percent_cycle_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_capacity_thermal_percent_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_conversion_loss_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_cost_to_cycle_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_cycles_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_dispatch_sched_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_power_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_power_target_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_pvs_PV_ramp_interval_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_pvs_P_pv_ac_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_pvs_battpower_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_pvs_battsoc_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_pvs_curtail_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Battwatts_Outputs_batt_pvs_energy_to_grid_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battwatts_Outputs_batt_pvs_energy_to_grid_percent_sam_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_pvs_forecast_pv_energy_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_pvs_outpower_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Battwatts_Outputs_batt_pvs_violation_count_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_pvs_violation_list_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Battwatts_Outputs_batt_pvs_violation_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_q0_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_q1_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_q2_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_qmax_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_qmaxI_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_qmax_thermal_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_revenue_charge_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_revenue_clipcharge_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_revenue_discharge_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_revenue_gridcharge_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Battwatts_Outputs_batt_system_charge_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_system_loss_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_temperature_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_to_grid_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_to_load_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_to_system_load_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_voltage_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_batt_voltage_cell_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_cdf_of_surviving_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_crit_load_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_crit_load_unmet_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_fuelcell_to_batt_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_gen_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_gen_without_battery_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_grid_power_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_grid_power_target_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_grid_to_batt_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_grid_to_load_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_interconnection_loss_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_market_sell_rate_series_yr1_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_monthly_batt_to_grid_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_monthly_batt_to_load_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_monthly_batt_to_system_load_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_monthly_crit_load_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_monthly_crit_load_unmet_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_monthly_crit_load_unmet_percentage_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_monthly_grid_to_batt_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_monthly_grid_to_load_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_monthly_interconnection_loss_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_monthly_outage_losses_unmet_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_monthly_system_to_batt_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_monthly_system_to_grid_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_monthly_system_to_load_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_outage_durations_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_outage_losses_unmet_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_pdf_of_surviving_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_resilience_hrs_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Battwatts_Outputs_resilience_hrs_avg_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battwatts_Outputs_resilience_hrs_max_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battwatts_Outputs_resilience_hrs_min_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_survival_function_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_system_to_batt_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_system_to_grid_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battwatts_Outputs_system_to_load_aget(SAM_table ptr, int* length, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif