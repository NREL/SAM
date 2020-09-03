#ifndef SAM_BATTERY_H_
#define SAM_BATTERY_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// Battery Technology Model
	//

	/** 
	 * Create a Battery variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_Battery;

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_Battery_execute(SAM_table data, int verbosity, SAM_error* err);


	//
	// Simulation parameters
	//

	/**
	 * Set percent_complete: Estimated simulation status [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Simulation_percent_complete_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// Lifetime parameters
	//

	/**
	 * Set analysis_period: Lifetime analysis period [years]
	 * options: The number of years in the simulation
	 * constraints: None
	 * required if: system_use_lifetime_output=1
	 */
	SAM_EXPORT void SAM_Battery_Lifetime_analysis_period_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set inflation_rate: Inflation rate [%]
	 * options: None
	 * constraints: MIN=-99
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Battery_Lifetime_inflation_rate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set system_use_lifetime_output: Lifetime simulation [0/1]
	 * options: 0=SingleYearRepeated,1=RunEveryYear
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Battery_Lifetime_system_use_lifetime_output_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// BatterySystem parameters
	//

	/**
	 * Set batt_ac_dc_efficiency: Inverter AC to battery DC efficiency
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatterySystem_batt_ac_dc_efficiency_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_ac_or_dc: Battery interconnection (AC or DC)
	 * options: 0=DC_Connected,1=AC_Connected
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatterySystem_batt_ac_or_dc_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_computed_bank_capacity: Computed bank capacity [kWh]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatterySystem_batt_computed_bank_capacity_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_computed_series: Number of cells in series
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatterySystem_batt_computed_series_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_computed_strings: Number of strings of cells
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatterySystem_batt_computed_strings_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_current_charge_max: Maximum charge current [A]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatterySystem_batt_current_charge_max_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_current_choice: Limit cells by current or power
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatterySystem_batt_current_choice_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_current_discharge_max: Maximum discharge current [A]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatterySystem_batt_current_discharge_max_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_cycle_cost: Input battery cycle costs [$/cycle-kWh]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatterySystem_batt_cycle_cost_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set batt_cycle_cost_choice: Use SAM model for cycle costs or input custom [0/1]
	 * options: 0=UseCostModel,1=InputCost
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatterySystem_batt_cycle_cost_choice_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_dc_ac_efficiency: Battery DC to AC efficiency
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatterySystem_batt_dc_ac_efficiency_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_dc_dc_efficiency: System DC to battery DC efficiency
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatterySystem_batt_dc_dc_efficiency_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_inverter_efficiency_cutoff: Inverter efficiency at which to cut battery charge or discharge off [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatterySystem_batt_inverter_efficiency_cutoff_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_loss_choice: Loss power input option [0/1]
	 * options: 0=Monthly,1=TimeSeries
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Battery_BatterySystem_batt_loss_choice_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_losses: Battery system losses at each timestep [kW]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Battery_BatterySystem_batt_losses_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set batt_losses_charging: Battery system losses when charging [kW]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Battery_BatterySystem_batt_losses_charging_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set batt_losses_discharging: Battery system losses when discharging [kW]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Battery_BatterySystem_batt_losses_discharging_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set batt_losses_idle: Battery system losses when idle [kW]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Battery_BatterySystem_batt_losses_idle_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set batt_mass: Battery mass [kg]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatterySystem_batt_mass_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_meter_position: Position of battery relative to electric meter
	 * options: 0=BehindTheMeter,1=FrontOfMeter
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatterySystem_batt_meter_position_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_power_charge_max_kwac: Maximum charge power (AC) [kWac]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatterySystem_batt_power_charge_max_kwac_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_power_charge_max_kwdc: Maximum charge power (DC) [kWdc]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatterySystem_batt_power_charge_max_kwdc_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_power_discharge_max_kwac: Maximum discharge power (AC) [kWac]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatterySystem_batt_power_discharge_max_kwac_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_power_discharge_max_kwdc: Maximum discharge power (DC) [kWdc]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatterySystem_batt_power_discharge_max_kwdc_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_replacement_capacity: Capacity degradation at which to replace battery [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatterySystem_batt_replacement_capacity_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_replacement_option: Enable battery replacement? [0=none,1=capacity based,2=user schedule]
	 * options: None
	 * constraints: INTEGER,MIN=0,MAX=2
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Battery_BatterySystem_batt_replacement_option_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_replacement_schedule: Battery bank number of replacements in each year [number/year]
	 * options: length <= analysis_period
	 * constraints: None
	 * required if: batt_replacement_option=2
	 */
	SAM_EXPORT void SAM_Battery_BatterySystem_batt_replacement_schedule_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set batt_replacement_schedule_percent: Percentage of battery capacity to replace in each year [%]
	 * options: length <= analysis_period
	 * constraints: None
	 * required if: batt_replacement_option=2
	 */
	SAM_EXPORT void SAM_Battery_BatterySystem_batt_replacement_schedule_percent_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set batt_surface_area: Battery surface area [m^2]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatterySystem_batt_surface_area_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set en_batt: Enable battery storage model [0/1]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Battery_BatterySystem_en_batt_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set om_replacement_cost1: Cost to replace battery per kWh [$/kWh]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatterySystem_om_replacement_cost1_aset(SAM_table ptr, double* arr, int length, SAM_error *err);


	//
	// SystemOutput parameters
	//

	/**
	 * Set annual_energy: Annual Energy [kWh]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Battery_SystemOutput_annual_energy_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set capacity_factor: Capacity factor [%]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Battery_SystemOutput_capacity_factor_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set gen: System power generated [kW]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_SystemOutput_gen_aset(SAM_table ptr, double* arr, int length, SAM_error *err);


	//
	// Load parameters
	//

	/**
	 * Set crit_load: Critical electricity load (year 1) [kW]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Load_crit_load_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set load: Electricity load (year 1) [kW]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Load_load_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set load_escalation: Annual load escalation [%/year]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Battery_Load_load_escalation_aset(SAM_table ptr, double* arr, int length, SAM_error *err);


	//
	// BatteryCell parameters
	//

	/**
	 * Set LeadAcid_q10_computed: Capacity at 10-hour discharge rate [Ah]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryCell_LeadAcid_q10_computed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set LeadAcid_q20_computed: Capacity at 20-hour discharge rate [Ah]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryCell_LeadAcid_q20_computed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set LeadAcid_qn_computed: Capacity at discharge rate for n-hour rate [Ah]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryCell_LeadAcid_qn_computed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set LeadAcid_tn: Time to discharge [h]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryCell_LeadAcid_tn_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_C_rate: Rate at which voltage vs. capacity curve input
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryCell_batt_C_rate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_Cp: Battery specific heat capacity [J/KgK]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryCell_batt_Cp_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_Qexp: Cell capacity at end of exponential zone [Ah]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryCell_batt_Qexp_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_Qfull: Fully charged cell capacity [Ah]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryCell_batt_Qfull_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_Qfull_flow: Fully charged flow battery capacity [Ah]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryCell_batt_Qfull_flow_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_Qnom: Cell capacity at end of nominal zone [Ah]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryCell_batt_Qnom_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_Vexp: Cell voltage at end of exponential zone [V]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryCell_batt_Vexp_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_Vfull: Fully charged cell voltage [V]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryCell_batt_Vfull_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_Vnom: Cell voltage at end of nominal zone [V]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryCell_batt_Vnom_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_Vnom_default: Default nominal cell voltage [V]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryCell_batt_Vnom_default_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_calendar_a: Calendar life model coefficient [1/sqrt(day)]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryCell_batt_calendar_a_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_calendar_b: Calendar life model coefficient [K]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryCell_batt_calendar_b_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_calendar_c: Calendar life model coefficient [K]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryCell_batt_calendar_c_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_calendar_choice: Calendar life degradation input option [0/1/2]
	 * options: 0=NoCalendarDegradation,1=LithiomIonModel,2=InputLossTable
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryCell_batt_calendar_choice_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_calendar_lifetime_matrix: Days vs capacity
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryCell_batt_calendar_lifetime_matrix_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set batt_calendar_q0: Calendar life model initial capacity cofficient
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryCell_batt_calendar_q0_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_chem: Battery chemistry
	 * options: 0=LeadAcid,1=LiIon
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryCell_batt_chem_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_h_to_ambient: Heat transfer between battery and environment [W/m2K]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryCell_batt_h_to_ambient_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_initial_SOC: Initial state-of-charge [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryCell_batt_initial_SOC_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_lifetime_matrix: Cycles vs capacity at different depths-of-discharge
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryCell_batt_lifetime_matrix_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set batt_maximum_SOC: Maximum allowed state-of-charge [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryCell_batt_maximum_SOC_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_minimum_SOC: Minimum allowed state-of-charge [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryCell_batt_minimum_SOC_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_minimum_modetime: Minimum time at charge state [min]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryCell_batt_minimum_modetime_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_resistance: Internal resistance [Ohm]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryCell_batt_resistance_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_room_temperature_celsius: Temperature of storage room [C]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryCell_batt_room_temperature_celsius_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set batt_voltage_choice: Battery voltage input option [0/1]
	 * options: 0=UseVoltageModel,1=InputVoltageTable
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Battery_BatteryCell_batt_voltage_choice_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_voltage_matrix: Battery voltage vs. depth-of-discharge
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryCell_batt_voltage_matrix_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set cap_vs_temp: Effective capacity as function of temperature [C,%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryCell_cap_vs_temp_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);


	//
	// Inverter parameters
	//

	/**
	 * Set inv_cec_cg_eff_cec: Inverter Coefficient Generator CEC Efficiency [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Inverter_inv_cec_cg_eff_cec_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set inv_cec_cg_paco: Inverter Coefficient Generator Max AC Power [Wac]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Inverter_inv_cec_cg_paco_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set inv_ds_eff: Inverter Datasheet Efficiency [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Inverter_inv_ds_eff_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set inv_ds_paco: Inverter Datasheet Maximum AC Power [Wac]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Inverter_inv_ds_paco_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set inv_pd_eff: Inverter Partload Efficiency [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Inverter_inv_pd_eff_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set inv_pd_paco: Inverter Partload Maximum AC Power [Wac]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Inverter_inv_pd_paco_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set inv_snl_eff_cec: Inverter Sandia CEC Efficiency [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Inverter_inv_snl_eff_cec_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set inv_snl_paco: Inverter Sandia Maximum AC Power [Wac]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Inverter_inv_snl_paco_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set inverter_count: Number of inverters
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Inverter_inverter_count_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set inverter_model: Inverter model specifier
	 * options: 0=cec,1=datasheet,2=partload,3=coefficientgenerator,4=generic
	 * constraints: INTEGER,MIN=0,MAX=4
	 * required if: ?=4
	 */
	SAM_EXPORT void SAM_Battery_Inverter_inverter_model_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// Losses parameters
	//

	/**
	 * Set dcoptimizer_loss: DC optimizer loss
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Losses_dcoptimizer_loss_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// BatteryDispatch parameters
	//

	/**
	 * Set batt_auto_gridcharge_max_daily: Allowed grid charging percent per day for automated dispatch [kW]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryDispatch_batt_auto_gridcharge_max_daily_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_custom_dispatch: Custom battery power for every time step [kW]
	 * options: kWAC if AC-connected, else kWDC
	 * constraints: None
	 * required if: en_batt=1&batt_dispatch_choice=3
	 */
	SAM_EXPORT void SAM_Battery_BatteryDispatch_batt_custom_dispatch_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set batt_dispatch_auto_can_charge: System charging allowed for automated dispatch? [kW]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryDispatch_batt_dispatch_auto_can_charge_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_dispatch_auto_can_clipcharge: Battery can charge from clipped power for automated dispatch? [kW]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryDispatch_batt_dispatch_auto_can_clipcharge_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_dispatch_auto_can_fuelcellcharge: Charging from fuel cell allowed for automated dispatch? [kW]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryDispatch_batt_dispatch_auto_can_fuelcellcharge_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_dispatch_auto_can_gridcharge: Grid charging allowed for automated dispatch? [kW]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryDispatch_batt_dispatch_auto_can_gridcharge_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_dispatch_choice: Battery dispatch algorithm [0/1/2/3/4/5]
	 * options: If behind the meter: 0=PeakShavingLookAhead,1=PeakShavingLookBehind,2=InputGridTarget,3=InputBatteryPower,4=ManualDispatch,5=PriceSignalForecast if front of meter: 0=AutomatedLookAhead,1=AutomatedLookBehind,2=AutomatedInputForecast,3=InputBatteryPower,4=ManualDispatch
	 * constraints: None
	 * required if: en_batt=1
	 */
	SAM_EXPORT void SAM_Battery_BatteryDispatch_batt_dispatch_choice_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_dispatch_update_frequency_hours: Frequency to update the look-ahead dispatch [hours]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryDispatch_batt_dispatch_update_frequency_hours_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_look_ahead_hours: Hours to look ahead in automated dispatch [hours]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryDispatch_batt_look_ahead_hours_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_pv_ac_forecast: PV ac power forecast [kW]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryDispatch_batt_pv_ac_forecast_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set batt_pv_clipping_forecast: PV clipping forecast [kW]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryDispatch_batt_pv_clipping_forecast_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set batt_target_choice: Target power input option [0/1]
	 * options: 0=InputMonthlyTarget,1=InputFullTimeSeries
	 * constraints: None
	 * required if: en_batt=1&batt_meter_position=0&batt_dispatch_choice=2
	 */
	SAM_EXPORT void SAM_Battery_BatteryDispatch_batt_target_choice_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_target_power: Grid target power for every time step [kW]
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_meter_position=0&batt_dispatch_choice=2
	 */
	SAM_EXPORT void SAM_Battery_BatteryDispatch_batt_target_power_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set batt_target_power_monthly: Grid target power on monthly basis [kW]
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_meter_position=0&batt_dispatch_choice=2
	 */
	SAM_EXPORT void SAM_Battery_BatteryDispatch_batt_target_power_monthly_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set dispatch_manual_charge: Periods 1-6 charging from system allowed?
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_dispatch_choice=4
	 */
	SAM_EXPORT void SAM_Battery_BatteryDispatch_dispatch_manual_charge_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set dispatch_manual_discharge: Periods 1-6 discharging allowed?
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_dispatch_choice=4
	 */
	SAM_EXPORT void SAM_Battery_BatteryDispatch_dispatch_manual_discharge_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set dispatch_manual_fuelcellcharge: Periods 1-6 charging from fuel cell allowed?
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_BatteryDispatch_dispatch_manual_fuelcellcharge_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set dispatch_manual_gridcharge: Periods 1-6 grid charging allowed?
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_dispatch_choice=4
	 */
	SAM_EXPORT void SAM_Battery_BatteryDispatch_dispatch_manual_gridcharge_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set dispatch_manual_percent_discharge: Periods 1-6 discharge percent [%]
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_dispatch_choice=4
	 */
	SAM_EXPORT void SAM_Battery_BatteryDispatch_dispatch_manual_percent_discharge_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set dispatch_manual_percent_gridcharge: Periods 1-6 gridcharge percent [%]
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_dispatch_choice=4
	 */
	SAM_EXPORT void SAM_Battery_BatteryDispatch_dispatch_manual_percent_gridcharge_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set dispatch_manual_sched: Battery dispatch schedule for weekday
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_dispatch_choice=4
	 */
	SAM_EXPORT void SAM_Battery_BatteryDispatch_dispatch_manual_sched_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set dispatch_manual_sched_weekend: Battery dispatch schedule for weekend
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_dispatch_choice=4
	 */
	SAM_EXPORT void SAM_Battery_BatteryDispatch_dispatch_manual_sched_weekend_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);


	//
	// ElectricityRates parameters
	//

	/**
	 * Set en_electricity_rates: Enable Electricity Rates [0/1]
	 * options: 0=EnableElectricityRates,1=NoRates
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_ElectricityRates_en_electricity_rates_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set rate_escalation: Annual electricity rate escalation [%/year]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Battery_ElectricityRates_rate_escalation_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set ur_dc_enable: Enable demand charge [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Battery_ElectricityRates_ur_dc_enable_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_dc_flat_mat: Demand rates (flat) table
	 * options: None
	 * constraints: None
	 * required if: ur_dc_enable=1
	 */
	SAM_EXPORT void SAM_Battery_ElectricityRates_ur_dc_flat_mat_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_dc_sched_weekday: Demand charge weekday schedule
	 * options: 12x24
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_ElectricityRates_ur_dc_sched_weekday_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_dc_sched_weekend: Demand charge weekend schedule
	 * options: 12x24
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_ElectricityRates_ur_dc_sched_weekend_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_dc_tou_mat: Demand rates (TOU) table
	 * options: None
	 * constraints: None
	 * required if: ur_dc_enable=1
	 */
	SAM_EXPORT void SAM_Battery_ElectricityRates_ur_dc_tou_mat_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_ec_sched_weekday: Energy charge weekday schedule
	 * options: 12x24
	 * constraints: None
	 * required if: en_batt=1&batt_meter_position=0&batt_dispatch_choice=5
	 */
	SAM_EXPORT void SAM_Battery_ElectricityRates_ur_ec_sched_weekday_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_ec_sched_weekend: Energy charge weekend schedule
	 * options: 12x24
	 * constraints: None
	 * required if: en_batt=1&batt_meter_position=0&batt_dispatch_choice=5
	 */
	SAM_EXPORT void SAM_Battery_ElectricityRates_ur_ec_sched_weekend_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_ec_tou_mat: Energy rates table
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_meter_position=0&batt_dispatch_choice=5
	 */
	SAM_EXPORT void SAM_Battery_ElectricityRates_ur_ec_tou_mat_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_en_ts_buy_rate: Enable time step buy rates [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Battery_ElectricityRates_ur_en_ts_buy_rate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_en_ts_sell_rate: Enable time step sell rates [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Battery_ElectricityRates_ur_en_ts_sell_rate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_metering_option: Metering options [0=net energy metering,1=net energy metering with $ credits,2=net billing,3=net billing with carryover to next month,4=buy all - sell all]
	 * options: Net metering monthly excess
	 * constraints: INTEGER,MIN=0,MAX=4
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Battery_ElectricityRates_ur_metering_option_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_nm_credit_month: Month of rollover credits [$/kWh]
	 * options: None
	 * constraints: INTEGER,MIN=0,MAX=11
	 * required if: ?=11
	 */
	SAM_EXPORT void SAM_Battery_ElectricityRates_ur_nm_credit_month_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_nm_credit_rollover: Roll over credits to next year [0/1]
	 * options: None
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Battery_ElectricityRates_ur_nm_credit_rollover_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_nm_yearend_sell_rate: Net metering credit sell rate [$/kWh]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Battery_ElectricityRates_ur_nm_yearend_sell_rate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_sell_eq_buy: Set sell rate equal to buy rate [0/1]
	 * options: Optional override
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Battery_ElectricityRates_ur_sell_eq_buy_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_ts_buy_rate: Time step buy rates [0/1]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_ElectricityRates_ur_ts_buy_rate_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set ur_ts_sell_rate: Time step sell rates [0/1]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_ElectricityRates_ur_ts_sell_rate_aset(SAM_table ptr, double* arr, int length, SAM_error *err);


	//
	// FuelCell parameters
	//

	/**
	 * Set fuelcell_power: Electricity from fuel cell [kW]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_FuelCell_fuelcell_power_aset(SAM_table ptr, double* arr, int length, SAM_error *err);


	//
	// PriceSignal parameters
	//

	/**
	 * Set dispatch_factors_ts: Dispatch payment factor time step
	 * options: None
	 * constraints: None
	 * required if: forecast_price_signal_model=0&en_batt=1&batt_meter_position=1&ppa_multiplier_model=1
	 */
	SAM_EXPORT void SAM_Battery_PriceSignal_dispatch_factors_ts_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set dispatch_sched_weekday: Diurnal weekday TOD periods [1..9]
	 * options: 12 x 24 matrix
	 * constraints: None
	 * required if: en_batt=1&batt_meter_position=1&forecast_price_signal_model=0&ppa_multiplier_model=0
	 */
	SAM_EXPORT void SAM_Battery_PriceSignal_dispatch_sched_weekday_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set dispatch_sched_weekend: Diurnal weekend TOD periods [1..9]
	 * options: 12 x 24 matrix
	 * constraints: None
	 * required if: en_batt=1&batt_meter_position=1&forecast_price_signal_model=0&ppa_multiplier_model=0
	 */
	SAM_EXPORT void SAM_Battery_PriceSignal_dispatch_sched_weekend_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set dispatch_tod_factors: TOD factors for periods 1-9
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_meter_position=1&forecast_price_signal_model=0&ppa_multiplier_model=0
	 */
	SAM_EXPORT void SAM_Battery_PriceSignal_dispatch_tod_factors_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set forecast_price_signal_model: Forecast price signal model selected [0/1]
	 * options: 0=PPA based,1=Merchant Plant
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Battery_PriceSignal_forecast_price_signal_model_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mp_ancserv1_revenue: Ancillary services 1 revenue input [ [MW, $/MW]]
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_meter_position=1&forecast_price_signal_model=1
	 */
	SAM_EXPORT void SAM_Battery_PriceSignal_mp_ancserv1_revenue_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set mp_ancserv2_revenue: Ancillary services 2 revenue input [ [MW, $/MW]]
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_meter_position=1&forecast_price_signal_model=1
	 */
	SAM_EXPORT void SAM_Battery_PriceSignal_mp_ancserv2_revenue_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set mp_ancserv3_revenue: Ancillary services 3 revenue input [ [MW, $/MW]]
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_meter_position=1&forecast_price_signal_model=1
	 */
	SAM_EXPORT void SAM_Battery_PriceSignal_mp_ancserv3_revenue_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set mp_ancserv4_revenue: Ancillary services 4 revenue input [ [MW, $/MW]]
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_meter_position=1&forecast_price_signal_model=1
	 */
	SAM_EXPORT void SAM_Battery_PriceSignal_mp_ancserv4_revenue_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set mp_enable_ancserv1: Enable ancillary services 1 revenue [0/1]
	 * options: None
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: forecast_price_signal_model=1
	 */
	SAM_EXPORT void SAM_Battery_PriceSignal_mp_enable_ancserv1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mp_enable_ancserv2: Enable ancillary services 2 revenue [0/1]
	 * options: None
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: forecast_price_signal_model=1
	 */
	SAM_EXPORT void SAM_Battery_PriceSignal_mp_enable_ancserv2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mp_enable_ancserv3: Enable ancillary services 3 revenue [0/1]
	 * options: None
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: forecast_price_signal_model=1
	 */
	SAM_EXPORT void SAM_Battery_PriceSignal_mp_enable_ancserv3_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mp_enable_ancserv4: Enable ancillary services 4 revenue [0/1]
	 * options: None
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: forecast_price_signal_model=1
	 */
	SAM_EXPORT void SAM_Battery_PriceSignal_mp_enable_ancserv4_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mp_enable_energy_market_revenue: Enable energy market revenue [0/1]
	 * options: 0=false,1=true
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: en_batt=1&batt_meter_position=1&forecast_price_signal_model=1
	 */
	SAM_EXPORT void SAM_Battery_PriceSignal_mp_enable_energy_market_revenue_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mp_energy_market_revenue: Energy market revenue input [ [MW, $/MW]]
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_meter_position=1&forecast_price_signal_model=1
	 */
	SAM_EXPORT void SAM_Battery_PriceSignal_mp_energy_market_revenue_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ppa_multiplier_model: PPA multiplier model [0/1]
	 * options: 0=diurnal,1=timestep
	 * constraints: INTEGER,MIN=0
	 * required if: forecast_price_signal_model=0&en_batt=1&batt_meter_position=1
	 */
	SAM_EXPORT void SAM_Battery_PriceSignal_ppa_multiplier_model_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ppa_price_input: PPA Price Input
	 * options: None
	 * constraints: None
	 * required if: forecast_price_signal_model=0&en_batt=1&batt_meter_position=1
	 */
	SAM_EXPORT void SAM_Battery_PriceSignal_ppa_price_input_aset(SAM_table ptr, double* arr, int length, SAM_error *err);


	/**
	 * Simulation Getters
	 */

	SAM_EXPORT double SAM_Battery_Simulation_percent_complete_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Lifetime Getters
	 */

	SAM_EXPORT double SAM_Battery_Lifetime_analysis_period_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Lifetime_inflation_rate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Lifetime_system_use_lifetime_output_nget(SAM_table ptr, SAM_error *err);


	/**
	 * BatterySystem Getters
	 */

	SAM_EXPORT double SAM_Battery_BatterySystem_batt_ac_dc_efficiency_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_BatterySystem_batt_ac_or_dc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_BatterySystem_batt_computed_bank_capacity_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_BatterySystem_batt_computed_series_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_BatterySystem_batt_computed_strings_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_BatterySystem_batt_current_charge_max_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_BatterySystem_batt_current_choice_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_BatterySystem_batt_current_discharge_max_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_BatterySystem_batt_cycle_cost_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Battery_BatterySystem_batt_cycle_cost_choice_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_BatterySystem_batt_dc_ac_efficiency_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_BatterySystem_batt_dc_dc_efficiency_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_BatterySystem_batt_inverter_efficiency_cutoff_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_BatterySystem_batt_loss_choice_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_BatterySystem_batt_losses_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_BatterySystem_batt_losses_charging_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_BatterySystem_batt_losses_discharging_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_BatterySystem_batt_losses_idle_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Battery_BatterySystem_batt_mass_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_BatterySystem_batt_meter_position_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_BatterySystem_batt_power_charge_max_kwac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_BatterySystem_batt_power_charge_max_kwdc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_BatterySystem_batt_power_discharge_max_kwac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_BatterySystem_batt_power_discharge_max_kwdc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_BatterySystem_batt_replacement_capacity_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_BatterySystem_batt_replacement_option_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_BatterySystem_batt_replacement_schedule_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_BatterySystem_batt_replacement_schedule_percent_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Battery_BatterySystem_batt_surface_area_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_BatterySystem_en_batt_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_BatterySystem_om_replacement_cost1_aget(SAM_table ptr, int* length, SAM_error *err);


	/**
	 * SystemOutput Getters
	 */

	SAM_EXPORT double SAM_Battery_SystemOutput_annual_energy_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_SystemOutput_capacity_factor_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_SystemOutput_gen_aget(SAM_table ptr, int* length, SAM_error *err);


	/**
	 * Load Getters
	 */

	SAM_EXPORT double* SAM_Battery_Load_crit_load_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Load_load_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Load_load_escalation_aget(SAM_table ptr, int* length, SAM_error *err);


	/**
	 * BatteryCell Getters
	 */

	SAM_EXPORT double SAM_Battery_BatteryCell_LeadAcid_q10_computed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_BatteryCell_LeadAcid_q20_computed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_BatteryCell_LeadAcid_qn_computed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_BatteryCell_LeadAcid_tn_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_BatteryCell_batt_C_rate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_BatteryCell_batt_Cp_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_BatteryCell_batt_Qexp_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_BatteryCell_batt_Qfull_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_BatteryCell_batt_Qfull_flow_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_BatteryCell_batt_Qnom_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_BatteryCell_batt_Vexp_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_BatteryCell_batt_Vfull_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_BatteryCell_batt_Vnom_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_BatteryCell_batt_Vnom_default_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_BatteryCell_batt_calendar_a_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_BatteryCell_batt_calendar_b_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_BatteryCell_batt_calendar_c_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_BatteryCell_batt_calendar_choice_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_BatteryCell_batt_calendar_lifetime_matrix_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Battery_BatteryCell_batt_calendar_q0_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_BatteryCell_batt_chem_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_BatteryCell_batt_h_to_ambient_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_BatteryCell_batt_initial_SOC_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_BatteryCell_batt_lifetime_matrix_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Battery_BatteryCell_batt_maximum_SOC_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_BatteryCell_batt_minimum_SOC_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_BatteryCell_batt_minimum_modetime_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_BatteryCell_batt_resistance_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_BatteryCell_batt_room_temperature_celsius_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Battery_BatteryCell_batt_voltage_choice_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_BatteryCell_batt_voltage_matrix_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_BatteryCell_cap_vs_temp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);


	/**
	 * Inverter Getters
	 */

	SAM_EXPORT double SAM_Battery_Inverter_inv_cec_cg_eff_cec_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Inverter_inv_cec_cg_paco_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Inverter_inv_ds_eff_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Inverter_inv_ds_paco_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Inverter_inv_pd_eff_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Inverter_inv_pd_paco_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Inverter_inv_snl_eff_cec_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Inverter_inv_snl_paco_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Inverter_inverter_count_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Inverter_inverter_model_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Losses Getters
	 */

	SAM_EXPORT double SAM_Battery_Losses_dcoptimizer_loss_nget(SAM_table ptr, SAM_error *err);


	/**
	 * BatteryDispatch Getters
	 */

	SAM_EXPORT double SAM_Battery_BatteryDispatch_batt_auto_gridcharge_max_daily_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_BatteryDispatch_batt_custom_dispatch_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Battery_BatteryDispatch_batt_dispatch_auto_can_charge_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_BatteryDispatch_batt_dispatch_auto_can_clipcharge_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_BatteryDispatch_batt_dispatch_auto_can_fuelcellcharge_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_BatteryDispatch_batt_dispatch_auto_can_gridcharge_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_BatteryDispatch_batt_dispatch_choice_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_BatteryDispatch_batt_dispatch_update_frequency_hours_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_BatteryDispatch_batt_look_ahead_hours_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_BatteryDispatch_batt_pv_ac_forecast_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_BatteryDispatch_batt_pv_clipping_forecast_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Battery_BatteryDispatch_batt_target_choice_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_BatteryDispatch_batt_target_power_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_BatteryDispatch_batt_target_power_monthly_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_BatteryDispatch_dispatch_manual_charge_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_BatteryDispatch_dispatch_manual_discharge_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_BatteryDispatch_dispatch_manual_fuelcellcharge_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_BatteryDispatch_dispatch_manual_gridcharge_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_BatteryDispatch_dispatch_manual_percent_discharge_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_BatteryDispatch_dispatch_manual_percent_gridcharge_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_BatteryDispatch_dispatch_manual_sched_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_BatteryDispatch_dispatch_manual_sched_weekend_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);


	/**
	 * ElectricityRates Getters
	 */

	SAM_EXPORT double SAM_Battery_ElectricityRates_en_electricity_rates_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_ElectricityRates_rate_escalation_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Battery_ElectricityRates_ur_dc_enable_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_ElectricityRates_ur_dc_flat_mat_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_ElectricityRates_ur_dc_sched_weekday_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_ElectricityRates_ur_dc_sched_weekend_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_ElectricityRates_ur_dc_tou_mat_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_ElectricityRates_ur_ec_sched_weekday_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_ElectricityRates_ur_ec_sched_weekend_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_ElectricityRates_ur_ec_tou_mat_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Battery_ElectricityRates_ur_en_ts_buy_rate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_ElectricityRates_ur_en_ts_sell_rate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_ElectricityRates_ur_metering_option_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_ElectricityRates_ur_nm_credit_month_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_ElectricityRates_ur_nm_credit_rollover_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_ElectricityRates_ur_nm_yearend_sell_rate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_ElectricityRates_ur_sell_eq_buy_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_ElectricityRates_ur_ts_buy_rate_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_ElectricityRates_ur_ts_sell_rate_aget(SAM_table ptr, int* length, SAM_error *err);


	/**
	 * FuelCell Getters
	 */

	SAM_EXPORT double* SAM_Battery_FuelCell_fuelcell_power_aget(SAM_table ptr, int* length, SAM_error *err);


	/**
	 * PriceSignal Getters
	 */

	SAM_EXPORT double* SAM_Battery_PriceSignal_dispatch_factors_ts_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_PriceSignal_dispatch_sched_weekday_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_PriceSignal_dispatch_sched_weekend_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_PriceSignal_dispatch_tod_factors_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Battery_PriceSignal_forecast_price_signal_model_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_PriceSignal_mp_ancserv1_revenue_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_PriceSignal_mp_ancserv2_revenue_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_PriceSignal_mp_ancserv3_revenue_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_PriceSignal_mp_ancserv4_revenue_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Battery_PriceSignal_mp_enable_ancserv1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_PriceSignal_mp_enable_ancserv2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_PriceSignal_mp_enable_ancserv3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_PriceSignal_mp_enable_ancserv4_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_PriceSignal_mp_enable_energy_market_revenue_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_PriceSignal_mp_energy_market_revenue_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Battery_PriceSignal_ppa_multiplier_model_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_PriceSignal_ppa_price_input_aget(SAM_table ptr, int* length, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double* SAM_Battery_Outputs_annual_export_to_grid_energy_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_annual_import_to_grid_energy_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Outputs_average_battery_conversion_efficiency_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Outputs_average_battery_roundtrip_efficiency_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Outputs_avg_critical_load_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_DOD_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_DOD_cycle_average_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_I_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_SOC_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_annual_charge_energy_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_annual_charge_from_grid_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_annual_charge_from_pv_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_annual_discharge_energy_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_annual_energy_loss_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_annual_energy_system_loss_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Outputs_batt_bank_installed_capacity_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_bank_replacement_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_capacity_percent_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_capacity_percent_calendar_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_capacity_percent_cycle_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_capacity_thermal_percent_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_conversion_loss_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_cost_to_cycle_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_cycles_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_dispatch_sched_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_power_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_power_target_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Outputs_batt_pv_charge_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_q0_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_q1_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_q2_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_qmax_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_qmaxI_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_qmax_thermal_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_revenue_charge_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_revenue_clipcharge_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_revenue_discharge_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_revenue_gridcharge_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_system_loss_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_temperature_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_to_grid_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_to_load_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_voltage_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_voltage_cell_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_cdf_of_surviving_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_fuelcell_to_batt_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_grid_power_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_grid_power_target_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_grid_to_batt_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_grid_to_load_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_market_sell_rate_series_yr1_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_monthly_batt_to_grid_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_monthly_batt_to_load_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_monthly_grid_to_batt_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_monthly_grid_to_load_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_monthly_pv_to_batt_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_monthly_pv_to_grid_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_monthly_pv_to_load_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_outage_durations_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_pdf_of_surviving_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_pv_to_batt_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_pv_to_grid_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_pv_to_load_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_resilience_hrs_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Outputs_resilience_hrs_avg_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Outputs_resilience_hrs_max_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Outputs_resilience_hrs_min_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_survival_function_aget(SAM_table ptr, int* length, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif