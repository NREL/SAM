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

	SAM_EXPORT SAM_Battery SAM_Battery_construct(const char* def, SAM_error* err);

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_Battery_execute(SAM_Battery data, int verbosity, SAM_error* err);

	SAM_EXPORT void SAM_Battery_destruct(SAM_Battery system);


	//
	// Simulation parameters
	//

	/**
	 * Set analysis_period: Lifetime analysis period [years]
	 * options: The number of years in the simulation
	 * constraints: None
	 * required if: system_use_lifetime_output=1
	 */
	SAM_EXPORT void SAM_Battery_Simulation_analysis_period_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set percent_complete: Estimated simulation status [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Simulation_percent_complete_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set system_use_lifetime_output: Lifetime simulation [0/1]
	 * options: 0=SingleYearRepeated,1=RunEveryYear
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Battery_Simulation_system_use_lifetime_output_nset(SAM_Battery ptr, double number, SAM_error *err);


	//
	// Battery parameters
	//

	/**
	 * Set LeadAcid_q10_computed: Capacity at 10-hour discharge rate [Ah]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Battery_LeadAcid_q10_computed_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set LeadAcid_q20_computed: Capacity at 20-hour discharge rate [Ah]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Battery_LeadAcid_q20_computed_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set LeadAcid_qn_computed: Capacity at discharge rate for n-hour rate [Ah]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Battery_LeadAcid_qn_computed_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set LeadAcid_tn: Time to discharge [h]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Battery_LeadAcid_tn_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set annual_energy: Annual Energy [kWh]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Battery_Battery_annual_energy_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set batt_C_rate: Rate at which voltage vs. capacity curve input
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_C_rate_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set batt_Cp: Battery specific heat capacity [J/KgK]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_Cp_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set batt_Qexp: Cell capacity at end of exponential zone [Ah]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_Qexp_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set batt_Qfull: Fully charged cell capacity [Ah]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_Qfull_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set batt_Qfull_flow: Fully charged flow battery capacity [Ah]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_Qfull_flow_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set batt_Qnom: Cell capacity at end of nominal zone [Ah]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_Qnom_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set batt_Vexp: Cell voltage at end of exponential zone [V]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_Vexp_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set batt_Vfull: Fully charged cell voltage [V]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_Vfull_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set batt_Vnom: Cell voltage at end of nominal zone [V]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_Vnom_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set batt_Vnom_default: Default nominal cell voltage [V]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_Vnom_default_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set batt_ac_dc_efficiency: Inverter AC to battery DC efficiency
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_ac_dc_efficiency_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set batt_ac_or_dc: Battery interconnection (AC or DC)
	 * options: 0=DC_Connected,1=AC_Connected
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_ac_or_dc_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set batt_auto_gridcharge_max_daily: Allowed grid charging percent per day for automated dispatch [kW]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_auto_gridcharge_max_daily_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set batt_calendar_a: Calendar life model coefficient [1/sqrt(day)]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_calendar_a_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set batt_calendar_b: Calendar life model coefficient [K]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_calendar_b_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set batt_calendar_c: Calendar life model coefficient [K]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_calendar_c_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set batt_calendar_choice: Calendar life degradation input option [0/1/2]
	 * options: 0=NoCalendarDegradation,1=LithiomIonModel,2=InputLossTable
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_calendar_choice_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set batt_calendar_lifetime_matrix: Days vs capacity
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_calendar_lifetime_matrix_mset(SAM_Battery ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set batt_calendar_q0: Calendar life model initial capacity cofficient
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_calendar_q0_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set batt_chem: Battery chemistry
	 * options: 0=LeadAcid,1=LiIon
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_chem_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set batt_computed_bank_capacity: Computed bank capacity [kWh]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_computed_bank_capacity_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set batt_computed_series: Number of cells in series
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_computed_series_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set batt_computed_strings: Number of strings of cells
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_computed_strings_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set batt_current_charge_max: Maximum charge current [A]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_current_charge_max_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set batt_current_choice: Limit cells by current or power
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_current_choice_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set batt_current_discharge_max: Maximum discharge current [A]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_current_discharge_max_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set batt_custom_dispatch: Custom battery power for every time step [kW]
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_dispatch_choice=3
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_custom_dispatch_aset(SAM_Battery ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set batt_cycle_cost: Input battery cycle costs [$/cycle-kWh]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_cycle_cost_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set batt_cycle_cost_choice: Use SAM model for cycle costs or input custom [0/1]
	 * options: 0=UseCostModel,1=InputCost
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_cycle_cost_choice_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set batt_dc_ac_efficiency: Battery DC to AC efficiency
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_dc_ac_efficiency_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set batt_dc_dc_efficiency: PV DC to battery DC efficiency
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_dc_dc_efficiency_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set batt_dispatch_auto_can_charge: PV charging allowed for automated dispatch? [kW]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_dispatch_auto_can_charge_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set batt_dispatch_auto_can_clipcharge: Battery can charge from clipped PV for automated dispatch? [kW]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_dispatch_auto_can_clipcharge_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set batt_dispatch_auto_can_fuelcellcharge: Charging from fuel cell allowed for automated dispatch? [kW]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_dispatch_auto_can_fuelcellcharge_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set batt_dispatch_auto_can_gridcharge: Grid charging allowed for automated dispatch? [kW]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_dispatch_auto_can_gridcharge_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set batt_dispatch_choice: Battery dispatch algorithm [0/1/2/3/4]
	 * options: If behind the meter: 0=PeakShavingLookAhead,1=PeakShavingLookBehind,2=InputGridTarget,3=InputBatteryPower,4=ManualDispatch, if front of meter: 0=AutomatedLookAhead,1=AutomatedLookBehind,2=AutomatedInputForecast,3=InputBatteryPower,4=ManualDispatch
	 * constraints: None
	 * required if: en_batt=1
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_dispatch_choice_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set batt_dispatch_update_frequency_hours: Frequency to update the look-ahead dispatch [hours]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_dispatch_update_frequency_hours_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set batt_h_to_ambient: Heat transfer between battery and environment [W/m2K]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_h_to_ambient_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set batt_height: Battery height [m]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_height_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set batt_initial_SOC: Initial state-of-charge [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_initial_SOC_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set batt_inverter_efficiency_cutoff: Inverter efficiency at which to cut battery charge or discharge off [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_inverter_efficiency_cutoff_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set batt_length: Battery length [m]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_length_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set batt_lifetime_matrix: Cycles vs capacity at different depths-of-discharge
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_lifetime_matrix_mset(SAM_Battery ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set batt_look_ahead_hours: Hours to look ahead in automated dispatch [hours]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_look_ahead_hours_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set batt_loss_choice: Loss power input option [0/1]
	 * options: 0=Monthly,1=TimeSeries
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_loss_choice_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set batt_losses: Battery system losses at each timestep [kW]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_losses_aset(SAM_Battery ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set batt_losses_charging: Battery system losses when charging [kW]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_losses_charging_aset(SAM_Battery ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set batt_losses_discharging: Battery system losses when discharging [kW]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_losses_discharging_aset(SAM_Battery ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set batt_losses_idle: Battery system losses when idle [kW]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_losses_idle_aset(SAM_Battery ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set batt_mass: Battery mass [kg]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_mass_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set batt_maximum_SOC: Maximum allowed state-of-charge [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_maximum_SOC_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set batt_meter_position: Position of battery relative to electric meter
	 * options: 0=BehindTheMeter,1=FrontOfMeter
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_meter_position_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set batt_minimum_SOC: Minimum allowed state-of-charge [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_minimum_SOC_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set batt_minimum_modetime: Minimum time at charge state [min]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_minimum_modetime_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set batt_power_charge_max_kwac: Maximum charge power (AC) [kWac]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_power_charge_max_kwac_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set batt_power_charge_max_kwdc: Maximum charge power (DC) [kWdc]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_power_charge_max_kwdc_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set batt_power_discharge_max_kwac: Maximum discharge power (AC) [kWac]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_power_discharge_max_kwac_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set batt_power_discharge_max_kwdc: Maximum discharge power (DC) [kWdc]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_power_discharge_max_kwdc_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set batt_pv_clipping_forecast: PV clipping forecast [kW]
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_meter_position=1&batt_dispatch_choice=2
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_pv_clipping_forecast_aset(SAM_Battery ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set batt_pv_dc_forecast: PV dc power forecast [kW]
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_meter_position=1&batt_dispatch_choice=2
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_pv_dc_forecast_aset(SAM_Battery ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set batt_replacement_capacity: Capacity degradation at which to replace battery [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_replacement_capacity_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set batt_replacement_option: Enable battery replacement? [0=none,1=capacity based,2=user schedule]
	 * options: None
	 * constraints: INTEGER,MIN=0,MAX=2
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_replacement_option_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set batt_replacement_schedule: Battery bank replacements per year (user specified) [number/year]
	 * options: None
	 * constraints: None
	 * required if: batt_replacement_option=2
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_replacement_schedule_aset(SAM_Battery ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set batt_replacement_schedule_percent: Percentage of battery capacity to replace in year [%]
	 * options: None
	 * constraints: None
	 * required if: batt_replacement_option=2
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_replacement_schedule_percent_aset(SAM_Battery ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set batt_resistance: Internal resistance [Ohm]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_resistance_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set batt_room_temperature_celsius: Temperature of storage room [C]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_room_temperature_celsius_aset(SAM_Battery ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set batt_target_choice: Target power input option [0/1]
	 * options: 0=InputMonthlyTarget,1=InputFullTimeSeries
	 * constraints: None
	 * required if: en_batt=1&batt_meter_position=0&batt_dispatch_choice=2
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_target_choice_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set batt_target_power: Grid target power for every time step [kW]
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_meter_position=0&batt_dispatch_choice=2
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_target_power_aset(SAM_Battery ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set batt_target_power_monthly: Grid target power on monthly basis [kW]
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_meter_position=0&batt_dispatch_choice=2
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_target_power_monthly_aset(SAM_Battery ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set batt_voltage_choice: Battery voltage input option [0/1]
	 * options: 0=UseVoltageModel,1=InputVoltageTable
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_voltage_choice_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set batt_voltage_matrix: Battery voltage vs. depth-of-discharge
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_voltage_matrix_mset(SAM_Battery ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set batt_width: Battery width [m]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Battery_batt_width_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set cap_vs_temp: Effective capacity as function of temperature [C,%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Battery_cap_vs_temp_mset(SAM_Battery ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set dispatch_manual_charge: Periods 1-6 charging from system allowed?
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_dispatch_choice=4
	 */
	SAM_EXPORT void SAM_Battery_Battery_dispatch_manual_charge_aset(SAM_Battery ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set dispatch_manual_discharge: Periods 1-6 discharging allowed?
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_dispatch_choice=4
	 */
	SAM_EXPORT void SAM_Battery_Battery_dispatch_manual_discharge_aset(SAM_Battery ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set dispatch_manual_gridcharge: Periods 1-6 grid charging allowed?
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_dispatch_choice=4
	 */
	SAM_EXPORT void SAM_Battery_Battery_dispatch_manual_gridcharge_aset(SAM_Battery ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set dispatch_manual_percent_discharge: Periods 1-6 discharge percent [%]
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_dispatch_choice=4
	 */
	SAM_EXPORT void SAM_Battery_Battery_dispatch_manual_percent_discharge_aset(SAM_Battery ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set dispatch_manual_percent_gridcharge: Periods 1-6 gridcharge percent [%]
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_dispatch_choice=4
	 */
	SAM_EXPORT void SAM_Battery_Battery_dispatch_manual_percent_gridcharge_aset(SAM_Battery ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set dispatch_manual_sched: Battery dispatch schedule for weekday
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_dispatch_choice=4
	 */
	SAM_EXPORT void SAM_Battery_Battery_dispatch_manual_sched_mset(SAM_Battery ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set dispatch_manual_sched_weekend: Battery dispatch schedule for weekend
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_dispatch_choice=4
	 */
	SAM_EXPORT void SAM_Battery_Battery_dispatch_manual_sched_weekend_mset(SAM_Battery ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set en_batt: Enable battery storage model [0/1]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Battery_Battery_en_batt_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set om_replacement_cost1: Cost to replace battery per kWh [$/kWh]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Battery_om_replacement_cost1_aset(SAM_Battery ptr, double* arr, int length, SAM_error *err);


	//
	// System parameters
	//

	/**
	 * Set capacity_factor: Capacity factor [%]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Battery_System_capacity_factor_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set gen: System power generated [kW]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_System_gen_aset(SAM_Battery ptr, double* arr, int length, SAM_error *err);


	//
	// ElectricLoad parameters
	//

	/**
	 * Set load: Electricity load (year 1) [kW]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_ElectricLoad_load_aset(SAM_Battery ptr, double* arr, int length, SAM_error *err);


	//
	// Common parameters
	//

	/**
	 * Set inverter_model: Inverter model specifier
	 * options: 0=cec,1=datasheet,2=partload,3=coefficientgenerator,4=generic
	 * constraints: INTEGER,MIN=0,MAX=4
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Common_inverter_model_nset(SAM_Battery ptr, double number, SAM_error *err);


	//
	// Inverter parameters
	//

	/**
	 * Set inv_cec_cg_eff_cec: Inverter Coefficient Generator CEC Efficiency [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Inverter_inv_cec_cg_eff_cec_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set inv_cec_cg_paco: Inverter Coefficient Generator Max AC Power [Wac]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Inverter_inv_cec_cg_paco_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set inv_ds_eff: Inverter Datasheet Efficiency [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Inverter_inv_ds_eff_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set inv_ds_paco: Inverter Datasheet Maximum AC Power [Wac]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Inverter_inv_ds_paco_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set inv_pd_eff: Inverter Partload Efficiency [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Inverter_inv_pd_eff_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set inv_pd_paco: Inverter Partload Maximum AC Power [Wac]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Inverter_inv_pd_paco_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set inv_snl_eff_cec: Inverter Sandia CEC Efficiency [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Inverter_inv_snl_eff_cec_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set inv_snl_paco: Inverter Sandia Maximum AC Power [Wac]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Inverter_inv_snl_paco_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set inverter_count: Number of inverters
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_Inverter_inverter_count_nset(SAM_Battery ptr, double number, SAM_error *err);


	//
	// PV parameters
	//

	/**
	 * Set dcoptimizer_loss: PV loss in DC/DC w/MPPT conversion
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_PV_dcoptimizer_loss_nset(SAM_Battery ptr, double number, SAM_error *err);


	//
	// FuelCell parameters
	//

	/**
	 * Set dispatch_manual_fuelcellcharge: Periods 1-6 charging from fuel cell allowed?
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_FuelCell_dispatch_manual_fuelcellcharge_aset(SAM_Battery ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set fuelcell_power: Electricity from fuel cell [kW]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_FuelCell_fuelcell_power_aset(SAM_Battery ptr, double* arr, int length, SAM_error *err);


	//
	// ElectricityRate parameters
	//

	/**
	 * Set en_electricity_rates: Enable Electricity Rates [0/1]
	 * options: 0=EnableElectricityRates,1=NoRates
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Battery_ElectricityRate_en_electricity_rates_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set ur_ec_sched_weekday: Energy charge weekday schedule
	 * options: 12 x 24 matrix
	 * constraints: None
	 * required if: en_batt=1&batt_meter_position=1&batt_dispatch_choice=2
	 */
	SAM_EXPORT void SAM_Battery_ElectricityRate_ur_ec_sched_weekday_mset(SAM_Battery ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_ec_sched_weekend: Energy charge weekend schedule
	 * options: 12 x 24 matrix
	 * constraints: None
	 * required if: en_batt=1&batt_meter_position=1&batt_dispatch_choice=2
	 */
	SAM_EXPORT void SAM_Battery_ElectricityRate_ur_ec_sched_weekend_mset(SAM_Battery ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_ec_tou_mat: Energy rates table
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_meter_position=1&batt_dispatch_choice=2
	 */
	SAM_EXPORT void SAM_Battery_ElectricityRate_ur_ec_tou_mat_mset(SAM_Battery ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_en_ts_sell_rate: Enable time step sell rates [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: en_batt=1&batt_meter_position=1&batt_dispatch_choice=2
	 */
	SAM_EXPORT void SAM_Battery_ElectricityRate_ur_en_ts_sell_rate_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set ur_ts_buy_rate: Time step buy rates [0/1]
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_meter_position=1&batt_dispatch_choice=2
	 */
	SAM_EXPORT void SAM_Battery_ElectricityRate_ur_ts_buy_rate_aset(SAM_Battery ptr, double* arr, int length, SAM_error *err);


	//
	// TimeOfDelivery parameters
	//

	/**
	 * Set dispatch_factors_ts: Dispatch payment factor time step
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_meter_position=1&batt_dispatch_choice=2&ppa_multiplier_model=1
	 */
	SAM_EXPORT void SAM_Battery_TimeOfDelivery_dispatch_factors_ts_aset(SAM_Battery ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set dispatch_sched_weekday: Diurnal weekday TOD periods [1..9]
	 * options: 12 x 24 matrix
	 * constraints: None
	 * required if: en_batt=1&batt_meter_position=1&batt_dispatch_choice=2&ppa_multiplier_model=0
	 */
	SAM_EXPORT void SAM_Battery_TimeOfDelivery_dispatch_sched_weekday_mset(SAM_Battery ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set dispatch_sched_weekend: Diurnal weekend TOD periods [1..9]
	 * options: 12 x 24 matrix
	 * constraints: None
	 * required if: en_batt=1&batt_meter_position=1&batt_dispatch_choice=2&ppa_multiplier_model=0
	 */
	SAM_EXPORT void SAM_Battery_TimeOfDelivery_dispatch_sched_weekend_mset(SAM_Battery ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set dispatch_tod_factors: TOD factors for periods 1-9
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_meter_position=1&batt_dispatch_choice=2&ppa_multiplier_model=0
	 */
	SAM_EXPORT void SAM_Battery_TimeOfDelivery_dispatch_tod_factors_aset(SAM_Battery ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set ppa_multiplier_model: PPA multiplier model [0/1]
	 * options: 0=diurnal,1=timestep
	 * constraints: INTEGER,MIN=0
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Battery_TimeOfDelivery_ppa_multiplier_model_nset(SAM_Battery ptr, double number, SAM_error *err);

	/**
	 * Set ppa_price_input: PPA Price Input
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_meter_position=1&batt_dispatch_choice=2
	 */
	SAM_EXPORT void SAM_Battery_TimeOfDelivery_ppa_price_input_nset(SAM_Battery ptr, double number, SAM_error *err);


	/**
	 * Simulation Getters
	 */

	SAM_EXPORT double SAM_Battery_Simulation_analysis_period_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Simulation_percent_complete_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Simulation_system_use_lifetime_output_nget(SAM_Battery ptr, SAM_error *err);


	/**
	 * Battery Getters
	 */

	SAM_EXPORT double SAM_Battery_Battery_LeadAcid_q10_computed_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Battery_LeadAcid_q20_computed_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Battery_LeadAcid_qn_computed_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Battery_LeadAcid_tn_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Battery_annual_energy_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Battery_batt_C_rate_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Battery_batt_Cp_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Battery_batt_Qexp_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Battery_batt_Qfull_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Battery_batt_Qfull_flow_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Battery_batt_Qnom_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Battery_batt_Vexp_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Battery_batt_Vfull_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Battery_batt_Vnom_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Battery_batt_Vnom_default_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Battery_batt_ac_dc_efficiency_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Battery_batt_ac_or_dc_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Battery_batt_auto_gridcharge_max_daily_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Battery_batt_calendar_a_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Battery_batt_calendar_b_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Battery_batt_calendar_c_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Battery_batt_calendar_choice_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Battery_batt_calendar_lifetime_matrix_mget(SAM_Battery ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Battery_batt_calendar_q0_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Battery_batt_chem_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Battery_batt_computed_bank_capacity_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Battery_batt_computed_series_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Battery_batt_computed_strings_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Battery_batt_current_charge_max_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Battery_batt_current_choice_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Battery_batt_current_discharge_max_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Battery_batt_custom_dispatch_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Battery_batt_cycle_cost_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Battery_batt_cycle_cost_choice_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Battery_batt_dc_ac_efficiency_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Battery_batt_dc_dc_efficiency_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Battery_batt_dispatch_auto_can_charge_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Battery_batt_dispatch_auto_can_clipcharge_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Battery_batt_dispatch_auto_can_fuelcellcharge_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Battery_batt_dispatch_auto_can_gridcharge_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Battery_batt_dispatch_choice_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Battery_batt_dispatch_update_frequency_hours_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Battery_batt_h_to_ambient_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Battery_batt_height_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Battery_batt_initial_SOC_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Battery_batt_inverter_efficiency_cutoff_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Battery_batt_length_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Battery_batt_lifetime_matrix_mget(SAM_Battery ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Battery_batt_look_ahead_hours_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Battery_batt_loss_choice_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Battery_batt_losses_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Battery_batt_losses_charging_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Battery_batt_losses_discharging_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Battery_batt_losses_idle_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Battery_batt_mass_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Battery_batt_maximum_SOC_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Battery_batt_meter_position_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Battery_batt_minimum_SOC_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Battery_batt_minimum_modetime_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Battery_batt_power_charge_max_kwac_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Battery_batt_power_charge_max_kwdc_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Battery_batt_power_discharge_max_kwac_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Battery_batt_power_discharge_max_kwdc_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Battery_batt_pv_clipping_forecast_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Battery_batt_pv_dc_forecast_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Battery_batt_replacement_capacity_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Battery_batt_replacement_option_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Battery_batt_replacement_schedule_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Battery_batt_replacement_schedule_percent_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Battery_batt_resistance_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Battery_batt_room_temperature_celsius_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Battery_batt_target_choice_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Battery_batt_target_power_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Battery_batt_target_power_monthly_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Battery_batt_voltage_choice_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Battery_batt_voltage_matrix_mget(SAM_Battery ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Battery_batt_width_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Battery_cap_vs_temp_mget(SAM_Battery ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Battery_dispatch_manual_charge_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Battery_dispatch_manual_discharge_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Battery_dispatch_manual_gridcharge_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Battery_dispatch_manual_percent_discharge_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Battery_dispatch_manual_percent_gridcharge_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Battery_dispatch_manual_sched_mget(SAM_Battery ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Battery_dispatch_manual_sched_weekend_mget(SAM_Battery ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Battery_en_batt_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Battery_om_replacement_cost1_aget(SAM_Battery ptr, int* length, SAM_error *err);


	/**
	 * System Getters
	 */

	SAM_EXPORT double SAM_Battery_System_capacity_factor_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_System_gen_aget(SAM_Battery ptr, int* length, SAM_error *err);


	/**
	 * ElectricLoad Getters
	 */

	SAM_EXPORT double* SAM_Battery_ElectricLoad_load_aget(SAM_Battery ptr, int* length, SAM_error *err);


	/**
	 * Common Getters
	 */

	SAM_EXPORT double SAM_Battery_Common_inverter_model_nget(SAM_Battery ptr, SAM_error *err);


	/**
	 * Inverter Getters
	 */

	SAM_EXPORT double SAM_Battery_Inverter_inv_cec_cg_eff_cec_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Inverter_inv_cec_cg_paco_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Inverter_inv_ds_eff_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Inverter_inv_ds_paco_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Inverter_inv_pd_eff_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Inverter_inv_pd_paco_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Inverter_inv_snl_eff_cec_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Inverter_inv_snl_paco_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Inverter_inverter_count_nget(SAM_Battery ptr, SAM_error *err);


	/**
	 * PV Getters
	 */

	SAM_EXPORT double SAM_Battery_PV_dcoptimizer_loss_nget(SAM_Battery ptr, SAM_error *err);


	/**
	 * FuelCell Getters
	 */

	SAM_EXPORT double* SAM_Battery_FuelCell_dispatch_manual_fuelcellcharge_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_FuelCell_fuelcell_power_aget(SAM_Battery ptr, int* length, SAM_error *err);


	/**
	 * ElectricityRate Getters
	 */

	SAM_EXPORT double SAM_Battery_ElectricityRate_en_electricity_rates_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_ElectricityRate_ur_ec_sched_weekday_mget(SAM_Battery ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_ElectricityRate_ur_ec_sched_weekend_mget(SAM_Battery ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_ElectricityRate_ur_ec_tou_mat_mget(SAM_Battery ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Battery_ElectricityRate_ur_en_ts_sell_rate_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_ElectricityRate_ur_ts_buy_rate_aget(SAM_Battery ptr, int* length, SAM_error *err);


	/**
	 * TimeOfDelivery Getters
	 */

	SAM_EXPORT double* SAM_Battery_TimeOfDelivery_dispatch_factors_ts_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_TimeOfDelivery_dispatch_sched_weekday_mget(SAM_Battery ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_TimeOfDelivery_dispatch_sched_weekend_mget(SAM_Battery ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_TimeOfDelivery_dispatch_tod_factors_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Battery_TimeOfDelivery_ppa_multiplier_model_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_TimeOfDelivery_ppa_price_input_nget(SAM_Battery ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double* SAM_Battery_Outputs_annual_export_to_grid_energy_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_annual_import_to_grid_energy_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Outputs_average_battery_conversion_efficiency_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Outputs_average_battery_roundtrip_efficiency_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_DOD_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_DOD_cycle_average_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_I_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_SOC_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_annual_charge_energy_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_annual_charge_from_grid_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_annual_charge_from_pv_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_annual_discharge_energy_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_annual_energy_loss_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_annual_energy_system_loss_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Outputs_batt_bank_installed_capacity_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_bank_replacement_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_capacity_percent_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_capacity_percent_calendar_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_capacity_percent_cycle_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_capacity_thermal_percent_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_conversion_loss_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_cost_to_cycle_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_cycles_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_dispatch_sched_mget(SAM_Battery ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_power_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_power_target_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Battery_Outputs_batt_pv_charge_percent_nget(SAM_Battery ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_q0_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_q1_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_q2_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_qmax_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_qmaxI_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_qmax_thermal_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_revenue_charge_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_revenue_clipcharge_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_revenue_discharge_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_revenue_gridcharge_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_system_loss_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_temperature_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_to_grid_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_to_load_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_voltage_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_batt_voltage_cell_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_fuelcell_to_batt_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_grid_power_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_grid_power_target_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_grid_to_batt_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_grid_to_load_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_market_sell_rate_series_yr1_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_monthly_batt_to_grid_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_monthly_batt_to_load_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_monthly_grid_to_batt_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_monthly_grid_to_load_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_monthly_pv_to_batt_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_monthly_pv_to_grid_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_monthly_pv_to_load_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_pv_to_batt_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_pv_to_grid_aget(SAM_Battery ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Battery_Outputs_pv_to_load_aget(SAM_Battery ptr, int* length, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif