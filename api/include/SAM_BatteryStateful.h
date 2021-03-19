#ifndef SAM_BATTERYSTATEFUL_H_
#define SAM_BATTERYSTATEFUL_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// BatteryStateful Technology Model
	//

	/** 
	 * Create a BatteryStateful variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_BatteryStateful;

	SAM_EXPORT SAM_BatteryStateful SAM_BatteryStateful_setup(SAM_table data, SAM_error* err);


	//
	// Controls parameters
	//

	/**
	 * Set control_mode: Control using current (0) or power (1) [0/1]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_BatteryStateful_Controls_control_mode_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dt_hr: Time step in hours [hr]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_BatteryStateful_Controls_dt_hr_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set input_current: Current at which to run battery [A]
	 * options: None
	 * constraints: None
	 * required if: control_mode=0
	 */
	SAM_EXPORT void SAM_BatteryStateful_Controls_input_current_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set input_power: Power at which to run battery [kW]
	 * options: None
	 * constraints: None
	 * required if: control_mode=1
	 */
	SAM_EXPORT void SAM_BatteryStateful_Controls_input_power_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// ParamsCell parameters
	//

	/**
	 * Set C_rate: Rate at which voltage vs. capacity curve input
	 * options: None
	 * constraints: None
	 * required if: voltage_choice=0&chem~2
	 */
	SAM_EXPORT void SAM_BatteryStateful_ParamsCell_C_rate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set Qexp: Cell capacity at end of exponential zone [Ah]
	 * options: None
	 * constraints: None
	 * required if: voltage_choice=0&chem~2
	 */
	SAM_EXPORT void SAM_BatteryStateful_ParamsCell_Qexp_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set Qfull: Fully charged cell capacity [Ah]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_BatteryStateful_ParamsCell_Qfull_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set Qfull_flow: Fully charged flow battery capacity [Ah]
	 * options: None
	 * constraints: None
	 * required if: voltage_choice=0&chem=3
	 */
	SAM_EXPORT void SAM_BatteryStateful_ParamsCell_Qfull_flow_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set Qnom: Cell capacity at end of nominal zone [Ah]
	 * options: None
	 * constraints: None
	 * required if: voltage_choice=0&chem~2
	 */
	SAM_EXPORT void SAM_BatteryStateful_ParamsCell_Qnom_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set Vcut: Cell cutoff voltage [V]
	 * options: None
	 * constraints: None
	 * required if: voltage_choice=0&chem~2
	 */
	SAM_EXPORT void SAM_BatteryStateful_ParamsCell_Vcut_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set Vexp: Cell voltage at end of exponential zone [V]
	 * options: None
	 * constraints: None
	 * required if: voltage_choice=0&chem~2
	 */
	SAM_EXPORT void SAM_BatteryStateful_ParamsCell_Vexp_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set Vfull: Fully charged cell voltage [V]
	 * options: None
	 * constraints: None
	 * required if: voltage_choice=0&chem~2
	 */
	SAM_EXPORT void SAM_BatteryStateful_ParamsCell_Vfull_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set Vnom: Cell voltage at end of nominal zone [V]
	 * options: None
	 * constraints: None
	 * required if: voltage_choice=0&chem~2
	 */
	SAM_EXPORT void SAM_BatteryStateful_ParamsCell_Vnom_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set Vnom_default: Default nominal cell voltage [V]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_BatteryStateful_ParamsCell_Vnom_default_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set calendar_a: Calendar life model coefficient [1/sqrt(day)]
	 * options: None
	 * constraints: None
	 * required if: life_model=0&calendar_choice=1
	 */
	SAM_EXPORT void SAM_BatteryStateful_ParamsCell_calendar_a_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set calendar_b: Calendar life model coefficient [K]
	 * options: None
	 * constraints: None
	 * required if: life_model=0&calendar_choice=1
	 */
	SAM_EXPORT void SAM_BatteryStateful_ParamsCell_calendar_b_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set calendar_c: Calendar life model coefficient [K]
	 * options: None
	 * constraints: None
	 * required if: life_model=0&calendar_choice=1
	 */
	SAM_EXPORT void SAM_BatteryStateful_ParamsCell_calendar_c_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set calendar_choice: Calendar life degradation input option [0/1/2]
	 * options: 0=None,1=LithiomIonModel,2=InputLossTable
	 * constraints: None
	 * required if: life_model=0
	 */
	SAM_EXPORT void SAM_BatteryStateful_ParamsCell_calendar_choice_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set calendar_matrix: Table with Day # and Capacity % columns [[[#, %]]]
	 * options: None
	 * constraints: None
	 * required if: life_model=0&calendar_choice=2
	 */
	SAM_EXPORT void SAM_BatteryStateful_ParamsCell_calendar_matrix_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set calendar_q0: Calendar life model initial capacity cofficient
	 * options: None
	 * constraints: None
	 * required if: life_model=0&calendar_choice=1
	 */
	SAM_EXPORT void SAM_BatteryStateful_ParamsCell_calendar_q0_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set chem: Lead Acid (0), Li Ion (1), Vanadium Redox (2), Iron Flow (3) [0/1/2/3]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_BatteryStateful_ParamsCell_chem_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cycling_matrix: Table with DOD %, Cycle #, and Capacity % columns [[[%, #, %]]]
	 * options: None
	 * constraints: None
	 * required if: life_model=0
	 */
	SAM_EXPORT void SAM_BatteryStateful_ParamsCell_cycling_matrix_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set initial_SOC: Initial state-of-charge [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_BatteryStateful_ParamsCell_initial_SOC_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set leadacid_q10: Capacity at 10-hour discharge rate [Ah]
	 * options: None
	 * constraints: None
	 * required if: chem=0
	 */
	SAM_EXPORT void SAM_BatteryStateful_ParamsCell_leadacid_q10_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set leadacid_q20: Capacity at 20-hour discharge rate [Ah]
	 * options: None
	 * constraints: None
	 * required if: chem=0
	 */
	SAM_EXPORT void SAM_BatteryStateful_ParamsCell_leadacid_q20_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set leadacid_qn: Capacity at discharge rate for n-hour rate [Ah]
	 * options: None
	 * constraints: None
	 * required if: chem=0
	 */
	SAM_EXPORT void SAM_BatteryStateful_ParamsCell_leadacid_qn_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set leadacid_tn: Hours to discharge for qn rate [h]
	 * options: None
	 * constraints: None
	 * required if: chem=0
	 */
	SAM_EXPORT void SAM_BatteryStateful_ParamsCell_leadacid_tn_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set life_model: Battery life model specifier [0/1]
	 * options: 0=calendar/cycle,1=NMC
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_BatteryStateful_ParamsCell_life_model_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set maximum_SOC: Maximum allowed state-of-charge [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_BatteryStateful_ParamsCell_maximum_SOC_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set minimum_SOC: Minimum allowed state-of-charge [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_BatteryStateful_ParamsCell_minimum_SOC_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set resistance: Internal resistance [Ohm]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_BatteryStateful_ParamsCell_resistance_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set voltage_choice: Battery voltage input option [0/1]
	 * options: 0=Model,1=Table
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_BatteryStateful_ParamsCell_voltage_choice_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set voltage_matrix: Table with depth-of-discharge % and Voltage as columns [[[%, V]]]
	 * options: None
	 * constraints: None
	 * required if: voltage_choice=1
	 */
	SAM_EXPORT void SAM_BatteryStateful_ParamsCell_voltage_matrix_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);


	//
	// ParamsPack parameters
	//

	/**
	 * Set Cp: Battery specific heat capacity [J/KgK]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_BatteryStateful_ParamsPack_Cp_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_room_init: Temperature of storage room [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_BatteryStateful_ParamsPack_T_room_init_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cap_vs_temp: Table with Temperature and Capacity % as columns [[[C,%]]]
	 * options: None
	 * constraints: None
	 * required if: life_model=0
	 */
	SAM_EXPORT void SAM_BatteryStateful_ParamsPack_cap_vs_temp_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set h: Heat transfer between battery and environment [W/m2K]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_BatteryStateful_ParamsPack_h_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set loss_choice: Loss power input option [0/1]
	 * options: 0=Monthly,1=TimeSeries
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_BatteryStateful_ParamsPack_loss_choice_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mass: Battery mass [kg]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_BatteryStateful_ParamsPack_mass_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set monthly_charge_loss: Battery system losses when charging [[kW]]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_BatteryStateful_ParamsPack_monthly_charge_loss_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set monthly_discharge_loss: Battery system losses when discharging [[kW]]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_BatteryStateful_ParamsPack_monthly_discharge_loss_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set monthly_idle_loss: Battery system losses when idle [[kW]]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_BatteryStateful_ParamsPack_monthly_idle_loss_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set nominal_energy: Nominal installed energy [kWh]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_BatteryStateful_ParamsPack_nominal_energy_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set nominal_voltage: Nominal DC voltage [V]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_BatteryStateful_ParamsPack_nominal_voltage_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set replacement_capacity: Capacity degradation at which to replace battery [%]
	 * options: None
	 * constraints: None
	 * required if: replacement_option=1
	 */
	SAM_EXPORT void SAM_BatteryStateful_ParamsPack_replacement_capacity_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set replacement_option: Replacements: none (0), by capacity (1), or schedule (2) [0=none,1=capacity limit,2=yearly schedule]
	 * options: None
	 * constraints: INTEGER,MIN=0,MAX=2
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_BatteryStateful_ParamsPack_replacement_option_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set replacement_schedule_percent: Percentage of battery capacity to replace in each year [[%/year]]
	 * options: length <= analysis_period
	 * constraints: None
	 * required if: replacement_option=2
	 */
	SAM_EXPORT void SAM_BatteryStateful_ParamsPack_replacement_schedule_percent_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set schedule_loss: Battery system losses at each timestep [[kW]]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_BatteryStateful_ParamsPack_schedule_loss_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set surface_area: Battery surface area [m^2]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_BatteryStateful_ParamsPack_surface_area_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// StatePack parameters
	//

	/**
	 * Set I: Current [A]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_BatteryStateful_StatePack_I_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set I_chargeable: Estimated max chargeable current [A]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_BatteryStateful_StatePack_I_chargeable_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set I_dischargeable: Estimated max dischargeable current [A]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_BatteryStateful_StatePack_I_dischargeable_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set P: Power [kW]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_BatteryStateful_StatePack_P_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set P_chargeable: Estimated max chargeable power  [kW]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_BatteryStateful_StatePack_P_chargeable_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set P_dischargeable: Estimated max dischargeable power [kW]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_BatteryStateful_StatePack_P_dischargeable_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set Q: Capacity [Ah]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_BatteryStateful_StatePack_Q_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set Q_max: Max Capacity [Ah]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_BatteryStateful_StatePack_Q_max_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set SOC: State of Charge [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_BatteryStateful_StatePack_SOC_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_batt: Battery temperature averaged over time step [C]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_BatteryStateful_StatePack_T_batt_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_room: Room temperature [C]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_BatteryStateful_StatePack_T_room_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set V: Voltage [V]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_BatteryStateful_StatePack_V_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set heat_dissipated: Heat dissipated due to flux [kW]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_BatteryStateful_StatePack_heat_dissipated_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set indices_replaced: Lifetime indices of replacement occurrences
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_BatteryStateful_StatePack_indices_replaced_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set last_idx: Last index (lifetime)
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_BatteryStateful_StatePack_last_idx_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set loss_kw: Ancillary power loss (kW DC for DC connected, AC for AC connected) [kW]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_BatteryStateful_StatePack_loss_kw_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set n_replacements: Number of replacements at current year
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_BatteryStateful_StatePack_n_replacements_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// StateCell parameters
	//

	/**
	 * Set I_loss: Lifetime and thermal losses [A]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_BatteryStateful_StateCell_I_loss_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set SOC_prev: State of Charge of last time step [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_BatteryStateful_StateCell_SOC_prev_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_batt_prev: Battery temperature at end of last time step [C]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_BatteryStateful_StateCell_T_batt_prev_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set average_range: Average cycle range [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_BatteryStateful_StateCell_average_range_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cell_current: Cell current [A]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_BatteryStateful_StateCell_cell_current_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cell_voltage: Cell voltage [V]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_BatteryStateful_StateCell_cell_voltage_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set chargeChange: Whether Charge mode changed since last step [0/1]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_BatteryStateful_StateCell_chargeChange_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set charge_mode: Charge (0), Idle (1), Discharge (2) [0/1/2]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_BatteryStateful_StateCell_charge_mode_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set day_age_of_battery: Day age of battery [day]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_BatteryStateful_StateCell_day_age_of_battery_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dq_relative_calendar_old: Change in capacity of last time step [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_BatteryStateful_StateCell_dq_relative_calendar_old_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set n_cycles: Number of cycles
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_BatteryStateful_StateCell_n_cycles_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set prev_charge: Charge mode of last time step [0/1/2]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_BatteryStateful_StateCell_prev_charge_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set q0: Cell capacity at timestep [Ah]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_BatteryStateful_StateCell_q0_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set q1_0: Lead acid - Cell charge available [Ah]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_BatteryStateful_StateCell_q1_0_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set q2: Lead acid - Cell capacity at 10-hr discharge rate [Ah]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_BatteryStateful_StateCell_q2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set q2_0: Lead acid - Cell charge bound [Ah]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_BatteryStateful_StateCell_q2_0_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set q_relative: Overall relative capacity due to lifetime effects [Ah]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_BatteryStateful_StateCell_q_relative_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set q_relative_calendar: Relative capacity due to calendar effects [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_BatteryStateful_StateCell_q_relative_calendar_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set q_relative_cycle: Relative capacity due to cycling effects [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_BatteryStateful_StateCell_q_relative_cycle_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set q_relative_thermal: Relative capacity due to thermal effects [Ah]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_BatteryStateful_StateCell_q_relative_thermal_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set qmax_lifetime: Maximum possible cell capacity [Ah]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_BatteryStateful_StateCell_qmax_lifetime_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set qmax_thermal: Maximum cell capacity adjusted for temperature effects [Ah]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_BatteryStateful_StateCell_qmax_thermal_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set qn: Lead acid - Cell capacity at n-hr discharge rate [Ah]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_BatteryStateful_StateCell_qn_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set rainflow_Xlt: Rainflow range of second to last half cycle [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_BatteryStateful_StateCell_rainflow_Xlt_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set rainflow_Ylt: Rainflow range of last half cycle [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_BatteryStateful_StateCell_rainflow_Ylt_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set rainflow_jlt: Rainflow number of turning points
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_BatteryStateful_StateCell_rainflow_jlt_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set rainflow_peaks: Rainflow peaks of DOD [[%]]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_BatteryStateful_StateCell_rainflow_peaks_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set range: Cycle range [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_BatteryStateful_StateCell_range_nset(SAM_table ptr, double number, SAM_error *err);


	/**
	 * Controls Getters
	 */

	SAM_EXPORT double SAM_BatteryStateful_Controls_control_mode_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_Controls_dt_hr_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_Controls_input_current_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_Controls_input_power_nget(SAM_table ptr, SAM_error *err);


	/**
	 * ParamsCell Getters
	 */

	SAM_EXPORT double SAM_BatteryStateful_ParamsCell_C_rate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_ParamsCell_Qexp_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_ParamsCell_Qfull_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_ParamsCell_Qfull_flow_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_ParamsCell_Qnom_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_ParamsCell_Vcut_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_ParamsCell_Vexp_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_ParamsCell_Vfull_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_ParamsCell_Vnom_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_ParamsCell_Vnom_default_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_ParamsCell_calendar_a_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_ParamsCell_calendar_b_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_ParamsCell_calendar_c_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_ParamsCell_calendar_choice_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_BatteryStateful_ParamsCell_calendar_matrix_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_ParamsCell_calendar_q0_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_ParamsCell_chem_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_BatteryStateful_ParamsCell_cycling_matrix_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_ParamsCell_initial_SOC_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_ParamsCell_leadacid_q10_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_ParamsCell_leadacid_q20_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_ParamsCell_leadacid_qn_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_ParamsCell_leadacid_tn_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_ParamsCell_life_model_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_ParamsCell_maximum_SOC_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_ParamsCell_minimum_SOC_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_ParamsCell_resistance_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_ParamsCell_voltage_choice_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_BatteryStateful_ParamsCell_voltage_matrix_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);


	/**
	 * ParamsPack Getters
	 */

	SAM_EXPORT double SAM_BatteryStateful_ParamsPack_Cp_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_ParamsPack_T_room_init_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_BatteryStateful_ParamsPack_cap_vs_temp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_ParamsPack_h_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_ParamsPack_loss_choice_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_ParamsPack_mass_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_BatteryStateful_ParamsPack_monthly_charge_loss_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_BatteryStateful_ParamsPack_monthly_discharge_loss_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_BatteryStateful_ParamsPack_monthly_idle_loss_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_ParamsPack_nominal_energy_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_ParamsPack_nominal_voltage_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_ParamsPack_replacement_capacity_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_ParamsPack_replacement_option_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_BatteryStateful_ParamsPack_replacement_schedule_percent_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_BatteryStateful_ParamsPack_schedule_loss_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_ParamsPack_surface_area_nget(SAM_table ptr, SAM_error *err);


	/**
	 * StatePack Getters
	 */

	SAM_EXPORT double SAM_BatteryStateful_StatePack_I_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_StatePack_I_chargeable_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_StatePack_I_dischargeable_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_StatePack_P_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_StatePack_P_chargeable_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_StatePack_P_dischargeable_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_StatePack_Q_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_StatePack_Q_max_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_StatePack_SOC_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_StatePack_T_batt_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_StatePack_T_room_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_StatePack_V_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_StatePack_heat_dissipated_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_BatteryStateful_StatePack_indices_replaced_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_StatePack_last_idx_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_StatePack_loss_kw_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_StatePack_n_replacements_nget(SAM_table ptr, SAM_error *err);


	/**
	 * StateCell Getters
	 */

	SAM_EXPORT double SAM_BatteryStateful_StateCell_I_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_StateCell_SOC_prev_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_StateCell_T_batt_prev_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_StateCell_average_range_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_StateCell_cell_current_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_StateCell_cell_voltage_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_StateCell_chargeChange_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_StateCell_charge_mode_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_StateCell_day_age_of_battery_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_StateCell_dq_relative_calendar_old_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_StateCell_n_cycles_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_StateCell_prev_charge_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_StateCell_q0_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_StateCell_q1_0_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_StateCell_q2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_StateCell_q2_0_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_StateCell_q_relative_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_StateCell_q_relative_calendar_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_StateCell_q_relative_cycle_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_StateCell_q_relative_thermal_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_StateCell_qmax_lifetime_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_StateCell_qmax_thermal_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_StateCell_qn_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_StateCell_rainflow_Xlt_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_StateCell_rainflow_Ylt_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_StateCell_rainflow_jlt_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_BatteryStateful_StateCell_rainflow_peaks_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_BatteryStateful_StateCell_range_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif