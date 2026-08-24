#ifndef SAM_REACTORTESPOWER_H_
#define SAM_REACTORTESPOWER_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// ReactorTesPower Technology Model
	//

	/** 
	 * Create a ReactorTesPower variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_ReactorTesPower;

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_ReactorTesPower_execute(SAM_table data, int verbosity, SAM_error* err);


	//
	// SolarResource parameters
	//

	/**
	 * Set solar_resource_file: Local weather file path
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: ?
	 */
	SAM_EXPORT void SAM_ReactorTesPower_SolarResource_solar_resource_file_sset(SAM_table ptr, const char* str, SAM_error *err);


	//
	// SystemControl parameters
	//

	/**
	 * Set T_tank_cold_init: Initial cold tank temp [C]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_ReactorTesPower_SystemControl_T_tank_cold_init_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_tank_hot_init: Initial hot tank temp [C]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_ReactorTesPower_SystemControl_T_tank_hot_init_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set bop_par: Balance of plant parasitic power fraction [MWe/MWcap]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_ReactorTesPower_SystemControl_bop_par_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set bop_par_0: Balance of plant parasitic power fraction - const coeff
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_ReactorTesPower_SystemControl_bop_par_0_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set bop_par_1: Balance of plant parasitic power fraction - linear coeff
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_ReactorTesPower_SystemControl_bop_par_1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set bop_par_2: Balance of plant parasitic power fraction - quadratic coeff
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_ReactorTesPower_SystemControl_bop_par_2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set bop_par_f: Balance of plant parasitic power fraction - mult frac
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_ReactorTesPower_SystemControl_bop_par_f_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set can_cycle_use_standby: Can the cycle use standby operation?
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_ReactorTesPower_SystemControl_can_cycle_use_standby_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set f_turb_tou_periods: Dispatch logic for turbine load fraction
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_ReactorTesPower_SystemControl_f_turb_tou_periods_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set is_dispatch: Allow dispatch optimization?
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_ReactorTesPower_SystemControl_is_dispatch_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set is_dispatch_targets: Run solution from user-specified dispatch targets? [-]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_ReactorTesPower_SystemControl_is_dispatch_targets_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set is_pc_sb_allowed_in: User-provided is power cycle standby allowed? [-]
	 * options: None
	 * constraints: None
	 * required if: is_dispatch_targets=1
	 */
	SAM_EXPORT void SAM_ReactorTesPower_SystemControl_is_pc_sb_allowed_in_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set is_pc_su_allowed_in: User-provided is power cycle startup allowed? [-]
	 * options: None
	 * constraints: None
	 * required if: is_dispatch_targets=1
	 */
	SAM_EXPORT void SAM_ReactorTesPower_SystemControl_is_pc_su_allowed_in_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set is_tod_pc_target_also_pc_max: Is the TOD target cycle heat input also the max cycle heat input?
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_ReactorTesPower_SystemControl_is_tod_pc_target_also_pc_max_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set pb_fixed_par: Fixed parasitic load - runs at all times [MWe/MWcap]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_ReactorTesPower_SystemControl_pb_fixed_par_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set pc_op_mode_initial: Initial cycle operation mode 0:startup, 1:on, 2:standby, 3:off, 4:startup_controlled [-]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_ReactorTesPower_SystemControl_pc_op_mode_initial_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set pc_startup_energy_remain_initial: Initial cycle startup energy remaining [kwh]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_ReactorTesPower_SystemControl_pc_startup_energy_remain_initial_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set pc_startup_time_remain_init: Initial cycle startup time remaining [hr]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_ReactorTesPower_SystemControl_pc_startup_time_remain_init_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set q_pc_max_in: User-provided max thermal power to PC [MWt]
	 * options: None
	 * constraints: None
	 * required if: is_dispatch_targets=1
	 */
	SAM_EXPORT void SAM_ReactorTesPower_SystemControl_q_pc_max_in_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set q_pc_target_on_in: User-provided target thermal power to PC [MWt]
	 * options: None
	 * constraints: None
	 * required if: is_dispatch_targets=1
	 */
	SAM_EXPORT void SAM_ReactorTesPower_SystemControl_q_pc_target_on_in_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set q_pc_target_su_in: User-provided target thermal power to PC [MWt]
	 * options: None
	 * constraints: None
	 * required if: is_dispatch_targets=1
	 */
	SAM_EXPORT void SAM_ReactorTesPower_SystemControl_q_pc_target_su_in_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set sim_type: 1 (default): timeseries, 2: design only
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_ReactorTesPower_SystemControl_sim_type_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set time_start: Simulation start time [s]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_ReactorTesPower_SystemControl_time_start_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set time_steps_per_hour: Number of simulation time steps per hour
	 * options: None
	 * constraints: None
	 * required if: ?=-1
	 */
	SAM_EXPORT void SAM_ReactorTesPower_SystemControl_time_steps_per_hour_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set time_stop: Simulation stop time [s]
	 * options: None
	 * constraints: None
	 * required if: ?=31536000
	 */
	SAM_EXPORT void SAM_ReactorTesPower_SystemControl_time_stop_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set timestep_load_fractions: Turbine load fraction for each timestep, alternative to block dispatch
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_ReactorTesPower_SystemControl_timestep_load_fractions_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set vacuum_arrays: Allocate arrays for only the required number of steps
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_ReactorTesPower_SystemControl_vacuum_arrays_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set weekday_schedule: 12x24 CSP operation Time-of-Use Weekday schedule
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_ReactorTesPower_SystemControl_weekday_schedule_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set weekend_schedule: 12x24 CSP operation Time-of-Use Weekend schedule
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_ReactorTesPower_SystemControl_weekend_schedule_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);


	//
	// FinancialModel parameters
	//

	/**
	 * Set reactor_financial_model:  [1-8]
	 * options: None
	 * constraints: INTEGER,MIN=0
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_ReactorTesPower_FinancialModel_reactor_financial_model_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// SystemDesign parameters
	//

	/**
	 * Set P_ref: Reference output electric power at design condition [MW]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_ReactorTesPower_SystemDesign_P_ref_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_htf_cold_des: Cold HTF inlet temperature at design conditions [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_ReactorTesPower_SystemDesign_T_htf_cold_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_htf_hot_des: Hot HTF outlet temperature at design conditions [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_ReactorTesPower_SystemDesign_T_htf_hot_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set design_eff: Power cycle efficiency at design [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_ReactorTesPower_SystemDesign_design_eff_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set reactor_mult: Solar multiple [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_ReactorTesPower_SystemDesign_reactor_mult_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tshours: Equivalent full-load thermal storage hours [hr]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_ReactorTesPower_SystemDesign_tshours_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// TowerAndReceiver parameters
	//

	/**
	 * Set hot_htf_code: Reactor HTF, 17=Salt (60% NaNO3, 40% KNO3) 10=Salt (46.5% LiF 11.5% NaF 42% KF) 50=Lookup tables
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_ReactorTesPower_TowerAndReceiver_hot_htf_code_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ud_hot_htf_props: User defined htf property data [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_ReactorTesPower_TowerAndReceiver_ud_hot_htf_props_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);


	//
	// ThermalStorage parameters
	//

	/**
	 * Set cold_tank_Thtr: Minimum allowable cold tank HTF temperature [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_ReactorTesPower_ThermalStorage_cold_tank_Thtr_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cold_tank_max_heat: Rated heater capacity for cold tank heating [MW]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_ReactorTesPower_ThermalStorage_cold_tank_max_heat_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set h_tank: Total height of tank (height of HTF when tank is full) [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_ReactorTesPower_ThermalStorage_h_tank_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set h_tank_min: Minimum allowable HTF height in storage tank [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_ReactorTesPower_ThermalStorage_h_tank_min_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set hot_tank_Thtr: Minimum allowable hot tank HTF temperature [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_ReactorTesPower_ThermalStorage_hot_tank_Thtr_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set hot_tank_max_heat: Rated heater capacity for hot tank heating [MW]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_ReactorTesPower_ThermalStorage_hot_tank_max_heat_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tank_pairs: Number of equivalent tank pairs
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_ReactorTesPower_ThermalStorage_tank_pairs_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tanks_in_parallel: Tanks are in parallel, not in series, with solar field [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_ReactorTesPower_ThermalStorage_tanks_in_parallel_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tes_init_hot_htf_percent: Initial fraction of available volume that is hot [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_ReactorTesPower_ThermalStorage_tes_init_hot_htf_percent_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set u_tank: Loss coefficient from the tank [W/m2-K]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_ReactorTesPower_ThermalStorage_u_tank_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// PowerCycle parameters
	//

	/**
	 * Set cycle_cutoff_frac: Minimum turbine operation fraction before shutdown
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_ReactorTesPower_PowerCycle_cycle_cutoff_frac_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cycle_max_frac: Maximum turbine over design operation fraction
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_ReactorTesPower_PowerCycle_cycle_max_frac_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set pb_pump_coef: Pumping power to move 1kg of HTF through PB loop [kW/kg]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_ReactorTesPower_PowerCycle_pb_pump_coef_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set pc_config: PC configuration 0=Steam Rankine (224), 1=user defined
	 * options: None
	 * constraints: INTEGER
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_ReactorTesPower_PowerCycle_pc_config_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set q_sby_frac: Fraction of thermal power required for standby
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_ReactorTesPower_PowerCycle_q_sby_frac_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set startup_frac: Fraction of design thermal power needed for startup [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_ReactorTesPower_PowerCycle_startup_frac_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set startup_time: Time needed for power block startup [hr]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_ReactorTesPower_PowerCycle_startup_time_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// RankineCycle parameters
	//

	/**
	 * Set CT: Condenser type: 1=evaporative, 2=air
	 * options: None
	 * constraints: None
	 * required if: pc_config=0
	 */
	SAM_EXPORT void SAM_ReactorTesPower_RankineCycle_CT_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set P_cond_min: Minimum condenser pressure [inHg]
	 * options: None
	 * constraints: None
	 * required if: pc_config=0
	 */
	SAM_EXPORT void SAM_ReactorTesPower_RankineCycle_P_cond_min_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set P_cond_ratio: Condenser pressure ratio
	 * options: None
	 * constraints: None
	 * required if: pc_config=0
	 */
	SAM_EXPORT void SAM_ReactorTesPower_RankineCycle_P_cond_ratio_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_ITD_des: ITD at design for dry system [C]
	 * options: None
	 * constraints: None
	 * required if: pc_config=0
	 */
	SAM_EXPORT void SAM_ReactorTesPower_RankineCycle_T_ITD_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_amb_des: Reference ambient temperature at design point [C]
	 * options: None
	 * constraints: None
	 * required if: pc_config=0
	 */
	SAM_EXPORT void SAM_ReactorTesPower_RankineCycle_T_amb_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_approach: Cooling tower approach temperature [C]
	 * options: None
	 * constraints: None
	 * required if: pc_config=0
	 */
	SAM_EXPORT void SAM_ReactorTesPower_RankineCycle_T_approach_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dT_cw_ref: Reference condenser cooling water inlet/outlet temperature difference [C]
	 * options: None
	 * constraints: None
	 * required if: pc_config=0
	 */
	SAM_EXPORT void SAM_ReactorTesPower_RankineCycle_dT_cw_ref_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set n_pl_inc: Number of part-load increments for the heat rejection system [none]
	 * options: None
	 * constraints: INTEGER
	 * required if: pc_config=0
	 */
	SAM_EXPORT void SAM_ReactorTesPower_RankineCycle_n_pl_inc_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set pb_bd_frac: Power block blowdown steam fraction
	 * options: None
	 * constraints: None
	 * required if: pc_config=0
	 */
	SAM_EXPORT void SAM_ReactorTesPower_RankineCycle_pb_bd_frac_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tech_type: Turbine inlet pressure control 1=Fixed, 3=Sliding
	 * options: None
	 * constraints: None
	 * required if: pc_config=0
	 */
	SAM_EXPORT void SAM_ReactorTesPower_RankineCycle_tech_type_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// UserDefinedPowerCycle parameters
	//

	/**
	 * Set ud_f_W_dot_cool_des: Percent of user-defined power cycle design gross output consumed by cooling [%]
	 * options: None
	 * constraints: None
	 * required if: pc_config=1
	 */
	SAM_EXPORT void SAM_ReactorTesPower_UserDefinedPowerCycle_ud_f_W_dot_cool_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ud_ind_od: Off design user-defined power cycle performance as function of T_htf, m_dot_htf [ND], and T_amb
	 * options: None
	 * constraints: None
	 * required if: pc_config=1
	 */
	SAM_EXPORT void SAM_ReactorTesPower_UserDefinedPowerCycle_ud_ind_od_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ud_is_sco2_regr: 0: (default) simple max htf mass flow correction; 1: sco2 heuristic regression; 2: no correction
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_ReactorTesPower_UserDefinedPowerCycle_ud_is_sco2_regr_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ud_m_dot_water_cool_des: Mass flow rate of water required at user-defined power cycle design point [kg/s]
	 * options: None
	 * constraints: None
	 * required if: pc_config=1
	 */
	SAM_EXPORT void SAM_ReactorTesPower_UserDefinedPowerCycle_ud_m_dot_water_cool_des_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// TimeOfDeliveryFactors parameters
	//

	/**
	 * Set dispatch_factors_ts: Dispatch payment factor array
	 * options: None
	 * constraints: None
	 * required if: ppa_multiplier_model=1&reactor_financial_model<5&is_dispatch=1&sim_type=1
	 */
	SAM_EXPORT void SAM_ReactorTesPower_TimeOfDeliveryFactors_dispatch_factors_ts_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set dispatch_sched_weekday: PPA pricing weekday schedule, 12x24
	 * options: None
	 * constraints: None
	 * required if: ppa_multiplier_model=0&reactor_financial_model<5&is_dispatch=1&sim_type=1
	 */
	SAM_EXPORT void SAM_ReactorTesPower_TimeOfDeliveryFactors_dispatch_sched_weekday_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set dispatch_sched_weekend: PPA pricing weekend schedule, 12x24
	 * options: None
	 * constraints: None
	 * required if: ppa_multiplier_model=0&reactor_financial_model<5&is_dispatch=1&sim_type=1
	 */
	SAM_EXPORT void SAM_ReactorTesPower_TimeOfDeliveryFactors_dispatch_sched_weekend_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set dispatch_tod_factors: TOD factors for periods 1 through 9
	 * options: We added this array input after SAM 2022.12.21 to replace the functionality of former single value inputs dispatch_factor1 through dispatch_factor9
	 * constraints: None
	 * required if: ppa_multiplier_model=0&reactor_financial_model<5&is_dispatch=1&sim_type=1
	 */
	SAM_EXPORT void SAM_ReactorTesPower_TimeOfDeliveryFactors_dispatch_tod_factors_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set is_timestep_load_fractions: Use turbine load fraction for each timestep instead of block dispatch?
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_ReactorTesPower_TimeOfDeliveryFactors_is_timestep_load_fractions_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ppa_multiplier_model: PPA multiplier model 0: dispatch factors dispatch_factorX, 1: hourly multipliers dispatch_factors_ts [0/1]
	 * options: 0=diurnal,1=timestep
	 * constraints: INTEGER,MIN=0
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_ReactorTesPower_TimeOfDeliveryFactors_ppa_multiplier_model_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set start_day_of_year: Start day of year for TOD periods [0..6]
	 * options: 0=Monday, 6=Sunday
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_ReactorTesPower_TimeOfDeliveryFactors_start_day_of_year_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// FinancialSolutionMode parameters
	//

	/**
	 * Set ppa_soln_mode: PPA solution mode (0=Specify IRR target, 1=Specify PPA price)
	 * options: None
	 * constraints: None
	 * required if: sim_type=1&ppa_multiplier_model=0&reactor_financial_model<5&is_dispatch=1&sim_type=1
	 */
	SAM_EXPORT void SAM_ReactorTesPower_FinancialSolutionMode_ppa_soln_mode_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// ElectricityRates parameters
	//

	/**
	 * Set en_electricity_rates: Enable electricity rates for grid purchase [0/1]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_ReactorTesPower_ElectricityRates_en_electricity_rates_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// Revenue parameters
	//

	/**
	 * Set mp_energy_market_revenue: Energy market revenue input
	 * options: Lifetime x 2[Cleared Capacity(MW),Price($/MWh)]
	 * constraints: None
	 * required if: reactor_financial_model=6&is_dispatch=1&sim_type=1
	 */
	SAM_EXPORT void SAM_ReactorTesPower_Revenue_mp_energy_market_revenue_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ppa_price_input: PPA prices - yearly [$/kWh]
	 * options: None
	 * constraints: None
	 * required if: ppa_multiplier_model=0&reactor_financial_model<5&is_dispatch=1&sim_type=1
	 */
	SAM_EXPORT void SAM_ReactorTesPower_Revenue_ppa_price_input_aset(SAM_table ptr, double* arr, int length, SAM_error *err);


	//
	// SystemCosts parameters
	//

	/**
	 * Set bop_spec_cost: BOS specific cost [$/kWe]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_ReactorTesPower_SystemCosts_bop_spec_cost_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set contingency_rate: Contingency for cost overrun [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_ReactorTesPower_SystemCosts_contingency_rate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cycle_spec_cost: Power cycle specific cost [$/kWe]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_ReactorTesPower_SystemCosts_cycle_spec_cost_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set epc_cost_fixed: EPC fixed [$]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_ReactorTesPower_SystemCosts_epc_cost_fixed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set epc_cost_per_watt: EPC cost per watt [$/W]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_ReactorTesPower_SystemCosts_epc_cost_per_watt_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set epc_cost_perc_of_direct: EPC cost percent of direct [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_ReactorTesPower_SystemCosts_epc_cost_perc_of_direct_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set land_cost_fixed: Land fixed [$]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_ReactorTesPower_SystemCosts_land_cost_fixed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set land_cost_per_watt: Land cost per watt [$/W]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_ReactorTesPower_SystemCosts_land_cost_per_watt_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set land_cost_perc_of_direct: Land cost percent of direct [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_ReactorTesPower_SystemCosts_land_cost_perc_of_direct_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set reactor_spec_cost: Reactor specific cost [$/kWt]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_ReactorTesPower_SystemCosts_reactor_spec_cost_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set sales_tax_frac: Percent of cost to which sales tax applies [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_ReactorTesPower_SystemCosts_sales_tax_frac_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tes_spec_cost: Thermal energy storage cost [$/kWht]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_ReactorTesPower_SystemCosts_tes_spec_cost_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// FinancialParameters parameters
	//

	/**
	 * Set const_per_interest_rate1: Interest rate, loan 1 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_ReactorTesPower_FinancialParameters_const_per_interest_rate1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_interest_rate2: Interest rate, loan 2 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_ReactorTesPower_FinancialParameters_const_per_interest_rate2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_interest_rate3: Interest rate, loan 3 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_ReactorTesPower_FinancialParameters_const_per_interest_rate3_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_interest_rate4: Interest rate, loan 4 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_ReactorTesPower_FinancialParameters_const_per_interest_rate4_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_interest_rate5: Interest rate, loan 5 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_ReactorTesPower_FinancialParameters_const_per_interest_rate5_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_months1: Months prior to operation, loan 1
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_ReactorTesPower_FinancialParameters_const_per_months1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_months2: Months prior to operation, loan 2
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_ReactorTesPower_FinancialParameters_const_per_months2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_months3: Months prior to operation, loan 3
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_ReactorTesPower_FinancialParameters_const_per_months3_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_months4: Months prior to operation, loan 4
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_ReactorTesPower_FinancialParameters_const_per_months4_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_months5: Months prior to operation, loan 5
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_ReactorTesPower_FinancialParameters_const_per_months5_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_percent1: Percent of total installed cost, loan 1 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_ReactorTesPower_FinancialParameters_const_per_percent1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_percent2: Percent of total installed cost, loan 2 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_ReactorTesPower_FinancialParameters_const_per_percent2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_percent3: Percent of total installed cost, loan 3 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_ReactorTesPower_FinancialParameters_const_per_percent3_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_percent4: Percent of total installed cost, loan 4 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_ReactorTesPower_FinancialParameters_const_per_percent4_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_percent5: Percent of total installed cost, loan 5 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_ReactorTesPower_FinancialParameters_const_per_percent5_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_upfront_rate1: Upfront fee on principal, loan 1 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_ReactorTesPower_FinancialParameters_const_per_upfront_rate1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_upfront_rate2: Upfront fee on principal, loan 2 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_ReactorTesPower_FinancialParameters_const_per_upfront_rate2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_upfront_rate3: Upfront fee on principal, loan 3 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_ReactorTesPower_FinancialParameters_const_per_upfront_rate3_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_upfront_rate4: Upfront fee on principal, loan 4 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_ReactorTesPower_FinancialParameters_const_per_upfront_rate4_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_upfront_rate5: Upfront fee on principal, loan 5 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_ReactorTesPower_FinancialParameters_const_per_upfront_rate5_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set sales_tax_rate: Sales tax rate [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_ReactorTesPower_FinancialParameters_sales_tax_rate_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// AdjustmentFactors parameters
	//

	/**
	 * Set adjust_constant: Constant loss adjustment [%]
	 * options: 'adjust' and 'constant' separated by _ instead of : after SAM 2022.12.21
	 * constraints: MAX=100
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_ReactorTesPower_AdjustmentFactors_adjust_constant_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set adjust_en_periods: Enable period-based adjustment factors [0/1]
	 * options: 'adjust' and 'en_periods' separated by _ instead of : after SAM 2022.12.21
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_ReactorTesPower_AdjustmentFactors_adjust_en_periods_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set adjust_en_timeindex: Enable lifetime adjustment factors [0/1]
	 * options: 'adjust' and 'en_timeindex' separated by _ instead of : after SAM 2022.12.21
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_ReactorTesPower_AdjustmentFactors_adjust_en_timeindex_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set adjust_periods: Period-based adjustment factors [%]
	 * options: Syntax: n x 3 matrix [ start, end, loss ]; Version upgrade: 'adjust' and 'periods' separated by _ instead of : after SAM 2022.12.21
	 * constraints: COLS=3
	 * required if: adjust_en_periods=1
	 */
	SAM_EXPORT void SAM_ReactorTesPower_AdjustmentFactors_adjust_periods_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set adjust_timeindex: Lifetime adjustment factors [%]
	 * options: 'adjust' and 'timeindex' separated by _ instead of : after SAM 2022.12.21
	 * constraints: None
	 * required if: adjust_en_timeindex=1
	 */
	SAM_EXPORT void SAM_ReactorTesPower_AdjustmentFactors_adjust_timeindex_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set sf_adjust_constant: SF Constant loss adjustment [%]
	 * options: 'sf_adjust' and 'constant' separated by _ instead of : after SAM 2022.12.21
	 * constraints: MAX=100
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_ReactorTesPower_AdjustmentFactors_sf_adjust_constant_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set sf_adjust_en_periods: Enable period-based adjustment factors [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_ReactorTesPower_AdjustmentFactors_sf_adjust_en_periods_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set sf_adjust_en_timeindex: Enable lifetime adjustment factors [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_ReactorTesPower_AdjustmentFactors_sf_adjust_en_timeindex_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set sf_adjust_periods: SF Period-based Adjustment Factors [%]
	 * options: n x 3 matrix [ start, end, loss ]
	 * constraints: COLS=3
	 * required if: sf_adjust_en_periods=1
	 */
	SAM_EXPORT void SAM_ReactorTesPower_AdjustmentFactors_sf_adjust_periods_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set sf_adjust_timeindex: SF Lifetime Adjustment Factors [%]
	 * options: None
	 * constraints: None
	 * required if: sf_adjust_en_timeindex=1
	 */
	SAM_EXPORT void SAM_ReactorTesPower_AdjustmentFactors_sf_adjust_timeindex_aset(SAM_table ptr, double* arr, int length, SAM_error *err);


	/**
	 * SolarResource Getters
	 */

	SAM_EXPORT const char* SAM_ReactorTesPower_SolarResource_solar_resource_file_sget(SAM_table ptr, SAM_error *err);


	/**
	 * SystemControl Getters
	 */

	SAM_EXPORT double SAM_ReactorTesPower_SystemControl_T_tank_cold_init_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_SystemControl_T_tank_hot_init_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_SystemControl_bop_par_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_SystemControl_bop_par_0_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_SystemControl_bop_par_1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_SystemControl_bop_par_2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_SystemControl_bop_par_f_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_SystemControl_can_cycle_use_standby_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_SystemControl_f_turb_tou_periods_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_SystemControl_is_dispatch_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_SystemControl_is_dispatch_targets_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_SystemControl_is_pc_sb_allowed_in_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_SystemControl_is_pc_su_allowed_in_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_SystemControl_is_tod_pc_target_also_pc_max_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_SystemControl_pb_fixed_par_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_SystemControl_pc_op_mode_initial_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_SystemControl_pc_startup_energy_remain_initial_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_SystemControl_pc_startup_time_remain_init_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_SystemControl_q_pc_max_in_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_SystemControl_q_pc_target_on_in_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_SystemControl_q_pc_target_su_in_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_SystemControl_sim_type_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_SystemControl_time_start_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_SystemControl_time_steps_per_hour_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_SystemControl_time_stop_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_SystemControl_timestep_load_fractions_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_SystemControl_vacuum_arrays_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_SystemControl_weekday_schedule_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_SystemControl_weekend_schedule_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);


	/**
	 * FinancialModel Getters
	 */

	SAM_EXPORT double SAM_ReactorTesPower_FinancialModel_reactor_financial_model_nget(SAM_table ptr, SAM_error *err);


	/**
	 * SystemDesign Getters
	 */

	SAM_EXPORT double SAM_ReactorTesPower_SystemDesign_P_ref_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_SystemDesign_T_htf_cold_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_SystemDesign_T_htf_hot_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_SystemDesign_design_eff_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_SystemDesign_reactor_mult_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_SystemDesign_tshours_nget(SAM_table ptr, SAM_error *err);


	/**
	 * TowerAndReceiver Getters
	 */

	SAM_EXPORT double SAM_ReactorTesPower_TowerAndReceiver_hot_htf_code_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_TowerAndReceiver_ud_hot_htf_props_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);


	/**
	 * ThermalStorage Getters
	 */

	SAM_EXPORT double SAM_ReactorTesPower_ThermalStorage_cold_tank_Thtr_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_ThermalStorage_cold_tank_max_heat_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_ThermalStorage_h_tank_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_ThermalStorage_h_tank_min_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_ThermalStorage_hot_tank_Thtr_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_ThermalStorage_hot_tank_max_heat_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_ThermalStorage_tank_pairs_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_ThermalStorage_tanks_in_parallel_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_ThermalStorage_tes_init_hot_htf_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_ThermalStorage_u_tank_nget(SAM_table ptr, SAM_error *err);


	/**
	 * PowerCycle Getters
	 */

	SAM_EXPORT double SAM_ReactorTesPower_PowerCycle_cycle_cutoff_frac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_PowerCycle_cycle_max_frac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_PowerCycle_pb_pump_coef_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_PowerCycle_pc_config_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_PowerCycle_q_sby_frac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_PowerCycle_startup_frac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_PowerCycle_startup_time_nget(SAM_table ptr, SAM_error *err);


	/**
	 * RankineCycle Getters
	 */

	SAM_EXPORT double SAM_ReactorTesPower_RankineCycle_CT_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_RankineCycle_P_cond_min_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_RankineCycle_P_cond_ratio_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_RankineCycle_T_ITD_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_RankineCycle_T_amb_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_RankineCycle_T_approach_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_RankineCycle_dT_cw_ref_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_RankineCycle_n_pl_inc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_RankineCycle_pb_bd_frac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_RankineCycle_tech_type_nget(SAM_table ptr, SAM_error *err);


	/**
	 * UserDefinedPowerCycle Getters
	 */

	SAM_EXPORT double SAM_ReactorTesPower_UserDefinedPowerCycle_ud_f_W_dot_cool_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_UserDefinedPowerCycle_ud_ind_od_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_UserDefinedPowerCycle_ud_is_sco2_regr_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_UserDefinedPowerCycle_ud_m_dot_water_cool_des_nget(SAM_table ptr, SAM_error *err);


	/**
	 * TimeOfDeliveryFactors Getters
	 */

	SAM_EXPORT double* SAM_ReactorTesPower_TimeOfDeliveryFactors_dispatch_factors_ts_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_TimeOfDeliveryFactors_dispatch_sched_weekday_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_TimeOfDeliveryFactors_dispatch_sched_weekend_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_TimeOfDeliveryFactors_dispatch_tod_factors_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_TimeOfDeliveryFactors_is_timestep_load_fractions_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_TimeOfDeliveryFactors_ppa_multiplier_model_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_TimeOfDeliveryFactors_start_day_of_year_nget(SAM_table ptr, SAM_error *err);


	/**
	 * FinancialSolutionMode Getters
	 */

	SAM_EXPORT double SAM_ReactorTesPower_FinancialSolutionMode_ppa_soln_mode_nget(SAM_table ptr, SAM_error *err);


	/**
	 * ElectricityRates Getters
	 */

	SAM_EXPORT double SAM_ReactorTesPower_ElectricityRates_en_electricity_rates_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Revenue Getters
	 */

	SAM_EXPORT double* SAM_ReactorTesPower_Revenue_mp_energy_market_revenue_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Revenue_ppa_price_input_aget(SAM_table ptr, int* length, SAM_error *err);


	/**
	 * SystemCosts Getters
	 */

	SAM_EXPORT double SAM_ReactorTesPower_SystemCosts_bop_spec_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_SystemCosts_contingency_rate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_SystemCosts_cycle_spec_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_SystemCosts_epc_cost_fixed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_SystemCosts_epc_cost_per_watt_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_SystemCosts_epc_cost_perc_of_direct_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_SystemCosts_land_cost_fixed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_SystemCosts_land_cost_per_watt_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_SystemCosts_land_cost_perc_of_direct_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_SystemCosts_reactor_spec_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_SystemCosts_sales_tax_frac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_SystemCosts_tes_spec_cost_nget(SAM_table ptr, SAM_error *err);


	/**
	 * FinancialParameters Getters
	 */

	SAM_EXPORT double SAM_ReactorTesPower_FinancialParameters_const_per_interest_rate1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_FinancialParameters_const_per_interest_rate2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_FinancialParameters_const_per_interest_rate3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_FinancialParameters_const_per_interest_rate4_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_FinancialParameters_const_per_interest_rate5_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_FinancialParameters_const_per_months1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_FinancialParameters_const_per_months2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_FinancialParameters_const_per_months3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_FinancialParameters_const_per_months4_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_FinancialParameters_const_per_months5_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_FinancialParameters_const_per_percent1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_FinancialParameters_const_per_percent2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_FinancialParameters_const_per_percent3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_FinancialParameters_const_per_percent4_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_FinancialParameters_const_per_percent5_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_FinancialParameters_const_per_upfront_rate1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_FinancialParameters_const_per_upfront_rate2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_FinancialParameters_const_per_upfront_rate3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_FinancialParameters_const_per_upfront_rate4_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_FinancialParameters_const_per_upfront_rate5_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_FinancialParameters_sales_tax_rate_nget(SAM_table ptr, SAM_error *err);


	/**
	 * AdjustmentFactors Getters
	 */

	SAM_EXPORT double SAM_ReactorTesPower_AdjustmentFactors_adjust_constant_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_AdjustmentFactors_adjust_en_periods_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_AdjustmentFactors_adjust_en_timeindex_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_AdjustmentFactors_adjust_periods_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_AdjustmentFactors_adjust_timeindex_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_AdjustmentFactors_sf_adjust_constant_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_AdjustmentFactors_sf_adjust_en_periods_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_AdjustmentFactors_sf_adjust_en_timeindex_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_AdjustmentFactors_sf_adjust_periods_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_AdjustmentFactors_sf_adjust_timeindex_aget(SAM_table ptr, int* length, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_P_cond_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_P_cond_iter_err_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_P_cooling_tower_tot_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_P_cycle_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_P_fixed_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_P_out_net_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_P_plant_balance_tot_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_Q_dot_HTF_ND_des_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_Q_tes_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_T_amb_high_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_T_amb_low_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_T_amb_ref_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_T_cond_out_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_T_htf_high_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_T_htf_low_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_T_htf_ref_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_T_pc_in_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_T_pc_out_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_T_tes_cold_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_T_tes_hot_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_V_tes_htf_avail_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_V_tes_htf_total_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_W_dot_bop_design_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_W_dot_cooling_ND_des_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_W_dot_cycle_cooling_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_W_dot_cycle_pump_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_W_dot_fixed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_W_dot_gross_ND_des_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_all_hours_electricity_sales_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_all_hours_revenue_fraction_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_annual_W_cooling_tower_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_annual_W_cycle_gross_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_annual_energy_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_annual_energy_distribution_time_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_annual_fuel_usage_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_annual_total_water_use_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_beam_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_bop_cost_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_capacity_factor_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_capacity_factor_highest_1000_ppas_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_capacity_factor_highest_2000_ppas_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_capacity_factor_warmest_100_Tambs_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_const_per_interest1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_const_per_interest2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_const_per_interest3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_const_per_interest4_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_const_per_interest5_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_const_per_interest_total_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_const_per_percent_total_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_const_per_principal1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_const_per_principal2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_const_per_principal3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_const_per_principal4_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_const_per_principal5_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_const_per_principal_total_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_const_per_total1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_const_per_total2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_const_per_total3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_const_per_total4_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_const_per_total5_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_construction_financing_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_contingency_cost_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_conversion_factor_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_cp_battery_nameplate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_cp_system_nameplate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_cycle_Tdb_table_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_cycle_cost_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_cycle_eff_load_table_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_cycle_htf_pump_power_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_d_tank_tes_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_dens_store_htf_at_T_ave_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_direct_subtotal_cost_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_e_ch_tes_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_epc_cost_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_eta_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_gen_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_hot_hours_electricity_sales_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_hot_hours_revenue_fraction_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_hot_tank_htf_percent_final_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_installed_per_cap_cost_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_is_pc_sb_allowed_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_is_pc_su_allowed_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_kwh_per_kw_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_land_cost_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_m_dot_balance_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_m_dot_cr_to_tes_hot_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_m_dot_cycle_to_field_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_m_dot_field_to_cycle_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_m_dot_htf_ND_high_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_m_dot_htf_ND_low_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_m_dot_htf_ND_ref_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_m_dot_htf_cycle_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_m_dot_pc_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_m_dot_pc_to_tes_cold_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_m_dot_tes_cold_out_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_m_dot_tes_hot_out_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_m_dot_water_ND_des_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_m_dot_water_pc_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_mass_tes_cold_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_mass_tes_hot_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_n_T_amb_pars_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_n_T_htf_pars_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_n_m_dot_pars_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_n_op_modes_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_nameplate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_op_mode_1_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_op_mode_2_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_op_mode_3_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_operating_modes_a_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_operating_modes_b_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_operating_modes_c_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_pc_op_mode_final_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_pc_startup_energy_remain_final_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_pc_startup_time_remain_final_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_pricing_mult_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_q_balance_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_q_ch_tes_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_q_dc_tes_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_q_dot_cycle_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_q_dot_est_tes_ch_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_q_dot_est_tes_dc_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_q_dot_loss_tes_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_q_dot_pc_max_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_q_dot_pc_min_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_q_dot_pc_sb_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_q_dot_pc_startup_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_q_dot_pc_target_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_q_dot_reactor_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_q_heater_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_q_pb_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_q_pc_startup_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_reactor_cost_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_reactor_thermal_power_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_reactor_turn_down_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_rh_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_sales_energy_capacity_factor_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_sales_tax_cost_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_sim_cpu_run_time_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_system_capacity_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_tank_losses_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_tdry_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_tes_cost_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_tes_htf_pump_power_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_time_hr_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_total_direct_cost_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_total_indirect_cost_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_total_installed_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_tou_value_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_ReactorTesPower_Outputs_tshours_reactor_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_ReactorTesPower_Outputs_twet_aget(SAM_table ptr, int* length, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif