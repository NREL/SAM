#ifndef SAM_ETESELECTRICRESISTANCE_H_
#define SAM_ETESELECTRICRESISTANCE_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// EtesElectricResistance Technology Model
	//

	/** 
	 * Create a EtesElectricResistance variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_EtesElectricResistance;

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_EtesElectricResistance_execute(SAM_table data, int verbosity, SAM_error* err);


	//
	// SolarResource parameters
	//

	/**
	 * Set solar_resource_file: Local weather file path
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: ?
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_SolarResource_solar_resource_file_sset(SAM_table ptr, const char* str, SAM_error *err);


	//
	// SystemControl parameters
	//

	/**
	 * Set bop_par: Balance of plant parasitic power fraction [MWe/MWcap]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_SystemControl_bop_par_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set bop_par_0: Balance of plant parasitic power fraction - const coeff
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_SystemControl_bop_par_0_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set bop_par_1: Balance of plant parasitic power fraction - linear coeff
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_SystemControl_bop_par_1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set bop_par_2: Balance of plant parasitic power fraction - quadratic coeff
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_SystemControl_bop_par_2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set bop_par_f: Balance of plant parasitic power fraction - mult frac
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_SystemControl_bop_par_f_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_csu_cost: Cycle startup cost [$/MWe-cycle/start]
	 * options: None
	 * constraints: None
	 * required if: is_dispatch=1
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_SystemControl_disp_csu_cost_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_down_time_min: Minimum time requirement for cycle to not generate power [hr]
	 * options: None
	 * constraints: None
	 * required if: is_dispatch=1
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_SystemControl_disp_down_time_min_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_frequency: Frequency for dispatch optimization calculations [hour]
	 * options: None
	 * constraints: None
	 * required if: is_dispatch=1
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_SystemControl_disp_frequency_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_horizon: Time horizon for dispatch optimization [hour]
	 * options: None
	 * constraints: None
	 * required if: is_dispatch=1
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_SystemControl_disp_horizon_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_hsu_cost: Heater startup cost [$/MWe-cycle/start]
	 * options: None
	 * constraints: None
	 * required if: is_dispatch=1
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_SystemControl_disp_hsu_cost_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_max_iter: Max number of dispatch optimization iterations
	 * options: None
	 * constraints: None
	 * required if: is_dispatch=1
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_SystemControl_disp_max_iter_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_mip_gap: Dispatch optimization solution tolerance
	 * options: None
	 * constraints: None
	 * required if: is_dispatch=1
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_SystemControl_disp_mip_gap_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_pen_delta_w: Dispatch cycle production change penalty [$/MWe-change]
	 * options: None
	 * constraints: None
	 * required if: is_dispatch=1
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_SystemControl_disp_pen_delta_w_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_reporting: Dispatch optimization reporting level
	 * options: None
	 * constraints: None
	 * required if: ?=-1
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_SystemControl_disp_reporting_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_spec_bb: Dispatch optimization B&B heuristic
	 * options: None
	 * constraints: None
	 * required if: ?=-1
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_SystemControl_disp_spec_bb_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_spec_presolve: Dispatch optimization presolve heuristic
	 * options: None
	 * constraints: None
	 * required if: ?=-1
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_SystemControl_disp_spec_presolve_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_spec_scaling: Dispatch optimization scaling heuristic
	 * options: None
	 * constraints: None
	 * required if: ?=-1
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_SystemControl_disp_spec_scaling_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_steps_per_hour: Time steps per hour for dispatch optimization calculations
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_SystemControl_disp_steps_per_hour_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_time_weighting: Dispatch optimization future time discounting factor
	 * options: None
	 * constraints: None
	 * required if: is_dispatch=1
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_SystemControl_disp_time_weighting_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_timeout: Max dispatch optimization solve duration [s]
	 * options: None
	 * constraints: None
	 * required if: is_dispatch=1
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_SystemControl_disp_timeout_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_up_time_min: Minimum time requirement for cycle to generate power [hr]
	 * options: None
	 * constraints: None
	 * required if: is_dispatch=1
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_SystemControl_disp_up_time_min_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set is_dispatch: Allow dispatch optimization?
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_SystemControl_is_dispatch_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set pb_fixed_par: Fixed parasitic load that don't generate heat - runs at all times [MWe/MWcap]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_SystemControl_pb_fixed_par_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set sim_type: 1 (default): timeseries, 2: design only
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_SystemControl_sim_type_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set time_start: Simulation start time [s]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_SystemControl_time_start_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set time_steps_per_hour: Number of simulation time steps per hour
	 * options: None
	 * constraints: None
	 * required if: ?=-1
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_SystemControl_time_steps_per_hour_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set time_stop: Simulation stop time [s]
	 * options: None
	 * constraints: None
	 * required if: ?=31536000
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_SystemControl_time_stop_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set vacuum_arrays: Allocate arrays for only the required number of steps
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_SystemControl_vacuum_arrays_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// FinancialModel parameters
	//

	/**
	 * Set etes_financial_model:  [1-8]
	 * options: None
	 * constraints: INTEGER,MIN=0
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_FinancialModel_etes_financial_model_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// SystemDesign parameters
	//

	/**
	 * Set P_ref: Reference output electric power at design condition [MW]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_SystemDesign_P_ref_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_htf_cold_des: Cold HTF inlet temperature at design conditions [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_SystemDesign_T_htf_cold_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_htf_hot_des: Hot HTF outlet temperature at design conditions [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_SystemDesign_T_htf_hot_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set design_eff: Power cycle efficiency at design [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_SystemDesign_design_eff_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set gross_net_conversion_factor: Estimated gross to net conversion factor
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_SystemDesign_gross_net_conversion_factor_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set heater_mult: Heater multiple relative to design cycle thermal power [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_SystemDesign_heater_mult_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tshours: Equivalent full-load thermal storage hours [hr]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_SystemDesign_tshours_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// PowerCycle parameters
	//

	/**
	 * Set cycle_cutoff_frac: Minimum turbine operation fraction before shutdown
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_PowerCycle_cycle_cutoff_frac_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cycle_max_frac: Maximum turbine over design operation fraction
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_PowerCycle_cycle_max_frac_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set pb_pump_coef: Pumping power to move 1kg of HTF through PB loop [kW/kg]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_PowerCycle_pb_pump_coef_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set pc_config: PC configuration 0=Steam Rankine, 1=user defined
	 * options: None
	 * constraints: INTEGER
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_PowerCycle_pc_config_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set q_sby_frac: Fraction of thermal power required for standby
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_PowerCycle_q_sby_frac_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set startup_frac: Fraction of design thermal power needed for startup [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_PowerCycle_startup_frac_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set startup_time: Time needed for power block startup [hr]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_PowerCycle_startup_time_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// RankineCycle parameters
	//

	/**
	 * Set CT: Condensor type: 1=evaporative, 2=air
	 * options: None
	 * constraints: None
	 * required if: pc_config=0
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_RankineCycle_CT_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set P_boil: Boiler operating pressure [bar]
	 * options: None
	 * constraints: None
	 * required if: pc_config=0
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_RankineCycle_P_boil_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set P_cond_min: Minimum condenser pressure [inHg]
	 * options: None
	 * constraints: None
	 * required if: pc_config=0
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_RankineCycle_P_cond_min_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set P_cond_ratio: Condenser pressure ratio
	 * options: None
	 * constraints: None
	 * required if: pc_config=0
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_RankineCycle_P_cond_ratio_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_ITD_des: ITD at design for dry system [C]
	 * options: None
	 * constraints: None
	 * required if: pc_config=0
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_RankineCycle_T_ITD_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_amb_des: Reference ambient temperature at design point [C]
	 * options: None
	 * constraints: None
	 * required if: pc_config=0
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_RankineCycle_T_amb_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_approach: Cooling tower approach temperature [C]
	 * options: None
	 * constraints: None
	 * required if: pc_config=0
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_RankineCycle_T_approach_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dT_cw_ref: Reference condenser cooling water inlet/outlet temperature difference [C]
	 * options: None
	 * constraints: None
	 * required if: pc_config=0
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_RankineCycle_dT_cw_ref_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set n_pl_inc: Number of part-load increments for the heat rejection system [none]
	 * options: None
	 * constraints: INTEGER
	 * required if: pc_config=0
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_RankineCycle_n_pl_inc_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set pb_bd_frac: Power block blowdown steam fraction
	 * options: None
	 * constraints: None
	 * required if: pc_config=0
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_RankineCycle_pb_bd_frac_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tech_type: Turbine inlet pressure control 1=Fixed, 3=Sliding
	 * options: None
	 * constraints: None
	 * required if: pc_config=0
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_RankineCycle_tech_type_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// UserDefinedPowerCycle parameters
	//

	/**
	 * Set ud_f_W_dot_cool_des: Percent of user-defined power cycle design gross output consumed by cooling [%]
	 * options: None
	 * constraints: None
	 * required if: pc_config=1
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_UserDefinedPowerCycle_ud_f_W_dot_cool_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ud_ind_od: Off design user-defined power cycle performance as function of T_htf, m_dot_htf [ND], and T_amb
	 * options: None
	 * constraints: None
	 * required if: pc_config=1
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_UserDefinedPowerCycle_ud_ind_od_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ud_m_dot_water_cool_des: Mass flow rate of water required at user-defined power cycle design point [kg/s]
	 * options: None
	 * constraints: None
	 * required if: pc_config=1
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_UserDefinedPowerCycle_ud_m_dot_water_cool_des_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// ThermalStorage parameters
	//

	/**
	 * Set cold_tank_Thtr: Minimum allowable cold tank HTF temperature [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_ThermalStorage_cold_tank_Thtr_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cold_tank_max_heat: Rated heater capacity for cold tank heating [MW]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_ThermalStorage_cold_tank_max_heat_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set h_tank: Total height of tank (height of HTF when tank is full) [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_ThermalStorage_h_tank_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set h_tank_min: Minimum allowable HTF height in storage tank [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_ThermalStorage_h_tank_min_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set hot_htf_code: Receiver HTF, 17=Salt (60% NaNO3, 40% KNO3) 10=Salt (46.5% LiF 11.5% NaF 42% KF) 50=Lookup tables
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_ThermalStorage_hot_htf_code_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set hot_tank_Thtr: Minimum allowable hot tank HTF temperature [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_ThermalStorage_hot_tank_Thtr_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set hot_tank_max_heat: Rated heater capacity for hot tank heating [MW]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_ThermalStorage_hot_tank_max_heat_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tank_pairs: Number of equivalent tank pairs
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_ThermalStorage_tank_pairs_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tes_init_hot_htf_percent: Initial fraction of available volume that is hot [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_ThermalStorage_tes_init_hot_htf_percent_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set u_tank: Loss coefficient from the tank [W/m2-K]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_ThermalStorage_u_tank_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ud_hot_htf_props: User-defined TES fluid property data [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_ThermalStorage_ud_hot_htf_props_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);


	//
	// Heater parameters
	//

	/**
	 * Set f_q_dot_des_allowable_su: Fraction of design power allowed during startup [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_Heater_f_q_dot_des_allowable_su_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set f_q_dot_heater_min: Minimum allowable heater output as fraction of design
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_Heater_f_q_dot_heater_min_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set hrs_startup_at_max_rate: Duration of startup at max startup power [hr]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_Heater_hrs_startup_at_max_rate_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// TimeOfDeliveryFactors parameters
	//

	/**
	 * Set dispatch_factor1: Dispatch payment factor 1
	 * options: None
	 * constraints: None
	 * required if: ppa_multiplier_model=0&etes_financial_model<5&is_dispatch=1&sim_type=1
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_TimeOfDeliveryFactors_dispatch_factor1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dispatch_factor2: Dispatch payment factor 2
	 * options: None
	 * constraints: None
	 * required if: ppa_multiplier_model=0&etes_financial_model<5&is_dispatch=1&sim_type=1
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_TimeOfDeliveryFactors_dispatch_factor2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dispatch_factor3: Dispatch payment factor 3
	 * options: None
	 * constraints: None
	 * required if: ppa_multiplier_model=0&etes_financial_model<5&is_dispatch=1&sim_type=1
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_TimeOfDeliveryFactors_dispatch_factor3_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dispatch_factor4: Dispatch payment factor 4
	 * options: None
	 * constraints: None
	 * required if: ppa_multiplier_model=0&etes_financial_model<5&is_dispatch=1&sim_type=1
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_TimeOfDeliveryFactors_dispatch_factor4_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dispatch_factor5: Dispatch payment factor 5
	 * options: None
	 * constraints: None
	 * required if: ppa_multiplier_model=0&etes_financial_model<5&is_dispatch=1&sim_type=1
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_TimeOfDeliveryFactors_dispatch_factor5_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dispatch_factor6: Dispatch payment factor 6
	 * options: None
	 * constraints: None
	 * required if: ppa_multiplier_model=0&etes_financial_model<5&is_dispatch=1&sim_type=1
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_TimeOfDeliveryFactors_dispatch_factor6_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dispatch_factor7: Dispatch payment factor 7
	 * options: None
	 * constraints: None
	 * required if: ppa_multiplier_model=0&etes_financial_model<5&is_dispatch=1&sim_type=1
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_TimeOfDeliveryFactors_dispatch_factor7_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dispatch_factor8: Dispatch payment factor 8
	 * options: None
	 * constraints: None
	 * required if: ppa_multiplier_model=0&etes_financial_model<5&is_dispatch=1&sim_type=1
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_TimeOfDeliveryFactors_dispatch_factor8_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dispatch_factor9: Dispatch payment factor 9
	 * options: None
	 * constraints: None
	 * required if: ppa_multiplier_model=0&etes_financial_model<5&is_dispatch=1&sim_type=1
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_TimeOfDeliveryFactors_dispatch_factor9_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dispatch_factors_ts: Dispatch payment factor timeseries array
	 * options: None
	 * constraints: None
	 * required if: ppa_multiplier_model=1&etes_financial_model<5&is_dispatch=1&sim_type=1
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_TimeOfDeliveryFactors_dispatch_factors_ts_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set dispatch_sched_weekday: PPA pricing weekday schedule, 12x24
	 * options: None
	 * constraints: None
	 * required if: ppa_multiplier_model=0&etes_financial_model<5&is_dispatch=1&sim_type=1
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_TimeOfDeliveryFactors_dispatch_sched_weekday_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set dispatch_sched_weekend: PPA pricing weekend schedule, 12x24
	 * options: None
	 * constraints: None
	 * required if: ppa_multiplier_model=0&etes_financial_model<5&is_dispatch=1&sim_type=1
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_TimeOfDeliveryFactors_dispatch_sched_weekend_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ppa_multiplier_model: PPA multiplier model [0/1]
	 * options: 0=diurnal,1=timestep
	 * constraints: INTEGER,MIN=0
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_TimeOfDeliveryFactors_ppa_multiplier_model_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// Revenue parameters
	//

	/**
	 * Set mp_energy_market_revenue: Energy market revenue input
	 * options: Lifetime x 2[Cleared Capacity(MW),Price($/MWh)]
	 * constraints: None
	 * required if: etes_financial_model=6&is_dispatch=1&sim_type=1
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_Revenue_mp_energy_market_revenue_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ppa_price_input: PPA prices - yearly [$/kWh]
	 * options: None
	 * constraints: None
	 * required if: ppa_multiplier_model=0&etes_financial_model<5&is_dispatch=1&sim_type=1
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_Revenue_ppa_price_input_aset(SAM_table ptr, double* arr, int length, SAM_error *err);


	//
	// SystemCost parameters
	//

	/**
	 * Set cycle_spec_cost: Power cycle specific cost [$/kWe]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_SystemCost_cycle_spec_cost_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// SystemCosts parameters
	//

	/**
	 * Set bop_spec_cost: Balance of plant specific cost [$/kWe]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_SystemCosts_bop_spec_cost_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set contingency_rate: Contingency for cost overrun [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_SystemCosts_contingency_rate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set epc_cost_fixed: EPC fixed [$]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_SystemCosts_epc_cost_fixed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set epc_cost_per_watt: EPC cost per watt [$/W]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_SystemCosts_epc_cost_per_watt_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set epc_cost_perc_of_direct: EPC cost percent of direct [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_SystemCosts_epc_cost_perc_of_direct_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set heater_spec_cost: Heater specific cost [$/kWht]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_SystemCosts_heater_spec_cost_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set land_cost_fixed: Land fixed [$]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_SystemCosts_land_cost_fixed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set land_cost_per_watt: Land cost per watt [$/W]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_SystemCosts_land_cost_per_watt_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set land_cost_perc_of_direct: Land cost percent of direct [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_SystemCosts_land_cost_perc_of_direct_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set sales_tax_frac: Percent of cost to which sales tax applies [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_SystemCosts_sales_tax_frac_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tes_spec_cost: Thermal energy storage specific cost [$/kWht]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_SystemCosts_tes_spec_cost_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// FinancialParameters parameters
	//

	/**
	 * Set const_per_interest_rate1: Interest rate, loan 1 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_FinancialParameters_const_per_interest_rate1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_interest_rate2: Interest rate, loan 2 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_FinancialParameters_const_per_interest_rate2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_interest_rate3: Interest rate, loan 3 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_FinancialParameters_const_per_interest_rate3_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_interest_rate4: Interest rate, loan 4 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_FinancialParameters_const_per_interest_rate4_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_interest_rate5: Interest rate, loan 5 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_FinancialParameters_const_per_interest_rate5_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_months1: Months prior to operation, loan 1
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_FinancialParameters_const_per_months1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_months2: Months prior to operation, loan 2
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_FinancialParameters_const_per_months2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_months3: Months prior to operation, loan 3
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_FinancialParameters_const_per_months3_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_months4: Months prior to operation, loan 4
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_FinancialParameters_const_per_months4_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_months5: Months prior to operation, loan 5
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_FinancialParameters_const_per_months5_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_percent1: Percent of total installed cost, loan 1 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_FinancialParameters_const_per_percent1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_percent2: Percent of total installed cost, loan 2 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_FinancialParameters_const_per_percent2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_percent3: Percent of total installed cost, loan 3 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_FinancialParameters_const_per_percent3_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_percent4: Percent of total installed cost, loan 4 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_FinancialParameters_const_per_percent4_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_percent5: Percent of total installed cost, loan 5 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_FinancialParameters_const_per_percent5_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_upfront_rate1: Upfront fee on principal, loan 1 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_FinancialParameters_const_per_upfront_rate1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_upfront_rate2: Upfront fee on principal, loan 2 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_FinancialParameters_const_per_upfront_rate2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_upfront_rate3: Upfront fee on principal, loan 3 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_FinancialParameters_const_per_upfront_rate3_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_upfront_rate4: Upfront fee on principal, loan 4 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_FinancialParameters_const_per_upfront_rate4_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_upfront_rate5: Upfront fee on principal, loan 5 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_FinancialParameters_const_per_upfront_rate5_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set sales_tax_rate: Sales tax rate [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesElectricResistance_FinancialParameters_sales_tax_rate_nset(SAM_table ptr, double number, SAM_error *err);


	/**
	 * SolarResource Getters
	 */

	SAM_EXPORT const char* SAM_EtesElectricResistance_SolarResource_solar_resource_file_sget(SAM_table ptr, SAM_error *err);


	/**
	 * SystemControl Getters
	 */

	SAM_EXPORT double SAM_EtesElectricResistance_SystemControl_bop_par_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_SystemControl_bop_par_0_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_SystemControl_bop_par_1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_SystemControl_bop_par_2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_SystemControl_bop_par_f_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_SystemControl_disp_csu_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_SystemControl_disp_down_time_min_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_SystemControl_disp_frequency_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_SystemControl_disp_horizon_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_SystemControl_disp_hsu_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_SystemControl_disp_max_iter_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_SystemControl_disp_mip_gap_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_SystemControl_disp_pen_delta_w_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_SystemControl_disp_reporting_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_SystemControl_disp_spec_bb_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_SystemControl_disp_spec_presolve_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_SystemControl_disp_spec_scaling_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_SystemControl_disp_steps_per_hour_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_SystemControl_disp_time_weighting_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_SystemControl_disp_timeout_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_SystemControl_disp_up_time_min_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_SystemControl_is_dispatch_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_SystemControl_pb_fixed_par_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_SystemControl_sim_type_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_SystemControl_time_start_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_SystemControl_time_steps_per_hour_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_SystemControl_time_stop_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_SystemControl_vacuum_arrays_nget(SAM_table ptr, SAM_error *err);


	/**
	 * FinancialModel Getters
	 */

	SAM_EXPORT double SAM_EtesElectricResistance_FinancialModel_etes_financial_model_nget(SAM_table ptr, SAM_error *err);


	/**
	 * SystemDesign Getters
	 */

	SAM_EXPORT double SAM_EtesElectricResistance_SystemDesign_P_ref_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_SystemDesign_T_htf_cold_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_SystemDesign_T_htf_hot_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_SystemDesign_design_eff_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_SystemDesign_gross_net_conversion_factor_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_SystemDesign_heater_mult_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_SystemDesign_tshours_nget(SAM_table ptr, SAM_error *err);


	/**
	 * PowerCycle Getters
	 */

	SAM_EXPORT double SAM_EtesElectricResistance_PowerCycle_cycle_cutoff_frac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_PowerCycle_cycle_max_frac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_PowerCycle_pb_pump_coef_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_PowerCycle_pc_config_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_PowerCycle_q_sby_frac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_PowerCycle_startup_frac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_PowerCycle_startup_time_nget(SAM_table ptr, SAM_error *err);


	/**
	 * RankineCycle Getters
	 */

	SAM_EXPORT double SAM_EtesElectricResistance_RankineCycle_CT_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_RankineCycle_P_boil_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_RankineCycle_P_cond_min_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_RankineCycle_P_cond_ratio_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_RankineCycle_T_ITD_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_RankineCycle_T_amb_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_RankineCycle_T_approach_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_RankineCycle_dT_cw_ref_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_RankineCycle_n_pl_inc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_RankineCycle_pb_bd_frac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_RankineCycle_tech_type_nget(SAM_table ptr, SAM_error *err);


	/**
	 * UserDefinedPowerCycle Getters
	 */

	SAM_EXPORT double SAM_EtesElectricResistance_UserDefinedPowerCycle_ud_f_W_dot_cool_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_EtesElectricResistance_UserDefinedPowerCycle_ud_ind_od_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_UserDefinedPowerCycle_ud_m_dot_water_cool_des_nget(SAM_table ptr, SAM_error *err);


	/**
	 * ThermalStorage Getters
	 */

	SAM_EXPORT double SAM_EtesElectricResistance_ThermalStorage_cold_tank_Thtr_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_ThermalStorage_cold_tank_max_heat_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_ThermalStorage_h_tank_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_ThermalStorage_h_tank_min_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_ThermalStorage_hot_htf_code_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_ThermalStorage_hot_tank_Thtr_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_ThermalStorage_hot_tank_max_heat_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_ThermalStorage_tank_pairs_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_ThermalStorage_tes_init_hot_htf_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_ThermalStorage_u_tank_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_EtesElectricResistance_ThermalStorage_ud_hot_htf_props_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);


	/**
	 * Heater Getters
	 */

	SAM_EXPORT double SAM_EtesElectricResistance_Heater_f_q_dot_des_allowable_su_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_Heater_f_q_dot_heater_min_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_Heater_hrs_startup_at_max_rate_nget(SAM_table ptr, SAM_error *err);


	/**
	 * TimeOfDeliveryFactors Getters
	 */

	SAM_EXPORT double SAM_EtesElectricResistance_TimeOfDeliveryFactors_dispatch_factor1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_TimeOfDeliveryFactors_dispatch_factor2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_TimeOfDeliveryFactors_dispatch_factor3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_TimeOfDeliveryFactors_dispatch_factor4_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_TimeOfDeliveryFactors_dispatch_factor5_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_TimeOfDeliveryFactors_dispatch_factor6_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_TimeOfDeliveryFactors_dispatch_factor7_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_TimeOfDeliveryFactors_dispatch_factor8_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_TimeOfDeliveryFactors_dispatch_factor9_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_EtesElectricResistance_TimeOfDeliveryFactors_dispatch_factors_ts_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesElectricResistance_TimeOfDeliveryFactors_dispatch_sched_weekday_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_EtesElectricResistance_TimeOfDeliveryFactors_dispatch_sched_weekend_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_TimeOfDeliveryFactors_ppa_multiplier_model_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Revenue Getters
	 */

	SAM_EXPORT double* SAM_EtesElectricResistance_Revenue_mp_energy_market_revenue_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_EtesElectricResistance_Revenue_ppa_price_input_aget(SAM_table ptr, int* length, SAM_error *err);


	/**
	 * SystemCost Getters
	 */

	SAM_EXPORT double SAM_EtesElectricResistance_SystemCost_cycle_spec_cost_nget(SAM_table ptr, SAM_error *err);


	/**
	 * SystemCosts Getters
	 */

	SAM_EXPORT double SAM_EtesElectricResistance_SystemCosts_bop_spec_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_SystemCosts_contingency_rate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_SystemCosts_epc_cost_fixed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_SystemCosts_epc_cost_per_watt_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_SystemCosts_epc_cost_perc_of_direct_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_SystemCosts_heater_spec_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_SystemCosts_land_cost_fixed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_SystemCosts_land_cost_per_watt_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_SystemCosts_land_cost_perc_of_direct_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_SystemCosts_sales_tax_frac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_SystemCosts_tes_spec_cost_nget(SAM_table ptr, SAM_error *err);


	/**
	 * FinancialParameters Getters
	 */

	SAM_EXPORT double SAM_EtesElectricResistance_FinancialParameters_const_per_interest_rate1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_FinancialParameters_const_per_interest_rate2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_FinancialParameters_const_per_interest_rate3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_FinancialParameters_const_per_interest_rate4_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_FinancialParameters_const_per_interest_rate5_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_FinancialParameters_const_per_months1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_FinancialParameters_const_per_months2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_FinancialParameters_const_per_months3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_FinancialParameters_const_per_months4_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_FinancialParameters_const_per_months5_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_FinancialParameters_const_per_percent1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_FinancialParameters_const_per_percent2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_FinancialParameters_const_per_percent3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_FinancialParameters_const_per_percent4_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_FinancialParameters_const_per_percent5_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_FinancialParameters_const_per_upfront_rate1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_FinancialParameters_const_per_upfront_rate2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_FinancialParameters_const_per_upfront_rate3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_FinancialParameters_const_per_upfront_rate4_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_FinancialParameters_const_per_upfront_rate5_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_FinancialParameters_sales_tax_rate_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double SAM_EtesElectricResistance_Outputs_E_heater_su_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_Outputs_Q_tes_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_T_htf_cycle_in_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_T_htf_cycle_out_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_T_htf_heater_in_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_T_htf_heater_out_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_T_tes_cold_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_T_tes_hot_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_Outputs_V_tes_htf_avail_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_Outputs_V_tes_htf_total_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_Outputs_W_dot_bop_design_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_W_dot_bop_parasitics_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_W_dot_cycle_cooling_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_W_dot_cycle_gross_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_W_dot_cycle_htf_pump_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_W_dot_cycle_net_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_W_dot_fixed_parasitics_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_W_dot_heater_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_W_dot_out_net_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_Outputs_annual_E_cycle_gross_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_Outputs_annual_E_heater_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_Outputs_annual_E_tes_heater_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_Outputs_annual_Q_cycle_thermal_in_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_Outputs_annual_Q_cycle_thermal_startup_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_Outputs_annual_Q_heater_startup_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_Outputs_annual_Q_heater_to_htf_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_Outputs_annual_Q_tes_losses_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_Outputs_annual_energy_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_Outputs_annual_energy_full_availability_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_Outputs_bop_cost_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_Outputs_construction_financing_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_Outputs_contingency_cost_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_Outputs_cp_htf_cycle_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_Outputs_cycle_cost_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_Outputs_d_tank_tes_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_Outputs_dens_store_htf_at_T_ave_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_Outputs_direct_subtotal_cost_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_Outputs_disp_iter_ann_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_disp_obj_relax_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_disp_objective_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_Outputs_disp_objective_ann_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_disp_pceff_expected_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_disp_presolve_nconstr_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_Outputs_disp_presolve_nconstr_ann_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_disp_presolve_nvar_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_Outputs_disp_presolve_nvar_ann_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_disp_qpbsu_expected_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_disp_qsf_expected_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_disp_qsfprod_expected_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_disp_qsfsu_expected_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_disp_rel_mip_gap_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_disp_rev_expected_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_disp_solve_iter_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_disp_solve_state_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_Outputs_disp_solve_state_ann_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_disp_solve_time_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_Outputs_disp_solve_time_ann_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_disp_subopt_flag_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_disp_tes_expected_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_disp_wpb_expected_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_e_ch_tes_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_elec_purchase_price_mult_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_Outputs_epc_cost_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_eta_cycle_gross_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_eta_cycle_net_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_Outputs_flip_target_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_gen_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_Outputs_heater_cost_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_Outputs_installed_per_cap_cost_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_Outputs_land_cost_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_m_dot_balance_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_m_dot_htf_cycle_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_Outputs_m_dot_htf_cycle_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_m_dot_htf_heater_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_m_dot_water_cycle_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_mass_tes_cold_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_mass_tes_hot_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_n_op_modes_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_Outputs_nameplate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_op_mode_1_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_op_mode_2_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_op_mode_3_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_Outputs_ppa_soln_mode_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_q_balance_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_q_dot_ch_tes_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_q_dot_cycle_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_q_dot_cycle_startup_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_q_dot_dc_tes_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_Outputs_q_dot_heater_design_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_q_dot_heater_startup_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_q_dot_heater_to_htf_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_Outputs_q_dot_loss_tes_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_q_dot_tes_heater_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_q_dot_tes_losses_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_Outputs_q_pb_design_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_Outputs_sales_tax_cost_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_Outputs_system_capacity_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_tdry_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_Outputs_tes_cost_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_time_hr_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_Outputs_total_direct_cost_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_Outputs_total_indirect_cost_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_Outputs_total_installed_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_tou_period_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_EtesElectricResistance_Outputs_tshours_heater_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_twet_aget(SAM_table ptr, int* length, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif