#ifndef SAM_ETESPTES_H_
#define SAM_ETESPTES_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// EtesPtes Technology Model
	//

	/** 
	 * Create a EtesPtes variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_EtesPtes;

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_EtesPtes_execute(SAM_table data, int verbosity, SAM_error* err);


	//
	// SolarResource parameters
	//

	/**
	 * Set solar_resource_file: Local weather file path
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: ?
	 */
	SAM_EXPORT void SAM_EtesPtes_SolarResource_solar_resource_file_sset(SAM_table ptr, const char* str, SAM_error *err);


	//
	// SystemControl parameters
	//

	/**
	 * Set bop_par: Balance of plant parasitic power fraction [MWe/MWcap]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_SystemControl_bop_par_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set bop_par_0: Balance of plant parasitic power fraction - const coeff
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_SystemControl_bop_par_0_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set bop_par_1: Balance of plant parasitic power fraction - linear coeff
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_SystemControl_bop_par_1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set bop_par_2: Balance of plant parasitic power fraction - quadratic coeff
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_SystemControl_bop_par_2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set bop_par_f: Balance of plant parasitic power fraction - mult frac
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_SystemControl_bop_par_f_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_csu_cost: Cycle startup cost [$/MWe-cycle/start]
	 * options: None
	 * constraints: None
	 * required if: is_dispatch=1
	 */
	SAM_EXPORT void SAM_EtesPtes_SystemControl_disp_csu_cost_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_down_time_min: Minimum time requirement for cycle to not generate power [hr]
	 * options: None
	 * constraints: None
	 * required if: is_dispatch=1
	 */
	SAM_EXPORT void SAM_EtesPtes_SystemControl_disp_down_time_min_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_frequency: Frequency for dispatch optimization calculations [hour]
	 * options: None
	 * constraints: None
	 * required if: is_dispatch=1
	 */
	SAM_EXPORT void SAM_EtesPtes_SystemControl_disp_frequency_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_horizon: Time horizon for dispatch optimization [hour]
	 * options: None
	 * constraints: None
	 * required if: is_dispatch=1
	 */
	SAM_EXPORT void SAM_EtesPtes_SystemControl_disp_horizon_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_hsu_cost: Heater startup cost [$/MWe-cycle/start]
	 * options: None
	 * constraints: None
	 * required if: is_dispatch=1
	 */
	SAM_EXPORT void SAM_EtesPtes_SystemControl_disp_hsu_cost_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_max_iter: Max number of dispatch optimization iterations
	 * options: None
	 * constraints: None
	 * required if: is_dispatch=1
	 */
	SAM_EXPORT void SAM_EtesPtes_SystemControl_disp_max_iter_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_mip_gap: Dispatch optimization solution tolerance
	 * options: None
	 * constraints: None
	 * required if: is_dispatch=1
	 */
	SAM_EXPORT void SAM_EtesPtes_SystemControl_disp_mip_gap_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_pen_delta_w: Dispatch cycle production change penalty [$/MWe-change]
	 * options: None
	 * constraints: None
	 * required if: is_dispatch=1
	 */
	SAM_EXPORT void SAM_EtesPtes_SystemControl_disp_pen_delta_w_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_reporting: Dispatch optimization reporting level
	 * options: None
	 * constraints: None
	 * required if: ?=-1
	 */
	SAM_EXPORT void SAM_EtesPtes_SystemControl_disp_reporting_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_spec_bb: Dispatch optimization B&B heuristic
	 * options: None
	 * constraints: None
	 * required if: ?=-1
	 */
	SAM_EXPORT void SAM_EtesPtes_SystemControl_disp_spec_bb_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_spec_presolve: Dispatch optimization presolve heuristic
	 * options: None
	 * constraints: None
	 * required if: ?=-1
	 */
	SAM_EXPORT void SAM_EtesPtes_SystemControl_disp_spec_presolve_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_spec_scaling: Dispatch optimization scaling heuristic
	 * options: None
	 * constraints: None
	 * required if: ?=-1
	 */
	SAM_EXPORT void SAM_EtesPtes_SystemControl_disp_spec_scaling_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_steps_per_hour: Time steps per hour for dispatch optimization calculations
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_EtesPtes_SystemControl_disp_steps_per_hour_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_time_weighting: Dispatch optimization future time discounting factor
	 * options: None
	 * constraints: None
	 * required if: is_dispatch=1
	 */
	SAM_EXPORT void SAM_EtesPtes_SystemControl_disp_time_weighting_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_timeout: Max dispatch optimization solve duration [s]
	 * options: None
	 * constraints: None
	 * required if: is_dispatch=1
	 */
	SAM_EXPORT void SAM_EtesPtes_SystemControl_disp_timeout_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set disp_up_time_min: Minimum time requirement for cycle to generate power [hr]
	 * options: None
	 * constraints: None
	 * required if: is_dispatch=1
	 */
	SAM_EXPORT void SAM_EtesPtes_SystemControl_disp_up_time_min_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set is_dispatch: Allow dispatch optimization?
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_EtesPtes_SystemControl_is_dispatch_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set pb_fixed_par: Fixed parasitic load that don't generate heat - runs at all times [MWe/MWcap]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_SystemControl_pb_fixed_par_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set sim_type: 1 (default): timeseries, 2: design only
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_EtesPtes_SystemControl_sim_type_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set time_start: Simulation start time [s]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_EtesPtes_SystemControl_time_start_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set time_steps_per_hour: Number of simulation time steps per hour
	 * options: None
	 * constraints: None
	 * required if: ?=-1
	 */
	SAM_EXPORT void SAM_EtesPtes_SystemControl_time_steps_per_hour_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set time_stop: Simulation stop time [s]
	 * options: None
	 * constraints: None
	 * required if: ?=31536000
	 */
	SAM_EXPORT void SAM_EtesPtes_SystemControl_time_stop_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set vacuum_arrays: Allocate arrays for only the required number of steps
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_EtesPtes_SystemControl_vacuum_arrays_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// FinancialModel parameters
	//

	/**
	 * Set etes_financial_model:  [1-8]
	 * options: None
	 * constraints: INTEGER,MIN=0
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_EtesPtes_FinancialModel_etes_financial_model_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// SystemDesign parameters
	//

	/**
	 * Set T_CT_cold_htf_des: CT TES cold temperature [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_SystemDesign_T_CT_cold_htf_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_CT_hot_htf_des: CT TES hot temperature [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_SystemDesign_T_CT_hot_htf_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_HT_cold_htf_des: HT TES cold temperature [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_SystemDesign_T_HT_cold_htf_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_HT_hot_htf_des: HT TES hot temperature [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_SystemDesign_T_HT_hot_htf_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set W_dot_pc_thermo_des: PC design thermodynamic power [MWe]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_SystemDesign_W_dot_pc_thermo_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cop_hp_thermo_des: Heat pump design thermodynamic heat COP [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_SystemDesign_cop_hp_thermo_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set eta_pc_thermo_des: PC design thermodynamic efficiency [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_SystemDesign_eta_pc_thermo_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set f_hp_parasitic_des: Heat pump parasitics as fraction of design thermo power in [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_SystemDesign_f_hp_parasitic_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set f_pc_parasitic_des: PC parasitics as fraction of design thermo power out [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_SystemDesign_f_pc_parasitic_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set heater_mult: Heater multiple relative to design cycle thermal power [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_SystemDesign_heater_mult_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tshours: Equivalent full-load thermal storage hours [hr]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_SystemDesign_tshours_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// ThermalStorage parameters
	//

	/**
	 * Set cold_htf_code: Cold HTF code - see htf_props.h for list
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_ThermalStorage_cold_htf_code_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set hot_htf_code: Hot HTF code - see htf_props.h for list
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_ThermalStorage_hot_htf_code_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ud_cold_htf_props: User-defined Cold HTF fluid property data [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_ThermalStorage_ud_cold_htf_props_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ud_hot_htf_props: User-defined Hot HTF fluid property data [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_ThermalStorage_ud_hot_htf_props_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);


	//
	// Heater parameters
	//

	/**
	 * Set f_q_dot_des_allowable_su: Fraction of design power allowed during startup [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_Heater_f_q_dot_des_allowable_su_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set f_q_dot_heater_min: Minimum allowable heater output as fraction of design
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_Heater_f_q_dot_heater_min_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set hrs_startup_at_max_rate: Duration of startup at max startup power [hr]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_Heater_hrs_startup_at_max_rate_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// PowerCycle parameters
	//

	/**
	 * Set CT_pb_pump_coef: COLD TES pumping power to move 1kg of HTF through PB loop [kW/kg/s]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_PowerCycle_CT_pb_pump_coef_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cycle_cutoff_frac: Minimum turbine operation fraction before shutdown
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_PowerCycle_cycle_cutoff_frac_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cycle_max_frac: Maximum turbine over design operation fraction
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_PowerCycle_cycle_max_frac_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set heat_pump_CT_HTF_pump_coef: Cold temp HX pumping power to move 1 kg/s [kW/kg/s]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_PowerCycle_heat_pump_CT_HTF_pump_coef_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set heat_pump_HT_HTF_pump_coef: High temp HX pumping power to move 1 kg/s [kW/kg/s]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_PowerCycle_heat_pump_HT_HTF_pump_coef_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set pb_pump_coef: COLD TES pumping power to move 1kg of HTF through PB loop [kW/kg/s]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_PowerCycle_pb_pump_coef_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set q_sby_frac: Fraction of thermal power required for standby
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_PowerCycle_q_sby_frac_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set startup_frac: Fraction of design thermal power needed for startup [none]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_PowerCycle_startup_frac_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set startup_time: Time needed for power block startup [hr]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_PowerCycle_startup_time_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// HotThermalStorage parameters
	//

	/**
	 * Set cold_tank_Thtr: HOT TES Minimum allowable cold tank HTF temperature [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_HotThermalStorage_cold_tank_Thtr_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cold_tank_max_heat: HOT TES Rated heater capacity for cold tank heating [MW]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_HotThermalStorage_cold_tank_max_heat_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set h_tank: HOT TES Total height of tank (height of HTF when tank is full) [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_HotThermalStorage_h_tank_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set h_tank_min: HOT TES Minimum allowable HTF height in storage tank [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_HotThermalStorage_h_tank_min_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set hot_tank_Thtr: HOT TES Minimum allowable hot tank HTF temperature [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_HotThermalStorage_hot_tank_Thtr_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set hot_tank_max_heat: HOT TES Rated heater capacity for hot tank heating [MW]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_HotThermalStorage_hot_tank_max_heat_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tank_pairs: HOT TES Number of equivalent tank pairs
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_HotThermalStorage_tank_pairs_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tes_init_hot_htf_percent: HOT TES Initial fraction of available volume that is hot [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_HotThermalStorage_tes_init_hot_htf_percent_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set u_tank: HOT TES Loss coefficient from the tank [W/m2-K]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_HotThermalStorage_u_tank_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// ColdThermalStorage parameters
	//

	/**
	 * Set CT_h_tank: COLD TES Total height of tank (height of HTF when tank is full) [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_ColdThermalStorage_CT_h_tank_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set CT_h_tank_min: COLD TES Minimum allowable HTF height in storage tank [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_ColdThermalStorage_CT_h_tank_min_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set CT_tank_pairs: COLD TES Number of equivalent tank pairs
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_ColdThermalStorage_CT_tank_pairs_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set CT_u_tank: COLD TES Loss coefficient from the tank [W/m2-K]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_ColdThermalStorage_CT_u_tank_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// TimeOfDeliveryFactors parameters
	//

	/**
	 * Set dispatch_factor1: Dispatch payment factor 1
	 * options: None
	 * constraints: None
	 * required if: ppa_multiplier_model=0&etes_financial_model<5&is_dispatch=1&sim_type=1
	 */
	SAM_EXPORT void SAM_EtesPtes_TimeOfDeliveryFactors_dispatch_factor1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dispatch_factor2: Dispatch payment factor 2
	 * options: None
	 * constraints: None
	 * required if: ppa_multiplier_model=0&etes_financial_model<5&is_dispatch=1&sim_type=1
	 */
	SAM_EXPORT void SAM_EtesPtes_TimeOfDeliveryFactors_dispatch_factor2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dispatch_factor3: Dispatch payment factor 3
	 * options: None
	 * constraints: None
	 * required if: ppa_multiplier_model=0&etes_financial_model<5&is_dispatch=1&sim_type=1
	 */
	SAM_EXPORT void SAM_EtesPtes_TimeOfDeliveryFactors_dispatch_factor3_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dispatch_factor4: Dispatch payment factor 4
	 * options: None
	 * constraints: None
	 * required if: ppa_multiplier_model=0&etes_financial_model<5&is_dispatch=1&sim_type=1
	 */
	SAM_EXPORT void SAM_EtesPtes_TimeOfDeliveryFactors_dispatch_factor4_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dispatch_factor5: Dispatch payment factor 5
	 * options: None
	 * constraints: None
	 * required if: ppa_multiplier_model=0&etes_financial_model<5&is_dispatch=1&sim_type=1
	 */
	SAM_EXPORT void SAM_EtesPtes_TimeOfDeliveryFactors_dispatch_factor5_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dispatch_factor6: Dispatch payment factor 6
	 * options: None
	 * constraints: None
	 * required if: ppa_multiplier_model=0&etes_financial_model<5&is_dispatch=1&sim_type=1
	 */
	SAM_EXPORT void SAM_EtesPtes_TimeOfDeliveryFactors_dispatch_factor6_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dispatch_factor7: Dispatch payment factor 7
	 * options: None
	 * constraints: None
	 * required if: ppa_multiplier_model=0&etes_financial_model<5&is_dispatch=1&sim_type=1
	 */
	SAM_EXPORT void SAM_EtesPtes_TimeOfDeliveryFactors_dispatch_factor7_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dispatch_factor8: Dispatch payment factor 8
	 * options: None
	 * constraints: None
	 * required if: ppa_multiplier_model=0&etes_financial_model<5&is_dispatch=1&sim_type=1
	 */
	SAM_EXPORT void SAM_EtesPtes_TimeOfDeliveryFactors_dispatch_factor8_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dispatch_factor9: Dispatch payment factor 9
	 * options: None
	 * constraints: None
	 * required if: ppa_multiplier_model=0&etes_financial_model<5&is_dispatch=1&sim_type=1
	 */
	SAM_EXPORT void SAM_EtesPtes_TimeOfDeliveryFactors_dispatch_factor9_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dispatch_factors_ts: Dispatch payment factor timeseries array
	 * options: None
	 * constraints: None
	 * required if: ppa_multiplier_model=1&etes_financial_model<5&is_dispatch=1&sim_type=1
	 */
	SAM_EXPORT void SAM_EtesPtes_TimeOfDeliveryFactors_dispatch_factors_ts_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set dispatch_sched_weekday: PPA pricing weekday schedule, 12x24
	 * options: None
	 * constraints: None
	 * required if: ppa_multiplier_model=0&etes_financial_model<5&is_dispatch=1&sim_type=1
	 */
	SAM_EXPORT void SAM_EtesPtes_TimeOfDeliveryFactors_dispatch_sched_weekday_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set dispatch_sched_weekend: PPA pricing weekend schedule, 12x24
	 * options: None
	 * constraints: None
	 * required if: ppa_multiplier_model=0&etes_financial_model<5&is_dispatch=1&sim_type=1
	 */
	SAM_EXPORT void SAM_EtesPtes_TimeOfDeliveryFactors_dispatch_sched_weekend_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ppa_multiplier_model: PPA multiplier model [0/1]
	 * options: 0=diurnal,1=timestep
	 * constraints: INTEGER,MIN=0
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_EtesPtes_TimeOfDeliveryFactors_ppa_multiplier_model_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// Revenue parameters
	//

	/**
	 * Set ppa_price_input: PPA prices - yearly [$/kWh]
	 * options: None
	 * constraints: None
	 * required if: ppa_multiplier_model=0&etes_financial_model<5&is_dispatch=1&sim_type=1
	 */
	SAM_EXPORT void SAM_EtesPtes_Revenue_ppa_price_input_aset(SAM_table ptr, double* arr, int length, SAM_error *err);


	//
	// SystemCosts parameters
	//

	/**
	 * Set CT_tes_spec_cost: Cold Temp thermal energy storage specific cost [$/kWht]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_SystemCosts_CT_tes_spec_cost_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set bop_spec_cost: Balance of plant specific cost [$/kWe]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_SystemCosts_bop_spec_cost_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set contingency_rate: Contingency for cost overrun [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_SystemCosts_contingency_rate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cycle_spec_cost: Power cycle specific cost [$/kWe]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_SystemCosts_cycle_spec_cost_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set epc_cost_fixed: EPC fixed [$]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_SystemCosts_epc_cost_fixed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set epc_cost_per_watt: EPC cost per watt [$/W]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_SystemCosts_epc_cost_per_watt_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set epc_cost_perc_of_direct: EPC cost percent of direct [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_SystemCosts_epc_cost_perc_of_direct_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set heat_pump_spec_cost: Heater pump specific cost [$/kWht]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_SystemCosts_heat_pump_spec_cost_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set land_cost_fixed: Land fixed [$]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_SystemCosts_land_cost_fixed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set land_cost_per_watt: Land cost per watt [$/W]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_SystemCosts_land_cost_per_watt_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set land_cost_perc_of_direct: Land cost percent of direct [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_SystemCosts_land_cost_perc_of_direct_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set sales_tax_frac: Percent of cost to which sales tax applies [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_SystemCosts_sales_tax_frac_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tes_spec_cost: Hot Temp thermal energy storage specific cost [$/kWht]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_SystemCosts_tes_spec_cost_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// FinancialParameters parameters
	//

	/**
	 * Set const_per_interest_rate1: Interest rate, loan 1 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_FinancialParameters_const_per_interest_rate1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_interest_rate2: Interest rate, loan 2 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_FinancialParameters_const_per_interest_rate2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_interest_rate3: Interest rate, loan 3 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_FinancialParameters_const_per_interest_rate3_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_interest_rate4: Interest rate, loan 4 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_FinancialParameters_const_per_interest_rate4_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_interest_rate5: Interest rate, loan 5 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_FinancialParameters_const_per_interest_rate5_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_months1: Months prior to operation, loan 1
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_FinancialParameters_const_per_months1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_months2: Months prior to operation, loan 2
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_FinancialParameters_const_per_months2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_months3: Months prior to operation, loan 3
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_FinancialParameters_const_per_months3_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_months4: Months prior to operation, loan 4
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_FinancialParameters_const_per_months4_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_months5: Months prior to operation, loan 5
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_FinancialParameters_const_per_months5_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_percent1: Percent of total installed cost, loan 1 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_FinancialParameters_const_per_percent1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_percent2: Percent of total installed cost, loan 2 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_FinancialParameters_const_per_percent2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_percent3: Percent of total installed cost, loan 3 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_FinancialParameters_const_per_percent3_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_percent4: Percent of total installed cost, loan 4 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_FinancialParameters_const_per_percent4_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_percent5: Percent of total installed cost, loan 5 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_FinancialParameters_const_per_percent5_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_upfront_rate1: Upfront fee on principal, loan 1 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_FinancialParameters_const_per_upfront_rate1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_upfront_rate2: Upfront fee on principal, loan 2 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_FinancialParameters_const_per_upfront_rate2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_upfront_rate3: Upfront fee on principal, loan 3 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_FinancialParameters_const_per_upfront_rate3_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_upfront_rate4: Upfront fee on principal, loan 4 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_FinancialParameters_const_per_upfront_rate4_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set const_per_upfront_rate5: Upfront fee on principal, loan 5 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_FinancialParameters_const_per_upfront_rate5_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set sales_tax_rate: Sales tax rate [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_EtesPtes_FinancialParameters_sales_tax_rate_nset(SAM_table ptr, double number, SAM_error *err);


	/**
	 * SolarResource Getters
	 */

	SAM_EXPORT const char* SAM_EtesPtes_SolarResource_solar_resource_file_sget(SAM_table ptr, SAM_error *err);


	/**
	 * SystemControl Getters
	 */

	SAM_EXPORT double SAM_EtesPtes_SystemControl_bop_par_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_SystemControl_bop_par_0_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_SystemControl_bop_par_1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_SystemControl_bop_par_2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_SystemControl_bop_par_f_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_SystemControl_disp_csu_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_SystemControl_disp_down_time_min_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_SystemControl_disp_frequency_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_SystemControl_disp_horizon_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_SystemControl_disp_hsu_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_SystemControl_disp_max_iter_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_SystemControl_disp_mip_gap_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_SystemControl_disp_pen_delta_w_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_SystemControl_disp_reporting_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_SystemControl_disp_spec_bb_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_SystemControl_disp_spec_presolve_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_SystemControl_disp_spec_scaling_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_SystemControl_disp_steps_per_hour_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_SystemControl_disp_time_weighting_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_SystemControl_disp_timeout_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_SystemControl_disp_up_time_min_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_SystemControl_is_dispatch_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_SystemControl_pb_fixed_par_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_SystemControl_sim_type_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_SystemControl_time_start_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_SystemControl_time_steps_per_hour_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_SystemControl_time_stop_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_SystemControl_vacuum_arrays_nget(SAM_table ptr, SAM_error *err);


	/**
	 * FinancialModel Getters
	 */

	SAM_EXPORT double SAM_EtesPtes_FinancialModel_etes_financial_model_nget(SAM_table ptr, SAM_error *err);


	/**
	 * SystemDesign Getters
	 */

	SAM_EXPORT double SAM_EtesPtes_SystemDesign_T_CT_cold_htf_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_SystemDesign_T_CT_hot_htf_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_SystemDesign_T_HT_cold_htf_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_SystemDesign_T_HT_hot_htf_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_SystemDesign_W_dot_pc_thermo_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_SystemDesign_cop_hp_thermo_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_SystemDesign_eta_pc_thermo_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_SystemDesign_f_hp_parasitic_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_SystemDesign_f_pc_parasitic_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_SystemDesign_heater_mult_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_SystemDesign_tshours_nget(SAM_table ptr, SAM_error *err);


	/**
	 * ThermalStorage Getters
	 */

	SAM_EXPORT double SAM_EtesPtes_ThermalStorage_cold_htf_code_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_ThermalStorage_hot_htf_code_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_ThermalStorage_ud_cold_htf_props_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_ThermalStorage_ud_hot_htf_props_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);


	/**
	 * Heater Getters
	 */

	SAM_EXPORT double SAM_EtesPtes_Heater_f_q_dot_des_allowable_su_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_Heater_f_q_dot_heater_min_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_Heater_hrs_startup_at_max_rate_nget(SAM_table ptr, SAM_error *err);


	/**
	 * PowerCycle Getters
	 */

	SAM_EXPORT double SAM_EtesPtes_PowerCycle_CT_pb_pump_coef_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_PowerCycle_cycle_cutoff_frac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_PowerCycle_cycle_max_frac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_PowerCycle_heat_pump_CT_HTF_pump_coef_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_PowerCycle_heat_pump_HT_HTF_pump_coef_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_PowerCycle_pb_pump_coef_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_PowerCycle_q_sby_frac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_PowerCycle_startup_frac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_PowerCycle_startup_time_nget(SAM_table ptr, SAM_error *err);


	/**
	 * HotThermalStorage Getters
	 */

	SAM_EXPORT double SAM_EtesPtes_HotThermalStorage_cold_tank_Thtr_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_HotThermalStorage_cold_tank_max_heat_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_HotThermalStorage_h_tank_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_HotThermalStorage_h_tank_min_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_HotThermalStorage_hot_tank_Thtr_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_HotThermalStorage_hot_tank_max_heat_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_HotThermalStorage_tank_pairs_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_HotThermalStorage_tes_init_hot_htf_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_HotThermalStorage_u_tank_nget(SAM_table ptr, SAM_error *err);


	/**
	 * ColdThermalStorage Getters
	 */

	SAM_EXPORT double SAM_EtesPtes_ColdThermalStorage_CT_h_tank_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_ColdThermalStorage_CT_h_tank_min_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_ColdThermalStorage_CT_tank_pairs_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_ColdThermalStorage_CT_u_tank_nget(SAM_table ptr, SAM_error *err);


	/**
	 * TimeOfDeliveryFactors Getters
	 */

	SAM_EXPORT double SAM_EtesPtes_TimeOfDeliveryFactors_dispatch_factor1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_TimeOfDeliveryFactors_dispatch_factor2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_TimeOfDeliveryFactors_dispatch_factor3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_TimeOfDeliveryFactors_dispatch_factor4_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_TimeOfDeliveryFactors_dispatch_factor5_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_TimeOfDeliveryFactors_dispatch_factor6_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_TimeOfDeliveryFactors_dispatch_factor7_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_TimeOfDeliveryFactors_dispatch_factor8_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_TimeOfDeliveryFactors_dispatch_factor9_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_TimeOfDeliveryFactors_dispatch_factors_ts_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_TimeOfDeliveryFactors_dispatch_sched_weekday_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_TimeOfDeliveryFactors_dispatch_sched_weekend_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_TimeOfDeliveryFactors_ppa_multiplier_model_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Revenue Getters
	 */

	SAM_EXPORT double* SAM_EtesPtes_Revenue_ppa_price_input_aget(SAM_table ptr, int* length, SAM_error *err);


	/**
	 * SystemCosts Getters
	 */

	SAM_EXPORT double SAM_EtesPtes_SystemCosts_CT_tes_spec_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_SystemCosts_bop_spec_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_SystemCosts_contingency_rate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_SystemCosts_cycle_spec_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_SystemCosts_epc_cost_fixed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_SystemCosts_epc_cost_per_watt_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_SystemCosts_epc_cost_perc_of_direct_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_SystemCosts_heat_pump_spec_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_SystemCosts_land_cost_fixed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_SystemCosts_land_cost_per_watt_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_SystemCosts_land_cost_perc_of_direct_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_SystemCosts_sales_tax_frac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_SystemCosts_tes_spec_cost_nget(SAM_table ptr, SAM_error *err);


	/**
	 * FinancialParameters Getters
	 */

	SAM_EXPORT double SAM_EtesPtes_FinancialParameters_const_per_interest_rate1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_FinancialParameters_const_per_interest_rate2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_FinancialParameters_const_per_interest_rate3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_FinancialParameters_const_per_interest_rate4_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_FinancialParameters_const_per_interest_rate5_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_FinancialParameters_const_per_months1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_FinancialParameters_const_per_months2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_FinancialParameters_const_per_months3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_FinancialParameters_const_per_months4_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_FinancialParameters_const_per_months5_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_FinancialParameters_const_per_percent1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_FinancialParameters_const_per_percent2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_FinancialParameters_const_per_percent3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_FinancialParameters_const_per_percent4_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_FinancialParameters_const_per_percent5_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_FinancialParameters_const_per_upfront_rate1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_FinancialParameters_const_per_upfront_rate2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_FinancialParameters_const_per_upfront_rate3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_FinancialParameters_const_per_upfront_rate4_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_FinancialParameters_const_per_upfront_rate5_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_FinancialParameters_sales_tax_rate_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double SAM_EtesPtes_Outputs_COP_net_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_Outputs_CT_tes_cost_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_Outputs_E_hp_su_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_Outputs_Q_CT_tes_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_Outputs_Q_tes_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_Outputs_T_CT_tes_cold_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_Outputs_T_CT_tes_hot_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_Outputs_T_hp_CT_htf_cold_out_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_Outputs_T_hp_CT_htf_hot_in_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_Outputs_T_hp_HT_htf_cold_in_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_Outputs_T_hp_HT_htf_hot_out_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_Outputs_T_pc_CT_htf_cold_in_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_Outputs_T_pc_CT_htf_hot_out_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_Outputs_T_pc_HT_htf_cold_out_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_Outputs_T_pc_HT_htf_hot_in_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_Outputs_T_tes_cold_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_Outputs_T_tes_hot_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_Outputs_V_CT_tes_htf_avail_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_Outputs_V_CT_tes_htf_total_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_Outputs_V_tes_htf_avail_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_Outputs_V_tes_htf_total_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_Outputs_W_dot_bop_design_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_Outputs_W_dot_bop_parasitics_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_Outputs_W_dot_fixed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_Outputs_W_dot_fixed_parasitics_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_Outputs_W_dot_hp_CT_htf_pump_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_Outputs_W_dot_hp_CT_htf_pump_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_Outputs_W_dot_hp_HT_htf_pump_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_Outputs_W_dot_hp_HT_htf_pump_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_Outputs_W_dot_hp_elec_parasitic_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_Outputs_W_dot_hp_in_net_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_Outputs_W_dot_hp_in_thermo_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_Outputs_W_dot_hp_net_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_Outputs_W_dot_hp_parasitics_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_Outputs_W_dot_hp_thermo_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_Outputs_W_dot_out_net_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_Outputs_W_dot_pc_CT_htf_pump_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_Outputs_W_dot_pc_CT_htf_pump_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_Outputs_W_dot_pc_HT_htf_pump_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_Outputs_W_dot_pc_HT_htf_pump_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_Outputs_W_dot_pc_elec_parasitic_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_Outputs_W_dot_pc_net_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_Outputs_W_dot_pc_parasitics_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_Outputs_W_dot_pc_thermo_out_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_Outputs_annual_energy_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_Outputs_bop_cost_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_Outputs_charge_capacity_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_Outputs_construction_financing_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_Outputs_contingency_cost_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_Outputs_cop_hot_hp_thermo_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_Outputs_cp_battery_capacity_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_Outputs_cp_system_capacity_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_Outputs_cycle_cost_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_Outputs_d_CT_tank_tes_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_Outputs_d_tank_tes_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_Outputs_direct_subtotal_cost_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_Outputs_e_ch_tes_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_Outputs_elec_purchase_price_mult_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_Outputs_epc_cost_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_Outputs_eta_pc_net_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_Outputs_eta_pc_thermo_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_Outputs_flip_target_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_Outputs_gen_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_Outputs_heater_cost_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_Outputs_installed_per_cap_cost_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_Outputs_land_cost_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_Outputs_m_dot_balance_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_Outputs_m_dot_hp_CT_htf_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_Outputs_m_dot_hp_CT_htf_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_Outputs_m_dot_hp_HT_htf_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_Outputs_m_dot_hp_HT_htf_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_Outputs_m_dot_pc_CT_htf_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_Outputs_m_dot_pc_CT_htf_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_Outputs_m_dot_pc_HT_htf_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_Outputs_m_dot_pc_HT_htf_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_Outputs_mass_CT_tes_cold_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_Outputs_mass_CT_tes_hot_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_Outputs_mass_tes_cold_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_Outputs_mass_tes_hot_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_Outputs_n_op_modes_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_Outputs_nameplate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_Outputs_op_mode_1_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_Outputs_op_mode_2_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_Outputs_op_mode_3_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_Outputs_ppa_soln_mode_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_Outputs_q_balance_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_Outputs_q_dot_CT_tes_heater_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_Outputs_q_dot_CT_tes_losses_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_Outputs_q_dot_ch_tes_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_Outputs_q_dot_dc_tes_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_Outputs_q_dot_hp_cold_in_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_Outputs_q_dot_hp_from_CT_htf_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_Outputs_q_dot_hp_hot_out_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_Outputs_q_dot_hp_startup_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_Outputs_q_dot_hp_to_HT_htf_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_Outputs_q_dot_loss_CT_tes_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_Outputs_q_dot_loss_tes_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_Outputs_q_dot_pc_cold_out_thermo_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_Outputs_q_dot_pc_cold_to_CTES_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_Outputs_q_dot_pc_cold_to_surroundings_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_Outputs_q_dot_pc_from_HT_htf_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_Outputs_q_dot_pc_hot_in_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_Outputs_q_dot_pc_rejected_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_Outputs_q_dot_pc_startup_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_Outputs_q_dot_pc_thermo_out_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_Outputs_q_dot_pc_to_CT_htf_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_Outputs_q_dot_tes_heater_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_Outputs_q_dot_tes_losses_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_Outputs_q_pc_target_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_Outputs_rte_net_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_Outputs_rte_thermo_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_Outputs_sales_tax_cost_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_Outputs_system_capacity_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_Outputs_tdry_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_Outputs_tes_cost_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_Outputs_time_hr_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_Outputs_total_direct_cost_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_Outputs_total_indirect_cost_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_Outputs_total_installed_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_Outputs_tou_period_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_EtesPtes_Outputs_tshours_heater_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_EtesPtes_Outputs_twet_aget(SAM_table ptr, int* length, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif