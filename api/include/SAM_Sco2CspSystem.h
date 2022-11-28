#ifndef SAM_SCO2CSPSYSTEM_H_
#define SAM_SCO2CSPSYSTEM_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// Sco2CspSystem Technology Model
	//

	/** 
	 * Create a Sco2CspSystem variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_Sco2CspSystem;

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_Sco2CspSystem_execute(SAM_table data, int verbosity, SAM_error* err);


	//
	// SystemDesign parameters
	//

	/**
	 * Set T_amb_des: Ambient temperature [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Sco2CspSystem_SystemDesign_T_amb_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_htf_hot_des: HTF design hot temperature (PHX inlet) [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Sco2CspSystem_SystemDesign_T_htf_hot_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set W_dot_net_des: Design cycle power output (no cooling parasitics) [MWe]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Sco2CspSystem_SystemDesign_W_dot_net_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dT_PHX_hot_approach: Temp diff btw hot HTF and turbine inlet [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Sco2CspSystem_SystemDesign_dT_PHX_hot_approach_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dT_mc_approach: Temp diff btw ambient air and main compressor inlet [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Sco2CspSystem_SystemDesign_dT_mc_approach_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set design_method: 1 = Specify efficiency, 2 = Specify total recup UA, 3 = Specify each recup design
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Sco2CspSystem_SystemDesign_design_method_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set eta_thermal_des: Power cycle thermal efficiency
	 * options: None
	 * constraints: None
	 * required if: design_method=1
	 */
	SAM_EXPORT void SAM_Sco2CspSystem_SystemDesign_eta_thermal_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set htf: Integer code for HTF used in PHX
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Sco2CspSystem_SystemDesign_htf_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set htf_props: User defined HTF property data
	 * options: 7 columns (T,Cp,dens,visc,kvisc,cond,h), at least 3 rows
	 * constraints: None
	 * required if: ?=[[0]]
	 */
	SAM_EXPORT void SAM_Sco2CspSystem_SystemDesign_htf_props_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set site_elevation: Site elevation [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Sco2CspSystem_SystemDesign_site_elevation_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// HeatExchangerDesign parameters
	//

	/**
	 * Set HTR_HP_deltaP_des_in: HTR high pressure side pressure drop as fraction of inlet pressure [-]
	 * options: High temperature recuperator
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Sco2CspSystem_HeatExchangerDesign_HTR_HP_deltaP_des_in_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set HTR_LP_deltaP_des_in: HTR low pressure side pressure drop as fraction of inlet pressure [-]
	 * options: High temperature recuperator
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Sco2CspSystem_HeatExchangerDesign_HTR_LP_deltaP_des_in_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set HTR_UA_des_in: Design HTR conductance [kW/K]
	 * options: High temperature recuperator
	 * constraints: None
	 * required if: design_method=3
	 */
	SAM_EXPORT void SAM_Sco2CspSystem_HeatExchangerDesign_HTR_UA_des_in_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set HTR_design_code: 1 = UA, 2 = min dT, 3 = effectiveness [-]
	 * options: High temperature recuperator
	 * constraints: None
	 * required if: design_method=3
	 */
	SAM_EXPORT void SAM_Sco2CspSystem_HeatExchangerDesign_HTR_design_code_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set HTR_eff_des_in: Design effectiveness for HTR [-]
	 * options: High temperature recuperator
	 * constraints: None
	 * required if: design_method=3
	 */
	SAM_EXPORT void SAM_Sco2CspSystem_HeatExchangerDesign_HTR_eff_des_in_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set HTR_min_dT_des_in: Design minimum allowable temperature difference in HTR [C]
	 * options: High temperature recuperator
	 * constraints: None
	 * required if: design_method=3
	 */
	SAM_EXPORT void SAM_Sco2CspSystem_HeatExchangerDesign_HTR_min_dT_des_in_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set HTR_n_sub_hx: HTR number of model subsections [-]
	 * options: High temperature recuperator
	 * constraints: None
	 * required if: ?=10
	 */
	SAM_EXPORT void SAM_Sco2CspSystem_HeatExchangerDesign_HTR_n_sub_hx_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set HTR_od_model: 0: mass flow scale, 1: conductance ratio model [-]
	 * options: High temperature recuperator
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Sco2CspSystem_HeatExchangerDesign_HTR_od_model_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set HT_recup_eff_max: Maximum allowable effectiveness in HTR [-]
	 * options: High temperature recuperator
	 * constraints: None
	 * required if: ?=1.0
	 */
	SAM_EXPORT void SAM_Sco2CspSystem_HeatExchangerDesign_HT_recup_eff_max_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set LTR_HP_deltaP_des_in: LTR high pressure side pressure drop as fraction of inlet pressure [-]
	 * options: Low temperature recuperator
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Sco2CspSystem_HeatExchangerDesign_LTR_HP_deltaP_des_in_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set LTR_LP_deltaP_des_in: LTR low pressure side pressure drop as fraction of inlet pressure [-]
	 * options: Low temperature recuperator
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Sco2CspSystem_HeatExchangerDesign_LTR_LP_deltaP_des_in_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set LTR_UA_des_in: Design LTR conductance [kW/K]
	 * options: Low temperature recuperator
	 * constraints: None
	 * required if: design_method=3
	 */
	SAM_EXPORT void SAM_Sco2CspSystem_HeatExchangerDesign_LTR_UA_des_in_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set LTR_design_code: 1 = UA, 2 = min dT, 3 = effectiveness [-]
	 * options: Low temperature recuperator
	 * constraints: None
	 * required if: design_method=3
	 */
	SAM_EXPORT void SAM_Sco2CspSystem_HeatExchangerDesign_LTR_design_code_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set LTR_eff_des_in: Design effectiveness for LTR [-]
	 * options: Low temperature recuperator
	 * constraints: None
	 * required if: design_method=3
	 */
	SAM_EXPORT void SAM_Sco2CspSystem_HeatExchangerDesign_LTR_eff_des_in_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set LTR_min_dT_des_in: Design minimum allowable temperature difference in LTR [C]
	 * options: Low temperature recuperator
	 * constraints: None
	 * required if: design_method=3
	 */
	SAM_EXPORT void SAM_Sco2CspSystem_HeatExchangerDesign_LTR_min_dT_des_in_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set LTR_n_sub_hx: LTR number of model subsections [-]
	 * options: Low temperature recuperator
	 * constraints: None
	 * required if: ?=10
	 */
	SAM_EXPORT void SAM_Sco2CspSystem_HeatExchangerDesign_LTR_n_sub_hx_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set LTR_od_model: 0: mass flow scale, 1: conductance ratio model [-]
	 * options: Low temperature recuperator
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Sco2CspSystem_HeatExchangerDesign_LTR_od_model_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set LT_recup_eff_max: Maximum allowable effectiveness in LTR [-]
	 * options: Low temperature recuperator
	 * constraints: None
	 * required if: ?=1.0
	 */
	SAM_EXPORT void SAM_Sco2CspSystem_HeatExchangerDesign_LT_recup_eff_max_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set UA_recup_tot_des: Total recuperator conductance [kW/K]
	 * options: Combined recuperator design
	 * constraints: None
	 * required if: design_method=2
	 */
	SAM_EXPORT void SAM_Sco2CspSystem_HeatExchangerDesign_UA_recup_tot_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cycle_config: 1 = recompression, 2 = partial cooling
	 * options: High temperature recuperator
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Sco2CspSystem_HeatExchangerDesign_cycle_config_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set des_objective: [2] = hit min phx deltat then max eta, [else] max eta
	 * options: High temperature recuperator
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Sco2CspSystem_HeatExchangerDesign_des_objective_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set is_IP_fixed: partial cooling config: 0 = No, >0 = fixed HP-IP pressure ratio at input, <0 = fixed IP at abs(input)
	 * options: High temperature recuperator
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Sco2CspSystem_HeatExchangerDesign_is_IP_fixed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set is_PR_fixed: 0 = No, >0 = fixed pressure ratio at input <0 = fixed LP at abs(input) [High temperature recuperator]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Sco2CspSystem_HeatExchangerDesign_is_PR_fixed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set is_P_high_fixed: 1 = Yes (=P_high_limit), 0 = No, optimized (default)
	 * options: High temperature recuperator
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Sco2CspSystem_HeatExchangerDesign_is_P_high_fixed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set is_recomp_ok: 1 = Yes, 0 = simple cycle only, < 0 = fix f_recomp to abs(input)
	 * options: High temperature recuperator
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Sco2CspSystem_HeatExchangerDesign_is_recomp_ok_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set min_phx_deltaT: Minimum design temperature difference across PHX [C]
	 * options: High temperature recuperator
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Sco2CspSystem_HeatExchangerDesign_min_phx_deltaT_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set od_rel_tol: Baseline off-design relative convergence tolerance exponent (10^-od_rel_tol) [-]
	 * options: High temperature recuperator
	 * constraints: None
	 * required if: ?=3
	 */
	SAM_EXPORT void SAM_Sco2CspSystem_HeatExchangerDesign_od_rel_tol_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set rel_tol: Baseline solver and optimization relative tolerance exponent (10^-rel_tol) [-]
	 * options: High temperature recuperator
	 * constraints: None
	 * required if: ?=3
	 */
	SAM_EXPORT void SAM_Sco2CspSystem_HeatExchangerDesign_rel_tol_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// Common parameters
	//

	/**
	 * Set PHX_co2_deltaP_des_in: PHX co2 side pressure drop as fraction of inlet pressure [-]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Sco2CspSystem_Common_PHX_co2_deltaP_des_in_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set P_high_limit: High pressure limit in cycle [MPa]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Sco2CspSystem_Common_P_high_limit_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set deltaP_counterHX_frac: Fraction of CO2 inlet pressure that is design point counterflow HX (recups & PHX) pressure drop [-]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Sco2CspSystem_Common_deltaP_counterHX_frac_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set eta_isen_mc: Design main compressor isentropic efficiency [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Sco2CspSystem_Common_eta_isen_mc_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set eta_isen_pc: Design precompressor isentropic efficiency [-]
	 * options: None
	 * constraints: None
	 * required if: cycle_config=2
	 */
	SAM_EXPORT void SAM_Sco2CspSystem_Common_eta_isen_pc_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set eta_isen_rc: Design re-compressor isentropic efficiency [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Sco2CspSystem_Common_eta_isen_rc_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set eta_isen_t: Design turbine isentropic efficiency [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Sco2CspSystem_Common_eta_isen_t_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set is_gen_od_polynomials: Generate off-design polynomials for Generic CSP models? 1 = Yes, 0 = No
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Sco2CspSystem_Common_is_gen_od_polynomials_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mc_comp_type: Main compressor compressor type 1: SNL 2: CompA [-]
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Sco2CspSystem_Common_mc_comp_type_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set od_P_mc_in_sweep: Columns: 0) T_htf_C, 1) m_dot_htf_ND, 2) T_amb_C, 3) T_mc_in_C, 4) T_pc_in_C, 5) f_N_rc (=1 use design, <0, frac_des = abs(input), 6) f_N_mc (=1 use design, <0, frac_des = abs(input), 7) f_N_pc (=1 use design, =0 optimize, <0, frac_des = abs(input)), 8) PHX_f_dP (=1 use design, <0 = abs(input)
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Sco2CspSystem_Common_od_P_mc_in_sweep_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set od_T_mc_in_sweep: Columns: 0) T_htf_C, 1) m_dot_htf_ND, 2) T_amb_C,3) f_N_rc (=1 use design, <0, frac_des = abs(input),4) f_N_mc (=1 use design, <0, frac_des = abs(input),5) f_N_pc (=1 use design, <0, frac_des = abs(input),6) PHX_f_dP (=1 use design, <0 = abs(input)
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Sco2CspSystem_Common_od_T_mc_in_sweep_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set od_T_t_in_mode: 0: model solves co2/HTF PHX od model to calculate turbine inlet temp, 1: model sets turbine inlet temp to HTF hot temp
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Sco2CspSystem_Common_od_T_t_in_mode_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set od_cases: Columns: 0) T_htf_C, 1) m_dot_htf_ND, 2) T_amb_C, 3) f_N_rc (=1 use design, =0 optimize, <0, frac_des = abs(input)), 4) f_N_mc (=1 use design, =0 optimize, <0, frac_des = abs(input)), 5) f_N_pc (=1 use design, =0 optimize, <0, frac_des = abs(input)), 6) PHX_f_dP (=1 use design, <0 = abs(input), Rows: cases
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Sco2CspSystem_Common_od_cases_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set od_generate_udpc: True/False, f_N_rc (=1 use design, =0 optimize, <0, frac_des = abs(input), f_N_mc (=1 use design, =0 optimize, <0, frac_des = abs(input), PHX_f_dP (=1 use design, <0 = abs(input)
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Sco2CspSystem_Common_od_generate_udpc_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set od_max_htf_m_dot: Columns: T_htf_C, T_amb_C, f_N_rc (=1 use design, <0, frac_des = abs(input), f_N_mc (=1 use design, <0, frac_des = abs(input), PHX_f_dP (=1 use design, <0 = abs(input), Rows: cases
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Sco2CspSystem_Common_od_max_htf_m_dot_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set od_opt_objective: 0: find P_LP_in to achieve target power, optimize efficiency 1: find P_LP_in to achieve T_HTF_cold, optimize efficiency
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Sco2CspSystem_Common_od_opt_objective_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set od_set_control: Columns: 0) T_htf_C, 1) m_dot_htf_ND, 2) T_amb_C, 3) P_LP_in_MPa, 4) T_mc_in_C, 5) T_pc_in_C, 6) f_N_rc (=1 use design, <0, frac_des = abs(input), 7) f_N_mc (=1 use design, <0, frac_des = abs(input), 8) f_N_pc (=1 use design, =0 optimize, <0, frac_des = abs(input)), 9) PHX_f_dP (=1 use design, <0 = abs(input), Rows: cases
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Sco2CspSystem_Common_od_set_control_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);


	//
	// PHXDesign parameters
	//

	/**
	 * Set PHX_n_sub_hx: Number of subsections in PHX model [-]
	 * options: None
	 * constraints: None
	 * required if: ?=10
	 */
	SAM_EXPORT void SAM_Sco2CspSystem_PHXDesign_PHX_n_sub_hx_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set PHX_od_model: 0: mass flow scale, 1: conductance ratio model [-]
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Sco2CspSystem_PHXDesign_PHX_od_model_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dT_PHX_cold_approach: Temp diff btw cold HTF and cold CO2 [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Sco2CspSystem_PHXDesign_dT_PHX_cold_approach_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// AirCoolerDesign parameters
	//

	/**
	 * Set N_nodes_air_cooler_pass: Number of nodes in single air cooler pass
	 * options: None
	 * constraints: None
	 * required if: ?=10
	 */
	SAM_EXPORT void SAM_Sco2CspSystem_AirCoolerDesign_N_nodes_air_cooler_pass_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set deltaP_cooler_frac: Fraction of CO2 inlet pressure that is design point cooler CO2 pressure drop
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Sco2CspSystem_AirCoolerDesign_deltaP_cooler_frac_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set eta_air_cooler_fan: Air cooler fan isentropic efficiency
	 * options: None
	 * constraints: None
	 * required if: ?=0.5
	 */
	SAM_EXPORT void SAM_Sco2CspSystem_AirCoolerDesign_eta_air_cooler_fan_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set fan_power_frac: Fraction of net cycle power consumed by air cooler fan
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Sco2CspSystem_AirCoolerDesign_fan_power_frac_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set is_design_air_cooler: Defaults to True. False will skip air cooler calcs
	 * options: None
	 * constraints: None
	 * required if: ?=1.0
	 */
	SAM_EXPORT void SAM_Sco2CspSystem_AirCoolerDesign_is_design_air_cooler_nset(SAM_table ptr, double number, SAM_error *err);


	/**
	 * SystemDesign Getters
	 */

	SAM_EXPORT double SAM_Sco2CspSystem_SystemDesign_T_amb_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_SystemDesign_T_htf_hot_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_SystemDesign_W_dot_net_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_SystemDesign_dT_PHX_hot_approach_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_SystemDesign_dT_mc_approach_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_SystemDesign_design_method_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_SystemDesign_eta_thermal_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_SystemDesign_htf_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_SystemDesign_htf_props_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_SystemDesign_site_elevation_nget(SAM_table ptr, SAM_error *err);


	/**
	 * HeatExchangerDesign Getters
	 */

	SAM_EXPORT double SAM_Sco2CspSystem_HeatExchangerDesign_HTR_HP_deltaP_des_in_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_HeatExchangerDesign_HTR_LP_deltaP_des_in_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_HeatExchangerDesign_HTR_UA_des_in_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_HeatExchangerDesign_HTR_design_code_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_HeatExchangerDesign_HTR_eff_des_in_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_HeatExchangerDesign_HTR_min_dT_des_in_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_HeatExchangerDesign_HTR_n_sub_hx_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_HeatExchangerDesign_HTR_od_model_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_HeatExchangerDesign_HT_recup_eff_max_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_HeatExchangerDesign_LTR_HP_deltaP_des_in_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_HeatExchangerDesign_LTR_LP_deltaP_des_in_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_HeatExchangerDesign_LTR_UA_des_in_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_HeatExchangerDesign_LTR_design_code_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_HeatExchangerDesign_LTR_eff_des_in_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_HeatExchangerDesign_LTR_min_dT_des_in_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_HeatExchangerDesign_LTR_n_sub_hx_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_HeatExchangerDesign_LTR_od_model_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_HeatExchangerDesign_LT_recup_eff_max_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_HeatExchangerDesign_UA_recup_tot_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_HeatExchangerDesign_cycle_config_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_HeatExchangerDesign_des_objective_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_HeatExchangerDesign_is_IP_fixed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_HeatExchangerDesign_is_PR_fixed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_HeatExchangerDesign_is_P_high_fixed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_HeatExchangerDesign_is_recomp_ok_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_HeatExchangerDesign_min_phx_deltaT_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_HeatExchangerDesign_od_rel_tol_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_HeatExchangerDesign_rel_tol_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Common Getters
	 */

	SAM_EXPORT double SAM_Sco2CspSystem_Common_PHX_co2_deltaP_des_in_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Common_P_high_limit_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Common_deltaP_counterHX_frac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Common_eta_isen_mc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Common_eta_isen_pc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Common_eta_isen_rc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Common_eta_isen_t_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Common_is_gen_od_polynomials_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Common_mc_comp_type_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Common_od_P_mc_in_sweep_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Common_od_T_mc_in_sweep_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Common_od_T_t_in_mode_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Common_od_cases_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Common_od_generate_udpc_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Common_od_max_htf_m_dot_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Common_od_opt_objective_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Common_od_set_control_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);


	/**
	 * PHXDesign Getters
	 */

	SAM_EXPORT double SAM_Sco2CspSystem_PHXDesign_PHX_n_sub_hx_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_PHXDesign_PHX_od_model_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_PHXDesign_dT_PHX_cold_approach_nget(SAM_table ptr, SAM_error *err);


	/**
	 * AirCoolerDesign Getters
	 */

	SAM_EXPORT double SAM_Sco2CspSystem_AirCoolerDesign_N_nodes_air_cooler_pass_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_AirCoolerDesign_deltaP_cooler_frac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_AirCoolerDesign_eta_air_cooler_fan_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_AirCoolerDesign_fan_power_frac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_AirCoolerDesign_is_design_air_cooler_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_HTR_HP_T_in_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_HTR_HP_T_in_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_HTR_HP_deltaP_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_HTR_HP_deltaP_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_HTR_LP_T_out_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_HTR_LP_T_out_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_HTR_LP_deltaP_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_HTR_LP_deltaP_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_HTR_UA_assigned_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_HTR_UA_calculated_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_HTR_cost_bare_erected_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_HTR_cost_equipment_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_HTR_min_dT_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_HTR_min_dT_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_LTR_HP_T_out_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_LTR_HP_T_out_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_LTR_HP_deltaP_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_LTR_HP_deltaP_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_LTR_LP_deltaP_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_LTR_LP_deltaP_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_LTR_UA_assigned_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_LTR_UA_calculated_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_LTR_cost_bare_erected_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_LTR_cost_equipment_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_LTR_min_dT_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_LTR_min_dT_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_NTU_HTR_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_NTU_LTR_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_NTU_PHX_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_PHX_co2_deltaP_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_PHX_cost_bare_erected_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_PHX_cost_equipment_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_P_co2_PHX_in_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_P_co2_PHX_in_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_P_comp_in_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_P_comp_in_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_P_comp_out_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_P_mc_data_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_P_mc_out_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_P_pc_data_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_P_rc_data_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_P_state_points_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_P_t_data_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_Q_dot_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_T_HTR_HP_data_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_T_HTR_LP_data_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_T_LTR_HP_data_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_T_LTR_LP_data_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_T_PHX_data_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_T_amb_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_T_co2_PHX_in_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_T_co2_PHX_in_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_T_co2_PHX_out_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_T_comp_in_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_T_htf_cold_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_T_htf_cold_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_T_htf_hot_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_T_main_cooler_data_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_T_mc_in_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_T_pre_cooler_data_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_T_state_points_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_T_turb_in_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_UA_PHX_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_W_dot_net_less_cooling_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_W_dot_net_less_cooling_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_W_dot_net_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_c_tot_W_dot_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_c_tot_W_dot_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_c_tot_cost_equip_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_cooler_tot_UA_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_cooler_tot_W_dot_fan_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_cooler_tot_W_dot_fan_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_cooler_tot_cost_bare_erected_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_cooler_tot_cost_equipment_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_cycle_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_cycle_spec_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_cycle_spec_cost_thermal_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_deltaT_HTF_PHX_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_deltaT_HTF_PHX_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_diff_E_cycle_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_diff_Q_HTR_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_diff_Q_LTR_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_diff_m_dot_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_eff_HTR_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_eff_HTR_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_eff_LTR_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_eff_LTR_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_eff_PHX_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_eta_thermal_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_eta_thermal_net_less_cooling_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_eta_thermal_net_less_cooling_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_eta_thermal_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_h_mc_data_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_h_pc_data_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_h_rc_data_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_h_state_points_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_h_t_data_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_m_dot_co2_full_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_m_dot_co2_full_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_m_dot_htf_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_m_dot_htf_fracs_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_mc_D_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_mc_N_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_mc_N_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_mc_N_od_perc_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_mc_T_out_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_mc_T_out_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_mc_W_dot_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_mc_W_dot_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_mc_cooler_P_in_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_mc_cooler_T_in_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_mc_cooler_T_in_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_mc_cooler_UA_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_mc_cooler_W_dot_fan_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_mc_cooler_W_dot_fan_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_mc_cooler_co2_deltaP_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_mc_cooler_co2_deltaP_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_mc_cooler_cost_bare_erected_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_mc_cooler_cost_equipment_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_mc_cooler_in_isen_deltah_to_P_mc_out_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_mc_cooler_in_isen_deltah_to_P_mc_out_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_mc_cooler_m_dot_co2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_mc_cooler_q_dot_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_mc_cooler_rho_in_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_mc_cooler_rho_in_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_mc_cost_bare_erected_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_mc_cost_equipment_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_mc_eta_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_mc_eta_stages_des_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_mc_eta_stages_od_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_mc_f_bypass_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_mc_ideal_spec_work_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_mc_ideal_spec_work_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_mc_m_dot_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_mc_m_dot_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_mc_n_stages_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_mc_phi_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_mc_phi_od_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_mc_phi_surge_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_mc_psi_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_mc_psi_max_at_N_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_mc_psi_od_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_mc_rho_in_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_mc_rho_in_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_mc_tip_ratio_des_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_mc_tip_ratio_od_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_od_code_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_pc_D_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_pc_N_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_pc_N_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_pc_P_in_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_pc_P_in_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_pc_T_in_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_pc_T_in_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_pc_W_dot_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_pc_W_dot_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_pc_cooler_P_in_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_pc_cooler_T_in_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_pc_cooler_UA_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_pc_cooler_W_dot_fan_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_pc_cooler_W_dot_fan_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_pc_cooler_cost_bare_erected_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_pc_cooler_cost_equipment_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_pc_cooler_m_dot_co2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_pc_cooler_q_dot_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_pc_cost_bare_erected_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_pc_cost_equipment_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_pc_eta_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_pc_eta_stages_des_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_pc_eta_stages_od_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_pc_f_bypass_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_pc_ideal_spec_work_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_pc_ideal_spec_work_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_pc_m_dot_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_pc_m_dot_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_pc_n_stages_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_pc_phi_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_pc_phi_od_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_pc_phi_surge_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_pc_rho_in_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_pc_rho_in_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_pc_tip_ratio_des_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_pc_tip_ratio_od_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_phx_co2_deltaP_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_phx_eff_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_piping_inventory_etc_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_q_dot_HTR_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_q_dot_HTR_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_q_dot_LTR_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_q_dot_LTR_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_q_dot_PHX_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_rc_D_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_rc_N_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_rc_N_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_rc_N_od_perc_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_rc_P_in_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_rc_P_in_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_rc_P_out_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_rc_P_out_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_rc_T_in_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_rc_T_in_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_rc_T_out_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_rc_T_out_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_rc_W_dot_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_rc_W_dot_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_rc_cost_bare_erected_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_rc_cost_equipment_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_rc_eta_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_rc_eta_stages_des_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_rc_eta_stages_od_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_rc_m_dot_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_rc_m_dot_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_rc_n_stages_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_rc_phi_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_rc_phi_od_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_rc_phi_surge_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_rc_psi_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_rc_psi_max_at_N_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_rc_psi_od_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_rc_tip_ratio_des_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_rc_tip_ratio_od_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_recomp_frac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_recomp_frac_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_recup_LTR_UA_frac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_recup_total_UA_assigned_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_recup_total_UA_calculated_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_recup_total_cost_bare_erected_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_recup_total_cost_equipment_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_s_HTR_HP_data_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_s_HTR_LP_data_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_s_LTR_HP_data_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_s_LTR_LP_data_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_s_PHX_data_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_s_main_cooler_data_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_s_pre_cooler_data_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_s_state_points_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_sim_time_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_t_D_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_t_N_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_t_N_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_t_P_in_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_t_P_in_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_t_P_out_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_t_P_out_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_t_T_out_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_t_T_out_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_t_W_dot_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_t_W_dot_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_t_cost_bare_erected_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_t_cost_equipment_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_t_delta_h_isen_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_t_delta_h_isen_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_t_eta_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_t_m_dot_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_t_m_dot_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_t_nu_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_t_nu_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_t_rho_in_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_t_rho_in_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_t_tip_ratio_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_t_tip_ratio_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_udpc_n_T_amb_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_udpc_n_T_htf_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspSystem_Outputs_udpc_n_m_dot_htf_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_udpc_table_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif