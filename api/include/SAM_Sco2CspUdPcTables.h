#ifndef SAM_SCO2CSPUDPCTABLES_H_
#define SAM_SCO2CSPUDPCTABLES_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// Sco2CspUdPcTables Technology Model
	//

	/** 
	 * Create a Sco2CspUdPcTables variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_Sco2CspUdPcTables;

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_Sco2CspUdPcTables_execute(SAM_table data, int verbosity, SAM_error* err);


	//
	// SystemDesign parameters
	//

	/**
	 * Set T_amb_des: Ambient temperature [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Sco2CspUdPcTables_SystemDesign_T_amb_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_htf_hot_des: HTF design hot temperature (PHX inlet) [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Sco2CspUdPcTables_SystemDesign_T_htf_hot_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set W_dot_net_des: Design cycle power output (no cooling parasitics) [MWe]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Sco2CspUdPcTables_SystemDesign_W_dot_net_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dT_PHX_hot_approach: Temp diff btw hot HTF and turbine inlet [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Sco2CspUdPcTables_SystemDesign_dT_PHX_hot_approach_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dT_mc_approach: Temp diff btw ambient air and main compressor inlet [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Sco2CspUdPcTables_SystemDesign_dT_mc_approach_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set design_method: 1 = Specify efficiency, 2 = Specify total recup UA, 3 = Specify each recup design
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Sco2CspUdPcTables_SystemDesign_design_method_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set eta_thermal_des: Power cycle thermal efficiency
	 * options: None
	 * constraints: None
	 * required if: design_method=1
	 */
	SAM_EXPORT void SAM_Sco2CspUdPcTables_SystemDesign_eta_thermal_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set htf: Integer code for HTF used in PHX
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Sco2CspUdPcTables_SystemDesign_htf_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set htf_props: User defined HTF property data
	 * options: 7 columns (T,Cp,dens,visc,kvisc,cond,h), at least 3 rows
	 * constraints: None
	 * required if: ?=[[0]]
	 */
	SAM_EXPORT void SAM_Sco2CspUdPcTables_SystemDesign_htf_props_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set site_elevation: Site elevation [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Sco2CspUdPcTables_SystemDesign_site_elevation_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// HeatExchangerDesign parameters
	//

	/**
	 * Set HTR_HP_deltaP_des_in: HTR high pressure side pressure drop as fraction of inlet pressure [-]
	 * options: High temperature recuperator
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Sco2CspUdPcTables_HeatExchangerDesign_HTR_HP_deltaP_des_in_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set HTR_LP_deltaP_des_in: HTR low pressure side pressure drop as fraction of inlet pressure [-]
	 * options: High temperature recuperator
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Sco2CspUdPcTables_HeatExchangerDesign_HTR_LP_deltaP_des_in_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set HTR_UA_des_in: Design HTR conductance [kW/K]
	 * options: High temperature recuperator
	 * constraints: None
	 * required if: design_method=3
	 */
	SAM_EXPORT void SAM_Sco2CspUdPcTables_HeatExchangerDesign_HTR_UA_des_in_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set HTR_design_code: 1 = UA, 2 = min dT, 3 = effectiveness [-]
	 * options: High temperature recuperator
	 * constraints: None
	 * required if: design_method=3
	 */
	SAM_EXPORT void SAM_Sco2CspUdPcTables_HeatExchangerDesign_HTR_design_code_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set HTR_eff_des_in: Design effectiveness for HTR [-]
	 * options: High temperature recuperator
	 * constraints: None
	 * required if: design_method=3
	 */
	SAM_EXPORT void SAM_Sco2CspUdPcTables_HeatExchangerDesign_HTR_eff_des_in_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set HTR_min_dT_des_in: Design minimum allowable temperature difference in HTR [C]
	 * options: High temperature recuperator
	 * constraints: None
	 * required if: design_method=3
	 */
	SAM_EXPORT void SAM_Sco2CspUdPcTables_HeatExchangerDesign_HTR_min_dT_des_in_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set HTR_n_sub_hx: HTR number of model subsections [-]
	 * options: High temperature recuperator
	 * constraints: None
	 * required if: ?=10
	 */
	SAM_EXPORT void SAM_Sco2CspUdPcTables_HeatExchangerDesign_HTR_n_sub_hx_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set HTR_od_model: 0: mass flow scale, 1: conductance ratio model [-]
	 * options: High temperature recuperator
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Sco2CspUdPcTables_HeatExchangerDesign_HTR_od_model_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set HT_recup_eff_max: Maximum allowable effectiveness in HTR [-]
	 * options: High temperature recuperator
	 * constraints: None
	 * required if: ?=1.0
	 */
	SAM_EXPORT void SAM_Sco2CspUdPcTables_HeatExchangerDesign_HT_recup_eff_max_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set LTR_HP_deltaP_des_in: LTR high pressure side pressure drop as fraction of inlet pressure [-]
	 * options: Low temperature recuperator
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Sco2CspUdPcTables_HeatExchangerDesign_LTR_HP_deltaP_des_in_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set LTR_LP_deltaP_des_in: LTR low pressure side pressure drop as fraction of inlet pressure [-]
	 * options: Low temperature recuperator
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Sco2CspUdPcTables_HeatExchangerDesign_LTR_LP_deltaP_des_in_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set LTR_UA_des_in: Design LTR conductance [kW/K]
	 * options: Low temperature recuperator
	 * constraints: None
	 * required if: design_method=3
	 */
	SAM_EXPORT void SAM_Sco2CspUdPcTables_HeatExchangerDesign_LTR_UA_des_in_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set LTR_design_code: 1 = UA, 2 = min dT, 3 = effectiveness [-]
	 * options: Low temperature recuperator
	 * constraints: None
	 * required if: design_method=3
	 */
	SAM_EXPORT void SAM_Sco2CspUdPcTables_HeatExchangerDesign_LTR_design_code_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set LTR_eff_des_in: Design effectiveness for LTR [-]
	 * options: Low temperature recuperator
	 * constraints: None
	 * required if: design_method=3
	 */
	SAM_EXPORT void SAM_Sco2CspUdPcTables_HeatExchangerDesign_LTR_eff_des_in_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set LTR_min_dT_des_in: Design minimum allowable temperature difference in LTR [C]
	 * options: Low temperature recuperator
	 * constraints: None
	 * required if: design_method=3
	 */
	SAM_EXPORT void SAM_Sco2CspUdPcTables_HeatExchangerDesign_LTR_min_dT_des_in_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set LTR_n_sub_hx: LTR number of model subsections [-]
	 * options: Low temperature recuperator
	 * constraints: None
	 * required if: ?=10
	 */
	SAM_EXPORT void SAM_Sco2CspUdPcTables_HeatExchangerDesign_LTR_n_sub_hx_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set LTR_od_model: 0: mass flow scale, 1: conductance ratio model [-]
	 * options: Low temperature recuperator
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Sco2CspUdPcTables_HeatExchangerDesign_LTR_od_model_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set LT_recup_eff_max: Maximum allowable effectiveness in LTR [-]
	 * options: Low temperature recuperator
	 * constraints: None
	 * required if: ?=1.0
	 */
	SAM_EXPORT void SAM_Sco2CspUdPcTables_HeatExchangerDesign_LT_recup_eff_max_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set UA_recup_tot_des: Total recuperator conductance [kW/K]
	 * options: Combined recuperator design
	 * constraints: None
	 * required if: design_method=2
	 */
	SAM_EXPORT void SAM_Sco2CspUdPcTables_HeatExchangerDesign_UA_recup_tot_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cycle_config: 1 = recompression, 2 = partial cooling
	 * options: High temperature recuperator
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Sco2CspUdPcTables_HeatExchangerDesign_cycle_config_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set des_objective: [2] = hit min phx deltat then max eta, [else] max eta
	 * options: High temperature recuperator
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Sco2CspUdPcTables_HeatExchangerDesign_des_objective_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set is_IP_fixed: partial cooling config: 0 = No, >0 = fixed HP-IP pressure ratio at input, <0 = fixed IP at abs(input)
	 * options: High temperature recuperator
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Sco2CspUdPcTables_HeatExchangerDesign_is_IP_fixed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set is_PR_fixed: 0 = No, >0 = fixed pressure ratio at input <0 = fixed LP at abs(input) [High temperature recuperator]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Sco2CspUdPcTables_HeatExchangerDesign_is_PR_fixed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set is_P_high_fixed: 1 = Yes (=P_high_limit), 0 = No, optimized (default)
	 * options: High temperature recuperator
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Sco2CspUdPcTables_HeatExchangerDesign_is_P_high_fixed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set is_recomp_ok: 1 = Yes, 0 = simple cycle only, < 0 = fix f_recomp to abs(input)
	 * options: High temperature recuperator
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Sco2CspUdPcTables_HeatExchangerDesign_is_recomp_ok_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set min_phx_deltaT: Minimum design temperature difference across PHX [C]
	 * options: High temperature recuperator
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Sco2CspUdPcTables_HeatExchangerDesign_min_phx_deltaT_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set rel_tol: Baseline solver and optimization relative tolerance exponent (10^-rel_tol) [-]
	 * options: High temperature recuperator
	 * constraints: None
	 * required if: ?=3
	 */
	SAM_EXPORT void SAM_Sco2CspUdPcTables_HeatExchangerDesign_rel_tol_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// Common parameters
	//

	/**
	 * Set PHX_co2_deltaP_des_in: PHX co2 side pressure drop as fraction of inlet pressure [-]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Sco2CspUdPcTables_Common_PHX_co2_deltaP_des_in_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set P_high_limit: High pressure limit in cycle [MPa]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Sco2CspUdPcTables_Common_P_high_limit_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_amb_high: Upper level of ambient temperature [C]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Sco2CspUdPcTables_Common_T_amb_high_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_amb_low: Lower level of ambient temperature [C]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Sco2CspUdPcTables_Common_T_amb_low_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_htf_hot_high: Upper level of HTF hot temperature [C]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Sco2CspUdPcTables_Common_T_htf_hot_high_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_htf_hot_low: Lower level of HTF hot temperature [C]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Sco2CspUdPcTables_Common_T_htf_hot_low_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set deltaP_counterHX_frac: Fraction of CO2 inlet pressure that is design point counterflow HX (recups & PHX) pressure drop [-]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Sco2CspUdPcTables_Common_deltaP_counterHX_frac_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set eta_isen_mc: Design main compressor isentropic efficiency [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Sco2CspUdPcTables_Common_eta_isen_mc_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set eta_isen_pc: Design precompressor isentropic efficiency [-]
	 * options: None
	 * constraints: None
	 * required if: cycle_config=2
	 */
	SAM_EXPORT void SAM_Sco2CspUdPcTables_Common_eta_isen_pc_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set eta_isen_rc: Design re-compressor isentropic efficiency [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Sco2CspUdPcTables_Common_eta_isen_rc_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set eta_isen_t: Design turbine isentropic efficiency [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Sco2CspUdPcTables_Common_eta_isen_t_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set is_apply_default_htf_mins: 1 = yes (0.5 rc, 0.7 simple), 0 = no, only use 'm_dot_htf_ND_low'
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Sco2CspUdPcTables_Common_is_apply_default_htf_mins_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set is_generate_udpc: 1 = generate udpc tables, 0 = only calculate design point cyle
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Sco2CspUdPcTables_Common_is_generate_udpc_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set m_dot_htf_ND_high: Upper level of normalized HTF mass flow rate
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Sco2CspUdPcTables_Common_m_dot_htf_ND_high_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set m_dot_htf_ND_low: Lower level of normalized HTF mass flow rate
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Sco2CspUdPcTables_Common_m_dot_htf_ND_low_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mc_comp_type: Main compressor compressor type 1: SNL 2: CompA [-]
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Sco2CspUdPcTables_Common_mc_comp_type_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set n_T_amb: Number of ambient temperature parametric runs
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Sco2CspUdPcTables_Common_n_T_amb_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set n_T_htf_hot: Number of HTF hot temperature parametric runs
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Sco2CspUdPcTables_Common_n_T_htf_hot_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set n_m_dot_htf_ND: Number of normalized HTF mass flow rate parametric runs
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Sco2CspUdPcTables_Common_n_m_dot_htf_ND_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// PHXDesign parameters
	//

	/**
	 * Set PHX_n_sub_hx: Number of subsections in PHX model [-]
	 * options: None
	 * constraints: None
	 * required if: ?=10
	 */
	SAM_EXPORT void SAM_Sco2CspUdPcTables_PHXDesign_PHX_n_sub_hx_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set PHX_od_model: 0: mass flow scale, 1: conductance ratio model [-]
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Sco2CspUdPcTables_PHXDesign_PHX_od_model_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dT_PHX_cold_approach: Temp diff btw cold HTF and cold CO2 [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Sco2CspUdPcTables_PHXDesign_dT_PHX_cold_approach_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// AirCoolerDesign parameters
	//

	/**
	 * Set N_nodes_air_cooler_pass: Number of nodes in single air cooler pass
	 * options: None
	 * constraints: None
	 * required if: ?=10
	 */
	SAM_EXPORT void SAM_Sco2CspUdPcTables_AirCoolerDesign_N_nodes_air_cooler_pass_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set deltaP_cooler_frac: Fraction of CO2 inlet pressure that is design point cooler CO2 pressure drop
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Sco2CspUdPcTables_AirCoolerDesign_deltaP_cooler_frac_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set eta_air_cooler_fan: Air cooler fan isentropic efficiency
	 * options: None
	 * constraints: None
	 * required if: ?=0.5
	 */
	SAM_EXPORT void SAM_Sco2CspUdPcTables_AirCoolerDesign_eta_air_cooler_fan_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set fan_power_frac: Fraction of net cycle power consumed by air cooler fan
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Sco2CspUdPcTables_AirCoolerDesign_fan_power_frac_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set is_design_air_cooler: Defaults to True. False will skip air cooler calcs
	 * options: None
	 * constraints: None
	 * required if: ?=1.0
	 */
	SAM_EXPORT void SAM_Sco2CspUdPcTables_AirCoolerDesign_is_design_air_cooler_nset(SAM_table ptr, double number, SAM_error *err);


	/**
	 * SystemDesign Getters
	 */

	SAM_EXPORT double SAM_Sco2CspUdPcTables_SystemDesign_T_amb_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_SystemDesign_T_htf_hot_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_SystemDesign_W_dot_net_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_SystemDesign_dT_PHX_hot_approach_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_SystemDesign_dT_mc_approach_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_SystemDesign_design_method_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_SystemDesign_eta_thermal_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_SystemDesign_htf_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspUdPcTables_SystemDesign_htf_props_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_SystemDesign_site_elevation_nget(SAM_table ptr, SAM_error *err);


	/**
	 * HeatExchangerDesign Getters
	 */

	SAM_EXPORT double SAM_Sco2CspUdPcTables_HeatExchangerDesign_HTR_HP_deltaP_des_in_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_HeatExchangerDesign_HTR_LP_deltaP_des_in_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_HeatExchangerDesign_HTR_UA_des_in_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_HeatExchangerDesign_HTR_design_code_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_HeatExchangerDesign_HTR_eff_des_in_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_HeatExchangerDesign_HTR_min_dT_des_in_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_HeatExchangerDesign_HTR_n_sub_hx_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_HeatExchangerDesign_HTR_od_model_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_HeatExchangerDesign_HT_recup_eff_max_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_HeatExchangerDesign_LTR_HP_deltaP_des_in_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_HeatExchangerDesign_LTR_LP_deltaP_des_in_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_HeatExchangerDesign_LTR_UA_des_in_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_HeatExchangerDesign_LTR_design_code_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_HeatExchangerDesign_LTR_eff_des_in_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_HeatExchangerDesign_LTR_min_dT_des_in_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_HeatExchangerDesign_LTR_n_sub_hx_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_HeatExchangerDesign_LTR_od_model_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_HeatExchangerDesign_LT_recup_eff_max_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_HeatExchangerDesign_UA_recup_tot_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_HeatExchangerDesign_cycle_config_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_HeatExchangerDesign_des_objective_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_HeatExchangerDesign_is_IP_fixed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_HeatExchangerDesign_is_PR_fixed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_HeatExchangerDesign_is_P_high_fixed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_HeatExchangerDesign_is_recomp_ok_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_HeatExchangerDesign_min_phx_deltaT_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_HeatExchangerDesign_rel_tol_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Common Getters
	 */

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Common_PHX_co2_deltaP_des_in_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Common_P_high_limit_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Common_T_amb_high_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Common_T_amb_low_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Common_T_htf_hot_high_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Common_T_htf_hot_low_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Common_deltaP_counterHX_frac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Common_eta_isen_mc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Common_eta_isen_pc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Common_eta_isen_rc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Common_eta_isen_t_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Common_is_apply_default_htf_mins_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Common_is_generate_udpc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Common_m_dot_htf_ND_high_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Common_m_dot_htf_ND_low_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Common_mc_comp_type_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Common_n_T_amb_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Common_n_T_htf_hot_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Common_n_m_dot_htf_ND_nget(SAM_table ptr, SAM_error *err);


	/**
	 * PHXDesign Getters
	 */

	SAM_EXPORT double SAM_Sco2CspUdPcTables_PHXDesign_PHX_n_sub_hx_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_PHXDesign_PHX_od_model_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_PHXDesign_dT_PHX_cold_approach_nget(SAM_table ptr, SAM_error *err);


	/**
	 * AirCoolerDesign Getters
	 */

	SAM_EXPORT double SAM_Sco2CspUdPcTables_AirCoolerDesign_N_nodes_air_cooler_pass_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_AirCoolerDesign_deltaP_cooler_frac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_AirCoolerDesign_eta_air_cooler_fan_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_AirCoolerDesign_fan_power_frac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_AirCoolerDesign_is_design_air_cooler_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_HTR_HP_T_in_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_HTR_HP_deltaP_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_HTR_LP_T_out_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_HTR_LP_deltaP_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_HTR_UA_assigned_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_HTR_UA_calculated_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_HTR_cost_bare_erected_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_HTR_cost_equipment_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_HTR_min_dT_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_LTR_HP_T_out_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_LTR_HP_deltaP_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_LTR_LP_deltaP_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_LTR_UA_assigned_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_LTR_UA_calculated_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_LTR_cost_bare_erected_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_LTR_cost_equipment_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_LTR_min_dT_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_NTU_HTR_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_NTU_LTR_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_NTU_PHX_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_PHX_co2_deltaP_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_PHX_cost_bare_erected_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_PHX_cost_equipment_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_P_co2_PHX_in_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_P_comp_in_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_P_comp_out_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_P_mc_data_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_P_pc_data_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_P_rc_data_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_P_state_points_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_P_t_data_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_T_HTR_HP_data_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_T_HTR_LP_data_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_T_LTR_HP_data_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_T_LTR_LP_data_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_T_PHX_data_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_T_amb_ind_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_T_co2_PHX_in_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_T_comp_in_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_T_htf_cold_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_T_htf_ind_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_T_main_cooler_data_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_T_pre_cooler_data_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_T_state_points_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_T_turb_in_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_UA_PHX_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_W_dot_net_less_cooling_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_c_tot_W_dot_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_c_tot_cost_equip_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_cooler_tot_UA_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_cooler_tot_W_dot_fan_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_cooler_tot_cost_bare_erected_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_cooler_tot_cost_equipment_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_cycle_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_cycle_spec_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_cycle_spec_cost_thermal_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_deltaT_HTF_PHX_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_eff_HTR_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_eff_LTR_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_eff_PHX_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_eta_thermal_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_eta_thermal_net_less_cooling_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_h_mc_data_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_h_pc_data_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_h_rc_data_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_h_state_points_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_h_t_data_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_m_dot_co2_full_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_m_dot_htf_ND_ind_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_m_dot_htf_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_mc_D_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_mc_N_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_mc_T_out_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_mc_W_dot_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_mc_cooler_P_in_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_mc_cooler_T_in_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_mc_cooler_UA_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_mc_cooler_W_dot_fan_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_mc_cooler_co2_deltaP_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_mc_cooler_cost_bare_erected_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_mc_cooler_cost_equipment_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_mc_cooler_in_isen_deltah_to_P_mc_out_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_mc_cooler_m_dot_co2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_mc_cooler_q_dot_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_mc_cooler_rho_in_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_mc_cost_bare_erected_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_mc_cost_equipment_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_mc_eta_stages_des_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_mc_ideal_spec_work_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_mc_m_dot_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_mc_n_stages_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_mc_phi_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_mc_phi_surge_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_mc_psi_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_mc_psi_max_at_N_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_mc_rho_in_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_mc_tip_ratio_des_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_pc_D_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_pc_N_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_pc_P_in_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_pc_T_in_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_pc_W_dot_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_pc_cooler_P_in_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_pc_cooler_T_in_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_pc_cooler_UA_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_pc_cooler_W_dot_fan_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_pc_cooler_cost_bare_erected_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_pc_cooler_cost_equipment_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_pc_cooler_m_dot_co2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_pc_cooler_q_dot_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_pc_cost_bare_erected_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_pc_cost_equipment_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_pc_eta_stages_des_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_pc_ideal_spec_work_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_pc_m_dot_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_pc_n_stages_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_pc_phi_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_pc_phi_surge_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_pc_rho_in_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_pc_tip_ratio_des_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_piping_inventory_etc_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_q_dot_HTR_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_q_dot_LTR_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_q_dot_PHX_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_rc_D_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_rc_N_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_rc_P_in_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_rc_P_out_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_rc_T_in_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_rc_T_out_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_rc_W_dot_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_rc_cost_bare_erected_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_rc_cost_equipment_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_rc_eta_stages_des_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_rc_m_dot_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_rc_n_stages_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_rc_phi_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_rc_phi_surge_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_rc_psi_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_rc_psi_max_at_N_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_rc_tip_ratio_des_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_recomp_frac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_recup_LTR_UA_frac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_recup_total_UA_assigned_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_recup_total_UA_calculated_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_recup_total_cost_bare_erected_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_recup_total_cost_equipment_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_s_HTR_HP_data_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_s_HTR_LP_data_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_s_LTR_HP_data_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_s_LTR_LP_data_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_s_PHX_data_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_s_main_cooler_data_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_s_pre_cooler_data_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CspUdPcTables_Outputs_s_state_points_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_t_D_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_t_N_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_t_P_in_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_t_P_out_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_t_T_out_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_t_W_dot_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_t_cost_bare_erected_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_t_cost_equipment_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_t_delta_h_isen_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_t_m_dot_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_t_nu_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_t_rho_in_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CspUdPcTables_Outputs_t_tip_ratio_des_nget(SAM_table ptr, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif