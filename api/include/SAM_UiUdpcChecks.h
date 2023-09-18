#ifndef SAM_UIUDPCCHECKS_H_
#define SAM_UIUDPCCHECKS_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// UiUdpcChecks Technology Model
	//

	/** 
	 * Create a UiUdpcChecks variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_UiUdpcChecks;

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_UiUdpcChecks_execute(SAM_table data, int verbosity, SAM_error* err);


	//
	// UserDefinedPowerCycle parameters
	//

	/**
	 * Set ud_ind_od: Off design user-defined power cycle performance as function of T_htf, m_dot_htf [ND], and T_amb
	 * options: None
	 * constraints: None
	 * required if: ?=[[0]]
	 */
	SAM_EXPORT void SAM_UiUdpcChecks_UserDefinedPowerCycle_ud_ind_od_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);


	//
	// Common parameters
	//

	/**
	 * Set T_htf_cold_des: Cold outlet HTF design temperature [C]
	 * options: None
	 * constraints: None
	 * required if: is_calc_m_dot_vs_T_amb=1
	 */
	SAM_EXPORT void SAM_UiUdpcChecks_Common_T_htf_cold_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_htf_des_in: Input HTF design temperature [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_UiUdpcChecks_Common_T_htf_des_in_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cooler_tot_W_dot_fan: Total cooler fan power [MWe]
	 * options: Cooler Totals
	 * constraints: None
	 * required if: is_calc_m_dot_vs_T_amb=1
	 */
	SAM_EXPORT void SAM_UiUdpcChecks_Common_cooler_tot_W_dot_fan_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set is_calc_m_dot_vs_T_amb: 0 (defalt) no; 1: return array of max m_dot vs T_amb
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_UiUdpcChecks_Common_is_calc_m_dot_vs_T_amb_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// SystemDesign parameters
	//

	/**
	 * Set W_dot_net_des: Design cycle power output (no cooling parasitics) [MWe]
	 * options: None
	 * constraints: None
	 * required if: is_calc_m_dot_vs_T_amb=1
	 */
	SAM_EXPORT void SAM_UiUdpcChecks_SystemDesign_W_dot_net_des_nset(SAM_table ptr, double number, SAM_error *err);


	/**
	 * UserDefinedPowerCycle Getters
	 */

	SAM_EXPORT double* SAM_UiUdpcChecks_UserDefinedPowerCycle_ud_ind_od_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);


	/**
	 * Common Getters
	 */

	SAM_EXPORT double SAM_UiUdpcChecks_Common_T_htf_cold_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Common_T_htf_des_in_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Common_cooler_tot_W_dot_fan_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Common_is_calc_m_dot_vs_T_amb_nget(SAM_table ptr, SAM_error *err);


	/**
	 * SystemDesign Getters
	 */

	SAM_EXPORT double SAM_UiUdpcChecks_SystemDesign_W_dot_net_des_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_Q_dot_HTF_ND_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_T_amb_HT_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_T_amb_LT_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_T_amb_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_T_amb_high_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_T_amb_low_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_UiUdpcChecks_Outputs_T_amb_pars_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_UiUdpcChecks_Outputs_T_amb_sweep_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_T_htf_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_T_htf_high_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_T_htf_low_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_UiUdpcChecks_Outputs_T_htf_pars_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_UiUdpcChecks_Outputs_W_dot_ND_regr_vs_T_amb__T_HTF_low_level_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_UiUdpcChecks_Outputs_W_dot_ND_regr_vs_m_dot__T_amb_HT_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_UiUdpcChecks_Outputs_W_dot_ND_regr_vs_m_dot__T_amb_LT_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_UiUdpcChecks_Outputs_W_dot_ND_regr_vs_m_dot__T_amb_design_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_UiUdpcChecks_Outputs_W_dot_ND_regr_vs_m_dot__T_amb_high_level_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_UiUdpcChecks_Outputs_W_dot_ND_regr_vs_m_dot__T_amb_low_level_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_UiUdpcChecks_Outputs_W_dot_ND_vs_m_dot__T_amb_HT_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_UiUdpcChecks_Outputs_W_dot_ND_vs_m_dot__T_amb_LT_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_W_dot_cooling_ND_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_W_dot_gross_ND_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_W_dot_htf_ND_max_at_T_amb_HT_regr_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_W_dot_htf_ND_max_at_T_amb_HT_rule0_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_W_dot_htf_ND_max_at_T_amb_LT_regr_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_W_dot_htf_ND_max_at_T_amb_LT_rule0_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_W_dot_htf_ND_max_at_T_amb_design_regr_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_W_dot_htf_ND_max_at_T_amb_design_rule0_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_W_dot_htf_ND_max_at_T_amb_high_level_regr_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_W_dot_htf_ND_max_at_T_amb_high_level_rule0_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_W_dot_htf_ND_max_at_T_amb_low_level_regr_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_W_dot_htf_ND_max_at_T_amb_low_level_rule0_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_eta_ND_max_at_T_amb_HT_regr_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_eta_ND_max_at_T_amb_HT_rule0_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_eta_ND_max_at_T_amb_LT_regr_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_eta_ND_max_at_T_amb_LT_rule0_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_eta_ND_max_at_T_amb_design_regr_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_eta_ND_max_at_T_amb_design_rule0_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_eta_ND_max_at_T_amb_high_level_regr_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_eta_ND_max_at_T_amb_high_level_rule0_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_eta_ND_max_at_T_amb_low_level_regr_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_eta_ND_max_at_T_amb_low_level_rule0_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_UiUdpcChecks_Outputs_eta_ND_regr_vs_T_amb__T_HTF_low_level_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_UiUdpcChecks_Outputs_eta_ND_regr_vs_m_dot__T_amb_HT_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_UiUdpcChecks_Outputs_eta_ND_regr_vs_m_dot__T_amb_LT_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_UiUdpcChecks_Outputs_eta_ND_regr_vs_m_dot__T_amb_design_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_UiUdpcChecks_Outputs_eta_ND_regr_vs_m_dot__T_amb_high_level_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_UiUdpcChecks_Outputs_eta_ND_regr_vs_m_dot__T_amb_low_level_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_UiUdpcChecks_Outputs_eta_ND_vs_m_dot__T_amb_HT_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_UiUdpcChecks_Outputs_eta_ND_vs_m_dot__T_amb_LT_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_m_dot_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_m_dot_high_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_m_dot_htf_ND_max_at_T_amb_HT_regr_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_m_dot_htf_ND_max_at_T_amb_HT_rule0_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_m_dot_htf_ND_max_at_T_amb_LT_regr_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_m_dot_htf_ND_max_at_T_amb_LT_rule0_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_m_dot_htf_ND_max_at_T_amb_design_regr_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_m_dot_htf_ND_max_at_T_amb_design_rule0_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_m_dot_htf_ND_max_at_T_amb_high_level_regr_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_m_dot_htf_ND_max_at_T_amb_high_level_rule0_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_m_dot_htf_ND_max_at_T_amb_low_level_regr_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_m_dot_htf_ND_max_at_T_amb_low_level_rule0_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_UiUdpcChecks_Outputs_m_dot_htf_ND_max_vs_T_amb_rule0_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_m_dot_low_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_UiUdpcChecks_Outputs_m_dot_pars_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_m_dot_water_ND_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_n_T_amb_pars_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_n_T_htf_pars_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_n_m_dot_pars_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_UiUdpcChecks_Outputs_q_dot_ND_regr_vs_T_amb__T_HTF_low_level_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_UiUdpcChecks_Outputs_q_dot_ND_regr_vs_m_dot__T_amb_HT_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_UiUdpcChecks_Outputs_q_dot_ND_regr_vs_m_dot__T_amb_LT_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_UiUdpcChecks_Outputs_q_dot_ND_regr_vs_m_dot__T_amb_design_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_UiUdpcChecks_Outputs_q_dot_ND_regr_vs_m_dot__T_amb_high_level_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_UiUdpcChecks_Outputs_q_dot_ND_regr_vs_m_dot__T_amb_low_level_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_UiUdpcChecks_Outputs_q_dot_ND_vs_m_dot__T_amb_HT_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_UiUdpcChecks_Outputs_q_dot_ND_vs_m_dot__T_amb_LT_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_q_dot_htf_ND_max_at_T_amb_HT_regr_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_q_dot_htf_ND_max_at_T_amb_HT_rule0_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_q_dot_htf_ND_max_at_T_amb_LT_regr_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_q_dot_htf_ND_max_at_T_amb_LT_rule0_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_q_dot_htf_ND_max_at_T_amb_design_regr_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_q_dot_htf_ND_max_at_T_amb_design_rule0_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_q_dot_htf_ND_max_at_T_amb_high_level_regr_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_q_dot_htf_ND_max_at_T_amb_high_level_rule0_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_q_dot_htf_ND_max_at_T_amb_low_level_regr_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_q_dot_htf_ND_max_at_T_amb_low_level_rule0_nget(SAM_table ptr, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif