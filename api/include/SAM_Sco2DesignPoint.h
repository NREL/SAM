#ifndef SAM_SCO2DESIGNPOINT_H_
#define SAM_SCO2DESIGNPOINT_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// Sco2DesignPoint Technology Model
	//

	/** 
	 * Create a Sco2DesignPoint variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_Sco2DesignPoint;

	SAM_EXPORT SAM_Sco2DesignPoint SAM_Sco2DesignPoint_construct(const char* def, SAM_error* err);

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_Sco2DesignPoint_execute(SAM_Sco2DesignPoint data, int verbosity, SAM_error* err);

	SAM_EXPORT void SAM_Sco2DesignPoint_destruct(SAM_Sco2DesignPoint system);


	//
	// Common parameters
	//

	/**
	 * Set P_high_limit: High pressure limit in cycle [MPa]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Sco2DesignPoint_Common_P_high_limit_nset(SAM_Sco2DesignPoint ptr, double number, SAM_error *err);

	/**
	 * Set T_amb_array: Array of ambient temperatures for off-design parametric [C]
	 * options: None
	 * constraints: None
	 * required if: run_off_des_study=1
	 */
	SAM_EXPORT void SAM_Sco2DesignPoint_Common_T_amb_array_aset(SAM_Sco2DesignPoint ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set T_amb_des: Design: Ambient temperature for air cooler [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Sco2DesignPoint_Common_T_amb_des_nset(SAM_Sco2DesignPoint ptr, double number, SAM_error *err);

	/**
	 * Set T_htf_hot_des: Tower design outlet temp [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Sco2DesignPoint_Common_T_htf_hot_des_nset(SAM_Sco2DesignPoint ptr, double number, SAM_error *err);

	/**
	 * Set W_dot_net_des: Design cycle power output [MW]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Sco2DesignPoint_Common_W_dot_net_des_nset(SAM_Sco2DesignPoint ptr, double number, SAM_error *err);

	/**
	 * Set deltaT_ACC: Temp diff btw ambient air and compressor inlet [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Sco2DesignPoint_Common_deltaT_ACC_nset(SAM_Sco2DesignPoint ptr, double number, SAM_error *err);

	/**
	 * Set deltaT_PHX: Temp diff btw hot HTF and turbine inlet [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Sco2DesignPoint_Common_deltaT_PHX_nset(SAM_Sco2DesignPoint ptr, double number, SAM_error *err);

	/**
	 * Set eta_c: Design compressor(s) isentropic efficiency [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Sco2DesignPoint_Common_eta_c_nset(SAM_Sco2DesignPoint ptr, double number, SAM_error *err);

	/**
	 * Set eta_des: Power cycle thermal efficiency
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Sco2DesignPoint_Common_eta_des_nset(SAM_Sco2DesignPoint ptr, double number, SAM_error *err);

	/**
	 * Set eta_t: Design turbine isentropic efficiency [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Sco2DesignPoint_Common_eta_t_nset(SAM_Sco2DesignPoint ptr, double number, SAM_error *err);

	/**
	 * Set part_load_fracs: Array of part load q_dot_in fractions for off-design parametric
	 * options: None
	 * constraints: None
	 * required if: run_off_des_study=1
	 */
	SAM_EXPORT void SAM_Sco2DesignPoint_Common_part_load_fracs_aset(SAM_Sco2DesignPoint ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set run_off_des_study: 1 = yes, 0/other = no
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Sco2DesignPoint_Common_run_off_des_study_nset(SAM_Sco2DesignPoint ptr, double number, SAM_error *err);


	/**
	 * Common Getters
	 */

	SAM_EXPORT double SAM_Sco2DesignPoint_Common_P_high_limit_nget(SAM_Sco2DesignPoint ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2DesignPoint_Common_T_amb_array_aget(SAM_Sco2DesignPoint ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2DesignPoint_Common_T_amb_des_nget(SAM_Sco2DesignPoint ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2DesignPoint_Common_T_htf_hot_des_nget(SAM_Sco2DesignPoint ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2DesignPoint_Common_W_dot_net_des_nget(SAM_Sco2DesignPoint ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2DesignPoint_Common_deltaT_ACC_nget(SAM_Sco2DesignPoint ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2DesignPoint_Common_deltaT_PHX_nget(SAM_Sco2DesignPoint ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2DesignPoint_Common_eta_c_nget(SAM_Sco2DesignPoint ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2DesignPoint_Common_eta_des_nget(SAM_Sco2DesignPoint ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2DesignPoint_Common_eta_t_nget(SAM_Sco2DesignPoint ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2DesignPoint_Common_part_load_fracs_aget(SAM_Sco2DesignPoint ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2DesignPoint_Common_run_off_des_study_nget(SAM_Sco2DesignPoint ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double SAM_Sco2DesignPoint_Outputs_P_comp_in_nget(SAM_Sco2DesignPoint ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2DesignPoint_Outputs_P_comp_out_nget(SAM_Sco2DesignPoint ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2DesignPoint_Outputs_T_amb_array_out_aget(SAM_Sco2DesignPoint ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2DesignPoint_Outputs_T_amb_coefs_aget(SAM_Sco2DesignPoint ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2DesignPoint_Outputs_T_amb_eta_aget(SAM_Sco2DesignPoint ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2DesignPoint_Outputs_T_amb_r_squared_nget(SAM_Sco2DesignPoint ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2DesignPoint_Outputs_T_htf_cold_nget(SAM_Sco2DesignPoint ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2DesignPoint_Outputs_UA_total_nget(SAM_Sco2DesignPoint ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2DesignPoint_Outputs_eta_thermal_calc_nget(SAM_Sco2DesignPoint ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2DesignPoint_Outputs_part_load_coefs_aget(SAM_Sco2DesignPoint ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2DesignPoint_Outputs_part_load_eta_aget(SAM_Sco2DesignPoint ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2DesignPoint_Outputs_part_load_fracs_out_aget(SAM_Sco2DesignPoint ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2DesignPoint_Outputs_part_load_r_squared_nget(SAM_Sco2DesignPoint ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2DesignPoint_Outputs_recomp_frac_nget(SAM_Sco2DesignPoint ptr, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif