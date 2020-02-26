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

	SAM_EXPORT SAM_UiUdpcChecks SAM_UiUdpcChecks_construct(const char* def, SAM_error* err);

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_UiUdpcChecks_execute(SAM_UiUdpcChecks data, int verbosity, SAM_error* err);

	SAM_EXPORT void SAM_UiUdpcChecks_destruct(SAM_UiUdpcChecks system);


	//
	// UserDefinedPowerCycle parameters
	//

	/**
	 * Set ud_ind_od: Off design user-defined power cycle performance as function of T_htf, m_dot_htf [ND], and T_amb
	 * options: None
	 * constraints: None
	 * required if: ?=[[0]]
	 */
	SAM_EXPORT void SAM_UiUdpcChecks_UserDefinedPowerCycle_ud_ind_od_mset(SAM_UiUdpcChecks ptr, double* mat, int nrows, int ncols, SAM_error *err);


	/**
	 * UserDefinedPowerCycle Getters
	 */

	SAM_EXPORT double* SAM_UiUdpcChecks_UserDefinedPowerCycle_ud_ind_od_mget(SAM_UiUdpcChecks ptr, int* nrows, int* ncols, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_T_amb_des_nget(SAM_UiUdpcChecks ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_T_amb_high_nget(SAM_UiUdpcChecks ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_T_amb_low_nget(SAM_UiUdpcChecks ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_T_htf_des_nget(SAM_UiUdpcChecks ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_T_htf_high_nget(SAM_UiUdpcChecks ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_T_htf_low_nget(SAM_UiUdpcChecks ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_m_dot_des_nget(SAM_UiUdpcChecks ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_m_dot_high_nget(SAM_UiUdpcChecks ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_m_dot_low_nget(SAM_UiUdpcChecks ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_n_T_amb_pars_nget(SAM_UiUdpcChecks ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_n_T_htf_pars_nget(SAM_UiUdpcChecks ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiUdpcChecks_Outputs_n_m_dot_pars_nget(SAM_UiUdpcChecks ptr, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif