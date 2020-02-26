#ifndef SAM_UITESCALCS_H_
#define SAM_UITESCALCS_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// UiTesCalcs Technology Model
	//

	/** 
	 * Create a UiTesCalcs variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_UiTesCalcs;

	SAM_EXPORT SAM_UiTesCalcs SAM_UiTesCalcs_construct(const char* def, SAM_error* err);

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_UiTesCalcs_execute(SAM_UiTesCalcs data, int verbosity, SAM_error* err);

	SAM_EXPORT void SAM_UiTesCalcs_destruct(SAM_UiTesCalcs system);


	//
	// Common parameters
	//

	/**
	 * Set TES_HTF_code: TES storage fluid code
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_UiTesCalcs_Common_TES_HTF_code_nset(SAM_UiTesCalcs ptr, double number, SAM_error *err);

	/**
	 * Set TES_HTF_props: User defined tes storage fluid prop data
	 * options: 7 columns (T,Cp,dens,visc,kvisc,cond,h), at least 3 rows
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_UiTesCalcs_Common_TES_HTF_props_mset(SAM_UiTesCalcs ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set T_HTF_cold: Cold HTF temp (out of TES HX, if applicable) [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_UiTesCalcs_Common_T_HTF_cold_nset(SAM_UiTesCalcs ptr, double number, SAM_error *err);

	/**
	 * Set T_HTF_hot: Hot HTF temp (into TES HX, if applicable) [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_UiTesCalcs_Common_T_HTF_hot_nset(SAM_UiTesCalcs ptr, double number, SAM_error *err);

	/**
	 * Set W_dot_pb_des: Power cycle output at design [MWe]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_UiTesCalcs_Common_W_dot_pb_des_nset(SAM_UiTesCalcs ptr, double number, SAM_error *err);

	/**
	 * Set eta_pb_des: Power cycle thermal efficiency
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_UiTesCalcs_Common_eta_pb_des_nset(SAM_UiTesCalcs ptr, double number, SAM_error *err);

	/**
	 * Set h_tank: Total height of tank (HTF when tank is full [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_UiTesCalcs_Common_h_tank_nset(SAM_UiTesCalcs ptr, double number, SAM_error *err);

	/**
	 * Set h_tank_min: Min. allowable HTF height in storage tank [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_UiTesCalcs_Common_h_tank_min_nset(SAM_UiTesCalcs ptr, double number, SAM_error *err);

	/**
	 * Set tank_pairs: Number of equivalent tank pairs
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_UiTesCalcs_Common_tank_pairs_nset(SAM_UiTesCalcs ptr, double number, SAM_error *err);

	/**
	 * Set tes_hrs: Hours of TES relative to q_dot_pb_des [hr]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_UiTesCalcs_Common_tes_hrs_nset(SAM_UiTesCalcs ptr, double number, SAM_error *err);

	/**
	 * Set u_tank: Loss coefficient from the tank [W/m2-K]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_UiTesCalcs_Common_u_tank_nset(SAM_UiTesCalcs ptr, double number, SAM_error *err);


	/**
	 * Common Getters
	 */

	SAM_EXPORT double SAM_UiTesCalcs_Common_TES_HTF_code_nget(SAM_UiTesCalcs ptr, SAM_error *err);

	SAM_EXPORT double* SAM_UiTesCalcs_Common_TES_HTF_props_mget(SAM_UiTesCalcs ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_UiTesCalcs_Common_T_HTF_cold_nget(SAM_UiTesCalcs ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiTesCalcs_Common_T_HTF_hot_nget(SAM_UiTesCalcs ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiTesCalcs_Common_W_dot_pb_des_nget(SAM_UiTesCalcs ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiTesCalcs_Common_eta_pb_des_nget(SAM_UiTesCalcs ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiTesCalcs_Common_h_tank_nget(SAM_UiTesCalcs ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiTesCalcs_Common_h_tank_min_nget(SAM_UiTesCalcs ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiTesCalcs_Common_tank_pairs_nget(SAM_UiTesCalcs ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiTesCalcs_Common_tes_hrs_nget(SAM_UiTesCalcs ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiTesCalcs_Common_u_tank_nget(SAM_UiTesCalcs ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double SAM_UiTesCalcs_Outputs_HTF_dens_nget(SAM_UiTesCalcs ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiTesCalcs_Outputs_d_tank_nget(SAM_UiTesCalcs ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiTesCalcs_Outputs_q_dot_loss_nget(SAM_UiTesCalcs ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiTesCalcs_Outputs_q_tes_des_nget(SAM_UiTesCalcs ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiTesCalcs_Outputs_vol_one_temp_avail_nget(SAM_UiTesCalcs ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiTesCalcs_Outputs_vol_one_temp_total_nget(SAM_UiTesCalcs ptr, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif