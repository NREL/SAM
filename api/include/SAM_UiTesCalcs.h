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

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_UiTesCalcs_execute(SAM_table data, int verbosity, SAM_error* err);


	//
	// Common parameters
	//

	/**
	 * Set P_ref: Power cycle output at design [MWe]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_UiTesCalcs_Common_P_ref_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_htf_cold_des: Cold design HTF temp into field [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_UiTesCalcs_Common_T_htf_cold_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_htf_hot_des: Hot design HTF temp from field [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_UiTesCalcs_Common_T_htf_hot_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set design_eff: Power cycle thermal efficiency
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_UiTesCalcs_Common_design_eff_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dt_hot: Heat exchanger approach temperature
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_UiTesCalcs_Common_dt_hot_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set field_fl_props: User defined field fluid prop data
	 * options: 7 columns (T,Cp,dens,visc,kvisc,cond,h), at least 3 rows
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_UiTesCalcs_Common_field_fl_props_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set field_fluid: Field fluid code
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_UiTesCalcs_Common_field_fluid_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set h_tank: Total height of tank (HTF when tank is full [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_UiTesCalcs_Common_h_tank_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set h_tank_min: Min. allowable HTF height in storage tank [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_UiTesCalcs_Common_h_tank_min_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set store_fl_props: User defined tes storage fluid prop data
	 * options: 7 columns (T,Cp,dens,visc,kvisc,cond,h), at least 3 rows
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_UiTesCalcs_Common_store_fl_props_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set store_fluid: TES storage fluid code
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_UiTesCalcs_Common_store_fluid_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tank_pairs: Number of equivalent tank pairs
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_UiTesCalcs_Common_tank_pairs_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tshours: Hours of TES relative to q_dot_pb_des [hr]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_UiTesCalcs_Common_tshours_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set u_tank: Loss coefficient from the tank [W/m2-K]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_UiTesCalcs_Common_u_tank_nset(SAM_table ptr, double number, SAM_error *err);


	/**
	 * Common Getters
	 */

	SAM_EXPORT double SAM_UiTesCalcs_Common_P_ref_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiTesCalcs_Common_T_htf_cold_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiTesCalcs_Common_T_htf_hot_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiTesCalcs_Common_design_eff_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiTesCalcs_Common_dt_hot_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_UiTesCalcs_Common_field_fl_props_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_UiTesCalcs_Common_field_fluid_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiTesCalcs_Common_h_tank_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiTesCalcs_Common_h_tank_min_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_UiTesCalcs_Common_store_fl_props_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_UiTesCalcs_Common_store_fluid_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiTesCalcs_Common_tank_pairs_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiTesCalcs_Common_tshours_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiTesCalcs_Common_u_tank_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double SAM_UiTesCalcs_Outputs_are_htfs_equal_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiTesCalcs_Outputs_csp_pt_tes_htf_density_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiTesCalcs_Outputs_csp_pt_tes_tank_diameter_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiTesCalcs_Outputs_q_dot_tes_est_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiTesCalcs_Outputs_q_tes_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiTesCalcs_Outputs_tes_avail_vol_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_UiTesCalcs_Outputs_vol_tank_nget(SAM_table ptr, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif