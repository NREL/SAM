#ifndef SAM_SCO2AIRCOOLER_H_
#define SAM_SCO2AIRCOOLER_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// Sco2AirCooler Technology Model
	//

	/** 
	 * Create a Sco2AirCooler variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_Sco2AirCooler;

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_Sco2AirCooler_execute(SAM_table data, int verbosity, SAM_error* err);


	//
	// Common parameters
	//

	/**
	 * Set P_co2_hot_des: Pressure of CO2 at inlet to cooler [MPa]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Sco2AirCooler_Common_P_co2_hot_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_amb_des: Ambient temperature at design [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Sco2AirCooler_Common_T_amb_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_co2_cold_des: Cold temperature of CO2 at cooler exit [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Sco2AirCooler_Common_T_co2_cold_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_co2_hot_des: Hot temperature of CO2 at inlet to cooler [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Sco2AirCooler_Common_T_co2_hot_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set W_dot_fan_des: Air fan power [MWe]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Sco2AirCooler_Common_W_dot_fan_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set deltaP_co2_des: Pressure drop of CO2 through cooler [MPa]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Sco2AirCooler_Common_deltaP_co2_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set od_calc_T_co2_cold: Columns: T_co2_hot_C, P_co2_hot_MPa, W_dot_fan_ND, m_dot_CO2_ND, T_amb_C. Rows: cases
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Sco2AirCooler_Common_od_calc_T_co2_cold_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set od_calc_W_dot_fan: Columns: T_co2_hot_C, P_co2_hot_MPa, T_co2_cold_C, m_dot_CO2_ND, T_amb_C. Rows: cases
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Sco2AirCooler_Common_od_calc_W_dot_fan_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set q_dot_des: Heat rejected from CO2 stream [MWt]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Sco2AirCooler_Common_q_dot_des_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set site_elevation: Site elevation [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Sco2AirCooler_Common_site_elevation_nset(SAM_table ptr, double number, SAM_error *err);


	/**
	 * Common Getters
	 */

	SAM_EXPORT double SAM_Sco2AirCooler_Common_P_co2_hot_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2AirCooler_Common_T_amb_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2AirCooler_Common_T_co2_cold_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2AirCooler_Common_T_co2_hot_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2AirCooler_Common_W_dot_fan_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2AirCooler_Common_deltaP_co2_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2AirCooler_Common_od_calc_T_co2_cold_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2AirCooler_Common_od_calc_W_dot_fan_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Sco2AirCooler_Common_q_dot_des_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2AirCooler_Common_site_elevation_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double* SAM_Sco2AirCooler_Outputs_P_co2_cold_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2AirCooler_Outputs_P_co2_hot_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2AirCooler_Outputs_T_amb_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2AirCooler_Outputs_T_co2_cold_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2AirCooler_Outputs_T_co2_hot_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2AirCooler_Outputs_UA_total_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2AirCooler_Outputs_W_dot_fan_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2AirCooler_Outputs_W_dot_fan_od_ND_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2AirCooler_Outputs_d_tube_in_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2AirCooler_Outputs_d_tube_out_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2AirCooler_Outputs_deltaP_co2_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2AirCooler_Outputs_depth_footprint_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2AirCooler_Outputs_length_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2AirCooler_Outputs_m_V_hx_material_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2AirCooler_Outputs_m_dot_co2_od_ND_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2AirCooler_Outputs_n_passes_series_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2AirCooler_Outputs_number_of_tubes_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2AirCooler_Outputs_parallel_paths_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2AirCooler_Outputs_q_dot_od_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2AirCooler_Outputs_q_dot_od_ND_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2AirCooler_Outputs_width_footprint_nget(SAM_table ptr, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif