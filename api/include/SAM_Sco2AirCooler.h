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

	SAM_EXPORT SAM_Sco2AirCooler SAM_Sco2AirCooler_construct(const char* def, SAM_error* err);

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_Sco2AirCooler_execute(SAM_Sco2AirCooler data, int verbosity, SAM_error* err);

	SAM_EXPORT void SAM_Sco2AirCooler_destruct(SAM_Sco2AirCooler system);


	//
	// Common parameters
	//

	/**
	 * Set P_co2_hot_in: Pressure of CO2 at inlet to cooler [MPa]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Sco2AirCooler_Common_P_co2_hot_in_nset(SAM_Sco2AirCooler ptr, double number, SAM_error *err);

	/**
	 * Set T_amb: Ambient temperature at design [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Sco2AirCooler_Common_T_amb_nset(SAM_Sco2AirCooler ptr, double number, SAM_error *err);

	/**
	 * Set T_co2_cold_out: Cold temperature of CO2 at cooler exit [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Sco2AirCooler_Common_T_co2_cold_out_nset(SAM_Sco2AirCooler ptr, double number, SAM_error *err);

	/**
	 * Set T_co2_hot_in: Hot temperature of CO2 at inlet to cooler [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Sco2AirCooler_Common_T_co2_hot_in_nset(SAM_Sco2AirCooler ptr, double number, SAM_error *err);

	/**
	 * Set W_dot_fan: Air fan power [MWe]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Sco2AirCooler_Common_W_dot_fan_nset(SAM_Sco2AirCooler ptr, double number, SAM_error *err);

	/**
	 * Set deltaP: Pressure drop of CO2 through cooler [MPa]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Sco2AirCooler_Common_deltaP_nset(SAM_Sco2AirCooler ptr, double number, SAM_error *err);

	/**
	 * Set q_dot_reject: Heat rejected from CO2 stream [MWt]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Sco2AirCooler_Common_q_dot_reject_nset(SAM_Sco2AirCooler ptr, double number, SAM_error *err);

	/**
	 * Set site_elevation: Site elevation [m]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Sco2AirCooler_Common_site_elevation_nset(SAM_Sco2AirCooler ptr, double number, SAM_error *err);


	/**
	 * Common Getters
	 */

	SAM_EXPORT double SAM_Sco2AirCooler_Common_P_co2_hot_in_nget(SAM_Sco2AirCooler ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2AirCooler_Common_T_amb_nget(SAM_Sco2AirCooler ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2AirCooler_Common_T_co2_cold_out_nget(SAM_Sco2AirCooler ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2AirCooler_Common_T_co2_hot_in_nget(SAM_Sco2AirCooler ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2AirCooler_Common_W_dot_fan_nget(SAM_Sco2AirCooler ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2AirCooler_Common_deltaP_nget(SAM_Sco2AirCooler ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2AirCooler_Common_q_dot_reject_nget(SAM_Sco2AirCooler ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2AirCooler_Common_site_elevation_nget(SAM_Sco2AirCooler ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double SAM_Sco2AirCooler_Outputs_UA_total_nget(SAM_Sco2AirCooler ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2AirCooler_Outputs_d_tube_in_nget(SAM_Sco2AirCooler ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2AirCooler_Outputs_d_tube_out_nget(SAM_Sco2AirCooler ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2AirCooler_Outputs_depth_footprint_nget(SAM_Sco2AirCooler ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2AirCooler_Outputs_length_nget(SAM_Sco2AirCooler ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2AirCooler_Outputs_m_V_hx_material_nget(SAM_Sco2AirCooler ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2AirCooler_Outputs_n_passes_series_nget(SAM_Sco2AirCooler ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2AirCooler_Outputs_number_of_tubes_nget(SAM_Sco2AirCooler ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2AirCooler_Outputs_parallel_paths_nget(SAM_Sco2AirCooler ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2AirCooler_Outputs_width_footprint_nget(SAM_Sco2AirCooler ptr, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif