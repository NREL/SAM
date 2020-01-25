#ifndef SAM_PVWATTSV1POA_H_
#define SAM_PVWATTSV1POA_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// Pvwattsv1Poa Technology Model
	//

	/** 
	 * Create a Pvwattsv1Poa variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_Pvwattsv1Poa;

	SAM_EXPORT SAM_Pvwattsv1Poa SAM_Pvwattsv1Poa_construct(const char* def, SAM_error* err);

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_Pvwattsv1Poa_execute(SAM_Pvwattsv1Poa data, int verbosity, SAM_error* err);

	SAM_EXPORT void SAM_Pvwattsv1Poa_destruct(SAM_Pvwattsv1Poa system);


	//
	// Weather parameters
	//

	/**
	 * Set beam: Direct normal radiation [W/m2]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvwattsv1Poa_Weather_beam_aset(SAM_Pvwattsv1Poa ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set incidence: Incidence angle to surface [deg]
	 * options: None
	 * constraints: LENGTH_EQUAL=beam
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvwattsv1Poa_Weather_incidence_aset(SAM_Pvwattsv1Poa ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set poa_beam: Incident direct normal radiation [W/m2]
	 * options: None
	 * constraints: LENGTH_EQUAL=beam
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvwattsv1Poa_Weather_poa_beam_aset(SAM_Pvwattsv1Poa ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set poa_gnddiff: Incident ground diffuse irradiance [W/m2]
	 * options: None
	 * constraints: LENGTH_EQUAL=beam
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvwattsv1Poa_Weather_poa_gnddiff_aset(SAM_Pvwattsv1Poa ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set poa_skydiff: Incident sky diffuse radiation [W/m2]
	 * options: None
	 * constraints: LENGTH_EQUAL=beam
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvwattsv1Poa_Weather_poa_skydiff_aset(SAM_Pvwattsv1Poa ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set tdry: Dry bulb temperature ['C]
	 * options: None
	 * constraints: LENGTH_EQUAL=beam
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvwattsv1Poa_Weather_tdry_aset(SAM_Pvwattsv1Poa ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set wspd: Wind speed [m/s]
	 * options: None
	 * constraints: LENGTH_EQUAL=beam
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvwattsv1Poa_Weather_wspd_aset(SAM_Pvwattsv1Poa ptr, double* arr, int length, SAM_error *err);


	//
	// PVWatts parameters
	//

	/**
	 * Set derate: System derate value [frac]
	 * options: None
	 * constraints: MIN=0,MAX=1
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvwattsv1Poa_PVWatts_derate_nset(SAM_Pvwattsv1Poa ptr, double number, SAM_error *err);

	/**
	 * Set gamma: Max power temperature coefficient [%/'C]
	 * options: None
	 * constraints: None
	 * required if: ?=-0.5
	 */
	SAM_EXPORT void SAM_Pvwattsv1Poa_PVWatts_gamma_nset(SAM_Pvwattsv1Poa ptr, double number, SAM_error *err);

	/**
	 * Set inoct: Nominal operating cell temperature ['C]
	 * options: None
	 * constraints: POSITIVE
	 * required if: ?=45.0
	 */
	SAM_EXPORT void SAM_Pvwattsv1Poa_PVWatts_inoct_nset(SAM_Pvwattsv1Poa ptr, double number, SAM_error *err);

	/**
	 * Set inv_eff: Inverter efficiency at rated power [frac]
	 * options: None
	 * constraints: MIN=0,MAX=1
	 * required if: ?=0.92
	 */
	SAM_EXPORT void SAM_Pvwattsv1Poa_PVWatts_inv_eff_nset(SAM_Pvwattsv1Poa ptr, double number, SAM_error *err);

	/**
	 * Set step: Time step of input data [sec]
	 * options: None
	 * constraints: POSITIVE
	 * required if: ?=3600
	 */
	SAM_EXPORT void SAM_Pvwattsv1Poa_PVWatts_step_nset(SAM_Pvwattsv1Poa ptr, double number, SAM_error *err);

	/**
	 * Set system_size: Nameplate capacity [kW]
	 * options: None
	 * constraints: MIN=0.5,MAX=100000
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvwattsv1Poa_PVWatts_system_size_nset(SAM_Pvwattsv1Poa ptr, double number, SAM_error *err);

	/**
	 * Set t_ref: Reference cell temperature ['C]
	 * options: None
	 * constraints: POSITIVE
	 * required if: ?=25.0
	 */
	SAM_EXPORT void SAM_Pvwattsv1Poa_PVWatts_t_ref_nset(SAM_Pvwattsv1Poa ptr, double number, SAM_error *err);


	/**
	 * Weather Getters
	 */

	SAM_EXPORT double* SAM_Pvwattsv1Poa_Weather_beam_aget(SAM_Pvwattsv1Poa ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv1Poa_Weather_incidence_aget(SAM_Pvwattsv1Poa ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv1Poa_Weather_poa_beam_aget(SAM_Pvwattsv1Poa ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv1Poa_Weather_poa_gnddiff_aget(SAM_Pvwattsv1Poa ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv1Poa_Weather_poa_skydiff_aget(SAM_Pvwattsv1Poa ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv1Poa_Weather_tdry_aget(SAM_Pvwattsv1Poa ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv1Poa_Weather_wspd_aget(SAM_Pvwattsv1Poa ptr, int* length, SAM_error *err);


	/**
	 * PVWatts Getters
	 */

	SAM_EXPORT double SAM_Pvwattsv1Poa_PVWatts_derate_nget(SAM_Pvwattsv1Poa ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv1Poa_PVWatts_gamma_nget(SAM_Pvwattsv1Poa ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv1Poa_PVWatts_inoct_nget(SAM_Pvwattsv1Poa ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv1Poa_PVWatts_inv_eff_nget(SAM_Pvwattsv1Poa ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv1Poa_PVWatts_step_nget(SAM_Pvwattsv1Poa ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv1Poa_PVWatts_system_size_nget(SAM_Pvwattsv1Poa ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv1Poa_PVWatts_t_ref_nget(SAM_Pvwattsv1Poa ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double* SAM_Pvwattsv1Poa_Outputs_ac_aget(SAM_Pvwattsv1Poa ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv1Poa_Outputs_dc_aget(SAM_Pvwattsv1Poa ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv1Poa_Outputs_tcell_aget(SAM_Pvwattsv1Poa ptr, int* length, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif