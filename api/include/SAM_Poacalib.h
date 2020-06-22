#ifndef SAM_POACALIB_H_
#define SAM_POACALIB_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// Poacalib Technology Model
	//

	/** 
	 * Create a Poacalib variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_Poacalib;

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_Poacalib_execute(SAM_table data, int verbosity, SAM_error* err);


	//
	// POACalibrate parameters
	//

	/**
	 * Set albedo: Albedo
	 * options: None
	 * constraints: MIN=0,MAX=1
	 * required if: *
	 */
	SAM_EXPORT void SAM_Poacalib_POACalibrate_albedo_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set array_az: Array Azimuth [degrees]
	 * options: 0=N, 90=E, 180=S
	 * constraints: MIN=0,MAX=360
	 * required if: *
	 */
	SAM_EXPORT void SAM_Poacalib_POACalibrate_array_az_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set array_tilt: Array tilt [degrees]
	 * options: 0-90
	 * constraints: MIN=0,MAX=90
	 * required if: *
	 */
	SAM_EXPORT void SAM_Poacalib_POACalibrate_array_tilt_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set beam: Beam Irradiation [W/m^2]
	 * options: None
	 * constraints: LENGTH=8760
	 * required if: *
	 */
	SAM_EXPORT void SAM_Poacalib_POACalibrate_beam_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set diffuse: Diffuse Irradiation [W/m^2]
	 * options: None
	 * constraints: LENGTH=8760
	 * required if: *
	 */
	SAM_EXPORT void SAM_Poacalib_POACalibrate_diffuse_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set latitude: Latitude [decimal degrees]
	 * options: N= positive
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Poacalib_POACalibrate_latitude_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set longitude: Longitude [decimal degrees]
	 * options: E= positive
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Poacalib_POACalibrate_longitude_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set poa: Plane of Array [W/m^2]
	 * options: None
	 * constraints: LENGTH=8760
	 * required if: *
	 */
	SAM_EXPORT void SAM_Poacalib_POACalibrate_poa_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set time_zone: Time Zone
	 * options: -7= Denver
	 * constraints: MIN=-12,MAX=12
	 * required if: *
	 */
	SAM_EXPORT void SAM_Poacalib_POACalibrate_time_zone_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set year: Year
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Poacalib_POACalibrate_year_nset(SAM_table ptr, double number, SAM_error *err);


	/**
	 * POACalibrate Getters
	 */

	SAM_EXPORT double SAM_Poacalib_POACalibrate_albedo_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Poacalib_POACalibrate_array_az_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Poacalib_POACalibrate_array_tilt_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Poacalib_POACalibrate_beam_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Poacalib_POACalibrate_diffuse_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Poacalib_POACalibrate_latitude_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Poacalib_POACalibrate_longitude_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Poacalib_POACalibrate_poa_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Poacalib_POACalibrate_time_zone_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Poacalib_POACalibrate_year_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double* SAM_Poacalib_Outputs_pcalc_aget(SAM_table ptr, int* length, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif