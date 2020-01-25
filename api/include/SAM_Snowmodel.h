#ifndef SAM_SNOWMODEL_H_
#define SAM_SNOWMODEL_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// Snowmodel Technology Model
	//

	/** 
	 * Create a Snowmodel variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_Snowmodel;

	SAM_EXPORT SAM_Snowmodel SAM_Snowmodel_construct(const char* def, SAM_error* err);

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_Snowmodel_execute(SAM_Snowmodel data, int verbosity, SAM_error* err);

	SAM_EXPORT void SAM_Snowmodel_destruct(SAM_Snowmodel system);


	//
	// PVSnowModel parameters
	//

	/**
	 * Set snowdepth: Snow Depth [cm]
	 * options: None
	 * constraints: LENGTH=8760
	 * required if: *
	 */
	SAM_EXPORT void SAM_Snowmodel_PVSnowModel_snowdepth_aset(SAM_Snowmodel ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set subarray1_nmody: Number of Modules in a Row
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Snowmodel_PVSnowModel_subarray1_nmody_nset(SAM_Snowmodel ptr, double number, SAM_error *err);

	/**
	 * Set subarray1_poa_shaded: Plane of Array Incidence [W/m^2]
	 * options: None
	 * constraints: LENGTH=8760
	 * required if: *
	 */
	SAM_EXPORT void SAM_Snowmodel_PVSnowModel_subarray1_poa_shaded_aset(SAM_Snowmodel ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set subarray1_surf_tilt: Surface Tilt [Degrees]
	 * options: None
	 * constraints: LENGTH=8760
	 * required if: *
	 */
	SAM_EXPORT void SAM_Snowmodel_PVSnowModel_subarray1_surf_tilt_aset(SAM_Snowmodel ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set subarray1_tilt: Base tilt [Degrees]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Snowmodel_PVSnowModel_subarray1_tilt_nset(SAM_Snowmodel ptr, double number, SAM_error *err);

	/**
	 * Set subarray1_track_mode: Tracking Mode
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Snowmodel_PVSnowModel_subarray1_track_mode_nset(SAM_Snowmodel ptr, double number, SAM_error *err);

	/**
	 * Set tdry: Ambient Temperature [Degrees Celsius]
	 * options: None
	 * constraints: LENGTH=8760
	 * required if: *
	 */
	SAM_EXPORT void SAM_Snowmodel_PVSnowModel_tdry_aset(SAM_Snowmodel ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set wspd: Wind Speed [m/s]
	 * options: None
	 * constraints: LENGTH=8760
	 * required if: *
	 */
	SAM_EXPORT void SAM_Snowmodel_PVSnowModel_wspd_aset(SAM_Snowmodel ptr, double* arr, int length, SAM_error *err);


	//
	// TimeSeries parameters
	//

	/**
	 * Set hourly_gen: Hourly Energy [kwh]
	 * options: None
	 * constraints: LENGTH=8760
	 * required if: *
	 */
	SAM_EXPORT void SAM_Snowmodel_TimeSeries_hourly_gen_aset(SAM_Snowmodel ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set sunup: Sun up over horizon [0/1]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Snowmodel_TimeSeries_sunup_aset(SAM_Snowmodel ptr, double* arr, int length, SAM_error *err);


	/**
	 * PVSnowModel Getters
	 */

	SAM_EXPORT double* SAM_Snowmodel_PVSnowModel_snowdepth_aget(SAM_Snowmodel ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Snowmodel_PVSnowModel_subarray1_nmody_nget(SAM_Snowmodel ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Snowmodel_PVSnowModel_subarray1_poa_shaded_aget(SAM_Snowmodel ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Snowmodel_PVSnowModel_subarray1_surf_tilt_aget(SAM_Snowmodel ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Snowmodel_PVSnowModel_subarray1_tilt_nget(SAM_Snowmodel ptr, SAM_error *err);

	SAM_EXPORT double SAM_Snowmodel_PVSnowModel_subarray1_track_mode_nget(SAM_Snowmodel ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Snowmodel_PVSnowModel_tdry_aget(SAM_Snowmodel ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Snowmodel_PVSnowModel_wspd_aget(SAM_Snowmodel ptr, int* length, SAM_error *err);


	/**
	 * TimeSeries Getters
	 */

	SAM_EXPORT double* SAM_Snowmodel_TimeSeries_hourly_gen_aget(SAM_Snowmodel ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Snowmodel_TimeSeries_sunup_aget(SAM_Snowmodel ptr, int* length, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double SAM_Snowmodel_Outputs_annual_energy_nget(SAM_Snowmodel ptr, SAM_error *err);

	SAM_EXPORT double SAM_Snowmodel_Outputs_annual_energy_before_snow_nget(SAM_Snowmodel ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Snowmodel_Outputs_hourly_energy_before_snow_aget(SAM_Snowmodel ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Snowmodel_Outputs_hourly_gen_aget(SAM_Snowmodel ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Snowmodel_Outputs_monthly_energy_aget(SAM_Snowmodel ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Snowmodel_Outputs_monthly_energy_before_snow_aget(SAM_Snowmodel ptr, int* length, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif