#ifndef SAM_WAVEFILEREADER_H_
#define SAM_WAVEFILEREADER_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// WaveFileReader Technology Model
	//

	/** 
	 * Create a WaveFileReader variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_WaveFileReader;

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_WaveFileReader_execute(SAM_table data, int verbosity, SAM_error* err);


	//
	// WeatherReader parameters
	//

	/**
	 * Set use_specific_wf_wave: user specified file [0/1]
	 * options: None
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_WaveFileReader_WeatherReader_use_specific_wf_wave_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set wave_resource_filename: local weather file path
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: *
	 */
	SAM_EXPORT void SAM_WaveFileReader_WeatherReader_wave_resource_filename_sset(SAM_table ptr, const char* str, SAM_error *err);


	/**
	 * WeatherReader Getters
	 */

	SAM_EXPORT double SAM_WaveFileReader_WeatherReader_use_specific_wf_wave_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT const char* SAM_WaveFileReader_WeatherReader_wave_resource_filename_sget(SAM_table ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double SAM_WaveFileReader_Outputs_average_power_flux_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT const char* SAM_WaveFileReader_Outputs_bathymetry_sget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT const char* SAM_WaveFileReader_Outputs_city_sget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT const char* SAM_WaveFileReader_Outputs_country_sget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT const char* SAM_WaveFileReader_Outputs_data_source_sget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WaveFileReader_Outputs_lat_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WaveFileReader_Outputs_lon_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT const char* SAM_WaveFileReader_Outputs_name_sget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT const char* SAM_WaveFileReader_Outputs_nearby_buoy_number_sget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT const char* SAM_WaveFileReader_Outputs_notes_sget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT const char* SAM_WaveFileReader_Outputs_sea_bed_sget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT const char* SAM_WaveFileReader_Outputs_state_sget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WaveFileReader_Outputs_tz_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_WaveFileReader_Outputs_wave_resource_matrix_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif