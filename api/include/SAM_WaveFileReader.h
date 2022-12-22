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
	 * Set wave_resource_filename: File path with Wave Height x Period Distribution as 2-D PDF
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: wave_resource_model_choice=0
	 */
	SAM_EXPORT void SAM_WaveFileReader_WeatherReader_wave_resource_filename_sset(SAM_table ptr, const char* str, SAM_error *err);

	/**
	 * Set wave_resource_filename_ts: File path with 3-hour Wave Height and Period data as Time Series array
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: wave_resource_model_choice=1
	 */
	SAM_EXPORT void SAM_WaveFileReader_WeatherReader_wave_resource_filename_ts_sset(SAM_table ptr, const char* str, SAM_error *err);

	/**
	 * Set wave_resource_model_choice: Joint PDF or 3-hour wave resource data [0/1]
	 * options: None
	 * constraints: INTEGER
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_WaveFileReader_WeatherReader_wave_resource_model_choice_nset(SAM_table ptr, double number, SAM_error *err);


	/**
	 * WeatherReader Getters
	 */

	SAM_EXPORT double SAM_WaveFileReader_WeatherReader_use_specific_wf_wave_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT const char* SAM_WaveFileReader_WeatherReader_wave_resource_filename_sget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT const char* SAM_WaveFileReader_WeatherReader_wave_resource_filename_ts_sget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WaveFileReader_WeatherReader_wave_resource_model_choice_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double SAM_WaveFileReader_Outputs_average_power_flux_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT const char* SAM_WaveFileReader_Outputs_bathymetry_sget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT const char* SAM_WaveFileReader_Outputs_city_sget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT const char* SAM_WaveFileReader_Outputs_country_sget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT const char* SAM_WaveFileReader_Outputs_data_source_sget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_WaveFileReader_Outputs_day_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_WaveFileReader_Outputs_distance_to_shore_file_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_WaveFileReader_Outputs_energy_period_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_WaveFileReader_Outputs_hour_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_WaveFileReader_Outputs_lat_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WaveFileReader_Outputs_location_id_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WaveFileReader_Outputs_lon_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_WaveFileReader_Outputs_minute_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_WaveFileReader_Outputs_month_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT const char* SAM_WaveFileReader_Outputs_name_sget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT const char* SAM_WaveFileReader_Outputs_nearby_buoy_number_sget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT const char* SAM_WaveFileReader_Outputs_notes_sget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WaveFileReader_Outputs_number_hours_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WaveFileReader_Outputs_number_records_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT const char* SAM_WaveFileReader_Outputs_sea_bed_sget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_WaveFileReader_Outputs_significant_wave_height_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT const char* SAM_WaveFileReader_Outputs_state_sget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WaveFileReader_Outputs_tz_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WaveFileReader_Outputs_water_depth_file_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_WaveFileReader_Outputs_wave_resource_matrix_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_WaveFileReader_Outputs_year_aget(SAM_table ptr, int* length, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif