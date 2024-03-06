#ifndef SAM_TIDALFILEREADER_H_
#define SAM_TIDALFILEREADER_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// TidalFileReader Technology Model
	//

	/** 
	 * Create a TidalFileReader variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_TidalFileReader;

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_TidalFileReader_execute(SAM_table data, int verbosity, SAM_error* err);


	//
	// WeatherReader parameters
	//

	/**
	 * Set tidal_resource_filename: File path with tidal resource data
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: tidal_resource_model_choice=0
	 */
	SAM_EXPORT void SAM_TidalFileReader_WeatherReader_tidal_resource_filename_sset(SAM_table ptr, const char* str, SAM_error *err);

	/**
	 * Set tidal_resource_model_choice: Resource distribution or time series tidal resource data [0/1]
	 * options: None
	 * constraints: INTEGER
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_TidalFileReader_WeatherReader_tidal_resource_model_choice_nset(SAM_table ptr, double number, SAM_error *err);


	/**
	 * WeatherReader Getters
	 */

	SAM_EXPORT const char* SAM_TidalFileReader_WeatherReader_tidal_resource_filename_sget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TidalFileReader_WeatherReader_tidal_resource_model_choice_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double SAM_TidalFileReader_Outputs_average_power_flux_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT const char* SAM_TidalFileReader_Outputs_bathymetry_sget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT const char* SAM_TidalFileReader_Outputs_city_sget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT const char* SAM_TidalFileReader_Outputs_country_sget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT const char* SAM_TidalFileReader_Outputs_data_source_sget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TidalFileReader_Outputs_day_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TidalFileReader_Outputs_distance_to_shore_file_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TidalFileReader_Outputs_hour_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TidalFileReader_Outputs_lat_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT const char* SAM_TidalFileReader_Outputs_location_sget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT const char* SAM_TidalFileReader_Outputs_location_id_sget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TidalFileReader_Outputs_lon_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TidalFileReader_Outputs_minute_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_TidalFileReader_Outputs_month_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT const char* SAM_TidalFileReader_Outputs_name_sget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT const char* SAM_TidalFileReader_Outputs_nearby_buoy_number_sget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT const char* SAM_TidalFileReader_Outputs_notes_sget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TidalFileReader_Outputs_number_hours_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TidalFileReader_Outputs_number_records_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT const char* SAM_TidalFileReader_Outputs_sea_bed_sget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TidalFileReader_Outputs_significant_wave_height_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT const char* SAM_TidalFileReader_Outputs_state_sget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TidalFileReader_Outputs_tidal_velocity_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_TidalFileReader_Outputs_tz_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_TidalFileReader_Outputs_water_depth_file_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_TidalFileReader_Outputs_year_aget(SAM_table ptr, int* length, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif