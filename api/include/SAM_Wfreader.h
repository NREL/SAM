#ifndef SAM_WFREADER_H_
#define SAM_WFREADER_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// Wfreader Technology Model
	//

	/** 
	 * Create a Wfreader variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_Wfreader;

	SAM_EXPORT SAM_Wfreader SAM_Wfreader_construct(const char* def, SAM_error* err);

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_Wfreader_execute(SAM_Wfreader data, int verbosity, SAM_error* err);

	SAM_EXPORT void SAM_Wfreader_destruct(SAM_Wfreader system);


	//
	// WeatherReader parameters
	//

	/**
	 * Set file_name: local weather file path
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: *
	 */
	SAM_EXPORT void SAM_Wfreader_WeatherReader_file_name_sset(SAM_Wfreader ptr, const char* str, SAM_error *err);

	/**
	 * Set header_only: read header only [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Wfreader_WeatherReader_header_only_nset(SAM_Wfreader ptr, double number, SAM_error *err);


	/**
	 * WeatherReader Getters
	 */

	SAM_EXPORT const char* SAM_Wfreader_WeatherReader_file_name_sget(SAM_Wfreader ptr, SAM_error *err);

	SAM_EXPORT double SAM_Wfreader_WeatherReader_header_only_nget(SAM_Wfreader ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double* SAM_Wfreader_Outputs_albedo_aget(SAM_Wfreader ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Wfreader_Outputs_annual_albedo_nget(SAM_Wfreader ptr, SAM_error *err);

	SAM_EXPORT double SAM_Wfreader_Outputs_annual_beam_nget(SAM_Wfreader ptr, SAM_error *err);

	SAM_EXPORT double SAM_Wfreader_Outputs_annual_diffuse_nget(SAM_Wfreader ptr, SAM_error *err);

	SAM_EXPORT double SAM_Wfreader_Outputs_annual_global_nget(SAM_Wfreader ptr, SAM_error *err);

	SAM_EXPORT double SAM_Wfreader_Outputs_annual_snow_nget(SAM_Wfreader ptr, SAM_error *err);

	SAM_EXPORT double SAM_Wfreader_Outputs_annual_tdry_nget(SAM_Wfreader ptr, SAM_error *err);

	SAM_EXPORT double SAM_Wfreader_Outputs_annual_wspd_nget(SAM_Wfreader ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Wfreader_Outputs_beam_aget(SAM_Wfreader ptr, int* length, SAM_error *err);

	SAM_EXPORT const char* SAM_Wfreader_Outputs_city_sget(SAM_Wfreader ptr, SAM_error *err);

	SAM_EXPORT const char* SAM_Wfreader_Outputs_country_sget(SAM_Wfreader ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Wfreader_Outputs_day_aget(SAM_Wfreader ptr, int* length, SAM_error *err);

	SAM_EXPORT const char* SAM_Wfreader_Outputs_description_sget(SAM_Wfreader ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Wfreader_Outputs_diffuse_aget(SAM_Wfreader ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Wfreader_Outputs_elev_nget(SAM_Wfreader ptr, SAM_error *err);

	SAM_EXPORT const char* SAM_Wfreader_Outputs_format_sget(SAM_Wfreader ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Wfreader_Outputs_global_aget(SAM_Wfreader ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Wfreader_Outputs_hour_aget(SAM_Wfreader ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Wfreader_Outputs_lat_nget(SAM_Wfreader ptr, SAM_error *err);

	SAM_EXPORT const char* SAM_Wfreader_Outputs_location_sget(SAM_Wfreader ptr, SAM_error *err);

	SAM_EXPORT double SAM_Wfreader_Outputs_lon_nget(SAM_Wfreader ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Wfreader_Outputs_minute_aget(SAM_Wfreader ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Wfreader_Outputs_month_aget(SAM_Wfreader ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Wfreader_Outputs_nrecords_nget(SAM_Wfreader ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Wfreader_Outputs_poa_aget(SAM_Wfreader ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Wfreader_Outputs_pres_aget(SAM_Wfreader ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Wfreader_Outputs_rhum_aget(SAM_Wfreader ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Wfreader_Outputs_snow_aget(SAM_Wfreader ptr, int* length, SAM_error *err);

	SAM_EXPORT const char* SAM_Wfreader_Outputs_source_sget(SAM_Wfreader ptr, SAM_error *err);

	SAM_EXPORT double SAM_Wfreader_Outputs_start_nget(SAM_Wfreader ptr, SAM_error *err);

	SAM_EXPORT const char* SAM_Wfreader_Outputs_state_sget(SAM_Wfreader ptr, SAM_error *err);

	SAM_EXPORT double SAM_Wfreader_Outputs_step_nget(SAM_Wfreader ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Wfreader_Outputs_tdew_aget(SAM_Wfreader ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Wfreader_Outputs_tdry_aget(SAM_Wfreader ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Wfreader_Outputs_twet_aget(SAM_Wfreader ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Wfreader_Outputs_tz_nget(SAM_Wfreader ptr, SAM_error *err);

	SAM_EXPORT const char* SAM_Wfreader_Outputs_url_sget(SAM_Wfreader ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Wfreader_Outputs_wdir_aget(SAM_Wfreader ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Wfreader_Outputs_wspd_aget(SAM_Wfreader ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Wfreader_Outputs_year_aget(SAM_Wfreader ptr, int* length, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif