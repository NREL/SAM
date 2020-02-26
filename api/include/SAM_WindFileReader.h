#ifndef SAM_WINDFILEREADER_H_
#define SAM_WINDFILEREADER_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>

#ifdef __cplusplus
extern "C"
{
#endif

//
// WindFileReader Technology Model
//

/**
 * Create a WindFileReader variable table.
 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
 * @param[in,out] err: a pointer to an error object
 */

SAM_EXPORT typedef void *SAM_WindFileReader;

SAM_EXPORT SAM_WindFileReader SAM_WindFileReader_construct(const char *def, SAM_error *err);

/// verbosity level 0 or 1. Returns 1 on success
SAM_EXPORT int SAM_WindFileReader_execute(SAM_WindFileReader data, int verbosity, SAM_error *err);

SAM_EXPORT void SAM_WindFileReader_destruct(SAM_WindFileReader system);


//
// WeatherReader parameters
//

/**
 * Set file_name: local weather file path
 * options: None
 * constraints: LOCAL_FILE
 * required if: *
 */
SAM_EXPORT void
SAM_WindFileReader_WeatherReader_file_name_sset(SAM_WindFileReader ptr, const char *str, SAM_error *err);

/**
 * Set interpolate: interpolate to closest height measured? [m]
 * options: None
 * constraints: BOOLEAN
 * required if: scan_header_only=0
 */
SAM_EXPORT void
SAM_WindFileReader_WeatherReader_interpolate_nset(SAM_WindFileReader ptr, double number, SAM_error *err);

/**
 * Set requested_ht: requested measurement height [m]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_WindFileReader_WeatherReader_requested_ht_nset(SAM_WindFileReader ptr, double number, SAM_error *err);

/**
 * Set scan_header_only: only reader headers [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=0
 */
SAM_EXPORT void
SAM_WindFileReader_WeatherReader_scan_header_only_nset(SAM_WindFileReader ptr, double number, SAM_error *err);


/**
 * WeatherReader Getters
 */

SAM_EXPORT const char *SAM_WindFileReader_WeatherReader_file_name_sget(SAM_WindFileReader ptr, SAM_error *err);

SAM_EXPORT double SAM_WindFileReader_WeatherReader_interpolate_nget(SAM_WindFileReader ptr, SAM_error *err);

SAM_EXPORT double SAM_WindFileReader_WeatherReader_requested_ht_nget(SAM_WindFileReader ptr, SAM_error *err);

SAM_EXPORT double SAM_WindFileReader_WeatherReader_scan_header_only_nget(SAM_WindFileReader ptr, SAM_error *err);


/**
 * Outputs Getters
 */

SAM_EXPORT const char *SAM_WindFileReader_Outputs_city_sget(SAM_WindFileReader ptr, SAM_error *err);

SAM_EXPORT double SAM_WindFileReader_Outputs_closest_dir_meas_ht_nget(SAM_WindFileReader ptr, SAM_error *err);

SAM_EXPORT double SAM_WindFileReader_Outputs_closest_speed_meas_ht_nget(SAM_WindFileReader ptr, SAM_error *err);

SAM_EXPORT const char *SAM_WindFileReader_Outputs_country_sget(SAM_WindFileReader ptr, SAM_error *err);

SAM_EXPORT const char *SAM_WindFileReader_Outputs_description_sget(SAM_WindFileReader ptr, SAM_error *err);

SAM_EXPORT double SAM_WindFileReader_Outputs_elev_nget(SAM_WindFileReader ptr, SAM_error *err);

SAM_EXPORT double SAM_WindFileReader_Outputs_lat_nget(SAM_WindFileReader ptr, SAM_error *err);

SAM_EXPORT const char *SAM_WindFileReader_Outputs_location_id_sget(SAM_WindFileReader ptr, SAM_error *err);

SAM_EXPORT double SAM_WindFileReader_Outputs_lon_nget(SAM_WindFileReader ptr, SAM_error *err);

SAM_EXPORT double *SAM_WindFileReader_Outputs_pressure_aget(SAM_WindFileReader ptr, int *length, SAM_error *err);

SAM_EXPORT const char *SAM_WindFileReader_Outputs_state_sget(SAM_WindFileReader ptr, SAM_error *err);

SAM_EXPORT double *SAM_WindFileReader_Outputs_temperature_aget(SAM_WindFileReader ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_WindFileReader_Outputs_wind_direction_aget(SAM_WindFileReader ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_WindFileReader_Outputs_wind_speed_aget(SAM_WindFileReader ptr, int *length, SAM_error *err);

SAM_EXPORT double SAM_WindFileReader_Outputs_year_nget(SAM_WindFileReader ptr, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif
