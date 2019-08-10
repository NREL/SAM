#ifndef SAM_SWH_FUNCTIONS_H_
#define SAM_SWH_FUNCTIONS_H_

#include "Swh-data.h"

#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	/** 
	 * Create a LocationAndResource variable table for a SolarWaterHeatingNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_Swh_LocationAndResource SAM_Swh_LocationAndResource_create(const char* def, SAM_error* err);


	/**
	 * Set solar_data_file_name: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Swh_LocationAndResource_solar_data_file_name_set(SAM_Swh_LocationAndResource ptr, const char* string, SAM_error* err);

	/**
	 * Set use_specific_weather_file: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Swh_LocationAndResource_use_specific_weather_file_set(SAM_Swh_LocationAndResource ptr, const char* string, SAM_error* err);

	/**
	 * Set user_specified_weather_file: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Swh_LocationAndResource_user_specified_weather_file_set(SAM_Swh_LocationAndResource ptr, const char* string, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT const char* SAM_Swh_LocationAndResource_solar_data_file_name_get(SAM_Swh_LocationAndResource ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Swh_LocationAndResource_use_specific_weather_file_get(SAM_Swh_LocationAndResource ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Swh_LocationAndResource_user_specified_weather_file_get(SAM_Swh_LocationAndResource ptr, SAM_error* err);



	/** 
	 * Create a SolarWaterHeating variable table for a SolarWaterHeatingNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_Swh_SolarWaterHeating SAM_Swh_SolarWaterHeating_create(const char* def, SAM_error* err);


	/**
	 * Set coll_mode: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Swh_SolarWaterHeating_coll_mode_set(SAM_Swh_SolarWaterHeating ptr, const char* string, SAM_error* err);

	/**
	 * Set ncoll: Number of collectors
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: POSITIVE,INTEGER
	 * required if: None
	 */
	SAM_EXPORT void SAM_Swh_SolarWaterHeating_ncoll_set(SAM_Swh_SolarWaterHeating ptr, float number, SAM_error* err);

	/**
	 * Set srcc_FRUL: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Swh_SolarWaterHeating_srcc_FRUL_set(SAM_Swh_SolarWaterHeating ptr, const char* string, SAM_error* err);

	/**
	 * Set srcc_FRta: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Swh_SolarWaterHeating_srcc_FRta_set(SAM_Swh_SolarWaterHeating ptr, const char* string, SAM_error* err);

	/**
	 * Set srcc_area: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Swh_SolarWaterHeating_srcc_area_set(SAM_Swh_SolarWaterHeating ptr, const char* string, SAM_error* err);

	/**
	 * Set use_custom_mains: Use custom mains
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: None
	 */
	SAM_EXPORT void SAM_Swh_SolarWaterHeating_use_custom_mains_set(SAM_Swh_SolarWaterHeating ptr, float number, SAM_error* err);

	/**
	 * Set use_custom_set: Use custom set points
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: None
	 */
	SAM_EXPORT void SAM_Swh_SolarWaterHeating_use_custom_set_set(SAM_Swh_SolarWaterHeating ptr, float number, SAM_error* err);

	/**
	 * Set user_FRUL: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Swh_SolarWaterHeating_user_FRUL_set(SAM_Swh_SolarWaterHeating ptr, const char* string, SAM_error* err);

	/**
	 * Set user_FRta: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Swh_SolarWaterHeating_user_FRta_set(SAM_Swh_SolarWaterHeating ptr, const char* string, SAM_error* err);

	/**
	 * Set user_area_coll: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Swh_SolarWaterHeating_user_area_coll_set(SAM_Swh_SolarWaterHeating ptr, const char* string, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT const char* SAM_Swh_SolarWaterHeating_coll_mode_get(SAM_Swh_SolarWaterHeating ptr, SAM_error* err);

	SAM_EXPORT float SAM_Swh_SolarWaterHeating_ncoll_get(SAM_Swh_SolarWaterHeating ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Swh_SolarWaterHeating_srcc_FRUL_get(SAM_Swh_SolarWaterHeating ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Swh_SolarWaterHeating_srcc_FRta_get(SAM_Swh_SolarWaterHeating ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Swh_SolarWaterHeating_srcc_area_get(SAM_Swh_SolarWaterHeating ptr, SAM_error* err);

	SAM_EXPORT float SAM_Swh_SolarWaterHeating_use_custom_mains_get(SAM_Swh_SolarWaterHeating ptr, SAM_error* err);

	SAM_EXPORT float SAM_Swh_SolarWaterHeating_use_custom_set_get(SAM_Swh_SolarWaterHeating ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Swh_SolarWaterHeating_user_FRUL_get(SAM_Swh_SolarWaterHeating ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Swh_SolarWaterHeating_user_FRta_get(SAM_Swh_SolarWaterHeating ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Swh_SolarWaterHeating_user_area_coll_get(SAM_Swh_SolarWaterHeating ptr, SAM_error* err);



	/** 
	 * Create a Common variable table for a SolarWaterHeatingNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_Swh_Common SAM_Swh_Common_create(const char* def, SAM_error* err);


	/**
	 * Set adjust:constant: Constant loss adjustment
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: MAX=100
	 * required if: None
	 */
	SAM_EXPORT void SAM_Swh_Common_adjust:constant_set(SAM_Swh_Common ptr, float number, SAM_error* err);

	/**
	 * Set adjust:hourly: Hourly loss adjustments
	 * type: array
	 * units: %
	 * options: None
	 * constraints: LENGTH=8760
	 * required if: None
	 */
	SAM_EXPORT void SAM_Swh_Common_adjust:hourly_set(SAM_Swh_Common ptr, float* array, int length, SAM_error* err);

	/**
	 * Set adjust:periods: Period-based loss adjustments
	 * type: matrix
	 * units: %
	 * options: n x 3 matrix [ start, end, loss ]
	 * constraints: COLS=3
	 * required if: None
	 */
	SAM_EXPORT void SAM_Swh_Common_adjust:periods_set(SAM_Swh_Common ptr, float* matrix, int nr, int nc, SAM_error* err);

	/**
	 * Set shading:azal: Azimuth x altitude beam shading loss
	 * type: matrix
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Swh_Common_shading:azal_set(SAM_Swh_Common ptr, float* matrix, int nr, int nc, SAM_error* err);

	/**
	 * Set shading:diff: Diffuse shading loss
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Swh_Common_shading:diff_set(SAM_Swh_Common ptr, float number, SAM_error* err);

	/**
	 * Set shading:mxh: Month x Hour beam shading loss
	 * type: matrix
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Swh_Common_shading:mxh_set(SAM_Swh_Common ptr, float* matrix, int nr, int nc, SAM_error* err);

	/**
	 * Set shading:timestep: Time step beam shading loss
	 * type: matrix
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Swh_Common_shading:timestep_set(SAM_Swh_Common ptr, float* matrix, int nr, int nc, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_Swh_Common_adjust:constant_get(SAM_Swh_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Swh_Common_adjust:hourly_get(SAM_Swh_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Swh_Common_adjust:periods_get(SAM_Swh_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Swh_Common_shading:azal_get(SAM_Swh_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Swh_Common_shading:diff_get(SAM_Swh_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Swh_Common_shading:mxh_get(SAM_Swh_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Swh_Common_shading:timestep_get(SAM_Swh_Common ptr, SAM_error* err);



#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif