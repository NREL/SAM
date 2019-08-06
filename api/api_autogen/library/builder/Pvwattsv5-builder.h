#ifndef SAM_PVWATTSV5_FUNCTIONS_H_
#define SAM_PVWATTSV5_FUNCTIONS_H_

#include "Pvwattsv5-data.h"

#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	/** 
	 * Create a LocationAndResource variable table for a PVWattsNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_Pvwattsv5_LocationAndResource SAM_Pvwattsv5_LocationAndResource_create(const char* def, SAM_error* err);


	/**
	 * Set solar_data_file_name: Weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvwattsv5_LocationAndResource_solar_data_file_name_set(SAM_Pvwattsv5_LocationAndResource ptr, const char* string, SAM_error* err);

	/**
	 * Set use_specific_weather_file: Weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvwattsv5_LocationAndResource_use_specific_weather_file_set(SAM_Pvwattsv5_LocationAndResource ptr, const char* string, SAM_error* err);

	/**
	 * Set user_specified_weather_file: Weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvwattsv5_LocationAndResource_user_specified_weather_file_set(SAM_Pvwattsv5_LocationAndResource ptr, const char* string, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT const char* SAM_Pvwattsv5_LocationAndResource_solar_data_file_name_get(SAM_Pvwattsv5_LocationAndResource ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Pvwattsv5_LocationAndResource_use_specific_weather_file_get(SAM_Pvwattsv5_LocationAndResource ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Pvwattsv5_LocationAndResource_user_specified_weather_file_get(SAM_Pvwattsv5_LocationAndResource ptr, SAM_error* err);



	/** 
	 * Create a SystemDesign variable table for a PVWattsNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_Pvwattsv5_SystemDesign SAM_Pvwattsv5_SystemDesign_create(const char* def, SAM_error* err);


	/**
	 * Set array_type: Array type
	 * type: numeric
	 * units: 0/1/2/3/4
	 * options: Fixed OR,Fixed Roof,1Axis,Backtracked,2Axis
	 * constraints: MIN=0,MAX=4,INTEGER
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvwattsv5_SystemDesign_array_type_set(SAM_Pvwattsv5_SystemDesign ptr, float number, SAM_error* err);

	/**
	 * Set dc_ac_ratio: DC to AC ratio
	 * type: numeric
	 * units: ratio
	 * options: None
	 * constraints: POSITIVE
	 * required if: ?=1.1
	 */
	SAM_EXPORT void SAM_Pvwattsv5_SystemDesign_dc_ac_ratio_set(SAM_Pvwattsv5_SystemDesign ptr, float number, SAM_error* err);

	/**
	 * Set system_capacity: System size (DC nameplate)
	 * type: numeric
	 * units: kW
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvwattsv5_SystemDesign_system_capacity_set(SAM_Pvwattsv5_SystemDesign ptr, float number, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_Pvwattsv5_SystemDesign_array_type_get(SAM_Pvwattsv5_SystemDesign ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvwattsv5_SystemDesign_dc_ac_ratio_get(SAM_Pvwattsv5_SystemDesign ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvwattsv5_SystemDesign_system_capacity_get(SAM_Pvwattsv5_SystemDesign ptr, SAM_error* err);



	/** 
	 * Create a Common variable table for a PVWattsNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_Pvwattsv5_Common SAM_Pvwattsv5_Common_create(const char* def, SAM_error* err);


	/**
	 * Set adjust:constant: Constant loss adjustment
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: MAX=100
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvwattsv5_Common_adjust:constant_set(SAM_Pvwattsv5_Common ptr, float number, SAM_error* err);

	/**
	 * Set adjust:hourly: Hourly loss adjustments
	 * type: array
	 * units: %
	 * options: None
	 * constraints: LENGTH=8760
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvwattsv5_Common_adjust:hourly_set(SAM_Pvwattsv5_Common ptr, float* array, int length, SAM_error* err);

	/**
	 * Set adjust:periods: Period-based loss adjustments
	 * type: matrix
	 * units: %
	 * options: n x 3 matrix [ start, end, loss ]
	 * constraints: COLS=3
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvwattsv5_Common_adjust:periods_set(SAM_Pvwattsv5_Common ptr, float* matrix, int nr, int nc, SAM_error* err);

	/**
	 * Set batt_simple_enable: Enable Battery
	 * type: numeric
	 * units: 0/1
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvwattsv5_Common_batt_simple_enable_set(SAM_Pvwattsv5_Common ptr, float number, SAM_error* err);

	/**
	 * Set shading:azal: Azimuth x altitude beam shading loss
	 * type: matrix
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvwattsv5_Common_shading:azal_set(SAM_Pvwattsv5_Common ptr, float* matrix, int nr, int nc, SAM_error* err);

	/**
	 * Set shading:diff: Diffuse shading loss
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvwattsv5_Common_shading:diff_set(SAM_Pvwattsv5_Common ptr, float number, SAM_error* err);

	/**
	 * Set shading:mxh: Month x Hour beam shading loss
	 * type: matrix
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvwattsv5_Common_shading:mxh_set(SAM_Pvwattsv5_Common ptr, float* matrix, int nr, int nc, SAM_error* err);

	/**
	 * Set shading:timestep: Time step beam shading loss
	 * type: matrix
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvwattsv5_Common_shading:timestep_set(SAM_Pvwattsv5_Common ptr, float* matrix, int nr, int nc, SAM_error* err);

	/**
	 * Set solar_resource_data: Weather data
	 * type: table
	 * units: None
	 * options: dn,df,tdry,wspd,lat,lon,tz
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvwattsv5_Common_solar_resource_data_set(SAM_Pvwattsv5_Common ptr, var_table vt, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_Pvwattsv5_Common_adjust:constant_get(SAM_Pvwattsv5_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Pvwattsv5_Common_adjust:hourly_get(SAM_Pvwattsv5_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Pvwattsv5_Common_adjust:periods_get(SAM_Pvwattsv5_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvwattsv5_Common_batt_simple_enable_get(SAM_Pvwattsv5_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Pvwattsv5_Common_shading:azal_get(SAM_Pvwattsv5_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvwattsv5_Common_shading:diff_get(SAM_Pvwattsv5_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Pvwattsv5_Common_shading:mxh_get(SAM_Pvwattsv5_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Pvwattsv5_Common_shading:timestep_get(SAM_Pvwattsv5_Common ptr, SAM_error* err);

	SAM_EXPORT var_table SAM_Pvwattsv5_Common_solar_resource_data_get(SAM_Pvwattsv5_Common ptr, SAM_error* err);



#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif