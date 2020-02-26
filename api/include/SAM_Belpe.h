#ifndef SAM_BELPE_H_
#define SAM_BELPE_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// Belpe Technology Model
	//

	/** 
	 * Create a Belpe variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_Belpe;

	SAM_EXPORT SAM_Belpe SAM_Belpe_construct(const char* def, SAM_error* err);

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_Belpe_execute(SAM_Belpe data, int verbosity, SAM_error* err);

	SAM_EXPORT void SAM_Belpe_destruct(SAM_Belpe system);


	//
	// LoadProfileEstimator parameters
	//

	/**
	 * Set Monthly_util: Monthly consumption from utility bill [kWh]
	 * options: None
	 * constraints: LENGTH=12
	 * required if: en_belpe=1
	 */
	SAM_EXPORT void SAM_Belpe_LoadProfileEstimator_Monthly_util_aset(SAM_Belpe ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set Occ_Schedule: Hourly occupant schedule [frac/hr]
	 * options: None
	 * constraints: LENGTH=24
	 * required if: en_belpe=1
	 */
	SAM_EXPORT void SAM_Belpe_LoadProfileEstimator_Occ_Schedule_aset(SAM_Belpe ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set Occupants: Occupants [#]
	 * options: None
	 * constraints: None
	 * required if: en_belpe=1
	 */
	SAM_EXPORT void SAM_Belpe_LoadProfileEstimator_Occupants_nset(SAM_Belpe ptr, double number, SAM_error *err);

	/**
	 * Set Retrofits: Energy retrofitted [0/1]
	 * options: 0=No, 1=Yes
	 * constraints: None
	 * required if: en_belpe=1
	 */
	SAM_EXPORT void SAM_Belpe_LoadProfileEstimator_Retrofits_nset(SAM_Belpe ptr, double number, SAM_error *err);

	/**
	 * Set Stories: Number of stories [#]
	 * options: None
	 * constraints: None
	 * required if: en_belpe=1
	 */
	SAM_EXPORT void SAM_Belpe_LoadProfileEstimator_Stories_nset(SAM_Belpe ptr, double number, SAM_error *err);

	/**
	 * Set TCool: Cooling setpoint [degF]
	 * options: None
	 * constraints: None
	 * required if: en_belpe=1
	 */
	SAM_EXPORT void SAM_Belpe_LoadProfileEstimator_TCool_nset(SAM_Belpe ptr, double number, SAM_error *err);

	/**
	 * Set TCoolSB: Cooling setpoint setback [degF]
	 * options: None
	 * constraints: None
	 * required if: en_belpe=1
	 */
	SAM_EXPORT void SAM_Belpe_LoadProfileEstimator_TCoolSB_nset(SAM_Belpe ptr, double number, SAM_error *err);

	/**
	 * Set THeat: Heating setpoint [degF]
	 * options: None
	 * constraints: None
	 * required if: en_belpe=1
	 */
	SAM_EXPORT void SAM_Belpe_LoadProfileEstimator_THeat_nset(SAM_Belpe ptr, double number, SAM_error *err);

	/**
	 * Set THeatSB: Heating setpoint setback [degf]
	 * options: None
	 * constraints: None
	 * required if: en_belpe=1
	 */
	SAM_EXPORT void SAM_Belpe_LoadProfileEstimator_THeatSB_nset(SAM_Belpe ptr, double number, SAM_error *err);

	/**
	 * Set T_Sched: Temperature schedule [0/1]
	 * options: None
	 * constraints: LENGTH=24
	 * required if: en_belpe=1
	 */
	SAM_EXPORT void SAM_Belpe_LoadProfileEstimator_T_Sched_aset(SAM_Belpe ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set YrBuilt: Year built [yr]
	 * options: None
	 * constraints: None
	 * required if: en_belpe=1
	 */
	SAM_EXPORT void SAM_Belpe_LoadProfileEstimator_YrBuilt_nset(SAM_Belpe ptr, double number, SAM_error *err);

	/**
	 * Set en_belpe: Enable building load calculator [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: *
	 */
	SAM_EXPORT void SAM_Belpe_LoadProfileEstimator_en_belpe_nset(SAM_Belpe ptr, double number, SAM_error *err);

	/**
	 * Set en_cool: Enable electric cool [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: en_belpe=1
	 */
	SAM_EXPORT void SAM_Belpe_LoadProfileEstimator_en_cool_nset(SAM_Belpe ptr, double number, SAM_error *err);

	/**
	 * Set en_dish: Enable electric dishwasher [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: en_belpe=1
	 */
	SAM_EXPORT void SAM_Belpe_LoadProfileEstimator_en_dish_nset(SAM_Belpe ptr, double number, SAM_error *err);

	/**
	 * Set en_dry: Enable electric dryer [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: en_belpe=1
	 */
	SAM_EXPORT void SAM_Belpe_LoadProfileEstimator_en_dry_nset(SAM_Belpe ptr, double number, SAM_error *err);

	/**
	 * Set en_fridge: Enable electric fridge [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: en_belpe=1
	 */
	SAM_EXPORT void SAM_Belpe_LoadProfileEstimator_en_fridge_nset(SAM_Belpe ptr, double number, SAM_error *err);

	/**
	 * Set en_heat: Enable electric heat [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: en_belpe=1
	 */
	SAM_EXPORT void SAM_Belpe_LoadProfileEstimator_en_heat_nset(SAM_Belpe ptr, double number, SAM_error *err);

	/**
	 * Set en_mels: Enable misc electric loads [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: en_belpe=1
	 */
	SAM_EXPORT void SAM_Belpe_LoadProfileEstimator_en_mels_nset(SAM_Belpe ptr, double number, SAM_error *err);

	/**
	 * Set en_range: Enable electric range [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: en_belpe=1
	 */
	SAM_EXPORT void SAM_Belpe_LoadProfileEstimator_en_range_nset(SAM_Belpe ptr, double number, SAM_error *err);

	/**
	 * Set en_wash: Enable electric washer [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: en_belpe=1
	 */
	SAM_EXPORT void SAM_Belpe_LoadProfileEstimator_en_wash_nset(SAM_Belpe ptr, double number, SAM_error *err);

	/**
	 * Set floor_area: Building floor area [m2]
	 * options: None
	 * constraints: None
	 * required if: en_belpe=1
	 */
	SAM_EXPORT void SAM_Belpe_LoadProfileEstimator_floor_area_nset(SAM_Belpe ptr, double number, SAM_error *err);

	/**
	 * Set load: Electricity load (year 1) [kW]
	 * options: None
	 * constraints: None
	 * required if: en_belpe=0
	 */
	SAM_EXPORT void SAM_Belpe_LoadProfileEstimator_load_aset(SAM_Belpe ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set solar_resource_file: Weather Data file [n/a]
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: en_belpe=1
	 */
	SAM_EXPORT void SAM_Belpe_LoadProfileEstimator_solar_resource_file_sset(SAM_Belpe ptr, const char* str, SAM_error *err);


	/**
	 * LoadProfileEstimator Getters
	 */

	SAM_EXPORT double* SAM_Belpe_LoadProfileEstimator_Monthly_util_aget(SAM_Belpe ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Belpe_LoadProfileEstimator_Occ_Schedule_aget(SAM_Belpe ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Belpe_LoadProfileEstimator_Occupants_nget(SAM_Belpe ptr, SAM_error *err);

	SAM_EXPORT double SAM_Belpe_LoadProfileEstimator_Retrofits_nget(SAM_Belpe ptr, SAM_error *err);

	SAM_EXPORT double SAM_Belpe_LoadProfileEstimator_Stories_nget(SAM_Belpe ptr, SAM_error *err);

	SAM_EXPORT double SAM_Belpe_LoadProfileEstimator_TCool_nget(SAM_Belpe ptr, SAM_error *err);

	SAM_EXPORT double SAM_Belpe_LoadProfileEstimator_TCoolSB_nget(SAM_Belpe ptr, SAM_error *err);

	SAM_EXPORT double SAM_Belpe_LoadProfileEstimator_THeat_nget(SAM_Belpe ptr, SAM_error *err);

	SAM_EXPORT double SAM_Belpe_LoadProfileEstimator_THeatSB_nget(SAM_Belpe ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Belpe_LoadProfileEstimator_T_Sched_aget(SAM_Belpe ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Belpe_LoadProfileEstimator_YrBuilt_nget(SAM_Belpe ptr, SAM_error *err);

	SAM_EXPORT double SAM_Belpe_LoadProfileEstimator_en_belpe_nget(SAM_Belpe ptr, SAM_error *err);

	SAM_EXPORT double SAM_Belpe_LoadProfileEstimator_en_cool_nget(SAM_Belpe ptr, SAM_error *err);

	SAM_EXPORT double SAM_Belpe_LoadProfileEstimator_en_dish_nget(SAM_Belpe ptr, SAM_error *err);

	SAM_EXPORT double SAM_Belpe_LoadProfileEstimator_en_dry_nget(SAM_Belpe ptr, SAM_error *err);

	SAM_EXPORT double SAM_Belpe_LoadProfileEstimator_en_fridge_nget(SAM_Belpe ptr, SAM_error *err);

	SAM_EXPORT double SAM_Belpe_LoadProfileEstimator_en_heat_nget(SAM_Belpe ptr, SAM_error *err);

	SAM_EXPORT double SAM_Belpe_LoadProfileEstimator_en_mels_nget(SAM_Belpe ptr, SAM_error *err);

	SAM_EXPORT double SAM_Belpe_LoadProfileEstimator_en_range_nget(SAM_Belpe ptr, SAM_error *err);

	SAM_EXPORT double SAM_Belpe_LoadProfileEstimator_en_wash_nget(SAM_Belpe ptr, SAM_error *err);

	SAM_EXPORT double SAM_Belpe_LoadProfileEstimator_floor_area_nget(SAM_Belpe ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Belpe_LoadProfileEstimator_load_aget(SAM_Belpe ptr, int* length, SAM_error *err);

	SAM_EXPORT const char* SAM_Belpe_LoadProfileEstimator_solar_resource_file_sget(SAM_Belpe ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif