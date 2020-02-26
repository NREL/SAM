#ifndef SAM_PVWATTSV51TS_H_
#define SAM_PVWATTSV51TS_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// Pvwattsv51ts Technology Model
	//

	/** 
	 * Create a Pvwattsv51ts variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_Pvwattsv51ts;

	SAM_EXPORT SAM_Pvwattsv51ts SAM_Pvwattsv51ts_construct(const char* def, SAM_error* err);

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_Pvwattsv51ts_execute(SAM_Pvwattsv51ts data, int verbosity, SAM_error* err);

	SAM_EXPORT void SAM_Pvwattsv51ts_destruct(SAM_Pvwattsv51ts system);


	//
	// PVWatts parameters
	//

	/**
	 * Set alb: Albedo [frac]
	 * options: None
	 * constraints: None
	 * required if: ?=0.2
	 */
	SAM_EXPORT void SAM_Pvwattsv51ts_PVWatts_alb_nset(SAM_Pvwattsv51ts ptr, double number, SAM_error *err);

	/**
	 * Set beam: Beam normal irradiance [W/m2]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvwattsv51ts_PVWatts_beam_nset(SAM_Pvwattsv51ts ptr, double number, SAM_error *err);

	/**
	 * Set day: Day [dy]
	 * options: 1-days in month
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvwattsv51ts_PVWatts_day_nset(SAM_Pvwattsv51ts ptr, double number, SAM_error *err);

	/**
	 * Set diffuse: Diffuse irradiance [W/m2]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvwattsv51ts_PVWatts_diffuse_nset(SAM_Pvwattsv51ts ptr, double number, SAM_error *err);

	/**
	 * Set hour: Hour [hr]
	 * options: 0-23
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvwattsv51ts_PVWatts_hour_nset(SAM_Pvwattsv51ts ptr, double number, SAM_error *err);

	/**
	 * Set lat: Latitude [deg]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvwattsv51ts_PVWatts_lat_nset(SAM_Pvwattsv51ts ptr, double number, SAM_error *err);

	/**
	 * Set lon: Longitude [deg]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvwattsv51ts_PVWatts_lon_nset(SAM_Pvwattsv51ts ptr, double number, SAM_error *err);

	/**
	 * Set minute: Minute [min]
	 * options: 0-59
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvwattsv51ts_PVWatts_minute_nset(SAM_Pvwattsv51ts ptr, double number, SAM_error *err);

	/**
	 * Set month: Month [mn]
	 * options: 1-12
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvwattsv51ts_PVWatts_month_nset(SAM_Pvwattsv51ts ptr, double number, SAM_error *err);

	/**
	 * Set poa: Plane of array irradiance [W/m2]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvwattsv51ts_PVWatts_poa_nset(SAM_Pvwattsv51ts ptr, double number, SAM_error *err);

	/**
	 * Set tamb: Ambient temperature [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvwattsv51ts_PVWatts_tamb_nset(SAM_Pvwattsv51ts ptr, double number, SAM_error *err);

	/**
	 * Set tcell: Module temperature [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvwattsv51ts_PVWatts_tcell_nset(SAM_Pvwattsv51ts ptr, double number, SAM_error *err);

	/**
	 * Set time_step: Time step of input data [hr]
	 * options: None
	 * constraints: POSITIVE
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Pvwattsv51ts_PVWatts_time_step_nset(SAM_Pvwattsv51ts ptr, double number, SAM_error *err);

	/**
	 * Set tz: Time zone [hr]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvwattsv51ts_PVWatts_tz_nset(SAM_Pvwattsv51ts ptr, double number, SAM_error *err);

	/**
	 * Set wspd: Wind speed [m/s]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvwattsv51ts_PVWatts_wspd_nset(SAM_Pvwattsv51ts ptr, double number, SAM_error *err);

	/**
	 * Set year: Year [yr]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvwattsv51ts_PVWatts_year_nset(SAM_Pvwattsv51ts ptr, double number, SAM_error *err);


	//
	// SystemDesign parameters
	//

	/**
	 * Set array_type: Array type [0/1/2/3/4]
	 * options: Fixed OR,Fixed Roof,1Axis,Backtracked,2Axis
	 * constraints: MIN=0,MAX=4,INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvwattsv51ts_SystemDesign_array_type_nset(SAM_Pvwattsv51ts ptr, double number, SAM_error *err);

	/**
	 * Set azimuth: Azimuth angle [deg]
	 * options: E=90,S=180,W=270
	 * constraints: MIN=0,MAX=360
	 * required if: array_type<4
	 */
	SAM_EXPORT void SAM_Pvwattsv51ts_SystemDesign_azimuth_nset(SAM_Pvwattsv51ts ptr, double number, SAM_error *err);

	/**
	 * Set dc_ac_ratio: DC to AC ratio [ratio]
	 * options: None
	 * constraints: POSITIVE
	 * required if: ?=1.1
	 */
	SAM_EXPORT void SAM_Pvwattsv51ts_SystemDesign_dc_ac_ratio_nset(SAM_Pvwattsv51ts ptr, double number, SAM_error *err);

	/**
	 * Set gcr: Ground coverage ratio [0..1]
	 * options: None
	 * constraints: MIN=0,MAX=1
	 * required if: ?=0.4
	 */
	SAM_EXPORT void SAM_Pvwattsv51ts_SystemDesign_gcr_nset(SAM_Pvwattsv51ts ptr, double number, SAM_error *err);

	/**
	 * Set inv_eff: Inverter efficiency at rated power [%]
	 * options: None
	 * constraints: MIN=90,MAX=99.5
	 * required if: ?=96
	 */
	SAM_EXPORT void SAM_Pvwattsv51ts_SystemDesign_inv_eff_nset(SAM_Pvwattsv51ts ptr, double number, SAM_error *err);

	/**
	 * Set losses: System losses [%]
	 * options: Total system losses
	 * constraints: MIN=-5,MAX=99
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvwattsv51ts_SystemDesign_losses_nset(SAM_Pvwattsv51ts ptr, double number, SAM_error *err);

	/**
	 * Set module_type: Module type [0/1/2]
	 * options: Standard,Premium,Thin film
	 * constraints: MIN=0,MAX=2,INTEGER
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvwattsv51ts_SystemDesign_module_type_nset(SAM_Pvwattsv51ts ptr, double number, SAM_error *err);

	/**
	 * Set system_capacity: System size (DC nameplate) [kW]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvwattsv51ts_SystemDesign_system_capacity_nset(SAM_Pvwattsv51ts ptr, double number, SAM_error *err);

	/**
	 * Set tilt: Tilt angle [deg]
	 * options: H=0,V=90
	 * constraints: MIN=0,MAX=90
	 * required if: array_type<4
	 */
	SAM_EXPORT void SAM_Pvwattsv51ts_SystemDesign_tilt_nset(SAM_Pvwattsv51ts ptr, double number, SAM_error *err);


	/**
	 * PVWatts Getters
	 */

	SAM_EXPORT double SAM_Pvwattsv51ts_PVWatts_alb_nget(SAM_Pvwattsv51ts ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv51ts_PVWatts_beam_nget(SAM_Pvwattsv51ts ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv51ts_PVWatts_day_nget(SAM_Pvwattsv51ts ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv51ts_PVWatts_diffuse_nget(SAM_Pvwattsv51ts ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv51ts_PVWatts_hour_nget(SAM_Pvwattsv51ts ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv51ts_PVWatts_lat_nget(SAM_Pvwattsv51ts ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv51ts_PVWatts_lon_nget(SAM_Pvwattsv51ts ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv51ts_PVWatts_minute_nget(SAM_Pvwattsv51ts ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv51ts_PVWatts_month_nget(SAM_Pvwattsv51ts ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv51ts_PVWatts_poa_nget(SAM_Pvwattsv51ts ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv51ts_PVWatts_tamb_nget(SAM_Pvwattsv51ts ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv51ts_PVWatts_tcell_nget(SAM_Pvwattsv51ts ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv51ts_PVWatts_time_step_nget(SAM_Pvwattsv51ts ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv51ts_PVWatts_tz_nget(SAM_Pvwattsv51ts ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv51ts_PVWatts_wspd_nget(SAM_Pvwattsv51ts ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv51ts_PVWatts_year_nget(SAM_Pvwattsv51ts ptr, SAM_error *err);


	/**
	 * SystemDesign Getters
	 */

	SAM_EXPORT double SAM_Pvwattsv51ts_SystemDesign_array_type_nget(SAM_Pvwattsv51ts ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv51ts_SystemDesign_azimuth_nget(SAM_Pvwattsv51ts ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv51ts_SystemDesign_dc_ac_ratio_nget(SAM_Pvwattsv51ts ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv51ts_SystemDesign_gcr_nget(SAM_Pvwattsv51ts ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv51ts_SystemDesign_inv_eff_nget(SAM_Pvwattsv51ts ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv51ts_SystemDesign_losses_nget(SAM_Pvwattsv51ts ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv51ts_SystemDesign_module_type_nget(SAM_Pvwattsv51ts ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv51ts_SystemDesign_system_capacity_nget(SAM_Pvwattsv51ts ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv51ts_SystemDesign_tilt_nget(SAM_Pvwattsv51ts ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double SAM_Pvwattsv51ts_Outputs_ac_nget(SAM_Pvwattsv51ts ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv51ts_Outputs_dc_nget(SAM_Pvwattsv51ts ptr, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif