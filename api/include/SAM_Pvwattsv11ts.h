#ifndef SAM_PVWATTSV11TS_H_
#define SAM_PVWATTSV11TS_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// Pvwattsv11ts Technology Model
	//

	/** 
	 * Create a Pvwattsv11ts variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_Pvwattsv11ts;

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_Pvwattsv11ts_execute(SAM_table data, int verbosity, SAM_error* err);


	//
	// PVWatts parameters
	//

	/**
	 * Set azimuth: Azimuth angle [deg]
	 * options: E=90,S=180,W=270
	 * constraints: MIN=0,MAX=360
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvwattsv11ts_PVWatts_azimuth_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set beam: Beam normal irradiance [W/m2]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvwattsv11ts_PVWatts_beam_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set day: Day [dy]
	 * options: 1-days in month
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvwattsv11ts_PVWatts_day_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set derate: System derate value [frac]
	 * options: None
	 * constraints: MIN=0,MAX=1
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvwattsv11ts_PVWatts_derate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set diffuse: Diffuse irradiance [W/m2]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvwattsv11ts_PVWatts_diffuse_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set elevation: Elevation [m]
	 * options: None
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Pvwattsv11ts_PVWatts_elevation_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set fd: Diffuse fraction [0..1]
	 * options: None
	 * constraints: MIN=0,MAX=1
	 * required if: ?=1.0
	 */
	SAM_EXPORT void SAM_Pvwattsv11ts_PVWatts_fd_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set gamma: Max power temperature coefficient [%/C]
	 * options: None
	 * constraints: None
	 * required if: ?=-0.5
	 */
	SAM_EXPORT void SAM_Pvwattsv11ts_PVWatts_gamma_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set hour: Hour [hr]
	 * options: 0-23
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvwattsv11ts_PVWatts_hour_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set i_ref: Rating condition irradiance [W/m2]
	 * options: None
	 * constraints: POSITIVE
	 * required if: ?=1000
	 */
	SAM_EXPORT void SAM_Pvwattsv11ts_PVWatts_i_ref_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set inv_eff: Inverter efficiency at rated power [frac]
	 * options: None
	 * constraints: MIN=0,MAX=1
	 * required if: ?=0.92
	 */
	SAM_EXPORT void SAM_Pvwattsv11ts_PVWatts_inv_eff_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set lat: Latitude [deg]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvwattsv11ts_PVWatts_lat_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set lon: Longitude [deg]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvwattsv11ts_PVWatts_lon_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set minute: Minute [min]
	 * options: 0-59
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvwattsv11ts_PVWatts_minute_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set month: Month [mn]
	 * options: 1-12
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvwattsv11ts_PVWatts_month_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set poa: Plane of array irradiance [W/m2]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvwattsv11ts_PVWatts_poa_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set poa_cutin: Min reqd irradiance for operation [W/m2]
	 * options: None
	 * constraints: MIN=0
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvwattsv11ts_PVWatts_poa_cutin_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set pressure: Pressure [millibars]
	 * options: None
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Pvwattsv11ts_PVWatts_pressure_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set rotlim: Tracker rotation limit (+/- 1 axis) [deg]
	 * options: None
	 * constraints: MIN=1,MAX=90
	 * required if: ?=45.0
	 */
	SAM_EXPORT void SAM_Pvwattsv11ts_PVWatts_rotlim_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set snow: Snow cover [cm]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvwattsv11ts_PVWatts_snow_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set system_size: Nameplate capacity [kW]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvwattsv11ts_PVWatts_system_size_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set t_noct: Nominal operating cell temperature [C]
	 * options: None
	 * constraints: POSITIVE
	 * required if: ?=45.0
	 */
	SAM_EXPORT void SAM_Pvwattsv11ts_PVWatts_t_noct_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set t_ref: Reference cell temperature [C]
	 * options: None
	 * constraints: POSITIVE
	 * required if: ?=25.0
	 */
	SAM_EXPORT void SAM_Pvwattsv11ts_PVWatts_t_ref_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tamb: Ambient temperature (dry bulb temperature) [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvwattsv11ts_PVWatts_tamb_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tcell: Module temperature [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvwattsv11ts_PVWatts_tcell_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tilt: Tilt angle [deg]
	 * options: H=0,V=90
	 * constraints: MIN=0,MAX=90
	 * required if: naof:tilt_eq_lat
	 */
	SAM_EXPORT void SAM_Pvwattsv11ts_PVWatts_tilt_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set time_step: Time step of input data [hr]
	 * options: None
	 * constraints: POSITIVE
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Pvwattsv11ts_PVWatts_time_step_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set track_mode: Tracking mode [0/1/2/3]
	 * options: Fixed,1Axis,2Axis,AziAxis
	 * constraints: MIN=0,MAX=3,INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvwattsv11ts_PVWatts_track_mode_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tz: Time zone [hr]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvwattsv11ts_PVWatts_tz_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set w_stow: Wind stow speed [m/s]
	 * options: None
	 * constraints: MIN=0
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvwattsv11ts_PVWatts_w_stow_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set wspd: Wind speed [m/s]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvwattsv11ts_PVWatts_wspd_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set year: Year [yr]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvwattsv11ts_PVWatts_year_nset(SAM_table ptr, double number, SAM_error *err);


	/**
	 * PVWatts Getters
	 */

	SAM_EXPORT double SAM_Pvwattsv11ts_PVWatts_azimuth_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv11ts_PVWatts_beam_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv11ts_PVWatts_day_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv11ts_PVWatts_derate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv11ts_PVWatts_diffuse_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv11ts_PVWatts_elevation_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv11ts_PVWatts_fd_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv11ts_PVWatts_gamma_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv11ts_PVWatts_hour_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv11ts_PVWatts_i_ref_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv11ts_PVWatts_inv_eff_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv11ts_PVWatts_lat_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv11ts_PVWatts_lon_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv11ts_PVWatts_minute_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv11ts_PVWatts_month_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv11ts_PVWatts_poa_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv11ts_PVWatts_poa_cutin_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv11ts_PVWatts_pressure_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv11ts_PVWatts_rotlim_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv11ts_PVWatts_snow_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv11ts_PVWatts_system_size_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv11ts_PVWatts_t_noct_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv11ts_PVWatts_t_ref_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv11ts_PVWatts_tamb_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv11ts_PVWatts_tcell_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv11ts_PVWatts_tilt_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv11ts_PVWatts_time_step_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv11ts_PVWatts_track_mode_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv11ts_PVWatts_tz_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv11ts_PVWatts_w_stow_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv11ts_PVWatts_wspd_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv11ts_PVWatts_year_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double SAM_Pvwattsv11ts_Outputs_ac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv11ts_Outputs_dc_nget(SAM_table ptr, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif