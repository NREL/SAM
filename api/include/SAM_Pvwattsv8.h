#ifndef SAM_PVWATTSV8_H_
#define SAM_PVWATTSV8_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// Pvwattsv8 Technology Model
	//

	/** 
	 * Create a Pvwattsv8 variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_Pvwattsv8;

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_Pvwattsv8_execute(SAM_table data, int verbosity, SAM_error* err);


	//
	// SolarResource parameters
	//

	/**
	 * Set albedo: Albedo [0..1]
	 * options: array of 1 constant value or 12 monthly values for use when weather file albedo data not available
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvwattsv8_SolarResource_albedo_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set albedo_default: Albedo default [0..1]
	 * options: default when albedo input data is invalid
	 * constraints: None
	 * required if: ?=0.2
	 */
	SAM_EXPORT void SAM_Pvwattsv8_SolarResource_albedo_default_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set albedo_default_snow: Albedo default for snow [0..1]
	 * options: default when albedo input data is invalid with snow
	 * constraints: None
	 * required if: ?=0.6
	 */
	SAM_EXPORT void SAM_Pvwattsv8_SolarResource_albedo_default_snow_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set solar_resource_data: Weather data
	 * options: dn,df,tdry,wspd,lat,lon,tz,elev
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvwattsv8_SolarResource_solar_resource_data_tset(SAM_table ptr, SAM_table tab, SAM_error *err);

	/**
	 * Set solar_resource_file: Weather file path
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvwattsv8_SolarResource_solar_resource_file_sset(SAM_table ptr, const char* str, SAM_error *err);

	/**
	 * Set use_wf_albedo: Use albedo from weather file [0/1]
	 * options: use albedo input, use weather file albedo if available
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvwattsv8_SolarResource_use_wf_albedo_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// Lifetime parameters
	//

	/**
	 * Set analysis_period: Analysis period [years]
	 * options: None
	 * constraints: None
	 * required if: system_use_lifetime_output=1
	 */
	SAM_EXPORT void SAM_Pvwattsv8_Lifetime_analysis_period_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dc_degradation: Annual DC degradation for lifetime simulations [%/year]
	 * options: None
	 * constraints: None
	 * required if: system_use_lifetime_output=1
	 */
	SAM_EXPORT void SAM_Pvwattsv8_Lifetime_dc_degradation_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set system_use_lifetime_output: Run lifetime simulation [0/1]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvwattsv8_Lifetime_system_use_lifetime_output_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// SystemDesign parameters
	//

	/**
	 * Set array_type: Array type [0/1/2/3/4]
	 * options: fixed open rack,fixed roof mount,1-axis tracking,1-axis backtracking,2-axis tracking
	 * constraints: MIN=0,MAX=4,INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvwattsv8_SystemDesign_array_type_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set azimuth: Azimuth angle [deg]
	 * options: E=90,S=180,W=270
	 * constraints: MIN=0,MAX=360
	 * required if: array_type<4
	 */
	SAM_EXPORT void SAM_Pvwattsv8_SystemDesign_azimuth_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_simple_enable: Enable Battery [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvwattsv8_SystemDesign_batt_simple_enable_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set bifaciality: Module bifaciality factor [0 or ~0.65]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvwattsv8_SystemDesign_bifaciality_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dc_ac_ratio: DC to AC ratio [ratio]
	 * options: None
	 * constraints: POSITIVE
	 * required if: ?=1.1
	 */
	SAM_EXPORT void SAM_Pvwattsv8_SystemDesign_dc_ac_ratio_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set en_snowloss: Enable snow loss model [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvwattsv8_SystemDesign_en_snowloss_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set enable_wind_stow: Enable tracker stow at high wind speeds [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvwattsv8_SystemDesign_enable_wind_stow_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set gcr: Ground coverage ratio [0..1]
	 * options: None
	 * constraints: MIN=0.01,MAX=0.99
	 * required if: ?=0.4
	 */
	SAM_EXPORT void SAM_Pvwattsv8_SystemDesign_gcr_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set gust_factor: Wind gust estimation factor
	 * options: None
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Pvwattsv8_SystemDesign_gust_factor_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set inv_eff: Inverter efficiency at rated power [%]
	 * options: None
	 * constraints: MIN=90,MAX=99.5
	 * required if: ?=96
	 */
	SAM_EXPORT void SAM_Pvwattsv8_SystemDesign_inv_eff_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set losses: Other DC losses [%]
	 * options: total system losses
	 * constraints: MIN=-5,MAX=99
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvwattsv8_SystemDesign_losses_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set module_type: Module type [0/1/2]
	 * options: standard,premium,thin film
	 * constraints: MIN=0,MAX=2,INTEGER
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvwattsv8_SystemDesign_module_type_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set rotlim: Tracker rotation angle limit [deg]
	 * options: None
	 * constraints: None
	 * required if: ?=45.0
	 */
	SAM_EXPORT void SAM_Pvwattsv8_SystemDesign_rotlim_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set shading:azal: Azimuth x altitude beam shading loss [%]
	 * options: None
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Pvwattsv8_SystemDesign_shading_azal_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set shading:diff: Diffuse shading loss [%]
	 * options: None
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Pvwattsv8_SystemDesign_shading_diff_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set shading:mxh: Month x Hour beam shading loss [%]
	 * options: None
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Pvwattsv8_SystemDesign_shading_mxh_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set shading:timestep: Time step beam shading loss [%]
	 * options: None
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Pvwattsv8_SystemDesign_shading_timestep_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set soiling: Soiling loss [%]
	 * options: None
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Pvwattsv8_SystemDesign_soiling_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set stow_wspd: Tracker stow wind speed threshold [m/s]
	 * options: None
	 * constraints: None
	 * required if: ?=10
	 */
	SAM_EXPORT void SAM_Pvwattsv8_SystemDesign_stow_wspd_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set system_capacity: System size (DC nameplate) [kW]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvwattsv8_SystemDesign_system_capacity_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tilt: Tilt angle [deg]
	 * options: H=0,V=90
	 * constraints: MIN=0,MAX=90
	 * required if: array_type<4
	 */
	SAM_EXPORT void SAM_Pvwattsv8_SystemDesign_tilt_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set wind_stow_angle: Tracker angle for wind stow [deg]
	 * options: None
	 * constraints: None
	 * required if: ?=30.0
	 */
	SAM_EXPORT void SAM_Pvwattsv8_SystemDesign_wind_stow_angle_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set xfmr_ll: GSU transformer load loss (resistive) [%(ac)]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Pvwattsv8_SystemDesign_xfmr_ll_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set xfmr_nll: GSU transformer no load loss (iron core) [%(ac)]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Pvwattsv8_SystemDesign_xfmr_nll_nset(SAM_table ptr, double number, SAM_error *err);


	/**
	 * SolarResource Getters
	 */

	SAM_EXPORT double* SAM_Pvwattsv8_SolarResource_albedo_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv8_SolarResource_albedo_default_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv8_SolarResource_albedo_default_snow_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT SAM_table SAM_Pvwattsv8_SolarResource_solar_resource_data_tget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT const char* SAM_Pvwattsv8_SolarResource_solar_resource_file_sget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv8_SolarResource_use_wf_albedo_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Lifetime Getters
	 */

	SAM_EXPORT double SAM_Pvwattsv8_Lifetime_analysis_period_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv8_Lifetime_dc_degradation_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv8_Lifetime_system_use_lifetime_output_nget(SAM_table ptr, SAM_error *err);


	/**
	 * SystemDesign Getters
	 */

	SAM_EXPORT double SAM_Pvwattsv8_SystemDesign_array_type_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv8_SystemDesign_azimuth_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv8_SystemDesign_batt_simple_enable_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv8_SystemDesign_bifaciality_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv8_SystemDesign_dc_ac_ratio_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv8_SystemDesign_en_snowloss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv8_SystemDesign_enable_wind_stow_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv8_SystemDesign_gcr_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv8_SystemDesign_gust_factor_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv8_SystemDesign_inv_eff_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv8_SystemDesign_losses_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv8_SystemDesign_module_type_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv8_SystemDesign_rotlim_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv8_SystemDesign_shading_azal_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv8_SystemDesign_shading_diff_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv8_SystemDesign_shading_mxh_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv8_SystemDesign_shading_timestep_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv8_SystemDesign_soiling_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv8_SystemDesign_stow_wspd_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv8_SystemDesign_system_capacity_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv8_SystemDesign_tilt_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv8_SystemDesign_wind_stow_angle_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv8_SystemDesign_xfmr_ll_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv8_SystemDesign_xfmr_nll_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double* SAM_Pvwattsv8_Outputs_ac_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv8_Outputs_ac_annual_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv8_Outputs_ac_annual_pre_adjust_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv8_Outputs_ac_monthly_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv8_Outputs_ac_pre_adjust_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv8_Outputs_alb_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv8_Outputs_annual_energy_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv8_Outputs_annual_energy_distribution_time_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv8_Outputs_aoi_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv8_Outputs_capacity_factor_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv8_Outputs_capacity_factor_ac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT const char* SAM_Pvwattsv8_Outputs_city_sget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv8_Outputs_dc_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv8_Outputs_dc_monthly_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv8_Outputs_dcsnowderate_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv8_Outputs_df_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv8_Outputs_dn_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv8_Outputs_elev_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv8_Outputs_gen_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv8_Outputs_gh_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv8_Outputs_inv_eff_output_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv8_Outputs_inverter_efficiency_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv8_Outputs_kwh_per_kw_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv8_Outputs_lat_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT const char* SAM_Pvwattsv8_Outputs_location_sget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv8_Outputs_lon_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv8_Outputs_monthly_energy_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv8_Outputs_percent_complete_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv8_Outputs_poa_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv8_Outputs_poa_monthly_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv8_Outputs_shad_beam_factor_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv8_Outputs_snow_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv8_Outputs_soiling_f_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv8_Outputs_solrad_annual_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv8_Outputs_solrad_monthly_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv8_Outputs_ss_beam_factor_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv8_Outputs_ss_gnd_diffuse_factor_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv8_Outputs_ss_sky_diffuse_factor_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT const char* SAM_Pvwattsv8_Outputs_state_sget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv8_Outputs_sunup_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv8_Outputs_tamb_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv8_Outputs_tcell_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv8_Outputs_tpoa_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv8_Outputs_ts_shift_hours_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv8_Outputs_tz_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv8_Outputs_wspd_aget(SAM_table ptr, int* length, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif