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
	 * options: albedo input array of 1 constant value or 12 monthly values
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvwattsv8_SolarResource_albedo_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set albedo_default: Albedo default [0..1]
	 * options: default when albedo invalid
	 * constraints: None
	 * required if: ?=0.2
	 */
	SAM_EXPORT void SAM_Pvwattsv8_SolarResource_albedo_default_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set albedo_default_snow: Albedo default for snow [0..1]
	 * options: default when albedo invalid and snow model enabled
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
	 * options: 0=albedo input, 1=albedo from weather file (use albedo default if invalid)
	 * constraints: BOOLEAN
	 * required if: ?=1
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
	 * Set azimuth: Azimuth angle [degrees]
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
	 * required if: ?=0.3
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
	 * Set losses: DC system losses [%]
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
	 * Set rotlim: Tracker rotation angle limit [degrees]
	 * options: None
	 * constraints: None
	 * required if: ?=45.0
	 */
	SAM_EXPORT void SAM_Pvwattsv8_SystemDesign_rotlim_nset(SAM_table ptr, double number, SAM_error *err);

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
	 * Set tilt: Tilt angle [degrees]
	 * options: H=0,V=90
	 * constraints: MIN=0,MAX=90
	 * required if: array_type<4
	 */
	SAM_EXPORT void SAM_Pvwattsv8_SystemDesign_tilt_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set wind_stow_angle: Tracker angle for wind stow [degrees]
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


	//
	// Shading parameters
	//

	/**
	 * Set shading_azal: Azimuth x altitude beam shading losses [%]
	 * options: None
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Pvwattsv8_Shading_shading_azal_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set shading_diff: Diffuse shading loss [%]
	 * options: None
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Pvwattsv8_Shading_shading_diff_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set shading_en_azal: Enable azimuth x altitude beam shading losses [0/1]
	 * options: 0=false,1=true
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvwattsv8_Shading_shading_en_azal_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set shading_en_diff: Enable diffuse shading loss [0/1]
	 * options: 0=false,1=true
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvwattsv8_Shading_shading_en_diff_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set shading_en_mxh: Enable month x Hour beam shading losses [0/1]
	 * options: 0=false,1=true
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvwattsv8_Shading_shading_en_mxh_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set shading_en_string_option: Enable shading string option [0/1]
	 * options: 0=false,1=true
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvwattsv8_Shading_shading_en_string_option_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set shading_en_timestep: Enable timestep beam shading losses [0/1]
	 * options: 0=false,1=true
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvwattsv8_Shading_shading_en_timestep_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set shading_mxh: Month x Hour beam shading losses [%]
	 * options: None
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Pvwattsv8_Shading_shading_mxh_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set shading_string_option: Shading string option
	 * options: 0=shadingdb,1=average,2=maximum,3=minimum
	 * constraints: INTEGER,MIN=-1,MAX=4
	 * required if: ?=-1
	 */
	SAM_EXPORT void SAM_Pvwattsv8_Shading_shading_string_option_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set shading_timestep: Timestep beam shading losses [%]
	 * options: None
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Pvwattsv8_Shading_shading_timestep_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);


	//
	// AdjustmentFactors parameters
	//

	/**
	 * Set adjust_constant: Constant loss adjustment [%]
	 * options: 'adjust' and 'constant' separated by _ instead of : after SAM 2022.12.21
	 * constraints: MAX=100
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvwattsv8_AdjustmentFactors_adjust_constant_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set adjust_en_periods: Enable period-based adjustment factors [0/1]
	 * options: 'adjust' and 'en_periods' separated by _ instead of : after SAM 2022.12.21
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvwattsv8_AdjustmentFactors_adjust_en_periods_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set adjust_en_timeindex: Enable lifetime adjustment factors [0/1]
	 * options: 'adjust' and 'en_timeindex' separated by _ instead of : after SAM 2022.12.21
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvwattsv8_AdjustmentFactors_adjust_en_timeindex_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set adjust_periods: Period-based adjustment factors [%]
	 * options: Syntax: n x 3 matrix [ start, end, loss ]; Version upgrade: 'adjust' and 'periods' separated by _ instead of : after SAM 2022.12.21
	 * constraints: COLS=3
	 * required if: adjust_en_periods=1
	 */
	SAM_EXPORT void SAM_Pvwattsv8_AdjustmentFactors_adjust_periods_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set adjust_timeindex: Lifetime adjustment factors [%]
	 * options: 'adjust' and 'timeindex' separated by _ instead of : after SAM 2022.12.21
	 * constraints: None
	 * required if: adjust_en_timeindex=1
	 */
	SAM_EXPORT void SAM_Pvwattsv8_AdjustmentFactors_adjust_timeindex_aset(SAM_table ptr, double* arr, int length, SAM_error *err);


	//
	// HybridCosts parameters
	//

	/**
	 * Set degradation: Annual AC degradation [%]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Pvwattsv8_HybridCosts_degradation_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set land_area: Total land area [acres]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvwattsv8_HybridCosts_land_area_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set om_capacity: Capacity-based O&M amount [$/kWcap]
	 * options: !battery,!fuelcell
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Pvwattsv8_HybridCosts_om_capacity_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set om_capacity_escal: Capacity-based O&M escalation [%/year]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Pvwattsv8_HybridCosts_om_capacity_escal_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set om_fixed: Fixed O&M annual amount [$/year]
	 * options: !battery,!fuelcell
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Pvwattsv8_HybridCosts_om_fixed_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set om_fixed_escal: Fixed O&M escalation [%/year]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Pvwattsv8_HybridCosts_om_fixed_escal_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set om_land_lease: Land lease cost [$/acre]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvwattsv8_HybridCosts_om_land_lease_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set om_land_lease_escal: Land lease cost escalation [%/yr]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvwattsv8_HybridCosts_om_land_lease_escal_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set om_production: Production-based O&M amount [$/MWh]
	 * options: !battery,!fuelcell
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Pvwattsv8_HybridCosts_om_production_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set om_production_escal: Production-based O&M escalation [%/year]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Pvwattsv8_HybridCosts_om_production_escal_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set total_installed_cost: Total installed cost [$]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvwattsv8_HybridCosts_total_installed_cost_nset(SAM_table ptr, double number, SAM_error *err);


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

	SAM_EXPORT double* SAM_Pvwattsv8_SystemDesign_soiling_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv8_SystemDesign_stow_wspd_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv8_SystemDesign_system_capacity_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv8_SystemDesign_tilt_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv8_SystemDesign_wind_stow_angle_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv8_SystemDesign_xfmr_ll_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv8_SystemDesign_xfmr_nll_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Shading Getters
	 */

	SAM_EXPORT double* SAM_Pvwattsv8_Shading_shading_azal_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv8_Shading_shading_diff_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv8_Shading_shading_en_azal_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv8_Shading_shading_en_diff_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv8_Shading_shading_en_mxh_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv8_Shading_shading_en_string_option_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv8_Shading_shading_en_timestep_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv8_Shading_shading_mxh_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv8_Shading_shading_string_option_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv8_Shading_shading_timestep_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);


	/**
	 * AdjustmentFactors Getters
	 */

	SAM_EXPORT double SAM_Pvwattsv8_AdjustmentFactors_adjust_constant_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv8_AdjustmentFactors_adjust_en_periods_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv8_AdjustmentFactors_adjust_en_timeindex_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv8_AdjustmentFactors_adjust_periods_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv8_AdjustmentFactors_adjust_timeindex_aget(SAM_table ptr, int* length, SAM_error *err);


	/**
	 * HybridCosts Getters
	 */

	SAM_EXPORT double* SAM_Pvwattsv8_HybridCosts_degradation_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv8_HybridCosts_land_area_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv8_HybridCosts_om_capacity_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv8_HybridCosts_om_capacity_escal_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv8_HybridCosts_om_fixed_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv8_HybridCosts_om_fixed_escal_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv8_HybridCosts_om_land_lease_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv8_HybridCosts_om_land_lease_escal_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv8_HybridCosts_om_production_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv8_HybridCosts_om_production_escal_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv8_HybridCosts_total_installed_cost_nget(SAM_table ptr, SAM_error *err);


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

	SAM_EXPORT double* SAM_Pvwattsv8_Outputs_cf_battery_replacement_cost_schedule_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv8_Outputs_cf_energy_net_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv8_Outputs_cf_fuelcell_replacement_cost_schedule_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv8_Outputs_cf_land_lease_expense_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv8_Outputs_cf_om_capacity_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv8_Outputs_cf_om_fixed_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv8_Outputs_cf_om_fuel_cost_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv8_Outputs_cf_om_land_lease_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv8_Outputs_cf_om_production_aget(SAM_table ptr, int* length, SAM_error *err);

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

	SAM_EXPORT double* SAM_Pvwattsv8_Outputs_snow_cover_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv8_Outputs_soiling_f_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv8_Outputs_solrad_annual_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv8_Outputs_solrad_monthly_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv8_Outputs_ss_beam_factor_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv8_Outputs_ss_gnd_diffuse_factor_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv8_Outputs_ss_sky_diffuse_factor_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT const char* SAM_Pvwattsv8_Outputs_state_sget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv8_Outputs_sunup_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv8_Outputs_system_capacity_ac_nget(SAM_table ptr, SAM_error *err);

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