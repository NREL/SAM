#ifndef SAM_PVSAMV1_H_
#define SAM_PVSAMV1_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// Pvsamv1 Technology Model
	//

	/** 
	 * Create a Pvsamv1 variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_Pvsamv1;

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_Pvsamv1_execute(SAM_table data, int verbosity, SAM_error* err);


	//
	// SolarResource parameters
	//

	/**
	 * Set albedo: User specified ground albedo [0..1]
	 * options: None
	 * constraints: LENGTH=12
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvsamv1_SolarResource_albedo_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set irrad_mode: Irradiance input translation mode
	 * options: 0=beam&diffuse,1=total&beam,2=total&diffuse,3=poa_reference,4=poa_pyranometer
	 * constraints: INTEGER,MIN=0,MAX=4
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_SolarResource_irrad_mode_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set sky_model: Diffuse sky model
	 * options: 0=isotropic,1=hkdr,2=perez
	 * constraints: INTEGER,MIN=0,MAX=2
	 * required if: ?=2
	 */
	SAM_EXPORT void SAM_Pvsamv1_SolarResource_sky_model_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set solar_resource_data: Weather data
	 * options: lat,lon,tz,elev,year,month,hour,minute,gh,dn,df,poa,tdry,twet,tdew,rhum,pres,Snow,alb,aod,wspd,wdir
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Pvsamv1_SolarResource_solar_resource_data_tset(SAM_table ptr, SAM_table tab, SAM_error *err);

	/**
	 * Set solar_resource_file: Weather file in TMY2, TMY3, EPW, or SAM CSV
	 * options: None
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Pvsamv1_SolarResource_solar_resource_file_sset(SAM_table ptr, const char* str, SAM_error *err);

	/**
	 * Set use_wf_albedo: Use albedo in weather file if provided [0/1]
	 * options: 0=user-specified,1=weatherfile
	 * constraints: BOOLEAN
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_SolarResource_use_wf_albedo_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// Losses parameters
	//

	/**
	 * Set acwiring_loss: AC wiring loss [%]
	 * options: None
	 * constraints: MIN=0,MAX=100
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvsamv1_Losses_acwiring_loss_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dcoptimizer_loss: DC power optimizer loss [%]
	 * options: None
	 * constraints: MIN=0,MAX=100
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvsamv1_Losses_dcoptimizer_loss_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set en_snow_model: Toggle snow loss estimation [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_Losses_en_snow_model_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray1_dcwiring_loss: Sub-array 1 DC wiring loss [%]
	 * options: None
	 * constraints: MIN=0,MAX=100
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvsamv1_Losses_subarray1_dcwiring_loss_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray1_diodeconn_loss: Sub-array 1 DC diodes and connections loss [%]
	 * options: None
	 * constraints: MIN=0,MAX=100
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvsamv1_Losses_subarray1_diodeconn_loss_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray1_mismatch_loss: Sub-array 1 DC mismatch loss [%]
	 * options: None
	 * constraints: MIN=0,MAX=100
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvsamv1_Losses_subarray1_mismatch_loss_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray1_nameplate_loss: Sub-array 1 DC nameplate loss [%]
	 * options: None
	 * constraints: MIN=-5,MAX=100
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvsamv1_Losses_subarray1_nameplate_loss_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray1_rear_irradiance_loss: Sub-array 1 rear irradiance loss [%]
	 * options: None
	 * constraints: MIN=0,MAX=100
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvsamv1_Losses_subarray1_rear_irradiance_loss_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray1_soiling: Sub-array 1 Monthly soiling loss [%]
	 * options: None
	 * constraints: LENGTH=12
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvsamv1_Losses_subarray1_soiling_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set subarray1_tracking_loss: Sub-array 1 DC tracking error loss [%]
	 * options: None
	 * constraints: MIN=0,MAX=100
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvsamv1_Losses_subarray1_tracking_loss_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray2_dcwiring_loss: Sub-array 2 DC wiring loss [%]
	 * options: None
	 * constraints: MIN=0,MAX=100
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Pvsamv1_Losses_subarray2_dcwiring_loss_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray2_diodeconn_loss: Sub-array 2 DC diodes and connections loss [%]
	 * options: None
	 * constraints: MIN=0,MAX=100
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Pvsamv1_Losses_subarray2_diodeconn_loss_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray2_mismatch_loss: Sub-array 2 DC mismatch loss [%]
	 * options: None
	 * constraints: MIN=0,MAX=100
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Pvsamv1_Losses_subarray2_mismatch_loss_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray2_nameplate_loss: Sub-array 2 DC nameplate loss [%]
	 * options: None
	 * constraints: MIN=-5,MAX=100
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Pvsamv1_Losses_subarray2_nameplate_loss_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray2_rear_irradiance_loss: Sub-array 2 rear irradiance loss [%]
	 * options: None
	 * constraints: MIN=0,MAX=100
	 * required if: subarray2_enable=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_Losses_subarray2_rear_irradiance_loss_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray2_soiling: Sub-array 2 Monthly soiling loss [%]
	 * options: None
	 * constraints: LENGTH=12
	 * required if: subarray2_enable=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_Losses_subarray2_soiling_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set subarray2_tracking_loss: Sub-array 2 DC tracking error loss [%]
	 * options: None
	 * constraints: MIN=0,MAX=100
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Pvsamv1_Losses_subarray2_tracking_loss_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray3_dcwiring_loss: Sub-array 3 DC wiring loss [%]
	 * options: None
	 * constraints: MIN=0,MAX=100
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Pvsamv1_Losses_subarray3_dcwiring_loss_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray3_diodeconn_loss: Sub-array 3 DC diodes and connections loss [%]
	 * options: None
	 * constraints: MIN=0,MAX=100
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Pvsamv1_Losses_subarray3_diodeconn_loss_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray3_mismatch_loss: Sub-array 3 DC mismatch loss [%]
	 * options: None
	 * constraints: MIN=0,MAX=100
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Pvsamv1_Losses_subarray3_mismatch_loss_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray3_nameplate_loss: Sub-array 3 DC nameplate loss [%]
	 * options: None
	 * constraints: MIN=-5,MAX=100
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Pvsamv1_Losses_subarray3_nameplate_loss_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray3_rear_irradiance_loss: Sub-array 3 rear irradiance loss [%]
	 * options: None
	 * constraints: MIN=0,MAX=100
	 * required if: subarray3_enable=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_Losses_subarray3_rear_irradiance_loss_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray3_soiling: Sub-array 3 Monthly soiling loss [%]
	 * options: None
	 * constraints: LENGTH=12
	 * required if: subarray3_enable=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_Losses_subarray3_soiling_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set subarray3_tracking_loss: Sub-array 3 DC tracking error loss [%]
	 * options: None
	 * constraints: MIN=0,MAX=100
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Pvsamv1_Losses_subarray3_tracking_loss_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray4_dcwiring_loss: Sub-array 4 DC wiring loss [%]
	 * options: None
	 * constraints: MIN=0,MAX=100
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Pvsamv1_Losses_subarray4_dcwiring_loss_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray4_diodeconn_loss: Sub-array 4 DC diodes and connections loss [%]
	 * options: ?
	 * constraints: MIN=0,MAX=100
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Pvsamv1_Losses_subarray4_diodeconn_loss_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray4_mismatch_loss: Sub-array 4 DC mismatch loss [%]
	 * options: None
	 * constraints: MIN=0,MAX=100
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Pvsamv1_Losses_subarray4_mismatch_loss_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray4_nameplate_loss: Sub-array 4 DC nameplate loss [%]
	 * options: None
	 * constraints: MIN=-5,MAX=100
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Pvsamv1_Losses_subarray4_nameplate_loss_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray4_rear_irradiance_loss: Sub-array 4 rear irradiance loss [%]
	 * options: None
	 * constraints: MIN=0,MAX=100
	 * required if: subarray4_enable=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_Losses_subarray4_rear_irradiance_loss_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray4_soiling: Sub-array 4 Monthly soiling loss [%]
	 * options: None
	 * constraints: LENGTH=12
	 * required if: subarray4_enable=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_Losses_subarray4_soiling_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set subarray4_tracking_loss: Sub-array 4 DC tracking error loss [%]
	 * options: None
	 * constraints: MIN=0,MAX=100
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Pvsamv1_Losses_subarray4_tracking_loss_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set transformer_load_loss: Power transformer load loss [%]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_Losses_transformer_load_loss_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set transformer_no_load_loss: Power transformer no load loss [%]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_Losses_transformer_no_load_loss_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set transmission_loss: Transmission loss [%]
	 * options: None
	 * constraints: MIN=0,MAX=100
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvsamv1_Losses_transmission_loss_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// Lifetime parameters
	//

	/**
	 * Set ac_lifetime_losses: Lifetime daily AC losses [%]
	 * options: None
	 * constraints: None
	 * required if: en_ac_lifetime_losses=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_Lifetime_ac_lifetime_losses_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set analysis_period: Lifetime analysis period [years]
	 * options: None
	 * constraints: None
	 * required if: system_use_lifetime_output=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_Lifetime_analysis_period_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dc_degradation: Annual DC degradation [%/year]
	 * options: None
	 * constraints: None
	 * required if: system_use_lifetime_output=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_Lifetime_dc_degradation_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set dc_lifetime_losses: Lifetime daily DC losses [%]
	 * options: None
	 * constraints: None
	 * required if: en_dc_lifetime_losses=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_Lifetime_dc_lifetime_losses_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set en_ac_lifetime_losses: Enable lifetime daily AC losses [0/1]
	 * options: None
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_Lifetime_en_ac_lifetime_losses_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set en_dc_lifetime_losses: Enable lifetime daily DC losses [0/1]
	 * options: None
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_Lifetime_en_dc_lifetime_losses_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set inflation_rate: Inflation rate [%]
	 * options: None
	 * constraints: MIN=-99
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_Lifetime_inflation_rate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set save_full_lifetime_variables: Save and display vars for full lifetime [0/1]
	 * options: None
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: system_use_lifetime_output=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_Lifetime_save_full_lifetime_variables_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set system_use_lifetime_output: PV lifetime simulation [0/1]
	 * options: None
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_Lifetime_system_use_lifetime_output_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// SystemDesign parameters
	//

	/**
	 * Set enable_mismatch_vmax_calc: Enable mismatched subarray Vmax calculation
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_SystemDesign_enable_mismatch_vmax_calc_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set inverter_count: Number of inverters
	 * options: None
	 * constraints: INTEGER,POSITIVE
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvsamv1_SystemDesign_inverter_count_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray1_azimuth: Sub-array 1 Azimuth [deg]
	 * options: 0=N,90=E,180=S,270=W
	 * constraints: MIN=0,MAX=359.9
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray1_azimuth_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray1_backtrack: Sub-array 1 Backtracking enabled
	 * options: 0=no backtracking,1=backtrack
	 * constraints: BOOLEAN
	 * required if: subarray1_track_mode=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray1_backtrack_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray1_gcr: Sub-array 1 Ground coverage ratio [0..1]
	 * options: None
	 * constraints: MIN=0.01,MAX=0.99
	 * required if: ?=0.3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray1_gcr_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray1_modules_per_string: Sub-array 1 Modules per string
	 * options: None
	 * constraints: INTEGER,POSITIVE
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray1_modules_per_string_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray1_monthly_tilt: Sub-array 1 monthly tilt input [deg]
	 * options: None
	 * constraints: LENGTH=12
	 * required if: subarray1_track_mode=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray1_monthly_tilt_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set subarray1_mppt_input: Sub-array 1 Inverter MPPT input number
	 * options: None
	 * constraints: INTEGER,POSITIVE
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray1_mppt_input_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray1_nstrings: Sub-array 1 Number of parallel strings
	 * options: None
	 * constraints: INTEGER
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray1_nstrings_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray1_rotlim: Sub-array 1 Tracker rotation limit [deg]
	 * options: None
	 * constraints: MIN=0,MAX=85
	 * required if: ?=45
	 */
	SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray1_rotlim_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray1_tilt: Sub-array 1 Tilt [deg]
	 * options: 0=horizontal,90=vertical
	 * constraints: MIN=0,MAX=90
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray1_tilt_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray1_tilt_eq_lat: Sub-array 1 Tilt=latitude override [0/1]
	 * options: 0=false,1=override
	 * constraints: BOOLEAN
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray1_tilt_eq_lat_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray1_track_mode: Sub-array 1 Tracking mode
	 * options: 0=fixed,1=1axis,2=2axis,3=azi,4=monthly
	 * constraints: INTEGER,MIN=0,MAX=4
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray1_track_mode_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray2_azimuth: Sub-array 2 Azimuth [deg]
	 * options: 0=N,90=E,180=S,270=W
	 * constraints: MIN=0,MAX=359.9
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray2_azimuth_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray2_backtrack: Sub-array 2 Backtracking enabled
	 * options: 0=no backtracking,1=backtrack
	 * constraints: BOOLEAN
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray2_backtrack_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray2_enable: Sub-array 2 Enable [0/1]
	 * options: 0=disabled,1=enabled
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray2_enable_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray2_gcr: Sub-array 2 Ground coverage ratio [0..1]
	 * options: None
	 * constraints: MIN=0.01,MAX=0.99
	 * required if: ?=0.3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray2_gcr_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray2_modules_per_string: Sub-array 2 Modules per string
	 * options: None
	 * constraints: INTEGER,MIN=1
	 * required if: subarray2_enable=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray2_modules_per_string_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray2_monthly_tilt: Sub-array 2 Monthly tilt input [deg]
	 * options: None
	 * constraints: LENGTH=12
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray2_monthly_tilt_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set subarray2_mppt_input: Sub-array 2 Inverter MPPT input number
	 * options: None
	 * constraints: INTEGER,POSITIVE
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray2_mppt_input_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray2_nstrings: Sub-array 2 Number of parallel strings
	 * options: None
	 * constraints: INTEGER,MIN=1
	 * required if: subarray2_enable=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray2_nstrings_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray2_rotlim: Sub-array 2 Tracker rotation limit [deg]
	 * options: None
	 * constraints: MIN=0,MAX=85
	 * required if: ?=45
	 */
	SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray2_rotlim_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray2_tilt: Sub-array 2 Tilt [deg]
	 * options: 0=horizontal,90=vertical
	 * constraints: MIN=0,MAX=90
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray2_tilt_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray2_tilt_eq_lat: Sub-array 2 Tilt=latitude override [0/1]
	 * options: 0=false,1=override
	 * constraints: BOOLEAN
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray2_tilt_eq_lat_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray2_track_mode: Sub-array 2 Tracking mode
	 * options: 0=fixed,1=1axis,2=2axis,3=azi,4=monthly
	 * constraints: INTEGER,MIN=0,MAX=4
	 * required if: subarray2_enable=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray2_track_mode_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray3_azimuth: Sub-array 3 Azimuth [deg]
	 * options: 0=N,90=E,180=S,270=W
	 * constraints: MIN=0,MAX=359.9
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray3_azimuth_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray3_backtrack: Sub-array 3 Backtracking enabled
	 * options: 0=no backtracking,1=backtrack
	 * constraints: BOOLEAN
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray3_backtrack_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray3_enable: Sub-array 3 Enable [0/1]
	 * options: 0=disabled,1=enabled
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray3_enable_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray3_gcr: Sub-array 3 Ground coverage ratio [0..1]
	 * options: None
	 * constraints: MIN=0.01,MAX=0.99
	 * required if: ?=0.3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray3_gcr_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray3_modules_per_string: Sub-array 3 Modules per string
	 * options: None
	 * constraints: INTEGER,MIN=1
	 * required if: subarray3_enable=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray3_modules_per_string_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray3_monthly_tilt: Sub-array 3 Monthly tilt input [deg]
	 * options: None
	 * constraints: LENGTH=12
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray3_monthly_tilt_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set subarray3_mppt_input: Sub-array 3 Inverter MPPT input number
	 * options: None
	 * constraints: INTEGER,POSITIVE
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray3_mppt_input_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray3_nstrings: Sub-array 3 Number of parallel strings
	 * options: None
	 * constraints: INTEGER,MIN=1
	 * required if: subarray3_enable=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray3_nstrings_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray3_rotlim: Sub-array 3 Tracker rotation limit [deg]
	 * options: None
	 * constraints: MIN=0,MAX=85
	 * required if: ?=45
	 */
	SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray3_rotlim_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray3_tilt: Sub-array 3 Tilt [deg]
	 * options: 0=horizontal,90=vertical
	 * constraints: MIN=0,MAX=90
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray3_tilt_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray3_tilt_eq_lat: Sub-array 3 Tilt=latitude override [0/1]
	 * options: 0=false,1=override
	 * constraints: BOOLEAN
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray3_tilt_eq_lat_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray3_track_mode: Sub-array 3 Tracking mode
	 * options: 0=fixed,1=1axis,2=2axis,3=azi,4=monthly
	 * constraints: INTEGER,MIN=0,MAX=4
	 * required if: subarray3_enable=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray3_track_mode_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray4_azimuth: Sub-array 4 Azimuth [deg]
	 * options: 0=N,90=E,180=S,270=W
	 * constraints: MIN=0,MAX=359.9
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray4_azimuth_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray4_backtrack: Sub-array 4 Backtracking enabled
	 * options: 0=no backtracking,1=backtrack
	 * constraints: BOOLEAN
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray4_backtrack_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray4_enable: Sub-array 4 Enable [0/1]
	 * options: 0=disabled,1=enabled
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray4_enable_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray4_gcr: Sub-array 4 Ground coverage ratio [0..1]
	 * options: None
	 * constraints: MIN=0.01,MAX=0.99
	 * required if: ?=0.3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray4_gcr_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray4_modules_per_string: Sub-array 4 Modules per string
	 * options: None
	 * constraints: INTEGER,MIN=1
	 * required if: subarray4_enable=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray4_modules_per_string_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray4_monthly_tilt: Sub-array 4 Monthly tilt input [deg]
	 * options: None
	 * constraints: LENGTH=12
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray4_monthly_tilt_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set subarray4_mppt_input: Sub-array 4 Inverter MPPT input number
	 * options: None
	 * constraints: INTEGER,POSITIVE
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray4_mppt_input_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray4_nstrings: Sub-array 4 Number of parallel strings
	 * options: None
	 * constraints: INTEGER,MIN=1
	 * required if: subarray4_enable=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray4_nstrings_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray4_rotlim: Sub-array 4 Tracker rotation limit [deg]
	 * options: None
	 * constraints: MIN=0,MAX=85
	 * required if: ?=45
	 */
	SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray4_rotlim_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray4_tilt: Sub-array 4 Tilt [deg]
	 * options: 0=horizontal,90=vertical
	 * constraints: MIN=0,MAX=90
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray4_tilt_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray4_tilt_eq_lat: Sub-array 4 Tilt=latitude override [0/1]
	 * options: 0=false,1=override
	 * constraints: BOOLEAN
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray4_tilt_eq_lat_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray4_track_mode: Sub-array 4 Tracking mode
	 * options: 0=fixed,1=1axis,2=2axis,3=azi,4=monthly
	 * constraints: INTEGER,MIN=0,MAX=4
	 * required if: subarray4_enable=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray4_track_mode_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set system_capacity: DC Nameplate capacity [kWdc]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvsamv1_SystemDesign_system_capacity_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// Shading parameters
	//

	/**
	 * Set subarray1_shade_mode: Sub-array 1 shading mode (fixed tilt or 1x tracking) [0/1/2]
	 * options: 0=none,1=standard(non-linear),2=thin film(linear)
	 * constraints: INTEGER,MIN=0,MAX=2
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvsamv1_Shading_subarray1_shade_mode_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray1_shading:azal: Sub-array 1 Azimuth x altitude beam shading losses [%]
	 * options: None
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Pvsamv1_Shading_subarray1_shading_azal_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set subarray1_shading:diff: Sub-array 1 Diffuse shading loss [%]
	 * options: None
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Pvsamv1_Shading_subarray1_shading_diff_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray1_shading:mxh: Sub-array 1 Month x Hour beam shading losses [%]
	 * options: None
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Pvsamv1_Shading_subarray1_shading_mxh_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set subarray1_shading:string_option: Sub-array 1 shading string option
	 * options: 0=shadingdb,1=average,2=maximum,3=minimum
	 * constraints: INTEGER,MIN=-1,MAX=4
	 * required if: ?=-1
	 */
	SAM_EXPORT void SAM_Pvsamv1_Shading_subarray1_shading_string_option_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray1_shading:timestep: Sub-array 1 timestep beam shading losses [%]
	 * options: None
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Pvsamv1_Shading_subarray1_shading_timestep_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set subarray2_shade_mode: Sub-array 2 Shading mode (fixed tilt or 1x tracking) [0/1/2]
	 * options: 0=none,1=standard(non-linear),2=thin film(linear)
	 * constraints: INTEGER,MIN=0,MAX=2
	 * required if: subarray2_enable=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_Shading_subarray2_shade_mode_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray2_shading:azal: Sub-array 2 Azimuth x altitude beam shading losses [%]
	 * options: None
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Pvsamv1_Shading_subarray2_shading_azal_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set subarray2_shading:diff: Sub-array 2 Diffuse shading loss [%]
	 * options: None
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Pvsamv1_Shading_subarray2_shading_diff_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray2_shading:mxh: Sub-array 2 Month x Hour beam shading losses [%]
	 * options: None
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Pvsamv1_Shading_subarray2_shading_mxh_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set subarray2_shading:string_option: Sub-array 2 Shading string option
	 * options: 0=shadingdb,1=average,2=maximum,3=minimum
	 * constraints: INTEGER,MIN=-1,MAX=4
	 * required if: ?=-1
	 */
	SAM_EXPORT void SAM_Pvsamv1_Shading_subarray2_shading_string_option_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray2_shading:timestep: Sub-array 2 Timestep beam shading losses [%]
	 * options: None
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Pvsamv1_Shading_subarray2_shading_timestep_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set subarray3_shade_mode: Sub-array 3 Shading mode (fixed tilt or 1x tracking) [0/1/2]
	 * options: 0=none,1=standard(non-linear),2=thin film(linear)
	 * constraints: INTEGER,MIN=0,MAX=2
	 * required if: subarray3_enable=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_Shading_subarray3_shade_mode_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray3_shading:azal: Sub-array 3 Azimuth x altitude beam shading losses [%]
	 * options: None
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Pvsamv1_Shading_subarray3_shading_azal_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set subarray3_shading:diff: Sub-array 3 Diffuse shading loss [%]
	 * options: None
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Pvsamv1_Shading_subarray3_shading_diff_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray3_shading:mxh: Sub-array 3 Month x Hour beam shading losses [%]
	 * options: None
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Pvsamv1_Shading_subarray3_shading_mxh_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set subarray3_shading:string_option: Sub-array 3 Shading string option
	 * options: 0=shadingdb,1=average,2=maximum,3=minimum
	 * constraints: INTEGER,MIN=-1,MAX=4
	 * required if: ?=-1
	 */
	SAM_EXPORT void SAM_Pvsamv1_Shading_subarray3_shading_string_option_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray3_shading:timestep: Sub-array 3 Timestep beam shading losses [%]
	 * options: None
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Pvsamv1_Shading_subarray3_shading_timestep_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set subarray4_shade_mode: Sub-array 4 shading mode (fixed tilt or 1x tracking) [0/1/2]
	 * options: 0=none,1=standard(non-linear),2=thin film(linear)
	 * constraints: INTEGER,MIN=0,MAX=2
	 * required if: subarray4_enable=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_Shading_subarray4_shade_mode_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray4_shading:azal: Sub-array 4 Azimuth x altitude beam shading losses [%]
	 * options: None
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Pvsamv1_Shading_subarray4_shading_azal_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set subarray4_shading:diff: Sub-array 4 Diffuse shading loss [%]
	 * options: None
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Pvsamv1_Shading_subarray4_shading_diff_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray4_shading:mxh: Sub-array 4 Month x Hour beam shading losses [%]
	 * options: None
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Pvsamv1_Shading_subarray4_shading_mxh_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set subarray4_shading:string_option: Sub-array 4 Shading string option
	 * options: 0=shadingdb,1=average,2=maximum,3=minimum
	 * constraints: INTEGER,MIN=-1,MAX=4
	 * required if: ?=-1
	 */
	SAM_EXPORT void SAM_Pvsamv1_Shading_subarray4_shading_string_option_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray4_shading:timestep: Sub-array 4 Timestep beam shading losses [%]
	 * options: None
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Pvsamv1_Shading_subarray4_shading_timestep_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);


	//
	// Layout parameters
	//

	/**
	 * Set module_aspect_ratio: Module aspect ratio
	 * options: None
	 * constraints: POSITIVE
	 * required if: ?=1.7
	 */
	SAM_EXPORT void SAM_Pvsamv1_Layout_module_aspect_ratio_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray1_mod_orient: Sub-array 1 Module orientation [0/1]
	 * options: 0=portrait,1=landscape
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvsamv1_Layout_subarray1_mod_orient_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray1_nmodx: Sub-array 1 Number of modules along bottom of row
	 * options: None
	 * constraints: INTEGER,POSITIVE
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvsamv1_Layout_subarray1_nmodx_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray1_nmody: Sub-array 1 Number of modules along side of row
	 * options: None
	 * constraints: INTEGER,POSITIVE
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvsamv1_Layout_subarray1_nmody_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray2_mod_orient: Sub-array 2 Module orientation [0/1]
	 * options: 0=portrait,1=landscape
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: subarray2_enable=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_Layout_subarray2_mod_orient_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray2_nmodx: Sub-array 2 Number of modules along bottom of row
	 * options: None
	 * constraints: INTEGER,POSITIVE
	 * required if: subarray2_enable=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_Layout_subarray2_nmodx_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray2_nmody: Sub-array 2 Number of modules along side of row
	 * options: None
	 * constraints: INTEGER,POSITIVE
	 * required if: subarray2_enable=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_Layout_subarray2_nmody_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray3_mod_orient: Sub-array 3 Module orientation [0/1]
	 * options: 0=portrait,1=landscape
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: subarray3_enable=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_Layout_subarray3_mod_orient_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray3_nmodx: Sub-array 3 Number of modules along bottom of row
	 * options: None
	 * constraints: INTEGER,POSITIVE
	 * required if: subarray3_enable=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_Layout_subarray3_nmodx_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray3_nmody: Sub-array 3 Number of modules along side of row
	 * options: None
	 * constraints: INTEGER,POSITIVE
	 * required if: subarray3_enable=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_Layout_subarray3_nmody_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray4_mod_orient: Sub-array 4 Module orientation [0/1]
	 * options: 0=portrait,1=landscape
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: subarray4_enable=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_Layout_subarray4_mod_orient_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray4_nmodx: Sub-array 4 Number of modules along bottom of row
	 * options: None
	 * constraints: INTEGER,POSITIVE
	 * required if: subarray4_enable=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_Layout_subarray4_nmodx_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subarray4_nmody: Sub-array 4 Number of modules along side of row
	 * options: None
	 * constraints: INTEGER,POSITIVE
	 * required if: subarray4_enable=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_Layout_subarray4_nmody_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// Module parameters
	//

	/**
	 * Set module_model: Photovoltaic module model specifier
	 * options: 0=spe,1=cec,2=6par_user,3=snl,4=sd11-iec61853,5=PVYield
	 * constraints: INTEGER,MIN=0,MAX=5
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvsamv1_Module_module_model_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// SimpleEfficiencyModuleModel parameters
	//

	/**
	 * Set spe_a: Cell temp parameter a
	 * options: None
	 * constraints: None
	 * required if: module_model=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_a_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set spe_area: Module area [m2]
	 * options: None
	 * constraints: None
	 * required if: module_model=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_area_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set spe_b: Cell temp parameter b
	 * options: None
	 * constraints: None
	 * required if: module_model=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_b_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set spe_bifacial_ground_clearance_height: Module ground clearance height [m]
	 * options: None
	 * constraints: None
	 * required if: module_model=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_bifacial_ground_clearance_height_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set spe_bifacial_transmission_factor: Bifacial transmission factor [0-1]
	 * options: None
	 * constraints: None
	 * required if: module_model=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_bifacial_transmission_factor_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set spe_bifaciality: Bifaciality factor [%]
	 * options: None
	 * constraints: None
	 * required if: module_model=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_bifaciality_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set spe_dT: Cell temp parameter dT
	 * options: None
	 * constraints: None
	 * required if: module_model=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_dT_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set spe_eff0: Efficiency at irradiance level 0 [%]
	 * options: None
	 * constraints: None
	 * required if: module_model=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_eff0_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set spe_eff1: Efficiency at irradiance level 1 [%]
	 * options: None
	 * constraints: None
	 * required if: module_model=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_eff1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set spe_eff2: Efficiency at irradiance level 2 [%]
	 * options: None
	 * constraints: None
	 * required if: module_model=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_eff2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set spe_eff3: Efficiency at irradiance level 3 [%]
	 * options: None
	 * constraints: None
	 * required if: module_model=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_eff3_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set spe_eff4: Efficiency at irradiance level 4 [%]
	 * options: None
	 * constraints: None
	 * required if: module_model=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_eff4_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set spe_fd: Diffuse fraction [0..1]
	 * options: None
	 * constraints: MIN=0,MAX=1
	 * required if: module_model=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_fd_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set spe_is_bifacial: Modules are bifacial [0/1]
	 * options: None
	 * constraints: None
	 * required if: module_model=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_is_bifacial_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set spe_module_structure: Mounting and module structure
	 * options: 0=glass/cell/polymer sheet - open rack,1=glass/cell/glass - open rack,2=polymer/thin film/steel - open rack,3=Insulated back, building-integrated PV,4=close roof mount,5=user-defined
	 * constraints: INTEGER,MIN=0,MAX=5
	 * required if: module_model=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_module_structure_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set spe_rad0: Irradiance level 0 [W/m2]
	 * options: None
	 * constraints: None
	 * required if: module_model=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_rad0_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set spe_rad1: Irradiance level 1 [W/m2]
	 * options: None
	 * constraints: None
	 * required if: module_model=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_rad1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set spe_rad2: Irradiance level 2 [W/m2]
	 * options: None
	 * constraints: None
	 * required if: module_model=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_rad2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set spe_rad3: Irradiance level 3 [W/m2]
	 * options: None
	 * constraints: None
	 * required if: module_model=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_rad3_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set spe_rad4: Irradiance level 4 [W/m2]
	 * options: None
	 * constraints: None
	 * required if: module_model=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_rad4_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set spe_reference: Reference irradiance level
	 * options: None
	 * constraints: INTEGER,MIN=0,MAX=4
	 * required if: module_model=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_reference_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set spe_temp_coeff: Temperature coefficient [%/C]
	 * options: None
	 * constraints: None
	 * required if: module_model=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_temp_coeff_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set spe_vmp: Nominal max power voltage [V]
	 * options: None
	 * constraints: POSITIVE
	 * required if: module_model=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_vmp_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set spe_voc: Nominal open circuit voltage [V]
	 * options: None
	 * constraints: POSITIVE
	 * required if: module_model=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_voc_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// CECPerformanceModelWithModuleDatabase parameters
	//

	/**
	 * Set cec_a_ref: Nonideality factor a
	 * options: None
	 * constraints: None
	 * required if: module_model=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_a_ref_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cec_adjust: Temperature coefficient adjustment [%]
	 * options: None
	 * constraints: None
	 * required if: module_model=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_adjust_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cec_alpha_sc: Short circuit current temperature coefficient [A/C]
	 * options: None
	 * constraints: None
	 * required if: module_model=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_alpha_sc_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cec_area: Module area [m2]
	 * options: None
	 * constraints: None
	 * required if: module_model=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_area_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cec_array_cols: Columns of modules in array
	 * options: None
	 * constraints: None
	 * required if: module_model=1&cec_temp_corr_mode=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_array_cols_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cec_array_rows: Rows of modules in array
	 * options: None
	 * constraints: None
	 * required if: module_model=1&cec_temp_corr_mode=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_array_rows_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cec_backside_temp: Module backside temperature [C]
	 * options: None
	 * constraints: POSITIVE
	 * required if: module_model=1&cec_temp_corr_mode=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_backside_temp_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cec_beta_oc: Open circuit voltage temperature coefficient [V/C]
	 * options: None
	 * constraints: None
	 * required if: module_model=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_beta_oc_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cec_bifacial_ground_clearance_height: Module ground clearance height [m]
	 * options: None
	 * constraints: None
	 * required if: module_model=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_bifacial_ground_clearance_height_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cec_bifacial_transmission_factor: Bifacial transmission factor [0-1]
	 * options: None
	 * constraints: None
	 * required if: module_model=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_bifacial_transmission_factor_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cec_bifaciality: Bifaciality factor [%]
	 * options: None
	 * constraints: None
	 * required if: module_model=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_bifaciality_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cec_gamma_r: Maximum power point temperature coefficient [%/C]
	 * options: None
	 * constraints: None
	 * required if: module_model=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_gamma_r_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cec_gap_spacing: Gap spacing [m]
	 * options: None
	 * constraints: None
	 * required if: module_model=1&cec_temp_corr_mode=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_gap_spacing_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cec_heat_transfer: Heat transfer dimensions
	 * options: 0=module,1=array
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: module_model=1&cec_temp_corr_mode=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_heat_transfer_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cec_height: Array mounting height
	 * options: 0=one story,1=two story
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: module_model=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_height_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cec_i_l_ref: Light current [A]
	 * options: None
	 * constraints: None
	 * required if: module_model=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_i_l_ref_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cec_i_mp_ref: Maximum power point current [A]
	 * options: None
	 * constraints: None
	 * required if: module_model=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_i_mp_ref_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cec_i_o_ref: Saturation current [A]
	 * options: None
	 * constraints: None
	 * required if: module_model=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_i_o_ref_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cec_i_sc_ref: Short circuit current [A]
	 * options: None
	 * constraints: None
	 * required if: module_model=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_i_sc_ref_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cec_is_bifacial: Modules are bifacial [0/1]
	 * options: None
	 * constraints: None
	 * required if: module_model=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_is_bifacial_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cec_module_length: Module height [m]
	 * options: None
	 * constraints: None
	 * required if: module_model=1&cec_temp_corr_mode=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_module_length_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cec_module_width: Module width [m]
	 * options: None
	 * constraints: None
	 * required if: module_model=1&cec_temp_corr_mode=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_module_width_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cec_mounting_config: Mounting configuration
	 * options: 0=rack,1=flush,2=integrated,3=gap
	 * constraints: INTEGER,MIN=0,MAX=3
	 * required if: module_model=1&cec_temp_corr_mode=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_mounting_config_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cec_mounting_orientation: Mounting structure orientation
	 * options: 0=do not impede flow,1=vertical supports,2=horizontal supports
	 * constraints: INTEGER,MIN=0,MAX=2
	 * required if: module_model=1&cec_temp_corr_mode=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_mounting_orientation_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cec_n_s: Number of cells in series
	 * options: None
	 * constraints: POSITIVE
	 * required if: module_model=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_n_s_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cec_r_s: Series resistance [ohm]
	 * options: None
	 * constraints: None
	 * required if: module_model=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_r_s_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cec_r_sh_ref: Shunt resistance [ohm]
	 * options: None
	 * constraints: None
	 * required if: module_model=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_r_sh_ref_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cec_standoff: Standoff mode
	 * options: 0=bipv,1=>3.5in,2=2.5-3.5in,3=1.5-2.5in,4=0.5-1.5in,5=<0.5in,6=ground/rack
	 * constraints: INTEGER,MIN=0,MAX=6
	 * required if: module_model=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_standoff_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cec_t_noct: Nominal operating cell temperature [C]
	 * options: None
	 * constraints: None
	 * required if: module_model=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_t_noct_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cec_temp_corr_mode: Cell temperature model selection
	 * options: 0=noct,1=mc
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: module_model=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_temp_corr_mode_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cec_transient_thermal_model_unit_mass: Module unit mass [kg/m^2]
	 * options: None
	 * constraints: POSITIVE
	 * required if: module_model=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_transient_thermal_model_unit_mass_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cec_v_mp_ref: Maximum power point voltage [V]
	 * options: None
	 * constraints: None
	 * required if: module_model=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_v_mp_ref_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cec_v_oc_ref: Open circuit voltage [V]
	 * options: None
	 * constraints: None
	 * required if: module_model=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_v_oc_ref_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// CECPerformanceModelWithUserEnteredSpecifications parameters
	//

	/**
	 * Set 6par_aisc: Short circuit current temperature coefficient [A/C]
	 * options: None
	 * constraints: None
	 * required if: module_model=2
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_aisc_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set 6par_area: Module area [m2]
	 * options: None
	 * constraints: None
	 * required if: module_model=2
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_area_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set 6par_bifacial_ground_clearance_height: Module ground clearance height [m]
	 * options: None
	 * constraints: None
	 * required if: module_model=2
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_bifacial_ground_clearance_height_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set 6par_bifacial_transmission_factor: Bifacial transmission factor [0-1]
	 * options: None
	 * constraints: None
	 * required if: module_model=2
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_bifacial_transmission_factor_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set 6par_bifaciality: Bifaciality factor [%]
	 * options: None
	 * constraints: None
	 * required if: module_model=2
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_bifaciality_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set 6par_bvoc: Open circuit voltage temperature coefficient [V/C]
	 * options: None
	 * constraints: None
	 * required if: module_model=2
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_bvoc_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set 6par_celltech: Solar cell technology type
	 * options: monoSi=0,multiSi=1,CdTe=2,CIS=3,CIGS=4,Amorphous=5
	 * constraints: INTEGER,MIN=0,MAX=5
	 * required if: module_model=2
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_celltech_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set 6par_gpmp: Maximum power point temperature coefficient [%/C]
	 * options: None
	 * constraints: None
	 * required if: module_model=2
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_gpmp_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set 6par_imp: Imp [A]
	 * options: None
	 * constraints: None
	 * required if: module_model=2
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_imp_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set 6par_is_bifacial: Modules are bifacial [0/1]
	 * options: None
	 * constraints: None
	 * required if: module_model=2
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_is_bifacial_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set 6par_isc: Isc [A]
	 * options: None
	 * constraints: None
	 * required if: module_model=2
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_isc_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set 6par_mounting: Array mounting height
	 * options: 0=one story,1=two story
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: module_model=2
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_mounting_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set 6par_nser: Nseries
	 * options: None
	 * constraints: INTEGER,POSITIVE
	 * required if: module_model=2
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_nser_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set 6par_standoff: Standoff mode
	 * options: 0=bipv,1=>3.5in,2=2.5-3.5in,3=1.5-2.5in,4=0.5-1.5in,5=<0.5in,6=ground/rack
	 * constraints: INTEGER,MIN=0,MAX=6
	 * required if: module_model=2
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_standoff_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set 6par_tnoct: Nominal operating cell temperature [C]
	 * options: None
	 * constraints: None
	 * required if: module_model=2
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_tnoct_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set 6par_transient_thermal_model_unit_mass: Module unit mass [kg/m^2]
	 * options: None
	 * constraints: None
	 * required if: module_model=2
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_transient_thermal_model_unit_mass_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set 6par_vmp: Maximum power point voltage [V]
	 * options: None
	 * constraints: None
	 * required if: module_model=2
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_vmp_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set 6par_voc: Voc [V]
	 * options: None
	 * constraints: None
	 * required if: module_model=2
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_voc_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// SandiaPVArrayPerformanceModelWithModuleDatabase parameters
	//

	/**
	 * Set snl_a: Temperature coefficient a
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_a_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set snl_a0: Air mass polynomial coeff 0
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_a0_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set snl_a1: Air mass polynomial coeff 1
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_a1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set snl_a2: Air mass polynomial coeff 2
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_a2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set snl_a3: Air mass polynomial coeff 3
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_a3_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set snl_a4: Air mass polynomial coeff 4
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_a4_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set snl_aimp: Max power point current temperature coefficient
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_aimp_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set snl_aisc: Short circuit current temperature coefficient
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_aisc_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set snl_area: Module area
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_area_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set snl_b: Temperature coefficient b
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_b_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set snl_b0: Incidence angle modifier polynomial coeff 0
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_b0_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set snl_b1: Incidence angle modifier polynomial coeff 1
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_b1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set snl_b2: Incidence angle modifier polynomial coeff 2
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_b2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set snl_b3: Incidence angle modifier polynomial coeff 3
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_b3_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set snl_b4: Incidence angle modifier polynomial coeff 4
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_b4_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set snl_b5: Incidence angle modifier polynomial coeff 5
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_b5_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set snl_bvmpo: Max power point voltage temperature coefficient
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_bvmpo_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set snl_bvoco: Open circuit voltage temperature coefficient
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_bvoco_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set snl_c0: C0
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_c0_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set snl_c1: C1
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_c1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set snl_c2: C2
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_c2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set snl_c3: C3
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_c3_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set snl_c4: C4
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_c4_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set snl_c5: C5
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_c5_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set snl_c6: C6
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_c6_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set snl_c7: C7
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_c7_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set snl_dtc: Temperature coefficient dT
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_dtc_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set snl_fd: Diffuse fraction
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_fd_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set snl_impo: Max power point current
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_impo_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set snl_isco: Short circuit current
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_isco_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set snl_ixo: Ix midpoint current
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_ixo_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set snl_ixxo: Ixx midpoint current
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_ixxo_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set snl_mbvmp: Irradiance dependence of Vmp temperature coefficient
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_mbvmp_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set snl_mbvoc: Irradiance dependence of Voc temperature coefficient
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_mbvoc_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set snl_module_structure: Module and mounting structure configuration
	 * options: 0=Use Database Values,1=glass/cell/polymer sheet-open rack,2=glass/cell/glass-open rack,3=polymer/thin film/steel-open rack,4=Insulated back BIPV,5=close roof mount,6=user-defined
	 * constraints: INTEGER,MIN=0,MAX=6
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_module_structure_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set snl_n: Diode factor
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_n_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set snl_ref_a: User-specified a
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_ref_a_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set snl_ref_b: User-specified b
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_ref_b_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set snl_ref_dT: User-specified dT
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_ref_dT_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set snl_series_cells: Number of cells in series
	 * options: None
	 * constraints: INTEGER
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_series_cells_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set snl_transient_thermal_model_unit_mass: Module unit mass [kg/m^2]
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_transient_thermal_model_unit_mass_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set snl_vmpo: Max power point voltage
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_vmpo_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set snl_voco: Open circuit voltage
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_voco_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// IEC61853SingleDiodeModel parameters
	//

	/**
	 * Set sd11par_AMa0: Air mass modifier coeff 0
	 * options: None
	 * constraints: None
	 * required if: module_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_AMa0_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set sd11par_AMa1: Air mass modifier coeff 1
	 * options: None
	 * constraints: None
	 * required if: module_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_AMa1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set sd11par_AMa2: Air mass modifier coeff 2
	 * options: None
	 * constraints: None
	 * required if: module_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_AMa2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set sd11par_AMa3: Air mass modifier coeff 3
	 * options: None
	 * constraints: None
	 * required if: module_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_AMa3_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set sd11par_AMa4: Air mass modifier coeff 4
	 * options: None
	 * constraints: None
	 * required if: module_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_AMa4_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set sd11par_Egref: Bandgap voltage [eV]
	 * options: None
	 * constraints: None
	 * required if: module_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_Egref_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set sd11par_Il: Light current [A]
	 * options: None
	 * constraints: None
	 * required if: module_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_Il_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set sd11par_Imp0: Imp (STC) [A]
	 * options: None
	 * constraints: None
	 * required if: module_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_Imp0_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set sd11par_Io: Saturation current [A]
	 * options: None
	 * constraints: None
	 * required if: module_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_Io_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set sd11par_Isc0: Isc (STC) [A]
	 * options: None
	 * constraints: None
	 * required if: module_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_Isc0_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set sd11par_Vmp0: Vmp (STC) [V]
	 * options: None
	 * constraints: None
	 * required if: module_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_Vmp0_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set sd11par_Voc0: Voc (STC) [V]
	 * options: None
	 * constraints: None
	 * required if: module_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_Voc0_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set sd11par_alphaIsc: Short curcuit current temperature coefficient [A/C]
	 * options: None
	 * constraints: None
	 * required if: module_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_alphaIsc_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set sd11par_area: Module area [m2]
	 * options: None
	 * constraints: None
	 * required if: module_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_area_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set sd11par_c1: Rsh fit parameter 1
	 * options: None
	 * constraints: None
	 * required if: module_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_c1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set sd11par_c2: Rsh fit parameter 2
	 * options: None
	 * constraints: None
	 * required if: module_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_c2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set sd11par_c3: Rsh fit parameter 3
	 * options: None
	 * constraints: None
	 * required if: module_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_c3_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set sd11par_d1: Rs fit parameter 1
	 * options: None
	 * constraints: None
	 * required if: module_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_d1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set sd11par_d2: Rs fit parameter 2
	 * options: None
	 * constraints: None
	 * required if: module_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_d2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set sd11par_d3: Rs fit parameter 3
	 * options: None
	 * constraints: None
	 * required if: module_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_d3_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set sd11par_glass: Module cover glass type
	 * options: 0=normal,1=AR glass
	 * constraints: None
	 * required if: module_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_glass_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set sd11par_mounting: Array mounting height
	 * options: 0=one story,1=two story
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: module_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_mounting_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set sd11par_n: Diode nonideality factor
	 * options: None
	 * constraints: None
	 * required if: module_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_n_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set sd11par_nser: Nseries
	 * options: None
	 * constraints: INTEGER,POSITIVE
	 * required if: module_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_nser_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set sd11par_standoff: Standoff mode
	 * options: 0=bipv,1=>3.5in,2=2.5-3.5in,3=1.5-2.5in,4=0.5-1.5in,5=<0.5in,6=ground/rack
	 * constraints: INTEGER,MIN=0,MAX=6
	 * required if: module_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_standoff_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set sd11par_tnoct: Nominal operating cell temperature [C]
	 * options: None
	 * constraints: None
	 * required if: module_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_tnoct_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// MermoudLejeuneSingleDiodeModel parameters
	//

	/**
	 * Set mlm_AM_c_lp0: Coefficient 0 for Lee/Panchula Air Mass Modifier [-]
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_c_lp0_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mlm_AM_c_lp1: Coefficient 1 for Lee/Panchula Air Mass Modifier [-]
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_c_lp1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mlm_AM_c_lp2: Coefficient 2 for Lee/Panchula Air Mass Modifier [-]
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_c_lp2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mlm_AM_c_lp3: Coefficient 3 for Lee/Panchula Air Mass Modifier [-]
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_c_lp3_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mlm_AM_c_lp4: Coefficient 4 for Lee/Panchula Air Mass Modifier [-]
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_c_lp4_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mlm_AM_c_lp5: Coefficient 5 for Lee/Panchula Air Mass Modifier [-]
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_c_lp5_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mlm_AM_c_sa0: Coefficient 0 for Sandia Air Mass Modifier [-]
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_c_sa0_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mlm_AM_c_sa1: Coefficient 1 for Sandia Air Mass Modifier [-]
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_c_sa1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mlm_AM_c_sa2: Coefficient 2 for Sandia Air Mass Modifier [-]
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_c_sa2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mlm_AM_c_sa3: Coefficient 3 for Sandia Air Mass Modifier [-]
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_c_sa3_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mlm_AM_c_sa4: Coefficient 4 for Sandia Air Mass Modifier [-]
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_c_sa4_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mlm_AM_mode: Air-mass modifier mode [-]
	 * options: 1: Do not consider AM effects, 2: Use Sandia polynomial [corr=f(AM)], 3: Use standard coefficients from DeSoto model [corr=f(AM)], 4: Use First Solar polynomial [corr=f(AM, p_wat)]
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_mode_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mlm_D2MuTau: Coefficient for recombination losses [V]
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_D2MuTau_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mlm_E_g: Reference bandgap energy [eV]
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_E_g_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mlm_IAM_c_as: ASHRAE incidence modifier coefficient b_0 [-]
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_IAM_c_as_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mlm_IAM_c_cs_iamValue: Spline IAM - IAM values [-]
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_IAM_c_cs_iamValue_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set mlm_IAM_c_cs_incAngle: Spline IAM - Incidence angles [deg]
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_IAM_c_cs_incAngle_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set mlm_IAM_c_sa0: Sandia IAM coefficient 0 [-]
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_IAM_c_sa0_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mlm_IAM_c_sa1: Sandia IAM coefficient 1 [-]
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_IAM_c_sa1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mlm_IAM_c_sa2: Sandia IAM coefficient 2 [-]
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_IAM_c_sa2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mlm_IAM_c_sa3: Sandia IAM coefficient 3 [-]
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_IAM_c_sa3_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mlm_IAM_c_sa4: Sandia IAM coefficient 4 [-]
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_IAM_c_sa4_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mlm_IAM_c_sa5: Sandia IAM coefficient 5 [-]
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_IAM_c_sa5_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mlm_IAM_mode: Incidence Angle Modifier mode [-]
	 * options: 1: Use ASHRAE formula, 2: Use Sandia polynomial, 3: Use cubic spline with user-supplied data
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_IAM_mode_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mlm_I_mp_ref: I_mp at STC [A]
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_I_mp_ref_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mlm_I_sc_ref: I_sc at STC [A]
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_I_sc_ref_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mlm_Length: Module length (long side) [m]
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_Length_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mlm_N_diodes: Number of diodes [-]
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_N_diodes_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mlm_N_parallel: Number of cells in parallel [-]
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_N_parallel_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mlm_N_series: Number of cells in series [-]
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_N_series_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mlm_R_s: Series resistance [V/A]
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_R_s_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mlm_R_sh0: Rsh,0 [V/A]
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_R_sh0_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mlm_R_shexp: Rsh exponential coefficient [-]
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_R_shexp_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mlm_R_shref: Reference shunt resistance [V/A]
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_R_shref_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mlm_S_ref: Reference irradiance (Typically 1000W/m²) [W/m²]
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_S_ref_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mlm_T_c_fa_U0: Extended Faiman model U_0 [W/m²K]
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_T_c_fa_U0_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mlm_T_c_fa_U1: Extended Faiman model U_1 [W/m³sK]
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_T_c_fa_U1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mlm_T_c_fa_alpha: Extended Faiman model absorptivity [-]
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_T_c_fa_alpha_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mlm_T_c_no_mounting: NOCT Array mounting height [-]
	 * options: 0=one story,1=two story
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_T_c_no_mounting_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mlm_T_c_no_standoff: NOCT standoff mode [-]
	 * options: 0=bipv,1=>3.5in,2=2.5-3.5in,3=1.5-2.5in,4=0.5-1.5in,5=<0.5in,6=ground/rack
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_T_c_no_standoff_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mlm_T_c_no_tnoct: NOCT cell temperature [°C]
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_T_c_no_tnoct_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mlm_T_mode: Cell temperature model mode [-]
	 * options: 1: NOCT
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_T_mode_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mlm_T_ref: Reference temperature (Typically 25°C) [°C]
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_T_ref_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mlm_V_mp_ref: V_mp at STC [V]
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_V_mp_ref_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mlm_V_oc_ref: V_oc at STC [V]
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_V_oc_ref_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mlm_Width: Module width (short side) [m]
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_Width_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mlm_alpha_isc: Temperature coefficient for I_sc [A/K]
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_alpha_isc_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mlm_beta_voc_spec: Temperature coefficient for V_oc [V/K]
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_beta_voc_spec_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mlm_groundRelfectionFraction: Ground reflection fraction [-]
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_groundRelfectionFraction_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mlm_mu_n: Temperature coefficient of gamma [1/K]
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_mu_n_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mlm_n_0: Gamma [-]
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_n_0_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// Inverter parameters
	//

	/**
	 * Set inv_cec_cg_eff_cec: Inverter Coefficient Generator CEC Efficiency [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Inverter_inv_cec_cg_eff_cec_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set inv_cec_cg_paco: Inverter Coefficient Generator Max AC Power [Wac]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Inverter_inv_cec_cg_paco_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set inv_ds_eff: Inverter Datasheet Efficiency [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Inverter_inv_ds_eff_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set inv_ds_paco: Inverter Datasheet Maximum AC Power [Wac]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Inverter_inv_ds_paco_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set inv_num_mppt: Number of MPPT inputs
	 * options: None
	 * constraints: INTEGER,MIN=0,MAX=4
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_Inverter_inv_num_mppt_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set inv_pd_eff: Inverter Partload Efficiency [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Inverter_inv_pd_eff_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set inv_pd_paco: Inverter Partload Maximum AC Power [Wac]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Inverter_inv_pd_paco_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set inv_snl_eff_cec: Inverter Sandia CEC Efficiency [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Inverter_inv_snl_eff_cec_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set inv_snl_paco: Inverter Sandia Maximum AC Power [Wac]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Inverter_inv_snl_paco_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set inverter_count: Number of inverters
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Inverter_inverter_count_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set inverter_model: Inverter model specifier
	 * options: 0=cec,1=datasheet,2=partload,3=coefficientgenerator,4=PVYield
	 * constraints: INTEGER,MIN=0,MAX=4
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvsamv1_Inverter_inverter_model_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mppt_hi_inverter: Maximum inverter MPPT voltage window [Vdc]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_Inverter_mppt_hi_inverter_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mppt_low_inverter: Minimum inverter MPPT voltage window [Vdc]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_Inverter_mppt_low_inverter_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// InverterCECDatabase parameters
	//

	/**
	 * Set inv_snl_c0: Curvature between AC power and DC power at ref [1/W]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterCECDatabase_inv_snl_c0_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set inv_snl_c1: Coefficient of Pdco variation with DC input voltage [1/V]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterCECDatabase_inv_snl_c1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set inv_snl_c2: Coefficient of Pso variation with DC input voltage [1/V]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterCECDatabase_inv_snl_c2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set inv_snl_c3: Coefficient of Co variation with DC input voltage [1/V]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterCECDatabase_inv_snl_c3_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set inv_snl_paco: AC maximum power rating [Wac]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterCECDatabase_inv_snl_paco_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set inv_snl_pdco: DC input power at which AC power rating is achieved [Wdc]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterCECDatabase_inv_snl_pdco_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set inv_snl_pnt: AC power consumed by inverter at night [Wac]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterCECDatabase_inv_snl_pnt_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set inv_snl_pso: DC power required to enable the inversion process [Wdc]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterCECDatabase_inv_snl_pso_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set inv_snl_vdcmax: Maximum DC input operating voltage [Vdc]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterCECDatabase_inv_snl_vdcmax_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set inv_snl_vdco: DC input voltage for the rated AC power rating [Vdc]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterCECDatabase_inv_snl_vdco_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set inv_tdc_cec_db: Temperature derate curves for CEC Database [(Vdc, C, %/C)]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterCECDatabase_inv_tdc_cec_db_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);


	//
	// InverterCECCoefficientGenerator parameters
	//

	/**
	 * Set inv_cec_cg_c0: Curvature between AC power and DC power at ref [1/W]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_c0_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set inv_cec_cg_c1: Coefficient of Pdco variation with DC input voltage [1/V]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_c1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set inv_cec_cg_c2: Coefficient of Pso variation with DC input voltage [1/V]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_c2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set inv_cec_cg_c3: Coefficient of Co variation with DC input voltage [1/V]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_c3_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set inv_cec_cg_paco: AC maximum power rating [Wac]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_paco_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set inv_cec_cg_pdco: DC input power at which AC power rating is achieved [Wdc]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_pdco_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set inv_cec_cg_pnt: AC power consumed by inverter at night [Wac]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_pnt_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set inv_cec_cg_psco: DC power required to enable the inversion process [Wdc]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_psco_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set inv_cec_cg_vdcmax: Maximum DC input operating voltage [Vdc]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_vdcmax_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set inv_cec_cg_vdco: DC input voltage for the rated AC power rating [Vdc]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_vdco_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set inv_tdc_cec_cg: Temperature derate curves for CEC Coef Gen [(Vdc, C, %/C)]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_tdc_cec_cg_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);


	//
	// InverterDatasheet parameters
	//

	/**
	 * Set inv_ds_eff: Weighted or Peak or Nominal Efficiency [Wdc]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterDatasheet_inv_ds_eff_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set inv_ds_paco: AC maximum power rating [Wac]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterDatasheet_inv_ds_paco_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set inv_ds_pnt: AC power consumed by inverter at night [Wac]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterDatasheet_inv_ds_pnt_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set inv_ds_pso: DC power required to enable the inversion process [Wdc]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterDatasheet_inv_ds_pso_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set inv_ds_vdcmax: Maximum DC input operating voltage [Vdc]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterDatasheet_inv_ds_vdcmax_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set inv_ds_vdco: DC input voltage for the rated AC power rating [Vdc]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterDatasheet_inv_ds_vdco_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set inv_tdc_ds: Temperature derate curves for Inv Datasheet [(Vdc, C, %/C)]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterDatasheet_inv_tdc_ds_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);


	//
	// InverterPartLoadCurve parameters
	//

	/**
	 * Set inv_pd_efficiency: Partload curve efficiency values [%]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=2
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterPartLoadCurve_inv_pd_efficiency_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set inv_pd_paco: AC maximum power rating [Wac]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=2
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterPartLoadCurve_inv_pd_paco_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set inv_pd_partload: Partload curve partload values [%]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=2
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterPartLoadCurve_inv_pd_partload_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set inv_pd_pdco: DC input power at which AC power rating is achieved [Wdc]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=2
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterPartLoadCurve_inv_pd_pdco_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set inv_pd_pnt: AC power consumed by inverter at night [Wac]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=2
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterPartLoadCurve_inv_pd_pnt_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set inv_pd_vdcmax: Maximum DC input operating voltage [Vdc]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=2
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterPartLoadCurve_inv_pd_vdcmax_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set inv_pd_vdco: DC input voltage for the rated AC power rating [Vdc]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=2
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterPartLoadCurve_inv_pd_vdco_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set inv_tdc_plc: Temperature derate curves for Part Load Curve [(Vdc, C, %/C)]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=2
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterPartLoadCurve_inv_tdc_plc_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);


	//
	// InverterMermoudLejeuneModel parameters
	//

	/**
	 * Set ond_Aux_Loss:  [W]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_Aux_Loss_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ond_CompPMax:  [-]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_CompPMax_sset(SAM_table ptr, const char* str, SAM_error *err);

	/**
	 * Set ond_CompVMax:  [-]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_CompVMax_sset(SAM_table ptr, const char* str, SAM_error *err);

	/**
	 * Set ond_IMaxAC:  [A]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_IMaxAC_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ond_IMaxDC:  [A]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_IMaxDC_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ond_INomAC:  [A]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_INomAC_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ond_INomDC:  [A]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_INomDC_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ond_ModeAffEnum:  [-]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_ModeAffEnum_sset(SAM_table ptr, const char* str, SAM_error *err);

	/**
	 * Set ond_ModeOper:  [-]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_ModeOper_sset(SAM_table ptr, const char* str, SAM_error *err);

	/**
	 * Set ond_NbInputs:  [-]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_NbInputs_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ond_NbMPPT:  [-]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_NbMPPT_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ond_Night_Loss:  [W]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_Night_Loss_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ond_PLim1:  [W]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_PLim1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ond_PLimAbs:  [W]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_PLimAbs_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ond_PMaxDC:  [W]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_PMaxDC_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ond_PMaxOUT:  [W]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_PMaxOUT_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ond_PNomConv:  [W]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_PNomConv_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ond_PNomDC:  [W]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_PNomDC_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ond_PSeuil:  [W]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_PSeuil_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ond_TPLim1:  [°C]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_TPLim1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ond_TPLimAbs:  [°C]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_TPLimAbs_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ond_TPMax:  [°C]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_TPMax_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ond_TPNom:  [°C]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_TPNom_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ond_VAbsMax:  [V]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_VAbsMax_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ond_VMPPMax:  [V]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_VMPPMax_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ond_VMppMin:  [V]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_VMppMin_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ond_VNomEff:  [V]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_VNomEff_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set ond_VOutConv:  [W]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_VOutConv_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ond_doAllowOverpower:  [-]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_doAllowOverpower_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ond_doUseTemperatureLimit:  [-]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_doUseTemperatureLimit_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ond_effCurve_Pac:  [W]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_effCurve_Pac_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ond_effCurve_Pdc:  [W]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_effCurve_Pdc_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ond_effCurve_elements:  [-]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_effCurve_elements_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ond_effCurve_eta:  [-]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_effCurve_eta_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ond_lossRAc:  [A]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_lossRAc_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ond_lossRDc:  [V/A]
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_lossRDc_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// BatterySystem parameters
	//

	/**
	 * Set batt_ac_dc_efficiency: Inverter AC to battery DC efficiency
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatterySystem_batt_ac_dc_efficiency_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_ac_or_dc: Battery interconnection (AC or DC)
	 * options: 0=DC_Connected,1=AC_Connected
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatterySystem_batt_ac_or_dc_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_computed_bank_capacity: Battery computed bank capacity [kWh]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatterySystem_batt_computed_bank_capacity_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_computed_series: Battery number of cells in series
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatterySystem_batt_computed_series_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_computed_strings: Battery number of strings of cells
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatterySystem_batt_computed_strings_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_current_charge_max: Battery maximum charge current [A]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatterySystem_batt_current_charge_max_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_current_choice: Limit cells by current or power
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatterySystem_batt_current_choice_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_current_discharge_max: Battery maximum discharge current [A]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatterySystem_batt_current_discharge_max_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_dc_ac_efficiency: Battery DC to AC efficiency
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatterySystem_batt_dc_ac_efficiency_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_dc_dc_efficiency: System DC to battery DC efficiency
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatterySystem_batt_dc_dc_efficiency_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_inverter_efficiency_cutoff: Inverter efficiency at which to cut battery charge or discharge off [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatterySystem_batt_inverter_efficiency_cutoff_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_loss_choice: Loss power input option [0/1]
	 * options: 0=Monthly,1=TimeSeries
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatterySystem_batt_loss_choice_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_losses: Battery system losses at each timestep (kW DC for DC connected, AC for AC connected) [kW]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatterySystem_batt_losses_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set batt_losses_charging: Battery system losses when charging (kW DC for DC connected, AC for AC connected) [kW]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatterySystem_batt_losses_charging_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set batt_losses_discharging: Battery system losses when discharging (kW DC for DC connected, AC for AC connected) [kW]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatterySystem_batt_losses_discharging_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set batt_losses_idle: Battery system losses when idle (kW DC for DC connected, AC for AC connected) [kW]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatterySystem_batt_losses_idle_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set batt_mass: Battery mass [kg]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatterySystem_batt_mass_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_meter_position: Position of battery relative to electric meter
	 * options: 0=BehindTheMeter,1=FrontOfMeter
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatterySystem_batt_meter_position_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_power_charge_max_kwac: Battery maximum charge power (AC) [kWac]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatterySystem_batt_power_charge_max_kwac_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_power_charge_max_kwdc: Battery maximum charge power (DC) [kWdc]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatterySystem_batt_power_charge_max_kwdc_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_power_discharge_max_kwac: Battery maximum discharge power (AC) [kWac]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatterySystem_batt_power_discharge_max_kwac_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_power_discharge_max_kwdc: Battery maximum discharge power (DC) [kWdc]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatterySystem_batt_power_discharge_max_kwdc_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_replacement_capacity: Capacity degradation at which to replace battery [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatterySystem_batt_replacement_capacity_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_replacement_option: Enable battery replacement? [0=none,1=capacity based,2=user schedule]
	 * options: None
	 * constraints: INTEGER,MIN=0,MAX=2
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatterySystem_batt_replacement_option_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_replacement_schedule_percent: Percentage of battery capacity to replace in each year [%]
	 * options: length <= analysis_period
	 * constraints: None
	 * required if: batt_replacement_option=2
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatterySystem_batt_replacement_schedule_percent_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set batt_surface_area: Battery surface area [m^2]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatterySystem_batt_surface_area_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set en_batt: Enable battery storage model [0/1]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatterySystem_en_batt_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set om_replacement_cost1: Cost to replace battery per kWh [$/kWh]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatterySystem_om_replacement_cost1_aset(SAM_table ptr, double* arr, int length, SAM_error *err);


	//
	// Load parameters
	//

	/**
	 * Set crit_load: Critical Electricity load (year 1) [kW]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Load_crit_load_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set load: Electricity load (year 1) [kW]
	 * options: None
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Pvsamv1_Load_load_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set load_escalation: Annual load escalation [%/year]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_Load_load_escalation_aset(SAM_table ptr, double* arr, int length, SAM_error *err);


	//
	// BatteryCell parameters
	//

	/**
	 * Set LeadAcid_q10_computed: Capacity at 10-hour discharge rate [Ah]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatteryCell_LeadAcid_q10_computed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set LeadAcid_q20_computed: Capacity at 20-hour discharge rate [Ah]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatteryCell_LeadAcid_q20_computed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set LeadAcid_qn_computed: Capacity at discharge rate for n-hour rate [Ah]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatteryCell_LeadAcid_qn_computed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set LeadAcid_tn: Time to discharge [h]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatteryCell_LeadAcid_tn_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_C_rate: Rate at which voltage vs. capacity curve input
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatteryCell_batt_C_rate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_Cp: Battery specific heat capacity [J/KgK]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatteryCell_batt_Cp_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_Qexp: Cell capacity at end of exponential zone [Ah]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatteryCell_batt_Qexp_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_Qfull: Fully charged cell capacity [Ah]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatteryCell_batt_Qfull_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_Qfull_flow: Fully charged flow battery capacity [Ah]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatteryCell_batt_Qfull_flow_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_Qnom: Cell capacity at end of nominal zone [Ah]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatteryCell_batt_Qnom_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_Vcut: Cutoff voltage for battery rated capacity [V]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatteryCell_batt_Vcut_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_Vexp: Cell voltage at end of exponential zone [V]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatteryCell_batt_Vexp_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_Vfull: Fully charged cell voltage [V]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatteryCell_batt_Vfull_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_Vnom: Cell voltage at end of nominal zone [V]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatteryCell_batt_Vnom_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_Vnom_default: Default nominal cell voltage [V]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatteryCell_batt_Vnom_default_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_calendar_a: Calendar life model coefficient [1/sqrt(day)]
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_life_model=0&batt_calendar_choice=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatteryCell_batt_calendar_a_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_calendar_b: Calendar life model coefficient [K]
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_life_model=0&batt_calendar_choice=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatteryCell_batt_calendar_b_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_calendar_c: Calendar life model coefficient [K]
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_life_model=0&batt_calendar_choice=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatteryCell_batt_calendar_c_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_calendar_choice: Calendar life degradation input option [0/1/2]
	 * options: 0=NoCalendarDegradation,1=LithiomIonModel,2=InputLossTable
	 * constraints: None
	 * required if: en_batt=1&batt_life_model=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatteryCell_batt_calendar_choice_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_calendar_lifetime_matrix: Days vs capacity
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_life_model=0&batt_calendar_choice=2
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatteryCell_batt_calendar_lifetime_matrix_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set batt_calendar_q0: Calendar life model initial capacity cofficient
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_life_model=0&batt_calendar_choice=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatteryCell_batt_calendar_q0_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_chem: Battery chemistry
	 * options: 0=LeadAcid,1=LiIon
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatteryCell_batt_chem_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_h_to_ambient: Heat transfer between battery and environment [W/m2K]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatteryCell_batt_h_to_ambient_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_initial_SOC: Initial state-of-charge [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatteryCell_batt_initial_SOC_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_life_model: Battery life model specifier [0/1]
	 * options: 0=calendar/cycle,1=NMC
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatteryCell_batt_life_model_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_lifetime_matrix: Cycles vs capacity at different depths-of-discharge
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_life_model=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatteryCell_batt_lifetime_matrix_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set batt_maximum_SOC: Maximum allowed state-of-charge [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatteryCell_batt_maximum_SOC_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_minimum_SOC: Minimum allowed state-of-charge [%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatteryCell_batt_minimum_SOC_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_minimum_modetime: Minimum time at charge state [min]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatteryCell_batt_minimum_modetime_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_resistance: Internal resistance [Ohm]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatteryCell_batt_resistance_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_room_temperature_celsius: Temperature of storage room [C]
	 * options: length=1 for fixed, # of weatherfile records otherwise
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatteryCell_batt_room_temperature_celsius_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set batt_voltage_choice: Battery voltage input option [0/1]
	 * options: 0=UseVoltageModel,1=InputVoltageTable
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatteryCell_batt_voltage_choice_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_voltage_matrix: Battery voltage vs. depth-of-discharge
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatteryCell_batt_voltage_matrix_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set cap_vs_temp: Effective capacity as function of temperature [C,%]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatteryCell_cap_vs_temp_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);


	//
	// BatteryDispatch parameters
	//

	/**
	 * Set batt_auto_gridcharge_max_daily: Allowed grid charging percent per day for automated dispatch [kW]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatteryDispatch_batt_auto_gridcharge_max_daily_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_custom_dispatch: Custom battery power for every time step [kW]
	 * options: kWAC if AC-connected, else kWDC
	 * constraints: None
	 * required if: en_batt=1&batt_dispatch_choice=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatteryDispatch_batt_custom_dispatch_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set batt_cycle_cost: Input battery cycle degradaton penalty per year [$/cycle-kWh]
	 * options: length 1 or analysis_period, length 1 will be extended using inflation
	 * constraints: None
	 * required if: batt_cycle_cost_choice=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatteryDispatch_batt_cycle_cost_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set batt_cycle_cost_choice: Use SAM cost model for degradaton penalty or input custom via batt_cycle_cost [0/1]
	 * options: 0=UseCostModel,1=InputCost
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatteryDispatch_batt_cycle_cost_choice_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_dispatch_auto_can_charge: System charging allowed for automated dispatch? [kW]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatteryDispatch_batt_dispatch_auto_can_charge_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_dispatch_auto_can_clipcharge: Battery can charge from clipped power for automated dispatch? [kW]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatteryDispatch_batt_dispatch_auto_can_clipcharge_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_dispatch_auto_can_fuelcellcharge: Charging from fuel cell allowed for automated dispatch? [kW]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatteryDispatch_batt_dispatch_auto_can_fuelcellcharge_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_dispatch_auto_can_gridcharge: Grid charging allowed for automated dispatch? [kW]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatteryDispatch_batt_dispatch_auto_can_gridcharge_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_dispatch_choice: Battery dispatch algorithm [0/1/2/3/4/5]
	 * options: If behind the meter: 0=PeakShavingLookAhead,1=PeakShavingLookBehind,2=InputGridTarget,3=InputBatteryPower,4=ManualDispatch,5=PriceSignalForecast if front of meter: 0=AutomatedLookAhead,1=AutomatedLookBehind,2=AutomatedInputForecast,3=InputBatteryPower,4=ManualDispatch
	 * constraints: None
	 * required if: en_batt=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatteryDispatch_batt_dispatch_choice_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_dispatch_update_frequency_hours: Frequency to update the look-ahead dispatch [hours]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatteryDispatch_batt_dispatch_update_frequency_hours_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_look_ahead_hours: Hours to look ahead in automated dispatch [hours]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatteryDispatch_batt_look_ahead_hours_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_pv_ac_forecast: PV ac power forecast [kW]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatteryDispatch_batt_pv_ac_forecast_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set batt_pv_clipping_forecast: PV clipping forecast [kW]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatteryDispatch_batt_pv_clipping_forecast_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set batt_target_choice: Target power input option [0/1]
	 * options: 0=InputMonthlyTarget,1=InputFullTimeSeries
	 * constraints: None
	 * required if: en_batt=1&batt_meter_position=0&batt_dispatch_choice=2
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatteryDispatch_batt_target_choice_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set batt_target_power: Grid target power for every time step [kW]
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_meter_position=0&batt_dispatch_choice=2
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatteryDispatch_batt_target_power_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set batt_target_power_monthly: Grid target power on monthly basis [kW]
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_meter_position=0&batt_dispatch_choice=2
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatteryDispatch_batt_target_power_monthly_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set dispatch_manual_charge: Periods 1-6 charging from system allowed?
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_dispatch_choice=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatteryDispatch_dispatch_manual_charge_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set dispatch_manual_discharge: Periods 1-6 discharging allowed?
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_dispatch_choice=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatteryDispatch_dispatch_manual_discharge_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set dispatch_manual_fuelcellcharge: Periods 1-6 charging from fuel cell allowed?
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatteryDispatch_dispatch_manual_fuelcellcharge_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set dispatch_manual_gridcharge: Periods 1-6 grid charging allowed?
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_dispatch_choice=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatteryDispatch_dispatch_manual_gridcharge_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set dispatch_manual_percent_discharge: Periods 1-6 discharge percent [%]
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_dispatch_choice=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatteryDispatch_dispatch_manual_percent_discharge_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set dispatch_manual_percent_gridcharge: Periods 1-6 gridcharge percent [%]
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_dispatch_choice=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatteryDispatch_dispatch_manual_percent_gridcharge_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set dispatch_manual_sched: Battery dispatch schedule for weekday
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_dispatch_choice=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatteryDispatch_dispatch_manual_sched_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set dispatch_manual_sched_weekend: Battery dispatch schedule for weekend
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_dispatch_choice=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_BatteryDispatch_dispatch_manual_sched_weekend_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);


	//
	// FuelCell parameters
	//

	/**
	 * Set fuelcell_power: Electricity from fuel cell [kW]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_FuelCell_fuelcell_power_aset(SAM_table ptr, double* arr, int length, SAM_error *err);


	//
	// PriceSignal parameters
	//

	/**
	 * Set dispatch_factors_ts: Dispatch payment factor time step
	 * options: None
	 * constraints: None
	 * required if: forecast_price_signal_model=0&en_batt=1&batt_meter_position=1&ppa_multiplier_model=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_PriceSignal_dispatch_factors_ts_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set dispatch_sched_weekday: Diurnal weekday TOD periods [1..9]
	 * options: 12 x 24 matrix
	 * constraints: None
	 * required if: en_batt=1&batt_meter_position=1&forecast_price_signal_model=0&ppa_multiplier_model=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_PriceSignal_dispatch_sched_weekday_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set dispatch_sched_weekend: Diurnal weekend TOD periods [1..9]
	 * options: 12 x 24 matrix
	 * constraints: None
	 * required if: en_batt=1&batt_meter_position=1&forecast_price_signal_model=0&ppa_multiplier_model=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_PriceSignal_dispatch_sched_weekend_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set dispatch_tod_factors: TOD factors for periods 1-9
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_meter_position=1&forecast_price_signal_model=0&ppa_multiplier_model=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_PriceSignal_dispatch_tod_factors_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set forecast_price_signal_model: Forecast price signal model selected [0/1]
	 * options: 0=PPA based,1=Merchant Plant
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_PriceSignal_forecast_price_signal_model_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mp_ancserv1_revenue: Ancillary services 1 revenue input [ [MW, $/MW]]
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_meter_position=1&forecast_price_signal_model=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_PriceSignal_mp_ancserv1_revenue_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set mp_ancserv2_revenue: Ancillary services 2 revenue input [ [MW, $/MW]]
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_meter_position=1&forecast_price_signal_model=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_PriceSignal_mp_ancserv2_revenue_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set mp_ancserv3_revenue: Ancillary services 3 revenue input [ [MW, $/MW]]
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_meter_position=1&forecast_price_signal_model=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_PriceSignal_mp_ancserv3_revenue_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set mp_ancserv4_revenue: Ancillary services 4 revenue input [ [MW, $/MW]]
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_meter_position=1&forecast_price_signal_model=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_PriceSignal_mp_ancserv4_revenue_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set mp_enable_ancserv1: Enable ancillary services 1 revenue [0/1]
	 * options: None
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: forecast_price_signal_model=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_PriceSignal_mp_enable_ancserv1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mp_enable_ancserv2: Enable ancillary services 2 revenue [0/1]
	 * options: None
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: forecast_price_signal_model=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_PriceSignal_mp_enable_ancserv2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mp_enable_ancserv3: Enable ancillary services 3 revenue [0/1]
	 * options: None
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: forecast_price_signal_model=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_PriceSignal_mp_enable_ancserv3_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mp_enable_ancserv4: Enable ancillary services 4 revenue [0/1]
	 * options: None
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: forecast_price_signal_model=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_PriceSignal_mp_enable_ancserv4_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mp_enable_energy_market_revenue: Enable energy market revenue [0/1]
	 * options: 0=false,1=true
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: en_batt=1&batt_meter_position=1&forecast_price_signal_model=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_PriceSignal_mp_enable_energy_market_revenue_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mp_energy_market_revenue: Energy market revenue input [ [MW, $/MW]]
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_meter_position=1&forecast_price_signal_model=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_PriceSignal_mp_energy_market_revenue_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ppa_multiplier_model: PPA multiplier model [0/1]
	 * options: 0=diurnal,1=timestep
	 * constraints: INTEGER,MIN=0
	 * required if: forecast_price_signal_model=0&en_batt=1&batt_meter_position=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_PriceSignal_ppa_multiplier_model_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ppa_price_input: PPA Price Input
	 * options: None
	 * constraints: None
	 * required if: forecast_price_signal_model=0&en_batt=1&batt_meter_position=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_PriceSignal_ppa_price_input_aset(SAM_table ptr, double* arr, int length, SAM_error *err);


	//
	// ElectricityRates parameters
	//

	/**
	 * Set rate_escalation: Annual electricity rate escalation [%/year]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_ElectricityRates_rate_escalation_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set ur_annual_min_charge: Annual minimum charge [$]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Pvsamv1_ElectricityRates_ur_annual_min_charge_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_dc_enable: Enable demand charge [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_ElectricityRates_ur_dc_enable_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_dc_flat_mat: Demand rates (flat) table
	 * options: None
	 * constraints: None
	 * required if: ur_dc_enable=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_ElectricityRates_ur_dc_flat_mat_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_dc_sched_weekday: Demand charge weekday schedule
	 * options: 12x24
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_ElectricityRates_ur_dc_sched_weekday_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_dc_sched_weekend: Demand charge weekend schedule
	 * options: 12x24
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_ElectricityRates_ur_dc_sched_weekend_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_dc_tou_mat: Demand rates (TOU) table
	 * options: None
	 * constraints: None
	 * required if: ur_dc_enable=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_ElectricityRates_ur_dc_tou_mat_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_ec_sched_weekday: Energy charge weekday schedule
	 * options: 12x24
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_ElectricityRates_ur_ec_sched_weekday_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_ec_sched_weekend: Energy charge weekend schedule
	 * options: 12x24
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_ElectricityRates_ur_ec_sched_weekend_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_ec_tou_mat: Energy rates table
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_ElectricityRates_ur_ec_tou_mat_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set ur_en_ts_buy_rate: Enable time step buy rates [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_ElectricityRates_ur_en_ts_buy_rate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_en_ts_sell_rate: Enable time step sell rates [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_ElectricityRates_ur_en_ts_sell_rate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_metering_option: Metering options [0=net energy metering,1=net energy metering with $ credits,2=net billing,3=net billing with carryover to next month,4=buy all - sell all]
	 * options: Net metering monthly excess
	 * constraints: INTEGER,MIN=0,MAX=4
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_ElectricityRates_ur_metering_option_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_monthly_fixed_charge: Monthly fixed charge [$]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Pvsamv1_ElectricityRates_ur_monthly_fixed_charge_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_monthly_min_charge: Monthly minimum charge [$]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Pvsamv1_ElectricityRates_ur_monthly_min_charge_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_nm_credit_month: Month of year end payout (true-up) [mn]
	 * options: None
	 * constraints: INTEGER,MIN=0,MAX=11
	 * required if: ?=11
	 */
	SAM_EXPORT void SAM_Pvsamv1_ElectricityRates_ur_nm_credit_month_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_nm_credit_rollover: Apply net metering true-up credits to future bills [0/1]
	 * options: None
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_ElectricityRates_ur_nm_credit_rollover_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_nm_yearend_sell_rate: Net metering true-up credit sell rate [$/kWh]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Pvsamv1_ElectricityRates_ur_nm_yearend_sell_rate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_sell_eq_buy: Set sell rate equal to buy rate [0/1]
	 * options: Optional override
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_ElectricityRates_ur_sell_eq_buy_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ur_ts_buy_rate: Time step buy rates [0/1]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_ElectricityRates_ur_ts_buy_rate_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set ur_ts_sell_rate: Time step sell rates [0/1]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_ElectricityRates_ur_ts_sell_rate_aset(SAM_table ptr, double* arr, int length, SAM_error *err);


	/**
	 * SolarResource Getters
	 */

	SAM_EXPORT double* SAM_Pvsamv1_SolarResource_albedo_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SolarResource_irrad_mode_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SolarResource_sky_model_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT SAM_table SAM_Pvsamv1_SolarResource_solar_resource_data_tget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT const char* SAM_Pvsamv1_SolarResource_solar_resource_file_sget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SolarResource_use_wf_albedo_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Losses Getters
	 */

	SAM_EXPORT double SAM_Pvsamv1_Losses_acwiring_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Losses_dcoptimizer_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Losses_en_snow_model_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Losses_subarray1_dcwiring_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Losses_subarray1_diodeconn_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Losses_subarray1_mismatch_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Losses_subarray1_nameplate_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Losses_subarray1_rear_irradiance_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Losses_subarray1_soiling_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Losses_subarray1_tracking_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Losses_subarray2_dcwiring_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Losses_subarray2_diodeconn_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Losses_subarray2_mismatch_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Losses_subarray2_nameplate_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Losses_subarray2_rear_irradiance_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Losses_subarray2_soiling_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Losses_subarray2_tracking_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Losses_subarray3_dcwiring_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Losses_subarray3_diodeconn_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Losses_subarray3_mismatch_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Losses_subarray3_nameplate_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Losses_subarray3_rear_irradiance_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Losses_subarray3_soiling_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Losses_subarray3_tracking_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Losses_subarray4_dcwiring_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Losses_subarray4_diodeconn_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Losses_subarray4_mismatch_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Losses_subarray4_nameplate_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Losses_subarray4_rear_irradiance_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Losses_subarray4_soiling_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Losses_subarray4_tracking_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Losses_transformer_load_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Losses_transformer_no_load_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Losses_transmission_loss_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Lifetime Getters
	 */

	SAM_EXPORT double* SAM_Pvsamv1_Lifetime_ac_lifetime_losses_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Lifetime_analysis_period_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Lifetime_dc_degradation_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Lifetime_dc_lifetime_losses_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Lifetime_en_ac_lifetime_losses_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Lifetime_en_dc_lifetime_losses_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Lifetime_inflation_rate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Lifetime_save_full_lifetime_variables_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Lifetime_system_use_lifetime_output_nget(SAM_table ptr, SAM_error *err);


	/**
	 * SystemDesign Getters
	 */

	SAM_EXPORT double SAM_Pvsamv1_SystemDesign_enable_mismatch_vmax_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SystemDesign_inverter_count_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray1_azimuth_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray1_backtrack_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray1_gcr_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray1_modules_per_string_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_SystemDesign_subarray1_monthly_tilt_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray1_mppt_input_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray1_nstrings_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray1_rotlim_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray1_tilt_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray1_tilt_eq_lat_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray1_track_mode_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray2_azimuth_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray2_backtrack_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray2_enable_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray2_gcr_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray2_modules_per_string_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_SystemDesign_subarray2_monthly_tilt_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray2_mppt_input_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray2_nstrings_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray2_rotlim_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray2_tilt_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray2_tilt_eq_lat_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray2_track_mode_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray3_azimuth_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray3_backtrack_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray3_enable_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray3_gcr_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray3_modules_per_string_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_SystemDesign_subarray3_monthly_tilt_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray3_mppt_input_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray3_nstrings_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray3_rotlim_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray3_tilt_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray3_tilt_eq_lat_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray3_track_mode_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray4_azimuth_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray4_backtrack_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray4_enable_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray4_gcr_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray4_modules_per_string_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_SystemDesign_subarray4_monthly_tilt_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray4_mppt_input_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray4_nstrings_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray4_rotlim_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray4_tilt_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray4_tilt_eq_lat_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SystemDesign_subarray4_track_mode_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SystemDesign_system_capacity_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Shading Getters
	 */

	SAM_EXPORT double SAM_Pvsamv1_Shading_subarray1_shade_mode_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Shading_subarray1_shading_azal_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Shading_subarray1_shading_diff_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Shading_subarray1_shading_mxh_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Shading_subarray1_shading_string_option_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Shading_subarray1_shading_timestep_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Shading_subarray2_shade_mode_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Shading_subarray2_shading_azal_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Shading_subarray2_shading_diff_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Shading_subarray2_shading_mxh_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Shading_subarray2_shading_string_option_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Shading_subarray2_shading_timestep_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Shading_subarray3_shade_mode_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Shading_subarray3_shading_azal_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Shading_subarray3_shading_diff_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Shading_subarray3_shading_mxh_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Shading_subarray3_shading_string_option_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Shading_subarray3_shading_timestep_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Shading_subarray4_shade_mode_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Shading_subarray4_shading_azal_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Shading_subarray4_shading_diff_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Shading_subarray4_shading_mxh_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Shading_subarray4_shading_string_option_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Shading_subarray4_shading_timestep_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);


	/**
	 * Layout Getters
	 */

	SAM_EXPORT double SAM_Pvsamv1_Layout_module_aspect_ratio_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Layout_subarray1_mod_orient_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Layout_subarray1_nmodx_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Layout_subarray1_nmody_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Layout_subarray2_mod_orient_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Layout_subarray2_nmodx_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Layout_subarray2_nmody_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Layout_subarray3_mod_orient_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Layout_subarray3_nmodx_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Layout_subarray3_nmody_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Layout_subarray4_mod_orient_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Layout_subarray4_nmodx_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Layout_subarray4_nmody_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Module Getters
	 */

	SAM_EXPORT double SAM_Pvsamv1_Module_module_model_nget(SAM_table ptr, SAM_error *err);


	/**
	 * SimpleEfficiencyModuleModel Getters
	 */

	SAM_EXPORT double SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_a_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_area_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_b_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_bifacial_ground_clearance_height_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_bifacial_transmission_factor_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_bifaciality_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_dT_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_eff0_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_eff1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_eff2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_eff3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_eff4_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_fd_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_is_bifacial_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_module_structure_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_rad0_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_rad1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_rad2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_rad3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_rad4_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_reference_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_temp_coeff_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_vmp_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_voc_nget(SAM_table ptr, SAM_error *err);


	/**
	 * CECPerformanceModelWithModuleDatabase Getters
	 */

	SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_a_ref_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_adjust_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_alpha_sc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_area_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_array_cols_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_array_rows_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_backside_temp_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_beta_oc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_bifacial_ground_clearance_height_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_bifacial_transmission_factor_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_bifaciality_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_gamma_r_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_gap_spacing_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_heat_transfer_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_height_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_i_l_ref_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_i_mp_ref_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_i_o_ref_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_i_sc_ref_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_is_bifacial_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_module_length_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_module_width_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_mounting_config_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_mounting_orientation_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_n_s_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_r_s_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_r_sh_ref_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_standoff_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_t_noct_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_temp_corr_mode_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_transient_thermal_model_unit_mass_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_v_mp_ref_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_v_oc_ref_nget(SAM_table ptr, SAM_error *err);


	/**
	 * CECPerformanceModelWithUserEnteredSpecifications Getters
	 */

	SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_aisc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_area_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_bifacial_ground_clearance_height_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_bifacial_transmission_factor_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_bifaciality_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_bvoc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_celltech_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_gpmp_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_imp_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_is_bifacial_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_isc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_mounting_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_nser_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_standoff_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_tnoct_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_transient_thermal_model_unit_mass_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_vmp_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_sixpar_voc_nget(SAM_table ptr, SAM_error *err);


	/**
	 * SandiaPVArrayPerformanceModelWithModuleDatabase Getters
	 */

	SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_a_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_a0_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_a1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_a2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_a3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_a4_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_aimp_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_aisc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_area_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_b_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_b0_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_b1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_b2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_b3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_b4_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_b5_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_bvmpo_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_bvoco_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_c0_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_c1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_c2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_c3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_c4_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_c5_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_c6_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_c7_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_dtc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_fd_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_impo_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_isco_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_ixo_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_ixxo_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_mbvmp_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_mbvoc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_module_structure_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_n_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_ref_a_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_ref_b_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_ref_dT_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_series_cells_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_transient_thermal_model_unit_mass_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_vmpo_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_voco_nget(SAM_table ptr, SAM_error *err);


	/**
	 * IEC61853SingleDiodeModel Getters
	 */

	SAM_EXPORT double SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_AMa0_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_AMa1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_AMa2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_AMa3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_AMa4_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_Egref_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_Il_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_Imp0_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_Io_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_Isc0_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_Vmp0_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_Voc0_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_alphaIsc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_area_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_c1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_c2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_c3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_d1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_d2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_d3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_glass_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_mounting_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_n_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_nser_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_standoff_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_tnoct_nget(SAM_table ptr, SAM_error *err);


	/**
	 * MermoudLejeuneSingleDiodeModel Getters
	 */

	SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_c_lp0_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_c_lp1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_c_lp2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_c_lp3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_c_lp4_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_c_lp5_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_c_sa0_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_c_sa1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_c_sa2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_c_sa3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_c_sa4_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_AM_mode_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_D2MuTau_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_E_g_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_IAM_c_as_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_IAM_c_cs_iamValue_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_IAM_c_cs_incAngle_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_IAM_c_sa0_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_IAM_c_sa1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_IAM_c_sa2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_IAM_c_sa3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_IAM_c_sa4_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_IAM_c_sa5_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_IAM_mode_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_I_mp_ref_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_I_sc_ref_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_Length_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_N_diodes_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_N_parallel_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_N_series_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_R_s_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_R_sh0_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_R_shexp_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_R_shref_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_S_ref_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_T_c_fa_U0_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_T_c_fa_U1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_T_c_fa_alpha_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_T_c_no_mounting_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_T_c_no_standoff_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_T_c_no_tnoct_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_T_mode_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_T_ref_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_V_mp_ref_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_V_oc_ref_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_Width_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_alpha_isc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_beta_voc_spec_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_groundRelfectionFraction_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_mu_n_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_MermoudLejeuneSingleDiodeModel_mlm_n_0_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Inverter Getters
	 */

	SAM_EXPORT double SAM_Pvsamv1_Inverter_inv_cec_cg_eff_cec_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Inverter_inv_cec_cg_paco_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Inverter_inv_ds_eff_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Inverter_inv_ds_paco_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Inverter_inv_num_mppt_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Inverter_inv_pd_eff_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Inverter_inv_pd_paco_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Inverter_inv_snl_eff_cec_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Inverter_inv_snl_paco_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Inverter_inverter_count_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Inverter_inverter_model_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Inverter_mppt_hi_inverter_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Inverter_mppt_low_inverter_nget(SAM_table ptr, SAM_error *err);


	/**
	 * InverterCECDatabase Getters
	 */

	SAM_EXPORT double SAM_Pvsamv1_InverterCECDatabase_inv_snl_c0_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_InverterCECDatabase_inv_snl_c1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_InverterCECDatabase_inv_snl_c2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_InverterCECDatabase_inv_snl_c3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_InverterCECDatabase_inv_snl_paco_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_InverterCECDatabase_inv_snl_pdco_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_InverterCECDatabase_inv_snl_pnt_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_InverterCECDatabase_inv_snl_pso_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_InverterCECDatabase_inv_snl_vdcmax_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_InverterCECDatabase_inv_snl_vdco_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_InverterCECDatabase_inv_tdc_cec_db_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);


	/**
	 * InverterCECCoefficientGenerator Getters
	 */

	SAM_EXPORT double SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_c0_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_c1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_c2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_c3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_paco_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_pdco_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_pnt_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_psco_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_vdcmax_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_vdco_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_tdc_cec_cg_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);


	/**
	 * InverterDatasheet Getters
	 */

	SAM_EXPORT double SAM_Pvsamv1_InverterDatasheet_inv_ds_eff_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_InverterDatasheet_inv_ds_paco_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_InverterDatasheet_inv_ds_pnt_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_InverterDatasheet_inv_ds_pso_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_InverterDatasheet_inv_ds_vdcmax_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_InverterDatasheet_inv_ds_vdco_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_InverterDatasheet_inv_tdc_ds_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);


	/**
	 * InverterPartLoadCurve Getters
	 */

	SAM_EXPORT double* SAM_Pvsamv1_InverterPartLoadCurve_inv_pd_efficiency_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_InverterPartLoadCurve_inv_pd_paco_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_InverterPartLoadCurve_inv_pd_partload_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_InverterPartLoadCurve_inv_pd_pdco_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_InverterPartLoadCurve_inv_pd_pnt_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_InverterPartLoadCurve_inv_pd_vdcmax_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_InverterPartLoadCurve_inv_pd_vdco_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_InverterPartLoadCurve_inv_tdc_plc_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);


	/**
	 * InverterMermoudLejeuneModel Getters
	 */

	SAM_EXPORT double SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_Aux_Loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT const char* SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_CompPMax_sget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT const char* SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_CompVMax_sget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_IMaxAC_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_IMaxDC_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_INomAC_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_INomDC_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT const char* SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_ModeAffEnum_sget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT const char* SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_ModeOper_sget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_NbInputs_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_NbMPPT_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_Night_Loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_PLim1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_PLimAbs_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_PMaxDC_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_PMaxOUT_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_PNomConv_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_PNomDC_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_PSeuil_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_TPLim1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_TPLimAbs_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_TPMax_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_TPNom_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_VAbsMax_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_VMPPMax_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_VMppMin_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_VNomEff_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_VOutConv_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_doAllowOverpower_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_doUseTemperatureLimit_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_effCurve_Pac_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_effCurve_Pdc_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_effCurve_elements_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_effCurve_eta_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_lossRAc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_InverterMermoudLejeuneModel_ond_lossRDc_nget(SAM_table ptr, SAM_error *err);


	/**
	 * BatterySystem Getters
	 */

	SAM_EXPORT double SAM_Pvsamv1_BatterySystem_batt_ac_dc_efficiency_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_BatterySystem_batt_ac_or_dc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_BatterySystem_batt_computed_bank_capacity_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_BatterySystem_batt_computed_series_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_BatterySystem_batt_computed_strings_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_BatterySystem_batt_current_charge_max_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_BatterySystem_batt_current_choice_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_BatterySystem_batt_current_discharge_max_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_BatterySystem_batt_dc_ac_efficiency_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_BatterySystem_batt_dc_dc_efficiency_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_BatterySystem_batt_inverter_efficiency_cutoff_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_BatterySystem_batt_loss_choice_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_BatterySystem_batt_losses_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_BatterySystem_batt_losses_charging_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_BatterySystem_batt_losses_discharging_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_BatterySystem_batt_losses_idle_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_BatterySystem_batt_mass_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_BatterySystem_batt_meter_position_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_BatterySystem_batt_power_charge_max_kwac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_BatterySystem_batt_power_charge_max_kwdc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_BatterySystem_batt_power_discharge_max_kwac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_BatterySystem_batt_power_discharge_max_kwdc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_BatterySystem_batt_replacement_capacity_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_BatterySystem_batt_replacement_option_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_BatterySystem_batt_replacement_schedule_percent_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_BatterySystem_batt_surface_area_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_BatterySystem_en_batt_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_BatterySystem_om_replacement_cost1_aget(SAM_table ptr, int* length, SAM_error *err);


	/**
	 * Load Getters
	 */

	SAM_EXPORT double* SAM_Pvsamv1_Load_crit_load_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Load_load_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Load_load_escalation_aget(SAM_table ptr, int* length, SAM_error *err);


	/**
	 * BatteryCell Getters
	 */

	SAM_EXPORT double SAM_Pvsamv1_BatteryCell_LeadAcid_q10_computed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_BatteryCell_LeadAcid_q20_computed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_BatteryCell_LeadAcid_qn_computed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_BatteryCell_LeadAcid_tn_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_BatteryCell_batt_C_rate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_BatteryCell_batt_Cp_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_BatteryCell_batt_Qexp_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_BatteryCell_batt_Qfull_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_BatteryCell_batt_Qfull_flow_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_BatteryCell_batt_Qnom_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_BatteryCell_batt_Vcut_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_BatteryCell_batt_Vexp_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_BatteryCell_batt_Vfull_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_BatteryCell_batt_Vnom_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_BatteryCell_batt_Vnom_default_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_BatteryCell_batt_calendar_a_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_BatteryCell_batt_calendar_b_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_BatteryCell_batt_calendar_c_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_BatteryCell_batt_calendar_choice_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_BatteryCell_batt_calendar_lifetime_matrix_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_BatteryCell_batt_calendar_q0_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_BatteryCell_batt_chem_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_BatteryCell_batt_h_to_ambient_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_BatteryCell_batt_initial_SOC_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_BatteryCell_batt_life_model_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_BatteryCell_batt_lifetime_matrix_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_BatteryCell_batt_maximum_SOC_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_BatteryCell_batt_minimum_SOC_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_BatteryCell_batt_minimum_modetime_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_BatteryCell_batt_resistance_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_BatteryCell_batt_room_temperature_celsius_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_BatteryCell_batt_voltage_choice_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_BatteryCell_batt_voltage_matrix_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_BatteryCell_cap_vs_temp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);


	/**
	 * BatteryDispatch Getters
	 */

	SAM_EXPORT double SAM_Pvsamv1_BatteryDispatch_batt_auto_gridcharge_max_daily_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_BatteryDispatch_batt_custom_dispatch_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_BatteryDispatch_batt_cycle_cost_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_BatteryDispatch_batt_cycle_cost_choice_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_BatteryDispatch_batt_dispatch_auto_can_charge_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_BatteryDispatch_batt_dispatch_auto_can_clipcharge_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_BatteryDispatch_batt_dispatch_auto_can_fuelcellcharge_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_BatteryDispatch_batt_dispatch_auto_can_gridcharge_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_BatteryDispatch_batt_dispatch_choice_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_BatteryDispatch_batt_dispatch_update_frequency_hours_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_BatteryDispatch_batt_look_ahead_hours_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_BatteryDispatch_batt_pv_ac_forecast_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_BatteryDispatch_batt_pv_clipping_forecast_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_BatteryDispatch_batt_target_choice_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_BatteryDispatch_batt_target_power_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_BatteryDispatch_batt_target_power_monthly_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_BatteryDispatch_dispatch_manual_charge_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_BatteryDispatch_dispatch_manual_discharge_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_BatteryDispatch_dispatch_manual_fuelcellcharge_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_BatteryDispatch_dispatch_manual_gridcharge_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_BatteryDispatch_dispatch_manual_percent_discharge_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_BatteryDispatch_dispatch_manual_percent_gridcharge_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_BatteryDispatch_dispatch_manual_sched_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_BatteryDispatch_dispatch_manual_sched_weekend_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);


	/**
	 * FuelCell Getters
	 */

	SAM_EXPORT double* SAM_Pvsamv1_FuelCell_fuelcell_power_aget(SAM_table ptr, int* length, SAM_error *err);


	/**
	 * PriceSignal Getters
	 */

	SAM_EXPORT double* SAM_Pvsamv1_PriceSignal_dispatch_factors_ts_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_PriceSignal_dispatch_sched_weekday_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_PriceSignal_dispatch_sched_weekend_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_PriceSignal_dispatch_tod_factors_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_PriceSignal_forecast_price_signal_model_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_PriceSignal_mp_ancserv1_revenue_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_PriceSignal_mp_ancserv2_revenue_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_PriceSignal_mp_ancserv3_revenue_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_PriceSignal_mp_ancserv4_revenue_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_PriceSignal_mp_enable_ancserv1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_PriceSignal_mp_enable_ancserv2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_PriceSignal_mp_enable_ancserv3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_PriceSignal_mp_enable_ancserv4_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_PriceSignal_mp_enable_energy_market_revenue_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_PriceSignal_mp_energy_market_revenue_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_PriceSignal_ppa_multiplier_model_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_PriceSignal_ppa_price_input_aget(SAM_table ptr, int* length, SAM_error *err);


	/**
	 * ElectricityRates Getters
	 */

	SAM_EXPORT double* SAM_Pvsamv1_ElectricityRates_rate_escalation_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_ElectricityRates_ur_annual_min_charge_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_ElectricityRates_ur_dc_enable_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_ElectricityRates_ur_dc_flat_mat_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_ElectricityRates_ur_dc_sched_weekday_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_ElectricityRates_ur_dc_sched_weekend_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_ElectricityRates_ur_dc_tou_mat_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_ElectricityRates_ur_ec_sched_weekday_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_ElectricityRates_ur_ec_sched_weekend_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_ElectricityRates_ur_ec_tou_mat_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_ElectricityRates_ur_en_ts_buy_rate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_ElectricityRates_ur_en_ts_sell_rate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_ElectricityRates_ur_metering_option_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_ElectricityRates_ur_monthly_fixed_charge_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_ElectricityRates_ur_monthly_min_charge_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_ElectricityRates_ur_nm_credit_month_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_ElectricityRates_ur_nm_credit_rollover_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_ElectricityRates_ur_nm_yearend_sell_rate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_ElectricityRates_ur_sell_eq_buy_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_ElectricityRates_ur_ts_buy_rate_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_ElectricityRates_ur_ts_sell_rate_aget(SAM_table ptr, int* length, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double SAM_Pvsamv1_Outputs_ac_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_ac_transmission_loss_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_ac_wiring_loss_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_airmass_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_alb_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_ac_battery_loss_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_ac_gross_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_ac_inv_clip_loss_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_ac_inv_eff_loss_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_ac_inv_pnt_loss_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_ac_inv_pso_loss_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_ac_lifetime_loss_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_ac_loss_ond_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_ac_perf_adj_loss_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_ac_wiring_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_ac_wiring_loss_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_dc_battery_loss_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_dc_diodes_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_dc_diodes_loss_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_dc_gross_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_dc_inv_tdc_loss_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_dc_invmppt_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_dc_lifetime_loss_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_dc_loss_ond_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_dc_mismatch_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_dc_mismatch_loss_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_dc_module_loss_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_dc_mppt_clip_loss_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_dc_nameplate_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_dc_nameplate_loss_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_dc_net_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_dc_nominal_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_dc_optimizer_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_dc_optimizer_loss_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_dc_perf_adj_loss_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_dc_snow_loss_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_dc_tracking_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_dc_tracking_loss_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_dc_wiring_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_dc_wiring_loss_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_energy_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_annual_energy_distribution_time_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_annual_export_to_grid_energy_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_gh_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_annual_import_to_grid_energy_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_inv_cliploss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_inv_pntloss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_inv_psoloss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_inv_tdcloss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_poa_beam_eff_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_poa_beam_nom_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_poa_cover_loss_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_poa_eff_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_poa_front_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_poa_nom_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_poa_rear_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_poa_rear_gain_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_poa_shaded_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_poa_shaded_soiled_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_poa_shading_loss_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_poa_soiling_loss_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_snow_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_subarray1_dc_diodes_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_subarray1_dc_gross_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_subarray1_dc_mismatch_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_subarray1_dc_nameplate_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_subarray1_dc_tracking_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_subarray1_dc_wiring_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_subarray2_dc_diodes_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_subarray2_dc_gross_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_subarray2_dc_mismatch_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_subarray2_dc_nameplate_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_subarray2_dc_tracking_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_subarray2_dc_wiring_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_subarray3_dc_diodes_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_subarray3_dc_gross_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_subarray3_dc_mismatch_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_subarray3_dc_nameplate_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_subarray3_dc_tracking_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_subarray3_dc_wiring_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_subarray4_dc_diodes_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_subarray4_dc_gross_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_subarray4_dc_mismatch_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_subarray4_dc_nameplate_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_subarray4_dc_tracking_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_subarray4_dc_wiring_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_total_loss_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_transmission_loss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_transmission_loss_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_annual_xfmr_loss_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_average_battery_conversion_efficiency_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_average_battery_roundtrip_efficiency_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_avg_critical_load_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_DOD_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_DOD_cycle_average_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_I_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_SOC_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_annual_charge_energy_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_annual_charge_from_grid_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_annual_charge_from_system_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_annual_discharge_energy_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_annual_energy_loss_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_annual_energy_system_loss_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_batt_bank_installed_capacity_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_bank_replacement_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_capacity_percent_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_capacity_percent_calendar_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_capacity_percent_cycle_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_capacity_thermal_percent_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_conversion_loss_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_cost_to_cycle_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_cycles_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_dispatch_sched_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_power_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_power_target_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_q0_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_q1_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_q2_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_qmax_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_qmaxI_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_qmax_thermal_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_revenue_charge_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_revenue_clipcharge_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_revenue_discharge_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_revenue_gridcharge_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_batt_system_charge_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_system_loss_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_temperature_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_to_grid_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_to_load_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_voltage_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_batt_voltage_cell_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_capacity_factor_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_capacity_factor_ac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_cdf_of_surviving_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_dc_degrade_factor_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_dc_invmppt_loss_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_dc_net_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_dc_snow_loss_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_df_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_df_calc_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_dn_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_dn_calc_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_fuelcell_to_batt_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_gen_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_gen_without_battery_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_gh_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_gh_calc_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_grid_power_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_grid_power_target_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_grid_to_batt_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_grid_to_load_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_inv_cliploss_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_inv_eff_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_inv_pntloss_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_inv_psoloss_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_inv_tdcloss_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_inv_total_loss_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_inverterMPPT1_DCVoltage_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_inverterMPPT2_DCVoltage_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_inverterMPPT3_DCVoltage_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_inverterMPPT4_DCVoltage_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_kwh_per_kw_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_market_sell_rate_series_yr1_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_monthly_batt_to_grid_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_monthly_batt_to_load_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_monthly_dc_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_monthly_energy_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_monthly_grid_to_batt_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_monthly_grid_to_load_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_monthly_poa_beam_eff_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_monthly_poa_beam_nom_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_monthly_poa_eff_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_monthly_poa_front_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_monthly_poa_nom_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_monthly_poa_rear_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_monthly_snow_loss_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_monthly_system_to_batt_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_monthly_system_to_grid_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_monthly_system_to_load_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_nameplate_dc_rating_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_outage_durations_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_pdf_of_surviving_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_performance_ratio_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_poa_beam_eff_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_poa_beam_nom_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_poa_eff_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_poa_front_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_poa_nom_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_poa_rear_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_poa_shaded_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_poa_shaded_soiled_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_resilience_hrs_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_resilience_hrs_avg_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_resilience_hrs_max_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_resilience_hrs_min_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_shadedb_subarray1_shade_frac_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_shadedb_subarray2_shade_frac_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_shadedb_subarray3_shade_frac_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_shadedb_subarray4_shade_frac_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_sixpar_Adj_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_sixpar_Il_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_sixpar_Io_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_sixpar_Rs_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_sixpar_Rsh_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_sixpar_a_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_snowdepth_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_sol_alt_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_sol_azi_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_sol_zen_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray1_aoi_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray1_aoi_modifier_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray1_axisrot_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray1_beam_shading_factor_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray1_celltemp_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray1_celltempSS_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray1_dc_gross_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray1_dc_voltage_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_subarray1_dcloss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray1_idealrot_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray1_isc_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray1_linear_derate_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray1_modeff_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray1_poa_eff_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray1_poa_eff_beam_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray1_poa_eff_diff_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray1_poa_front_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray1_poa_nom_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray1_poa_rear_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray1_poa_shaded_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray1_poa_shaded_soiled_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray1_snow_coverage_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray1_snow_loss_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray1_soiling_derate_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray1_ss_derate_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray1_ss_diffuse_derate_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray1_ss_reflected_derate_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray1_surf_azi_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray1_surf_tilt_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray1_voc_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray2_aoi_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray2_aoi_modifier_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray2_axisrot_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray2_beam_shading_factor_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray2_celltemp_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray2_celltempSS_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray2_dc_gross_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray2_dc_voltage_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_subarray2_dcloss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray2_idealrot_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray2_isc_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray2_linear_derate_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray2_modeff_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray2_poa_eff_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray2_poa_eff_beam_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray2_poa_eff_diff_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray2_poa_front_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray2_poa_nom_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray2_poa_rear_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray2_poa_shaded_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray2_poa_shaded_soiled_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray2_snow_coverage_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray2_snow_loss_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray2_soiling_derate_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray2_ss_derate_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray2_ss_diffuse_derate_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray2_ss_reflected_derate_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray2_surf_azi_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray2_surf_tilt_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray2_voc_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray3_aoi_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray3_aoi_modifier_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray3_axisrot_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray3_beam_shading_factor_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray3_celltemp_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray3_celltempSS_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray3_dc_gross_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray3_dc_voltage_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_subarray3_dcloss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray3_idealrot_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray3_isc_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray3_linear_derate_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray3_modeff_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray3_poa_eff_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray3_poa_eff_beam_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray3_poa_eff_diff_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray3_poa_front_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray3_poa_nom_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray3_poa_rear_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray3_poa_shaded_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray3_poa_shaded_soiled_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray3_snow_coverage_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray3_snow_loss_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray3_soiling_derate_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray3_ss_derate_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray3_ss_diffuse_derate_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray3_ss_reflected_derate_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray3_surf_azi_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray3_surf_tilt_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray3_voc_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray4_aoi_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray4_aoi_modifier_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray4_axisrot_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray4_beam_shading_factor_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray4_celltemp_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray4_celltempSS_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray4_dc_gross_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray4_dc_voltage_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_subarray4_dcloss_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray4_idealrot_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray4_isc_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray4_linear_derate_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray4_modeff_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray4_poa_eff_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray4_poa_eff_beam_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray4_poa_eff_diff_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray4_poa_front_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray4_poa_nom_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray4_poa_rear_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray4_poa_shaded_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray4_poa_shaded_soiled_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray4_snow_coverage_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray4_snow_loss_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray4_soiling_derate_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray4_ss_derate_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray4_ss_diffuse_derate_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray4_ss_reflected_derate_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray4_surf_azi_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray4_surf_tilt_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_subarray4_voc_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_sunpos_hour_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_sunup_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_survival_function_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_system_to_batt_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_system_to_grid_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_system_to_load_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_tdry_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_ts_shift_hours_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_wfpoa_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_wspd_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_xfmr_ll_ts_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_xfmr_ll_year1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_xfmr_loss_ts_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_xfmr_loss_year1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvsamv1_Outputs_xfmr_nll_ts_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvsamv1_Outputs_xfmr_nll_year1_nget(SAM_table ptr, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif