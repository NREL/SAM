#ifndef SAM_PVWATTSV1_H_
#define SAM_PVWATTSV1_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// Pvwattsv1 Technology Model
	//

	/** 
	 * Create a Pvwattsv1 variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_Pvwattsv1;

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_Pvwattsv1_execute(SAM_table data, int verbosity, SAM_error* err);


	//
	// Weather parameters
	//

	/**
	 * Set solar_resource_file: local weather file path
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvwattsv1_Weather_solar_resource_file_sset(SAM_table ptr, const char* str, SAM_error *err);


	//
	// PVWatts parameters
	//

	/**
	 * Set albedo: Albedo (ground reflectance) [frac]
	 * options: None
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Pvwattsv1_PVWatts_albedo_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ar_glass: Enable anti-reflective glass coating (beta) [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvwattsv1_PVWatts_ar_glass_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set azimuth: Azimuth angle [deg]
	 * options: E=90,S=180,W=270
	 * constraints: MIN=0,MAX=360
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvwattsv1_PVWatts_azimuth_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set concen: Concentration ratio
	 * options: None
	 * constraints: MIN=1
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Pvwattsv1_PVWatts_concen_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set derate: System derate value [frac]
	 * options: None
	 * constraints: MIN=0,MAX=1
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvwattsv1_PVWatts_derate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set enable_user_poa: Enable user-defined POA irradiance input [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvwattsv1_PVWatts_enable_user_poa_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set fd: Diffuse fraction [0..1]
	 * options: None
	 * constraints: MIN=0,MAX=1
	 * required if: ?=1.0
	 */
	SAM_EXPORT void SAM_Pvwattsv1_PVWatts_fd_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set fhconv: Convective heat transfer factor
	 * options: None
	 * constraints: MIN=0.1
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Pvwattsv1_PVWatts_fhconv_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set gamma: Max power temperature coefficient [%/C]
	 * options: None
	 * constraints: None
	 * required if: ?=-0.5
	 */
	SAM_EXPORT void SAM_Pvwattsv1_PVWatts_gamma_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set gcr: Ground coverage ratio [0..1]
	 * options: None
	 * constraints: MIN=0,MAX=3
	 * required if: ?=0.3
	 */
	SAM_EXPORT void SAM_Pvwattsv1_PVWatts_gcr_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set i_ref: Rating condition irradiance [W/m2]
	 * options: None
	 * constraints: POSITIVE
	 * required if: ?=1000
	 */
	SAM_EXPORT void SAM_Pvwattsv1_PVWatts_i_ref_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set inoct: Nominal operating cell temperature [C]
	 * options: None
	 * constraints: POSITIVE
	 * required if: ?=45.0
	 */
	SAM_EXPORT void SAM_Pvwattsv1_PVWatts_inoct_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set inv_eff: Inverter efficiency at rated power [frac]
	 * options: None
	 * constraints: MIN=0,MAX=1
	 * required if: ?=0.92
	 */
	SAM_EXPORT void SAM_Pvwattsv1_PVWatts_inv_eff_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set poa_cutin: Min reqd irradiance for operation [W/m2]
	 * options: None
	 * constraints: MIN=0
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvwattsv1_PVWatts_poa_cutin_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set rotlim: Tracker rotation limit (+/- 1 axis) [deg]
	 * options: None
	 * constraints: MIN=1,MAX=90
	 * required if: ?=45.0
	 */
	SAM_EXPORT void SAM_Pvwattsv1_PVWatts_rotlim_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set shade_mode_1x: Tracker self-shading mode [0/1/2]
	 * options: 0=shading,1=backtrack,2=none
	 * constraints: INTEGER,MIN=0,MAX=2
	 * required if: ?=2
	 */
	SAM_EXPORT void SAM_Pvwattsv1_PVWatts_shade_mode_1x_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set shading:azal: Azimuth x altitude beam shading factors
	 * options: None
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Pvwattsv1_PVWatts_shading_azal_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set shading:diff: Diffuse shading factor
	 * options: None
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Pvwattsv1_PVWatts_shading_diff_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set shading:mxh: Month x Hour beam shading factors
	 * options: None
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Pvwattsv1_PVWatts_shading_mxh_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set shading:timestep: Time step beam shading factors
	 * options: None
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Pvwattsv1_PVWatts_shading_timestep_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set system_size: Nameplate capacity [kW]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvwattsv1_PVWatts_system_size_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tilt: Tilt angle [deg]
	 * options: H=0,V=90
	 * constraints: MIN=0,MAX=90
	 * required if: naof:tilt_eq_lat
	 */
	SAM_EXPORT void SAM_Pvwattsv1_PVWatts_tilt_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tilt_eq_lat: Tilt=latitude override [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: na:tilt
	 */
	SAM_EXPORT void SAM_Pvwattsv1_PVWatts_tilt_eq_lat_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set track_mode: Tracking mode [0/1/2/3]
	 * options: Fixed,1Axis,2Axis,AziAxis
	 * constraints: MIN=0,MAX=3,INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_Pvwattsv1_PVWatts_track_mode_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tref: Reference cell temperature [C]
	 * options: None
	 * constraints: POSITIVE
	 * required if: ?=25.0
	 */
	SAM_EXPORT void SAM_Pvwattsv1_PVWatts_tref_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set u0: thermal model coeff U0
	 * options: None
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Pvwattsv1_PVWatts_u0_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set u1: thermal model coeff U0
	 * options: None
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Pvwattsv1_PVWatts_u1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set user_poa: User-defined POA irradiance [W/m2]
	 * options: None
	 * constraints: LENGTH=8760
	 * required if: enable_user_poa=1
	 */
	SAM_EXPORT void SAM_Pvwattsv1_PVWatts_user_poa_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set w_stow: Wind stow speed [m/s]
	 * options: None
	 * constraints: MIN=0
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvwattsv1_PVWatts_w_stow_nset(SAM_table ptr, double number, SAM_error *err);


	/**
	 * Weather Getters
	 */

	SAM_EXPORT const char* SAM_Pvwattsv1_Weather_solar_resource_file_sget(SAM_table ptr, SAM_error *err);


	/**
	 * PVWatts Getters
	 */

	SAM_EXPORT double SAM_Pvwattsv1_PVWatts_albedo_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv1_PVWatts_ar_glass_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv1_PVWatts_azimuth_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv1_PVWatts_concen_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv1_PVWatts_derate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv1_PVWatts_enable_user_poa_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv1_PVWatts_fd_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv1_PVWatts_fhconv_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv1_PVWatts_gamma_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv1_PVWatts_gcr_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv1_PVWatts_i_ref_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv1_PVWatts_inoct_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv1_PVWatts_inv_eff_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv1_PVWatts_poa_cutin_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv1_PVWatts_rotlim_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv1_PVWatts_shade_mode_1x_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv1_PVWatts_shading_azal_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv1_PVWatts_shading_diff_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv1_PVWatts_shading_mxh_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv1_PVWatts_shading_timestep_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv1_PVWatts_system_size_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv1_PVWatts_tilt_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv1_PVWatts_tilt_eq_lat_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv1_PVWatts_track_mode_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv1_PVWatts_tref_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv1_PVWatts_u0_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv1_PVWatts_u1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv1_PVWatts_user_poa_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv1_PVWatts_w_stow_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double* SAM_Pvwattsv1_Outputs_ac_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv1_Outputs_ac_annual_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv1_Outputs_ac_monthly_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv1_Outputs_annual_energy_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv1_Outputs_annual_energy_distribution_time_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT const char* SAM_Pvwattsv1_Outputs_city_sget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv1_Outputs_dc_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv1_Outputs_dc_monthly_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv1_Outputs_df_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv1_Outputs_dn_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv1_Outputs_elev_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv1_Outputs_gen_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv1_Outputs_gh_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv1_Outputs_lat_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT const char* SAM_Pvwattsv1_Outputs_location_sget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv1_Outputs_lon_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv1_Outputs_monthly_energy_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv1_Outputs_poa_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv1_Outputs_poa_monthly_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv1_Outputs_shad_beam_factor_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv1_Outputs_solrad_annual_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv1_Outputs_solrad_monthly_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT const char* SAM_Pvwattsv1_Outputs_state_sget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv1_Outputs_sunup_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv1_Outputs_tamb_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv1_Outputs_tcell_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv1_Outputs_tdew_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv1_Outputs_tpoa_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Pvwattsv1_Outputs_tz_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Pvwattsv1_Outputs_wspd_aget(SAM_table ptr, int* length, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif