#ifndef SAM_IRRADPROC_H_
#define SAM_IRRADPROC_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>

#ifdef __cplusplus
extern "C"
{
#endif

//
// Irradproc Technology Model
//

/**
 * Create a Irradproc variable table.
 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
 * @param[in,out] err: a pointer to an error object
 */

SAM_EXPORT typedef void *SAM_Irradproc;

SAM_EXPORT SAM_Irradproc SAM_Irradproc_construct(const char *def, SAM_error *err);

/// verbosity level 0 or 1. Returns 1 on success
SAM_EXPORT int SAM_Irradproc_execute(SAM_Irradproc data, int verbosity, SAM_error *err);

SAM_EXPORT void SAM_Irradproc_destruct(SAM_Irradproc system);


//
// IrradianceProcessor parameters
//

/**
 * Set albedo: Ground reflectance (time depend.) [frac]
 * options: 0..1
 * constraints: LENGTH_EQUAL=beam
 * required if: ?
 */
SAM_EXPORT void
SAM_Irradproc_IrradianceProcessor_albedo_aset(SAM_Irradproc ptr, double *arr, int length, SAM_error *err);

/**
 * Set albedo_const: Ground reflectance (single value) [frac]
 * options: 0..1
 * constraints: None
 * required if: ?=0.2
 */
SAM_EXPORT void SAM_Irradproc_IrradianceProcessor_albedo_const_nset(SAM_Irradproc ptr, double number, SAM_error *err);

/**
 * Set azimuth: Azimuth angle [deg]
 * options: E=90,S=180,W=270
 * constraints: MIN=0,MAX=360
 * required if: *
 */
SAM_EXPORT void SAM_Irradproc_IrradianceProcessor_azimuth_nset(SAM_Irradproc ptr, double number, SAM_error *err);

/**
 * Set backtrack: Enable backtracking [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=0
 */
SAM_EXPORT void SAM_Irradproc_IrradianceProcessor_backtrack_nset(SAM_Irradproc ptr, double number, SAM_error *err);

/**
 * Set beam: Beam normal irradiance [W/m2]
 * options: None
 * constraints: None
 * required if: irrad_mode~2
 */
SAM_EXPORT void SAM_Irradproc_IrradianceProcessor_beam_aset(SAM_Irradproc ptr, double *arr, int length, SAM_error *err);

/**
 * Set day: Day [dy]
 * options: 1-days in month
 * constraints: LENGTH_EQUAL=beam
 * required if: *
 */
SAM_EXPORT void SAM_Irradproc_IrradianceProcessor_day_aset(SAM_Irradproc ptr, double *arr, int length, SAM_error *err);

/**
 * Set diffuse: Diffuse horizontal irradiance [W/m2]
 * options: None
 * constraints: LENGTH_EQUAL=beam
 * required if: irrad_mode~1
 */
SAM_EXPORT void
SAM_Irradproc_IrradianceProcessor_diffuse_aset(SAM_Irradproc ptr, double *arr, int length, SAM_error *err);

/**
 * Set gcr: Ground coverage ratio [0..1]
 * options: None
 * constraints: MIN=0,MAX=1
 * required if: backtrack=1
 */
SAM_EXPORT void SAM_Irradproc_IrradianceProcessor_gcr_nset(SAM_Irradproc ptr, double number, SAM_error *err);

/**
 * Set global: Global horizontal irradiance [W/m2]
 * options: None
 * constraints: LENGTH_EQUAL=beam
 * required if: irrad_mode~0
 */
SAM_EXPORT void
SAM_Irradproc_IrradianceProcessor_global_aset(SAM_Irradproc ptr, double *arr, int length, SAM_error *err);

/**
 * Set hour: Hour [hr]
 * options: 0-23
 * constraints: LENGTH_EQUAL=beam
 * required if: *
 */
SAM_EXPORT void SAM_Irradproc_IrradianceProcessor_hour_aset(SAM_Irradproc ptr, double *arr, int length, SAM_error *err);

/**
 * Set irrad_mode: Irradiance input mode [0/1/2]
 * options: Beam+Diff,Global+Beam, Global+Diff
 * constraints: INTEGER,MIN=0,MAX=2
 * required if: ?=0
 */
SAM_EXPORT void SAM_Irradproc_IrradianceProcessor_irrad_mode_nset(SAM_Irradproc ptr, double number, SAM_error *err);

/**
 * Set lat: Latitude [deg]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Irradproc_IrradianceProcessor_lat_nset(SAM_Irradproc ptr, double number, SAM_error *err);

/**
 * Set lon: Longitude [deg]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Irradproc_IrradianceProcessor_lon_nset(SAM_Irradproc ptr, double number, SAM_error *err);

/**
 * Set minute: Minute [min]
 * options: 0-59
 * constraints: LENGTH_EQUAL=beam
 * required if: *
 */
SAM_EXPORT void
SAM_Irradproc_IrradianceProcessor_minute_aset(SAM_Irradproc ptr, double *arr, int length, SAM_error *err);

/**
 * Set month: Month [mn]
 * options: 1-12
 * constraints: LENGTH_EQUAL=beam
 * required if: *
 */
SAM_EXPORT void
SAM_Irradproc_IrradianceProcessor_month_aset(SAM_Irradproc ptr, double *arr, int length, SAM_error *err);

/**
 * Set rotlim: Rotational limit on tracker [deg]
 * options: None
 * constraints: MIN=0,MAX=90
 * required if: ?=45
 */
SAM_EXPORT void SAM_Irradproc_IrradianceProcessor_rotlim_nset(SAM_Irradproc ptr, double number, SAM_error *err);

/**
 * Set sky_model: Tilted surface irradiance model [0/1/2]
 * options: Isotropic,HDKR,Perez
 * constraints: INTEGER,MIN=0,MAX=2
 * required if: ?=2
 */
SAM_EXPORT void SAM_Irradproc_IrradianceProcessor_sky_model_nset(SAM_Irradproc ptr, double number, SAM_error *err);

/**
 * Set tilt: Tilt angle [deg]
 * options: H=0,V=90
 * constraints: MIN=0,MAX=90
 * required if: ?
 */
SAM_EXPORT void SAM_Irradproc_IrradianceProcessor_tilt_nset(SAM_Irradproc ptr, double number, SAM_error *err);

/**
 * Set track_mode: Tracking mode [0/1/2]
 * options: Fixed,1Axis,2Axis
 * constraints: MIN=0,MAX=2,INTEGER
 * required if: *
 */
SAM_EXPORT void SAM_Irradproc_IrradianceProcessor_track_mode_nset(SAM_Irradproc ptr, double number, SAM_error *err);

/**
 * Set tz: Time zone [hr]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Irradproc_IrradianceProcessor_tz_nset(SAM_Irradproc ptr, double number, SAM_error *err);

/**
 * Set year: Year [yr]
 * options: None
 * constraints: LENGTH_EQUAL=beam
 * required if: *
 */
SAM_EXPORT void SAM_Irradproc_IrradianceProcessor_year_aset(SAM_Irradproc ptr, double *arr, int length, SAM_error *err);


/**
 * IrradianceProcessor Getters
 */

SAM_EXPORT double *SAM_Irradproc_IrradianceProcessor_albedo_aget(SAM_Irradproc ptr, int *length, SAM_error *err);

SAM_EXPORT double SAM_Irradproc_IrradianceProcessor_albedo_const_nget(SAM_Irradproc ptr, SAM_error *err);

SAM_EXPORT double SAM_Irradproc_IrradianceProcessor_azimuth_nget(SAM_Irradproc ptr, SAM_error *err);

SAM_EXPORT double SAM_Irradproc_IrradianceProcessor_backtrack_nget(SAM_Irradproc ptr, SAM_error *err);

SAM_EXPORT double *SAM_Irradproc_IrradianceProcessor_beam_aget(SAM_Irradproc ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Irradproc_IrradianceProcessor_day_aget(SAM_Irradproc ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Irradproc_IrradianceProcessor_diffuse_aget(SAM_Irradproc ptr, int *length, SAM_error *err);

SAM_EXPORT double SAM_Irradproc_IrradianceProcessor_gcr_nget(SAM_Irradproc ptr, SAM_error *err);

SAM_EXPORT double *SAM_Irradproc_IrradianceProcessor_global_aget(SAM_Irradproc ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Irradproc_IrradianceProcessor_hour_aget(SAM_Irradproc ptr, int *length, SAM_error *err);

SAM_EXPORT double SAM_Irradproc_IrradianceProcessor_irrad_mode_nget(SAM_Irradproc ptr, SAM_error *err);

SAM_EXPORT double SAM_Irradproc_IrradianceProcessor_lat_nget(SAM_Irradproc ptr, SAM_error *err);

SAM_EXPORT double SAM_Irradproc_IrradianceProcessor_lon_nget(SAM_Irradproc ptr, SAM_error *err);

SAM_EXPORT double *SAM_Irradproc_IrradianceProcessor_minute_aget(SAM_Irradproc ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Irradproc_IrradianceProcessor_month_aget(SAM_Irradproc ptr, int *length, SAM_error *err);

SAM_EXPORT double SAM_Irradproc_IrradianceProcessor_rotlim_nget(SAM_Irradproc ptr, SAM_error *err);

SAM_EXPORT double SAM_Irradproc_IrradianceProcessor_sky_model_nget(SAM_Irradproc ptr, SAM_error *err);

SAM_EXPORT double SAM_Irradproc_IrradianceProcessor_tilt_nget(SAM_Irradproc ptr, SAM_error *err);

SAM_EXPORT double SAM_Irradproc_IrradianceProcessor_track_mode_nget(SAM_Irradproc ptr, SAM_error *err);

SAM_EXPORT double SAM_Irradproc_IrradianceProcessor_tz_nget(SAM_Irradproc ptr, SAM_error *err);

SAM_EXPORT double *SAM_Irradproc_IrradianceProcessor_year_aget(SAM_Irradproc ptr, int *length, SAM_error *err);


/**
 * Outputs Getters
 */

SAM_EXPORT double *SAM_Irradproc_Outputs_axis_rotation_aget(SAM_Irradproc ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Irradproc_Outputs_bt_diff_aget(SAM_Irradproc ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Irradproc_Outputs_incidence_aget(SAM_Irradproc ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Irradproc_Outputs_poa_beam_aget(SAM_Irradproc ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Irradproc_Outputs_poa_gnddiff_aget(SAM_Irradproc ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Irradproc_Outputs_poa_skydiff_aget(SAM_Irradproc ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Irradproc_Outputs_poa_skydiff_cir_aget(SAM_Irradproc ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Irradproc_Outputs_poa_skydiff_hor_aget(SAM_Irradproc ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Irradproc_Outputs_poa_skydiff_iso_aget(SAM_Irradproc ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Irradproc_Outputs_sun_azm_aget(SAM_Irradproc ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Irradproc_Outputs_sun_dec_aget(SAM_Irradproc ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Irradproc_Outputs_sun_elv_aget(SAM_Irradproc ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Irradproc_Outputs_sun_zen_aget(SAM_Irradproc ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Irradproc_Outputs_surf_azm_aget(SAM_Irradproc ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Irradproc_Outputs_surf_tilt_aget(SAM_Irradproc ptr, int *length, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif
