#ifndef SAM_PV6PARMOD_H_
#define SAM_PV6PARMOD_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>

#ifdef __cplusplus
extern "C"
{
#endif

//
// Pv6parmod Technology Model
//

/**
 * Create a Pv6parmod variable table.
 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
 * @param[in,out] err: a pointer to an error object
 */

SAM_EXPORT typedef void *SAM_Pv6parmod;

SAM_EXPORT SAM_Pv6parmod SAM_Pv6parmod_construct(const char *def, SAM_error *err);

/// verbosity level 0 or 1. Returns 1 on success
SAM_EXPORT int SAM_Pv6parmod_execute(SAM_Pv6parmod data, int verbosity, SAM_error *err);

SAM_EXPORT void SAM_Pv6parmod_destruct(SAM_Pv6parmod system);


//
// Weather parameters
//

/**
 * Set elev: Site elevation [m]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Pv6parmod_Weather_elev_nset(SAM_Pv6parmod ptr, double number, SAM_error *err);

/**
 * Set incidence: Incidence angle to surface [deg]
 * options: None
 * constraints: LENGTH_EQUAL=poa_beam
 * required if: *
 */
SAM_EXPORT void SAM_Pv6parmod_Weather_incidence_aset(SAM_Pv6parmod ptr, double *arr, int length, SAM_error *err);

/**
 * Set poa_beam: Incident direct normal radiation [W/m2]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Pv6parmod_Weather_poa_beam_aset(SAM_Pv6parmod ptr, double *arr, int length, SAM_error *err);

/**
 * Set poa_gnddiff: Incident ground diffuse irradiance [W/m2]
 * options: None
 * constraints: LENGTH_EQUAL=poa_beam
 * required if: *
 */
SAM_EXPORT void SAM_Pv6parmod_Weather_poa_gnddiff_aset(SAM_Pv6parmod ptr, double *arr, int length, SAM_error *err);

/**
 * Set poa_skydiff: Incident sky diffuse radiation [W/m2]
 * options: None
 * constraints: LENGTH_EQUAL=poa_beam
 * required if: *
 */
SAM_EXPORT void SAM_Pv6parmod_Weather_poa_skydiff_aset(SAM_Pv6parmod ptr, double *arr, int length, SAM_error *err);

/**
 * Set sun_zen: Sun zenith angle [deg]
 * options: None
 * constraints: LENGTH_EQUAL=poa_beam
 * required if: *
 */
SAM_EXPORT void SAM_Pv6parmod_Weather_sun_zen_aset(SAM_Pv6parmod ptr, double *arr, int length, SAM_error *err);

/**
 * Set surf_tilt: Surface tilt angle [deg]
 * options: None
 * constraints: LENGTH_EQUAL=poa_beam
 * required if: *
 */
SAM_EXPORT void SAM_Pv6parmod_Weather_surf_tilt_aset(SAM_Pv6parmod ptr, double *arr, int length, SAM_error *err);

/**
 * Set tdry: Dry bulb temperature ['C]
 * options: None
 * constraints: LENGTH_EQUAL=poa_beam
 * required if: *
 */
SAM_EXPORT void SAM_Pv6parmod_Weather_tdry_aset(SAM_Pv6parmod ptr, double *arr, int length, SAM_error *err);

/**
 * Set wdir: Wind direction [deg]
 * options: None
 * constraints: LENGTH_EQUAL=poa_beam
 * required if: *
 */
SAM_EXPORT void SAM_Pv6parmod_Weather_wdir_aset(SAM_Pv6parmod ptr, double *arr, int length, SAM_error *err);

/**
 * Set wspd: Wind speed [m/s]
 * options: None
 * constraints: LENGTH_EQUAL=poa_beam
 * required if: *
 */
SAM_EXPORT void SAM_Pv6parmod_Weather_wspd_aset(SAM_Pv6parmod ptr, double *arr, int length, SAM_error *err);


//
// CEC6ParameterPVModuleModel parameters
//

/**
 * Set Adj: OC SC temp coeff adjustment [%]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Pv6parmod_CEC6ParameterPVModuleModel_Adj_nset(SAM_Pv6parmod ptr, double number, SAM_error *err);

/**
 * Set Il: Light current [A]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Pv6parmod_CEC6ParameterPVModuleModel_Il_nset(SAM_Pv6parmod ptr, double number, SAM_error *err);

/**
 * Set Imp: Maximum power point current [A]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Pv6parmod_CEC6ParameterPVModuleModel_Imp_nset(SAM_Pv6parmod ptr, double number, SAM_error *err);

/**
 * Set Io: Saturation current [A]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Pv6parmod_CEC6ParameterPVModuleModel_Io_nset(SAM_Pv6parmod ptr, double number, SAM_error *err);

/**
 * Set Isc: Short circuit current [A]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Pv6parmod_CEC6ParameterPVModuleModel_Isc_nset(SAM_Pv6parmod ptr, double number, SAM_error *err);

/**
 * Set Rs: Series resistance [ohm]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Pv6parmod_CEC6ParameterPVModuleModel_Rs_nset(SAM_Pv6parmod ptr, double number, SAM_error *err);

/**
 * Set Rsh: Shunt resistance [ohm]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Pv6parmod_CEC6ParameterPVModuleModel_Rsh_nset(SAM_Pv6parmod ptr, double number, SAM_error *err);

/**
 * Set Vmp: Maximum power point voltage [V]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Pv6parmod_CEC6ParameterPVModuleModel_Vmp_nset(SAM_Pv6parmod ptr, double number, SAM_error *err);

/**
 * Set Voc: Open circuit voltage [V]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Pv6parmod_CEC6ParameterPVModuleModel_Voc_nset(SAM_Pv6parmod ptr, double number, SAM_error *err);

/**
 * Set a: Modified nonideality factor [1/V]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Pv6parmod_CEC6ParameterPVModuleModel_a_nset(SAM_Pv6parmod ptr, double number, SAM_error *err);

/**
 * Set alpha_isc: Temp coeff of current at SC [A/'C]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_Pv6parmod_CEC6ParameterPVModuleModel_alpha_isc_nset(SAM_Pv6parmod ptr, double number, SAM_error *err);

/**
 * Set area: Module area [m2]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Pv6parmod_CEC6ParameterPVModuleModel_area_nset(SAM_Pv6parmod ptr, double number, SAM_error *err);

/**
 * Set beta_voc: Temp coeff of voltage at OC [V/'C]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_Pv6parmod_CEC6ParameterPVModuleModel_beta_voc_nset(SAM_Pv6parmod ptr, double number, SAM_error *err);

/**
 * Set gamma_pmp: Temp coeff of power at MP [%/'C]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_Pv6parmod_CEC6ParameterPVModuleModel_gamma_pmp_nset(SAM_Pv6parmod ptr, double number, SAM_error *err);

/**
 * Set height: System installation height [0/1]
 * options: 0=less than 22ft, 1=more than 22ft
 * constraints: INTEGER,MIN=0,MAX=1
 * required if: ?=0
 */
SAM_EXPORT void SAM_Pv6parmod_CEC6ParameterPVModuleModel_height_nset(SAM_Pv6parmod ptr, double number, SAM_error *err);

/**
 * Set opvoltage: Module operating voltage [Volt]
 * options: None
 * constraints: None
 * required if: ?
 */
SAM_EXPORT void
SAM_Pv6parmod_CEC6ParameterPVModuleModel_opvoltage_aset(SAM_Pv6parmod ptr, double *arr, int length, SAM_error *err);

/**
 * Set standoff: Mounting standoff option [0..6]
 * options: 0=bipv, 1= >3.5in, 2=2.5-3.5in, 3=1.5-2.5in, 4=0.5-1.5in, 5= <0.5in, 6=ground/rack
 * constraints: INTEGER,MIN=0,MAX=6
 * required if: ?=6
 */
SAM_EXPORT void
SAM_Pv6parmod_CEC6ParameterPVModuleModel_standoff_nset(SAM_Pv6parmod ptr, double number, SAM_error *err);

/**
 * Set tnoct: NOCT cell temperature ['C]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Pv6parmod_CEC6ParameterPVModuleModel_tnoct_nset(SAM_Pv6parmod ptr, double number, SAM_error *err);


/**
 * Weather Getters
 */

SAM_EXPORT double SAM_Pv6parmod_Weather_elev_nget(SAM_Pv6parmod ptr, SAM_error *err);

SAM_EXPORT double *SAM_Pv6parmod_Weather_incidence_aget(SAM_Pv6parmod ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Pv6parmod_Weather_poa_beam_aget(SAM_Pv6parmod ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Pv6parmod_Weather_poa_gnddiff_aget(SAM_Pv6parmod ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Pv6parmod_Weather_poa_skydiff_aget(SAM_Pv6parmod ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Pv6parmod_Weather_sun_zen_aget(SAM_Pv6parmod ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Pv6parmod_Weather_surf_tilt_aget(SAM_Pv6parmod ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Pv6parmod_Weather_tdry_aget(SAM_Pv6parmod ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Pv6parmod_Weather_wdir_aget(SAM_Pv6parmod ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Pv6parmod_Weather_wspd_aget(SAM_Pv6parmod ptr, int *length, SAM_error *err);


/**
 * CEC6ParameterPVModuleModel Getters
 */

SAM_EXPORT double SAM_Pv6parmod_CEC6ParameterPVModuleModel_Adj_nget(SAM_Pv6parmod ptr, SAM_error *err);

SAM_EXPORT double SAM_Pv6parmod_CEC6ParameterPVModuleModel_Il_nget(SAM_Pv6parmod ptr, SAM_error *err);

SAM_EXPORT double SAM_Pv6parmod_CEC6ParameterPVModuleModel_Imp_nget(SAM_Pv6parmod ptr, SAM_error *err);

SAM_EXPORT double SAM_Pv6parmod_CEC6ParameterPVModuleModel_Io_nget(SAM_Pv6parmod ptr, SAM_error *err);

SAM_EXPORT double SAM_Pv6parmod_CEC6ParameterPVModuleModel_Isc_nget(SAM_Pv6parmod ptr, SAM_error *err);

SAM_EXPORT double SAM_Pv6parmod_CEC6ParameterPVModuleModel_Rs_nget(SAM_Pv6parmod ptr, SAM_error *err);

SAM_EXPORT double SAM_Pv6parmod_CEC6ParameterPVModuleModel_Rsh_nget(SAM_Pv6parmod ptr, SAM_error *err);

SAM_EXPORT double SAM_Pv6parmod_CEC6ParameterPVModuleModel_Vmp_nget(SAM_Pv6parmod ptr, SAM_error *err);

SAM_EXPORT double SAM_Pv6parmod_CEC6ParameterPVModuleModel_Voc_nget(SAM_Pv6parmod ptr, SAM_error *err);

SAM_EXPORT double SAM_Pv6parmod_CEC6ParameterPVModuleModel_a_nget(SAM_Pv6parmod ptr, SAM_error *err);

SAM_EXPORT double SAM_Pv6parmod_CEC6ParameterPVModuleModel_alpha_isc_nget(SAM_Pv6parmod ptr, SAM_error *err);

SAM_EXPORT double SAM_Pv6parmod_CEC6ParameterPVModuleModel_area_nget(SAM_Pv6parmod ptr, SAM_error *err);

SAM_EXPORT double SAM_Pv6parmod_CEC6ParameterPVModuleModel_beta_voc_nget(SAM_Pv6parmod ptr, SAM_error *err);

SAM_EXPORT double SAM_Pv6parmod_CEC6ParameterPVModuleModel_gamma_pmp_nget(SAM_Pv6parmod ptr, SAM_error *err);

SAM_EXPORT double SAM_Pv6parmod_CEC6ParameterPVModuleModel_height_nget(SAM_Pv6parmod ptr, SAM_error *err);

SAM_EXPORT double *
SAM_Pv6parmod_CEC6ParameterPVModuleModel_opvoltage_aget(SAM_Pv6parmod ptr, int *length, SAM_error *err);

SAM_EXPORT double SAM_Pv6parmod_CEC6ParameterPVModuleModel_standoff_nget(SAM_Pv6parmod ptr, SAM_error *err);

SAM_EXPORT double SAM_Pv6parmod_CEC6ParameterPVModuleModel_tnoct_nget(SAM_Pv6parmod ptr, SAM_error *err);


/**
 * Outputs Getters
 */

SAM_EXPORT double *SAM_Pv6parmod_Outputs_dc_aget(SAM_Pv6parmod ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Pv6parmod_Outputs_dc_current_aget(SAM_Pv6parmod ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Pv6parmod_Outputs_dc_voltage_aget(SAM_Pv6parmod ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Pv6parmod_Outputs_eff_aget(SAM_Pv6parmod ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Pv6parmod_Outputs_tcell_aget(SAM_Pv6parmod ptr, int *length, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif
