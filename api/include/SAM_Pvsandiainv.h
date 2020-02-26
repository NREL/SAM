#ifndef SAM_PVSANDIAINV_H_
#define SAM_PVSANDIAINV_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>

#ifdef __cplusplus
extern "C"
{
#endif

//
// Pvsandiainv Technology Model
//

/**
 * Create a Pvsandiainv variable table.
 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
 * @param[in,out] err: a pointer to an error object
 */

SAM_EXPORT typedef void *SAM_Pvsandiainv;

SAM_EXPORT SAM_Pvsandiainv SAM_Pvsandiainv_construct(const char *def, SAM_error *err);

/// verbosity level 0 or 1. Returns 1 on success
SAM_EXPORT int SAM_Pvsandiainv_execute(SAM_Pvsandiainv data, int verbosity, SAM_error *err);

SAM_EXPORT void SAM_Pvsandiainv_destruct(SAM_Pvsandiainv system);


//
// SandiaInverterModel parameters
//

/**
 * Set c0: C0: Defines parabolic curvature of relationship between ac power and dc power at reference conditions [1/W]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Pvsandiainv_SandiaInverterModel_c0_nset(SAM_Pvsandiainv ptr, double number, SAM_error *err);

/**
 * Set c1: C1: Parameter allowing Pdco to vary linearly with dc voltage input [1/V]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Pvsandiainv_SandiaInverterModel_c1_nset(SAM_Pvsandiainv ptr, double number, SAM_error *err);

/**
 * Set c2: C2: Parameter allowing Pso to vary linearly with dc voltage input  [1/V]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Pvsandiainv_SandiaInverterModel_c2_nset(SAM_Pvsandiainv ptr, double number, SAM_error *err);

/**
 * Set c3: C3: Parameter allowing C0 to vary linearly with dc voltage input [1/V]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Pvsandiainv_SandiaInverterModel_c3_nset(SAM_Pvsandiainv ptr, double number, SAM_error *err);

/**
 * Set dc: DC power input to inverter [Watt]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_Pvsandiainv_SandiaInverterModel_dc_aset(SAM_Pvsandiainv ptr, double *arr, int length, SAM_error *err);

/**
 * Set dc_voltage: DC voltage input to inverter [Volt]
 * options: None
 * constraints: LENGTH_EQUAL=dc
 * required if: *
 */
SAM_EXPORT void
SAM_Pvsandiainv_SandiaInverterModel_dc_voltage_aset(SAM_Pvsandiainv ptr, double *arr, int length, SAM_error *err);

/**
 * Set paco: Max AC power rating [Wac]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Pvsandiainv_SandiaInverterModel_paco_nset(SAM_Pvsandiainv ptr, double number, SAM_error *err);

/**
 * Set pdco: DC power level at which Paco is achieved [Wdc]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Pvsandiainv_SandiaInverterModel_pdco_nset(SAM_Pvsandiainv ptr, double number, SAM_error *err);

/**
 * Set pntare: Parasitic AC consumption [Wac]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Pvsandiainv_SandiaInverterModel_pntare_nset(SAM_Pvsandiainv ptr, double number, SAM_error *err);

/**
 * Set pso: DC power level required to start inversion [Wdc]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Pvsandiainv_SandiaInverterModel_pso_nset(SAM_Pvsandiainv ptr, double number, SAM_error *err);

/**
 * Set vdco: DV voltage level at which Paco is achieved [Volt]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Pvsandiainv_SandiaInverterModel_vdco_nset(SAM_Pvsandiainv ptr, double number, SAM_error *err);


/**
 * SandiaInverterModel Getters
 */

SAM_EXPORT double SAM_Pvsandiainv_SandiaInverterModel_c0_nget(SAM_Pvsandiainv ptr, SAM_error *err);

SAM_EXPORT double SAM_Pvsandiainv_SandiaInverterModel_c1_nget(SAM_Pvsandiainv ptr, SAM_error *err);

SAM_EXPORT double SAM_Pvsandiainv_SandiaInverterModel_c2_nget(SAM_Pvsandiainv ptr, SAM_error *err);

SAM_EXPORT double SAM_Pvsandiainv_SandiaInverterModel_c3_nget(SAM_Pvsandiainv ptr, SAM_error *err);

SAM_EXPORT double *SAM_Pvsandiainv_SandiaInverterModel_dc_aget(SAM_Pvsandiainv ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Pvsandiainv_SandiaInverterModel_dc_voltage_aget(SAM_Pvsandiainv ptr, int *length, SAM_error *err);

SAM_EXPORT double SAM_Pvsandiainv_SandiaInverterModel_paco_nget(SAM_Pvsandiainv ptr, SAM_error *err);

SAM_EXPORT double SAM_Pvsandiainv_SandiaInverterModel_pdco_nget(SAM_Pvsandiainv ptr, SAM_error *err);

SAM_EXPORT double SAM_Pvsandiainv_SandiaInverterModel_pntare_nget(SAM_Pvsandiainv ptr, SAM_error *err);

SAM_EXPORT double SAM_Pvsandiainv_SandiaInverterModel_pso_nget(SAM_Pvsandiainv ptr, SAM_error *err);

SAM_EXPORT double SAM_Pvsandiainv_SandiaInverterModel_vdco_nget(SAM_Pvsandiainv ptr, SAM_error *err);


/**
 * Outputs Getters
 */

SAM_EXPORT double *SAM_Pvsandiainv_Outputs_ac_aget(SAM_Pvsandiainv ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Pvsandiainv_Outputs_acpar_aget(SAM_Pvsandiainv ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Pvsandiainv_Outputs_cliploss_aget(SAM_Pvsandiainv ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Pvsandiainv_Outputs_eff_inv_aget(SAM_Pvsandiainv ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Pvsandiainv_Outputs_ntloss_aget(SAM_Pvsandiainv ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Pvsandiainv_Outputs_plr_aget(SAM_Pvsandiainv ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Pvsandiainv_Outputs_soloss_aget(SAM_Pvsandiainv ptr, int *length, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif
