#ifndef SAM_PVGETSHADELOSSMPP_H_
#define SAM_PVGETSHADELOSSMPP_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>

#ifdef __cplusplus
extern "C"
{
#endif

//
// PvGetShadeLossMpp Technology Model
//

/**
 * Create a PvGetShadeLossMpp variable table.
 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
 * @param[in,out] err: a pointer to an error object
 */

SAM_EXPORT typedef void *SAM_PvGetShadeLossMpp;

SAM_EXPORT SAM_PvGetShadeLossMpp SAM_PvGetShadeLossMpp_construct(const char *def, SAM_error *err);

/// verbosity level 0 or 1. Returns 1 on success
SAM_EXPORT int SAM_PvGetShadeLossMpp_execute(SAM_PvGetShadeLossMpp data, int verbosity, SAM_error *err);

SAM_EXPORT void SAM_PvGetShadeLossMpp_destruct(SAM_PvGetShadeLossMpp system);


//
// PVShadeLossDB parameters
//

/**
 * Set diffuse_irrad: Diffuse irradiance
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_PvGetShadeLossMpp_PVShadeLossDB_diffuse_irrad_aset(SAM_PvGetShadeLossMpp ptr, double *arr, int length,
                                                       SAM_error *err);

/**
 * Set global_poa_irrad: Global POA irradiance
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_PvGetShadeLossMpp_PVShadeLossDB_global_poa_irrad_aset(SAM_PvGetShadeLossMpp ptr, double *arr, int length,
                                                          SAM_error *err);

/**
 * Set mods_per_string: Modules per string
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_PvGetShadeLossMpp_PVShadeLossDB_mods_per_string_aset(SAM_PvGetShadeLossMpp ptr, double *arr, int length,
                                                         SAM_error *err);

/**
 * Set pv_cell_temp: PV cell temperature
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_PvGetShadeLossMpp_PVShadeLossDB_pv_cell_temp_aset(SAM_PvGetShadeLossMpp ptr, double *arr, int length,
                                                      SAM_error *err);

/**
 * Set str_shade_fracs: Shading fractions for each string
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_PvGetShadeLossMpp_PVShadeLossDB_str_shade_fracs_mset(SAM_PvGetShadeLossMpp ptr, double *mat, int nrows, int ncols,
                                                         SAM_error *err);

/**
 * Set str_vmp_stc: Unshaded Vmp of the string at STC
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_PvGetShadeLossMpp_PVShadeLossDB_str_vmp_stc_aset(SAM_PvGetShadeLossMpp ptr, double *arr, int length,
                                                                     SAM_error *err);

/**
 * Set v_mppt_high: Upper bound of inverter MPPT range
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_PvGetShadeLossMpp_PVShadeLossDB_v_mppt_high_aset(SAM_PvGetShadeLossMpp ptr, double *arr, int length,
                                                                     SAM_error *err);

/**
 * Set v_mppt_low: Lower bound of inverter MPPT range
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_PvGetShadeLossMpp_PVShadeLossDB_v_mppt_low_aset(SAM_PvGetShadeLossMpp ptr, double *arr, int length, SAM_error *err);


/**
 * PVShadeLossDB Getters
 */

SAM_EXPORT double *
SAM_PvGetShadeLossMpp_PVShadeLossDB_diffuse_irrad_aget(SAM_PvGetShadeLossMpp ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_PvGetShadeLossMpp_PVShadeLossDB_global_poa_irrad_aget(SAM_PvGetShadeLossMpp ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_PvGetShadeLossMpp_PVShadeLossDB_mods_per_string_aget(SAM_PvGetShadeLossMpp ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_PvGetShadeLossMpp_PVShadeLossDB_pv_cell_temp_aget(SAM_PvGetShadeLossMpp ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_PvGetShadeLossMpp_PVShadeLossDB_str_shade_fracs_mget(SAM_PvGetShadeLossMpp ptr, int *nrows, int *ncols,
                                                         SAM_error *err);

SAM_EXPORT double *
SAM_PvGetShadeLossMpp_PVShadeLossDB_str_vmp_stc_aget(SAM_PvGetShadeLossMpp ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_PvGetShadeLossMpp_PVShadeLossDB_v_mppt_high_aget(SAM_PvGetShadeLossMpp ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_PvGetShadeLossMpp_PVShadeLossDB_v_mppt_low_aget(SAM_PvGetShadeLossMpp ptr, int *length, SAM_error *err);


/**
 * Outputs Getters
 */

SAM_EXPORT double *SAM_PvGetShadeLossMpp_Outputs_N_aget(SAM_PvGetShadeLossMpp ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_PvGetShadeLossMpp_Outputs_S_aget(SAM_PvGetShadeLossMpp ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_PvGetShadeLossMpp_Outputs_d_aget(SAM_PvGetShadeLossMpp ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_PvGetShadeLossMpp_Outputs_shade_loss_aget(SAM_PvGetShadeLossMpp ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_PvGetShadeLossMpp_Outputs_t_aget(SAM_PvGetShadeLossMpp ptr, int *length, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif
