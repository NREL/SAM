#ifndef SAM_6PARSOLVE_H_
#define SAM_6PARSOLVE_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>

#ifdef __cplusplus
extern "C"
{
#endif

//
// 6parsolve Technology Model
//

/**
 * Create a 6parsolve variable table.
 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
 * @param[in,out] err: a pointer to an error object
 */

SAM_EXPORT typedef void *SAM_6parsolve;

SAM_EXPORT SAM_6parsolve SAM_6parsolve_construct(const char *def, SAM_error *err);

/// verbosity level 0 or 1. Returns 1 on success
SAM_EXPORT int SAM_6parsolve_execute(SAM_6parsolve data, int verbosity, SAM_error *err);

SAM_EXPORT void SAM_6parsolve_destruct(SAM_6parsolve system);


//
// SixParameterSolver parameters
//

/**
 * Set Imp: Maximum power point current [A]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_6parsolve_SixParameterSolver_Imp_nset(SAM_6parsolve ptr, double number, SAM_error *err);

/**
 * Set Isc: Short circuit current [A]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_6parsolve_SixParameterSolver_Isc_nset(SAM_6parsolve ptr, double number, SAM_error *err);

/**
 * Set Nser: Number of cells in series
 * options: None
 * constraints: INTEGER,POSITIVE
 * required if: *
 */
SAM_EXPORT void SAM_6parsolve_SixParameterSolver_Nser_nset(SAM_6parsolve ptr, double number, SAM_error *err);

/**
 * Set Tref: Reference cell temperature ['C]
 * options: None
 * constraints: None
 * required if: ?
 */
SAM_EXPORT void SAM_6parsolve_SixParameterSolver_Tref_nset(SAM_6parsolve ptr, double number, SAM_error *err);

/**
 * Set Vmp: Maximum power point voltage [V]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_6parsolve_SixParameterSolver_Vmp_nset(SAM_6parsolve ptr, double number, SAM_error *err);

/**
 * Set Voc: Open circuit voltage [V]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_6parsolve_SixParameterSolver_Voc_nset(SAM_6parsolve ptr, double number, SAM_error *err);

/**
 * Set alpha_isc: Temp coeff of current at SC [A/'C]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_6parsolve_SixParameterSolver_alpha_isc_nset(SAM_6parsolve ptr, double number, SAM_error *err);

/**
 * Set beta_voc: Temp coeff of voltage at OC [V/'C]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_6parsolve_SixParameterSolver_beta_voc_nset(SAM_6parsolve ptr, double number, SAM_error *err);

/**
 * Set celltype: Cell technology type [monoSi,multiSi/polySi,cis,cigs,cdte,amorphous]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_6parsolve_SixParameterSolver_celltype_sset(SAM_6parsolve ptr, const char *str, SAM_error *err);

/**
 * Set gamma_pmp: Temp coeff of power at MP [%/'C]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_6parsolve_SixParameterSolver_gamma_pmp_nset(SAM_6parsolve ptr, double number, SAM_error *err);


/**
 * SixParameterSolver Getters
 */

SAM_EXPORT double SAM_6parsolve_SixParameterSolver_Imp_nget(SAM_6parsolve ptr, SAM_error *err);

SAM_EXPORT double SAM_6parsolve_SixParameterSolver_Isc_nget(SAM_6parsolve ptr, SAM_error *err);

SAM_EXPORT double SAM_6parsolve_SixParameterSolver_Nser_nget(SAM_6parsolve ptr, SAM_error *err);

SAM_EXPORT double SAM_6parsolve_SixParameterSolver_Tref_nget(SAM_6parsolve ptr, SAM_error *err);

SAM_EXPORT double SAM_6parsolve_SixParameterSolver_Vmp_nget(SAM_6parsolve ptr, SAM_error *err);

SAM_EXPORT double SAM_6parsolve_SixParameterSolver_Voc_nget(SAM_6parsolve ptr, SAM_error *err);

SAM_EXPORT double SAM_6parsolve_SixParameterSolver_alpha_isc_nget(SAM_6parsolve ptr, SAM_error *err);

SAM_EXPORT double SAM_6parsolve_SixParameterSolver_beta_voc_nget(SAM_6parsolve ptr, SAM_error *err);

SAM_EXPORT const char *SAM_6parsolve_SixParameterSolver_celltype_sget(SAM_6parsolve ptr, SAM_error *err);

SAM_EXPORT double SAM_6parsolve_SixParameterSolver_gamma_pmp_nget(SAM_6parsolve ptr, SAM_error *err);


/**
 * Outputs Getters
 */

SAM_EXPORT double SAM_6parsolve_Outputs_Adj_nget(SAM_6parsolve ptr, SAM_error *err);

SAM_EXPORT double SAM_6parsolve_Outputs_Il_nget(SAM_6parsolve ptr, SAM_error *err);

SAM_EXPORT double SAM_6parsolve_Outputs_Io_nget(SAM_6parsolve ptr, SAM_error *err);

SAM_EXPORT double SAM_6parsolve_Outputs_Rs_nget(SAM_6parsolve ptr, SAM_error *err);

SAM_EXPORT double SAM_6parsolve_Outputs_Rsh_nget(SAM_6parsolve ptr, SAM_error *err);

SAM_EXPORT double SAM_6parsolve_Outputs_a_nget(SAM_6parsolve ptr, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif
