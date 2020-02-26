#ifndef SAM_SINGLEDIODEPARAMS_H_
#define SAM_SINGLEDIODEPARAMS_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>

#ifdef __cplusplus
extern "C"
{
#endif

//
// Singlediodeparams Technology Model
//

/**
 * Create a Singlediodeparams variable table.
 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
 * @param[in,out] err: a pointer to an error object
 */

SAM_EXPORT typedef void *SAM_Singlediodeparams;

SAM_EXPORT SAM_Singlediodeparams SAM_Singlediodeparams_construct(const char *def, SAM_error *err);

/// verbosity level 0 or 1. Returns 1 on success
SAM_EXPORT int SAM_Singlediodeparams_execute(SAM_Singlediodeparams data, int verbosity, SAM_error *err);

SAM_EXPORT void SAM_Singlediodeparams_destruct(SAM_Singlediodeparams system);


//
// SingleDiodeModel parameters
//

/**
 * Set Adj_ref: OC SC temp coeff adjustment [%]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_Singlediodeparams_SingleDiodeModel_Adj_ref_nset(SAM_Singlediodeparams ptr, double number, SAM_error *err);

/**
 * Set I: Irradiance [W/m2]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Singlediodeparams_SingleDiodeModel_I_nset(SAM_Singlediodeparams ptr, double number, SAM_error *err);

/**
 * Set Il_ref: Light current [A]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_Singlediodeparams_SingleDiodeModel_Il_ref_nset(SAM_Singlediodeparams ptr, double number, SAM_error *err);

/**
 * Set Io_ref: Saturation current [A]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_Singlediodeparams_SingleDiodeModel_Io_ref_nset(SAM_Singlediodeparams ptr, double number, SAM_error *err);

/**
 * Set Rs_ref: Series resistance [ohm]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_Singlediodeparams_SingleDiodeModel_Rs_ref_nset(SAM_Singlediodeparams ptr, double number, SAM_error *err);

/**
 * Set Rsh_ref: Shunt resistance [ohm]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_Singlediodeparams_SingleDiodeModel_Rsh_ref_nset(SAM_Singlediodeparams ptr, double number, SAM_error *err);

/**
 * Set T: Temperature [C]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Singlediodeparams_SingleDiodeModel_T_nset(SAM_Singlediodeparams ptr, double number, SAM_error *err);

/**
 * Set a_ref: Modified nonideality factor [1/V]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_Singlediodeparams_SingleDiodeModel_a_ref_nset(SAM_Singlediodeparams ptr, double number, SAM_error *err);

/**
 * Set alpha_isc: Temp coeff of current at SC [A/'C]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_Singlediodeparams_SingleDiodeModel_alpha_isc_nset(SAM_Singlediodeparams ptr, double number, SAM_error *err);


/**
 * SingleDiodeModel Getters
 */

SAM_EXPORT double SAM_Singlediodeparams_SingleDiodeModel_Adj_ref_nget(SAM_Singlediodeparams ptr, SAM_error *err);

SAM_EXPORT double SAM_Singlediodeparams_SingleDiodeModel_I_nget(SAM_Singlediodeparams ptr, SAM_error *err);

SAM_EXPORT double SAM_Singlediodeparams_SingleDiodeModel_Il_ref_nget(SAM_Singlediodeparams ptr, SAM_error *err);

SAM_EXPORT double SAM_Singlediodeparams_SingleDiodeModel_Io_ref_nget(SAM_Singlediodeparams ptr, SAM_error *err);

SAM_EXPORT double SAM_Singlediodeparams_SingleDiodeModel_Rs_ref_nget(SAM_Singlediodeparams ptr, SAM_error *err);

SAM_EXPORT double SAM_Singlediodeparams_SingleDiodeModel_Rsh_ref_nget(SAM_Singlediodeparams ptr, SAM_error *err);

SAM_EXPORT double SAM_Singlediodeparams_SingleDiodeModel_T_nget(SAM_Singlediodeparams ptr, SAM_error *err);

SAM_EXPORT double SAM_Singlediodeparams_SingleDiodeModel_a_ref_nget(SAM_Singlediodeparams ptr, SAM_error *err);

SAM_EXPORT double SAM_Singlediodeparams_SingleDiodeModel_alpha_isc_nget(SAM_Singlediodeparams ptr, SAM_error *err);


/**
 * Outputs Getters
 */

SAM_EXPORT double SAM_Singlediodeparams_Outputs_Il_nget(SAM_Singlediodeparams ptr, SAM_error *err);

SAM_EXPORT double SAM_Singlediodeparams_Outputs_Io_nget(SAM_Singlediodeparams ptr, SAM_error *err);

SAM_EXPORT double SAM_Singlediodeparams_Outputs_Rs_nget(SAM_Singlediodeparams ptr, SAM_error *err);

SAM_EXPORT double SAM_Singlediodeparams_Outputs_Rsh_nget(SAM_Singlediodeparams ptr, SAM_error *err);

SAM_EXPORT double SAM_Singlediodeparams_Outputs_a_nget(SAM_Singlediodeparams ptr, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif
