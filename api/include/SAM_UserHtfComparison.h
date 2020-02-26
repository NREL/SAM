#ifndef SAM_USERHTFCOMPARISON_H_
#define SAM_USERHTFCOMPARISON_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>

#ifdef __cplusplus
extern "C"
{
#endif

//
// UserHtfComparison Technology Model
//

/**
 * Create a UserHtfComparison variable table.
 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
 * @param[in,out] err: a pointer to an error object
 */

SAM_EXPORT typedef void *SAM_UserHtfComparison;

SAM_EXPORT SAM_UserHtfComparison SAM_UserHtfComparison_construct(const char *def, SAM_error *err);

/// verbosity level 0 or 1. Returns 1 on success
SAM_EXPORT int SAM_UserHtfComparison_execute(SAM_UserHtfComparison data, int verbosity, SAM_error *err);

SAM_EXPORT void SAM_UserHtfComparison_destruct(SAM_UserHtfComparison system);


//
// Common parameters
//

/**
 * Set HTF_code1: HTF fluid code: Fluid 1 [-]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_UserHtfComparison_Common_HTF_code1_nset(SAM_UserHtfComparison ptr, double number, SAM_error *err);

/**
 * Set HTF_code2: HTF fluid code: Fluid 2 [-]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_UserHtfComparison_Common_HTF_code2_nset(SAM_UserHtfComparison ptr, double number, SAM_error *err);

/**
 * Set fl_props1: User defined field fluid property data, Fluid 1 [-]
 * options: 7 columns (T,Cp,dens,visc,kvisc,cond,h), at least 3 rows
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_UserHtfComparison_Common_fl_props1_mset(SAM_UserHtfComparison ptr, double *mat, int nrows, int ncols,
                                            SAM_error *err);

/**
 * Set fl_props2: User defined field fluid property data, Fluid 2 [-]
 * options: 7 columns (T,Cp,dens,visc,kvisc,cond,h), at least 3 rows
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_UserHtfComparison_Common_fl_props2_mset(SAM_UserHtfComparison ptr, double *mat, int nrows, int ncols,
                                            SAM_error *err);


/**
 * Common Getters
 */

SAM_EXPORT double SAM_UserHtfComparison_Common_HTF_code1_nget(SAM_UserHtfComparison ptr, SAM_error *err);

SAM_EXPORT double SAM_UserHtfComparison_Common_HTF_code2_nget(SAM_UserHtfComparison ptr, SAM_error *err);

SAM_EXPORT double *
SAM_UserHtfComparison_Common_fl_props1_mget(SAM_UserHtfComparison ptr, int *nrows, int *ncols, SAM_error *err);

SAM_EXPORT double *
SAM_UserHtfComparison_Common_fl_props2_mget(SAM_UserHtfComparison ptr, int *nrows, int *ncols, SAM_error *err);


/**
 * Outputs Getters
 */

SAM_EXPORT double SAM_UserHtfComparison_Outputs_are_equal_nget(SAM_UserHtfComparison ptr, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif
