#ifndef SAM_IEC61853INTERP_H_
#define SAM_IEC61853INTERP_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>

#ifdef __cplusplus
extern "C"
{
#endif

//
// Iec61853interp Technology Model
//

/**
 * Create a Iec61853interp variable table.
 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
 * @param[in,out] err: a pointer to an error object
 */

SAM_EXPORT typedef void *SAM_Iec61853interp;

SAM_EXPORT SAM_Iec61853interp SAM_Iec61853interp_construct(const char *def, SAM_error *err);

/// verbosity level 0 or 1. Returns 1 on success
SAM_EXPORT int SAM_Iec61853interp_execute(SAM_Iec61853interp data, int verbosity, SAM_error *err);

SAM_EXPORT void SAM_Iec61853interp_destruct(SAM_Iec61853interp system);


//
// IEC61853 parameters
//

/**
 * Set input: IEC-61853 matrix test data [various]
 * options: [IRR,TC,PMP,VMP,VOC,ISC]
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_Iec61853interp_IEC61853_input_mset(SAM_Iec61853interp ptr, double *mat, int nrows, int ncols, SAM_error *err);

/**
 * Set param: Parameter solution matrix
 * options: [IL,IO,RS,RSH,A]
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_Iec61853interp_IEC61853_param_mset(SAM_Iec61853interp ptr, double *mat, int nrows, int ncols, SAM_error *err);


//
// SingleDiodeModel parameters
//

/**
 * Set I: Irradiance [W/m2]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Iec61853interp_SingleDiodeModel_I_nset(SAM_Iec61853interp ptr, double number, SAM_error *err);

/**
 * Set T: Temperature [C]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Iec61853interp_SingleDiodeModel_T_nset(SAM_Iec61853interp ptr, double number, SAM_error *err);


/**
 * IEC61853 Getters
 */

SAM_EXPORT double *
SAM_Iec61853interp_IEC61853_input_mget(SAM_Iec61853interp ptr, int *nrows, int *ncols, SAM_error *err);

SAM_EXPORT double *
SAM_Iec61853interp_IEC61853_param_mget(SAM_Iec61853interp ptr, int *nrows, int *ncols, SAM_error *err);


/**
 * SingleDiodeModel Getters
 */

SAM_EXPORT double SAM_Iec61853interp_SingleDiodeModel_I_nget(SAM_Iec61853interp ptr, SAM_error *err);

SAM_EXPORT double SAM_Iec61853interp_SingleDiodeModel_T_nget(SAM_Iec61853interp ptr, SAM_error *err);


/**
 * Outputs Getters
 */

SAM_EXPORT double SAM_Iec61853interp_Outputs_Il_nget(SAM_Iec61853interp ptr, SAM_error *err);

SAM_EXPORT double SAM_Iec61853interp_Outputs_Io_nget(SAM_Iec61853interp ptr, SAM_error *err);

SAM_EXPORT double SAM_Iec61853interp_Outputs_Rs_nget(SAM_Iec61853interp ptr, SAM_error *err);

SAM_EXPORT double SAM_Iec61853interp_Outputs_Rsh_nget(SAM_Iec61853interp ptr, SAM_error *err);

SAM_EXPORT double SAM_Iec61853interp_Outputs_a_nget(SAM_Iec61853interp ptr, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif
