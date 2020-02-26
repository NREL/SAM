#ifndef SAM_IEC61853PAR_H_
#define SAM_IEC61853PAR_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>

#ifdef __cplusplus
extern "C"
{
#endif

//
// Iec61853par Technology Model
//

/**
 * Create a Iec61853par variable table.
 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
 * @param[in,out] err: a pointer to an error object
 */

SAM_EXPORT typedef void *SAM_Iec61853par;

SAM_EXPORT SAM_Iec61853par SAM_Iec61853par_construct(const char *def, SAM_error *err);

/// verbosity level 0 or 1. Returns 1 on success
SAM_EXPORT int SAM_Iec61853par_execute(SAM_Iec61853par data, int verbosity, SAM_error *err);

SAM_EXPORT void SAM_Iec61853par_destruct(SAM_Iec61853par system);


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
SAM_Iec61853par_IEC61853_input_mset(SAM_Iec61853par ptr, double *mat, int nrows, int ncols, SAM_error *err);

/**
 * Set nser: Number of cells in series
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Iec61853par_IEC61853_nser_nset(SAM_Iec61853par ptr, double number, SAM_error *err);

/**
 * Set type: Cell technology type [0..5]
 * options: monoSi,multiSi/polySi,cdte,cis,cigs,amorphous
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Iec61853par_IEC61853_type_nset(SAM_Iec61853par ptr, double number, SAM_error *err);

/**
 * Set verbose: Output solver messages [0/1]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Iec61853par_IEC61853_verbose_nset(SAM_Iec61853par ptr, double number, SAM_error *err);


/**
 * IEC61853 Getters
 */

SAM_EXPORT double *SAM_Iec61853par_IEC61853_input_mget(SAM_Iec61853par ptr, int *nrows, int *ncols, SAM_error *err);

SAM_EXPORT double SAM_Iec61853par_IEC61853_nser_nget(SAM_Iec61853par ptr, SAM_error *err);

SAM_EXPORT double SAM_Iec61853par_IEC61853_type_nget(SAM_Iec61853par ptr, SAM_error *err);

SAM_EXPORT double SAM_Iec61853par_IEC61853_verbose_nget(SAM_Iec61853par ptr, SAM_error *err);


/**
 * Outputs Getters
 */

SAM_EXPORT double SAM_Iec61853par_Outputs_C1_nget(SAM_Iec61853par ptr, SAM_error *err);

SAM_EXPORT double SAM_Iec61853par_Outputs_C2_nget(SAM_Iec61853par ptr, SAM_error *err);

SAM_EXPORT double SAM_Iec61853par_Outputs_C3_nget(SAM_Iec61853par ptr, SAM_error *err);

SAM_EXPORT double SAM_Iec61853par_Outputs_D1_nget(SAM_Iec61853par ptr, SAM_error *err);

SAM_EXPORT double SAM_Iec61853par_Outputs_D2_nget(SAM_Iec61853par ptr, SAM_error *err);

SAM_EXPORT double SAM_Iec61853par_Outputs_D3_nget(SAM_Iec61853par ptr, SAM_error *err);

SAM_EXPORT double SAM_Iec61853par_Outputs_Egref_nget(SAM_Iec61853par ptr, SAM_error *err);

SAM_EXPORT double SAM_Iec61853par_Outputs_Il_nget(SAM_Iec61853par ptr, SAM_error *err);

SAM_EXPORT double SAM_Iec61853par_Outputs_Io_nget(SAM_Iec61853par ptr, SAM_error *err);

SAM_EXPORT double SAM_Iec61853par_Outputs_alphaIsc_nget(SAM_Iec61853par ptr, SAM_error *err);

SAM_EXPORT double SAM_Iec61853par_Outputs_betaVoc_nget(SAM_Iec61853par ptr, SAM_error *err);

SAM_EXPORT double SAM_Iec61853par_Outputs_gammaPmp_nget(SAM_Iec61853par ptr, SAM_error *err);

SAM_EXPORT double SAM_Iec61853par_Outputs_n_nget(SAM_Iec61853par ptr, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif
