#ifndef SAM_TIMESEQ_H_
#define SAM_TIMESEQ_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>

#ifdef __cplusplus
extern "C"
{
#endif

//
// Timeseq Technology Model
//

/**
 * Create a Timeseq variable table.
 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
 * @param[in,out] err: a pointer to an error object
 */

SAM_EXPORT typedef void *SAM_Timeseq;

SAM_EXPORT SAM_Timeseq SAM_Timeseq_construct(const char *def, SAM_error *err);

/// verbosity level 0 or 1. Returns 1 on success
SAM_EXPORT int SAM_Timeseq_execute(SAM_Timeseq data, int verbosity, SAM_error *err);

SAM_EXPORT void SAM_Timeseq_destruct(SAM_Timeseq system);


//
// TimeSequence parameters
//

/**
 * Set end_time: End time [seconds]
 * options: 0=jan1st 12am
 * constraints: MIN=0,MAX=31536000
 * required if: *
 */
SAM_EXPORT void SAM_Timeseq_TimeSequence_end_time_nset(SAM_Timeseq ptr, double number, SAM_error *err);

/**
 * Set start_time: Start time [seconds]
 * options: 0=jan1st 12am
 * constraints: MIN=0,MAX=31536000
 * required if: *
 */
SAM_EXPORT void SAM_Timeseq_TimeSequence_start_time_nset(SAM_Timeseq ptr, double number, SAM_error *err);

/**
 * Set time_step: Time step [seconds]
 * options: None
 * constraints: MIN=1,MAX=3600
 * required if: *
 */
SAM_EXPORT void SAM_Timeseq_TimeSequence_time_step_nset(SAM_Timeseq ptr, double number, SAM_error *err);


/**
 * TimeSequence Getters
 */

SAM_EXPORT double SAM_Timeseq_TimeSequence_end_time_nget(SAM_Timeseq ptr, SAM_error *err);

SAM_EXPORT double SAM_Timeseq_TimeSequence_start_time_nget(SAM_Timeseq ptr, SAM_error *err);

SAM_EXPORT double SAM_Timeseq_TimeSequence_time_step_nget(SAM_Timeseq ptr, SAM_error *err);


/**
 * Outputs Getters
 */

SAM_EXPORT double *SAM_Timeseq_Outputs_day_aget(SAM_Timeseq ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Timeseq_Outputs_hour_aget(SAM_Timeseq ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Timeseq_Outputs_minute_aget(SAM_Timeseq ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Timeseq_Outputs_month_aget(SAM_Timeseq ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Timeseq_Outputs_time_aget(SAM_Timeseq ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Timeseq_Outputs_timehr_aget(SAM_Timeseq ptr, int *length, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif
