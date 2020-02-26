#ifndef SAM_WFCHECK_H_
#define SAM_WFCHECK_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>

#ifdef __cplusplus
extern "C"
{
#endif

//
// Wfcheck Technology Model
//

/**
 * Create a Wfcheck variable table.
 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
 * @param[in,out] err: a pointer to an error object
 */

SAM_EXPORT typedef void *SAM_Wfcheck;

SAM_EXPORT SAM_Wfcheck SAM_Wfcheck_construct(const char *def, SAM_error *err);

/// verbosity level 0 or 1. Returns 1 on success
SAM_EXPORT int SAM_Wfcheck_execute(SAM_Wfcheck data, int verbosity, SAM_error *err);

SAM_EXPORT void SAM_Wfcheck_destruct(SAM_Wfcheck system);


//
// WeatherFileChecker parameters
//

/**
 * Set input_file: Input weather file name
 * options: wfcsv format
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Wfcheck_WeatherFileChecker_input_file_sset(SAM_Wfcheck ptr, const char *str, SAM_error *err);


/**
 * WeatherFileChecker Getters
 */

SAM_EXPORT const char *SAM_Wfcheck_WeatherFileChecker_input_file_sget(SAM_Wfcheck ptr, SAM_error *err);


/**
 * Outputs Getters
 */

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif
