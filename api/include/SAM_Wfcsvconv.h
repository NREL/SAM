#ifndef SAM_WFCSVCONV_H_
#define SAM_WFCSVCONV_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>

#ifdef __cplusplus
extern "C"
{
#endif

//
// Wfcsvconv Technology Model
//

/**
 * Create a Wfcsvconv variable table.
 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
 * @param[in,out] err: a pointer to an error object
 */

SAM_EXPORT typedef void *SAM_Wfcsvconv;

SAM_EXPORT SAM_Wfcsvconv SAM_Wfcsvconv_construct(const char *def, SAM_error *err);

/// verbosity level 0 or 1. Returns 1 on success
SAM_EXPORT int SAM_Wfcsvconv_execute(SAM_Wfcsvconv data, int verbosity, SAM_error *err);

SAM_EXPORT void SAM_Wfcsvconv_destruct(SAM_Wfcsvconv system);


//
// WeatherFileConverter parameters
//

/**
 * Set input_file: Input weather file name
 * options: tmy2,tmy3,intl,epw,smw
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Wfcsvconv_WeatherFileConverter_input_file_sset(SAM_Wfcsvconv ptr, const char *str, SAM_error *err);

/**
 * Set output_file: Output file name
 * options: None
 * constraints: None
 * required if: ?
 */
SAM_EXPORT void SAM_Wfcsvconv_WeatherFileConverter_output_file_sset(SAM_Wfcsvconv ptr, const char *str, SAM_error *err);

/**
 * Set output_filename_format: Output file name format
 * options: recognizes $city $state $country $type $loc
 * constraints: None
 * required if: ?
 */
SAM_EXPORT void
SAM_Wfcsvconv_WeatherFileConverter_output_filename_format_sset(SAM_Wfcsvconv ptr, const char *str, SAM_error *err);

/**
 * Set output_folder: Output folder
 * options: None
 * constraints: None
 * required if: ?
 */
SAM_EXPORT void
SAM_Wfcsvconv_WeatherFileConverter_output_folder_sset(SAM_Wfcsvconv ptr, const char *str, SAM_error *err);


/**
 * WeatherFileConverter Getters
 */

SAM_EXPORT const char *SAM_Wfcsvconv_WeatherFileConverter_input_file_sget(SAM_Wfcsvconv ptr, SAM_error *err);

SAM_EXPORT const char *SAM_Wfcsvconv_WeatherFileConverter_output_file_sget(SAM_Wfcsvconv ptr, SAM_error *err);

SAM_EXPORT const char *
SAM_Wfcsvconv_WeatherFileConverter_output_filename_format_sget(SAM_Wfcsvconv ptr, SAM_error *err);

SAM_EXPORT const char *SAM_Wfcsvconv_WeatherFileConverter_output_folder_sget(SAM_Wfcsvconv ptr, SAM_error *err);


/**
 * Outputs Getters
 */

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif
