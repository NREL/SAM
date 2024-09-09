#ifndef SAM_HYBRID_H_
#define SAM_HYBRID_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// Hybrid Technology Model
	//

	/** 
	 * Create a Hybrid variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_Hybrid;

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_Hybrid_execute(SAM_table data, int verbosity, SAM_error* err);


	//
	// Common parameters
	//

	/**
	 * Set input: input_table for multiple technologies and one financial market
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Hybrid_Common_input_tset(SAM_table ptr, SAM_table tab, SAM_error *err);


	/**
	 * Common Getters
	 */

	SAM_EXPORT SAM_table SAM_Hybrid_Common_input_tget(SAM_table ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT SAM_table SAM_Hybrid_Outputs_output_tget(SAM_table ptr, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif