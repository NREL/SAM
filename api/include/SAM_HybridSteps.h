#ifndef SAM_HYBRIDSTEPS_H_
#define SAM_HYBRIDSTEPS_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// HybridSteps Technology Model
	//

	/** 
	 * Create a HybridSteps variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_HybridSteps;

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_HybridSteps_execute(SAM_table data, int verbosity, SAM_error* err);


	//
	// Common parameters
	//

	/**
	 * Set input: input_table input for one technology
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_HybridSteps_Common_input_tset(SAM_table ptr, SAM_table tab, SAM_error *err);


	/**
	 * Common Getters
	 */

	SAM_EXPORT SAM_table SAM_HybridSteps_Common_input_tget(SAM_table ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT SAM_table SAM_HybridSteps_Outputs_output_tget(SAM_table ptr, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif