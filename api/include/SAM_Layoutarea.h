#ifndef SAM_LAYOUTAREA_H_
#define SAM_LAYOUTAREA_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// Layoutarea Technology Model
	//

	/** 
	 * Create a Layoutarea variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_Layoutarea;

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_Layoutarea_execute(SAM_table data, int verbosity, SAM_error* err);


	//
	// Common parameters
	//

	/**
	 * Set positions: Positions within calculataed area
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Layoutarea_Common_positions_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);


	/**
	 * Common Getters
	 */

	SAM_EXPORT double* SAM_Layoutarea_Common_positions_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double SAM_Layoutarea_Outputs_area_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Layoutarea_Outputs_convex_hull_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif