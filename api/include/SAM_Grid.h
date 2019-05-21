#ifndef SAM_GRID_H_
#define SAM_GRID_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// Grid Technology Model
	//

	/** 
	 * Create a Grid variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_Grid;

	SAM_EXPORT SAM_Grid SAM_Grid_construct(const char* def, SAM_error* err);

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_Grid_execute(SAM_Grid data, int verbosity, SAM_error* err);

	SAM_EXPORT void SAM_Grid_destruct(SAM_Grid system);


	/**
	 * Outputs Getters
	 */

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif