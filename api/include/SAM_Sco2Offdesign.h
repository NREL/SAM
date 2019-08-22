#ifndef SAM_SCO2OFFDESIGN_H_
#define SAM_SCO2OFFDESIGN_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// Sco2Offdesign Technology Model
	//

	/** 
	 * Create a Sco2Offdesign variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_Sco2Offdesign;

	SAM_EXPORT SAM_Sco2Offdesign SAM_Sco2Offdesign_construct(const char* def, SAM_error* err);

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_Sco2Offdesign_execute(SAM_Sco2Offdesign data, int verbosity, SAM_error* err);

	SAM_EXPORT void SAM_Sco2Offdesign_destruct(SAM_Sco2Offdesign system);


	/**
	 * Outputs Getters
	 */

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif