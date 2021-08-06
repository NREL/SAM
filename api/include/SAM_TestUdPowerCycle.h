#ifndef SAM_TESTUDPOWERCYCLE_H_
#define SAM_TESTUDPOWERCYCLE_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// TestUdPowerCycle Technology Model
	//

	/** 
	 * Create a TestUdPowerCycle variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_TestUdPowerCycle;

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_TestUdPowerCycle_execute(SAM_table data, int verbosity, SAM_error* err);


	//
	// Common parameters
	//

	/**
	 * Set q_pb_design: Design point power block thermal power [MWt]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TestUdPowerCycle_Common_q_pb_design_nset(SAM_table ptr, double number, SAM_error *err);


	/**
	 * Common Getters
	 */

	SAM_EXPORT double SAM_TestUdPowerCycle_Common_q_pb_design_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double SAM_TestUdPowerCycle_Outputs_W_dot_fossil_nget(SAM_table ptr, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif