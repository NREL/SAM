#ifndef SAM_CSPDSGLFUI_H_
#define SAM_CSPDSGLFUI_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// CspDsgLfUi Technology Model
	//

	/** 
	 * Create a CspDsgLfUi variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_CspDsgLfUi;

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_CspDsgLfUi_execute(SAM_table data, int verbosity, SAM_error* err);


	//
	// Common parameters
	//

	/**
	 * Set P_boil: Boiling pressure [bar]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CspDsgLfUi_Common_P_boil_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set deltaT_subcooled: Subcooled temperature difference from saturation temp [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CspDsgLfUi_Common_deltaT_subcooled_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set use_quality_or_subcooled: 0 = 2 phase outlet, 1 = subcooled
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CspDsgLfUi_Common_use_quality_or_subcooled_nset(SAM_table ptr, double number, SAM_error *err);


	/**
	 * Common Getters
	 */

	SAM_EXPORT double SAM_CspDsgLfUi_Common_P_boil_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_CspDsgLfUi_Common_deltaT_subcooled_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_CspDsgLfUi_Common_use_quality_or_subcooled_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double SAM_CspDsgLfUi_Outputs_T_hot_out_target_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_CspDsgLfUi_Outputs_T_saturation_nget(SAM_table ptr, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif