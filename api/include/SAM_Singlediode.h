#ifndef SAM_SINGLEDIODE_H_
#define SAM_SINGLEDIODE_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// Singlediode Technology Model
	//

	/** 
	 * Create a Singlediode variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_Singlediode;

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_Singlediode_execute(SAM_table data, int verbosity, SAM_error* err);


	//
	// SingleDiodeModel parameters
	//

	/**
	 * Set Il: Light current [A]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Singlediode_SingleDiodeModel_Il_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set Io: Saturation current [A]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Singlediode_SingleDiodeModel_Io_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set Rs: Series resistance [ohm]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Singlediode_SingleDiodeModel_Rs_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set Rsh: Shunt resistance [ohm]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Singlediode_SingleDiodeModel_Rsh_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set Vop: Module operating voltage [V]
	 * options: None
	 * constraints: None
	 * required if: ?
	 */
	SAM_EXPORT void SAM_Singlediode_SingleDiodeModel_Vop_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set a: Modified nonideality factor [1/V]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Singlediode_SingleDiodeModel_a_nset(SAM_table ptr, double number, SAM_error *err);


	/**
	 * SingleDiodeModel Getters
	 */

	SAM_EXPORT double SAM_Singlediode_SingleDiodeModel_Il_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Singlediode_SingleDiodeModel_Io_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Singlediode_SingleDiodeModel_Rs_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Singlediode_SingleDiodeModel_Rsh_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Singlediode_SingleDiodeModel_Vop_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Singlediode_SingleDiodeModel_a_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double SAM_Singlediode_Outputs_I_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Singlediode_Outputs_Isc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Singlediode_Outputs_V_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Singlediode_Outputs_Voc_nget(SAM_table ptr, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif