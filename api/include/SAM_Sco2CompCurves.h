#ifndef SAM_SCO2COMPCURVES_H_
#define SAM_SCO2COMPCURVES_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// Sco2CompCurves Technology Model
	//

	/** 
	 * Create a Sco2CompCurves variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_Sco2CompCurves;

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_Sco2CompCurves_execute(SAM_table data, int verbosity, SAM_error* err);


	//
	// Common parameters
	//

	/**
	 * Set P_comp_in: Compressor inlet pressure [MPa]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Sco2CompCurves_Common_P_comp_in_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set T_comp_in: Compressor inlet temperature [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Sco2CompCurves_Common_T_comp_in_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set comp_type: Integer corresponding to compressor model [-]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Sco2CompCurves_Common_comp_type_nset(SAM_table ptr, double number, SAM_error *err);


	/**
	 * Common Getters
	 */

	SAM_EXPORT double SAM_Sco2CompCurves_Common_P_comp_in_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CompCurves_Common_T_comp_in_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CompCurves_Common_comp_type_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double* SAM_Sco2CompCurves_Outputs_eta_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CompCurves_Outputs_eta_ND_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CompCurves_Outputs_eta_norm_design_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CompCurves_Outputs_phi_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CompCurves_Outputs_phi_ND_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CompCurves_Outputs_phi_design_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CompCurves_Outputs_psi_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Sco2CompCurves_Outputs_psi_ND_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Sco2CompCurves_Outputs_psi_design_nget(SAM_table ptr, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif