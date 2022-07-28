#ifndef SAM_INVCECCG_H_
#define SAM_INVCECCG_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// InvCecCg Technology Model
	//

	/** 
	 * Create a InvCecCg variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_InvCecCg;

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_InvCecCg_execute(SAM_table data, int verbosity, SAM_error* err);


	//
	// Common parameters
	//

	/**
	 * Set inv_cec_cg_paco: Rated max output [W]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_InvCecCg_Common_inv_cec_cg_paco_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set inv_cec_cg_sample_power_units: Sample data units for power output [0=W,1=kW]
	 * options: None
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_InvCecCg_Common_inv_cec_cg_sample_power_units_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set inv_cec_cg_test_samples: Sample data
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_InvCecCg_Common_inv_cec_cg_test_samples_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);


	/**
	 * Common Getters
	 */

	SAM_EXPORT double SAM_InvCecCg_Common_inv_cec_cg_paco_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_InvCecCg_Common_inv_cec_cg_sample_power_units_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_InvCecCg_Common_inv_cec_cg_test_samples_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double SAM_InvCecCg_Outputs_Pdco_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_InvCecCg_Outputs_Pso_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_InvCecCg_Outputs_Vdco_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_InvCecCg_Outputs_c0_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_InvCecCg_Outputs_c1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_InvCecCg_Outputs_c2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_InvCecCg_Outputs_c3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_InvCecCg_Outputs_inv_cec_cg_C0_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_InvCecCg_Outputs_inv_cec_cg_C1_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_InvCecCg_Outputs_inv_cec_cg_C2_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_InvCecCg_Outputs_inv_cec_cg_C3_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_InvCecCg_Outputs_inv_cec_cg_Pdco_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_InvCecCg_Outputs_inv_cec_cg_Psco_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_InvCecCg_Outputs_inv_cec_cg_Vdc_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_InvCecCg_Outputs_inv_cec_cg_Vdc_Vnom_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_InvCecCg_Outputs_inv_cec_cg_Vmax_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_InvCecCg_Outputs_inv_cec_cg_Vmax_abc_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_InvCecCg_Outputs_inv_cec_cg_Vmin_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_InvCecCg_Outputs_inv_cec_cg_Vmin_abc_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_InvCecCg_Outputs_inv_cec_cg_Vnom_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_InvCecCg_Outputs_inv_cec_cg_Vnom_abc_aget(SAM_table ptr, int* length, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif