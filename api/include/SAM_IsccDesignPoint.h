#ifndef SAM_ISCCDESIGNPOINT_H_
#define SAM_ISCCDESIGNPOINT_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>

#ifdef __cplusplus
extern "C"
{
#endif

//
// IsccDesignPoint Technology Model
//

/**
 * Create a IsccDesignPoint variable table.
 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
 * @param[in,out] err: a pointer to an error object
 */

SAM_EXPORT typedef void *SAM_IsccDesignPoint;

SAM_EXPORT SAM_IsccDesignPoint SAM_IsccDesignPoint_construct(const char *def, SAM_error *err);

/// verbosity level 0 or 1. Returns 1 on success
SAM_EXPORT int SAM_IsccDesignPoint_execute(SAM_IsccDesignPoint data, int verbosity, SAM_error *err);

SAM_EXPORT void SAM_IsccDesignPoint_destruct(SAM_IsccDesignPoint system);


//
// Common parameters
//

/**
 * Set HTF_code: HTF fluid code [-]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_IsccDesignPoint_Common_HTF_code_nset(SAM_IsccDesignPoint ptr, double number, SAM_error *err);

/**
 * Set elev: Plant elevation [m]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_IsccDesignPoint_Common_elev_nset(SAM_IsccDesignPoint ptr, double number, SAM_error *err);

/**
 * Set field_fl_props: User defined field fluid property data [-]
 * options: 7 columns (T,Cp,dens,visc,kvisc,cond,h), at least 3 rows
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_IsccDesignPoint_Common_field_fl_props_mset(SAM_IsccDesignPoint ptr, double *mat, int nrows, int ncols,
                                               SAM_error *err);

/**
 * Set ngcc_model: 1: NREL, 2: GE
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_IsccDesignPoint_Common_ngcc_model_nset(SAM_IsccDesignPoint ptr, double number, SAM_error *err);

/**
 * Set pinch_point_cold: Cold side pinch point [C]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_IsccDesignPoint_Common_pinch_point_cold_nset(SAM_IsccDesignPoint ptr, double number, SAM_error *err);

/**
 * Set pinch_point_hot: Hot side pinch point [C]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_IsccDesignPoint_Common_pinch_point_hot_nset(SAM_IsccDesignPoint ptr, double number, SAM_error *err);

/**
 * Set q_pb_design: Design point power block thermal power [MWt]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_IsccDesignPoint_Common_q_pb_design_nset(SAM_IsccDesignPoint ptr, double number, SAM_error *err);


/**
 * Common Getters
 */

SAM_EXPORT double SAM_IsccDesignPoint_Common_HTF_code_nget(SAM_IsccDesignPoint ptr, SAM_error *err);

SAM_EXPORT double SAM_IsccDesignPoint_Common_elev_nget(SAM_IsccDesignPoint ptr, SAM_error *err);

SAM_EXPORT double *
SAM_IsccDesignPoint_Common_field_fl_props_mget(SAM_IsccDesignPoint ptr, int *nrows, int *ncols, SAM_error *err);

SAM_EXPORT double SAM_IsccDesignPoint_Common_ngcc_model_nget(SAM_IsccDesignPoint ptr, SAM_error *err);

SAM_EXPORT double SAM_IsccDesignPoint_Common_pinch_point_cold_nget(SAM_IsccDesignPoint ptr, SAM_error *err);

SAM_EXPORT double SAM_IsccDesignPoint_Common_pinch_point_hot_nget(SAM_IsccDesignPoint ptr, SAM_error *err);

SAM_EXPORT double SAM_IsccDesignPoint_Common_q_pb_design_nget(SAM_IsccDesignPoint ptr, SAM_error *err);


/**
 * Outputs Getters
 */

SAM_EXPORT double SAM_IsccDesignPoint_Outputs_T_htf_cold_nget(SAM_IsccDesignPoint ptr, SAM_error *err);

SAM_EXPORT double SAM_IsccDesignPoint_Outputs_T_st_inject_nget(SAM_IsccDesignPoint ptr, SAM_error *err);

SAM_EXPORT double SAM_IsccDesignPoint_Outputs_W_dot_fossil_nget(SAM_IsccDesignPoint ptr, SAM_error *err);

SAM_EXPORT double SAM_IsccDesignPoint_Outputs_W_dot_solar_nget(SAM_IsccDesignPoint ptr, SAM_error *err);

SAM_EXPORT double SAM_IsccDesignPoint_Outputs_q_solar_max_nget(SAM_IsccDesignPoint ptr, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif
