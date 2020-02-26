
#ifndef SAM_ADJUSTMENTFACTORS_H
#define SAM_ADJUSTMENTFACTORS_H

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>

#ifdef __cplusplus
extern "C"
{
#endif

//
// Adjustment Factors for All Technology Models
//

/**
 * Set adjust:constant
 * options: None
 * constraints: None
 * required if: None
 */
SAM_EXPORT void SAM_AdjustmentFactors_adjust_constant_nset(void *ptr, double num, SAM_error *err);

/**
 * Set adjust:hourly
 * options: None
 * constraints: None
 * required if: None
 */
SAM_EXPORT void SAM_AdjustmentFactors_adjust_hourly_aset(void *ptr, double *arr, int length, SAM_error *err);

/**
 * Set adjust:periods
 * options: None
 * constraints: None
 * required if: None
 */
SAM_EXPORT void SAM_AdjustmentFactors_adjust_periods_mset(void *ptr, double *mat, int nrows, int ncols, SAM_error *err);

//
// DC Adjustment Factors for All Technology Models
//

/**
 * Set dc_adjust:constant
 * options: None
 * constraints: None
 * required if: None
 */
SAM_EXPORT void SAM_AdjustmentFactors_dc_adjust_constant_nset(void *ptr, double num, SAM_error *err);

/**
 * Set dc_adjust:hourly
 * options: None
 * constraints: None
 * required if: None
 */
SAM_EXPORT void SAM_AdjustmentFactors_dc_adjust_hourly_aset(void *ptr, double *arr, int length, SAM_error *err);

/**
 * Set dc_adjust:periods
 * options: None
 * constraints: None
 * required if: None
 */
SAM_EXPORT void
SAM_AdjustmentFactors_dc_adjust_periods_mset(void *ptr, double *mat, int nrows, int ncols, SAM_error *err);

//
// SF Adjustment Factors for All Technology Models
//

/**
 * Set sf_adjust:constant
 * options: None
 * constraints: None
 * required if: None
 */
SAM_EXPORT void SAM_AdjustmentFactors_sf_adjust_constant_nset(void *ptr, double num, SAM_error *err);

/**
 * Set sf_adjust:hourly
 * options: None
 * constraints: None
 * required if: None
 */
SAM_EXPORT void SAM_AdjustmentFactors_sf_adjust_hourly_aset(void *ptr, double *arr, int length, SAM_error *err);

/**
 * Set sf_adjust:periods
 * options: None
 * constraints: None
 * required if: None
 */
SAM_EXPORT void
SAM_AdjustmentFactors_sf_adjust_periods_mset(void *ptr, double *mat, int nrows, int ncols, SAM_error *err);

/**
 * Getters
 */

SAM_EXPORT double SAM_AdjustmentFactors_adjust_constant_nget(void *ptr, SAM_error *err);

SAM_EXPORT double *SAM_AdjustmentFactors_adjust_hourly_aget(void *ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_AdjustmentFactors_adjust_periods_mget(void *ptr, int *nrows, int *ncols, SAM_error *err);

SAM_EXPORT double SAM_AdjustmentFactors_dc_adjust_constant_nget(void *ptr, SAM_error *err);

SAM_EXPORT double *SAM_AdjustmentFactors_dc_adjust_hourly_aget(void *ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_AdjustmentFactors_dc_adjust_periods_mget(void *ptr, int *nrows, int *ncols, SAM_error *err);

SAM_EXPORT double SAM_AdjustmentFactors_sf_adjust_constant_nget(void *ptr, SAM_error *err);

SAM_EXPORT double *SAM_AdjustmentFactors_sf_adjust_hourly_aget(void *ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_AdjustmentFactors_sf_adjust_periods_mget(void *ptr, int *nrows, int *ncols, SAM_error *err);


#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif
