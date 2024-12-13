/*
BSD 3-Clause License

Copyright (c) Alliance for Sustainable Energy, LLC. See also https://github.com/NREL/SAM/blob/develop/LICENSE
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its
   contributors may be used to endorse or promote products derived from
   this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/


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
 * Set adjust_constant
 * options: None
 * constraints: None
 * required if: None
 */
SAM_EXPORT void SAM_AdjustmentFactors_adjust_constant_nset(void* ptr, double num, SAM_error *err);

/**
 * Set adjust_en_hourly
 * options: None
 * constraints: None
 * required if: None
 */
SAM_EXPORT void SAM_AdjustmentFactors_adjust_en_hourly_nset(void* ptr, double num, SAM_error *err);

/**
 * Set adjust_en_periods
 * options: None
 * constraints: None
 * required if: None
 */
SAM_EXPORT void SAM_AdjustmentFactors_adjust_en_periods_nset(void* ptr, double num, SAM_error *err);

/**
 * Set adjust_en_timeindex
 * options: None
 * constraints: None
 * required if: None
 */
SAM_EXPORT void SAM_AdjustmentFactors_adjust_en_timeindex_nset(void* ptr, double num, SAM_error *err);

/**
 * Set adjust_hourly
 * options: None
 * constraints: None
 * required if: adjust_en_hourly is True
 */
SAM_EXPORT void SAM_AdjustmentFactors_adjust_hourly_aset(void* ptr, double* arr, int length, SAM_error *err);

/**
 * Set adjust_periods
 * options: None
 * constraints: None
 * required if: adjust_en_periods is True
 */
SAM_EXPORT void SAM_AdjustmentFactors_adjust_periods_mset(void* ptr, double* mat, int nrows, int ncols, SAM_error *err);

/**
 * Set adjust_timeindex
 * options: None
 * constraints: None
 * required if: adjust_en_timeindex is True
 */
SAM_EXPORT void SAM_AdjustmentFactors_adjust_timeindex_aset(void* ptr, double* arr, int length, SAM_error *err);

//
// DC Adjustment Factors for All Technology Models
//

/**
 * Set dc_adjust_constant
 * options: None
 * constraints: None
 * required if: None
 */
SAM_EXPORT void SAM_AdjustmentFactors_dc_adjust_constant_nset(void* ptr, double num, SAM_error *err);

/**
 * Set dc_adjust_en_hourly
 * options: None
 * constraints: None
 * required if: None
 */
SAM_EXPORT void SAM_AdjustmentFactors_dc_adjust_en_hourly_nset(void* ptr, double num, SAM_error *err);

/**
 * Set dc_adjust_en_periods
 * options: None
 * constraints: None
 * required if: None
 */
SAM_EXPORT void SAM_AdjustmentFactors_dc_adjust_en_periods_nset(void* ptr, double num, SAM_error *err);

/**
 * Set dc_adjust_en_timeindex
 * options: None
 * constraints: None
 * required if: None
 */
SAM_EXPORT void SAM_AdjustmentFactors_dc_adjust_en_timeindex_nset(void* ptr, double num, SAM_error *err);

/**
 * Set dc_adjust_hourly
 * options: None
 * constraints: None
 * required if: dc_adjust_en_hourly is True
 */
SAM_EXPORT void SAM_AdjustmentFactors_dc_adjust_hourly_aset(void* ptr, double* arr, int length, SAM_error *err);

/**
 * Set dc_adjust_periods
 * options: None
 * constraints: None
 * required if: dc_adjust_en_periods is True
 */
SAM_EXPORT void SAM_AdjustmentFactors_dc_adjust_periods_mset(void* ptr, double* mat, int nrows, int ncols, SAM_error *err);

/**
 * Set dc_adjust_timeindex
 * options: None
 * constraints: None
 * required if: dc_adjust_en_timeindex is True
 */
SAM_EXPORT void SAM_AdjustmentFactors_dc_adjust_timeindex_aset(void* ptr, double* arr, int length, SAM_error *err);

//
// SF Adjustment Factors for All Technology Models
//

/**
 * Set sf_adjust_constant
 * options: None
 * constraints: None
 * required if: None
 */
SAM_EXPORT void SAM_AdjustmentFactors_sf_adjust_constant_nset(void* ptr, double num, SAM_error *err);

/**
 * Set sf_adjust_en_hourly
 * options: None
 * constraints: None
 * required if: None
 */
SAM_EXPORT void SAM_AdjustmentFactors_sf_adjust_en_hourly_nset(void* ptr, double num, SAM_error *err);

/**
 * Set sf_adjust_en_periods
 * options: None
 * constraints: None
 * required if: None
 */
SAM_EXPORT void SAM_AdjustmentFactors_sf_adjust_en_periods_nset(void* ptr, double num, SAM_error *err);

/**
 * Set sf_adjust_en_index
 * options: None
 * constraints: None
 * required if: None
 */
SAM_EXPORT void SAM_AdjustmentFactors_sf_adjust_en_timeindex_nset(void* ptr, double num, SAM_error *err);

/**
 * Set sf_adjust_hourly
 * options: None
 * constraints: None
 * required if: sf_adjust_en_hourly is True
 */
SAM_EXPORT void SAM_AdjustmentFactors_sf_adjust_hourly_aset(void* ptr, double* arr, int length, SAM_error *err);

/**
 * Set sf_adjust_periods
 * options: None
 * constraints: None
 * required if: sf_adjust_en_periods is True
 */
SAM_EXPORT void SAM_AdjustmentFactors_sf_adjust_periods_mset(void* ptr, double* mat, int nrows, int ncols, SAM_error *err);

/**
 * Set sf_adjust_timeindex
 * options: None
 * constraints: None
 * required if: sf_adjust_en_timeindex is True
 */
SAM_EXPORT void SAM_AdjustmentFactors_sf_adjust_timeindex_aset(void* ptr, double* arr, int length, SAM_error *err);

/**
 * Getters
 */

SAM_EXPORT double SAM_AdjustmentFactors_adjust_constant_nget(void* ptr, SAM_error *err);

SAM_EXPORT double SAM_AdjustmentFactors_adjust_en_hourly_nget(void* ptr, SAM_error *err);

SAM_EXPORT double SAM_AdjustmentFactors_adjust_en_periods_nget(void* ptr, SAM_error *err);

SAM_EXPORT double SAM_AdjustmentFactors_adjust_en_timeindex_nget(void* ptr, SAM_error *err);

SAM_EXPORT double SAM_AdjustmentFactors_adjust_constant_nget(void* ptr, SAM_error *err);

SAM_EXPORT double* SAM_AdjustmentFactors_adjust_hourly_aget(void* ptr, int* length, SAM_error *err);

SAM_EXPORT double* SAM_AdjustmentFactors_adjust_periods_mget(void *ptr, int *nrows, int *ncols, SAM_error *err);

SAM_EXPORT double* SAM_AdjustmentFactors_adjust_timeindex_aget(void* ptr, int* length, SAM_error *err);


SAM_EXPORT double SAM_AdjustmentFactors_dc_adjust_constant_nget(void* ptr, SAM_error *err);

SAM_EXPORT double SAM_AdjustmentFactors_dc_adjust_en_hourly_nget(void* ptr, SAM_error *err);

SAM_EXPORT double SAM_AdjustmentFactors_dc_adjust_en_periods_nget(void* ptr, SAM_error *err);

SAM_EXPORT double SAM_AdjustmentFactors_dc_adjust_en_timeindex_nget(void* ptr, SAM_error *err);

SAM_EXPORT double* SAM_AdjustmentFactors_dc_adjust_hourly_aget(void* ptr, int* length, SAM_error *err);

SAM_EXPORT double* SAM_AdjustmentFactors_dc_adjust_periods_mget(void *ptr, int *nrows, int *ncols, SAM_error *err);

SAM_EXPORT double* SAM_AdjustmentFactors_dc_adjust_timeindex_aget(void* ptr, int* length, SAM_error *err);


SAM_EXPORT double SAM_AdjustmentFactors_sf_adjust_constant_nget(void* ptr, SAM_error *err);

SAM_EXPORT double SAM_AdjustmentFactors_sf_adjust_en_hourly_nget(void* ptr, SAM_error *err);

SAM_EXPORT double SAM_AdjustmentFactors_sf_adjust_en_periods_nget(void* ptr, SAM_error *err);

SAM_EXPORT double SAM_AdjustmentFactors_sf_adjust_en_timeindex_nget(void* ptr, SAM_error *err);

SAM_EXPORT double* SAM_AdjustmentFactors_sf_adjust_hourly_aget(void* ptr, int* length, SAM_error *err);

SAM_EXPORT double* SAM_AdjustmentFactors_sf_adjust_periods_mget(void *ptr, int *nrows, int *ncols, SAM_error *err);

SAM_EXPORT double* SAM_AdjustmentFactors_sf_adjust_timeindex_aget(void* ptr, int* length, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif
