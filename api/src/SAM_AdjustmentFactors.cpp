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


#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"

#include "SAM_AdjustmentFactors.h"

/**
 * Setters
 */

SAM_EXPORT void SAM_AdjustmentFactors_adjust_constant_nset(void* ptr, double number, SAM_error *err){
    translateExceptions(err, [&]{
        ssc_data_set_number(ptr, "adjust_constant", number);
    });
}

SAM_EXPORT void SAM_AdjustmentFactors_adjust_en_hourly_nset(void* ptr, double num, SAM_error *err){
    translateExceptions(err, [&]{
        ssc_data_set_number(ptr, "adjust_en_hourly", num);
    });
}


SAM_EXPORT void SAM_AdjustmentFactors_adjust_en_periods_nset(void* ptr, double num, SAM_error *err){
    translateExceptions(err, [&]{
        ssc_data_set_number(ptr, "adjust_en_periods", num);
    });
}

SAM_EXPORT void SAM_AdjustmentFactors_adjust_en_timeindex_nset(void* ptr, double num, SAM_error *err){
    translateExceptions(err, [&]{
        ssc_data_set_number(ptr, "adjust_en_timeindex", num);
    });
}

SAM_EXPORT void SAM_AdjustmentFactors_adjust_hourly_aset(void* ptr, double* arr, int length, SAM_error *err){
    translateExceptions(err, [&]{
        ssc_data_set_array(ptr, "adjust_hourly", arr, length);
    });
}

SAM_EXPORT void SAM_AdjustmentFactors_adjust_periods_mset(void* ptr, double* mat, int nrows, int ncols, SAM_error *err){
    translateExceptions(err, [&]{
        ssc_data_set_matrix(ptr, "adjust_periods", mat, nrows, ncols);
    });
}

SAM_EXPORT void SAM_AdjustmentFactors_adjust_timeindex_aset(void* ptr, double* arr, int length, SAM_error *err){
    translateExceptions(err, [&]{
        ssc_data_set_array(ptr, "adjust_timeindex", arr, length);
    });
}

// dc adjust
SAM_EXPORT void SAM_AdjustmentFactors_dc_adjust_constant_nset(void* ptr, double number, SAM_error *err){
    translateExceptions(err, [&]{
        ssc_data_set_number(ptr, "dc_adjust_constant", number);
    });
}

SAM_EXPORT void SAM_AdjustmentFactors_dc_adjust_en_hourly_nset(void* ptr, double num, SAM_error *err){
    translateExceptions(err, [&]{
        ssc_data_set_number(ptr, "dc_adjust_en_hourly", num);
    });
}


SAM_EXPORT void SAM_AdjustmentFactors_dc_adjust_en_periods_nset(void* ptr, double num, SAM_error *err){
    translateExceptions(err, [&]{
        ssc_data_set_number(ptr, "dc_adjust_en_periods", num);
    });
}

SAM_EXPORT void SAM_AdjustmentFactors_dc_adjust_en_timeindex_nset(void* ptr, double num, SAM_error *err){
    translateExceptions(err, [&]{
        ssc_data_set_number(ptr, "dc_adjust_en_timeindex", num);
    });
}

SAM_EXPORT void SAM_AdjustmentFactors_dc_adjust_hourly_aset(void* ptr, double* arr, int length, SAM_error *err){
    translateExceptions(err, [&]{
        ssc_data_set_array(ptr, "dc_adjust_hourly", arr, length);
    });
}

SAM_EXPORT void SAM_AdjustmentFactors_dc_adjust_periods_mset(void* ptr, double* mat, int nrows, int ncols, SAM_error *err){
    translateExceptions(err, [&]{
        ssc_data_set_matrix(ptr, "dc_adjust_periods", mat, nrows, ncols);
    });
}

SAM_EXPORT void SAM_AdjustmentFactors_dc_adjust_timeindex_aset(void* ptr, double* arr, int length, SAM_error *err){
    translateExceptions(err, [&]{
        ssc_data_set_array(ptr, "dc_adjust_timeindex", arr, length);
    });
}

// sf adjust
SAM_EXPORT void SAM_AdjustmentFactors_sf_adjust_constant_nset(void* ptr, double number, SAM_error *err){
    translateExceptions(err, [&]{
        ssc_data_set_number(ptr, "sf_adjust_constant", number);
    });
}

SAM_EXPORT void SAM_AdjustmentFactors_sf_adjust_en_hourly_nset(void* ptr, double num, SAM_error *err){
    translateExceptions(err, [&]{
        ssc_data_set_number(ptr, "sf_adjust_en_hourly", num);
    });
}

SAM_EXPORT void SAM_AdjustmentFactors_sf_adjust_en_periods_nset(void* ptr, double num, SAM_error *err){
    translateExceptions(err, [&]{
        ssc_data_set_number(ptr, "sf_adjust_en_periods", num);
    });
}

SAM_EXPORT void SAM_AdjustmentFactors_sf_adjust_en_timeindex_nset(void* ptr, double num, SAM_error *err){
    translateExceptions(err, [&]{
        ssc_data_set_number(ptr, "sf_adjust_en_timeindex", num);
    });
}

SAM_EXPORT void SAM_AdjustmentFactors_sf_adjust_hourly_aset(void* ptr, double* arr, int length, SAM_error *err){
    translateExceptions(err, [&]{
        ssc_data_set_array(ptr, "sf_adjust_hourly", arr, length);
    });
}

SAM_EXPORT void SAM_AdjustmentFactors_sf_adjust_periods_mset(void* ptr, double* mat, int nrows, int ncols, SAM_error *err){
    translateExceptions(err, [&]{
        ssc_data_set_matrix(ptr, "sf_adjust_periods", mat, nrows, ncols);
    });
}

SAM_EXPORT void SAM_AdjustmentFactors_sf_adjust_timeindex_aset(void* ptr, double* arr, int length, SAM_error *err){
    translateExceptions(err, [&]{
        ssc_data_set_array(ptr, "sf_adjust_timeindex", arr, length);
    });
}

/**
 * Getters
 */

SAM_EXPORT double SAM_AdjustmentFactors_adjust_constant_nget(void* ptr, SAM_error *err){
    double result;
    translateExceptions(err, [&]{
        if (!ssc_data_get_number(ptr, "adjust_constant", &result))
            make_access_error("SAM_AdjustmentFactors", "adjust_constant");
    });
    return result;
}

SAM_EXPORT double SAM_AdjustmentFactors_adjust_en_hourly_nget(void* ptr, SAM_error *err){
    double result;
    translateExceptions(err, [&]{
        if (!ssc_data_get_number(ptr, "adjust_en_hourly", &result))
            make_access_error("SAM_AdjustmentFactors", "adjust_en_hourly");
    });
    return result;
}

SAM_EXPORT double SAM_AdjustmentFactors_adjust_en_periods_nget(void* ptr, SAM_error *err){
    double result;
    translateExceptions(err, [&]{
        if (!ssc_data_get_number(ptr, "adjust_en_periods", &result))
            make_access_error("SAM_AdjustmentFactors", "adjust_en_periods");
    });
    return result;
}

SAM_EXPORT double SAM_AdjustmentFactors_adjust_en_timeindex_nget(void* ptr, SAM_error *err){
    double result;
    translateExceptions(err, [&]{
        if (!ssc_data_get_number(ptr, "adjust_en_timeindex", &result))
            make_access_error("SAM_AdjustmentFactors", "adjust_en_timeindex");
    });
    return result;
}

SAM_EXPORT double* SAM_AdjustmentFactors_adjust_hourly_aget(void* ptr, int* length, SAM_error *err){
    double* result;
    translateExceptions(err, [&]{
        result = ssc_data_get_array(ptr, "adjust_hourly", length);
        if (!result)
            make_access_error("SAM_AdjustmentFactors", "adjust_hourly");
    });
    return result;
}

SAM_EXPORT double* SAM_AdjustmentFactors_adjust_periods_mget(void *ptr, int *nrows, int *ncols, SAM_error *err){
    double* result;
    translateExceptions(err, [&]{
        result = ssc_data_get_matrix(ptr, "adjust_periods", nrows, ncols);
        if (!result)
            make_access_error("SAM_AdjustmentFactors", "adjust_periods");
    });
    return result;
}

SAM_EXPORT double* SAM_AdjustmentFactors_adjust_timeindex_aget(void* ptr, int* length, SAM_error *err){
    double* result;
    translateExceptions(err, [&]{
        result = ssc_data_get_array(ptr, "adjust_timeindex", length);
        if (!result)
            make_access_error("SAM_AdjustmentFactors", "adjust_timeindex");
    });
    return result;
}

// dc adjust
SAM_EXPORT double SAM_AdjustmentFactors_dc_adjust_constant_nget(void* ptr, SAM_error *err){
    double result;
    translateExceptions(err, [&]{
        if (!ssc_data_get_number(ptr, "dc_adjust_constant", &result))
            make_access_error("SAM_AdjustmentFactors", "dc_adjust_constant");
    });
    return result;
}

SAM_EXPORT double SAM_AdjustmentFactors_dc_adjust_en_hourly_nget(void* ptr, SAM_error *err){
    double result;
    translateExceptions(err, [&]{
        if (!ssc_data_get_number(ptr, "dc_adjust_en_hourly", &result))
            make_access_error("SAM_AdjustmentFactors", "dc_adjust_en_hourly");
    });
    return result;
}

SAM_EXPORT double SAM_AdjustmentFactors_dc_adjust_en_periods_nget(void* ptr, SAM_error *err){
    double result;
    translateExceptions(err, [&]{
        if (!ssc_data_get_number(ptr, "dc_adjust_en_periods", &result))
            make_access_error("SAM_AdjustmentFactors", "dc_adjust_en_periods");
    });
    return result;
}

SAM_EXPORT double SAM_AdjustmentFactors_dc_adjust_en_timeindex_nget(void* ptr, SAM_error *err){
    double result;
    translateExceptions(err, [&]{
        if (!ssc_data_get_number(ptr, "dc_adjust_en_timeindex", &result))
            make_access_error("SAM_AdjustmentFactors", "dc_adjust_en_timeindex");
    });
    return result;
}

SAM_EXPORT double* SAM_AdjustmentFactors_dc_adjust_hourly_aget(void* ptr, int* length, SAM_error *err){
    double* result;
    translateExceptions(err, [&]{
        result = ssc_data_get_array(ptr, "dc_adjust_hourly", length);
        if (!result)
            make_access_error("SAM_AdjustmentFactors", "dc_adjust_hourly");
    });
    return result;
}

SAM_EXPORT double* SAM_AdjustmentFactors_dc_adjust_periods_mget(void *ptr, int *nrows, int *ncols, SAM_error *err){
    double* result;
    translateExceptions(err, [&]{
        result = ssc_data_get_matrix(ptr, "dc_adjust_periods", nrows, ncols);
        if (!result)
            make_access_error("SAM_AdjustmentFactors", "dc_adjust_periods");
    });
    return result;
}

SAM_EXPORT double* SAM_AdjustmentFactors_dc_adjust_timeindex_aget(void* ptr, int* length, SAM_error *err){
    double* result;
    translateExceptions(err, [&]{
        result = ssc_data_get_array(ptr, "dc_adjust_timeindex", length);
        if (!result)
            make_access_error("SAM_AdjustmentFactors", "dc_adjust_timeindex");
    });
    return result;
}

// sf adjust
SAM_EXPORT double SAM_AdjustmentFactors_sf_adjust_constant_nget(void* ptr, SAM_error *err){
    double result;
    translateExceptions(err, [&]{
        if (!ssc_data_get_number(ptr, "sf_adjust_constant", &result))
            make_access_error("SAM_AdjustmentFactors", "sf_adjust_constant");
    });
    return result;
}

SAM_EXPORT double SAM_AdjustmentFactors_sf_adjust_en_hourly_nget(void* ptr, SAM_error *err){
    double result;
    translateExceptions(err, [&]{
        if (!ssc_data_get_number(ptr, "sf_adjust_en_hourly", &result))
            make_access_error("SAM_AdjustmentFactors", "sf_adjust_en_hourly");
    });
    return result;
}

SAM_EXPORT double SAM_AdjustmentFactors_sf_adjust_en_periods_nget(void* ptr, SAM_error *err){
    double result;
    translateExceptions(err, [&]{
        if (!ssc_data_get_number(ptr, "sf_adjust_en_periods", &result))
            make_access_error("SAM_AdjustmentFactors", "sf_adjust_en_periods");
    });
    return result;
}

SAM_EXPORT double SAM_AdjustmentFactors_sf_adjust_en_timeindex_nget(void* ptr, SAM_error *err){
    double result;
    translateExceptions(err, [&]{
        if (!ssc_data_get_number(ptr, "sf_adjust_en_timeindex", &result))
            make_access_error("SAM_AdjustmentFactors", "sf_adjust_en_timeindex");
    });
    return result;
}

SAM_EXPORT double* SAM_AdjustmentFactors_sf_adjust_hourly_aget(void* ptr, int* length, SAM_error *err){
    double* result;
    translateExceptions(err, [&]{
        result = ssc_data_get_array(ptr, "sf_adjust_hourly", length);
        if (!result)
            make_access_error("SAM_AdjustmentFactors", "sf_adjust_hourly");
    });
    return result;
}

SAM_EXPORT double* SAM_AdjustmentFactors_sf_adjust_periods_mget(void *ptr, int *nrows, int *ncols, SAM_error *err){
    double* result;
    translateExceptions(err, [&]{
        result = ssc_data_get_matrix(ptr, "sf_adjust_periods", nrows, ncols);
        if (!result)
            make_access_error("SAM_AdjustmentFactors", "sf_adjust_periods");
    });
    return result;
}

SAM_EXPORT double* SAM_AdjustmentFactors_sf_adjust_timeindex_aget(void* ptr, int* length, SAM_error *err){
    double* result;
    translateExceptions(err, [&]{
        result = ssc_data_get_array(ptr, "sf_adjust_timeindex", length);
        if (!result)
            make_access_error("SAM_AdjustmentFactors", "sf_adjust_timeindex");
    });
    return result;
}
