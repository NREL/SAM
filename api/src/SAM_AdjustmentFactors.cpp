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

SAM_EXPORT void SAM_AdjustmentFactors_adjust_constant_nset(void *ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "adjust:constant", number);
    });
}

SAM_EXPORT void SAM_AdjustmentFactors_adjust_hourly_aset(void *ptr, double *arr, int length, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_array(ptr, "adjust:hourly", arr, length);
    });
}

SAM_EXPORT void
SAM_AdjustmentFactors_adjust_periods_mset(void *ptr, double *mat, int nrows, int ncols, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_matrix(ptr, "adjust:periods", mat, nrows, ncols);
    });
}

SAM_EXPORT void SAM_AdjustmentFactors_dc_adjust_constant_nset(void *ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "dc_adjust:constant", number);
    });
}

SAM_EXPORT void SAM_AdjustmentFactors_dc_adjust_hourly_aset(void *ptr, double *arr, int length, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_array(ptr, "dc_adjust:hourly", arr, length);
    });
}

SAM_EXPORT void
SAM_AdjustmentFactors_dc_adjust_periods_mset(void *ptr, double *mat, int nrows, int ncols, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_matrix(ptr, "dc_adjust:periods", mat, nrows, ncols);
    });
}

SAM_EXPORT void SAM_AdjustmentFactors_sf_adjust_constant_nset(void *ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "sf_adjust:constant", number);
    });
}

SAM_EXPORT void SAM_AdjustmentFactors_sf_adjust_hourly_aset(void *ptr, double *arr, int length, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_array(ptr, "sf_adjust:hourly", arr, length);
    });
}

SAM_EXPORT void
SAM_AdjustmentFactors_sf_adjust_periods_mset(void *ptr, double *mat, int nrows, int ncols, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_matrix(ptr, "sf_adjust:periods", mat, nrows, ncols);
    });
}

/**
 * Getters
 */

SAM_EXPORT double SAM_AdjustmentFactors_adjust_constant_nget(void *ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "adjust:constant", &result))
            make_access_error("SAM_AdjustmentFactors", "adjust:constant");
    });
    return result;
}

SAM_EXPORT double *SAM_AdjustmentFactors_adjust_hourly_aget(void *ptr, int *length, SAM_error *err) {
    double *result;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "adjust:hourly", length);
        if (!result)
            make_access_error("SAM_AdjustmentFactors", "adjust:hourly");
    });
    return result;
}

SAM_EXPORT double *SAM_AdjustmentFactors_adjust_periods_mget(void *ptr, int *nrows, int *ncols, SAM_error *err) {
    double *result;
    translateExceptions(err, [&] {
        result = ssc_data_get_matrix(ptr, "adjust:periods", nrows, ncols);
        if (!result)
            make_access_error("SAM_AdjustmentFactors", "adjust:periods");
    });
    return result;
}

SAM_EXPORT double SAM_AdjustmentFactors_dc_adjust_constant_nget(void *ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "dc_adjust:constant", &result))
            make_access_error("SAM_AdjustmentFactors", "dc_adjust:constant");
    });
    return result;
}

SAM_EXPORT double *SAM_AdjustmentFactors_dc_adjust_hourly_aget(void *ptr, int *length, SAM_error *err) {
    double *result;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "dc_adjust:hourly", length);
        if (!result)
            make_access_error("SAM_AdjustmentFactors", "dc_adjust:hourly");
    });
    return result;
}

SAM_EXPORT double *SAM_AdjustmentFactors_dc_adjust_periods_mget(void *ptr, int *nrows, int *ncols, SAM_error *err) {
    double *result;
    translateExceptions(err, [&] {
        result = ssc_data_get_matrix(ptr, "dc_adjust:periods", nrows, ncols);
        if (!result)
            make_access_error("SAM_AdjustmentFactors", "dc_adjust:periods");
    });
    return result;
}

SAM_EXPORT double SAM_AdjustmentFactors_sf_adjust_constant_nget(void *ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "sf_adjust:constant", &result))
            make_access_error("SAM_AdjustmentFactors", "sf_adjust:constant");
    });
    return result;
}

SAM_EXPORT double *SAM_AdjustmentFactors_sf_adjust_hourly_aget(void *ptr, int *length, SAM_error *err) {
    double *result;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "sf_adjust:hourly", length);
        if (!result)
            make_access_error("SAM_AdjustmentFactors", "sf_adjust:hourly");
    });
    return result;
}

SAM_EXPORT double *SAM_AdjustmentFactors_sf_adjust_periods_mget(void *ptr, int *nrows, int *ncols, SAM_error *err) {
    double *result;
    translateExceptions(err, [&] {
        result = ssc_data_get_matrix(ptr, "sf_adjust:periods", nrows, ncols);
        if (!result)
            make_access_error("SAM_AdjustmentFactors", "sf_adjust:periods");
    });
    return result;
}
