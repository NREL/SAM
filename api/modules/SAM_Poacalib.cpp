#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_Poacalib.h"

SAM_EXPORT SAM_Poacalib SAM_Poacalib_construct(const char *def, SAM_error *err) {
    SAM_Poacalib result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_create();
    });
    return result;
}

SAM_EXPORT int SAM_Poacalib_execute(SAM_Poacalib data, int verbosity, SAM_error *err) {
    int n_err = 0;
    translateExceptions(err, [&] {
        n_err += SAM_module_exec("poacalib", data, verbosity, err);
    });
    return n_err;
}


SAM_EXPORT void SAM_Poacalib_destruct(SAM_Poacalib system) {
    ssc_data_free(system);
}

SAM_EXPORT void SAM_Poacalib_POACalibrate_albedo_nset(SAM_Poacalib ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "albedo", number);
    });
}

SAM_EXPORT void SAM_Poacalib_POACalibrate_array_az_nset(SAM_Poacalib ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "array_az", number);
    });
}

SAM_EXPORT void SAM_Poacalib_POACalibrate_array_tilt_nset(SAM_Poacalib ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "array_tilt", number);
    });
}

SAM_EXPORT void SAM_Poacalib_POACalibrate_beam_aset(SAM_Poacalib ptr, double *arr, int length, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_array(ptr, "beam", arr, length);
    });
}

SAM_EXPORT void SAM_Poacalib_POACalibrate_diffuse_aset(SAM_Poacalib ptr, double *arr, int length, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_array(ptr, "diffuse", arr, length);
    });
}

SAM_EXPORT void SAM_Poacalib_POACalibrate_latitude_nset(SAM_Poacalib ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "latitude", number);
    });
}

SAM_EXPORT void SAM_Poacalib_POACalibrate_longitude_nset(SAM_Poacalib ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "longitude", number);
    });
}

SAM_EXPORT void SAM_Poacalib_POACalibrate_poa_aset(SAM_Poacalib ptr, double *arr, int length, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_array(ptr, "poa", arr, length);
    });
}

SAM_EXPORT void SAM_Poacalib_POACalibrate_time_zone_nset(SAM_Poacalib ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "time_zone", number);
    });
}

SAM_EXPORT void SAM_Poacalib_POACalibrate_year_nset(SAM_Poacalib ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "year", number);
    });
}

SAM_EXPORT double SAM_Poacalib_POACalibrate_albedo_nget(SAM_Poacalib ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "albedo", &result))
            make_access_error("SAM_Poacalib", "albedo");
    });
    return result;
}


SAM_EXPORT double SAM_Poacalib_POACalibrate_array_az_nget(SAM_Poacalib ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "array_az", &result))
            make_access_error("SAM_Poacalib", "array_az");
    });
    return result;
}


SAM_EXPORT double SAM_Poacalib_POACalibrate_array_tilt_nget(SAM_Poacalib ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "array_tilt", &result))
            make_access_error("SAM_Poacalib", "array_tilt");
    });
    return result;
}


SAM_EXPORT double *SAM_Poacalib_POACalibrate_beam_aget(SAM_Poacalib ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "beam", length);
        if (!result)
            make_access_error("SAM_Poacalib", "beam");
    });
    return result;
}


SAM_EXPORT double *SAM_Poacalib_POACalibrate_diffuse_aget(SAM_Poacalib ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "diffuse", length);
        if (!result)
            make_access_error("SAM_Poacalib", "diffuse");
    });
    return result;
}


SAM_EXPORT double SAM_Poacalib_POACalibrate_latitude_nget(SAM_Poacalib ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "latitude", &result))
            make_access_error("SAM_Poacalib", "latitude");
    });
    return result;
}


SAM_EXPORT double SAM_Poacalib_POACalibrate_longitude_nget(SAM_Poacalib ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "longitude", &result))
            make_access_error("SAM_Poacalib", "longitude");
    });
    return result;
}


SAM_EXPORT double *SAM_Poacalib_POACalibrate_poa_aget(SAM_Poacalib ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "poa", length);
        if (!result)
            make_access_error("SAM_Poacalib", "poa");
    });
    return result;
}


SAM_EXPORT double SAM_Poacalib_POACalibrate_time_zone_nget(SAM_Poacalib ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "time_zone", &result))
            make_access_error("SAM_Poacalib", "time_zone");
    });
    return result;
}


SAM_EXPORT double SAM_Poacalib_POACalibrate_year_nget(SAM_Poacalib ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "year", &result))
            make_access_error("SAM_Poacalib", "year");
    });
    return result;
}


SAM_EXPORT double *SAM_Poacalib_Outputs_pcalc_aget(SAM_Poacalib ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "pcalc", length);
        if (!result)
            make_access_error("SAM_Poacalib", "pcalc");
    });
    return result;
}



