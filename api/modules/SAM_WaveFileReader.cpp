#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_WaveFileReader.h"

SAM_EXPORT SAM_WaveFileReader SAM_WaveFileReader_construct(const char *def, SAM_error *err) {
    SAM_WaveFileReader result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_create();
    });
    return result;
}

SAM_EXPORT int SAM_WaveFileReader_execute(SAM_WaveFileReader data, int verbosity, SAM_error *err) {
    int n_err = 0;
    translateExceptions(err, [&] {
        n_err += SAM_module_exec("wave_file_reader", data, verbosity, err);
    });
    return n_err;
}


SAM_EXPORT void SAM_WaveFileReader_destruct(SAM_WaveFileReader system) {
    ssc_data_free(system);
}

SAM_EXPORT void
SAM_WaveFileReader_WeatherReader_use_specific_wf_wave_nset(SAM_WaveFileReader ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "use_specific_wf_wave", number);
    });
}

SAM_EXPORT void
SAM_WaveFileReader_WeatherReader_wave_resource_filename_sset(SAM_WaveFileReader ptr, const char *str, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_string(ptr, "wave_resource_filename", str);
    });
}

SAM_EXPORT double SAM_WaveFileReader_WeatherReader_use_specific_wf_wave_nget(SAM_WaveFileReader ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "use_specific_wf_wave", &result))
            make_access_error("SAM_WaveFileReader", "use_specific_wf_wave");
    });
    return result;
}


SAM_EXPORT const char *
SAM_WaveFileReader_WeatherReader_wave_resource_filename_sget(SAM_WaveFileReader ptr, SAM_error *err) {
    const char *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_string(ptr, "wave_resource_filename");
        if (!result)
            make_access_error("SAM_WaveFileReader", "wave_resource_filename");
    });
    return result;
}


SAM_EXPORT double SAM_WaveFileReader_Outputs_average_power_flux_nget(SAM_WaveFileReader ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "average_power_flux", &result))
            make_access_error("SAM_WaveFileReader", "average_power_flux");
    });
    return result;
}


SAM_EXPORT const char *SAM_WaveFileReader_Outputs_bathymetry_sget(SAM_WaveFileReader ptr, SAM_error *err) {
    const char *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_string(ptr, "bathymetry");
        if (!result)
            make_access_error("SAM_WaveFileReader", "bathymetry");
    });
    return result;
}


SAM_EXPORT const char *SAM_WaveFileReader_Outputs_city_sget(SAM_WaveFileReader ptr, SAM_error *err) {
    const char *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_string(ptr, "city");
        if (!result)
            make_access_error("SAM_WaveFileReader", "city");
    });
    return result;
}


SAM_EXPORT const char *SAM_WaveFileReader_Outputs_country_sget(SAM_WaveFileReader ptr, SAM_error *err) {
    const char *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_string(ptr, "country");
        if (!result)
            make_access_error("SAM_WaveFileReader", "country");
    });
    return result;
}


SAM_EXPORT const char *SAM_WaveFileReader_Outputs_data_source_sget(SAM_WaveFileReader ptr, SAM_error *err) {
    const char *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_string(ptr, "data_source");
        if (!result)
            make_access_error("SAM_WaveFileReader", "data_source");
    });
    return result;
}


SAM_EXPORT double SAM_WaveFileReader_Outputs_lat_nget(SAM_WaveFileReader ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "lat", &result))
            make_access_error("SAM_WaveFileReader", "lat");
    });
    return result;
}


SAM_EXPORT double SAM_WaveFileReader_Outputs_lon_nget(SAM_WaveFileReader ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "lon", &result))
            make_access_error("SAM_WaveFileReader", "lon");
    });
    return result;
}


SAM_EXPORT const char *SAM_WaveFileReader_Outputs_name_sget(SAM_WaveFileReader ptr, SAM_error *err) {
    const char *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_string(ptr, "name");
        if (!result)
            make_access_error("SAM_WaveFileReader", "name");
    });
    return result;
}


SAM_EXPORT const char *SAM_WaveFileReader_Outputs_nearby_buoy_number_sget(SAM_WaveFileReader ptr, SAM_error *err) {
    const char *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_string(ptr, "nearby_buoy_number");
        if (!result)
            make_access_error("SAM_WaveFileReader", "nearby_buoy_number");
    });
    return result;
}


SAM_EXPORT const char *SAM_WaveFileReader_Outputs_notes_sget(SAM_WaveFileReader ptr, SAM_error *err) {
    const char *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_string(ptr, "notes");
        if (!result)
            make_access_error("SAM_WaveFileReader", "notes");
    });
    return result;
}


SAM_EXPORT const char *SAM_WaveFileReader_Outputs_sea_bed_sget(SAM_WaveFileReader ptr, SAM_error *err) {
    const char *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_string(ptr, "sea_bed");
        if (!result)
            make_access_error("SAM_WaveFileReader", "sea_bed");
    });
    return result;
}


SAM_EXPORT const char *SAM_WaveFileReader_Outputs_state_sget(SAM_WaveFileReader ptr, SAM_error *err) {
    const char *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_string(ptr, "state");
        if (!result)
            make_access_error("SAM_WaveFileReader", "state");
    });
    return result;
}


SAM_EXPORT double SAM_WaveFileReader_Outputs_tz_nget(SAM_WaveFileReader ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "tz", &result))
            make_access_error("SAM_WaveFileReader", "tz");
    });
    return result;
}


SAM_EXPORT double *
SAM_WaveFileReader_Outputs_wave_resource_matrix_mget(SAM_WaveFileReader ptr, int *nrows, int *ncols, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_matrix(ptr, "wave_resource_matrix", nrows, ncols);
        if (!result)
            make_access_error("SAM_WaveFileReader", "wave_resource_matrix");
    });
    return result;
}



