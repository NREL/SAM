#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_Wfreader.h"

SAM_EXPORT SAM_Wfreader SAM_Wfreader_construct(const char *def, SAM_error *err) {
    SAM_Wfreader result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_create();
    });
    return result;
}

SAM_EXPORT int SAM_Wfreader_execute(SAM_Wfreader data, int verbosity, SAM_error *err) {
    int n_err = 0;
    translateExceptions(err, [&] {
        n_err += SAM_module_exec("wfreader", data, verbosity, err);
    });
    return n_err;
}


SAM_EXPORT void SAM_Wfreader_destruct(SAM_Wfreader system) {
    ssc_data_free(system);
}

SAM_EXPORT void SAM_Wfreader_WeatherReader_file_name_sset(SAM_Wfreader ptr, const char *str, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_string(ptr, "file_name", str);
    });
}

SAM_EXPORT void SAM_Wfreader_WeatherReader_header_only_nset(SAM_Wfreader ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "header_only", number);
    });
}

SAM_EXPORT const char *SAM_Wfreader_WeatherReader_file_name_sget(SAM_Wfreader ptr, SAM_error *err) {
    const char *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_string(ptr, "file_name");
        if (!result)
            make_access_error("SAM_Wfreader", "file_name");
    });
    return result;
}


SAM_EXPORT double SAM_Wfreader_WeatherReader_header_only_nget(SAM_Wfreader ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "header_only", &result))
            make_access_error("SAM_Wfreader", "header_only");
    });
    return result;
}


SAM_EXPORT double *SAM_Wfreader_Outputs_albedo_aget(SAM_Wfreader ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "albedo", length);
        if (!result)
            make_access_error("SAM_Wfreader", "albedo");
    });
    return result;
}


SAM_EXPORT double SAM_Wfreader_Outputs_annual_albedo_nget(SAM_Wfreader ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "annual_albedo", &result))
            make_access_error("SAM_Wfreader", "annual_albedo");
    });
    return result;
}


SAM_EXPORT double SAM_Wfreader_Outputs_annual_beam_nget(SAM_Wfreader ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "annual_beam", &result))
            make_access_error("SAM_Wfreader", "annual_beam");
    });
    return result;
}


SAM_EXPORT double SAM_Wfreader_Outputs_annual_diffuse_nget(SAM_Wfreader ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "annual_diffuse", &result))
            make_access_error("SAM_Wfreader", "annual_diffuse");
    });
    return result;
}


SAM_EXPORT double SAM_Wfreader_Outputs_annual_global_nget(SAM_Wfreader ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "annual_global", &result))
            make_access_error("SAM_Wfreader", "annual_global");
    });
    return result;
}


SAM_EXPORT double SAM_Wfreader_Outputs_annual_snow_nget(SAM_Wfreader ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "annual_snow", &result))
            make_access_error("SAM_Wfreader", "annual_snow");
    });
    return result;
}


SAM_EXPORT double SAM_Wfreader_Outputs_annual_tdry_nget(SAM_Wfreader ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "annual_tdry", &result))
            make_access_error("SAM_Wfreader", "annual_tdry");
    });
    return result;
}


SAM_EXPORT double SAM_Wfreader_Outputs_annual_wspd_nget(SAM_Wfreader ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "annual_wspd", &result))
            make_access_error("SAM_Wfreader", "annual_wspd");
    });
    return result;
}


SAM_EXPORT double *SAM_Wfreader_Outputs_beam_aget(SAM_Wfreader ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "beam", length);
        if (!result)
            make_access_error("SAM_Wfreader", "beam");
    });
    return result;
}


SAM_EXPORT const char *SAM_Wfreader_Outputs_city_sget(SAM_Wfreader ptr, SAM_error *err) {
    const char *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_string(ptr, "city");
        if (!result)
            make_access_error("SAM_Wfreader", "city");
    });
    return result;
}


SAM_EXPORT const char *SAM_Wfreader_Outputs_country_sget(SAM_Wfreader ptr, SAM_error *err) {
    const char *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_string(ptr, "country");
        if (!result)
            make_access_error("SAM_Wfreader", "country");
    });
    return result;
}


SAM_EXPORT double *SAM_Wfreader_Outputs_day_aget(SAM_Wfreader ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "day", length);
        if (!result)
            make_access_error("SAM_Wfreader", "day");
    });
    return result;
}


SAM_EXPORT const char *SAM_Wfreader_Outputs_description_sget(SAM_Wfreader ptr, SAM_error *err) {
    const char *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_string(ptr, "description");
        if (!result)
            make_access_error("SAM_Wfreader", "description");
    });
    return result;
}


SAM_EXPORT double *SAM_Wfreader_Outputs_diffuse_aget(SAM_Wfreader ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "diffuse", length);
        if (!result)
            make_access_error("SAM_Wfreader", "diffuse");
    });
    return result;
}


SAM_EXPORT double SAM_Wfreader_Outputs_elev_nget(SAM_Wfreader ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "elev", &result))
            make_access_error("SAM_Wfreader", "elev");
    });
    return result;
}


SAM_EXPORT const char *SAM_Wfreader_Outputs_format_sget(SAM_Wfreader ptr, SAM_error *err) {
    const char *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_string(ptr, "format");
        if (!result)
            make_access_error("SAM_Wfreader", "format");
    });
    return result;
}


SAM_EXPORT double *SAM_Wfreader_Outputs_global_aget(SAM_Wfreader ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "global", length);
        if (!result)
            make_access_error("SAM_Wfreader", "global");
    });
    return result;
}


SAM_EXPORT double *SAM_Wfreader_Outputs_hour_aget(SAM_Wfreader ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "hour", length);
        if (!result)
            make_access_error("SAM_Wfreader", "hour");
    });
    return result;
}


SAM_EXPORT double SAM_Wfreader_Outputs_lat_nget(SAM_Wfreader ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "lat", &result))
            make_access_error("SAM_Wfreader", "lat");
    });
    return result;
}


SAM_EXPORT const char *SAM_Wfreader_Outputs_location_sget(SAM_Wfreader ptr, SAM_error *err) {
    const char *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_string(ptr, "location");
        if (!result)
            make_access_error("SAM_Wfreader", "location");
    });
    return result;
}


SAM_EXPORT double SAM_Wfreader_Outputs_lon_nget(SAM_Wfreader ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "lon", &result))
            make_access_error("SAM_Wfreader", "lon");
    });
    return result;
}


SAM_EXPORT double *SAM_Wfreader_Outputs_minute_aget(SAM_Wfreader ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "minute", length);
        if (!result)
            make_access_error("SAM_Wfreader", "minute");
    });
    return result;
}


SAM_EXPORT double *SAM_Wfreader_Outputs_month_aget(SAM_Wfreader ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "month", length);
        if (!result)
            make_access_error("SAM_Wfreader", "month");
    });
    return result;
}


SAM_EXPORT double SAM_Wfreader_Outputs_nrecords_nget(SAM_Wfreader ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "nrecords", &result))
            make_access_error("SAM_Wfreader", "nrecords");
    });
    return result;
}


SAM_EXPORT double *SAM_Wfreader_Outputs_poa_aget(SAM_Wfreader ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "poa", length);
        if (!result)
            make_access_error("SAM_Wfreader", "poa");
    });
    return result;
}


SAM_EXPORT double *SAM_Wfreader_Outputs_pres_aget(SAM_Wfreader ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "pres", length);
        if (!result)
            make_access_error("SAM_Wfreader", "pres");
    });
    return result;
}


SAM_EXPORT double *SAM_Wfreader_Outputs_rhum_aget(SAM_Wfreader ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "rhum", length);
        if (!result)
            make_access_error("SAM_Wfreader", "rhum");
    });
    return result;
}


SAM_EXPORT double *SAM_Wfreader_Outputs_snow_aget(SAM_Wfreader ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "snow", length);
        if (!result)
            make_access_error("SAM_Wfreader", "snow");
    });
    return result;
}


SAM_EXPORT const char *SAM_Wfreader_Outputs_source_sget(SAM_Wfreader ptr, SAM_error *err) {
    const char *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_string(ptr, "source");
        if (!result)
            make_access_error("SAM_Wfreader", "source");
    });
    return result;
}


SAM_EXPORT double SAM_Wfreader_Outputs_start_nget(SAM_Wfreader ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "start", &result))
            make_access_error("SAM_Wfreader", "start");
    });
    return result;
}


SAM_EXPORT const char *SAM_Wfreader_Outputs_state_sget(SAM_Wfreader ptr, SAM_error *err) {
    const char *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_string(ptr, "state");
        if (!result)
            make_access_error("SAM_Wfreader", "state");
    });
    return result;
}


SAM_EXPORT double SAM_Wfreader_Outputs_step_nget(SAM_Wfreader ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "step", &result))
            make_access_error("SAM_Wfreader", "step");
    });
    return result;
}


SAM_EXPORT double *SAM_Wfreader_Outputs_tdew_aget(SAM_Wfreader ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "tdew", length);
        if (!result)
            make_access_error("SAM_Wfreader", "tdew");
    });
    return result;
}


SAM_EXPORT double *SAM_Wfreader_Outputs_tdry_aget(SAM_Wfreader ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "tdry", length);
        if (!result)
            make_access_error("SAM_Wfreader", "tdry");
    });
    return result;
}


SAM_EXPORT double *SAM_Wfreader_Outputs_twet_aget(SAM_Wfreader ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "twet", length);
        if (!result)
            make_access_error("SAM_Wfreader", "twet");
    });
    return result;
}


SAM_EXPORT double SAM_Wfreader_Outputs_tz_nget(SAM_Wfreader ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "tz", &result))
            make_access_error("SAM_Wfreader", "tz");
    });
    return result;
}


SAM_EXPORT const char *SAM_Wfreader_Outputs_url_sget(SAM_Wfreader ptr, SAM_error *err) {
    const char *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_string(ptr, "url");
        if (!result)
            make_access_error("SAM_Wfreader", "url");
    });
    return result;
}


SAM_EXPORT double *SAM_Wfreader_Outputs_wdir_aget(SAM_Wfreader ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "wdir", length);
        if (!result)
            make_access_error("SAM_Wfreader", "wdir");
    });
    return result;
}


SAM_EXPORT double *SAM_Wfreader_Outputs_wspd_aget(SAM_Wfreader ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "wspd", length);
        if (!result)
            make_access_error("SAM_Wfreader", "wspd");
    });
    return result;
}


SAM_EXPORT double *SAM_Wfreader_Outputs_year_aget(SAM_Wfreader ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "year", length);
        if (!result)
            make_access_error("SAM_Wfreader", "year");
    });
    return result;
}



