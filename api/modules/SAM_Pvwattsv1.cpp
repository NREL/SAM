#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_Pvwattsv1.h"

SAM_EXPORT SAM_Pvwattsv1 SAM_Pvwattsv1_construct(const char *def, SAM_error *err) {
    SAM_Pvwattsv1 result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_create();
    });
    return result;
}

SAM_EXPORT int SAM_Pvwattsv1_execute(SAM_Pvwattsv1 data, int verbosity, SAM_error *err) {
    int n_err = 0;
    translateExceptions(err, [&] {
        n_err += SAM_module_exec("pvwattsv1", data, verbosity, err);
    });
    return n_err;
}


SAM_EXPORT void SAM_Pvwattsv1_destruct(SAM_Pvwattsv1 system) {
    ssc_data_free(system);
}

SAM_EXPORT void SAM_Pvwattsv1_Weather_solar_resource_file_sset(SAM_Pvwattsv1 ptr, const char *str, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_string(ptr, "solar_resource_file", str);
    });
}

SAM_EXPORT void SAM_Pvwattsv1_PVWatts_albedo_nset(SAM_Pvwattsv1 ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "albedo", number);
    });
}

SAM_EXPORT void SAM_Pvwattsv1_PVWatts_ar_glass_nset(SAM_Pvwattsv1 ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "ar_glass", number);
    });
}

SAM_EXPORT void SAM_Pvwattsv1_PVWatts_azimuth_nset(SAM_Pvwattsv1 ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "azimuth", number);
    });
}

SAM_EXPORT void SAM_Pvwattsv1_PVWatts_concen_nset(SAM_Pvwattsv1 ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "concen", number);
    });
}

SAM_EXPORT void SAM_Pvwattsv1_PVWatts_derate_nset(SAM_Pvwattsv1 ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "derate", number);
    });
}

SAM_EXPORT void SAM_Pvwattsv1_PVWatts_enable_user_poa_nset(SAM_Pvwattsv1 ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "enable_user_poa", number);
    });
}

SAM_EXPORT void SAM_Pvwattsv1_PVWatts_fd_nset(SAM_Pvwattsv1 ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "fd", number);
    });
}

SAM_EXPORT void SAM_Pvwattsv1_PVWatts_fhconv_nset(SAM_Pvwattsv1 ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "fhconv", number);
    });
}

SAM_EXPORT void SAM_Pvwattsv1_PVWatts_gamma_nset(SAM_Pvwattsv1 ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "gamma", number);
    });
}

SAM_EXPORT void SAM_Pvwattsv1_PVWatts_gcr_nset(SAM_Pvwattsv1 ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "gcr", number);
    });
}

SAM_EXPORT void SAM_Pvwattsv1_PVWatts_i_ref_nset(SAM_Pvwattsv1 ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "i_ref", number);
    });
}

SAM_EXPORT void SAM_Pvwattsv1_PVWatts_inoct_nset(SAM_Pvwattsv1 ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "inoct", number);
    });
}

SAM_EXPORT void SAM_Pvwattsv1_PVWatts_inv_eff_nset(SAM_Pvwattsv1 ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "inv_eff", number);
    });
}

SAM_EXPORT void SAM_Pvwattsv1_PVWatts_poa_cutin_nset(SAM_Pvwattsv1 ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "poa_cutin", number);
    });
}

SAM_EXPORT void SAM_Pvwattsv1_PVWatts_rotlim_nset(SAM_Pvwattsv1 ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "rotlim", number);
    });
}

SAM_EXPORT void SAM_Pvwattsv1_PVWatts_shade_mode_1x_nset(SAM_Pvwattsv1 ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "shade_mode_1x", number);
    });
}

SAM_EXPORT void
SAM_Pvwattsv1_PVWatts_shading_azal_mset(SAM_Pvwattsv1 ptr, double *mat, int nrows, int ncols, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_matrix(ptr, "shading:azal", mat, nrows, ncols);
    });
}

SAM_EXPORT void SAM_Pvwattsv1_PVWatts_shading_diff_nset(SAM_Pvwattsv1 ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "shading:diff", number);
    });
}

SAM_EXPORT void
SAM_Pvwattsv1_PVWatts_shading_mxh_mset(SAM_Pvwattsv1 ptr, double *mat, int nrows, int ncols, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_matrix(ptr, "shading:mxh", mat, nrows, ncols);
    });
}

SAM_EXPORT void
SAM_Pvwattsv1_PVWatts_shading_timestep_mset(SAM_Pvwattsv1 ptr, double *mat, int nrows, int ncols, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_matrix(ptr, "shading:timestep", mat, nrows, ncols);
    });
}

SAM_EXPORT void SAM_Pvwattsv1_PVWatts_system_size_nset(SAM_Pvwattsv1 ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "system_size", number);
    });
}

SAM_EXPORT void SAM_Pvwattsv1_PVWatts_tilt_nset(SAM_Pvwattsv1 ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "tilt", number);
    });
}

SAM_EXPORT void SAM_Pvwattsv1_PVWatts_tilt_eq_lat_nset(SAM_Pvwattsv1 ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "tilt_eq_lat", number);
    });
}

SAM_EXPORT void SAM_Pvwattsv1_PVWatts_track_mode_nset(SAM_Pvwattsv1 ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "track_mode", number);
    });
}

SAM_EXPORT void SAM_Pvwattsv1_PVWatts_tref_nset(SAM_Pvwattsv1 ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "tref", number);
    });
}

SAM_EXPORT void SAM_Pvwattsv1_PVWatts_u0_nset(SAM_Pvwattsv1 ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "u0", number);
    });
}

SAM_EXPORT void SAM_Pvwattsv1_PVWatts_u1_nset(SAM_Pvwattsv1 ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "u1", number);
    });
}

SAM_EXPORT void SAM_Pvwattsv1_PVWatts_user_poa_aset(SAM_Pvwattsv1 ptr, double *arr, int length, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_array(ptr, "user_poa", arr, length);
    });
}

SAM_EXPORT void SAM_Pvwattsv1_PVWatts_w_stow_nset(SAM_Pvwattsv1 ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "w_stow", number);
    });
}

SAM_EXPORT const char *SAM_Pvwattsv1_Weather_solar_resource_file_sget(SAM_Pvwattsv1 ptr, SAM_error *err) {
    const char *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_string(ptr, "solar_resource_file");
        if (!result)
            make_access_error("SAM_Pvwattsv1", "solar_resource_file");
    });
    return result;
}


SAM_EXPORT double SAM_Pvwattsv1_PVWatts_albedo_nget(SAM_Pvwattsv1 ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "albedo", &result))
            make_access_error("SAM_Pvwattsv1", "albedo");
    });
    return result;
}


SAM_EXPORT double SAM_Pvwattsv1_PVWatts_ar_glass_nget(SAM_Pvwattsv1 ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "ar_glass", &result))
            make_access_error("SAM_Pvwattsv1", "ar_glass");
    });
    return result;
}


SAM_EXPORT double SAM_Pvwattsv1_PVWatts_azimuth_nget(SAM_Pvwattsv1 ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "azimuth", &result))
            make_access_error("SAM_Pvwattsv1", "azimuth");
    });
    return result;
}


SAM_EXPORT double SAM_Pvwattsv1_PVWatts_concen_nget(SAM_Pvwattsv1 ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "concen", &result))
            make_access_error("SAM_Pvwattsv1", "concen");
    });
    return result;
}


SAM_EXPORT double SAM_Pvwattsv1_PVWatts_derate_nget(SAM_Pvwattsv1 ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "derate", &result))
            make_access_error("SAM_Pvwattsv1", "derate");
    });
    return result;
}


SAM_EXPORT double SAM_Pvwattsv1_PVWatts_enable_user_poa_nget(SAM_Pvwattsv1 ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "enable_user_poa", &result))
            make_access_error("SAM_Pvwattsv1", "enable_user_poa");
    });
    return result;
}


SAM_EXPORT double SAM_Pvwattsv1_PVWatts_fd_nget(SAM_Pvwattsv1 ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "fd", &result))
            make_access_error("SAM_Pvwattsv1", "fd");
    });
    return result;
}


SAM_EXPORT double SAM_Pvwattsv1_PVWatts_fhconv_nget(SAM_Pvwattsv1 ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "fhconv", &result))
            make_access_error("SAM_Pvwattsv1", "fhconv");
    });
    return result;
}


SAM_EXPORT double SAM_Pvwattsv1_PVWatts_gamma_nget(SAM_Pvwattsv1 ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "gamma", &result))
            make_access_error("SAM_Pvwattsv1", "gamma");
    });
    return result;
}


SAM_EXPORT double SAM_Pvwattsv1_PVWatts_gcr_nget(SAM_Pvwattsv1 ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "gcr", &result))
            make_access_error("SAM_Pvwattsv1", "gcr");
    });
    return result;
}


SAM_EXPORT double SAM_Pvwattsv1_PVWatts_i_ref_nget(SAM_Pvwattsv1 ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "i_ref", &result))
            make_access_error("SAM_Pvwattsv1", "i_ref");
    });
    return result;
}


SAM_EXPORT double SAM_Pvwattsv1_PVWatts_inoct_nget(SAM_Pvwattsv1 ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "inoct", &result))
            make_access_error("SAM_Pvwattsv1", "inoct");
    });
    return result;
}


SAM_EXPORT double SAM_Pvwattsv1_PVWatts_inv_eff_nget(SAM_Pvwattsv1 ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "inv_eff", &result))
            make_access_error("SAM_Pvwattsv1", "inv_eff");
    });
    return result;
}


SAM_EXPORT double SAM_Pvwattsv1_PVWatts_poa_cutin_nget(SAM_Pvwattsv1 ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "poa_cutin", &result))
            make_access_error("SAM_Pvwattsv1", "poa_cutin");
    });
    return result;
}


SAM_EXPORT double SAM_Pvwattsv1_PVWatts_rotlim_nget(SAM_Pvwattsv1 ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "rotlim", &result))
            make_access_error("SAM_Pvwattsv1", "rotlim");
    });
    return result;
}


SAM_EXPORT double SAM_Pvwattsv1_PVWatts_shade_mode_1x_nget(SAM_Pvwattsv1 ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "shade_mode_1x", &result))
            make_access_error("SAM_Pvwattsv1", "shade_mode_1x");
    });
    return result;
}


SAM_EXPORT double *SAM_Pvwattsv1_PVWatts_shading_azal_mget(SAM_Pvwattsv1 ptr, int *nrows, int *ncols, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_matrix(ptr, "shading:azal", nrows, ncols);
        if (!result)
            make_access_error("SAM_Pvwattsv1", "shading:azal");
    });
    return result;
}


SAM_EXPORT double SAM_Pvwattsv1_PVWatts_shading_diff_nget(SAM_Pvwattsv1 ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "shading:diff", &result))
            make_access_error("SAM_Pvwattsv1", "shading:diff");
    });
    return result;
}


SAM_EXPORT double *SAM_Pvwattsv1_PVWatts_shading_mxh_mget(SAM_Pvwattsv1 ptr, int *nrows, int *ncols, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_matrix(ptr, "shading:mxh", nrows, ncols);
        if (!result)
            make_access_error("SAM_Pvwattsv1", "shading:mxh");
    });
    return result;
}


SAM_EXPORT double *
SAM_Pvwattsv1_PVWatts_shading_timestep_mget(SAM_Pvwattsv1 ptr, int *nrows, int *ncols, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_matrix(ptr, "shading:timestep", nrows, ncols);
        if (!result)
            make_access_error("SAM_Pvwattsv1", "shading:timestep");
    });
    return result;
}


SAM_EXPORT double SAM_Pvwattsv1_PVWatts_system_size_nget(SAM_Pvwattsv1 ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "system_size", &result))
            make_access_error("SAM_Pvwattsv1", "system_size");
    });
    return result;
}


SAM_EXPORT double SAM_Pvwattsv1_PVWatts_tilt_nget(SAM_Pvwattsv1 ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "tilt", &result))
            make_access_error("SAM_Pvwattsv1", "tilt");
    });
    return result;
}


SAM_EXPORT double SAM_Pvwattsv1_PVWatts_tilt_eq_lat_nget(SAM_Pvwattsv1 ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "tilt_eq_lat", &result))
            make_access_error("SAM_Pvwattsv1", "tilt_eq_lat");
    });
    return result;
}


SAM_EXPORT double SAM_Pvwattsv1_PVWatts_track_mode_nget(SAM_Pvwattsv1 ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "track_mode", &result))
            make_access_error("SAM_Pvwattsv1", "track_mode");
    });
    return result;
}


SAM_EXPORT double SAM_Pvwattsv1_PVWatts_tref_nget(SAM_Pvwattsv1 ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "tref", &result))
            make_access_error("SAM_Pvwattsv1", "tref");
    });
    return result;
}


SAM_EXPORT double SAM_Pvwattsv1_PVWatts_u0_nget(SAM_Pvwattsv1 ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "u0", &result))
            make_access_error("SAM_Pvwattsv1", "u0");
    });
    return result;
}


SAM_EXPORT double SAM_Pvwattsv1_PVWatts_u1_nget(SAM_Pvwattsv1 ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "u1", &result))
            make_access_error("SAM_Pvwattsv1", "u1");
    });
    return result;
}


SAM_EXPORT double *SAM_Pvwattsv1_PVWatts_user_poa_aget(SAM_Pvwattsv1 ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "user_poa", length);
        if (!result)
            make_access_error("SAM_Pvwattsv1", "user_poa");
    });
    return result;
}


SAM_EXPORT double SAM_Pvwattsv1_PVWatts_w_stow_nget(SAM_Pvwattsv1 ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "w_stow", &result))
            make_access_error("SAM_Pvwattsv1", "w_stow");
    });
    return result;
}


SAM_EXPORT double *SAM_Pvwattsv1_Outputs_ac_aget(SAM_Pvwattsv1 ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "ac", length);
        if (!result)
            make_access_error("SAM_Pvwattsv1", "ac");
    });
    return result;
}


SAM_EXPORT double SAM_Pvwattsv1_Outputs_ac_annual_nget(SAM_Pvwattsv1 ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "ac_annual", &result))
            make_access_error("SAM_Pvwattsv1", "ac_annual");
    });
    return result;
}


SAM_EXPORT double *SAM_Pvwattsv1_Outputs_ac_monthly_aget(SAM_Pvwattsv1 ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "ac_monthly", length);
        if (!result)
            make_access_error("SAM_Pvwattsv1", "ac_monthly");
    });
    return result;
}


SAM_EXPORT double SAM_Pvwattsv1_Outputs_annual_energy_nget(SAM_Pvwattsv1 ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "annual_energy", &result))
            make_access_error("SAM_Pvwattsv1", "annual_energy");
    });
    return result;
}


SAM_EXPORT const char *SAM_Pvwattsv1_Outputs_city_sget(SAM_Pvwattsv1 ptr, SAM_error *err) {
    const char *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_string(ptr, "city");
        if (!result)
            make_access_error("SAM_Pvwattsv1", "city");
    });
    return result;
}


SAM_EXPORT double *SAM_Pvwattsv1_Outputs_dc_aget(SAM_Pvwattsv1 ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "dc", length);
        if (!result)
            make_access_error("SAM_Pvwattsv1", "dc");
    });
    return result;
}


SAM_EXPORT double *SAM_Pvwattsv1_Outputs_dc_monthly_aget(SAM_Pvwattsv1 ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "dc_monthly", length);
        if (!result)
            make_access_error("SAM_Pvwattsv1", "dc_monthly");
    });
    return result;
}


SAM_EXPORT double *SAM_Pvwattsv1_Outputs_df_aget(SAM_Pvwattsv1 ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "df", length);
        if (!result)
            make_access_error("SAM_Pvwattsv1", "df");
    });
    return result;
}


SAM_EXPORT double *SAM_Pvwattsv1_Outputs_dn_aget(SAM_Pvwattsv1 ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "dn", length);
        if (!result)
            make_access_error("SAM_Pvwattsv1", "dn");
    });
    return result;
}


SAM_EXPORT double SAM_Pvwattsv1_Outputs_elev_nget(SAM_Pvwattsv1 ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "elev", &result))
            make_access_error("SAM_Pvwattsv1", "elev");
    });
    return result;
}


SAM_EXPORT double *SAM_Pvwattsv1_Outputs_gen_aget(SAM_Pvwattsv1 ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "gen", length);
        if (!result)
            make_access_error("SAM_Pvwattsv1", "gen");
    });
    return result;
}


SAM_EXPORT double *SAM_Pvwattsv1_Outputs_gh_aget(SAM_Pvwattsv1 ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "gh", length);
        if (!result)
            make_access_error("SAM_Pvwattsv1", "gh");
    });
    return result;
}


SAM_EXPORT double SAM_Pvwattsv1_Outputs_lat_nget(SAM_Pvwattsv1 ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "lat", &result))
            make_access_error("SAM_Pvwattsv1", "lat");
    });
    return result;
}


SAM_EXPORT const char *SAM_Pvwattsv1_Outputs_location_sget(SAM_Pvwattsv1 ptr, SAM_error *err) {
    const char *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_string(ptr, "location");
        if (!result)
            make_access_error("SAM_Pvwattsv1", "location");
    });
    return result;
}


SAM_EXPORT double SAM_Pvwattsv1_Outputs_lon_nget(SAM_Pvwattsv1 ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "lon", &result))
            make_access_error("SAM_Pvwattsv1", "lon");
    });
    return result;
}


SAM_EXPORT double *SAM_Pvwattsv1_Outputs_monthly_energy_aget(SAM_Pvwattsv1 ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "monthly_energy", length);
        if (!result)
            make_access_error("SAM_Pvwattsv1", "monthly_energy");
    });
    return result;
}


SAM_EXPORT double *SAM_Pvwattsv1_Outputs_poa_aget(SAM_Pvwattsv1 ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "poa", length);
        if (!result)
            make_access_error("SAM_Pvwattsv1", "poa");
    });
    return result;
}


SAM_EXPORT double *SAM_Pvwattsv1_Outputs_poa_monthly_aget(SAM_Pvwattsv1 ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "poa_monthly", length);
        if (!result)
            make_access_error("SAM_Pvwattsv1", "poa_monthly");
    });
    return result;
}


SAM_EXPORT double *SAM_Pvwattsv1_Outputs_shad_beam_factor_aget(SAM_Pvwattsv1 ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "shad_beam_factor", length);
        if (!result)
            make_access_error("SAM_Pvwattsv1", "shad_beam_factor");
    });
    return result;
}


SAM_EXPORT double SAM_Pvwattsv1_Outputs_solrad_annual_nget(SAM_Pvwattsv1 ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "solrad_annual", &result))
            make_access_error("SAM_Pvwattsv1", "solrad_annual");
    });
    return result;
}


SAM_EXPORT double *SAM_Pvwattsv1_Outputs_solrad_monthly_aget(SAM_Pvwattsv1 ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "solrad_monthly", length);
        if (!result)
            make_access_error("SAM_Pvwattsv1", "solrad_monthly");
    });
    return result;
}


SAM_EXPORT const char *SAM_Pvwattsv1_Outputs_state_sget(SAM_Pvwattsv1 ptr, SAM_error *err) {
    const char *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_string(ptr, "state");
        if (!result)
            make_access_error("SAM_Pvwattsv1", "state");
    });
    return result;
}


SAM_EXPORT double *SAM_Pvwattsv1_Outputs_sunup_aget(SAM_Pvwattsv1 ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "sunup", length);
        if (!result)
            make_access_error("SAM_Pvwattsv1", "sunup");
    });
    return result;
}


SAM_EXPORT double *SAM_Pvwattsv1_Outputs_tamb_aget(SAM_Pvwattsv1 ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "tamb", length);
        if (!result)
            make_access_error("SAM_Pvwattsv1", "tamb");
    });
    return result;
}


SAM_EXPORT double *SAM_Pvwattsv1_Outputs_tcell_aget(SAM_Pvwattsv1 ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "tcell", length);
        if (!result)
            make_access_error("SAM_Pvwattsv1", "tcell");
    });
    return result;
}


SAM_EXPORT double *SAM_Pvwattsv1_Outputs_tdew_aget(SAM_Pvwattsv1 ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "tdew", length);
        if (!result)
            make_access_error("SAM_Pvwattsv1", "tdew");
    });
    return result;
}


SAM_EXPORT double *SAM_Pvwattsv1_Outputs_tpoa_aget(SAM_Pvwattsv1 ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "tpoa", length);
        if (!result)
            make_access_error("SAM_Pvwattsv1", "tpoa");
    });
    return result;
}


SAM_EXPORT double SAM_Pvwattsv1_Outputs_tz_nget(SAM_Pvwattsv1 ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "tz", &result))
            make_access_error("SAM_Pvwattsv1", "tz");
    });
    return result;
}


SAM_EXPORT double *SAM_Pvwattsv1_Outputs_wspd_aget(SAM_Pvwattsv1 ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "wspd", length);
        if (!result)
            make_access_error("SAM_Pvwattsv1", "wspd");
    });
    return result;
}



