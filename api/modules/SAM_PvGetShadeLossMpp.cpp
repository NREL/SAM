#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_PvGetShadeLossMpp.h"

SAM_EXPORT SAM_PvGetShadeLossMpp SAM_PvGetShadeLossMpp_construct(const char *def, SAM_error *err) {
    SAM_PvGetShadeLossMpp result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_create();
    });
    return result;
}

SAM_EXPORT int SAM_PvGetShadeLossMpp_execute(SAM_PvGetShadeLossMpp data, int verbosity, SAM_error *err) {
    int n_err = 0;
    translateExceptions(err, [&] {
        n_err += SAM_module_exec("pv_get_shade_loss_mpp", data, verbosity, err);
    });
    return n_err;
}


SAM_EXPORT void SAM_PvGetShadeLossMpp_destruct(SAM_PvGetShadeLossMpp system) {
    ssc_data_free(system);
}

SAM_EXPORT void
SAM_PvGetShadeLossMpp_PVShadeLossDB_diffuse_irrad_aset(SAM_PvGetShadeLossMpp ptr, double *arr, int length,
                                                       SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_array(ptr, "diffuse_irrad", arr, length);
    });
}

SAM_EXPORT void
SAM_PvGetShadeLossMpp_PVShadeLossDB_global_poa_irrad_aset(SAM_PvGetShadeLossMpp ptr, double *arr, int length,
                                                          SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_array(ptr, "global_poa_irrad", arr, length);
    });
}

SAM_EXPORT void
SAM_PvGetShadeLossMpp_PVShadeLossDB_mods_per_string_aset(SAM_PvGetShadeLossMpp ptr, double *arr, int length,
                                                         SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_array(ptr, "mods_per_string", arr, length);
    });
}

SAM_EXPORT void
SAM_PvGetShadeLossMpp_PVShadeLossDB_pv_cell_temp_aset(SAM_PvGetShadeLossMpp ptr, double *arr, int length,
                                                      SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_array(ptr, "pv_cell_temp", arr, length);
    });
}

SAM_EXPORT void
SAM_PvGetShadeLossMpp_PVShadeLossDB_str_shade_fracs_mset(SAM_PvGetShadeLossMpp ptr, double *mat, int nrows, int ncols,
                                                         SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_matrix(ptr, "str_shade_fracs", mat, nrows, ncols);
    });
}

SAM_EXPORT void SAM_PvGetShadeLossMpp_PVShadeLossDB_str_vmp_stc_aset(SAM_PvGetShadeLossMpp ptr, double *arr, int length,
                                                                     SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_array(ptr, "str_vmp_stc", arr, length);
    });
}

SAM_EXPORT void SAM_PvGetShadeLossMpp_PVShadeLossDB_v_mppt_high_aset(SAM_PvGetShadeLossMpp ptr, double *arr, int length,
                                                                     SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_array(ptr, "v_mppt_high", arr, length);
    });
}

SAM_EXPORT void SAM_PvGetShadeLossMpp_PVShadeLossDB_v_mppt_low_aset(SAM_PvGetShadeLossMpp ptr, double *arr, int length,
                                                                    SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_array(ptr, "v_mppt_low", arr, length);
    });
}

SAM_EXPORT double *
SAM_PvGetShadeLossMpp_PVShadeLossDB_diffuse_irrad_aget(SAM_PvGetShadeLossMpp ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "diffuse_irrad", length);
        if (!result)
            make_access_error("SAM_PvGetShadeLossMpp", "diffuse_irrad");
    });
    return result;
}


SAM_EXPORT double *
SAM_PvGetShadeLossMpp_PVShadeLossDB_global_poa_irrad_aget(SAM_PvGetShadeLossMpp ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "global_poa_irrad", length);
        if (!result)
            make_access_error("SAM_PvGetShadeLossMpp", "global_poa_irrad");
    });
    return result;
}


SAM_EXPORT double *
SAM_PvGetShadeLossMpp_PVShadeLossDB_mods_per_string_aget(SAM_PvGetShadeLossMpp ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "mods_per_string", length);
        if (!result)
            make_access_error("SAM_PvGetShadeLossMpp", "mods_per_string");
    });
    return result;
}


SAM_EXPORT double *
SAM_PvGetShadeLossMpp_PVShadeLossDB_pv_cell_temp_aget(SAM_PvGetShadeLossMpp ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "pv_cell_temp", length);
        if (!result)
            make_access_error("SAM_PvGetShadeLossMpp", "pv_cell_temp");
    });
    return result;
}


SAM_EXPORT double *
SAM_PvGetShadeLossMpp_PVShadeLossDB_str_shade_fracs_mget(SAM_PvGetShadeLossMpp ptr, int *nrows, int *ncols,
                                                         SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_matrix(ptr, "str_shade_fracs", nrows, ncols);
        if (!result)
            make_access_error("SAM_PvGetShadeLossMpp", "str_shade_fracs");
    });
    return result;
}


SAM_EXPORT double *
SAM_PvGetShadeLossMpp_PVShadeLossDB_str_vmp_stc_aget(SAM_PvGetShadeLossMpp ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "str_vmp_stc", length);
        if (!result)
            make_access_error("SAM_PvGetShadeLossMpp", "str_vmp_stc");
    });
    return result;
}


SAM_EXPORT double *
SAM_PvGetShadeLossMpp_PVShadeLossDB_v_mppt_high_aget(SAM_PvGetShadeLossMpp ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "v_mppt_high", length);
        if (!result)
            make_access_error("SAM_PvGetShadeLossMpp", "v_mppt_high");
    });
    return result;
}


SAM_EXPORT double *
SAM_PvGetShadeLossMpp_PVShadeLossDB_v_mppt_low_aget(SAM_PvGetShadeLossMpp ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "v_mppt_low", length);
        if (!result)
            make_access_error("SAM_PvGetShadeLossMpp", "v_mppt_low");
    });
    return result;
}


SAM_EXPORT double *SAM_PvGetShadeLossMpp_Outputs_N_aget(SAM_PvGetShadeLossMpp ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "N", length);
        if (!result)
            make_access_error("SAM_PvGetShadeLossMpp", "N");
    });
    return result;
}


SAM_EXPORT double *SAM_PvGetShadeLossMpp_Outputs_S_aget(SAM_PvGetShadeLossMpp ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "S", length);
        if (!result)
            make_access_error("SAM_PvGetShadeLossMpp", "S");
    });
    return result;
}


SAM_EXPORT double *SAM_PvGetShadeLossMpp_Outputs_d_aget(SAM_PvGetShadeLossMpp ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "d", length);
        if (!result)
            make_access_error("SAM_PvGetShadeLossMpp", "d");
    });
    return result;
}


SAM_EXPORT double *
SAM_PvGetShadeLossMpp_Outputs_shade_loss_aget(SAM_PvGetShadeLossMpp ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "shade_loss", length);
        if (!result)
            make_access_error("SAM_PvGetShadeLossMpp", "shade_loss");
    });
    return result;
}


SAM_EXPORT double *SAM_PvGetShadeLossMpp_Outputs_t_aget(SAM_PvGetShadeLossMpp ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "t", length);
        if (!result)
            make_access_error("SAM_PvGetShadeLossMpp", "t");
    });
    return result;
}



