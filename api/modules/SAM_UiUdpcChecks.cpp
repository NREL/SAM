#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_UiUdpcChecks.h"

SAM_EXPORT SAM_UiUdpcChecks SAM_UiUdpcChecks_construct(const char *def, SAM_error *err) {
    SAM_UiUdpcChecks result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_create();
    });
    return result;
}

SAM_EXPORT int SAM_UiUdpcChecks_execute(SAM_UiUdpcChecks data, int verbosity, SAM_error *err) {
    int n_err = 0;
    translateExceptions(err, [&] {
        n_err += SAM_module_exec("ui_udpc_checks", data, verbosity, err);
    });
    return n_err;
}


SAM_EXPORT void SAM_UiUdpcChecks_destruct(SAM_UiUdpcChecks system) {
    ssc_data_free(system);
}

SAM_EXPORT void
SAM_UiUdpcChecks_UserDefinedPowerCycle_ud_ind_od_mset(SAM_UiUdpcChecks ptr, double *mat, int nrows, int ncols,
                                                      SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_matrix(ptr, "ud_ind_od", mat, nrows, ncols);
    });
}

SAM_EXPORT double *
SAM_UiUdpcChecks_UserDefinedPowerCycle_ud_ind_od_mget(SAM_UiUdpcChecks ptr, int *nrows, int *ncols, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_matrix(ptr, "ud_ind_od", nrows, ncols);
        if (!result)
            make_access_error("SAM_UiUdpcChecks", "ud_ind_od");
    });
    return result;
}


SAM_EXPORT double SAM_UiUdpcChecks_Outputs_T_amb_des_nget(SAM_UiUdpcChecks ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "T_amb_des", &result))
            make_access_error("SAM_UiUdpcChecks", "T_amb_des");
    });
    return result;
}


SAM_EXPORT double SAM_UiUdpcChecks_Outputs_T_amb_high_nget(SAM_UiUdpcChecks ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "T_amb_high", &result))
            make_access_error("SAM_UiUdpcChecks", "T_amb_high");
    });
    return result;
}


SAM_EXPORT double SAM_UiUdpcChecks_Outputs_T_amb_low_nget(SAM_UiUdpcChecks ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "T_amb_low", &result))
            make_access_error("SAM_UiUdpcChecks", "T_amb_low");
    });
    return result;
}


SAM_EXPORT double SAM_UiUdpcChecks_Outputs_T_htf_des_nget(SAM_UiUdpcChecks ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "T_htf_des", &result))
            make_access_error("SAM_UiUdpcChecks", "T_htf_des");
    });
    return result;
}


SAM_EXPORT double SAM_UiUdpcChecks_Outputs_T_htf_high_nget(SAM_UiUdpcChecks ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "T_htf_high", &result))
            make_access_error("SAM_UiUdpcChecks", "T_htf_high");
    });
    return result;
}


SAM_EXPORT double SAM_UiUdpcChecks_Outputs_T_htf_low_nget(SAM_UiUdpcChecks ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "T_htf_low", &result))
            make_access_error("SAM_UiUdpcChecks", "T_htf_low");
    });
    return result;
}


SAM_EXPORT double SAM_UiUdpcChecks_Outputs_m_dot_des_nget(SAM_UiUdpcChecks ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "m_dot_des", &result))
            make_access_error("SAM_UiUdpcChecks", "m_dot_des");
    });
    return result;
}


SAM_EXPORT double SAM_UiUdpcChecks_Outputs_m_dot_high_nget(SAM_UiUdpcChecks ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "m_dot_high", &result))
            make_access_error("SAM_UiUdpcChecks", "m_dot_high");
    });
    return result;
}


SAM_EXPORT double SAM_UiUdpcChecks_Outputs_m_dot_low_nget(SAM_UiUdpcChecks ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "m_dot_low", &result))
            make_access_error("SAM_UiUdpcChecks", "m_dot_low");
    });
    return result;
}


SAM_EXPORT double SAM_UiUdpcChecks_Outputs_n_T_amb_pars_nget(SAM_UiUdpcChecks ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "n_T_amb_pars", &result))
            make_access_error("SAM_UiUdpcChecks", "n_T_amb_pars");
    });
    return result;
}


SAM_EXPORT double SAM_UiUdpcChecks_Outputs_n_T_htf_pars_nget(SAM_UiUdpcChecks ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "n_T_htf_pars", &result))
            make_access_error("SAM_UiUdpcChecks", "n_T_htf_pars");
    });
    return result;
}


SAM_EXPORT double SAM_UiUdpcChecks_Outputs_n_m_dot_pars_nget(SAM_UiUdpcChecks ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "n_m_dot_pars", &result))
            make_access_error("SAM_UiUdpcChecks", "n_m_dot_pars");
    });
    return result;
}



