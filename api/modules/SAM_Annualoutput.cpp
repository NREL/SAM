#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_Annualoutput.h"

SAM_EXPORT SAM_Annualoutput SAM_Annualoutput_construct(const char *def, SAM_error *err) {
    SAM_Annualoutput result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_create();
    });
    return result;
}

SAM_EXPORT int SAM_Annualoutput_execute(SAM_Annualoutput data, int verbosity, SAM_error *err) {
    int n_err = 0;
    translateExceptions(err, [&] {
        n_err += SAM_module_exec("annualoutput", data, verbosity, err);
    });
    return n_err;
}


SAM_EXPORT void SAM_Annualoutput_destruct(SAM_Annualoutput system) {
    ssc_data_free(system);
}

SAM_EXPORT void
SAM_Annualoutput_AnnualOutput_analysis_period_nset(SAM_Annualoutput ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "analysis_period", number);
    });
}

SAM_EXPORT void
SAM_Annualoutput_AnnualOutput_energy_availability_aset(SAM_Annualoutput ptr, double *arr, int length, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_array(ptr, "energy_availability", arr, length);
    });
}

SAM_EXPORT void
SAM_Annualoutput_AnnualOutput_energy_curtailment_mset(SAM_Annualoutput ptr, double *mat, int nrows, int ncols,
                                                      SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_matrix(ptr, "energy_curtailment", mat, nrows, ncols);
    });
}

SAM_EXPORT void
SAM_Annualoutput_AnnualOutput_energy_degradation_aset(SAM_Annualoutput ptr, double *arr, int length, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_array(ptr, "energy_degradation", arr, length);
    });
}

SAM_EXPORT void
SAM_Annualoutput_AnnualOutput_system_hourly_energy_aset(SAM_Annualoutput ptr, double *arr, int length, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_array(ptr, "system_hourly_energy", arr, length);
    });
}

SAM_EXPORT void
SAM_Annualoutput_AnnualOutput_system_use_lifetime_output_nset(SAM_Annualoutput ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "system_use_lifetime_output", number);
    });
}

SAM_EXPORT double SAM_Annualoutput_AnnualOutput_analysis_period_nget(SAM_Annualoutput ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "analysis_period", &result))
            make_access_error("SAM_Annualoutput", "analysis_period");
    });
    return result;
}


SAM_EXPORT double *
SAM_Annualoutput_AnnualOutput_energy_availability_aget(SAM_Annualoutput ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "energy_availability", length);
        if (!result)
            make_access_error("SAM_Annualoutput", "energy_availability");
    });
    return result;
}


SAM_EXPORT double *
SAM_Annualoutput_AnnualOutput_energy_curtailment_mget(SAM_Annualoutput ptr, int *nrows, int *ncols, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_matrix(ptr, "energy_curtailment", nrows, ncols);
        if (!result)
            make_access_error("SAM_Annualoutput", "energy_curtailment");
    });
    return result;
}


SAM_EXPORT double *
SAM_Annualoutput_AnnualOutput_energy_degradation_aget(SAM_Annualoutput ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "energy_degradation", length);
        if (!result)
            make_access_error("SAM_Annualoutput", "energy_degradation");
    });
    return result;
}


SAM_EXPORT double *
SAM_Annualoutput_AnnualOutput_system_hourly_energy_aget(SAM_Annualoutput ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "system_hourly_energy", length);
        if (!result)
            make_access_error("SAM_Annualoutput", "system_hourly_energy");
    });
    return result;
}


SAM_EXPORT double SAM_Annualoutput_AnnualOutput_system_use_lifetime_output_nget(SAM_Annualoutput ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "system_use_lifetime_output", &result))
            make_access_error("SAM_Annualoutput", "system_use_lifetime_output");
    });
    return result;
}


SAM_EXPORT double *
SAM_Annualoutput_Outputs_annual_availability_aget(SAM_Annualoutput ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "annual_availability", length);
        if (!result)
            make_access_error("SAM_Annualoutput", "annual_availability");
    });
    return result;
}


SAM_EXPORT double *SAM_Annualoutput_Outputs_annual_degradation_aget(SAM_Annualoutput ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "annual_degradation", length);
        if (!result)
            make_access_error("SAM_Annualoutput", "annual_degradation");
    });
    return result;
}


SAM_EXPORT double *SAM_Annualoutput_Outputs_annual_energy_aget(SAM_Annualoutput ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "annual_energy", length);
        if (!result)
            make_access_error("SAM_Annualoutput", "annual_energy");
    });
    return result;
}


SAM_EXPORT double *SAM_Annualoutput_Outputs_hourly_energy_aget(SAM_Annualoutput ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "hourly_energy", length);
        if (!result)
            make_access_error("SAM_Annualoutput", "hourly_energy");
    });
    return result;
}


SAM_EXPORT double *SAM_Annualoutput_Outputs_monthly_energy_aget(SAM_Annualoutput ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "monthly_energy", length);
        if (!result)
            make_access_error("SAM_Annualoutput", "monthly_energy");
    });
    return result;
}



