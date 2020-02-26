#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_GenericSystem.h"

SAM_EXPORT SAM_GenericSystem SAM_GenericSystem_construct(const char *def, SAM_error *err) {
    SAM_GenericSystem result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_create();
    });
    return result;
}

SAM_EXPORT int SAM_GenericSystem_execute(SAM_GenericSystem data, int verbosity, SAM_error *err) {
    int n_err = 0;
    translateExceptions(err, [&] {
        n_err += SAM_module_exec("generic_system", data, verbosity, err);
    });
    return n_err;
}


SAM_EXPORT void SAM_GenericSystem_destruct(SAM_GenericSystem system) {
    ssc_data_free(system);
}

SAM_EXPORT void SAM_GenericSystem_Plant_conv_eff_nset(SAM_GenericSystem ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "conv_eff", number);
    });
}

SAM_EXPORT void SAM_GenericSystem_Plant_derate_nset(SAM_GenericSystem ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "derate", number);
    });
}

SAM_EXPORT void
SAM_GenericSystem_Plant_energy_output_array_aset(SAM_GenericSystem ptr, double *arr, int length, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_array(ptr, "energy_output_array", arr, length);
    });
}

SAM_EXPORT void SAM_GenericSystem_Plant_heat_rate_nset(SAM_GenericSystem ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "heat_rate", number);
    });
}

SAM_EXPORT void SAM_GenericSystem_Plant_spec_mode_nset(SAM_GenericSystem ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "spec_mode", number);
    });
}

SAM_EXPORT void SAM_GenericSystem_Plant_system_capacity_nset(SAM_GenericSystem ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "system_capacity", number);
    });
}

SAM_EXPORT void
SAM_GenericSystem_Plant_user_capacity_factor_nset(SAM_GenericSystem ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "user_capacity_factor", number);
    });
}

SAM_EXPORT void SAM_GenericSystem_Lifetime_analysis_period_nset(SAM_GenericSystem ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "analysis_period", number);
    });
}

SAM_EXPORT void
SAM_GenericSystem_Lifetime_generic_degradation_aset(SAM_GenericSystem ptr, double *arr, int length, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_array(ptr, "generic_degradation", arr, length);
    });
}

SAM_EXPORT void
SAM_GenericSystem_Lifetime_system_use_lifetime_output_nset(SAM_GenericSystem ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "system_use_lifetime_output", number);
    });
}

SAM_EXPORT double SAM_GenericSystem_Plant_conv_eff_nget(SAM_GenericSystem ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "conv_eff", &result))
            make_access_error("SAM_GenericSystem", "conv_eff");
    });
    return result;
}


SAM_EXPORT double SAM_GenericSystem_Plant_derate_nget(SAM_GenericSystem ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "derate", &result))
            make_access_error("SAM_GenericSystem", "derate");
    });
    return result;
}


SAM_EXPORT double *
SAM_GenericSystem_Plant_energy_output_array_aget(SAM_GenericSystem ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "energy_output_array", length);
        if (!result)
            make_access_error("SAM_GenericSystem", "energy_output_array");
    });
    return result;
}


SAM_EXPORT double SAM_GenericSystem_Plant_heat_rate_nget(SAM_GenericSystem ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "heat_rate", &result))
            make_access_error("SAM_GenericSystem", "heat_rate");
    });
    return result;
}


SAM_EXPORT double SAM_GenericSystem_Plant_spec_mode_nget(SAM_GenericSystem ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "spec_mode", &result))
            make_access_error("SAM_GenericSystem", "spec_mode");
    });
    return result;
}


SAM_EXPORT double SAM_GenericSystem_Plant_system_capacity_nget(SAM_GenericSystem ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "system_capacity", &result))
            make_access_error("SAM_GenericSystem", "system_capacity");
    });
    return result;
}


SAM_EXPORT double SAM_GenericSystem_Plant_user_capacity_factor_nget(SAM_GenericSystem ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "user_capacity_factor", &result))
            make_access_error("SAM_GenericSystem", "user_capacity_factor");
    });
    return result;
}


SAM_EXPORT double SAM_GenericSystem_Lifetime_analysis_period_nget(SAM_GenericSystem ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "analysis_period", &result))
            make_access_error("SAM_GenericSystem", "analysis_period");
    });
    return result;
}


SAM_EXPORT double *
SAM_GenericSystem_Lifetime_generic_degradation_aget(SAM_GenericSystem ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "generic_degradation", length);
        if (!result)
            make_access_error("SAM_GenericSystem", "generic_degradation");
    });
    return result;
}


SAM_EXPORT double SAM_GenericSystem_Lifetime_system_use_lifetime_output_nget(SAM_GenericSystem ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "system_use_lifetime_output", &result))
            make_access_error("SAM_GenericSystem", "system_use_lifetime_output");
    });
    return result;
}


SAM_EXPORT double SAM_GenericSystem_Outputs_annual_energy_nget(SAM_GenericSystem ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "annual_energy", &result))
            make_access_error("SAM_GenericSystem", "annual_energy");
    });
    return result;
}


SAM_EXPORT double SAM_GenericSystem_Outputs_annual_fuel_usage_nget(SAM_GenericSystem ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "annual_fuel_usage", &result))
            make_access_error("SAM_GenericSystem", "annual_fuel_usage");
    });
    return result;
}


SAM_EXPORT double SAM_GenericSystem_Outputs_capacity_factor_nget(SAM_GenericSystem ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "capacity_factor", &result))
            make_access_error("SAM_GenericSystem", "capacity_factor");
    });
    return result;
}


SAM_EXPORT double *SAM_GenericSystem_Outputs_gen_aget(SAM_GenericSystem ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "gen", length);
        if (!result)
            make_access_error("SAM_GenericSystem", "gen");
    });
    return result;
}


SAM_EXPORT double SAM_GenericSystem_Outputs_kwh_per_kw_nget(SAM_GenericSystem ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "kwh_per_kw", &result))
            make_access_error("SAM_GenericSystem", "kwh_per_kw");
    });
    return result;
}


SAM_EXPORT double *SAM_GenericSystem_Outputs_monthly_energy_aget(SAM_GenericSystem ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "monthly_energy", length);
        if (!result)
            make_access_error("SAM_GenericSystem", "monthly_energy");
    });
    return result;
}


SAM_EXPORT double SAM_GenericSystem_Outputs_system_heat_rate_nget(SAM_GenericSystem ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "system_heat_rate", &result))
            make_access_error("SAM_GenericSystem", "system_heat_rate");
    });
    return result;
}


SAM_EXPORT double SAM_GenericSystem_Outputs_water_usage_nget(SAM_GenericSystem ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "water_usage", &result))
            make_access_error("SAM_GenericSystem", "water_usage");
    });
    return result;
}



