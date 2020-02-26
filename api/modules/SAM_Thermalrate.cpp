#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_Thermalrate.h"

SAM_EXPORT SAM_Thermalrate SAM_Thermalrate_construct(const char *def, SAM_error *err) {
    SAM_Thermalrate result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_create();
    });
    return result;
}

SAM_EXPORT int SAM_Thermalrate_execute(SAM_Thermalrate data, int verbosity, SAM_error *err) {
    int n_err = 0;
    translateExceptions(err, [&] {
        n_err += SAM_module_exec("thermalrate", data, verbosity, err);
    });
    return n_err;
}


SAM_EXPORT void SAM_Thermalrate_destruct(SAM_Thermalrate system) {
    ssc_data_free(system);
}

SAM_EXPORT void SAM_Thermalrate_ThermalRate_en_thermal_rates_nset(SAM_Thermalrate ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "en_thermal_rates", number);
    });
}

SAM_EXPORT void
SAM_Thermalrate_ThermalRate_fuelcell_power_thermal_aset(SAM_Thermalrate ptr, double *arr, int length, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_array(ptr, "fuelcell_power_thermal", arr, length);
    });
}

SAM_EXPORT void
SAM_Thermalrate_ThermalRate_thermal_buy_rate_aset(SAM_Thermalrate ptr, double *arr, int length, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_array(ptr, "thermal_buy_rate", arr, length);
    });
}

SAM_EXPORT void
SAM_Thermalrate_ThermalRate_thermal_buy_rate_flat_nset(SAM_Thermalrate ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "thermal_buy_rate_flat", number);
    });
}

SAM_EXPORT void
SAM_Thermalrate_ThermalRate_thermal_buy_rate_option_nset(SAM_Thermalrate ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "thermal_buy_rate_option", number);
    });
}

SAM_EXPORT void
SAM_Thermalrate_ThermalRate_thermal_degradation_aset(SAM_Thermalrate ptr, double *arr, int length, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_array(ptr, "thermal_degradation", arr, length);
    });
}

SAM_EXPORT void
SAM_Thermalrate_ThermalRate_thermal_load_aset(SAM_Thermalrate ptr, double *arr, int length, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_array(ptr, "thermal_load", arr, length);
    });
}

SAM_EXPORT void
SAM_Thermalrate_ThermalRate_thermal_load_escalation_aset(SAM_Thermalrate ptr, double *arr, int length, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_array(ptr, "thermal_load_escalation", arr, length);
    });
}

SAM_EXPORT void
SAM_Thermalrate_ThermalRate_thermal_rate_escalation_aset(SAM_Thermalrate ptr, double *arr, int length, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_array(ptr, "thermal_rate_escalation", arr, length);
    });
}

SAM_EXPORT void
SAM_Thermalrate_ThermalRate_thermal_sell_rate_aset(SAM_Thermalrate ptr, double *arr, int length, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_array(ptr, "thermal_sell_rate", arr, length);
    });
}

SAM_EXPORT void
SAM_Thermalrate_ThermalRate_thermal_sell_rate_flat_nset(SAM_Thermalrate ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "thermal_sell_rate_flat", number);
    });
}

SAM_EXPORT void
SAM_Thermalrate_ThermalRate_thermal_sell_rate_option_nset(SAM_Thermalrate ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "thermal_sell_rate_option", number);
    });
}

SAM_EXPORT void SAM_Thermalrate_Lifetime_analysis_period_nset(SAM_Thermalrate ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "analysis_period", number);
    });
}

SAM_EXPORT void SAM_Thermalrate_Lifetime_inflation_rate_nset(SAM_Thermalrate ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "inflation_rate", number);
    });
}

SAM_EXPORT void
SAM_Thermalrate_Lifetime_system_use_lifetime_output_nset(SAM_Thermalrate ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "system_use_lifetime_output", number);
    });
}

SAM_EXPORT double SAM_Thermalrate_ThermalRate_en_thermal_rates_nget(SAM_Thermalrate ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "en_thermal_rates", &result))
            make_access_error("SAM_Thermalrate", "en_thermal_rates");
    });
    return result;
}


SAM_EXPORT double *
SAM_Thermalrate_ThermalRate_fuelcell_power_thermal_aget(SAM_Thermalrate ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "fuelcell_power_thermal", length);
        if (!result)
            make_access_error("SAM_Thermalrate", "fuelcell_power_thermal");
    });
    return result;
}


SAM_EXPORT double *SAM_Thermalrate_ThermalRate_thermal_buy_rate_aget(SAM_Thermalrate ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "thermal_buy_rate", length);
        if (!result)
            make_access_error("SAM_Thermalrate", "thermal_buy_rate");
    });
    return result;
}


SAM_EXPORT double SAM_Thermalrate_ThermalRate_thermal_buy_rate_flat_nget(SAM_Thermalrate ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "thermal_buy_rate_flat", &result))
            make_access_error("SAM_Thermalrate", "thermal_buy_rate_flat");
    });
    return result;
}


SAM_EXPORT double SAM_Thermalrate_ThermalRate_thermal_buy_rate_option_nget(SAM_Thermalrate ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "thermal_buy_rate_option", &result))
            make_access_error("SAM_Thermalrate", "thermal_buy_rate_option");
    });
    return result;
}


SAM_EXPORT double *
SAM_Thermalrate_ThermalRate_thermal_degradation_aget(SAM_Thermalrate ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "thermal_degradation", length);
        if (!result)
            make_access_error("SAM_Thermalrate", "thermal_degradation");
    });
    return result;
}


SAM_EXPORT double *SAM_Thermalrate_ThermalRate_thermal_load_aget(SAM_Thermalrate ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "thermal_load", length);
        if (!result)
            make_access_error("SAM_Thermalrate", "thermal_load");
    });
    return result;
}


SAM_EXPORT double *
SAM_Thermalrate_ThermalRate_thermal_load_escalation_aget(SAM_Thermalrate ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "thermal_load_escalation", length);
        if (!result)
            make_access_error("SAM_Thermalrate", "thermal_load_escalation");
    });
    return result;
}


SAM_EXPORT double *
SAM_Thermalrate_ThermalRate_thermal_rate_escalation_aget(SAM_Thermalrate ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "thermal_rate_escalation", length);
        if (!result)
            make_access_error("SAM_Thermalrate", "thermal_rate_escalation");
    });
    return result;
}


SAM_EXPORT double *
SAM_Thermalrate_ThermalRate_thermal_sell_rate_aget(SAM_Thermalrate ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "thermal_sell_rate", length);
        if (!result)
            make_access_error("SAM_Thermalrate", "thermal_sell_rate");
    });
    return result;
}


SAM_EXPORT double SAM_Thermalrate_ThermalRate_thermal_sell_rate_flat_nget(SAM_Thermalrate ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "thermal_sell_rate_flat", &result))
            make_access_error("SAM_Thermalrate", "thermal_sell_rate_flat");
    });
    return result;
}


SAM_EXPORT double SAM_Thermalrate_ThermalRate_thermal_sell_rate_option_nget(SAM_Thermalrate ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "thermal_sell_rate_option", &result))
            make_access_error("SAM_Thermalrate", "thermal_sell_rate_option");
    });
    return result;
}


SAM_EXPORT double SAM_Thermalrate_Lifetime_analysis_period_nget(SAM_Thermalrate ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "analysis_period", &result))
            make_access_error("SAM_Thermalrate", "analysis_period");
    });
    return result;
}


SAM_EXPORT double SAM_Thermalrate_Lifetime_inflation_rate_nget(SAM_Thermalrate ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "inflation_rate", &result))
            make_access_error("SAM_Thermalrate", "inflation_rate");
    });
    return result;
}


SAM_EXPORT double SAM_Thermalrate_Lifetime_system_use_lifetime_output_nget(SAM_Thermalrate ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "system_use_lifetime_output", &result))
            make_access_error("SAM_Thermalrate", "system_use_lifetime_output");
    });
    return result;
}


SAM_EXPORT double SAM_Thermalrate_Outputs_thermal_cost_with_system_year1_nget(SAM_Thermalrate ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "thermal_cost_with_system_year1", &result))
            make_access_error("SAM_Thermalrate", "thermal_cost_with_system_year1");
    });
    return result;
}


SAM_EXPORT double SAM_Thermalrate_Outputs_thermal_cost_without_system_year1_nget(SAM_Thermalrate ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "thermal_cost_without_system_year1", &result))
            make_access_error("SAM_Thermalrate", "thermal_cost_without_system_year1");
    });
    return result;
}


SAM_EXPORT double SAM_Thermalrate_Outputs_thermal_load_year1_nget(SAM_Thermalrate ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "thermal_load_year1", &result))
            make_access_error("SAM_Thermalrate", "thermal_load_year1");
    });
    return result;
}


SAM_EXPORT double *
SAM_Thermalrate_Outputs_thermal_revenue_with_system_aget(SAM_Thermalrate ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "thermal_revenue_with_system", length);
        if (!result)
            make_access_error("SAM_Thermalrate", "thermal_revenue_with_system");
    });
    return result;
}


SAM_EXPORT double *
SAM_Thermalrate_Outputs_thermal_revenue_without_system_aget(SAM_Thermalrate ptr, int *length, SAM_error *err) {
    double *result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_get_array(ptr, "thermal_revenue_without_system", length);
        if (!result)
            make_access_error("SAM_Thermalrate", "thermal_revenue_without_system");
    });
    return result;
}


SAM_EXPORT double SAM_Thermalrate_Outputs_thermal_savings_year1_nget(SAM_Thermalrate ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "thermal_savings_year1", &result))
            make_access_error("SAM_Thermalrate", "thermal_savings_year1");
    });
    return result;
}



