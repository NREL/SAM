#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_Lcoefcr.h"

SAM_EXPORT SAM_Lcoefcr SAM_Lcoefcr_construct(const char *def, SAM_error *err) {
    SAM_Lcoefcr result = nullptr;
    translateExceptions(err, [&] {
        result = ssc_data_create();
    });
    return result;
}

SAM_EXPORT int SAM_Lcoefcr_execute(SAM_Lcoefcr data, int verbosity, SAM_error *err) {
    int n_err = 0;
    translateExceptions(err, [&] {
        n_err += SAM_module_exec("lcoefcr", data, verbosity, err);
    });
    return n_err;
}


SAM_EXPORT void SAM_Lcoefcr_destruct(SAM_Lcoefcr system) {
    ssc_data_free(system);
}

SAM_EXPORT void SAM_Lcoefcr_SimpleLCOE_annual_energy_nset(SAM_Lcoefcr ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "annual_energy", number);
    });
}

SAM_EXPORT void SAM_Lcoefcr_SimpleLCOE_capital_cost_nset(SAM_Lcoefcr ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "capital_cost", number);
    });
}

SAM_EXPORT void SAM_Lcoefcr_SimpleLCOE_fixed_charge_rate_nset(SAM_Lcoefcr ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "fixed_charge_rate", number);
    });
}

SAM_EXPORT void SAM_Lcoefcr_SimpleLCOE_fixed_operating_cost_nset(SAM_Lcoefcr ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "fixed_operating_cost", number);
    });
}

SAM_EXPORT void SAM_Lcoefcr_SimpleLCOE_variable_operating_cost_nset(SAM_Lcoefcr ptr, double number, SAM_error *err) {
    translateExceptions(err, [&] {
        ssc_data_set_number(ptr, "variable_operating_cost", number);
    });
}

SAM_EXPORT double SAM_Lcoefcr_SimpleLCOE_annual_energy_nget(SAM_Lcoefcr ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "annual_energy", &result))
            make_access_error("SAM_Lcoefcr", "annual_energy");
    });
    return result;
}


SAM_EXPORT double SAM_Lcoefcr_SimpleLCOE_capital_cost_nget(SAM_Lcoefcr ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "capital_cost", &result))
            make_access_error("SAM_Lcoefcr", "capital_cost");
    });
    return result;
}


SAM_EXPORT double SAM_Lcoefcr_SimpleLCOE_fixed_charge_rate_nget(SAM_Lcoefcr ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "fixed_charge_rate", &result))
            make_access_error("SAM_Lcoefcr", "fixed_charge_rate");
    });
    return result;
}


SAM_EXPORT double SAM_Lcoefcr_SimpleLCOE_fixed_operating_cost_nget(SAM_Lcoefcr ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "fixed_operating_cost", &result))
            make_access_error("SAM_Lcoefcr", "fixed_operating_cost");
    });
    return result;
}


SAM_EXPORT double SAM_Lcoefcr_SimpleLCOE_variable_operating_cost_nget(SAM_Lcoefcr ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "variable_operating_cost", &result))
            make_access_error("SAM_Lcoefcr", "variable_operating_cost");
    });
    return result;
}


SAM_EXPORT double SAM_Lcoefcr_Outputs_lcoe_fcr_nget(SAM_Lcoefcr ptr, SAM_error *err) {
    double result;
    translateExceptions(err, [&] {
        if (!ssc_data_get_number(ptr, "lcoe_fcr", &result))
            make_access_error("SAM_Lcoefcr", "lcoe_fcr");
    });
    return result;
}



