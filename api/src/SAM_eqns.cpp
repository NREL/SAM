#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>
#include <ssc/cmod_windpower_eqns.h>
#include <ssc/cmod_pvsamv1_eqns.h>
#include <ssc/cmod_merchantplant_eqns.h>

#include "SAM_api.h"
#include "SAM_eqns.h"
#include "ErrorHandler.h"


SAM_EXPORT void SAM_windpower_turbine_powercurve_eqn(ssc_data_t data, SAM_error *err) {
    translateExceptions(err, [&] {
        Turbine_calculate_powercurve(data);
    });
}

SAM_EXPORT void SAM_Reopt_size_battery_post_eqn(ssc_data_t data, SAM_error *err) {
    translateExceptions(err, [&] {
        Reopt_size_battery_params(data);
    });
}

SAM_EXPORT void SAM_mp_ancillary_services_eqn(ssc_data_t data, SAM_error *err) {
    translateExceptions(err, [&] {
        mp_ancillary_services(data);
    });
}
