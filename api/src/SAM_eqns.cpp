#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>
#include <ssc/cmod_windpower_eqns.h>

#include "SAM_api.h"
#include "SAM_eqns.h"
#include "ErrorHandler.h"


SAM_EXPORT void SAM_windpower_turbine_powercurve_eqn(ssc_data_t data, SAM_error* err){
    translateExceptions(err, [&]{
        Turbine_calculate_powercurve(data);
    });
}