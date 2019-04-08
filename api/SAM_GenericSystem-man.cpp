#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/cmod_generic_system-builder.h>
#include <ssc/sscapi.h>

#include "include/SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_GenericSystem-man.h"


SAM_EXPORT SAM_GenericSystem SAM_GenericSystem_construct(const char* def, SAM_error* err){
    SAM_GenericSystem result = nullptr;
    translateExceptions(err, [&]{
        result = ssc_data_create();
    });
    return result;
}

SAM_EXPORT int SAM_GenericSystem_execute(SAM_GenericSystem data, int verbosity, SAM_error* err){
    int n_err = 0;
    translateExceptions(err, [&]{
        n_err += SAM_module_exec("generic_system", data, verbosity, err);
    });
    return n_err;
}


SAM_EXPORT void SAM_GenericSystem_destruct(SAM_GenericSystem gs_system)
{
	ssc_data_free(gs_system);
}

SAM_EXPORT void SAM_GenericSystem_PowerPlant_derate_fset(SAM_GenericSystem ptr, float number, SAM_error *err){
    translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "derate", number);
    });
}

SAM_EXPORT void SAM_GenericSystem_PowerPlant_heat_rate_set(SAM_GenericSystem ptr, float number, SAM_error* err){
    translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "heat_rate", number);

    });
}

SAM_EXPORT void SAM_GenericSystem_PowerPlant_energy_output_array_aset(SAM_GenericSystem ptr, float *array, int length,
                                                                      SAM_error *err){
    translateExceptions(err, [&]{
        ssc_data_set_array(ptr, "energy_output_array", array, length);

    });
}

SAM_EXPORT float SAM_GenericSystem_PowerPlant_conv_eff_eval(SAM_GenericSystem ptr, SAM_error* err){
    float result = 0.f;
    translateExceptions(err, [&]{
        result = GenericSystem_conv_eff_eval(ptr);
    });
    return result;
}

SAM_EXPORT float SAM_GenericSystem_PowerPlant_derate_fget(SAM_GenericSystem ptr, SAM_error *err){
    float result;
    translateExceptions(err, [&]{
        if (!ssc_data_get_number(ptr, "derate", &result))
            make_access_error("SAM_GenericSystem", "derate");
    });
    return result;
}

SAM_EXPORT float* SAM_GenericSystem_PowerPlant_energy_output_array_aget(SAM_GenericSystem ptr, int *length,
                                                                        SAM_error *err){
    float* result;
    translateExceptions(err, [&]{
        result = ssc_data_get_array(ptr, "energy_output_array", length);
        if (!result)
            make_access_error("SAM_GenericSystem", "energy_output_array");
    });
    return result;
}
