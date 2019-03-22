#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/cmod_generic_system-builder.h>
#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_GenericSystem.h"


SAM_EXPORT SAM_GenericSystem SAM_GenericSystem_construct(const char* def, SAM_error* err){
    SAM_GenericSystem result = nullptr;
    translateExceptions(err, [&]{
        
        result = ssc_data_create();
    });
    return result;
}

SAM_EXPORT void SAM_GenericSystem_destruct(SAM_GenericSystem gs_system)
{
	ssc_data_free(gs_system);
}

SAM_EXPORT void SAM_GenericSystem_PowerPlant_derate_set(SAM_GenericSystem ptr, float number, SAM_error* err){
    translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "derate", number);
    });
}

SAM_EXPORT void SAM_GenericSystem_PowerPlant_heat_rate_set(SAM_GenericSystem ptr, float number, SAM_error* err){
    translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "heat_rate", number);

    });
}


SAM_EXPORT float SAM_GenericSystem_PowerPlant_conv_eff_eval(SAM_GenericSystem ptr, SAM_error* err){
    float result = 0.f;
    translateExceptions(err, [&]{
        result = GenericSystem_conv_eff_eval(ptr);
    });
    return result;
}

SAM_EXPORT float SAM_GenericSystem_PowerPlant_derate_get(SAM_GenericSystem ptr, SAM_error* err){
    float result;
    translateExceptions(err, [&]{
        if (!ssc_data_get_number(ptr, "derate", &result))
            make_access_error("SAM_GenericSystem", "derate");
    });
    return result;
}
