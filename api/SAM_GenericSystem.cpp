#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/vartab.h>
#include <ssc/cmod_generic_system-builder.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_GenericSystem.h"


SAM_EXPORT SAM_GenericSystem SAM_GenericSystem_construct(const char* def, SAM_error* err){
    SAM_GenericSystem result = nullptr;
    translateExceptions(err, [&]{
        if (std::strcmp(def, "Res")){

        }
        result = new var_table;
    });
    return result;
}

void SAM_GenericSystem_destruct(SAM_GenericSystem gs_system)
{
    var_table *vt = static_cast<var_table*>(gs_system);
    if (vt) delete vt;
}

void SAM_GenericSystem_PowerPlant_derate_set(SAM_GenericSystem ptr, float number, SAM_error* err){
    translateExceptions(err, [&]{
        var_table* vt = static_cast<var_table*>(ptr);
        vt->assign("derate", number);
    });
}

void SAM_GenericSystem_PowerPlant_heat_rate_set(SAM_GenericSystem ptr, float number, SAM_error* err){
    translateExceptions(err, [&]{
        var_table* vt = static_cast<var_table*>(ptr);
        vt->assign("heat_rate", number);
    });
}


float SAM_GenericSystem_PowerPlant_conv_eff_eval(SAM_GenericSystem ptr, SAM_error* err){
    float result = 0.f;
    translateExceptions(err, [&]{
        var_table* vt = static_cast<var_table*>(ptr);
        result = GenericSystem_conv_eff_eval(vt);
    });
    return result;
}

SAM_EXPORT float SAM_GenericSystem_PowerPlant_derate_get(SAM_GenericSystem ptr, SAM_error* err){
    float result;
    translateExceptions(err, [&]{
        var_table* vt = static_cast<var_table*>(ptr);
        if (var_data* v = vt->lookup("derate")){
            result = v->num;
        }
        else
            make_access_error("SAM_GenericSystem", "derate");
    });
    return result;
}
