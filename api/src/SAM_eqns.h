#ifndef SYSTEM_ADVISOR_MODEL_SAM_EQNS_H
#define SYSTEM_ADVISOR_MODEL_SAM_EQNS_H

#include "visibility.h"
#include "SAM_api.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef void (*ssc_equation_ptr)(SAM_table data);

SAM_EXPORT void SAM_windpower_turbine_powercurve_eqn(SAM_table data, SAM_error *err);

SAM_EXPORT void SAM_Reopt_size_battery_post_eqn(SAM_table data, SAM_error *err);

SAM_EXPORT void SAM_mp_ancillary_services_eqn(SAM_table data, SAM_error *err);


#ifdef __cplusplus
} // extern "C"
#endif


#endif //SYSTEM_ADVISOR_MODEL_SAM_EQNS_H
