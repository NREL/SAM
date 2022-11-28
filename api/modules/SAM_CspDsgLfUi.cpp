#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_CspDsgLfUi.h"

SAM_EXPORT int SAM_CspDsgLfUi_execute(SAM_table data, int verbosity, SAM_error* err){
	return SAM_module_exec("csp_dsg_lf_ui", data, verbosity, err);
}

SAM_EXPORT void SAM_CspDsgLfUi_Common_P_boil_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "P_boil", number);
	});
}

SAM_EXPORT void SAM_CspDsgLfUi_Common_deltaT_subcooled_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "deltaT_subcooled", number);
	});
}

SAM_EXPORT void SAM_CspDsgLfUi_Common_use_quality_or_subcooled_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "use_quality_or_subcooled", number);
	});
}

SAM_EXPORT double SAM_CspDsgLfUi_Common_P_boil_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_boil", &result))
		make_access_error("SAM_CspDsgLfUi", "P_boil");
	});
	return result;
}



SAM_EXPORT double SAM_CspDsgLfUi_Common_deltaT_subcooled_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "deltaT_subcooled", &result))
		make_access_error("SAM_CspDsgLfUi", "deltaT_subcooled");
	});
	return result;
}



SAM_EXPORT double SAM_CspDsgLfUi_Common_use_quality_or_subcooled_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "use_quality_or_subcooled", &result))
		make_access_error("SAM_CspDsgLfUi", "use_quality_or_subcooled");
	});
	return result;
}



SAM_EXPORT double SAM_CspDsgLfUi_Outputs_T_hot_out_target_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_hot_out_target", &result))
		make_access_error("SAM_CspDsgLfUi", "T_hot_out_target");
	});
	return result;
}



SAM_EXPORT double SAM_CspDsgLfUi_Outputs_T_saturation_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_saturation", &result))
		make_access_error("SAM_CspDsgLfUi", "T_saturation");
	});
	return result;
}



