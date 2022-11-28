#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_Pvwattsv1Poa.h"

SAM_EXPORT int SAM_Pvwattsv1Poa_execute(SAM_table data, int verbosity, SAM_error* err){
	return SAM_module_exec("pvwattsv1_poa", data, verbosity, err);
}

SAM_EXPORT void SAM_Pvwattsv1Poa_Weather_beam_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "beam", arr, length);
	});
}

SAM_EXPORT void SAM_Pvwattsv1Poa_Weather_incidence_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "incidence", arr, length);
	});
}

SAM_EXPORT void SAM_Pvwattsv1Poa_Weather_poa_beam_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "poa_beam", arr, length);
	});
}

SAM_EXPORT void SAM_Pvwattsv1Poa_Weather_poa_gnddiff_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "poa_gnddiff", arr, length);
	});
}

SAM_EXPORT void SAM_Pvwattsv1Poa_Weather_poa_skydiff_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "poa_skydiff", arr, length);
	});
}

SAM_EXPORT void SAM_Pvwattsv1Poa_Weather_tdry_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "tdry", arr, length);
	});
}

SAM_EXPORT void SAM_Pvwattsv1Poa_Weather_wspd_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "wspd", arr, length);
	});
}

SAM_EXPORT void SAM_Pvwattsv1Poa_PVWatts_derate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "derate", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv1Poa_PVWatts_gamma_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "gamma", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv1Poa_PVWatts_inoct_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inoct", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv1Poa_PVWatts_inv_eff_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inv_eff", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv1Poa_PVWatts_step_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "step", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv1Poa_PVWatts_system_size_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "system_size", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv1Poa_PVWatts_t_ref_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "t_ref", number);
	});
}

SAM_EXPORT double* SAM_Pvwattsv1Poa_Weather_beam_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "beam", length);
	if (!result)
		make_access_error("SAM_Pvwattsv1Poa", "beam");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvwattsv1Poa_Weather_incidence_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "incidence", length);
	if (!result)
		make_access_error("SAM_Pvwattsv1Poa", "incidence");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvwattsv1Poa_Weather_poa_beam_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "poa_beam", length);
	if (!result)
		make_access_error("SAM_Pvwattsv1Poa", "poa_beam");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvwattsv1Poa_Weather_poa_gnddiff_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "poa_gnddiff", length);
	if (!result)
		make_access_error("SAM_Pvwattsv1Poa", "poa_gnddiff");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvwattsv1Poa_Weather_poa_skydiff_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "poa_skydiff", length);
	if (!result)
		make_access_error("SAM_Pvwattsv1Poa", "poa_skydiff");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvwattsv1Poa_Weather_tdry_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tdry", length);
	if (!result)
		make_access_error("SAM_Pvwattsv1Poa", "tdry");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvwattsv1Poa_Weather_wspd_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "wspd", length);
	if (!result)
		make_access_error("SAM_Pvwattsv1Poa", "wspd");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv1Poa_PVWatts_derate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "derate", &result))
		make_access_error("SAM_Pvwattsv1Poa", "derate");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv1Poa_PVWatts_gamma_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "gamma", &result))
		make_access_error("SAM_Pvwattsv1Poa", "gamma");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv1Poa_PVWatts_inoct_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inoct", &result))
		make_access_error("SAM_Pvwattsv1Poa", "inoct");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv1Poa_PVWatts_inv_eff_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inv_eff", &result))
		make_access_error("SAM_Pvwattsv1Poa", "inv_eff");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv1Poa_PVWatts_step_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "step", &result))
		make_access_error("SAM_Pvwattsv1Poa", "step");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv1Poa_PVWatts_system_size_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_size", &result))
		make_access_error("SAM_Pvwattsv1Poa", "system_size");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv1Poa_PVWatts_t_ref_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "t_ref", &result))
		make_access_error("SAM_Pvwattsv1Poa", "t_ref");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvwattsv1Poa_Outputs_ac_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "ac", length);
	if (!result)
		make_access_error("SAM_Pvwattsv1Poa", "ac");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvwattsv1Poa_Outputs_dc_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dc", length);
	if (!result)
		make_access_error("SAM_Pvwattsv1Poa", "dc");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvwattsv1Poa_Outputs_tcell_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tcell", length);
	if (!result)
		make_access_error("SAM_Pvwattsv1Poa", "tcell");
	});
	return result;
}



