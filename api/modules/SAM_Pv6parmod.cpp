#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_Pv6parmod.h"

SAM_EXPORT int SAM_Pv6parmod_execute(SAM_table data, int verbosity, SAM_error* err){
	int n_err = 0;
	translateExceptions(err, [&]{
		n_err += SAM_module_exec("pv6parmod", data, verbosity, err);
	});
	return n_err;
}


SAM_EXPORT void SAM_Pv6parmod_Weather_elev_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "elev", number);
	});
}

SAM_EXPORT void SAM_Pv6parmod_Weather_incidence_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "incidence", arr, length);
	});
}

SAM_EXPORT void SAM_Pv6parmod_Weather_poa_beam_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "poa_beam", arr, length);
	});
}

SAM_EXPORT void SAM_Pv6parmod_Weather_poa_gnddiff_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "poa_gnddiff", arr, length);
	});
}

SAM_EXPORT void SAM_Pv6parmod_Weather_poa_skydiff_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "poa_skydiff", arr, length);
	});
}

SAM_EXPORT void SAM_Pv6parmod_Weather_sun_zen_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "sun_zen", arr, length);
	});
}

SAM_EXPORT void SAM_Pv6parmod_Weather_surf_tilt_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "surf_tilt", arr, length);
	});
}

SAM_EXPORT void SAM_Pv6parmod_Weather_tdry_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "tdry", arr, length);
	});
}

SAM_EXPORT void SAM_Pv6parmod_Weather_wdir_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "wdir", arr, length);
	});
}

SAM_EXPORT void SAM_Pv6parmod_Weather_wspd_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "wspd", arr, length);
	});
}

SAM_EXPORT void SAM_Pv6parmod_CEC6ParameterPVModuleModel_Adj_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Adj", number);
	});
}

SAM_EXPORT void SAM_Pv6parmod_CEC6ParameterPVModuleModel_Il_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Il", number);
	});
}

SAM_EXPORT void SAM_Pv6parmod_CEC6ParameterPVModuleModel_Imp_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Imp", number);
	});
}

SAM_EXPORT void SAM_Pv6parmod_CEC6ParameterPVModuleModel_Io_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Io", number);
	});
}

SAM_EXPORT void SAM_Pv6parmod_CEC6ParameterPVModuleModel_Isc_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Isc", number);
	});
}

SAM_EXPORT void SAM_Pv6parmod_CEC6ParameterPVModuleModel_Rs_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Rs", number);
	});
}

SAM_EXPORT void SAM_Pv6parmod_CEC6ParameterPVModuleModel_Rsh_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Rsh", number);
	});
}

SAM_EXPORT void SAM_Pv6parmod_CEC6ParameterPVModuleModel_Vmp_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Vmp", number);
	});
}

SAM_EXPORT void SAM_Pv6parmod_CEC6ParameterPVModuleModel_Voc_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Voc", number);
	});
}

SAM_EXPORT void SAM_Pv6parmod_CEC6ParameterPVModuleModel_a_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "a", number);
	});
}

SAM_EXPORT void SAM_Pv6parmod_CEC6ParameterPVModuleModel_alpha_isc_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "alpha_isc", number);
	});
}

SAM_EXPORT void SAM_Pv6parmod_CEC6ParameterPVModuleModel_area_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "area", number);
	});
}

SAM_EXPORT void SAM_Pv6parmod_CEC6ParameterPVModuleModel_beta_voc_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "beta_voc", number);
	});
}

SAM_EXPORT void SAM_Pv6parmod_CEC6ParameterPVModuleModel_gamma_pmp_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "gamma_pmp", number);
	});
}

SAM_EXPORT void SAM_Pv6parmod_CEC6ParameterPVModuleModel_height_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "height", number);
	});
}

SAM_EXPORT void SAM_Pv6parmod_CEC6ParameterPVModuleModel_opvoltage_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "opvoltage", arr, length);
	});
}

SAM_EXPORT void SAM_Pv6parmod_CEC6ParameterPVModuleModel_standoff_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "standoff", number);
	});
}

SAM_EXPORT void SAM_Pv6parmod_CEC6ParameterPVModuleModel_tnoct_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tnoct", number);
	});
}

SAM_EXPORT double SAM_Pv6parmod_Weather_elev_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "elev", &result))
		make_access_error("SAM_Pv6parmod", "elev");
	});
	return result;
}



SAM_EXPORT double* SAM_Pv6parmod_Weather_incidence_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "incidence", length);
	if (!result)
		make_access_error("SAM_Pv6parmod", "incidence");
	});
	return result;
}



SAM_EXPORT double* SAM_Pv6parmod_Weather_poa_beam_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "poa_beam", length);
	if (!result)
		make_access_error("SAM_Pv6parmod", "poa_beam");
	});
	return result;
}



SAM_EXPORT double* SAM_Pv6parmod_Weather_poa_gnddiff_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "poa_gnddiff", length);
	if (!result)
		make_access_error("SAM_Pv6parmod", "poa_gnddiff");
	});
	return result;
}



SAM_EXPORT double* SAM_Pv6parmod_Weather_poa_skydiff_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "poa_skydiff", length);
	if (!result)
		make_access_error("SAM_Pv6parmod", "poa_skydiff");
	});
	return result;
}



SAM_EXPORT double* SAM_Pv6parmod_Weather_sun_zen_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "sun_zen", length);
	if (!result)
		make_access_error("SAM_Pv6parmod", "sun_zen");
	});
	return result;
}



SAM_EXPORT double* SAM_Pv6parmod_Weather_surf_tilt_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "surf_tilt", length);
	if (!result)
		make_access_error("SAM_Pv6parmod", "surf_tilt");
	});
	return result;
}



SAM_EXPORT double* SAM_Pv6parmod_Weather_tdry_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tdry", length);
	if (!result)
		make_access_error("SAM_Pv6parmod", "tdry");
	});
	return result;
}



SAM_EXPORT double* SAM_Pv6parmod_Weather_wdir_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "wdir", length);
	if (!result)
		make_access_error("SAM_Pv6parmod", "wdir");
	});
	return result;
}



SAM_EXPORT double* SAM_Pv6parmod_Weather_wspd_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "wspd", length);
	if (!result)
		make_access_error("SAM_Pv6parmod", "wspd");
	});
	return result;
}



SAM_EXPORT double SAM_Pv6parmod_CEC6ParameterPVModuleModel_Adj_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Adj", &result))
		make_access_error("SAM_Pv6parmod", "Adj");
	});
	return result;
}



SAM_EXPORT double SAM_Pv6parmod_CEC6ParameterPVModuleModel_Il_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Il", &result))
		make_access_error("SAM_Pv6parmod", "Il");
	});
	return result;
}



SAM_EXPORT double SAM_Pv6parmod_CEC6ParameterPVModuleModel_Imp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Imp", &result))
		make_access_error("SAM_Pv6parmod", "Imp");
	});
	return result;
}



SAM_EXPORT double SAM_Pv6parmod_CEC6ParameterPVModuleModel_Io_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Io", &result))
		make_access_error("SAM_Pv6parmod", "Io");
	});
	return result;
}



SAM_EXPORT double SAM_Pv6parmod_CEC6ParameterPVModuleModel_Isc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Isc", &result))
		make_access_error("SAM_Pv6parmod", "Isc");
	});
	return result;
}



SAM_EXPORT double SAM_Pv6parmod_CEC6ParameterPVModuleModel_Rs_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Rs", &result))
		make_access_error("SAM_Pv6parmod", "Rs");
	});
	return result;
}



SAM_EXPORT double SAM_Pv6parmod_CEC6ParameterPVModuleModel_Rsh_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Rsh", &result))
		make_access_error("SAM_Pv6parmod", "Rsh");
	});
	return result;
}



SAM_EXPORT double SAM_Pv6parmod_CEC6ParameterPVModuleModel_Vmp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Vmp", &result))
		make_access_error("SAM_Pv6parmod", "Vmp");
	});
	return result;
}



SAM_EXPORT double SAM_Pv6parmod_CEC6ParameterPVModuleModel_Voc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Voc", &result))
		make_access_error("SAM_Pv6parmod", "Voc");
	});
	return result;
}



SAM_EXPORT double SAM_Pv6parmod_CEC6ParameterPVModuleModel_a_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "a", &result))
		make_access_error("SAM_Pv6parmod", "a");
	});
	return result;
}



SAM_EXPORT double SAM_Pv6parmod_CEC6ParameterPVModuleModel_alpha_isc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "alpha_isc", &result))
		make_access_error("SAM_Pv6parmod", "alpha_isc");
	});
	return result;
}



SAM_EXPORT double SAM_Pv6parmod_CEC6ParameterPVModuleModel_area_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "area", &result))
		make_access_error("SAM_Pv6parmod", "area");
	});
	return result;
}



SAM_EXPORT double SAM_Pv6parmod_CEC6ParameterPVModuleModel_beta_voc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "beta_voc", &result))
		make_access_error("SAM_Pv6parmod", "beta_voc");
	});
	return result;
}



SAM_EXPORT double SAM_Pv6parmod_CEC6ParameterPVModuleModel_gamma_pmp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "gamma_pmp", &result))
		make_access_error("SAM_Pv6parmod", "gamma_pmp");
	});
	return result;
}



SAM_EXPORT double SAM_Pv6parmod_CEC6ParameterPVModuleModel_height_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "height", &result))
		make_access_error("SAM_Pv6parmod", "height");
	});
	return result;
}



SAM_EXPORT double* SAM_Pv6parmod_CEC6ParameterPVModuleModel_opvoltage_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "opvoltage", length);
	if (!result)
		make_access_error("SAM_Pv6parmod", "opvoltage");
	});
	return result;
}



SAM_EXPORT double SAM_Pv6parmod_CEC6ParameterPVModuleModel_standoff_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "standoff", &result))
		make_access_error("SAM_Pv6parmod", "standoff");
	});
	return result;
}



SAM_EXPORT double SAM_Pv6parmod_CEC6ParameterPVModuleModel_tnoct_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tnoct", &result))
		make_access_error("SAM_Pv6parmod", "tnoct");
	});
	return result;
}



SAM_EXPORT double* SAM_Pv6parmod_Outputs_dc_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dc", length);
	if (!result)
		make_access_error("SAM_Pv6parmod", "dc");
	});
	return result;
}



SAM_EXPORT double* SAM_Pv6parmod_Outputs_dc_current_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dc_current", length);
	if (!result)
		make_access_error("SAM_Pv6parmod", "dc_current");
	});
	return result;
}



SAM_EXPORT double* SAM_Pv6parmod_Outputs_dc_voltage_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dc_voltage", length);
	if (!result)
		make_access_error("SAM_Pv6parmod", "dc_voltage");
	});
	return result;
}



SAM_EXPORT double* SAM_Pv6parmod_Outputs_eff_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "eff", length);
	if (!result)
		make_access_error("SAM_Pv6parmod", "eff");
	});
	return result;
}



SAM_EXPORT double* SAM_Pv6parmod_Outputs_tcell_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tcell", length);
	if (!result)
		make_access_error("SAM_Pv6parmod", "tcell");
	});
	return result;
}



