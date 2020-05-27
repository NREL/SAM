#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_Pvwattsv7.h"

SAM_EXPORT int SAM_Pvwattsv7_execute(SAM_table data, int verbosity, SAM_error* err){
	int n_err = 0;
	translateExceptions(err, [&]{
		n_err += SAM_module_exec("pvwattsv7", data, verbosity, err);
	});
	return n_err;
}


SAM_EXPORT void SAM_Pvwattsv7_SolarResource_albedo_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "albedo", arr, length);
	});
}

SAM_EXPORT void SAM_Pvwattsv7_SolarResource_solar_resource_data_tset(SAM_table ptr, SAM_table tab, SAM_error *err){
	SAM_table_set_table(ptr, "solar_resource_data", tab, err);
}



SAM_EXPORT void SAM_Pvwattsv7_SolarResource_solar_resource_file_sset(SAM_table ptr, const char* str, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_string(ptr, "solar_resource_file", str);
	});
}

SAM_EXPORT void SAM_Pvwattsv7_Lifetime_analysis_period_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "analysis_period", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv7_Lifetime_dc_degradation_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "dc_degradation", arr, length);
	});
}

SAM_EXPORT void SAM_Pvwattsv7_Lifetime_system_use_lifetime_output_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "system_use_lifetime_output", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv7_SystemDesign_ac_plant_max_f_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ac_plant_max_f", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv7_SystemDesign_array_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "array_type", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv7_SystemDesign_azimuth_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "azimuth", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv7_SystemDesign_batt_simple_enable_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_simple_enable", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv7_SystemDesign_bifaciality_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "bifaciality", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv7_SystemDesign_dc_ac_ratio_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dc_ac_ratio", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv7_SystemDesign_en_snowloss_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "en_snowloss", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv7_SystemDesign_enable_wind_stow_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "enable_wind_stow", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv7_SystemDesign_gcr_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "gcr", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv7_SystemDesign_gust_factor_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "gust_factor", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv7_SystemDesign_inv_eff_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inv_eff", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv7_SystemDesign_losses_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "losses", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv7_SystemDesign_module_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "module_type", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv7_SystemDesign_rotlim_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rotlim", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv7_SystemDesign_shading_azal_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "shading:azal", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Pvwattsv7_SystemDesign_shading_diff_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "shading:diff", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv7_SystemDesign_shading_mxh_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "shading:mxh", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Pvwattsv7_SystemDesign_shading_timestep_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "shading:timestep", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Pvwattsv7_SystemDesign_soiling_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "soiling", arr, length);
	});
}

SAM_EXPORT void SAM_Pvwattsv7_SystemDesign_stow_wspd_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "stow_wspd", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv7_SystemDesign_system_capacity_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "system_capacity", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv7_SystemDesign_tilt_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tilt", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv7_SystemDesign_wind_stow_angle_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "wind_stow_angle", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv7_SystemDesign_xfmr_ll_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "xfmr_ll", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv7_SystemDesign_xfmr_nll_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "xfmr_nll", number);
	});
}

SAM_EXPORT double* SAM_Pvwattsv7_SolarResource_albedo_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "albedo", length);
	if (!result)
		make_access_error("SAM_Pvwattsv7", "albedo");
	});
	return result;
}



SAM_EXPORT SAM_table SAM_Pvwattsv7_SolarResource_solar_resource_data_tget(SAM_table ptr, SAM_error *err){
	SAM_table result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_table(ptr, "solar_resource_data");
	if (!result)
		make_access_error("SAM_Pvwattsv7", "solar_resource_data");
	});
	return result;
}



SAM_EXPORT const char* SAM_Pvwattsv7_SolarResource_solar_resource_file_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "solar_resource_file");
	if (!result)
		make_access_error("SAM_Pvwattsv7", "solar_resource_file");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv7_Lifetime_analysis_period_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "analysis_period", &result))
		make_access_error("SAM_Pvwattsv7", "analysis_period");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvwattsv7_Lifetime_dc_degradation_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dc_degradation", length);
	if (!result)
		make_access_error("SAM_Pvwattsv7", "dc_degradation");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv7_Lifetime_system_use_lifetime_output_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_use_lifetime_output", &result))
		make_access_error("SAM_Pvwattsv7", "system_use_lifetime_output");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv7_SystemDesign_ac_plant_max_f_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ac_plant_max_f", &result))
		make_access_error("SAM_Pvwattsv7", "ac_plant_max_f");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv7_SystemDesign_array_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "array_type", &result))
		make_access_error("SAM_Pvwattsv7", "array_type");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv7_SystemDesign_azimuth_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "azimuth", &result))
		make_access_error("SAM_Pvwattsv7", "azimuth");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv7_SystemDesign_batt_simple_enable_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_simple_enable", &result))
		make_access_error("SAM_Pvwattsv7", "batt_simple_enable");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv7_SystemDesign_bifaciality_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "bifaciality", &result))
		make_access_error("SAM_Pvwattsv7", "bifaciality");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv7_SystemDesign_dc_ac_ratio_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dc_ac_ratio", &result))
		make_access_error("SAM_Pvwattsv7", "dc_ac_ratio");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv7_SystemDesign_en_snowloss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "en_snowloss", &result))
		make_access_error("SAM_Pvwattsv7", "en_snowloss");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv7_SystemDesign_enable_wind_stow_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "enable_wind_stow", &result))
		make_access_error("SAM_Pvwattsv7", "enable_wind_stow");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv7_SystemDesign_gcr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "gcr", &result))
		make_access_error("SAM_Pvwattsv7", "gcr");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv7_SystemDesign_gust_factor_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "gust_factor", &result))
		make_access_error("SAM_Pvwattsv7", "gust_factor");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv7_SystemDesign_inv_eff_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inv_eff", &result))
		make_access_error("SAM_Pvwattsv7", "inv_eff");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv7_SystemDesign_losses_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "losses", &result))
		make_access_error("SAM_Pvwattsv7", "losses");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv7_SystemDesign_module_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "module_type", &result))
		make_access_error("SAM_Pvwattsv7", "module_type");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv7_SystemDesign_rotlim_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rotlim", &result))
		make_access_error("SAM_Pvwattsv7", "rotlim");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvwattsv7_SystemDesign_shading_azal_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "shading:azal", nrows, ncols);
	if (!result)
		make_access_error("SAM_Pvwattsv7", "shading:azal");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv7_SystemDesign_shading_diff_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "shading:diff", &result))
		make_access_error("SAM_Pvwattsv7", "shading:diff");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvwattsv7_SystemDesign_shading_mxh_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "shading:mxh", nrows, ncols);
	if (!result)
		make_access_error("SAM_Pvwattsv7", "shading:mxh");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvwattsv7_SystemDesign_shading_timestep_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "shading:timestep", nrows, ncols);
	if (!result)
		make_access_error("SAM_Pvwattsv7", "shading:timestep");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvwattsv7_SystemDesign_soiling_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "soiling", length);
	if (!result)
		make_access_error("SAM_Pvwattsv7", "soiling");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv7_SystemDesign_stow_wspd_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "stow_wspd", &result))
		make_access_error("SAM_Pvwattsv7", "stow_wspd");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv7_SystemDesign_system_capacity_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_capacity", &result))
		make_access_error("SAM_Pvwattsv7", "system_capacity");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv7_SystemDesign_tilt_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tilt", &result))
		make_access_error("SAM_Pvwattsv7", "tilt");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv7_SystemDesign_wind_stow_angle_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "wind_stow_angle", &result))
		make_access_error("SAM_Pvwattsv7", "wind_stow_angle");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv7_SystemDesign_xfmr_ll_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "xfmr_ll", &result))
		make_access_error("SAM_Pvwattsv7", "xfmr_ll");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv7_SystemDesign_xfmr_nll_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "xfmr_nll", &result))
		make_access_error("SAM_Pvwattsv7", "xfmr_nll");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvwattsv7_Outputs_ac_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "ac", length);
	if (!result)
		make_access_error("SAM_Pvwattsv7", "ac");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv7_Outputs_ac_annual_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ac_annual", &result))
		make_access_error("SAM_Pvwattsv7", "ac_annual");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvwattsv7_Outputs_ac_monthly_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "ac_monthly", length);
	if (!result)
		make_access_error("SAM_Pvwattsv7", "ac_monthly");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv7_Outputs_annual_energy_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_energy", &result))
		make_access_error("SAM_Pvwattsv7", "annual_energy");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvwattsv7_Outputs_aoi_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "aoi", length);
	if (!result)
		make_access_error("SAM_Pvwattsv7", "aoi");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv7_Outputs_capacity_factor_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "capacity_factor", &result))
		make_access_error("SAM_Pvwattsv7", "capacity_factor");
	});
	return result;
}



SAM_EXPORT const char* SAM_Pvwattsv7_Outputs_city_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "city");
	if (!result)
		make_access_error("SAM_Pvwattsv7", "city");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvwattsv7_Outputs_dc_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dc", length);
	if (!result)
		make_access_error("SAM_Pvwattsv7", "dc");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvwattsv7_Outputs_dc_monthly_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dc_monthly", length);
	if (!result)
		make_access_error("SAM_Pvwattsv7", "dc_monthly");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvwattsv7_Outputs_dcsnowderate_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dcsnowderate", length);
	if (!result)
		make_access_error("SAM_Pvwattsv7", "dcsnowderate");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvwattsv7_Outputs_df_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "df", length);
	if (!result)
		make_access_error("SAM_Pvwattsv7", "df");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvwattsv7_Outputs_dn_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dn", length);
	if (!result)
		make_access_error("SAM_Pvwattsv7", "dn");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv7_Outputs_elev_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "elev", &result))
		make_access_error("SAM_Pvwattsv7", "elev");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv7_Outputs_estimated_rows_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "estimated_rows", &result))
		make_access_error("SAM_Pvwattsv7", "estimated_rows");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvwattsv7_Outputs_gen_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "gen", length);
	if (!result)
		make_access_error("SAM_Pvwattsv7", "gen");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvwattsv7_Outputs_gh_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "gh", length);
	if (!result)
		make_access_error("SAM_Pvwattsv7", "gh");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv7_Outputs_inverter_efficiency_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inverter_efficiency", &result))
		make_access_error("SAM_Pvwattsv7", "inverter_efficiency");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv7_Outputs_kwh_per_kw_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "kwh_per_kw", &result))
		make_access_error("SAM_Pvwattsv7", "kwh_per_kw");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv7_Outputs_lat_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "lat", &result))
		make_access_error("SAM_Pvwattsv7", "lat");
	});
	return result;
}



SAM_EXPORT const char* SAM_Pvwattsv7_Outputs_location_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "location");
	if (!result)
		make_access_error("SAM_Pvwattsv7", "location");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv7_Outputs_lon_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "lon", &result))
		make_access_error("SAM_Pvwattsv7", "lon");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvwattsv7_Outputs_monthly_energy_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_energy", length);
	if (!result)
		make_access_error("SAM_Pvwattsv7", "monthly_energy");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv7_Outputs_percent_complete_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "percent_complete", &result))
		make_access_error("SAM_Pvwattsv7", "percent_complete");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvwattsv7_Outputs_poa_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "poa", length);
	if (!result)
		make_access_error("SAM_Pvwattsv7", "poa");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvwattsv7_Outputs_poa_monthly_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "poa_monthly", length);
	if (!result)
		make_access_error("SAM_Pvwattsv7", "poa_monthly");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvwattsv7_Outputs_shad_beam_factor_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "shad_beam_factor", length);
	if (!result)
		make_access_error("SAM_Pvwattsv7", "shad_beam_factor");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvwattsv7_Outputs_snow_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "snow", length);
	if (!result)
		make_access_error("SAM_Pvwattsv7", "snow");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv7_Outputs_solrad_annual_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "solrad_annual", &result))
		make_access_error("SAM_Pvwattsv7", "solrad_annual");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvwattsv7_Outputs_solrad_monthly_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "solrad_monthly", length);
	if (!result)
		make_access_error("SAM_Pvwattsv7", "solrad_monthly");
	});
	return result;
}



SAM_EXPORT const char* SAM_Pvwattsv7_Outputs_state_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "state");
	if (!result)
		make_access_error("SAM_Pvwattsv7", "state");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvwattsv7_Outputs_sunup_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "sunup", length);
	if (!result)
		make_access_error("SAM_Pvwattsv7", "sunup");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvwattsv7_Outputs_tamb_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tamb", length);
	if (!result)
		make_access_error("SAM_Pvwattsv7", "tamb");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvwattsv7_Outputs_tcell_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tcell", length);
	if (!result)
		make_access_error("SAM_Pvwattsv7", "tcell");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvwattsv7_Outputs_tpoa_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tpoa", length);
	if (!result)
		make_access_error("SAM_Pvwattsv7", "tpoa");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv7_Outputs_ts_shift_hours_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ts_shift_hours", &result))
		make_access_error("SAM_Pvwattsv7", "ts_shift_hours");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv7_Outputs_tz_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tz", &result))
		make_access_error("SAM_Pvwattsv7", "tz");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvwattsv7_Outputs_wspd_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "wspd", length);
	if (!result)
		make_access_error("SAM_Pvwattsv7", "wspd");
	});
	return result;
}



