#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_Pvwattsv8.h"

SAM_EXPORT int SAM_Pvwattsv8_execute(SAM_table data, int verbosity, SAM_error* err){
	return SAM_module_exec("pvwattsv8", data, verbosity, err);
}

SAM_EXPORT void SAM_Pvwattsv8_SolarResource_albedo_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "albedo", arr, length);
	});
}

SAM_EXPORT void SAM_Pvwattsv8_SolarResource_albedo_default_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "albedo_default", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv8_SolarResource_albedo_default_snow_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "albedo_default_snow", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv8_SolarResource_solar_resource_data_tset(SAM_table ptr, SAM_table tab, SAM_error *err){
	SAM_table_set_table(ptr, "solar_resource_data", tab, err);
}



SAM_EXPORT void SAM_Pvwattsv8_SolarResource_solar_resource_file_sset(SAM_table ptr, const char* str, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_string(ptr, "solar_resource_file", str);
	});
}

SAM_EXPORT void SAM_Pvwattsv8_SolarResource_use_wf_albedo_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "use_wf_albedo", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv8_Lifetime_analysis_period_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "analysis_period", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv8_Lifetime_dc_degradation_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "dc_degradation", arr, length);
	});
}

SAM_EXPORT void SAM_Pvwattsv8_Lifetime_system_use_lifetime_output_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "system_use_lifetime_output", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv8_SystemDesign_array_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "array_type", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv8_SystemDesign_azimuth_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "azimuth", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv8_SystemDesign_batt_simple_enable_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_simple_enable", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv8_SystemDesign_bifaciality_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "bifaciality", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv8_SystemDesign_dc_ac_ratio_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dc_ac_ratio", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv8_SystemDesign_en_snowloss_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "en_snowloss", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv8_SystemDesign_enable_wind_stow_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "enable_wind_stow", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv8_SystemDesign_gcr_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "gcr", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv8_SystemDesign_gust_factor_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "gust_factor", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv8_SystemDesign_inv_eff_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inv_eff", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv8_SystemDesign_losses_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "losses", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv8_SystemDesign_module_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "module_type", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv8_SystemDesign_rotlim_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rotlim", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv8_SystemDesign_soiling_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "soiling", arr, length);
	});
}

SAM_EXPORT void SAM_Pvwattsv8_SystemDesign_stow_wspd_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "stow_wspd", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv8_SystemDesign_system_capacity_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "system_capacity", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv8_SystemDesign_tilt_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tilt", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv8_SystemDesign_wind_stow_angle_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "wind_stow_angle", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv8_SystemDesign_xfmr_ll_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "xfmr_ll", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv8_SystemDesign_xfmr_nll_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "xfmr_nll", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv8_Shading_shading_azal_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "shading_azal", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Pvwattsv8_Shading_shading_diff_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "shading_diff", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv8_Shading_shading_en_azal_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "shading_en_azal", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv8_Shading_shading_en_diff_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "shading_en_diff", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv8_Shading_shading_en_mxh_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "shading_en_mxh", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv8_Shading_shading_en_string_option_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "shading_en_string_option", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv8_Shading_shading_en_timestep_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "shading_en_timestep", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv8_Shading_shading_mxh_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "shading_mxh", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Pvwattsv8_Shading_shading_string_option_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "shading_string_option", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv8_Shading_shading_timestep_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "shading_timestep", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Pvwattsv8_AdjustmentFactors_adjust_constant_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "adjust_constant", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv8_AdjustmentFactors_adjust_en_periods_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "adjust_en_periods", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv8_AdjustmentFactors_adjust_en_timeindex_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "adjust_en_timeindex", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv8_AdjustmentFactors_adjust_periods_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "adjust_periods", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Pvwattsv8_AdjustmentFactors_adjust_timeindex_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "adjust_timeindex", arr, length);
	});
}

SAM_EXPORT void SAM_Pvwattsv8_HybridCosts_degradation_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "degradation", arr, length);
	});
}

SAM_EXPORT void SAM_Pvwattsv8_HybridCosts_land_area_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "land_area", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv8_HybridCosts_om_capacity_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "om_capacity", arr, length);
	});
}

SAM_EXPORT void SAM_Pvwattsv8_HybridCosts_om_capacity_escal_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "om_capacity_escal", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv8_HybridCosts_om_fixed_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "om_fixed", arr, length);
	});
}

SAM_EXPORT void SAM_Pvwattsv8_HybridCosts_om_fixed_escal_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "om_fixed_escal", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv8_HybridCosts_om_land_lease_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "om_land_lease", arr, length);
	});
}

SAM_EXPORT void SAM_Pvwattsv8_HybridCosts_om_land_lease_escal_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "om_land_lease_escal", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv8_HybridCosts_om_production_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "om_production", arr, length);
	});
}

SAM_EXPORT void SAM_Pvwattsv8_HybridCosts_om_production_escal_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "om_production_escal", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv8_HybridCosts_total_installed_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "total_installed_cost", number);
	});
}

SAM_EXPORT double* SAM_Pvwattsv8_SolarResource_albedo_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "albedo", length);
	if (!result)
		make_access_error("SAM_Pvwattsv8", "albedo");
	});
	return result;
}

SAM_EXPORT double SAM_Pvwattsv8_SolarResource_albedo_default_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "albedo_default", &result))
		make_access_error("SAM_Pvwattsv8", "albedo_default");
	});
	return result;
}

SAM_EXPORT double SAM_Pvwattsv8_SolarResource_albedo_default_snow_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "albedo_default_snow", &result))
		make_access_error("SAM_Pvwattsv8", "albedo_default_snow");
	});
	return result;
}

SAM_EXPORT SAM_table SAM_Pvwattsv8_SolarResource_solar_resource_data_tget(SAM_table ptr, SAM_error *err){
	SAM_table result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_table(ptr, "solar_resource_data");
	if (!result)
		make_access_error("SAM_Pvwattsv8", "solar_resource_data");
	});
	return result;
}

SAM_EXPORT const char* SAM_Pvwattsv8_SolarResource_solar_resource_file_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "solar_resource_file");
	if (!result)
		make_access_error("SAM_Pvwattsv8", "solar_resource_file");
	});
	return result;
}

SAM_EXPORT double SAM_Pvwattsv8_SolarResource_use_wf_albedo_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "use_wf_albedo", &result))
		make_access_error("SAM_Pvwattsv8", "use_wf_albedo");
	});
	return result;
}

SAM_EXPORT double SAM_Pvwattsv8_Lifetime_analysis_period_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "analysis_period", &result))
		make_access_error("SAM_Pvwattsv8", "analysis_period");
	});
	return result;
}

SAM_EXPORT double* SAM_Pvwattsv8_Lifetime_dc_degradation_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dc_degradation", length);
	if (!result)
		make_access_error("SAM_Pvwattsv8", "dc_degradation");
	});
	return result;
}

SAM_EXPORT double SAM_Pvwattsv8_Lifetime_system_use_lifetime_output_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_use_lifetime_output", &result))
		make_access_error("SAM_Pvwattsv8", "system_use_lifetime_output");
	});
	return result;
}

SAM_EXPORT double SAM_Pvwattsv8_SystemDesign_array_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "array_type", &result))
		make_access_error("SAM_Pvwattsv8", "array_type");
	});
	return result;
}

SAM_EXPORT double SAM_Pvwattsv8_SystemDesign_azimuth_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "azimuth", &result))
		make_access_error("SAM_Pvwattsv8", "azimuth");
	});
	return result;
}

SAM_EXPORT double SAM_Pvwattsv8_SystemDesign_batt_simple_enable_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_simple_enable", &result))
		make_access_error("SAM_Pvwattsv8", "batt_simple_enable");
	});
	return result;
}

SAM_EXPORT double SAM_Pvwattsv8_SystemDesign_bifaciality_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "bifaciality", &result))
		make_access_error("SAM_Pvwattsv8", "bifaciality");
	});
	return result;
}

SAM_EXPORT double SAM_Pvwattsv8_SystemDesign_dc_ac_ratio_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dc_ac_ratio", &result))
		make_access_error("SAM_Pvwattsv8", "dc_ac_ratio");
	});
	return result;
}

SAM_EXPORT double SAM_Pvwattsv8_SystemDesign_en_snowloss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "en_snowloss", &result))
		make_access_error("SAM_Pvwattsv8", "en_snowloss");
	});
	return result;
}

SAM_EXPORT double SAM_Pvwattsv8_SystemDesign_enable_wind_stow_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "enable_wind_stow", &result))
		make_access_error("SAM_Pvwattsv8", "enable_wind_stow");
	});
	return result;
}

SAM_EXPORT double SAM_Pvwattsv8_SystemDesign_gcr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "gcr", &result))
		make_access_error("SAM_Pvwattsv8", "gcr");
	});
	return result;
}

SAM_EXPORT double SAM_Pvwattsv8_SystemDesign_gust_factor_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "gust_factor", &result))
		make_access_error("SAM_Pvwattsv8", "gust_factor");
	});
	return result;
}

SAM_EXPORT double SAM_Pvwattsv8_SystemDesign_inv_eff_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inv_eff", &result))
		make_access_error("SAM_Pvwattsv8", "inv_eff");
	});
	return result;
}

SAM_EXPORT double SAM_Pvwattsv8_SystemDesign_losses_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "losses", &result))
		make_access_error("SAM_Pvwattsv8", "losses");
	});
	return result;
}

SAM_EXPORT double SAM_Pvwattsv8_SystemDesign_module_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "module_type", &result))
		make_access_error("SAM_Pvwattsv8", "module_type");
	});
	return result;
}

SAM_EXPORT double SAM_Pvwattsv8_SystemDesign_rotlim_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rotlim", &result))
		make_access_error("SAM_Pvwattsv8", "rotlim");
	});
	return result;
}

SAM_EXPORT double* SAM_Pvwattsv8_SystemDesign_soiling_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "soiling", length);
	if (!result)
		make_access_error("SAM_Pvwattsv8", "soiling");
	});
	return result;
}

SAM_EXPORT double SAM_Pvwattsv8_SystemDesign_stow_wspd_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "stow_wspd", &result))
		make_access_error("SAM_Pvwattsv8", "stow_wspd");
	});
	return result;
}

SAM_EXPORT double SAM_Pvwattsv8_SystemDesign_system_capacity_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_capacity", &result))
		make_access_error("SAM_Pvwattsv8", "system_capacity");
	});
	return result;
}

SAM_EXPORT double SAM_Pvwattsv8_SystemDesign_tilt_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tilt", &result))
		make_access_error("SAM_Pvwattsv8", "tilt");
	});
	return result;
}

SAM_EXPORT double SAM_Pvwattsv8_SystemDesign_wind_stow_angle_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "wind_stow_angle", &result))
		make_access_error("SAM_Pvwattsv8", "wind_stow_angle");
	});
	return result;
}

SAM_EXPORT double SAM_Pvwattsv8_SystemDesign_xfmr_ll_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "xfmr_ll", &result))
		make_access_error("SAM_Pvwattsv8", "xfmr_ll");
	});
	return result;
}

SAM_EXPORT double SAM_Pvwattsv8_SystemDesign_xfmr_nll_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "xfmr_nll", &result))
		make_access_error("SAM_Pvwattsv8", "xfmr_nll");
	});
	return result;
}

SAM_EXPORT double* SAM_Pvwattsv8_Shading_shading_azal_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "shading_azal", nrows, ncols);
	if (!result)
		make_access_error("SAM_Pvwattsv8", "shading_azal");
	});
	return result;
}

SAM_EXPORT double SAM_Pvwattsv8_Shading_shading_diff_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "shading_diff", &result))
		make_access_error("SAM_Pvwattsv8", "shading_diff");
	});
	return result;
}

SAM_EXPORT double SAM_Pvwattsv8_Shading_shading_en_azal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "shading_en_azal", &result))
		make_access_error("SAM_Pvwattsv8", "shading_en_azal");
	});
	return result;
}

SAM_EXPORT double SAM_Pvwattsv8_Shading_shading_en_diff_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "shading_en_diff", &result))
		make_access_error("SAM_Pvwattsv8", "shading_en_diff");
	});
	return result;
}

SAM_EXPORT double SAM_Pvwattsv8_Shading_shading_en_mxh_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "shading_en_mxh", &result))
		make_access_error("SAM_Pvwattsv8", "shading_en_mxh");
	});
	return result;
}

SAM_EXPORT double SAM_Pvwattsv8_Shading_shading_en_string_option_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "shading_en_string_option", &result))
		make_access_error("SAM_Pvwattsv8", "shading_en_string_option");
	});
	return result;
}

SAM_EXPORT double SAM_Pvwattsv8_Shading_shading_en_timestep_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "shading_en_timestep", &result))
		make_access_error("SAM_Pvwattsv8", "shading_en_timestep");
	});
	return result;
}

SAM_EXPORT double* SAM_Pvwattsv8_Shading_shading_mxh_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "shading_mxh", nrows, ncols);
	if (!result)
		make_access_error("SAM_Pvwattsv8", "shading_mxh");
	});
	return result;
}

SAM_EXPORT double SAM_Pvwattsv8_Shading_shading_string_option_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "shading_string_option", &result))
		make_access_error("SAM_Pvwattsv8", "shading_string_option");
	});
	return result;
}

SAM_EXPORT double* SAM_Pvwattsv8_Shading_shading_timestep_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "shading_timestep", nrows, ncols);
	if (!result)
		make_access_error("SAM_Pvwattsv8", "shading_timestep");
	});
	return result;
}

SAM_EXPORT double SAM_Pvwattsv8_AdjustmentFactors_adjust_constant_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "adjust_constant", &result))
		make_access_error("SAM_Pvwattsv8", "adjust_constant");
	});
	return result;
}

SAM_EXPORT double SAM_Pvwattsv8_AdjustmentFactors_adjust_en_periods_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "adjust_en_periods", &result))
		make_access_error("SAM_Pvwattsv8", "adjust_en_periods");
	});
	return result;
}

SAM_EXPORT double SAM_Pvwattsv8_AdjustmentFactors_adjust_en_timeindex_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "adjust_en_timeindex", &result))
		make_access_error("SAM_Pvwattsv8", "adjust_en_timeindex");
	});
	return result;
}

SAM_EXPORT double* SAM_Pvwattsv8_AdjustmentFactors_adjust_periods_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "adjust_periods", nrows, ncols);
	if (!result)
		make_access_error("SAM_Pvwattsv8", "adjust_periods");
	});
	return result;
}

SAM_EXPORT double* SAM_Pvwattsv8_AdjustmentFactors_adjust_timeindex_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "adjust_timeindex", length);
	if (!result)
		make_access_error("SAM_Pvwattsv8", "adjust_timeindex");
	});
	return result;
}

SAM_EXPORT double* SAM_Pvwattsv8_HybridCosts_degradation_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "degradation", length);
	if (!result)
		make_access_error("SAM_Pvwattsv8", "degradation");
	});
	return result;
}

SAM_EXPORT double SAM_Pvwattsv8_HybridCosts_land_area_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "land_area", &result))
		make_access_error("SAM_Pvwattsv8", "land_area");
	});
	return result;
}

SAM_EXPORT double* SAM_Pvwattsv8_HybridCosts_om_capacity_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "om_capacity", length);
	if (!result)
		make_access_error("SAM_Pvwattsv8", "om_capacity");
	});
	return result;
}

SAM_EXPORT double SAM_Pvwattsv8_HybridCosts_om_capacity_escal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "om_capacity_escal", &result))
		make_access_error("SAM_Pvwattsv8", "om_capacity_escal");
	});
	return result;
}

SAM_EXPORT double* SAM_Pvwattsv8_HybridCosts_om_fixed_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "om_fixed", length);
	if (!result)
		make_access_error("SAM_Pvwattsv8", "om_fixed");
	});
	return result;
}

SAM_EXPORT double SAM_Pvwattsv8_HybridCosts_om_fixed_escal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "om_fixed_escal", &result))
		make_access_error("SAM_Pvwattsv8", "om_fixed_escal");
	});
	return result;
}

SAM_EXPORT double* SAM_Pvwattsv8_HybridCosts_om_land_lease_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "om_land_lease", length);
	if (!result)
		make_access_error("SAM_Pvwattsv8", "om_land_lease");
	});
	return result;
}

SAM_EXPORT double SAM_Pvwattsv8_HybridCosts_om_land_lease_escal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "om_land_lease_escal", &result))
		make_access_error("SAM_Pvwattsv8", "om_land_lease_escal");
	});
	return result;
}

SAM_EXPORT double* SAM_Pvwattsv8_HybridCosts_om_production_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "om_production", length);
	if (!result)
		make_access_error("SAM_Pvwattsv8", "om_production");
	});
	return result;
}

SAM_EXPORT double SAM_Pvwattsv8_HybridCosts_om_production_escal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "om_production_escal", &result))
		make_access_error("SAM_Pvwattsv8", "om_production_escal");
	});
	return result;
}

SAM_EXPORT double SAM_Pvwattsv8_HybridCosts_total_installed_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_installed_cost", &result))
		make_access_error("SAM_Pvwattsv8", "total_installed_cost");
	});
	return result;
}

SAM_EXPORT double* SAM_Pvwattsv8_Outputs_ac_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "ac", length);
	if (!result)
		make_access_error("SAM_Pvwattsv8", "ac");
	});
	return result;
}

SAM_EXPORT double SAM_Pvwattsv8_Outputs_ac_annual_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ac_annual", &result))
		make_access_error("SAM_Pvwattsv8", "ac_annual");
	});
	return result;
}

SAM_EXPORT double SAM_Pvwattsv8_Outputs_ac_annual_pre_adjust_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ac_annual_pre_adjust", &result))
		make_access_error("SAM_Pvwattsv8", "ac_annual_pre_adjust");
	});
	return result;
}

SAM_EXPORT double* SAM_Pvwattsv8_Outputs_ac_monthly_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "ac_monthly", length);
	if (!result)
		make_access_error("SAM_Pvwattsv8", "ac_monthly");
	});
	return result;
}

SAM_EXPORT double* SAM_Pvwattsv8_Outputs_ac_pre_adjust_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "ac_pre_adjust", length);
	if (!result)
		make_access_error("SAM_Pvwattsv8", "ac_pre_adjust");
	});
	return result;
}

SAM_EXPORT double* SAM_Pvwattsv8_Outputs_alb_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "alb", length);
	if (!result)
		make_access_error("SAM_Pvwattsv8", "alb");
	});
	return result;
}

SAM_EXPORT double SAM_Pvwattsv8_Outputs_annual_energy_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_energy", &result))
		make_access_error("SAM_Pvwattsv8", "annual_energy");
	});
	return result;
}

SAM_EXPORT double* SAM_Pvwattsv8_Outputs_annual_energy_distribution_time_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "annual_energy_distribution_time", nrows, ncols);
	if (!result)
		make_access_error("SAM_Pvwattsv8", "annual_energy_distribution_time");
	});
	return result;
}

SAM_EXPORT double* SAM_Pvwattsv8_Outputs_aoi_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "aoi", length);
	if (!result)
		make_access_error("SAM_Pvwattsv8", "aoi");
	});
	return result;
}

SAM_EXPORT double SAM_Pvwattsv8_Outputs_capacity_factor_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "capacity_factor", &result))
		make_access_error("SAM_Pvwattsv8", "capacity_factor");
	});
	return result;
}

SAM_EXPORT double SAM_Pvwattsv8_Outputs_capacity_factor_ac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "capacity_factor_ac", &result))
		make_access_error("SAM_Pvwattsv8", "capacity_factor_ac");
	});
	return result;
}

SAM_EXPORT double* SAM_Pvwattsv8_Outputs_cf_battery_replacement_cost_schedule_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_battery_replacement_cost_schedule", length);
	if (!result)
		make_access_error("SAM_Pvwattsv8", "cf_battery_replacement_cost_schedule");
	});
	return result;
}

SAM_EXPORT double* SAM_Pvwattsv8_Outputs_cf_energy_net_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_energy_net", length);
	if (!result)
		make_access_error("SAM_Pvwattsv8", "cf_energy_net");
	});
	return result;
}

SAM_EXPORT double* SAM_Pvwattsv8_Outputs_cf_fuelcell_replacement_cost_schedule_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_fuelcell_replacement_cost_schedule", length);
	if (!result)
		make_access_error("SAM_Pvwattsv8", "cf_fuelcell_replacement_cost_schedule");
	});
	return result;
}

SAM_EXPORT double* SAM_Pvwattsv8_Outputs_cf_land_lease_expense_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_land_lease_expense", length);
	if (!result)
		make_access_error("SAM_Pvwattsv8", "cf_land_lease_expense");
	});
	return result;
}

SAM_EXPORT double* SAM_Pvwattsv8_Outputs_cf_om_capacity_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_om_capacity", length);
	if (!result)
		make_access_error("SAM_Pvwattsv8", "cf_om_capacity");
	});
	return result;
}

SAM_EXPORT double* SAM_Pvwattsv8_Outputs_cf_om_fixed_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_om_fixed", length);
	if (!result)
		make_access_error("SAM_Pvwattsv8", "cf_om_fixed");
	});
	return result;
}

SAM_EXPORT double* SAM_Pvwattsv8_Outputs_cf_om_fuel_cost_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_om_fuel_cost", length);
	if (!result)
		make_access_error("SAM_Pvwattsv8", "cf_om_fuel_cost");
	});
	return result;
}

SAM_EXPORT double* SAM_Pvwattsv8_Outputs_cf_om_land_lease_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_om_land_lease", length);
	if (!result)
		make_access_error("SAM_Pvwattsv8", "cf_om_land_lease");
	});
	return result;
}

SAM_EXPORT double* SAM_Pvwattsv8_Outputs_cf_om_production_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_om_production", length);
	if (!result)
		make_access_error("SAM_Pvwattsv8", "cf_om_production");
	});
	return result;
}

SAM_EXPORT const char* SAM_Pvwattsv8_Outputs_city_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "city");
	if (!result)
		make_access_error("SAM_Pvwattsv8", "city");
	});
	return result;
}

SAM_EXPORT double* SAM_Pvwattsv8_Outputs_dc_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dc", length);
	if (!result)
		make_access_error("SAM_Pvwattsv8", "dc");
	});
	return result;
}

SAM_EXPORT double* SAM_Pvwattsv8_Outputs_dc_monthly_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dc_monthly", length);
	if (!result)
		make_access_error("SAM_Pvwattsv8", "dc_monthly");
	});
	return result;
}

SAM_EXPORT double* SAM_Pvwattsv8_Outputs_dcsnowderate_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dcsnowderate", length);
	if (!result)
		make_access_error("SAM_Pvwattsv8", "dcsnowderate");
	});
	return result;
}

SAM_EXPORT double* SAM_Pvwattsv8_Outputs_df_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "df", length);
	if (!result)
		make_access_error("SAM_Pvwattsv8", "df");
	});
	return result;
}

SAM_EXPORT double* SAM_Pvwattsv8_Outputs_dn_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dn", length);
	if (!result)
		make_access_error("SAM_Pvwattsv8", "dn");
	});
	return result;
}

SAM_EXPORT double SAM_Pvwattsv8_Outputs_elev_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "elev", &result))
		make_access_error("SAM_Pvwattsv8", "elev");
	});
	return result;
}

SAM_EXPORT double* SAM_Pvwattsv8_Outputs_gen_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "gen", length);
	if (!result)
		make_access_error("SAM_Pvwattsv8", "gen");
	});
	return result;
}

SAM_EXPORT double* SAM_Pvwattsv8_Outputs_gh_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "gh", length);
	if (!result)
		make_access_error("SAM_Pvwattsv8", "gh");
	});
	return result;
}

SAM_EXPORT double* SAM_Pvwattsv8_Outputs_inv_eff_output_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "inv_eff_output", length);
	if (!result)
		make_access_error("SAM_Pvwattsv8", "inv_eff_output");
	});
	return result;
}

SAM_EXPORT double SAM_Pvwattsv8_Outputs_inverter_efficiency_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inverter_efficiency", &result))
		make_access_error("SAM_Pvwattsv8", "inverter_efficiency");
	});
	return result;
}

SAM_EXPORT double SAM_Pvwattsv8_Outputs_kwh_per_kw_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "kwh_per_kw", &result))
		make_access_error("SAM_Pvwattsv8", "kwh_per_kw");
	});
	return result;
}

SAM_EXPORT double SAM_Pvwattsv8_Outputs_lat_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "lat", &result))
		make_access_error("SAM_Pvwattsv8", "lat");
	});
	return result;
}

SAM_EXPORT const char* SAM_Pvwattsv8_Outputs_location_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "location");
	if (!result)
		make_access_error("SAM_Pvwattsv8", "location");
	});
	return result;
}

SAM_EXPORT double SAM_Pvwattsv8_Outputs_lon_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "lon", &result))
		make_access_error("SAM_Pvwattsv8", "lon");
	});
	return result;
}

SAM_EXPORT double* SAM_Pvwattsv8_Outputs_monthly_energy_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_energy", length);
	if (!result)
		make_access_error("SAM_Pvwattsv8", "monthly_energy");
	});
	return result;
}

SAM_EXPORT double SAM_Pvwattsv8_Outputs_percent_complete_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "percent_complete", &result))
		make_access_error("SAM_Pvwattsv8", "percent_complete");
	});
	return result;
}

SAM_EXPORT double* SAM_Pvwattsv8_Outputs_poa_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "poa", length);
	if (!result)
		make_access_error("SAM_Pvwattsv8", "poa");
	});
	return result;
}

SAM_EXPORT double* SAM_Pvwattsv8_Outputs_poa_monthly_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "poa_monthly", length);
	if (!result)
		make_access_error("SAM_Pvwattsv8", "poa_monthly");
	});
	return result;
}

SAM_EXPORT double* SAM_Pvwattsv8_Outputs_shad_beam_factor_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "shad_beam_factor", length);
	if (!result)
		make_access_error("SAM_Pvwattsv8", "shad_beam_factor");
	});
	return result;
}

SAM_EXPORT double* SAM_Pvwattsv8_Outputs_snow_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "snow", length);
	if (!result)
		make_access_error("SAM_Pvwattsv8", "snow");
	});
	return result;
}

SAM_EXPORT double* SAM_Pvwattsv8_Outputs_snow_cover_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "snow_cover", length);
	if (!result)
		make_access_error("SAM_Pvwattsv8", "snow_cover");
	});
	return result;
}

SAM_EXPORT double* SAM_Pvwattsv8_Outputs_soiling_f_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "soiling_f", length);
	if (!result)
		make_access_error("SAM_Pvwattsv8", "soiling_f");
	});
	return result;
}

SAM_EXPORT double SAM_Pvwattsv8_Outputs_solrad_annual_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "solrad_annual", &result))
		make_access_error("SAM_Pvwattsv8", "solrad_annual");
	});
	return result;
}

SAM_EXPORT double* SAM_Pvwattsv8_Outputs_solrad_monthly_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "solrad_monthly", length);
	if (!result)
		make_access_error("SAM_Pvwattsv8", "solrad_monthly");
	});
	return result;
}

SAM_EXPORT double* SAM_Pvwattsv8_Outputs_ss_beam_factor_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "ss_beam_factor", length);
	if (!result)
		make_access_error("SAM_Pvwattsv8", "ss_beam_factor");
	});
	return result;
}

SAM_EXPORT double* SAM_Pvwattsv8_Outputs_ss_gnd_diffuse_factor_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "ss_gnd_diffuse_factor", length);
	if (!result)
		make_access_error("SAM_Pvwattsv8", "ss_gnd_diffuse_factor");
	});
	return result;
}

SAM_EXPORT double* SAM_Pvwattsv8_Outputs_ss_sky_diffuse_factor_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "ss_sky_diffuse_factor", length);
	if (!result)
		make_access_error("SAM_Pvwattsv8", "ss_sky_diffuse_factor");
	});
	return result;
}

SAM_EXPORT const char* SAM_Pvwattsv8_Outputs_state_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "state");
	if (!result)
		make_access_error("SAM_Pvwattsv8", "state");
	});
	return result;
}

SAM_EXPORT double* SAM_Pvwattsv8_Outputs_sunup_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "sunup", length);
	if (!result)
		make_access_error("SAM_Pvwattsv8", "sunup");
	});
	return result;
}

SAM_EXPORT double SAM_Pvwattsv8_Outputs_system_capacity_ac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_capacity_ac", &result))
		make_access_error("SAM_Pvwattsv8", "system_capacity_ac");
	});
	return result;
}

SAM_EXPORT double* SAM_Pvwattsv8_Outputs_tamb_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tamb", length);
	if (!result)
		make_access_error("SAM_Pvwattsv8", "tamb");
	});
	return result;
}

SAM_EXPORT double* SAM_Pvwattsv8_Outputs_tcell_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tcell", length);
	if (!result)
		make_access_error("SAM_Pvwattsv8", "tcell");
	});
	return result;
}

SAM_EXPORT double* SAM_Pvwattsv8_Outputs_tpoa_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tpoa", length);
	if (!result)
		make_access_error("SAM_Pvwattsv8", "tpoa");
	});
	return result;
}

SAM_EXPORT double SAM_Pvwattsv8_Outputs_ts_shift_hours_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ts_shift_hours", &result))
		make_access_error("SAM_Pvwattsv8", "ts_shift_hours");
	});
	return result;
}

SAM_EXPORT double SAM_Pvwattsv8_Outputs_tz_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tz", &result))
		make_access_error("SAM_Pvwattsv8", "tz");
	});
	return result;
}

SAM_EXPORT double* SAM_Pvwattsv8_Outputs_wspd_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "wspd", length);
	if (!result)
		make_access_error("SAM_Pvwattsv8", "wspd");
	});
	return result;
}

