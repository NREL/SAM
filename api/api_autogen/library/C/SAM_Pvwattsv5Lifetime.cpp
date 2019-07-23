#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_Pvwattsv5Lifetime.h"

SAM_EXPORT SAM_Pvwattsv5Lifetime SAM_Pvwattsv5Lifetime_construct(const char* def, SAM_error* err){
	SAM_Pvwattsv5Lifetime result = nullptr;
	translateExceptions(err, [&]{
		result = ssc_data_create();
	});
	return result;
}

SAM_EXPORT int SAM_Pvwattsv5Lifetime_execute(SAM_Pvwattsv5Lifetime data, int verbosity, SAM_error* err){
	int n_err = 0;
	translateExceptions(err, [&]{
		n_err += SAM_module_exec("pvwattsv5_lifetime", data, verbosity, err);
	});
	return n_err;
}


SAM_EXPORT void SAM_Pvwattsv5Lifetime_destruct(SAM_Pvwattsv5Lifetime system)
{
	ssc_data_free(system);
}

SAM_EXPORT void SAM_Pvwattsv5Lifetime_Common_system_use_lifetime_output_nset(SAM_Pvwattsv5Lifetime ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "system_use_lifetime_output", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv5Lifetime_FinancialAnalysisParameters_analysis_period_nset(SAM_Pvwattsv5Lifetime ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "analysis_period", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv5Lifetime_LifetimePV_dc_degradation_aset(SAM_Pvwattsv5Lifetime ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "dc_degradation", arr, length);
	});
}

SAM_EXPORT void SAM_Pvwattsv5Lifetime_Weather_solar_resource_data_tset(SAM_Pvwattsv5Lifetime ptr, SAM_table tab, SAM_error *err){
	SAM_table_set_table(ptr, "solar_resource_data", tab, err);
}



SAM_EXPORT void SAM_Pvwattsv5Lifetime_Weather_solar_resource_file_sset(SAM_Pvwattsv5Lifetime ptr, const char* str, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_string(ptr, "solar_resource_file", str);
	});
}

SAM_EXPORT void SAM_Pvwattsv5Lifetime_PVWatts_array_type_nset(SAM_Pvwattsv5Lifetime ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "array_type", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv5Lifetime_PVWatts_azimuth_nset(SAM_Pvwattsv5Lifetime ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "azimuth", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv5Lifetime_PVWatts_dc_ac_ratio_nset(SAM_Pvwattsv5Lifetime ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dc_ac_ratio", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv5Lifetime_PVWatts_gcr_nset(SAM_Pvwattsv5Lifetime ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "gcr", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv5Lifetime_PVWatts_inv_eff_nset(SAM_Pvwattsv5Lifetime ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inv_eff", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv5Lifetime_PVWatts_losses_nset(SAM_Pvwattsv5Lifetime ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "losses", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv5Lifetime_PVWatts_module_type_nset(SAM_Pvwattsv5Lifetime ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "module_type", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv5Lifetime_PVWatts_shading_azal_mset(SAM_Pvwattsv5Lifetime ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "shading:azal", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Pvwattsv5Lifetime_PVWatts_shading_diff_nset(SAM_Pvwattsv5Lifetime ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "shading:diff", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv5Lifetime_PVWatts_shading_mxh_mset(SAM_Pvwattsv5Lifetime ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "shading:mxh", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Pvwattsv5Lifetime_PVWatts_shading_timestep_mset(SAM_Pvwattsv5Lifetime ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "shading:timestep", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Pvwattsv5Lifetime_PVWatts_system_capacity_nset(SAM_Pvwattsv5Lifetime ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "system_capacity", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv5Lifetime_PVWatts_tilt_nset(SAM_Pvwattsv5Lifetime ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tilt", number);
	});
}

SAM_EXPORT void SAM_Pvwattsv5Lifetime_Battwatts_batt_simple_enable_nset(SAM_Pvwattsv5Lifetime ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_simple_enable", number);
	});
}

SAM_EXPORT double SAM_Pvwattsv5Lifetime_Common_system_use_lifetime_output_nget(SAM_Pvwattsv5Lifetime ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_use_lifetime_output", &result))
		make_access_error("SAM_Pvwattsv5Lifetime", "system_use_lifetime_output");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv5Lifetime_FinancialAnalysisParameters_analysis_period_nget(SAM_Pvwattsv5Lifetime ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "analysis_period", &result))
		make_access_error("SAM_Pvwattsv5Lifetime", "analysis_period");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvwattsv5Lifetime_LifetimePV_dc_degradation_aget(SAM_Pvwattsv5Lifetime ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dc_degradation", length);
	if (!result)
		make_access_error("SAM_Pvwattsv5Lifetime", "dc_degradation");
	});
	return result;
}



SAM_EXPORT SAM_table SAM_Pvwattsv5Lifetime_Weather_solar_resource_data_tget(SAM_Pvwattsv5Lifetime ptr, SAM_error *err){
	SAM_table result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_table(ptr, "solar_resource_data");
	if (!result)
		make_access_error("SAM_Pvwattsv5Lifetime", "solar_resource_data");
	});
	return result;
}



SAM_EXPORT const char* SAM_Pvwattsv5Lifetime_Weather_solar_resource_file_sget(SAM_Pvwattsv5Lifetime ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "solar_resource_file");
	if (!result)
		make_access_error("SAM_Pvwattsv5Lifetime", "solar_resource_file");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv5Lifetime_PVWatts_array_type_nget(SAM_Pvwattsv5Lifetime ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "array_type", &result))
		make_access_error("SAM_Pvwattsv5Lifetime", "array_type");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv5Lifetime_PVWatts_azimuth_nget(SAM_Pvwattsv5Lifetime ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "azimuth", &result))
		make_access_error("SAM_Pvwattsv5Lifetime", "azimuth");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv5Lifetime_PVWatts_dc_ac_ratio_nget(SAM_Pvwattsv5Lifetime ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dc_ac_ratio", &result))
		make_access_error("SAM_Pvwattsv5Lifetime", "dc_ac_ratio");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv5Lifetime_PVWatts_gcr_nget(SAM_Pvwattsv5Lifetime ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "gcr", &result))
		make_access_error("SAM_Pvwattsv5Lifetime", "gcr");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv5Lifetime_PVWatts_inv_eff_nget(SAM_Pvwattsv5Lifetime ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inv_eff", &result))
		make_access_error("SAM_Pvwattsv5Lifetime", "inv_eff");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv5Lifetime_PVWatts_losses_nget(SAM_Pvwattsv5Lifetime ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "losses", &result))
		make_access_error("SAM_Pvwattsv5Lifetime", "losses");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv5Lifetime_PVWatts_module_type_nget(SAM_Pvwattsv5Lifetime ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "module_type", &result))
		make_access_error("SAM_Pvwattsv5Lifetime", "module_type");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvwattsv5Lifetime_PVWatts_shading_azal_mget(SAM_Pvwattsv5Lifetime ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "shading:azal", nrows, ncols);
	if (!result)
		make_access_error("SAM_Pvwattsv5Lifetime", "shading:azal");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv5Lifetime_PVWatts_shading_diff_nget(SAM_Pvwattsv5Lifetime ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "shading:diff", &result))
		make_access_error("SAM_Pvwattsv5Lifetime", "shading:diff");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvwattsv5Lifetime_PVWatts_shading_mxh_mget(SAM_Pvwattsv5Lifetime ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "shading:mxh", nrows, ncols);
	if (!result)
		make_access_error("SAM_Pvwattsv5Lifetime", "shading:mxh");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvwattsv5Lifetime_PVWatts_shading_timestep_mget(SAM_Pvwattsv5Lifetime ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "shading:timestep", nrows, ncols);
	if (!result)
		make_access_error("SAM_Pvwattsv5Lifetime", "shading:timestep");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv5Lifetime_PVWatts_system_capacity_nget(SAM_Pvwattsv5Lifetime ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_capacity", &result))
		make_access_error("SAM_Pvwattsv5Lifetime", "system_capacity");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv5Lifetime_PVWatts_tilt_nget(SAM_Pvwattsv5Lifetime ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tilt", &result))
		make_access_error("SAM_Pvwattsv5Lifetime", "tilt");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv5Lifetime_Battwatts_batt_simple_enable_nget(SAM_Pvwattsv5Lifetime ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_simple_enable", &result))
		make_access_error("SAM_Pvwattsv5Lifetime", "batt_simple_enable");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvwattsv5Lifetime_Outputs_ac_aget(SAM_Pvwattsv5Lifetime ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "ac", length);
	if (!result)
		make_access_error("SAM_Pvwattsv5Lifetime", "ac");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv5Lifetime_Outputs_ac_annual_nget(SAM_Pvwattsv5Lifetime ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ac_annual", &result))
		make_access_error("SAM_Pvwattsv5Lifetime", "ac_annual");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvwattsv5Lifetime_Outputs_ac_monthly_aget(SAM_Pvwattsv5Lifetime ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "ac_monthly", length);
	if (!result)
		make_access_error("SAM_Pvwattsv5Lifetime", "ac_monthly");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv5Lifetime_Outputs_annual_energy_nget(SAM_Pvwattsv5Lifetime ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_energy", &result))
		make_access_error("SAM_Pvwattsv5Lifetime", "annual_energy");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvwattsv5Lifetime_Outputs_aoi_aget(SAM_Pvwattsv5Lifetime ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "aoi", length);
	if (!result)
		make_access_error("SAM_Pvwattsv5Lifetime", "aoi");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv5Lifetime_Outputs_capacity_factor_nget(SAM_Pvwattsv5Lifetime ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "capacity_factor", &result))
		make_access_error("SAM_Pvwattsv5Lifetime", "capacity_factor");
	});
	return result;
}



SAM_EXPORT const char* SAM_Pvwattsv5Lifetime_Outputs_city_sget(SAM_Pvwattsv5Lifetime ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "city");
	if (!result)
		make_access_error("SAM_Pvwattsv5Lifetime", "city");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvwattsv5Lifetime_Outputs_dc_aget(SAM_Pvwattsv5Lifetime ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dc", length);
	if (!result)
		make_access_error("SAM_Pvwattsv5Lifetime", "dc");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvwattsv5Lifetime_Outputs_dc_monthly_aget(SAM_Pvwattsv5Lifetime ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dc_monthly", length);
	if (!result)
		make_access_error("SAM_Pvwattsv5Lifetime", "dc_monthly");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvwattsv5Lifetime_Outputs_df_aget(SAM_Pvwattsv5Lifetime ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "df", length);
	if (!result)
		make_access_error("SAM_Pvwattsv5Lifetime", "df");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvwattsv5Lifetime_Outputs_dn_aget(SAM_Pvwattsv5Lifetime ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dn", length);
	if (!result)
		make_access_error("SAM_Pvwattsv5Lifetime", "dn");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv5Lifetime_Outputs_elev_nget(SAM_Pvwattsv5Lifetime ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "elev", &result))
		make_access_error("SAM_Pvwattsv5Lifetime", "elev");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvwattsv5Lifetime_Outputs_gh_aget(SAM_Pvwattsv5Lifetime ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "gh", length);
	if (!result)
		make_access_error("SAM_Pvwattsv5Lifetime", "gh");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv5Lifetime_Outputs_inverter_efficiency_nget(SAM_Pvwattsv5Lifetime ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inverter_efficiency", &result))
		make_access_error("SAM_Pvwattsv5Lifetime", "inverter_efficiency");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv5Lifetime_Outputs_inverter_model_nget(SAM_Pvwattsv5Lifetime ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inverter_model", &result))
		make_access_error("SAM_Pvwattsv5Lifetime", "inverter_model");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv5Lifetime_Outputs_kwh_per_kw_nget(SAM_Pvwattsv5Lifetime ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "kwh_per_kw", &result))
		make_access_error("SAM_Pvwattsv5Lifetime", "kwh_per_kw");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv5Lifetime_Outputs_lat_nget(SAM_Pvwattsv5Lifetime ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "lat", &result))
		make_access_error("SAM_Pvwattsv5Lifetime", "lat");
	});
	return result;
}



SAM_EXPORT const char* SAM_Pvwattsv5Lifetime_Outputs_location_sget(SAM_Pvwattsv5Lifetime ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "location");
	if (!result)
		make_access_error("SAM_Pvwattsv5Lifetime", "location");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv5Lifetime_Outputs_lon_nget(SAM_Pvwattsv5Lifetime ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "lon", &result))
		make_access_error("SAM_Pvwattsv5Lifetime", "lon");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvwattsv5Lifetime_Outputs_monthly_energy_aget(SAM_Pvwattsv5Lifetime ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_energy", length);
	if (!result)
		make_access_error("SAM_Pvwattsv5Lifetime", "monthly_energy");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv5Lifetime_Outputs_percent_complete_nget(SAM_Pvwattsv5Lifetime ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "percent_complete", &result))
		make_access_error("SAM_Pvwattsv5Lifetime", "percent_complete");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvwattsv5Lifetime_Outputs_poa_aget(SAM_Pvwattsv5Lifetime ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "poa", length);
	if (!result)
		make_access_error("SAM_Pvwattsv5Lifetime", "poa");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvwattsv5Lifetime_Outputs_poa_monthly_aget(SAM_Pvwattsv5Lifetime ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "poa_monthly", length);
	if (!result)
		make_access_error("SAM_Pvwattsv5Lifetime", "poa_monthly");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvwattsv5Lifetime_Outputs_shad_beam_factor_aget(SAM_Pvwattsv5Lifetime ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "shad_beam_factor", length);
	if (!result)
		make_access_error("SAM_Pvwattsv5Lifetime", "shad_beam_factor");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv5Lifetime_Outputs_solrad_annual_nget(SAM_Pvwattsv5Lifetime ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "solrad_annual", &result))
		make_access_error("SAM_Pvwattsv5Lifetime", "solrad_annual");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvwattsv5Lifetime_Outputs_solrad_monthly_aget(SAM_Pvwattsv5Lifetime ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "solrad_monthly", length);
	if (!result)
		make_access_error("SAM_Pvwattsv5Lifetime", "solrad_monthly");
	});
	return result;
}



SAM_EXPORT const char* SAM_Pvwattsv5Lifetime_Outputs_state_sget(SAM_Pvwattsv5Lifetime ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "state");
	if (!result)
		make_access_error("SAM_Pvwattsv5Lifetime", "state");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvwattsv5Lifetime_Outputs_sunup_aget(SAM_Pvwattsv5Lifetime ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "sunup", length);
	if (!result)
		make_access_error("SAM_Pvwattsv5Lifetime", "sunup");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvwattsv5Lifetime_Outputs_tamb_aget(SAM_Pvwattsv5Lifetime ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tamb", length);
	if (!result)
		make_access_error("SAM_Pvwattsv5Lifetime", "tamb");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvwattsv5Lifetime_Outputs_tcell_aget(SAM_Pvwattsv5Lifetime ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tcell", length);
	if (!result)
		make_access_error("SAM_Pvwattsv5Lifetime", "tcell");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvwattsv5Lifetime_Outputs_tpoa_aget(SAM_Pvwattsv5Lifetime ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tpoa", length);
	if (!result)
		make_access_error("SAM_Pvwattsv5Lifetime", "tpoa");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv5Lifetime_Outputs_ts_shift_hours_nget(SAM_Pvwattsv5Lifetime ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ts_shift_hours", &result))
		make_access_error("SAM_Pvwattsv5Lifetime", "ts_shift_hours");
	});
	return result;
}



SAM_EXPORT double SAM_Pvwattsv5Lifetime_Outputs_tz_nget(SAM_Pvwattsv5Lifetime ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tz", &result))
		make_access_error("SAM_Pvwattsv5Lifetime", "tz");
	});
	return result;
}



SAM_EXPORT double* SAM_Pvwattsv5Lifetime_Outputs_wspd_aget(SAM_Pvwattsv5Lifetime ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "wspd", length);
	if (!result)
		make_access_error("SAM_Pvwattsv5Lifetime", "wspd");
	});
	return result;
}



