#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_Windpower.h"

SAM_EXPORT SAM_Windpower SAM_Windpower_construct(const char* def, SAM_error* err){
	SAM_Windpower result = nullptr;
	translateExceptions(err, [&]{
		result = ssc_data_create();
	});
	return result;
}

SAM_EXPORT int SAM_Windpower_execute(SAM_Windpower data, int verbosity, SAM_error* err){
	int n_err = 0;
	translateExceptions(err, [&]{
		n_err += SAM_module_exec("windpower", data, verbosity, err);
	});
	return n_err;
}


SAM_EXPORT void SAM_Windpower_destruct(SAM_Windpower system)
{
	ssc_data_free(system);
}

SAM_EXPORT void SAM_Windpower_WindResourceFile_wind_resource_data_tset(SAM_Windpower ptr, SAM_table tab, SAM_error *err){
	SAM_table_set_table(ptr, "wind_resource_data", tab, err);
}



SAM_EXPORT void SAM_Windpower_WindResourceFile_wind_resource_filename_sset(SAM_Windpower ptr, const char* str, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_string(ptr, "wind_resource_filename", str);
	});
}

SAM_EXPORT void SAM_Windpower_WindTurbine_wind_resource_shear_nset(SAM_Windpower ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "wind_resource_shear", number);
	});
}

SAM_EXPORT void SAM_Windpower_WindTurbine_wind_turbine_hub_ht_nset(SAM_Windpower ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "wind_turbine_hub_ht", number);
	});
}

SAM_EXPORT void SAM_Windpower_WindTurbine_wind_turbine_max_cp_nset(SAM_Windpower ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "wind_turbine_max_cp", number);
	});
}

SAM_EXPORT void SAM_Windpower_WindTurbine_wind_turbine_powercurve_powerout_aset(SAM_Windpower ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "wind_turbine_powercurve_powerout", arr, length);
	});
}

SAM_EXPORT void SAM_Windpower_WindTurbine_wind_turbine_powercurve_windspeeds_aset(SAM_Windpower ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "wind_turbine_powercurve_windspeeds", arr, length);
	});
}

SAM_EXPORT void SAM_Windpower_WindTurbine_wind_turbine_rotor_diameter_nset(SAM_Windpower ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "wind_turbine_rotor_diameter", number);
	});
}

SAM_EXPORT void SAM_Windpower_WindFarm_system_capacity_nset(SAM_Windpower ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "system_capacity", number);
	});
}

SAM_EXPORT void SAM_Windpower_WindFarm_wind_farm_losses_percent_nset(SAM_Windpower ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "wind_farm_losses_percent", number);
	});
}

SAM_EXPORT void SAM_Windpower_WindFarm_wind_farm_wake_model_nset(SAM_Windpower ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "wind_farm_wake_model", number);
	});
}

SAM_EXPORT void SAM_Windpower_WindFarm_wind_farm_xCoordinates_aset(SAM_Windpower ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "wind_farm_xCoordinates", arr, length);
	});
}

SAM_EXPORT void SAM_Windpower_WindFarm_wind_farm_yCoordinates_aset(SAM_Windpower ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "wind_farm_yCoordinates", arr, length);
	});
}

SAM_EXPORT void SAM_Windpower_WindFarm_wind_resource_turbulence_coeff_nset(SAM_Windpower ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "wind_resource_turbulence_coeff", number);
	});
}

SAM_EXPORT void SAM_Windpower_WindPower_en_icing_cutoff_nset(SAM_Windpower ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "en_icing_cutoff", number);
	});
}

SAM_EXPORT void SAM_Windpower_WindPower_en_low_temp_cutoff_nset(SAM_Windpower ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "en_low_temp_cutoff", number);
	});
}

SAM_EXPORT void SAM_Windpower_WindPower_icing_cutoff_rh_nset(SAM_Windpower ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "icing_cutoff_rh", number);
	});
}

SAM_EXPORT void SAM_Windpower_WindPower_icing_cutoff_temp_nset(SAM_Windpower ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "icing_cutoff_temp", number);
	});
}

SAM_EXPORT void SAM_Windpower_WindPower_low_temp_cutoff_nset(SAM_Windpower ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "low_temp_cutoff", number);
	});
}

SAM_EXPORT void SAM_Windpower_WindPower_wind_resource_model_choice_nset(SAM_Windpower ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "wind_resource_model_choice", number);
	});
}

SAM_EXPORT void SAM_Windpower_WindSpeedWeibullDistribution_weibull_k_factor_nset(SAM_Windpower ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "weibull_k_factor", number);
	});
}

SAM_EXPORT void SAM_Windpower_WindSpeedWeibullDistribution_weibull_reference_height_nset(SAM_Windpower ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "weibull_reference_height", number);
	});
}

SAM_EXPORT void SAM_Windpower_WindSpeedWeibullDistribution_weibull_wind_speed_nset(SAM_Windpower ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "weibull_wind_speed", number);
	});
}

SAM_EXPORT SAM_table SAM_Windpower_WindResourceFile_wind_resource_data_tget(SAM_Windpower ptr, SAM_error *err){
	SAM_table result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_table(ptr, "wind_resource_data");
	if (!result)
		make_access_error("SAM_Windpower", "wind_resource_data");
	});
	return result;
}



SAM_EXPORT const char* SAM_Windpower_WindResourceFile_wind_resource_filename_sget(SAM_Windpower ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "wind_resource_filename");
	if (!result)
		make_access_error("SAM_Windpower", "wind_resource_filename");
	});
	return result;
}



SAM_EXPORT double SAM_Windpower_WindTurbine_wind_resource_shear_nget(SAM_Windpower ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "wind_resource_shear", &result))
		make_access_error("SAM_Windpower", "wind_resource_shear");
	});
	return result;
}



SAM_EXPORT double SAM_Windpower_WindTurbine_wind_turbine_hub_ht_nget(SAM_Windpower ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "wind_turbine_hub_ht", &result))
		make_access_error("SAM_Windpower", "wind_turbine_hub_ht");
	});
	return result;
}



SAM_EXPORT double SAM_Windpower_WindTurbine_wind_turbine_max_cp_nget(SAM_Windpower ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "wind_turbine_max_cp", &result))
		make_access_error("SAM_Windpower", "wind_turbine_max_cp");
	});
	return result;
}



SAM_EXPORT double* SAM_Windpower_WindTurbine_wind_turbine_powercurve_powerout_aget(SAM_Windpower ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "wind_turbine_powercurve_powerout", length);
	if (!result)
		make_access_error("SAM_Windpower", "wind_turbine_powercurve_powerout");
	});
	return result;
}



SAM_EXPORT double* SAM_Windpower_WindTurbine_wind_turbine_powercurve_windspeeds_aget(SAM_Windpower ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "wind_turbine_powercurve_windspeeds", length);
	if (!result)
		make_access_error("SAM_Windpower", "wind_turbine_powercurve_windspeeds");
	});
	return result;
}



SAM_EXPORT double SAM_Windpower_WindTurbine_wind_turbine_rotor_diameter_nget(SAM_Windpower ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "wind_turbine_rotor_diameter", &result))
		make_access_error("SAM_Windpower", "wind_turbine_rotor_diameter");
	});
	return result;
}



SAM_EXPORT double SAM_Windpower_WindFarm_system_capacity_nget(SAM_Windpower ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_capacity", &result))
		make_access_error("SAM_Windpower", "system_capacity");
	});
	return result;
}



SAM_EXPORT double SAM_Windpower_WindFarm_wind_farm_losses_percent_nget(SAM_Windpower ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "wind_farm_losses_percent", &result))
		make_access_error("SAM_Windpower", "wind_farm_losses_percent");
	});
	return result;
}



SAM_EXPORT double SAM_Windpower_WindFarm_wind_farm_wake_model_nget(SAM_Windpower ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "wind_farm_wake_model", &result))
		make_access_error("SAM_Windpower", "wind_farm_wake_model");
	});
	return result;
}



SAM_EXPORT double* SAM_Windpower_WindFarm_wind_farm_xCoordinates_aget(SAM_Windpower ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "wind_farm_xCoordinates", length);
	if (!result)
		make_access_error("SAM_Windpower", "wind_farm_xCoordinates");
	});
	return result;
}



SAM_EXPORT double* SAM_Windpower_WindFarm_wind_farm_yCoordinates_aget(SAM_Windpower ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "wind_farm_yCoordinates", length);
	if (!result)
		make_access_error("SAM_Windpower", "wind_farm_yCoordinates");
	});
	return result;
}



SAM_EXPORT double SAM_Windpower_WindFarm_wind_resource_turbulence_coeff_nget(SAM_Windpower ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "wind_resource_turbulence_coeff", &result))
		make_access_error("SAM_Windpower", "wind_resource_turbulence_coeff");
	});
	return result;
}



SAM_EXPORT double SAM_Windpower_WindPower_en_icing_cutoff_nget(SAM_Windpower ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "en_icing_cutoff", &result))
		make_access_error("SAM_Windpower", "en_icing_cutoff");
	});
	return result;
}



SAM_EXPORT double SAM_Windpower_WindPower_en_low_temp_cutoff_nget(SAM_Windpower ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "en_low_temp_cutoff", &result))
		make_access_error("SAM_Windpower", "en_low_temp_cutoff");
	});
	return result;
}



SAM_EXPORT double SAM_Windpower_WindPower_icing_cutoff_rh_nget(SAM_Windpower ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "icing_cutoff_rh", &result))
		make_access_error("SAM_Windpower", "icing_cutoff_rh");
	});
	return result;
}



SAM_EXPORT double SAM_Windpower_WindPower_icing_cutoff_temp_nget(SAM_Windpower ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "icing_cutoff_temp", &result))
		make_access_error("SAM_Windpower", "icing_cutoff_temp");
	});
	return result;
}



SAM_EXPORT double SAM_Windpower_WindPower_low_temp_cutoff_nget(SAM_Windpower ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "low_temp_cutoff", &result))
		make_access_error("SAM_Windpower", "low_temp_cutoff");
	});
	return result;
}



SAM_EXPORT double SAM_Windpower_WindPower_wind_resource_model_choice_nget(SAM_Windpower ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "wind_resource_model_choice", &result))
		make_access_error("SAM_Windpower", "wind_resource_model_choice");
	});
	return result;
}



SAM_EXPORT double SAM_Windpower_WindSpeedWeibullDistribution_weibull_k_factor_nget(SAM_Windpower ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "weibull_k_factor", &result))
		make_access_error("SAM_Windpower", "weibull_k_factor");
	});
	return result;
}



SAM_EXPORT double SAM_Windpower_WindSpeedWeibullDistribution_weibull_reference_height_nget(SAM_Windpower ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "weibull_reference_height", &result))
		make_access_error("SAM_Windpower", "weibull_reference_height");
	});
	return result;
}



SAM_EXPORT double SAM_Windpower_WindSpeedWeibullDistribution_weibull_wind_speed_nget(SAM_Windpower ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "weibull_wind_speed", &result))
		make_access_error("SAM_Windpower", "weibull_wind_speed");
	});
	return result;
}



SAM_EXPORT double SAM_Windpower_Outputs_annual_energy_nget(SAM_Windpower ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_energy", &result))
		make_access_error("SAM_Windpower", "annual_energy");
	});
	return result;
}



SAM_EXPORT double SAM_Windpower_Outputs_capacity_factor_nget(SAM_Windpower ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "capacity_factor", &result))
		make_access_error("SAM_Windpower", "capacity_factor");
	});
	return result;
}



SAM_EXPORT double SAM_Windpower_Outputs_cutoff_losses_nget(SAM_Windpower ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cutoff_losses", &result))
		make_access_error("SAM_Windpower", "cutoff_losses");
	});
	return result;
}



SAM_EXPORT double* SAM_Windpower_Outputs_gen_aget(SAM_Windpower ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "gen", length);
	if (!result)
		make_access_error("SAM_Windpower", "gen");
	});
	return result;
}



SAM_EXPORT double SAM_Windpower_Outputs_kwh_per_kw_nget(SAM_Windpower ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "kwh_per_kw", &result))
		make_access_error("SAM_Windpower", "kwh_per_kw");
	});
	return result;
}



SAM_EXPORT double* SAM_Windpower_Outputs_monthly_energy_aget(SAM_Windpower ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_energy", length);
	if (!result)
		make_access_error("SAM_Windpower", "monthly_energy");
	});
	return result;
}



SAM_EXPORT double* SAM_Windpower_Outputs_pressure_aget(SAM_Windpower ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pressure", length);
	if (!result)
		make_access_error("SAM_Windpower", "pressure");
	});
	return result;
}



SAM_EXPORT double* SAM_Windpower_Outputs_temp_aget(SAM_Windpower ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "temp", length);
	if (!result)
		make_access_error("SAM_Windpower", "temp");
	});
	return result;
}



SAM_EXPORT double* SAM_Windpower_Outputs_turbine_output_by_windspeed_bin_aget(SAM_Windpower ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "turbine_output_by_windspeed_bin", length);
	if (!result)
		make_access_error("SAM_Windpower", "turbine_output_by_windspeed_bin");
	});
	return result;
}



SAM_EXPORT double* SAM_Windpower_Outputs_wind_direction_aget(SAM_Windpower ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "wind_direction", length);
	if (!result)
		make_access_error("SAM_Windpower", "wind_direction");
	});
	return result;
}



SAM_EXPORT double* SAM_Windpower_Outputs_wind_speed_aget(SAM_Windpower ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "wind_speed", length);
	if (!result)
		make_access_error("SAM_Windpower", "wind_speed");
	});
	return result;
}



