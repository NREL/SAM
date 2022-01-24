#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_MhkWave.h"

SAM_EXPORT int SAM_MhkWave_execute(SAM_table data, int verbosity, SAM_error* err){
	return SAM_module_exec("mhk_wave", data, verbosity, err);
}

SAM_EXPORT void SAM_MhkWave_MHKWave_balance_of_system_cost_total_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "balance_of_system_cost_total", number);
	});
}

SAM_EXPORT void SAM_MhkWave_MHKWave_day_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "day", arr, length);
	});
}

SAM_EXPORT void SAM_MhkWave_MHKWave_device_costs_total_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "device_costs_total", number);
	});
}

SAM_EXPORT void SAM_MhkWave_MHKWave_device_rated_power_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "device_rated_power", number);
	});
}

SAM_EXPORT void SAM_MhkWave_MHKWave_energy_period_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "energy_period", arr, length);
	});
}

SAM_EXPORT void SAM_MhkWave_MHKWave_financial_cost_total_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "financial_cost_total", number);
	});
}

SAM_EXPORT void SAM_MhkWave_MHKWave_fixed_charge_rate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fixed_charge_rate", number);
	});
}

SAM_EXPORT void SAM_MhkWave_MHKWave_hour_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "hour", arr, length);
	});
}

SAM_EXPORT void SAM_MhkWave_MHKWave_loss_additional_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "loss_additional", number);
	});
}

SAM_EXPORT void SAM_MhkWave_MHKWave_loss_array_spacing_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "loss_array_spacing", number);
	});
}

SAM_EXPORT void SAM_MhkWave_MHKWave_loss_downtime_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "loss_downtime", number);
	});
}

SAM_EXPORT void SAM_MhkWave_MHKWave_loss_resource_overprediction_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "loss_resource_overprediction", number);
	});
}

SAM_EXPORT void SAM_MhkWave_MHKWave_loss_transmission_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "loss_transmission", number);
	});
}

SAM_EXPORT void SAM_MhkWave_MHKWave_minute_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "minute", arr, length);
	});
}

SAM_EXPORT void SAM_MhkWave_MHKWave_month_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "month", arr, length);
	});
}

SAM_EXPORT void SAM_MhkWave_MHKWave_number_devices_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "number_devices", number);
	});
}

SAM_EXPORT void SAM_MhkWave_MHKWave_number_hours_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "number_hours", number);
	});
}

SAM_EXPORT void SAM_MhkWave_MHKWave_number_records_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "number_records", number);
	});
}

SAM_EXPORT void SAM_MhkWave_MHKWave_significant_wave_height_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "significant_wave_height", arr, length);
	});
}

SAM_EXPORT void SAM_MhkWave_MHKWave_system_capacity_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "system_capacity", number);
	});
}

SAM_EXPORT void SAM_MhkWave_MHKWave_total_operating_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "total_operating_cost", number);
	});
}

SAM_EXPORT void SAM_MhkWave_MHKWave_wave_power_matrix_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "wave_power_matrix", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_MhkWave_MHKWave_wave_resource_data_tset(SAM_table ptr, SAM_table tab, SAM_error *err){
	SAM_table_set_table(ptr, "wave_resource_data", tab, err);
}



SAM_EXPORT void SAM_MhkWave_MHKWave_wave_resource_matrix_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "wave_resource_matrix", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_MhkWave_MHKWave_wave_resource_model_choice_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "wave_resource_model_choice", number);
	});
}

SAM_EXPORT void SAM_MhkWave_MHKWave_year_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "year", arr, length);
	});
}

SAM_EXPORT double SAM_MhkWave_MHKWave_balance_of_system_cost_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "balance_of_system_cost_total", &result))
		make_access_error("SAM_MhkWave", "balance_of_system_cost_total");
	});
	return result;
}



SAM_EXPORT double* SAM_MhkWave_MHKWave_day_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "day", length);
	if (!result)
		make_access_error("SAM_MhkWave", "day");
	});
	return result;
}



SAM_EXPORT double SAM_MhkWave_MHKWave_device_costs_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "device_costs_total", &result))
		make_access_error("SAM_MhkWave", "device_costs_total");
	});
	return result;
}



SAM_EXPORT double SAM_MhkWave_MHKWave_device_rated_power_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "device_rated_power", &result))
		make_access_error("SAM_MhkWave", "device_rated_power");
	});
	return result;
}



SAM_EXPORT double* SAM_MhkWave_MHKWave_energy_period_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "energy_period", length);
	if (!result)
		make_access_error("SAM_MhkWave", "energy_period");
	});
	return result;
}



SAM_EXPORT double SAM_MhkWave_MHKWave_financial_cost_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "financial_cost_total", &result))
		make_access_error("SAM_MhkWave", "financial_cost_total");
	});
	return result;
}



SAM_EXPORT double SAM_MhkWave_MHKWave_fixed_charge_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fixed_charge_rate", &result))
		make_access_error("SAM_MhkWave", "fixed_charge_rate");
	});
	return result;
}



SAM_EXPORT double* SAM_MhkWave_MHKWave_hour_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "hour", length);
	if (!result)
		make_access_error("SAM_MhkWave", "hour");
	});
	return result;
}



SAM_EXPORT double SAM_MhkWave_MHKWave_loss_additional_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "loss_additional", &result))
		make_access_error("SAM_MhkWave", "loss_additional");
	});
	return result;
}



SAM_EXPORT double SAM_MhkWave_MHKWave_loss_array_spacing_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "loss_array_spacing", &result))
		make_access_error("SAM_MhkWave", "loss_array_spacing");
	});
	return result;
}



SAM_EXPORT double SAM_MhkWave_MHKWave_loss_downtime_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "loss_downtime", &result))
		make_access_error("SAM_MhkWave", "loss_downtime");
	});
	return result;
}



SAM_EXPORT double SAM_MhkWave_MHKWave_loss_resource_overprediction_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "loss_resource_overprediction", &result))
		make_access_error("SAM_MhkWave", "loss_resource_overprediction");
	});
	return result;
}



SAM_EXPORT double SAM_MhkWave_MHKWave_loss_transmission_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "loss_transmission", &result))
		make_access_error("SAM_MhkWave", "loss_transmission");
	});
	return result;
}



SAM_EXPORT double* SAM_MhkWave_MHKWave_minute_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "minute", length);
	if (!result)
		make_access_error("SAM_MhkWave", "minute");
	});
	return result;
}



SAM_EXPORT double* SAM_MhkWave_MHKWave_month_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "month", length);
	if (!result)
		make_access_error("SAM_MhkWave", "month");
	});
	return result;
}



SAM_EXPORT double SAM_MhkWave_MHKWave_number_devices_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "number_devices", &result))
		make_access_error("SAM_MhkWave", "number_devices");
	});
	return result;
}



SAM_EXPORT double SAM_MhkWave_MHKWave_number_hours_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "number_hours", &result))
		make_access_error("SAM_MhkWave", "number_hours");
	});
	return result;
}



SAM_EXPORT double SAM_MhkWave_MHKWave_number_records_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "number_records", &result))
		make_access_error("SAM_MhkWave", "number_records");
	});
	return result;
}



SAM_EXPORT double* SAM_MhkWave_MHKWave_significant_wave_height_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "significant_wave_height", length);
	if (!result)
		make_access_error("SAM_MhkWave", "significant_wave_height");
	});
	return result;
}



SAM_EXPORT double SAM_MhkWave_MHKWave_system_capacity_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_capacity", &result))
		make_access_error("SAM_MhkWave", "system_capacity");
	});
	return result;
}



SAM_EXPORT double SAM_MhkWave_MHKWave_total_operating_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_operating_cost", &result))
		make_access_error("SAM_MhkWave", "total_operating_cost");
	});
	return result;
}



SAM_EXPORT double* SAM_MhkWave_MHKWave_wave_power_matrix_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "wave_power_matrix", nrows, ncols);
	if (!result)
		make_access_error("SAM_MhkWave", "wave_power_matrix");
	});
	return result;
}



SAM_EXPORT SAM_table SAM_MhkWave_MHKWave_wave_resource_data_tget(SAM_table ptr, SAM_error *err){
	SAM_table result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_table(ptr, "wave_resource_data");
	if (!result)
		make_access_error("SAM_MhkWave", "wave_resource_data");
	});
	return result;
}



SAM_EXPORT double* SAM_MhkWave_MHKWave_wave_resource_matrix_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "wave_resource_matrix", nrows, ncols);
	if (!result)
		make_access_error("SAM_MhkWave", "wave_resource_matrix");
	});
	return result;
}



SAM_EXPORT double SAM_MhkWave_MHKWave_wave_resource_model_choice_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "wave_resource_model_choice", &result))
		make_access_error("SAM_MhkWave", "wave_resource_model_choice");
	});
	return result;
}



SAM_EXPORT double* SAM_MhkWave_MHKWave_year_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year", length);
	if (!result)
		make_access_error("SAM_MhkWave", "year");
	});
	return result;
}



SAM_EXPORT double SAM_MhkWave_Outputs_annual_energy_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_energy", &result))
		make_access_error("SAM_MhkWave", "annual_energy");
	});
	return result;
}



SAM_EXPORT double* SAM_MhkWave_Outputs_annual_energy_distribution_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "annual_energy_distribution", nrows, ncols);
	if (!result)
		make_access_error("SAM_MhkWave", "annual_energy_distribution");
	});
	return result;
}



SAM_EXPORT double* SAM_MhkWave_Outputs_annual_energy_distribution_time_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "annual_energy_distribution_time", nrows, ncols);
	if (!result)
		make_access_error("SAM_MhkWave", "annual_energy_distribution_time");
	});
	return result;
}



SAM_EXPORT double SAM_MhkWave_Outputs_capacity_factor_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "capacity_factor", &result))
		make_access_error("SAM_MhkWave", "capacity_factor");
	});
	return result;
}



SAM_EXPORT double SAM_MhkWave_Outputs_device_average_power_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "device_average_power", &result))
		make_access_error("SAM_MhkWave", "device_average_power");
	});
	return result;
}



SAM_EXPORT double* SAM_MhkWave_Outputs_energy_hourly_kWh_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "energy_hourly_kWh", length);
	if (!result)
		make_access_error("SAM_MhkWave", "energy_hourly_kWh");
	});
	return result;
}



SAM_EXPORT double* SAM_MhkWave_Outputs_energy_period_data_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "energy_period_data", length);
	if (!result)
		make_access_error("SAM_MhkWave", "energy_period_data");
	});
	return result;
}



SAM_EXPORT double* SAM_MhkWave_Outputs_energy_period_index_mat_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "energy_period_index_mat", length);
	if (!result)
		make_access_error("SAM_MhkWave", "energy_period_index_mat");
	});
	return result;
}



SAM_EXPORT double* SAM_MhkWave_Outputs_gen_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "gen", length);
	if (!result)
		make_access_error("SAM_MhkWave", "gen");
	});
	return result;
}



SAM_EXPORT double SAM_MhkWave_Outputs_numberHours_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "numberHours", &result))
		make_access_error("SAM_MhkWave", "numberHours");
	});
	return result;
}



SAM_EXPORT double SAM_MhkWave_Outputs_numberRecords_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "numberRecords", &result))
		make_access_error("SAM_MhkWave", "numberRecords");
	});
	return result;
}



SAM_EXPORT double* SAM_MhkWave_Outputs_sig_wave_height_data_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "sig_wave_height_data", length);
	if (!result)
		make_access_error("SAM_MhkWave", "sig_wave_height_data");
	});
	return result;
}



SAM_EXPORT double* SAM_MhkWave_Outputs_sig_wave_height_index_mat_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "sig_wave_height_index_mat", length);
	if (!result)
		make_access_error("SAM_MhkWave", "sig_wave_height_index_mat");
	});
	return result;
}



SAM_EXPORT double SAM_MhkWave_Outputs_total_bos_cost_kwh_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_bos_cost_kwh", &result))
		make_access_error("SAM_MhkWave", "total_bos_cost_kwh");
	});
	return result;
}



SAM_EXPORT double SAM_MhkWave_Outputs_total_bos_cost_lcoe_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_bos_cost_lcoe", &result))
		make_access_error("SAM_MhkWave", "total_bos_cost_lcoe");
	});
	return result;
}



SAM_EXPORT double SAM_MhkWave_Outputs_total_bos_cost_per_kw_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_bos_cost_per_kw", &result))
		make_access_error("SAM_MhkWave", "total_bos_cost_per_kw");
	});
	return result;
}



SAM_EXPORT double SAM_MhkWave_Outputs_total_capital_cost_kwh_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_capital_cost_kwh", &result))
		make_access_error("SAM_MhkWave", "total_capital_cost_kwh");
	});
	return result;
}



SAM_EXPORT double SAM_MhkWave_Outputs_total_capital_cost_lcoe_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_capital_cost_lcoe", &result))
		make_access_error("SAM_MhkWave", "total_capital_cost_lcoe");
	});
	return result;
}



SAM_EXPORT double SAM_MhkWave_Outputs_total_capital_cost_per_kw_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_capital_cost_per_kw", &result))
		make_access_error("SAM_MhkWave", "total_capital_cost_per_kw");
	});
	return result;
}



SAM_EXPORT double SAM_MhkWave_Outputs_total_device_cost_kwh_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_device_cost_kwh", &result))
		make_access_error("SAM_MhkWave", "total_device_cost_kwh");
	});
	return result;
}



SAM_EXPORT double SAM_MhkWave_Outputs_total_device_cost_lcoe_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_device_cost_lcoe", &result))
		make_access_error("SAM_MhkWave", "total_device_cost_lcoe");
	});
	return result;
}



SAM_EXPORT double SAM_MhkWave_Outputs_total_device_cost_per_kw_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_device_cost_per_kw", &result))
		make_access_error("SAM_MhkWave", "total_device_cost_per_kw");
	});
	return result;
}



SAM_EXPORT double SAM_MhkWave_Outputs_total_financial_cost_kwh_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_financial_cost_kwh", &result))
		make_access_error("SAM_MhkWave", "total_financial_cost_kwh");
	});
	return result;
}



SAM_EXPORT double SAM_MhkWave_Outputs_total_financial_cost_lcoe_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_financial_cost_lcoe", &result))
		make_access_error("SAM_MhkWave", "total_financial_cost_lcoe");
	});
	return result;
}



SAM_EXPORT double SAM_MhkWave_Outputs_total_financial_cost_per_kw_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_financial_cost_per_kw", &result))
		make_access_error("SAM_MhkWave", "total_financial_cost_per_kw");
	});
	return result;
}



SAM_EXPORT double SAM_MhkWave_Outputs_total_om_cost_kwh_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_om_cost_kwh", &result))
		make_access_error("SAM_MhkWave", "total_om_cost_kwh");
	});
	return result;
}



SAM_EXPORT double SAM_MhkWave_Outputs_total_om_cost_lcoe_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_om_cost_lcoe", &result))
		make_access_error("SAM_MhkWave", "total_om_cost_lcoe");
	});
	return result;
}



SAM_EXPORT double SAM_MhkWave_Outputs_total_operations_cost_per_kw_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_operations_cost_per_kw", &result))
		make_access_error("SAM_MhkWave", "total_operations_cost_per_kw");
	});
	return result;
}



SAM_EXPORT double SAM_MhkWave_Outputs_wave_power_end_height_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "wave_power_end_height", &result))
		make_access_error("SAM_MhkWave", "wave_power_end_height");
	});
	return result;
}



SAM_EXPORT double SAM_MhkWave_Outputs_wave_power_end_period_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "wave_power_end_period", &result))
		make_access_error("SAM_MhkWave", "wave_power_end_period");
	});
	return result;
}



SAM_EXPORT double* SAM_MhkWave_Outputs_wave_power_index_mat_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "wave_power_index_mat", length);
	if (!result)
		make_access_error("SAM_MhkWave", "wave_power_index_mat");
	});
	return result;
}



SAM_EXPORT double SAM_MhkWave_Outputs_wave_power_start_height_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "wave_power_start_height", &result))
		make_access_error("SAM_MhkWave", "wave_power_start_height");
	});
	return result;
}



SAM_EXPORT double SAM_MhkWave_Outputs_wave_power_start_period_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "wave_power_start_period", &result))
		make_access_error("SAM_MhkWave", "wave_power_start_period");
	});
	return result;
}



SAM_EXPORT double SAM_MhkWave_Outputs_wave_resource_end_height_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "wave_resource_end_height", &result))
		make_access_error("SAM_MhkWave", "wave_resource_end_height");
	});
	return result;
}



SAM_EXPORT double SAM_MhkWave_Outputs_wave_resource_end_period_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "wave_resource_end_period", &result))
		make_access_error("SAM_MhkWave", "wave_resource_end_period");
	});
	return result;
}



SAM_EXPORT double SAM_MhkWave_Outputs_wave_resource_start_height_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "wave_resource_start_height", &result))
		make_access_error("SAM_MhkWave", "wave_resource_start_height");
	});
	return result;
}



SAM_EXPORT double SAM_MhkWave_Outputs_wave_resource_start_period_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "wave_resource_start_period", &result))
		make_access_error("SAM_MhkWave", "wave_resource_start_period");
	});
	return result;
}



