#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_MhkTidal.h"

SAM_EXPORT int SAM_MhkTidal_execute(SAM_table data, int verbosity, SAM_error* err){
	return SAM_module_exec("mhk_tidal", data, verbosity, err);
}

SAM_EXPORT void SAM_MhkTidal_MHKTidal_balance_of_system_cost_total_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "balance_of_system_cost_total", number);
	});
}

SAM_EXPORT void SAM_MhkTidal_MHKTidal_device_costs_total_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "device_costs_total", number);
	});
}

SAM_EXPORT void SAM_MhkTidal_MHKTidal_financial_cost_total_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "financial_cost_total", number);
	});
}

SAM_EXPORT void SAM_MhkTidal_MHKTidal_fixed_charge_rate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fixed_charge_rate", number);
	});
}

SAM_EXPORT void SAM_MhkTidal_MHKTidal_loss_additional_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "loss_additional", number);
	});
}

SAM_EXPORT void SAM_MhkTidal_MHKTidal_loss_array_spacing_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "loss_array_spacing", number);
	});
}

SAM_EXPORT void SAM_MhkTidal_MHKTidal_loss_downtime_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "loss_downtime", number);
	});
}

SAM_EXPORT void SAM_MhkTidal_MHKTidal_loss_resource_overprediction_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "loss_resource_overprediction", number);
	});
}

SAM_EXPORT void SAM_MhkTidal_MHKTidal_loss_transmission_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "loss_transmission", number);
	});
}

SAM_EXPORT void SAM_MhkTidal_MHKTidal_number_devices_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "number_devices", number);
	});
}

SAM_EXPORT void SAM_MhkTidal_MHKTidal_system_capacity_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "system_capacity", number);
	});
}

SAM_EXPORT void SAM_MhkTidal_MHKTidal_tidal_power_curve_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "tidal_power_curve", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_MhkTidal_MHKTidal_tidal_resource_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "tidal_resource", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_MhkTidal_MHKTidal_total_operating_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "total_operating_cost", number);
	});
}

SAM_EXPORT double SAM_MhkTidal_MHKTidal_balance_of_system_cost_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "balance_of_system_cost_total", &result))
		make_access_error("SAM_MhkTidal", "balance_of_system_cost_total");
	});
	return result;
}



SAM_EXPORT double SAM_MhkTidal_MHKTidal_device_costs_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "device_costs_total", &result))
		make_access_error("SAM_MhkTidal", "device_costs_total");
	});
	return result;
}



SAM_EXPORT double SAM_MhkTidal_MHKTidal_financial_cost_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "financial_cost_total", &result))
		make_access_error("SAM_MhkTidal", "financial_cost_total");
	});
	return result;
}



SAM_EXPORT double SAM_MhkTidal_MHKTidal_fixed_charge_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fixed_charge_rate", &result))
		make_access_error("SAM_MhkTidal", "fixed_charge_rate");
	});
	return result;
}



SAM_EXPORT double SAM_MhkTidal_MHKTidal_loss_additional_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "loss_additional", &result))
		make_access_error("SAM_MhkTidal", "loss_additional");
	});
	return result;
}



SAM_EXPORT double SAM_MhkTidal_MHKTidal_loss_array_spacing_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "loss_array_spacing", &result))
		make_access_error("SAM_MhkTidal", "loss_array_spacing");
	});
	return result;
}



SAM_EXPORT double SAM_MhkTidal_MHKTidal_loss_downtime_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "loss_downtime", &result))
		make_access_error("SAM_MhkTidal", "loss_downtime");
	});
	return result;
}



SAM_EXPORT double SAM_MhkTidal_MHKTidal_loss_resource_overprediction_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "loss_resource_overprediction", &result))
		make_access_error("SAM_MhkTidal", "loss_resource_overprediction");
	});
	return result;
}



SAM_EXPORT double SAM_MhkTidal_MHKTidal_loss_transmission_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "loss_transmission", &result))
		make_access_error("SAM_MhkTidal", "loss_transmission");
	});
	return result;
}



SAM_EXPORT double SAM_MhkTidal_MHKTidal_number_devices_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "number_devices", &result))
		make_access_error("SAM_MhkTidal", "number_devices");
	});
	return result;
}



SAM_EXPORT double SAM_MhkTidal_MHKTidal_system_capacity_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_capacity", &result))
		make_access_error("SAM_MhkTidal", "system_capacity");
	});
	return result;
}



SAM_EXPORT double* SAM_MhkTidal_MHKTidal_tidal_power_curve_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "tidal_power_curve", nrows, ncols);
	if (!result)
		make_access_error("SAM_MhkTidal", "tidal_power_curve");
	});
	return result;
}



SAM_EXPORT double* SAM_MhkTidal_MHKTidal_tidal_resource_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "tidal_resource", nrows, ncols);
	if (!result)
		make_access_error("SAM_MhkTidal", "tidal_resource");
	});
	return result;
}



SAM_EXPORT double SAM_MhkTidal_MHKTidal_total_operating_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_operating_cost", &result))
		make_access_error("SAM_MhkTidal", "total_operating_cost");
	});
	return result;
}



SAM_EXPORT double* SAM_MhkTidal_Outputs_annual_cumulative_energy_distribution_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "annual_cumulative_energy_distribution", length);
	if (!result)
		make_access_error("SAM_MhkTidal", "annual_cumulative_energy_distribution");
	});
	return result;
}



SAM_EXPORT double SAM_MhkTidal_Outputs_annual_energy_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_energy", &result))
		make_access_error("SAM_MhkTidal", "annual_energy");
	});
	return result;
}



SAM_EXPORT double* SAM_MhkTidal_Outputs_annual_energy_distribution_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "annual_energy_distribution", length);
	if (!result)
		make_access_error("SAM_MhkTidal", "annual_energy_distribution");
	});
	return result;
}



SAM_EXPORT double SAM_MhkTidal_Outputs_capacity_factor_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "capacity_factor", &result))
		make_access_error("SAM_MhkTidal", "capacity_factor");
	});
	return result;
}



SAM_EXPORT double SAM_MhkTidal_Outputs_device_average_power_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "device_average_power", &result))
		make_access_error("SAM_MhkTidal", "device_average_power");
	});
	return result;
}



SAM_EXPORT double SAM_MhkTidal_Outputs_device_rated_capacity_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "device_rated_capacity", &result))
		make_access_error("SAM_MhkTidal", "device_rated_capacity");
	});
	return result;
}



SAM_EXPORT double SAM_MhkTidal_Outputs_tidal_power_end_velocity_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tidal_power_end_velocity", &result))
		make_access_error("SAM_MhkTidal", "tidal_power_end_velocity");
	});
	return result;
}



SAM_EXPORT double SAM_MhkTidal_Outputs_tidal_power_start_velocity_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tidal_power_start_velocity", &result))
		make_access_error("SAM_MhkTidal", "tidal_power_start_velocity");
	});
	return result;
}



SAM_EXPORT double SAM_MhkTidal_Outputs_tidal_resource_end_velocity_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tidal_resource_end_velocity", &result))
		make_access_error("SAM_MhkTidal", "tidal_resource_end_velocity");
	});
	return result;
}



SAM_EXPORT double SAM_MhkTidal_Outputs_tidal_resource_start_velocity_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tidal_resource_start_velocity", &result))
		make_access_error("SAM_MhkTidal", "tidal_resource_start_velocity");
	});
	return result;
}



SAM_EXPORT double SAM_MhkTidal_Outputs_total_bos_cost_kwh_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_bos_cost_kwh", &result))
		make_access_error("SAM_MhkTidal", "total_bos_cost_kwh");
	});
	return result;
}



SAM_EXPORT double SAM_MhkTidal_Outputs_total_bos_cost_lcoe_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_bos_cost_lcoe", &result))
		make_access_error("SAM_MhkTidal", "total_bos_cost_lcoe");
	});
	return result;
}



SAM_EXPORT double SAM_MhkTidal_Outputs_total_bos_cost_per_kw_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_bos_cost_per_kw", &result))
		make_access_error("SAM_MhkTidal", "total_bos_cost_per_kw");
	});
	return result;
}



SAM_EXPORT double SAM_MhkTidal_Outputs_total_capital_cost_kwh_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_capital_cost_kwh", &result))
		make_access_error("SAM_MhkTidal", "total_capital_cost_kwh");
	});
	return result;
}



SAM_EXPORT double SAM_MhkTidal_Outputs_total_capital_cost_lcoe_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_capital_cost_lcoe", &result))
		make_access_error("SAM_MhkTidal", "total_capital_cost_lcoe");
	});
	return result;
}



SAM_EXPORT double SAM_MhkTidal_Outputs_total_capital_cost_per_kw_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_capital_cost_per_kw", &result))
		make_access_error("SAM_MhkTidal", "total_capital_cost_per_kw");
	});
	return result;
}



SAM_EXPORT double SAM_MhkTidal_Outputs_total_device_cost_kwh_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_device_cost_kwh", &result))
		make_access_error("SAM_MhkTidal", "total_device_cost_kwh");
	});
	return result;
}



SAM_EXPORT double SAM_MhkTidal_Outputs_total_device_cost_lcoe_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_device_cost_lcoe", &result))
		make_access_error("SAM_MhkTidal", "total_device_cost_lcoe");
	});
	return result;
}



SAM_EXPORT double SAM_MhkTidal_Outputs_total_device_cost_per_kw_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_device_cost_per_kw", &result))
		make_access_error("SAM_MhkTidal", "total_device_cost_per_kw");
	});
	return result;
}



SAM_EXPORT double SAM_MhkTidal_Outputs_total_financial_cost_kwh_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_financial_cost_kwh", &result))
		make_access_error("SAM_MhkTidal", "total_financial_cost_kwh");
	});
	return result;
}



SAM_EXPORT double SAM_MhkTidal_Outputs_total_financial_cost_lcoe_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_financial_cost_lcoe", &result))
		make_access_error("SAM_MhkTidal", "total_financial_cost_lcoe");
	});
	return result;
}



SAM_EXPORT double SAM_MhkTidal_Outputs_total_financial_cost_per_kw_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_financial_cost_per_kw", &result))
		make_access_error("SAM_MhkTidal", "total_financial_cost_per_kw");
	});
	return result;
}



SAM_EXPORT double SAM_MhkTidal_Outputs_total_om_cost_kwh_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_om_cost_kwh", &result))
		make_access_error("SAM_MhkTidal", "total_om_cost_kwh");
	});
	return result;
}



SAM_EXPORT double SAM_MhkTidal_Outputs_total_om_cost_lcoe_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_om_cost_lcoe", &result))
		make_access_error("SAM_MhkTidal", "total_om_cost_lcoe");
	});
	return result;
}



SAM_EXPORT double SAM_MhkTidal_Outputs_total_operations_cost_per_kw_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_operations_cost_per_kw", &result))
		make_access_error("SAM_MhkTidal", "total_operations_cost_per_kw");
	});
	return result;
}



