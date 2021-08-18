#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_Fuelcell.h"

SAM_EXPORT int SAM_Fuelcell_execute(SAM_table data, int verbosity, SAM_error* err){
	return SAM_module_exec("fuelcell", data, verbosity, err);
}

SAM_EXPORT void SAM_Fuelcell_Lifetime_analysis_period_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "analysis_period", number);
	});
}

SAM_EXPORT void SAM_Fuelcell_Lifetime_system_use_lifetime_output_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "system_use_lifetime_output", number);
	});
}

SAM_EXPORT void SAM_Fuelcell_Common_annual_energy_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "annual_energy", number);
	});
}

SAM_EXPORT void SAM_Fuelcell_Common_capacity_factor_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "capacity_factor", number);
	});
}

SAM_EXPORT void SAM_Fuelcell_Common_gen_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "gen", arr, length);
	});
}

SAM_EXPORT void SAM_Fuelcell_Load_load_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "load", arr, length);
	});
}

SAM_EXPORT void SAM_Fuelcell_FuelCell_dispatch_manual_fuelcellcharge_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "dispatch_manual_fuelcellcharge", arr, length);
	});
}

SAM_EXPORT void SAM_Fuelcell_FuelCell_dispatch_manual_fuelcelldischarge_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "dispatch_manual_fuelcelldischarge", arr, length);
	});
}

SAM_EXPORT void SAM_Fuelcell_FuelCell_dispatch_manual_percent_fc_discharge_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "dispatch_manual_percent_fc_discharge", arr, length);
	});
}

SAM_EXPORT void SAM_Fuelcell_FuelCell_dispatch_manual_sched_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "dispatch_manual_sched", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Fuelcell_FuelCell_dispatch_manual_sched_weekend_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "dispatch_manual_sched_weekend", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Fuelcell_FuelCell_dispatch_manual_units_fc_discharge_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "dispatch_manual_units_fc_discharge", arr, length);
	});
}

SAM_EXPORT void SAM_Fuelcell_FuelCell_fuelcell_availability_schedule_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "fuelcell_availability_schedule", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Fuelcell_FuelCell_fuelcell_degradation_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fuelcell_degradation", number);
	});
}

SAM_EXPORT void SAM_Fuelcell_FuelCell_fuelcell_degradation_restart_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fuelcell_degradation_restart", number);
	});
}

SAM_EXPORT void SAM_Fuelcell_FuelCell_fuelcell_degradation_restart_schedule_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fuelcell_degradation_restart_schedule", number);
	});
}

SAM_EXPORT void SAM_Fuelcell_FuelCell_fuelcell_degradation_restarts_per_year_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fuelcell_degradation_restarts_per_year", number);
	});
}

SAM_EXPORT void SAM_Fuelcell_FuelCell_fuelcell_dispatch_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "fuelcell_dispatch", arr, length);
	});
}

SAM_EXPORT void SAM_Fuelcell_FuelCell_fuelcell_dispatch_choice_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fuelcell_dispatch_choice", number);
	});
}

SAM_EXPORT void SAM_Fuelcell_FuelCell_fuelcell_dynamic_response_down_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fuelcell_dynamic_response_down", number);
	});
}

SAM_EXPORT void SAM_Fuelcell_FuelCell_fuelcell_dynamic_response_up_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fuelcell_dynamic_response_up", number);
	});
}

SAM_EXPORT void SAM_Fuelcell_FuelCell_fuelcell_efficiency_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "fuelcell_efficiency", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Fuelcell_FuelCell_fuelcell_efficiency_choice_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fuelcell_efficiency_choice", number);
	});
}

SAM_EXPORT void SAM_Fuelcell_FuelCell_fuelcell_fixed_pct_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fuelcell_fixed_pct", number);
	});
}

SAM_EXPORT void SAM_Fuelcell_FuelCell_fuelcell_fuel_available_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fuelcell_fuel_available", number);
	});
}

SAM_EXPORT void SAM_Fuelcell_FuelCell_fuelcell_fuel_price_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fuelcell_fuel_price", number);
	});
}

SAM_EXPORT void SAM_Fuelcell_FuelCell_fuelcell_fuel_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fuelcell_fuel_type", number);
	});
}

SAM_EXPORT void SAM_Fuelcell_FuelCell_fuelcell_is_started_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fuelcell_is_started", number);
	});
}

SAM_EXPORT void SAM_Fuelcell_FuelCell_fuelcell_lhv_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fuelcell_lhv", number);
	});
}

SAM_EXPORT void SAM_Fuelcell_FuelCell_fuelcell_number_of_units_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fuelcell_number_of_units", number);
	});
}

SAM_EXPORT void SAM_Fuelcell_FuelCell_fuelcell_operation_options_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fuelcell_operation_options", number);
	});
}

SAM_EXPORT void SAM_Fuelcell_FuelCell_fuelcell_replacement_option_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fuelcell_replacement_option", number);
	});
}

SAM_EXPORT void SAM_Fuelcell_FuelCell_fuelcell_replacement_percent_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fuelcell_replacement_percent", number);
	});
}

SAM_EXPORT void SAM_Fuelcell_FuelCell_fuelcell_replacement_schedule_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "fuelcell_replacement_schedule", arr, length);
	});
}

SAM_EXPORT void SAM_Fuelcell_FuelCell_fuelcell_shutdown_time_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fuelcell_shutdown_time", number);
	});
}

SAM_EXPORT void SAM_Fuelcell_FuelCell_fuelcell_startup_time_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fuelcell_startup_time", number);
	});
}

SAM_EXPORT void SAM_Fuelcell_FuelCell_fuelcell_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fuelcell_type", number);
	});
}

SAM_EXPORT void SAM_Fuelcell_FuelCell_fuelcell_unit_max_power_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fuelcell_unit_max_power", number);
	});
}

SAM_EXPORT void SAM_Fuelcell_FuelCell_fuelcell_unit_min_power_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fuelcell_unit_min_power", number);
	});
}

SAM_EXPORT double SAM_Fuelcell_Lifetime_analysis_period_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "analysis_period", &result))
		make_access_error("SAM_Fuelcell", "analysis_period");
	});
	return result;
}



SAM_EXPORT double SAM_Fuelcell_Lifetime_system_use_lifetime_output_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_use_lifetime_output", &result))
		make_access_error("SAM_Fuelcell", "system_use_lifetime_output");
	});
	return result;
}



SAM_EXPORT double SAM_Fuelcell_Common_annual_energy_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_energy", &result))
		make_access_error("SAM_Fuelcell", "annual_energy");
	});
	return result;
}



SAM_EXPORT double SAM_Fuelcell_Common_capacity_factor_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "capacity_factor", &result))
		make_access_error("SAM_Fuelcell", "capacity_factor");
	});
	return result;
}



SAM_EXPORT double* SAM_Fuelcell_Common_gen_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "gen", length);
	if (!result)
		make_access_error("SAM_Fuelcell", "gen");
	});
	return result;
}



SAM_EXPORT double* SAM_Fuelcell_Load_load_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "load", length);
	if (!result)
		make_access_error("SAM_Fuelcell", "load");
	});
	return result;
}



SAM_EXPORT double* SAM_Fuelcell_FuelCell_dispatch_manual_fuelcellcharge_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dispatch_manual_fuelcellcharge", length);
	if (!result)
		make_access_error("SAM_Fuelcell", "dispatch_manual_fuelcellcharge");
	});
	return result;
}



SAM_EXPORT double* SAM_Fuelcell_FuelCell_dispatch_manual_fuelcelldischarge_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dispatch_manual_fuelcelldischarge", length);
	if (!result)
		make_access_error("SAM_Fuelcell", "dispatch_manual_fuelcelldischarge");
	});
	return result;
}



SAM_EXPORT double* SAM_Fuelcell_FuelCell_dispatch_manual_percent_fc_discharge_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dispatch_manual_percent_fc_discharge", length);
	if (!result)
		make_access_error("SAM_Fuelcell", "dispatch_manual_percent_fc_discharge");
	});
	return result;
}



SAM_EXPORT double* SAM_Fuelcell_FuelCell_dispatch_manual_sched_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "dispatch_manual_sched", nrows, ncols);
	if (!result)
		make_access_error("SAM_Fuelcell", "dispatch_manual_sched");
	});
	return result;
}



SAM_EXPORT double* SAM_Fuelcell_FuelCell_dispatch_manual_sched_weekend_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "dispatch_manual_sched_weekend", nrows, ncols);
	if (!result)
		make_access_error("SAM_Fuelcell", "dispatch_manual_sched_weekend");
	});
	return result;
}



SAM_EXPORT double* SAM_Fuelcell_FuelCell_dispatch_manual_units_fc_discharge_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dispatch_manual_units_fc_discharge", length);
	if (!result)
		make_access_error("SAM_Fuelcell", "dispatch_manual_units_fc_discharge");
	});
	return result;
}



SAM_EXPORT double* SAM_Fuelcell_FuelCell_fuelcell_availability_schedule_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "fuelcell_availability_schedule", nrows, ncols);
	if (!result)
		make_access_error("SAM_Fuelcell", "fuelcell_availability_schedule");
	});
	return result;
}



SAM_EXPORT double SAM_Fuelcell_FuelCell_fuelcell_degradation_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fuelcell_degradation", &result))
		make_access_error("SAM_Fuelcell", "fuelcell_degradation");
	});
	return result;
}



SAM_EXPORT double SAM_Fuelcell_FuelCell_fuelcell_degradation_restart_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fuelcell_degradation_restart", &result))
		make_access_error("SAM_Fuelcell", "fuelcell_degradation_restart");
	});
	return result;
}



SAM_EXPORT double SAM_Fuelcell_FuelCell_fuelcell_degradation_restart_schedule_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fuelcell_degradation_restart_schedule", &result))
		make_access_error("SAM_Fuelcell", "fuelcell_degradation_restart_schedule");
	});
	return result;
}



SAM_EXPORT double SAM_Fuelcell_FuelCell_fuelcell_degradation_restarts_per_year_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fuelcell_degradation_restarts_per_year", &result))
		make_access_error("SAM_Fuelcell", "fuelcell_degradation_restarts_per_year");
	});
	return result;
}



SAM_EXPORT double* SAM_Fuelcell_FuelCell_fuelcell_dispatch_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "fuelcell_dispatch", length);
	if (!result)
		make_access_error("SAM_Fuelcell", "fuelcell_dispatch");
	});
	return result;
}



SAM_EXPORT double SAM_Fuelcell_FuelCell_fuelcell_dispatch_choice_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fuelcell_dispatch_choice", &result))
		make_access_error("SAM_Fuelcell", "fuelcell_dispatch_choice");
	});
	return result;
}



SAM_EXPORT double SAM_Fuelcell_FuelCell_fuelcell_dynamic_response_down_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fuelcell_dynamic_response_down", &result))
		make_access_error("SAM_Fuelcell", "fuelcell_dynamic_response_down");
	});
	return result;
}



SAM_EXPORT double SAM_Fuelcell_FuelCell_fuelcell_dynamic_response_up_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fuelcell_dynamic_response_up", &result))
		make_access_error("SAM_Fuelcell", "fuelcell_dynamic_response_up");
	});
	return result;
}



SAM_EXPORT double* SAM_Fuelcell_FuelCell_fuelcell_efficiency_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "fuelcell_efficiency", nrows, ncols);
	if (!result)
		make_access_error("SAM_Fuelcell", "fuelcell_efficiency");
	});
	return result;
}



SAM_EXPORT double SAM_Fuelcell_FuelCell_fuelcell_efficiency_choice_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fuelcell_efficiency_choice", &result))
		make_access_error("SAM_Fuelcell", "fuelcell_efficiency_choice");
	});
	return result;
}



SAM_EXPORT double SAM_Fuelcell_FuelCell_fuelcell_fixed_pct_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fuelcell_fixed_pct", &result))
		make_access_error("SAM_Fuelcell", "fuelcell_fixed_pct");
	});
	return result;
}



SAM_EXPORT double SAM_Fuelcell_FuelCell_fuelcell_fuel_available_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fuelcell_fuel_available", &result))
		make_access_error("SAM_Fuelcell", "fuelcell_fuel_available");
	});
	return result;
}



SAM_EXPORT double SAM_Fuelcell_FuelCell_fuelcell_fuel_price_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fuelcell_fuel_price", &result))
		make_access_error("SAM_Fuelcell", "fuelcell_fuel_price");
	});
	return result;
}



SAM_EXPORT double SAM_Fuelcell_FuelCell_fuelcell_fuel_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fuelcell_fuel_type", &result))
		make_access_error("SAM_Fuelcell", "fuelcell_fuel_type");
	});
	return result;
}



SAM_EXPORT double SAM_Fuelcell_FuelCell_fuelcell_is_started_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fuelcell_is_started", &result))
		make_access_error("SAM_Fuelcell", "fuelcell_is_started");
	});
	return result;
}



SAM_EXPORT double SAM_Fuelcell_FuelCell_fuelcell_lhv_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fuelcell_lhv", &result))
		make_access_error("SAM_Fuelcell", "fuelcell_lhv");
	});
	return result;
}



SAM_EXPORT double SAM_Fuelcell_FuelCell_fuelcell_number_of_units_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fuelcell_number_of_units", &result))
		make_access_error("SAM_Fuelcell", "fuelcell_number_of_units");
	});
	return result;
}



SAM_EXPORT double SAM_Fuelcell_FuelCell_fuelcell_operation_options_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fuelcell_operation_options", &result))
		make_access_error("SAM_Fuelcell", "fuelcell_operation_options");
	});
	return result;
}



SAM_EXPORT double SAM_Fuelcell_FuelCell_fuelcell_replacement_option_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fuelcell_replacement_option", &result))
		make_access_error("SAM_Fuelcell", "fuelcell_replacement_option");
	});
	return result;
}



SAM_EXPORT double SAM_Fuelcell_FuelCell_fuelcell_replacement_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fuelcell_replacement_percent", &result))
		make_access_error("SAM_Fuelcell", "fuelcell_replacement_percent");
	});
	return result;
}



SAM_EXPORT double* SAM_Fuelcell_FuelCell_fuelcell_replacement_schedule_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "fuelcell_replacement_schedule", length);
	if (!result)
		make_access_error("SAM_Fuelcell", "fuelcell_replacement_schedule");
	});
	return result;
}



SAM_EXPORT double SAM_Fuelcell_FuelCell_fuelcell_shutdown_time_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fuelcell_shutdown_time", &result))
		make_access_error("SAM_Fuelcell", "fuelcell_shutdown_time");
	});
	return result;
}



SAM_EXPORT double SAM_Fuelcell_FuelCell_fuelcell_startup_time_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fuelcell_startup_time", &result))
		make_access_error("SAM_Fuelcell", "fuelcell_startup_time");
	});
	return result;
}



SAM_EXPORT double SAM_Fuelcell_FuelCell_fuelcell_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fuelcell_type", &result))
		make_access_error("SAM_Fuelcell", "fuelcell_type");
	});
	return result;
}



SAM_EXPORT double SAM_Fuelcell_FuelCell_fuelcell_unit_max_power_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fuelcell_unit_max_power", &result))
		make_access_error("SAM_Fuelcell", "fuelcell_unit_max_power");
	});
	return result;
}



SAM_EXPORT double SAM_Fuelcell_FuelCell_fuelcell_unit_min_power_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fuelcell_unit_min_power", &result))
		make_access_error("SAM_Fuelcell", "fuelcell_unit_min_power");
	});
	return result;
}



SAM_EXPORT double* SAM_Fuelcell_Outputs_annual_energy_distribution_time_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "annual_energy_distribution_time", nrows, ncols);
	if (!result)
		make_access_error("SAM_Fuelcell", "annual_energy_distribution_time");
	});
	return result;
}



SAM_EXPORT double* SAM_Fuelcell_Outputs_annual_energy_distribution_time_fc_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "annual_energy_distribution_time_fc", nrows, ncols);
	if (!result)
		make_access_error("SAM_Fuelcell", "annual_energy_distribution_time_fc");
	});
	return result;
}



SAM_EXPORT double SAM_Fuelcell_Outputs_annual_fuel_usage_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_fuel_usage", &result))
		make_access_error("SAM_Fuelcell", "annual_fuel_usage");
	});
	return result;
}



SAM_EXPORT double* SAM_Fuelcell_Outputs_annual_fuel_usage_lifetime_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "annual_fuel_usage_lifetime", length);
	if (!result)
		make_access_error("SAM_Fuelcell", "annual_fuel_usage_lifetime");
	});
	return result;
}



SAM_EXPORT double* SAM_Fuelcell_Outputs_fuelcell_electrical_efficiency_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "fuelcell_electrical_efficiency", length);
	if (!result)
		make_access_error("SAM_Fuelcell", "fuelcell_electrical_efficiency");
	});
	return result;
}



SAM_EXPORT double* SAM_Fuelcell_Outputs_fuelcell_fuel_consumption_mcf_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "fuelcell_fuel_consumption_mcf", length);
	if (!result)
		make_access_error("SAM_Fuelcell", "fuelcell_fuel_consumption_mcf");
	});
	return result;
}



SAM_EXPORT double* SAM_Fuelcell_Outputs_fuelcell_percent_load_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "fuelcell_percent_load", length);
	if (!result)
		make_access_error("SAM_Fuelcell", "fuelcell_percent_load");
	});
	return result;
}



SAM_EXPORT double* SAM_Fuelcell_Outputs_fuelcell_power_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "fuelcell_power", length);
	if (!result)
		make_access_error("SAM_Fuelcell", "fuelcell_power");
	});
	return result;
}



SAM_EXPORT double* SAM_Fuelcell_Outputs_fuelcell_power_max_percent_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "fuelcell_power_max_percent", length);
	if (!result)
		make_access_error("SAM_Fuelcell", "fuelcell_power_max_percent");
	});
	return result;
}



SAM_EXPORT double* SAM_Fuelcell_Outputs_fuelcell_power_thermal_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "fuelcell_power_thermal", length);
	if (!result)
		make_access_error("SAM_Fuelcell", "fuelcell_power_thermal");
	});
	return result;
}



SAM_EXPORT double* SAM_Fuelcell_Outputs_fuelcell_replacement_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "fuelcell_replacement", length);
	if (!result)
		make_access_error("SAM_Fuelcell", "fuelcell_replacement");
	});
	return result;
}



SAM_EXPORT double* SAM_Fuelcell_Outputs_fuelcell_to_grid_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "fuelcell_to_grid", length);
	if (!result)
		make_access_error("SAM_Fuelcell", "fuelcell_to_grid");
	});
	return result;
}



SAM_EXPORT double* SAM_Fuelcell_Outputs_fuelcell_to_load_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "fuelcell_to_load", length);
	if (!result)
		make_access_error("SAM_Fuelcell", "fuelcell_to_load");
	});
	return result;
}



SAM_EXPORT double* SAM_Fuelcell_Outputs_gen_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "gen", length);
	if (!result)
		make_access_error("SAM_Fuelcell", "gen");
	});
	return result;
}



SAM_EXPORT double SAM_Fuelcell_Outputs_system_heat_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_heat_rate", &result))
		make_access_error("SAM_Fuelcell", "system_heat_rate");
	});
	return result;
}



