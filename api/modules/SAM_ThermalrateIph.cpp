#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_ThermalrateIph.h"

SAM_EXPORT int SAM_ThermalrateIph_execute(SAM_table data, int verbosity, SAM_error* err){
	return SAM_module_exec("thermalrate_iph", data, verbosity, err);
}

SAM_EXPORT void SAM_ThermalrateIph_ThermalRate_en_thermal_rates_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "en_thermal_rates", number);
	});
}

SAM_EXPORT void SAM_ThermalrateIph_ThermalRate_gen_heat_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "gen_heat", arr, length);
	});
}

SAM_EXPORT void SAM_ThermalrateIph_ThermalRate_thermal_buy_rate_flat_heat_btu_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "thermal_buy_rate_flat_heat_btu", number);
	});
}

SAM_EXPORT void SAM_ThermalrateIph_ThermalRate_thermal_buy_rate_option_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "thermal_buy_rate_option", number);
	});
}

SAM_EXPORT void SAM_ThermalrateIph_ThermalRate_thermal_conversion_efficiency_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "thermal_conversion_efficiency", number);
	});
}

SAM_EXPORT void SAM_ThermalrateIph_ThermalRate_thermal_degradation_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "thermal_degradation", arr, length);
	});
}

SAM_EXPORT void SAM_ThermalrateIph_ThermalRate_thermal_load_escalation_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "thermal_load_escalation", arr, length);
	});
}

SAM_EXPORT void SAM_ThermalrateIph_ThermalRate_thermal_load_heat_btu_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "thermal_load_heat_btu", arr, length);
	});
}

SAM_EXPORT void SAM_ThermalrateIph_ThermalRate_thermal_monthly_buy_rate_heat_btu_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "thermal_monthly_buy_rate_heat_btu", arr, length);
	});
}

SAM_EXPORT void SAM_ThermalrateIph_ThermalRate_thermal_monthly_sell_rate_heat_btu_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "thermal_monthly_sell_rate_heat_btu", arr, length);
	});
}

SAM_EXPORT void SAM_ThermalrateIph_ThermalRate_thermal_rate_escalation_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "thermal_rate_escalation", arr, length);
	});
}

SAM_EXPORT void SAM_ThermalrateIph_ThermalRate_thermal_sell_rate_flat_heat_btu_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "thermal_sell_rate_flat_heat_btu", number);
	});
}

SAM_EXPORT void SAM_ThermalrateIph_ThermalRate_thermal_sell_rate_option_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "thermal_sell_rate_option", number);
	});
}

SAM_EXPORT void SAM_ThermalrateIph_ThermalRate_thermal_timestep_buy_rate_heat_btu_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "thermal_timestep_buy_rate_heat_btu", arr, length);
	});
}

SAM_EXPORT void SAM_ThermalrateIph_ThermalRate_thermal_timestep_sell_rate_heat_btu_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "thermal_timestep_sell_rate_heat_btu", arr, length);
	});
}

SAM_EXPORT void SAM_ThermalrateIph_Lifetime_analysis_period_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "analysis_period", number);
	});
}

SAM_EXPORT void SAM_ThermalrateIph_Lifetime_inflation_rate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inflation_rate", number);
	});
}

SAM_EXPORT void SAM_ThermalrateIph_Lifetime_system_use_lifetime_output_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "system_use_lifetime_output", number);
	});
}

SAM_EXPORT double SAM_ThermalrateIph_ThermalRate_en_thermal_rates_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "en_thermal_rates", &result))
		make_access_error("SAM_ThermalrateIph", "en_thermal_rates");
	});
	return result;
}

SAM_EXPORT double* SAM_ThermalrateIph_ThermalRate_gen_heat_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "gen_heat", length);
	if (!result)
		make_access_error("SAM_ThermalrateIph", "gen_heat");
	});
	return result;
}

SAM_EXPORT double SAM_ThermalrateIph_ThermalRate_thermal_buy_rate_flat_heat_btu_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "thermal_buy_rate_flat_heat_btu", &result))
		make_access_error("SAM_ThermalrateIph", "thermal_buy_rate_flat_heat_btu");
	});
	return result;
}

SAM_EXPORT double SAM_ThermalrateIph_ThermalRate_thermal_buy_rate_option_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "thermal_buy_rate_option", &result))
		make_access_error("SAM_ThermalrateIph", "thermal_buy_rate_option");
	});
	return result;
}

SAM_EXPORT double SAM_ThermalrateIph_ThermalRate_thermal_conversion_efficiency_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "thermal_conversion_efficiency", &result))
		make_access_error("SAM_ThermalrateIph", "thermal_conversion_efficiency");
	});
	return result;
}

SAM_EXPORT double* SAM_ThermalrateIph_ThermalRate_thermal_degradation_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "thermal_degradation", length);
	if (!result)
		make_access_error("SAM_ThermalrateIph", "thermal_degradation");
	});
	return result;
}

SAM_EXPORT double* SAM_ThermalrateIph_ThermalRate_thermal_load_escalation_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "thermal_load_escalation", length);
	if (!result)
		make_access_error("SAM_ThermalrateIph", "thermal_load_escalation");
	});
	return result;
}

SAM_EXPORT double* SAM_ThermalrateIph_ThermalRate_thermal_load_heat_btu_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "thermal_load_heat_btu", length);
	if (!result)
		make_access_error("SAM_ThermalrateIph", "thermal_load_heat_btu");
	});
	return result;
}

SAM_EXPORT double* SAM_ThermalrateIph_ThermalRate_thermal_monthly_buy_rate_heat_btu_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "thermal_monthly_buy_rate_heat_btu", length);
	if (!result)
		make_access_error("SAM_ThermalrateIph", "thermal_monthly_buy_rate_heat_btu");
	});
	return result;
}

SAM_EXPORT double* SAM_ThermalrateIph_ThermalRate_thermal_monthly_sell_rate_heat_btu_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "thermal_monthly_sell_rate_heat_btu", length);
	if (!result)
		make_access_error("SAM_ThermalrateIph", "thermal_monthly_sell_rate_heat_btu");
	});
	return result;
}

SAM_EXPORT double* SAM_ThermalrateIph_ThermalRate_thermal_rate_escalation_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "thermal_rate_escalation", length);
	if (!result)
		make_access_error("SAM_ThermalrateIph", "thermal_rate_escalation");
	});
	return result;
}

SAM_EXPORT double SAM_ThermalrateIph_ThermalRate_thermal_sell_rate_flat_heat_btu_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "thermal_sell_rate_flat_heat_btu", &result))
		make_access_error("SAM_ThermalrateIph", "thermal_sell_rate_flat_heat_btu");
	});
	return result;
}

SAM_EXPORT double SAM_ThermalrateIph_ThermalRate_thermal_sell_rate_option_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "thermal_sell_rate_option", &result))
		make_access_error("SAM_ThermalrateIph", "thermal_sell_rate_option");
	});
	return result;
}

SAM_EXPORT double* SAM_ThermalrateIph_ThermalRate_thermal_timestep_buy_rate_heat_btu_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "thermal_timestep_buy_rate_heat_btu", length);
	if (!result)
		make_access_error("SAM_ThermalrateIph", "thermal_timestep_buy_rate_heat_btu");
	});
	return result;
}

SAM_EXPORT double* SAM_ThermalrateIph_ThermalRate_thermal_timestep_sell_rate_heat_btu_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "thermal_timestep_sell_rate_heat_btu", length);
	if (!result)
		make_access_error("SAM_ThermalrateIph", "thermal_timestep_sell_rate_heat_btu");
	});
	return result;
}

SAM_EXPORT double SAM_ThermalrateIph_Lifetime_analysis_period_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "analysis_period", &result))
		make_access_error("SAM_ThermalrateIph", "analysis_period");
	});
	return result;
}

SAM_EXPORT double SAM_ThermalrateIph_Lifetime_inflation_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inflation_rate", &result))
		make_access_error("SAM_ThermalrateIph", "inflation_rate");
	});
	return result;
}

SAM_EXPORT double SAM_ThermalrateIph_Lifetime_system_use_lifetime_output_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_use_lifetime_output", &result))
		make_access_error("SAM_ThermalrateIph", "system_use_lifetime_output");
	});
	return result;
}

SAM_EXPORT double* SAM_ThermalrateIph_Outputs_annual_thermal_value_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "annual_thermal_value", length);
	if (!result)
		make_access_error("SAM_ThermalrateIph", "annual_thermal_value");
	});
	return result;
}

SAM_EXPORT double* SAM_ThermalrateIph_Outputs_thermal_cost_with_system_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "thermal_cost_with_system", length);
	if (!result)
		make_access_error("SAM_ThermalrateIph", "thermal_cost_with_system");
	});
	return result;
}

SAM_EXPORT double SAM_ThermalrateIph_Outputs_thermal_cost_with_system_year1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "thermal_cost_with_system_year1", &result))
		make_access_error("SAM_ThermalrateIph", "thermal_cost_with_system_year1");
	});
	return result;
}

SAM_EXPORT double* SAM_ThermalrateIph_Outputs_thermal_cost_without_system_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "thermal_cost_without_system", length);
	if (!result)
		make_access_error("SAM_ThermalrateIph", "thermal_cost_without_system");
	});
	return result;
}

SAM_EXPORT double SAM_ThermalrateIph_Outputs_thermal_cost_without_system_year1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "thermal_cost_without_system_year1", &result))
		make_access_error("SAM_ThermalrateIph", "thermal_cost_without_system_year1");
	});
	return result;
}

SAM_EXPORT double SAM_ThermalrateIph_Outputs_thermal_load_year1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "thermal_load_year1", &result))
		make_access_error("SAM_ThermalrateIph", "thermal_load_year1");
	});
	return result;
}

SAM_EXPORT double* SAM_ThermalrateIph_Outputs_thermal_revenue_with_system_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "thermal_revenue_with_system", length);
	if (!result)
		make_access_error("SAM_ThermalrateIph", "thermal_revenue_with_system");
	});
	return result;
}

SAM_EXPORT double* SAM_ThermalrateIph_Outputs_thermal_revenue_without_system_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "thermal_revenue_without_system", length);
	if (!result)
		make_access_error("SAM_ThermalrateIph", "thermal_revenue_without_system");
	});
	return result;
}

SAM_EXPORT double SAM_ThermalrateIph_Outputs_thermal_savings_year1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "thermal_savings_year1", &result))
		make_access_error("SAM_ThermalrateIph", "thermal_savings_year1");
	});
	return result;
}

SAM_EXPORT double* SAM_ThermalrateIph_Outputs_year1_monthly_load_heat_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_monthly_load_heat", length);
	if (!result)
		make_access_error("SAM_ThermalrateIph", "year1_monthly_load_heat");
	});
	return result;
}

