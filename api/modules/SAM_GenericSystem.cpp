#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_GenericSystem.h"

SAM_EXPORT int SAM_GenericSystem_execute(SAM_table data, int verbosity, SAM_error* err){
	return SAM_module_exec("generic_system", data, verbosity, err);
}

SAM_EXPORT void SAM_GenericSystem_Plant_conv_eff_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "conv_eff", number);
	});
}

SAM_EXPORT void SAM_GenericSystem_Plant_derate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "derate", number);
	});
}

SAM_EXPORT void SAM_GenericSystem_Plant_energy_output_array_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "energy_output_array", arr, length);
	});
}

SAM_EXPORT void SAM_GenericSystem_Plant_heat_rate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "heat_rate", number);
	});
}

SAM_EXPORT void SAM_GenericSystem_Plant_spec_mode_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "spec_mode", number);
	});
}

SAM_EXPORT void SAM_GenericSystem_Plant_system_capacity_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "system_capacity", number);
	});
}

SAM_EXPORT void SAM_GenericSystem_Plant_user_capacity_factor_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "user_capacity_factor", number);
	});
}

SAM_EXPORT void SAM_GenericSystem_Lifetime_analysis_period_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "analysis_period", number);
	});
}

SAM_EXPORT void SAM_GenericSystem_Lifetime_generic_degradation_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "generic_degradation", arr, length);
	});
}

SAM_EXPORT void SAM_GenericSystem_Lifetime_system_use_lifetime_output_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "system_use_lifetime_output", number);
	});
}

SAM_EXPORT void SAM_GenericSystem_AdjustmentFactors_adjust_constant_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "adjust_constant", number);
	});
}

SAM_EXPORT void SAM_GenericSystem_AdjustmentFactors_adjust_en_periods_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "adjust_en_periods", number);
	});
}

SAM_EXPORT void SAM_GenericSystem_AdjustmentFactors_adjust_en_timeindex_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "adjust_en_timeindex", number);
	});
}

SAM_EXPORT void SAM_GenericSystem_AdjustmentFactors_adjust_periods_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "adjust_periods", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_GenericSystem_AdjustmentFactors_adjust_timeindex_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "adjust_timeindex", arr, length);
	});
}

SAM_EXPORT void SAM_GenericSystem_HybridCosts_annual_fuel_usage_lifetime_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "annual_fuel_usage_lifetime", arr, length);
	});
}

SAM_EXPORT void SAM_GenericSystem_HybridCosts_degradation_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "degradation", arr, length);
	});
}

SAM_EXPORT void SAM_GenericSystem_HybridCosts_land_area_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "land_area", number);
	});
}

SAM_EXPORT void SAM_GenericSystem_HybridCosts_om_capacity_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "om_capacity", arr, length);
	});
}

SAM_EXPORT void SAM_GenericSystem_HybridCosts_om_capacity_escal_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "om_capacity_escal", number);
	});
}

SAM_EXPORT void SAM_GenericSystem_HybridCosts_om_fixed_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "om_fixed", arr, length);
	});
}

SAM_EXPORT void SAM_GenericSystem_HybridCosts_om_fixed_escal_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "om_fixed_escal", number);
	});
}

SAM_EXPORT void SAM_GenericSystem_HybridCosts_om_fuel_cost_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "om_fuel_cost", arr, length);
	});
}

SAM_EXPORT void SAM_GenericSystem_HybridCosts_om_fuel_cost_escal_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "om_fuel_cost_escal", number);
	});
}

SAM_EXPORT void SAM_GenericSystem_HybridCosts_om_land_lease_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "om_land_lease", arr, length);
	});
}

SAM_EXPORT void SAM_GenericSystem_HybridCosts_om_land_lease_escal_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "om_land_lease_escal", number);
	});
}

SAM_EXPORT void SAM_GenericSystem_HybridCosts_om_production_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "om_production", arr, length);
	});
}

SAM_EXPORT void SAM_GenericSystem_HybridCosts_om_production_escal_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "om_production_escal", number);
	});
}

SAM_EXPORT void SAM_GenericSystem_HybridCosts_total_installed_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "total_installed_cost", number);
	});
}

SAM_EXPORT double SAM_GenericSystem_Plant_conv_eff_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "conv_eff", &result))
		make_access_error("SAM_GenericSystem", "conv_eff");
	});
	return result;
}

SAM_EXPORT double SAM_GenericSystem_Plant_derate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "derate", &result))
		make_access_error("SAM_GenericSystem", "derate");
	});
	return result;
}

SAM_EXPORT double* SAM_GenericSystem_Plant_energy_output_array_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "energy_output_array", length);
	if (!result)
		make_access_error("SAM_GenericSystem", "energy_output_array");
	});
	return result;
}

SAM_EXPORT double SAM_GenericSystem_Plant_heat_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "heat_rate", &result))
		make_access_error("SAM_GenericSystem", "heat_rate");
	});
	return result;
}

SAM_EXPORT double SAM_GenericSystem_Plant_spec_mode_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "spec_mode", &result))
		make_access_error("SAM_GenericSystem", "spec_mode");
	});
	return result;
}

SAM_EXPORT double SAM_GenericSystem_Plant_system_capacity_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_capacity", &result))
		make_access_error("SAM_GenericSystem", "system_capacity");
	});
	return result;
}

SAM_EXPORT double SAM_GenericSystem_Plant_user_capacity_factor_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "user_capacity_factor", &result))
		make_access_error("SAM_GenericSystem", "user_capacity_factor");
	});
	return result;
}

SAM_EXPORT double SAM_GenericSystem_Lifetime_analysis_period_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "analysis_period", &result))
		make_access_error("SAM_GenericSystem", "analysis_period");
	});
	return result;
}

SAM_EXPORT double* SAM_GenericSystem_Lifetime_generic_degradation_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "generic_degradation", length);
	if (!result)
		make_access_error("SAM_GenericSystem", "generic_degradation");
	});
	return result;
}

SAM_EXPORT double SAM_GenericSystem_Lifetime_system_use_lifetime_output_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_use_lifetime_output", &result))
		make_access_error("SAM_GenericSystem", "system_use_lifetime_output");
	});
	return result;
}

SAM_EXPORT double SAM_GenericSystem_AdjustmentFactors_adjust_constant_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "adjust_constant", &result))
		make_access_error("SAM_GenericSystem", "adjust_constant");
	});
	return result;
}

SAM_EXPORT double SAM_GenericSystem_AdjustmentFactors_adjust_en_periods_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "adjust_en_periods", &result))
		make_access_error("SAM_GenericSystem", "adjust_en_periods");
	});
	return result;
}

SAM_EXPORT double SAM_GenericSystem_AdjustmentFactors_adjust_en_timeindex_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "adjust_en_timeindex", &result))
		make_access_error("SAM_GenericSystem", "adjust_en_timeindex");
	});
	return result;
}

SAM_EXPORT double* SAM_GenericSystem_AdjustmentFactors_adjust_periods_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "adjust_periods", nrows, ncols);
	if (!result)
		make_access_error("SAM_GenericSystem", "adjust_periods");
	});
	return result;
}

SAM_EXPORT double* SAM_GenericSystem_AdjustmentFactors_adjust_timeindex_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "adjust_timeindex", length);
	if (!result)
		make_access_error("SAM_GenericSystem", "adjust_timeindex");
	});
	return result;
}

SAM_EXPORT double* SAM_GenericSystem_HybridCosts_annual_fuel_usage_lifetime_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "annual_fuel_usage_lifetime", length);
	if (!result)
		make_access_error("SAM_GenericSystem", "annual_fuel_usage_lifetime");
	});
	return result;
}

SAM_EXPORT double* SAM_GenericSystem_HybridCosts_degradation_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "degradation", length);
	if (!result)
		make_access_error("SAM_GenericSystem", "degradation");
	});
	return result;
}

SAM_EXPORT double SAM_GenericSystem_HybridCosts_land_area_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "land_area", &result))
		make_access_error("SAM_GenericSystem", "land_area");
	});
	return result;
}

SAM_EXPORT double* SAM_GenericSystem_HybridCosts_om_capacity_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "om_capacity", length);
	if (!result)
		make_access_error("SAM_GenericSystem", "om_capacity");
	});
	return result;
}

SAM_EXPORT double SAM_GenericSystem_HybridCosts_om_capacity_escal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "om_capacity_escal", &result))
		make_access_error("SAM_GenericSystem", "om_capacity_escal");
	});
	return result;
}

SAM_EXPORT double* SAM_GenericSystem_HybridCosts_om_fixed_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "om_fixed", length);
	if (!result)
		make_access_error("SAM_GenericSystem", "om_fixed");
	});
	return result;
}

SAM_EXPORT double SAM_GenericSystem_HybridCosts_om_fixed_escal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "om_fixed_escal", &result))
		make_access_error("SAM_GenericSystem", "om_fixed_escal");
	});
	return result;
}

SAM_EXPORT double* SAM_GenericSystem_HybridCosts_om_fuel_cost_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "om_fuel_cost", length);
	if (!result)
		make_access_error("SAM_GenericSystem", "om_fuel_cost");
	});
	return result;
}

SAM_EXPORT double SAM_GenericSystem_HybridCosts_om_fuel_cost_escal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "om_fuel_cost_escal", &result))
		make_access_error("SAM_GenericSystem", "om_fuel_cost_escal");
	});
	return result;
}

SAM_EXPORT double* SAM_GenericSystem_HybridCosts_om_land_lease_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "om_land_lease", length);
	if (!result)
		make_access_error("SAM_GenericSystem", "om_land_lease");
	});
	return result;
}

SAM_EXPORT double SAM_GenericSystem_HybridCosts_om_land_lease_escal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "om_land_lease_escal", &result))
		make_access_error("SAM_GenericSystem", "om_land_lease_escal");
	});
	return result;
}

SAM_EXPORT double* SAM_GenericSystem_HybridCosts_om_production_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "om_production", length);
	if (!result)
		make_access_error("SAM_GenericSystem", "om_production");
	});
	return result;
}

SAM_EXPORT double SAM_GenericSystem_HybridCosts_om_production_escal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "om_production_escal", &result))
		make_access_error("SAM_GenericSystem", "om_production_escal");
	});
	return result;
}

SAM_EXPORT double SAM_GenericSystem_HybridCosts_total_installed_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_installed_cost", &result))
		make_access_error("SAM_GenericSystem", "total_installed_cost");
	});
	return result;
}

SAM_EXPORT double SAM_GenericSystem_Outputs_annual_energy_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_energy", &result))
		make_access_error("SAM_GenericSystem", "annual_energy");
	});
	return result;
}

SAM_EXPORT double* SAM_GenericSystem_Outputs_annual_energy_distribution_time_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "annual_energy_distribution_time", nrows, ncols);
	if (!result)
		make_access_error("SAM_GenericSystem", "annual_energy_distribution_time");
	});
	return result;
}

SAM_EXPORT double SAM_GenericSystem_Outputs_annual_fuel_usage_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_fuel_usage", &result))
		make_access_error("SAM_GenericSystem", "annual_fuel_usage");
	});
	return result;
}

SAM_EXPORT double SAM_GenericSystem_Outputs_capacity_factor_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "capacity_factor", &result))
		make_access_error("SAM_GenericSystem", "capacity_factor");
	});
	return result;
}

SAM_EXPORT double* SAM_GenericSystem_Outputs_cf_battery_replacement_cost_schedule_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_battery_replacement_cost_schedule", length);
	if (!result)
		make_access_error("SAM_GenericSystem", "cf_battery_replacement_cost_schedule");
	});
	return result;
}

SAM_EXPORT double* SAM_GenericSystem_Outputs_cf_energy_net_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_energy_net", length);
	if (!result)
		make_access_error("SAM_GenericSystem", "cf_energy_net");
	});
	return result;
}

SAM_EXPORT double* SAM_GenericSystem_Outputs_cf_fuelcell_replacement_cost_schedule_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_fuelcell_replacement_cost_schedule", length);
	if (!result)
		make_access_error("SAM_GenericSystem", "cf_fuelcell_replacement_cost_schedule");
	});
	return result;
}

SAM_EXPORT double* SAM_GenericSystem_Outputs_cf_land_lease_expense_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_land_lease_expense", length);
	if (!result)
		make_access_error("SAM_GenericSystem", "cf_land_lease_expense");
	});
	return result;
}

SAM_EXPORT double* SAM_GenericSystem_Outputs_cf_om_capacity_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_om_capacity", length);
	if (!result)
		make_access_error("SAM_GenericSystem", "cf_om_capacity");
	});
	return result;
}

SAM_EXPORT double* SAM_GenericSystem_Outputs_cf_om_fixed_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_om_fixed", length);
	if (!result)
		make_access_error("SAM_GenericSystem", "cf_om_fixed");
	});
	return result;
}

SAM_EXPORT double* SAM_GenericSystem_Outputs_cf_om_fuel_cost_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_om_fuel_cost", length);
	if (!result)
		make_access_error("SAM_GenericSystem", "cf_om_fuel_cost");
	});
	return result;
}

SAM_EXPORT double* SAM_GenericSystem_Outputs_cf_om_land_lease_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_om_land_lease", length);
	if (!result)
		make_access_error("SAM_GenericSystem", "cf_om_land_lease");
	});
	return result;
}

SAM_EXPORT double* SAM_GenericSystem_Outputs_cf_om_production_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_om_production", length);
	if (!result)
		make_access_error("SAM_GenericSystem", "cf_om_production");
	});
	return result;
}

SAM_EXPORT double* SAM_GenericSystem_Outputs_gen_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "gen", length);
	if (!result)
		make_access_error("SAM_GenericSystem", "gen");
	});
	return result;
}

SAM_EXPORT double SAM_GenericSystem_Outputs_kwh_per_kw_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "kwh_per_kw", &result))
		make_access_error("SAM_GenericSystem", "kwh_per_kw");
	});
	return result;
}

SAM_EXPORT double* SAM_GenericSystem_Outputs_monthly_energy_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_energy", length);
	if (!result)
		make_access_error("SAM_GenericSystem", "monthly_energy");
	});
	return result;
}

SAM_EXPORT double SAM_GenericSystem_Outputs_system_heat_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_heat_rate", &result))
		make_access_error("SAM_GenericSystem", "system_heat_rate");
	});
	return result;
}

SAM_EXPORT double SAM_GenericSystem_Outputs_water_usage_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "water_usage", &result))
		make_access_error("SAM_GenericSystem", "water_usage");
	});
	return result;
}

