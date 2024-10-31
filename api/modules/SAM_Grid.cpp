#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_Grid.h"

SAM_EXPORT int SAM_Grid_execute(SAM_table data, int verbosity, SAM_error* err){
	return SAM_module_exec("grid", data, verbosity, err);
}

SAM_EXPORT void SAM_Grid_Lifetime_analysis_period_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "analysis_period", number);
	});
}

SAM_EXPORT void SAM_Grid_Lifetime_system_use_lifetime_output_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "system_use_lifetime_output", number);
	});
}

SAM_EXPORT void SAM_Grid_SystemOutput_annual_energy_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "annual_energy", number);
	});
}

SAM_EXPORT void SAM_Grid_SystemOutput_energy_hourly_kW_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "energy_hourly_kW", arr, length);
	});
}

SAM_EXPORT void SAM_Grid_SystemOutput_gen_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "gen", arr, length);
	});
}

SAM_EXPORT void SAM_Grid_Load_crit_load_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "crit_load", arr, length);
	});
}

SAM_EXPORT void SAM_Grid_Load_grid_outage_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "grid_outage", arr, length);
	});
}

SAM_EXPORT void SAM_Grid_Load_load_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "load", arr, length);
	});
}

SAM_EXPORT void SAM_Grid_Load_load_escalation_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "load_escalation", arr, length);
	});
}

SAM_EXPORT void SAM_Grid_Monthly_monthly_energy_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "monthly_energy", arr, length);
	});
}

SAM_EXPORT void SAM_Grid_GridLimits_enable_interconnection_limit_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "enable_interconnection_limit", number);
	});
}

SAM_EXPORT void SAM_Grid_GridLimits_grid_curtailment_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "grid_curtailment", arr, length);
	});
}

SAM_EXPORT void SAM_Grid_GridLimits_grid_interconnection_limit_kwac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "grid_interconnection_limit_kwac", number);
	});
}

SAM_EXPORT void SAM_Grid_HybridCosts_degradation_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "degradation", arr, length);
	});
}

SAM_EXPORT void SAM_Grid_HybridCosts_land_area_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "land_area", number);
	});
}

SAM_EXPORT void SAM_Grid_HybridCosts_om_capacity_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "om_capacity", arr, length);
	});
}

SAM_EXPORT void SAM_Grid_HybridCosts_om_capacity_escal_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "om_capacity_escal", number);
	});
}

SAM_EXPORT void SAM_Grid_HybridCosts_om_fixed_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "om_fixed", arr, length);
	});
}

SAM_EXPORT void SAM_Grid_HybridCosts_om_fixed_escal_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "om_fixed_escal", number);
	});
}

SAM_EXPORT void SAM_Grid_HybridCosts_om_land_lease_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "om_land_lease", arr, length);
	});
}

SAM_EXPORT void SAM_Grid_HybridCosts_om_land_lease_escal_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "om_land_lease_escal", number);
	});
}

SAM_EXPORT void SAM_Grid_HybridCosts_om_production_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "om_production", arr, length);
	});
}

SAM_EXPORT void SAM_Grid_HybridCosts_om_production_escal_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "om_production_escal", number);
	});
}

SAM_EXPORT void SAM_Grid_HybridCosts_total_installed_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "total_installed_cost", number);
	});
}

SAM_EXPORT double SAM_Grid_Lifetime_analysis_period_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "analysis_period", &result))
		make_access_error("SAM_Grid", "analysis_period");
	});
	return result;
}

SAM_EXPORT double SAM_Grid_Lifetime_system_use_lifetime_output_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_use_lifetime_output", &result))
		make_access_error("SAM_Grid", "system_use_lifetime_output");
	});
	return result;
}

SAM_EXPORT double SAM_Grid_SystemOutput_annual_energy_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_energy", &result))
		make_access_error("SAM_Grid", "annual_energy");
	});
	return result;
}

SAM_EXPORT double* SAM_Grid_SystemOutput_energy_hourly_kW_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "energy_hourly_kW", length);
	if (!result)
		make_access_error("SAM_Grid", "energy_hourly_kW");
	});
	return result;
}

SAM_EXPORT double* SAM_Grid_SystemOutput_gen_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "gen", length);
	if (!result)
		make_access_error("SAM_Grid", "gen");
	});
	return result;
}

SAM_EXPORT double* SAM_Grid_Load_crit_load_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "crit_load", length);
	if (!result)
		make_access_error("SAM_Grid", "crit_load");
	});
	return result;
}

SAM_EXPORT double* SAM_Grid_Load_grid_outage_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "grid_outage", length);
	if (!result)
		make_access_error("SAM_Grid", "grid_outage");
	});
	return result;
}

SAM_EXPORT double* SAM_Grid_Load_load_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "load", length);
	if (!result)
		make_access_error("SAM_Grid", "load");
	});
	return result;
}

SAM_EXPORT double* SAM_Grid_Load_load_escalation_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "load_escalation", length);
	if (!result)
		make_access_error("SAM_Grid", "load_escalation");
	});
	return result;
}

SAM_EXPORT double* SAM_Grid_Monthly_monthly_energy_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_energy", length);
	if (!result)
		make_access_error("SAM_Grid", "monthly_energy");
	});
	return result;
}

SAM_EXPORT double SAM_Grid_GridLimits_enable_interconnection_limit_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "enable_interconnection_limit", &result))
		make_access_error("SAM_Grid", "enable_interconnection_limit");
	});
	return result;
}

SAM_EXPORT double* SAM_Grid_GridLimits_grid_curtailment_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "grid_curtailment", length);
	if (!result)
		make_access_error("SAM_Grid", "grid_curtailment");
	});
	return result;
}

SAM_EXPORT double SAM_Grid_GridLimits_grid_interconnection_limit_kwac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "grid_interconnection_limit_kwac", &result))
		make_access_error("SAM_Grid", "grid_interconnection_limit_kwac");
	});
	return result;
}

SAM_EXPORT double* SAM_Grid_HybridCosts_degradation_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "degradation", length);
	if (!result)
		make_access_error("SAM_Grid", "degradation");
	});
	return result;
}

SAM_EXPORT double SAM_Grid_HybridCosts_land_area_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "land_area", &result))
		make_access_error("SAM_Grid", "land_area");
	});
	return result;
}

SAM_EXPORT double* SAM_Grid_HybridCosts_om_capacity_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "om_capacity", length);
	if (!result)
		make_access_error("SAM_Grid", "om_capacity");
	});
	return result;
}

SAM_EXPORT double SAM_Grid_HybridCosts_om_capacity_escal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "om_capacity_escal", &result))
		make_access_error("SAM_Grid", "om_capacity_escal");
	});
	return result;
}

SAM_EXPORT double* SAM_Grid_HybridCosts_om_fixed_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "om_fixed", length);
	if (!result)
		make_access_error("SAM_Grid", "om_fixed");
	});
	return result;
}

SAM_EXPORT double SAM_Grid_HybridCosts_om_fixed_escal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "om_fixed_escal", &result))
		make_access_error("SAM_Grid", "om_fixed_escal");
	});
	return result;
}

SAM_EXPORT double* SAM_Grid_HybridCosts_om_land_lease_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "om_land_lease", length);
	if (!result)
		make_access_error("SAM_Grid", "om_land_lease");
	});
	return result;
}

SAM_EXPORT double SAM_Grid_HybridCosts_om_land_lease_escal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "om_land_lease_escal", &result))
		make_access_error("SAM_Grid", "om_land_lease_escal");
	});
	return result;
}

SAM_EXPORT double* SAM_Grid_HybridCosts_om_production_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "om_production", length);
	if (!result)
		make_access_error("SAM_Grid", "om_production");
	});
	return result;
}

SAM_EXPORT double SAM_Grid_HybridCosts_om_production_escal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "om_production_escal", &result))
		make_access_error("SAM_Grid", "om_production_escal");
	});
	return result;
}

SAM_EXPORT double SAM_Grid_HybridCosts_total_installed_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_installed_cost", &result))
		make_access_error("SAM_Grid", "total_installed_cost");
	});
	return result;
}

SAM_EXPORT double SAM_Grid_Outputs_annual_ac_curtailment_loss_kwh_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_ac_curtailment_loss_kwh", &result))
		make_access_error("SAM_Grid", "annual_ac_curtailment_loss_kwh");
	});
	return result;
}

SAM_EXPORT double SAM_Grid_Outputs_annual_ac_curtailment_loss_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_ac_curtailment_loss_percent", &result))
		make_access_error("SAM_Grid", "annual_ac_curtailment_loss_percent");
	});
	return result;
}

SAM_EXPORT double SAM_Grid_Outputs_annual_ac_interconnect_loss_kwh_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_ac_interconnect_loss_kwh", &result))
		make_access_error("SAM_Grid", "annual_ac_interconnect_loss_kwh");
	});
	return result;
}

SAM_EXPORT double SAM_Grid_Outputs_annual_ac_interconnect_loss_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_ac_interconnect_loss_percent", &result))
		make_access_error("SAM_Grid", "annual_ac_interconnect_loss_percent");
	});
	return result;
}

SAM_EXPORT double* SAM_Grid_Outputs_annual_energy_distribution_time_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "annual_energy_distribution_time", nrows, ncols);
	if (!result)
		make_access_error("SAM_Grid", "annual_energy_distribution_time");
	});
	return result;
}

SAM_EXPORT double SAM_Grid_Outputs_annual_energy_pre_curtailment_ac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_energy_pre_curtailment_ac", &result))
		make_access_error("SAM_Grid", "annual_energy_pre_curtailment_ac");
	});
	return result;
}

SAM_EXPORT double SAM_Grid_Outputs_annual_energy_pre_interconnect_ac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_energy_pre_interconnect_ac", &result))
		make_access_error("SAM_Grid", "annual_energy_pre_interconnect_ac");
	});
	return result;
}

SAM_EXPORT double SAM_Grid_Outputs_capacity_factor_curtailment_ac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "capacity_factor_curtailment_ac", &result))
		make_access_error("SAM_Grid", "capacity_factor_curtailment_ac");
	});
	return result;
}

SAM_EXPORT double SAM_Grid_Outputs_capacity_factor_interconnect_ac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "capacity_factor_interconnect_ac", &result))
		make_access_error("SAM_Grid", "capacity_factor_interconnect_ac");
	});
	return result;
}

SAM_EXPORT double* SAM_Grid_Outputs_cf_land_lease_expense_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_land_lease_expense", length);
	if (!result)
		make_access_error("SAM_Grid", "cf_land_lease_expense");
	});
	return result;
}

SAM_EXPORT double* SAM_Grid_Outputs_full_load_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "full_load", length);
	if (!result)
		make_access_error("SAM_Grid", "full_load");
	});
	return result;
}

SAM_EXPORT double* SAM_Grid_Outputs_gen_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "gen", length);
	if (!result)
		make_access_error("SAM_Grid", "gen");
	});
	return result;
}

SAM_EXPORT double* SAM_Grid_Outputs_system_pre_curtailment_kwac_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "system_pre_curtailment_kwac", length);
	if (!result)
		make_access_error("SAM_Grid", "system_pre_curtailment_kwac");
	});
	return result;
}

SAM_EXPORT double* SAM_Grid_Outputs_system_pre_interconnect_kwac_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "system_pre_interconnect_kwac", length);
	if (!result)
		make_access_error("SAM_Grid", "system_pre_interconnect_kwac");
	});
	return result;
}

