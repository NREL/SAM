#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_LcoefcrDesign.h"

SAM_EXPORT int SAM_LcoefcrDesign_execute(SAM_table data, int verbosity, SAM_error* err){
	return SAM_module_exec("lcoefcr_design", data, verbosity, err);
}

SAM_EXPORT void SAM_LcoefcrDesign_SystemControl_sim_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sim_type", number);
	});
}

SAM_EXPORT void SAM_LcoefcrDesign_SimpleLCOE_annual_energy_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "annual_energy", number);
	});
}

SAM_EXPORT void SAM_LcoefcrDesign_SimpleLCOE_c_construction_cost_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "c_construction_cost", arr, length);
	});
}

SAM_EXPORT void SAM_LcoefcrDesign_SimpleLCOE_c_construction_interest_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "c_construction_interest", number);
	});
}

SAM_EXPORT void SAM_LcoefcrDesign_SimpleLCOE_c_debt_percent_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "c_debt_percent", number);
	});
}

SAM_EXPORT void SAM_LcoefcrDesign_SimpleLCOE_c_depreciation_schedule_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "c_depreciation_schedule", arr, length);
	});
}

SAM_EXPORT void SAM_LcoefcrDesign_SimpleLCOE_c_equity_return_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "c_equity_return", number);
	});
}

SAM_EXPORT void SAM_LcoefcrDesign_SimpleLCOE_c_inflation_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "c_inflation", number);
	});
}

SAM_EXPORT void SAM_LcoefcrDesign_SimpleLCOE_c_lifetime_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "c_lifetime", number);
	});
}

SAM_EXPORT void SAM_LcoefcrDesign_SimpleLCOE_c_nominal_interest_rate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "c_nominal_interest_rate", number);
	});
}

SAM_EXPORT void SAM_LcoefcrDesign_SimpleLCOE_c_tax_rate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "c_tax_rate", number);
	});
}

SAM_EXPORT void SAM_LcoefcrDesign_SimpleLCOE_fixed_operating_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fixed_operating_cost", number);
	});
}

SAM_EXPORT void SAM_LcoefcrDesign_SimpleLCOE_ui_fcr_input_option_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ui_fcr_input_option", number);
	});
}

SAM_EXPORT void SAM_LcoefcrDesign_SimpleLCOE_ui_fixed_charge_rate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ui_fixed_charge_rate", number);
	});
}

SAM_EXPORT void SAM_LcoefcrDesign_SimpleLCOE_variable_operating_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "variable_operating_cost", number);
	});
}

SAM_EXPORT void SAM_LcoefcrDesign_SystemCosts_total_installed_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "total_installed_cost", number);
	});
}

SAM_EXPORT void SAM_LcoefcrDesign_IPHLCOH_annual_electricity_consumption_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "annual_electricity_consumption", number);
	});
}

SAM_EXPORT void SAM_LcoefcrDesign_IPHLCOH_electricity_rate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "electricity_rate", number);
	});
}

SAM_EXPORT double SAM_LcoefcrDesign_SystemControl_sim_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sim_type", &result))
		make_access_error("SAM_LcoefcrDesign", "sim_type");
	});
	return result;
}

SAM_EXPORT double SAM_LcoefcrDesign_SimpleLCOE_annual_energy_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_energy", &result))
		make_access_error("SAM_LcoefcrDesign", "annual_energy");
	});
	return result;
}

SAM_EXPORT double* SAM_LcoefcrDesign_SimpleLCOE_c_construction_cost_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "c_construction_cost", length);
	if (!result)
		make_access_error("SAM_LcoefcrDesign", "c_construction_cost");
	});
	return result;
}

SAM_EXPORT double SAM_LcoefcrDesign_SimpleLCOE_c_construction_interest_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "c_construction_interest", &result))
		make_access_error("SAM_LcoefcrDesign", "c_construction_interest");
	});
	return result;
}

SAM_EXPORT double SAM_LcoefcrDesign_SimpleLCOE_c_debt_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "c_debt_percent", &result))
		make_access_error("SAM_LcoefcrDesign", "c_debt_percent");
	});
	return result;
}

SAM_EXPORT double* SAM_LcoefcrDesign_SimpleLCOE_c_depreciation_schedule_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "c_depreciation_schedule", length);
	if (!result)
		make_access_error("SAM_LcoefcrDesign", "c_depreciation_schedule");
	});
	return result;
}

SAM_EXPORT double SAM_LcoefcrDesign_SimpleLCOE_c_equity_return_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "c_equity_return", &result))
		make_access_error("SAM_LcoefcrDesign", "c_equity_return");
	});
	return result;
}

SAM_EXPORT double SAM_LcoefcrDesign_SimpleLCOE_c_inflation_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "c_inflation", &result))
		make_access_error("SAM_LcoefcrDesign", "c_inflation");
	});
	return result;
}

SAM_EXPORT double SAM_LcoefcrDesign_SimpleLCOE_c_lifetime_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "c_lifetime", &result))
		make_access_error("SAM_LcoefcrDesign", "c_lifetime");
	});
	return result;
}

SAM_EXPORT double SAM_LcoefcrDesign_SimpleLCOE_c_nominal_interest_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "c_nominal_interest_rate", &result))
		make_access_error("SAM_LcoefcrDesign", "c_nominal_interest_rate");
	});
	return result;
}

SAM_EXPORT double SAM_LcoefcrDesign_SimpleLCOE_c_tax_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "c_tax_rate", &result))
		make_access_error("SAM_LcoefcrDesign", "c_tax_rate");
	});
	return result;
}

SAM_EXPORT double SAM_LcoefcrDesign_SimpleLCOE_fixed_operating_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fixed_operating_cost", &result))
		make_access_error("SAM_LcoefcrDesign", "fixed_operating_cost");
	});
	return result;
}

SAM_EXPORT double SAM_LcoefcrDesign_SimpleLCOE_ui_fcr_input_option_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ui_fcr_input_option", &result))
		make_access_error("SAM_LcoefcrDesign", "ui_fcr_input_option");
	});
	return result;
}

SAM_EXPORT double SAM_LcoefcrDesign_SimpleLCOE_ui_fixed_charge_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ui_fixed_charge_rate", &result))
		make_access_error("SAM_LcoefcrDesign", "ui_fixed_charge_rate");
	});
	return result;
}

SAM_EXPORT double SAM_LcoefcrDesign_SimpleLCOE_variable_operating_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "variable_operating_cost", &result))
		make_access_error("SAM_LcoefcrDesign", "variable_operating_cost");
	});
	return result;
}

SAM_EXPORT double SAM_LcoefcrDesign_SystemCosts_total_installed_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_installed_cost", &result))
		make_access_error("SAM_LcoefcrDesign", "total_installed_cost");
	});
	return result;
}

SAM_EXPORT double SAM_LcoefcrDesign_IPHLCOH_annual_electricity_consumption_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_electricity_consumption", &result))
		make_access_error("SAM_LcoefcrDesign", "annual_electricity_consumption");
	});
	return result;
}

SAM_EXPORT double SAM_LcoefcrDesign_IPHLCOH_electricity_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "electricity_rate", &result))
		make_access_error("SAM_LcoefcrDesign", "electricity_rate");
	});
	return result;
}

SAM_EXPORT double SAM_LcoefcrDesign_Outputs_cfin_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cfin", &result))
		make_access_error("SAM_LcoefcrDesign", "cfin");
	});
	return result;
}

SAM_EXPORT double SAM_LcoefcrDesign_Outputs_crf_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "crf", &result))
		make_access_error("SAM_LcoefcrDesign", "crf");
	});
	return result;
}

SAM_EXPORT double SAM_LcoefcrDesign_Outputs_fixed_charge_rate_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fixed_charge_rate_calc", &result))
		make_access_error("SAM_LcoefcrDesign", "fixed_charge_rate_calc");
	});
	return result;
}

SAM_EXPORT double SAM_LcoefcrDesign_Outputs_lcoe_fcr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "lcoe_fcr", &result))
		make_access_error("SAM_LcoefcrDesign", "lcoe_fcr");
	});
	return result;
}

SAM_EXPORT double SAM_LcoefcrDesign_Outputs_pfin_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pfin", &result))
		make_access_error("SAM_LcoefcrDesign", "pfin");
	});
	return result;
}

SAM_EXPORT double SAM_LcoefcrDesign_Outputs_wacc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "wacc", &result))
		make_access_error("SAM_LcoefcrDesign", "wacc");
	});
	return result;
}

