#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_Thirdpartyownership.h"

SAM_EXPORT int SAM_Thirdpartyownership_execute(SAM_table data, int verbosity, SAM_error* err){
	int n_err = 0;
	translateExceptions(err, [&]{
		n_err += SAM_module_exec("thirdpartyownership", data, verbosity, err);
	});
	return n_err;
}


SAM_EXPORT void SAM_Thirdpartyownership_Depreciation_depr_fed_custom_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "depr_fed_custom", arr, length);
	});
}

SAM_EXPORT void SAM_Thirdpartyownership_Depreciation_depr_fed_sl_years_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "depr_fed_sl_years", number);
	});
}

SAM_EXPORT void SAM_Thirdpartyownership_Depreciation_depr_fed_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "depr_fed_type", number);
	});
}

SAM_EXPORT void SAM_Thirdpartyownership_Depreciation_depr_sta_custom_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "depr_sta_custom", arr, length);
	});
}

SAM_EXPORT void SAM_Thirdpartyownership_Depreciation_depr_sta_sl_years_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "depr_sta_sl_years", number);
	});
}

SAM_EXPORT void SAM_Thirdpartyownership_Depreciation_depr_sta_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "depr_sta_type", number);
	});
}

SAM_EXPORT void SAM_Thirdpartyownership_Financials_analysis_period_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "analysis_period", number);
	});
}

SAM_EXPORT void SAM_Thirdpartyownership_Financials_inflation_rate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inflation_rate", number);
	});
}

SAM_EXPORT void SAM_Thirdpartyownership_Financials_real_discount_rate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "real_discount_rate", number);
	});
}

SAM_EXPORT void SAM_Thirdpartyownership_FinancialThirdPartyOwnership_lease_or_ppa_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "lease_or_ppa", number);
	});
}

SAM_EXPORT void SAM_Thirdpartyownership_Common_annual_energy_value_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "annual_energy_value", arr, length);
	});
}

SAM_EXPORT void SAM_Thirdpartyownership_Common_gen_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "gen", arr, length);
	});
}

SAM_EXPORT void SAM_Thirdpartyownership_AnnualOutput_degradation_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "degradation", arr, length);
	});
}

SAM_EXPORT void SAM_Thirdpartyownership_AnnualOutput_system_use_lifetime_output_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "system_use_lifetime_output", number);
	});
}

SAM_EXPORT void SAM_Thirdpartyownership_CashFlow_lease_escalation_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "lease_escalation", number);
	});
}

SAM_EXPORT void SAM_Thirdpartyownership_CashFlow_lease_price_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "lease_price", number);
	});
}

SAM_EXPORT void SAM_Thirdpartyownership_CashFlow_ppa_escalation_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ppa_escalation", number);
	});
}

SAM_EXPORT void SAM_Thirdpartyownership_CashFlow_ppa_price_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ppa_price", number);
	});
}

SAM_EXPORT void SAM_Thirdpartyownership_ElectricityCost_elec_cost_with_system_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "elec_cost_with_system", arr, length);
	});
}

SAM_EXPORT void SAM_Thirdpartyownership_ElectricityCost_elec_cost_without_system_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "elec_cost_without_system", arr, length);
	});
}

SAM_EXPORT double* SAM_Thirdpartyownership_Depreciation_depr_fed_custom_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "depr_fed_custom", length);
	if (!result)
		make_access_error("SAM_Thirdpartyownership", "depr_fed_custom");
	});
	return result;
}



SAM_EXPORT double SAM_Thirdpartyownership_Depreciation_depr_fed_sl_years_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fed_sl_years", &result))
		make_access_error("SAM_Thirdpartyownership", "depr_fed_sl_years");
	});
	return result;
}



SAM_EXPORT double SAM_Thirdpartyownership_Depreciation_depr_fed_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fed_type", &result))
		make_access_error("SAM_Thirdpartyownership", "depr_fed_type");
	});
	return result;
}



SAM_EXPORT double* SAM_Thirdpartyownership_Depreciation_depr_sta_custom_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "depr_sta_custom", length);
	if (!result)
		make_access_error("SAM_Thirdpartyownership", "depr_sta_custom");
	});
	return result;
}



SAM_EXPORT double SAM_Thirdpartyownership_Depreciation_depr_sta_sl_years_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_sta_sl_years", &result))
		make_access_error("SAM_Thirdpartyownership", "depr_sta_sl_years");
	});
	return result;
}



SAM_EXPORT double SAM_Thirdpartyownership_Depreciation_depr_sta_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_sta_type", &result))
		make_access_error("SAM_Thirdpartyownership", "depr_sta_type");
	});
	return result;
}



SAM_EXPORT double SAM_Thirdpartyownership_Financials_analysis_period_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "analysis_period", &result))
		make_access_error("SAM_Thirdpartyownership", "analysis_period");
	});
	return result;
}



SAM_EXPORT double SAM_Thirdpartyownership_Financials_inflation_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inflation_rate", &result))
		make_access_error("SAM_Thirdpartyownership", "inflation_rate");
	});
	return result;
}



SAM_EXPORT double SAM_Thirdpartyownership_Financials_real_discount_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "real_discount_rate", &result))
		make_access_error("SAM_Thirdpartyownership", "real_discount_rate");
	});
	return result;
}



SAM_EXPORT double SAM_Thirdpartyownership_FinancialThirdPartyOwnership_lease_or_ppa_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "lease_or_ppa", &result))
		make_access_error("SAM_Thirdpartyownership", "lease_or_ppa");
	});
	return result;
}



SAM_EXPORT double* SAM_Thirdpartyownership_Common_annual_energy_value_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "annual_energy_value", length);
	if (!result)
		make_access_error("SAM_Thirdpartyownership", "annual_energy_value");
	});
	return result;
}



SAM_EXPORT double* SAM_Thirdpartyownership_Common_gen_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "gen", length);
	if (!result)
		make_access_error("SAM_Thirdpartyownership", "gen");
	});
	return result;
}



SAM_EXPORT double* SAM_Thirdpartyownership_AnnualOutput_degradation_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "degradation", length);
	if (!result)
		make_access_error("SAM_Thirdpartyownership", "degradation");
	});
	return result;
}



SAM_EXPORT double SAM_Thirdpartyownership_AnnualOutput_system_use_lifetime_output_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_use_lifetime_output", &result))
		make_access_error("SAM_Thirdpartyownership", "system_use_lifetime_output");
	});
	return result;
}



SAM_EXPORT double SAM_Thirdpartyownership_CashFlow_lease_escalation_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "lease_escalation", &result))
		make_access_error("SAM_Thirdpartyownership", "lease_escalation");
	});
	return result;
}



SAM_EXPORT double SAM_Thirdpartyownership_CashFlow_lease_price_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "lease_price", &result))
		make_access_error("SAM_Thirdpartyownership", "lease_price");
	});
	return result;
}



SAM_EXPORT double SAM_Thirdpartyownership_CashFlow_ppa_escalation_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ppa_escalation", &result))
		make_access_error("SAM_Thirdpartyownership", "ppa_escalation");
	});
	return result;
}



SAM_EXPORT double SAM_Thirdpartyownership_CashFlow_ppa_price_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ppa_price", &result))
		make_access_error("SAM_Thirdpartyownership", "ppa_price");
	});
	return result;
}



SAM_EXPORT double* SAM_Thirdpartyownership_ElectricityCost_elec_cost_with_system_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "elec_cost_with_system", length);
	if (!result)
		make_access_error("SAM_Thirdpartyownership", "elec_cost_with_system");
	});
	return result;
}



SAM_EXPORT double* SAM_Thirdpartyownership_ElectricityCost_elec_cost_without_system_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "elec_cost_without_system", length);
	if (!result)
		make_access_error("SAM_Thirdpartyownership", "elec_cost_without_system");
	});
	return result;
}



SAM_EXPORT double* SAM_Thirdpartyownership_Outputs_cf_after_tax_cash_flow_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_after_tax_cash_flow", length);
	if (!result)
		make_access_error("SAM_Thirdpartyownership", "cf_after_tax_cash_flow");
	});
	return result;
}



SAM_EXPORT double* SAM_Thirdpartyownership_Outputs_cf_after_tax_net_equity_cost_flow_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_after_tax_net_equity_cost_flow", length);
	if (!result)
		make_access_error("SAM_Thirdpartyownership", "cf_after_tax_net_equity_cost_flow");
	});
	return result;
}



SAM_EXPORT double* SAM_Thirdpartyownership_Outputs_cf_agreement_cost_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_agreement_cost", length);
	if (!result)
		make_access_error("SAM_Thirdpartyownership", "cf_agreement_cost");
	});
	return result;
}



SAM_EXPORT double* SAM_Thirdpartyownership_Outputs_cf_cumulative_payback_with_expenses_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_cumulative_payback_with_expenses", length);
	if (!result)
		make_access_error("SAM_Thirdpartyownership", "cf_cumulative_payback_with_expenses");
	});
	return result;
}



SAM_EXPORT double* SAM_Thirdpartyownership_Outputs_cf_energy_net_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_energy_net", length);
	if (!result)
		make_access_error("SAM_Thirdpartyownership", "cf_energy_net");
	});
	return result;
}



SAM_EXPORT double SAM_Thirdpartyownership_Outputs_cf_length_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cf_length", &result))
		make_access_error("SAM_Thirdpartyownership", "cf_length");
	});
	return result;
}



SAM_EXPORT double* SAM_Thirdpartyownership_Outputs_cf_nte_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_nte", length);
	if (!result)
		make_access_error("SAM_Thirdpartyownership", "cf_nte");
	});
	return result;
}



SAM_EXPORT double* SAM_Thirdpartyownership_Outputs_cf_payback_with_expenses_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_payback_with_expenses", length);
	if (!result)
		make_access_error("SAM_Thirdpartyownership", "cf_payback_with_expenses");
	});
	return result;
}



SAM_EXPORT double SAM_Thirdpartyownership_Outputs_lnte_nom_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "lnte_nom", &result))
		make_access_error("SAM_Thirdpartyownership", "lnte_nom");
	});
	return result;
}



SAM_EXPORT double SAM_Thirdpartyownership_Outputs_lnte_real_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "lnte_real", &result))
		make_access_error("SAM_Thirdpartyownership", "lnte_real");
	});
	return result;
}



SAM_EXPORT double SAM_Thirdpartyownership_Outputs_npv_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "npv", &result))
		make_access_error("SAM_Thirdpartyownership", "npv");
	});
	return result;
}



SAM_EXPORT double SAM_Thirdpartyownership_Outputs_year1_nte_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "year1_nte", &result))
		make_access_error("SAM_Thirdpartyownership", "year1_nte");
	});
	return result;
}



