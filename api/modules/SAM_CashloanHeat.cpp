#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_CashloanHeat.h"

SAM_EXPORT int SAM_CashloanHeat_execute(SAM_table data, int verbosity, SAM_error* err){
	return SAM_module_exec("cashloan_heat", data, verbosity, err);
}

SAM_EXPORT void SAM_CashloanHeat_FinancialParameters_analysis_period_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "analysis_period", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_FinancialParameters_debt_fraction_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "debt_fraction", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_FinancialParameters_federal_tax_rate_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "federal_tax_rate", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_FinancialParameters_inflation_rate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inflation_rate", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_FinancialParameters_insurance_rate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "insurance_rate", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_FinancialParameters_loan_rate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "loan_rate", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_FinancialParameters_loan_term_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "loan_term", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_FinancialParameters_market_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "market", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_FinancialParameters_mortgage_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mortgage", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_FinancialParameters_prop_tax_assessed_decline_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "prop_tax_assessed_decline", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_FinancialParameters_prop_tax_cost_assessed_percent_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "prop_tax_cost_assessed_percent", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_FinancialParameters_property_tax_rate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "property_tax_rate", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_FinancialParameters_real_discount_rate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "real_discount_rate", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_FinancialParameters_salvage_percentage_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "salvage_percentage", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_FinancialParameters_state_tax_rate_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "state_tax_rate", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_FinancialParameters_system_capacity_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "system_capacity", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_FinancialParameters_system_heat_rate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "system_heat_rate", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_SystemCosts_add_om_num_types_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "add_om_num_types", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_SystemCosts_annual_fuel_usage_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "annual_fuel_usage", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_SystemCosts_annual_fuel_usage_lifetime_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "annual_fuel_usage_lifetime", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_SystemCosts_om_batt_capacity_cost_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "om_batt_capacity_cost", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_SystemCosts_om_batt_fixed_cost_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "om_batt_fixed_cost", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_SystemCosts_om_batt_nameplate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "om_batt_nameplate", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_SystemCosts_om_batt_replacement_cost_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "om_batt_replacement_cost", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_SystemCosts_om_batt_variable_cost_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "om_batt_variable_cost", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_SystemCosts_om_capacity_escal_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "om_capacity_escal", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_SystemCosts_om_capacity_heat_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "om_capacity_heat", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_SystemCosts_om_elec_price_for_heat_techs_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "om_elec_price_for_heat_techs", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_SystemCosts_om_elec_price_for_heat_techs_escal_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "om_elec_price_for_heat_techs_escal", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_SystemCosts_om_fixed_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "om_fixed", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_SystemCosts_om_fixed_escal_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "om_fixed_escal", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_SystemCosts_om_fuel_cost_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "om_fuel_cost", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_SystemCosts_om_fuel_cost_escal_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "om_fuel_cost_escal", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_SystemCosts_om_fuelcell_capacity_cost_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "om_fuelcell_capacity_cost", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_SystemCosts_om_fuelcell_fixed_cost_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "om_fuelcell_fixed_cost", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_SystemCosts_om_fuelcell_nameplate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "om_fuelcell_nameplate", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_SystemCosts_om_fuelcell_replacement_cost_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "om_fuelcell_replacement_cost", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_SystemCosts_om_fuelcell_variable_cost_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "om_fuelcell_variable_cost", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_SystemCosts_om_opt_fuel_1_cost_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "om_opt_fuel_1_cost", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_SystemCosts_om_opt_fuel_1_cost_escal_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "om_opt_fuel_1_cost_escal", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_SystemCosts_om_opt_fuel_1_usage_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "om_opt_fuel_1_usage", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_SystemCosts_om_opt_fuel_2_cost_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "om_opt_fuel_2_cost", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_SystemCosts_om_opt_fuel_2_cost_escal_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "om_opt_fuel_2_cost_escal", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_SystemCosts_om_opt_fuel_2_usage_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "om_opt_fuel_2_usage", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_SystemCosts_om_production1_values_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "om_production1_values", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_SystemCosts_om_production2_values_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "om_production2_values", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_SystemCosts_om_production_escal_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "om_production_escal", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_SystemCosts_om_production_heat_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "om_production_heat", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_SystemCosts_om_replacement_cost_escal_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "om_replacement_cost_escal", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_SystemCosts_total_installed_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "total_installed_cost", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_LandLease_land_area_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "land_area", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_LandLease_om_land_lease_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "om_land_lease", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_LandLease_om_land_lease_escal_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "om_land_lease_escal", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_Depreciation_depr_fed_custom_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "depr_fed_custom", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_Depreciation_depr_fed_sl_years_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "depr_fed_sl_years", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_Depreciation_depr_fed_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "depr_fed_type", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_Depreciation_depr_sta_custom_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "depr_sta_custom", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_Depreciation_depr_sta_sl_years_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "depr_sta_sl_years", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_Depreciation_depr_sta_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "depr_sta_type", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_TaxCreditIncentives_itc_fed_amount_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "itc_fed_amount", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_TaxCreditIncentives_itc_fed_amount_deprbas_fed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "itc_fed_amount_deprbas_fed", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_TaxCreditIncentives_itc_fed_amount_deprbas_sta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "itc_fed_amount_deprbas_sta", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_TaxCreditIncentives_itc_fed_percent_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "itc_fed_percent", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_TaxCreditIncentives_itc_fed_percent_deprbas_fed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "itc_fed_percent_deprbas_fed", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_TaxCreditIncentives_itc_fed_percent_deprbas_sta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "itc_fed_percent_deprbas_sta", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_TaxCreditIncentives_itc_fed_percent_maxvalue_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "itc_fed_percent_maxvalue", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_TaxCreditIncentives_itc_sta_amount_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "itc_sta_amount", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_TaxCreditIncentives_itc_sta_amount_deprbas_fed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "itc_sta_amount_deprbas_fed", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_TaxCreditIncentives_itc_sta_amount_deprbas_sta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "itc_sta_amount_deprbas_sta", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_TaxCreditIncentives_itc_sta_percent_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "itc_sta_percent", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_TaxCreditIncentives_itc_sta_percent_deprbas_fed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "itc_sta_percent_deprbas_fed", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_TaxCreditIncentives_itc_sta_percent_deprbas_sta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "itc_sta_percent_deprbas_sta", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_TaxCreditIncentives_itc_sta_percent_maxvalue_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "itc_sta_percent_maxvalue", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_TaxCreditIncentives_ptc_fed_amount_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "ptc_fed_amount", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_TaxCreditIncentives_ptc_fed_amount_heat_btu_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "ptc_fed_amount_heat_btu", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_TaxCreditIncentives_ptc_fed_escal_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ptc_fed_escal", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_TaxCreditIncentives_ptc_fed_term_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ptc_fed_term", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_TaxCreditIncentives_ptc_sta_amount_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "ptc_sta_amount", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_TaxCreditIncentives_ptc_sta_amount_heat_btu_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "ptc_sta_amount_heat_btu", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_TaxCreditIncentives_ptc_sta_escal_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ptc_sta_escal", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_TaxCreditIncentives_ptc_sta_term_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ptc_sta_term", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_cbi_fed_amount_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cbi_fed_amount", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_cbi_fed_amount_heat_btu_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cbi_fed_amount_heat_btu", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_cbi_fed_deprbas_fed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cbi_fed_deprbas_fed", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_cbi_fed_deprbas_sta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cbi_fed_deprbas_sta", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_cbi_fed_maxvalue_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cbi_fed_maxvalue", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_cbi_fed_tax_fed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cbi_fed_tax_fed", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_cbi_fed_tax_sta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cbi_fed_tax_sta", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_cbi_oth_amount_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cbi_oth_amount", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_cbi_oth_amount_heat_btu_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cbi_oth_amount_heat_btu", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_cbi_oth_deprbas_fed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cbi_oth_deprbas_fed", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_cbi_oth_deprbas_sta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cbi_oth_deprbas_sta", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_cbi_oth_maxvalue_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cbi_oth_maxvalue", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_cbi_oth_tax_fed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cbi_oth_tax_fed", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_cbi_oth_tax_sta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cbi_oth_tax_sta", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_cbi_sta_amount_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cbi_sta_amount", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_cbi_sta_amount_heat_btu_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cbi_sta_amount_heat_btu", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_cbi_sta_deprbas_fed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cbi_sta_deprbas_fed", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_cbi_sta_deprbas_sta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cbi_sta_deprbas_sta", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_cbi_sta_maxvalue_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cbi_sta_maxvalue", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_cbi_sta_tax_fed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cbi_sta_tax_fed", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_cbi_sta_tax_sta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cbi_sta_tax_sta", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_cbi_uti_amount_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cbi_uti_amount", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_cbi_uti_amount_heat_btu_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cbi_uti_amount_heat_btu", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_cbi_uti_deprbas_fed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cbi_uti_deprbas_fed", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_cbi_uti_deprbas_sta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cbi_uti_deprbas_sta", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_cbi_uti_maxvalue_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cbi_uti_maxvalue", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_cbi_uti_tax_fed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cbi_uti_tax_fed", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_cbi_uti_tax_sta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cbi_uti_tax_sta", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_ibi_fed_amount_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_fed_amount", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_ibi_fed_amount_deprbas_fed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_fed_amount_deprbas_fed", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_ibi_fed_amount_deprbas_sta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_fed_amount_deprbas_sta", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_ibi_fed_amount_tax_fed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_fed_amount_tax_fed", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_ibi_fed_amount_tax_sta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_fed_amount_tax_sta", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_ibi_fed_percent_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_fed_percent", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_ibi_fed_percent_deprbas_fed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_fed_percent_deprbas_fed", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_ibi_fed_percent_deprbas_sta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_fed_percent_deprbas_sta", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_ibi_fed_percent_maxvalue_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_fed_percent_maxvalue", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_ibi_fed_percent_tax_fed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_fed_percent_tax_fed", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_ibi_fed_percent_tax_sta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_fed_percent_tax_sta", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_ibi_oth_amount_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_oth_amount", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_ibi_oth_amount_deprbas_fed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_oth_amount_deprbas_fed", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_ibi_oth_amount_deprbas_sta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_oth_amount_deprbas_sta", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_ibi_oth_amount_tax_fed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_oth_amount_tax_fed", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_ibi_oth_amount_tax_sta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_oth_amount_tax_sta", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_ibi_oth_percent_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_oth_percent", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_ibi_oth_percent_deprbas_fed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_oth_percent_deprbas_fed", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_ibi_oth_percent_deprbas_sta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_oth_percent_deprbas_sta", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_ibi_oth_percent_maxvalue_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_oth_percent_maxvalue", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_ibi_oth_percent_tax_fed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_oth_percent_tax_fed", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_ibi_oth_percent_tax_sta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_oth_percent_tax_sta", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_ibi_sta_amount_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_sta_amount", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_ibi_sta_amount_deprbas_fed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_sta_amount_deprbas_fed", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_ibi_sta_amount_deprbas_sta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_sta_amount_deprbas_sta", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_ibi_sta_amount_tax_fed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_sta_amount_tax_fed", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_ibi_sta_amount_tax_sta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_sta_amount_tax_sta", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_ibi_sta_percent_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_sta_percent", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_ibi_sta_percent_deprbas_fed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_sta_percent_deprbas_fed", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_ibi_sta_percent_deprbas_sta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_sta_percent_deprbas_sta", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_ibi_sta_percent_maxvalue_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_sta_percent_maxvalue", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_ibi_sta_percent_tax_fed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_sta_percent_tax_fed", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_ibi_sta_percent_tax_sta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_sta_percent_tax_sta", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_ibi_uti_amount_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_uti_amount", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_ibi_uti_amount_deprbas_fed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_uti_amount_deprbas_fed", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_ibi_uti_amount_deprbas_sta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_uti_amount_deprbas_sta", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_ibi_uti_amount_tax_fed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_uti_amount_tax_fed", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_ibi_uti_amount_tax_sta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_uti_amount_tax_sta", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_ibi_uti_percent_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_uti_percent", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_ibi_uti_percent_deprbas_fed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_uti_percent_deprbas_fed", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_ibi_uti_percent_deprbas_sta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_uti_percent_deprbas_sta", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_ibi_uti_percent_maxvalue_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_uti_percent_maxvalue", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_ibi_uti_percent_tax_fed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_uti_percent_tax_fed", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_ibi_uti_percent_tax_sta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_uti_percent_tax_sta", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_pbi_fed_amount_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "pbi_fed_amount", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_pbi_fed_amount_heat_btu_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "pbi_fed_amount_heat_btu", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_pbi_fed_escal_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pbi_fed_escal", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_pbi_fed_tax_fed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pbi_fed_tax_fed", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_pbi_fed_tax_sta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pbi_fed_tax_sta", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_pbi_fed_term_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pbi_fed_term", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_pbi_oth_amount_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "pbi_oth_amount", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_pbi_oth_amount_heat_btu_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "pbi_oth_amount_heat_btu", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_pbi_oth_escal_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pbi_oth_escal", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_pbi_oth_tax_fed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pbi_oth_tax_fed", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_pbi_oth_tax_sta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pbi_oth_tax_sta", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_pbi_oth_term_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pbi_oth_term", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_pbi_sta_amount_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "pbi_sta_amount", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_pbi_sta_amount_heat_btu_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "pbi_sta_amount_heat_btu", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_pbi_sta_escal_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pbi_sta_escal", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_pbi_sta_tax_fed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pbi_sta_tax_fed", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_pbi_sta_tax_sta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pbi_sta_tax_sta", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_pbi_sta_term_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pbi_sta_term", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_pbi_uti_amount_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "pbi_uti_amount", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_pbi_uti_amount_heat_btu_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "pbi_uti_amount_heat_btu", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_pbi_uti_escal_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pbi_uti_escal", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_pbi_uti_tax_fed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pbi_uti_tax_fed", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_pbi_uti_tax_sta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pbi_uti_tax_sta", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_PaymentIncentives_pbi_uti_term_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pbi_uti_term", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_BatterySystem_batt_bank_replacement_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "batt_bank_replacement", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_BatterySystem_batt_computed_bank_capacity_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_computed_bank_capacity", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_BatterySystem_batt_replacement_option_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_replacement_option", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_BatterySystem_batt_replacement_schedule_percent_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "batt_replacement_schedule_percent", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_BatterySystem_battery_per_kWh_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "battery_per_kWh", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_BatterySystem_en_batt_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "en_batt", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_BatterySystem_en_standalone_batt_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "en_standalone_batt", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_BatterySystem_en_wave_batt_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "en_wave_batt", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_FuelCell_annual_fuel_usage_lifetime_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "annual_fuel_usage_lifetime", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_FuelCell_en_fuelcell_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "en_fuelcell", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_FuelCell_fuelcell_annual_energy_discharged_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "fuelcell_annual_energy_discharged", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_FuelCell_fuelcell_computed_bank_capacity_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fuelcell_computed_bank_capacity", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_FuelCell_fuelcell_per_kWh_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fuelcell_per_kWh", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_FuelCell_fuelcell_replacement_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "fuelcell_replacement", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_FuelCell_fuelcell_replacement_option_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fuelcell_replacement_option", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_FuelCell_fuelcell_replacement_schedule_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "fuelcell_replacement_schedule", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_ChargesByMonth_charge_w_sys_dc_tou_ym_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "charge_w_sys_dc_tou_ym", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_CashloanHeat_ChargesByMonth_charge_w_sys_ec_ym_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "charge_w_sys_ec_ym", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_CashloanHeat_ChargesByMonth_charge_w_sys_fixed_ym_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "charge_w_sys_fixed_ym", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_CashloanHeat_ChargesByMonth_net_billing_credits_ym_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "net_billing_credits_ym", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_CashloanHeat_ChargesByMonth_nm_dollars_applied_ym_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "nm_dollars_applied_ym", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_CashloanHeat_ChargesByMonth_true_up_credits_ym_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "true_up_credits_ym", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_CashloanHeat_ChargesByMonth_utility_bill_w_sys_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "utility_bill_w_sys", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_Battery_batt_capacity_percent_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "batt_capacity_percent", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_Battery_monthly_batt_to_grid_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "monthly_batt_to_grid", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_Battery_monthly_grid_to_batt_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "monthly_grid_to_batt", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_Battery_monthly_grid_to_load_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "monthly_grid_to_load", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_TimeSeries_year1_hourly_dc_with_system_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "year1_hourly_dc_with_system", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_TimeSeries_year1_hourly_e_fromgrid_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "year1_hourly_e_fromgrid", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_TimeSeries_year1_hourly_ec_with_system_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "year1_hourly_ec_with_system", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_SystemOutput_annual_energy_value_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "annual_energy_value", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_SystemOutput_annual_thermal_value_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "annual_thermal_value", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_SystemOutput_degradation_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "degradation", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_SystemOutput_gen_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "gen", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_SystemOutput_gen_purchases_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "gen_purchases", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_Lifetime_system_use_lifetime_output_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "system_use_lifetime_output", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_ThirdPartyOwnership_elec_cost_with_system_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "elec_cost_with_system", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_ThirdPartyOwnership_elec_cost_without_system_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "elec_cost_without_system", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_LCOS_batt_annual_charge_energy_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "batt_annual_charge_energy", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_LCOS_batt_annual_charge_from_system_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "batt_annual_charge_from_system", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_LCOS_batt_annual_discharge_energy_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "batt_annual_discharge_energy", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_LCOS_batt_capacity_percent_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "batt_capacity_percent", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_LCOS_batt_salvage_percentage_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_salvage_percentage", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_LCOS_battery_total_cost_lcos_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "battery_total_cost_lcos", number);
	});
}

SAM_EXPORT void SAM_CashloanHeat_LCOS_charge_w_sys_ec_ym_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "charge_w_sys_ec_ym", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_CashloanHeat_LCOS_grid_to_batt_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "grid_to_batt", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_LCOS_monthly_batt_to_grid_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "monthly_batt_to_grid", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_LCOS_monthly_grid_to_batt_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "monthly_grid_to_batt", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_LCOS_monthly_grid_to_load_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "monthly_grid_to_load", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_LCOS_monthly_system_to_grid_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "monthly_system_to_grid", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_LCOS_true_up_credits_ym_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "true_up_credits_ym", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_CashloanHeat_LCOS_year1_monthly_ec_charge_gross_with_system_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "year1_monthly_ec_charge_gross_with_system", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_LCOS_year1_monthly_ec_charge_with_system_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "year1_monthly_ec_charge_with_system", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_LCOS_year1_monthly_electricity_to_grid_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "year1_monthly_electricity_to_grid", arr, length);
	});
}

SAM_EXPORT void SAM_CashloanHeat_ElectricityRates_rate_escalation_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "rate_escalation", arr, length);
	});
}

SAM_EXPORT double SAM_CashloanHeat_FinancialParameters_analysis_period_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "analysis_period", &result))
		make_access_error("SAM_CashloanHeat", "analysis_period");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_FinancialParameters_debt_fraction_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "debt_fraction", &result))
		make_access_error("SAM_CashloanHeat", "debt_fraction");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_FinancialParameters_federal_tax_rate_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "federal_tax_rate", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "federal_tax_rate");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_FinancialParameters_inflation_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inflation_rate", &result))
		make_access_error("SAM_CashloanHeat", "inflation_rate");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_FinancialParameters_insurance_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "insurance_rate", &result))
		make_access_error("SAM_CashloanHeat", "insurance_rate");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_FinancialParameters_loan_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "loan_rate", &result))
		make_access_error("SAM_CashloanHeat", "loan_rate");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_FinancialParameters_loan_term_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "loan_term", &result))
		make_access_error("SAM_CashloanHeat", "loan_term");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_FinancialParameters_market_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "market", &result))
		make_access_error("SAM_CashloanHeat", "market");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_FinancialParameters_mortgage_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mortgage", &result))
		make_access_error("SAM_CashloanHeat", "mortgage");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_FinancialParameters_prop_tax_assessed_decline_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "prop_tax_assessed_decline", &result))
		make_access_error("SAM_CashloanHeat", "prop_tax_assessed_decline");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_FinancialParameters_prop_tax_cost_assessed_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "prop_tax_cost_assessed_percent", &result))
		make_access_error("SAM_CashloanHeat", "prop_tax_cost_assessed_percent");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_FinancialParameters_property_tax_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "property_tax_rate", &result))
		make_access_error("SAM_CashloanHeat", "property_tax_rate");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_FinancialParameters_real_discount_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "real_discount_rate", &result))
		make_access_error("SAM_CashloanHeat", "real_discount_rate");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_FinancialParameters_salvage_percentage_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "salvage_percentage", &result))
		make_access_error("SAM_CashloanHeat", "salvage_percentage");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_FinancialParameters_state_tax_rate_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "state_tax_rate", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "state_tax_rate");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_FinancialParameters_system_capacity_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_capacity", &result))
		make_access_error("SAM_CashloanHeat", "system_capacity");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_FinancialParameters_system_heat_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_heat_rate", &result))
		make_access_error("SAM_CashloanHeat", "system_heat_rate");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_SystemCosts_add_om_num_types_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "add_om_num_types", &result))
		make_access_error("SAM_CashloanHeat", "add_om_num_types");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_SystemCosts_annual_fuel_usage_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_fuel_usage", &result))
		make_access_error("SAM_CashloanHeat", "annual_fuel_usage");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_SystemCosts_annual_fuel_usage_lifetime_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "annual_fuel_usage_lifetime", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "annual_fuel_usage_lifetime");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_SystemCosts_om_batt_capacity_cost_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "om_batt_capacity_cost", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "om_batt_capacity_cost");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_SystemCosts_om_batt_fixed_cost_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "om_batt_fixed_cost", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "om_batt_fixed_cost");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_SystemCosts_om_batt_nameplate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "om_batt_nameplate", &result))
		make_access_error("SAM_CashloanHeat", "om_batt_nameplate");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_SystemCosts_om_batt_replacement_cost_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "om_batt_replacement_cost", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "om_batt_replacement_cost");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_SystemCosts_om_batt_variable_cost_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "om_batt_variable_cost", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "om_batt_variable_cost");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_SystemCosts_om_capacity_escal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "om_capacity_escal", &result))
		make_access_error("SAM_CashloanHeat", "om_capacity_escal");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_SystemCosts_om_capacity_heat_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "om_capacity_heat", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "om_capacity_heat");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_SystemCosts_om_elec_price_for_heat_techs_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "om_elec_price_for_heat_techs", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "om_elec_price_for_heat_techs");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_SystemCosts_om_elec_price_for_heat_techs_escal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "om_elec_price_for_heat_techs_escal", &result))
		make_access_error("SAM_CashloanHeat", "om_elec_price_for_heat_techs_escal");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_SystemCosts_om_fixed_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "om_fixed", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "om_fixed");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_SystemCosts_om_fixed_escal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "om_fixed_escal", &result))
		make_access_error("SAM_CashloanHeat", "om_fixed_escal");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_SystemCosts_om_fuel_cost_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "om_fuel_cost", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "om_fuel_cost");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_SystemCosts_om_fuel_cost_escal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "om_fuel_cost_escal", &result))
		make_access_error("SAM_CashloanHeat", "om_fuel_cost_escal");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_SystemCosts_om_fuelcell_capacity_cost_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "om_fuelcell_capacity_cost", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "om_fuelcell_capacity_cost");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_SystemCosts_om_fuelcell_fixed_cost_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "om_fuelcell_fixed_cost", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "om_fuelcell_fixed_cost");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_SystemCosts_om_fuelcell_nameplate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "om_fuelcell_nameplate", &result))
		make_access_error("SAM_CashloanHeat", "om_fuelcell_nameplate");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_SystemCosts_om_fuelcell_replacement_cost_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "om_fuelcell_replacement_cost", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "om_fuelcell_replacement_cost");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_SystemCosts_om_fuelcell_variable_cost_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "om_fuelcell_variable_cost", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "om_fuelcell_variable_cost");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_SystemCosts_om_opt_fuel_1_cost_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "om_opt_fuel_1_cost", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "om_opt_fuel_1_cost");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_SystemCosts_om_opt_fuel_1_cost_escal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "om_opt_fuel_1_cost_escal", &result))
		make_access_error("SAM_CashloanHeat", "om_opt_fuel_1_cost_escal");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_SystemCosts_om_opt_fuel_1_usage_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "om_opt_fuel_1_usage", &result))
		make_access_error("SAM_CashloanHeat", "om_opt_fuel_1_usage");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_SystemCosts_om_opt_fuel_2_cost_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "om_opt_fuel_2_cost", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "om_opt_fuel_2_cost");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_SystemCosts_om_opt_fuel_2_cost_escal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "om_opt_fuel_2_cost_escal", &result))
		make_access_error("SAM_CashloanHeat", "om_opt_fuel_2_cost_escal");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_SystemCosts_om_opt_fuel_2_usage_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "om_opt_fuel_2_usage", &result))
		make_access_error("SAM_CashloanHeat", "om_opt_fuel_2_usage");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_SystemCosts_om_production1_values_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "om_production1_values", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "om_production1_values");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_SystemCosts_om_production2_values_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "om_production2_values", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "om_production2_values");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_SystemCosts_om_production_escal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "om_production_escal", &result))
		make_access_error("SAM_CashloanHeat", "om_production_escal");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_SystemCosts_om_production_heat_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "om_production_heat", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "om_production_heat");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_SystemCosts_om_replacement_cost_escal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "om_replacement_cost_escal", &result))
		make_access_error("SAM_CashloanHeat", "om_replacement_cost_escal");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_SystemCosts_total_installed_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_installed_cost", &result))
		make_access_error("SAM_CashloanHeat", "total_installed_cost");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_LandLease_land_area_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "land_area", &result))
		make_access_error("SAM_CashloanHeat", "land_area");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_LandLease_om_land_lease_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "om_land_lease", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "om_land_lease");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_LandLease_om_land_lease_escal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "om_land_lease_escal", &result))
		make_access_error("SAM_CashloanHeat", "om_land_lease_escal");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Depreciation_depr_fed_custom_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "depr_fed_custom", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "depr_fed_custom");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_Depreciation_depr_fed_sl_years_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fed_sl_years", &result))
		make_access_error("SAM_CashloanHeat", "depr_fed_sl_years");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_Depreciation_depr_fed_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fed_type", &result))
		make_access_error("SAM_CashloanHeat", "depr_fed_type");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Depreciation_depr_sta_custom_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "depr_sta_custom", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "depr_sta_custom");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_Depreciation_depr_sta_sl_years_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_sta_sl_years", &result))
		make_access_error("SAM_CashloanHeat", "depr_sta_sl_years");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_Depreciation_depr_sta_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_sta_type", &result))
		make_access_error("SAM_CashloanHeat", "depr_sta_type");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_TaxCreditIncentives_itc_fed_amount_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "itc_fed_amount", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "itc_fed_amount");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_TaxCreditIncentives_itc_fed_amount_deprbas_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_fed_amount_deprbas_fed", &result))
		make_access_error("SAM_CashloanHeat", "itc_fed_amount_deprbas_fed");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_TaxCreditIncentives_itc_fed_amount_deprbas_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_fed_amount_deprbas_sta", &result))
		make_access_error("SAM_CashloanHeat", "itc_fed_amount_deprbas_sta");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_TaxCreditIncentives_itc_fed_percent_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "itc_fed_percent", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "itc_fed_percent");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_TaxCreditIncentives_itc_fed_percent_deprbas_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_fed_percent_deprbas_fed", &result))
		make_access_error("SAM_CashloanHeat", "itc_fed_percent_deprbas_fed");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_TaxCreditIncentives_itc_fed_percent_deprbas_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_fed_percent_deprbas_sta", &result))
		make_access_error("SAM_CashloanHeat", "itc_fed_percent_deprbas_sta");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_TaxCreditIncentives_itc_fed_percent_maxvalue_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "itc_fed_percent_maxvalue", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "itc_fed_percent_maxvalue");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_TaxCreditIncentives_itc_sta_amount_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "itc_sta_amount", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "itc_sta_amount");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_TaxCreditIncentives_itc_sta_amount_deprbas_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_sta_amount_deprbas_fed", &result))
		make_access_error("SAM_CashloanHeat", "itc_sta_amount_deprbas_fed");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_TaxCreditIncentives_itc_sta_amount_deprbas_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_sta_amount_deprbas_sta", &result))
		make_access_error("SAM_CashloanHeat", "itc_sta_amount_deprbas_sta");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_TaxCreditIncentives_itc_sta_percent_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "itc_sta_percent", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "itc_sta_percent");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_TaxCreditIncentives_itc_sta_percent_deprbas_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_sta_percent_deprbas_fed", &result))
		make_access_error("SAM_CashloanHeat", "itc_sta_percent_deprbas_fed");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_TaxCreditIncentives_itc_sta_percent_deprbas_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_sta_percent_deprbas_sta", &result))
		make_access_error("SAM_CashloanHeat", "itc_sta_percent_deprbas_sta");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_TaxCreditIncentives_itc_sta_percent_maxvalue_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "itc_sta_percent_maxvalue", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "itc_sta_percent_maxvalue");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_TaxCreditIncentives_ptc_fed_amount_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "ptc_fed_amount", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "ptc_fed_amount");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_TaxCreditIncentives_ptc_fed_amount_heat_btu_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "ptc_fed_amount_heat_btu", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "ptc_fed_amount_heat_btu");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_TaxCreditIncentives_ptc_fed_escal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ptc_fed_escal", &result))
		make_access_error("SAM_CashloanHeat", "ptc_fed_escal");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_TaxCreditIncentives_ptc_fed_term_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ptc_fed_term", &result))
		make_access_error("SAM_CashloanHeat", "ptc_fed_term");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_TaxCreditIncentives_ptc_sta_amount_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "ptc_sta_amount", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "ptc_sta_amount");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_TaxCreditIncentives_ptc_sta_amount_heat_btu_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "ptc_sta_amount_heat_btu", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "ptc_sta_amount_heat_btu");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_TaxCreditIncentives_ptc_sta_escal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ptc_sta_escal", &result))
		make_access_error("SAM_CashloanHeat", "ptc_sta_escal");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_TaxCreditIncentives_ptc_sta_term_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ptc_sta_term", &result))
		make_access_error("SAM_CashloanHeat", "ptc_sta_term");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_cbi_fed_amount_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cbi_fed_amount", &result))
		make_access_error("SAM_CashloanHeat", "cbi_fed_amount");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_cbi_fed_amount_heat_btu_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cbi_fed_amount_heat_btu", &result))
		make_access_error("SAM_CashloanHeat", "cbi_fed_amount_heat_btu");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_cbi_fed_deprbas_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cbi_fed_deprbas_fed", &result))
		make_access_error("SAM_CashloanHeat", "cbi_fed_deprbas_fed");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_cbi_fed_deprbas_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cbi_fed_deprbas_sta", &result))
		make_access_error("SAM_CashloanHeat", "cbi_fed_deprbas_sta");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_cbi_fed_maxvalue_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cbi_fed_maxvalue", &result))
		make_access_error("SAM_CashloanHeat", "cbi_fed_maxvalue");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_cbi_fed_tax_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cbi_fed_tax_fed", &result))
		make_access_error("SAM_CashloanHeat", "cbi_fed_tax_fed");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_cbi_fed_tax_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cbi_fed_tax_sta", &result))
		make_access_error("SAM_CashloanHeat", "cbi_fed_tax_sta");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_cbi_oth_amount_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cbi_oth_amount", &result))
		make_access_error("SAM_CashloanHeat", "cbi_oth_amount");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_cbi_oth_amount_heat_btu_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cbi_oth_amount_heat_btu", &result))
		make_access_error("SAM_CashloanHeat", "cbi_oth_amount_heat_btu");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_cbi_oth_deprbas_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cbi_oth_deprbas_fed", &result))
		make_access_error("SAM_CashloanHeat", "cbi_oth_deprbas_fed");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_cbi_oth_deprbas_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cbi_oth_deprbas_sta", &result))
		make_access_error("SAM_CashloanHeat", "cbi_oth_deprbas_sta");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_cbi_oth_maxvalue_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cbi_oth_maxvalue", &result))
		make_access_error("SAM_CashloanHeat", "cbi_oth_maxvalue");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_cbi_oth_tax_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cbi_oth_tax_fed", &result))
		make_access_error("SAM_CashloanHeat", "cbi_oth_tax_fed");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_cbi_oth_tax_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cbi_oth_tax_sta", &result))
		make_access_error("SAM_CashloanHeat", "cbi_oth_tax_sta");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_cbi_sta_amount_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cbi_sta_amount", &result))
		make_access_error("SAM_CashloanHeat", "cbi_sta_amount");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_cbi_sta_amount_heat_btu_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cbi_sta_amount_heat_btu", &result))
		make_access_error("SAM_CashloanHeat", "cbi_sta_amount_heat_btu");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_cbi_sta_deprbas_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cbi_sta_deprbas_fed", &result))
		make_access_error("SAM_CashloanHeat", "cbi_sta_deprbas_fed");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_cbi_sta_deprbas_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cbi_sta_deprbas_sta", &result))
		make_access_error("SAM_CashloanHeat", "cbi_sta_deprbas_sta");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_cbi_sta_maxvalue_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cbi_sta_maxvalue", &result))
		make_access_error("SAM_CashloanHeat", "cbi_sta_maxvalue");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_cbi_sta_tax_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cbi_sta_tax_fed", &result))
		make_access_error("SAM_CashloanHeat", "cbi_sta_tax_fed");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_cbi_sta_tax_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cbi_sta_tax_sta", &result))
		make_access_error("SAM_CashloanHeat", "cbi_sta_tax_sta");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_cbi_uti_amount_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cbi_uti_amount", &result))
		make_access_error("SAM_CashloanHeat", "cbi_uti_amount");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_cbi_uti_amount_heat_btu_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cbi_uti_amount_heat_btu", &result))
		make_access_error("SAM_CashloanHeat", "cbi_uti_amount_heat_btu");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_cbi_uti_deprbas_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cbi_uti_deprbas_fed", &result))
		make_access_error("SAM_CashloanHeat", "cbi_uti_deprbas_fed");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_cbi_uti_deprbas_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cbi_uti_deprbas_sta", &result))
		make_access_error("SAM_CashloanHeat", "cbi_uti_deprbas_sta");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_cbi_uti_maxvalue_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cbi_uti_maxvalue", &result))
		make_access_error("SAM_CashloanHeat", "cbi_uti_maxvalue");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_cbi_uti_tax_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cbi_uti_tax_fed", &result))
		make_access_error("SAM_CashloanHeat", "cbi_uti_tax_fed");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_cbi_uti_tax_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cbi_uti_tax_sta", &result))
		make_access_error("SAM_CashloanHeat", "cbi_uti_tax_sta");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_ibi_fed_amount_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_fed_amount", &result))
		make_access_error("SAM_CashloanHeat", "ibi_fed_amount");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_ibi_fed_amount_deprbas_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_fed_amount_deprbas_fed", &result))
		make_access_error("SAM_CashloanHeat", "ibi_fed_amount_deprbas_fed");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_ibi_fed_amount_deprbas_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_fed_amount_deprbas_sta", &result))
		make_access_error("SAM_CashloanHeat", "ibi_fed_amount_deprbas_sta");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_ibi_fed_amount_tax_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_fed_amount_tax_fed", &result))
		make_access_error("SAM_CashloanHeat", "ibi_fed_amount_tax_fed");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_ibi_fed_amount_tax_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_fed_amount_tax_sta", &result))
		make_access_error("SAM_CashloanHeat", "ibi_fed_amount_tax_sta");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_ibi_fed_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_fed_percent", &result))
		make_access_error("SAM_CashloanHeat", "ibi_fed_percent");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_ibi_fed_percent_deprbas_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_fed_percent_deprbas_fed", &result))
		make_access_error("SAM_CashloanHeat", "ibi_fed_percent_deprbas_fed");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_ibi_fed_percent_deprbas_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_fed_percent_deprbas_sta", &result))
		make_access_error("SAM_CashloanHeat", "ibi_fed_percent_deprbas_sta");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_ibi_fed_percent_maxvalue_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_fed_percent_maxvalue", &result))
		make_access_error("SAM_CashloanHeat", "ibi_fed_percent_maxvalue");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_ibi_fed_percent_tax_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_fed_percent_tax_fed", &result))
		make_access_error("SAM_CashloanHeat", "ibi_fed_percent_tax_fed");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_ibi_fed_percent_tax_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_fed_percent_tax_sta", &result))
		make_access_error("SAM_CashloanHeat", "ibi_fed_percent_tax_sta");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_ibi_oth_amount_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_oth_amount", &result))
		make_access_error("SAM_CashloanHeat", "ibi_oth_amount");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_ibi_oth_amount_deprbas_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_oth_amount_deprbas_fed", &result))
		make_access_error("SAM_CashloanHeat", "ibi_oth_amount_deprbas_fed");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_ibi_oth_amount_deprbas_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_oth_amount_deprbas_sta", &result))
		make_access_error("SAM_CashloanHeat", "ibi_oth_amount_deprbas_sta");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_ibi_oth_amount_tax_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_oth_amount_tax_fed", &result))
		make_access_error("SAM_CashloanHeat", "ibi_oth_amount_tax_fed");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_ibi_oth_amount_tax_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_oth_amount_tax_sta", &result))
		make_access_error("SAM_CashloanHeat", "ibi_oth_amount_tax_sta");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_ibi_oth_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_oth_percent", &result))
		make_access_error("SAM_CashloanHeat", "ibi_oth_percent");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_ibi_oth_percent_deprbas_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_oth_percent_deprbas_fed", &result))
		make_access_error("SAM_CashloanHeat", "ibi_oth_percent_deprbas_fed");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_ibi_oth_percent_deprbas_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_oth_percent_deprbas_sta", &result))
		make_access_error("SAM_CashloanHeat", "ibi_oth_percent_deprbas_sta");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_ibi_oth_percent_maxvalue_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_oth_percent_maxvalue", &result))
		make_access_error("SAM_CashloanHeat", "ibi_oth_percent_maxvalue");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_ibi_oth_percent_tax_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_oth_percent_tax_fed", &result))
		make_access_error("SAM_CashloanHeat", "ibi_oth_percent_tax_fed");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_ibi_oth_percent_tax_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_oth_percent_tax_sta", &result))
		make_access_error("SAM_CashloanHeat", "ibi_oth_percent_tax_sta");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_ibi_sta_amount_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_sta_amount", &result))
		make_access_error("SAM_CashloanHeat", "ibi_sta_amount");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_ibi_sta_amount_deprbas_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_sta_amount_deprbas_fed", &result))
		make_access_error("SAM_CashloanHeat", "ibi_sta_amount_deprbas_fed");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_ibi_sta_amount_deprbas_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_sta_amount_deprbas_sta", &result))
		make_access_error("SAM_CashloanHeat", "ibi_sta_amount_deprbas_sta");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_ibi_sta_amount_tax_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_sta_amount_tax_fed", &result))
		make_access_error("SAM_CashloanHeat", "ibi_sta_amount_tax_fed");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_ibi_sta_amount_tax_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_sta_amount_tax_sta", &result))
		make_access_error("SAM_CashloanHeat", "ibi_sta_amount_tax_sta");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_ibi_sta_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_sta_percent", &result))
		make_access_error("SAM_CashloanHeat", "ibi_sta_percent");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_ibi_sta_percent_deprbas_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_sta_percent_deprbas_fed", &result))
		make_access_error("SAM_CashloanHeat", "ibi_sta_percent_deprbas_fed");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_ibi_sta_percent_deprbas_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_sta_percent_deprbas_sta", &result))
		make_access_error("SAM_CashloanHeat", "ibi_sta_percent_deprbas_sta");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_ibi_sta_percent_maxvalue_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_sta_percent_maxvalue", &result))
		make_access_error("SAM_CashloanHeat", "ibi_sta_percent_maxvalue");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_ibi_sta_percent_tax_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_sta_percent_tax_fed", &result))
		make_access_error("SAM_CashloanHeat", "ibi_sta_percent_tax_fed");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_ibi_sta_percent_tax_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_sta_percent_tax_sta", &result))
		make_access_error("SAM_CashloanHeat", "ibi_sta_percent_tax_sta");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_ibi_uti_amount_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_uti_amount", &result))
		make_access_error("SAM_CashloanHeat", "ibi_uti_amount");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_ibi_uti_amount_deprbas_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_uti_amount_deprbas_fed", &result))
		make_access_error("SAM_CashloanHeat", "ibi_uti_amount_deprbas_fed");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_ibi_uti_amount_deprbas_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_uti_amount_deprbas_sta", &result))
		make_access_error("SAM_CashloanHeat", "ibi_uti_amount_deprbas_sta");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_ibi_uti_amount_tax_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_uti_amount_tax_fed", &result))
		make_access_error("SAM_CashloanHeat", "ibi_uti_amount_tax_fed");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_ibi_uti_amount_tax_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_uti_amount_tax_sta", &result))
		make_access_error("SAM_CashloanHeat", "ibi_uti_amount_tax_sta");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_ibi_uti_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_uti_percent", &result))
		make_access_error("SAM_CashloanHeat", "ibi_uti_percent");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_ibi_uti_percent_deprbas_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_uti_percent_deprbas_fed", &result))
		make_access_error("SAM_CashloanHeat", "ibi_uti_percent_deprbas_fed");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_ibi_uti_percent_deprbas_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_uti_percent_deprbas_sta", &result))
		make_access_error("SAM_CashloanHeat", "ibi_uti_percent_deprbas_sta");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_ibi_uti_percent_maxvalue_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_uti_percent_maxvalue", &result))
		make_access_error("SAM_CashloanHeat", "ibi_uti_percent_maxvalue");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_ibi_uti_percent_tax_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_uti_percent_tax_fed", &result))
		make_access_error("SAM_CashloanHeat", "ibi_uti_percent_tax_fed");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_ibi_uti_percent_tax_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_uti_percent_tax_sta", &result))
		make_access_error("SAM_CashloanHeat", "ibi_uti_percent_tax_sta");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_PaymentIncentives_pbi_fed_amount_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pbi_fed_amount", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "pbi_fed_amount");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_PaymentIncentives_pbi_fed_amount_heat_btu_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pbi_fed_amount_heat_btu", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "pbi_fed_amount_heat_btu");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_pbi_fed_escal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pbi_fed_escal", &result))
		make_access_error("SAM_CashloanHeat", "pbi_fed_escal");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_pbi_fed_tax_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pbi_fed_tax_fed", &result))
		make_access_error("SAM_CashloanHeat", "pbi_fed_tax_fed");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_pbi_fed_tax_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pbi_fed_tax_sta", &result))
		make_access_error("SAM_CashloanHeat", "pbi_fed_tax_sta");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_pbi_fed_term_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pbi_fed_term", &result))
		make_access_error("SAM_CashloanHeat", "pbi_fed_term");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_PaymentIncentives_pbi_oth_amount_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pbi_oth_amount", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "pbi_oth_amount");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_PaymentIncentives_pbi_oth_amount_heat_btu_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pbi_oth_amount_heat_btu", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "pbi_oth_amount_heat_btu");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_pbi_oth_escal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pbi_oth_escal", &result))
		make_access_error("SAM_CashloanHeat", "pbi_oth_escal");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_pbi_oth_tax_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pbi_oth_tax_fed", &result))
		make_access_error("SAM_CashloanHeat", "pbi_oth_tax_fed");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_pbi_oth_tax_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pbi_oth_tax_sta", &result))
		make_access_error("SAM_CashloanHeat", "pbi_oth_tax_sta");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_pbi_oth_term_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pbi_oth_term", &result))
		make_access_error("SAM_CashloanHeat", "pbi_oth_term");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_PaymentIncentives_pbi_sta_amount_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pbi_sta_amount", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "pbi_sta_amount");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_PaymentIncentives_pbi_sta_amount_heat_btu_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pbi_sta_amount_heat_btu", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "pbi_sta_amount_heat_btu");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_pbi_sta_escal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pbi_sta_escal", &result))
		make_access_error("SAM_CashloanHeat", "pbi_sta_escal");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_pbi_sta_tax_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pbi_sta_tax_fed", &result))
		make_access_error("SAM_CashloanHeat", "pbi_sta_tax_fed");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_pbi_sta_tax_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pbi_sta_tax_sta", &result))
		make_access_error("SAM_CashloanHeat", "pbi_sta_tax_sta");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_pbi_sta_term_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pbi_sta_term", &result))
		make_access_error("SAM_CashloanHeat", "pbi_sta_term");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_PaymentIncentives_pbi_uti_amount_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pbi_uti_amount", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "pbi_uti_amount");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_PaymentIncentives_pbi_uti_amount_heat_btu_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pbi_uti_amount_heat_btu", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "pbi_uti_amount_heat_btu");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_pbi_uti_escal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pbi_uti_escal", &result))
		make_access_error("SAM_CashloanHeat", "pbi_uti_escal");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_pbi_uti_tax_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pbi_uti_tax_fed", &result))
		make_access_error("SAM_CashloanHeat", "pbi_uti_tax_fed");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_pbi_uti_tax_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pbi_uti_tax_sta", &result))
		make_access_error("SAM_CashloanHeat", "pbi_uti_tax_sta");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_PaymentIncentives_pbi_uti_term_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pbi_uti_term", &result))
		make_access_error("SAM_CashloanHeat", "pbi_uti_term");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_BatterySystem_batt_bank_replacement_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_bank_replacement", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "batt_bank_replacement");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_BatterySystem_batt_computed_bank_capacity_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_computed_bank_capacity", &result))
		make_access_error("SAM_CashloanHeat", "batt_computed_bank_capacity");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_BatterySystem_batt_replacement_option_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_replacement_option", &result))
		make_access_error("SAM_CashloanHeat", "batt_replacement_option");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_BatterySystem_batt_replacement_schedule_percent_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_replacement_schedule_percent", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "batt_replacement_schedule_percent");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_BatterySystem_battery_per_kWh_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "battery_per_kWh", &result))
		make_access_error("SAM_CashloanHeat", "battery_per_kWh");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_BatterySystem_en_batt_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "en_batt", &result))
		make_access_error("SAM_CashloanHeat", "en_batt");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_BatterySystem_en_standalone_batt_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "en_standalone_batt", &result))
		make_access_error("SAM_CashloanHeat", "en_standalone_batt");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_BatterySystem_en_wave_batt_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "en_wave_batt", &result))
		make_access_error("SAM_CashloanHeat", "en_wave_batt");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_FuelCell_annual_fuel_usage_lifetime_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "annual_fuel_usage_lifetime", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "annual_fuel_usage_lifetime");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_FuelCell_en_fuelcell_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "en_fuelcell", &result))
		make_access_error("SAM_CashloanHeat", "en_fuelcell");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_FuelCell_fuelcell_annual_energy_discharged_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "fuelcell_annual_energy_discharged", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "fuelcell_annual_energy_discharged");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_FuelCell_fuelcell_computed_bank_capacity_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fuelcell_computed_bank_capacity", &result))
		make_access_error("SAM_CashloanHeat", "fuelcell_computed_bank_capacity");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_FuelCell_fuelcell_per_kWh_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fuelcell_per_kWh", &result))
		make_access_error("SAM_CashloanHeat", "fuelcell_per_kWh");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_FuelCell_fuelcell_replacement_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "fuelcell_replacement", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "fuelcell_replacement");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_FuelCell_fuelcell_replacement_option_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fuelcell_replacement_option", &result))
		make_access_error("SAM_CashloanHeat", "fuelcell_replacement_option");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_FuelCell_fuelcell_replacement_schedule_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "fuelcell_replacement_schedule", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "fuelcell_replacement_schedule");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_ChargesByMonth_charge_w_sys_dc_tou_ym_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "charge_w_sys_dc_tou_ym", nrows, ncols);
	if (!result)
		make_access_error("SAM_CashloanHeat", "charge_w_sys_dc_tou_ym");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_ChargesByMonth_charge_w_sys_ec_ym_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "charge_w_sys_ec_ym", nrows, ncols);
	if (!result)
		make_access_error("SAM_CashloanHeat", "charge_w_sys_ec_ym");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_ChargesByMonth_charge_w_sys_fixed_ym_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "charge_w_sys_fixed_ym", nrows, ncols);
	if (!result)
		make_access_error("SAM_CashloanHeat", "charge_w_sys_fixed_ym");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_ChargesByMonth_net_billing_credits_ym_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "net_billing_credits_ym", nrows, ncols);
	if (!result)
		make_access_error("SAM_CashloanHeat", "net_billing_credits_ym");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_ChargesByMonth_nm_dollars_applied_ym_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "nm_dollars_applied_ym", nrows, ncols);
	if (!result)
		make_access_error("SAM_CashloanHeat", "nm_dollars_applied_ym");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_ChargesByMonth_true_up_credits_ym_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "true_up_credits_ym", nrows, ncols);
	if (!result)
		make_access_error("SAM_CashloanHeat", "true_up_credits_ym");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_ChargesByMonth_utility_bill_w_sys_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "utility_bill_w_sys", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "utility_bill_w_sys");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Battery_batt_capacity_percent_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_capacity_percent", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "batt_capacity_percent");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Battery_monthly_batt_to_grid_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_batt_to_grid", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "monthly_batt_to_grid");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Battery_monthly_grid_to_batt_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_grid_to_batt", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "monthly_grid_to_batt");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Battery_monthly_grid_to_load_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_grid_to_load", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "monthly_grid_to_load");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_TimeSeries_year1_hourly_dc_with_system_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_hourly_dc_with_system", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "year1_hourly_dc_with_system");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_TimeSeries_year1_hourly_e_fromgrid_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_hourly_e_fromgrid", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "year1_hourly_e_fromgrid");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_TimeSeries_year1_hourly_ec_with_system_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_hourly_ec_with_system", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "year1_hourly_ec_with_system");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_SystemOutput_annual_energy_value_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "annual_energy_value", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "annual_energy_value");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_SystemOutput_annual_thermal_value_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "annual_thermal_value", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "annual_thermal_value");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_SystemOutput_degradation_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "degradation", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "degradation");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_SystemOutput_gen_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "gen", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "gen");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_SystemOutput_gen_purchases_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "gen_purchases", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "gen_purchases");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_Lifetime_system_use_lifetime_output_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_use_lifetime_output", &result))
		make_access_error("SAM_CashloanHeat", "system_use_lifetime_output");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_ThirdPartyOwnership_elec_cost_with_system_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "elec_cost_with_system", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "elec_cost_with_system");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_ThirdPartyOwnership_elec_cost_without_system_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "elec_cost_without_system", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "elec_cost_without_system");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_LCOS_batt_annual_charge_energy_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_annual_charge_energy", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "batt_annual_charge_energy");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_LCOS_batt_annual_charge_from_system_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_annual_charge_from_system", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "batt_annual_charge_from_system");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_LCOS_batt_annual_discharge_energy_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_annual_discharge_energy", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "batt_annual_discharge_energy");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_LCOS_batt_capacity_percent_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_capacity_percent", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "batt_capacity_percent");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_LCOS_batt_salvage_percentage_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_salvage_percentage", &result))
		make_access_error("SAM_CashloanHeat", "batt_salvage_percentage");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_LCOS_battery_total_cost_lcos_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "battery_total_cost_lcos", &result))
		make_access_error("SAM_CashloanHeat", "battery_total_cost_lcos");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_LCOS_charge_w_sys_ec_ym_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "charge_w_sys_ec_ym", nrows, ncols);
	if (!result)
		make_access_error("SAM_CashloanHeat", "charge_w_sys_ec_ym");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_LCOS_grid_to_batt_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "grid_to_batt", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "grid_to_batt");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_LCOS_monthly_batt_to_grid_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_batt_to_grid", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "monthly_batt_to_grid");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_LCOS_monthly_grid_to_batt_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_grid_to_batt", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "monthly_grid_to_batt");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_LCOS_monthly_grid_to_load_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_grid_to_load", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "monthly_grid_to_load");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_LCOS_monthly_system_to_grid_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_system_to_grid", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "monthly_system_to_grid");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_LCOS_true_up_credits_ym_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "true_up_credits_ym", nrows, ncols);
	if (!result)
		make_access_error("SAM_CashloanHeat", "true_up_credits_ym");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_LCOS_year1_monthly_ec_charge_gross_with_system_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_monthly_ec_charge_gross_with_system", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "year1_monthly_ec_charge_gross_with_system");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_LCOS_year1_monthly_ec_charge_with_system_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_monthly_ec_charge_with_system", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "year1_monthly_ec_charge_with_system");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_LCOS_year1_monthly_electricity_to_grid_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_monthly_electricity_to_grid", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "year1_monthly_electricity_to_grid");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_ElectricityRates_rate_escalation_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "rate_escalation", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "rate_escalation");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_Outputs_adjusted_installed_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "adjusted_installed_cost", &result))
		make_access_error("SAM_CashloanHeat", "adjusted_installed_cost");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_Outputs_cbi_fedtax_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cbi_fedtax_total", &result))
		make_access_error("SAM_CashloanHeat", "cbi_fedtax_total");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_Outputs_cbi_statax_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cbi_statax_total", &result))
		make_access_error("SAM_CashloanHeat", "cbi_statax_total");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_Outputs_cbi_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cbi_total", &result))
		make_access_error("SAM_CashloanHeat", "cbi_total");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_Outputs_cbi_total_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cbi_total_fed", &result))
		make_access_error("SAM_CashloanHeat", "cbi_total_fed");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_Outputs_cbi_total_oth_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cbi_total_oth", &result))
		make_access_error("SAM_CashloanHeat", "cbi_total_oth");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_Outputs_cbi_total_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cbi_total_sta", &result))
		make_access_error("SAM_CashloanHeat", "cbi_total_sta");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_Outputs_cbi_total_uti_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cbi_total_uti", &result))
		make_access_error("SAM_CashloanHeat", "cbi_total_uti");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_after_tax_cash_flow_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_after_tax_cash_flow", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_after_tax_cash_flow");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_after_tax_net_equity_cost_flow_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_after_tax_net_equity_cost_flow", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_after_tax_net_equity_cost_flow");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_annual_cost_lcos_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_annual_cost_lcos", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_annual_cost_lcos");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_annual_discharge_lcos_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_annual_discharge_lcos", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_annual_discharge_lcos");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_battery_replacement_cost_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_battery_replacement_cost", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_battery_replacement_cost");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_battery_replacement_cost_schedule_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_battery_replacement_cost_schedule", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_battery_replacement_cost_schedule");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_charging_cost_grid_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_charging_cost_grid", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_charging_cost_grid");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_charging_cost_grid_month_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_charging_cost_grid_month", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_charging_cost_grid_month");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_charging_cost_pv_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_charging_cost_pv", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_charging_cost_pv");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_cumulative_payback_with_expenses_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_cumulative_payback_with_expenses", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_cumulative_payback_with_expenses");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_cumulative_payback_without_expenses_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_cumulative_payback_without_expenses", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_cumulative_payback_without_expenses");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_debt_balance_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_debt_balance", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_debt_balance");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_debt_payment_interest_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_debt_payment_interest", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_debt_payment_interest");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_debt_payment_principal_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_debt_payment_principal", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_debt_payment_principal");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_debt_payment_total_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_debt_payment_total", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_debt_payment_total");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_deductible_expenses_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_deductible_expenses", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_deductible_expenses");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_discounted_costs_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_discounted_costs", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_discounted_costs");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_discounted_cumulative_payback_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_discounted_cumulative_payback", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_discounted_cumulative_payback");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_discounted_payback_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_discounted_payback", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_discounted_payback");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_discounted_savings_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_discounted_savings", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_discounted_savings");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_effective_tax_frac_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_effective_tax_frac", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_effective_tax_frac");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_energy_net_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_energy_net", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_energy_net");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_energy_net_heat_btu_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_energy_net_heat_btu", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_energy_net_heat_btu");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_energy_purchases_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_energy_purchases", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_energy_purchases");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_energy_sales_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_energy_sales", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_energy_sales");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_energy_value_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_energy_value", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_energy_value");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_energy_without_battery_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_energy_without_battery", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_energy_without_battery");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_fed_depr_sched_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_fed_depr_sched", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_fed_depr_sched");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_fed_depreciation_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_fed_depreciation", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_fed_depreciation");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_fed_incentive_income_less_deductions_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_fed_incentive_income_less_deductions", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_fed_incentive_income_less_deductions");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_fed_tax_savings_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_fed_tax_savings", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_fed_tax_savings");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_fed_taxable_incentive_income_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_fed_taxable_incentive_income", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_fed_taxable_incentive_income");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_fed_taxable_income_less_deductions_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_fed_taxable_income_less_deductions", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_fed_taxable_income_less_deductions");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_federal_tax_frac_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_federal_tax_frac", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_federal_tax_frac");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_fuelcell_replacement_cost_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_fuelcell_replacement_cost", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_fuelcell_replacement_cost");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_fuelcell_replacement_cost_schedule_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_fuelcell_replacement_cost_schedule", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_fuelcell_replacement_cost_schedule");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_insurance_expense_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_insurance_expense", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_insurance_expense");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_itc_fed_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_itc_fed", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_itc_fed");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_itc_fed_amount_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_itc_fed_amount", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_itc_fed_amount");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_itc_fed_percent_amount_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_itc_fed_percent_amount", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_itc_fed_percent_amount");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_itc_sta_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_itc_sta", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_itc_sta");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_itc_sta_amount_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_itc_sta_amount", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_itc_sta_amount");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_itc_sta_percent_amount_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_itc_sta_percent_amount", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_itc_sta_percent_amount");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_itc_total_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_itc_total", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_itc_total");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_land_lease_expense_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_land_lease_expense", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_land_lease_expense");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_Outputs_cf_length_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cf_length", &result))
		make_access_error("SAM_CashloanHeat", "cf_length");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_net_salvage_value_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_net_salvage_value", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_net_salvage_value");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_nte_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_nte", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_nte");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_om_batt_capacity_expense_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_om_batt_capacity_expense", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_om_batt_capacity_expense");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_om_batt_fixed_expense_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_om_batt_fixed_expense", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_om_batt_fixed_expense");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_om_capacity1_expense_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_om_capacity1_expense", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_om_capacity1_expense");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_om_capacity2_expense_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_om_capacity2_expense", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_om_capacity2_expense");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_om_capacity_expense_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_om_capacity_expense", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_om_capacity_expense");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_om_elec_price_for_heat_techs_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_om_elec_price_for_heat_techs", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_om_elec_price_for_heat_techs");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_om_fixed1_expense_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_om_fixed1_expense", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_om_fixed1_expense");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_om_fixed2_expense_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_om_fixed2_expense", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_om_fixed2_expense");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_om_fixed_expense_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_om_fixed_expense", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_om_fixed_expense");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_om_fuel_expense_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_om_fuel_expense", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_om_fuel_expense");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_om_opt_fuel_1_expense_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_om_opt_fuel_1_expense", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_om_opt_fuel_1_expense");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_om_opt_fuel_2_expense_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_om_opt_fuel_2_expense", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_om_opt_fuel_2_expense");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_om_production1_expense_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_om_production1_expense", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_om_production1_expense");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_om_production2_expense_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_om_production2_expense", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_om_production2_expense");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_om_production_expense_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_om_production_expense", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_om_production_expense");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_operating_expenses_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_operating_expenses", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_operating_expenses");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_parasitic_cost_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_parasitic_cost", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_parasitic_cost");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_payback_with_expenses_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_payback_with_expenses", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_payback_with_expenses");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_payback_without_expenses_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_payback_without_expenses", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_payback_without_expenses");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_pbi_fedtax_total_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_pbi_fedtax_total", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_pbi_fedtax_total");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_pbi_statax_total_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_pbi_statax_total", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_pbi_statax_total");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_pbi_total_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_pbi_total", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_pbi_total");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_pbi_total_fed_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_pbi_total_fed", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_pbi_total_fed");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_pbi_total_oth_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_pbi_total_oth", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_pbi_total_oth");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_pbi_total_sta_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_pbi_total_sta", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_pbi_total_sta");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_pbi_total_uti_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_pbi_total_uti", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_pbi_total_uti");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_property_tax_assessed_value_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_property_tax_assessed_value", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_property_tax_assessed_value");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_property_tax_expense_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_property_tax_expense", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_property_tax_expense");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_ptc_fed_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_ptc_fed", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_ptc_fed");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_ptc_sta_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_ptc_sta", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_ptc_sta");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_salvage_cost_lcos_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_salvage_cost_lcos", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_salvage_cost_lcos");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_sta_and_fed_tax_savings_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_sta_and_fed_tax_savings", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_sta_and_fed_tax_savings");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_sta_depr_sched_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_sta_depr_sched", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_sta_depr_sched");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_sta_depreciation_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_sta_depreciation", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_sta_depreciation");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_sta_incentive_income_less_deductions_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_sta_incentive_income_less_deductions", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_sta_incentive_income_less_deductions");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_sta_tax_savings_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_sta_tax_savings", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_sta_tax_savings");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_sta_taxable_incentive_income_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_sta_taxable_incentive_income", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_sta_taxable_incentive_income");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_sta_taxable_income_less_deductions_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_sta_taxable_income_less_deductions", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_sta_taxable_income_less_deductions");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_state_tax_frac_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_state_tax_frac", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_state_tax_frac");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_thermal_value_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_thermal_value", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_thermal_value");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_util_escal_rate_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_util_escal_rate", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_util_escal_rate");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_utility_bill_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_utility_bill", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_utility_bill");
	});
	return result;
}

SAM_EXPORT double* SAM_CashloanHeat_Outputs_cf_value_added_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_value_added", length);
	if (!result)
		make_access_error("SAM_CashloanHeat", "cf_value_added");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_Outputs_discounted_payback_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "discounted_payback", &result))
		make_access_error("SAM_CashloanHeat", "discounted_payback");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_Outputs_effective_tax_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "effective_tax_rate", &result))
		make_access_error("SAM_CashloanHeat", "effective_tax_rate");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_Outputs_first_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "first_cost", &result))
		make_access_error("SAM_CashloanHeat", "first_cost");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_Outputs_ibi_fedtax_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_fedtax_total", &result))
		make_access_error("SAM_CashloanHeat", "ibi_fedtax_total");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_Outputs_ibi_statax_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_statax_total", &result))
		make_access_error("SAM_CashloanHeat", "ibi_statax_total");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_Outputs_ibi_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_total", &result))
		make_access_error("SAM_CashloanHeat", "ibi_total");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_Outputs_ibi_total_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_total_fed", &result))
		make_access_error("SAM_CashloanHeat", "ibi_total_fed");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_Outputs_ibi_total_oth_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_total_oth", &result))
		make_access_error("SAM_CashloanHeat", "ibi_total_oth");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_Outputs_ibi_total_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_total_sta", &result))
		make_access_error("SAM_CashloanHeat", "ibi_total_sta");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_Outputs_ibi_total_uti_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_total_uti", &result))
		make_access_error("SAM_CashloanHeat", "ibi_total_uti");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_Outputs_itc_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_total", &result))
		make_access_error("SAM_CashloanHeat", "itc_total");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_Outputs_itc_total_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_total_fed", &result))
		make_access_error("SAM_CashloanHeat", "itc_total_fed");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_Outputs_itc_total_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_total_sta", &result))
		make_access_error("SAM_CashloanHeat", "itc_total_sta");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_Outputs_lcoe_nom_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "lcoe_nom", &result))
		make_access_error("SAM_CashloanHeat", "lcoe_nom");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_Outputs_lcoe_real_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "lcoe_real", &result))
		make_access_error("SAM_CashloanHeat", "lcoe_real");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_Outputs_lcoptc_fed_nom_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "lcoptc_fed_nom", &result))
		make_access_error("SAM_CashloanHeat", "lcoptc_fed_nom");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_Outputs_lcoptc_fed_real_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "lcoptc_fed_real", &result))
		make_access_error("SAM_CashloanHeat", "lcoptc_fed_real");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_Outputs_lcoptc_sta_nom_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "lcoptc_sta_nom", &result))
		make_access_error("SAM_CashloanHeat", "lcoptc_sta_nom");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_Outputs_lcoptc_sta_real_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "lcoptc_sta_real", &result))
		make_access_error("SAM_CashloanHeat", "lcoptc_sta_real");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_Outputs_lcos_nom_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "lcos_nom", &result))
		make_access_error("SAM_CashloanHeat", "lcos_nom");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_Outputs_lcos_real_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "lcos_real", &result))
		make_access_error("SAM_CashloanHeat", "lcos_real");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_Outputs_lnte_nom_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "lnte_nom", &result))
		make_access_error("SAM_CashloanHeat", "lnte_nom");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_Outputs_lnte_real_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "lnte_real", &result))
		make_access_error("SAM_CashloanHeat", "lnte_real");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_Outputs_loan_amount_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "loan_amount", &result))
		make_access_error("SAM_CashloanHeat", "loan_amount");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_Outputs_npv_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "npv", &result))
		make_access_error("SAM_CashloanHeat", "npv");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_Outputs_npv_annual_costs_lcos_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "npv_annual_costs_lcos", &result))
		make_access_error("SAM_CashloanHeat", "npv_annual_costs_lcos");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_Outputs_npv_energy_lcos_nom_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "npv_energy_lcos_nom", &result))
		make_access_error("SAM_CashloanHeat", "npv_energy_lcos_nom");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_Outputs_npv_energy_lcos_real_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "npv_energy_lcos_real", &result))
		make_access_error("SAM_CashloanHeat", "npv_energy_lcos_real");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_Outputs_payback_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "payback", &result))
		make_access_error("SAM_CashloanHeat", "payback");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_Outputs_present_value_fuel_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "present_value_fuel", &result))
		make_access_error("SAM_CashloanHeat", "present_value_fuel");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_Outputs_present_value_insandproptax_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "present_value_insandproptax", &result))
		make_access_error("SAM_CashloanHeat", "present_value_insandproptax");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_Outputs_present_value_oandm_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "present_value_oandm", &result))
		make_access_error("SAM_CashloanHeat", "present_value_oandm");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_Outputs_present_value_oandm_nonfuel_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "present_value_oandm_nonfuel", &result))
		make_access_error("SAM_CashloanHeat", "present_value_oandm_nonfuel");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_Outputs_total_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_cost", &result))
		make_access_error("SAM_CashloanHeat", "total_cost");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_Outputs_wacc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "wacc", &result))
		make_access_error("SAM_CashloanHeat", "wacc");
	});
	return result;
}

SAM_EXPORT double SAM_CashloanHeat_Outputs_year1_nte_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "year1_nte", &result))
		make_access_error("SAM_CashloanHeat", "year1_nte");
	});
	return result;
}

