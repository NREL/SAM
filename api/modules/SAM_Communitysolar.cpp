#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_Communitysolar.h"

SAM_EXPORT int SAM_Communitysolar_execute(SAM_table data, int verbosity, SAM_error* err){
	return SAM_module_exec("communitysolar", data, verbosity, err);
}

SAM_EXPORT void SAM_Communitysolar_FinancialParameters_analysis_period_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "analysis_period", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_FinancialParameters_construction_financing_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "construction_financing_cost", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_FinancialParameters_cost_debt_closing_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cost_debt_closing", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_FinancialParameters_cost_debt_fee_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cost_debt_fee", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_FinancialParameters_cost_other_financing_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cost_other_financing", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_FinancialParameters_debt_option_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "debt_option", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_FinancialParameters_debt_percent_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "debt_percent", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_FinancialParameters_dscr_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dscr", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_FinancialParameters_dscr_limit_debt_fraction_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dscr_limit_debt_fraction", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_FinancialParameters_dscr_maximum_debt_fraction_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dscr_maximum_debt_fraction", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_FinancialParameters_dscr_reserve_months_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dscr_reserve_months", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_FinancialParameters_equip1_reserve_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "equip1_reserve_cost", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_FinancialParameters_equip1_reserve_freq_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "equip1_reserve_freq", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_FinancialParameters_equip2_reserve_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "equip2_reserve_cost", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_FinancialParameters_equip2_reserve_freq_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "equip2_reserve_freq", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_FinancialParameters_equip3_reserve_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "equip3_reserve_cost", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_FinancialParameters_equip3_reserve_freq_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "equip3_reserve_freq", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_FinancialParameters_equip_reserve_depr_fed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "equip_reserve_depr_fed", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_FinancialParameters_equip_reserve_depr_sta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "equip_reserve_depr_sta", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_FinancialParameters_federal_tax_rate_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "federal_tax_rate", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_FinancialParameters_inflation_rate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inflation_rate", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_FinancialParameters_insurance_rate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "insurance_rate", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_FinancialParameters_loan_moratorium_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "loan_moratorium", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_FinancialParameters_months_receivables_reserve_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "months_receivables_reserve", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_FinancialParameters_months_working_reserve_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "months_working_reserve", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_FinancialParameters_payment_option_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "payment_option", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_FinancialParameters_prop_tax_assessed_decline_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "prop_tax_assessed_decline", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_FinancialParameters_prop_tax_cost_assessed_percent_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "prop_tax_cost_assessed_percent", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_FinancialParameters_property_tax_rate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "property_tax_rate", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_FinancialParameters_real_discount_rate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "real_discount_rate", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_FinancialParameters_reserves_interest_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "reserves_interest", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_FinancialParameters_roe_input_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "roe_input", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_FinancialParameters_salvage_percentage_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "salvage_percentage", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_FinancialParameters_state_tax_rate_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "state_tax_rate", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_FinancialParameters_system_capacity_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "system_capacity", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_FinancialParameters_system_heat_rate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "system_heat_rate", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_FinancialParameters_term_int_rate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "term_int_rate", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_FinancialParameters_term_tenor_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "term_tenor", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_SystemCosts_add_om_num_types_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "add_om_num_types", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_SystemCosts_annual_fuel_usage_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "annual_fuel_usage", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_SystemCosts_annual_fuel_usage_lifetime_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "annual_fuel_usage_lifetime", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_SystemCosts_fuelcell_annual_energy_discharged_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "fuelcell_annual_energy_discharged", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_SystemCosts_om_batt_capacity_cost_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "om_batt_capacity_cost", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_SystemCosts_om_batt_fixed_cost_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "om_batt_fixed_cost", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_SystemCosts_om_batt_nameplate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "om_batt_nameplate", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_SystemCosts_om_batt_replacement_cost_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "om_batt_replacement_cost", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_SystemCosts_om_batt_variable_cost_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "om_batt_variable_cost", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_SystemCosts_om_capacity_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "om_capacity", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_SystemCosts_om_capacity_escal_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "om_capacity_escal", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_SystemCosts_om_fixed_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "om_fixed", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_SystemCosts_om_fixed_escal_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "om_fixed_escal", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_SystemCosts_om_fuel_cost_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "om_fuel_cost", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_SystemCosts_om_fuel_cost_escal_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "om_fuel_cost_escal", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_SystemCosts_om_fuelcell_capacity_cost_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "om_fuelcell_capacity_cost", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_SystemCosts_om_fuelcell_fixed_cost_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "om_fuelcell_fixed_cost", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_SystemCosts_om_fuelcell_nameplate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "om_fuelcell_nameplate", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_SystemCosts_om_fuelcell_replacement_cost_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "om_fuelcell_replacement_cost", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_SystemCosts_om_fuelcell_variable_cost_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "om_fuelcell_variable_cost", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_SystemCosts_om_opt_fuel_1_cost_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "om_opt_fuel_1_cost", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_SystemCosts_om_opt_fuel_1_cost_escal_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "om_opt_fuel_1_cost_escal", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_SystemCosts_om_opt_fuel_1_usage_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "om_opt_fuel_1_usage", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_SystemCosts_om_opt_fuel_2_cost_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "om_opt_fuel_2_cost", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_SystemCosts_om_opt_fuel_2_cost_escal_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "om_opt_fuel_2_cost_escal", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_SystemCosts_om_opt_fuel_2_usage_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "om_opt_fuel_2_usage", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_SystemCosts_om_production_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "om_production", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_SystemCosts_om_production1_values_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "om_production1_values", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_SystemCosts_om_production2_values_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "om_production2_values", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_SystemCosts_om_production_escal_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "om_production_escal", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_SystemCosts_om_replacement_cost_escal_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "om_replacement_cost_escal", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_SystemCosts_system_lifetime_recapitalize_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "system_lifetime_recapitalize", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_SystemCosts_system_recapitalization_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "system_recapitalization_cost", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_SystemCosts_system_recapitalization_escalation_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "system_recapitalization_escalation", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_SystemCosts_system_use_recapitalization_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "system_use_recapitalization", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_SystemCosts_total_installed_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "total_installed_cost", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_LandLease_land_area_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "land_area", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_LandLease_om_land_lease_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "om_land_lease", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_LandLease_om_land_lease_escal_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "om_land_lease_escal", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_TaxCreditIncentives_itc_fed_amount_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "itc_fed_amount", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_TaxCreditIncentives_itc_fed_amount_deprbas_fed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "itc_fed_amount_deprbas_fed", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_TaxCreditIncentives_itc_fed_amount_deprbas_sta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "itc_fed_amount_deprbas_sta", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_TaxCreditIncentives_itc_fed_percent_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "itc_fed_percent", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_TaxCreditIncentives_itc_fed_percent_deprbas_fed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "itc_fed_percent_deprbas_fed", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_TaxCreditIncentives_itc_fed_percent_deprbas_sta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "itc_fed_percent_deprbas_sta", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_TaxCreditIncentives_itc_fed_percent_maxvalue_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "itc_fed_percent_maxvalue", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_TaxCreditIncentives_itc_sta_amount_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "itc_sta_amount", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_TaxCreditIncentives_itc_sta_amount_deprbas_fed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "itc_sta_amount_deprbas_fed", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_TaxCreditIncentives_itc_sta_amount_deprbas_sta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "itc_sta_amount_deprbas_sta", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_TaxCreditIncentives_itc_sta_percent_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "itc_sta_percent", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_TaxCreditIncentives_itc_sta_percent_deprbas_fed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "itc_sta_percent_deprbas_fed", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_TaxCreditIncentives_itc_sta_percent_deprbas_sta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "itc_sta_percent_deprbas_sta", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_TaxCreditIncentives_itc_sta_percent_maxvalue_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "itc_sta_percent_maxvalue", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_TaxCreditIncentives_ptc_fed_amount_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "ptc_fed_amount", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_TaxCreditIncentives_ptc_fed_escal_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ptc_fed_escal", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_TaxCreditIncentives_ptc_fed_term_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ptc_fed_term", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_TaxCreditIncentives_ptc_sta_amount_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "ptc_sta_amount", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_TaxCreditIncentives_ptc_sta_escal_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ptc_sta_escal", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_TaxCreditIncentives_ptc_sta_term_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ptc_sta_term", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_Depreciation_depr_alloc_custom_percent_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "depr_alloc_custom_percent", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_Depreciation_depr_alloc_macrs_15_percent_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "depr_alloc_macrs_15_percent", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_Depreciation_depr_alloc_macrs_5_percent_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "depr_alloc_macrs_5_percent", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_Depreciation_depr_alloc_sl_15_percent_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "depr_alloc_sl_15_percent", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_Depreciation_depr_alloc_sl_20_percent_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "depr_alloc_sl_20_percent", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_Depreciation_depr_alloc_sl_39_percent_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "depr_alloc_sl_39_percent", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_Depreciation_depr_alloc_sl_5_percent_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "depr_alloc_sl_5_percent", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_Depreciation_depr_bonus_fed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "depr_bonus_fed", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_Depreciation_depr_bonus_fed_custom_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "depr_bonus_fed_custom", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_Depreciation_depr_bonus_fed_macrs_15_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "depr_bonus_fed_macrs_15", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_Depreciation_depr_bonus_fed_macrs_5_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "depr_bonus_fed_macrs_5", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_Depreciation_depr_bonus_fed_sl_15_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "depr_bonus_fed_sl_15", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_Depreciation_depr_bonus_fed_sl_20_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "depr_bonus_fed_sl_20", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_Depreciation_depr_bonus_fed_sl_39_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "depr_bonus_fed_sl_39", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_Depreciation_depr_bonus_fed_sl_5_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "depr_bonus_fed_sl_5", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_Depreciation_depr_bonus_sta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "depr_bonus_sta", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_Depreciation_depr_bonus_sta_custom_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "depr_bonus_sta_custom", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_Depreciation_depr_bonus_sta_macrs_15_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "depr_bonus_sta_macrs_15", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_Depreciation_depr_bonus_sta_macrs_5_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "depr_bonus_sta_macrs_5", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_Depreciation_depr_bonus_sta_sl_15_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "depr_bonus_sta_sl_15", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_Depreciation_depr_bonus_sta_sl_20_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "depr_bonus_sta_sl_20", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_Depreciation_depr_bonus_sta_sl_39_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "depr_bonus_sta_sl_39", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_Depreciation_depr_bonus_sta_sl_5_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "depr_bonus_sta_sl_5", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_Depreciation_depr_custom_schedule_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "depr_custom_schedule", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_Depreciation_depr_fedbas_method_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "depr_fedbas_method", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_Depreciation_depr_itc_fed_custom_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "depr_itc_fed_custom", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_Depreciation_depr_itc_fed_macrs_15_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "depr_itc_fed_macrs_15", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_Depreciation_depr_itc_fed_macrs_5_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "depr_itc_fed_macrs_5", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_Depreciation_depr_itc_fed_sl_15_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "depr_itc_fed_sl_15", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_Depreciation_depr_itc_fed_sl_20_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "depr_itc_fed_sl_20", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_Depreciation_depr_itc_fed_sl_39_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "depr_itc_fed_sl_39", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_Depreciation_depr_itc_fed_sl_5_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "depr_itc_fed_sl_5", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_Depreciation_depr_itc_sta_custom_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "depr_itc_sta_custom", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_Depreciation_depr_itc_sta_macrs_15_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "depr_itc_sta_macrs_15", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_Depreciation_depr_itc_sta_macrs_5_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "depr_itc_sta_macrs_5", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_Depreciation_depr_itc_sta_sl_15_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "depr_itc_sta_sl_15", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_Depreciation_depr_itc_sta_sl_20_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "depr_itc_sta_sl_20", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_Depreciation_depr_itc_sta_sl_39_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "depr_itc_sta_sl_39", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_Depreciation_depr_itc_sta_sl_5_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "depr_itc_sta_sl_5", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_Depreciation_depr_stabas_method_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "depr_stabas_method", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_cbi_fed_amount_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cbi_fed_amount", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_cbi_fed_deprbas_fed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cbi_fed_deprbas_fed", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_cbi_fed_deprbas_sta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cbi_fed_deprbas_sta", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_cbi_fed_maxvalue_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cbi_fed_maxvalue", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_cbi_fed_tax_fed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cbi_fed_tax_fed", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_cbi_fed_tax_sta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cbi_fed_tax_sta", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_cbi_oth_amount_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cbi_oth_amount", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_cbi_oth_deprbas_fed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cbi_oth_deprbas_fed", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_cbi_oth_deprbas_sta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cbi_oth_deprbas_sta", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_cbi_oth_maxvalue_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cbi_oth_maxvalue", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_cbi_oth_tax_fed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cbi_oth_tax_fed", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_cbi_oth_tax_sta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cbi_oth_tax_sta", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_cbi_sta_amount_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cbi_sta_amount", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_cbi_sta_deprbas_fed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cbi_sta_deprbas_fed", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_cbi_sta_deprbas_sta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cbi_sta_deprbas_sta", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_cbi_sta_maxvalue_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cbi_sta_maxvalue", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_cbi_sta_tax_fed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cbi_sta_tax_fed", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_cbi_sta_tax_sta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cbi_sta_tax_sta", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_cbi_uti_amount_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cbi_uti_amount", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_cbi_uti_deprbas_fed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cbi_uti_deprbas_fed", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_cbi_uti_deprbas_sta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cbi_uti_deprbas_sta", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_cbi_uti_maxvalue_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cbi_uti_maxvalue", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_cbi_uti_tax_fed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cbi_uti_tax_fed", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_cbi_uti_tax_sta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cbi_uti_tax_sta", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_ibi_fed_amount_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_fed_amount", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_ibi_fed_amount_deprbas_fed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_fed_amount_deprbas_fed", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_ibi_fed_amount_deprbas_sta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_fed_amount_deprbas_sta", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_ibi_fed_amount_tax_fed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_fed_amount_tax_fed", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_ibi_fed_amount_tax_sta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_fed_amount_tax_sta", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_ibi_fed_percent_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_fed_percent", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_ibi_fed_percent_deprbas_fed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_fed_percent_deprbas_fed", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_ibi_fed_percent_deprbas_sta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_fed_percent_deprbas_sta", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_ibi_fed_percent_maxvalue_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_fed_percent_maxvalue", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_ibi_fed_percent_tax_fed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_fed_percent_tax_fed", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_ibi_fed_percent_tax_sta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_fed_percent_tax_sta", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_ibi_oth_amount_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_oth_amount", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_ibi_oth_amount_deprbas_fed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_oth_amount_deprbas_fed", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_ibi_oth_amount_deprbas_sta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_oth_amount_deprbas_sta", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_ibi_oth_amount_tax_fed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_oth_amount_tax_fed", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_ibi_oth_amount_tax_sta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_oth_amount_tax_sta", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_ibi_oth_percent_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_oth_percent", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_ibi_oth_percent_deprbas_fed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_oth_percent_deprbas_fed", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_ibi_oth_percent_deprbas_sta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_oth_percent_deprbas_sta", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_ibi_oth_percent_maxvalue_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_oth_percent_maxvalue", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_ibi_oth_percent_tax_fed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_oth_percent_tax_fed", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_ibi_oth_percent_tax_sta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_oth_percent_tax_sta", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_ibi_sta_amount_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_sta_amount", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_ibi_sta_amount_deprbas_fed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_sta_amount_deprbas_fed", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_ibi_sta_amount_deprbas_sta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_sta_amount_deprbas_sta", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_ibi_sta_amount_tax_fed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_sta_amount_tax_fed", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_ibi_sta_amount_tax_sta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_sta_amount_tax_sta", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_ibi_sta_percent_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_sta_percent", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_ibi_sta_percent_deprbas_fed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_sta_percent_deprbas_fed", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_ibi_sta_percent_deprbas_sta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_sta_percent_deprbas_sta", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_ibi_sta_percent_maxvalue_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_sta_percent_maxvalue", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_ibi_sta_percent_tax_fed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_sta_percent_tax_fed", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_ibi_sta_percent_tax_sta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_sta_percent_tax_sta", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_ibi_uti_amount_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_uti_amount", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_ibi_uti_amount_deprbas_fed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_uti_amount_deprbas_fed", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_ibi_uti_amount_deprbas_sta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_uti_amount_deprbas_sta", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_ibi_uti_amount_tax_fed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_uti_amount_tax_fed", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_ibi_uti_amount_tax_sta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_uti_amount_tax_sta", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_ibi_uti_percent_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_uti_percent", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_ibi_uti_percent_deprbas_fed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_uti_percent_deprbas_fed", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_ibi_uti_percent_deprbas_sta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_uti_percent_deprbas_sta", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_ibi_uti_percent_maxvalue_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_uti_percent_maxvalue", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_ibi_uti_percent_tax_fed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_uti_percent_tax_fed", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_ibi_uti_percent_tax_sta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ibi_uti_percent_tax_sta", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_pbi_fed_amount_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "pbi_fed_amount", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_pbi_fed_escal_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pbi_fed_escal", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_pbi_fed_for_ds_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pbi_fed_for_ds", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_pbi_fed_tax_fed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pbi_fed_tax_fed", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_pbi_fed_tax_sta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pbi_fed_tax_sta", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_pbi_fed_term_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pbi_fed_term", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_pbi_oth_amount_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "pbi_oth_amount", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_pbi_oth_escal_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pbi_oth_escal", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_pbi_oth_for_ds_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pbi_oth_for_ds", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_pbi_oth_tax_fed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pbi_oth_tax_fed", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_pbi_oth_tax_sta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pbi_oth_tax_sta", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_pbi_oth_term_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pbi_oth_term", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_pbi_sta_amount_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "pbi_sta_amount", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_pbi_sta_escal_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pbi_sta_escal", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_pbi_sta_for_ds_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pbi_sta_for_ds", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_pbi_sta_tax_fed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pbi_sta_tax_fed", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_pbi_sta_tax_sta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pbi_sta_tax_sta", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_pbi_sta_term_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pbi_sta_term", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_pbi_uti_amount_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "pbi_uti_amount", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_pbi_uti_escal_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pbi_uti_escal", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_pbi_uti_for_ds_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pbi_uti_for_ds", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_pbi_uti_tax_fed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pbi_uti_tax_fed", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_pbi_uti_tax_sta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pbi_uti_tax_sta", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_PaymentIncentives_pbi_uti_term_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pbi_uti_term", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_Revenue_flip_target_percent_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "flip_target_percent", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_Revenue_flip_target_year_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "flip_target_year", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_BatterySystem_batt_bank_replacement_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "batt_bank_replacement", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_BatterySystem_batt_computed_bank_capacity_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_computed_bank_capacity", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_BatterySystem_batt_meter_position_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_meter_position", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_BatterySystem_batt_replacement_option_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_replacement_option", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_BatterySystem_batt_replacement_schedule_percent_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "batt_replacement_schedule_percent", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_BatterySystem_battery_per_kWh_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "battery_per_kWh", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_BatterySystem_en_batt_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "en_batt", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_BatterySystem_en_standalone_batt_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "en_standalone_batt", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_ElectricityRates_en_electricity_rates_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "en_electricity_rates", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_SystemOutput_annual_energy_pre_curtailment_ac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "annual_energy_pre_curtailment_ac", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_SystemOutput_degradation_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "degradation", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_SystemOutput_gen_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "gen", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_SystemOutput_gen_purchases_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "gen_purchases", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_SystemOutput_gen_without_battery_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "gen_without_battery", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_SystemOutput_system_capacity_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "system_capacity", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_SystemOutput_system_pre_curtailment_kwac_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "system_pre_curtailment_kwac", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_UtilityBill_utility_bill_w_sys_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "utility_bill_w_sys", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_Lifetime_system_use_lifetime_output_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "system_use_lifetime_output", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_CommunitySolar_cs_cost_recurring_capacity_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "cs_cost_recurring_capacity", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_CommunitySolar_cs_cost_recurring_capacity_escal_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cs_cost_recurring_capacity_escal", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_CommunitySolar_cs_cost_recurring_fixed_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "cs_cost_recurring_fixed", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_CommunitySolar_cs_cost_recurring_fixed_escal_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cs_cost_recurring_fixed_escal", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_CommunitySolar_cs_cost_recurring_generation_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "cs_cost_recurring_generation", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_CommunitySolar_cs_cost_recurring_generation_escal_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cs_cost_recurring_generation_escal", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_CommunitySolar_cs_cost_upfront_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cs_cost_upfront", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_CommunitySolar_cs_cost_upfront_per_capacity_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cs_cost_upfront_per_capacity", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_CommunitySolar_subscriber1_bill_credit_rate_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "subscriber1_bill_credit_rate", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_CommunitySolar_subscriber1_bill_credit_rate_escal_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subscriber1_bill_credit_rate_escal", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_CommunitySolar_subscriber1_growth_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subscriber1_growth", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_CommunitySolar_subscriber1_payment_annual_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "subscriber1_payment_annual", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_CommunitySolar_subscriber1_payment_annual_escal_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subscriber1_payment_annual_escal", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_CommunitySolar_subscriber1_payment_generation_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "subscriber1_payment_generation", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_CommunitySolar_subscriber1_payment_generation_escal_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subscriber1_payment_generation_escal", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_CommunitySolar_subscriber1_payment_upfront_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subscriber1_payment_upfront", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_CommunitySolar_subscriber1_share_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "subscriber1_share", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_CommunitySolar_subscriber2_bill_credit_rate_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "subscriber2_bill_credit_rate", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_CommunitySolar_subscriber2_bill_credit_rate_escal_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subscriber2_bill_credit_rate_escal", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_CommunitySolar_subscriber2_growth_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subscriber2_growth", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_CommunitySolar_subscriber2_payment_annual_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "subscriber2_payment_annual", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_CommunitySolar_subscriber2_payment_annual_escal_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subscriber2_payment_annual_escal", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_CommunitySolar_subscriber2_payment_generation_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "subscriber2_payment_generation", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_CommunitySolar_subscriber2_payment_generation_escal_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subscriber2_payment_generation_escal", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_CommunitySolar_subscriber2_payment_upfront_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subscriber2_payment_upfront", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_CommunitySolar_subscriber2_share_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "subscriber2_share", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_CommunitySolar_subscriber3_bill_credit_rate_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "subscriber3_bill_credit_rate", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_CommunitySolar_subscriber3_bill_credit_rate_escal_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subscriber3_bill_credit_rate_escal", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_CommunitySolar_subscriber3_growth_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subscriber3_growth", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_CommunitySolar_subscriber3_payment_annual_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "subscriber3_payment_annual", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_CommunitySolar_subscriber3_payment_annual_escal_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subscriber3_payment_annual_escal", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_CommunitySolar_subscriber3_payment_generation_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "subscriber3_payment_generation", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_CommunitySolar_subscriber3_payment_generation_escal_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subscriber3_payment_generation_escal", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_CommunitySolar_subscriber3_payment_upfront_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subscriber3_payment_upfront", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_CommunitySolar_subscriber3_share_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "subscriber3_share", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_CommunitySolar_subscriber4_bill_credit_rate_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "subscriber4_bill_credit_rate", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_CommunitySolar_subscriber4_bill_credit_rate_escal_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subscriber4_bill_credit_rate_escal", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_CommunitySolar_subscriber4_growth_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subscriber4_growth", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_CommunitySolar_subscriber4_payment_annual_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "subscriber4_payment_annual", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_CommunitySolar_subscriber4_payment_annual_escal_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subscriber4_payment_annual_escal", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_CommunitySolar_subscriber4_payment_generation_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "subscriber4_payment_generation", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_CommunitySolar_subscriber4_payment_generation_escal_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subscriber4_payment_generation_escal", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_CommunitySolar_subscriber4_payment_upfront_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subscriber4_payment_upfront", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_CommunitySolar_subscriber4_share_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "subscriber4_share", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_CommunitySolar_unsubscribed_payment_generation_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "unsubscribed_payment_generation", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_CommunitySolar_unsubscribed_payment_generation_escal_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "unsubscribed_payment_generation_escal", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_FuelCell_en_fuelcell_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "en_fuelcell", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_FuelCell_fuelcell_computed_bank_capacity_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fuelcell_computed_bank_capacity", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_FuelCell_fuelcell_per_kWh_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fuelcell_per_kWh", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_FuelCell_fuelcell_replacement_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "fuelcell_replacement", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_FuelCell_fuelcell_replacement_option_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fuelcell_replacement_option", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_FuelCell_fuelcell_replacement_schedule_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "fuelcell_replacement_schedule", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_GridLimits_grid_curtailment_price_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "grid_curtailment_price", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_GridLimits_grid_curtailment_price_esc_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "grid_curtailment_price_esc", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_LCOS_batt_annual_charge_energy_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "batt_annual_charge_energy", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_LCOS_batt_annual_charge_from_system_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "batt_annual_charge_from_system", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_LCOS_batt_annual_discharge_energy_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "batt_annual_discharge_energy", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_LCOS_batt_capacity_percent_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "batt_capacity_percent", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_LCOS_batt_salvage_percentage_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "batt_salvage_percentage", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_LCOS_battery_total_cost_lcos_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "battery_total_cost_lcos", number);
	});
}

SAM_EXPORT void SAM_Communitysolar_LCOS_charge_w_sys_ec_ym_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "charge_w_sys_ec_ym", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Communitysolar_LCOS_grid_to_batt_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "grid_to_batt", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_LCOS_monthly_batt_to_grid_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "monthly_batt_to_grid", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_LCOS_monthly_grid_to_batt_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "monthly_grid_to_batt", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_LCOS_monthly_grid_to_load_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "monthly_grid_to_load", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_LCOS_monthly_system_to_grid_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "monthly_system_to_grid", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_LCOS_true_up_credits_ym_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "true_up_credits_ym", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Communitysolar_LCOS_year1_monthly_ec_charge_gross_with_system_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "year1_monthly_ec_charge_gross_with_system", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_LCOS_year1_monthly_ec_charge_with_system_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "year1_monthly_ec_charge_with_system", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_LCOS_year1_monthly_electricity_to_grid_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "year1_monthly_electricity_to_grid", arr, length);
	});
}

SAM_EXPORT void SAM_Communitysolar_ChargesByMonth_net_billing_credits_ym_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "net_billing_credits_ym", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Communitysolar_ChargesByMonth_nm_dollars_applied_ym_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "nm_dollars_applied_ym", mat, nrows, ncols);
	});
}

SAM_EXPORT double SAM_Communitysolar_FinancialParameters_analysis_period_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "analysis_period", &result))
		make_access_error("SAM_Communitysolar", "analysis_period");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_FinancialParameters_construction_financing_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "construction_financing_cost", &result))
		make_access_error("SAM_Communitysolar", "construction_financing_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_FinancialParameters_cost_debt_closing_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cost_debt_closing", &result))
		make_access_error("SAM_Communitysolar", "cost_debt_closing");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_FinancialParameters_cost_debt_fee_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cost_debt_fee", &result))
		make_access_error("SAM_Communitysolar", "cost_debt_fee");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_FinancialParameters_cost_other_financing_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cost_other_financing", &result))
		make_access_error("SAM_Communitysolar", "cost_other_financing");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_FinancialParameters_debt_option_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "debt_option", &result))
		make_access_error("SAM_Communitysolar", "debt_option");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_FinancialParameters_debt_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "debt_percent", &result))
		make_access_error("SAM_Communitysolar", "debt_percent");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_FinancialParameters_dscr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dscr", &result))
		make_access_error("SAM_Communitysolar", "dscr");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_FinancialParameters_dscr_limit_debt_fraction_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dscr_limit_debt_fraction", &result))
		make_access_error("SAM_Communitysolar", "dscr_limit_debt_fraction");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_FinancialParameters_dscr_maximum_debt_fraction_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dscr_maximum_debt_fraction", &result))
		make_access_error("SAM_Communitysolar", "dscr_maximum_debt_fraction");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_FinancialParameters_dscr_reserve_months_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dscr_reserve_months", &result))
		make_access_error("SAM_Communitysolar", "dscr_reserve_months");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_FinancialParameters_equip1_reserve_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "equip1_reserve_cost", &result))
		make_access_error("SAM_Communitysolar", "equip1_reserve_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_FinancialParameters_equip1_reserve_freq_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "equip1_reserve_freq", &result))
		make_access_error("SAM_Communitysolar", "equip1_reserve_freq");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_FinancialParameters_equip2_reserve_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "equip2_reserve_cost", &result))
		make_access_error("SAM_Communitysolar", "equip2_reserve_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_FinancialParameters_equip2_reserve_freq_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "equip2_reserve_freq", &result))
		make_access_error("SAM_Communitysolar", "equip2_reserve_freq");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_FinancialParameters_equip3_reserve_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "equip3_reserve_cost", &result))
		make_access_error("SAM_Communitysolar", "equip3_reserve_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_FinancialParameters_equip3_reserve_freq_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "equip3_reserve_freq", &result))
		make_access_error("SAM_Communitysolar", "equip3_reserve_freq");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_FinancialParameters_equip_reserve_depr_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "equip_reserve_depr_fed", &result))
		make_access_error("SAM_Communitysolar", "equip_reserve_depr_fed");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_FinancialParameters_equip_reserve_depr_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "equip_reserve_depr_sta", &result))
		make_access_error("SAM_Communitysolar", "equip_reserve_depr_sta");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_FinancialParameters_federal_tax_rate_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "federal_tax_rate", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "federal_tax_rate");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_FinancialParameters_inflation_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inflation_rate", &result))
		make_access_error("SAM_Communitysolar", "inflation_rate");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_FinancialParameters_insurance_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "insurance_rate", &result))
		make_access_error("SAM_Communitysolar", "insurance_rate");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_FinancialParameters_loan_moratorium_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "loan_moratorium", &result))
		make_access_error("SAM_Communitysolar", "loan_moratorium");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_FinancialParameters_months_receivables_reserve_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "months_receivables_reserve", &result))
		make_access_error("SAM_Communitysolar", "months_receivables_reserve");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_FinancialParameters_months_working_reserve_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "months_working_reserve", &result))
		make_access_error("SAM_Communitysolar", "months_working_reserve");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_FinancialParameters_payment_option_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "payment_option", &result))
		make_access_error("SAM_Communitysolar", "payment_option");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_FinancialParameters_prop_tax_assessed_decline_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "prop_tax_assessed_decline", &result))
		make_access_error("SAM_Communitysolar", "prop_tax_assessed_decline");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_FinancialParameters_prop_tax_cost_assessed_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "prop_tax_cost_assessed_percent", &result))
		make_access_error("SAM_Communitysolar", "prop_tax_cost_assessed_percent");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_FinancialParameters_property_tax_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "property_tax_rate", &result))
		make_access_error("SAM_Communitysolar", "property_tax_rate");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_FinancialParameters_real_discount_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "real_discount_rate", &result))
		make_access_error("SAM_Communitysolar", "real_discount_rate");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_FinancialParameters_reserves_interest_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "reserves_interest", &result))
		make_access_error("SAM_Communitysolar", "reserves_interest");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_FinancialParameters_roe_input_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "roe_input", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "roe_input");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_FinancialParameters_salvage_percentage_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "salvage_percentage", &result))
		make_access_error("SAM_Communitysolar", "salvage_percentage");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_FinancialParameters_state_tax_rate_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "state_tax_rate", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "state_tax_rate");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_FinancialParameters_system_capacity_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_capacity", &result))
		make_access_error("SAM_Communitysolar", "system_capacity");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_FinancialParameters_system_heat_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_heat_rate", &result))
		make_access_error("SAM_Communitysolar", "system_heat_rate");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_FinancialParameters_term_int_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "term_int_rate", &result))
		make_access_error("SAM_Communitysolar", "term_int_rate");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_FinancialParameters_term_tenor_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "term_tenor", &result))
		make_access_error("SAM_Communitysolar", "term_tenor");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_SystemCosts_add_om_num_types_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "add_om_num_types", &result))
		make_access_error("SAM_Communitysolar", "add_om_num_types");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_SystemCosts_annual_fuel_usage_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_fuel_usage", &result))
		make_access_error("SAM_Communitysolar", "annual_fuel_usage");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_SystemCosts_annual_fuel_usage_lifetime_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "annual_fuel_usage_lifetime", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "annual_fuel_usage_lifetime");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_SystemCosts_fuelcell_annual_energy_discharged_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "fuelcell_annual_energy_discharged", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "fuelcell_annual_energy_discharged");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_SystemCosts_om_batt_capacity_cost_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "om_batt_capacity_cost", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "om_batt_capacity_cost");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_SystemCosts_om_batt_fixed_cost_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "om_batt_fixed_cost", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "om_batt_fixed_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_SystemCosts_om_batt_nameplate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "om_batt_nameplate", &result))
		make_access_error("SAM_Communitysolar", "om_batt_nameplate");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_SystemCosts_om_batt_replacement_cost_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "om_batt_replacement_cost", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "om_batt_replacement_cost");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_SystemCosts_om_batt_variable_cost_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "om_batt_variable_cost", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "om_batt_variable_cost");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_SystemCosts_om_capacity_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "om_capacity", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "om_capacity");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_SystemCosts_om_capacity_escal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "om_capacity_escal", &result))
		make_access_error("SAM_Communitysolar", "om_capacity_escal");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_SystemCosts_om_fixed_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "om_fixed", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "om_fixed");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_SystemCosts_om_fixed_escal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "om_fixed_escal", &result))
		make_access_error("SAM_Communitysolar", "om_fixed_escal");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_SystemCosts_om_fuel_cost_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "om_fuel_cost", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "om_fuel_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_SystemCosts_om_fuel_cost_escal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "om_fuel_cost_escal", &result))
		make_access_error("SAM_Communitysolar", "om_fuel_cost_escal");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_SystemCosts_om_fuelcell_capacity_cost_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "om_fuelcell_capacity_cost", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "om_fuelcell_capacity_cost");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_SystemCosts_om_fuelcell_fixed_cost_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "om_fuelcell_fixed_cost", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "om_fuelcell_fixed_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_SystemCosts_om_fuelcell_nameplate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "om_fuelcell_nameplate", &result))
		make_access_error("SAM_Communitysolar", "om_fuelcell_nameplate");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_SystemCosts_om_fuelcell_replacement_cost_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "om_fuelcell_replacement_cost", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "om_fuelcell_replacement_cost");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_SystemCosts_om_fuelcell_variable_cost_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "om_fuelcell_variable_cost", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "om_fuelcell_variable_cost");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_SystemCosts_om_opt_fuel_1_cost_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "om_opt_fuel_1_cost", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "om_opt_fuel_1_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_SystemCosts_om_opt_fuel_1_cost_escal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "om_opt_fuel_1_cost_escal", &result))
		make_access_error("SAM_Communitysolar", "om_opt_fuel_1_cost_escal");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_SystemCosts_om_opt_fuel_1_usage_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "om_opt_fuel_1_usage", &result))
		make_access_error("SAM_Communitysolar", "om_opt_fuel_1_usage");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_SystemCosts_om_opt_fuel_2_cost_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "om_opt_fuel_2_cost", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "om_opt_fuel_2_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_SystemCosts_om_opt_fuel_2_cost_escal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "om_opt_fuel_2_cost_escal", &result))
		make_access_error("SAM_Communitysolar", "om_opt_fuel_2_cost_escal");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_SystemCosts_om_opt_fuel_2_usage_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "om_opt_fuel_2_usage", &result))
		make_access_error("SAM_Communitysolar", "om_opt_fuel_2_usage");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_SystemCosts_om_production_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "om_production", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "om_production");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_SystemCosts_om_production1_values_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "om_production1_values", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "om_production1_values");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_SystemCosts_om_production2_values_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "om_production2_values", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "om_production2_values");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_SystemCosts_om_production_escal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "om_production_escal", &result))
		make_access_error("SAM_Communitysolar", "om_production_escal");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_SystemCosts_om_replacement_cost_escal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "om_replacement_cost_escal", &result))
		make_access_error("SAM_Communitysolar", "om_replacement_cost_escal");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_SystemCosts_system_lifetime_recapitalize_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "system_lifetime_recapitalize", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "system_lifetime_recapitalize");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_SystemCosts_system_recapitalization_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_recapitalization_cost", &result))
		make_access_error("SAM_Communitysolar", "system_recapitalization_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_SystemCosts_system_recapitalization_escalation_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_recapitalization_escalation", &result))
		make_access_error("SAM_Communitysolar", "system_recapitalization_escalation");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_SystemCosts_system_use_recapitalization_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_use_recapitalization", &result))
		make_access_error("SAM_Communitysolar", "system_use_recapitalization");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_SystemCosts_total_installed_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_installed_cost", &result))
		make_access_error("SAM_Communitysolar", "total_installed_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_LandLease_land_area_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "land_area", &result))
		make_access_error("SAM_Communitysolar", "land_area");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_LandLease_om_land_lease_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "om_land_lease", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "om_land_lease");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_LandLease_om_land_lease_escal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "om_land_lease_escal", &result))
		make_access_error("SAM_Communitysolar", "om_land_lease_escal");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_TaxCreditIncentives_itc_fed_amount_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_fed_amount", &result))
		make_access_error("SAM_Communitysolar", "itc_fed_amount");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_TaxCreditIncentives_itc_fed_amount_deprbas_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_fed_amount_deprbas_fed", &result))
		make_access_error("SAM_Communitysolar", "itc_fed_amount_deprbas_fed");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_TaxCreditIncentives_itc_fed_amount_deprbas_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_fed_amount_deprbas_sta", &result))
		make_access_error("SAM_Communitysolar", "itc_fed_amount_deprbas_sta");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_TaxCreditIncentives_itc_fed_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_fed_percent", &result))
		make_access_error("SAM_Communitysolar", "itc_fed_percent");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_TaxCreditIncentives_itc_fed_percent_deprbas_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_fed_percent_deprbas_fed", &result))
		make_access_error("SAM_Communitysolar", "itc_fed_percent_deprbas_fed");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_TaxCreditIncentives_itc_fed_percent_deprbas_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_fed_percent_deprbas_sta", &result))
		make_access_error("SAM_Communitysolar", "itc_fed_percent_deprbas_sta");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_TaxCreditIncentives_itc_fed_percent_maxvalue_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_fed_percent_maxvalue", &result))
		make_access_error("SAM_Communitysolar", "itc_fed_percent_maxvalue");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_TaxCreditIncentives_itc_sta_amount_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_sta_amount", &result))
		make_access_error("SAM_Communitysolar", "itc_sta_amount");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_TaxCreditIncentives_itc_sta_amount_deprbas_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_sta_amount_deprbas_fed", &result))
		make_access_error("SAM_Communitysolar", "itc_sta_amount_deprbas_fed");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_TaxCreditIncentives_itc_sta_amount_deprbas_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_sta_amount_deprbas_sta", &result))
		make_access_error("SAM_Communitysolar", "itc_sta_amount_deprbas_sta");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_TaxCreditIncentives_itc_sta_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_sta_percent", &result))
		make_access_error("SAM_Communitysolar", "itc_sta_percent");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_TaxCreditIncentives_itc_sta_percent_deprbas_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_sta_percent_deprbas_fed", &result))
		make_access_error("SAM_Communitysolar", "itc_sta_percent_deprbas_fed");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_TaxCreditIncentives_itc_sta_percent_deprbas_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_sta_percent_deprbas_sta", &result))
		make_access_error("SAM_Communitysolar", "itc_sta_percent_deprbas_sta");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_TaxCreditIncentives_itc_sta_percent_maxvalue_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_sta_percent_maxvalue", &result))
		make_access_error("SAM_Communitysolar", "itc_sta_percent_maxvalue");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_TaxCreditIncentives_ptc_fed_amount_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "ptc_fed_amount", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "ptc_fed_amount");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_TaxCreditIncentives_ptc_fed_escal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ptc_fed_escal", &result))
		make_access_error("SAM_Communitysolar", "ptc_fed_escal");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_TaxCreditIncentives_ptc_fed_term_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ptc_fed_term", &result))
		make_access_error("SAM_Communitysolar", "ptc_fed_term");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_TaxCreditIncentives_ptc_sta_amount_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "ptc_sta_amount", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "ptc_sta_amount");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_TaxCreditIncentives_ptc_sta_escal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ptc_sta_escal", &result))
		make_access_error("SAM_Communitysolar", "ptc_sta_escal");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_TaxCreditIncentives_ptc_sta_term_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ptc_sta_term", &result))
		make_access_error("SAM_Communitysolar", "ptc_sta_term");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Depreciation_depr_alloc_custom_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_alloc_custom_percent", &result))
		make_access_error("SAM_Communitysolar", "depr_alloc_custom_percent");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Depreciation_depr_alloc_macrs_15_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_alloc_macrs_15_percent", &result))
		make_access_error("SAM_Communitysolar", "depr_alloc_macrs_15_percent");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Depreciation_depr_alloc_macrs_5_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_alloc_macrs_5_percent", &result))
		make_access_error("SAM_Communitysolar", "depr_alloc_macrs_5_percent");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Depreciation_depr_alloc_sl_15_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_alloc_sl_15_percent", &result))
		make_access_error("SAM_Communitysolar", "depr_alloc_sl_15_percent");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Depreciation_depr_alloc_sl_20_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_alloc_sl_20_percent", &result))
		make_access_error("SAM_Communitysolar", "depr_alloc_sl_20_percent");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Depreciation_depr_alloc_sl_39_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_alloc_sl_39_percent", &result))
		make_access_error("SAM_Communitysolar", "depr_alloc_sl_39_percent");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Depreciation_depr_alloc_sl_5_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_alloc_sl_5_percent", &result))
		make_access_error("SAM_Communitysolar", "depr_alloc_sl_5_percent");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Depreciation_depr_bonus_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_bonus_fed", &result))
		make_access_error("SAM_Communitysolar", "depr_bonus_fed");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Depreciation_depr_bonus_fed_custom_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_bonus_fed_custom", &result))
		make_access_error("SAM_Communitysolar", "depr_bonus_fed_custom");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Depreciation_depr_bonus_fed_macrs_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_bonus_fed_macrs_15", &result))
		make_access_error("SAM_Communitysolar", "depr_bonus_fed_macrs_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Depreciation_depr_bonus_fed_macrs_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_bonus_fed_macrs_5", &result))
		make_access_error("SAM_Communitysolar", "depr_bonus_fed_macrs_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Depreciation_depr_bonus_fed_sl_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_bonus_fed_sl_15", &result))
		make_access_error("SAM_Communitysolar", "depr_bonus_fed_sl_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Depreciation_depr_bonus_fed_sl_20_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_bonus_fed_sl_20", &result))
		make_access_error("SAM_Communitysolar", "depr_bonus_fed_sl_20");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Depreciation_depr_bonus_fed_sl_39_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_bonus_fed_sl_39", &result))
		make_access_error("SAM_Communitysolar", "depr_bonus_fed_sl_39");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Depreciation_depr_bonus_fed_sl_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_bonus_fed_sl_5", &result))
		make_access_error("SAM_Communitysolar", "depr_bonus_fed_sl_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Depreciation_depr_bonus_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_bonus_sta", &result))
		make_access_error("SAM_Communitysolar", "depr_bonus_sta");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Depreciation_depr_bonus_sta_custom_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_bonus_sta_custom", &result))
		make_access_error("SAM_Communitysolar", "depr_bonus_sta_custom");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Depreciation_depr_bonus_sta_macrs_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_bonus_sta_macrs_15", &result))
		make_access_error("SAM_Communitysolar", "depr_bonus_sta_macrs_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Depreciation_depr_bonus_sta_macrs_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_bonus_sta_macrs_5", &result))
		make_access_error("SAM_Communitysolar", "depr_bonus_sta_macrs_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Depreciation_depr_bonus_sta_sl_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_bonus_sta_sl_15", &result))
		make_access_error("SAM_Communitysolar", "depr_bonus_sta_sl_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Depreciation_depr_bonus_sta_sl_20_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_bonus_sta_sl_20", &result))
		make_access_error("SAM_Communitysolar", "depr_bonus_sta_sl_20");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Depreciation_depr_bonus_sta_sl_39_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_bonus_sta_sl_39", &result))
		make_access_error("SAM_Communitysolar", "depr_bonus_sta_sl_39");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Depreciation_depr_bonus_sta_sl_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_bonus_sta_sl_5", &result))
		make_access_error("SAM_Communitysolar", "depr_bonus_sta_sl_5");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Depreciation_depr_custom_schedule_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "depr_custom_schedule", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "depr_custom_schedule");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Depreciation_depr_fedbas_method_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_method", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_method");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Depreciation_depr_itc_fed_custom_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_itc_fed_custom", &result))
		make_access_error("SAM_Communitysolar", "depr_itc_fed_custom");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Depreciation_depr_itc_fed_macrs_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_itc_fed_macrs_15", &result))
		make_access_error("SAM_Communitysolar", "depr_itc_fed_macrs_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Depreciation_depr_itc_fed_macrs_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_itc_fed_macrs_5", &result))
		make_access_error("SAM_Communitysolar", "depr_itc_fed_macrs_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Depreciation_depr_itc_fed_sl_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_itc_fed_sl_15", &result))
		make_access_error("SAM_Communitysolar", "depr_itc_fed_sl_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Depreciation_depr_itc_fed_sl_20_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_itc_fed_sl_20", &result))
		make_access_error("SAM_Communitysolar", "depr_itc_fed_sl_20");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Depreciation_depr_itc_fed_sl_39_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_itc_fed_sl_39", &result))
		make_access_error("SAM_Communitysolar", "depr_itc_fed_sl_39");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Depreciation_depr_itc_fed_sl_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_itc_fed_sl_5", &result))
		make_access_error("SAM_Communitysolar", "depr_itc_fed_sl_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Depreciation_depr_itc_sta_custom_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_itc_sta_custom", &result))
		make_access_error("SAM_Communitysolar", "depr_itc_sta_custom");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Depreciation_depr_itc_sta_macrs_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_itc_sta_macrs_15", &result))
		make_access_error("SAM_Communitysolar", "depr_itc_sta_macrs_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Depreciation_depr_itc_sta_macrs_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_itc_sta_macrs_5", &result))
		make_access_error("SAM_Communitysolar", "depr_itc_sta_macrs_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Depreciation_depr_itc_sta_sl_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_itc_sta_sl_15", &result))
		make_access_error("SAM_Communitysolar", "depr_itc_sta_sl_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Depreciation_depr_itc_sta_sl_20_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_itc_sta_sl_20", &result))
		make_access_error("SAM_Communitysolar", "depr_itc_sta_sl_20");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Depreciation_depr_itc_sta_sl_39_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_itc_sta_sl_39", &result))
		make_access_error("SAM_Communitysolar", "depr_itc_sta_sl_39");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Depreciation_depr_itc_sta_sl_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_itc_sta_sl_5", &result))
		make_access_error("SAM_Communitysolar", "depr_itc_sta_sl_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Depreciation_depr_stabas_method_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_method", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_method");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_cbi_fed_amount_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cbi_fed_amount", &result))
		make_access_error("SAM_Communitysolar", "cbi_fed_amount");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_cbi_fed_deprbas_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cbi_fed_deprbas_fed", &result))
		make_access_error("SAM_Communitysolar", "cbi_fed_deprbas_fed");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_cbi_fed_deprbas_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cbi_fed_deprbas_sta", &result))
		make_access_error("SAM_Communitysolar", "cbi_fed_deprbas_sta");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_cbi_fed_maxvalue_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cbi_fed_maxvalue", &result))
		make_access_error("SAM_Communitysolar", "cbi_fed_maxvalue");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_cbi_fed_tax_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cbi_fed_tax_fed", &result))
		make_access_error("SAM_Communitysolar", "cbi_fed_tax_fed");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_cbi_fed_tax_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cbi_fed_tax_sta", &result))
		make_access_error("SAM_Communitysolar", "cbi_fed_tax_sta");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_cbi_oth_amount_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cbi_oth_amount", &result))
		make_access_error("SAM_Communitysolar", "cbi_oth_amount");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_cbi_oth_deprbas_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cbi_oth_deprbas_fed", &result))
		make_access_error("SAM_Communitysolar", "cbi_oth_deprbas_fed");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_cbi_oth_deprbas_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cbi_oth_deprbas_sta", &result))
		make_access_error("SAM_Communitysolar", "cbi_oth_deprbas_sta");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_cbi_oth_maxvalue_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cbi_oth_maxvalue", &result))
		make_access_error("SAM_Communitysolar", "cbi_oth_maxvalue");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_cbi_oth_tax_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cbi_oth_tax_fed", &result))
		make_access_error("SAM_Communitysolar", "cbi_oth_tax_fed");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_cbi_oth_tax_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cbi_oth_tax_sta", &result))
		make_access_error("SAM_Communitysolar", "cbi_oth_tax_sta");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_cbi_sta_amount_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cbi_sta_amount", &result))
		make_access_error("SAM_Communitysolar", "cbi_sta_amount");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_cbi_sta_deprbas_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cbi_sta_deprbas_fed", &result))
		make_access_error("SAM_Communitysolar", "cbi_sta_deprbas_fed");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_cbi_sta_deprbas_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cbi_sta_deprbas_sta", &result))
		make_access_error("SAM_Communitysolar", "cbi_sta_deprbas_sta");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_cbi_sta_maxvalue_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cbi_sta_maxvalue", &result))
		make_access_error("SAM_Communitysolar", "cbi_sta_maxvalue");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_cbi_sta_tax_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cbi_sta_tax_fed", &result))
		make_access_error("SAM_Communitysolar", "cbi_sta_tax_fed");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_cbi_sta_tax_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cbi_sta_tax_sta", &result))
		make_access_error("SAM_Communitysolar", "cbi_sta_tax_sta");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_cbi_uti_amount_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cbi_uti_amount", &result))
		make_access_error("SAM_Communitysolar", "cbi_uti_amount");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_cbi_uti_deprbas_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cbi_uti_deprbas_fed", &result))
		make_access_error("SAM_Communitysolar", "cbi_uti_deprbas_fed");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_cbi_uti_deprbas_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cbi_uti_deprbas_sta", &result))
		make_access_error("SAM_Communitysolar", "cbi_uti_deprbas_sta");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_cbi_uti_maxvalue_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cbi_uti_maxvalue", &result))
		make_access_error("SAM_Communitysolar", "cbi_uti_maxvalue");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_cbi_uti_tax_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cbi_uti_tax_fed", &result))
		make_access_error("SAM_Communitysolar", "cbi_uti_tax_fed");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_cbi_uti_tax_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cbi_uti_tax_sta", &result))
		make_access_error("SAM_Communitysolar", "cbi_uti_tax_sta");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_ibi_fed_amount_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_fed_amount", &result))
		make_access_error("SAM_Communitysolar", "ibi_fed_amount");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_ibi_fed_amount_deprbas_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_fed_amount_deprbas_fed", &result))
		make_access_error("SAM_Communitysolar", "ibi_fed_amount_deprbas_fed");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_ibi_fed_amount_deprbas_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_fed_amount_deprbas_sta", &result))
		make_access_error("SAM_Communitysolar", "ibi_fed_amount_deprbas_sta");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_ibi_fed_amount_tax_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_fed_amount_tax_fed", &result))
		make_access_error("SAM_Communitysolar", "ibi_fed_amount_tax_fed");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_ibi_fed_amount_tax_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_fed_amount_tax_sta", &result))
		make_access_error("SAM_Communitysolar", "ibi_fed_amount_tax_sta");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_ibi_fed_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_fed_percent", &result))
		make_access_error("SAM_Communitysolar", "ibi_fed_percent");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_ibi_fed_percent_deprbas_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_fed_percent_deprbas_fed", &result))
		make_access_error("SAM_Communitysolar", "ibi_fed_percent_deprbas_fed");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_ibi_fed_percent_deprbas_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_fed_percent_deprbas_sta", &result))
		make_access_error("SAM_Communitysolar", "ibi_fed_percent_deprbas_sta");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_ibi_fed_percent_maxvalue_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_fed_percent_maxvalue", &result))
		make_access_error("SAM_Communitysolar", "ibi_fed_percent_maxvalue");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_ibi_fed_percent_tax_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_fed_percent_tax_fed", &result))
		make_access_error("SAM_Communitysolar", "ibi_fed_percent_tax_fed");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_ibi_fed_percent_tax_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_fed_percent_tax_sta", &result))
		make_access_error("SAM_Communitysolar", "ibi_fed_percent_tax_sta");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_ibi_oth_amount_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_oth_amount", &result))
		make_access_error("SAM_Communitysolar", "ibi_oth_amount");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_ibi_oth_amount_deprbas_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_oth_amount_deprbas_fed", &result))
		make_access_error("SAM_Communitysolar", "ibi_oth_amount_deprbas_fed");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_ibi_oth_amount_deprbas_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_oth_amount_deprbas_sta", &result))
		make_access_error("SAM_Communitysolar", "ibi_oth_amount_deprbas_sta");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_ibi_oth_amount_tax_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_oth_amount_tax_fed", &result))
		make_access_error("SAM_Communitysolar", "ibi_oth_amount_tax_fed");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_ibi_oth_amount_tax_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_oth_amount_tax_sta", &result))
		make_access_error("SAM_Communitysolar", "ibi_oth_amount_tax_sta");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_ibi_oth_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_oth_percent", &result))
		make_access_error("SAM_Communitysolar", "ibi_oth_percent");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_ibi_oth_percent_deprbas_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_oth_percent_deprbas_fed", &result))
		make_access_error("SAM_Communitysolar", "ibi_oth_percent_deprbas_fed");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_ibi_oth_percent_deprbas_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_oth_percent_deprbas_sta", &result))
		make_access_error("SAM_Communitysolar", "ibi_oth_percent_deprbas_sta");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_ibi_oth_percent_maxvalue_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_oth_percent_maxvalue", &result))
		make_access_error("SAM_Communitysolar", "ibi_oth_percent_maxvalue");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_ibi_oth_percent_tax_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_oth_percent_tax_fed", &result))
		make_access_error("SAM_Communitysolar", "ibi_oth_percent_tax_fed");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_ibi_oth_percent_tax_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_oth_percent_tax_sta", &result))
		make_access_error("SAM_Communitysolar", "ibi_oth_percent_tax_sta");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_ibi_sta_amount_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_sta_amount", &result))
		make_access_error("SAM_Communitysolar", "ibi_sta_amount");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_ibi_sta_amount_deprbas_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_sta_amount_deprbas_fed", &result))
		make_access_error("SAM_Communitysolar", "ibi_sta_amount_deprbas_fed");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_ibi_sta_amount_deprbas_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_sta_amount_deprbas_sta", &result))
		make_access_error("SAM_Communitysolar", "ibi_sta_amount_deprbas_sta");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_ibi_sta_amount_tax_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_sta_amount_tax_fed", &result))
		make_access_error("SAM_Communitysolar", "ibi_sta_amount_tax_fed");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_ibi_sta_amount_tax_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_sta_amount_tax_sta", &result))
		make_access_error("SAM_Communitysolar", "ibi_sta_amount_tax_sta");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_ibi_sta_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_sta_percent", &result))
		make_access_error("SAM_Communitysolar", "ibi_sta_percent");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_ibi_sta_percent_deprbas_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_sta_percent_deprbas_fed", &result))
		make_access_error("SAM_Communitysolar", "ibi_sta_percent_deprbas_fed");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_ibi_sta_percent_deprbas_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_sta_percent_deprbas_sta", &result))
		make_access_error("SAM_Communitysolar", "ibi_sta_percent_deprbas_sta");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_ibi_sta_percent_maxvalue_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_sta_percent_maxvalue", &result))
		make_access_error("SAM_Communitysolar", "ibi_sta_percent_maxvalue");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_ibi_sta_percent_tax_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_sta_percent_tax_fed", &result))
		make_access_error("SAM_Communitysolar", "ibi_sta_percent_tax_fed");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_ibi_sta_percent_tax_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_sta_percent_tax_sta", &result))
		make_access_error("SAM_Communitysolar", "ibi_sta_percent_tax_sta");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_ibi_uti_amount_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_uti_amount", &result))
		make_access_error("SAM_Communitysolar", "ibi_uti_amount");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_ibi_uti_amount_deprbas_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_uti_amount_deprbas_fed", &result))
		make_access_error("SAM_Communitysolar", "ibi_uti_amount_deprbas_fed");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_ibi_uti_amount_deprbas_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_uti_amount_deprbas_sta", &result))
		make_access_error("SAM_Communitysolar", "ibi_uti_amount_deprbas_sta");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_ibi_uti_amount_tax_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_uti_amount_tax_fed", &result))
		make_access_error("SAM_Communitysolar", "ibi_uti_amount_tax_fed");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_ibi_uti_amount_tax_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_uti_amount_tax_sta", &result))
		make_access_error("SAM_Communitysolar", "ibi_uti_amount_tax_sta");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_ibi_uti_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_uti_percent", &result))
		make_access_error("SAM_Communitysolar", "ibi_uti_percent");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_ibi_uti_percent_deprbas_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_uti_percent_deprbas_fed", &result))
		make_access_error("SAM_Communitysolar", "ibi_uti_percent_deprbas_fed");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_ibi_uti_percent_deprbas_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_uti_percent_deprbas_sta", &result))
		make_access_error("SAM_Communitysolar", "ibi_uti_percent_deprbas_sta");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_ibi_uti_percent_maxvalue_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_uti_percent_maxvalue", &result))
		make_access_error("SAM_Communitysolar", "ibi_uti_percent_maxvalue");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_ibi_uti_percent_tax_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_uti_percent_tax_fed", &result))
		make_access_error("SAM_Communitysolar", "ibi_uti_percent_tax_fed");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_ibi_uti_percent_tax_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_uti_percent_tax_sta", &result))
		make_access_error("SAM_Communitysolar", "ibi_uti_percent_tax_sta");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_PaymentIncentives_pbi_fed_amount_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pbi_fed_amount", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "pbi_fed_amount");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_pbi_fed_escal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pbi_fed_escal", &result))
		make_access_error("SAM_Communitysolar", "pbi_fed_escal");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_pbi_fed_for_ds_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pbi_fed_for_ds", &result))
		make_access_error("SAM_Communitysolar", "pbi_fed_for_ds");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_pbi_fed_tax_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pbi_fed_tax_fed", &result))
		make_access_error("SAM_Communitysolar", "pbi_fed_tax_fed");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_pbi_fed_tax_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pbi_fed_tax_sta", &result))
		make_access_error("SAM_Communitysolar", "pbi_fed_tax_sta");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_pbi_fed_term_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pbi_fed_term", &result))
		make_access_error("SAM_Communitysolar", "pbi_fed_term");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_PaymentIncentives_pbi_oth_amount_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pbi_oth_amount", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "pbi_oth_amount");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_pbi_oth_escal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pbi_oth_escal", &result))
		make_access_error("SAM_Communitysolar", "pbi_oth_escal");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_pbi_oth_for_ds_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pbi_oth_for_ds", &result))
		make_access_error("SAM_Communitysolar", "pbi_oth_for_ds");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_pbi_oth_tax_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pbi_oth_tax_fed", &result))
		make_access_error("SAM_Communitysolar", "pbi_oth_tax_fed");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_pbi_oth_tax_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pbi_oth_tax_sta", &result))
		make_access_error("SAM_Communitysolar", "pbi_oth_tax_sta");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_pbi_oth_term_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pbi_oth_term", &result))
		make_access_error("SAM_Communitysolar", "pbi_oth_term");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_PaymentIncentives_pbi_sta_amount_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pbi_sta_amount", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "pbi_sta_amount");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_pbi_sta_escal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pbi_sta_escal", &result))
		make_access_error("SAM_Communitysolar", "pbi_sta_escal");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_pbi_sta_for_ds_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pbi_sta_for_ds", &result))
		make_access_error("SAM_Communitysolar", "pbi_sta_for_ds");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_pbi_sta_tax_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pbi_sta_tax_fed", &result))
		make_access_error("SAM_Communitysolar", "pbi_sta_tax_fed");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_pbi_sta_tax_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pbi_sta_tax_sta", &result))
		make_access_error("SAM_Communitysolar", "pbi_sta_tax_sta");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_pbi_sta_term_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pbi_sta_term", &result))
		make_access_error("SAM_Communitysolar", "pbi_sta_term");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_PaymentIncentives_pbi_uti_amount_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pbi_uti_amount", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "pbi_uti_amount");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_pbi_uti_escal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pbi_uti_escal", &result))
		make_access_error("SAM_Communitysolar", "pbi_uti_escal");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_pbi_uti_for_ds_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pbi_uti_for_ds", &result))
		make_access_error("SAM_Communitysolar", "pbi_uti_for_ds");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_pbi_uti_tax_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pbi_uti_tax_fed", &result))
		make_access_error("SAM_Communitysolar", "pbi_uti_tax_fed");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_pbi_uti_tax_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pbi_uti_tax_sta", &result))
		make_access_error("SAM_Communitysolar", "pbi_uti_tax_sta");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_PaymentIncentives_pbi_uti_term_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pbi_uti_term", &result))
		make_access_error("SAM_Communitysolar", "pbi_uti_term");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Revenue_flip_target_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "flip_target_percent", &result))
		make_access_error("SAM_Communitysolar", "flip_target_percent");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Revenue_flip_target_year_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "flip_target_year", &result))
		make_access_error("SAM_Communitysolar", "flip_target_year");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_BatterySystem_batt_bank_replacement_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_bank_replacement", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "batt_bank_replacement");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_BatterySystem_batt_computed_bank_capacity_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_computed_bank_capacity", &result))
		make_access_error("SAM_Communitysolar", "batt_computed_bank_capacity");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_BatterySystem_batt_meter_position_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_meter_position", &result))
		make_access_error("SAM_Communitysolar", "batt_meter_position");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_BatterySystem_batt_replacement_option_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_replacement_option", &result))
		make_access_error("SAM_Communitysolar", "batt_replacement_option");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_BatterySystem_batt_replacement_schedule_percent_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_replacement_schedule_percent", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "batt_replacement_schedule_percent");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_BatterySystem_battery_per_kWh_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "battery_per_kWh", &result))
		make_access_error("SAM_Communitysolar", "battery_per_kWh");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_BatterySystem_en_batt_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "en_batt", &result))
		make_access_error("SAM_Communitysolar", "en_batt");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_BatterySystem_en_standalone_batt_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "en_standalone_batt", &result))
		make_access_error("SAM_Communitysolar", "en_standalone_batt");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_ElectricityRates_en_electricity_rates_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "en_electricity_rates", &result))
		make_access_error("SAM_Communitysolar", "en_electricity_rates");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_SystemOutput_annual_energy_pre_curtailment_ac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_energy_pre_curtailment_ac", &result))
		make_access_error("SAM_Communitysolar", "annual_energy_pre_curtailment_ac");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_SystemOutput_degradation_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "degradation", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "degradation");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_SystemOutput_gen_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "gen", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "gen");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_SystemOutput_gen_purchases_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "gen_purchases", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "gen_purchases");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_SystemOutput_gen_without_battery_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "gen_without_battery", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "gen_without_battery");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_SystemOutput_system_capacity_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_capacity", &result))
		make_access_error("SAM_Communitysolar", "system_capacity");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_SystemOutput_system_pre_curtailment_kwac_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "system_pre_curtailment_kwac", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "system_pre_curtailment_kwac");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_UtilityBill_utility_bill_w_sys_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "utility_bill_w_sys", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "utility_bill_w_sys");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Lifetime_system_use_lifetime_output_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_use_lifetime_output", &result))
		make_access_error("SAM_Communitysolar", "system_use_lifetime_output");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_CommunitySolar_cs_cost_recurring_capacity_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cs_cost_recurring_capacity", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cs_cost_recurring_capacity");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_CommunitySolar_cs_cost_recurring_capacity_escal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cs_cost_recurring_capacity_escal", &result))
		make_access_error("SAM_Communitysolar", "cs_cost_recurring_capacity_escal");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_CommunitySolar_cs_cost_recurring_fixed_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cs_cost_recurring_fixed", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cs_cost_recurring_fixed");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_CommunitySolar_cs_cost_recurring_fixed_escal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cs_cost_recurring_fixed_escal", &result))
		make_access_error("SAM_Communitysolar", "cs_cost_recurring_fixed_escal");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_CommunitySolar_cs_cost_recurring_generation_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cs_cost_recurring_generation", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cs_cost_recurring_generation");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_CommunitySolar_cs_cost_recurring_generation_escal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cs_cost_recurring_generation_escal", &result))
		make_access_error("SAM_Communitysolar", "cs_cost_recurring_generation_escal");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_CommunitySolar_cs_cost_upfront_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cs_cost_upfront", &result))
		make_access_error("SAM_Communitysolar", "cs_cost_upfront");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_CommunitySolar_cs_cost_upfront_per_capacity_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cs_cost_upfront_per_capacity", &result))
		make_access_error("SAM_Communitysolar", "cs_cost_upfront_per_capacity");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_CommunitySolar_subscriber1_bill_credit_rate_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subscriber1_bill_credit_rate", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "subscriber1_bill_credit_rate");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_CommunitySolar_subscriber1_bill_credit_rate_escal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subscriber1_bill_credit_rate_escal", &result))
		make_access_error("SAM_Communitysolar", "subscriber1_bill_credit_rate_escal");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_CommunitySolar_subscriber1_growth_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subscriber1_growth", &result))
		make_access_error("SAM_Communitysolar", "subscriber1_growth");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_CommunitySolar_subscriber1_payment_annual_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subscriber1_payment_annual", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "subscriber1_payment_annual");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_CommunitySolar_subscriber1_payment_annual_escal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subscriber1_payment_annual_escal", &result))
		make_access_error("SAM_Communitysolar", "subscriber1_payment_annual_escal");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_CommunitySolar_subscriber1_payment_generation_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subscriber1_payment_generation", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "subscriber1_payment_generation");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_CommunitySolar_subscriber1_payment_generation_escal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subscriber1_payment_generation_escal", &result))
		make_access_error("SAM_Communitysolar", "subscriber1_payment_generation_escal");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_CommunitySolar_subscriber1_payment_upfront_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subscriber1_payment_upfront", &result))
		make_access_error("SAM_Communitysolar", "subscriber1_payment_upfront");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_CommunitySolar_subscriber1_share_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subscriber1_share", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "subscriber1_share");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_CommunitySolar_subscriber2_bill_credit_rate_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subscriber2_bill_credit_rate", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "subscriber2_bill_credit_rate");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_CommunitySolar_subscriber2_bill_credit_rate_escal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subscriber2_bill_credit_rate_escal", &result))
		make_access_error("SAM_Communitysolar", "subscriber2_bill_credit_rate_escal");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_CommunitySolar_subscriber2_growth_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subscriber2_growth", &result))
		make_access_error("SAM_Communitysolar", "subscriber2_growth");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_CommunitySolar_subscriber2_payment_annual_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subscriber2_payment_annual", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "subscriber2_payment_annual");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_CommunitySolar_subscriber2_payment_annual_escal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subscriber2_payment_annual_escal", &result))
		make_access_error("SAM_Communitysolar", "subscriber2_payment_annual_escal");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_CommunitySolar_subscriber2_payment_generation_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subscriber2_payment_generation", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "subscriber2_payment_generation");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_CommunitySolar_subscriber2_payment_generation_escal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subscriber2_payment_generation_escal", &result))
		make_access_error("SAM_Communitysolar", "subscriber2_payment_generation_escal");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_CommunitySolar_subscriber2_payment_upfront_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subscriber2_payment_upfront", &result))
		make_access_error("SAM_Communitysolar", "subscriber2_payment_upfront");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_CommunitySolar_subscriber2_share_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subscriber2_share", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "subscriber2_share");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_CommunitySolar_subscriber3_bill_credit_rate_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subscriber3_bill_credit_rate", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "subscriber3_bill_credit_rate");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_CommunitySolar_subscriber3_bill_credit_rate_escal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subscriber3_bill_credit_rate_escal", &result))
		make_access_error("SAM_Communitysolar", "subscriber3_bill_credit_rate_escal");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_CommunitySolar_subscriber3_growth_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subscriber3_growth", &result))
		make_access_error("SAM_Communitysolar", "subscriber3_growth");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_CommunitySolar_subscriber3_payment_annual_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subscriber3_payment_annual", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "subscriber3_payment_annual");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_CommunitySolar_subscriber3_payment_annual_escal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subscriber3_payment_annual_escal", &result))
		make_access_error("SAM_Communitysolar", "subscriber3_payment_annual_escal");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_CommunitySolar_subscriber3_payment_generation_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subscriber3_payment_generation", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "subscriber3_payment_generation");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_CommunitySolar_subscriber3_payment_generation_escal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subscriber3_payment_generation_escal", &result))
		make_access_error("SAM_Communitysolar", "subscriber3_payment_generation_escal");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_CommunitySolar_subscriber3_payment_upfront_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subscriber3_payment_upfront", &result))
		make_access_error("SAM_Communitysolar", "subscriber3_payment_upfront");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_CommunitySolar_subscriber3_share_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subscriber3_share", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "subscriber3_share");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_CommunitySolar_subscriber4_bill_credit_rate_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subscriber4_bill_credit_rate", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "subscriber4_bill_credit_rate");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_CommunitySolar_subscriber4_bill_credit_rate_escal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subscriber4_bill_credit_rate_escal", &result))
		make_access_error("SAM_Communitysolar", "subscriber4_bill_credit_rate_escal");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_CommunitySolar_subscriber4_growth_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subscriber4_growth", &result))
		make_access_error("SAM_Communitysolar", "subscriber4_growth");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_CommunitySolar_subscriber4_payment_annual_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subscriber4_payment_annual", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "subscriber4_payment_annual");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_CommunitySolar_subscriber4_payment_annual_escal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subscriber4_payment_annual_escal", &result))
		make_access_error("SAM_Communitysolar", "subscriber4_payment_annual_escal");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_CommunitySolar_subscriber4_payment_generation_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subscriber4_payment_generation", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "subscriber4_payment_generation");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_CommunitySolar_subscriber4_payment_generation_escal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subscriber4_payment_generation_escal", &result))
		make_access_error("SAM_Communitysolar", "subscriber4_payment_generation_escal");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_CommunitySolar_subscriber4_payment_upfront_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subscriber4_payment_upfront", &result))
		make_access_error("SAM_Communitysolar", "subscriber4_payment_upfront");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_CommunitySolar_subscriber4_share_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "subscriber4_share", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "subscriber4_share");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_CommunitySolar_unsubscribed_payment_generation_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "unsubscribed_payment_generation", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "unsubscribed_payment_generation");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_CommunitySolar_unsubscribed_payment_generation_escal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "unsubscribed_payment_generation_escal", &result))
		make_access_error("SAM_Communitysolar", "unsubscribed_payment_generation_escal");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_FuelCell_en_fuelcell_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "en_fuelcell", &result))
		make_access_error("SAM_Communitysolar", "en_fuelcell");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_FuelCell_fuelcell_computed_bank_capacity_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fuelcell_computed_bank_capacity", &result))
		make_access_error("SAM_Communitysolar", "fuelcell_computed_bank_capacity");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_FuelCell_fuelcell_per_kWh_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fuelcell_per_kWh", &result))
		make_access_error("SAM_Communitysolar", "fuelcell_per_kWh");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_FuelCell_fuelcell_replacement_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "fuelcell_replacement", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "fuelcell_replacement");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_FuelCell_fuelcell_replacement_option_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fuelcell_replacement_option", &result))
		make_access_error("SAM_Communitysolar", "fuelcell_replacement_option");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_FuelCell_fuelcell_replacement_schedule_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "fuelcell_replacement_schedule", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "fuelcell_replacement_schedule");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_GridLimits_grid_curtailment_price_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "grid_curtailment_price", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "grid_curtailment_price");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_GridLimits_grid_curtailment_price_esc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "grid_curtailment_price_esc", &result))
		make_access_error("SAM_Communitysolar", "grid_curtailment_price_esc");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_LCOS_batt_annual_charge_energy_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_annual_charge_energy", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "batt_annual_charge_energy");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_LCOS_batt_annual_charge_from_system_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_annual_charge_from_system", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "batt_annual_charge_from_system");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_LCOS_batt_annual_discharge_energy_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_annual_discharge_energy", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "batt_annual_discharge_energy");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_LCOS_batt_capacity_percent_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "batt_capacity_percent", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "batt_capacity_percent");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_LCOS_batt_salvage_percentage_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "batt_salvage_percentage", &result))
		make_access_error("SAM_Communitysolar", "batt_salvage_percentage");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_LCOS_battery_total_cost_lcos_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "battery_total_cost_lcos", &result))
		make_access_error("SAM_Communitysolar", "battery_total_cost_lcos");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_LCOS_charge_w_sys_ec_ym_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "charge_w_sys_ec_ym", nrows, ncols);
	if (!result)
		make_access_error("SAM_Communitysolar", "charge_w_sys_ec_ym");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_LCOS_grid_to_batt_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "grid_to_batt", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "grid_to_batt");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_LCOS_monthly_batt_to_grid_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_batt_to_grid", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "monthly_batt_to_grid");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_LCOS_monthly_grid_to_batt_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_grid_to_batt", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "monthly_grid_to_batt");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_LCOS_monthly_grid_to_load_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_grid_to_load", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "monthly_grid_to_load");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_LCOS_monthly_system_to_grid_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_system_to_grid", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "monthly_system_to_grid");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_LCOS_true_up_credits_ym_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "true_up_credits_ym", nrows, ncols);
	if (!result)
		make_access_error("SAM_Communitysolar", "true_up_credits_ym");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_LCOS_year1_monthly_ec_charge_gross_with_system_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_monthly_ec_charge_gross_with_system", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "year1_monthly_ec_charge_gross_with_system");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_LCOS_year1_monthly_ec_charge_with_system_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_monthly_ec_charge_with_system", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "year1_monthly_ec_charge_with_system");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_LCOS_year1_monthly_electricity_to_grid_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "year1_monthly_electricity_to_grid", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "year1_monthly_electricity_to_grid");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_ChargesByMonth_net_billing_credits_ym_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "net_billing_credits_ym", nrows, ncols);
	if (!result)
		make_access_error("SAM_Communitysolar", "net_billing_credits_ym");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_ChargesByMonth_nm_dollars_applied_ym_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "nm_dollars_applied_ym", nrows, ncols);
	if (!result)
		make_access_error("SAM_Communitysolar", "nm_dollars_applied_ym");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_adjusted_installed_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "adjusted_installed_cost", &result))
		make_access_error("SAM_Communitysolar", "adjusted_installed_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_analysis_period_irr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "analysis_period_irr", &result))
		make_access_error("SAM_Communitysolar", "analysis_period_irr");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_cash_for_debt_service_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cash_for_debt_service", &result))
		make_access_error("SAM_Communitysolar", "cash_for_debt_service");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_cbi_fedtax_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cbi_fedtax_total", &result))
		make_access_error("SAM_Communitysolar", "cbi_fedtax_total");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_cbi_statax_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cbi_statax_total", &result))
		make_access_error("SAM_Communitysolar", "cbi_statax_total");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_cbi_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cbi_total", &result))
		make_access_error("SAM_Communitysolar", "cbi_total");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_cbi_total_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cbi_total_fed", &result))
		make_access_error("SAM_Communitysolar", "cbi_total_fed");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_cbi_total_oth_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cbi_total_oth", &result))
		make_access_error("SAM_Communitysolar", "cbi_total_oth");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_cbi_total_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cbi_total_sta", &result))
		make_access_error("SAM_Communitysolar", "cbi_total_sta");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_cbi_total_uti_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cbi_total_uti", &result))
		make_access_error("SAM_Communitysolar", "cbi_total_uti");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_annual_cost_lcos_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_annual_cost_lcos", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_annual_cost_lcos");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_annual_costs_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_annual_costs", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_annual_costs");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_annual_discharge_lcos_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_annual_discharge_lcos", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_annual_discharge_lcos");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_battery_replacement_cost_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_battery_replacement_cost", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_battery_replacement_cost");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_battery_replacement_cost_schedule_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_battery_replacement_cost_schedule", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_battery_replacement_cost_schedule");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_capacity_payment_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_capacity_payment", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_capacity_payment");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_cash_for_ds_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_cash_for_ds", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_cash_for_ds");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_charging_cost_grid_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_charging_cost_grid", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_charging_cost_grid");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_charging_cost_grid_month_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_charging_cost_grid_month", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_charging_cost_grid_month");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_charging_cost_pv_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_charging_cost_pv", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_charging_cost_pv");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_community_solar_recurring_capacity_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_community_solar_recurring_capacity", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_community_solar_recurring_capacity");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_community_solar_recurring_fixed_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_community_solar_recurring_fixed", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_community_solar_recurring_fixed");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_community_solar_recurring_generation_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_community_solar_recurring_generation", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_community_solar_recurring_generation");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_community_solar_subscriber1_revenue_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_community_solar_subscriber1_revenue", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_community_solar_subscriber1_revenue");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_community_solar_subscriber2_revenue_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_community_solar_subscriber2_revenue", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_community_solar_subscriber2_revenue");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_community_solar_subscriber3_revenue_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_community_solar_subscriber3_revenue", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_community_solar_subscriber3_revenue");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_community_solar_subscriber4_revenue_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_community_solar_subscriber4_revenue", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_community_solar_subscriber4_revenue");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_community_solar_unsubscribed_revenue_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_community_solar_unsubscribed_revenue", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_community_solar_unsubscribed_revenue");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_community_solar_upfront_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_community_solar_upfront", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_community_solar_upfront");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_community_solar_upfront_per_capacity_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_community_solar_upfront_per_capacity", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_community_solar_upfront_per_capacity");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_curtailment_value_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_curtailment_value", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_curtailment_value");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_debt_balance_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_debt_balance", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_debt_balance");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_debt_payment_interest_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_debt_payment_interest", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_debt_payment_interest");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_debt_payment_principal_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_debt_payment_principal", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_debt_payment_principal");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_debt_payment_total_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_debt_payment_total", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_debt_payment_total");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_debt_size_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_debt_size", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_debt_size");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_disbursement_debtservice_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_disbursement_debtservice", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_disbursement_debtservice");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_disbursement_equip1_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_disbursement_equip1", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_disbursement_equip1");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_disbursement_equip2_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_disbursement_equip2", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_disbursement_equip2");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_disbursement_equip3_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_disbursement_equip3", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_disbursement_equip3");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_disbursement_om_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_disbursement_om", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_disbursement_om");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_disbursement_receivables_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_disbursement_receivables", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_disbursement_receivables");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_ebitda_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_ebitda", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_ebitda");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_effective_tax_frac_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_effective_tax_frac", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_effective_tax_frac");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_energy_curtailed_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_energy_curtailed", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_energy_curtailed");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_energy_net_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_energy_net", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_energy_net");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_energy_purchases_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_energy_purchases", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_energy_purchases");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_energy_sales_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_energy_sales", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_energy_sales");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_energy_without_battery_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_energy_without_battery", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_energy_without_battery");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_feddepr_custom_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_feddepr_custom", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_feddepr_custom");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_feddepr_macrs_15_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_feddepr_macrs_15", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_feddepr_macrs_15");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_feddepr_macrs_5_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_feddepr_macrs_5", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_feddepr_macrs_5");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_feddepr_me1_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_feddepr_me1", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_feddepr_me1");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_feddepr_me2_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_feddepr_me2", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_feddepr_me2");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_feddepr_me3_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_feddepr_me3", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_feddepr_me3");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_feddepr_sl_15_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_feddepr_sl_15", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_feddepr_sl_15");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_feddepr_sl_20_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_feddepr_sl_20", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_feddepr_sl_20");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_feddepr_sl_39_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_feddepr_sl_39", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_feddepr_sl_39");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_feddepr_sl_5_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_feddepr_sl_5", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_feddepr_sl_5");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_feddepr_total_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_feddepr_total", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_feddepr_total");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_federal_tax_frac_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_federal_tax_frac", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_federal_tax_frac");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_fedtax_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_fedtax", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_fedtax");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_fedtax_income_prior_incentives_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_fedtax_income_prior_incentives", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_fedtax_income_prior_incentives");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_fedtax_income_with_incentives_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_fedtax_income_with_incentives", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_fedtax_income_with_incentives");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_fedtax_taxable_incentives_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_fedtax_taxable_incentives", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_fedtax_taxable_incentives");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_fuelcell_replacement_cost_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_fuelcell_replacement_cost", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_fuelcell_replacement_cost");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_fuelcell_replacement_cost_schedule_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_fuelcell_replacement_cost_schedule", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_fuelcell_replacement_cost_schedule");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_funding_debtservice_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_funding_debtservice", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_funding_debtservice");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_funding_equip1_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_funding_equip1", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_funding_equip1");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_funding_equip2_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_funding_equip2", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_funding_equip2");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_funding_equip3_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_funding_equip3", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_funding_equip3");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_funding_om_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_funding_om", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_funding_om");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_funding_receivables_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_funding_receivables", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_funding_receivables");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_insurance_expense_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_insurance_expense", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_insurance_expense");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_land_lease_expense_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_land_lease_expense", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_land_lease_expense");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_lcog_costs_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_lcog_costs", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_lcog_costs");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_cf_length_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cf_length", &result))
		make_access_error("SAM_Communitysolar", "cf_length");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_net_salvage_value_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_net_salvage_value", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_net_salvage_value");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_om_batt_capacity_expense_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_om_batt_capacity_expense", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_om_batt_capacity_expense");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_om_batt_fixed_expense_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_om_batt_fixed_expense", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_om_batt_fixed_expense");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_om_batt_production_expense_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_om_batt_production_expense", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_om_batt_production_expense");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_om_capacity1_expense_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_om_capacity1_expense", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_om_capacity1_expense");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_om_capacity2_expense_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_om_capacity2_expense", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_om_capacity2_expense");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_om_capacity_expense_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_om_capacity_expense", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_om_capacity_expense");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_om_fixed1_expense_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_om_fixed1_expense", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_om_fixed1_expense");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_om_fixed2_expense_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_om_fixed2_expense", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_om_fixed2_expense");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_om_fixed_expense_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_om_fixed_expense", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_om_fixed_expense");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_om_fuel_expense_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_om_fuel_expense", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_om_fuel_expense");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_om_opt_fuel_1_expense_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_om_opt_fuel_1_expense", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_om_opt_fuel_1_expense");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_om_opt_fuel_2_expense_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_om_opt_fuel_2_expense", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_om_opt_fuel_2_expense");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_om_production1_expense_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_om_production1_expense", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_om_production1_expense");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_om_production2_expense_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_om_production2_expense", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_om_production2_expense");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_om_production_expense_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_om_production_expense", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_om_production_expense");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_operating_expenses_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_operating_expenses", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_operating_expenses");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_pbi_fedtax_total_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_pbi_fedtax_total", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_pbi_fedtax_total");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_pbi_statax_total_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_pbi_statax_total", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_pbi_statax_total");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_pbi_total_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_pbi_total", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_pbi_total");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_pbi_total_fed_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_pbi_total_fed", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_pbi_total_fed");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_pbi_total_oth_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_pbi_total_oth", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_pbi_total_oth");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_pbi_total_sta_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_pbi_total_sta", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_pbi_total_sta");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_pbi_total_uti_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_pbi_total_uti", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_pbi_total_uti");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_pretax_cashflow_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_pretax_cashflow", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_pretax_cashflow");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_pretax_dscr_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_pretax_dscr", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_pretax_dscr");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_project_dsra_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_project_dsra", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_project_dsra");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_project_financing_activities_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_project_financing_activities", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_project_financing_activities");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_project_investing_activities_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_project_investing_activities", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_project_investing_activities");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_project_me1cs_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_project_me1cs", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_project_me1cs");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_project_me1ra_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_project_me1ra", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_project_me1ra");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_project_me2cs_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_project_me2cs", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_project_me2cs");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_project_me2ra_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_project_me2ra", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_project_me2ra");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_project_me3cs_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_project_me3cs", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_project_me3cs");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_project_me3ra_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_project_me3ra", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_project_me3ra");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_project_mecs_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_project_mecs", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_project_mecs");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_project_operating_activities_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_project_operating_activities", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_project_operating_activities");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_project_ra_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_project_ra", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_project_ra");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_project_receivablesra_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_project_receivablesra", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_project_receivablesra");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_project_return_aftertax_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_project_return_aftertax", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_project_return_aftertax");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_project_return_aftertax_cash_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_project_return_aftertax_cash", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_project_return_aftertax_cash");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_project_return_aftertax_irr_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_project_return_aftertax_irr", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_project_return_aftertax_irr");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_project_return_aftertax_max_irr_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_project_return_aftertax_max_irr", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_project_return_aftertax_max_irr");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_project_return_aftertax_npv_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_project_return_aftertax_npv", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_project_return_aftertax_npv");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_project_return_pretax_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_project_return_pretax", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_project_return_pretax");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_project_return_pretax_irr_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_project_return_pretax_irr", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_project_return_pretax_irr");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_project_return_pretax_npv_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_project_return_pretax_npv", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_project_return_pretax_npv");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_project_wcra_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_project_wcra", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_project_wcra");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_property_tax_assessed_value_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_property_tax_assessed_value", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_property_tax_assessed_value");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_property_tax_expense_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_property_tax_expense", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_property_tax_expense");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_ptc_fed_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_ptc_fed", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_ptc_fed");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_ptc_sta_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_ptc_sta", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_ptc_sta");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_ptc_total_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_ptc_total", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_ptc_total");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_pv_cash_for_ds_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_pv_cash_for_ds", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_pv_cash_for_ds");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_pv_interest_factor_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_pv_interest_factor", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_pv_interest_factor");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_recapitalization_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_recapitalization", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_recapitalization");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_reserve_debtservice_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_reserve_debtservice", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_reserve_debtservice");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_reserve_equip1_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_reserve_equip1", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_reserve_equip1");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_reserve_equip2_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_reserve_equip2", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_reserve_equip2");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_reserve_equip3_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_reserve_equip3", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_reserve_equip3");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_reserve_interest_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_reserve_interest", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_reserve_interest");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_reserve_om_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_reserve_om", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_reserve_om");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_reserve_receivables_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_reserve_receivables", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_reserve_receivables");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_reserve_total_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_reserve_total", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_reserve_total");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_return_on_equity_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_return_on_equity", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_return_on_equity");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_return_on_equity_dollars_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_return_on_equity_dollars", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_return_on_equity_dollars");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_return_on_equity_input_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_return_on_equity_input", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_return_on_equity_input");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_salvage_cost_lcos_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_salvage_cost_lcos", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_salvage_cost_lcos");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_stadepr_custom_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_stadepr_custom", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_stadepr_custom");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_stadepr_macrs_15_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_stadepr_macrs_15", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_stadepr_macrs_15");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_stadepr_macrs_5_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_stadepr_macrs_5", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_stadepr_macrs_5");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_stadepr_me1_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_stadepr_me1", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_stadepr_me1");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_stadepr_me2_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_stadepr_me2", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_stadepr_me2");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_stadepr_me3_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_stadepr_me3", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_stadepr_me3");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_stadepr_sl_15_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_stadepr_sl_15", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_stadepr_sl_15");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_stadepr_sl_20_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_stadepr_sl_20", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_stadepr_sl_20");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_stadepr_sl_39_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_stadepr_sl_39", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_stadepr_sl_39");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_stadepr_sl_5_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_stadepr_sl_5", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_stadepr_sl_5");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_stadepr_total_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_stadepr_total", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_stadepr_total");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_statax_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_statax", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_statax");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_statax_income_prior_incentives_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_statax_income_prior_incentives", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_statax_income_prior_incentives");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_statax_income_with_incentives_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_statax_income_with_incentives", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_statax_income_with_incentives");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_statax_taxable_incentives_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_statax_taxable_incentives", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_statax_taxable_incentives");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_state_tax_frac_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_state_tax_frac", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_state_tax_frac");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_subscriber1_bill_credit_amount_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_subscriber1_bill_credit_amount", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_subscriber1_bill_credit_amount");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_subscriber1_bill_credit_rate_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_subscriber1_bill_credit_rate", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_subscriber1_bill_credit_rate");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_subscriber1_cost_of_participation_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_subscriber1_cost_of_participation", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_subscriber1_cost_of_participation");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_subscriber1_generation_payment_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_subscriber1_generation_payment", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_subscriber1_generation_payment");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_subscriber1_net_benefit_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_subscriber1_net_benefit", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_subscriber1_net_benefit");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_subscriber1_net_benefit_cumulative_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_subscriber1_net_benefit_cumulative", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_subscriber1_net_benefit_cumulative");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_subscriber1_revenue_annual_payment_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_subscriber1_revenue_annual_payment", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_subscriber1_revenue_annual_payment");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_subscriber1_revenue_generation_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_subscriber1_revenue_generation", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_subscriber1_revenue_generation");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_subscriber1_revenue_upfront_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_subscriber1_revenue_upfront", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_subscriber1_revenue_upfront");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_subscriber1_share_fraction_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_subscriber1_share_fraction", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_subscriber1_share_fraction");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_subscriber1_share_of_generation_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_subscriber1_share_of_generation", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_subscriber1_share_of_generation");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_subscriber2_bill_credit_amount_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_subscriber2_bill_credit_amount", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_subscriber2_bill_credit_amount");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_subscriber2_bill_credit_rate_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_subscriber2_bill_credit_rate", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_subscriber2_bill_credit_rate");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_subscriber2_cost_of_participation_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_subscriber2_cost_of_participation", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_subscriber2_cost_of_participation");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_subscriber2_generation_payment_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_subscriber2_generation_payment", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_subscriber2_generation_payment");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_subscriber2_net_benefit_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_subscriber2_net_benefit", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_subscriber2_net_benefit");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_subscriber2_net_benefit_cumulative_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_subscriber2_net_benefit_cumulative", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_subscriber2_net_benefit_cumulative");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_subscriber2_revenue_annual_payment_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_subscriber2_revenue_annual_payment", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_subscriber2_revenue_annual_payment");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_subscriber2_revenue_generation_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_subscriber2_revenue_generation", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_subscriber2_revenue_generation");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_subscriber2_revenue_upfront_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_subscriber2_revenue_upfront", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_subscriber2_revenue_upfront");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_subscriber2_share_fraction_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_subscriber2_share_fraction", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_subscriber2_share_fraction");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_subscriber2_share_of_generation_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_subscriber2_share_of_generation", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_subscriber2_share_of_generation");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_subscriber3_bill_credit_amount_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_subscriber3_bill_credit_amount", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_subscriber3_bill_credit_amount");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_subscriber3_bill_credit_rate_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_subscriber3_bill_credit_rate", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_subscriber3_bill_credit_rate");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_subscriber3_cost_of_participation_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_subscriber3_cost_of_participation", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_subscriber3_cost_of_participation");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_subscriber3_generation_payment_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_subscriber3_generation_payment", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_subscriber3_generation_payment");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_subscriber3_net_benefit_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_subscriber3_net_benefit", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_subscriber3_net_benefit");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_subscriber3_net_benefit_cumulative_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_subscriber3_net_benefit_cumulative", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_subscriber3_net_benefit_cumulative");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_subscriber3_revenue_annual_payment_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_subscriber3_revenue_annual_payment", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_subscriber3_revenue_annual_payment");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_subscriber3_revenue_generation_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_subscriber3_revenue_generation", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_subscriber3_revenue_generation");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_subscriber3_revenue_upfront_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_subscriber3_revenue_upfront", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_subscriber3_revenue_upfront");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_subscriber3_share_fraction_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_subscriber3_share_fraction", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_subscriber3_share_fraction");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_subscriber3_share_of_generation_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_subscriber3_share_of_generation", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_subscriber3_share_of_generation");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_subscriber4_bill_credit_amount_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_subscriber4_bill_credit_amount", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_subscriber4_bill_credit_amount");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_subscriber4_bill_credit_rate_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_subscriber4_bill_credit_rate", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_subscriber4_bill_credit_rate");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_subscriber4_cost_of_participation_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_subscriber4_cost_of_participation", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_subscriber4_cost_of_participation");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_subscriber4_generation_payment_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_subscriber4_generation_payment", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_subscriber4_generation_payment");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_subscriber4_net_benefit_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_subscriber4_net_benefit", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_subscriber4_net_benefit");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_subscriber4_net_benefit_cumulative_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_subscriber4_net_benefit_cumulative", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_subscriber4_net_benefit_cumulative");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_subscriber4_revenue_annual_payment_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_subscriber4_revenue_annual_payment", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_subscriber4_revenue_annual_payment");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_subscriber4_revenue_generation_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_subscriber4_revenue_generation", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_subscriber4_revenue_generation");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_subscriber4_revenue_upfront_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_subscriber4_revenue_upfront", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_subscriber4_revenue_upfront");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_subscriber4_share_fraction_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_subscriber4_share_fraction", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_subscriber4_share_fraction");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_subscriber4_share_of_generation_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_subscriber4_share_of_generation", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_subscriber4_share_of_generation");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_total_revenue_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_total_revenue", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_total_revenue");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_unsubscribed_share_fraction_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_unsubscribed_share_fraction", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_unsubscribed_share_fraction");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_unsubscribed_share_of_generation_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_unsubscribed_share_of_generation", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_unsubscribed_share_of_generation");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_util_escal_rate_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_util_escal_rate", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_util_escal_rate");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_cf_utility_bill_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cf_utility_bill", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "cf_utility_bill");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_community_solar_upfront_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "community_solar_upfront_cost", &result))
		make_access_error("SAM_Communitysolar", "community_solar_upfront_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_community_solar_upfront_revenue_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "community_solar_upfront_revenue", &result))
		make_access_error("SAM_Communitysolar", "community_solar_upfront_revenue");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_cost_debt_upfront_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cost_debt_upfront", &result))
		make_access_error("SAM_Communitysolar", "cost_debt_upfront");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_cost_financing_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cost_financing", &result))
		make_access_error("SAM_Communitysolar", "cost_financing");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_cost_installed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cost_installed", &result))
		make_access_error("SAM_Communitysolar", "cost_installed");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_cost_installedperwatt_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cost_installedperwatt", &result))
		make_access_error("SAM_Communitysolar", "cost_installedperwatt");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_cost_prefinancing_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cost_prefinancing", &result))
		make_access_error("SAM_Communitysolar", "cost_prefinancing");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_debt_fraction_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "debt_fraction", &result))
		make_access_error("SAM_Communitysolar", "debt_fraction");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_alloc_custom_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_alloc_custom", &result))
		make_access_error("SAM_Communitysolar", "depr_alloc_custom");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_alloc_macrs_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_alloc_macrs_15", &result))
		make_access_error("SAM_Communitysolar", "depr_alloc_macrs_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_alloc_macrs_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_alloc_macrs_5", &result))
		make_access_error("SAM_Communitysolar", "depr_alloc_macrs_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_alloc_none_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_alloc_none", &result))
		make_access_error("SAM_Communitysolar", "depr_alloc_none");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_alloc_none_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_alloc_none_percent", &result))
		make_access_error("SAM_Communitysolar", "depr_alloc_none_percent");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_alloc_sl_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_alloc_sl_15", &result))
		make_access_error("SAM_Communitysolar", "depr_alloc_sl_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_alloc_sl_20_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_alloc_sl_20", &result))
		make_access_error("SAM_Communitysolar", "depr_alloc_sl_20");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_alloc_sl_39_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_alloc_sl_39", &result))
		make_access_error("SAM_Communitysolar", "depr_alloc_sl_39");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_alloc_sl_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_alloc_sl_5", &result))
		make_access_error("SAM_Communitysolar", "depr_alloc_sl_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_alloc_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_alloc_total", &result))
		make_access_error("SAM_Communitysolar", "depr_alloc_total");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_after_itc_custom_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_after_itc_custom", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_after_itc_custom");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_after_itc_macrs_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_after_itc_macrs_15", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_after_itc_macrs_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_after_itc_macrs_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_after_itc_macrs_5", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_after_itc_macrs_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_after_itc_sl_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_after_itc_sl_15", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_after_itc_sl_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_after_itc_sl_20_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_after_itc_sl_20", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_after_itc_sl_20");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_after_itc_sl_39_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_after_itc_sl_39", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_after_itc_sl_39");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_after_itc_sl_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_after_itc_sl_5", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_after_itc_sl_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_after_itc_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_after_itc_total", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_after_itc_total");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_cbi_reduc_custom_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_cbi_reduc_custom", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_cbi_reduc_custom");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_cbi_reduc_macrs_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_cbi_reduc_macrs_15", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_cbi_reduc_macrs_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_cbi_reduc_macrs_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_cbi_reduc_macrs_5", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_cbi_reduc_macrs_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_cbi_reduc_sl_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_cbi_reduc_sl_15", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_cbi_reduc_sl_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_cbi_reduc_sl_20_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_cbi_reduc_sl_20", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_cbi_reduc_sl_20");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_cbi_reduc_sl_39_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_cbi_reduc_sl_39", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_cbi_reduc_sl_39");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_cbi_reduc_sl_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_cbi_reduc_sl_5", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_cbi_reduc_sl_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_cbi_reduc_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_cbi_reduc_total", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_cbi_reduc_total");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_custom_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_custom", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_custom");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_first_year_bonus_custom_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_first_year_bonus_custom", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_first_year_bonus_custom");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_first_year_bonus_macrs_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_first_year_bonus_macrs_15", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_first_year_bonus_macrs_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_first_year_bonus_macrs_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_first_year_bonus_macrs_5", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_first_year_bonus_macrs_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_first_year_bonus_sl_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_first_year_bonus_sl_15", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_first_year_bonus_sl_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_first_year_bonus_sl_20_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_first_year_bonus_sl_20", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_first_year_bonus_sl_20");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_first_year_bonus_sl_39_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_first_year_bonus_sl_39", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_first_year_bonus_sl_39");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_first_year_bonus_sl_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_first_year_bonus_sl_5", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_first_year_bonus_sl_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_first_year_bonus_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_first_year_bonus_total", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_first_year_bonus_total");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_fixed_amount_custom_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_fixed_amount_custom", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_fixed_amount_custom");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_fixed_amount_macrs_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_fixed_amount_macrs_15", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_fixed_amount_macrs_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_fixed_amount_macrs_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_fixed_amount_macrs_5", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_fixed_amount_macrs_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_fixed_amount_sl_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_fixed_amount_sl_15", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_fixed_amount_sl_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_fixed_amount_sl_20_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_fixed_amount_sl_20", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_fixed_amount_sl_20");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_fixed_amount_sl_39_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_fixed_amount_sl_39", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_fixed_amount_sl_39");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_fixed_amount_sl_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_fixed_amount_sl_5", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_fixed_amount_sl_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_fixed_amount_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_fixed_amount_total", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_fixed_amount_total");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_ibi_reduc_custom_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_ibi_reduc_custom", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_ibi_reduc_custom");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_ibi_reduc_macrs_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_ibi_reduc_macrs_15", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_ibi_reduc_macrs_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_ibi_reduc_macrs_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_ibi_reduc_macrs_5", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_ibi_reduc_macrs_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_ibi_reduc_sl_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_ibi_reduc_sl_15", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_ibi_reduc_sl_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_ibi_reduc_sl_20_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_ibi_reduc_sl_20", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_ibi_reduc_sl_20");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_ibi_reduc_sl_39_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_ibi_reduc_sl_39", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_ibi_reduc_sl_39");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_ibi_reduc_sl_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_ibi_reduc_sl_5", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_ibi_reduc_sl_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_ibi_reduc_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_ibi_reduc_total", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_ibi_reduc_total");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_itc_fed_reduction_custom_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_itc_fed_reduction_custom", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_itc_fed_reduction_custom");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_itc_fed_reduction_macrs_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_itc_fed_reduction_macrs_15", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_itc_fed_reduction_macrs_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_itc_fed_reduction_macrs_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_itc_fed_reduction_macrs_5", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_itc_fed_reduction_macrs_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_itc_fed_reduction_sl_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_itc_fed_reduction_sl_15", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_itc_fed_reduction_sl_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_itc_fed_reduction_sl_20_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_itc_fed_reduction_sl_20", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_itc_fed_reduction_sl_20");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_itc_fed_reduction_sl_39_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_itc_fed_reduction_sl_39", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_itc_fed_reduction_sl_39");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_itc_fed_reduction_sl_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_itc_fed_reduction_sl_5", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_itc_fed_reduction_sl_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_itc_fed_reduction_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_itc_fed_reduction_total", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_itc_fed_reduction_total");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_itc_sta_reduction_custom_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_itc_sta_reduction_custom", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_itc_sta_reduction_custom");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_itc_sta_reduction_macrs_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_itc_sta_reduction_macrs_15", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_itc_sta_reduction_macrs_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_itc_sta_reduction_macrs_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_itc_sta_reduction_macrs_5", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_itc_sta_reduction_macrs_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_itc_sta_reduction_sl_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_itc_sta_reduction_sl_15", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_itc_sta_reduction_sl_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_itc_sta_reduction_sl_20_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_itc_sta_reduction_sl_20", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_itc_sta_reduction_sl_20");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_itc_sta_reduction_sl_39_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_itc_sta_reduction_sl_39", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_itc_sta_reduction_sl_39");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_itc_sta_reduction_sl_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_itc_sta_reduction_sl_5", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_itc_sta_reduction_sl_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_itc_sta_reduction_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_itc_sta_reduction_total", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_itc_sta_reduction_total");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_macrs_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_macrs_15", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_macrs_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_macrs_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_macrs_5", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_macrs_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_percent_amount_custom_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_percent_amount_custom", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_percent_amount_custom");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_percent_amount_macrs_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_percent_amount_macrs_15", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_percent_amount_macrs_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_percent_amount_macrs_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_percent_amount_macrs_5", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_percent_amount_macrs_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_percent_amount_sl_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_percent_amount_sl_15", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_percent_amount_sl_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_percent_amount_sl_20_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_percent_amount_sl_20", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_percent_amount_sl_20");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_percent_amount_sl_39_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_percent_amount_sl_39", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_percent_amount_sl_39");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_percent_amount_sl_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_percent_amount_sl_5", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_percent_amount_sl_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_percent_amount_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_percent_amount_total", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_percent_amount_total");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_percent_custom_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_percent_custom", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_percent_custom");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_percent_macrs_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_percent_macrs_15", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_percent_macrs_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_percent_macrs_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_percent_macrs_5", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_percent_macrs_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_percent_qual_custom_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_percent_qual_custom", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_percent_qual_custom");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_percent_qual_macrs_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_percent_qual_macrs_15", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_percent_qual_macrs_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_percent_qual_macrs_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_percent_qual_macrs_5", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_percent_qual_macrs_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_percent_qual_sl_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_percent_qual_sl_15", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_percent_qual_sl_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_percent_qual_sl_20_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_percent_qual_sl_20", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_percent_qual_sl_20");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_percent_qual_sl_39_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_percent_qual_sl_39", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_percent_qual_sl_39");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_percent_qual_sl_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_percent_qual_sl_5", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_percent_qual_sl_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_percent_qual_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_percent_qual_total", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_percent_qual_total");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_percent_sl_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_percent_sl_15", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_percent_sl_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_percent_sl_20_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_percent_sl_20", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_percent_sl_20");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_percent_sl_39_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_percent_sl_39", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_percent_sl_39");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_percent_sl_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_percent_sl_5", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_percent_sl_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_percent_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_percent_total", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_percent_total");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_prior_itc_custom_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_prior_itc_custom", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_prior_itc_custom");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_prior_itc_macrs_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_prior_itc_macrs_15", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_prior_itc_macrs_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_prior_itc_macrs_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_prior_itc_macrs_5", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_prior_itc_macrs_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_prior_itc_sl_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_prior_itc_sl_15", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_prior_itc_sl_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_prior_itc_sl_20_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_prior_itc_sl_20", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_prior_itc_sl_20");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_prior_itc_sl_39_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_prior_itc_sl_39", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_prior_itc_sl_39");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_prior_itc_sl_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_prior_itc_sl_5", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_prior_itc_sl_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_prior_itc_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_prior_itc_total", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_prior_itc_total");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_sl_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_sl_15", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_sl_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_sl_20_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_sl_20", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_sl_20");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_sl_39_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_sl_39", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_sl_39");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_sl_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_sl_5", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_sl_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_fedbas_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_fedbas_total", &result))
		make_access_error("SAM_Communitysolar", "depr_fedbas_total");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_after_itc_custom_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_after_itc_custom", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_after_itc_custom");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_after_itc_macrs_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_after_itc_macrs_15", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_after_itc_macrs_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_after_itc_macrs_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_after_itc_macrs_5", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_after_itc_macrs_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_after_itc_sl_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_after_itc_sl_15", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_after_itc_sl_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_after_itc_sl_20_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_after_itc_sl_20", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_after_itc_sl_20");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_after_itc_sl_39_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_after_itc_sl_39", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_after_itc_sl_39");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_after_itc_sl_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_after_itc_sl_5", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_after_itc_sl_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_after_itc_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_after_itc_total", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_after_itc_total");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_cbi_reduc_custom_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_cbi_reduc_custom", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_cbi_reduc_custom");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_cbi_reduc_macrs_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_cbi_reduc_macrs_15", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_cbi_reduc_macrs_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_cbi_reduc_macrs_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_cbi_reduc_macrs_5", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_cbi_reduc_macrs_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_cbi_reduc_sl_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_cbi_reduc_sl_15", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_cbi_reduc_sl_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_cbi_reduc_sl_20_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_cbi_reduc_sl_20", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_cbi_reduc_sl_20");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_cbi_reduc_sl_39_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_cbi_reduc_sl_39", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_cbi_reduc_sl_39");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_cbi_reduc_sl_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_cbi_reduc_sl_5", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_cbi_reduc_sl_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_cbi_reduc_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_cbi_reduc_total", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_cbi_reduc_total");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_custom_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_custom", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_custom");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_first_year_bonus_custom_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_first_year_bonus_custom", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_first_year_bonus_custom");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_first_year_bonus_macrs_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_first_year_bonus_macrs_15", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_first_year_bonus_macrs_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_first_year_bonus_macrs_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_first_year_bonus_macrs_5", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_first_year_bonus_macrs_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_first_year_bonus_sl_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_first_year_bonus_sl_15", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_first_year_bonus_sl_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_first_year_bonus_sl_20_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_first_year_bonus_sl_20", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_first_year_bonus_sl_20");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_first_year_bonus_sl_39_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_first_year_bonus_sl_39", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_first_year_bonus_sl_39");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_first_year_bonus_sl_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_first_year_bonus_sl_5", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_first_year_bonus_sl_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_first_year_bonus_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_first_year_bonus_total", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_first_year_bonus_total");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_fixed_amount_custom_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_fixed_amount_custom", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_fixed_amount_custom");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_fixed_amount_macrs_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_fixed_amount_macrs_15", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_fixed_amount_macrs_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_fixed_amount_macrs_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_fixed_amount_macrs_5", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_fixed_amount_macrs_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_fixed_amount_sl_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_fixed_amount_sl_15", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_fixed_amount_sl_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_fixed_amount_sl_20_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_fixed_amount_sl_20", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_fixed_amount_sl_20");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_fixed_amount_sl_39_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_fixed_amount_sl_39", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_fixed_amount_sl_39");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_fixed_amount_sl_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_fixed_amount_sl_5", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_fixed_amount_sl_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_fixed_amount_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_fixed_amount_total", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_fixed_amount_total");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_ibi_reduc_custom_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_ibi_reduc_custom", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_ibi_reduc_custom");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_ibi_reduc_macrs_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_ibi_reduc_macrs_15", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_ibi_reduc_macrs_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_ibi_reduc_macrs_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_ibi_reduc_macrs_5", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_ibi_reduc_macrs_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_ibi_reduc_sl_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_ibi_reduc_sl_15", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_ibi_reduc_sl_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_ibi_reduc_sl_20_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_ibi_reduc_sl_20", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_ibi_reduc_sl_20");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_ibi_reduc_sl_39_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_ibi_reduc_sl_39", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_ibi_reduc_sl_39");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_ibi_reduc_sl_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_ibi_reduc_sl_5", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_ibi_reduc_sl_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_ibi_reduc_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_ibi_reduc_total", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_ibi_reduc_total");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_itc_fed_reduction_custom_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_itc_fed_reduction_custom", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_itc_fed_reduction_custom");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_itc_fed_reduction_macrs_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_itc_fed_reduction_macrs_15", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_itc_fed_reduction_macrs_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_itc_fed_reduction_macrs_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_itc_fed_reduction_macrs_5", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_itc_fed_reduction_macrs_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_itc_fed_reduction_sl_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_itc_fed_reduction_sl_15", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_itc_fed_reduction_sl_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_itc_fed_reduction_sl_20_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_itc_fed_reduction_sl_20", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_itc_fed_reduction_sl_20");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_itc_fed_reduction_sl_39_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_itc_fed_reduction_sl_39", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_itc_fed_reduction_sl_39");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_itc_fed_reduction_sl_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_itc_fed_reduction_sl_5", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_itc_fed_reduction_sl_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_itc_fed_reduction_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_itc_fed_reduction_total", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_itc_fed_reduction_total");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_itc_sta_reduction_custom_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_itc_sta_reduction_custom", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_itc_sta_reduction_custom");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_itc_sta_reduction_macrs_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_itc_sta_reduction_macrs_15", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_itc_sta_reduction_macrs_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_itc_sta_reduction_macrs_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_itc_sta_reduction_macrs_5", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_itc_sta_reduction_macrs_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_itc_sta_reduction_sl_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_itc_sta_reduction_sl_15", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_itc_sta_reduction_sl_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_itc_sta_reduction_sl_20_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_itc_sta_reduction_sl_20", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_itc_sta_reduction_sl_20");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_itc_sta_reduction_sl_39_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_itc_sta_reduction_sl_39", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_itc_sta_reduction_sl_39");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_itc_sta_reduction_sl_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_itc_sta_reduction_sl_5", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_itc_sta_reduction_sl_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_itc_sta_reduction_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_itc_sta_reduction_total", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_itc_sta_reduction_total");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_macrs_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_macrs_15", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_macrs_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_macrs_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_macrs_5", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_macrs_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_percent_amount_custom_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_percent_amount_custom", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_percent_amount_custom");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_percent_amount_macrs_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_percent_amount_macrs_15", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_percent_amount_macrs_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_percent_amount_macrs_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_percent_amount_macrs_5", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_percent_amount_macrs_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_percent_amount_sl_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_percent_amount_sl_15", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_percent_amount_sl_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_percent_amount_sl_20_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_percent_amount_sl_20", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_percent_amount_sl_20");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_percent_amount_sl_39_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_percent_amount_sl_39", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_percent_amount_sl_39");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_percent_amount_sl_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_percent_amount_sl_5", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_percent_amount_sl_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_percent_amount_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_percent_amount_total", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_percent_amount_total");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_percent_custom_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_percent_custom", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_percent_custom");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_percent_macrs_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_percent_macrs_15", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_percent_macrs_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_percent_macrs_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_percent_macrs_5", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_percent_macrs_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_percent_qual_custom_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_percent_qual_custom", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_percent_qual_custom");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_percent_qual_macrs_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_percent_qual_macrs_15", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_percent_qual_macrs_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_percent_qual_macrs_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_percent_qual_macrs_5", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_percent_qual_macrs_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_percent_qual_sl_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_percent_qual_sl_15", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_percent_qual_sl_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_percent_qual_sl_20_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_percent_qual_sl_20", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_percent_qual_sl_20");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_percent_qual_sl_39_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_percent_qual_sl_39", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_percent_qual_sl_39");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_percent_qual_sl_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_percent_qual_sl_5", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_percent_qual_sl_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_percent_qual_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_percent_qual_total", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_percent_qual_total");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_percent_sl_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_percent_sl_15", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_percent_sl_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_percent_sl_20_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_percent_sl_20", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_percent_sl_20");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_percent_sl_39_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_percent_sl_39", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_percent_sl_39");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_percent_sl_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_percent_sl_5", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_percent_sl_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_percent_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_percent_total", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_percent_total");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_prior_itc_custom_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_prior_itc_custom", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_prior_itc_custom");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_prior_itc_macrs_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_prior_itc_macrs_15", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_prior_itc_macrs_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_prior_itc_macrs_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_prior_itc_macrs_5", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_prior_itc_macrs_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_prior_itc_sl_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_prior_itc_sl_15", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_prior_itc_sl_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_prior_itc_sl_20_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_prior_itc_sl_20", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_prior_itc_sl_20");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_prior_itc_sl_39_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_prior_itc_sl_39", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_prior_itc_sl_39");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_prior_itc_sl_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_prior_itc_sl_5", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_prior_itc_sl_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_prior_itc_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_prior_itc_total", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_prior_itc_total");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_sl_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_sl_15", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_sl_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_sl_20_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_sl_20", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_sl_20");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_sl_39_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_sl_39", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_sl_39");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_sl_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_sl_5", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_sl_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_depr_stabas_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depr_stabas_total", &result))
		make_access_error("SAM_Communitysolar", "depr_stabas_total");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_effective_tax_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "effective_tax_rate", &result))
		make_access_error("SAM_Communitysolar", "effective_tax_rate");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_flip_actual_irr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "flip_actual_irr", &result))
		make_access_error("SAM_Communitysolar", "flip_actual_irr");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_flip_actual_year_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "flip_actual_year", &result))
		make_access_error("SAM_Communitysolar", "flip_actual_year");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_flip_target_irr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "flip_target_irr", &result))
		make_access_error("SAM_Communitysolar", "flip_target_irr");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_flip_target_year_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "flip_target_year", &result))
		make_access_error("SAM_Communitysolar", "flip_target_year");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_gen_purchases_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "gen_purchases", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "gen_purchases");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_ibi_fedtax_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_fedtax_total", &result))
		make_access_error("SAM_Communitysolar", "ibi_fedtax_total");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_ibi_statax_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_statax_total", &result))
		make_access_error("SAM_Communitysolar", "ibi_statax_total");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_ibi_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_total", &result))
		make_access_error("SAM_Communitysolar", "ibi_total");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_ibi_total_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_total_fed", &result))
		make_access_error("SAM_Communitysolar", "ibi_total_fed");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_ibi_total_oth_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_total_oth", &result))
		make_access_error("SAM_Communitysolar", "ibi_total_oth");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_ibi_total_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_total_sta", &result))
		make_access_error("SAM_Communitysolar", "ibi_total_sta");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_ibi_total_uti_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ibi_total_uti", &result))
		make_access_error("SAM_Communitysolar", "ibi_total_uti");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_issuance_of_equity_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "issuance_of_equity", &result))
		make_access_error("SAM_Communitysolar", "issuance_of_equity");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_itc_disallow_fed_fixed_custom_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_disallow_fed_fixed_custom", &result))
		make_access_error("SAM_Communitysolar", "itc_disallow_fed_fixed_custom");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_itc_disallow_fed_fixed_macrs_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_disallow_fed_fixed_macrs_15", &result))
		make_access_error("SAM_Communitysolar", "itc_disallow_fed_fixed_macrs_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_itc_disallow_fed_fixed_macrs_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_disallow_fed_fixed_macrs_5", &result))
		make_access_error("SAM_Communitysolar", "itc_disallow_fed_fixed_macrs_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_itc_disallow_fed_fixed_sl_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_disallow_fed_fixed_sl_15", &result))
		make_access_error("SAM_Communitysolar", "itc_disallow_fed_fixed_sl_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_itc_disallow_fed_fixed_sl_20_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_disallow_fed_fixed_sl_20", &result))
		make_access_error("SAM_Communitysolar", "itc_disallow_fed_fixed_sl_20");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_itc_disallow_fed_fixed_sl_39_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_disallow_fed_fixed_sl_39", &result))
		make_access_error("SAM_Communitysolar", "itc_disallow_fed_fixed_sl_39");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_itc_disallow_fed_fixed_sl_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_disallow_fed_fixed_sl_5", &result))
		make_access_error("SAM_Communitysolar", "itc_disallow_fed_fixed_sl_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_itc_disallow_fed_fixed_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_disallow_fed_fixed_total", &result))
		make_access_error("SAM_Communitysolar", "itc_disallow_fed_fixed_total");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_itc_disallow_fed_percent_custom_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_disallow_fed_percent_custom", &result))
		make_access_error("SAM_Communitysolar", "itc_disallow_fed_percent_custom");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_itc_disallow_fed_percent_macrs_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_disallow_fed_percent_macrs_15", &result))
		make_access_error("SAM_Communitysolar", "itc_disallow_fed_percent_macrs_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_itc_disallow_fed_percent_macrs_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_disallow_fed_percent_macrs_5", &result))
		make_access_error("SAM_Communitysolar", "itc_disallow_fed_percent_macrs_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_itc_disallow_fed_percent_sl_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_disallow_fed_percent_sl_15", &result))
		make_access_error("SAM_Communitysolar", "itc_disallow_fed_percent_sl_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_itc_disallow_fed_percent_sl_20_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_disallow_fed_percent_sl_20", &result))
		make_access_error("SAM_Communitysolar", "itc_disallow_fed_percent_sl_20");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_itc_disallow_fed_percent_sl_39_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_disallow_fed_percent_sl_39", &result))
		make_access_error("SAM_Communitysolar", "itc_disallow_fed_percent_sl_39");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_itc_disallow_fed_percent_sl_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_disallow_fed_percent_sl_5", &result))
		make_access_error("SAM_Communitysolar", "itc_disallow_fed_percent_sl_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_itc_disallow_fed_percent_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_disallow_fed_percent_total", &result))
		make_access_error("SAM_Communitysolar", "itc_disallow_fed_percent_total");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_itc_disallow_sta_fixed_custom_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_disallow_sta_fixed_custom", &result))
		make_access_error("SAM_Communitysolar", "itc_disallow_sta_fixed_custom");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_itc_disallow_sta_fixed_macrs_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_disallow_sta_fixed_macrs_15", &result))
		make_access_error("SAM_Communitysolar", "itc_disallow_sta_fixed_macrs_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_itc_disallow_sta_fixed_macrs_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_disallow_sta_fixed_macrs_5", &result))
		make_access_error("SAM_Communitysolar", "itc_disallow_sta_fixed_macrs_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_itc_disallow_sta_fixed_sl_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_disallow_sta_fixed_sl_15", &result))
		make_access_error("SAM_Communitysolar", "itc_disallow_sta_fixed_sl_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_itc_disallow_sta_fixed_sl_20_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_disallow_sta_fixed_sl_20", &result))
		make_access_error("SAM_Communitysolar", "itc_disallow_sta_fixed_sl_20");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_itc_disallow_sta_fixed_sl_39_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_disallow_sta_fixed_sl_39", &result))
		make_access_error("SAM_Communitysolar", "itc_disallow_sta_fixed_sl_39");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_itc_disallow_sta_fixed_sl_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_disallow_sta_fixed_sl_5", &result))
		make_access_error("SAM_Communitysolar", "itc_disallow_sta_fixed_sl_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_itc_disallow_sta_fixed_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_disallow_sta_fixed_total", &result))
		make_access_error("SAM_Communitysolar", "itc_disallow_sta_fixed_total");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_itc_disallow_sta_percent_custom_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_disallow_sta_percent_custom", &result))
		make_access_error("SAM_Communitysolar", "itc_disallow_sta_percent_custom");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_itc_disallow_sta_percent_macrs_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_disallow_sta_percent_macrs_15", &result))
		make_access_error("SAM_Communitysolar", "itc_disallow_sta_percent_macrs_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_itc_disallow_sta_percent_macrs_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_disallow_sta_percent_macrs_5", &result))
		make_access_error("SAM_Communitysolar", "itc_disallow_sta_percent_macrs_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_itc_disallow_sta_percent_sl_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_disallow_sta_percent_sl_15", &result))
		make_access_error("SAM_Communitysolar", "itc_disallow_sta_percent_sl_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_itc_disallow_sta_percent_sl_20_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_disallow_sta_percent_sl_20", &result))
		make_access_error("SAM_Communitysolar", "itc_disallow_sta_percent_sl_20");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_itc_disallow_sta_percent_sl_39_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_disallow_sta_percent_sl_39", &result))
		make_access_error("SAM_Communitysolar", "itc_disallow_sta_percent_sl_39");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_itc_disallow_sta_percent_sl_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_disallow_sta_percent_sl_5", &result))
		make_access_error("SAM_Communitysolar", "itc_disallow_sta_percent_sl_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_itc_disallow_sta_percent_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_disallow_sta_percent_total", &result))
		make_access_error("SAM_Communitysolar", "itc_disallow_sta_percent_total");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_itc_fed_fixed_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_fed_fixed_total", &result))
		make_access_error("SAM_Communitysolar", "itc_fed_fixed_total");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_itc_fed_percent_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_fed_percent_total", &result))
		make_access_error("SAM_Communitysolar", "itc_fed_percent_total");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_itc_fed_qual_custom_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_fed_qual_custom", &result))
		make_access_error("SAM_Communitysolar", "itc_fed_qual_custom");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_itc_fed_qual_macrs_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_fed_qual_macrs_15", &result))
		make_access_error("SAM_Communitysolar", "itc_fed_qual_macrs_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_itc_fed_qual_macrs_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_fed_qual_macrs_5", &result))
		make_access_error("SAM_Communitysolar", "itc_fed_qual_macrs_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_itc_fed_qual_sl_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_fed_qual_sl_15", &result))
		make_access_error("SAM_Communitysolar", "itc_fed_qual_sl_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_itc_fed_qual_sl_20_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_fed_qual_sl_20", &result))
		make_access_error("SAM_Communitysolar", "itc_fed_qual_sl_20");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_itc_fed_qual_sl_39_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_fed_qual_sl_39", &result))
		make_access_error("SAM_Communitysolar", "itc_fed_qual_sl_39");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_itc_fed_qual_sl_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_fed_qual_sl_5", &result))
		make_access_error("SAM_Communitysolar", "itc_fed_qual_sl_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_itc_fed_qual_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_fed_qual_total", &result))
		make_access_error("SAM_Communitysolar", "itc_fed_qual_total");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_itc_sta_fixed_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_sta_fixed_total", &result))
		make_access_error("SAM_Communitysolar", "itc_sta_fixed_total");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_itc_sta_percent_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_sta_percent_total", &result))
		make_access_error("SAM_Communitysolar", "itc_sta_percent_total");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_itc_sta_qual_custom_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_sta_qual_custom", &result))
		make_access_error("SAM_Communitysolar", "itc_sta_qual_custom");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_itc_sta_qual_macrs_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_sta_qual_macrs_15", &result))
		make_access_error("SAM_Communitysolar", "itc_sta_qual_macrs_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_itc_sta_qual_macrs_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_sta_qual_macrs_5", &result))
		make_access_error("SAM_Communitysolar", "itc_sta_qual_macrs_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_itc_sta_qual_sl_15_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_sta_qual_sl_15", &result))
		make_access_error("SAM_Communitysolar", "itc_sta_qual_sl_15");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_itc_sta_qual_sl_20_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_sta_qual_sl_20", &result))
		make_access_error("SAM_Communitysolar", "itc_sta_qual_sl_20");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_itc_sta_qual_sl_39_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_sta_qual_sl_39", &result))
		make_access_error("SAM_Communitysolar", "itc_sta_qual_sl_39");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_itc_sta_qual_sl_5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_sta_qual_sl_5", &result))
		make_access_error("SAM_Communitysolar", "itc_sta_qual_sl_5");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_itc_sta_qual_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_sta_qual_total", &result))
		make_access_error("SAM_Communitysolar", "itc_sta_qual_total");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_itc_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_total", &result))
		make_access_error("SAM_Communitysolar", "itc_total");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_itc_total_fed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_total_fed", &result))
		make_access_error("SAM_Communitysolar", "itc_total_fed");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_itc_total_sta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "itc_total_sta", &result))
		make_access_error("SAM_Communitysolar", "itc_total_sta");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_lcoe_nom_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "lcoe_nom", &result))
		make_access_error("SAM_Communitysolar", "lcoe_nom");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_lcoe_real_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "lcoe_real", &result))
		make_access_error("SAM_Communitysolar", "lcoe_real");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_lcog_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "lcog", &result))
		make_access_error("SAM_Communitysolar", "lcog");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_lcog_depr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "lcog_depr", &result))
		make_access_error("SAM_Communitysolar", "lcog_depr");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_lcog_loan_int_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "lcog_loan_int", &result))
		make_access_error("SAM_Communitysolar", "lcog_loan_int");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_lcog_om_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "lcog_om", &result))
		make_access_error("SAM_Communitysolar", "lcog_om");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_lcog_roe_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "lcog_roe", &result))
		make_access_error("SAM_Communitysolar", "lcog_roe");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_lcog_wc_int_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "lcog_wc_int", &result))
		make_access_error("SAM_Communitysolar", "lcog_wc_int");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_lcoptc_fed_nom_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "lcoptc_fed_nom", &result))
		make_access_error("SAM_Communitysolar", "lcoptc_fed_nom");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_lcoptc_fed_real_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "lcoptc_fed_real", &result))
		make_access_error("SAM_Communitysolar", "lcoptc_fed_real");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_lcoptc_sta_nom_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "lcoptc_sta_nom", &result))
		make_access_error("SAM_Communitysolar", "lcoptc_sta_nom");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_lcoptc_sta_real_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "lcoptc_sta_real", &result))
		make_access_error("SAM_Communitysolar", "lcoptc_sta_real");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_lcos_nom_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "lcos_nom", &result))
		make_access_error("SAM_Communitysolar", "lcos_nom");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_lcos_real_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "lcos_real", &result))
		make_access_error("SAM_Communitysolar", "lcos_real");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_min_dscr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "min_dscr", &result))
		make_access_error("SAM_Communitysolar", "min_dscr");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_nominal_discount_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "nominal_discount_rate", &result))
		make_access_error("SAM_Communitysolar", "nominal_discount_rate");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_npv_annual_costs_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "npv_annual_costs", &result))
		make_access_error("SAM_Communitysolar", "npv_annual_costs");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_npv_annual_costs_lcos_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "npv_annual_costs_lcos", &result))
		make_access_error("SAM_Communitysolar", "npv_annual_costs_lcos");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_npv_capacity_revenue_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "npv_capacity_revenue", &result))
		make_access_error("SAM_Communitysolar", "npv_capacity_revenue");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_npv_curtailment_revenue_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "npv_curtailment_revenue", &result))
		make_access_error("SAM_Communitysolar", "npv_curtailment_revenue");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_npv_energy_lcos_nom_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "npv_energy_lcos_nom", &result))
		make_access_error("SAM_Communitysolar", "npv_energy_lcos_nom");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_npv_energy_lcos_real_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "npv_energy_lcos_real", &result))
		make_access_error("SAM_Communitysolar", "npv_energy_lcos_real");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_npv_energy_nom_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "npv_energy_nom", &result))
		make_access_error("SAM_Communitysolar", "npv_energy_nom");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_npv_energy_real_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "npv_energy_real", &result))
		make_access_error("SAM_Communitysolar", "npv_energy_real");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_npv_fed_pbi_income_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "npv_fed_pbi_income", &result))
		make_access_error("SAM_Communitysolar", "npv_fed_pbi_income");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_npv_oth_pbi_income_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "npv_oth_pbi_income", &result))
		make_access_error("SAM_Communitysolar", "npv_oth_pbi_income");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_npv_salvage_value_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "npv_salvage_value", &result))
		make_access_error("SAM_Communitysolar", "npv_salvage_value");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_npv_sta_pbi_income_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "npv_sta_pbi_income", &result))
		make_access_error("SAM_Communitysolar", "npv_sta_pbi_income");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_npv_thermal_value_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "npv_thermal_value", &result))
		make_access_error("SAM_Communitysolar", "npv_thermal_value");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_npv_uti_pbi_income_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "npv_uti_pbi_income", &result))
		make_access_error("SAM_Communitysolar", "npv_uti_pbi_income");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_ppa_price_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ppa_price", &result))
		make_access_error("SAM_Communitysolar", "ppa_price");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_present_value_fuel_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "present_value_fuel", &result))
		make_access_error("SAM_Communitysolar", "present_value_fuel");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_present_value_insandproptax_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "present_value_insandproptax", &result))
		make_access_error("SAM_Communitysolar", "present_value_insandproptax");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_present_value_oandm_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "present_value_oandm", &result))
		make_access_error("SAM_Communitysolar", "present_value_oandm");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_present_value_oandm_nonfuel_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "present_value_oandm_nonfuel", &result))
		make_access_error("SAM_Communitysolar", "present_value_oandm_nonfuel");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_project_return_aftertax_irr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "project_return_aftertax_irr", &result))
		make_access_error("SAM_Communitysolar", "project_return_aftertax_irr");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_project_return_aftertax_npv_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "project_return_aftertax_npv", &result))
		make_access_error("SAM_Communitysolar", "project_return_aftertax_npv");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_prop_tax_assessed_value_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "prop_tax_assessed_value", &result))
		make_access_error("SAM_Communitysolar", "prop_tax_assessed_value");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_purchase_of_property_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "purchase_of_property", &result))
		make_access_error("SAM_Communitysolar", "purchase_of_property");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_pv_cafds_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pv_cafds", &result))
		make_access_error("SAM_Communitysolar", "pv_cafds");
	});
	return result;
}



SAM_EXPORT double* SAM_Communitysolar_Outputs_revenue_gen_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "revenue_gen", length);
	if (!result)
		make_access_error("SAM_Communitysolar", "revenue_gen");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_salvage_value_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "salvage_value", &result))
		make_access_error("SAM_Communitysolar", "salvage_value");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_size_of_debt_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "size_of_debt", &result))
		make_access_error("SAM_Communitysolar", "size_of_debt");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_size_of_equity_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "size_of_equity", &result))
		make_access_error("SAM_Communitysolar", "size_of_equity");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_subscriber1_npv_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subscriber1_npv", &result))
		make_access_error("SAM_Communitysolar", "subscriber1_npv");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_subscriber2_npv_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subscriber2_npv", &result))
		make_access_error("SAM_Communitysolar", "subscriber2_npv");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_subscriber3_npv_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subscriber3_npv", &result))
		make_access_error("SAM_Communitysolar", "subscriber3_npv");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_subscriber4_npv_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subscriber4_npv", &result))
		make_access_error("SAM_Communitysolar", "subscriber4_npv");
	});
	return result;
}



SAM_EXPORT double SAM_Communitysolar_Outputs_wacc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "wacc", &result))
		make_access_error("SAM_Communitysolar", "wacc");
	});
	return result;
}



