#ifndef SAM_IPPPPA_H_
#define SAM_IPPPPA_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// Ippppa Technology Model
	//

	/** 
	 * Create a Ippppa variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_Ippppa;

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_Ippppa_execute(SAM_table data, int verbosity, SAM_error* err);


	//
	// FinancialParameters parameters
	//

	/**
	 * Set analysis_period: Analyis period [years]
	 * options: None
	 * constraints: INTEGER,MIN=0,MAX=50
	 * required if: ?=30
	 */
	SAM_EXPORT void SAM_Ippppa_FinancialParameters_analysis_period_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set debt_fraction: Debt percentage [%]
	 * options: None
	 * constraints: MIN=0,MAX=100
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_FinancialParameters_debt_fraction_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set federal_tax_rate: Federal income tax rate [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Ippppa_FinancialParameters_federal_tax_rate_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set inflation_rate: Inflation rate [%]
	 * options: None
	 * constraints: MIN=-99
	 * required if: *
	 */
	SAM_EXPORT void SAM_Ippppa_FinancialParameters_inflation_rate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set insurance_rate: Insurance rate [%]
	 * options: None
	 * constraints: MIN=0,MAX=100
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Ippppa_FinancialParameters_insurance_rate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set loan_rate: Loan rate [%]
	 * options: None
	 * constraints: MIN=0,MAX=100
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_FinancialParameters_loan_rate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set loan_term: Loan term [years]
	 * options: None
	 * constraints: INTEGER,MIN=0,MAX=50
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_FinancialParameters_loan_term_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set prop_tax_assessed_decline: Assessed value annual decline [%]
	 * options: None
	 * constraints: MIN=0,MAX=100
	 * required if: ?=5
	 */
	SAM_EXPORT void SAM_Ippppa_FinancialParameters_prop_tax_assessed_decline_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set prop_tax_cost_assessed_percent: Percent of pre-financing costs assessed [%]
	 * options: None
	 * constraints: MIN=0,MAX=100
	 * required if: ?=95
	 */
	SAM_EXPORT void SAM_Ippppa_FinancialParameters_prop_tax_cost_assessed_percent_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set property_tax_rate: Property tax rate [%]
	 * options: None
	 * constraints: MIN=0,MAX=100
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Ippppa_FinancialParameters_property_tax_rate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set real_discount_rate: Real discount rate [%]
	 * options: None
	 * constraints: MIN=-99
	 * required if: *
	 */
	SAM_EXPORT void SAM_Ippppa_FinancialParameters_real_discount_rate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set state_tax_rate: State income tax rate [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Ippppa_FinancialParameters_state_tax_rate_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set system_capacity: System nameplate capacity [kW]
	 * options: None
	 * constraints: POSITIVE
	 * required if: *
	 */
	SAM_EXPORT void SAM_Ippppa_FinancialParameters_system_capacity_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set system_heat_rate: System heat rate [MMBTus/MWh]
	 * options: None
	 * constraints: MIN=0
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Ippppa_FinancialParameters_system_heat_rate_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// SystemCosts parameters
	//

	/**
	 * Set add_om_num_types: Number of O and M types
	 * options: None
	 * constraints: INTEGER,MIN=0,MAX=2
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_SystemCosts_add_om_num_types_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set annual_fuel_usage: Fuel usage (yr 1) [kWht]
	 * options: None
	 * constraints: MIN=0
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_SystemCosts_annual_fuel_usage_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set annual_fuel_usage_lifetime: Fuel usage (lifetime) [kWht]
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Ippppa_SystemCosts_annual_fuel_usage_lifetime_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set fuelcell_annual_energy_discharged: Fuel cell annual energy discharged [kWh]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_SystemCosts_fuelcell_annual_energy_discharged_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set om_batt_capacity_cost: Battery capacity-based System Costs amount [$/kWcap]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Ippppa_SystemCosts_om_batt_capacity_cost_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set om_batt_fixed_cost: Battery fixed System Costs annual amount [$/year]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Ippppa_SystemCosts_om_batt_fixed_cost_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set om_batt_nameplate: Battery capacity for System Costs values [kW]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_SystemCosts_om_batt_nameplate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set om_batt_replacement_cost: Replacement cost 1 [$/kWh]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Ippppa_SystemCosts_om_batt_replacement_cost_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set om_batt_variable_cost: Battery production-based System Costs amount [$/MWh]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Ippppa_SystemCosts_om_batt_variable_cost_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set om_capacity: Capacity-based O&M amount [$/kWcap]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Ippppa_SystemCosts_om_capacity_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set om_capacity_escal: Capacity-based O&M escalation [%/year]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Ippppa_SystemCosts_om_capacity_escal_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set om_fixed: Fixed O&M annual amount [$/year]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Ippppa_SystemCosts_om_fixed_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set om_fixed_escal: Fixed O&M escalation [%/year]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Ippppa_SystemCosts_om_fixed_escal_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set om_fuel_cost: Fuel cost [$/MMBtu]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Ippppa_SystemCosts_om_fuel_cost_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set om_fuel_cost_escal: Fuel cost escalation [%/year]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Ippppa_SystemCosts_om_fuel_cost_escal_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set om_fuelcell_capacity_cost: Fuel cell capacity-based System Costs amount [$/kWcap]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Ippppa_SystemCosts_om_fuelcell_capacity_cost_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set om_fuelcell_fixed_cost: Fuel cell fixed System Costs annual amount [$/year]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Ippppa_SystemCosts_om_fuelcell_fixed_cost_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set om_fuelcell_nameplate: Fuel cell capacity for System Costs values [kW]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_SystemCosts_om_fuelcell_nameplate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set om_fuelcell_replacement_cost: Replacement cost 2 [$/kW]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Ippppa_SystemCosts_om_fuelcell_replacement_cost_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set om_fuelcell_variable_cost: Fuel cell production-based System Costs amount [$/MWh]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Ippppa_SystemCosts_om_fuelcell_variable_cost_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set om_opt_fuel_1_cost: Biomass feedstock cost [$/unit]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Ippppa_SystemCosts_om_opt_fuel_1_cost_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set om_opt_fuel_1_cost_escal: Biomass feedstock cost escalation [%/year]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Ippppa_SystemCosts_om_opt_fuel_1_cost_escal_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set om_opt_fuel_1_usage: Biomass feedstock usage [unit]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Ippppa_SystemCosts_om_opt_fuel_1_usage_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set om_opt_fuel_2_cost: Coal feedstock cost [$/unit]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Ippppa_SystemCosts_om_opt_fuel_2_cost_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set om_opt_fuel_2_cost_escal: Coal feedstock cost escalation [%/year]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Ippppa_SystemCosts_om_opt_fuel_2_cost_escal_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set om_opt_fuel_2_usage: Coal feedstock usage [unit]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Ippppa_SystemCosts_om_opt_fuel_2_usage_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set om_production: Production-based O&M amount [$/MWh]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Ippppa_SystemCosts_om_production_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set om_production1_values: Battery production for System Costs values [kWh]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_SystemCosts_om_production1_values_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set om_production2_values: Fuel cell production for System Costs values [kWh]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_SystemCosts_om_production2_values_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set om_production_escal: Production-based O&M escalation [%/year]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Ippppa_SystemCosts_om_production_escal_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set om_replacement_cost_escal: Replacement cost escalation [%/year]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Ippppa_SystemCosts_om_replacement_cost_escal_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// LandLease parameters
	//

	/**
	 * Set land_area: Total land area [acres]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_LandLease_land_area_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set om_land_lease: Land lease cost [$/acre]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_LandLease_om_land_lease_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set om_land_lease_escal: Land lease cost escalation [%/yr]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_LandLease_om_land_lease_escal_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// Depreciation parameters
	//

	/**
	 * Set depr_fed_custom: Federal custom depreciation [%/year]
	 * options: None
	 * constraints: None
	 * required if: depr_fed_type=3
	 */
	SAM_EXPORT void SAM_Ippppa_Depreciation_depr_fed_custom_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set depr_fed_sl_years: Federal depreciation straight-line Years [years]
	 * options: None
	 * constraints: INTEGER,POSITIVE
	 * required if: depr_fed_type=2
	 */
	SAM_EXPORT void SAM_Ippppa_Depreciation_depr_fed_sl_years_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set depr_fed_type: Federal depreciation type
	 * options: 0=none,1=macrs_half_year,2=sl,3=custom
	 * constraints: INTEGER,MIN=0,MAX=3
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_Depreciation_depr_fed_type_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set depr_sta_custom: State custom depreciation [%/year]
	 * options: None
	 * constraints: None
	 * required if: depr_sta_type=3
	 */
	SAM_EXPORT void SAM_Ippppa_Depreciation_depr_sta_custom_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set depr_sta_sl_years: State depreciation straight-line years [years]
	 * options: None
	 * constraints: INTEGER,POSITIVE
	 * required if: depr_sta_type=2
	 */
	SAM_EXPORT void SAM_Ippppa_Depreciation_depr_sta_sl_years_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set depr_sta_type: State depreciation type
	 * options: 0=none,1=macrs_half_year,2=sl,3=custom
	 * constraints: INTEGER,MIN=0,MAX=3
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_Depreciation_depr_sta_type_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// TaxCreditIncentives parameters
	//

	/**
	 * Set itc_fed_amount: Federal amount-based ITC amount [$]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_TaxCreditIncentives_itc_fed_amount_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set itc_fed_amount_deprbas_fed: Federal amount-based ITC reduces federal depreciation basis [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Ippppa_TaxCreditIncentives_itc_fed_amount_deprbas_fed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set itc_fed_amount_deprbas_sta: Federal amount-based ITC reduces state depreciation basis [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Ippppa_TaxCreditIncentives_itc_fed_amount_deprbas_sta_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set itc_fed_percent: Federal percentage-based ITC percent [%]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_TaxCreditIncentives_itc_fed_percent_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set itc_fed_percent_deprbas_fed: Federal percentage-based ITC reduces federal depreciation basis [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Ippppa_TaxCreditIncentives_itc_fed_percent_deprbas_fed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set itc_fed_percent_deprbas_sta: Federal percentage-based ITC reduces state depreciation basis [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Ippppa_TaxCreditIncentives_itc_fed_percent_deprbas_sta_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set itc_fed_percent_maxvalue: Federal percentage-based ITC maximum value [$]
	 * options: None
	 * constraints: None
	 * required if: ?=1e99
	 */
	SAM_EXPORT void SAM_Ippppa_TaxCreditIncentives_itc_fed_percent_maxvalue_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set itc_sta_amount: State amount-based ITC amount [$]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_TaxCreditIncentives_itc_sta_amount_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set itc_sta_amount_deprbas_fed: State amount-based ITC reduces federal depreciation basis [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_TaxCreditIncentives_itc_sta_amount_deprbas_fed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set itc_sta_amount_deprbas_sta: State amount-based ITC reduces state depreciation basis [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_TaxCreditIncentives_itc_sta_amount_deprbas_sta_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set itc_sta_percent: State percentage-based ITC percent [%]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_TaxCreditIncentives_itc_sta_percent_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set itc_sta_percent_deprbas_fed: State percentage-based ITC reduces federal depreciation basis [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_TaxCreditIncentives_itc_sta_percent_deprbas_fed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set itc_sta_percent_deprbas_sta: State percentage-based ITC reduces state depreciation basis [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_TaxCreditIncentives_itc_sta_percent_deprbas_sta_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set itc_sta_percent_maxvalue: State percentage-based ITC maximum Value [$]
	 * options: None
	 * constraints: None
	 * required if: ?=1e99
	 */
	SAM_EXPORT void SAM_Ippppa_TaxCreditIncentives_itc_sta_percent_maxvalue_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set ptc_fed_amount: Federal PTC amount [$/kWh]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_TaxCreditIncentives_ptc_fed_amount_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set ptc_fed_escal: Federal PTC escalation [%/year]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_TaxCreditIncentives_ptc_fed_escal_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ptc_fed_term: Federal PTC term [years]
	 * options: None
	 * constraints: None
	 * required if: ?=10
	 */
	SAM_EXPORT void SAM_Ippppa_TaxCreditIncentives_ptc_fed_term_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ptc_sta_amount: State PTC amount [$/kWh]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_TaxCreditIncentives_ptc_sta_amount_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set ptc_sta_escal: State PTC escalation [%/year]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_TaxCreditIncentives_ptc_sta_escal_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ptc_sta_term: State PTC term [years]
	 * options: None
	 * constraints: None
	 * required if: ?=10
	 */
	SAM_EXPORT void SAM_Ippppa_TaxCreditIncentives_ptc_sta_term_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// PaymentIncentives parameters
	//

	/**
	 * Set cbi_fed_amount: Federal CBI amount [$/Watt]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_cbi_fed_amount_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cbi_fed_deprbas_fed: Federal CBI reduces federal depreciation basis [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_cbi_fed_deprbas_fed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cbi_fed_deprbas_sta: Federal CBI reduces state depreciation basis [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_cbi_fed_deprbas_sta_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cbi_fed_maxvalue: Federal CBI maximum [$]
	 * options: None
	 * constraints: None
	 * required if: ?=1e99
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_cbi_fed_maxvalue_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cbi_fed_tax_fed: Federal CBI federal taxable [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_cbi_fed_tax_fed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cbi_fed_tax_sta: Federal CBI state taxable [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_cbi_fed_tax_sta_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cbi_oth_amount: Other CBI amount [$/Watt]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_cbi_oth_amount_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cbi_oth_deprbas_fed: Other CBI reduces federal depreciation basis [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_cbi_oth_deprbas_fed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cbi_oth_deprbas_sta: Other CBI reduces state depreciation basis [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_cbi_oth_deprbas_sta_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cbi_oth_maxvalue: Other CBI maximum [$]
	 * options: None
	 * constraints: None
	 * required if: ?=1e99
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_cbi_oth_maxvalue_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cbi_oth_tax_fed: Other CBI federal taxable [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_cbi_oth_tax_fed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cbi_oth_tax_sta: Other CBI state taxable [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_cbi_oth_tax_sta_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cbi_sta_amount: State CBI amount [$/Watt]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_cbi_sta_amount_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cbi_sta_deprbas_fed: State CBI reduces federal depreciation basis [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_cbi_sta_deprbas_fed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cbi_sta_deprbas_sta: State CBI reduces state depreciation basis [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_cbi_sta_deprbas_sta_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cbi_sta_maxvalue: State CBI maximum [$]
	 * options: None
	 * constraints: None
	 * required if: ?=1e99
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_cbi_sta_maxvalue_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cbi_sta_tax_fed: State CBI federal taxable [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_cbi_sta_tax_fed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cbi_sta_tax_sta: State CBI state taxable [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_cbi_sta_tax_sta_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cbi_uti_amount: Utility CBI amount [$/Watt]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_cbi_uti_amount_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cbi_uti_deprbas_fed: Utility CBI reduces federal depreciation basis [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_cbi_uti_deprbas_fed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cbi_uti_deprbas_sta: Utility CBI reduces state depreciation basis [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_cbi_uti_deprbas_sta_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cbi_uti_maxvalue: Utility CBI maximum [$]
	 * options: None
	 * constraints: None
	 * required if: ?=1e99
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_cbi_uti_maxvalue_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cbi_uti_tax_fed: Utility CBI federal taxable [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_cbi_uti_tax_fed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cbi_uti_tax_sta: Utility CBI state taxable [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_cbi_uti_tax_sta_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ibi_fed_amount: Federal amount-based IBI amount [$]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_ibi_fed_amount_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ibi_fed_amount_deprbas_fed: Federal amount-based IBI reduces federal depreciation basis [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_ibi_fed_amount_deprbas_fed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ibi_fed_amount_deprbas_sta: Federal amount-based IBI reduces state depreciation basis [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_ibi_fed_amount_deprbas_sta_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ibi_fed_amount_tax_fed: Federal amount-based IBI federal taxable [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_ibi_fed_amount_tax_fed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ibi_fed_amount_tax_sta: Federal amount-based IBI state taxable [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_ibi_fed_amount_tax_sta_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ibi_fed_percent: Federal percentage-based IBI percent [%]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_ibi_fed_percent_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ibi_fed_percent_deprbas_fed: Federal percentage-based IBI reduces federal depreciation basis [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_ibi_fed_percent_deprbas_fed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ibi_fed_percent_deprbas_sta: Federal percentage-based IBI reduces state depreciation basis [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_ibi_fed_percent_deprbas_sta_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ibi_fed_percent_maxvalue: Federal percentage-based IBI maximum value [$]
	 * options: None
	 * constraints: None
	 * required if: ?=1e99
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_ibi_fed_percent_maxvalue_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ibi_fed_percent_tax_fed: Federal percentage-based IBI federal taxable [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_ibi_fed_percent_tax_fed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ibi_fed_percent_tax_sta: Federal percentage-based IBI state taxable [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_ibi_fed_percent_tax_sta_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ibi_oth_amount: Other amount-based IBI amount [$]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_ibi_oth_amount_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ibi_oth_amount_deprbas_fed: Other amount-based IBI reduces federal depreciation basis [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_ibi_oth_amount_deprbas_fed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ibi_oth_amount_deprbas_sta: Other amount-based IBI reduces state depreciation basis [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_ibi_oth_amount_deprbas_sta_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ibi_oth_amount_tax_fed: Other amount-based IBI federal taxable [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_ibi_oth_amount_tax_fed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ibi_oth_amount_tax_sta: Other amount-based IBI state taxable [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_ibi_oth_amount_tax_sta_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ibi_oth_percent: Other percentage-based IBI percent [%]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_ibi_oth_percent_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ibi_oth_percent_deprbas_fed: Other percentage-based IBI reduces federal depreciation basis [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_ibi_oth_percent_deprbas_fed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ibi_oth_percent_deprbas_sta: Other percentage-based IBI reduces state depreciation basis [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_ibi_oth_percent_deprbas_sta_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ibi_oth_percent_maxvalue: Other percentage-based IBI maximum value [$]
	 * options: None
	 * constraints: None
	 * required if: ?=1e99
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_ibi_oth_percent_maxvalue_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ibi_oth_percent_tax_fed: Other percentage-based IBI federal taxable [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_ibi_oth_percent_tax_fed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ibi_oth_percent_tax_sta: Other percentage-based IBI state taxable [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_ibi_oth_percent_tax_sta_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ibi_sta_amount: State amount-based IBI amount [$]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_ibi_sta_amount_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ibi_sta_amount_deprbas_fed: State amount-based IBI reduces federal depreciation basis [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_ibi_sta_amount_deprbas_fed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ibi_sta_amount_deprbas_sta: State amount-based IBI reduces state depreciation basis [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_ibi_sta_amount_deprbas_sta_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ibi_sta_amount_tax_fed: State amount-based IBI federal taxable [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_ibi_sta_amount_tax_fed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ibi_sta_amount_tax_sta: State amount-based IBI state taxable [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_ibi_sta_amount_tax_sta_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ibi_sta_percent: State percentage-based IBI percent [%]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_ibi_sta_percent_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ibi_sta_percent_deprbas_fed: State percentage-based IBI reduces federal depreciation basis [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_ibi_sta_percent_deprbas_fed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ibi_sta_percent_deprbas_sta: State percentage-based IBI reduces state depreciation basis [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_ibi_sta_percent_deprbas_sta_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ibi_sta_percent_maxvalue: State percentage-based IBI maximum value [$]
	 * options: None
	 * constraints: None
	 * required if: ?=1e99
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_ibi_sta_percent_maxvalue_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ibi_sta_percent_tax_fed: State percentage-based IBI federal taxable [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_ibi_sta_percent_tax_fed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ibi_sta_percent_tax_sta: State percentage-based IBI state taxable [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_ibi_sta_percent_tax_sta_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ibi_uti_amount: Utility amount-based IBI amount [$]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_ibi_uti_amount_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ibi_uti_amount_deprbas_fed: Utility amount-based IBI reduces federal depreciation basis [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_ibi_uti_amount_deprbas_fed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ibi_uti_amount_deprbas_sta: Utility amount-based IBI reduces state depreciation basis [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_ibi_uti_amount_deprbas_sta_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ibi_uti_amount_tax_fed: Utility amount-based IBI federal taxable [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_ibi_uti_amount_tax_fed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ibi_uti_amount_tax_sta: Utility amount-based IBI state taxable [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_ibi_uti_amount_tax_sta_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ibi_uti_percent: Utility percentage-based IBI percent [%]
	 * options: None
	 * constraints: None
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_ibi_uti_percent_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ibi_uti_percent_deprbas_fed: Utility percentage-based IBI reduces federal depreciation basis [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_ibi_uti_percent_deprbas_fed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ibi_uti_percent_deprbas_sta: Utility percentage-based IBI reduces state depreciation basis [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_ibi_uti_percent_deprbas_sta_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ibi_uti_percent_maxvalue: Utility percentage-based IBI maximum value [$]
	 * options: None
	 * constraints: None
	 * required if: ?=1e99
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_ibi_uti_percent_maxvalue_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ibi_uti_percent_tax_fed: Utility percentage-based IBI federal taxable [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_ibi_uti_percent_tax_fed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ibi_uti_percent_tax_sta: Utility percentage-based IBI state taxable [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_ibi_uti_percent_tax_sta_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set pbi_fed_amount: Federal PBI amount [$/kWh]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_pbi_fed_amount_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set pbi_fed_escal: Federal PBI escalation [%]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_pbi_fed_escal_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set pbi_fed_tax_fed: Federal PBI federal taxable [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_pbi_fed_tax_fed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set pbi_fed_tax_sta: Federal PBI state taxable [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_pbi_fed_tax_sta_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set pbi_fed_term: Federal PBI term [years]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_pbi_fed_term_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set pbi_oth_amount: Other PBI amount [$/kWh]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_pbi_oth_amount_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set pbi_oth_escal: Other PBI escalation [%]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_pbi_oth_escal_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set pbi_oth_tax_fed: Other PBI federal taxable [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_pbi_oth_tax_fed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set pbi_oth_tax_sta: Other PBI state taxable [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_pbi_oth_tax_sta_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set pbi_oth_term: Other PBI term [years]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_pbi_oth_term_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set pbi_sta_amount: State PBI amount [$/kWh]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_pbi_sta_amount_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set pbi_sta_escal: State PBI escalation [%]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_pbi_sta_escal_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set pbi_sta_tax_fed: State PBI federal taxable [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_pbi_sta_tax_fed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set pbi_sta_tax_sta: State PBI state taxable [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_pbi_sta_tax_sta_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set pbi_sta_term: State PBI term [years]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_pbi_sta_term_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set pbi_uti_amount: Utility PBI amount [$/kWh]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_pbi_uti_amount_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set pbi_uti_escal: Utility PBI escalation [%]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_pbi_uti_escal_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set pbi_uti_tax_fed: Utility PBI federal taxable [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_pbi_uti_tax_fed_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set pbi_uti_tax_sta: Utility PBI state taxable [0/1]
	 * options: None
	 * constraints: BOOLEAN
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_pbi_uti_tax_sta_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set pbi_uti_term: Utility PBI term [years]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_PaymentIncentives_pbi_uti_term_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// Common parameters
	//

	/**
	 * Set bid_price: Initial year PPA price [$/kWh]
	 * options: None
	 * constraints: None
	 * required if: ?=0.10
	 */
	SAM_EXPORT void SAM_Ippppa_Common_bid_price_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set bid_price_esc: PPA escalation [%]
	 * options: None
	 * constraints: MIN=0,MAX=100
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_Common_bid_price_esc_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set construction_financing_cost: Construction financing total [$]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Ippppa_Common_construction_financing_cost_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set degradation: Annual energy degradation
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Ippppa_Common_degradation_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set dispatch_factor1: Dispatch payment factor 1
	 * options: None
	 * constraints: None
	 * required if: market=0
	 */
	SAM_EXPORT void SAM_Ippppa_Common_dispatch_factor1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dispatch_factor2: Dispatch payment factor 2
	 * options: None
	 * constraints: None
	 * required if: market=0
	 */
	SAM_EXPORT void SAM_Ippppa_Common_dispatch_factor2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dispatch_factor3: Dispatch payment factor 3
	 * options: None
	 * constraints: None
	 * required if: market=0
	 */
	SAM_EXPORT void SAM_Ippppa_Common_dispatch_factor3_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dispatch_factor4: Dispatch payment factor 4
	 * options: None
	 * constraints: None
	 * required if: market=0
	 */
	SAM_EXPORT void SAM_Ippppa_Common_dispatch_factor4_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dispatch_factor5: Dispatch payment factor 5
	 * options: None
	 * constraints: None
	 * required if: market=0
	 */
	SAM_EXPORT void SAM_Ippppa_Common_dispatch_factor5_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dispatch_factor6: Dispatch payment factor 6
	 * options: None
	 * constraints: None
	 * required if: market=0
	 */
	SAM_EXPORT void SAM_Ippppa_Common_dispatch_factor6_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dispatch_factor7: Dispatch payment factor 7
	 * options: None
	 * constraints: None
	 * required if: market=0
	 */
	SAM_EXPORT void SAM_Ippppa_Common_dispatch_factor7_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dispatch_factor8: Dispatch payment factor 8
	 * options: None
	 * constraints: None
	 * required if: market=0
	 */
	SAM_EXPORT void SAM_Ippppa_Common_dispatch_factor8_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dispatch_factor9: Dispatch payment factor 9
	 * options: None
	 * constraints: None
	 * required if: market=0
	 */
	SAM_EXPORT void SAM_Ippppa_Common_dispatch_factor9_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dispatch_sched_weekday: Diurnal weekday dispatch periods [1..9]
	 * options: 12 x 24 matrix
	 * constraints: None
	 * required if: market=0
	 */
	SAM_EXPORT void SAM_Ippppa_Common_dispatch_sched_weekday_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set dispatch_sched_weekend: Diurnal weekend dispatch periods [1..9]
	 * options: 12 x 24 matrix
	 * constraints: None
	 * required if: market=0
	 */
	SAM_EXPORT void SAM_Ippppa_Common_dispatch_sched_weekend_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set gen: Power generated by renewable resource [kW]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Ippppa_Common_gen_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set market: Utility IPP or Commercial PPA [0/1]
	 * options: 0=ipp,1=ppa
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_Common_market_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set min_dscr_required: Minimum DSCR required [0/1]
	 * options: 0=no,1=yes
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Ippppa_Common_min_dscr_required_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set min_dscr_target: Minimum required DSCR
	 * options: None
	 * constraints: None
	 * required if: ?=1.4
	 */
	SAM_EXPORT void SAM_Ippppa_Common_min_dscr_target_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set min_irr_target: Minimum required IRR [%]
	 * options: None
	 * constraints: None
	 * required if: ?=15
	 */
	SAM_EXPORT void SAM_Ippppa_Common_min_irr_target_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set optimize_lcoe_wrt_debt_fraction: Optimize LCOE with respect to debt percent [0/1]
	 * options: 0=no,1=yes
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_Common_optimize_lcoe_wrt_debt_fraction_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set optimize_lcoe_wrt_ppa_escalation: Optimize LCOE with respect to PPA escalation [0/1]
	 * options: 0=no,1=yes
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_Common_optimize_lcoe_wrt_ppa_escalation_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set positive_cashflow_required: Positive cash flow required [0/1]
	 * options: 0=no,1=yes
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_Ippppa_Common_positive_cashflow_required_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ppa_escalation: PPA escalation [%]
	 * options: None
	 * constraints: None
	 * required if: ?=0.6
	 */
	SAM_EXPORT void SAM_Ippppa_Common_ppa_escalation_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ppa_soln_max: PPA solution maximum ppa [cents/kWh]
	 * options: None
	 * constraints: None
	 * required if: ?=100
	 */
	SAM_EXPORT void SAM_Ippppa_Common_ppa_soln_max_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ppa_soln_max_iterations: PPA solution maximum number of iterations
	 * options: None
	 * constraints: INTEGER,MIN=1
	 * required if: ?=100
	 */
	SAM_EXPORT void SAM_Ippppa_Common_ppa_soln_max_iterations_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ppa_soln_min: PPA solution minimum ppa [cents/kWh]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_Common_ppa_soln_min_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ppa_soln_tolerance: PPA solution tolerance
	 * options: None
	 * constraints: None
	 * required if: ?=1e-3
	 */
	SAM_EXPORT void SAM_Ippppa_Common_ppa_soln_tolerance_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set salvage_percentage: Salvage value percentage [%]
	 * options: None
	 * constraints: MIN=0,MAX=100
	 * required if: ?=0.0
	 */
	SAM_EXPORT void SAM_Ippppa_Common_salvage_percentage_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set soln_mode: PPA solution mode [0/1]
	 * options: 0=solve ppa,1=specify ppa
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_Common_soln_mode_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set system_capacity: System nameplate capacity [kW]
	 * options: None
	 * constraints: MIN=1e-3
	 * required if: *
	 */
	SAM_EXPORT void SAM_Ippppa_Common_system_capacity_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set system_recapitalization_boolean: Recapitalization boolean
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_Common_system_recapitalization_boolean_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set system_recapitalization_cost: Recapitalization cost [$]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_Common_system_recapitalization_cost_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set system_recapitalization_escalation: Recapitalization escalation (above inflation) [%]
	 * options: None
	 * constraints: MIN=0,MAX=100
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_Common_system_recapitalization_escalation_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set system_use_lifetime_output: Lifetime hourly system outputs [0/1]
	 * options: 0=hourly first year,1=hourly lifetime
	 * constraints: INTEGER,MIN=0
	 * required if: *
	 */
	SAM_EXPORT void SAM_Ippppa_Common_system_use_lifetime_output_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set system_use_recapitalization: Recapitalization expenses [0/1]
	 * options: 0=None,1=Recapitalize
	 * constraints: INTEGER,MIN=0
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Ippppa_Common_system_use_recapitalization_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set total_installed_cost: Total installed cost [$]
	 * options: None
	 * constraints: MIN=0
	 * required if: *
	 */
	SAM_EXPORT void SAM_Ippppa_Common_total_installed_cost_nset(SAM_table ptr, double number, SAM_error *err);


	/**
	 * FinancialParameters Getters
	 */

	SAM_EXPORT double SAM_Ippppa_FinancialParameters_analysis_period_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_FinancialParameters_debt_fraction_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_FinancialParameters_federal_tax_rate_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_FinancialParameters_inflation_rate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_FinancialParameters_insurance_rate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_FinancialParameters_loan_rate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_FinancialParameters_loan_term_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_FinancialParameters_prop_tax_assessed_decline_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_FinancialParameters_prop_tax_cost_assessed_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_FinancialParameters_property_tax_rate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_FinancialParameters_real_discount_rate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_FinancialParameters_state_tax_rate_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_FinancialParameters_system_capacity_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_FinancialParameters_system_heat_rate_nget(SAM_table ptr, SAM_error *err);


	/**
	 * SystemCosts Getters
	 */

	SAM_EXPORT double SAM_Ippppa_SystemCosts_add_om_num_types_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_SystemCosts_annual_fuel_usage_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_SystemCosts_annual_fuel_usage_lifetime_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_SystemCosts_fuelcell_annual_energy_discharged_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_SystemCosts_om_batt_capacity_cost_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_SystemCosts_om_batt_fixed_cost_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_SystemCosts_om_batt_nameplate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_SystemCosts_om_batt_replacement_cost_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_SystemCosts_om_batt_variable_cost_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_SystemCosts_om_capacity_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_SystemCosts_om_capacity_escal_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_SystemCosts_om_fixed_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_SystemCosts_om_fixed_escal_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_SystemCosts_om_fuel_cost_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_SystemCosts_om_fuel_cost_escal_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_SystemCosts_om_fuelcell_capacity_cost_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_SystemCosts_om_fuelcell_fixed_cost_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_SystemCosts_om_fuelcell_nameplate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_SystemCosts_om_fuelcell_replacement_cost_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_SystemCosts_om_fuelcell_variable_cost_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_SystemCosts_om_opt_fuel_1_cost_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_SystemCosts_om_opt_fuel_1_cost_escal_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_SystemCosts_om_opt_fuel_1_usage_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_SystemCosts_om_opt_fuel_2_cost_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_SystemCosts_om_opt_fuel_2_cost_escal_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_SystemCosts_om_opt_fuel_2_usage_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_SystemCosts_om_production_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_SystemCosts_om_production1_values_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_SystemCosts_om_production2_values_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_SystemCosts_om_production_escal_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_SystemCosts_om_replacement_cost_escal_nget(SAM_table ptr, SAM_error *err);


	/**
	 * LandLease Getters
	 */

	SAM_EXPORT double SAM_Ippppa_LandLease_land_area_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_LandLease_om_land_lease_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_LandLease_om_land_lease_escal_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Depreciation Getters
	 */

	SAM_EXPORT double* SAM_Ippppa_Depreciation_depr_fed_custom_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Depreciation_depr_fed_sl_years_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Depreciation_depr_fed_type_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Depreciation_depr_sta_custom_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Depreciation_depr_sta_sl_years_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Depreciation_depr_sta_type_nget(SAM_table ptr, SAM_error *err);


	/**
	 * TaxCreditIncentives Getters
	 */

	SAM_EXPORT double* SAM_Ippppa_TaxCreditIncentives_itc_fed_amount_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_TaxCreditIncentives_itc_fed_amount_deprbas_fed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_TaxCreditIncentives_itc_fed_amount_deprbas_sta_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_TaxCreditIncentives_itc_fed_percent_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_TaxCreditIncentives_itc_fed_percent_deprbas_fed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_TaxCreditIncentives_itc_fed_percent_deprbas_sta_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_TaxCreditIncentives_itc_fed_percent_maxvalue_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_TaxCreditIncentives_itc_sta_amount_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_TaxCreditIncentives_itc_sta_amount_deprbas_fed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_TaxCreditIncentives_itc_sta_amount_deprbas_sta_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_TaxCreditIncentives_itc_sta_percent_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_TaxCreditIncentives_itc_sta_percent_deprbas_fed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_TaxCreditIncentives_itc_sta_percent_deprbas_sta_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_TaxCreditIncentives_itc_sta_percent_maxvalue_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_TaxCreditIncentives_ptc_fed_amount_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_TaxCreditIncentives_ptc_fed_escal_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_TaxCreditIncentives_ptc_fed_term_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_TaxCreditIncentives_ptc_sta_amount_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_TaxCreditIncentives_ptc_sta_escal_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_TaxCreditIncentives_ptc_sta_term_nget(SAM_table ptr, SAM_error *err);


	/**
	 * PaymentIncentives Getters
	 */

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_cbi_fed_amount_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_cbi_fed_deprbas_fed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_cbi_fed_deprbas_sta_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_cbi_fed_maxvalue_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_cbi_fed_tax_fed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_cbi_fed_tax_sta_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_cbi_oth_amount_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_cbi_oth_deprbas_fed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_cbi_oth_deprbas_sta_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_cbi_oth_maxvalue_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_cbi_oth_tax_fed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_cbi_oth_tax_sta_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_cbi_sta_amount_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_cbi_sta_deprbas_fed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_cbi_sta_deprbas_sta_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_cbi_sta_maxvalue_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_cbi_sta_tax_fed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_cbi_sta_tax_sta_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_cbi_uti_amount_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_cbi_uti_deprbas_fed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_cbi_uti_deprbas_sta_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_cbi_uti_maxvalue_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_cbi_uti_tax_fed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_cbi_uti_tax_sta_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_ibi_fed_amount_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_ibi_fed_amount_deprbas_fed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_ibi_fed_amount_deprbas_sta_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_ibi_fed_amount_tax_fed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_ibi_fed_amount_tax_sta_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_ibi_fed_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_ibi_fed_percent_deprbas_fed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_ibi_fed_percent_deprbas_sta_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_ibi_fed_percent_maxvalue_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_ibi_fed_percent_tax_fed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_ibi_fed_percent_tax_sta_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_ibi_oth_amount_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_ibi_oth_amount_deprbas_fed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_ibi_oth_amount_deprbas_sta_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_ibi_oth_amount_tax_fed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_ibi_oth_amount_tax_sta_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_ibi_oth_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_ibi_oth_percent_deprbas_fed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_ibi_oth_percent_deprbas_sta_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_ibi_oth_percent_maxvalue_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_ibi_oth_percent_tax_fed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_ibi_oth_percent_tax_sta_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_ibi_sta_amount_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_ibi_sta_amount_deprbas_fed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_ibi_sta_amount_deprbas_sta_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_ibi_sta_amount_tax_fed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_ibi_sta_amount_tax_sta_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_ibi_sta_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_ibi_sta_percent_deprbas_fed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_ibi_sta_percent_deprbas_sta_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_ibi_sta_percent_maxvalue_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_ibi_sta_percent_tax_fed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_ibi_sta_percent_tax_sta_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_ibi_uti_amount_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_ibi_uti_amount_deprbas_fed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_ibi_uti_amount_deprbas_sta_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_ibi_uti_amount_tax_fed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_ibi_uti_amount_tax_sta_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_ibi_uti_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_ibi_uti_percent_deprbas_fed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_ibi_uti_percent_deprbas_sta_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_ibi_uti_percent_maxvalue_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_ibi_uti_percent_tax_fed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_ibi_uti_percent_tax_sta_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_PaymentIncentives_pbi_fed_amount_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_pbi_fed_escal_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_pbi_fed_tax_fed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_pbi_fed_tax_sta_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_pbi_fed_term_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_PaymentIncentives_pbi_oth_amount_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_pbi_oth_escal_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_pbi_oth_tax_fed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_pbi_oth_tax_sta_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_pbi_oth_term_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_PaymentIncentives_pbi_sta_amount_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_pbi_sta_escal_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_pbi_sta_tax_fed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_pbi_sta_tax_sta_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_pbi_sta_term_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_PaymentIncentives_pbi_uti_amount_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_pbi_uti_escal_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_pbi_uti_tax_fed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_pbi_uti_tax_sta_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_PaymentIncentives_pbi_uti_term_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Common Getters
	 */

	SAM_EXPORT double* SAM_Ippppa_Common_bid_price_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Common_bid_price_esc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Common_construction_financing_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Common_degradation_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Common_dispatch_factor1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Common_dispatch_factor2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Common_dispatch_factor3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Common_dispatch_factor4_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Common_dispatch_factor5_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Common_dispatch_factor6_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Common_dispatch_factor7_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Common_dispatch_factor8_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Common_dispatch_factor9_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Common_dispatch_sched_weekday_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Common_dispatch_sched_weekend_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Common_gen_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Common_market_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Common_min_dscr_required_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Common_min_dscr_target_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Common_min_irr_target_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Common_optimize_lcoe_wrt_debt_fraction_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Common_optimize_lcoe_wrt_ppa_escalation_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Common_positive_cashflow_required_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Common_ppa_escalation_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Common_ppa_soln_max_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Common_ppa_soln_max_iterations_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Common_ppa_soln_min_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Common_ppa_soln_tolerance_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Common_salvage_percentage_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Common_soln_mode_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Common_system_capacity_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Common_system_recapitalization_boolean_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Common_system_recapitalization_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Common_system_recapitalization_escalation_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Common_system_use_lifetime_output_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Common_system_use_recapitalization_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Common_total_installed_cost_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double SAM_Ippppa_Outputs_actual_debt_frac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_actual_ppa_escalation_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_cbi_fedtax_total_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_cbi_statax_total_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_cbi_total_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_cbi_total_fed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_cbi_total_oth_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_cbi_total_sta_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_cbi_total_uti_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_after_tax_cash_flow_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_after_tax_net_equity_cash_flow_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_after_tax_net_equity_cost_flow_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_debt_balance_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_debt_payment_interest_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_debt_payment_principal_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_debt_payment_total_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_deductible_expenses_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_degradation_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_effective_tax_frac_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_energy_net_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_energy_net_apr_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_energy_net_aug_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_energy_net_dec_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_energy_net_dispatch1_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_energy_net_dispatch2_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_energy_net_dispatch3_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_energy_net_dispatch4_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_energy_net_dispatch5_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_energy_net_dispatch6_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_energy_net_dispatch7_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_energy_net_dispatch8_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_energy_net_dispatch9_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_energy_net_feb_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_energy_net_jan_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_energy_net_jul_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_energy_net_jun_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_energy_net_mar_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_energy_net_may_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_energy_net_monthly_firstyear_TOD1_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_energy_net_monthly_firstyear_TOD2_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_energy_net_monthly_firstyear_TOD3_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_energy_net_monthly_firstyear_TOD4_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_energy_net_monthly_firstyear_TOD5_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_energy_net_monthly_firstyear_TOD6_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_energy_net_monthly_firstyear_TOD7_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_energy_net_monthly_firstyear_TOD8_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_energy_net_monthly_firstyear_TOD9_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_energy_net_nov_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_energy_net_oct_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_energy_net_sep_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_energy_price_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_energy_value_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_fed_depr_sched_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_fed_depreciation_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_fed_incentive_income_less_deductions_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_fed_income_taxes_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_fed_tax_savings_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_fed_taxable_income_less_deductions_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_federal_tax_frac_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_insurance_expense_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_land_lease_expense_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_cf_length_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_net_salvage_value_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_om_capacity_expense_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_om_fixed_expense_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_om_fuel_expense_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_om_opt_fuel_1_expense_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_om_opt_fuel_2_expense_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_om_production_expense_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_operating_expenses_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_operating_income_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_pbi_fedtax_total_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_pbi_statax_total_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_pbi_total_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_pbi_total_fed_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_pbi_total_oth_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_pbi_total_sta_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_pbi_total_uti_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_ppa_price_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_pretax_dscr_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_property_tax_assessed_value_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_property_tax_expense_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_ptc_fed_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_ptc_sta_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_ptc_total_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_recapitalization_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_revenue_apr_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_revenue_aug_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_revenue_dec_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_revenue_dispatch1_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_revenue_dispatch2_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_revenue_dispatch3_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_revenue_dispatch4_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_revenue_dispatch5_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_revenue_dispatch6_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_revenue_dispatch7_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_revenue_dispatch8_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_revenue_dispatch9_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_revenue_feb_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_revenue_jan_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_revenue_jul_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_revenue_jun_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_revenue_mar_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_revenue_may_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_revenue_monthly_firstyear_TOD1_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_revenue_monthly_firstyear_TOD2_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_revenue_monthly_firstyear_TOD3_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_revenue_monthly_firstyear_TOD4_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_revenue_monthly_firstyear_TOD5_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_revenue_monthly_firstyear_TOD6_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_revenue_monthly_firstyear_TOD7_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_revenue_monthly_firstyear_TOD8_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_revenue_monthly_firstyear_TOD9_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_revenue_nov_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_revenue_oct_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_revenue_sep_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_sta_and_fed_tax_savings_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_sta_depr_sched_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_sta_depreciation_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_sta_incentive_income_less_deductions_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_sta_income_taxes_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_sta_tax_savings_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_sta_taxable_income_less_deductions_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Ippppa_Outputs_cf_state_tax_frac_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_debt_fraction_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_effective_tax_rate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_firstyear_energy_dispatch1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_firstyear_energy_dispatch2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_firstyear_energy_dispatch3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_firstyear_energy_dispatch4_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_firstyear_energy_dispatch5_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_firstyear_energy_dispatch6_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_firstyear_energy_dispatch7_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_firstyear_energy_dispatch8_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_firstyear_energy_dispatch9_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_firstyear_energy_price1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_firstyear_energy_price2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_firstyear_energy_price3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_firstyear_energy_price4_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_firstyear_energy_price5_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_firstyear_energy_price6_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_firstyear_energy_price7_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_firstyear_energy_price8_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_firstyear_energy_price9_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_firstyear_revenue_dispatch1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_firstyear_revenue_dispatch2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_firstyear_revenue_dispatch3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_firstyear_revenue_dispatch4_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_firstyear_revenue_dispatch5_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_firstyear_revenue_dispatch6_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_firstyear_revenue_dispatch7_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_firstyear_revenue_dispatch8_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_firstyear_revenue_dispatch9_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_ibi_fedtax_total_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_ibi_statax_total_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_ibi_total_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_ibi_total_fed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_ibi_total_oth_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_ibi_total_sta_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_ibi_total_uti_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_irr_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_itc_fed_total_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_itc_sta_total_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_itc_total_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_itc_total_fed_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_itc_total_sta_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_latcf_nom_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_latcf_real_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_lcoe_nom_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_lcoe_real_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_lcoptc_fed_nom_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_lcoptc_fed_real_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_lcoptc_sta_nom_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_lcoptc_sta_real_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_lppa_nom_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_lppa_real_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_min_cashflow_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_min_dscr_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_npv_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_ppa_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_ppa_escalation_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_present_value_fuel_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_present_value_insandproptax_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_present_value_oandm_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_present_value_oandm_nonfuel_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Ippppa_Outputs_wacc_nget(SAM_table ptr, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif