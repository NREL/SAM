#ifndef SAM_EQUPARTFLIP_H_
#define SAM_EQUPARTFLIP_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>

#ifdef __cplusplus
extern "C"
{
#endif

//
// Equpartflip Technology Model
//

/**
 * Create a Equpartflip variable table.
 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
 * @param[in,out] err: a pointer to an error object
 */

SAM_EXPORT typedef void *SAM_Equpartflip;

SAM_EXPORT SAM_Equpartflip SAM_Equpartflip_construct(const char *def, SAM_error *err);

/// verbosity level 0 or 1. Returns 1 on success
SAM_EXPORT int SAM_Equpartflip_execute(SAM_Equpartflip data, int verbosity, SAM_error *err);

SAM_EXPORT void SAM_Equpartflip_destruct(SAM_Equpartflip system);


//
// Revenue parameters
//

/**
 * Set ppa_escalation: PPA escalation rate [%/year]
 * options: None
 * constraints: None
 * required if: ?=0
 */
SAM_EXPORT void SAM_Equpartflip_Revenue_ppa_escalation_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set ppa_price_input: PPA price in first year [$/kWh]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_Equpartflip_Revenue_ppa_price_input_aset(SAM_Equpartflip ptr, double *arr, int length, SAM_error *err);

/**
 * Set ppa_soln_max: PPA solution maximum ppa [cents/kWh]
 * options: None
 * constraints: None
 * required if: ?=100
 */
SAM_EXPORT void SAM_Equpartflip_Revenue_ppa_soln_max_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set ppa_soln_max_iterations: PPA solution maximum number of iterations
 * options: None
 * constraints: INTEGER,MIN=1
 * required if: ?=100
 */
SAM_EXPORT void
SAM_Equpartflip_Revenue_ppa_soln_max_iterations_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set ppa_soln_min: PPA solution minimum ppa [cents/kWh]
 * options: None
 * constraints: None
 * required if: ?=0
 */
SAM_EXPORT void SAM_Equpartflip_Revenue_ppa_soln_min_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set ppa_soln_mode: PPA solution mode [0/1]
 * options: 0=solve ppa,1=specify ppa
 * constraints: INTEGER,MIN=0,MAX=1
 * required if: ?=0
 */
SAM_EXPORT void SAM_Equpartflip_Revenue_ppa_soln_mode_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set ppa_soln_tolerance: PPA solution tolerance
 * options: None
 * constraints: None
 * required if: ?=1e-5
 */
SAM_EXPORT void SAM_Equpartflip_Revenue_ppa_soln_tolerance_nset(SAM_Equpartflip ptr, double number, SAM_error *err);


//
// FinancialParameters parameters
//

/**
 * Set analysis_period: Analyis period [years]
 * options: None
 * constraints: INTEGER,MIN=0,MAX=50
 * required if: ?=30
 */
SAM_EXPORT void
SAM_Equpartflip_FinancialParameters_analysis_period_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set equip1_reserve_cost: Major equipment reserve 1 cost [$/W]
 * options: None
 * constraints: MIN=0
 * required if: ?=0.25
 */
SAM_EXPORT void
SAM_Equpartflip_FinancialParameters_equip1_reserve_cost_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set equip1_reserve_freq: Major equipment reserve 1 frequency [years]
 * options: None
 * constraints: INTEGER,MIN=0
 * required if: ?=12
 */
SAM_EXPORT void
SAM_Equpartflip_FinancialParameters_equip1_reserve_freq_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set equip2_reserve_cost: Major equipment reserve 2 cost [$/W]
 * options: None
 * constraints: MIN=0
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_FinancialParameters_equip2_reserve_cost_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set equip2_reserve_freq: Major equipment reserve 2 frequency [years]
 * options: None
 * constraints: INTEGER,MIN=0
 * required if: ?=15
 */
SAM_EXPORT void
SAM_Equpartflip_FinancialParameters_equip2_reserve_freq_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set equip3_reserve_cost: Major equipment reserve 3 cost [$/W]
 * options: None
 * constraints: MIN=0
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_FinancialParameters_equip3_reserve_cost_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set equip3_reserve_freq: Major equipment reserve 3 frequency [years]
 * options: None
 * constraints: INTEGER,MIN=0
 * required if: ?=20
 */
SAM_EXPORT void
SAM_Equpartflip_FinancialParameters_equip3_reserve_freq_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set equip_reserve_depr_fed: Major equipment reserve federal depreciation
 * options: 0=5yr MACRS,1=15yr MACRS,2=5yr SL,3=15yr SL, 4=20yr SL,5=39yr SL,6=Custom
 * constraints: INTEGER,MIN=0,MAX=6
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_FinancialParameters_equip_reserve_depr_fed_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set equip_reserve_depr_sta: Major equipment reserve state depreciation
 * options: 0=5yr MACRS,1=15yr MACRS,2=5yr SL,3=15yr SL, 4=20yr SL,5=39yr SL,6=Custom
 * constraints: INTEGER,MIN=0,MAX=6
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_FinancialParameters_equip_reserve_depr_sta_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set federal_tax_rate: Federal income tax rate [%]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_Equpartflip_FinancialParameters_federal_tax_rate_aset(SAM_Equpartflip ptr, double *arr, int length, SAM_error *err);

/**
 * Set inflation_rate: Inflation rate [%]
 * options: None
 * constraints: MIN=-99
 * required if: *
 */
SAM_EXPORT void
SAM_Equpartflip_FinancialParameters_inflation_rate_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set insurance_rate: Insurance rate [%]
 * options: None
 * constraints: MIN=0,MAX=100
 * required if: ?=0.0
 */
SAM_EXPORT void
SAM_Equpartflip_FinancialParameters_insurance_rate_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set prop_tax_assessed_decline: Assessed value annual decline [%]
 * options: None
 * constraints: MIN=0,MAX=100
 * required if: ?=5
 */
SAM_EXPORT void
SAM_Equpartflip_FinancialParameters_prop_tax_assessed_decline_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set prop_tax_cost_assessed_percent: Percent of pre-financing costs assessed [%]
 * options: None
 * constraints: MIN=0,MAX=100
 * required if: ?=95
 */
SAM_EXPORT void
SAM_Equpartflip_FinancialParameters_prop_tax_cost_assessed_percent_nset(SAM_Equpartflip ptr, double number,
                                                                        SAM_error *err);

/**
 * Set property_tax_rate: Property tax rate [%]
 * options: None
 * constraints: MIN=0,MAX=100
 * required if: ?=0.0
 */
SAM_EXPORT void
SAM_Equpartflip_FinancialParameters_property_tax_rate_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set real_discount_rate: Real discount rate [%]
 * options: None
 * constraints: MIN=-99
 * required if: *
 */
SAM_EXPORT void
SAM_Equpartflip_FinancialParameters_real_discount_rate_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set reserves_interest: Interest on reserves [%]
 * options: None
 * constraints: MIN=0,MAX=100
 * required if: ?=1.75
 */
SAM_EXPORT void
SAM_Equpartflip_FinancialParameters_reserves_interest_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set salvage_percentage: Net pre-tax cash salvage value [%]
 * options: None
 * constraints: MIN=0,MAX=100
 * required if: ?=10
 */
SAM_EXPORT void
SAM_Equpartflip_FinancialParameters_salvage_percentage_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set state_tax_rate: State income tax rate [%]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_Equpartflip_FinancialParameters_state_tax_rate_aset(SAM_Equpartflip ptr, double *arr, int length, SAM_error *err);

/**
 * Set system_capacity: System nameplate capacity [kW]
 * options: None
 * constraints: POSITIVE
 * required if: *
 */
SAM_EXPORT void
SAM_Equpartflip_FinancialParameters_system_capacity_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set system_heat_rate: System heat rate [MMBTus/MWh]
 * options: None
 * constraints: MIN=0
 * required if: ?=0.0
 */
SAM_EXPORT void
SAM_Equpartflip_FinancialParameters_system_heat_rate_nset(SAM_Equpartflip ptr, double number, SAM_error *err);


//
// SystemCosts parameters
//

/**
 * Set add_om_num_types: Number of O and M types
 * options: None
 * constraints: INTEGER,MIN=0,MAX=2
 * required if: ?=0
 */
SAM_EXPORT void SAM_Equpartflip_SystemCosts_add_om_num_types_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set annual_fuel_usage: Fuel usage (yr 1) [kWht]
 * options: None
 * constraints: MIN=0
 * required if: ?=0
 */
SAM_EXPORT void SAM_Equpartflip_SystemCosts_annual_fuel_usage_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set annual_fuel_usage_lifetime: Fuel usage (lifetime) [kWht]
 * options: None
 * constraints: None
 * required if: None
 */
SAM_EXPORT void
SAM_Equpartflip_SystemCosts_annual_fuel_usage_lifetime_aset(SAM_Equpartflip ptr, double *arr, int length,
                                                            SAM_error *err);

/**
 * Set om_capacity: Capacity-based O&M amount [$/kWcap]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void
SAM_Equpartflip_SystemCosts_om_capacity_aset(SAM_Equpartflip ptr, double *arr, int length, SAM_error *err);

/**
 * Set om_capacity1: Battery capacity-based System Costs amount [$/kWcap]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void
SAM_Equpartflip_SystemCosts_om_capacity1_aset(SAM_Equpartflip ptr, double *arr, int length, SAM_error *err);

/**
 * Set om_capacity1_nameplate: Battery capacity for System Costs values [kW]
 * options: None
 * constraints: None
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_SystemCosts_om_capacity1_nameplate_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set om_capacity2: Fuel cell capacity-based System Costs amount [$/kWcap]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void
SAM_Equpartflip_SystemCosts_om_capacity2_aset(SAM_Equpartflip ptr, double *arr, int length, SAM_error *err);

/**
 * Set om_capacity2_nameplate: Fuel cell capacity for System Costs values [kW]
 * options: None
 * constraints: None
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_SystemCosts_om_capacity2_nameplate_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set om_capacity_escal: Capacity-based O&M escalation [%/year]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Equpartflip_SystemCosts_om_capacity_escal_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set om_fixed: Fixed O&M annual amount [$/year]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Equpartflip_SystemCosts_om_fixed_aset(SAM_Equpartflip ptr, double *arr, int length, SAM_error *err);

/**
 * Set om_fixed1: Battery fixed System Costs annual amount [$/year]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void
SAM_Equpartflip_SystemCosts_om_fixed1_aset(SAM_Equpartflip ptr, double *arr, int length, SAM_error *err);

/**
 * Set om_fixed2: Fuel cell fixed System Costs annual amount [$/year]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void
SAM_Equpartflip_SystemCosts_om_fixed2_aset(SAM_Equpartflip ptr, double *arr, int length, SAM_error *err);

/**
 * Set om_fixed_escal: Fixed O&M escalation [%/year]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Equpartflip_SystemCosts_om_fixed_escal_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set om_fuel_cost: Fuel cost [$/MMBtu]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void
SAM_Equpartflip_SystemCosts_om_fuel_cost_aset(SAM_Equpartflip ptr, double *arr, int length, SAM_error *err);

/**
 * Set om_fuel_cost_escal: Fuel cost escalation [%/year]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Equpartflip_SystemCosts_om_fuel_cost_escal_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set om_opt_fuel_1_cost: Biomass feedstock cost [$/unit]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void
SAM_Equpartflip_SystemCosts_om_opt_fuel_1_cost_aset(SAM_Equpartflip ptr, double *arr, int length, SAM_error *err);

/**
 * Set om_opt_fuel_1_cost_escal: Biomass feedstock cost escalation [%/year]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void
SAM_Equpartflip_SystemCosts_om_opt_fuel_1_cost_escal_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set om_opt_fuel_1_usage: Biomass feedstock usage [unit]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void
SAM_Equpartflip_SystemCosts_om_opt_fuel_1_usage_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set om_opt_fuel_2_cost: Coal feedstock cost [$/unit]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void
SAM_Equpartflip_SystemCosts_om_opt_fuel_2_cost_aset(SAM_Equpartflip ptr, double *arr, int length, SAM_error *err);

/**
 * Set om_opt_fuel_2_cost_escal: Coal feedstock cost escalation [%/year]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void
SAM_Equpartflip_SystemCosts_om_opt_fuel_2_cost_escal_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set om_opt_fuel_2_usage: Coal feedstock usage [unit]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void
SAM_Equpartflip_SystemCosts_om_opt_fuel_2_usage_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set om_production: Production-based O&M amount [$/MWh]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void
SAM_Equpartflip_SystemCosts_om_production_aset(SAM_Equpartflip ptr, double *arr, int length, SAM_error *err);

/**
 * Set om_production1: Battery production-based System Costs amount [$/MWh]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void
SAM_Equpartflip_SystemCosts_om_production1_aset(SAM_Equpartflip ptr, double *arr, int length, SAM_error *err);

/**
 * Set om_production1_values: Battery production for System Costs values [kWh]
 * options: None
 * constraints: None
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_SystemCosts_om_production1_values_aset(SAM_Equpartflip ptr, double *arr, int length, SAM_error *err);

/**
 * Set om_production2: Fuel cell production-based System Costs amount [$/MWh]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void
SAM_Equpartflip_SystemCosts_om_production2_aset(SAM_Equpartflip ptr, double *arr, int length, SAM_error *err);

/**
 * Set om_production2_values: Fuel cell production for System Costs values [kWh]
 * options: None
 * constraints: None
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_SystemCosts_om_production2_values_aset(SAM_Equpartflip ptr, double *arr, int length, SAM_error *err);

/**
 * Set om_production_escal: Production-based O&M escalation [%/year]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void
SAM_Equpartflip_SystemCosts_om_production_escal_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set om_replacement_cost1: Replacement cost 1 [$/kWh]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void
SAM_Equpartflip_SystemCosts_om_replacement_cost1_aset(SAM_Equpartflip ptr, double *arr, int length, SAM_error *err);

/**
 * Set om_replacement_cost2: Replacement cost 2 [$/kW]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void
SAM_Equpartflip_SystemCosts_om_replacement_cost2_aset(SAM_Equpartflip ptr, double *arr, int length, SAM_error *err);

/**
 * Set om_replacement_cost_escal: Replacement cost escalation [%/year]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void
SAM_Equpartflip_SystemCosts_om_replacement_cost_escal_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set total_installed_cost: Installed cost [$]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_Equpartflip_SystemCosts_total_installed_cost_nset(SAM_Equpartflip ptr, double number, SAM_error *err);


//
// TaxCreditIncentives parameters
//

/**
 * Set itc_fed_amount: Federal amount-based ITC amount [$]
 * options: None
 * constraints: None
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_TaxCreditIncentives_itc_fed_amount_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set itc_fed_amount_deprbas_fed: Federal amount-based ITC reduces federal depreciation basis [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=1
 */
SAM_EXPORT void
SAM_Equpartflip_TaxCreditIncentives_itc_fed_amount_deprbas_fed_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set itc_fed_amount_deprbas_sta: Federal amount-based ITC reduces state depreciation basis [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=1
 */
SAM_EXPORT void
SAM_Equpartflip_TaxCreditIncentives_itc_fed_amount_deprbas_sta_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set itc_fed_percent: Federal percentage-based ITC percent [%]
 * options: None
 * constraints: None
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_TaxCreditIncentives_itc_fed_percent_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set itc_fed_percent_deprbas_fed: Federal percentage-based ITC reduces federal depreciation basis [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=1
 */
SAM_EXPORT void SAM_Equpartflip_TaxCreditIncentives_itc_fed_percent_deprbas_fed_nset(SAM_Equpartflip ptr, double number,
                                                                                     SAM_error *err);

/**
 * Set itc_fed_percent_deprbas_sta: Federal percentage-based ITC reduces state depreciation basis [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=1
 */
SAM_EXPORT void SAM_Equpartflip_TaxCreditIncentives_itc_fed_percent_deprbas_sta_nset(SAM_Equpartflip ptr, double number,
                                                                                     SAM_error *err);

/**
 * Set itc_fed_percent_maxvalue: Federal percentage-based ITC maximum value [$]
 * options: None
 * constraints: None
 * required if: ?=1e99
 */
SAM_EXPORT void
SAM_Equpartflip_TaxCreditIncentives_itc_fed_percent_maxvalue_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set itc_sta_amount: State amount-based ITC amount [$]
 * options: None
 * constraints: None
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_TaxCreditIncentives_itc_sta_amount_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set itc_sta_amount_deprbas_fed: State amount-based ITC reduces federal depreciation basis [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_TaxCreditIncentives_itc_sta_amount_deprbas_fed_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set itc_sta_amount_deprbas_sta: State amount-based ITC reduces state depreciation basis [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_TaxCreditIncentives_itc_sta_amount_deprbas_sta_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set itc_sta_percent: State percentage-based ITC percent [%]
 * options: None
 * constraints: None
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_TaxCreditIncentives_itc_sta_percent_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set itc_sta_percent_deprbas_fed: State percentage-based ITC reduces federal depreciation basis [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=0
 */
SAM_EXPORT void SAM_Equpartflip_TaxCreditIncentives_itc_sta_percent_deprbas_fed_nset(SAM_Equpartflip ptr, double number,
                                                                                     SAM_error *err);

/**
 * Set itc_sta_percent_deprbas_sta: State percentage-based ITC reduces state depreciation basis [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=0
 */
SAM_EXPORT void SAM_Equpartflip_TaxCreditIncentives_itc_sta_percent_deprbas_sta_nset(SAM_Equpartflip ptr, double number,
                                                                                     SAM_error *err);

/**
 * Set itc_sta_percent_maxvalue: State percentage-based ITC maximum Value [$]
 * options: None
 * constraints: None
 * required if: ?=1e99
 */
SAM_EXPORT void
SAM_Equpartflip_TaxCreditIncentives_itc_sta_percent_maxvalue_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set ptc_fed_amount: Federal PTC amount [$/kWh]
 * options: None
 * constraints: None
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_TaxCreditIncentives_ptc_fed_amount_aset(SAM_Equpartflip ptr, double *arr, int length, SAM_error *err);

/**
 * Set ptc_fed_escal: Federal PTC escalation [%/year]
 * options: None
 * constraints: None
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_TaxCreditIncentives_ptc_fed_escal_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set ptc_fed_term: Federal PTC term [years]
 * options: None
 * constraints: None
 * required if: ?=10
 */
SAM_EXPORT void
SAM_Equpartflip_TaxCreditIncentives_ptc_fed_term_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set ptc_sta_amount: State PTC amount [$/kWh]
 * options: None
 * constraints: None
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_TaxCreditIncentives_ptc_sta_amount_aset(SAM_Equpartflip ptr, double *arr, int length, SAM_error *err);

/**
 * Set ptc_sta_escal: State PTC escalation [%/year]
 * options: None
 * constraints: None
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_TaxCreditIncentives_ptc_sta_escal_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set ptc_sta_term: State PTC term [years]
 * options: None
 * constraints: None
 * required if: ?=10
 */
SAM_EXPORT void
SAM_Equpartflip_TaxCreditIncentives_ptc_sta_term_nset(SAM_Equpartflip ptr, double number, SAM_error *err);


//
// Depreciation parameters
//

/**
 * Set depr_alloc_custom_percent: Custom depreciation federal and state allocation [%]
 * options: None
 * constraints: MIN=0,MAX=100
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_Depreciation_depr_alloc_custom_percent_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set depr_alloc_macrs_15_percent: 15-yr MACRS depreciation federal and state allocation [%]
 * options: None
 * constraints: MIN=0,MAX=100
 * required if: ?=1.5
 */
SAM_EXPORT void
SAM_Equpartflip_Depreciation_depr_alloc_macrs_15_percent_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set depr_alloc_macrs_5_percent: 5-yr MACRS depreciation federal and state allocation [%]
 * options: None
 * constraints: MIN=0,MAX=100
 * required if: ?=89
 */
SAM_EXPORT void
SAM_Equpartflip_Depreciation_depr_alloc_macrs_5_percent_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set depr_alloc_sl_15_percent: 15-yr straight line depreciation federal and state allocation [%]
 * options: None
 * constraints: MIN=0,MAX=100
 * required if: ?=3
 */
SAM_EXPORT void
SAM_Equpartflip_Depreciation_depr_alloc_sl_15_percent_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set depr_alloc_sl_20_percent: 20-yr straight line depreciation federal and state allocation [%]
 * options: None
 * constraints: MIN=0,MAX=100
 * required if: ?=3
 */
SAM_EXPORT void
SAM_Equpartflip_Depreciation_depr_alloc_sl_20_percent_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set depr_alloc_sl_39_percent: 39-yr straight line depreciation federal and state allocation [%]
 * options: None
 * constraints: MIN=0,MAX=100
 * required if: ?=0.5
 */
SAM_EXPORT void
SAM_Equpartflip_Depreciation_depr_alloc_sl_39_percent_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set depr_alloc_sl_5_percent: 5-yr straight line depreciation federal and state allocation [%]
 * options: None
 * constraints: MIN=0,MAX=100
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_Depreciation_depr_alloc_sl_5_percent_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set depr_bonus_fed: Federal bonus depreciation [%]
 * options: None
 * constraints: MIN=0,MAX=100
 * required if: ?=0
 */
SAM_EXPORT void SAM_Equpartflip_Depreciation_depr_bonus_fed_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set depr_bonus_fed_custom: Federal bonus depreciation custom [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_Depreciation_depr_bonus_fed_custom_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set depr_bonus_fed_macrs_15: Federal bonus depreciation 15-yr MACRS [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_Depreciation_depr_bonus_fed_macrs_15_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set depr_bonus_fed_macrs_5: Federal bonus depreciation 5-yr MACRS [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=1
 */
SAM_EXPORT void
SAM_Equpartflip_Depreciation_depr_bonus_fed_macrs_5_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set depr_bonus_fed_sl_15: Federal bonus depreciation 15-yr straight line [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_Depreciation_depr_bonus_fed_sl_15_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set depr_bonus_fed_sl_20: Federal bonus depreciation 20-yr straight line [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_Depreciation_depr_bonus_fed_sl_20_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set depr_bonus_fed_sl_39: Federal bonus depreciation 39-yr straight line [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_Depreciation_depr_bonus_fed_sl_39_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set depr_bonus_fed_sl_5: Federal bonus depreciation 5-yr straight line [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_Depreciation_depr_bonus_fed_sl_5_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set depr_bonus_sta: State bonus depreciation [%]
 * options: None
 * constraints: MIN=0,MAX=100
 * required if: ?=0
 */
SAM_EXPORT void SAM_Equpartflip_Depreciation_depr_bonus_sta_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set depr_bonus_sta_custom: State bonus depreciation custom [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_Depreciation_depr_bonus_sta_custom_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set depr_bonus_sta_macrs_15: State bonus depreciation 15-yr MACRS [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_Depreciation_depr_bonus_sta_macrs_15_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set depr_bonus_sta_macrs_5: State bonus depreciation 5-yr MACRS [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=1
 */
SAM_EXPORT void
SAM_Equpartflip_Depreciation_depr_bonus_sta_macrs_5_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set depr_bonus_sta_sl_15: State bonus depreciation 15-yr straight line [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_Depreciation_depr_bonus_sta_sl_15_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set depr_bonus_sta_sl_20: State bonus depreciation 20-yr straight line [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_Depreciation_depr_bonus_sta_sl_20_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set depr_bonus_sta_sl_39: State bonus depreciation 39-yr straight line [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_Depreciation_depr_bonus_sta_sl_39_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set depr_bonus_sta_sl_5: State bonus depreciation 5-yr straight line [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_Depreciation_depr_bonus_sta_sl_5_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set depr_custom_schedule: Custom depreciation schedule [%]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_Equpartflip_Depreciation_depr_custom_schedule_aset(SAM_Equpartflip ptr, double *arr, int length, SAM_error *err);

/**
 * Set depr_fedbas_method: Method of federal depreciation reduction
 * options: 0=5yr MACRS,1=Proportional
 * constraints: INTEGER,MIN=0,MAX=1
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_Depreciation_depr_fedbas_method_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set depr_itc_fed_custom: Federal ITC depreciation custom [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_Depreciation_depr_itc_fed_custom_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set depr_itc_fed_macrs_15: Federal ITC depreciation 15-yr MACRS [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_Depreciation_depr_itc_fed_macrs_15_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set depr_itc_fed_macrs_5: Federal ITC depreciation 5-yr MACRS [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=1
 */
SAM_EXPORT void
SAM_Equpartflip_Depreciation_depr_itc_fed_macrs_5_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set depr_itc_fed_sl_15: Federal ITC depreciation 15-yr straight line [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_Depreciation_depr_itc_fed_sl_15_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set depr_itc_fed_sl_20: Federal ITC depreciation 20-yr straight line [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_Depreciation_depr_itc_fed_sl_20_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set depr_itc_fed_sl_39: Federal ITC depreciation 39-yr straight line [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_Depreciation_depr_itc_fed_sl_39_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set depr_itc_fed_sl_5: Federal ITC depreciation 5-yr straight line [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=0
 */
SAM_EXPORT void SAM_Equpartflip_Depreciation_depr_itc_fed_sl_5_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set depr_itc_sta_custom: State ITC depreciation custom [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_Depreciation_depr_itc_sta_custom_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set depr_itc_sta_macrs_15: State ITC depreciation 15-yr MACRS [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_Depreciation_depr_itc_sta_macrs_15_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set depr_itc_sta_macrs_5: State ITC depreciation 5-yr MACRS [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=1
 */
SAM_EXPORT void
SAM_Equpartflip_Depreciation_depr_itc_sta_macrs_5_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set depr_itc_sta_sl_15: State ITC depreciation 15-yr straight line [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_Depreciation_depr_itc_sta_sl_15_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set depr_itc_sta_sl_20: State ITC depreciation 20-yr straight line [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_Depreciation_depr_itc_sta_sl_20_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set depr_itc_sta_sl_39: State ITC depreciation 39-yr straight line [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_Depreciation_depr_itc_sta_sl_39_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set depr_itc_sta_sl_5: State ITC depreciation 5-yr straight line [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=0
 */
SAM_EXPORT void SAM_Equpartflip_Depreciation_depr_itc_sta_sl_5_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set depr_stabas_method: Method of state depreciation reduction
 * options: 0=5yr MACRS,1=Proportional
 * constraints: INTEGER,MIN=0,MAX=1
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_Depreciation_depr_stabas_method_nset(SAM_Equpartflip ptr, double number, SAM_error *err);


//
// PaymentIncentives parameters
//

/**
 * Set cbi_fed_amount: Federal CBI amount [$/Watt]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_cbi_fed_amount_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set cbi_fed_deprbas_fed: Federal CBI reduces federal depreciation basis [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_cbi_fed_deprbas_fed_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set cbi_fed_deprbas_sta: Federal CBI reduces state depreciation basis [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_cbi_fed_deprbas_sta_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set cbi_fed_maxvalue: Federal CBI maximum [$]
 * options: None
 * constraints: None
 * required if: ?=1e99
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_cbi_fed_maxvalue_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set cbi_fed_tax_fed: Federal CBI federal taxable [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=1
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_cbi_fed_tax_fed_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set cbi_fed_tax_sta: Federal CBI state taxable [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=1
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_cbi_fed_tax_sta_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set cbi_oth_amount: Other CBI amount [$/Watt]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_cbi_oth_amount_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set cbi_oth_deprbas_fed: Other CBI reduces federal depreciation basis [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_cbi_oth_deprbas_fed_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set cbi_oth_deprbas_sta: Other CBI reduces state depreciation basis [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_cbi_oth_deprbas_sta_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set cbi_oth_maxvalue: Other CBI maximum [$]
 * options: None
 * constraints: None
 * required if: ?=1e99
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_cbi_oth_maxvalue_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set cbi_oth_tax_fed: Other CBI federal taxable [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=1
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_cbi_oth_tax_fed_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set cbi_oth_tax_sta: Other CBI state taxable [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=1
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_cbi_oth_tax_sta_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set cbi_sta_amount: State CBI amount [$/Watt]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_cbi_sta_amount_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set cbi_sta_deprbas_fed: State CBI reduces federal depreciation basis [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_cbi_sta_deprbas_fed_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set cbi_sta_deprbas_sta: State CBI reduces state depreciation basis [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_cbi_sta_deprbas_sta_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set cbi_sta_maxvalue: State CBI maximum [$]
 * options: None
 * constraints: None
 * required if: ?=1e99
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_cbi_sta_maxvalue_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set cbi_sta_tax_fed: State CBI federal taxable [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=1
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_cbi_sta_tax_fed_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set cbi_sta_tax_sta: State CBI state taxable [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=1
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_cbi_sta_tax_sta_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set cbi_uti_amount: Utility CBI amount [$/Watt]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_cbi_uti_amount_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set cbi_uti_deprbas_fed: Utility CBI reduces federal depreciation basis [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_cbi_uti_deprbas_fed_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set cbi_uti_deprbas_sta: Utility CBI reduces state depreciation basis [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_cbi_uti_deprbas_sta_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set cbi_uti_maxvalue: Utility CBI maximum [$]
 * options: None
 * constraints: None
 * required if: ?=1e99
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_cbi_uti_maxvalue_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set cbi_uti_tax_fed: Utility CBI federal taxable [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=1
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_cbi_uti_tax_fed_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set cbi_uti_tax_sta: Utility CBI state taxable [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=1
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_cbi_uti_tax_sta_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set ibi_fed_amount: Federal amount-based IBI amount [$]
 * options: None
 * constraints: None
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_ibi_fed_amount_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set ibi_fed_amount_deprbas_fed: Federal amount-based IBI reduces federal depreciation basis [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_ibi_fed_amount_deprbas_fed_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set ibi_fed_amount_deprbas_sta: Federal amount-based IBI reduces state depreciation basis [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_ibi_fed_amount_deprbas_sta_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set ibi_fed_amount_tax_fed: Federal amount-based IBI federal taxable [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=1
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_ibi_fed_amount_tax_fed_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set ibi_fed_amount_tax_sta: Federal amount-based IBI state taxable [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=1
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_ibi_fed_amount_tax_sta_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set ibi_fed_percent: Federal percentage-based IBI percent [%]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_ibi_fed_percent_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set ibi_fed_percent_deprbas_fed: Federal percentage-based IBI reduces federal depreciation basis [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_ibi_fed_percent_deprbas_fed_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set ibi_fed_percent_deprbas_sta: Federal percentage-based IBI reduces state depreciation basis [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_ibi_fed_percent_deprbas_sta_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set ibi_fed_percent_maxvalue: Federal percentage-based IBI maximum value [$]
 * options: None
 * constraints: None
 * required if: ?=1e99
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_ibi_fed_percent_maxvalue_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set ibi_fed_percent_tax_fed: Federal percentage-based IBI federal taxable [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=1
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_ibi_fed_percent_tax_fed_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set ibi_fed_percent_tax_sta: Federal percentage-based IBI state taxable [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=1
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_ibi_fed_percent_tax_sta_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set ibi_oth_amount: Other amount-based IBI amount [$]
 * options: None
 * constraints: None
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_ibi_oth_amount_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set ibi_oth_amount_deprbas_fed: Other amount-based IBI reduces federal depreciation basis [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_ibi_oth_amount_deprbas_fed_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set ibi_oth_amount_deprbas_sta: Other amount-based IBI reduces state depreciation basis [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_ibi_oth_amount_deprbas_sta_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set ibi_oth_amount_tax_fed: Other amount-based IBI federal taxable [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=1
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_ibi_oth_amount_tax_fed_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set ibi_oth_amount_tax_sta: Other amount-based IBI state taxable [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=1
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_ibi_oth_amount_tax_sta_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set ibi_oth_percent: Other percentage-based IBI percent [%]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_ibi_oth_percent_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set ibi_oth_percent_deprbas_fed: Other percentage-based IBI reduces federal depreciation basis [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_ibi_oth_percent_deprbas_fed_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set ibi_oth_percent_deprbas_sta: Other percentage-based IBI reduces state depreciation basis [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_ibi_oth_percent_deprbas_sta_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set ibi_oth_percent_maxvalue: Other percentage-based IBI maximum value [$]
 * options: None
 * constraints: None
 * required if: ?=1e99
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_ibi_oth_percent_maxvalue_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set ibi_oth_percent_tax_fed: Other percentage-based IBI federal taxable [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=1
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_ibi_oth_percent_tax_fed_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set ibi_oth_percent_tax_sta: Other percentage-based IBI state taxable [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=1
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_ibi_oth_percent_tax_sta_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set ibi_sta_amount: State amount-based IBI amount [$]
 * options: None
 * constraints: None
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_ibi_sta_amount_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set ibi_sta_amount_deprbas_fed: State amount-based IBI reduces federal depreciation basis [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_ibi_sta_amount_deprbas_fed_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set ibi_sta_amount_deprbas_sta: State amount-based IBI reduces state depreciation basis [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_ibi_sta_amount_deprbas_sta_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set ibi_sta_amount_tax_fed: State amount-based IBI federal taxable [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=1
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_ibi_sta_amount_tax_fed_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set ibi_sta_amount_tax_sta: State amount-based IBI state taxable [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=1
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_ibi_sta_amount_tax_sta_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set ibi_sta_percent: State percentage-based IBI percent [%]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_ibi_sta_percent_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set ibi_sta_percent_deprbas_fed: State percentage-based IBI reduces federal depreciation basis [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_ibi_sta_percent_deprbas_fed_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set ibi_sta_percent_deprbas_sta: State percentage-based IBI reduces state depreciation basis [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_ibi_sta_percent_deprbas_sta_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set ibi_sta_percent_maxvalue: State percentage-based IBI maximum value [$]
 * options: None
 * constraints: None
 * required if: ?=1e99
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_ibi_sta_percent_maxvalue_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set ibi_sta_percent_tax_fed: State percentage-based IBI federal taxable [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=1
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_ibi_sta_percent_tax_fed_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set ibi_sta_percent_tax_sta: State percentage-based IBI state taxable [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=1
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_ibi_sta_percent_tax_sta_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set ibi_uti_amount: Utility amount-based IBI amount [$]
 * options: None
 * constraints: None
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_ibi_uti_amount_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set ibi_uti_amount_deprbas_fed: Utility amount-based IBI reduces federal depreciation basis [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_ibi_uti_amount_deprbas_fed_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set ibi_uti_amount_deprbas_sta: Utility amount-based IBI reduces state depreciation basis [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_ibi_uti_amount_deprbas_sta_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set ibi_uti_amount_tax_fed: Utility amount-based IBI federal taxable [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=1
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_ibi_uti_amount_tax_fed_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set ibi_uti_amount_tax_sta: Utility amount-based IBI state taxable [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=1
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_ibi_uti_amount_tax_sta_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set ibi_uti_percent: Utility percentage-based IBI percent [%]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_ibi_uti_percent_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set ibi_uti_percent_deprbas_fed: Utility percentage-based IBI reduces federal depreciation basis [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_ibi_uti_percent_deprbas_fed_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set ibi_uti_percent_deprbas_sta: Utility percentage-based IBI reduces state depreciation basis [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_ibi_uti_percent_deprbas_sta_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set ibi_uti_percent_maxvalue: Utility percentage-based IBI maximum value [$]
 * options: None
 * constraints: None
 * required if: ?=1e99
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_ibi_uti_percent_maxvalue_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set ibi_uti_percent_tax_fed: Utility percentage-based IBI federal taxable [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=1
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_ibi_uti_percent_tax_fed_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set ibi_uti_percent_tax_sta: Utility percentage-based IBI state taxable [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=1
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_ibi_uti_percent_tax_sta_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set pbi_fed_amount: Federal PBI amount [$/kWh]
 * options: None
 * constraints: None
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_pbi_fed_amount_aset(SAM_Equpartflip ptr, double *arr, int length, SAM_error *err);

/**
 * Set pbi_fed_escal: Federal PBI escalation [%]
 * options: None
 * constraints: None
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_pbi_fed_escal_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set pbi_fed_tax_fed: Federal PBI federal taxable [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=1
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_pbi_fed_tax_fed_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set pbi_fed_tax_sta: Federal PBI state taxable [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=1
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_pbi_fed_tax_sta_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set pbi_fed_term: Federal PBI term [years]
 * options: None
 * constraints: None
 * required if: ?=0
 */
SAM_EXPORT void SAM_Equpartflip_PaymentIncentives_pbi_fed_term_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set pbi_oth_amount: Other PBI amount [$/kWh]
 * options: None
 * constraints: None
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_pbi_oth_amount_aset(SAM_Equpartflip ptr, double *arr, int length, SAM_error *err);

/**
 * Set pbi_oth_escal: Other PBI escalation [%]
 * options: None
 * constraints: None
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_pbi_oth_escal_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set pbi_oth_tax_fed: Other PBI federal taxable [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=1
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_pbi_oth_tax_fed_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set pbi_oth_tax_sta: Other PBI state taxable [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=1
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_pbi_oth_tax_sta_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set pbi_oth_term: Other PBI term [years]
 * options: None
 * constraints: None
 * required if: ?=0
 */
SAM_EXPORT void SAM_Equpartflip_PaymentIncentives_pbi_oth_term_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set pbi_sta_amount: State PBI amount [$/kWh]
 * options: None
 * constraints: None
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_pbi_sta_amount_aset(SAM_Equpartflip ptr, double *arr, int length, SAM_error *err);

/**
 * Set pbi_sta_escal: State PBI escalation [%]
 * options: None
 * constraints: None
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_pbi_sta_escal_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set pbi_sta_tax_fed: State PBI federal taxable [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=1
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_pbi_sta_tax_fed_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set pbi_sta_tax_sta: State PBI state taxable [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=1
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_pbi_sta_tax_sta_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set pbi_sta_term: State PBI term [years]
 * options: None
 * constraints: None
 * required if: ?=0
 */
SAM_EXPORT void SAM_Equpartflip_PaymentIncentives_pbi_sta_term_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set pbi_uti_amount: Utility PBI amount [$/kWh]
 * options: None
 * constraints: None
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_pbi_uti_amount_aset(SAM_Equpartflip ptr, double *arr, int length, SAM_error *err);

/**
 * Set pbi_uti_escal: Utility PBI escalation [%]
 * options: None
 * constraints: None
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_pbi_uti_escal_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set pbi_uti_tax_fed: Utility PBI federal taxable [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=1
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_pbi_uti_tax_fed_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set pbi_uti_tax_sta: Utility PBI state taxable [0/1]
 * options: None
 * constraints: BOOLEAN
 * required if: ?=1
 */
SAM_EXPORT void
SAM_Equpartflip_PaymentIncentives_pbi_uti_tax_sta_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set pbi_uti_term: Utility PBI term [years]
 * options: None
 * constraints: None
 * required if: ?=0
 */
SAM_EXPORT void SAM_Equpartflip_PaymentIncentives_pbi_uti_term_nset(SAM_Equpartflip ptr, double number, SAM_error *err);


//
// SystemOutput parameters
//

/**
 * Set degradation: Annual energy degradation
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_Equpartflip_SystemOutput_degradation_aset(SAM_Equpartflip ptr, double *arr, int length, SAM_error *err);

/**
 * Set gen: Power generated by renewable resource [kW]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Equpartflip_SystemOutput_gen_aset(SAM_Equpartflip ptr, double *arr, int length, SAM_error *err);

/**
 * Set system_capacity: System nameplate capacity [kW]
 * options: None
 * constraints: MIN=1e-3
 * required if: *
 */
SAM_EXPORT void SAM_Equpartflip_SystemOutput_system_capacity_nset(SAM_Equpartflip ptr, double number, SAM_error *err);


//
// Recapitalization parameters
//

/**
 * Set system_lifetime_recapitalize: Recapitalization boolean
 * options: None
 * constraints: None
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_Recapitalization_system_lifetime_recapitalize_aset(SAM_Equpartflip ptr, double *arr, int length,
                                                                   SAM_error *err);

/**
 * Set system_recapitalization_cost: Recapitalization cost [$]
 * options: None
 * constraints: None
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_Recapitalization_system_recapitalization_cost_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set system_recapitalization_escalation: Recapitalization escalation (above inflation) [%]
 * options: None
 * constraints: MIN=0,MAX=100
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_Recapitalization_system_recapitalization_escalation_nset(SAM_Equpartflip ptr, double number,
                                                                         SAM_error *err);

/**
 * Set system_use_recapitalization: Recapitalization expenses [0/1]
 * options: 0=None,1=Recapitalize
 * constraints: INTEGER,MIN=0
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_Recapitalization_system_use_recapitalization_nset(SAM_Equpartflip ptr, double number, SAM_error *err);


//
// TimeOfDelivery parameters
//

/**
 * Set dispatch_factor1: TOD factor for period 1
 * options: None
 * constraints: None
 * required if: ppa_multiplier_model=0
 */
SAM_EXPORT void
SAM_Equpartflip_TimeOfDelivery_dispatch_factor1_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set dispatch_factor2: TOD factor for period 2
 * options: None
 * constraints: None
 * required if: ppa_multiplier_model=0
 */
SAM_EXPORT void
SAM_Equpartflip_TimeOfDelivery_dispatch_factor2_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set dispatch_factor3: TOD factor for period 3
 * options: None
 * constraints: None
 * required if: ppa_multiplier_model=0
 */
SAM_EXPORT void
SAM_Equpartflip_TimeOfDelivery_dispatch_factor3_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set dispatch_factor4: TOD factor for period 4
 * options: None
 * constraints: None
 * required if: ppa_multiplier_model=0
 */
SAM_EXPORT void
SAM_Equpartflip_TimeOfDelivery_dispatch_factor4_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set dispatch_factor5: TOD factor for period 5
 * options: None
 * constraints: None
 * required if: ppa_multiplier_model=0
 */
SAM_EXPORT void
SAM_Equpartflip_TimeOfDelivery_dispatch_factor5_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set dispatch_factor6: TOD factor for period 6
 * options: None
 * constraints: None
 * required if: ppa_multiplier_model=0
 */
SAM_EXPORT void
SAM_Equpartflip_TimeOfDelivery_dispatch_factor6_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set dispatch_factor7: TOD factor for period 7
 * options: None
 * constraints: None
 * required if: ppa_multiplier_model=0
 */
SAM_EXPORT void
SAM_Equpartflip_TimeOfDelivery_dispatch_factor7_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set dispatch_factor8: TOD factor for period 8
 * options: None
 * constraints: None
 * required if: ppa_multiplier_model=0
 */
SAM_EXPORT void
SAM_Equpartflip_TimeOfDelivery_dispatch_factor8_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set dispatch_factor9: TOD factor for period 9
 * options: None
 * constraints: None
 * required if: ppa_multiplier_model=0
 */
SAM_EXPORT void
SAM_Equpartflip_TimeOfDelivery_dispatch_factor9_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set dispatch_factors_ts: Dispatch payment factor array
 * options: None
 * constraints: None
 * required if: ppa_multiplier_model=1
 */
SAM_EXPORT void
SAM_Equpartflip_TimeOfDelivery_dispatch_factors_ts_aset(SAM_Equpartflip ptr, double *arr, int length, SAM_error *err);

/**
 * Set dispatch_sched_weekday: Diurnal weekday TOD periods [1..9]
 * options: 12 x 24 matrix
 * constraints: None
 * required if: ppa_multiplier_model=0
 */
SAM_EXPORT void
SAM_Equpartflip_TimeOfDelivery_dispatch_sched_weekday_mset(SAM_Equpartflip ptr, double *mat, int nrows, int ncols,
                                                           SAM_error *err);

/**
 * Set dispatch_sched_weekend: Diurnal weekend TOD periods [1..9]
 * options: 12 x 24 matrix
 * constraints: None
 * required if: ppa_multiplier_model=0
 */
SAM_EXPORT void
SAM_Equpartflip_TimeOfDelivery_dispatch_sched_weekend_mset(SAM_Equpartflip ptr, double *mat, int nrows, int ncols,
                                                           SAM_error *err);

/**
 * Set ppa_multiplier_model: PPA multiplier model [0/1]
 * options: 0=diurnal,1=timestep
 * constraints: INTEGER,MIN=0
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_TimeOfDelivery_ppa_multiplier_model_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set system_use_lifetime_output: Lifetime hourly system outputs [0/1]
 * options: 0=hourly first year,1=hourly lifetime
 * constraints: INTEGER,MIN=0
 * required if: *
 */
SAM_EXPORT void
SAM_Equpartflip_TimeOfDelivery_system_use_lifetime_output_nset(SAM_Equpartflip ptr, double number, SAM_error *err);


//
// ConstructionFinancing parameters
//

/**
 * Set construction_financing_cost: Construction financing total [$]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void
SAM_Equpartflip_ConstructionFinancing_construction_financing_cost_nset(SAM_Equpartflip ptr, double number,
                                                                       SAM_error *err);


//
// OtherCapitalCosts parameters
//

/**
 * Set cost_dev_fee_percent: Development fee (% pre-financing cost) [%]
 * options: None
 * constraints: MIN=0,MAX=100
 * required if: ?=3
 */
SAM_EXPORT void
SAM_Equpartflip_OtherCapitalCosts_cost_dev_fee_percent_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set cost_equity_closing: Equity closing cost [$]
 * options: None
 * constraints: MIN=0
 * required if: ?=100000
 */
SAM_EXPORT void
SAM_Equpartflip_OtherCapitalCosts_cost_equity_closing_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set cost_other_financing:  [$]
 * options: Other financing cost
 * constraints: MIN=0
 * required if: ?=150000
 */
SAM_EXPORT void
SAM_Equpartflip_OtherCapitalCosts_cost_other_financing_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set months_receivables_reserve: Receivables reserve months of PPA revenue [months]
 * options: None
 * constraints: MIN=0
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_OtherCapitalCosts_months_receivables_reserve_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set months_working_reserve: Working capital reserve months of operating costs [months]
 * options: None
 * constraints: MIN=0
 * required if: ?=6
 */
SAM_EXPORT void
SAM_Equpartflip_OtherCapitalCosts_months_working_reserve_nset(SAM_Equpartflip ptr, double number, SAM_error *err);


//
// IRRTargets parameters
//

/**
 * Set flip_target_percent: Investor percent of project benefit [%]
 * options: None
 * constraints: MIN=0,MAX=100
 * required if: ?=11
 */
SAM_EXPORT void SAM_Equpartflip_IRRTargets_flip_target_percent_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set flip_target_year: Target flip year
 * options: None
 * constraints: MIN=1
 * required if: ?=11
 */
SAM_EXPORT void SAM_Equpartflip_IRRTargets_flip_target_year_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set tax_investor_equity_percent: Investor equity [%]
 * options: None
 * constraints: MIN=0,MAX=100
 * required if: ?=98
 */
SAM_EXPORT void
SAM_Equpartflip_IRRTargets_tax_investor_equity_percent_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set tax_investor_postflip_cash_percent: Investor post-flip cash  [%]
 * options: None
 * constraints: MIN=0,MAX=100
 * required if: ?=15
 */
SAM_EXPORT void
SAM_Equpartflip_IRRTargets_tax_investor_postflip_cash_percent_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set tax_investor_postflip_tax_percent: Investor post-flip tax benefit  [%]
 * options: None
 * constraints: MIN=0,MAX=100
 * required if: ?=15
 */
SAM_EXPORT void
SAM_Equpartflip_IRRTargets_tax_investor_postflip_tax_percent_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set tax_investor_preflip_cash_percent: Investor pre-flip cash  [%]
 * options: None
 * constraints: MIN=0,MAX=100
 * required if: ?=98
 */
SAM_EXPORT void
SAM_Equpartflip_IRRTargets_tax_investor_preflip_cash_percent_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set tax_investor_preflip_tax_percent: Investor pre-flip tax benefit  [%]
 * options: None
 * constraints: MIN=0,MAX=100
 * required if: ?=98
 */
SAM_EXPORT void
SAM_Equpartflip_IRRTargets_tax_investor_preflip_tax_percent_nset(SAM_Equpartflip ptr, double number, SAM_error *err);


//
// DeveloperCapitalRecovery parameters
//

/**
 * Set sponsor_cap_recovery_mode: Developer Capital Recovery
 * options: 0=Time, 1=Full Capital Recovery
 * constraints: INTEGER,MIN=0,MAX=1
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_DeveloperCapitalRecovery_sponsor_cap_recovery_mode_nset(SAM_Equpartflip ptr, double number,
                                                                        SAM_error *err);

/**
 * Set sponsor_cap_recovery_year: Duration (in years) [years]
 * options: None
 * constraints: INTEGER
 * required if: ?=3
 */
SAM_EXPORT void
SAM_Equpartflip_DeveloperCapitalRecovery_sponsor_cap_recovery_year_nset(SAM_Equpartflip ptr, double number,
                                                                        SAM_error *err);


//
// BatterySystem parameters
//

/**
 * Set batt_bank_replacement: Battery bank replacements per year [number/year]
 * options: None
 * constraints: None
 * required if: None
 */
SAM_EXPORT void
SAM_Equpartflip_BatterySystem_batt_bank_replacement_aset(SAM_Equpartflip ptr, double *arr, int length, SAM_error *err);

/**
 * Set batt_computed_bank_capacity: Battery bank capacity [kWh]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void
SAM_Equpartflip_BatterySystem_batt_computed_bank_capacity_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set batt_replacement_option: Enable battery replacement? [0=none,1=capacity based,2=user schedule]
 * options: None
 * constraints: INTEGER,MIN=0,MAX=2
 * required if: ?=0
 */
SAM_EXPORT void
SAM_Equpartflip_BatterySystem_batt_replacement_option_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set batt_replacement_schedule: Battery bank replacements per year (user specified) [number/year]
 * options: None
 * constraints: None
 * required if: None
 */
SAM_EXPORT void
SAM_Equpartflip_BatterySystem_batt_replacement_schedule_aset(SAM_Equpartflip ptr, double *arr, int length,
                                                             SAM_error *err);

/**
 * Set battery_per_kWh: Battery cost [$/kWh]
 * options: None
 * constraints: None
 * required if: ?=0.0
 */
SAM_EXPORT void SAM_Equpartflip_BatterySystem_battery_per_kWh_nset(SAM_Equpartflip ptr, double number, SAM_error *err);

/**
 * Set en_batt: Enable battery storage model [0/1]
 * options: None
 * constraints: None
 * required if: ?=0
 */
SAM_EXPORT void SAM_Equpartflip_BatterySystem_en_batt_nset(SAM_Equpartflip ptr, double number, SAM_error *err);


/**
 * Revenue Getters
 */

SAM_EXPORT double SAM_Equpartflip_Revenue_ppa_escalation_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Revenue_ppa_price_input_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Revenue_ppa_soln_max_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Revenue_ppa_soln_max_iterations_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Revenue_ppa_soln_min_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Revenue_ppa_soln_mode_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Revenue_ppa_soln_tolerance_nget(SAM_Equpartflip ptr, SAM_error *err);


/**
 * FinancialParameters Getters
 */

SAM_EXPORT double SAM_Equpartflip_FinancialParameters_analysis_period_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_FinancialParameters_equip1_reserve_cost_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_FinancialParameters_equip1_reserve_freq_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_FinancialParameters_equip2_reserve_cost_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_FinancialParameters_equip2_reserve_freq_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_FinancialParameters_equip3_reserve_cost_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_FinancialParameters_equip3_reserve_freq_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_FinancialParameters_equip_reserve_depr_fed_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_FinancialParameters_equip_reserve_depr_sta_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_FinancialParameters_federal_tax_rate_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_FinancialParameters_inflation_rate_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_FinancialParameters_insurance_rate_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double
SAM_Equpartflip_FinancialParameters_prop_tax_assessed_decline_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double
SAM_Equpartflip_FinancialParameters_prop_tax_cost_assessed_percent_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_FinancialParameters_property_tax_rate_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_FinancialParameters_real_discount_rate_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_FinancialParameters_reserves_interest_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_FinancialParameters_salvage_percentage_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_FinancialParameters_state_tax_rate_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_FinancialParameters_system_capacity_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_FinancialParameters_system_heat_rate_nget(SAM_Equpartflip ptr, SAM_error *err);


/**
 * SystemCosts Getters
 */

SAM_EXPORT double SAM_Equpartflip_SystemCosts_add_om_num_types_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_SystemCosts_annual_fuel_usage_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_SystemCosts_annual_fuel_usage_lifetime_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_SystemCosts_om_capacity_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_SystemCosts_om_capacity1_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_SystemCosts_om_capacity1_nameplate_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_SystemCosts_om_capacity2_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_SystemCosts_om_capacity2_nameplate_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_SystemCosts_om_capacity_escal_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_SystemCosts_om_fixed_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_SystemCosts_om_fixed1_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_SystemCosts_om_fixed2_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_SystemCosts_om_fixed_escal_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_SystemCosts_om_fuel_cost_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_SystemCosts_om_fuel_cost_escal_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_SystemCosts_om_opt_fuel_1_cost_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_SystemCosts_om_opt_fuel_1_cost_escal_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_SystemCosts_om_opt_fuel_1_usage_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_SystemCosts_om_opt_fuel_2_cost_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_SystemCosts_om_opt_fuel_2_cost_escal_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_SystemCosts_om_opt_fuel_2_usage_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_SystemCosts_om_production_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_SystemCosts_om_production1_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_SystemCosts_om_production1_values_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_SystemCosts_om_production2_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_SystemCosts_om_production2_values_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_SystemCosts_om_production_escal_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_SystemCosts_om_replacement_cost1_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_SystemCosts_om_replacement_cost2_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_SystemCosts_om_replacement_cost_escal_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_SystemCosts_total_installed_cost_nget(SAM_Equpartflip ptr, SAM_error *err);


/**
 * TaxCreditIncentives Getters
 */

SAM_EXPORT double SAM_Equpartflip_TaxCreditIncentives_itc_fed_amount_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double
SAM_Equpartflip_TaxCreditIncentives_itc_fed_amount_deprbas_fed_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double
SAM_Equpartflip_TaxCreditIncentives_itc_fed_amount_deprbas_sta_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_TaxCreditIncentives_itc_fed_percent_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double
SAM_Equpartflip_TaxCreditIncentives_itc_fed_percent_deprbas_fed_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double
SAM_Equpartflip_TaxCreditIncentives_itc_fed_percent_deprbas_sta_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double
SAM_Equpartflip_TaxCreditIncentives_itc_fed_percent_maxvalue_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_TaxCreditIncentives_itc_sta_amount_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double
SAM_Equpartflip_TaxCreditIncentives_itc_sta_amount_deprbas_fed_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double
SAM_Equpartflip_TaxCreditIncentives_itc_sta_amount_deprbas_sta_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_TaxCreditIncentives_itc_sta_percent_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double
SAM_Equpartflip_TaxCreditIncentives_itc_sta_percent_deprbas_fed_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double
SAM_Equpartflip_TaxCreditIncentives_itc_sta_percent_deprbas_sta_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double
SAM_Equpartflip_TaxCreditIncentives_itc_sta_percent_maxvalue_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_TaxCreditIncentives_ptc_fed_amount_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_TaxCreditIncentives_ptc_fed_escal_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_TaxCreditIncentives_ptc_fed_term_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_TaxCreditIncentives_ptc_sta_amount_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_TaxCreditIncentives_ptc_sta_escal_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_TaxCreditIncentives_ptc_sta_term_nget(SAM_Equpartflip ptr, SAM_error *err);


/**
 * Depreciation Getters
 */

SAM_EXPORT double SAM_Equpartflip_Depreciation_depr_alloc_custom_percent_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Depreciation_depr_alloc_macrs_15_percent_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Depreciation_depr_alloc_macrs_5_percent_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Depreciation_depr_alloc_sl_15_percent_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Depreciation_depr_alloc_sl_20_percent_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Depreciation_depr_alloc_sl_39_percent_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Depreciation_depr_alloc_sl_5_percent_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Depreciation_depr_bonus_fed_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Depreciation_depr_bonus_fed_custom_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Depreciation_depr_bonus_fed_macrs_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Depreciation_depr_bonus_fed_macrs_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Depreciation_depr_bonus_fed_sl_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Depreciation_depr_bonus_fed_sl_20_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Depreciation_depr_bonus_fed_sl_39_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Depreciation_depr_bonus_fed_sl_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Depreciation_depr_bonus_sta_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Depreciation_depr_bonus_sta_custom_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Depreciation_depr_bonus_sta_macrs_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Depreciation_depr_bonus_sta_macrs_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Depreciation_depr_bonus_sta_sl_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Depreciation_depr_bonus_sta_sl_20_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Depreciation_depr_bonus_sta_sl_39_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Depreciation_depr_bonus_sta_sl_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Depreciation_depr_custom_schedule_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Depreciation_depr_fedbas_method_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Depreciation_depr_itc_fed_custom_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Depreciation_depr_itc_fed_macrs_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Depreciation_depr_itc_fed_macrs_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Depreciation_depr_itc_fed_sl_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Depreciation_depr_itc_fed_sl_20_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Depreciation_depr_itc_fed_sl_39_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Depreciation_depr_itc_fed_sl_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Depreciation_depr_itc_sta_custom_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Depreciation_depr_itc_sta_macrs_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Depreciation_depr_itc_sta_macrs_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Depreciation_depr_itc_sta_sl_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Depreciation_depr_itc_sta_sl_20_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Depreciation_depr_itc_sta_sl_39_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Depreciation_depr_itc_sta_sl_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Depreciation_depr_stabas_method_nget(SAM_Equpartflip ptr, SAM_error *err);


/**
 * PaymentIncentives Getters
 */

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_cbi_fed_amount_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_cbi_fed_deprbas_fed_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_cbi_fed_deprbas_sta_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_cbi_fed_maxvalue_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_cbi_fed_tax_fed_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_cbi_fed_tax_sta_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_cbi_oth_amount_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_cbi_oth_deprbas_fed_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_cbi_oth_deprbas_sta_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_cbi_oth_maxvalue_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_cbi_oth_tax_fed_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_cbi_oth_tax_sta_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_cbi_sta_amount_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_cbi_sta_deprbas_fed_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_cbi_sta_deprbas_sta_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_cbi_sta_maxvalue_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_cbi_sta_tax_fed_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_cbi_sta_tax_sta_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_cbi_uti_amount_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_cbi_uti_deprbas_fed_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_cbi_uti_deprbas_sta_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_cbi_uti_maxvalue_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_cbi_uti_tax_fed_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_cbi_uti_tax_sta_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_ibi_fed_amount_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double
SAM_Equpartflip_PaymentIncentives_ibi_fed_amount_deprbas_fed_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double
SAM_Equpartflip_PaymentIncentives_ibi_fed_amount_deprbas_sta_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_ibi_fed_amount_tax_fed_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_ibi_fed_amount_tax_sta_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_ibi_fed_percent_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double
SAM_Equpartflip_PaymentIncentives_ibi_fed_percent_deprbas_fed_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double
SAM_Equpartflip_PaymentIncentives_ibi_fed_percent_deprbas_sta_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_ibi_fed_percent_maxvalue_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_ibi_fed_percent_tax_fed_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_ibi_fed_percent_tax_sta_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_ibi_oth_amount_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double
SAM_Equpartflip_PaymentIncentives_ibi_oth_amount_deprbas_fed_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double
SAM_Equpartflip_PaymentIncentives_ibi_oth_amount_deprbas_sta_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_ibi_oth_amount_tax_fed_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_ibi_oth_amount_tax_sta_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_ibi_oth_percent_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double
SAM_Equpartflip_PaymentIncentives_ibi_oth_percent_deprbas_fed_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double
SAM_Equpartflip_PaymentIncentives_ibi_oth_percent_deprbas_sta_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_ibi_oth_percent_maxvalue_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_ibi_oth_percent_tax_fed_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_ibi_oth_percent_tax_sta_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_ibi_sta_amount_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double
SAM_Equpartflip_PaymentIncentives_ibi_sta_amount_deprbas_fed_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double
SAM_Equpartflip_PaymentIncentives_ibi_sta_amount_deprbas_sta_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_ibi_sta_amount_tax_fed_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_ibi_sta_amount_tax_sta_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_ibi_sta_percent_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double
SAM_Equpartflip_PaymentIncentives_ibi_sta_percent_deprbas_fed_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double
SAM_Equpartflip_PaymentIncentives_ibi_sta_percent_deprbas_sta_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_ibi_sta_percent_maxvalue_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_ibi_sta_percent_tax_fed_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_ibi_sta_percent_tax_sta_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_ibi_uti_amount_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double
SAM_Equpartflip_PaymentIncentives_ibi_uti_amount_deprbas_fed_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double
SAM_Equpartflip_PaymentIncentives_ibi_uti_amount_deprbas_sta_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_ibi_uti_amount_tax_fed_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_ibi_uti_amount_tax_sta_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_ibi_uti_percent_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double
SAM_Equpartflip_PaymentIncentives_ibi_uti_percent_deprbas_fed_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double
SAM_Equpartflip_PaymentIncentives_ibi_uti_percent_deprbas_sta_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_ibi_uti_percent_maxvalue_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_ibi_uti_percent_tax_fed_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_ibi_uti_percent_tax_sta_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_PaymentIncentives_pbi_fed_amount_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_pbi_fed_escal_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_pbi_fed_tax_fed_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_pbi_fed_tax_sta_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_pbi_fed_term_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_PaymentIncentives_pbi_oth_amount_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_pbi_oth_escal_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_pbi_oth_tax_fed_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_pbi_oth_tax_sta_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_pbi_oth_term_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_PaymentIncentives_pbi_sta_amount_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_pbi_sta_escal_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_pbi_sta_tax_fed_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_pbi_sta_tax_sta_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_pbi_sta_term_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_PaymentIncentives_pbi_uti_amount_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_pbi_uti_escal_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_pbi_uti_tax_fed_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_pbi_uti_tax_sta_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_PaymentIncentives_pbi_uti_term_nget(SAM_Equpartflip ptr, SAM_error *err);


/**
 * SystemOutput Getters
 */

SAM_EXPORT double *SAM_Equpartflip_SystemOutput_degradation_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_SystemOutput_gen_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_SystemOutput_system_capacity_nget(SAM_Equpartflip ptr, SAM_error *err);


/**
 * Recapitalization Getters
 */

SAM_EXPORT double *
SAM_Equpartflip_Recapitalization_system_lifetime_recapitalize_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double
SAM_Equpartflip_Recapitalization_system_recapitalization_cost_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double
SAM_Equpartflip_Recapitalization_system_recapitalization_escalation_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double
SAM_Equpartflip_Recapitalization_system_use_recapitalization_nget(SAM_Equpartflip ptr, SAM_error *err);


/**
 * TimeOfDelivery Getters
 */

SAM_EXPORT double SAM_Equpartflip_TimeOfDelivery_dispatch_factor1_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_TimeOfDelivery_dispatch_factor2_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_TimeOfDelivery_dispatch_factor3_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_TimeOfDelivery_dispatch_factor4_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_TimeOfDelivery_dispatch_factor5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_TimeOfDelivery_dispatch_factor6_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_TimeOfDelivery_dispatch_factor7_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_TimeOfDelivery_dispatch_factor8_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_TimeOfDelivery_dispatch_factor9_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_TimeOfDelivery_dispatch_factors_ts_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_TimeOfDelivery_dispatch_sched_weekday_mget(SAM_Equpartflip ptr, int *nrows, int *ncols, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_TimeOfDelivery_dispatch_sched_weekend_mget(SAM_Equpartflip ptr, int *nrows, int *ncols, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_TimeOfDelivery_ppa_multiplier_model_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_TimeOfDelivery_system_use_lifetime_output_nget(SAM_Equpartflip ptr, SAM_error *err);


/**
 * ConstructionFinancing Getters
 */

SAM_EXPORT double
SAM_Equpartflip_ConstructionFinancing_construction_financing_cost_nget(SAM_Equpartflip ptr, SAM_error *err);


/**
 * OtherCapitalCosts Getters
 */

SAM_EXPORT double SAM_Equpartflip_OtherCapitalCosts_cost_dev_fee_percent_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_OtherCapitalCosts_cost_equity_closing_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_OtherCapitalCosts_cost_other_financing_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double
SAM_Equpartflip_OtherCapitalCosts_months_receivables_reserve_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_OtherCapitalCosts_months_working_reserve_nget(SAM_Equpartflip ptr, SAM_error *err);


/**
 * IRRTargets Getters
 */

SAM_EXPORT double SAM_Equpartflip_IRRTargets_flip_target_percent_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_IRRTargets_flip_target_year_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_IRRTargets_tax_investor_equity_percent_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double
SAM_Equpartflip_IRRTargets_tax_investor_postflip_cash_percent_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double
SAM_Equpartflip_IRRTargets_tax_investor_postflip_tax_percent_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double
SAM_Equpartflip_IRRTargets_tax_investor_preflip_cash_percent_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_IRRTargets_tax_investor_preflip_tax_percent_nget(SAM_Equpartflip ptr, SAM_error *err);


/**
 * DeveloperCapitalRecovery Getters
 */

SAM_EXPORT double
SAM_Equpartflip_DeveloperCapitalRecovery_sponsor_cap_recovery_mode_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double
SAM_Equpartflip_DeveloperCapitalRecovery_sponsor_cap_recovery_year_nget(SAM_Equpartflip ptr, SAM_error *err);


/**
 * BatterySystem Getters
 */

SAM_EXPORT double *
SAM_Equpartflip_BatterySystem_batt_bank_replacement_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_BatterySystem_batt_computed_bank_capacity_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_BatterySystem_batt_replacement_option_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_BatterySystem_batt_replacement_schedule_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_BatterySystem_battery_per_kWh_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_BatterySystem_en_batt_nget(SAM_Equpartflip ptr, SAM_error *err);


/**
 * Outputs Getters
 */

SAM_EXPORT double SAM_Equpartflip_Outputs_adjusted_installed_cost_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_analysis_period_irr_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_cbi_fedtax_total_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_cbi_statax_total_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_cbi_total_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_cbi_total_fed_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_cbi_total_oth_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_cbi_total_sta_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_cbi_total_uti_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_annual_costs_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_battery_replacement_cost_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_battery_replacement_cost_schedule_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_disbursement_equip1_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_disbursement_equip2_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_disbursement_equip3_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_disbursement_om_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_disbursement_receivables_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_ebitda_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_effective_tax_frac_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_energy_net_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_energy_net_apr_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_energy_net_aug_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_energy_net_dec_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_energy_net_dispatch1_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_energy_net_dispatch2_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_energy_net_dispatch3_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_energy_net_dispatch4_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_energy_net_dispatch5_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_energy_net_dispatch6_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_energy_net_dispatch7_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_energy_net_dispatch8_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_energy_net_dispatch9_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_energy_net_feb_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_energy_net_jan_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_energy_net_jul_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_energy_net_jun_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_energy_net_mar_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_energy_net_may_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_energy_net_monthly_firstyear_TOD1_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_energy_net_monthly_firstyear_TOD2_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_energy_net_monthly_firstyear_TOD3_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_energy_net_monthly_firstyear_TOD4_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_energy_net_monthly_firstyear_TOD5_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_energy_net_monthly_firstyear_TOD6_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_energy_net_monthly_firstyear_TOD7_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_energy_net_monthly_firstyear_TOD8_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_energy_net_monthly_firstyear_TOD9_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_energy_net_nov_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_energy_net_oct_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_energy_net_sep_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_energy_value_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_feddepr_custom_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_feddepr_macrs_15_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_feddepr_macrs_5_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_feddepr_me1_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_feddepr_me2_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_feddepr_me3_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_feddepr_sl_15_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_feddepr_sl_20_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_feddepr_sl_39_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_feddepr_sl_5_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_feddepr_total_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_federal_tax_frac_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_fedtax_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_fedtax_income_prior_incentives_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_fedtax_income_with_incentives_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_fedtax_taxable_incentives_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_funding_equip1_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_funding_equip2_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_funding_equip3_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_funding_om_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_funding_receivables_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_insurance_expense_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_cf_length_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_net_salvage_value_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_om_capacity_expense_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_om_fixed_expense_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_om_fuel_expense_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_om_opt_fuel_1_expense_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_om_opt_fuel_2_expense_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_om_production_expense_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_operating_expenses_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_pbi_fedtax_total_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_pbi_statax_total_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_pbi_total_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_pbi_total_fed_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_pbi_total_oth_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_pbi_total_sta_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_pbi_total_uti_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_ppa_price_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_pretax_cashflow_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_project_financing_activities_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_project_investing_activities_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_project_me1cs_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_project_me1ra_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_project_me2cs_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_project_me2ra_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_project_me3cs_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_project_me3ra_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_project_mecs_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_project_operating_activities_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_project_ra_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_project_receivablesra_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_project_return_aftertax_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_project_return_aftertax_cash_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_project_return_aftertax_irr_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_project_return_aftertax_npv_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_project_return_pretax_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_project_return_pretax_irr_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_project_return_pretax_npv_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_project_wcra_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_property_tax_assessed_value_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_property_tax_expense_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_ptc_fed_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_ptc_sta_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_ptc_total_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_recapitalization_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_reserve_equip1_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_reserve_equip2_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_reserve_equip3_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_reserve_interest_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_reserve_om_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_reserve_receivables_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_reserve_total_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_revenue_apr_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_revenue_aug_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_revenue_dec_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_revenue_dispatch1_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_revenue_dispatch2_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_revenue_dispatch3_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_revenue_dispatch4_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_revenue_dispatch5_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_revenue_dispatch6_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_revenue_dispatch7_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_revenue_dispatch8_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_revenue_dispatch9_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_revenue_feb_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_revenue_jan_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_revenue_jul_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_revenue_jun_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_revenue_mar_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_revenue_may_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_revenue_monthly_firstyear_TOD1_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_revenue_monthly_firstyear_TOD2_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_revenue_monthly_firstyear_TOD3_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_revenue_monthly_firstyear_TOD4_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_revenue_monthly_firstyear_TOD5_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_revenue_monthly_firstyear_TOD6_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_revenue_monthly_firstyear_TOD7_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_revenue_monthly_firstyear_TOD8_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_revenue_monthly_firstyear_TOD9_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_revenue_nov_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_revenue_oct_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_revenue_sep_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_sponsor_aftertax_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_sponsor_aftertax_cash_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_sponsor_aftertax_irr_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_sponsor_aftertax_itc_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_sponsor_aftertax_npv_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_sponsor_aftertax_ptc_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_sponsor_aftertax_tax_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_sponsor_capital_recovery_balance_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_sponsor_capital_recovery_cash_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_sponsor_pretax_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_sponsor_pretax_cash_during_recovery_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_sponsor_pretax_cash_post_recovery_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_sponsor_pretax_irr_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_sponsor_pretax_npv_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_stadepr_custom_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_stadepr_macrs_15_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_stadepr_macrs_5_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_stadepr_me1_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_stadepr_me2_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_stadepr_me3_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_stadepr_sl_15_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_stadepr_sl_20_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_stadepr_sl_39_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_stadepr_sl_5_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_stadepr_total_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_statax_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_statax_income_prior_incentives_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_statax_income_with_incentives_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_statax_taxable_incentives_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_state_tax_frac_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_tax_investor_aftertax_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_tax_investor_aftertax_cash_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_tax_investor_aftertax_irr_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_tax_investor_aftertax_itc_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_tax_investor_aftertax_max_irr_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_tax_investor_aftertax_npv_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_tax_investor_aftertax_ptc_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_tax_investor_aftertax_tax_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_tax_investor_pretax_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_tax_investor_pretax_irr_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *
SAM_Equpartflip_Outputs_cf_tax_investor_pretax_npv_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_cf_total_revenue_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_cost_financing_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_cost_installed_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_cost_installedperwatt_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_cost_prefinancing_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_debt_fraction_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_alloc_custom_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_alloc_macrs_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_alloc_macrs_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_alloc_none_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_alloc_none_percent_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_alloc_sl_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_alloc_sl_20_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_alloc_sl_39_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_alloc_sl_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_alloc_total_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_after_itc_custom_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_after_itc_macrs_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_after_itc_macrs_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_after_itc_sl_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_after_itc_sl_20_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_after_itc_sl_39_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_after_itc_sl_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_after_itc_total_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_cbi_reduc_custom_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_cbi_reduc_macrs_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_cbi_reduc_macrs_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_cbi_reduc_sl_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_cbi_reduc_sl_20_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_cbi_reduc_sl_39_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_cbi_reduc_sl_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_cbi_reduc_total_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_custom_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_first_year_bonus_custom_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double
SAM_Equpartflip_Outputs_depr_fedbas_first_year_bonus_macrs_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double
SAM_Equpartflip_Outputs_depr_fedbas_first_year_bonus_macrs_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_first_year_bonus_sl_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_first_year_bonus_sl_20_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_first_year_bonus_sl_39_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_first_year_bonus_sl_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_first_year_bonus_total_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_fixed_amount_custom_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_fixed_amount_macrs_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_fixed_amount_macrs_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_fixed_amount_sl_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_fixed_amount_sl_20_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_fixed_amount_sl_39_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_fixed_amount_sl_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_fixed_amount_total_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_ibi_reduc_custom_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_ibi_reduc_macrs_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_ibi_reduc_macrs_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_ibi_reduc_sl_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_ibi_reduc_sl_20_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_ibi_reduc_sl_39_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_ibi_reduc_sl_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_ibi_reduc_total_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double
SAM_Equpartflip_Outputs_depr_fedbas_itc_fed_reduction_custom_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double
SAM_Equpartflip_Outputs_depr_fedbas_itc_fed_reduction_macrs_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double
SAM_Equpartflip_Outputs_depr_fedbas_itc_fed_reduction_macrs_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_itc_fed_reduction_sl_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_itc_fed_reduction_sl_20_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_itc_fed_reduction_sl_39_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_itc_fed_reduction_sl_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_itc_fed_reduction_total_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double
SAM_Equpartflip_Outputs_depr_fedbas_itc_sta_reduction_custom_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double
SAM_Equpartflip_Outputs_depr_fedbas_itc_sta_reduction_macrs_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double
SAM_Equpartflip_Outputs_depr_fedbas_itc_sta_reduction_macrs_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_itc_sta_reduction_sl_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_itc_sta_reduction_sl_20_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_itc_sta_reduction_sl_39_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_itc_sta_reduction_sl_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_itc_sta_reduction_total_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_macrs_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_macrs_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_percent_amount_custom_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_percent_amount_macrs_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_percent_amount_macrs_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_percent_amount_sl_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_percent_amount_sl_20_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_percent_amount_sl_39_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_percent_amount_sl_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_percent_amount_total_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_percent_custom_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_percent_macrs_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_percent_macrs_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_percent_qual_custom_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_percent_qual_macrs_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_percent_qual_macrs_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_percent_qual_sl_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_percent_qual_sl_20_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_percent_qual_sl_39_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_percent_qual_sl_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_percent_qual_total_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_percent_sl_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_percent_sl_20_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_percent_sl_39_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_percent_sl_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_percent_total_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_prior_itc_custom_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_prior_itc_macrs_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_prior_itc_macrs_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_prior_itc_sl_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_prior_itc_sl_20_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_prior_itc_sl_39_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_prior_itc_sl_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_prior_itc_total_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_sl_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_sl_20_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_sl_39_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_sl_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_fedbas_total_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_after_itc_custom_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_after_itc_macrs_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_after_itc_macrs_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_after_itc_sl_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_after_itc_sl_20_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_after_itc_sl_39_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_after_itc_sl_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_after_itc_total_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_cbi_reduc_custom_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_cbi_reduc_macrs_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_cbi_reduc_macrs_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_cbi_reduc_sl_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_cbi_reduc_sl_20_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_cbi_reduc_sl_39_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_cbi_reduc_sl_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_cbi_reduc_total_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_custom_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_first_year_bonus_custom_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double
SAM_Equpartflip_Outputs_depr_stabas_first_year_bonus_macrs_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double
SAM_Equpartflip_Outputs_depr_stabas_first_year_bonus_macrs_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_first_year_bonus_sl_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_first_year_bonus_sl_20_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_first_year_bonus_sl_39_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_first_year_bonus_sl_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_first_year_bonus_total_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_fixed_amount_custom_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_fixed_amount_macrs_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_fixed_amount_macrs_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_fixed_amount_sl_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_fixed_amount_sl_20_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_fixed_amount_sl_39_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_fixed_amount_sl_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_fixed_amount_total_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_ibi_reduc_custom_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_ibi_reduc_macrs_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_ibi_reduc_macrs_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_ibi_reduc_sl_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_ibi_reduc_sl_20_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_ibi_reduc_sl_39_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_ibi_reduc_sl_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_ibi_reduc_total_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double
SAM_Equpartflip_Outputs_depr_stabas_itc_fed_reduction_custom_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double
SAM_Equpartflip_Outputs_depr_stabas_itc_fed_reduction_macrs_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double
SAM_Equpartflip_Outputs_depr_stabas_itc_fed_reduction_macrs_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_itc_fed_reduction_sl_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_itc_fed_reduction_sl_20_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_itc_fed_reduction_sl_39_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_itc_fed_reduction_sl_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_itc_fed_reduction_total_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double
SAM_Equpartflip_Outputs_depr_stabas_itc_sta_reduction_custom_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double
SAM_Equpartflip_Outputs_depr_stabas_itc_sta_reduction_macrs_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double
SAM_Equpartflip_Outputs_depr_stabas_itc_sta_reduction_macrs_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_itc_sta_reduction_sl_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_itc_sta_reduction_sl_20_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_itc_sta_reduction_sl_39_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_itc_sta_reduction_sl_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_itc_sta_reduction_total_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_macrs_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_macrs_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_percent_amount_custom_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_percent_amount_macrs_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_percent_amount_macrs_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_percent_amount_sl_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_percent_amount_sl_20_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_percent_amount_sl_39_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_percent_amount_sl_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_percent_amount_total_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_percent_custom_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_percent_macrs_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_percent_macrs_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_percent_qual_custom_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_percent_qual_macrs_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_percent_qual_macrs_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_percent_qual_sl_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_percent_qual_sl_20_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_percent_qual_sl_39_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_percent_qual_sl_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_percent_qual_total_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_percent_sl_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_percent_sl_20_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_percent_sl_39_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_percent_sl_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_percent_total_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_prior_itc_custom_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_prior_itc_macrs_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_prior_itc_macrs_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_prior_itc_sl_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_prior_itc_sl_20_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_prior_itc_sl_39_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_prior_itc_sl_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_prior_itc_total_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_sl_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_sl_20_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_sl_39_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_sl_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_depr_stabas_total_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_effective_tax_rate_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_firstyear_energy_dispatch1_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_firstyear_energy_dispatch2_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_firstyear_energy_dispatch3_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_firstyear_energy_dispatch4_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_firstyear_energy_dispatch5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_firstyear_energy_dispatch6_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_firstyear_energy_dispatch7_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_firstyear_energy_dispatch8_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_firstyear_energy_dispatch9_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_firstyear_energy_price1_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_firstyear_energy_price2_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_firstyear_energy_price3_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_firstyear_energy_price4_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_firstyear_energy_price5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_firstyear_energy_price6_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_firstyear_energy_price7_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_firstyear_energy_price8_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_firstyear_energy_price9_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_firstyear_revenue_dispatch1_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_firstyear_revenue_dispatch2_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_firstyear_revenue_dispatch3_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_firstyear_revenue_dispatch4_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_firstyear_revenue_dispatch5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_firstyear_revenue_dispatch6_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_firstyear_revenue_dispatch7_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_firstyear_revenue_dispatch8_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_firstyear_revenue_dispatch9_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_flip_actual_irr_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_flip_actual_year_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_flip_target_irr_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_flip_target_year_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_ibi_fedtax_total_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_ibi_statax_total_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_ibi_total_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_ibi_total_fed_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_ibi_total_oth_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_ibi_total_sta_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_ibi_total_uti_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_issuance_of_equity_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_itc_disallow_fed_fixed_custom_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_itc_disallow_fed_fixed_macrs_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_itc_disallow_fed_fixed_macrs_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_itc_disallow_fed_fixed_sl_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_itc_disallow_fed_fixed_sl_20_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_itc_disallow_fed_fixed_sl_39_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_itc_disallow_fed_fixed_sl_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_itc_disallow_fed_fixed_total_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_itc_disallow_fed_percent_custom_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_itc_disallow_fed_percent_macrs_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_itc_disallow_fed_percent_macrs_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_itc_disallow_fed_percent_sl_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_itc_disallow_fed_percent_sl_20_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_itc_disallow_fed_percent_sl_39_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_itc_disallow_fed_percent_sl_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_itc_disallow_fed_percent_total_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_itc_disallow_sta_fixed_custom_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_itc_disallow_sta_fixed_macrs_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_itc_disallow_sta_fixed_macrs_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_itc_disallow_sta_fixed_sl_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_itc_disallow_sta_fixed_sl_20_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_itc_disallow_sta_fixed_sl_39_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_itc_disallow_sta_fixed_sl_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_itc_disallow_sta_fixed_total_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_itc_disallow_sta_percent_custom_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_itc_disallow_sta_percent_macrs_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_itc_disallow_sta_percent_macrs_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_itc_disallow_sta_percent_sl_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_itc_disallow_sta_percent_sl_20_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_itc_disallow_sta_percent_sl_39_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_itc_disallow_sta_percent_sl_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_itc_disallow_sta_percent_total_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_itc_fed_fixed_total_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_itc_fed_percent_total_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_itc_fed_qual_custom_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_itc_fed_qual_macrs_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_itc_fed_qual_macrs_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_itc_fed_qual_sl_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_itc_fed_qual_sl_20_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_itc_fed_qual_sl_39_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_itc_fed_qual_sl_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_itc_fed_qual_total_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_itc_sta_fixed_total_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_itc_sta_percent_total_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_itc_sta_qual_custom_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_itc_sta_qual_macrs_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_itc_sta_qual_macrs_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_itc_sta_qual_sl_15_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_itc_sta_qual_sl_20_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_itc_sta_qual_sl_39_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_itc_sta_qual_sl_5_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_itc_sta_qual_total_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_itc_total_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_itc_total_fed_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_itc_total_sta_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_lcoe_nom_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_lcoe_real_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_lcoptc_fed_nom_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_lcoptc_fed_real_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_lcoptc_sta_nom_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_lcoptc_sta_real_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_lppa_nom_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_lppa_real_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_nominal_discount_rate_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_npv_annual_costs_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_npv_energy_nom_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_npv_energy_real_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_npv_ppa_revenue_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_ppa_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_ppa_escalation_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double *SAM_Equpartflip_Outputs_ppa_multipliers_aget(SAM_Equpartflip ptr, int *length, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_ppa_price_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_present_value_fuel_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_present_value_insandproptax_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_present_value_oandm_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_present_value_oandm_nonfuel_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_prop_tax_assessed_value_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_purchase_of_property_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_salvage_value_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_size_of_equity_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_sponsor_aftertax_equity_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_sponsor_aftertax_irr_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_sponsor_aftertax_npv_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_sponsor_pretax_development_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_sponsor_pretax_equity_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_sponsor_pretax_irr_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_sponsor_pretax_npv_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_tax_investor_aftertax_irr_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_tax_investor_aftertax_npv_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_tax_investor_pretax_irr_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_tax_investor_pretax_npv_nget(SAM_Equpartflip ptr, SAM_error *err);

SAM_EXPORT double SAM_Equpartflip_Outputs_wacc_nget(SAM_Equpartflip ptr, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif
