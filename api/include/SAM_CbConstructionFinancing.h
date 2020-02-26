#ifndef SAM_CBCONSTRUCTIONFINANCING_H_
#define SAM_CBCONSTRUCTIONFINANCING_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// CbConstructionFinancing Technology Model
	//

	/** 
	 * Create a CbConstructionFinancing variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_CbConstructionFinancing;

	SAM_EXPORT SAM_CbConstructionFinancing SAM_CbConstructionFinancing_construct(const char* def, SAM_error* err);

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_CbConstructionFinancing_execute(SAM_CbConstructionFinancing data, int verbosity, SAM_error* err);

	SAM_EXPORT void SAM_CbConstructionFinancing_destruct(SAM_CbConstructionFinancing system);


	//
	// SystemCosts parameters
	//

	/**
	 * Set total_installed_cost: Total installed cost [$]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CbConstructionFinancing_SystemCosts_total_installed_cost_nset(SAM_CbConstructionFinancing ptr, double number, SAM_error *err);


	//
	// FinancialParameters parameters
	//

	/**
	 * Set const_per_interest_rate1: Interest rate, loan 1 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CbConstructionFinancing_FinancialParameters_const_per_interest_rate1_nset(SAM_CbConstructionFinancing ptr, double number, SAM_error *err);

	/**
	 * Set const_per_interest_rate2: Interest rate, loan 2 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CbConstructionFinancing_FinancialParameters_const_per_interest_rate2_nset(SAM_CbConstructionFinancing ptr, double number, SAM_error *err);

	/**
	 * Set const_per_interest_rate3: Interest rate, loan 3 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CbConstructionFinancing_FinancialParameters_const_per_interest_rate3_nset(SAM_CbConstructionFinancing ptr, double number, SAM_error *err);

	/**
	 * Set const_per_interest_rate4: Interest rate, loan 4 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CbConstructionFinancing_FinancialParameters_const_per_interest_rate4_nset(SAM_CbConstructionFinancing ptr, double number, SAM_error *err);

	/**
	 * Set const_per_interest_rate5: Interest rate, loan 5 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CbConstructionFinancing_FinancialParameters_const_per_interest_rate5_nset(SAM_CbConstructionFinancing ptr, double number, SAM_error *err);

	/**
	 * Set const_per_months1: Months prior to operation, loan 1
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CbConstructionFinancing_FinancialParameters_const_per_months1_nset(SAM_CbConstructionFinancing ptr, double number, SAM_error *err);

	/**
	 * Set const_per_months2: Months prior to operation, loan 2
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CbConstructionFinancing_FinancialParameters_const_per_months2_nset(SAM_CbConstructionFinancing ptr, double number, SAM_error *err);

	/**
	 * Set const_per_months3: Months prior to operation, loan 3
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CbConstructionFinancing_FinancialParameters_const_per_months3_nset(SAM_CbConstructionFinancing ptr, double number, SAM_error *err);

	/**
	 * Set const_per_months4: Months prior to operation, loan 4
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CbConstructionFinancing_FinancialParameters_const_per_months4_nset(SAM_CbConstructionFinancing ptr, double number, SAM_error *err);

	/**
	 * Set const_per_months5: Months prior to operation, loan 5
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CbConstructionFinancing_FinancialParameters_const_per_months5_nset(SAM_CbConstructionFinancing ptr, double number, SAM_error *err);

	/**
	 * Set const_per_percent1: Percent of tot. installed cost, loan 1 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CbConstructionFinancing_FinancialParameters_const_per_percent1_nset(SAM_CbConstructionFinancing ptr, double number, SAM_error *err);

	/**
	 * Set const_per_percent2: Percent of tot. installed cost, loan 2 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CbConstructionFinancing_FinancialParameters_const_per_percent2_nset(SAM_CbConstructionFinancing ptr, double number, SAM_error *err);

	/**
	 * Set const_per_percent3: Percent of tot. installed cost, loan 3 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CbConstructionFinancing_FinancialParameters_const_per_percent3_nset(SAM_CbConstructionFinancing ptr, double number, SAM_error *err);

	/**
	 * Set const_per_percent4: Percent of tot. installed cost, loan 4 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CbConstructionFinancing_FinancialParameters_const_per_percent4_nset(SAM_CbConstructionFinancing ptr, double number, SAM_error *err);

	/**
	 * Set const_per_percent5: Percent of tot. installed cost, loan 5 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CbConstructionFinancing_FinancialParameters_const_per_percent5_nset(SAM_CbConstructionFinancing ptr, double number, SAM_error *err);

	/**
	 * Set const_per_upfront_rate1: Upfront fee on principal, loan 1 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CbConstructionFinancing_FinancialParameters_const_per_upfront_rate1_nset(SAM_CbConstructionFinancing ptr, double number, SAM_error *err);

	/**
	 * Set const_per_upfront_rate2: Upfront fee on principal, loan 2 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CbConstructionFinancing_FinancialParameters_const_per_upfront_rate2_nset(SAM_CbConstructionFinancing ptr, double number, SAM_error *err);

	/**
	 * Set const_per_upfront_rate3: Upfront fee on principal, loan 3 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CbConstructionFinancing_FinancialParameters_const_per_upfront_rate3_nset(SAM_CbConstructionFinancing ptr, double number, SAM_error *err);

	/**
	 * Set const_per_upfront_rate4: Upfront fee on principal, loan 4 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CbConstructionFinancing_FinancialParameters_const_per_upfront_rate4_nset(SAM_CbConstructionFinancing ptr, double number, SAM_error *err);

	/**
	 * Set const_per_upfront_rate5: Upfront fee on principal, loan 5 [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CbConstructionFinancing_FinancialParameters_const_per_upfront_rate5_nset(SAM_CbConstructionFinancing ptr, double number, SAM_error *err);


	/**
	 * SystemCosts Getters
	 */

	SAM_EXPORT double SAM_CbConstructionFinancing_SystemCosts_total_installed_cost_nget(SAM_CbConstructionFinancing ptr, SAM_error *err);


	/**
	 * FinancialParameters Getters
	 */

	SAM_EXPORT double SAM_CbConstructionFinancing_FinancialParameters_const_per_interest_rate1_nget(SAM_CbConstructionFinancing ptr, SAM_error *err);

	SAM_EXPORT double SAM_CbConstructionFinancing_FinancialParameters_const_per_interest_rate2_nget(SAM_CbConstructionFinancing ptr, SAM_error *err);

	SAM_EXPORT double SAM_CbConstructionFinancing_FinancialParameters_const_per_interest_rate3_nget(SAM_CbConstructionFinancing ptr, SAM_error *err);

	SAM_EXPORT double SAM_CbConstructionFinancing_FinancialParameters_const_per_interest_rate4_nget(SAM_CbConstructionFinancing ptr, SAM_error *err);

	SAM_EXPORT double SAM_CbConstructionFinancing_FinancialParameters_const_per_interest_rate5_nget(SAM_CbConstructionFinancing ptr, SAM_error *err);

	SAM_EXPORT double SAM_CbConstructionFinancing_FinancialParameters_const_per_months1_nget(SAM_CbConstructionFinancing ptr, SAM_error *err);

	SAM_EXPORT double SAM_CbConstructionFinancing_FinancialParameters_const_per_months2_nget(SAM_CbConstructionFinancing ptr, SAM_error *err);

	SAM_EXPORT double SAM_CbConstructionFinancing_FinancialParameters_const_per_months3_nget(SAM_CbConstructionFinancing ptr, SAM_error *err);

	SAM_EXPORT double SAM_CbConstructionFinancing_FinancialParameters_const_per_months4_nget(SAM_CbConstructionFinancing ptr, SAM_error *err);

	SAM_EXPORT double SAM_CbConstructionFinancing_FinancialParameters_const_per_months5_nget(SAM_CbConstructionFinancing ptr, SAM_error *err);

	SAM_EXPORT double SAM_CbConstructionFinancing_FinancialParameters_const_per_percent1_nget(SAM_CbConstructionFinancing ptr, SAM_error *err);

	SAM_EXPORT double SAM_CbConstructionFinancing_FinancialParameters_const_per_percent2_nget(SAM_CbConstructionFinancing ptr, SAM_error *err);

	SAM_EXPORT double SAM_CbConstructionFinancing_FinancialParameters_const_per_percent3_nget(SAM_CbConstructionFinancing ptr, SAM_error *err);

	SAM_EXPORT double SAM_CbConstructionFinancing_FinancialParameters_const_per_percent4_nget(SAM_CbConstructionFinancing ptr, SAM_error *err);

	SAM_EXPORT double SAM_CbConstructionFinancing_FinancialParameters_const_per_percent5_nget(SAM_CbConstructionFinancing ptr, SAM_error *err);

	SAM_EXPORT double SAM_CbConstructionFinancing_FinancialParameters_const_per_upfront_rate1_nget(SAM_CbConstructionFinancing ptr, SAM_error *err);

	SAM_EXPORT double SAM_CbConstructionFinancing_FinancialParameters_const_per_upfront_rate2_nget(SAM_CbConstructionFinancing ptr, SAM_error *err);

	SAM_EXPORT double SAM_CbConstructionFinancing_FinancialParameters_const_per_upfront_rate3_nget(SAM_CbConstructionFinancing ptr, SAM_error *err);

	SAM_EXPORT double SAM_CbConstructionFinancing_FinancialParameters_const_per_upfront_rate4_nget(SAM_CbConstructionFinancing ptr, SAM_error *err);

	SAM_EXPORT double SAM_CbConstructionFinancing_FinancialParameters_const_per_upfront_rate5_nget(SAM_CbConstructionFinancing ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double SAM_CbConstructionFinancing_Outputs_const_per_interest1_nget(SAM_CbConstructionFinancing ptr, SAM_error *err);

	SAM_EXPORT double SAM_CbConstructionFinancing_Outputs_const_per_interest2_nget(SAM_CbConstructionFinancing ptr, SAM_error *err);

	SAM_EXPORT double SAM_CbConstructionFinancing_Outputs_const_per_interest3_nget(SAM_CbConstructionFinancing ptr, SAM_error *err);

	SAM_EXPORT double SAM_CbConstructionFinancing_Outputs_const_per_interest4_nget(SAM_CbConstructionFinancing ptr, SAM_error *err);

	SAM_EXPORT double SAM_CbConstructionFinancing_Outputs_const_per_interest5_nget(SAM_CbConstructionFinancing ptr, SAM_error *err);

	SAM_EXPORT double SAM_CbConstructionFinancing_Outputs_const_per_interest_total_nget(SAM_CbConstructionFinancing ptr, SAM_error *err);

	SAM_EXPORT double SAM_CbConstructionFinancing_Outputs_const_per_percent_total_nget(SAM_CbConstructionFinancing ptr, SAM_error *err);

	SAM_EXPORT double SAM_CbConstructionFinancing_Outputs_const_per_principal1_nget(SAM_CbConstructionFinancing ptr, SAM_error *err);

	SAM_EXPORT double SAM_CbConstructionFinancing_Outputs_const_per_principal2_nget(SAM_CbConstructionFinancing ptr, SAM_error *err);

	SAM_EXPORT double SAM_CbConstructionFinancing_Outputs_const_per_principal3_nget(SAM_CbConstructionFinancing ptr, SAM_error *err);

	SAM_EXPORT double SAM_CbConstructionFinancing_Outputs_const_per_principal4_nget(SAM_CbConstructionFinancing ptr, SAM_error *err);

	SAM_EXPORT double SAM_CbConstructionFinancing_Outputs_const_per_principal5_nget(SAM_CbConstructionFinancing ptr, SAM_error *err);

	SAM_EXPORT double SAM_CbConstructionFinancing_Outputs_const_per_principal_total_nget(SAM_CbConstructionFinancing ptr, SAM_error *err);

	SAM_EXPORT double SAM_CbConstructionFinancing_Outputs_const_per_total1_nget(SAM_CbConstructionFinancing ptr, SAM_error *err);

	SAM_EXPORT double SAM_CbConstructionFinancing_Outputs_const_per_total2_nget(SAM_CbConstructionFinancing ptr, SAM_error *err);

	SAM_EXPORT double SAM_CbConstructionFinancing_Outputs_const_per_total3_nget(SAM_CbConstructionFinancing ptr, SAM_error *err);

	SAM_EXPORT double SAM_CbConstructionFinancing_Outputs_const_per_total4_nget(SAM_CbConstructionFinancing ptr, SAM_error *err);

	SAM_EXPORT double SAM_CbConstructionFinancing_Outputs_const_per_total5_nget(SAM_CbConstructionFinancing ptr, SAM_error *err);

	SAM_EXPORT double SAM_CbConstructionFinancing_Outputs_construction_financing_cost_nget(SAM_CbConstructionFinancing ptr, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif