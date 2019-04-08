#ifndef SAM_LCOEFCR_H_
#define SAM_LCOEFCR_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// Lcoefcr Technology Model
	//

	/** 
	 * Create a Lcoefcr variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_Lcoefcr;

	SAM_EXPORT SAM_Lcoefcr SAM_Lcoefcr_construct(const char* def, SAM_error* err);

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_Lcoefcr_execute(SAM_Lcoefcr data, int verbosity, SAM_error* err);

	SAM_EXPORT void SAM_Lcoefcr_destruct(SAM_Lcoefcr system);


	//
	// FinancialParameters parameters
	//

	/**
	 * Set capital_cost: Capital cost [$]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Lcoefcr_FinancialParameters_capital_cost_fset(SAM_Lcoefcr ptr, float number, SAM_error *err);

	/**
	 * Set fixed_charge_rate: Fixed charge rate []
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Lcoefcr_FinancialParameters_fixed_charge_rate_fset(SAM_Lcoefcr ptr, float number, SAM_error *err);

	/**
	 * Set fixed_operating_cost: Annual fixed operating cost [$]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Lcoefcr_FinancialParameters_fixed_operating_cost_fset(SAM_Lcoefcr ptr, float number, SAM_error *err);

	/**
	 * Set variable_operating_cost: Annual variable operating cost [$/kWh]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Lcoefcr_FinancialParameters_variable_operating_cost_fset(SAM_Lcoefcr ptr, float number, SAM_error *err);


	//
	// SimpleLCOE parameters
	//

	/**
	 * Set annual_energy: Annual energy production [kWh]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Lcoefcr_SimpleLCOE_annual_energy_fset(SAM_Lcoefcr ptr, float number, SAM_error *err);


	/**
	 * FinancialParameters Getters
	 */

	SAM_EXPORT float SAM_Lcoefcr_FinancialParameters_capital_cost_fget(SAM_Lcoefcr ptr, SAM_error *err);

	SAM_EXPORT float SAM_Lcoefcr_FinancialParameters_fixed_charge_rate_fget(SAM_Lcoefcr ptr, SAM_error *err);

	SAM_EXPORT float SAM_Lcoefcr_FinancialParameters_fixed_operating_cost_fget(SAM_Lcoefcr ptr, SAM_error *err);

	SAM_EXPORT float SAM_Lcoefcr_FinancialParameters_variable_operating_cost_fget(SAM_Lcoefcr ptr, SAM_error *err);


	/**
	 * SimpleLCOE Getters
	 */

	SAM_EXPORT float SAM_Lcoefcr_SimpleLCOE_annual_energy_fget(SAM_Lcoefcr ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT float SAM_Lcoefcr_Outputs_lcoe_fcr_fget(SAM_Lcoefcr ptr, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif