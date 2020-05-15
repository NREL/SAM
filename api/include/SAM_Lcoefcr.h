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

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_Lcoefcr_execute(SAM_table data, int verbosity, SAM_error* err);


	//
	// SimpleLCOE parameters
	//

	/**
	 * Set annual_energy: Annual energy production [kWh]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Lcoefcr_SimpleLCOE_annual_energy_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set capital_cost: Capital cost [$]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Lcoefcr_SimpleLCOE_capital_cost_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set fixed_charge_rate: Fixed charge rate
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Lcoefcr_SimpleLCOE_fixed_charge_rate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set fixed_operating_cost: Annual fixed operating cost [$]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Lcoefcr_SimpleLCOE_fixed_operating_cost_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set variable_operating_cost: Annual variable operating cost [$/kWh]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Lcoefcr_SimpleLCOE_variable_operating_cost_nset(SAM_table ptr, double number, SAM_error *err);


	/**
	 * SimpleLCOE Getters
	 */

	SAM_EXPORT double SAM_Lcoefcr_SimpleLCOE_annual_energy_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Lcoefcr_SimpleLCOE_capital_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Lcoefcr_SimpleLCOE_fixed_charge_rate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Lcoefcr_SimpleLCOE_fixed_operating_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_Lcoefcr_SimpleLCOE_variable_operating_cost_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double SAM_Lcoefcr_Outputs_lcoe_fcr_nget(SAM_table ptr, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif