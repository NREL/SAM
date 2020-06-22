#ifndef SAM_IPHTOLCOEFCR_H_
#define SAM_IPHTOLCOEFCR_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// IphToLcoefcr Technology Model
	//

	/** 
	 * Create a IphToLcoefcr variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_IphToLcoefcr;

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_IphToLcoefcr_execute(SAM_table data, int verbosity, SAM_error* err);


	//
	// IPHLCOH parameters
	//

	/**
	 * Set annual_electricity_consumption: Annual electricity consumptoin w/ avail derate [kWe-hr]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_IphToLcoefcr_IPHLCOH_annual_electricity_consumption_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set electricity_rate: Cost of electricity used to operate pumps/trackers [$/kWe]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_IphToLcoefcr_IPHLCOH_electricity_rate_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// SimpleLCOE parameters
	//

	/**
	 * Set fixed_operating_cost: Annual fixed operating cost [$/kW]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_IphToLcoefcr_SimpleLCOE_fixed_operating_cost_nset(SAM_table ptr, double number, SAM_error *err);


	/**
	 * IPHLCOH Getters
	 */

	SAM_EXPORT double SAM_IphToLcoefcr_IPHLCOH_annual_electricity_consumption_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_IphToLcoefcr_IPHLCOH_electricity_rate_nget(SAM_table ptr, SAM_error *err);


	/**
	 * SimpleLCOE Getters
	 */

	SAM_EXPORT double SAM_IphToLcoefcr_SimpleLCOE_fixed_operating_cost_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif