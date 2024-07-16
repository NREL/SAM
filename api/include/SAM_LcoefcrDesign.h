#ifndef SAM_LCOEFCRDESIGN_H_
#define SAM_LCOEFCRDESIGN_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// LcoefcrDesign Technology Model
	//

	/** 
	 * Create a LcoefcrDesign variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_LcoefcrDesign;

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_LcoefcrDesign_execute(SAM_table data, int verbosity, SAM_error* err);


	//
	// SystemControl parameters
	//

	/**
	 * Set sim_type: 1 (default): timeseries, 2: design only
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_LcoefcrDesign_SystemControl_sim_type_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// SimpleLCOE parameters
	//

	/**
	 * Set annual_energy: Annual energy production [kWh]
	 * options: None
	 * constraints: None
	 * required if: sim_type=1
	 */
	SAM_EXPORT void SAM_LcoefcrDesign_SimpleLCOE_annual_energy_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set c_construction_cost: Construction cost schedule [%]
	 * options: None
	 * constraints: None
	 * required if: ui_fcr_input_option=1
	 */
	SAM_EXPORT void SAM_LcoefcrDesign_SimpleLCOE_c_construction_cost_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set c_construction_interest: Nominal construction interest rate [%]
	 * options: None
	 * constraints: None
	 * required if: ui_fcr_input_option=1
	 */
	SAM_EXPORT void SAM_LcoefcrDesign_SimpleLCOE_c_construction_interest_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set c_debt_percent: Project term debt (% of capital) [%]
	 * options: None
	 * constraints: None
	 * required if: ui_fcr_input_option=1
	 */
	SAM_EXPORT void SAM_LcoefcrDesign_SimpleLCOE_c_debt_percent_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set c_depreciation_schedule: Depreciation schedule [%]
	 * options: None
	 * constraints: None
	 * required if: ui_fcr_input_option=1
	 */
	SAM_EXPORT void SAM_LcoefcrDesign_SimpleLCOE_c_depreciation_schedule_aset(SAM_table ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set c_equity_return: IRR (nominal) [%]
	 * options: None
	 * constraints: None
	 * required if: ui_fcr_input_option=1
	 */
	SAM_EXPORT void SAM_LcoefcrDesign_SimpleLCOE_c_equity_return_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set c_inflation: Input fixed charge rate [%]
	 * options: None
	 * constraints: None
	 * required if: ui_fcr_input_option=1
	 */
	SAM_EXPORT void SAM_LcoefcrDesign_SimpleLCOE_c_inflation_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set c_lifetime: Analysis period [years]
	 * options: None
	 * constraints: None
	 * required if: ui_fcr_input_option=1
	 */
	SAM_EXPORT void SAM_LcoefcrDesign_SimpleLCOE_c_lifetime_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set c_nominal_interest_rate: Nominal debt interest rate [%]
	 * options: None
	 * constraints: None
	 * required if: ui_fcr_input_option=1
	 */
	SAM_EXPORT void SAM_LcoefcrDesign_SimpleLCOE_c_nominal_interest_rate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set c_tax_rate: Effective tax rate [%]
	 * options: None
	 * constraints: None
	 * required if: ui_fcr_input_option=1
	 */
	SAM_EXPORT void SAM_LcoefcrDesign_SimpleLCOE_c_tax_rate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set fixed_operating_cost: Annual fixed operating cost [$]
	 * options: None
	 * constraints: None
	 * required if: sim_type=1
	 */
	SAM_EXPORT void SAM_LcoefcrDesign_SimpleLCOE_fixed_operating_cost_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ui_fcr_input_option: 0: fixed charge rate; 1: calculate
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_LcoefcrDesign_SimpleLCOE_ui_fcr_input_option_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ui_fixed_charge_rate: Input fixed charge rate
	 * options: None
	 * constraints: None
	 * required if: ui_fcr_input_option=0
	 */
	SAM_EXPORT void SAM_LcoefcrDesign_SimpleLCOE_ui_fixed_charge_rate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set variable_operating_cost: Annual variable operating cost [$/kWh]
	 * options: None
	 * constraints: None
	 * required if: sim_type=1
	 */
	SAM_EXPORT void SAM_LcoefcrDesign_SimpleLCOE_variable_operating_cost_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// SystemCosts parameters
	//

	/**
	 * Set total_installed_cost: Total installed cost [$]
	 * options: None
	 * constraints: None
	 * required if: sim_type=1
	 */
	SAM_EXPORT void SAM_LcoefcrDesign_SystemCosts_total_installed_cost_nset(SAM_table ptr, double number, SAM_error *err);


	//
	// IPHLCOH parameters
	//

	/**
	 * Set annual_electricity_consumption: Annual electricity consumption with avail derate [kWe-hr]
	 * options: None
	 * constraints: None
	 * required if: sim_type=1
	 */
	SAM_EXPORT void SAM_LcoefcrDesign_IPHLCOH_annual_electricity_consumption_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set electricity_rate: Cost of electricity used to operate pumps and trackers [$/kWe-hr]
	 * options: None
	 * constraints: None
	 * required if: sim_type=1
	 */
	SAM_EXPORT void SAM_LcoefcrDesign_IPHLCOH_electricity_rate_nset(SAM_table ptr, double number, SAM_error *err);


	/**
	 * SystemControl Getters
	 */

	SAM_EXPORT double SAM_LcoefcrDesign_SystemControl_sim_type_nget(SAM_table ptr, SAM_error *err);


	/**
	 * SimpleLCOE Getters
	 */

	SAM_EXPORT double SAM_LcoefcrDesign_SimpleLCOE_annual_energy_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_LcoefcrDesign_SimpleLCOE_c_construction_cost_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_LcoefcrDesign_SimpleLCOE_c_construction_interest_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_LcoefcrDesign_SimpleLCOE_c_debt_percent_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_LcoefcrDesign_SimpleLCOE_c_depreciation_schedule_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_LcoefcrDesign_SimpleLCOE_c_equity_return_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_LcoefcrDesign_SimpleLCOE_c_inflation_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_LcoefcrDesign_SimpleLCOE_c_lifetime_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_LcoefcrDesign_SimpleLCOE_c_nominal_interest_rate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_LcoefcrDesign_SimpleLCOE_c_tax_rate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_LcoefcrDesign_SimpleLCOE_fixed_operating_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_LcoefcrDesign_SimpleLCOE_ui_fcr_input_option_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_LcoefcrDesign_SimpleLCOE_ui_fixed_charge_rate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_LcoefcrDesign_SimpleLCOE_variable_operating_cost_nget(SAM_table ptr, SAM_error *err);


	/**
	 * SystemCosts Getters
	 */

	SAM_EXPORT double SAM_LcoefcrDesign_SystemCosts_total_installed_cost_nget(SAM_table ptr, SAM_error *err);


	/**
	 * IPHLCOH Getters
	 */

	SAM_EXPORT double SAM_LcoefcrDesign_IPHLCOH_annual_electricity_consumption_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_LcoefcrDesign_IPHLCOH_electricity_rate_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double SAM_LcoefcrDesign_Outputs_cfin_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_LcoefcrDesign_Outputs_crf_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_LcoefcrDesign_Outputs_fixed_charge_rate_calc_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_LcoefcrDesign_Outputs_lcoe_fcr_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_LcoefcrDesign_Outputs_pfin_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_LcoefcrDesign_Outputs_wacc_nget(SAM_table ptr, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif