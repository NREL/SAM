#ifndef SAM_MHKWAVE_H_
#define SAM_MHKWAVE_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// MhkWave Technology Model
	//

	/** 
	 * Create a MhkWave variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_MhkWave;

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_MhkWave_execute(SAM_table data, int verbosity, SAM_error* err);


	//
	// MHKWave parameters
	//

	/**
	 * Set balance_of_system_cost_total: BOS costs [$]
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_MhkWave_MHKWave_balance_of_system_cost_total_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set device_costs_total: Device costs [$]
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_MhkWave_MHKWave_device_costs_total_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set device_rated_power: Rated capacity of device [kW]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_MhkWave_MHKWave_device_rated_power_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set financial_cost_total: Financial costs [$]
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_MhkWave_MHKWave_financial_cost_total_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set fixed_charge_rate: FCR from LCOE Cost page
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_MhkWave_MHKWave_fixed_charge_rate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set loss_additional: Additional losses [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_MhkWave_MHKWave_loss_additional_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set loss_array_spacing: Array spacing loss [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_MhkWave_MHKWave_loss_array_spacing_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set loss_downtime: Array/WEC downtime loss [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_MhkWave_MHKWave_loss_downtime_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set loss_resource_overprediction: Resource overprediction loss [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_MhkWave_MHKWave_loss_resource_overprediction_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set loss_transmission: Transmission losses [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_MhkWave_MHKWave_loss_transmission_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set number_devices: Number of wave devices in the system
	 * options: None
	 * constraints: INTEGER
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_MhkWave_MHKWave_number_devices_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set system_capacity: System Nameplate Capacity [kW]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_MhkWave_MHKWave_system_capacity_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set total_operating_cost: O&M costs [$]
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_MhkWave_MHKWave_total_operating_cost_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set wave_power_matrix: Wave Power Matrix
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_MhkWave_MHKWave_wave_power_matrix_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set wave_resource_matrix: Frequency distribution of wave resource as a function of Hs and Te
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_MhkWave_MHKWave_wave_resource_matrix_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);


	/**
	 * MHKWave Getters
	 */

	SAM_EXPORT double SAM_MhkWave_MHKWave_balance_of_system_cost_total_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkWave_MHKWave_device_costs_total_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkWave_MHKWave_device_rated_power_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkWave_MHKWave_financial_cost_total_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkWave_MHKWave_fixed_charge_rate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkWave_MHKWave_loss_additional_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkWave_MHKWave_loss_array_spacing_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkWave_MHKWave_loss_downtime_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkWave_MHKWave_loss_resource_overprediction_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkWave_MHKWave_loss_transmission_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkWave_MHKWave_number_devices_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkWave_MHKWave_system_capacity_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkWave_MHKWave_total_operating_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_MhkWave_MHKWave_wave_power_matrix_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_MhkWave_MHKWave_wave_resource_matrix_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double SAM_MhkWave_Outputs_annual_energy_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_MhkWave_Outputs_annual_energy_distribution_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_MhkWave_Outputs_capacity_factor_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkWave_Outputs_device_average_power_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkWave_Outputs_total_bos_cost_kwh_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkWave_Outputs_total_bos_cost_lcoe_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkWave_Outputs_total_capital_cost_kwh_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkWave_Outputs_total_capital_cost_lcoe_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkWave_Outputs_total_device_cost_kwh_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkWave_Outputs_total_device_cost_lcoe_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkWave_Outputs_total_financial_cost_kwh_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkWave_Outputs_total_financial_cost_lcoe_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkWave_Outputs_total_om_cost_kwh_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkWave_Outputs_total_om_cost_lcoe_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkWave_Outputs_wave_power_end_height_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkWave_Outputs_wave_power_end_period_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkWave_Outputs_wave_power_start_height_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkWave_Outputs_wave_power_start_period_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkWave_Outputs_wave_resource_end_height_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkWave_Outputs_wave_resource_end_period_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkWave_Outputs_wave_resource_start_height_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkWave_Outputs_wave_resource_start_period_nget(SAM_table ptr, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif