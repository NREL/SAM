#ifndef SAM_MHKTIDAL_H_
#define SAM_MHKTIDAL_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// MhkTidal Technology Model
	//

	/** 
	 * Create a MhkTidal variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_MhkTidal;

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_MhkTidal_execute(SAM_table data, int verbosity, SAM_error* err);


	//
	// MHKTidal parameters
	//

	/**
	 * Set balance_of_system_cost_total: BOS costs [$]
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_MhkTidal_MHKTidal_balance_of_system_cost_total_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set device_costs_total: Device costs [$]
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_MhkTidal_MHKTidal_device_costs_total_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set financial_cost_total: Financial costs [$]
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_MhkTidal_MHKTidal_financial_cost_total_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set fixed_charge_rate: FCR from LCOE Cost page
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_MhkTidal_MHKTidal_fixed_charge_rate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set loss_additional: Additional losses [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_MhkTidal_MHKTidal_loss_additional_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set loss_array_spacing: Array spacing loss [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_MhkTidal_MHKTidal_loss_array_spacing_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set loss_downtime: Array/WEC downtime loss [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_MhkTidal_MHKTidal_loss_downtime_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set loss_resource_overprediction: Resource overprediction loss [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_MhkTidal_MHKTidal_loss_resource_overprediction_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set loss_transmission: Transmission losses [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_MhkTidal_MHKTidal_loss_transmission_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set number_devices: Number of tidal devices in the system
	 * options: None
	 * constraints: INTEGER
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_MhkTidal_MHKTidal_number_devices_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set system_capacity: System Nameplate Capacity [kW]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_MhkTidal_MHKTidal_system_capacity_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tidal_power_curve: Power curve of tidal energy device as function of stream speeds [kW]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_MhkTidal_MHKTidal_tidal_power_curve_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set tidal_resource: Frequency distribution of resource as a function of stream speeds
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_MhkTidal_MHKTidal_tidal_resource_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set total_operating_cost: O&M costs [$]
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_MhkTidal_MHKTidal_total_operating_cost_nset(SAM_table ptr, double number, SAM_error *err);


	/**
	 * MHKTidal Getters
	 */

	SAM_EXPORT double SAM_MhkTidal_MHKTidal_balance_of_system_cost_total_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkTidal_MHKTidal_device_costs_total_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkTidal_MHKTidal_financial_cost_total_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkTidal_MHKTidal_fixed_charge_rate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkTidal_MHKTidal_loss_additional_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkTidal_MHKTidal_loss_array_spacing_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkTidal_MHKTidal_loss_downtime_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkTidal_MHKTidal_loss_resource_overprediction_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkTidal_MHKTidal_loss_transmission_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkTidal_MHKTidal_number_devices_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkTidal_MHKTidal_system_capacity_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_MhkTidal_MHKTidal_tidal_power_curve_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_MhkTidal_MHKTidal_tidal_resource_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_MhkTidal_MHKTidal_total_operating_cost_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double* SAM_MhkTidal_Outputs_annual_cumulative_energy_distribution_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_MhkTidal_Outputs_annual_energy_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double* SAM_MhkTidal_Outputs_annual_energy_distribution_aget(SAM_table ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_MhkTidal_Outputs_capacity_factor_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkTidal_Outputs_device_average_power_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkTidal_Outputs_device_rated_capacity_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkTidal_Outputs_tidal_power_end_velocity_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkTidal_Outputs_tidal_power_start_velocity_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkTidal_Outputs_tidal_resource_end_velocity_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkTidal_Outputs_tidal_resource_start_velocity_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkTidal_Outputs_total_bos_cost_kwh_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkTidal_Outputs_total_bos_cost_lcoe_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkTidal_Outputs_total_bos_cost_per_kw_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkTidal_Outputs_total_capital_cost_kwh_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkTidal_Outputs_total_capital_cost_lcoe_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkTidal_Outputs_total_capital_cost_per_kw_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkTidal_Outputs_total_device_cost_kwh_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkTidal_Outputs_total_device_cost_lcoe_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkTidal_Outputs_total_device_cost_per_kw_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkTidal_Outputs_total_financial_cost_kwh_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkTidal_Outputs_total_financial_cost_lcoe_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkTidal_Outputs_total_financial_cost_per_kw_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkTidal_Outputs_total_om_cost_kwh_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkTidal_Outputs_total_om_cost_lcoe_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkTidal_Outputs_total_operations_cost_per_kw_nget(SAM_table ptr, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif