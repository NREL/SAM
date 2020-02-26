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

	SAM_EXPORT SAM_MhkTidal SAM_MhkTidal_construct(const char* def, SAM_error* err);

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_MhkTidal_execute(SAM_MhkTidal data, int verbosity, SAM_error* err);

	SAM_EXPORT void SAM_MhkTidal_destruct(SAM_MhkTidal system);


	//
	// MHKTidal parameters
	//

	/**
	 * Set loss_additional: Additional losses [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_MhkTidal_MHKTidal_loss_additional_nset(SAM_MhkTidal ptr, double number, SAM_error *err);

	/**
	 * Set loss_array_spacing: Array spacing loss [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_MhkTidal_MHKTidal_loss_array_spacing_nset(SAM_MhkTidal ptr, double number, SAM_error *err);

	/**
	 * Set loss_downtime: Array/WEC downtime loss [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_MhkTidal_MHKTidal_loss_downtime_nset(SAM_MhkTidal ptr, double number, SAM_error *err);

	/**
	 * Set loss_resource_overprediction: Resource overprediction loss [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_MhkTidal_MHKTidal_loss_resource_overprediction_nset(SAM_MhkTidal ptr, double number, SAM_error *err);

	/**
	 * Set loss_transmission: Transmission losses [%]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_MhkTidal_MHKTidal_loss_transmission_nset(SAM_MhkTidal ptr, double number, SAM_error *err);

	/**
	 * Set number_devices: Number of tidal devices in the system
	 * options: None
	 * constraints: INTEGER
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_MhkTidal_MHKTidal_number_devices_nset(SAM_MhkTidal ptr, double number, SAM_error *err);

	/**
	 * Set tidal_power_curve: Power curve of tidal energy device as function of stream speeds [kW]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_MhkTidal_MHKTidal_tidal_power_curve_mset(SAM_MhkTidal ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set tidal_resource: Frequency distribution of resource as a function of stream speeds
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_MhkTidal_MHKTidal_tidal_resource_mset(SAM_MhkTidal ptr, double* mat, int nrows, int ncols, SAM_error *err);


	/**
	 * MHKTidal Getters
	 */

	SAM_EXPORT double SAM_MhkTidal_MHKTidal_loss_additional_nget(SAM_MhkTidal ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkTidal_MHKTidal_loss_array_spacing_nget(SAM_MhkTidal ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkTidal_MHKTidal_loss_downtime_nget(SAM_MhkTidal ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkTidal_MHKTidal_loss_resource_overprediction_nget(SAM_MhkTidal ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkTidal_MHKTidal_loss_transmission_nget(SAM_MhkTidal ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkTidal_MHKTidal_number_devices_nget(SAM_MhkTidal ptr, SAM_error *err);

	SAM_EXPORT double* SAM_MhkTidal_MHKTidal_tidal_power_curve_mget(SAM_MhkTidal ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_MhkTidal_MHKTidal_tidal_resource_mget(SAM_MhkTidal ptr, int* nrows, int* ncols, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double* SAM_MhkTidal_Outputs_annual_cumulative_energy_distribution_aget(SAM_MhkTidal ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_MhkTidal_Outputs_annual_energy_nget(SAM_MhkTidal ptr, SAM_error *err);

	SAM_EXPORT double* SAM_MhkTidal_Outputs_annual_energy_distribution_aget(SAM_MhkTidal ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_MhkTidal_Outputs_capacity_factor_nget(SAM_MhkTidal ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkTidal_Outputs_device_average_power_nget(SAM_MhkTidal ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkTidal_Outputs_device_rated_capacity_nget(SAM_MhkTidal ptr, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif