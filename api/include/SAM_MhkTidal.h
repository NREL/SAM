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
	 * Set annual_energy_loss: Total energy losses [%]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_MhkTidal_MHKTidal_annual_energy_loss_nset(SAM_MhkTidal ptr, double number, SAM_error *err);

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

	SAM_EXPORT double SAM_MhkTidal_MHKTidal_annual_energy_loss_nget(SAM_MhkTidal ptr, SAM_error *err);

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