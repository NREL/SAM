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

	SAM_EXPORT SAM_MhkWave SAM_MhkWave_construct(const char* def, SAM_error* err);

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_MhkWave_execute(SAM_MhkWave data, int verbosity, SAM_error* err);

	SAM_EXPORT void SAM_MhkWave_destruct(SAM_MhkWave system);


	//
	// MHKWave parameters
	//

	/**
	 * Set annual_energy_loss: Total energy losses [%]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_MhkWave_MHKWave_annual_energy_loss_nset(SAM_MhkWave ptr, double number, SAM_error *err);

	/**
	 * Set number_devices: Number of wave devices in the system
	 * options: None
	 * constraints: INTEGER
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_MhkWave_MHKWave_number_devices_nset(SAM_MhkWave ptr, double number, SAM_error *err);

	/**
	 * Set system_capacity: System Nameplate Capacity [kW]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_MhkWave_MHKWave_system_capacity_nset(SAM_MhkWave ptr, double number, SAM_error *err);

	/**
	 * Set wave_power_matrix: Wave Power Matrix
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_MhkWave_MHKWave_wave_power_matrix_mset(SAM_MhkWave ptr, double* mat, int nrows, int ncols, SAM_error *err);

	/**
	 * Set wave_resource_matrix: Frequency distribution of wave resource as a function of Hs and Te
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_MhkWave_MHKWave_wave_resource_matrix_mset(SAM_MhkWave ptr, double* mat, int nrows, int ncols, SAM_error *err);


	/**
	 * MHKWave Getters
	 */

	SAM_EXPORT double SAM_MhkWave_MHKWave_annual_energy_loss_nget(SAM_MhkWave ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkWave_MHKWave_number_devices_nget(SAM_MhkWave ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkWave_MHKWave_system_capacity_nget(SAM_MhkWave ptr, SAM_error *err);

	SAM_EXPORT double* SAM_MhkWave_MHKWave_wave_power_matrix_mget(SAM_MhkWave ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double* SAM_MhkWave_MHKWave_wave_resource_matrix_mget(SAM_MhkWave ptr, int* nrows, int* ncols, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double SAM_MhkWave_Outputs_annual_energy_nget(SAM_MhkWave ptr, SAM_error *err);

	SAM_EXPORT double* SAM_MhkWave_Outputs_annual_energy_distribution_mget(SAM_MhkWave ptr, int* nrows, int* ncols, SAM_error *err);

	SAM_EXPORT double SAM_MhkWave_Outputs_capacity_factor_nget(SAM_MhkWave ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkWave_Outputs_device_average_power_nget(SAM_MhkWave ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkWave_Outputs_device_rated_capacity_nget(SAM_MhkWave ptr, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif