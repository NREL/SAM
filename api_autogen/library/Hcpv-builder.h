#ifndef SAM_HCPV_FUNCTIONS_H_
#define SAM_HCPV_FUNCTIONS_H_

#include "Hcpv-data.h"

#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	/** 
	 * Create a Module variable table for a HighXConcentratingPVNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_Hcpv_Module SAM_Hcpv_Module_create(const char* def, SAM_error* err);


	/**
	 * Set module_alignment_error: Alignment loss factor
	 * type: numeric
	 * units: 0..1
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Hcpv_Module_module_alignment_error_set(SAM_Hcpv_Module ptr, float number, SAM_error* err);

	/**
	 * Set module_cell_area: Single cell area
	 * type: numeric
	 * units: cm^2
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Hcpv_Module_module_cell_area_set(SAM_Hcpv_Module ptr, float number, SAM_error* err);

	/**
	 * Set module_concentration: Concentration ratio
	 * type: numeric
	 * units: none
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Hcpv_Module_module_concentration_set(SAM_Hcpv_Module ptr, float number, SAM_error* err);

	/**
	 * Set module_flutter_loss_coeff: Wind flutter loss factor
	 * type: numeric
	 * units: 0..1 per m/s
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Hcpv_Module_module_flutter_loss_coeff_set(SAM_Hcpv_Module ptr, float number, SAM_error* err);

	/**
	 * Set module_ncells: Number of cells
	 * type: numeric
	 * units: none
	 * options: None
	 * constraints: INTEGER
	 * required if: None
	 */
	SAM_EXPORT void SAM_Hcpv_Module_module_ncells_set(SAM_Hcpv_Module ptr, float number, SAM_error* err);

	/**
	 * Set module_optical_error: Optical error factor
	 * type: numeric
	 * units: 0..1
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Hcpv_Module_module_optical_error_set(SAM_Hcpv_Module ptr, float number, SAM_error* err);

	/**
	 * Set module_reference: Index in arrays of the reference condition
	 * type: numeric
	 * units: none
	 * options: None
	 * constraints: INTEGER
	 * required if: None
	 */
	SAM_EXPORT void SAM_Hcpv_Module_module_reference_set(SAM_Hcpv_Module ptr, float number, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_Hcpv_Module_module_alignment_error_get(SAM_Hcpv_Module ptr, SAM_error* err);

	SAM_EXPORT float SAM_Hcpv_Module_module_cell_area_get(SAM_Hcpv_Module ptr, SAM_error* err);

	SAM_EXPORT float SAM_Hcpv_Module_module_concentration_get(SAM_Hcpv_Module ptr, SAM_error* err);

	SAM_EXPORT float SAM_Hcpv_Module_module_flutter_loss_coeff_get(SAM_Hcpv_Module ptr, SAM_error* err);

	SAM_EXPORT float SAM_Hcpv_Module_module_ncells_get(SAM_Hcpv_Module ptr, SAM_error* err);

	SAM_EXPORT float SAM_Hcpv_Module_module_optical_error_get(SAM_Hcpv_Module ptr, SAM_error* err);

	SAM_EXPORT float SAM_Hcpv_Module_module_reference_get(SAM_Hcpv_Module ptr, SAM_error* err);



	/** 
	 * Create a Inverter variable table for a HighXConcentratingPVNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_Hcpv_Inverter SAM_Hcpv_Inverter_create(const char* def, SAM_error* err);


	/**
	 * Set inv_snl_c0: Parameter defining the curvature (parabolic) of the relationship between ac-power and dc-power at the reference operating condition, default value of zero gives a linear relationship, (1/W)
	 * type: numeric
	 * units: xxx
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Hcpv_Inverter_inv_snl_c0_set(SAM_Hcpv_Inverter ptr, float number, SAM_error* err);

	/**
	 * Set inv_snl_c1: Empirical coefficient allowing Pdco to vary linearly with dc-voltage input, default value is zero, (1/V)
	 * type: numeric
	 * units: xxx
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Hcpv_Inverter_inv_snl_c1_set(SAM_Hcpv_Inverter ptr, float number, SAM_error* err);

	/**
	 * Set inv_snl_c2: Empirical coefficient allowing Pso to vary linearly with dc-voltage input, default value is zero, (1/V)
	 * type: numeric
	 * units: xxx
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Hcpv_Inverter_inv_snl_c2_set(SAM_Hcpv_Inverter ptr, float number, SAM_error* err);

	/**
	 * Set inv_snl_c3: Empirical coefficient allowing Co to vary linearly with dc-voltage input, default value is zero, (1/V)
	 * type: numeric
	 * units: xxx
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Hcpv_Inverter_inv_snl_c3_set(SAM_Hcpv_Inverter ptr, float number, SAM_error* err);

	/**
	 * Set inv_snl_paco: W maximum ac-power rating for inverter at reference or nominal operating condition, assumed to be an upper limit value, (W)
	 * type: numeric
	 * units: xxx
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Hcpv_Inverter_inv_snl_paco_set(SAM_Hcpv_Inverter ptr, float number, SAM_error* err);

	/**
	 * Set inv_snl_pdco: W dc-power level at which the ac-power rating is achieved at the reference operating condition, (W)
	 * type: numeric
	 * units: xxx
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Hcpv_Inverter_inv_snl_pdco_set(SAM_Hcpv_Inverter ptr, float number, SAM_error* err);

	/**
	 * Set inv_snl_pso: W dc-power required to start the inversion process, or self-consumption by inverter, strongly influences inverter efficiency at low power levels, (W)
	 * type: numeric
	 * units: xxx
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Hcpv_Inverter_inv_snl_pso_set(SAM_Hcpv_Inverter ptr, float number, SAM_error* err);

	/**
	 * Set inv_snl_vdco: V (Vnom) dc-voltage level at which the ac-power rating is achieved at the reference operating condition, (V)
	 * type: numeric
	 * units: xxx
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Hcpv_Inverter_inv_snl_vdco_set(SAM_Hcpv_Inverter ptr, float number, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_Hcpv_Inverter_inv_snl_c0_get(SAM_Hcpv_Inverter ptr, SAM_error* err);

	SAM_EXPORT float SAM_Hcpv_Inverter_inv_snl_c1_get(SAM_Hcpv_Inverter ptr, SAM_error* err);

	SAM_EXPORT float SAM_Hcpv_Inverter_inv_snl_c2_get(SAM_Hcpv_Inverter ptr, SAM_error* err);

	SAM_EXPORT float SAM_Hcpv_Inverter_inv_snl_c3_get(SAM_Hcpv_Inverter ptr, SAM_error* err);

	SAM_EXPORT float SAM_Hcpv_Inverter_inv_snl_paco_get(SAM_Hcpv_Inverter ptr, SAM_error* err);

	SAM_EXPORT float SAM_Hcpv_Inverter_inv_snl_pdco_get(SAM_Hcpv_Inverter ptr, SAM_error* err);

	SAM_EXPORT float SAM_Hcpv_Inverter_inv_snl_pso_get(SAM_Hcpv_Inverter ptr, SAM_error* err);

	SAM_EXPORT float SAM_Hcpv_Inverter_inv_snl_vdco_get(SAM_Hcpv_Inverter ptr, SAM_error* err);



	/** 
	 * Create a Array variable table for a HighXConcentratingPVNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_Hcpv_Array SAM_Hcpv_Array_create(const char* def, SAM_error* err);


	/**
	 * Set array_ac_wiring_loss: AC wiring loss factor
	 * type: numeric
	 * units: 0..1
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Hcpv_Array_array_ac_wiring_loss_set(SAM_Hcpv_Array ptr, float number, SAM_error* err);

	/**
	 * Set array_dc_mismatch_loss: DC module mismatch loss factor
	 * type: numeric
	 * units: 0..1
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Hcpv_Array_array_dc_mismatch_loss_set(SAM_Hcpv_Array ptr, float number, SAM_error* err);

	/**
	 * Set array_dc_wiring_loss: DC Wiring loss factor
	 * type: numeric
	 * units: 0..1
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Hcpv_Array_array_dc_wiring_loss_set(SAM_Hcpv_Array ptr, float number, SAM_error* err);

	/**
	 * Set array_diode_conn_loss: Diodes and connections loss factor
	 * type: numeric
	 * units: 0..1
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Hcpv_Array_array_diode_conn_loss_set(SAM_Hcpv_Array ptr, float number, SAM_error* err);

	/**
	 * Set array_modules_per_tracker: Modules on each tracker
	 * type: numeric
	 * units: none
	 * options: None
	 * constraints: INTEGER
	 * required if: None
	 */
	SAM_EXPORT void SAM_Hcpv_Array_array_modules_per_tracker_set(SAM_Hcpv_Array ptr, float number, SAM_error* err);

	/**
	 * Set array_num_trackers: Number of trackers
	 * type: numeric
	 * units: none
	 * options: None
	 * constraints: INTEGER
	 * required if: None
	 */
	SAM_EXPORT void SAM_Hcpv_Array_array_num_trackers_set(SAM_Hcpv_Array ptr, float number, SAM_error* err);

	/**
	 * Set array_tracker_power_fraction: Single tracker power fraction
	 * type: numeric
	 * units: 0..1
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Hcpv_Array_array_tracker_power_fraction_set(SAM_Hcpv_Array ptr, float number, SAM_error* err);

	/**
	 * Set array_tracking_error: General racking error
	 * type: numeric
	 * units: 0..1
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Hcpv_Array_array_tracking_error_set(SAM_Hcpv_Array ptr, float number, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_Hcpv_Array_array_ac_wiring_loss_get(SAM_Hcpv_Array ptr, SAM_error* err);

	SAM_EXPORT float SAM_Hcpv_Array_array_dc_mismatch_loss_get(SAM_Hcpv_Array ptr, SAM_error* err);

	SAM_EXPORT float SAM_Hcpv_Array_array_dc_wiring_loss_get(SAM_Hcpv_Array ptr, SAM_error* err);

	SAM_EXPORT float SAM_Hcpv_Array_array_diode_conn_loss_get(SAM_Hcpv_Array ptr, SAM_error* err);

	SAM_EXPORT float SAM_Hcpv_Array_array_modules_per_tracker_get(SAM_Hcpv_Array ptr, SAM_error* err);

	SAM_EXPORT float SAM_Hcpv_Array_array_num_trackers_get(SAM_Hcpv_Array ptr, SAM_error* err);

	SAM_EXPORT float SAM_Hcpv_Array_array_tracker_power_fraction_get(SAM_Hcpv_Array ptr, SAM_error* err);

	SAM_EXPORT float SAM_Hcpv_Array_array_tracking_error_get(SAM_Hcpv_Array ptr, SAM_error* err);



	/** 
	 * Create a Common variable table for a HighXConcentratingPVNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_Hcpv_Common SAM_Hcpv_Common_create(const char* def, SAM_error* err);


	/**
	 * Set adjust:constant: Constant loss adjustment
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: MAX=100
	 * required if: None
	 */
	SAM_EXPORT void SAM_Hcpv_Common_adjust:constant_set(SAM_Hcpv_Common ptr, float number, SAM_error* err);

	/**
	 * Set adjust:hourly: Hourly loss adjustments
	 * type: array
	 * units: %
	 * options: None
	 * constraints: LENGTH=8760
	 * required if: None
	 */
	SAM_EXPORT void SAM_Hcpv_Common_adjust:hourly_set(SAM_Hcpv_Common ptr, float* array, int length, SAM_error* err);

	/**
	 * Set adjust:periods: Period-based loss adjustments
	 * type: matrix
	 * units: %
	 * options: n x 3 matrix [ start, end, loss ]
	 * constraints: COLS=3
	 * required if: None
	 */
	SAM_EXPORT void SAM_Hcpv_Common_adjust:periods_set(SAM_Hcpv_Common ptr, float* matrix, int nr, int nc, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_Hcpv_Common_adjust:constant_get(SAM_Hcpv_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Hcpv_Common_adjust:hourly_get(SAM_Hcpv_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Hcpv_Common_adjust:periods_get(SAM_Hcpv_Common ptr, SAM_error* err);



#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif