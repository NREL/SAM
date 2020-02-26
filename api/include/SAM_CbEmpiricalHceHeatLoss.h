#ifndef SAM_CBEMPIRICALHCEHEATLOSS_H_
#define SAM_CBEMPIRICALHCEHEATLOSS_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// CbEmpiricalHceHeatLoss Technology Model
	//

	/** 
	 * Create a CbEmpiricalHceHeatLoss variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_CbEmpiricalHceHeatLoss;

	SAM_EXPORT SAM_CbEmpiricalHceHeatLoss SAM_CbEmpiricalHceHeatLoss_construct(const char* def, SAM_error* err);

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_CbEmpiricalHceHeatLoss_execute(SAM_CbEmpiricalHceHeatLoss data, int verbosity, SAM_error* err);

	SAM_EXPORT void SAM_CbEmpiricalHceHeatLoss_destruct(SAM_CbEmpiricalHceHeatLoss system);


	//
	// Hce parameters
	//

	/**
	 * Set HCEFrac: Fraction of field that is this type of HCE
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CbEmpiricalHceHeatLoss_Hce_HCEFrac_aset(SAM_CbEmpiricalHceHeatLoss ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set HCE_A0: label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CbEmpiricalHceHeatLoss_Hce_HCE_A0_aset(SAM_CbEmpiricalHceHeatLoss ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set HCE_A1: label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CbEmpiricalHceHeatLoss_Hce_HCE_A1_aset(SAM_CbEmpiricalHceHeatLoss ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set HCE_A2: label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CbEmpiricalHceHeatLoss_Hce_HCE_A2_aset(SAM_CbEmpiricalHceHeatLoss ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set HCE_A3: label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CbEmpiricalHceHeatLoss_Hce_HCE_A3_aset(SAM_CbEmpiricalHceHeatLoss ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set HCE_A4: label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CbEmpiricalHceHeatLoss_Hce_HCE_A4_aset(SAM_CbEmpiricalHceHeatLoss ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set HCE_A5: label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CbEmpiricalHceHeatLoss_Hce_HCE_A5_aset(SAM_CbEmpiricalHceHeatLoss ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set HCE_A6: label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CbEmpiricalHceHeatLoss_Hce_HCE_A6_aset(SAM_CbEmpiricalHceHeatLoss ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set PerfFac: label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CbEmpiricalHceHeatLoss_Hce_PerfFac_aset(SAM_CbEmpiricalHceHeatLoss ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set RefMirrAper: label
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CbEmpiricalHceHeatLoss_Hce_RefMirrAper_aset(SAM_CbEmpiricalHceHeatLoss ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set SfInTempD: Solar Field Inlet Temp at design [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CbEmpiricalHceHeatLoss_Hce_SfInTempD_nset(SAM_CbEmpiricalHceHeatLoss ptr, double number, SAM_error *err);

	/**
	 * Set SfOutTempD: Solar Field Outlet Temp at design [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CbEmpiricalHceHeatLoss_Hce_SfOutTempD_nset(SAM_CbEmpiricalHceHeatLoss ptr, double number, SAM_error *err);

	/**
	 * Set ui_reference_ambient_temperature: Ambient temp at design heat loss [C]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CbEmpiricalHceHeatLoss_Hce_ui_reference_ambient_temperature_nset(SAM_CbEmpiricalHceHeatLoss ptr, double number, SAM_error *err);

	/**
	 * Set ui_reference_direct_normal_irradiance: DNI at design [W/m2]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CbEmpiricalHceHeatLoss_Hce_ui_reference_direct_normal_irradiance_nset(SAM_CbEmpiricalHceHeatLoss ptr, double number, SAM_error *err);

	/**
	 * Set ui_reference_wind_speed: Wind speed for design heat loss [m/s]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_CbEmpiricalHceHeatLoss_Hce_ui_reference_wind_speed_nset(SAM_CbEmpiricalHceHeatLoss ptr, double number, SAM_error *err);


	/**
	 * Hce Getters
	 */

	SAM_EXPORT double* SAM_CbEmpiricalHceHeatLoss_Hce_HCEFrac_aget(SAM_CbEmpiricalHceHeatLoss ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_CbEmpiricalHceHeatLoss_Hce_HCE_A0_aget(SAM_CbEmpiricalHceHeatLoss ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_CbEmpiricalHceHeatLoss_Hce_HCE_A1_aget(SAM_CbEmpiricalHceHeatLoss ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_CbEmpiricalHceHeatLoss_Hce_HCE_A2_aget(SAM_CbEmpiricalHceHeatLoss ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_CbEmpiricalHceHeatLoss_Hce_HCE_A3_aget(SAM_CbEmpiricalHceHeatLoss ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_CbEmpiricalHceHeatLoss_Hce_HCE_A4_aget(SAM_CbEmpiricalHceHeatLoss ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_CbEmpiricalHceHeatLoss_Hce_HCE_A5_aget(SAM_CbEmpiricalHceHeatLoss ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_CbEmpiricalHceHeatLoss_Hce_HCE_A6_aget(SAM_CbEmpiricalHceHeatLoss ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_CbEmpiricalHceHeatLoss_Hce_PerfFac_aget(SAM_CbEmpiricalHceHeatLoss ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_CbEmpiricalHceHeatLoss_Hce_RefMirrAper_aget(SAM_CbEmpiricalHceHeatLoss ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_CbEmpiricalHceHeatLoss_Hce_SfInTempD_nget(SAM_CbEmpiricalHceHeatLoss ptr, SAM_error *err);

	SAM_EXPORT double SAM_CbEmpiricalHceHeatLoss_Hce_SfOutTempD_nget(SAM_CbEmpiricalHceHeatLoss ptr, SAM_error *err);

	SAM_EXPORT double SAM_CbEmpiricalHceHeatLoss_Hce_ui_reference_ambient_temperature_nget(SAM_CbEmpiricalHceHeatLoss ptr, SAM_error *err);

	SAM_EXPORT double SAM_CbEmpiricalHceHeatLoss_Hce_ui_reference_direct_normal_irradiance_nget(SAM_CbEmpiricalHceHeatLoss ptr, SAM_error *err);

	SAM_EXPORT double SAM_CbEmpiricalHceHeatLoss_Hce_ui_reference_wind_speed_nget(SAM_CbEmpiricalHceHeatLoss ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double* SAM_CbEmpiricalHceHeatLoss_Outputs_HL_aget(SAM_CbEmpiricalHceHeatLoss ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_CbEmpiricalHceHeatLoss_Outputs_HL_weighted_nget(SAM_CbEmpiricalHceHeatLoss ptr, SAM_error *err);

	SAM_EXPORT double SAM_CbEmpiricalHceHeatLoss_Outputs_HL_weighted_m2_nget(SAM_CbEmpiricalHceHeatLoss ptr, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif