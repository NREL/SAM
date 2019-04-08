#ifndef SAM_TCSLINEARFRESNEL_FUNCTIONS_H_
#define SAM_TCSLINEARFRESNEL_FUNCTIONS_H_

#include "TcslinearFresnel-data.h"

#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	/** 
	 * Create a SolarField variable table for a DSLFNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_TcslinearFresnel_SolarField SAM_TcslinearFresnel_SolarField_create(const char* def, SAM_error* err);


	/**
	 * Set I_bn_des: Design point irradiation value
	 * type: numeric
	 * units: W/m2
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_SolarField_I_bn_des_set(SAM_TcslinearFresnel_SolarField ptr, float number, SAM_error* err);

	/**
	 * Set T_amb_des_sf: Design-point ambient temperature
	 * type: numeric
	 * units: C
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_SolarField_T_amb_des_sf_set(SAM_TcslinearFresnel_SolarField ptr, float number, SAM_error* err);

	/**
	 * Set T_cold_ref: Reference HTF outlet temperature at design
	 * type: numeric
	 * units: C
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_SolarField_T_cold_ref_set(SAM_TcslinearFresnel_SolarField ptr, float number, SAM_error* err);

	/**
	 * Set T_hot: Hot HTF inlet temperature, from storage tank
	 * type: numeric
	 * units: C
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_SolarField_T_hot_set(SAM_TcslinearFresnel_SolarField ptr, float number, SAM_error* err);

	/**
	 * Set csp.lf.sf.sh_geom_unique: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_SolarField_csp.lf.sf.sh_geom_unique_set(SAM_TcslinearFresnel_SolarField ptr, const char* string, SAM_error* err);

	/**
	 * Set csp.lf.sf.sm_or_area: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_SolarField_csp.lf.sf.sm_or_area_set(SAM_TcslinearFresnel_SolarField ptr, const char* string, SAM_error* err);

	/**
	 * Set csp.lf.sf.specified_solar_multiple: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_SolarField_csp.lf.sf.specified_solar_multiple_set(SAM_TcslinearFresnel_SolarField ptr, const char* string, SAM_error* err);

	/**
	 * Set csp.lf.sf.specified_total_aperture: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_SolarField_csp.lf.sf.specified_total_aperture_set(SAM_TcslinearFresnel_SolarField ptr, const char* string, SAM_error* err);

	/**
	 * Set fP_boil_to_sh: Design-point pressure drop between the boiler and superheater frac
	 * type: numeric
	 * units: none
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_SolarField_fP_boil_to_sh_set(SAM_TcslinearFresnel_SolarField ptr, float number, SAM_error* err);

	/**
	 * Set fP_hdr_c: Average design-point cold header pressure drop fraction
	 * type: numeric
	 * units: none
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_SolarField_fP_hdr_c_set(SAM_TcslinearFresnel_SolarField ptr, float number, SAM_error* err);

	/**
	 * Set fP_hdr_h: Average design-point hot header pressure drop fraction
	 * type: numeric
	 * units: none
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_SolarField_fP_hdr_h_set(SAM_TcslinearFresnel_SolarField ptr, float number, SAM_error* err);

	/**
	 * Set fP_sf_boil: Design-point pressure drop across the solar field boiler fraction
	 * type: numeric
	 * units: none
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_SolarField_fP_sf_boil_set(SAM_TcslinearFresnel_SolarField ptr, float number, SAM_error* err);

	/**
	 * Set fP_sf_sh: Design-point pressure drop across the solar field superheater frac
	 * type: numeric
	 * units: none
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_SolarField_fP_sf_sh_set(SAM_TcslinearFresnel_SolarField ptr, float number, SAM_error* err);

	/**
	 * Set is_oncethru: Flag indicating whether flow is once through with superheat
	 * type: numeric
	 * units: none
	 * options: None
	 * constraints: INTEGER
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_SolarField_is_oncethru_set(SAM_TcslinearFresnel_SolarField ptr, float number, SAM_error* err);

	/**
	 * Set nModBoil: Number of modules in the boiler section
	 * type: numeric
	 * units: none
	 * options: None
	 * constraints: INTEGER
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_SolarField_nModBoil_set(SAM_TcslinearFresnel_SolarField ptr, float number, SAM_error* err);

	/**
	 * Set nModSH: Number of modules in the superheater section
	 * type: numeric
	 * units: none
	 * options: None
	 * constraints: INTEGER
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_SolarField_nModSH_set(SAM_TcslinearFresnel_SolarField ptr, float number, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_TcslinearFresnel_SolarField_I_bn_des_get(SAM_TcslinearFresnel_SolarField ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcslinearFresnel_SolarField_T_amb_des_sf_get(SAM_TcslinearFresnel_SolarField ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcslinearFresnel_SolarField_T_cold_ref_get(SAM_TcslinearFresnel_SolarField ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcslinearFresnel_SolarField_T_hot_get(SAM_TcslinearFresnel_SolarField ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcslinearFresnel_SolarField_csp.lf.sf.sh_geom_unique_get(SAM_TcslinearFresnel_SolarField ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcslinearFresnel_SolarField_csp.lf.sf.sm_or_area_get(SAM_TcslinearFresnel_SolarField ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcslinearFresnel_SolarField_csp.lf.sf.specified_solar_multiple_get(SAM_TcslinearFresnel_SolarField ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcslinearFresnel_SolarField_csp.lf.sf.specified_total_aperture_get(SAM_TcslinearFresnel_SolarField ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcslinearFresnel_SolarField_fP_boil_to_sh_get(SAM_TcslinearFresnel_SolarField ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcslinearFresnel_SolarField_fP_hdr_c_get(SAM_TcslinearFresnel_SolarField ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcslinearFresnel_SolarField_fP_hdr_h_get(SAM_TcslinearFresnel_SolarField ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcslinearFresnel_SolarField_fP_sf_boil_get(SAM_TcslinearFresnel_SolarField ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcslinearFresnel_SolarField_fP_sf_sh_get(SAM_TcslinearFresnel_SolarField ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcslinearFresnel_SolarField_is_oncethru_get(SAM_TcslinearFresnel_SolarField ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcslinearFresnel_SolarField_nModBoil_get(SAM_TcslinearFresnel_SolarField ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcslinearFresnel_SolarField_nModSH_get(SAM_TcslinearFresnel_SolarField ptr, SAM_error* err);



	/** 
	 * Create a CollectorAndReceiver variable table for a DSLFNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_TcslinearFresnel_CollectorAndReceiver SAM_TcslinearFresnel_CollectorAndReceiver_create(const char* def, SAM_error* err);


	/**
	 * Set csp.lf.geom1.refl_aper_area: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_CollectorAndReceiver_csp.lf.geom1.refl_aper_area_set(SAM_TcslinearFresnel_CollectorAndReceiver ptr, const char* string, SAM_error* err);

	/**
	 * Set csp.lf.geom2.coll_length: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_CollectorAndReceiver_csp.lf.geom2.coll_length_set(SAM_TcslinearFresnel_CollectorAndReceiver ptr, const char* string, SAM_error* err);

	/**
	 * Set csp.lf.geom2.refl_aper_area: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_CollectorAndReceiver_csp.lf.geom2.refl_aper_area_set(SAM_TcslinearFresnel_CollectorAndReceiver ptr, const char* string, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT const char* SAM_TcslinearFresnel_CollectorAndReceiver_csp.lf.geom1.refl_aper_area_get(SAM_TcslinearFresnel_CollectorAndReceiver ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcslinearFresnel_CollectorAndReceiver_csp.lf.geom2.coll_length_get(SAM_TcslinearFresnel_CollectorAndReceiver ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcslinearFresnel_CollectorAndReceiver_csp.lf.geom2.refl_aper_area_get(SAM_TcslinearFresnel_CollectorAndReceiver ptr, SAM_error* err);



	/** 
	 * Create a PowerCycle variable table for a DSLFNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_TcslinearFresnel_PowerCycle SAM_TcslinearFresnel_PowerCycle_create(const char* def, SAM_error* err);


	/**
	 * Set P_boil_des: Boiler operating pressure @ design
	 * type: numeric
	 * units: bar
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_PowerCycle_P_boil_des_set(SAM_TcslinearFresnel_PowerCycle ptr, float number, SAM_error* err);

	/**
	 * Set demand_var: Control signal indicating operational mode
	 * type: numeric
	 * units: none
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_PowerCycle_demand_var_set(SAM_TcslinearFresnel_PowerCycle ptr, float number, SAM_error* err);

	/**
	 * Set eta_ref: Reference conversion efficiency at design condition
	 * type: numeric
	 * units: none
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_PowerCycle_eta_ref_set(SAM_TcslinearFresnel_PowerCycle ptr, float number, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_TcslinearFresnel_PowerCycle_P_boil_des_get(SAM_TcslinearFresnel_PowerCycle ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcslinearFresnel_PowerCycle_demand_var_get(SAM_TcslinearFresnel_PowerCycle ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcslinearFresnel_PowerCycle_eta_ref_get(SAM_TcslinearFresnel_PowerCycle ptr, SAM_error* err);



	/** 
	 * Create a Parasitics variable table for a DSLFNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_TcslinearFresnel_Parasitics SAM_TcslinearFresnel_Parasitics_create(const char* def, SAM_error* err);


	/**
	 * Set PB_fixed_par: fraction of rated gross power consumed at all hours of the year
	 * type: numeric
	 * units: none
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Parasitics_PB_fixed_par_set(SAM_TcslinearFresnel_Parasitics ptr, float number, SAM_error* err);

	/**
	 * Set Pipe_hl_coef: Loss coefficient from the header.. runner pipe.. and non-HCE pipin
	 * type: numeric
	 * units: W/m2-K
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Parasitics_Pipe_hl_coef_set(SAM_TcslinearFresnel_Parasitics ptr, float number, SAM_error* err);

	/**
	 * Set SCA_drives_elec: Tracking power.. in Watts per SCA drive
	 * type: numeric
	 * units: W/m2
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Parasitics_SCA_drives_elec_set(SAM_TcslinearFresnel_Parasitics ptr, float number, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_TcslinearFresnel_Parasitics_PB_fixed_par_get(SAM_TcslinearFresnel_Parasitics ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcslinearFresnel_Parasitics_Pipe_hl_coef_get(SAM_TcslinearFresnel_Parasitics ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcslinearFresnel_Parasitics_SCA_drives_elec_get(SAM_TcslinearFresnel_Parasitics ptr, SAM_error* err);



	/** 
	 * Create a Common variable table for a DSLFNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_TcslinearFresnel_Common SAM_TcslinearFresnel_Common_create(const char* def, SAM_error* err);


	/**
	 * Set adjust:constant: Constant loss adjustment
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: MAX=100
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Common_adjust:constant_set(SAM_TcslinearFresnel_Common ptr, float number, SAM_error* err);

	/**
	 * Set adjust:hourly: Hourly loss adjustments
	 * type: array
	 * units: %
	 * options: None
	 * constraints: LENGTH=8760
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Common_adjust:hourly_set(SAM_TcslinearFresnel_Common ptr, float* array, int length, SAM_error* err);

	/**
	 * Set adjust:periods: Period-based loss adjustments
	 * type: matrix
	 * units: %
	 * options: n x 3 matrix [ start, end, loss ]
	 * constraints: COLS=3
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcslinearFresnel_Common_adjust:periods_set(SAM_TcslinearFresnel_Common ptr, float* matrix, int nr, int nc, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_TcslinearFresnel_Common_adjust:constant_get(SAM_TcslinearFresnel_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_TcslinearFresnel_Common_adjust:hourly_get(SAM_TcslinearFresnel_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_TcslinearFresnel_Common_adjust:periods_get(SAM_TcslinearFresnel_Common ptr, SAM_error* err);



#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif