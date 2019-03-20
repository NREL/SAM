#ifndef SAM_LINEARFRESNELDSGIPH_FUNCTIONS_H_
#define SAM_LINEARFRESNELDSGIPH_FUNCTIONS_H_

#include "LinearFresnelDsgIph-data.h"

#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	/** 
	 * Create a SystemDesign variable table for a DSGLIPHNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_LinearFresnelDsgIph_SystemDesign SAM_LinearFresnelDsgIph_SystemDesign_create(const char* def, SAM_error* err);


	/**
	 * Set I_bn_des: Design point irradiation value
	 * type: numeric
	 * units: W/m2
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_LinearFresnelDsgIph_SystemDesign_I_bn_des_set(SAM_LinearFresnelDsgIph_SystemDesign ptr, float number, SAM_error* err);

	/**
	 * Set P_turb_des: Design-point turbine inlet pressure
	 * type: numeric
	 * units: bar
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_LinearFresnelDsgIph_SystemDesign_P_turb_des_set(SAM_LinearFresnelDsgIph_SystemDesign ptr, float number, SAM_error* err);

	/**
	 * Set T_cold_ref: Reference HTF outlet temperature at design
	 * type: numeric
	 * units: C
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_LinearFresnelDsgIph_SystemDesign_T_cold_ref_set(SAM_LinearFresnelDsgIph_SystemDesign ptr, float number, SAM_error* err);

	/**
	 * Set T_hot: Hot HTF inlet temperature, from storage tank
	 * type: numeric
	 * units: C
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_LinearFresnelDsgIph_SystemDesign_T_hot_set(SAM_LinearFresnelDsgIph_SystemDesign ptr, float number, SAM_error* err);

	/**
	 * Set q_pb_des: Design heat input to the power block
	 * type: numeric
	 * units: MW
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_LinearFresnelDsgIph_SystemDesign_q_pb_des_set(SAM_LinearFresnelDsgIph_SystemDesign ptr, float number, SAM_error* err);

	/**
	 * Set specified_solar_multiple: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_LinearFresnelDsgIph_SystemDesign_specified_solar_multiple_set(SAM_LinearFresnelDsgIph_SystemDesign ptr, const char* string, SAM_error* err);

	/**
	 * Set x_b_des: Design point boiler outlet steam quality
	 * type: numeric
	 * units: none
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_LinearFresnelDsgIph_SystemDesign_x_b_des_set(SAM_LinearFresnelDsgIph_SystemDesign ptr, float number, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_LinearFresnelDsgIph_SystemDesign_I_bn_des_get(SAM_LinearFresnelDsgIph_SystemDesign ptr, SAM_error* err);

	SAM_EXPORT float SAM_LinearFresnelDsgIph_SystemDesign_P_turb_des_get(SAM_LinearFresnelDsgIph_SystemDesign ptr, SAM_error* err);

	SAM_EXPORT float SAM_LinearFresnelDsgIph_SystemDesign_T_cold_ref_get(SAM_LinearFresnelDsgIph_SystemDesign ptr, SAM_error* err);

	SAM_EXPORT float SAM_LinearFresnelDsgIph_SystemDesign_T_hot_get(SAM_LinearFresnelDsgIph_SystemDesign ptr, SAM_error* err);

	SAM_EXPORT float SAM_LinearFresnelDsgIph_SystemDesign_q_pb_des_get(SAM_LinearFresnelDsgIph_SystemDesign ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_LinearFresnelDsgIph_SystemDesign_specified_solar_multiple_get(SAM_LinearFresnelDsgIph_SystemDesign ptr, SAM_error* err);

	SAM_EXPORT float SAM_LinearFresnelDsgIph_SystemDesign_x_b_des_get(SAM_LinearFresnelDsgIph_SystemDesign ptr, SAM_error* err);



	/** 
	 * Create a SolarField variable table for a DSGLIPHNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_LinearFresnelDsgIph_SolarField SAM_LinearFresnelDsgIph_SolarField_create(const char* def, SAM_error* err);


	/**
	 * Set T_amb_des_sf: Design-point ambient temperature
	 * type: numeric
	 * units: C
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_LinearFresnelDsgIph_SolarField_T_amb_des_sf_set(SAM_LinearFresnelDsgIph_SolarField ptr, float number, SAM_error* err);

	/**
	 * Set fP_hdr_c: Average design-point cold header pressure drop fraction
	 * type: numeric
	 * units: none
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_LinearFresnelDsgIph_SolarField_fP_hdr_c_set(SAM_LinearFresnelDsgIph_SolarField ptr, float number, SAM_error* err);

	/**
	 * Set fP_hdr_h: Average design-point hot header pressure drop fraction
	 * type: numeric
	 * units: none
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_LinearFresnelDsgIph_SolarField_fP_hdr_h_set(SAM_LinearFresnelDsgIph_SolarField ptr, float number, SAM_error* err);

	/**
	 * Set fP_sf_boil: Design-point pressure drop across the solar field boiler fraction
	 * type: numeric
	 * units: none
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_LinearFresnelDsgIph_SolarField_fP_sf_boil_set(SAM_LinearFresnelDsgIph_SolarField ptr, float number, SAM_error* err);

	/**
	 * Set nModBoil: Number of modules in the boiler section
	 * type: numeric
	 * units: none
	 * options: None
	 * constraints: INTEGER
	 * required if: None
	 */
	SAM_EXPORT void SAM_LinearFresnelDsgIph_SolarField_nModBoil_set(SAM_LinearFresnelDsgIph_SolarField ptr, float number, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_LinearFresnelDsgIph_SolarField_T_amb_des_sf_get(SAM_LinearFresnelDsgIph_SolarField ptr, SAM_error* err);

	SAM_EXPORT float SAM_LinearFresnelDsgIph_SolarField_fP_hdr_c_get(SAM_LinearFresnelDsgIph_SolarField ptr, SAM_error* err);

	SAM_EXPORT float SAM_LinearFresnelDsgIph_SolarField_fP_hdr_h_get(SAM_LinearFresnelDsgIph_SolarField ptr, SAM_error* err);

	SAM_EXPORT float SAM_LinearFresnelDsgIph_SolarField_fP_sf_boil_get(SAM_LinearFresnelDsgIph_SolarField ptr, SAM_error* err);

	SAM_EXPORT float SAM_LinearFresnelDsgIph_SolarField_nModBoil_get(SAM_LinearFresnelDsgIph_SolarField ptr, SAM_error* err);



	/** 
	 * Create a CollectorAndReceiver variable table for a DSGLIPHNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_LinearFresnelDsgIph_CollectorAndReceiver SAM_LinearFresnelDsgIph_CollectorAndReceiver_create(const char* def, SAM_error* err);


	/**
	 * Set csp.lf.geom1.coll_length: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_LinearFresnelDsgIph_CollectorAndReceiver_csp.lf.geom1.coll_length_set(SAM_LinearFresnelDsgIph_CollectorAndReceiver ptr, const char* string, SAM_error* err);

	/**
	 * Set csp.lf.geom1.refl_aper_area: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_LinearFresnelDsgIph_CollectorAndReceiver_csp.lf.geom1.refl_aper_area_set(SAM_LinearFresnelDsgIph_CollectorAndReceiver ptr, const char* string, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT const char* SAM_LinearFresnelDsgIph_CollectorAndReceiver_csp.lf.geom1.coll_length_get(SAM_LinearFresnelDsgIph_CollectorAndReceiver ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_LinearFresnelDsgIph_CollectorAndReceiver_csp.lf.geom1.refl_aper_area_get(SAM_LinearFresnelDsgIph_CollectorAndReceiver ptr, SAM_error* err);



	/** 
	 * Create a Common variable table for a DSGLIPHNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_LinearFresnelDsgIph_Common SAM_LinearFresnelDsgIph_Common_create(const char* def, SAM_error* err);


	/**
	 * Set adjust:constant: Constant loss adjustment
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: MAX=100
	 * required if: None
	 */
	SAM_EXPORT void SAM_LinearFresnelDsgIph_Common_adjust:constant_set(SAM_LinearFresnelDsgIph_Common ptr, float number, SAM_error* err);

	/**
	 * Set adjust:hourly: Hourly loss adjustments
	 * type: array
	 * units: %
	 * options: None
	 * constraints: LENGTH=8760
	 * required if: None
	 */
	SAM_EXPORT void SAM_LinearFresnelDsgIph_Common_adjust:hourly_set(SAM_LinearFresnelDsgIph_Common ptr, float* array, int length, SAM_error* err);

	/**
	 * Set adjust:periods: Period-based loss adjustments
	 * type: matrix
	 * units: %
	 * options: n x 3 matrix [ start, end, loss ]
	 * constraints: COLS=3
	 * required if: None
	 */
	SAM_EXPORT void SAM_LinearFresnelDsgIph_Common_adjust:periods_set(SAM_LinearFresnelDsgIph_Common ptr, float* matrix, int nr, int nc, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_LinearFresnelDsgIph_Common_adjust:constant_get(SAM_LinearFresnelDsgIph_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_LinearFresnelDsgIph_Common_adjust:hourly_get(SAM_LinearFresnelDsgIph_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_LinearFresnelDsgIph_Common_adjust:periods_get(SAM_LinearFresnelDsgIph_Common ptr, SAM_error* err);



#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif