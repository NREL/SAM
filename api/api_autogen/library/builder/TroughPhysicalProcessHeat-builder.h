#ifndef SAM_TROUGHPHYSICALPROCESSHEAT_FUNCTIONS_H_
#define SAM_TROUGHPHYSICALPROCESSHEAT_FUNCTIONS_H_

#include "TroughPhysicalProcessHeat-data.h"

#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	/** 
	 * Create a SystemDesign variable table for a PhysicalTroughIPHNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_TroughPhysicalProcessHeat_SystemDesign SAM_TroughPhysicalProcessHeat_SystemDesign_create(const char* def, SAM_error* err);


	/**
	 * Set I_bn_des: Solar irradiation at design
	 * type: numeric
	 * units: C
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SystemDesign_I_bn_des_set(SAM_TroughPhysicalProcessHeat_SystemDesign ptr, float number, SAM_error* err);

	/**
	 * Set T_loop_in_des: Design loop inlet temperature
	 * type: numeric
	 * units: C
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SystemDesign_T_loop_in_des_set(SAM_TroughPhysicalProcessHeat_SystemDesign ptr, float number, SAM_error* err);

	/**
	 * Set T_loop_out: Target loop outlet temperature
	 * type: numeric
	 * units: C
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SystemDesign_T_loop_out_set(SAM_TroughPhysicalProcessHeat_SystemDesign ptr, float number, SAM_error* err);

	/**
	 * Set q_pb_design: Design heat input to power block
	 * type: numeric
	 * units: MWt
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SystemDesign_q_pb_design_set(SAM_TroughPhysicalProcessHeat_SystemDesign ptr, float number, SAM_error* err);

	/**
	 * Set specified_solar_multiple: Local weather file with path
	 * type: string
	 * units: none
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SystemDesign_specified_solar_multiple_set(SAM_TroughPhysicalProcessHeat_SystemDesign ptr, const char* string, SAM_error* err);

	/**
	 * Set tshours: Equivalent full-load thermal storage hours
	 * type: numeric
	 * units: hr
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SystemDesign_tshours_set(SAM_TroughPhysicalProcessHeat_SystemDesign ptr, float number, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_TroughPhysicalProcessHeat_SystemDesign_I_bn_des_get(SAM_TroughPhysicalProcessHeat_SystemDesign ptr, SAM_error* err);

	SAM_EXPORT float SAM_TroughPhysicalProcessHeat_SystemDesign_T_loop_in_des_get(SAM_TroughPhysicalProcessHeat_SystemDesign ptr, SAM_error* err);

	SAM_EXPORT float SAM_TroughPhysicalProcessHeat_SystemDesign_T_loop_out_get(SAM_TroughPhysicalProcessHeat_SystemDesign ptr, SAM_error* err);

	SAM_EXPORT float SAM_TroughPhysicalProcessHeat_SystemDesign_q_pb_design_get(SAM_TroughPhysicalProcessHeat_SystemDesign ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TroughPhysicalProcessHeat_SystemDesign_specified_solar_multiple_get(SAM_TroughPhysicalProcessHeat_SystemDesign ptr, SAM_error* err);

	SAM_EXPORT float SAM_TroughPhysicalProcessHeat_SystemDesign_tshours_get(SAM_TroughPhysicalProcessHeat_SystemDesign ptr, SAM_error* err);



	/** 
	 * Create a SolarField variable table for a PhysicalTroughIPHNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_TroughPhysicalProcessHeat_SolarField SAM_TroughPhysicalProcessHeat_SolarField_create(const char* def, SAM_error* err);


	/**
	 * Set Row_Distance: Spacing between rows (centerline to centerline)
	 * type: numeric
	 * units: m
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_Row_Distance_set(SAM_TroughPhysicalProcessHeat_SolarField ptr, float number, SAM_error* err);

	/**
	 * Set SCA_drives_elec: Tracking power, in Watts per SCA drive
	 * type: numeric
	 * units: W/m2-K
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_SCA_drives_elec_set(SAM_TroughPhysicalProcessHeat_SolarField ptr, float number, SAM_error* err);

	/**
	 * Set azimuth: Azimuth angle of surface/axis
	 * type: numeric
	 * units: none
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_azimuth_set(SAM_TroughPhysicalProcessHeat_SolarField ptr, float number, SAM_error* err);

	/**
	 * Set combo_htf_type: Local weather file with path
	 * type: string
	 * units: none
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_combo_htf_type_set(SAM_TroughPhysicalProcessHeat_SolarField ptr, const char* string, SAM_error* err);

	/**
	 * Set field_fl_props: User defined field fluid property data
	 * type: matrix
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_field_fl_props_set(SAM_TroughPhysicalProcessHeat_SolarField ptr, float* matrix, int nr, int nc, SAM_error* err);

	/**
	 * Set m_dot_htfmax: Maximum loop HTF flow rate
	 * type: numeric
	 * units: kg/s
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_m_dot_htfmax_set(SAM_TroughPhysicalProcessHeat_SolarField ptr, float number, SAM_error* err);

	/**
	 * Set m_dot_htfmin: Minimum loop HTF flow rate
	 * type: numeric
	 * units: kg/s
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_m_dot_htfmin_set(SAM_TroughPhysicalProcessHeat_SolarField ptr, float number, SAM_error* err);

	/**
	 * Set tilt: Tilt angle of surface/axis
	 * type: numeric
	 * units: none
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_tilt_set(SAM_TroughPhysicalProcessHeat_SolarField ptr, float number, SAM_error* err);

	/**
	 * Set trough_loop_control: Local weather file with path
	 * type: string
	 * units: none
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_SolarField_trough_loop_control_set(SAM_TroughPhysicalProcessHeat_SolarField ptr, const char* string, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_TroughPhysicalProcessHeat_SolarField_Row_Distance_get(SAM_TroughPhysicalProcessHeat_SolarField ptr, SAM_error* err);

	SAM_EXPORT float SAM_TroughPhysicalProcessHeat_SolarField_SCA_drives_elec_get(SAM_TroughPhysicalProcessHeat_SolarField ptr, SAM_error* err);

	SAM_EXPORT float SAM_TroughPhysicalProcessHeat_SolarField_azimuth_get(SAM_TroughPhysicalProcessHeat_SolarField ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TroughPhysicalProcessHeat_SolarField_combo_htf_type_get(SAM_TroughPhysicalProcessHeat_SolarField ptr, SAM_error* err);

	SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_SolarField_field_fl_props_get(SAM_TroughPhysicalProcessHeat_SolarField ptr, SAM_error* err);

	SAM_EXPORT float SAM_TroughPhysicalProcessHeat_SolarField_m_dot_htfmax_get(SAM_TroughPhysicalProcessHeat_SolarField ptr, SAM_error* err);

	SAM_EXPORT float SAM_TroughPhysicalProcessHeat_SolarField_m_dot_htfmin_get(SAM_TroughPhysicalProcessHeat_SolarField ptr, SAM_error* err);

	SAM_EXPORT float SAM_TroughPhysicalProcessHeat_SolarField_tilt_get(SAM_TroughPhysicalProcessHeat_SolarField ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TroughPhysicalProcessHeat_SolarField_trough_loop_control_get(SAM_TroughPhysicalProcessHeat_SolarField ptr, SAM_error* err);



	/** 
	 * Create a Collectors(SCAs) variable table for a PhysicalTroughIPHNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_TroughPhysicalProcessHeat_Collectors(SCAs) SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_create(const char* def, SAM_error* err);


	/**
	 * Set csp_dtr_sca_aperture_1: Local weather file with path
	 * type: string
	 * units: none
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_aperture_1_set(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, const char* string, SAM_error* err);

	/**
	 * Set csp_dtr_sca_aperture_2: Local weather file with path
	 * type: string
	 * units: none
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_aperture_2_set(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, const char* string, SAM_error* err);

	/**
	 * Set csp_dtr_sca_aperture_3: Local weather file with path
	 * type: string
	 * units: none
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_aperture_3_set(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, const char* string, SAM_error* err);

	/**
	 * Set csp_dtr_sca_aperture_4: Local weather file with path
	 * type: string
	 * units: none
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_aperture_4_set(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, const char* string, SAM_error* err);

	/**
	 * Set csp_dtr_sca_ave_focal_len_1: Local weather file with path
	 * type: string
	 * units: none
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_ave_focal_len_1_set(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, const char* string, SAM_error* err);

	/**
	 * Set csp_dtr_sca_ave_focal_len_2: Local weather file with path
	 * type: string
	 * units: none
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_ave_focal_len_2_set(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, const char* string, SAM_error* err);

	/**
	 * Set csp_dtr_sca_ave_focal_len_3: Local weather file with path
	 * type: string
	 * units: none
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_ave_focal_len_3_set(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, const char* string, SAM_error* err);

	/**
	 * Set csp_dtr_sca_ave_focal_len_4: Local weather file with path
	 * type: string
	 * units: none
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_ave_focal_len_4_set(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, const char* string, SAM_error* err);

	/**
	 * Set csp_dtr_sca_clean_reflectivity_1: Local weather file with path
	 * type: string
	 * units: none
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_clean_reflectivity_1_set(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, const char* string, SAM_error* err);

	/**
	 * Set csp_dtr_sca_clean_reflectivity_2: Local weather file with path
	 * type: string
	 * units: none
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_clean_reflectivity_2_set(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, const char* string, SAM_error* err);

	/**
	 * Set csp_dtr_sca_clean_reflectivity_3: Local weather file with path
	 * type: string
	 * units: none
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_clean_reflectivity_3_set(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, const char* string, SAM_error* err);

	/**
	 * Set csp_dtr_sca_clean_reflectivity_4: Local weather file with path
	 * type: string
	 * units: none
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_clean_reflectivity_4_set(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, const char* string, SAM_error* err);

	/**
	 * Set csp_dtr_sca_general_error_1: Local weather file with path
	 * type: string
	 * units: none
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_general_error_1_set(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, const char* string, SAM_error* err);

	/**
	 * Set csp_dtr_sca_general_error_2: Local weather file with path
	 * type: string
	 * units: none
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_general_error_2_set(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, const char* string, SAM_error* err);

	/**
	 * Set csp_dtr_sca_general_error_3: Local weather file with path
	 * type: string
	 * units: none
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_general_error_3_set(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, const char* string, SAM_error* err);

	/**
	 * Set csp_dtr_sca_general_error_4: Local weather file with path
	 * type: string
	 * units: none
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_general_error_4_set(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, const char* string, SAM_error* err);

	/**
	 * Set csp_dtr_sca_geometry_effects_1: Local weather file with path
	 * type: string
	 * units: none
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_geometry_effects_1_set(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, const char* string, SAM_error* err);

	/**
	 * Set csp_dtr_sca_geometry_effects_2: Local weather file with path
	 * type: string
	 * units: none
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_geometry_effects_2_set(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, const char* string, SAM_error* err);

	/**
	 * Set csp_dtr_sca_geometry_effects_3: Local weather file with path
	 * type: string
	 * units: none
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_geometry_effects_3_set(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, const char* string, SAM_error* err);

	/**
	 * Set csp_dtr_sca_geometry_effects_4: Local weather file with path
	 * type: string
	 * units: none
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_geometry_effects_4_set(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, const char* string, SAM_error* err);

	/**
	 * Set csp_dtr_sca_length_1: Local weather file with path
	 * type: string
	 * units: none
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_length_1_set(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, const char* string, SAM_error* err);

	/**
	 * Set csp_dtr_sca_length_2: Local weather file with path
	 * type: string
	 * units: none
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_length_2_set(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, const char* string, SAM_error* err);

	/**
	 * Set csp_dtr_sca_length_3: Local weather file with path
	 * type: string
	 * units: none
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_length_3_set(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, const char* string, SAM_error* err);

	/**
	 * Set csp_dtr_sca_length_4: Local weather file with path
	 * type: string
	 * units: none
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_length_4_set(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, const char* string, SAM_error* err);

	/**
	 * Set csp_dtr_sca_mirror_dirt_1: Local weather file with path
	 * type: string
	 * units: none
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_mirror_dirt_1_set(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, const char* string, SAM_error* err);

	/**
	 * Set csp_dtr_sca_mirror_dirt_2: Local weather file with path
	 * type: string
	 * units: none
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_mirror_dirt_2_set(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, const char* string, SAM_error* err);

	/**
	 * Set csp_dtr_sca_mirror_dirt_3: Local weather file with path
	 * type: string
	 * units: none
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_mirror_dirt_3_set(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, const char* string, SAM_error* err);

	/**
	 * Set csp_dtr_sca_mirror_dirt_4: Local weather file with path
	 * type: string
	 * units: none
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_mirror_dirt_4_set(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, const char* string, SAM_error* err);

	/**
	 * Set csp_dtr_sca_ncol_per_sca_1: Local weather file with path
	 * type: string
	 * units: none
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_ncol_per_sca_1_set(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, const char* string, SAM_error* err);

	/**
	 * Set csp_dtr_sca_ncol_per_sca_2: Local weather file with path
	 * type: string
	 * units: none
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_ncol_per_sca_2_set(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, const char* string, SAM_error* err);

	/**
	 * Set csp_dtr_sca_ncol_per_sca_3: Local weather file with path
	 * type: string
	 * units: none
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_ncol_per_sca_3_set(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, const char* string, SAM_error* err);

	/**
	 * Set csp_dtr_sca_ncol_per_sca_4: Local weather file with path
	 * type: string
	 * units: none
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_ncol_per_sca_4_set(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, const char* string, SAM_error* err);

	/**
	 * Set csp_dtr_sca_piping_dist_1: Local weather file with path
	 * type: string
	 * units: none
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_piping_dist_1_set(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, const char* string, SAM_error* err);

	/**
	 * Set csp_dtr_sca_piping_dist_2: Local weather file with path
	 * type: string
	 * units: none
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_piping_dist_2_set(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, const char* string, SAM_error* err);

	/**
	 * Set csp_dtr_sca_piping_dist_3: Local weather file with path
	 * type: string
	 * units: none
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_piping_dist_3_set(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, const char* string, SAM_error* err);

	/**
	 * Set csp_dtr_sca_piping_dist_4: Local weather file with path
	 * type: string
	 * units: none
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_piping_dist_4_set(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, const char* string, SAM_error* err);

	/**
	 * Set csp_dtr_sca_tracking_error_1: Local weather file with path
	 * type: string
	 * units: none
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_tracking_error_1_set(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, const char* string, SAM_error* err);

	/**
	 * Set csp_dtr_sca_tracking_error_2: Local weather file with path
	 * type: string
	 * units: none
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_tracking_error_2_set(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, const char* string, SAM_error* err);

	/**
	 * Set csp_dtr_sca_tracking_error_3: Local weather file with path
	 * type: string
	 * units: none
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_tracking_error_3_set(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, const char* string, SAM_error* err);

	/**
	 * Set csp_dtr_sca_tracking_error_4: Local weather file with path
	 * type: string
	 * units: none
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_tracking_error_4_set(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, const char* string, SAM_error* err);

	/**
	 * Set csp_dtr_sca_w_profile_1: Local weather file with path
	 * type: string
	 * units: none
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_w_profile_1_set(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, const char* string, SAM_error* err);

	/**
	 * Set csp_dtr_sca_w_profile_2: Local weather file with path
	 * type: string
	 * units: none
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_w_profile_2_set(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, const char* string, SAM_error* err);

	/**
	 * Set csp_dtr_sca_w_profile_3: Local weather file with path
	 * type: string
	 * units: none
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_w_profile_3_set(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, const char* string, SAM_error* err);

	/**
	 * Set csp_dtr_sca_w_profile_4: Local weather file with path
	 * type: string
	 * units: none
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_w_profile_4_set(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, const char* string, SAM_error* err);

	/**
	 * Set nColt: Number of collector types
	 * type: numeric
	 * units: none
	 * options: constant=4
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_nColt_set(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, float number, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT const char* SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_aperture_1_get(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_aperture_2_get(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_aperture_3_get(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_aperture_4_get(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_ave_focal_len_1_get(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_ave_focal_len_2_get(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_ave_focal_len_3_get(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_ave_focal_len_4_get(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_clean_reflectivity_1_get(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_clean_reflectivity_2_get(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_clean_reflectivity_3_get(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_clean_reflectivity_4_get(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_general_error_1_get(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_general_error_2_get(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_general_error_3_get(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_general_error_4_get(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_geometry_effects_1_get(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_geometry_effects_2_get(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_geometry_effects_3_get(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_geometry_effects_4_get(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_length_1_get(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_length_2_get(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_length_3_get(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_length_4_get(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_mirror_dirt_1_get(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_mirror_dirt_2_get(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_mirror_dirt_3_get(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_mirror_dirt_4_get(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_ncol_per_sca_1_get(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_ncol_per_sca_2_get(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_ncol_per_sca_3_get(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_ncol_per_sca_4_get(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_piping_dist_1_get(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_piping_dist_2_get(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_piping_dist_3_get(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_piping_dist_4_get(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_tracking_error_1_get(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_tracking_error_2_get(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_tracking_error_3_get(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_tracking_error_4_get(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_w_profile_1_get(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_w_profile_2_get(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_w_profile_3_get(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_csp_dtr_sca_w_profile_4_get(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, SAM_error* err);

	SAM_EXPORT float SAM_TroughPhysicalProcessHeat_Collectors(SCAs)_nColt_get(SAM_TroughPhysicalProcessHeat_Collectors(SCAs) ptr, SAM_error* err);



	/** 
	 * Create a ThermalStorage variable table for a PhysicalTroughIPHNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_TroughPhysicalProcessHeat_ThermalStorage SAM_TroughPhysicalProcessHeat_ThermalStorage_create(const char* def, SAM_error* err);


	/**
	 * Set h_tank: Total height of tank (height of HTF when tank is full
	 * type: numeric
	 * units: m
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_ThermalStorage_h_tank_set(SAM_TroughPhysicalProcessHeat_ThermalStorage ptr, float number, SAM_error* err);

	/**
	 * Set h_tank_min: Minimum allowable HTF height in storage tank
	 * type: numeric
	 * units: m
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_ThermalStorage_h_tank_min_set(SAM_TroughPhysicalProcessHeat_ThermalStorage ptr, float number, SAM_error* err);

	/**
	 * Set tank_pairs: Number of equivalent tank pairs
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: INTEGER
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_ThermalStorage_tank_pairs_set(SAM_TroughPhysicalProcessHeat_ThermalStorage ptr, float number, SAM_error* err);

	/**
	 * Set u_tank: Loss coefficient from the tank
	 * type: numeric
	 * units: W/m2-K
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_ThermalStorage_u_tank_set(SAM_TroughPhysicalProcessHeat_ThermalStorage ptr, float number, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_TroughPhysicalProcessHeat_ThermalStorage_h_tank_get(SAM_TroughPhysicalProcessHeat_ThermalStorage ptr, SAM_error* err);

	SAM_EXPORT float SAM_TroughPhysicalProcessHeat_ThermalStorage_h_tank_min_get(SAM_TroughPhysicalProcessHeat_ThermalStorage ptr, SAM_error* err);

	SAM_EXPORT float SAM_TroughPhysicalProcessHeat_ThermalStorage_tank_pairs_get(SAM_TroughPhysicalProcessHeat_ThermalStorage ptr, SAM_error* err);

	SAM_EXPORT float SAM_TroughPhysicalProcessHeat_ThermalStorage_u_tank_get(SAM_TroughPhysicalProcessHeat_ThermalStorage ptr, SAM_error* err);



	/** 
	 * Create a Common variable table for a PhysicalTroughIPHNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_TroughPhysicalProcessHeat_Common SAM_TroughPhysicalProcessHeat_Common_create(const char* def, SAM_error* err);


	/**
	 * Set adjust:constant: Constant loss adjustment
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: MAX=100
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_Common_adjust:constant_set(SAM_TroughPhysicalProcessHeat_Common ptr, float number, SAM_error* err);

	/**
	 * Set adjust:hourly: Hourly loss adjustments
	 * type: array
	 * units: %
	 * options: None
	 * constraints: LENGTH=8760
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_Common_adjust:hourly_set(SAM_TroughPhysicalProcessHeat_Common ptr, float* array, int length, SAM_error* err);

	/**
	 * Set adjust:periods: Period-based loss adjustments
	 * type: matrix
	 * units: %
	 * options: n x 3 matrix [ start, end, loss ]
	 * constraints: COLS=3
	 * required if: None
	 */
	SAM_EXPORT void SAM_TroughPhysicalProcessHeat_Common_adjust:periods_set(SAM_TroughPhysicalProcessHeat_Common ptr, float* matrix, int nr, int nc, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_TroughPhysicalProcessHeat_Common_adjust:constant_get(SAM_TroughPhysicalProcessHeat_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_Common_adjust:hourly_get(SAM_TroughPhysicalProcessHeat_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_TroughPhysicalProcessHeat_Common_adjust:periods_get(SAM_TroughPhysicalProcessHeat_Common ptr, SAM_error* err);



#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif