#ifndef SAM_PVSAMV1_FUNCTIONS_H_
#define SAM_PVSAMV1_FUNCTIONS_H_

#include "Pvsamv1-data.h"

#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	/** 
	 * Create a LocationAndResource variable table for a FlatPlatePVNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_Pvsamv1_LocationAndResource SAM_Pvsamv1_LocationAndResource_create(const char* def, SAM_error* err);


	/**
	 * Set solar_data_file_name: Weather file in TMY2, TMY3, EPW, or SAM CSV.
	 * type: string
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_LocationAndResource_solar_data_file_name_set(SAM_Pvsamv1_LocationAndResource ptr, const char* string, SAM_error* err);

	/**
	 * Set use_specific_weather_file: Weather file in TMY2, TMY3, EPW, or SAM CSV.
	 * type: string
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_LocationAndResource_use_specific_weather_file_set(SAM_Pvsamv1_LocationAndResource ptr, const char* string, SAM_error* err);

	/**
	 * Set user_specified_weather_file: Weather file in TMY2, TMY3, EPW, or SAM CSV.
	 * type: string
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_LocationAndResource_user_specified_weather_file_set(SAM_Pvsamv1_LocationAndResource ptr, const char* string, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT const char* SAM_Pvsamv1_LocationAndResource_solar_data_file_name_get(SAM_Pvsamv1_LocationAndResource ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Pvsamv1_LocationAndResource_use_specific_weather_file_get(SAM_Pvsamv1_LocationAndResource ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Pvsamv1_LocationAndResource_user_specified_weather_file_get(SAM_Pvsamv1_LocationAndResource ptr, SAM_error* err);



	/** 
	 * Create a SimpleEfficiencyModuleModel variable table for a FlatPlatePVNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_Pvsamv1_SimpleEfficiencyModuleModel SAM_Pvsamv1_SimpleEfficiencyModuleModel_create(const char* def, SAM_error* err);


	/**
	 * Set spe_area: Module area
	 * type: numeric
	 * units: m2
	 * options: None
	 * constraints: None
	 * required if: module_model=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_area_set(SAM_Pvsamv1_SimpleEfficiencyModuleModel ptr, float number, SAM_error* err);

	/**
	 * Set spe_eff0: Efficiency at irradiance level 0
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: module_model=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_eff0_set(SAM_Pvsamv1_SimpleEfficiencyModuleModel ptr, float number, SAM_error* err);

	/**
	 * Set spe_eff1: Efficiency at irradiance level 1
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: module_model=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_eff1_set(SAM_Pvsamv1_SimpleEfficiencyModuleModel ptr, float number, SAM_error* err);

	/**
	 * Set spe_eff2: Efficiency at irradiance level 2
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: module_model=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_eff2_set(SAM_Pvsamv1_SimpleEfficiencyModuleModel ptr, float number, SAM_error* err);

	/**
	 * Set spe_eff3: Efficiency at irradiance level 3
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: module_model=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_eff3_set(SAM_Pvsamv1_SimpleEfficiencyModuleModel ptr, float number, SAM_error* err);

	/**
	 * Set spe_eff4: Efficiency at irradiance level 4
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: module_model=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_eff4_set(SAM_Pvsamv1_SimpleEfficiencyModuleModel ptr, float number, SAM_error* err);

	/**
	 * Set spe_module_structure: Mounting and module structure
	 * type: numeric
	 * units: None
	 * options: 0=glass/cell/polymer sheet - open rack,1=glass/cell/glass - open rack,2=polymer/thin film/steel - open rack,3=Insulated back, building-integrated PV,4=close roof mount,5=user-defined
	 * constraints: INTEGER,MIN=0,MAX=5
	 * required if: module_model=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_module_structure_set(SAM_Pvsamv1_SimpleEfficiencyModuleModel ptr, float number, SAM_error* err);

	/**
	 * Set spe_rad0: Irradiance level 0
	 * type: numeric
	 * units: W/m2
	 * options: None
	 * constraints: None
	 * required if: module_model=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_rad0_set(SAM_Pvsamv1_SimpleEfficiencyModuleModel ptr, float number, SAM_error* err);

	/**
	 * Set spe_rad1: Irradiance level 1
	 * type: numeric
	 * units: W/m2
	 * options: None
	 * constraints: None
	 * required if: module_model=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_rad1_set(SAM_Pvsamv1_SimpleEfficiencyModuleModel ptr, float number, SAM_error* err);

	/**
	 * Set spe_rad2: Irradiance level 2
	 * type: numeric
	 * units: W/m2
	 * options: None
	 * constraints: None
	 * required if: module_model=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_rad2_set(SAM_Pvsamv1_SimpleEfficiencyModuleModel ptr, float number, SAM_error* err);

	/**
	 * Set spe_rad3: Irradiance level 3
	 * type: numeric
	 * units: W/m2
	 * options: None
	 * constraints: None
	 * required if: module_model=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_rad3_set(SAM_Pvsamv1_SimpleEfficiencyModuleModel ptr, float number, SAM_error* err);

	/**
	 * Set spe_rad4: Irradiance level 4
	 * type: numeric
	 * units: W/m2
	 * options: None
	 * constraints: None
	 * required if: module_model=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_rad4_set(SAM_Pvsamv1_SimpleEfficiencyModuleModel ptr, float number, SAM_error* err);

	/**
	 * Set spe_reference: Reference irradiance level
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: INTEGER,MIN=0,MAX=4
	 * required if: module_model=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_reference_set(SAM_Pvsamv1_SimpleEfficiencyModuleModel ptr, float number, SAM_error* err);

	/**
	 * Set spe_vmp: Nominal max power voltage
	 * type: numeric
	 * units: V
	 * options: None
	 * constraints: POSITIVE
	 * required if: module_model=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_vmp_set(SAM_Pvsamv1_SimpleEfficiencyModuleModel ptr, float number, SAM_error* err);

	/**
	 * Set spe_voc: Nominal open circuit voltage
	 * type: numeric
	 * units: V
	 * options: None
	 * constraints: POSITIVE
	 * required if: module_model=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_voc_set(SAM_Pvsamv1_SimpleEfficiencyModuleModel ptr, float number, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_area_get(SAM_Pvsamv1_SimpleEfficiencyModuleModel ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_eff0_get(SAM_Pvsamv1_SimpleEfficiencyModuleModel ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_eff1_get(SAM_Pvsamv1_SimpleEfficiencyModuleModel ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_eff2_get(SAM_Pvsamv1_SimpleEfficiencyModuleModel ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_eff3_get(SAM_Pvsamv1_SimpleEfficiencyModuleModel ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_eff4_get(SAM_Pvsamv1_SimpleEfficiencyModuleModel ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_module_structure_get(SAM_Pvsamv1_SimpleEfficiencyModuleModel ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_rad0_get(SAM_Pvsamv1_SimpleEfficiencyModuleModel ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_rad1_get(SAM_Pvsamv1_SimpleEfficiencyModuleModel ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_rad2_get(SAM_Pvsamv1_SimpleEfficiencyModuleModel ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_rad3_get(SAM_Pvsamv1_SimpleEfficiencyModuleModel ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_rad4_get(SAM_Pvsamv1_SimpleEfficiencyModuleModel ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_reference_get(SAM_Pvsamv1_SimpleEfficiencyModuleModel ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_vmp_get(SAM_Pvsamv1_SimpleEfficiencyModuleModel ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_SimpleEfficiencyModuleModel_spe_voc_get(SAM_Pvsamv1_SimpleEfficiencyModuleModel ptr, SAM_error* err);



	/** 
	 * Create a CECPerformanceModelWithModuleDatabase variable table for a FlatPlatePVNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_create(const char* def, SAM_error* err);


	/**
	 * Set cec_a_ref: Nonideality factor a
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: module_model=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_a_ref_set(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase ptr, float number, SAM_error* err);

	/**
	 * Set cec_alpha_sc: Short circuit current temperature coefficient
	 * type: numeric
	 * units: A/C
	 * options: None
	 * constraints: None
	 * required if: module_model=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_alpha_sc_set(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase ptr, float number, SAM_error* err);

	/**
	 * Set cec_area: Module area
	 * type: numeric
	 * units: m2
	 * options: None
	 * constraints: None
	 * required if: module_model=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_area_set(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase ptr, float number, SAM_error* err);

	/**
	 * Set cec_beta_oc: Open circuit voltage temperature coefficient
	 * type: numeric
	 * units: V/C
	 * options: None
	 * constraints: None
	 * required if: module_model=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_beta_oc_set(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase ptr, float number, SAM_error* err);

	/**
	 * Set cec_gamma_r: Maximum power point temperature coefficient
	 * type: numeric
	 * units: %/C
	 * options: None
	 * constraints: None
	 * required if: module_model=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_gamma_r_set(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase ptr, float number, SAM_error* err);

	/**
	 * Set cec_heat_transfer: Heat transfer dimensions
	 * type: numeric
	 * units: None
	 * options: 0=module,1=array
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: module_model=1&cec_temp_corr_mode=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_heat_transfer_set(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase ptr, float number, SAM_error* err);

	/**
	 * Set cec_i_l_ref: Light current
	 * type: numeric
	 * units: A
	 * options: None
	 * constraints: None
	 * required if: module_model=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_i_l_ref_set(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase ptr, float number, SAM_error* err);

	/**
	 * Set cec_i_mp_ref: Maximum power point current
	 * type: numeric
	 * units: A
	 * options: None
	 * constraints: None
	 * required if: module_model=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_i_mp_ref_set(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase ptr, float number, SAM_error* err);

	/**
	 * Set cec_i_o_ref: Saturation current
	 * type: numeric
	 * units: A
	 * options: None
	 * constraints: None
	 * required if: module_model=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_i_o_ref_set(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase ptr, float number, SAM_error* err);

	/**
	 * Set cec_i_sc_ref: Short circuit current
	 * type: numeric
	 * units: A
	 * options: None
	 * constraints: None
	 * required if: module_model=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_i_sc_ref_set(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase ptr, float number, SAM_error* err);

	/**
	 * Set cec_is_bifacial: Modules are bifacial
	 * type: numeric
	 * units: 0/1
	 * options: None
	 * constraints: None
	 * required if: module_model=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_is_bifacial_set(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase ptr, float number, SAM_error* err);

	/**
	 * Set cec_module_width: Module width
	 * type: numeric
	 * units: m
	 * options: None
	 * constraints: None
	 * required if: module_model=1&cec_temp_corr_mode=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_module_width_set(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase ptr, float number, SAM_error* err);

	/**
	 * Set cec_mounting_config: Mounting configuration
	 * type: numeric
	 * units: None
	 * options: 0=rack,1=flush,2=integrated,3=gap
	 * constraints: INTEGER,MIN=0,MAX=3
	 * required if: module_model=1&cec_temp_corr_mode=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_mounting_config_set(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase ptr, float number, SAM_error* err);

	/**
	 * Set cec_r_s: Series resistance
	 * type: numeric
	 * units: ohm
	 * options: None
	 * constraints: None
	 * required if: module_model=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_r_s_set(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase ptr, float number, SAM_error* err);

	/**
	 * Set cec_r_sh_ref: Shunt resistance
	 * type: numeric
	 * units: ohm
	 * options: None
	 * constraints: None
	 * required if: module_model=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_r_sh_ref_set(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase ptr, float number, SAM_error* err);

	/**
	 * Set cec_temp_corr_mode: Cell temperature model selection
	 * type: numeric
	 * units: None
	 * options: 0=noct,1=mc
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: module_model=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_temp_corr_mode_set(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase ptr, float number, SAM_error* err);

	/**
	 * Set cec_v_mp_ref: Maximum power point voltage
	 * type: numeric
	 * units: V
	 * options: None
	 * constraints: None
	 * required if: module_model=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_v_mp_ref_set(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase ptr, float number, SAM_error* err);

	/**
	 * Set cec_v_oc_ref: Open circuit voltage
	 * type: numeric
	 * units: V
	 * options: None
	 * constraints: None
	 * required if: module_model=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_v_oc_ref_set(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase ptr, float number, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_a_ref_get(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_alpha_sc_get(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_area_get(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_beta_oc_get(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_gamma_r_get(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_heat_transfer_get(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_i_l_ref_get(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_i_mp_ref_get(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_i_o_ref_get(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_i_sc_ref_get(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_is_bifacial_get(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_module_width_get(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_mounting_config_get(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_r_s_get(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_r_sh_ref_get(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_temp_corr_mode_get(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_v_mp_ref_get(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase_cec_v_oc_ref_get(SAM_Pvsamv1_CECPerformanceModelWithModuleDatabase ptr, SAM_error* err);



	/** 
	 * Create a CECPerformanceModelWithUserEnteredSpecifications variable table for a FlatPlatePVNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_create(const char* def, SAM_error* err);


	/**
	 * Set 6par_aisc_display: Weather file in TMY2, TMY3, EPW, or SAM CSV.
	 * type: string
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_6par_aisc_display_set(SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications ptr, const char* string, SAM_error* err);

	/**
	 * Set 6par_aisc_units: Weather file in TMY2, TMY3, EPW, or SAM CSV.
	 * type: string
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_6par_aisc_units_set(SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications ptr, const char* string, SAM_error* err);

	/**
	 * Set 6par_area: Module area
	 * type: numeric
	 * units: m2
	 * options: None
	 * constraints: None
	 * required if: module_model=2
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_6par_area_set(SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications ptr, float number, SAM_error* err);

	/**
	 * Set 6par_bvoc_display: Weather file in TMY2, TMY3, EPW, or SAM CSV.
	 * type: string
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_6par_bvoc_display_set(SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications ptr, const char* string, SAM_error* err);

	/**
	 * Set 6par_bvoc_units: Weather file in TMY2, TMY3, EPW, or SAM CSV.
	 * type: string
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_6par_bvoc_units_set(SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications ptr, const char* string, SAM_error* err);

	/**
	 * Set 6par_imp: Imp
	 * type: numeric
	 * units: A
	 * options: None
	 * constraints: None
	 * required if: module_model=2
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_6par_imp_set(SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications ptr, float number, SAM_error* err);

	/**
	 * Set 6par_isc: Isc
	 * type: numeric
	 * units: A
	 * options: None
	 * constraints: None
	 * required if: module_model=2
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_6par_isc_set(SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications ptr, float number, SAM_error* err);

	/**
	 * Set 6par_vmp: Maximum power point voltage
	 * type: numeric
	 * units: V
	 * options: None
	 * constraints: None
	 * required if: module_model=2
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_6par_vmp_set(SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications ptr, float number, SAM_error* err);

	/**
	 * Set 6par_voc: Voc
	 * type: numeric
	 * units: V
	 * options: None
	 * constraints: None
	 * required if: module_model=2
	 */
	SAM_EXPORT void SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_6par_voc_set(SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications ptr, float number, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT const char* SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_6par_aisc_display_get(SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_6par_aisc_units_get(SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_6par_area_get(SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_6par_bvoc_display_get(SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_6par_bvoc_units_get(SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_6par_imp_get(SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_6par_isc_get(SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_6par_vmp_get(SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications_6par_voc_get(SAM_Pvsamv1_CECPerformanceModelWithUserEnteredSpecifications ptr, SAM_error* err);



	/** 
	 * Create a SandiaPVArrayPerformanceModelWithModuleDatabase variable table for a FlatPlatePVNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_create(const char* def, SAM_error* err);


	/**
	 * Set snl_a: Temperature coefficient a
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_a_set(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, float number, SAM_error* err);

	/**
	 * Set snl_a0: Air mass polynomial coeff 0
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_a0_set(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, float number, SAM_error* err);

	/**
	 * Set snl_a1: Air mass polynomial coeff 1
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_a1_set(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, float number, SAM_error* err);

	/**
	 * Set snl_a2: Air mass polynomial coeff 2
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_a2_set(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, float number, SAM_error* err);

	/**
	 * Set snl_a3: Air mass polynomial coeff 3
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_a3_set(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, float number, SAM_error* err);

	/**
	 * Set snl_a4: Air mass polynomial coeff 4
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_a4_set(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, float number, SAM_error* err);

	/**
	 * Set snl_aimp: Max power point current temperature coefficient
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_aimp_set(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, float number, SAM_error* err);

	/**
	 * Set snl_aisc: Short circuit current temperature coefficient
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_aisc_set(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, float number, SAM_error* err);

	/**
	 * Set snl_area: Module area
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_area_set(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, float number, SAM_error* err);

	/**
	 * Set snl_b: Temperature coefficient b
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_b_set(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, float number, SAM_error* err);

	/**
	 * Set snl_b0: Incidence angle modifier polynomial coeff 0
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_b0_set(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, float number, SAM_error* err);

	/**
	 * Set snl_b1: Incidence angle modifier polynomial coeff 1
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_b1_set(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, float number, SAM_error* err);

	/**
	 * Set snl_b2: Incidence angle modifier polynomial coeff 2
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_b2_set(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, float number, SAM_error* err);

	/**
	 * Set snl_b3: Incidence angle modifier polynomial coeff 3
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_b3_set(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, float number, SAM_error* err);

	/**
	 * Set snl_b4: Incidence angle modifier polynomial coeff 4
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_b4_set(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, float number, SAM_error* err);

	/**
	 * Set snl_b5: Incidence angle modifier polynomial coeff 5
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_b5_set(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, float number, SAM_error* err);

	/**
	 * Set snl_bvmpo: Max power point voltage temperature coefficient
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_bvmpo_set(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, float number, SAM_error* err);

	/**
	 * Set snl_bvoco: Open circuit voltage temperature coefficient
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_bvoco_set(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, float number, SAM_error* err);

	/**
	 * Set snl_c0: C0
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_c0_set(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, float number, SAM_error* err);

	/**
	 * Set snl_c1: C1
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_c1_set(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, float number, SAM_error* err);

	/**
	 * Set snl_c2: C2
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_c2_set(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, float number, SAM_error* err);

	/**
	 * Set snl_c3: C3
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_c3_set(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, float number, SAM_error* err);

	/**
	 * Set snl_dtc: Temperature coefficient dT
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_dtc_set(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, float number, SAM_error* err);

	/**
	 * Set snl_fd: Diffuse fraction
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_fd_set(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, float number, SAM_error* err);

	/**
	 * Set snl_impo: Max power point current
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_impo_set(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, float number, SAM_error* err);

	/**
	 * Set snl_isco: Short circuit current
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_isco_set(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, float number, SAM_error* err);

	/**
	 * Set snl_mbvmp: Irradiance dependence of Vmp temperature coefficient
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_mbvmp_set(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, float number, SAM_error* err);

	/**
	 * Set snl_mbvoc: Irradiance dependence of Voc temperature coefficient
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_mbvoc_set(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, float number, SAM_error* err);

	/**
	 * Set snl_module_structure: Module and mounting structure configuration
	 * type: numeric
	 * units: None
	 * options: 0=Use Database Values,1=glass/cell/polymer sheet - open rack,2=glass/cell/glass - open rack,3=polymer/thin film/steel - open rack,4=Insulated back building-integrated PV,5=close roof mount,6=user-defined
	 * constraints: INTEGER,MIN=0,MAX=6
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_module_structure_set(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, float number, SAM_error* err);

	/**
	 * Set snl_n: Diode factor
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_n_set(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, float number, SAM_error* err);

	/**
	 * Set snl_series_cells: Number of cells in series
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: INTEGER
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_series_cells_set(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, float number, SAM_error* err);

	/**
	 * Set snl_specified_a: Weather file in TMY2, TMY3, EPW, or SAM CSV.
	 * type: string
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_specified_a_set(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, const char* string, SAM_error* err);

	/**
	 * Set snl_specified_b: Weather file in TMY2, TMY3, EPW, or SAM CSV.
	 * type: string
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_specified_b_set(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, const char* string, SAM_error* err);

	/**
	 * Set snl_specified_dT: Weather file in TMY2, TMY3, EPW, or SAM CSV.
	 * type: string
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_specified_dT_set(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, const char* string, SAM_error* err);

	/**
	 * Set snl_vmpo: Max power point voltage
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_vmpo_set(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, float number, SAM_error* err);

	/**
	 * Set snl_voco: Open circuit voltage
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: module_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_voco_set(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, float number, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_a_get(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_a0_get(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_a1_get(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_a2_get(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_a3_get(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_a4_get(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_aimp_get(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_aisc_get(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_area_get(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_b_get(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_b0_get(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_b1_get(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_b2_get(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_b3_get(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_b4_get(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_b5_get(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_bvmpo_get(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_bvoco_get(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_c0_get(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_c1_get(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_c2_get(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_c3_get(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_dtc_get(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_fd_get(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_impo_get(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_isco_get(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_mbvmp_get(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_mbvoc_get(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_module_structure_get(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_n_get(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_series_cells_get(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_specified_a_get(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_specified_b_get(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_specified_dT_get(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_vmpo_get(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase_snl_voco_get(SAM_Pvsamv1_SandiaPVArrayPerformanceModelWithModuleDatabase ptr, SAM_error* err);



	/** 
	 * Create a IEC61853SingleDiodeModel variable table for a FlatPlatePVNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_Pvsamv1_IEC61853SingleDiodeModel SAM_Pvsamv1_IEC61853SingleDiodeModel_create(const char* def, SAM_error* err);


	/**
	 * Set iec61853_test_data: Weather file in TMY2, TMY3, EPW, or SAM CSV.
	 * type: string
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_IEC61853SingleDiodeModel_iec61853_test_data_set(SAM_Pvsamv1_IEC61853SingleDiodeModel ptr, const char* string, SAM_error* err);

	/**
	 * Set sd11par_Pmp0: Weather file in TMY2, TMY3, EPW, or SAM CSV.
	 * type: string
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_Pmp0_set(SAM_Pvsamv1_IEC61853SingleDiodeModel ptr, const char* string, SAM_error* err);

	/**
	 * Set sd11par_area: Module area
	 * type: numeric
	 * units: m2
	 * options: None
	 * constraints: None
	 * required if: module_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_area_set(SAM_Pvsamv1_IEC61853SingleDiodeModel ptr, float number, SAM_error* err);

	/**
	 * Set sd11par_nser: Nseries
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: INTEGER,POSITIVE
	 * required if: module_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_nser_set(SAM_Pvsamv1_IEC61853SingleDiodeModel ptr, float number, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT const char* SAM_Pvsamv1_IEC61853SingleDiodeModel_iec61853_test_data_get(SAM_Pvsamv1_IEC61853SingleDiodeModel ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_Pmp0_get(SAM_Pvsamv1_IEC61853SingleDiodeModel ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_area_get(SAM_Pvsamv1_IEC61853SingleDiodeModel ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_IEC61853SingleDiodeModel_sd11par_nser_get(SAM_Pvsamv1_IEC61853SingleDiodeModel ptr, SAM_error* err);



	/** 
	 * Create a InverterCECDatabase variable table for a FlatPlatePVNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_Pvsamv1_InverterCECDatabase SAM_Pvsamv1_InverterCECDatabase_create(const char* def, SAM_error* err);


	/**
	 * Set inv_snl_c0: Curvature between AC power and DC power at ref
	 * type: numeric
	 * units: 1/W
	 * options: None
	 * constraints: None
	 * required if: inverter_model=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterCECDatabase_inv_snl_c0_set(SAM_Pvsamv1_InverterCECDatabase ptr, float number, SAM_error* err);

	/**
	 * Set inv_snl_c1: Coefficient of Pdco variation with DC input voltage
	 * type: numeric
	 * units: 1/V
	 * options: None
	 * constraints: None
	 * required if: inverter_model=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterCECDatabase_inv_snl_c1_set(SAM_Pvsamv1_InverterCECDatabase ptr, float number, SAM_error* err);

	/**
	 * Set inv_snl_c2: Coefficient of Pso variation with DC input voltage
	 * type: numeric
	 * units: 1/V
	 * options: None
	 * constraints: None
	 * required if: inverter_model=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterCECDatabase_inv_snl_c2_set(SAM_Pvsamv1_InverterCECDatabase ptr, float number, SAM_error* err);

	/**
	 * Set inv_snl_c3: Coefficient of Co variation with DC input voltage
	 * type: numeric
	 * units: 1/V
	 * options: None
	 * constraints: None
	 * required if: inverter_model=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterCECDatabase_inv_snl_c3_set(SAM_Pvsamv1_InverterCECDatabase ptr, float number, SAM_error* err);

	/**
	 * Set inv_snl_mppt_hi: Weather file in TMY2, TMY3, EPW, or SAM CSV.
	 * type: string
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterCECDatabase_inv_snl_mppt_hi_set(SAM_Pvsamv1_InverterCECDatabase ptr, const char* string, SAM_error* err);

	/**
	 * Set inv_snl_mppt_low: Weather file in TMY2, TMY3, EPW, or SAM CSV.
	 * type: string
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterCECDatabase_inv_snl_mppt_low_set(SAM_Pvsamv1_InverterCECDatabase ptr, const char* string, SAM_error* err);

	/**
	 * Set inv_snl_num_mppt: Weather file in TMY2, TMY3, EPW, or SAM CSV.
	 * type: string
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterCECDatabase_inv_snl_num_mppt_set(SAM_Pvsamv1_InverterCECDatabase ptr, const char* string, SAM_error* err);

	/**
	 * Set inv_snl_paco: AC maximum power rating
	 * type: numeric
	 * units: Wac
	 * options: None
	 * constraints: None
	 * required if: inverter_model=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterCECDatabase_inv_snl_paco_set(SAM_Pvsamv1_InverterCECDatabase ptr, float number, SAM_error* err);

	/**
	 * Set inv_snl_pdco: DC input power at which AC power rating is achieved
	 * type: numeric
	 * units: Wdc
	 * options: None
	 * constraints: None
	 * required if: inverter_model=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterCECDatabase_inv_snl_pdco_set(SAM_Pvsamv1_InverterCECDatabase ptr, float number, SAM_error* err);

	/**
	 * Set inv_snl_pso: DC power required to enable the inversion process
	 * type: numeric
	 * units: Wdc
	 * options: None
	 * constraints: None
	 * required if: inverter_model=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterCECDatabase_inv_snl_pso_set(SAM_Pvsamv1_InverterCECDatabase ptr, float number, SAM_error* err);

	/**
	 * Set inv_snl_vdcmax: Maximum DC input operating voltage
	 * type: numeric
	 * units: Vdc
	 * options: None
	 * constraints: None
	 * required if: inverter_model=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterCECDatabase_inv_snl_vdcmax_set(SAM_Pvsamv1_InverterCECDatabase ptr, float number, SAM_error* err);

	/**
	 * Set inv_snl_vdco: DC input voltage for the rated AC power rating
	 * type: numeric
	 * units: Vdc
	 * options: None
	 * constraints: None
	 * required if: inverter_model=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterCECDatabase_inv_snl_vdco_set(SAM_Pvsamv1_InverterCECDatabase ptr, float number, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_Pvsamv1_InverterCECDatabase_inv_snl_c0_get(SAM_Pvsamv1_InverterCECDatabase ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_InverterCECDatabase_inv_snl_c1_get(SAM_Pvsamv1_InverterCECDatabase ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_InverterCECDatabase_inv_snl_c2_get(SAM_Pvsamv1_InverterCECDatabase ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_InverterCECDatabase_inv_snl_c3_get(SAM_Pvsamv1_InverterCECDatabase ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Pvsamv1_InverterCECDatabase_inv_snl_mppt_hi_get(SAM_Pvsamv1_InverterCECDatabase ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Pvsamv1_InverterCECDatabase_inv_snl_mppt_low_get(SAM_Pvsamv1_InverterCECDatabase ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Pvsamv1_InverterCECDatabase_inv_snl_num_mppt_get(SAM_Pvsamv1_InverterCECDatabase ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_InverterCECDatabase_inv_snl_paco_get(SAM_Pvsamv1_InverterCECDatabase ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_InverterCECDatabase_inv_snl_pdco_get(SAM_Pvsamv1_InverterCECDatabase ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_InverterCECDatabase_inv_snl_pso_get(SAM_Pvsamv1_InverterCECDatabase ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_InverterCECDatabase_inv_snl_vdcmax_get(SAM_Pvsamv1_InverterCECDatabase ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_InverterCECDatabase_inv_snl_vdco_get(SAM_Pvsamv1_InverterCECDatabase ptr, SAM_error* err);



	/** 
	 * Create a InverterTempDerateCECDB variable table for a FlatPlatePVNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_Pvsamv1_InverterTempDerateCECDB SAM_Pvsamv1_InverterTempDerateCECDB_create(const char* def, SAM_error* err);


	/**
	 * Set inv_tdc_cec_db: Temperature derate curves for CEC Database
	 * type: matrix
	 * units: Vdc
	 * options: None
	 * constraints: None
	 * required if: inverter_model=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterTempDerateCECDB_inv_tdc_cec_db_set(SAM_Pvsamv1_InverterTempDerateCECDB ptr, float* matrix, int nr, int nc, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float* SAM_Pvsamv1_InverterTempDerateCECDB_inv_tdc_cec_db_get(SAM_Pvsamv1_InverterTempDerateCECDB ptr, SAM_error* err);



	/** 
	 * Create a InverterDatasheet variable table for a FlatPlatePVNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_Pvsamv1_InverterDatasheet SAM_Pvsamv1_InverterDatasheet_create(const char* def, SAM_error* err);


	/**
	 * Set inv_ds_eff_peak_or_nom: Weather file in TMY2, TMY3, EPW, or SAM CSV.
	 * type: string
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterDatasheet_inv_ds_eff_peak_or_nom_set(SAM_Pvsamv1_InverterDatasheet ptr, const char* string, SAM_error* err);

	/**
	 * Set inv_ds_eff_type: Weather file in TMY2, TMY3, EPW, or SAM CSV.
	 * type: string
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterDatasheet_inv_ds_eff_type_set(SAM_Pvsamv1_InverterDatasheet ptr, const char* string, SAM_error* err);

	/**
	 * Set inv_ds_eff_weighted: Weather file in TMY2, TMY3, EPW, or SAM CSV.
	 * type: string
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterDatasheet_inv_ds_eff_weighted_set(SAM_Pvsamv1_InverterDatasheet ptr, const char* string, SAM_error* err);

	/**
	 * Set inv_ds_mppt_hi: Weather file in TMY2, TMY3, EPW, or SAM CSV.
	 * type: string
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterDatasheet_inv_ds_mppt_hi_set(SAM_Pvsamv1_InverterDatasheet ptr, const char* string, SAM_error* err);

	/**
	 * Set inv_ds_mppt_low: Weather file in TMY2, TMY3, EPW, or SAM CSV.
	 * type: string
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterDatasheet_inv_ds_mppt_low_set(SAM_Pvsamv1_InverterDatasheet ptr, const char* string, SAM_error* err);

	/**
	 * Set inv_ds_num_mppt: Weather file in TMY2, TMY3, EPW, or SAM CSV.
	 * type: string
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterDatasheet_inv_ds_num_mppt_set(SAM_Pvsamv1_InverterDatasheet ptr, const char* string, SAM_error* err);

	/**
	 * Set inv_ds_paco: AC maximum power rating
	 * type: numeric
	 * units: Wac
	 * options: None
	 * constraints: None
	 * required if: inverter_model=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterDatasheet_inv_ds_paco_set(SAM_Pvsamv1_InverterDatasheet ptr, float number, SAM_error* err);

	/**
	 * Set inv_ds_vdcmax: Maximum DC input operating voltage
	 * type: numeric
	 * units: Vdc
	 * options: None
	 * constraints: None
	 * required if: inverter_model=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterDatasheet_inv_ds_vdcmax_set(SAM_Pvsamv1_InverterDatasheet ptr, float number, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT const char* SAM_Pvsamv1_InverterDatasheet_inv_ds_eff_peak_or_nom_get(SAM_Pvsamv1_InverterDatasheet ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Pvsamv1_InverterDatasheet_inv_ds_eff_type_get(SAM_Pvsamv1_InverterDatasheet ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Pvsamv1_InverterDatasheet_inv_ds_eff_weighted_get(SAM_Pvsamv1_InverterDatasheet ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Pvsamv1_InverterDatasheet_inv_ds_mppt_hi_get(SAM_Pvsamv1_InverterDatasheet ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Pvsamv1_InverterDatasheet_inv_ds_mppt_low_get(SAM_Pvsamv1_InverterDatasheet ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Pvsamv1_InverterDatasheet_inv_ds_num_mppt_get(SAM_Pvsamv1_InverterDatasheet ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_InverterDatasheet_inv_ds_paco_get(SAM_Pvsamv1_InverterDatasheet ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_InverterDatasheet_inv_ds_vdcmax_get(SAM_Pvsamv1_InverterDatasheet ptr, SAM_error* err);



	/** 
	 * Create a InverterTempDerateDS variable table for a FlatPlatePVNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_Pvsamv1_InverterTempDerateDS SAM_Pvsamv1_InverterTempDerateDS_create(const char* def, SAM_error* err);


	/**
	 * Set inv_tdc_ds: Temperature derate curves for Inv Datasheet
	 * type: matrix
	 * units: Vdc
	 * options: None
	 * constraints: None
	 * required if: inverter_model=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterTempDerateDS_inv_tdc_ds_set(SAM_Pvsamv1_InverterTempDerateDS ptr, float* matrix, int nr, int nc, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float* SAM_Pvsamv1_InverterTempDerateDS_inv_tdc_ds_get(SAM_Pvsamv1_InverterTempDerateDS ptr, SAM_error* err);



	/** 
	 * Create a InverterPartLoadCurve variable table for a FlatPlatePVNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_Pvsamv1_InverterPartLoadCurve SAM_Pvsamv1_InverterPartLoadCurve_create(const char* def, SAM_error* err);


	/**
	 * Set inv_pd_eff_cec: Weather file in TMY2, TMY3, EPW, or SAM CSV.
	 * type: string
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterPartLoadCurve_inv_pd_eff_cec_set(SAM_Pvsamv1_InverterPartLoadCurve ptr, const char* string, SAM_error* err);

	/**
	 * Set inv_pd_eff_euro: Weather file in TMY2, TMY3, EPW, or SAM CSV.
	 * type: string
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterPartLoadCurve_inv_pd_eff_euro_set(SAM_Pvsamv1_InverterPartLoadCurve ptr, const char* string, SAM_error* err);

	/**
	 * Set inv_pd_eff_type: Weather file in TMY2, TMY3, EPW, or SAM CSV.
	 * type: string
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterPartLoadCurve_inv_pd_eff_type_set(SAM_Pvsamv1_InverterPartLoadCurve ptr, const char* string, SAM_error* err);

	/**
	 * Set inv_pd_mppt_hi: Weather file in TMY2, TMY3, EPW, or SAM CSV.
	 * type: string
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterPartLoadCurve_inv_pd_mppt_hi_set(SAM_Pvsamv1_InverterPartLoadCurve ptr, const char* string, SAM_error* err);

	/**
	 * Set inv_pd_mppt_low: Weather file in TMY2, TMY3, EPW, or SAM CSV.
	 * type: string
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterPartLoadCurve_inv_pd_mppt_low_set(SAM_Pvsamv1_InverterPartLoadCurve ptr, const char* string, SAM_error* err);

	/**
	 * Set inv_pd_num_mppt: Weather file in TMY2, TMY3, EPW, or SAM CSV.
	 * type: string
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterPartLoadCurve_inv_pd_num_mppt_set(SAM_Pvsamv1_InverterPartLoadCurve ptr, const char* string, SAM_error* err);

	/**
	 * Set inv_pd_paco: AC maximum power rating
	 * type: numeric
	 * units: Wac
	 * options: None
	 * constraints: None
	 * required if: inverter_model=2
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterPartLoadCurve_inv_pd_paco_set(SAM_Pvsamv1_InverterPartLoadCurve ptr, float number, SAM_error* err);

	/**
	 * Set inv_pd_vdcmax: Maximum DC input operating voltage
	 * type: numeric
	 * units: Vdc
	 * options: None
	 * constraints: None
	 * required if: inverter_model=2
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterPartLoadCurve_inv_pd_vdcmax_set(SAM_Pvsamv1_InverterPartLoadCurve ptr, float number, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT const char* SAM_Pvsamv1_InverterPartLoadCurve_inv_pd_eff_cec_get(SAM_Pvsamv1_InverterPartLoadCurve ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Pvsamv1_InverterPartLoadCurve_inv_pd_eff_euro_get(SAM_Pvsamv1_InverterPartLoadCurve ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Pvsamv1_InverterPartLoadCurve_inv_pd_eff_type_get(SAM_Pvsamv1_InverterPartLoadCurve ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Pvsamv1_InverterPartLoadCurve_inv_pd_mppt_hi_get(SAM_Pvsamv1_InverterPartLoadCurve ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Pvsamv1_InverterPartLoadCurve_inv_pd_mppt_low_get(SAM_Pvsamv1_InverterPartLoadCurve ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Pvsamv1_InverterPartLoadCurve_inv_pd_num_mppt_get(SAM_Pvsamv1_InverterPartLoadCurve ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_InverterPartLoadCurve_inv_pd_paco_get(SAM_Pvsamv1_InverterPartLoadCurve ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_InverterPartLoadCurve_inv_pd_vdcmax_get(SAM_Pvsamv1_InverterPartLoadCurve ptr, SAM_error* err);



	/** 
	 * Create a InverterTempDeratePLC variable table for a FlatPlatePVNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_Pvsamv1_InverterTempDeratePLC SAM_Pvsamv1_InverterTempDeratePLC_create(const char* def, SAM_error* err);


	/**
	 * Set inv_tdc_plc: Temperature derate curves for Part Load Curve
	 * type: matrix
	 * units: C
	 * options: None
	 * constraints: None
	 * required if: inverter_model=2
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterTempDeratePLC_inv_tdc_plc_set(SAM_Pvsamv1_InverterTempDeratePLC ptr, float* matrix, int nr, int nc, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float* SAM_Pvsamv1_InverterTempDeratePLC_inv_tdc_plc_get(SAM_Pvsamv1_InverterTempDeratePLC ptr, SAM_error* err);



	/** 
	 * Create a InverterCECCoefficientGenerator variable table for a FlatPlatePVNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_Pvsamv1_InverterCECCoefficientGenerator SAM_Pvsamv1_InverterCECCoefficientGenerator_create(const char* def, SAM_error* err);


	/**
	 * Set inv_cec_cg_mppt_hi: Weather file in TMY2, TMY3, EPW, or SAM CSV.
	 * type: string
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_mppt_hi_set(SAM_Pvsamv1_InverterCECCoefficientGenerator ptr, const char* string, SAM_error* err);

	/**
	 * Set inv_cec_cg_mppt_low: Weather file in TMY2, TMY3, EPW, or SAM CSV.
	 * type: string
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_mppt_low_set(SAM_Pvsamv1_InverterCECCoefficientGenerator ptr, const char* string, SAM_error* err);

	/**
	 * Set inv_cec_cg_num_mppt: Weather file in TMY2, TMY3, EPW, or SAM CSV.
	 * type: string
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_num_mppt_set(SAM_Pvsamv1_InverterCECCoefficientGenerator ptr, const char* string, SAM_error* err);

	/**
	 * Set inv_cec_cg_paco: AC maximum power rating
	 * type: numeric
	 * units: Wac
	 * options: None
	 * constraints: None
	 * required if: inverter_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_paco_set(SAM_Pvsamv1_InverterCECCoefficientGenerator ptr, float number, SAM_error* err);

	/**
	 * Set inv_cec_cg_sample_power_units: Weather file in TMY2, TMY3, EPW, or SAM CSV.
	 * type: string
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_sample_power_units_set(SAM_Pvsamv1_InverterCECCoefficientGenerator ptr, const char* string, SAM_error* err);

	/**
	 * Set inv_cec_cg_test_samples: Weather file in TMY2, TMY3, EPW, or SAM CSV.
	 * type: string
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_test_samples_set(SAM_Pvsamv1_InverterCECCoefficientGenerator ptr, const char* string, SAM_error* err);

	/**
	 * Set inv_cec_cg_vdcmax: Maximum DC input operating voltage
	 * type: numeric
	 * units: Vdc
	 * options: None
	 * constraints: None
	 * required if: inverter_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_vdcmax_set(SAM_Pvsamv1_InverterCECCoefficientGenerator ptr, float number, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT const char* SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_mppt_hi_get(SAM_Pvsamv1_InverterCECCoefficientGenerator ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_mppt_low_get(SAM_Pvsamv1_InverterCECCoefficientGenerator ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_num_mppt_get(SAM_Pvsamv1_InverterCECCoefficientGenerator ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_paco_get(SAM_Pvsamv1_InverterCECCoefficientGenerator ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_sample_power_units_get(SAM_Pvsamv1_InverterCECCoefficientGenerator ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_test_samples_get(SAM_Pvsamv1_InverterCECCoefficientGenerator ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_InverterCECCoefficientGenerator_inv_cec_cg_vdcmax_get(SAM_Pvsamv1_InverterCECCoefficientGenerator ptr, SAM_error* err);



	/** 
	 * Create a InverterTempDerateCECCG variable table for a FlatPlatePVNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_Pvsamv1_InverterTempDerateCECCG SAM_Pvsamv1_InverterTempDerateCECCG_create(const char* def, SAM_error* err);


	/**
	 * Set inv_tdc_cec_cg: Temperature derate curves for CEC Coef Gen
	 * type: matrix
	 * units: Vdc
	 * options: None
	 * constraints: None
	 * required if: inverter_model=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_InverterTempDerateCECCG_inv_tdc_cec_cg_set(SAM_Pvsamv1_InverterTempDerateCECCG ptr, float* matrix, int nr, int nc, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float* SAM_Pvsamv1_InverterTempDerateCECCG_inv_tdc_cec_cg_get(SAM_Pvsamv1_InverterTempDerateCECCG ptr, SAM_error* err);



	/** 
	 * Create a SystemDesign variable table for a FlatPlatePVNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_Pvsamv1_SystemDesign SAM_Pvsamv1_SystemDesign_create(const char* def, SAM_error* err);


	/**
	 * Set batt_max_power: Weather file in TMY2, TMY3, EPW, or SAM CSV.
	 * type: string
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_SystemDesign_batt_max_power_set(SAM_Pvsamv1_SystemDesign ptr, const char* string, SAM_error* err);

	/**
	 * Set desired_dcac_ratio: Weather file in TMY2, TMY3, EPW, or SAM CSV.
	 * type: string
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_SystemDesign_desired_dcac_ratio_set(SAM_Pvsamv1_SystemDesign ptr, const char* string, SAM_error* err);

	/**
	 * Set desired_size: Weather file in TMY2, TMY3, EPW, or SAM CSV.
	 * type: string
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_SystemDesign_desired_size_set(SAM_Pvsamv1_SystemDesign ptr, const char* string, SAM_error* err);

	/**
	 * Set enable_auto_size: Weather file in TMY2, TMY3, EPW, or SAM CSV.
	 * type: string
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_SystemDesign_enable_auto_size_set(SAM_Pvsamv1_SystemDesign ptr, const char* string, SAM_error* err);

	/**
	 * Set subarray1_gcr: Sub-array 1 Ground coverage ratio
	 * type: numeric
	 * units: 0..1
	 * options: None
	 * constraints: MIN=0,MAX=3
	 * required if: ?=0.3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray1_gcr_set(SAM_Pvsamv1_SystemDesign ptr, float number, SAM_error* err);

	/**
	 * Set subarray2_gcr: Sub-array 2 Ground coverage ratio
	 * type: numeric
	 * units: 0..1
	 * options: None
	 * constraints: MIN=0,MAX=3
	 * required if: ?=0.3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray2_gcr_set(SAM_Pvsamv1_SystemDesign ptr, float number, SAM_error* err);

	/**
	 * Set subarray2_modules_per_string: Sub-array 2 Modules per string
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: INTEGER,MIN=1
	 * required if: subarray2_enable=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray2_modules_per_string_set(SAM_Pvsamv1_SystemDesign ptr, float number, SAM_error* err);

	/**
	 * Set subarray2_nstrings: Sub-array 2 Number of parallel strings
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: INTEGER,MIN=1
	 * required if: subarray2_enable=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray2_nstrings_set(SAM_Pvsamv1_SystemDesign ptr, float number, SAM_error* err);

	/**
	 * Set subarray3_gcr: Sub-array 3 Ground coverage ratio
	 * type: numeric
	 * units: 0..1
	 * options: None
	 * constraints: MIN=0,MAX=3
	 * required if: ?=0.3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray3_gcr_set(SAM_Pvsamv1_SystemDesign ptr, float number, SAM_error* err);

	/**
	 * Set subarray3_modules_per_string: Sub-array 3 Modules per string
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: INTEGER,MIN=1
	 * required if: subarray3_enable=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray3_modules_per_string_set(SAM_Pvsamv1_SystemDesign ptr, float number, SAM_error* err);

	/**
	 * Set subarray3_nstrings: Sub-array 3 Number of parallel strings
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: INTEGER,MIN=1
	 * required if: subarray3_enable=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray3_nstrings_set(SAM_Pvsamv1_SystemDesign ptr, float number, SAM_error* err);

	/**
	 * Set subarray4_gcr: Sub-array 4 Ground coverage ratio
	 * type: numeric
	 * units: 0..1
	 * options: None
	 * constraints: MIN=0,MAX=3
	 * required if: ?=0.3
	 */
	SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray4_gcr_set(SAM_Pvsamv1_SystemDesign ptr, float number, SAM_error* err);

	/**
	 * Set subarray4_modules_per_string: Sub-array 4 Modules per string
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: INTEGER,MIN=1
	 * required if: subarray4_enable=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray4_modules_per_string_set(SAM_Pvsamv1_SystemDesign ptr, float number, SAM_error* err);

	/**
	 * Set subarray4_nstrings: Sub-array 4 Number of parallel strings
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: INTEGER,MIN=1
	 * required if: subarray4_enable=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_SystemDesign_subarray4_nstrings_set(SAM_Pvsamv1_SystemDesign ptr, float number, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT const char* SAM_Pvsamv1_SystemDesign_batt_max_power_get(SAM_Pvsamv1_SystemDesign ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Pvsamv1_SystemDesign_desired_dcac_ratio_get(SAM_Pvsamv1_SystemDesign ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Pvsamv1_SystemDesign_desired_size_get(SAM_Pvsamv1_SystemDesign ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Pvsamv1_SystemDesign_enable_auto_size_get(SAM_Pvsamv1_SystemDesign ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_SystemDesign_subarray1_gcr_get(SAM_Pvsamv1_SystemDesign ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_SystemDesign_subarray2_gcr_get(SAM_Pvsamv1_SystemDesign ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_SystemDesign_subarray2_modules_per_string_get(SAM_Pvsamv1_SystemDesign ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_SystemDesign_subarray2_nstrings_get(SAM_Pvsamv1_SystemDesign ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_SystemDesign_subarray3_gcr_get(SAM_Pvsamv1_SystemDesign ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_SystemDesign_subarray3_modules_per_string_get(SAM_Pvsamv1_SystemDesign ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_SystemDesign_subarray3_nstrings_get(SAM_Pvsamv1_SystemDesign ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_SystemDesign_subarray4_gcr_get(SAM_Pvsamv1_SystemDesign ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_SystemDesign_subarray4_modules_per_string_get(SAM_Pvsamv1_SystemDesign ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_SystemDesign_subarray4_nstrings_get(SAM_Pvsamv1_SystemDesign ptr, SAM_error* err);



	/** 
	 * Create a ShadingAndLayout variable table for a FlatPlatePVNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_Pvsamv1_ShadingAndLayout SAM_Pvsamv1_ShadingAndLayout_create(const char* def, SAM_error* err);


	/**
	 * Set module_aspect_ratio: Module aspect ratio
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: ?=1.7
	 */
	SAM_EXPORT void SAM_Pvsamv1_ShadingAndLayout_module_aspect_ratio_set(SAM_Pvsamv1_ShadingAndLayout ptr, float number, SAM_error* err);

	/**
	 * Set subarray1_mod_orient: Sub-array 1 Module orientation
	 * type: numeric
	 * units: 0/1
	 * options: 0=portrait,1=landscape
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_ShadingAndLayout_subarray1_mod_orient_set(SAM_Pvsamv1_ShadingAndLayout ptr, float number, SAM_error* err);

	/**
	 * Set subarray1_nmodx: Sub-array 1 Number of modules along bottom of row
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: INTEGER,POSITIVE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_ShadingAndLayout_subarray1_nmodx_set(SAM_Pvsamv1_ShadingAndLayout ptr, float number, SAM_error* err);

	/**
	 * Set subarray1_nmody: Sub-array 1 Number of modules along side of row
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: INTEGER,POSITIVE
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_ShadingAndLayout_subarray1_nmody_set(SAM_Pvsamv1_ShadingAndLayout ptr, float number, SAM_error* err);

	/**
	 * Set subarray2_mod_orient: Sub-array 2 Module orientation
	 * type: numeric
	 * units: 0/1
	 * options: 0=portrait,1=landscape
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: subarray2_enable=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_ShadingAndLayout_subarray2_mod_orient_set(SAM_Pvsamv1_ShadingAndLayout ptr, float number, SAM_error* err);

	/**
	 * Set subarray2_nmodx: Sub-array 2 Number of modules along bottom of row
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: INTEGER,POSITIVE
	 * required if: subarray2_enable=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_ShadingAndLayout_subarray2_nmodx_set(SAM_Pvsamv1_ShadingAndLayout ptr, float number, SAM_error* err);

	/**
	 * Set subarray2_nmody: Sub-array 2 Number of modules along side of row
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: INTEGER,POSITIVE
	 * required if: subarray2_enable=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_ShadingAndLayout_subarray2_nmody_set(SAM_Pvsamv1_ShadingAndLayout ptr, float number, SAM_error* err);

	/**
	 * Set subarray3_mod_orient: Sub-array 3 Module orientation
	 * type: numeric
	 * units: 0/1
	 * options: 0=portrait,1=landscape
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: subarray3_enable=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_ShadingAndLayout_subarray3_mod_orient_set(SAM_Pvsamv1_ShadingAndLayout ptr, float number, SAM_error* err);

	/**
	 * Set subarray3_nmodx: Sub-array 3 Number of modules along bottom of row
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: INTEGER,POSITIVE
	 * required if: subarray3_enable=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_ShadingAndLayout_subarray3_nmodx_set(SAM_Pvsamv1_ShadingAndLayout ptr, float number, SAM_error* err);

	/**
	 * Set subarray3_nmody: Sub-array 3 Number of modules along side of row
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: INTEGER,POSITIVE
	 * required if: subarray3_enable=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_ShadingAndLayout_subarray3_nmody_set(SAM_Pvsamv1_ShadingAndLayout ptr, float number, SAM_error* err);

	/**
	 * Set subarray4_mod_orient: Sub-array 4 Module orientation
	 * type: numeric
	 * units: 0/1
	 * options: 0=portrait,1=landscape
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: subarray4_enable=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_ShadingAndLayout_subarray4_mod_orient_set(SAM_Pvsamv1_ShadingAndLayout ptr, float number, SAM_error* err);

	/**
	 * Set subarray4_nmodx: Sub-array 4 Number of modules along bottom of row
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: INTEGER,POSITIVE
	 * required if: subarray4_enable=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_ShadingAndLayout_subarray4_nmodx_set(SAM_Pvsamv1_ShadingAndLayout ptr, float number, SAM_error* err);

	/**
	 * Set subarray4_nmody: Sub-array 4 Number of modules along side of row
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: INTEGER,POSITIVE
	 * required if: subarray4_enable=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_ShadingAndLayout_subarray4_nmody_set(SAM_Pvsamv1_ShadingAndLayout ptr, float number, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_Pvsamv1_ShadingAndLayout_module_aspect_ratio_get(SAM_Pvsamv1_ShadingAndLayout ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_ShadingAndLayout_subarray1_mod_orient_get(SAM_Pvsamv1_ShadingAndLayout ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_ShadingAndLayout_subarray1_nmodx_get(SAM_Pvsamv1_ShadingAndLayout ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_ShadingAndLayout_subarray1_nmody_get(SAM_Pvsamv1_ShadingAndLayout ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_ShadingAndLayout_subarray2_mod_orient_get(SAM_Pvsamv1_ShadingAndLayout ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_ShadingAndLayout_subarray2_nmodx_get(SAM_Pvsamv1_ShadingAndLayout ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_ShadingAndLayout_subarray2_nmody_get(SAM_Pvsamv1_ShadingAndLayout ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_ShadingAndLayout_subarray3_mod_orient_get(SAM_Pvsamv1_ShadingAndLayout ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_ShadingAndLayout_subarray3_nmodx_get(SAM_Pvsamv1_ShadingAndLayout ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_ShadingAndLayout_subarray3_nmody_get(SAM_Pvsamv1_ShadingAndLayout ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_ShadingAndLayout_subarray4_mod_orient_get(SAM_Pvsamv1_ShadingAndLayout ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_ShadingAndLayout_subarray4_nmodx_get(SAM_Pvsamv1_ShadingAndLayout ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_ShadingAndLayout_subarray4_nmody_get(SAM_Pvsamv1_ShadingAndLayout ptr, SAM_error* err);



	/** 
	 * Create a Losses variable table for a FlatPlatePVNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_Pvsamv1_Losses SAM_Pvsamv1_Losses_create(const char* def, SAM_error* err);


	/**
	 * Set subarray1_soiling: Sub-array 1 Monthly soiling loss
	 * type: array
	 * units: %
	 * options: None
	 * constraints: LENGTH=12
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Losses_subarray1_soiling_set(SAM_Pvsamv1_Losses ptr, float* array, int length, SAM_error* err);

	/**
	 * Set subarray2_soiling: Sub-array 2 Monthly soiling loss
	 * type: array
	 * units: %
	 * options: None
	 * constraints: LENGTH=12
	 * required if: subarray2_enable=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_Losses_subarray2_soiling_set(SAM_Pvsamv1_Losses ptr, float* array, int length, SAM_error* err);

	/**
	 * Set subarray3_soiling: Sub-array 3 Monthly soiling loss
	 * type: array
	 * units: %
	 * options: None
	 * constraints: LENGTH=12
	 * required if: subarray3_enable=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_Losses_subarray3_soiling_set(SAM_Pvsamv1_Losses ptr, float* array, int length, SAM_error* err);

	/**
	 * Set subarray4_soiling: Sub-array 4 Monthly soiling loss
	 * type: array
	 * units: %
	 * options: None
	 * constraints: LENGTH=12
	 * required if: subarray4_enable=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_Losses_subarray4_soiling_set(SAM_Pvsamv1_Losses ptr, float* array, int length, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float* SAM_Pvsamv1_Losses_subarray1_soiling_get(SAM_Pvsamv1_Losses ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Pvsamv1_Losses_subarray2_soiling_get(SAM_Pvsamv1_Losses ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Pvsamv1_Losses_subarray3_soiling_get(SAM_Pvsamv1_Losses ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Pvsamv1_Losses_subarray4_soiling_get(SAM_Pvsamv1_Losses ptr, SAM_error* err);



	/** 
	 * Create a Common variable table for a FlatPlatePVNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_Pvsamv1_Common SAM_Pvsamv1_Common_create(const char* def, SAM_error* err);


	/**
	 * Set LeadAcid_q10_computed: Capacity at 10-hour discharge rate
	 * type: numeric
	 * units: Ah
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_LeadAcid_q10_computed_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set LeadAcid_q20_computed: Capacity at 20-hour discharge rate
	 * type: numeric
	 * units: Ah
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_LeadAcid_q20_computed_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set LeadAcid_qn_computed: Capacity at discharge rate for n-hour rate
	 * type: numeric
	 * units: Ah
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_LeadAcid_qn_computed_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set LeadAcid_tn: Time to discharge
	 * type: numeric
	 * units: h
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_LeadAcid_tn_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set ac_lifetime_losses: Lifetime daily AC losses
	 * type: array
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: en_ac_lifetime_losses=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_ac_lifetime_losses_set(SAM_Pvsamv1_Common ptr, float* array, int length, SAM_error* err);

	/**
	 * Set adjust:constant: Constant loss adjustment
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: MAX=100
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_adjust:constant_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set adjust:hourly: Hourly loss adjustments
	 * type: array
	 * units: %
	 * options: None
	 * constraints: LENGTH=8760
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_adjust:hourly_set(SAM_Pvsamv1_Common ptr, float* array, int length, SAM_error* err);

	/**
	 * Set adjust:periods: Period-based loss adjustments
	 * type: matrix
	 * units: %
	 * options: n x 3 matrix [ start, end, loss ]
	 * constraints: COLS=3
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_adjust:periods_set(SAM_Pvsamv1_Common ptr, float* matrix, int nr, int nc, SAM_error* err);

	/**
	 * Set analysis_period: Lifetime analysis period
	 * type: numeric
	 * units: years
	 * options: None
	 * constraints: None
	 * required if: system_use_lifetime_output=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_analysis_period_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set batt_C_rate: Rate at which voltage vs. capacity curve input
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_C_rate_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set batt_Cp: Battery specific heat capacity
	 * type: numeric
	 * units: J/KgK
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_Cp_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set batt_Qexp: Cell capacity at end of exponential zone
	 * type: numeric
	 * units: Ah
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_Qexp_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set batt_Qfull: Fully charged cell capacity
	 * type: numeric
	 * units: Ah
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_Qfull_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set batt_Qfull_flow: Fully charged flow battery capacity
	 * type: numeric
	 * units: Ah
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_Qfull_flow_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set batt_Qnom: Cell capacity at end of nominal zone
	 * type: numeric
	 * units: Ah
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_Qnom_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set batt_Vexp: Cell voltage at end of exponential zone
	 * type: numeric
	 * units: V
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_Vexp_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set batt_Vfull: Fully charged cell voltage
	 * type: numeric
	 * units: V
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_Vfull_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set batt_Vnom: Cell voltage at end of nominal zone
	 * type: numeric
	 * units: V
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_Vnom_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set batt_Vnom_default: Default nominal cell voltage
	 * type: numeric
	 * units: V
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_Vnom_default_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set batt_ac_dc_efficiency: Inverter AC to battery DC efficiency
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_ac_dc_efficiency_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set batt_ac_or_dc: Battery interconnection (AC or DC)
	 * type: numeric
	 * units: None
	 * options: 0=DC_Connected,1=AC_Connected
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_ac_or_dc_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set batt_auto_gridcharge_max_daily: Allowed grid charging percent per day for automated dispatch
	 * type: numeric
	 * units: kW
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_auto_gridcharge_max_daily_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set batt_calendar_a: Calendar life model coefficient
	 * type: numeric
	 * units: 1/sqrt(day)
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_calendar_a_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set batt_calendar_b: Calendar life model coefficient
	 * type: numeric
	 * units: K
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_calendar_b_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set batt_calendar_c: Calendar life model coefficient
	 * type: numeric
	 * units: K
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_calendar_c_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set batt_calendar_choice: Calendar life degradation input option
	 * type: numeric
	 * units: 0/1/2
	 * options: 0=NoCalendarDegradation,1=LithiomIonModel,2=InputLossTable
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_calendar_choice_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set batt_calendar_lifetime_matrix: Days vs capacity
	 * type: matrix
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_calendar_lifetime_matrix_set(SAM_Pvsamv1_Common ptr, float* matrix, int nr, int nc, SAM_error* err);

	/**
	 * Set batt_calendar_q0: Calendar life model initial capacity cofficient
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_calendar_q0_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set batt_chem: Battery chemistry
	 * type: numeric
	 * units: None
	 * options: 0=LeadAcid,1=LiIon
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_chem_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set batt_computed_bank_capacity: Computed bank capacity
	 * type: numeric
	 * units: kWh
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_computed_bank_capacity_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set batt_computed_series: Number of cells in series
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_computed_series_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set batt_computed_strings: Number of strings of cells
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_computed_strings_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set batt_current_charge_max: Maximum charge current
	 * type: numeric
	 * units: A
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_current_charge_max_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set batt_current_choice: Limit cells by current or power
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_current_choice_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set batt_current_discharge_max: Maximum discharge current
	 * type: numeric
	 * units: A
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_current_discharge_max_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set batt_custom_dispatch: Custom battery power for every time step
	 * type: array
	 * units: kW
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_dispatch_choice=3
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_custom_dispatch_set(SAM_Pvsamv1_Common ptr, float* array, int length, SAM_error* err);

	/**
	 * Set batt_cycle_cost: Input battery cycle costs
	 * type: numeric
	 * units: $/cycle-kWh
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_cycle_cost_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set batt_cycle_cost_choice: Use SAM model for cycle costs or input custom
	 * type: numeric
	 * units: 0/1
	 * options: 0=UseCostModel,1=InputCost
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_cycle_cost_choice_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set batt_dc_ac_efficiency: Battery DC to AC efficiency
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_dc_ac_efficiency_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set batt_dc_dc_efficiency: PV DC to battery DC efficiency
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_dc_dc_efficiency_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set batt_dispatch_auto_can_charge: PV charging allowed for automated dispatch?
	 * type: numeric
	 * units: kW
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_dispatch_auto_can_charge_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set batt_dispatch_auto_can_clipcharge: Battery can charge from clipped PV for automated dispatch?
	 * type: numeric
	 * units: kW
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_dispatch_auto_can_clipcharge_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set batt_dispatch_auto_can_fuelcellcharge: Charging from fuel cell allowed for automated dispatch?
	 * type: numeric
	 * units: kW
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_dispatch_auto_can_fuelcellcharge_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set batt_dispatch_auto_can_gridcharge: Grid charging allowed for automated dispatch?
	 * type: numeric
	 * units: kW
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_dispatch_auto_can_gridcharge_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set batt_dispatch_choice: Battery dispatch algorithm
	 * type: numeric
	 * units: 0/1/2/3/4
	 * options: If behind the meter: 0=PeakShavingLookAhead,1=PeakShavingLookBehind,2=InputGridTarget,3=InputBatteryPower,4=ManualDispatch, if front of meter: 0=AutomatedLookAhead,1=AutomatedLookBehind,2=AutomatedInputForecast,3=InputBatteryPower,4=ManualDispatch
	 * constraints: None
	 * required if: en_batt=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_dispatch_choice_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set batt_dispatch_update_frequency_hours: Frequency to update the look-ahead dispatch
	 * type: numeric
	 * units: hours
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_dispatch_update_frequency_hours_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set batt_h_to_ambient: Heat transfer between battery and environment
	 * type: numeric
	 * units: W/m2K
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_h_to_ambient_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set batt_height: Battery height
	 * type: numeric
	 * units: m
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_height_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set batt_initial_SOC: Initial state-of-charge
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_initial_SOC_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set batt_length: Battery length
	 * type: numeric
	 * units: m
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_length_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set batt_lifetime_matrix: Cycles vs capacity at different depths-of-discharge
	 * type: matrix
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_lifetime_matrix_set(SAM_Pvsamv1_Common ptr, float* matrix, int nr, int nc, SAM_error* err);

	/**
	 * Set batt_look_ahead_hours: Hours to look ahead in automated dispatch
	 * type: numeric
	 * units: hours
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_look_ahead_hours_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set batt_loss_choice: Loss power input option
	 * type: numeric
	 * units: 0/1
	 * options: 0=Monthly,1=TimeSeries
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_loss_choice_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set batt_losses: Battery system losses at each timestep
	 * type: array
	 * units: kW
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_losses_set(SAM_Pvsamv1_Common ptr, float* array, int length, SAM_error* err);

	/**
	 * Set batt_losses_charging: Battery system losses when charging
	 * type: array
	 * units: kW
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_losses_charging_set(SAM_Pvsamv1_Common ptr, float* array, int length, SAM_error* err);

	/**
	 * Set batt_losses_discharging: Battery system losses when discharging
	 * type: array
	 * units: kW
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_losses_discharging_set(SAM_Pvsamv1_Common ptr, float* array, int length, SAM_error* err);

	/**
	 * Set batt_losses_idle: Battery system losses when idle
	 * type: array
	 * units: kW
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_losses_idle_set(SAM_Pvsamv1_Common ptr, float* array, int length, SAM_error* err);

	/**
	 * Set batt_mass: Battery mass
	 * type: numeric
	 * units: kg
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_mass_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set batt_maximum_SOC: Maximum allowed state-of-charge
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_maximum_SOC_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set batt_meter_position: Position of battery relative to electric meter
	 * type: numeric
	 * units: None
	 * options: 0=BehindTheMeter,1=FrontOfMeter
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_meter_position_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set batt_minimum_SOC: Minimum allowed state-of-charge
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_minimum_SOC_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set batt_minimum_modetime: Minimum time at charge state
	 * type: numeric
	 * units: min
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_minimum_modetime_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set batt_power_charge_max: Maximum charge power
	 * type: numeric
	 * units: kW
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_power_charge_max_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set batt_power_discharge_max: Maximum discharge power
	 * type: numeric
	 * units: kW
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_power_discharge_max_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set batt_pv_clipping_forecast: PV clipping forecast
	 * type: array
	 * units: kW
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_meter_position=1&batt_dispatch_choice=2
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_pv_clipping_forecast_set(SAM_Pvsamv1_Common ptr, float* array, int length, SAM_error* err);

	/**
	 * Set batt_pv_dc_forecast: PV dc power forecast
	 * type: array
	 * units: kW
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_meter_position=1&batt_dispatch_choice=2
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_pv_dc_forecast_set(SAM_Pvsamv1_Common ptr, float* array, int length, SAM_error* err);

	/**
	 * Set batt_replacement_capacity: Capacity degradation at which to replace battery
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_replacement_capacity_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set batt_replacement_option: Enable battery replacement?
	 * type: numeric
	 * units: 0=none,1=capacity based,2=user schedule
	 * options: None
	 * constraints: INTEGER,MIN=0,MAX=2
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_replacement_option_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set batt_replacement_schedule: Battery bank replacements per year (user specified)
	 * type: array
	 * units: number/year
	 * options: None
	 * constraints: None
	 * required if: batt_replacement_option=2
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_replacement_schedule_set(SAM_Pvsamv1_Common ptr, float* array, int length, SAM_error* err);

	/**
	 * Set batt_resistance: Internal resistance
	 * type: numeric
	 * units: Ohm
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_resistance_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set batt_room_temperature_celsius: Temperature of storage room
	 * type: array
	 * units: C
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_room_temperature_celsius_set(SAM_Pvsamv1_Common ptr, float* array, int length, SAM_error* err);

	/**
	 * Set batt_target_choice: Target power input option
	 * type: numeric
	 * units: 0/1
	 * options: 0=InputMonthlyTarget,1=InputFullTimeSeries
	 * constraints: None
	 * required if: en_batt=1&batt_meter_position=0&batt_dispatch_choice=2
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_target_choice_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set batt_target_power: Grid target power for every time step
	 * type: array
	 * units: kW
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_meter_position=0&batt_dispatch_choice=2
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_target_power_set(SAM_Pvsamv1_Common ptr, float* array, int length, SAM_error* err);

	/**
	 * Set batt_target_power_monthly: Grid target power on monthly basis
	 * type: array
	 * units: kW
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_meter_position=0&batt_dispatch_choice=2
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_target_power_monthly_set(SAM_Pvsamv1_Common ptr, float* array, int length, SAM_error* err);

	/**
	 * Set batt_voltage_choice: Battery voltage input option
	 * type: numeric
	 * units: 0/1
	 * options: 0=UseVoltageModel,1=InputVoltageTable
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_voltage_choice_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set batt_voltage_matrix: Battery voltage vs. depth-of-discharge
	 * type: matrix
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_voltage_matrix_set(SAM_Pvsamv1_Common ptr, float* matrix, int nr, int nc, SAM_error* err);

	/**
	 * Set batt_width: Battery width
	 * type: numeric
	 * units: m
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_batt_width_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set cap_vs_temp: Effective capacity as function of temperature
	 * type: matrix
	 * units: C,%
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_cap_vs_temp_set(SAM_Pvsamv1_Common ptr, float* matrix, int nr, int nc, SAM_error* err);

	/**
	 * Set dc_adjust:constant: DC Constant loss adjustment
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: MAX=100
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_dc_adjust:constant_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set dc_adjust:hourly: DC Hourly loss adjustments
	 * type: array
	 * units: %
	 * options: None
	 * constraints: LENGTH=8760
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_dc_adjust:hourly_set(SAM_Pvsamv1_Common ptr, float* array, int length, SAM_error* err);

	/**
	 * Set dc_adjust:periods: DC Period-based loss adjustments
	 * type: matrix
	 * units: %
	 * options: n x 3 matrix [ start, end, loss ]
	 * constraints: COLS=3
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_dc_adjust:periods_set(SAM_Pvsamv1_Common ptr, float* matrix, int nr, int nc, SAM_error* err);

	/**
	 * Set dc_degradation: Annual module degradation
	 * type: array
	 * units: %/year
	 * options: None
	 * constraints: None
	 * required if: system_use_lifetime_output=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_dc_degradation_set(SAM_Pvsamv1_Common ptr, float* array, int length, SAM_error* err);

	/**
	 * Set dc_lifetime_losses: Lifetime daily DC losses
	 * type: array
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: en_dc_lifetime_losses=1
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_dc_lifetime_losses_set(SAM_Pvsamv1_Common ptr, float* array, int length, SAM_error* err);

	/**
	 * Set dispatch_manual_charge: Periods 1-6 charging from system allowed?
	 * type: array
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_dispatch_choice=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_dispatch_manual_charge_set(SAM_Pvsamv1_Common ptr, float* array, int length, SAM_error* err);

	/**
	 * Set dispatch_manual_discharge: Periods 1-6 discharging allowed?
	 * type: array
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_dispatch_choice=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_dispatch_manual_discharge_set(SAM_Pvsamv1_Common ptr, float* array, int length, SAM_error* err);

	/**
	 * Set dispatch_manual_fuelcellcharge: Periods 1-6 charging from fuel cell allowed?
	 * type: array
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_dispatch_manual_fuelcellcharge_set(SAM_Pvsamv1_Common ptr, float* array, int length, SAM_error* err);

	/**
	 * Set dispatch_manual_gridcharge: Periods 1-6 grid charging allowed?
	 * type: array
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_dispatch_choice=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_dispatch_manual_gridcharge_set(SAM_Pvsamv1_Common ptr, float* array, int length, SAM_error* err);

	/**
	 * Set dispatch_manual_percent_discharge: Periods 1-6 discharge percent
	 * type: array
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_dispatch_choice=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_dispatch_manual_percent_discharge_set(SAM_Pvsamv1_Common ptr, float* array, int length, SAM_error* err);

	/**
	 * Set dispatch_manual_percent_gridcharge: Periods 1-6 gridcharge percent
	 * type: array
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_dispatch_choice=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_dispatch_manual_percent_gridcharge_set(SAM_Pvsamv1_Common ptr, float* array, int length, SAM_error* err);

	/**
	 * Set dispatch_manual_sched: Battery dispatch schedule for weekday
	 * type: matrix
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_dispatch_choice=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_dispatch_manual_sched_set(SAM_Pvsamv1_Common ptr, float* matrix, int nr, int nc, SAM_error* err);

	/**
	 * Set dispatch_manual_sched_weekend: Battery dispatch schedule for weekend
	 * type: matrix
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_dispatch_choice=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_dispatch_manual_sched_weekend_set(SAM_Pvsamv1_Common ptr, float* matrix, int nr, int nc, SAM_error* err);

	/**
	 * Set dispatch_sched_weekday: Diurnal weekday TOD periods
	 * type: matrix
	 * units: 1..9
	 * options: 12 x 24 matrix
	 * constraints: None
	 * required if: en_batt=1&batt_meter_position=1&batt_dispatch_choice=2
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_dispatch_sched_weekday_set(SAM_Pvsamv1_Common ptr, float* matrix, int nr, int nc, SAM_error* err);

	/**
	 * Set dispatch_sched_weekend: Diurnal weekend TOD periods
	 * type: matrix
	 * units: 1..9
	 * options: 12 x 24 matrix
	 * constraints: None
	 * required if: en_batt=1&batt_meter_position=1&batt_dispatch_choice=2
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_dispatch_sched_weekend_set(SAM_Pvsamv1_Common ptr, float* matrix, int nr, int nc, SAM_error* err);

	/**
	 * Set dispatch_tod_factors: TOD factors for periods 1-9
	 * type: array
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_meter_position=1&batt_dispatch_choice=2
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_dispatch_tod_factors_set(SAM_Pvsamv1_Common ptr, float* array, int length, SAM_error* err);

	/**
	 * Set en_ac_lifetime_losses: Enable lifetime daily AC losses
	 * type: numeric
	 * units: 0/1
	 * options: None
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_en_ac_lifetime_losses_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set en_batt: Enable battery storage model
	 * type: numeric
	 * units: 0/1
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_en_batt_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set en_dc_lifetime_losses: Enable lifetime daily DC losses
	 * type: numeric
	 * units: 0/1
	 * options: None
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_en_dc_lifetime_losses_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set en_electricity_rates: Enable Electricity Rates
	 * type: numeric
	 * units: None
	 * options: 0/1
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_en_electricity_rates_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set fuelcell_power: Electricity from fuel cell
	 * type: array
	 * units: kW
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_fuelcell_power_set(SAM_Pvsamv1_Common ptr, float* array, int length, SAM_error* err);

	/**
	 * Set load: Electricity load (year 1)
	 * type: array
	 * units: kW
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_load_set(SAM_Pvsamv1_Common ptr, float* array, int length, SAM_error* err);

	/**
	 * Set mlm_AM_c_lp0: Coefficient 0 for Lee/Panchula Air Mass Modifier
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_mlm_AM_c_lp0_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set mlm_AM_c_lp1: Coefficient 1 for Lee/Panchula Air Mass Modifier
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_mlm_AM_c_lp1_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set mlm_AM_c_lp2: Coefficient 2 for Lee/Panchula Air Mass Modifier
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_mlm_AM_c_lp2_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set mlm_AM_c_lp3: Coefficient 3 for Lee/Panchula Air Mass Modifier
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_mlm_AM_c_lp3_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set mlm_AM_c_lp4: Coefficient 4 for Lee/Panchula Air Mass Modifier
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_mlm_AM_c_lp4_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set mlm_AM_c_lp5: Coefficient 5 for Lee/Panchula Air Mass Modifier
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_mlm_AM_c_lp5_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set mlm_AM_c_sa0: Coefficient 0 for Sandia Air Mass Modifier
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_mlm_AM_c_sa0_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set mlm_AM_c_sa1: Coefficient 1 for Sandia Air Mass Modifier
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_mlm_AM_c_sa1_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set mlm_AM_c_sa2: Coefficient 2 for Sandia Air Mass Modifier
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_mlm_AM_c_sa2_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set mlm_AM_c_sa3: Coefficient 3 for Sandia Air Mass Modifier
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_mlm_AM_c_sa3_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set mlm_AM_c_sa4: Coefficient 4 for Sandia Air Mass Modifier
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_mlm_AM_c_sa4_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set mlm_AM_mode: Air-mass modifier mode
	 * type: numeric
	 * units: -
	 * options: 1: Do not consider AM effects, 2: Use Sandia polynomial [corr=f(AM)], 3: Use standard coefficients from DeSoto model [corr=f(AM)], 4: Use First Solar polynomial [corr=f(AM, p_wat)]
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_mlm_AM_mode_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set mlm_D2MuTau: Coefficient for recombination losses
	 * type: numeric
	 * units: V
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_mlm_D2MuTau_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set mlm_E_g: Reference bandgap energy
	 * type: numeric
	 * units: eV
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_mlm_E_g_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set mlm_IAM_c_as: ASHRAE incidence modifier coefficient b_0
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_mlm_IAM_c_as_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set mlm_IAM_c_cs_iamValue: Spline IAM - IAM values
	 * type: array
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_mlm_IAM_c_cs_iamValue_set(SAM_Pvsamv1_Common ptr, float* array, int length, SAM_error* err);

	/**
	 * Set mlm_IAM_c_cs_incAngle: Spline IAM - Incidence angles
	 * type: array
	 * units: deg
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_mlm_IAM_c_cs_incAngle_set(SAM_Pvsamv1_Common ptr, float* array, int length, SAM_error* err);

	/**
	 * Set mlm_IAM_c_sa0: Sandia IAM coefficient 0
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_mlm_IAM_c_sa0_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set mlm_IAM_c_sa1: Sandia IAM coefficient 1
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_mlm_IAM_c_sa1_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set mlm_IAM_c_sa2: Sandia IAM coefficient 2
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_mlm_IAM_c_sa2_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set mlm_IAM_c_sa3: Sandia IAM coefficient 3
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_mlm_IAM_c_sa3_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set mlm_IAM_c_sa4: Sandia IAM coefficient 4
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_mlm_IAM_c_sa4_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set mlm_IAM_c_sa5: Sandia IAM coefficient 5
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_mlm_IAM_c_sa5_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set mlm_IAM_mode: Incidence Angle Modifier mode
	 * type: numeric
	 * units: -
	 * options: 1: Use ASHRAE formula, 2: Use Sandia polynomial, 3: Use cubic spline with user-supplied data
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_mlm_IAM_mode_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set mlm_I_mp_ref: I_mp at STC
	 * type: numeric
	 * units: A
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_mlm_I_mp_ref_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set mlm_I_sc_ref: I_sc at STC
	 * type: numeric
	 * units: A
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_mlm_I_sc_ref_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set mlm_Length: Module length (long side)
	 * type: numeric
	 * units: m
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_mlm_Length_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set mlm_N_diodes: Number of diodes
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_mlm_N_diodes_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set mlm_N_parallel: Number of cells in parallel
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_mlm_N_parallel_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set mlm_N_series: Number of cells in series
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_mlm_N_series_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set mlm_R_s: Series resistance
	 * type: numeric
	 * units: V/A
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_mlm_R_s_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set mlm_R_sh0: Rsh,0
	 * type: numeric
	 * units: V/A
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_mlm_R_sh0_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set mlm_R_shexp: Rsh exponential coefficient
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_mlm_R_shexp_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set mlm_R_shref: Reference shunt resistance
	 * type: numeric
	 * units: V/A
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_mlm_R_shref_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set mlm_S_ref: Reference irradiance (Typically 1000W/m²)
	 * type: numeric
	 * units: W/m²
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_mlm_S_ref_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set mlm_T_c_fa_U0: Extended Faiman model U_0
	 * type: numeric
	 * units: W/m²K
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_mlm_T_c_fa_U0_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set mlm_T_c_fa_U1: Extended Faiman model U_1
	 * type: numeric
	 * units: W/m³sK
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_mlm_T_c_fa_U1_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set mlm_T_c_fa_alpha: Extended Faiman model absorptivity
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_mlm_T_c_fa_alpha_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set mlm_T_c_no_mounting: NOCT Array mounting height
	 * type: numeric
	 * units: -
	 * options: 0=one story,1=two story
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_mlm_T_c_no_mounting_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set mlm_T_c_no_standoff: NOCT standoff mode
	 * type: numeric
	 * units: -
	 * options: 0=bipv,1=>3.5in,2=2.5-3.5in,3=1.5-2.5in,4=0.5-1.5in,5=<0.5in,6=ground/rack
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_mlm_T_c_no_standoff_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set mlm_T_c_no_tnoct: NOCT cell temperature
	 * type: numeric
	 * units: °C
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_mlm_T_c_no_tnoct_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set mlm_T_mode: Cell temperature model mode
	 * type: numeric
	 * units: -
	 * options: 1: NOCT
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_mlm_T_mode_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set mlm_T_ref: Reference temperature (Typically 25°C)
	 * type: numeric
	 * units: °C
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_mlm_T_ref_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set mlm_V_mp_ref: V_mp at STC
	 * type: numeric
	 * units: V
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_mlm_V_mp_ref_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set mlm_V_oc_ref: V_oc at STC
	 * type: numeric
	 * units: V
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_mlm_V_oc_ref_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set mlm_Width: Module width (short side)
	 * type: numeric
	 * units: m
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_mlm_Width_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set mlm_alpha_isc: Temperature coefficient for I_sc
	 * type: numeric
	 * units: A/K
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_mlm_alpha_isc_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set mlm_beta_voc_spec: Temperature coefficient for V_oc
	 * type: numeric
	 * units: V/K
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_mlm_beta_voc_spec_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set mlm_groundRelfectionFraction: Ground reflection fraction
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_mlm_groundRelfectionFraction_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set mlm_mu_n: Temperature coefficient of gamma
	 * type: numeric
	 * units: 1/K
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_mlm_mu_n_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set mlm_n_0: Gamma
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: module_model=5
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_mlm_n_0_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set om_replacement_cost1: Cost to replace battery per kWh
	 * type: array
	 * units: $/kWh
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_om_replacement_cost1_set(SAM_Pvsamv1_Common ptr, float* array, int length, SAM_error* err);

	/**
	 * Set ond_Aux_Loss: 
	 * type: numeric
	 * units: W
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_ond_Aux_Loss_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set ond_CompPMax: 
	 * type: string
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_ond_CompPMax_set(SAM_Pvsamv1_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set ond_CompVMax: 
	 * type: string
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_ond_CompVMax_set(SAM_Pvsamv1_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set ond_IMaxAC: 
	 * type: numeric
	 * units: A
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_ond_IMaxAC_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set ond_IMaxDC: 
	 * type: numeric
	 * units: A
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_ond_IMaxDC_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set ond_INomAC: 
	 * type: numeric
	 * units: A
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_ond_INomAC_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set ond_INomDC: 
	 * type: numeric
	 * units: A
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_ond_INomDC_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set ond_ModeAffEnum: 
	 * type: string
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_ond_ModeAffEnum_set(SAM_Pvsamv1_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set ond_ModeOper: 
	 * type: string
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_ond_ModeOper_set(SAM_Pvsamv1_Common ptr, const char* string, SAM_error* err);

	/**
	 * Set ond_NbInputs: 
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_ond_NbInputs_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set ond_NbMPPT: 
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_ond_NbMPPT_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set ond_Night_Loss: 
	 * type: numeric
	 * units: W
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_ond_Night_Loss_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set ond_PLim1: 
	 * type: numeric
	 * units: W
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_ond_PLim1_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set ond_PLimAbs: 
	 * type: numeric
	 * units: W
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_ond_PLimAbs_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set ond_PMaxDC: 
	 * type: numeric
	 * units: W
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_ond_PMaxDC_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set ond_PMaxOUT: 
	 * type: numeric
	 * units: W
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_ond_PMaxOUT_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set ond_PNomConv: 
	 * type: numeric
	 * units: W
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_ond_PNomConv_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set ond_PNomDC: 
	 * type: numeric
	 * units: W
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_ond_PNomDC_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set ond_PSeuil: 
	 * type: numeric
	 * units: W
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_ond_PSeuil_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set ond_TPLim1: 
	 * type: numeric
	 * units: °C
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_ond_TPLim1_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set ond_TPLimAbs: 
	 * type: numeric
	 * units: °C
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_ond_TPLimAbs_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set ond_TPMax: 
	 * type: numeric
	 * units: °C
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_ond_TPMax_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set ond_TPNom: 
	 * type: numeric
	 * units: °C
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_ond_TPNom_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set ond_VAbsMax: 
	 * type: numeric
	 * units: V
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_ond_VAbsMax_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set ond_VMPPMax: 
	 * type: numeric
	 * units: V
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_ond_VMPPMax_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set ond_VMppMin: 
	 * type: numeric
	 * units: V
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_ond_VMppMin_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set ond_VNomEff: 
	 * type: array
	 * units: V
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_ond_VNomEff_set(SAM_Pvsamv1_Common ptr, float* array, int length, SAM_error* err);

	/**
	 * Set ond_VOutConv: 
	 * type: numeric
	 * units: W
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_ond_VOutConv_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set ond_doAllowOverpower: 
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_ond_doAllowOverpower_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set ond_doUseTemperatureLimit: 
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_ond_doUseTemperatureLimit_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set ond_effCurve_Pac: 
	 * type: matrix
	 * units: W
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_ond_effCurve_Pac_set(SAM_Pvsamv1_Common ptr, float* matrix, int nr, int nc, SAM_error* err);

	/**
	 * Set ond_effCurve_Pdc: 
	 * type: matrix
	 * units: W
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_ond_effCurve_Pdc_set(SAM_Pvsamv1_Common ptr, float* matrix, int nr, int nc, SAM_error* err);

	/**
	 * Set ond_effCurve_elements: 
	 * type: numeric
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_ond_effCurve_elements_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set ond_effCurve_eta: 
	 * type: matrix
	 * units: -
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_ond_effCurve_eta_set(SAM_Pvsamv1_Common ptr, float* matrix, int nr, int nc, SAM_error* err);

	/**
	 * Set ond_lossRAc: 
	 * type: numeric
	 * units: A
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_ond_lossRAc_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set ond_lossRDc: 
	 * type: numeric
	 * units: V/A
	 * options: None
	 * constraints: None
	 * required if: inverter_model=4
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_ond_lossRDc_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set ppa_price_input: PPA Price Input
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_meter_position=1&batt_dispatch_choice=2
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_ppa_price_input_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set solar_resource_data: Weather data
	 * type: table
	 * units: None
	 * options: lat,lon,tz,elev,year,month,hour,minute,gh,dn,df,poa,tdry,twet,tdew,rhum,pres,snow,alb,aod,wspd,wdir
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_solar_resource_data_set(SAM_Pvsamv1_Common ptr, var_table vt, SAM_error* err);

	/**
	 * Set subarray1_shading:azal: Sub-array 1 Azimuth x altitude beam shading losses
	 * type: matrix
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_subarray1_shading:azal_set(SAM_Pvsamv1_Common ptr, float* matrix, int nr, int nc, SAM_error* err);

	/**
	 * Set subarray1_shading:diff: Sub-array 1 Diffuse shading loss
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_subarray1_shading:diff_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set subarray1_shading:mxh: Sub-array 1 Month x Hour beam shading losses
	 * type: matrix
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_subarray1_shading:mxh_set(SAM_Pvsamv1_Common ptr, float* matrix, int nr, int nc, SAM_error* err);

	/**
	 * Set subarray1_shading:string_option: Sub-array 1 shading string option
	 * type: numeric
	 * units: None
	 * options: 0=shadingdb,1=shadingdb_notc,2=average,3=maximum,4=minimum
	 * constraints: INTEGER,MIN=-1,MAX=4
	 * required if: ?=-1
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_subarray1_shading:string_option_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set subarray1_shading:timestep: Sub-array 1 timestep beam shading losses
	 * type: matrix
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_subarray1_shading:timestep_set(SAM_Pvsamv1_Common ptr, float* matrix, int nr, int nc, SAM_error* err);

	/**
	 * Set subarray2_shading:azal: Sub-array 2 Azimuth x altitude beam shading losses
	 * type: matrix
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_subarray2_shading:azal_set(SAM_Pvsamv1_Common ptr, float* matrix, int nr, int nc, SAM_error* err);

	/**
	 * Set subarray2_shading:diff: Sub-array 2 Diffuse shading loss
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_subarray2_shading:diff_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set subarray2_shading:mxh: Sub-array 2 Month x Hour beam shading losses
	 * type: matrix
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_subarray2_shading:mxh_set(SAM_Pvsamv1_Common ptr, float* matrix, int nr, int nc, SAM_error* err);

	/**
	 * Set subarray2_shading:string_option: Sub-array 2 Shading string option
	 * type: numeric
	 * units: None
	 * options: 0=shadingdb,1=shadingdb_notc,2=average,3=maximum,4=minimum
	 * constraints: INTEGER,MIN=-1,MAX=4
	 * required if: ?=-1
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_subarray2_shading:string_option_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set subarray2_shading:timestep: Sub-array 2 Timestep beam shading losses
	 * type: matrix
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_subarray2_shading:timestep_set(SAM_Pvsamv1_Common ptr, float* matrix, int nr, int nc, SAM_error* err);

	/**
	 * Set subarray3_shading:azal: Sub-array 3 Azimuth x altitude beam shading losses
	 * type: matrix
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_subarray3_shading:azal_set(SAM_Pvsamv1_Common ptr, float* matrix, int nr, int nc, SAM_error* err);

	/**
	 * Set subarray3_shading:diff: Sub-array 3 Diffuse shading loss
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_subarray3_shading:diff_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set subarray3_shading:mxh: Sub-array 3 Month x Hour beam shading losses
	 * type: matrix
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_subarray3_shading:mxh_set(SAM_Pvsamv1_Common ptr, float* matrix, int nr, int nc, SAM_error* err);

	/**
	 * Set subarray3_shading:string_option: Sub-array 3 Shading string option
	 * type: numeric
	 * units: None
	 * options: 0=shadingdb,1=shadingdb_notc,2=average,3=maximum,4=minimum
	 * constraints: INTEGER,MIN=-1,MAX=4
	 * required if: ?=-1
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_subarray3_shading:string_option_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set subarray3_shading:timestep: Sub-array 3 Timestep beam shading losses
	 * type: matrix
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_subarray3_shading:timestep_set(SAM_Pvsamv1_Common ptr, float* matrix, int nr, int nc, SAM_error* err);

	/**
	 * Set subarray4_shading:azal: Sub-array 4 Azimuth x altitude beam shading losses
	 * type: matrix
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_subarray4_shading:azal_set(SAM_Pvsamv1_Common ptr, float* matrix, int nr, int nc, SAM_error* err);

	/**
	 * Set subarray4_shading:diff: Sub-array 4 Diffuse shading loss
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_subarray4_shading:diff_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set subarray4_shading:mxh: Sub-array 4 Month x Hour beam shading losses
	 * type: matrix
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_subarray4_shading:mxh_set(SAM_Pvsamv1_Common ptr, float* matrix, int nr, int nc, SAM_error* err);

	/**
	 * Set subarray4_shading:string_option: Sub-array 4 Shading string option
	 * type: numeric
	 * units: None
	 * options: 0=shadingdb,1=shadingdb_notc,2=average,3=maximum,4=minimum
	 * constraints: INTEGER,MIN=-1,MAX=4
	 * required if: ?=-1
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_subarray4_shading:string_option_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set subarray4_shading:timestep: Sub-array 4 Timestep beam shading losses
	 * type: matrix
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_subarray4_shading:timestep_set(SAM_Pvsamv1_Common ptr, float* matrix, int nr, int nc, SAM_error* err);

	/**
	 * Set system_use_lifetime_output: PV lifetime simulation
	 * type: numeric
	 * units: 0/1
	 * options: None
	 * constraints: INTEGER,MIN=0,MAX=1
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_system_use_lifetime_output_set(SAM_Pvsamv1_Common ptr, float number, SAM_error* err);

	/**
	 * Set ur_ec_sched_weekday: Energy charge weekday schedule
	 * type: matrix
	 * units: None
	 * options: 12 x 24 matrix
	 * constraints: None
	 * required if: en_batt=1&batt_meter_position=1&batt_dispatch_choice=2
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_ur_ec_sched_weekday_set(SAM_Pvsamv1_Common ptr, float* matrix, int nr, int nc, SAM_error* err);

	/**
	 * Set ur_ec_sched_weekend: Energy charge weekend schedule
	 * type: matrix
	 * units: None
	 * options: 12 x 24 matrix
	 * constraints: None
	 * required if: en_batt=1&batt_meter_position=1&batt_dispatch_choice=2
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_ur_ec_sched_weekend_set(SAM_Pvsamv1_Common ptr, float* matrix, int nr, int nc, SAM_error* err);

	/**
	 * Set ur_ec_tou_mat: Energy rates table
	 * type: matrix
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: en_batt=1&batt_meter_position=1&batt_dispatch_choice=2
	 */
	SAM_EXPORT void SAM_Pvsamv1_Common_ur_ec_tou_mat_set(SAM_Pvsamv1_Common ptr, float* matrix, int nr, int nc, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_Pvsamv1_Common_LeadAcid_q10_computed_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_LeadAcid_q20_computed_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_LeadAcid_qn_computed_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_LeadAcid_tn_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Pvsamv1_Common_ac_lifetime_losses_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_adjust:constant_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Pvsamv1_Common_adjust:hourly_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Pvsamv1_Common_adjust:periods_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_analysis_period_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_batt_C_rate_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_batt_Cp_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_batt_Qexp_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_batt_Qfull_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_batt_Qfull_flow_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_batt_Qnom_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_batt_Vexp_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_batt_Vfull_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_batt_Vnom_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_batt_Vnom_default_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_batt_ac_dc_efficiency_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_batt_ac_or_dc_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_batt_auto_gridcharge_max_daily_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_batt_calendar_a_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_batt_calendar_b_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_batt_calendar_c_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_batt_calendar_choice_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Pvsamv1_Common_batt_calendar_lifetime_matrix_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_batt_calendar_q0_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_batt_chem_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_batt_computed_bank_capacity_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_batt_computed_series_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_batt_computed_strings_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_batt_current_charge_max_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_batt_current_choice_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_batt_current_discharge_max_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Pvsamv1_Common_batt_custom_dispatch_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_batt_cycle_cost_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_batt_cycle_cost_choice_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_batt_dc_ac_efficiency_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_batt_dc_dc_efficiency_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_batt_dispatch_auto_can_charge_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_batt_dispatch_auto_can_clipcharge_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_batt_dispatch_auto_can_fuelcellcharge_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_batt_dispatch_auto_can_gridcharge_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_batt_dispatch_choice_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_batt_dispatch_update_frequency_hours_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_batt_h_to_ambient_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_batt_height_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_batt_initial_SOC_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_batt_length_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Pvsamv1_Common_batt_lifetime_matrix_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_batt_look_ahead_hours_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_batt_loss_choice_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Pvsamv1_Common_batt_losses_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Pvsamv1_Common_batt_losses_charging_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Pvsamv1_Common_batt_losses_discharging_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Pvsamv1_Common_batt_losses_idle_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_batt_mass_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_batt_maximum_SOC_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_batt_meter_position_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_batt_minimum_SOC_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_batt_minimum_modetime_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_batt_power_charge_max_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_batt_power_discharge_max_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Pvsamv1_Common_batt_pv_clipping_forecast_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Pvsamv1_Common_batt_pv_dc_forecast_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_batt_replacement_capacity_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_batt_replacement_option_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Pvsamv1_Common_batt_replacement_schedule_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_batt_resistance_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Pvsamv1_Common_batt_room_temperature_celsius_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_batt_target_choice_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Pvsamv1_Common_batt_target_power_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Pvsamv1_Common_batt_target_power_monthly_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_batt_voltage_choice_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Pvsamv1_Common_batt_voltage_matrix_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_batt_width_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Pvsamv1_Common_cap_vs_temp_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_dc_adjust:constant_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Pvsamv1_Common_dc_adjust:hourly_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Pvsamv1_Common_dc_adjust:periods_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Pvsamv1_Common_dc_degradation_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Pvsamv1_Common_dc_lifetime_losses_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Pvsamv1_Common_dispatch_manual_charge_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Pvsamv1_Common_dispatch_manual_discharge_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Pvsamv1_Common_dispatch_manual_fuelcellcharge_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Pvsamv1_Common_dispatch_manual_gridcharge_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Pvsamv1_Common_dispatch_manual_percent_discharge_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Pvsamv1_Common_dispatch_manual_percent_gridcharge_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Pvsamv1_Common_dispatch_manual_sched_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Pvsamv1_Common_dispatch_manual_sched_weekend_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Pvsamv1_Common_dispatch_sched_weekday_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Pvsamv1_Common_dispatch_sched_weekend_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Pvsamv1_Common_dispatch_tod_factors_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_en_ac_lifetime_losses_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_en_batt_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_en_dc_lifetime_losses_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_en_electricity_rates_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Pvsamv1_Common_fuelcell_power_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Pvsamv1_Common_load_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_mlm_AM_c_lp0_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_mlm_AM_c_lp1_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_mlm_AM_c_lp2_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_mlm_AM_c_lp3_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_mlm_AM_c_lp4_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_mlm_AM_c_lp5_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_mlm_AM_c_sa0_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_mlm_AM_c_sa1_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_mlm_AM_c_sa2_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_mlm_AM_c_sa3_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_mlm_AM_c_sa4_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_mlm_AM_mode_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_mlm_D2MuTau_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_mlm_E_g_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_mlm_IAM_c_as_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Pvsamv1_Common_mlm_IAM_c_cs_iamValue_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Pvsamv1_Common_mlm_IAM_c_cs_incAngle_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_mlm_IAM_c_sa0_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_mlm_IAM_c_sa1_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_mlm_IAM_c_sa2_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_mlm_IAM_c_sa3_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_mlm_IAM_c_sa4_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_mlm_IAM_c_sa5_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_mlm_IAM_mode_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_mlm_I_mp_ref_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_mlm_I_sc_ref_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_mlm_Length_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_mlm_N_diodes_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_mlm_N_parallel_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_mlm_N_series_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_mlm_R_s_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_mlm_R_sh0_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_mlm_R_shexp_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_mlm_R_shref_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_mlm_S_ref_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_mlm_T_c_fa_U0_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_mlm_T_c_fa_U1_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_mlm_T_c_fa_alpha_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_mlm_T_c_no_mounting_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_mlm_T_c_no_standoff_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_mlm_T_c_no_tnoct_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_mlm_T_mode_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_mlm_T_ref_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_mlm_V_mp_ref_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_mlm_V_oc_ref_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_mlm_Width_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_mlm_alpha_isc_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_mlm_beta_voc_spec_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_mlm_groundRelfectionFraction_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_mlm_mu_n_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_mlm_n_0_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Pvsamv1_Common_om_replacement_cost1_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_ond_Aux_Loss_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Pvsamv1_Common_ond_CompPMax_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Pvsamv1_Common_ond_CompVMax_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_ond_IMaxAC_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_ond_IMaxDC_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_ond_INomAC_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_ond_INomDC_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Pvsamv1_Common_ond_ModeAffEnum_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_Pvsamv1_Common_ond_ModeOper_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_ond_NbInputs_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_ond_NbMPPT_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_ond_Night_Loss_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_ond_PLim1_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_ond_PLimAbs_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_ond_PMaxDC_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_ond_PMaxOUT_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_ond_PNomConv_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_ond_PNomDC_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_ond_PSeuil_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_ond_TPLim1_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_ond_TPLimAbs_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_ond_TPMax_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_ond_TPNom_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_ond_VAbsMax_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_ond_VMPPMax_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_ond_VMppMin_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Pvsamv1_Common_ond_VNomEff_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_ond_VOutConv_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_ond_doAllowOverpower_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_ond_doUseTemperatureLimit_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Pvsamv1_Common_ond_effCurve_Pac_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Pvsamv1_Common_ond_effCurve_Pdc_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_ond_effCurve_elements_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Pvsamv1_Common_ond_effCurve_eta_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_ond_lossRAc_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_ond_lossRDc_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_ppa_price_input_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT var_table SAM_Pvsamv1_Common_solar_resource_data_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Pvsamv1_Common_subarray1_shading:azal_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_subarray1_shading:diff_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Pvsamv1_Common_subarray1_shading:mxh_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_subarray1_shading:string_option_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Pvsamv1_Common_subarray1_shading:timestep_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Pvsamv1_Common_subarray2_shading:azal_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_subarray2_shading:diff_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Pvsamv1_Common_subarray2_shading:mxh_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_subarray2_shading:string_option_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Pvsamv1_Common_subarray2_shading:timestep_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Pvsamv1_Common_subarray3_shading:azal_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_subarray3_shading:diff_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Pvsamv1_Common_subarray3_shading:mxh_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_subarray3_shading:string_option_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Pvsamv1_Common_subarray3_shading:timestep_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Pvsamv1_Common_subarray4_shading:azal_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_subarray4_shading:diff_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Pvsamv1_Common_subarray4_shading:mxh_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_subarray4_shading:string_option_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Pvsamv1_Common_subarray4_shading:timestep_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float SAM_Pvsamv1_Common_system_use_lifetime_output_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Pvsamv1_Common_ur_ec_sched_weekday_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Pvsamv1_Common_ur_ec_sched_weekend_get(SAM_Pvsamv1_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Pvsamv1_Common_ur_ec_tou_mat_get(SAM_Pvsamv1_Common ptr, SAM_error* err);



#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif