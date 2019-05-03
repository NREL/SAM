#ifndef SAM_TCSTROUGHEMPIRICAL_FUNCTIONS_H_
#define SAM_TCSTROUGHEMPIRICAL_FUNCTIONS_H_

#include "TcstroughEmpirical-data.h"

#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	/** 
	 * Create a SolarField variable table for a EmpiricalTroughNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_TcstroughEmpirical_SolarField SAM_TcstroughEmpirical_SolarField_create(const char* def, SAM_error* err);


	/**
	 * Set Row_Distance: Distance between Rows of SCAs
	 * type: numeric
	 * units: m
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_SolarField_Row_Distance_set(SAM_TcstroughEmpirical_SolarField ptr, float number, SAM_error* err);

	/**
	 * Set SfInTempD: Solar Field Design Inlet Temperature
	 * type: numeric
	 * units: C
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_SolarField_SfInTempD_set(SAM_TcstroughEmpirical_SolarField ptr, float number, SAM_error* err);

	/**
	 * Set SfOutTempD: Solar Field Design Outlet Temperature
	 * type: numeric
	 * units: C
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_SolarField_SfOutTempD_set(SAM_TcstroughEmpirical_SolarField ptr, float number, SAM_error* err);

	/**
	 * Set SfPipeHl1: Solar field piping heat loss at reduced temp. - linear term
	 * type: numeric
	 * units: C^(-1)
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_SolarField_SfPipeHl1_set(SAM_TcstroughEmpirical_SolarField ptr, float number, SAM_error* err);

	/**
	 * Set SfPipeHl2: Solar field piping heat loss at reduced temp. - quadratic term
	 * type: numeric
	 * units: C^(-2)
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_SolarField_SfPipeHl2_set(SAM_TcstroughEmpirical_SolarField ptr, float number, SAM_error* err);

	/**
	 * Set SfPipeHl3: Solar field piping heat loss at reduced temp. - cubic term
	 * type: numeric
	 * units: C^(-3)
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_SolarField_SfPipeHl3_set(SAM_TcstroughEmpirical_SolarField ptr, float number, SAM_error* err);

	/**
	 * Set SfPipeHl300: Solar field piping heat loss at design
	 * type: numeric
	 * units: W/m2
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_SolarField_SfPipeHl300_set(SAM_TcstroughEmpirical_SolarField ptr, float number, SAM_error* err);

	/**
	 * Set ui_field_htf_type: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_SolarField_ui_field_htf_type_set(SAM_TcstroughEmpirical_SolarField ptr, const char* string, SAM_error* err);

	/**
	 * Set ui_field_layout_option: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_SolarField_ui_field_layout_option_set(SAM_TcstroughEmpirical_SolarField ptr, const char* string, SAM_error* err);

	/**
	 * Set ui_solar_field_area: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_SolarField_ui_solar_field_area_set(SAM_TcstroughEmpirical_SolarField ptr, const char* string, SAM_error* err);

	/**
	 * Set ui_solar_multiple: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_SolarField_ui_solar_multiple_set(SAM_TcstroughEmpirical_SolarField ptr, const char* string, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_TcstroughEmpirical_SolarField_Row_Distance_get(SAM_TcstroughEmpirical_SolarField ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcstroughEmpirical_SolarField_SfInTempD_get(SAM_TcstroughEmpirical_SolarField ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcstroughEmpirical_SolarField_SfOutTempD_get(SAM_TcstroughEmpirical_SolarField ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcstroughEmpirical_SolarField_SfPipeHl1_get(SAM_TcstroughEmpirical_SolarField ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcstroughEmpirical_SolarField_SfPipeHl2_get(SAM_TcstroughEmpirical_SolarField ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcstroughEmpirical_SolarField_SfPipeHl3_get(SAM_TcstroughEmpirical_SolarField ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcstroughEmpirical_SolarField_SfPipeHl300_get(SAM_TcstroughEmpirical_SolarField ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcstroughEmpirical_SolarField_ui_field_htf_type_get(SAM_TcstroughEmpirical_SolarField ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcstroughEmpirical_SolarField_ui_field_layout_option_get(SAM_TcstroughEmpirical_SolarField ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcstroughEmpirical_SolarField_ui_solar_field_area_get(SAM_TcstroughEmpirical_SolarField ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcstroughEmpirical_SolarField_ui_solar_multiple_get(SAM_TcstroughEmpirical_SolarField ptr, SAM_error* err);



	/** 
	 * Create a Collectors(SCAs) variable table for a EmpiricalTroughNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_TcstroughEmpirical_Collectors(SCAs) SAM_TcstroughEmpirical_Collectors(SCAs)_create(const char* def, SAM_error* err);


	/**
	 * Set lib_ConcFac: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Collectors(SCAs)_lib_ConcFac_set(SAM_TcstroughEmpirical_Collectors(SCAs) ptr, const char* string, SAM_error* err);

	/**
	 * Set lib_GeoAcc: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Collectors(SCAs)_lib_GeoAcc_set(SAM_TcstroughEmpirical_Collectors(SCAs) ptr, const char* string, SAM_error* err);

	/**
	 * Set lib_MirCln: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Collectors(SCAs)_lib_MirCln_set(SAM_TcstroughEmpirical_Collectors(SCAs) ptr, const char* string, SAM_error* err);

	/**
	 * Set lib_MirRef: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Collectors(SCAs)_lib_MirRef_set(SAM_TcstroughEmpirical_Collectors(SCAs) ptr, const char* string, SAM_error* err);

	/**
	 * Set lib_SCA_aper: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Collectors(SCAs)_lib_SCA_aper_set(SAM_TcstroughEmpirical_Collectors(SCAs) ptr, const char* string, SAM_error* err);

	/**
	 * Set lib_TrkTwstErr: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Collectors(SCAs)_lib_TrkTwstErr_set(SAM_TcstroughEmpirical_Collectors(SCAs) ptr, const char* string, SAM_error* err);

	/**
	 * Set ui_HCEdust: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Collectors(SCAs)_ui_HCEdust_set(SAM_TcstroughEmpirical_Collectors(SCAs) ptr, const char* string, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT const char* SAM_TcstroughEmpirical_Collectors(SCAs)_lib_ConcFac_get(SAM_TcstroughEmpirical_Collectors(SCAs) ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcstroughEmpirical_Collectors(SCAs)_lib_GeoAcc_get(SAM_TcstroughEmpirical_Collectors(SCAs) ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcstroughEmpirical_Collectors(SCAs)_lib_MirCln_get(SAM_TcstroughEmpirical_Collectors(SCAs) ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcstroughEmpirical_Collectors(SCAs)_lib_MirRef_get(SAM_TcstroughEmpirical_Collectors(SCAs) ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcstroughEmpirical_Collectors(SCAs)_lib_SCA_aper_get(SAM_TcstroughEmpirical_Collectors(SCAs) ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcstroughEmpirical_Collectors(SCAs)_lib_TrkTwstErr_get(SAM_TcstroughEmpirical_Collectors(SCAs) ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcstroughEmpirical_Collectors(SCAs)_ui_HCEdust_get(SAM_TcstroughEmpirical_Collectors(SCAs) ptr, SAM_error* err);



	/** 
	 * Create a Receivers(HCEs) variable table for a EmpiricalTroughNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_TcstroughEmpirical_Receivers(HCEs) SAM_TcstroughEmpirical_Receivers(HCEs)_create(const char* def, SAM_error* err);


	/**
	 * Set HCEBelShad_1: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Receivers(HCEs)_HCEBelShad_1_set(SAM_TcstroughEmpirical_Receivers(HCEs) ptr, const char* string, SAM_error* err);

	/**
	 * Set HCEEnvTrans_1: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Receivers(HCEs)_HCEEnvTrans_1_set(SAM_TcstroughEmpirical_Receivers(HCEs) ptr, const char* string, SAM_error* err);

	/**
	 * Set HCEabs_1: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Receivers(HCEs)_HCEabs_1_set(SAM_TcstroughEmpirical_Receivers(HCEs) ptr, const char* string, SAM_error* err);

	/**
	 * Set HCEmisc_1: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Receivers(HCEs)_HCEmisc_1_set(SAM_TcstroughEmpirical_Receivers(HCEs) ptr, const char* string, SAM_error* err);

	/**
	 * Set ui_hce_broken_glass_1: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Receivers(HCEs)_ui_hce_broken_glass_1_set(SAM_TcstroughEmpirical_Receivers(HCEs) ptr, const char* string, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT const char* SAM_TcstroughEmpirical_Receivers(HCEs)_HCEBelShad_1_get(SAM_TcstroughEmpirical_Receivers(HCEs) ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcstroughEmpirical_Receivers(HCEs)_HCEEnvTrans_1_get(SAM_TcstroughEmpirical_Receivers(HCEs) ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcstroughEmpirical_Receivers(HCEs)_HCEabs_1_get(SAM_TcstroughEmpirical_Receivers(HCEs) ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcstroughEmpirical_Receivers(HCEs)_HCEmisc_1_get(SAM_TcstroughEmpirical_Receivers(HCEs) ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcstroughEmpirical_Receivers(HCEs)_ui_hce_broken_glass_1_get(SAM_TcstroughEmpirical_Receivers(HCEs) ptr, SAM_error* err);



	/** 
	 * Create a PowerBlock variable table for a EmpiricalTroughNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_TcstroughEmpirical_PowerBlock SAM_TcstroughEmpirical_PowerBlock_create(const char* def, SAM_error* err);


	/**
	 * Set TurbOutG: Label
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_PowerBlock_TurbOutG_set(SAM_TcstroughEmpirical_PowerBlock ptr, float number, SAM_error* err);

	/**
	 * Set lib_E2TPLF0: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_PowerBlock_lib_E2TPLF0_set(SAM_TcstroughEmpirical_PowerBlock ptr, const char* string, SAM_error* err);

	/**
	 * Set lib_E2TPLF1: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_PowerBlock_lib_E2TPLF1_set(SAM_TcstroughEmpirical_PowerBlock ptr, const char* string, SAM_error* err);

	/**
	 * Set lib_E2TPLF2: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_PowerBlock_lib_E2TPLF2_set(SAM_TcstroughEmpirical_PowerBlock ptr, const char* string, SAM_error* err);

	/**
	 * Set lib_E2TPLF3: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_PowerBlock_lib_E2TPLF3_set(SAM_TcstroughEmpirical_PowerBlock ptr, const char* string, SAM_error* err);

	/**
	 * Set lib_E2TPLF4: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_PowerBlock_lib_E2TPLF4_set(SAM_TcstroughEmpirical_PowerBlock ptr, const char* string, SAM_error* err);

	/**
	 * Set lib_MaxGrOut: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_PowerBlock_lib_MaxGrOut_set(SAM_TcstroughEmpirical_PowerBlock ptr, const char* string, SAM_error* err);

	/**
	 * Set lib_MinGrOut: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_PowerBlock_lib_MinGrOut_set(SAM_TcstroughEmpirical_PowerBlock ptr, const char* string, SAM_error* err);

	/**
	 * Set lib_TurbEffG: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_PowerBlock_lib_TurbEffG_set(SAM_TcstroughEmpirical_PowerBlock ptr, const char* string, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_TcstroughEmpirical_PowerBlock_TurbOutG_get(SAM_TcstroughEmpirical_PowerBlock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcstroughEmpirical_PowerBlock_lib_E2TPLF0_get(SAM_TcstroughEmpirical_PowerBlock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcstroughEmpirical_PowerBlock_lib_E2TPLF1_get(SAM_TcstroughEmpirical_PowerBlock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcstroughEmpirical_PowerBlock_lib_E2TPLF2_get(SAM_TcstroughEmpirical_PowerBlock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcstroughEmpirical_PowerBlock_lib_E2TPLF3_get(SAM_TcstroughEmpirical_PowerBlock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcstroughEmpirical_PowerBlock_lib_E2TPLF4_get(SAM_TcstroughEmpirical_PowerBlock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcstroughEmpirical_PowerBlock_lib_MaxGrOut_get(SAM_TcstroughEmpirical_PowerBlock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcstroughEmpirical_PowerBlock_lib_MinGrOut_get(SAM_TcstroughEmpirical_PowerBlock ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcstroughEmpirical_PowerBlock_lib_TurbEffG_get(SAM_TcstroughEmpirical_PowerBlock ptr, SAM_error* err);



	/** 
	 * Create a ThermalStorage variable table for a EmpiricalTroughNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_TcstroughEmpirical_ThermalStorage SAM_TcstroughEmpirical_ThermalStorage_create(const char* def, SAM_error* err);


	/**
	 * Set TSHOURS: Label
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_ThermalStorage_TSHOURS_set(SAM_TcstroughEmpirical_ThermalStorage ptr, float number, SAM_error* err);

	/**
	 * Set TurTesEffAdj: Label
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_ThermalStorage_TurTesEffAdj_set(SAM_TcstroughEmpirical_ThermalStorage ptr, float number, SAM_error* err);

	/**
	 * Set TurTesOutAdj: Label
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_ThermalStorage_TurTesOutAdj_set(SAM_TcstroughEmpirical_ThermalStorage ptr, float number, SAM_error* err);

	/**
	 * Set ui_tes_htf_type: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_ThermalStorage_ui_tes_htf_type_set(SAM_TcstroughEmpirical_ThermalStorage ptr, const char* string, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_TcstroughEmpirical_ThermalStorage_TSHOURS_get(SAM_TcstroughEmpirical_ThermalStorage ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcstroughEmpirical_ThermalStorage_TurTesEffAdj_get(SAM_TcstroughEmpirical_ThermalStorage ptr, SAM_error* err);

	SAM_EXPORT float SAM_TcstroughEmpirical_ThermalStorage_TurTesOutAdj_get(SAM_TcstroughEmpirical_ThermalStorage ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcstroughEmpirical_ThermalStorage_ui_tes_htf_type_get(SAM_TcstroughEmpirical_ThermalStorage ptr, SAM_error* err);



	/** 
	 * Create a Parasitics variable table for a EmpiricalTroughNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_TcstroughEmpirical_Parasitics SAM_TcstroughEmpirical_Parasitics_create(const char* def, SAM_error* err);


	/**
	 * Set lib_BOPParPF: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Parasitics_lib_BOPParPF_set(SAM_TcstroughEmpirical_Parasitics ptr, const char* string, SAM_error* err);

	/**
	 * Set lib_ChtfParPF: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Parasitics_lib_ChtfParPF_set(SAM_TcstroughEmpirical_Parasitics ptr, const char* string, SAM_error* err);

	/**
	 * Set lib_CtParPF: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Parasitics_lib_CtParPF_set(SAM_TcstroughEmpirical_Parasitics ptr, const char* string, SAM_error* err);

	/**
	 * Set lib_HhtfParPF: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Parasitics_lib_HhtfParPF_set(SAM_TcstroughEmpirical_Parasitics ptr, const char* string, SAM_error* err);

	/**
	 * Set lib_HtrParPF: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Parasitics_lib_HtrParPF_set(SAM_TcstroughEmpirical_Parasitics ptr, const char* string, SAM_error* err);

	/**
	 * Set lib_SfParPF: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Parasitics_lib_SfParPF_set(SAM_TcstroughEmpirical_Parasitics ptr, const char* string, SAM_error* err);

	/**
	 * Set ui_par_antifreeze_const: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Parasitics_ui_par_antifreeze_const_set(SAM_TcstroughEmpirical_Parasitics ptr, const char* string, SAM_error* err);

	/**
	 * Set ui_par_bop_const: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Parasitics_ui_par_bop_const_set(SAM_TcstroughEmpirical_Parasitics ptr, const char* string, SAM_error* err);

	/**
	 * Set ui_par_ct0_const: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Parasitics_ui_par_ct0_const_set(SAM_TcstroughEmpirical_Parasitics ptr, const char* string, SAM_error* err);

	/**
	 * Set ui_par_fixedblock_const: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Parasitics_ui_par_fixedblock_const_set(SAM_TcstroughEmpirical_Parasitics ptr, const char* string, SAM_error* err);

	/**
	 * Set ui_par_hb_const: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Parasitics_ui_par_hb_const_set(SAM_TcstroughEmpirical_Parasitics ptr, const char* string, SAM_error* err);

	/**
	 * Set ui_par_htfpump_const: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Parasitics_ui_par_htfpump_const_set(SAM_TcstroughEmpirical_Parasitics ptr, const char* string, SAM_error* err);

	/**
	 * Set ui_par_sf_const: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Parasitics_ui_par_sf_const_set(SAM_TcstroughEmpirical_Parasitics ptr, const char* string, SAM_error* err);

	/**
	 * Set ui_par_tes_const: local weather file path
	 * type: string
	 * units: None
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Parasitics_ui_par_tes_const_set(SAM_TcstroughEmpirical_Parasitics ptr, const char* string, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT const char* SAM_TcstroughEmpirical_Parasitics_lib_BOPParPF_get(SAM_TcstroughEmpirical_Parasitics ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcstroughEmpirical_Parasitics_lib_ChtfParPF_get(SAM_TcstroughEmpirical_Parasitics ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcstroughEmpirical_Parasitics_lib_CtParPF_get(SAM_TcstroughEmpirical_Parasitics ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcstroughEmpirical_Parasitics_lib_HhtfParPF_get(SAM_TcstroughEmpirical_Parasitics ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcstroughEmpirical_Parasitics_lib_HtrParPF_get(SAM_TcstroughEmpirical_Parasitics ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcstroughEmpirical_Parasitics_lib_SfParPF_get(SAM_TcstroughEmpirical_Parasitics ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcstroughEmpirical_Parasitics_ui_par_antifreeze_const_get(SAM_TcstroughEmpirical_Parasitics ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcstroughEmpirical_Parasitics_ui_par_bop_const_get(SAM_TcstroughEmpirical_Parasitics ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcstroughEmpirical_Parasitics_ui_par_ct0_const_get(SAM_TcstroughEmpirical_Parasitics ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcstroughEmpirical_Parasitics_ui_par_fixedblock_const_get(SAM_TcstroughEmpirical_Parasitics ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcstroughEmpirical_Parasitics_ui_par_hb_const_get(SAM_TcstroughEmpirical_Parasitics ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcstroughEmpirical_Parasitics_ui_par_htfpump_const_get(SAM_TcstroughEmpirical_Parasitics ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcstroughEmpirical_Parasitics_ui_par_sf_const_get(SAM_TcstroughEmpirical_Parasitics ptr, SAM_error* err);

	SAM_EXPORT const char* SAM_TcstroughEmpirical_Parasitics_ui_par_tes_const_get(SAM_TcstroughEmpirical_Parasitics ptr, SAM_error* err);



	/** 
	 * Create a Common variable table for a EmpiricalTroughNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_TcstroughEmpirical_Common SAM_TcstroughEmpirical_Common_create(const char* def, SAM_error* err);


	/**
	 * Set adjust:constant: Constant loss adjustment
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: MAX=100
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Common_adjust:constant_set(SAM_TcstroughEmpirical_Common ptr, float number, SAM_error* err);

	/**
	 * Set adjust:hourly: Hourly loss adjustments
	 * type: array
	 * units: %
	 * options: None
	 * constraints: LENGTH=8760
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Common_adjust:hourly_set(SAM_TcstroughEmpirical_Common ptr, float* array, int length, SAM_error* err);

	/**
	 * Set adjust:periods: Period-based loss adjustments
	 * type: matrix
	 * units: %
	 * options: n x 3 matrix [ start, end, loss ]
	 * constraints: COLS=3
	 * required if: None
	 */
	SAM_EXPORT void SAM_TcstroughEmpirical_Common_adjust:periods_set(SAM_TcstroughEmpirical_Common ptr, float* matrix, int nr, int nc, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_TcstroughEmpirical_Common_adjust:constant_get(SAM_TcstroughEmpirical_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_TcstroughEmpirical_Common_adjust:hourly_get(SAM_TcstroughEmpirical_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_TcstroughEmpirical_Common_adjust:periods_get(SAM_TcstroughEmpirical_Common ptr, SAM_error* err);



#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif