#ifndef _CMOD_PVSAMV1_BUILDER_H_
#define _CMOD_PVSAMV1_BUILDER_H_

#include "vartab.h"


//
// Function code, sd11par_alphaIsc, sd11par_gammaPmp, sd11par_betaVoc, sd11par_n, sd11par_Il, sd11par_Egref, sd11par_Io, sd11par_c1, sd11par_c2, sd11par_c3, sd11par_d2, sd11par_d1, sd11par_d3 for a IEC61853 Single Diode Model module
// @param *vt: a var_table* that contains: input, nser, type, verbose, S
// @param[in,out] *cxt: a invoke_t* that for storing the results
// @returns single value or var_table
//
var_table Pvsamv1_IEC61853SingleDiodeModel_BtnCalcIec65153Par_func(var_table* vt, invoke_t* cxt = 0)


//
// Function inv_cec_cg_c0, inv_cec_cg_vdco, inv_cec_cg_pdco, inv_cec_cg_c3, inv_cec_cg_psco, inv_cec_cg_c1, inv_cec_cg_c2 for a Inverter CEC Coefficient Generator module
// @param *vt: a var_table* that contains: Vdco, Pdco, c1, Pso, c0, c2, c3
// @param[in,out] *cxt: a invoke_t* that for storing the results
// @returns single value or var_table
//
var_table Pvsamv1_InverterCECCoefficientGenerator_InverterCECCoefficientGenerator_func(var_table* vt, invoke_t* cxt = 0)


//
// Evaluates inv_snl_eff_cec, inv_snl_eff_euro for a Inverter CEC Database module
// @param *vt: a var_table* that contains: inv_snl_vdco, inv_snl_pdco, inv_snl_pso, inv_snl_paco, inv_snl_c0, inv_snl_c1, inv_snl_c2, inv_snl_c3
// @returns single value or var_table
//
var_table Pvsamv1_inv_snl_eff_cec_MIMO_eval(var_table* vt);

//
// Evaluates cec_p_mp_ref for a CEC Performance Model with Module Database module
// @param *vt: a var_table* that contains: cec_i_mp_ref, cec_v_mp_ref
// @returns single value or var_table
//
float Pvsamv1_cec_p_mp_ref_eval(var_table* vt);

//
// Evaluates inv_pd_pdco for a Inverter Part Load Curve module
// @param *vt: a var_table* that contains: inv_pd_paco, inv_pd_eff
// @returns single value or var_table
//
float Pvsamv1_inv_pd_pdco_eval(var_table* vt);

//
// Evaluates 6par_pmp for a CEC Performance Model with User Entered Specifications module
// @param *vt: a var_table* that contains: 6par_vmp, 6par_imp
// @returns single value or var_table
//
float Pvsamv1_6par_pmp_eval(var_table* vt);

//
// Evaluates total_modules for a PV System Design module
// @param *vt: a var_table* that contains: subarray1_modules_per_string, subarray1_nstrings, subarray2_modules_per_string, subarray2_nstrings, subarray2_enable, subarray3_modules_per_string, subarray3_nstrings, subarray3_enable, subarray4_modules_per_string, subarray4_nstrings, subarray4_enable
// @returns single value or var_table
//
float Pvsamv1_total_modules_eval(var_table* vt);

//
// Evaluates subarray2_enable, subarray3_enable, subarray4_enable, subarray1_modules_per_string, subarray1_nstrings, inverter_count for a PV System Design module
// @param *vt: a var_table* that contains: enable_auto_size, module_model, spe_vmp, cec_v_mp_ref, 6par_vmp, snl_ref_vmp, sd11par_Vmp0, spe_voc, cec_v_oc_ref, 6par_voc, snl_ref_voc, sd11par_Voc0, spe_power, cec_p_mp_ref, 6par_pmp, snl_ref_pmp, sd11par_Pmp0, inverter_model, inv_snl_mppt_low, inv_ds_mppt_low, inv_pd_mppt_low, inv_cec_cg_mppt_low, inv_snl_vdcmax, inv_ds_vdcmax, inv_pd_vdcmax, inv_cec_cg_vdcmax, inv_snl_mppt_hi, inv_ds_mppt_hi, inv_pd_mppt_hi, inv_cec_cg_mppt_hi, inv_snl_paco, inv_ds_paco, inv_pd_paco, inv_cec_cg_paco, en_batt, batt_ac_or_dc, batt_max_power, desired_size, desired_dcac_ratio
// @returns single value or var_table
//
var_table Pvsamv1_subarray2_enable_MIMO_eval(var_table* vt);

//
// Evaluates spe_power for a Simple Efficiency Module Model module
// @param *vt: a var_table* that contains: spe_reference, spe_eff0, spe_rad0, spe_eff1, spe_rad1, spe_eff2, spe_rad2, spe_eff3, spe_rad3, spe_eff4, spe_rad4, spe_area
// @returns single value or var_table
//
float Pvsamv1_spe_power_eval(var_table* vt);

//
// Function Pdco, c1, c3, c2, Vdco, Pso, c0, result for a  module
// @param *vt: a var_table* that contains: inv_cec_cg_paco
inv_cec_cg_test_samples, inv_cec_cg_paco, inv_cec_cg_sample_power_units, inv_cec_cg_test_samples, inv_cec_cg_sample_power_units, obj
// @param[in,out] *cxt: a invoke_t* that for storing the results
// @returns single value or var_table
//
var_table Pvsamv1_InverterCECCoefficientGenerator_InverterCECCoefficientGenerator_func(var_table* vt, invoke_t* cxt = 0)


//
// Evaluates inv_cec_cg_eff_cec, inv_cec_cg_eff_euro for a Inverter CEC Coefficient Generator module
// @param *vt: a var_table* that contains: inv_cec_cg_vdco, inv_cec_cg_pdco, inv_cec_cg_psco, inv_cec_cg_paco, inv_cec_cg_c0, inv_cec_cg_c1, inv_cec_cg_c2, inv_cec_cg_c3
// @returns single value or var_table
//
var_table Pvsamv1_inv_cec_cg_eff_cec_MIMO_eval(var_table* vt);

//
// Evaluates snl_ref_a, snl_ref_b, snl_ref_dT, snl_ref_isc, snl_ref_isc_temp_0, snl_ref_isc_temp_1, snl_ref_imp, snl_ref_imp_temp_0, snl_imp_temp_1, snl_ref_vmp, snl_ref_vmp_temp_0, snl_ref_vmp_temp_1, snl_ref_pmp, snl_ref_pmp_temp_0, snl_ref_pmp_temp_1, snl_ref_voc, snl_ref_voc_temp_0, snl_voc_temp_1, snl_ref_eff for a Sandia PV Array Performance Model with Module Database module
// @param *vt: a var_table* that contains: snl_module_structure, snl_a, snl_b, snl_dtc, snl_specified_a, snl_specified_b, snl_specified_dT, snl_fd, snl_a0, snl_a1, snl_a2, snl_a3, snl_a4, snl_b0, snl_b1, snl_b2, snl_b3, snl_b4, snl_b5, snl_isco, snl_aisc, snl_c0, snl_c1, snl_aimp, snl_impo, snl_bvmpo, snl_mbvmp, snl_n, snl_c3, snl_series_cells, snl_c2, snl_vmpo, snl_bvoco, snl_mbvoc, snl_voco, snl_area
// @returns single value or var_table
//
var_table Pvsamv1_snl_ref_a_MIMO_eval(var_table* vt);

#endif