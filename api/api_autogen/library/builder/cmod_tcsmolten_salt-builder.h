#ifndef _CMOD_TCSMOLTEN_SALT_BUILDER_H_
#define _CMOD_TCSMOLTEN_SALT_BUILDER_H_

#include "vartab.h"


//
// Function recomp_frac, HTR_UA_calc, P_comp_in, LTR_UA_calc, P_comp_out, T_htf_cold for a  module
// @param *vt: a var_table* that contains: htf, htf_props, T_htf_hot_des, dT_PHX_hot_approach, T_amb_des, dT_mc_approach, site_elevation, W_dot_net_des, design_method, eta_thermal_des, UA_recup_tot_des, cycle_config, is_recomp_ok, is_P_high_fixed, is_PR_fixed, des_objective, min_phx_deltaT, rel_tol, eta_isen_mc, eta_isen_rc, eta_isen_pc, eta_isen_t, LT_recup_eff_max, HT_recup_eff_max, deltaP_counterHX_frac, P_high_limit, dT_PHX_cold_approach, is_design_air_cooler, fan_power_frac, deltaP_cooler_frac, is_generate_udpc, is_apply_default_htf_mins, T_htf_hot_low, T_htf_hot_high, n_T_htf_hot, T_amb_low, T_amb_high, n_T_amb, m_dot_htf_ND_low, m_dot_htf_ND_high, n_m_dot_htf_ND
// @param[in,out] *cxt: a invoke_t* that for storing the results
// @returns single value or var_table
//
var_table TcsmoltenSalt_SupercriticalCarbonDioxidePowerCycle_BtnSco2DesAndUdpc_func(var_table* vt, invoke_t* cxt)


//
// Function landbase for a  module
// @param *vt: a var_table* that contains: positions
// @param[in,out] *cxt: a invoke_t* that for storing the results
// @returns single value or var_table
//


//
// Function land_area_base, x, y for a  module
// @param *vt: a var_table* that contains: helio_positions
i, landbase
// @param[in,out] *cxt: a invoke_t* that for storing the results
// @returns single value or var_table
//
var_table TcsmoltenSalt_TowerSolarPilotSolarField_TowerSolarPilotSolarField_func(var_table* vt, invoke_t* cxt)


//
// Evaluates is_wlim_series for a MSPT System Control module
// @param *vt: a var_table* that contains: is_dispatch
// @returns single value or var_table
//
float TcsmoltenSalt_is_wlim_series_eval(var_table* vt);

//
// Evaluates construction_financing_cost for a Financial Construction Financing module
// @param *vt: a var_table* that contains: const_per_total1, const_per_total2, const_per_total3, const_per_total4, const_per_total5
// @returns single value or var_table
//
float TcsmoltenSalt_construction_financing_cost_eval(var_table* vt);

//
// Function P_in_calc, recomp_frac_calc, P_out_calc, sco2ud_T_htf_cold_calc for a Supercritical Carbon Dioxide Power Cycle module
// @param *vt: a var_table* that contains: P_comp_in, P_comp_out, T_htf_cold, recomp_frac
// @param[in,out] *cxt: a invoke_t* that for storing the results
// @returns single value or var_table
//
var_table TcsmoltenSalt_SupercriticalCarbonDioxidePowerCycle_BtnSco2DesAndUdpc_func(var_table* vt, invoke_t* cxt)


//
// Evaluates A_sf_UI for a Tower SolarPilot Solar Field module
// @param *vt: a var_table* that contains: helio_width, helio_height, dens_mirror, n_hel
// @returns single value or var_table
//
float TcsmoltenSalt_A_sf_UI_eval(var_table* vt);

//
// Evaluates csp.pt.cost.receiver.area for a Tower SolarPilot Capital Costs module
// @param *vt: a var_table* that contains: receiver_type, rec_height, D_rec, rec_d_spec, csp.pt.rec.cav_ap_height, d_rec
// @returns single value or var_table
//
float TcsmoltenSalt_csp.pt.cost.receiver.area_eval(var_table* vt);

//
// Evaluates system_capacity for a Molten Salt Tower Power Block Common module
// @param *vt: a var_table* that contains: nameplate
// @returns single value or var_table
//
float TcsmoltenSalt_system_capacity_eval(var_table* vt);

//
// Evaluates q_pb_design for a MSPT System Design module
// @param *vt: a var_table* that contains: P_ref, design_eff
// @returns single value or var_table
//
float TcsmoltenSalt_q_pb_design_eval(var_table* vt);

//
// Function weekday_schedule, weekend_schedule for a MSPT Dispatch Control module
// @param *vt: a var_table* that contains: dispatch_sched_weekend, dispatch_sched_weekday
// @param[in,out] *cxt: a invoke_t* that for storing the results
// @returns single value or var_table
//
var_table TcsmoltenSalt_MSPTDispatchControl_CopySchedule_func(var_table* vt, invoke_t* cxt)


//
// Function htm_str for a  module
// @param *vt: a var_table* that contains: c_atm_0, solar_resource_file, helio_width, helio_height, helio_optical_error, helio_active_fraction, dens_mirror, helio_reflectance, rec_absorptance, rec_height, rec_aspect, rec_hl_perm2, q_design, dni_des, land_max, land_min, h_tower, c_atm_0, c_atm_1, c_atm_2, c_atm_3, n_facet_x, n_facet_y, focus_type, cant_type, n_flux_days, delta_flux_hrs, calc_fluxmaps, n_flux_x, n_flux_y, check_max_flux, tower_fixed_cost, tower_exp, rec_ref_cost, rec_ref_area, rec_cost_exp, site_spec_cost, heliostat_spec_cost, land_spec_cost, contingency_rate, sales_tax_rate, sales_tax_frac, cost_sf_fixed, is_optimize, flux_max, opt_init_step, opt_max_iter, opt_conv_tol, opt_algorithm, opt_flux_penalty, helio_positions_in, rec_ref_area, c_atm_1, cant_type, 0.000000, rec_hl_perm2, opt_init_step, plant_spec_cost, helio_optical_error_mrad*0.001000, cost_sf_fixed, site_spec_cost, rec_aspect, c_atm_3, bop_spec_cost, Q_rec_des, rec_absorptance, heliostat_spec_cost, rec_cost_exp, tower_exp, 0.250000, opt_conv_tol, solar_resource_file, h_tower, rec_ref_cost, sales_tax_rate, contingency_rate, sales_tax_frac, to_int()( is_optimize ), helio_reflectance, n_facet_y, land_max, H_rec0, land_min, c_atm_2, tower_fixed_cost, opt_max_iter, flux_max, rec_height, D_rec0, n_facet_x, 1.000000, helio_height, focus_type, THT0, np0, helio_active_fraction, land_spec_cost, helio_width, tes_spec_cost, dni_des, dens_mirror
// @param[in,out] *cxt: a invoke_t* that for storing the results
// @returns single value or var_table
//


//
// Function ret for a  module
// @param *vt: a var_table* that contains: hp, obj, positions
// @param[in,out] *cxt: a invoke_t* that for storing the results
// @returns single value or var_table
//


//
// Evaluates const_per_principal1, const_per_interest1, const_per_total1, const_per_principal2, const_per_interest2, const_per_total2, const_per_principal3, const_per_interest3, const_per_total3, const_per_principal4, const_per_interest4, const_per_total4, const_per_principal5, const_per_interest5, const_per_total5 for a Financial Construction Financing module
// @param *vt: a var_table* that contains: total_installed_cost, const_per_interest_rate1, const_per_months1, const_per_percent1, const_per_upfront_rate1, const_per_interest_rate2, const_per_months2, const_per_percent2, const_per_upfront_rate2, const_per_interest_rate3, const_per_months3, const_per_percent3, const_per_upfront_rate3, const_per_interest_rate4, const_per_months4, const_per_percent4, const_per_upfront_rate4, const_per_interest_rate5, const_per_months5, const_per_percent5, const_per_upfront_rate5
// @returns single value or var_table
//
var_table TcsmoltenSalt_const_per_principal1_MIMO_eval(var_table* vt);

//
// Evaluates rec_aspect for a MSPT Receiver module
// @param *vt: a var_table* that contains: D_rec, rec_height
// @returns single value or var_table
//
float TcsmoltenSalt_rec_aspect_eval(var_table* vt);

//
// Evaluates csp.pt.cost.site_improvements, csp.pt.cost.heliostats, csp.pt.cost.tower, csp.pt.cost.receiver, csp.pt.cost.storage, csp.pt.cost.power_block, csp.pt.cost.bop, csp.pt.cost.fossil, ui_direct_subtotal, csp.pt.cost.contingency, total_direct_cost, csp.pt.cost.epc.total, csp.pt.cost.plm.total, csp.pt.cost.sales_tax.total, total_indirect_cost, total_installed_cost, csp.pt.cost.installed_per_capacity for a Tower SolarPilot Capital Costs module
// @param *vt: a var_table* that contains: csp.pt.cost.heliostats_m2, site_spec_cost, heliostat_spec_cost, cost_sf_fixed, ui_tower_height, ui_receiver_height, ui_heliostat_height, tower_fixed_cost, tower_exp, csp.pt.cost.receiver.area, rec_ref_cost, rec_ref_area, rec_cost_exp, csp.pt.cost.storage_mwht, tes_spec_cost, csp.pt.cost.power_block_mwe, plant_spec_cost, bop_spec_cost, fossil_spec_cost, contingency_rate, csp.pt.cost.total_land_area, csp.pt.cost.nameplate, csp.pt.cost.epc.per_acre, csp.pt.cost.epc.percent, csp.pt.cost.epc.per_watt, csp.pt.cost.epc.fixed, land_spec_cost, csp.pt.cost.plm.percent, csp.pt.cost.plm.per_watt, csp.pt.cost.plm.fixed, sales_tax_frac, csp.pt.cost.sales_tax.value
// @returns single value or var_table
//
var_table TcsmoltenSalt_csp.pt.cost.site_improvements_MIMO_eval(var_table* vt);

#endif