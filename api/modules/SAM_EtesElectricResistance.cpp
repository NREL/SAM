#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_EtesElectricResistance.h"

SAM_EXPORT int SAM_EtesElectricResistance_execute(SAM_table data, int verbosity, SAM_error* err){
	return SAM_module_exec("etes_electric_resistance", data, verbosity, err);
}

SAM_EXPORT void SAM_EtesElectricResistance_SolarResource_solar_resource_file_sset(SAM_table ptr, const char* str, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_string(ptr, "solar_resource_file", str);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_SystemControl_bop_par_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "bop_par", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_SystemControl_bop_par_0_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "bop_par_0", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_SystemControl_bop_par_1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "bop_par_1", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_SystemControl_bop_par_2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "bop_par_2", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_SystemControl_bop_par_f_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "bop_par_f", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_SystemControl_disp_csu_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_csu_cost", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_SystemControl_disp_down_time_min_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_down_time_min", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_SystemControl_disp_frequency_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_frequency", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_SystemControl_disp_horizon_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_horizon", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_SystemControl_disp_hsu_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_hsu_cost", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_SystemControl_disp_max_iter_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_max_iter", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_SystemControl_disp_mip_gap_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_mip_gap", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_SystemControl_disp_pen_delta_w_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_pen_delta_w", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_SystemControl_disp_reporting_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_reporting", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_SystemControl_disp_spec_bb_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_spec_bb", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_SystemControl_disp_spec_presolve_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_spec_presolve", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_SystemControl_disp_spec_scaling_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_spec_scaling", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_SystemControl_disp_steps_per_hour_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_steps_per_hour", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_SystemControl_disp_time_weighting_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_time_weighting", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_SystemControl_disp_timeout_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_timeout", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_SystemControl_disp_up_time_min_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_up_time_min", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_SystemControl_is_dispatch_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "is_dispatch", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_SystemControl_pb_fixed_par_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pb_fixed_par", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_SystemControl_sim_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sim_type", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_SystemControl_time_start_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "time_start", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_SystemControl_time_steps_per_hour_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "time_steps_per_hour", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_SystemControl_time_stop_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "time_stop", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_SystemControl_vacuum_arrays_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "vacuum_arrays", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_FinancialModel_etes_financial_model_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "etes_financial_model", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_SystemDesign_P_ref_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "P_ref", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_SystemDesign_T_htf_cold_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_htf_cold_des", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_SystemDesign_T_htf_hot_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_htf_hot_des", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_SystemDesign_design_eff_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "design_eff", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_SystemDesign_gross_net_conversion_factor_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "gross_net_conversion_factor", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_SystemDesign_heater_mult_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "heater_mult", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_SystemDesign_tshours_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tshours", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_PowerCycle_cycle_cutoff_frac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cycle_cutoff_frac", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_PowerCycle_cycle_max_frac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cycle_max_frac", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_PowerCycle_pb_pump_coef_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pb_pump_coef", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_PowerCycle_pc_config_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pc_config", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_PowerCycle_q_sby_frac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "q_sby_frac", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_PowerCycle_startup_frac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "startup_frac", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_PowerCycle_startup_time_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "startup_time", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_RankineCycle_CT_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "CT", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_RankineCycle_P_boil_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "P_boil", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_RankineCycle_P_cond_min_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "P_cond_min", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_RankineCycle_P_cond_ratio_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "P_cond_ratio", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_RankineCycle_T_ITD_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_ITD_des", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_RankineCycle_T_amb_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_amb_des", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_RankineCycle_T_approach_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_approach", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_RankineCycle_dT_cw_ref_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dT_cw_ref", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_RankineCycle_n_pl_inc_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "n_pl_inc", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_RankineCycle_pb_bd_frac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pb_bd_frac", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_RankineCycle_tech_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tech_type", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_UserDefinedPowerCycle_ud_f_W_dot_cool_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ud_f_W_dot_cool_des", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_UserDefinedPowerCycle_ud_ind_od_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "ud_ind_od", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_UserDefinedPowerCycle_ud_m_dot_water_cool_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ud_m_dot_water_cool_des", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_ThermalStorage_cold_tank_Thtr_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cold_tank_Thtr", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_ThermalStorage_cold_tank_max_heat_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cold_tank_max_heat", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_ThermalStorage_h_tank_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "h_tank", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_ThermalStorage_h_tank_min_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "h_tank_min", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_ThermalStorage_hot_htf_code_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hot_htf_code", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_ThermalStorage_hot_tank_Thtr_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hot_tank_Thtr", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_ThermalStorage_hot_tank_max_heat_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hot_tank_max_heat", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_ThermalStorage_tank_pairs_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tank_pairs", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_ThermalStorage_tes_init_hot_htf_percent_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tes_init_hot_htf_percent", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_ThermalStorage_u_tank_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "u_tank", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_ThermalStorage_ud_hot_htf_props_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "ud_hot_htf_props", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_Heater_f_q_dot_des_allowable_su_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "f_q_dot_des_allowable_su", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_Heater_f_q_dot_heater_min_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "f_q_dot_heater_min", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_Heater_hrs_startup_at_max_rate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hrs_startup_at_max_rate", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_TimeOfDeliveryFactors_dispatch_factor1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dispatch_factor1", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_TimeOfDeliveryFactors_dispatch_factor2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dispatch_factor2", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_TimeOfDeliveryFactors_dispatch_factor3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dispatch_factor3", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_TimeOfDeliveryFactors_dispatch_factor4_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dispatch_factor4", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_TimeOfDeliveryFactors_dispatch_factor5_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dispatch_factor5", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_TimeOfDeliveryFactors_dispatch_factor6_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dispatch_factor6", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_TimeOfDeliveryFactors_dispatch_factor7_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dispatch_factor7", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_TimeOfDeliveryFactors_dispatch_factor8_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dispatch_factor8", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_TimeOfDeliveryFactors_dispatch_factor9_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dispatch_factor9", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_TimeOfDeliveryFactors_dispatch_factors_ts_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "dispatch_factors_ts", arr, length);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_TimeOfDeliveryFactors_dispatch_sched_weekday_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "dispatch_sched_weekday", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_TimeOfDeliveryFactors_dispatch_sched_weekend_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "dispatch_sched_weekend", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_TimeOfDeliveryFactors_ppa_multiplier_model_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ppa_multiplier_model", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_Revenue_mp_energy_market_revenue_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "mp_energy_market_revenue", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_Revenue_ppa_price_input_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "ppa_price_input", arr, length);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_SystemCost_cycle_spec_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cycle_spec_cost", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_SystemCosts_bop_spec_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "bop_spec_cost", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_SystemCosts_contingency_rate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "contingency_rate", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_SystemCosts_epc_cost_fixed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "epc_cost_fixed", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_SystemCosts_epc_cost_per_watt_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "epc_cost_per_watt", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_SystemCosts_epc_cost_perc_of_direct_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "epc_cost_perc_of_direct", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_SystemCosts_heater_spec_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "heater_spec_cost", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_SystemCosts_land_cost_fixed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "land_cost_fixed", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_SystemCosts_land_cost_per_watt_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "land_cost_per_watt", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_SystemCosts_land_cost_perc_of_direct_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "land_cost_perc_of_direct", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_SystemCosts_sales_tax_frac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sales_tax_frac", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_SystemCosts_tes_spec_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tes_spec_cost", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_FinancialParameters_const_per_interest_rate1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_interest_rate1", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_FinancialParameters_const_per_interest_rate2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_interest_rate2", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_FinancialParameters_const_per_interest_rate3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_interest_rate3", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_FinancialParameters_const_per_interest_rate4_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_interest_rate4", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_FinancialParameters_const_per_interest_rate5_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_interest_rate5", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_FinancialParameters_const_per_months1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_months1", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_FinancialParameters_const_per_months2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_months2", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_FinancialParameters_const_per_months3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_months3", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_FinancialParameters_const_per_months4_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_months4", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_FinancialParameters_const_per_months5_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_months5", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_FinancialParameters_const_per_percent1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_percent1", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_FinancialParameters_const_per_percent2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_percent2", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_FinancialParameters_const_per_percent3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_percent3", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_FinancialParameters_const_per_percent4_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_percent4", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_FinancialParameters_const_per_percent5_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_percent5", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_FinancialParameters_const_per_upfront_rate1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_upfront_rate1", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_FinancialParameters_const_per_upfront_rate2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_upfront_rate2", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_FinancialParameters_const_per_upfront_rate3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_upfront_rate3", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_FinancialParameters_const_per_upfront_rate4_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_upfront_rate4", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_FinancialParameters_const_per_upfront_rate5_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_upfront_rate5", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_FinancialParameters_sales_tax_rate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sales_tax_rate", number);
	});
}

SAM_EXPORT const char* SAM_EtesElectricResistance_SolarResource_solar_resource_file_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "solar_resource_file");
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "solar_resource_file");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_SystemControl_bop_par_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "bop_par", &result))
		make_access_error("SAM_EtesElectricResistance", "bop_par");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_SystemControl_bop_par_0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "bop_par_0", &result))
		make_access_error("SAM_EtesElectricResistance", "bop_par_0");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_SystemControl_bop_par_1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "bop_par_1", &result))
		make_access_error("SAM_EtesElectricResistance", "bop_par_1");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_SystemControl_bop_par_2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "bop_par_2", &result))
		make_access_error("SAM_EtesElectricResistance", "bop_par_2");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_SystemControl_bop_par_f_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "bop_par_f", &result))
		make_access_error("SAM_EtesElectricResistance", "bop_par_f");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_SystemControl_disp_csu_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_csu_cost", &result))
		make_access_error("SAM_EtesElectricResistance", "disp_csu_cost");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_SystemControl_disp_down_time_min_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_down_time_min", &result))
		make_access_error("SAM_EtesElectricResistance", "disp_down_time_min");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_SystemControl_disp_frequency_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_frequency", &result))
		make_access_error("SAM_EtesElectricResistance", "disp_frequency");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_SystemControl_disp_horizon_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_horizon", &result))
		make_access_error("SAM_EtesElectricResistance", "disp_horizon");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_SystemControl_disp_hsu_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_hsu_cost", &result))
		make_access_error("SAM_EtesElectricResistance", "disp_hsu_cost");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_SystemControl_disp_max_iter_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_max_iter", &result))
		make_access_error("SAM_EtesElectricResistance", "disp_max_iter");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_SystemControl_disp_mip_gap_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_mip_gap", &result))
		make_access_error("SAM_EtesElectricResistance", "disp_mip_gap");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_SystemControl_disp_pen_delta_w_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_pen_delta_w", &result))
		make_access_error("SAM_EtesElectricResistance", "disp_pen_delta_w");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_SystemControl_disp_reporting_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_reporting", &result))
		make_access_error("SAM_EtesElectricResistance", "disp_reporting");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_SystemControl_disp_spec_bb_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_spec_bb", &result))
		make_access_error("SAM_EtesElectricResistance", "disp_spec_bb");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_SystemControl_disp_spec_presolve_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_spec_presolve", &result))
		make_access_error("SAM_EtesElectricResistance", "disp_spec_presolve");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_SystemControl_disp_spec_scaling_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_spec_scaling", &result))
		make_access_error("SAM_EtesElectricResistance", "disp_spec_scaling");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_SystemControl_disp_steps_per_hour_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_steps_per_hour", &result))
		make_access_error("SAM_EtesElectricResistance", "disp_steps_per_hour");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_SystemControl_disp_time_weighting_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_time_weighting", &result))
		make_access_error("SAM_EtesElectricResistance", "disp_time_weighting");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_SystemControl_disp_timeout_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_timeout", &result))
		make_access_error("SAM_EtesElectricResistance", "disp_timeout");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_SystemControl_disp_up_time_min_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_up_time_min", &result))
		make_access_error("SAM_EtesElectricResistance", "disp_up_time_min");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_SystemControl_is_dispatch_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "is_dispatch", &result))
		make_access_error("SAM_EtesElectricResistance", "is_dispatch");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_SystemControl_pb_fixed_par_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pb_fixed_par", &result))
		make_access_error("SAM_EtesElectricResistance", "pb_fixed_par");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_SystemControl_sim_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sim_type", &result))
		make_access_error("SAM_EtesElectricResistance", "sim_type");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_SystemControl_time_start_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "time_start", &result))
		make_access_error("SAM_EtesElectricResistance", "time_start");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_SystemControl_time_steps_per_hour_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "time_steps_per_hour", &result))
		make_access_error("SAM_EtesElectricResistance", "time_steps_per_hour");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_SystemControl_time_stop_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "time_stop", &result))
		make_access_error("SAM_EtesElectricResistance", "time_stop");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_SystemControl_vacuum_arrays_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "vacuum_arrays", &result))
		make_access_error("SAM_EtesElectricResistance", "vacuum_arrays");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_FinancialModel_etes_financial_model_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "etes_financial_model", &result))
		make_access_error("SAM_EtesElectricResistance", "etes_financial_model");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_SystemDesign_P_ref_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_ref", &result))
		make_access_error("SAM_EtesElectricResistance", "P_ref");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_SystemDesign_T_htf_cold_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_htf_cold_des", &result))
		make_access_error("SAM_EtesElectricResistance", "T_htf_cold_des");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_SystemDesign_T_htf_hot_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_htf_hot_des", &result))
		make_access_error("SAM_EtesElectricResistance", "T_htf_hot_des");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_SystemDesign_design_eff_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "design_eff", &result))
		make_access_error("SAM_EtesElectricResistance", "design_eff");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_SystemDesign_gross_net_conversion_factor_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "gross_net_conversion_factor", &result))
		make_access_error("SAM_EtesElectricResistance", "gross_net_conversion_factor");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_SystemDesign_heater_mult_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "heater_mult", &result))
		make_access_error("SAM_EtesElectricResistance", "heater_mult");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_SystemDesign_tshours_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tshours", &result))
		make_access_error("SAM_EtesElectricResistance", "tshours");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_PowerCycle_cycle_cutoff_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cycle_cutoff_frac", &result))
		make_access_error("SAM_EtesElectricResistance", "cycle_cutoff_frac");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_PowerCycle_cycle_max_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cycle_max_frac", &result))
		make_access_error("SAM_EtesElectricResistance", "cycle_max_frac");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_PowerCycle_pb_pump_coef_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pb_pump_coef", &result))
		make_access_error("SAM_EtesElectricResistance", "pb_pump_coef");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_PowerCycle_pc_config_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pc_config", &result))
		make_access_error("SAM_EtesElectricResistance", "pc_config");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_PowerCycle_q_sby_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_sby_frac", &result))
		make_access_error("SAM_EtesElectricResistance", "q_sby_frac");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_PowerCycle_startup_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "startup_frac", &result))
		make_access_error("SAM_EtesElectricResistance", "startup_frac");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_PowerCycle_startup_time_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "startup_time", &result))
		make_access_error("SAM_EtesElectricResistance", "startup_time");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_RankineCycle_CT_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "CT", &result))
		make_access_error("SAM_EtesElectricResistance", "CT");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_RankineCycle_P_boil_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_boil", &result))
		make_access_error("SAM_EtesElectricResistance", "P_boil");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_RankineCycle_P_cond_min_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_cond_min", &result))
		make_access_error("SAM_EtesElectricResistance", "P_cond_min");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_RankineCycle_P_cond_ratio_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_cond_ratio", &result))
		make_access_error("SAM_EtesElectricResistance", "P_cond_ratio");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_RankineCycle_T_ITD_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_ITD_des", &result))
		make_access_error("SAM_EtesElectricResistance", "T_ITD_des");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_RankineCycle_T_amb_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_amb_des", &result))
		make_access_error("SAM_EtesElectricResistance", "T_amb_des");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_RankineCycle_T_approach_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_approach", &result))
		make_access_error("SAM_EtesElectricResistance", "T_approach");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_RankineCycle_dT_cw_ref_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dT_cw_ref", &result))
		make_access_error("SAM_EtesElectricResistance", "dT_cw_ref");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_RankineCycle_n_pl_inc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "n_pl_inc", &result))
		make_access_error("SAM_EtesElectricResistance", "n_pl_inc");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_RankineCycle_pb_bd_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pb_bd_frac", &result))
		make_access_error("SAM_EtesElectricResistance", "pb_bd_frac");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_RankineCycle_tech_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tech_type", &result))
		make_access_error("SAM_EtesElectricResistance", "tech_type");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_UserDefinedPowerCycle_ud_f_W_dot_cool_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ud_f_W_dot_cool_des", &result))
		make_access_error("SAM_EtesElectricResistance", "ud_f_W_dot_cool_des");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesElectricResistance_UserDefinedPowerCycle_ud_ind_od_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "ud_ind_od", nrows, ncols);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "ud_ind_od");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_UserDefinedPowerCycle_ud_m_dot_water_cool_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ud_m_dot_water_cool_des", &result))
		make_access_error("SAM_EtesElectricResistance", "ud_m_dot_water_cool_des");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_ThermalStorage_cold_tank_Thtr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cold_tank_Thtr", &result))
		make_access_error("SAM_EtesElectricResistance", "cold_tank_Thtr");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_ThermalStorage_cold_tank_max_heat_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cold_tank_max_heat", &result))
		make_access_error("SAM_EtesElectricResistance", "cold_tank_max_heat");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_ThermalStorage_h_tank_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "h_tank", &result))
		make_access_error("SAM_EtesElectricResistance", "h_tank");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_ThermalStorage_h_tank_min_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "h_tank_min", &result))
		make_access_error("SAM_EtesElectricResistance", "h_tank_min");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_ThermalStorage_hot_htf_code_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hot_htf_code", &result))
		make_access_error("SAM_EtesElectricResistance", "hot_htf_code");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_ThermalStorage_hot_tank_Thtr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hot_tank_Thtr", &result))
		make_access_error("SAM_EtesElectricResistance", "hot_tank_Thtr");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_ThermalStorage_hot_tank_max_heat_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hot_tank_max_heat", &result))
		make_access_error("SAM_EtesElectricResistance", "hot_tank_max_heat");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_ThermalStorage_tank_pairs_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tank_pairs", &result))
		make_access_error("SAM_EtesElectricResistance", "tank_pairs");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_ThermalStorage_tes_init_hot_htf_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_init_hot_htf_percent", &result))
		make_access_error("SAM_EtesElectricResistance", "tes_init_hot_htf_percent");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_ThermalStorage_u_tank_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "u_tank", &result))
		make_access_error("SAM_EtesElectricResistance", "u_tank");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesElectricResistance_ThermalStorage_ud_hot_htf_props_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "ud_hot_htf_props", nrows, ncols);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "ud_hot_htf_props");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_Heater_f_q_dot_des_allowable_su_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "f_q_dot_des_allowable_su", &result))
		make_access_error("SAM_EtesElectricResistance", "f_q_dot_des_allowable_su");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_Heater_f_q_dot_heater_min_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "f_q_dot_heater_min", &result))
		make_access_error("SAM_EtesElectricResistance", "f_q_dot_heater_min");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_Heater_hrs_startup_at_max_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hrs_startup_at_max_rate", &result))
		make_access_error("SAM_EtesElectricResistance", "hrs_startup_at_max_rate");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_TimeOfDeliveryFactors_dispatch_factor1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dispatch_factor1", &result))
		make_access_error("SAM_EtesElectricResistance", "dispatch_factor1");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_TimeOfDeliveryFactors_dispatch_factor2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dispatch_factor2", &result))
		make_access_error("SAM_EtesElectricResistance", "dispatch_factor2");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_TimeOfDeliveryFactors_dispatch_factor3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dispatch_factor3", &result))
		make_access_error("SAM_EtesElectricResistance", "dispatch_factor3");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_TimeOfDeliveryFactors_dispatch_factor4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dispatch_factor4", &result))
		make_access_error("SAM_EtesElectricResistance", "dispatch_factor4");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_TimeOfDeliveryFactors_dispatch_factor5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dispatch_factor5", &result))
		make_access_error("SAM_EtesElectricResistance", "dispatch_factor5");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_TimeOfDeliveryFactors_dispatch_factor6_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dispatch_factor6", &result))
		make_access_error("SAM_EtesElectricResistance", "dispatch_factor6");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_TimeOfDeliveryFactors_dispatch_factor7_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dispatch_factor7", &result))
		make_access_error("SAM_EtesElectricResistance", "dispatch_factor7");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_TimeOfDeliveryFactors_dispatch_factor8_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dispatch_factor8", &result))
		make_access_error("SAM_EtesElectricResistance", "dispatch_factor8");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_TimeOfDeliveryFactors_dispatch_factor9_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dispatch_factor9", &result))
		make_access_error("SAM_EtesElectricResistance", "dispatch_factor9");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesElectricResistance_TimeOfDeliveryFactors_dispatch_factors_ts_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dispatch_factors_ts", length);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "dispatch_factors_ts");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesElectricResistance_TimeOfDeliveryFactors_dispatch_sched_weekday_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "dispatch_sched_weekday", nrows, ncols);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "dispatch_sched_weekday");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesElectricResistance_TimeOfDeliveryFactors_dispatch_sched_weekend_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "dispatch_sched_weekend", nrows, ncols);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "dispatch_sched_weekend");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_TimeOfDeliveryFactors_ppa_multiplier_model_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ppa_multiplier_model", &result))
		make_access_error("SAM_EtesElectricResistance", "ppa_multiplier_model");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesElectricResistance_Revenue_mp_energy_market_revenue_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "mp_energy_market_revenue", nrows, ncols);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "mp_energy_market_revenue");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesElectricResistance_Revenue_ppa_price_input_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "ppa_price_input", length);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "ppa_price_input");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_SystemCost_cycle_spec_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cycle_spec_cost", &result))
		make_access_error("SAM_EtesElectricResistance", "cycle_spec_cost");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_SystemCosts_bop_spec_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "bop_spec_cost", &result))
		make_access_error("SAM_EtesElectricResistance", "bop_spec_cost");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_SystemCosts_contingency_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "contingency_rate", &result))
		make_access_error("SAM_EtesElectricResistance", "contingency_rate");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_SystemCosts_epc_cost_fixed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "epc_cost_fixed", &result))
		make_access_error("SAM_EtesElectricResistance", "epc_cost_fixed");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_SystemCosts_epc_cost_per_watt_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "epc_cost_per_watt", &result))
		make_access_error("SAM_EtesElectricResistance", "epc_cost_per_watt");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_SystemCosts_epc_cost_perc_of_direct_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "epc_cost_perc_of_direct", &result))
		make_access_error("SAM_EtesElectricResistance", "epc_cost_perc_of_direct");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_SystemCosts_heater_spec_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "heater_spec_cost", &result))
		make_access_error("SAM_EtesElectricResistance", "heater_spec_cost");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_SystemCosts_land_cost_fixed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "land_cost_fixed", &result))
		make_access_error("SAM_EtesElectricResistance", "land_cost_fixed");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_SystemCosts_land_cost_per_watt_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "land_cost_per_watt", &result))
		make_access_error("SAM_EtesElectricResistance", "land_cost_per_watt");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_SystemCosts_land_cost_perc_of_direct_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "land_cost_perc_of_direct", &result))
		make_access_error("SAM_EtesElectricResistance", "land_cost_perc_of_direct");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_SystemCosts_sales_tax_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sales_tax_frac", &result))
		make_access_error("SAM_EtesElectricResistance", "sales_tax_frac");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_SystemCosts_tes_spec_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_spec_cost", &result))
		make_access_error("SAM_EtesElectricResistance", "tes_spec_cost");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_FinancialParameters_const_per_interest_rate1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest_rate1", &result))
		make_access_error("SAM_EtesElectricResistance", "const_per_interest_rate1");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_FinancialParameters_const_per_interest_rate2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest_rate2", &result))
		make_access_error("SAM_EtesElectricResistance", "const_per_interest_rate2");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_FinancialParameters_const_per_interest_rate3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest_rate3", &result))
		make_access_error("SAM_EtesElectricResistance", "const_per_interest_rate3");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_FinancialParameters_const_per_interest_rate4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest_rate4", &result))
		make_access_error("SAM_EtesElectricResistance", "const_per_interest_rate4");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_FinancialParameters_const_per_interest_rate5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest_rate5", &result))
		make_access_error("SAM_EtesElectricResistance", "const_per_interest_rate5");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_FinancialParameters_const_per_months1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_months1", &result))
		make_access_error("SAM_EtesElectricResistance", "const_per_months1");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_FinancialParameters_const_per_months2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_months2", &result))
		make_access_error("SAM_EtesElectricResistance", "const_per_months2");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_FinancialParameters_const_per_months3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_months3", &result))
		make_access_error("SAM_EtesElectricResistance", "const_per_months3");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_FinancialParameters_const_per_months4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_months4", &result))
		make_access_error("SAM_EtesElectricResistance", "const_per_months4");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_FinancialParameters_const_per_months5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_months5", &result))
		make_access_error("SAM_EtesElectricResistance", "const_per_months5");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_FinancialParameters_const_per_percent1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_percent1", &result))
		make_access_error("SAM_EtesElectricResistance", "const_per_percent1");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_FinancialParameters_const_per_percent2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_percent2", &result))
		make_access_error("SAM_EtesElectricResistance", "const_per_percent2");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_FinancialParameters_const_per_percent3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_percent3", &result))
		make_access_error("SAM_EtesElectricResistance", "const_per_percent3");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_FinancialParameters_const_per_percent4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_percent4", &result))
		make_access_error("SAM_EtesElectricResistance", "const_per_percent4");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_FinancialParameters_const_per_percent5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_percent5", &result))
		make_access_error("SAM_EtesElectricResistance", "const_per_percent5");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_FinancialParameters_const_per_upfront_rate1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_upfront_rate1", &result))
		make_access_error("SAM_EtesElectricResistance", "const_per_upfront_rate1");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_FinancialParameters_const_per_upfront_rate2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_upfront_rate2", &result))
		make_access_error("SAM_EtesElectricResistance", "const_per_upfront_rate2");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_FinancialParameters_const_per_upfront_rate3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_upfront_rate3", &result))
		make_access_error("SAM_EtesElectricResistance", "const_per_upfront_rate3");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_FinancialParameters_const_per_upfront_rate4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_upfront_rate4", &result))
		make_access_error("SAM_EtesElectricResistance", "const_per_upfront_rate4");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_FinancialParameters_const_per_upfront_rate5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_upfront_rate5", &result))
		make_access_error("SAM_EtesElectricResistance", "const_per_upfront_rate5");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_FinancialParameters_sales_tax_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sales_tax_rate", &result))
		make_access_error("SAM_EtesElectricResistance", "sales_tax_rate");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_Outputs_E_heater_su_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "E_heater_su_des", &result))
		make_access_error("SAM_EtesElectricResistance", "E_heater_su_des");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_Outputs_Q_tes_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Q_tes_des", &result))
		make_access_error("SAM_EtesElectricResistance", "Q_tes_des");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_T_htf_cycle_in_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_htf_cycle_in", length);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "T_htf_cycle_in");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_T_htf_cycle_out_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_htf_cycle_out", length);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "T_htf_cycle_out");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_T_htf_heater_in_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_htf_heater_in", length);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "T_htf_heater_in");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_T_htf_heater_out_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_htf_heater_out", length);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "T_htf_heater_out");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_T_tes_cold_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_tes_cold", length);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "T_tes_cold");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_T_tes_hot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_tes_hot", length);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "T_tes_hot");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_Outputs_V_tes_htf_avail_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "V_tes_htf_avail", &result))
		make_access_error("SAM_EtesElectricResistance", "V_tes_htf_avail");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_Outputs_V_tes_htf_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "V_tes_htf_total", &result))
		make_access_error("SAM_EtesElectricResistance", "V_tes_htf_total");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_Outputs_W_dot_bop_design_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "W_dot_bop_design", &result))
		make_access_error("SAM_EtesElectricResistance", "W_dot_bop_design");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_W_dot_bop_parasitics_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_dot_bop_parasitics", length);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "W_dot_bop_parasitics");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_W_dot_cycle_cooling_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_dot_cycle_cooling", length);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "W_dot_cycle_cooling");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_W_dot_cycle_gross_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_dot_cycle_gross", length);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "W_dot_cycle_gross");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_W_dot_cycle_htf_pump_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_dot_cycle_htf_pump", length);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "W_dot_cycle_htf_pump");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_W_dot_cycle_net_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_dot_cycle_net", length);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "W_dot_cycle_net");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_W_dot_fixed_parasitics_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_dot_fixed_parasitics", length);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "W_dot_fixed_parasitics");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_W_dot_heater_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_dot_heater", length);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "W_dot_heater");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_W_dot_out_net_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_dot_out_net", length);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "W_dot_out_net");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_Outputs_annual_E_cycle_gross_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_E_cycle_gross", &result))
		make_access_error("SAM_EtesElectricResistance", "annual_E_cycle_gross");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_Outputs_annual_E_heater_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_E_heater", &result))
		make_access_error("SAM_EtesElectricResistance", "annual_E_heater");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_Outputs_annual_E_tes_heater_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_E_tes_heater", &result))
		make_access_error("SAM_EtesElectricResistance", "annual_E_tes_heater");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_Outputs_annual_Q_cycle_thermal_in_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_Q_cycle_thermal_in", &result))
		make_access_error("SAM_EtesElectricResistance", "annual_Q_cycle_thermal_in");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_Outputs_annual_Q_cycle_thermal_startup_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_Q_cycle_thermal_startup", &result))
		make_access_error("SAM_EtesElectricResistance", "annual_Q_cycle_thermal_startup");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_Outputs_annual_Q_heater_startup_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_Q_heater_startup", &result))
		make_access_error("SAM_EtesElectricResistance", "annual_Q_heater_startup");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_Outputs_annual_Q_heater_to_htf_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_Q_heater_to_htf", &result))
		make_access_error("SAM_EtesElectricResistance", "annual_Q_heater_to_htf");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_Outputs_annual_Q_tes_losses_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_Q_tes_losses", &result))
		make_access_error("SAM_EtesElectricResistance", "annual_Q_tes_losses");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_Outputs_annual_energy_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_energy", &result))
		make_access_error("SAM_EtesElectricResistance", "annual_energy");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_Outputs_annual_energy_full_availability_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_energy_full_availability", &result))
		make_access_error("SAM_EtesElectricResistance", "annual_energy_full_availability");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_Outputs_bop_cost_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "bop_cost_calc", &result))
		make_access_error("SAM_EtesElectricResistance", "bop_cost_calc");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_Outputs_construction_financing_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "construction_financing_cost", &result))
		make_access_error("SAM_EtesElectricResistance", "construction_financing_cost");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_Outputs_contingency_cost_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "contingency_cost_calc", &result))
		make_access_error("SAM_EtesElectricResistance", "contingency_cost_calc");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_Outputs_cp_htf_cycle_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cp_htf_cycle_des", &result))
		make_access_error("SAM_EtesElectricResistance", "cp_htf_cycle_des");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_Outputs_cycle_cost_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cycle_cost_calc", &result))
		make_access_error("SAM_EtesElectricResistance", "cycle_cost_calc");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_Outputs_d_tank_tes_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "d_tank_tes", &result))
		make_access_error("SAM_EtesElectricResistance", "d_tank_tes");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_Outputs_dens_store_htf_at_T_ave_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dens_store_htf_at_T_ave", &result))
		make_access_error("SAM_EtesElectricResistance", "dens_store_htf_at_T_ave");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_Outputs_direct_subtotal_cost_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "direct_subtotal_cost_calc", &result))
		make_access_error("SAM_EtesElectricResistance", "direct_subtotal_cost_calc");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_Outputs_disp_iter_ann_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_iter_ann", &result))
		make_access_error("SAM_EtesElectricResistance", "disp_iter_ann");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_disp_obj_relax_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_obj_relax", length);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "disp_obj_relax");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_disp_objective_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_objective", length);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "disp_objective");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_Outputs_disp_objective_ann_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_objective_ann", &result))
		make_access_error("SAM_EtesElectricResistance", "disp_objective_ann");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_disp_pceff_expected_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_pceff_expected", length);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "disp_pceff_expected");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_disp_presolve_nconstr_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_presolve_nconstr", length);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "disp_presolve_nconstr");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_Outputs_disp_presolve_nconstr_ann_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_presolve_nconstr_ann", &result))
		make_access_error("SAM_EtesElectricResistance", "disp_presolve_nconstr_ann");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_disp_presolve_nvar_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_presolve_nvar", length);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "disp_presolve_nvar");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_Outputs_disp_presolve_nvar_ann_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_presolve_nvar_ann", &result))
		make_access_error("SAM_EtesElectricResistance", "disp_presolve_nvar_ann");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_disp_qpbsu_expected_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_qpbsu_expected", length);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "disp_qpbsu_expected");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_disp_qsf_expected_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_qsf_expected", length);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "disp_qsf_expected");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_disp_qsfprod_expected_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_qsfprod_expected", length);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "disp_qsfprod_expected");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_disp_qsfsu_expected_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_qsfsu_expected", length);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "disp_qsfsu_expected");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_disp_rel_mip_gap_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_rel_mip_gap", length);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "disp_rel_mip_gap");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_disp_rev_expected_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_rev_expected", length);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "disp_rev_expected");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_disp_solve_iter_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_solve_iter", length);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "disp_solve_iter");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_disp_solve_state_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_solve_state", length);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "disp_solve_state");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_Outputs_disp_solve_state_ann_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_solve_state_ann", &result))
		make_access_error("SAM_EtesElectricResistance", "disp_solve_state_ann");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_disp_solve_time_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_solve_time", length);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "disp_solve_time");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_Outputs_disp_solve_time_ann_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_solve_time_ann", &result))
		make_access_error("SAM_EtesElectricResistance", "disp_solve_time_ann");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_disp_subopt_flag_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_subopt_flag", length);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "disp_subopt_flag");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_disp_tes_expected_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_tes_expected", length);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "disp_tes_expected");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_disp_wpb_expected_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "disp_wpb_expected", length);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "disp_wpb_expected");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_e_ch_tes_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "e_ch_tes", length);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "e_ch_tes");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_elec_purchase_price_mult_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "elec_purchase_price_mult", length);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "elec_purchase_price_mult");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_Outputs_epc_cost_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "epc_cost_calc", &result))
		make_access_error("SAM_EtesElectricResistance", "epc_cost_calc");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_eta_cycle_gross_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "eta_cycle_gross", length);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "eta_cycle_gross");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_eta_cycle_net_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "eta_cycle_net", length);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "eta_cycle_net");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_Outputs_flip_target_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "flip_target_percent", &result))
		make_access_error("SAM_EtesElectricResistance", "flip_target_percent");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_gen_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "gen", length);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "gen");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_Outputs_heater_cost_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "heater_cost_calc", &result))
		make_access_error("SAM_EtesElectricResistance", "heater_cost_calc");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_Outputs_installed_per_cap_cost_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "installed_per_cap_cost_calc", &result))
		make_access_error("SAM_EtesElectricResistance", "installed_per_cap_cost_calc");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_Outputs_land_cost_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "land_cost_calc", &result))
		make_access_error("SAM_EtesElectricResistance", "land_cost_calc");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_m_dot_balance_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_balance", length);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "m_dot_balance");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_m_dot_htf_cycle_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_htf_cycle", length);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "m_dot_htf_cycle");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_Outputs_m_dot_htf_cycle_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "m_dot_htf_cycle_des", &result))
		make_access_error("SAM_EtesElectricResistance", "m_dot_htf_cycle_des");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_m_dot_htf_heater_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_htf_heater", length);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "m_dot_htf_heater");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_m_dot_water_cycle_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_water_cycle", length);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "m_dot_water_cycle");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_mass_tes_cold_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "mass_tes_cold", length);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "mass_tes_cold");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_mass_tes_hot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "mass_tes_hot", length);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "mass_tes_hot");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_n_op_modes_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "n_op_modes", length);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "n_op_modes");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_Outputs_nameplate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "nameplate", &result))
		make_access_error("SAM_EtesElectricResistance", "nameplate");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_op_mode_1_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "op_mode_1", length);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "op_mode_1");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_op_mode_2_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "op_mode_2", length);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "op_mode_2");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_op_mode_3_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "op_mode_3", length);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "op_mode_3");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_Outputs_ppa_soln_mode_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ppa_soln_mode", &result))
		make_access_error("SAM_EtesElectricResistance", "ppa_soln_mode");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_q_balance_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_balance", length);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "q_balance");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_q_dot_ch_tes_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_ch_tes", length);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "q_dot_ch_tes");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_q_dot_cycle_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_cycle", length);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "q_dot_cycle");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_q_dot_cycle_startup_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_cycle_startup", length);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "q_dot_cycle_startup");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_q_dot_dc_tes_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_dc_tes", length);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "q_dot_dc_tes");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_Outputs_q_dot_heater_design_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_dot_heater_design", &result))
		make_access_error("SAM_EtesElectricResistance", "q_dot_heater_design");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_q_dot_heater_startup_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_heater_startup", length);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "q_dot_heater_startup");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_q_dot_heater_to_htf_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_heater_to_htf", length);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "q_dot_heater_to_htf");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_Outputs_q_dot_loss_tes_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_dot_loss_tes_des", &result))
		make_access_error("SAM_EtesElectricResistance", "q_dot_loss_tes_des");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_q_dot_tes_heater_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_tes_heater", length);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "q_dot_tes_heater");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_q_dot_tes_losses_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_tes_losses", length);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "q_dot_tes_losses");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_Outputs_q_pb_design_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_pb_design", &result))
		make_access_error("SAM_EtesElectricResistance", "q_pb_design");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_Outputs_sales_tax_cost_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sales_tax_cost_calc", &result))
		make_access_error("SAM_EtesElectricResistance", "sales_tax_cost_calc");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_Outputs_sim_cpu_run_time_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sim_cpu_run_time", &result))
		make_access_error("SAM_EtesElectricResistance", "sim_cpu_run_time");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_Outputs_system_capacity_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_capacity", &result))
		make_access_error("SAM_EtesElectricResistance", "system_capacity");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_tdry_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tdry", length);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "tdry");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_Outputs_tes_cost_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_cost_calc", &result))
		make_access_error("SAM_EtesElectricResistance", "tes_cost_calc");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_time_hr_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "time_hr", length);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "time_hr");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_Outputs_total_direct_cost_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_direct_cost_calc", &result))
		make_access_error("SAM_EtesElectricResistance", "total_direct_cost_calc");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_Outputs_total_indirect_cost_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_indirect_cost_calc", &result))
		make_access_error("SAM_EtesElectricResistance", "total_indirect_cost_calc");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_Outputs_total_installed_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_installed_cost", &result))
		make_access_error("SAM_EtesElectricResistance", "total_installed_cost");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_Outputs_total_land_area_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_land_area", &result))
		make_access_error("SAM_EtesElectricResistance", "total_land_area");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_tou_period_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tou_period", length);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "tou_period");
	});
	return result;
}



SAM_EXPORT double SAM_EtesElectricResistance_Outputs_tshours_heater_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tshours_heater", &result))
		make_access_error("SAM_EtesElectricResistance", "tshours_heater");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_twet_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "twet", length);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "twet");
	});
	return result;
}



