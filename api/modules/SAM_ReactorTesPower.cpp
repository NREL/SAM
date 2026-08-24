#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_ReactorTesPower.h"

SAM_EXPORT int SAM_ReactorTesPower_execute(SAM_table data, int verbosity, SAM_error* err){
	return SAM_module_exec("reactor_tes_power", data, verbosity, err);
}

SAM_EXPORT void SAM_ReactorTesPower_SolarResource_solar_resource_file_sset(SAM_table ptr, const char* str, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_string(ptr, "solar_resource_file", str);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_SystemControl_T_tank_cold_init_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_tank_cold_init", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_SystemControl_T_tank_hot_init_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_tank_hot_init", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_SystemControl_bop_par_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "bop_par", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_SystemControl_bop_par_0_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "bop_par_0", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_SystemControl_bop_par_1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "bop_par_1", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_SystemControl_bop_par_2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "bop_par_2", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_SystemControl_bop_par_f_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "bop_par_f", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_SystemControl_can_cycle_use_standby_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "can_cycle_use_standby", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_SystemControl_f_turb_tou_periods_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "f_turb_tou_periods", arr, length);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_SystemControl_is_dispatch_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "is_dispatch", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_SystemControl_is_dispatch_targets_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "is_dispatch_targets", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_SystemControl_is_pc_sb_allowed_in_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "is_pc_sb_allowed_in", arr, length);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_SystemControl_is_pc_su_allowed_in_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "is_pc_su_allowed_in", arr, length);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_SystemControl_is_tod_pc_target_also_pc_max_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "is_tod_pc_target_also_pc_max", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_SystemControl_pb_fixed_par_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pb_fixed_par", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_SystemControl_pc_op_mode_initial_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pc_op_mode_initial", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_SystemControl_pc_startup_energy_remain_initial_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pc_startup_energy_remain_initial", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_SystemControl_pc_startup_time_remain_init_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pc_startup_time_remain_init", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_SystemControl_q_pc_max_in_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "q_pc_max_in", arr, length);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_SystemControl_q_pc_target_on_in_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "q_pc_target_on_in", arr, length);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_SystemControl_q_pc_target_su_in_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "q_pc_target_su_in", arr, length);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_SystemControl_sim_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sim_type", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_SystemControl_time_start_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "time_start", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_SystemControl_time_steps_per_hour_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "time_steps_per_hour", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_SystemControl_time_stop_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "time_stop", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_SystemControl_timestep_load_fractions_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "timestep_load_fractions", arr, length);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_SystemControl_vacuum_arrays_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "vacuum_arrays", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_SystemControl_weekday_schedule_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "weekday_schedule", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_SystemControl_weekend_schedule_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "weekend_schedule", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_FinancialModel_reactor_financial_model_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "reactor_financial_model", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_SystemDesign_P_ref_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "P_ref", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_SystemDesign_T_htf_cold_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_htf_cold_des", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_SystemDesign_T_htf_hot_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_htf_hot_des", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_SystemDesign_design_eff_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "design_eff", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_SystemDesign_reactor_mult_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "reactor_mult", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_SystemDesign_tshours_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tshours", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_TowerAndReceiver_hot_htf_code_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hot_htf_code", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_TowerAndReceiver_ud_hot_htf_props_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "ud_hot_htf_props", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_ThermalStorage_cold_tank_Thtr_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cold_tank_Thtr", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_ThermalStorage_cold_tank_max_heat_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cold_tank_max_heat", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_ThermalStorage_h_tank_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "h_tank", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_ThermalStorage_h_tank_min_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "h_tank_min", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_ThermalStorage_hot_tank_Thtr_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hot_tank_Thtr", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_ThermalStorage_hot_tank_max_heat_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hot_tank_max_heat", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_ThermalStorage_tank_pairs_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tank_pairs", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_ThermalStorage_tanks_in_parallel_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tanks_in_parallel", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_ThermalStorage_tes_init_hot_htf_percent_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tes_init_hot_htf_percent", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_ThermalStorage_u_tank_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "u_tank", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_PowerCycle_cycle_cutoff_frac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cycle_cutoff_frac", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_PowerCycle_cycle_max_frac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cycle_max_frac", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_PowerCycle_pb_pump_coef_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pb_pump_coef", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_PowerCycle_pc_config_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pc_config", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_PowerCycle_q_sby_frac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "q_sby_frac", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_PowerCycle_startup_frac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "startup_frac", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_PowerCycle_startup_time_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "startup_time", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_RankineCycle_CT_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "CT", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_RankineCycle_P_cond_min_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "P_cond_min", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_RankineCycle_P_cond_ratio_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "P_cond_ratio", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_RankineCycle_T_ITD_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_ITD_des", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_RankineCycle_T_amb_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_amb_des", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_RankineCycle_T_approach_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_approach", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_RankineCycle_dT_cw_ref_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dT_cw_ref", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_RankineCycle_n_pl_inc_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "n_pl_inc", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_RankineCycle_pb_bd_frac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pb_bd_frac", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_RankineCycle_tech_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tech_type", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_UserDefinedPowerCycle_ud_f_W_dot_cool_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ud_f_W_dot_cool_des", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_UserDefinedPowerCycle_ud_ind_od_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "ud_ind_od", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_UserDefinedPowerCycle_ud_is_sco2_regr_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ud_is_sco2_regr", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_UserDefinedPowerCycle_ud_m_dot_water_cool_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ud_m_dot_water_cool_des", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_TimeOfDeliveryFactors_dispatch_factors_ts_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "dispatch_factors_ts", arr, length);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_TimeOfDeliveryFactors_dispatch_sched_weekday_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "dispatch_sched_weekday", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_TimeOfDeliveryFactors_dispatch_sched_weekend_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "dispatch_sched_weekend", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_TimeOfDeliveryFactors_dispatch_tod_factors_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "dispatch_tod_factors", arr, length);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_TimeOfDeliveryFactors_is_timestep_load_fractions_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "is_timestep_load_fractions", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_TimeOfDeliveryFactors_ppa_multiplier_model_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ppa_multiplier_model", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_TimeOfDeliveryFactors_start_day_of_year_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "start_day_of_year", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_FinancialSolutionMode_ppa_soln_mode_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ppa_soln_mode", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_ElectricityRates_en_electricity_rates_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "en_electricity_rates", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_Revenue_mp_energy_market_revenue_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "mp_energy_market_revenue", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_Revenue_ppa_price_input_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "ppa_price_input", arr, length);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_SystemCosts_bop_spec_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "bop_spec_cost", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_SystemCosts_contingency_rate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "contingency_rate", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_SystemCosts_cycle_spec_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cycle_spec_cost", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_SystemCosts_epc_cost_fixed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "epc_cost_fixed", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_SystemCosts_epc_cost_per_watt_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "epc_cost_per_watt", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_SystemCosts_epc_cost_perc_of_direct_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "epc_cost_perc_of_direct", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_SystemCosts_land_cost_fixed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "land_cost_fixed", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_SystemCosts_land_cost_per_watt_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "land_cost_per_watt", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_SystemCosts_land_cost_perc_of_direct_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "land_cost_perc_of_direct", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_SystemCosts_reactor_spec_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "reactor_spec_cost", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_SystemCosts_sales_tax_frac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sales_tax_frac", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_SystemCosts_tes_spec_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tes_spec_cost", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_FinancialParameters_const_per_interest_rate1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_interest_rate1", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_FinancialParameters_const_per_interest_rate2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_interest_rate2", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_FinancialParameters_const_per_interest_rate3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_interest_rate3", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_FinancialParameters_const_per_interest_rate4_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_interest_rate4", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_FinancialParameters_const_per_interest_rate5_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_interest_rate5", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_FinancialParameters_const_per_months1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_months1", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_FinancialParameters_const_per_months2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_months2", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_FinancialParameters_const_per_months3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_months3", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_FinancialParameters_const_per_months4_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_months4", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_FinancialParameters_const_per_months5_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_months5", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_FinancialParameters_const_per_percent1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_percent1", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_FinancialParameters_const_per_percent2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_percent2", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_FinancialParameters_const_per_percent3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_percent3", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_FinancialParameters_const_per_percent4_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_percent4", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_FinancialParameters_const_per_percent5_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_percent5", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_FinancialParameters_const_per_upfront_rate1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_upfront_rate1", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_FinancialParameters_const_per_upfront_rate2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_upfront_rate2", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_FinancialParameters_const_per_upfront_rate3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_upfront_rate3", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_FinancialParameters_const_per_upfront_rate4_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_upfront_rate4", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_FinancialParameters_const_per_upfront_rate5_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_upfront_rate5", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_FinancialParameters_sales_tax_rate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sales_tax_rate", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_AdjustmentFactors_adjust_constant_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "adjust_constant", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_AdjustmentFactors_adjust_en_periods_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "adjust_en_periods", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_AdjustmentFactors_adjust_en_timeindex_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "adjust_en_timeindex", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_AdjustmentFactors_adjust_periods_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "adjust_periods", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_AdjustmentFactors_adjust_timeindex_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "adjust_timeindex", arr, length);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_AdjustmentFactors_sf_adjust_constant_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sf_adjust_constant", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_AdjustmentFactors_sf_adjust_en_periods_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sf_adjust_en_periods", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_AdjustmentFactors_sf_adjust_en_timeindex_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sf_adjust_en_timeindex", number);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_AdjustmentFactors_sf_adjust_periods_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "sf_adjust_periods", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_ReactorTesPower_AdjustmentFactors_sf_adjust_timeindex_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "sf_adjust_timeindex", arr, length);
	});
}

SAM_EXPORT const char* SAM_ReactorTesPower_SolarResource_solar_resource_file_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "solar_resource_file");
	if (!result)
		make_access_error("SAM_ReactorTesPower", "solar_resource_file");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_SystemControl_T_tank_cold_init_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_tank_cold_init", &result))
		make_access_error("SAM_ReactorTesPower", "T_tank_cold_init");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_SystemControl_T_tank_hot_init_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_tank_hot_init", &result))
		make_access_error("SAM_ReactorTesPower", "T_tank_hot_init");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_SystemControl_bop_par_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "bop_par", &result))
		make_access_error("SAM_ReactorTesPower", "bop_par");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_SystemControl_bop_par_0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "bop_par_0", &result))
		make_access_error("SAM_ReactorTesPower", "bop_par_0");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_SystemControl_bop_par_1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "bop_par_1", &result))
		make_access_error("SAM_ReactorTesPower", "bop_par_1");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_SystemControl_bop_par_2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "bop_par_2", &result))
		make_access_error("SAM_ReactorTesPower", "bop_par_2");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_SystemControl_bop_par_f_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "bop_par_f", &result))
		make_access_error("SAM_ReactorTesPower", "bop_par_f");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_SystemControl_can_cycle_use_standby_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "can_cycle_use_standby", &result))
		make_access_error("SAM_ReactorTesPower", "can_cycle_use_standby");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_SystemControl_f_turb_tou_periods_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "f_turb_tou_periods", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "f_turb_tou_periods");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_SystemControl_is_dispatch_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "is_dispatch", &result))
		make_access_error("SAM_ReactorTesPower", "is_dispatch");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_SystemControl_is_dispatch_targets_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "is_dispatch_targets", &result))
		make_access_error("SAM_ReactorTesPower", "is_dispatch_targets");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_SystemControl_is_pc_sb_allowed_in_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "is_pc_sb_allowed_in", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "is_pc_sb_allowed_in");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_SystemControl_is_pc_su_allowed_in_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "is_pc_su_allowed_in", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "is_pc_su_allowed_in");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_SystemControl_is_tod_pc_target_also_pc_max_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "is_tod_pc_target_also_pc_max", &result))
		make_access_error("SAM_ReactorTesPower", "is_tod_pc_target_also_pc_max");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_SystemControl_pb_fixed_par_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pb_fixed_par", &result))
		make_access_error("SAM_ReactorTesPower", "pb_fixed_par");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_SystemControl_pc_op_mode_initial_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pc_op_mode_initial", &result))
		make_access_error("SAM_ReactorTesPower", "pc_op_mode_initial");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_SystemControl_pc_startup_energy_remain_initial_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pc_startup_energy_remain_initial", &result))
		make_access_error("SAM_ReactorTesPower", "pc_startup_energy_remain_initial");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_SystemControl_pc_startup_time_remain_init_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pc_startup_time_remain_init", &result))
		make_access_error("SAM_ReactorTesPower", "pc_startup_time_remain_init");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_SystemControl_q_pc_max_in_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_pc_max_in", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "q_pc_max_in");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_SystemControl_q_pc_target_on_in_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_pc_target_on_in", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "q_pc_target_on_in");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_SystemControl_q_pc_target_su_in_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_pc_target_su_in", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "q_pc_target_su_in");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_SystemControl_sim_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sim_type", &result))
		make_access_error("SAM_ReactorTesPower", "sim_type");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_SystemControl_time_start_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "time_start", &result))
		make_access_error("SAM_ReactorTesPower", "time_start");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_SystemControl_time_steps_per_hour_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "time_steps_per_hour", &result))
		make_access_error("SAM_ReactorTesPower", "time_steps_per_hour");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_SystemControl_time_stop_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "time_stop", &result))
		make_access_error("SAM_ReactorTesPower", "time_stop");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_SystemControl_timestep_load_fractions_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "timestep_load_fractions", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "timestep_load_fractions");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_SystemControl_vacuum_arrays_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "vacuum_arrays", &result))
		make_access_error("SAM_ReactorTesPower", "vacuum_arrays");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_SystemControl_weekday_schedule_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "weekday_schedule", nrows, ncols);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "weekday_schedule");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_SystemControl_weekend_schedule_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "weekend_schedule", nrows, ncols);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "weekend_schedule");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_FinancialModel_reactor_financial_model_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "reactor_financial_model", &result))
		make_access_error("SAM_ReactorTesPower", "reactor_financial_model");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_SystemDesign_P_ref_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_ref", &result))
		make_access_error("SAM_ReactorTesPower", "P_ref");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_SystemDesign_T_htf_cold_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_htf_cold_des", &result))
		make_access_error("SAM_ReactorTesPower", "T_htf_cold_des");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_SystemDesign_T_htf_hot_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_htf_hot_des", &result))
		make_access_error("SAM_ReactorTesPower", "T_htf_hot_des");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_SystemDesign_design_eff_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "design_eff", &result))
		make_access_error("SAM_ReactorTesPower", "design_eff");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_SystemDesign_reactor_mult_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "reactor_mult", &result))
		make_access_error("SAM_ReactorTesPower", "reactor_mult");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_SystemDesign_tshours_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tshours", &result))
		make_access_error("SAM_ReactorTesPower", "tshours");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_TowerAndReceiver_hot_htf_code_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hot_htf_code", &result))
		make_access_error("SAM_ReactorTesPower", "hot_htf_code");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_TowerAndReceiver_ud_hot_htf_props_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "ud_hot_htf_props", nrows, ncols);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "ud_hot_htf_props");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_ThermalStorage_cold_tank_Thtr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cold_tank_Thtr", &result))
		make_access_error("SAM_ReactorTesPower", "cold_tank_Thtr");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_ThermalStorage_cold_tank_max_heat_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cold_tank_max_heat", &result))
		make_access_error("SAM_ReactorTesPower", "cold_tank_max_heat");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_ThermalStorage_h_tank_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "h_tank", &result))
		make_access_error("SAM_ReactorTesPower", "h_tank");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_ThermalStorage_h_tank_min_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "h_tank_min", &result))
		make_access_error("SAM_ReactorTesPower", "h_tank_min");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_ThermalStorage_hot_tank_Thtr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hot_tank_Thtr", &result))
		make_access_error("SAM_ReactorTesPower", "hot_tank_Thtr");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_ThermalStorage_hot_tank_max_heat_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hot_tank_max_heat", &result))
		make_access_error("SAM_ReactorTesPower", "hot_tank_max_heat");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_ThermalStorage_tank_pairs_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tank_pairs", &result))
		make_access_error("SAM_ReactorTesPower", "tank_pairs");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_ThermalStorage_tanks_in_parallel_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tanks_in_parallel", &result))
		make_access_error("SAM_ReactorTesPower", "tanks_in_parallel");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_ThermalStorage_tes_init_hot_htf_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_init_hot_htf_percent", &result))
		make_access_error("SAM_ReactorTesPower", "tes_init_hot_htf_percent");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_ThermalStorage_u_tank_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "u_tank", &result))
		make_access_error("SAM_ReactorTesPower", "u_tank");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_PowerCycle_cycle_cutoff_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cycle_cutoff_frac", &result))
		make_access_error("SAM_ReactorTesPower", "cycle_cutoff_frac");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_PowerCycle_cycle_max_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cycle_max_frac", &result))
		make_access_error("SAM_ReactorTesPower", "cycle_max_frac");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_PowerCycle_pb_pump_coef_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pb_pump_coef", &result))
		make_access_error("SAM_ReactorTesPower", "pb_pump_coef");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_PowerCycle_pc_config_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pc_config", &result))
		make_access_error("SAM_ReactorTesPower", "pc_config");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_PowerCycle_q_sby_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_sby_frac", &result))
		make_access_error("SAM_ReactorTesPower", "q_sby_frac");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_PowerCycle_startup_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "startup_frac", &result))
		make_access_error("SAM_ReactorTesPower", "startup_frac");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_PowerCycle_startup_time_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "startup_time", &result))
		make_access_error("SAM_ReactorTesPower", "startup_time");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_RankineCycle_CT_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "CT", &result))
		make_access_error("SAM_ReactorTesPower", "CT");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_RankineCycle_P_cond_min_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_cond_min", &result))
		make_access_error("SAM_ReactorTesPower", "P_cond_min");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_RankineCycle_P_cond_ratio_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_cond_ratio", &result))
		make_access_error("SAM_ReactorTesPower", "P_cond_ratio");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_RankineCycle_T_ITD_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_ITD_des", &result))
		make_access_error("SAM_ReactorTesPower", "T_ITD_des");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_RankineCycle_T_amb_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_amb_des", &result))
		make_access_error("SAM_ReactorTesPower", "T_amb_des");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_RankineCycle_T_approach_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_approach", &result))
		make_access_error("SAM_ReactorTesPower", "T_approach");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_RankineCycle_dT_cw_ref_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dT_cw_ref", &result))
		make_access_error("SAM_ReactorTesPower", "dT_cw_ref");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_RankineCycle_n_pl_inc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "n_pl_inc", &result))
		make_access_error("SAM_ReactorTesPower", "n_pl_inc");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_RankineCycle_pb_bd_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pb_bd_frac", &result))
		make_access_error("SAM_ReactorTesPower", "pb_bd_frac");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_RankineCycle_tech_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tech_type", &result))
		make_access_error("SAM_ReactorTesPower", "tech_type");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_UserDefinedPowerCycle_ud_f_W_dot_cool_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ud_f_W_dot_cool_des", &result))
		make_access_error("SAM_ReactorTesPower", "ud_f_W_dot_cool_des");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_UserDefinedPowerCycle_ud_ind_od_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "ud_ind_od", nrows, ncols);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "ud_ind_od");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_UserDefinedPowerCycle_ud_is_sco2_regr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ud_is_sco2_regr", &result))
		make_access_error("SAM_ReactorTesPower", "ud_is_sco2_regr");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_UserDefinedPowerCycle_ud_m_dot_water_cool_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ud_m_dot_water_cool_des", &result))
		make_access_error("SAM_ReactorTesPower", "ud_m_dot_water_cool_des");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_TimeOfDeliveryFactors_dispatch_factors_ts_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dispatch_factors_ts", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "dispatch_factors_ts");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_TimeOfDeliveryFactors_dispatch_sched_weekday_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "dispatch_sched_weekday", nrows, ncols);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "dispatch_sched_weekday");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_TimeOfDeliveryFactors_dispatch_sched_weekend_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "dispatch_sched_weekend", nrows, ncols);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "dispatch_sched_weekend");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_TimeOfDeliveryFactors_dispatch_tod_factors_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dispatch_tod_factors", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "dispatch_tod_factors");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_TimeOfDeliveryFactors_is_timestep_load_fractions_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "is_timestep_load_fractions", &result))
		make_access_error("SAM_ReactorTesPower", "is_timestep_load_fractions");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_TimeOfDeliveryFactors_ppa_multiplier_model_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ppa_multiplier_model", &result))
		make_access_error("SAM_ReactorTesPower", "ppa_multiplier_model");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_TimeOfDeliveryFactors_start_day_of_year_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "start_day_of_year", &result))
		make_access_error("SAM_ReactorTesPower", "start_day_of_year");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_FinancialSolutionMode_ppa_soln_mode_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ppa_soln_mode", &result))
		make_access_error("SAM_ReactorTesPower", "ppa_soln_mode");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_ElectricityRates_en_electricity_rates_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "en_electricity_rates", &result))
		make_access_error("SAM_ReactorTesPower", "en_electricity_rates");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Revenue_mp_energy_market_revenue_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "mp_energy_market_revenue", nrows, ncols);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "mp_energy_market_revenue");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Revenue_ppa_price_input_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "ppa_price_input", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "ppa_price_input");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_SystemCosts_bop_spec_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "bop_spec_cost", &result))
		make_access_error("SAM_ReactorTesPower", "bop_spec_cost");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_SystemCosts_contingency_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "contingency_rate", &result))
		make_access_error("SAM_ReactorTesPower", "contingency_rate");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_SystemCosts_cycle_spec_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cycle_spec_cost", &result))
		make_access_error("SAM_ReactorTesPower", "cycle_spec_cost");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_SystemCosts_epc_cost_fixed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "epc_cost_fixed", &result))
		make_access_error("SAM_ReactorTesPower", "epc_cost_fixed");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_SystemCosts_epc_cost_per_watt_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "epc_cost_per_watt", &result))
		make_access_error("SAM_ReactorTesPower", "epc_cost_per_watt");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_SystemCosts_epc_cost_perc_of_direct_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "epc_cost_perc_of_direct", &result))
		make_access_error("SAM_ReactorTesPower", "epc_cost_perc_of_direct");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_SystemCosts_land_cost_fixed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "land_cost_fixed", &result))
		make_access_error("SAM_ReactorTesPower", "land_cost_fixed");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_SystemCosts_land_cost_per_watt_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "land_cost_per_watt", &result))
		make_access_error("SAM_ReactorTesPower", "land_cost_per_watt");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_SystemCosts_land_cost_perc_of_direct_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "land_cost_perc_of_direct", &result))
		make_access_error("SAM_ReactorTesPower", "land_cost_perc_of_direct");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_SystemCosts_reactor_spec_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "reactor_spec_cost", &result))
		make_access_error("SAM_ReactorTesPower", "reactor_spec_cost");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_SystemCosts_sales_tax_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sales_tax_frac", &result))
		make_access_error("SAM_ReactorTesPower", "sales_tax_frac");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_SystemCosts_tes_spec_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_spec_cost", &result))
		make_access_error("SAM_ReactorTesPower", "tes_spec_cost");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_FinancialParameters_const_per_interest_rate1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest_rate1", &result))
		make_access_error("SAM_ReactorTesPower", "const_per_interest_rate1");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_FinancialParameters_const_per_interest_rate2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest_rate2", &result))
		make_access_error("SAM_ReactorTesPower", "const_per_interest_rate2");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_FinancialParameters_const_per_interest_rate3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest_rate3", &result))
		make_access_error("SAM_ReactorTesPower", "const_per_interest_rate3");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_FinancialParameters_const_per_interest_rate4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest_rate4", &result))
		make_access_error("SAM_ReactorTesPower", "const_per_interest_rate4");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_FinancialParameters_const_per_interest_rate5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest_rate5", &result))
		make_access_error("SAM_ReactorTesPower", "const_per_interest_rate5");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_FinancialParameters_const_per_months1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_months1", &result))
		make_access_error("SAM_ReactorTesPower", "const_per_months1");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_FinancialParameters_const_per_months2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_months2", &result))
		make_access_error("SAM_ReactorTesPower", "const_per_months2");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_FinancialParameters_const_per_months3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_months3", &result))
		make_access_error("SAM_ReactorTesPower", "const_per_months3");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_FinancialParameters_const_per_months4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_months4", &result))
		make_access_error("SAM_ReactorTesPower", "const_per_months4");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_FinancialParameters_const_per_months5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_months5", &result))
		make_access_error("SAM_ReactorTesPower", "const_per_months5");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_FinancialParameters_const_per_percent1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_percent1", &result))
		make_access_error("SAM_ReactorTesPower", "const_per_percent1");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_FinancialParameters_const_per_percent2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_percent2", &result))
		make_access_error("SAM_ReactorTesPower", "const_per_percent2");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_FinancialParameters_const_per_percent3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_percent3", &result))
		make_access_error("SAM_ReactorTesPower", "const_per_percent3");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_FinancialParameters_const_per_percent4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_percent4", &result))
		make_access_error("SAM_ReactorTesPower", "const_per_percent4");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_FinancialParameters_const_per_percent5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_percent5", &result))
		make_access_error("SAM_ReactorTesPower", "const_per_percent5");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_FinancialParameters_const_per_upfront_rate1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_upfront_rate1", &result))
		make_access_error("SAM_ReactorTesPower", "const_per_upfront_rate1");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_FinancialParameters_const_per_upfront_rate2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_upfront_rate2", &result))
		make_access_error("SAM_ReactorTesPower", "const_per_upfront_rate2");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_FinancialParameters_const_per_upfront_rate3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_upfront_rate3", &result))
		make_access_error("SAM_ReactorTesPower", "const_per_upfront_rate3");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_FinancialParameters_const_per_upfront_rate4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_upfront_rate4", &result))
		make_access_error("SAM_ReactorTesPower", "const_per_upfront_rate4");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_FinancialParameters_const_per_upfront_rate5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_upfront_rate5", &result))
		make_access_error("SAM_ReactorTesPower", "const_per_upfront_rate5");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_FinancialParameters_sales_tax_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sales_tax_rate", &result))
		make_access_error("SAM_ReactorTesPower", "sales_tax_rate");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_AdjustmentFactors_adjust_constant_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "adjust_constant", &result))
		make_access_error("SAM_ReactorTesPower", "adjust_constant");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_AdjustmentFactors_adjust_en_periods_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "adjust_en_periods", &result))
		make_access_error("SAM_ReactorTesPower", "adjust_en_periods");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_AdjustmentFactors_adjust_en_timeindex_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "adjust_en_timeindex", &result))
		make_access_error("SAM_ReactorTesPower", "adjust_en_timeindex");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_AdjustmentFactors_adjust_periods_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "adjust_periods", nrows, ncols);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "adjust_periods");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_AdjustmentFactors_adjust_timeindex_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "adjust_timeindex", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "adjust_timeindex");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_AdjustmentFactors_sf_adjust_constant_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sf_adjust_constant", &result))
		make_access_error("SAM_ReactorTesPower", "sf_adjust_constant");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_AdjustmentFactors_sf_adjust_en_periods_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sf_adjust_en_periods", &result))
		make_access_error("SAM_ReactorTesPower", "sf_adjust_en_periods");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_AdjustmentFactors_sf_adjust_en_timeindex_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sf_adjust_en_timeindex", &result))
		make_access_error("SAM_ReactorTesPower", "sf_adjust_en_timeindex");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_AdjustmentFactors_sf_adjust_periods_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "sf_adjust_periods", nrows, ncols);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "sf_adjust_periods");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_AdjustmentFactors_sf_adjust_timeindex_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "sf_adjust_timeindex", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "sf_adjust_timeindex");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_P_cond_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_cond", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "P_cond");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_P_cond_iter_err_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_cond_iter_err", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "P_cond_iter_err");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_P_cooling_tower_tot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_cooling_tower_tot", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "P_cooling_tower_tot");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_P_cycle_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_cycle", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "P_cycle");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_P_fixed_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_fixed", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "P_fixed");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_P_out_net_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_out_net", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "P_out_net");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_P_plant_balance_tot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_plant_balance_tot", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "P_plant_balance_tot");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_Q_dot_HTF_ND_des_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Q_dot_HTF_ND_des_calc", &result))
		make_access_error("SAM_ReactorTesPower", "Q_dot_HTF_ND_des_calc");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_Q_tes_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Q_tes_des", &result))
		make_access_error("SAM_ReactorTesPower", "Q_tes_des");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_T_amb_high_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_amb_high_calc", &result))
		make_access_error("SAM_ReactorTesPower", "T_amb_high_calc");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_T_amb_low_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_amb_low_calc", &result))
		make_access_error("SAM_ReactorTesPower", "T_amb_low_calc");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_T_amb_ref_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_amb_ref_calc", &result))
		make_access_error("SAM_ReactorTesPower", "T_amb_ref_calc");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_T_cond_out_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_cond_out", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "T_cond_out");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_T_htf_high_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_htf_high_calc", &result))
		make_access_error("SAM_ReactorTesPower", "T_htf_high_calc");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_T_htf_low_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_htf_low_calc", &result))
		make_access_error("SAM_ReactorTesPower", "T_htf_low_calc");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_T_htf_ref_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_htf_ref_calc", &result))
		make_access_error("SAM_ReactorTesPower", "T_htf_ref_calc");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_T_pc_in_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_pc_in", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "T_pc_in");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_T_pc_out_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_pc_out", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "T_pc_out");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_T_tes_cold_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_tes_cold", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "T_tes_cold");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_T_tes_hot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_tes_hot", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "T_tes_hot");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_V_tes_htf_avail_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "V_tes_htf_avail_des", &result))
		make_access_error("SAM_ReactorTesPower", "V_tes_htf_avail_des");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_V_tes_htf_total_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "V_tes_htf_total_des", &result))
		make_access_error("SAM_ReactorTesPower", "V_tes_htf_total_des");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_W_dot_bop_design_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "W_dot_bop_design", &result))
		make_access_error("SAM_ReactorTesPower", "W_dot_bop_design");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_W_dot_cooling_ND_des_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "W_dot_cooling_ND_des_calc", &result))
		make_access_error("SAM_ReactorTesPower", "W_dot_cooling_ND_des_calc");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_W_dot_cycle_cooling_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "W_dot_cycle_cooling_des", &result))
		make_access_error("SAM_ReactorTesPower", "W_dot_cycle_cooling_des");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_W_dot_cycle_pump_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "W_dot_cycle_pump_des", &result))
		make_access_error("SAM_ReactorTesPower", "W_dot_cycle_pump_des");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_W_dot_fixed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "W_dot_fixed", &result))
		make_access_error("SAM_ReactorTesPower", "W_dot_fixed");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_W_dot_gross_ND_des_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "W_dot_gross_ND_des_calc", &result))
		make_access_error("SAM_ReactorTesPower", "W_dot_gross_ND_des_calc");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_all_hours_electricity_sales_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "all_hours_electricity_sales", &result))
		make_access_error("SAM_ReactorTesPower", "all_hours_electricity_sales");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_all_hours_revenue_fraction_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "all_hours_revenue_fraction", &result))
		make_access_error("SAM_ReactorTesPower", "all_hours_revenue_fraction");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_annual_W_cooling_tower_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_W_cooling_tower", &result))
		make_access_error("SAM_ReactorTesPower", "annual_W_cooling_tower");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_annual_W_cycle_gross_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_W_cycle_gross", &result))
		make_access_error("SAM_ReactorTesPower", "annual_W_cycle_gross");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_annual_energy_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_energy", &result))
		make_access_error("SAM_ReactorTesPower", "annual_energy");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_annual_energy_distribution_time_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "annual_energy_distribution_time", nrows, ncols);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "annual_energy_distribution_time");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_annual_fuel_usage_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_fuel_usage", &result))
		make_access_error("SAM_ReactorTesPower", "annual_fuel_usage");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_annual_total_water_use_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_total_water_use", &result))
		make_access_error("SAM_ReactorTesPower", "annual_total_water_use");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_beam_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "beam", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "beam");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_bop_cost_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "bop_cost_calc", &result))
		make_access_error("SAM_ReactorTesPower", "bop_cost_calc");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_capacity_factor_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "capacity_factor", &result))
		make_access_error("SAM_ReactorTesPower", "capacity_factor");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_capacity_factor_highest_1000_ppas_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "capacity_factor_highest_1000_ppas", &result))
		make_access_error("SAM_ReactorTesPower", "capacity_factor_highest_1000_ppas");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_capacity_factor_highest_2000_ppas_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "capacity_factor_highest_2000_ppas", &result))
		make_access_error("SAM_ReactorTesPower", "capacity_factor_highest_2000_ppas");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_capacity_factor_warmest_100_Tambs_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "capacity_factor_warmest_100_Tambs", &result))
		make_access_error("SAM_ReactorTesPower", "capacity_factor_warmest_100_Tambs");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_const_per_interest1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest1", &result))
		make_access_error("SAM_ReactorTesPower", "const_per_interest1");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_const_per_interest2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest2", &result))
		make_access_error("SAM_ReactorTesPower", "const_per_interest2");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_const_per_interest3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest3", &result))
		make_access_error("SAM_ReactorTesPower", "const_per_interest3");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_const_per_interest4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest4", &result))
		make_access_error("SAM_ReactorTesPower", "const_per_interest4");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_const_per_interest5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest5", &result))
		make_access_error("SAM_ReactorTesPower", "const_per_interest5");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_const_per_interest_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest_total", &result))
		make_access_error("SAM_ReactorTesPower", "const_per_interest_total");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_const_per_percent_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_percent_total", &result))
		make_access_error("SAM_ReactorTesPower", "const_per_percent_total");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_const_per_principal1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_principal1", &result))
		make_access_error("SAM_ReactorTesPower", "const_per_principal1");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_const_per_principal2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_principal2", &result))
		make_access_error("SAM_ReactorTesPower", "const_per_principal2");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_const_per_principal3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_principal3", &result))
		make_access_error("SAM_ReactorTesPower", "const_per_principal3");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_const_per_principal4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_principal4", &result))
		make_access_error("SAM_ReactorTesPower", "const_per_principal4");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_const_per_principal5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_principal5", &result))
		make_access_error("SAM_ReactorTesPower", "const_per_principal5");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_const_per_principal_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_principal_total", &result))
		make_access_error("SAM_ReactorTesPower", "const_per_principal_total");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_const_per_total1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_total1", &result))
		make_access_error("SAM_ReactorTesPower", "const_per_total1");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_const_per_total2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_total2", &result))
		make_access_error("SAM_ReactorTesPower", "const_per_total2");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_const_per_total3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_total3", &result))
		make_access_error("SAM_ReactorTesPower", "const_per_total3");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_const_per_total4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_total4", &result))
		make_access_error("SAM_ReactorTesPower", "const_per_total4");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_const_per_total5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_total5", &result))
		make_access_error("SAM_ReactorTesPower", "const_per_total5");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_construction_financing_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "construction_financing_cost", &result))
		make_access_error("SAM_ReactorTesPower", "construction_financing_cost");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_contingency_cost_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "contingency_cost_calc", &result))
		make_access_error("SAM_ReactorTesPower", "contingency_cost_calc");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_conversion_factor_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "conversion_factor", &result))
		make_access_error("SAM_ReactorTesPower", "conversion_factor");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_cp_battery_nameplate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cp_battery_nameplate", &result))
		make_access_error("SAM_ReactorTesPower", "cp_battery_nameplate");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_cp_system_nameplate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cp_system_nameplate", &result))
		make_access_error("SAM_ReactorTesPower", "cp_system_nameplate");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_cycle_Tdb_table_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "cycle_Tdb_table", nrows, ncols);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "cycle_Tdb_table");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_cycle_cost_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cycle_cost_calc", &result))
		make_access_error("SAM_ReactorTesPower", "cycle_cost_calc");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_cycle_eff_load_table_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "cycle_eff_load_table", nrows, ncols);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "cycle_eff_load_table");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_cycle_htf_pump_power_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cycle_htf_pump_power", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "cycle_htf_pump_power");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_d_tank_tes_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "d_tank_tes", &result))
		make_access_error("SAM_ReactorTesPower", "d_tank_tes");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_dens_store_htf_at_T_ave_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dens_store_htf_at_T_ave", &result))
		make_access_error("SAM_ReactorTesPower", "dens_store_htf_at_T_ave");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_direct_subtotal_cost_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "direct_subtotal_cost_calc", &result))
		make_access_error("SAM_ReactorTesPower", "direct_subtotal_cost_calc");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_e_ch_tes_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "e_ch_tes", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "e_ch_tes");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_epc_cost_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "epc_cost_calc", &result))
		make_access_error("SAM_ReactorTesPower", "epc_cost_calc");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_eta_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "eta", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "eta");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_gen_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "gen", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "gen");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_hot_hours_electricity_sales_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hot_hours_electricity_sales", &result))
		make_access_error("SAM_ReactorTesPower", "hot_hours_electricity_sales");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_hot_hours_revenue_fraction_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hot_hours_revenue_fraction", &result))
		make_access_error("SAM_ReactorTesPower", "hot_hours_revenue_fraction");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_hot_tank_htf_percent_final_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "hot_tank_htf_percent_final", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "hot_tank_htf_percent_final");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_installed_per_cap_cost_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "installed_per_cap_cost_calc", &result))
		make_access_error("SAM_ReactorTesPower", "installed_per_cap_cost_calc");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_is_pc_sb_allowed_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "is_pc_sb_allowed", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "is_pc_sb_allowed");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_is_pc_su_allowed_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "is_pc_su_allowed", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "is_pc_su_allowed");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_kwh_per_kw_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "kwh_per_kw", &result))
		make_access_error("SAM_ReactorTesPower", "kwh_per_kw");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_land_cost_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "land_cost_calc", &result))
		make_access_error("SAM_ReactorTesPower", "land_cost_calc");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_m_dot_balance_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_balance", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "m_dot_balance");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_m_dot_cr_to_tes_hot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_cr_to_tes_hot", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "m_dot_cr_to_tes_hot");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_m_dot_cycle_to_field_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_cycle_to_field", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "m_dot_cycle_to_field");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_m_dot_field_to_cycle_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_field_to_cycle", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "m_dot_field_to_cycle");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_m_dot_htf_ND_high_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "m_dot_htf_ND_high_calc", &result))
		make_access_error("SAM_ReactorTesPower", "m_dot_htf_ND_high_calc");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_m_dot_htf_ND_low_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "m_dot_htf_ND_low_calc", &result))
		make_access_error("SAM_ReactorTesPower", "m_dot_htf_ND_low_calc");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_m_dot_htf_ND_ref_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "m_dot_htf_ND_ref_calc", &result))
		make_access_error("SAM_ReactorTesPower", "m_dot_htf_ND_ref_calc");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_m_dot_htf_cycle_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "m_dot_htf_cycle_des", &result))
		make_access_error("SAM_ReactorTesPower", "m_dot_htf_cycle_des");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_m_dot_pc_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_pc", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "m_dot_pc");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_m_dot_pc_to_tes_cold_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_pc_to_tes_cold", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "m_dot_pc_to_tes_cold");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_m_dot_tes_cold_out_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_tes_cold_out", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "m_dot_tes_cold_out");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_m_dot_tes_hot_out_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_tes_hot_out", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "m_dot_tes_hot_out");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_m_dot_water_ND_des_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "m_dot_water_ND_des_calc", &result))
		make_access_error("SAM_ReactorTesPower", "m_dot_water_ND_des_calc");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_m_dot_water_pc_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_water_pc", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "m_dot_water_pc");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_mass_tes_cold_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "mass_tes_cold", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "mass_tes_cold");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_mass_tes_hot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "mass_tes_hot", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "mass_tes_hot");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_n_T_amb_pars_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "n_T_amb_pars_calc", &result))
		make_access_error("SAM_ReactorTesPower", "n_T_amb_pars_calc");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_n_T_htf_pars_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "n_T_htf_pars_calc", &result))
		make_access_error("SAM_ReactorTesPower", "n_T_htf_pars_calc");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_n_m_dot_pars_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "n_m_dot_pars_calc", &result))
		make_access_error("SAM_ReactorTesPower", "n_m_dot_pars_calc");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_n_op_modes_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "n_op_modes", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "n_op_modes");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_nameplate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "nameplate", &result))
		make_access_error("SAM_ReactorTesPower", "nameplate");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_op_mode_1_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "op_mode_1", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "op_mode_1");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_op_mode_2_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "op_mode_2", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "op_mode_2");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_op_mode_3_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "op_mode_3", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "op_mode_3");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_operating_modes_a_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "operating_modes_a", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "operating_modes_a");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_operating_modes_b_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "operating_modes_b", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "operating_modes_b");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_operating_modes_c_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "operating_modes_c", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "operating_modes_c");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_pc_op_mode_final_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pc_op_mode_final", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "pc_op_mode_final");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_pc_startup_energy_remain_final_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pc_startup_energy_remain_final", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "pc_startup_energy_remain_final");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_pc_startup_time_remain_final_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pc_startup_time_remain_final", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "pc_startup_time_remain_final");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_pricing_mult_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pricing_mult", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "pricing_mult");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_q_balance_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_balance", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "q_balance");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_q_ch_tes_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_ch_tes", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "q_ch_tes");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_q_dc_tes_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dc_tes", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "q_dc_tes");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_q_dot_cycle_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_dot_cycle_des", &result))
		make_access_error("SAM_ReactorTesPower", "q_dot_cycle_des");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_q_dot_est_tes_ch_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_est_tes_ch", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "q_dot_est_tes_ch");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_q_dot_est_tes_dc_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_est_tes_dc", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "q_dot_est_tes_dc");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_q_dot_loss_tes_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_dot_loss_tes_des", &result))
		make_access_error("SAM_ReactorTesPower", "q_dot_loss_tes_des");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_q_dot_pc_max_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_pc_max", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "q_dot_pc_max");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_q_dot_pc_min_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_pc_min", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "q_dot_pc_min");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_q_dot_pc_sb_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_pc_sb", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "q_dot_pc_sb");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_q_dot_pc_startup_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_pc_startup", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "q_dot_pc_startup");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_q_dot_pc_target_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_pc_target", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "q_dot_pc_target");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_q_dot_reactor_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_dot_reactor_des", &result))
		make_access_error("SAM_ReactorTesPower", "q_dot_reactor_des");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_q_heater_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_heater", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "q_heater");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_q_pb_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_pb", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "q_pb");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_q_pc_startup_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_pc_startup", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "q_pc_startup");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_reactor_cost_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "reactor_cost_calc", &result))
		make_access_error("SAM_ReactorTesPower", "reactor_cost_calc");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_reactor_thermal_power_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "reactor_thermal_power", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "reactor_thermal_power");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_reactor_turn_down_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "reactor_turn_down", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "reactor_turn_down");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_rh_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "rh", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "rh");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_sales_energy_capacity_factor_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sales_energy_capacity_factor", &result))
		make_access_error("SAM_ReactorTesPower", "sales_energy_capacity_factor");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_sales_tax_cost_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sales_tax_cost_calc", &result))
		make_access_error("SAM_ReactorTesPower", "sales_tax_cost_calc");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_sim_cpu_run_time_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sim_cpu_run_time", &result))
		make_access_error("SAM_ReactorTesPower", "sim_cpu_run_time");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_system_capacity_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_capacity", &result))
		make_access_error("SAM_ReactorTesPower", "system_capacity");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_tank_losses_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tank_losses", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "tank_losses");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_tdry_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tdry", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "tdry");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_tes_cost_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_cost_calc", &result))
		make_access_error("SAM_ReactorTesPower", "tes_cost_calc");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_tes_htf_pump_power_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tes_htf_pump_power", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "tes_htf_pump_power");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_time_hr_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "time_hr", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "time_hr");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_total_direct_cost_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_direct_cost_calc", &result))
		make_access_error("SAM_ReactorTesPower", "total_direct_cost_calc");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_total_indirect_cost_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_indirect_cost_calc", &result))
		make_access_error("SAM_ReactorTesPower", "total_indirect_cost_calc");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_total_installed_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_installed_cost", &result))
		make_access_error("SAM_ReactorTesPower", "total_installed_cost");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_tou_value_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tou_value", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "tou_value");
	});
	return result;
}

SAM_EXPORT double SAM_ReactorTesPower_Outputs_tshours_reactor_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tshours_reactor", &result))
		make_access_error("SAM_ReactorTesPower", "tshours_reactor");
	});
	return result;
}

SAM_EXPORT double* SAM_ReactorTesPower_Outputs_twet_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "twet", length);
	if (!result)
		make_access_error("SAM_ReactorTesPower", "twet");
	});
	return result;
}

