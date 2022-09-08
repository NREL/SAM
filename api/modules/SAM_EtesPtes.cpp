#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_EtesPtes.h"

SAM_EXPORT int SAM_EtesPtes_execute(SAM_table data, int verbosity, SAM_error* err){
	return SAM_module_exec("etes_ptes", data, verbosity, err);
}

SAM_EXPORT void SAM_EtesPtes_SolarResource_solar_resource_file_sset(SAM_table ptr, const char* str, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_string(ptr, "solar_resource_file", str);
	});
}

SAM_EXPORT void SAM_EtesPtes_SystemControl_bop_par_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "bop_par", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_SystemControl_bop_par_0_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "bop_par_0", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_SystemControl_bop_par_1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "bop_par_1", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_SystemControl_bop_par_2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "bop_par_2", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_SystemControl_bop_par_f_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "bop_par_f", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_SystemControl_disp_csu_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_csu_cost", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_SystemControl_disp_down_time_min_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_down_time_min", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_SystemControl_disp_frequency_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_frequency", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_SystemControl_disp_horizon_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_horizon", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_SystemControl_disp_hsu_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_hsu_cost", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_SystemControl_disp_max_iter_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_max_iter", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_SystemControl_disp_mip_gap_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_mip_gap", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_SystemControl_disp_pen_delta_w_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_pen_delta_w", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_SystemControl_disp_reporting_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_reporting", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_SystemControl_disp_spec_bb_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_spec_bb", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_SystemControl_disp_spec_presolve_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_spec_presolve", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_SystemControl_disp_spec_scaling_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_spec_scaling", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_SystemControl_disp_steps_per_hour_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_steps_per_hour", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_SystemControl_disp_time_weighting_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_time_weighting", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_SystemControl_disp_timeout_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_timeout", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_SystemControl_disp_up_time_min_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "disp_up_time_min", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_SystemControl_is_dispatch_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "is_dispatch", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_SystemControl_pb_fixed_par_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pb_fixed_par", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_SystemControl_sim_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sim_type", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_SystemControl_time_start_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "time_start", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_SystemControl_time_steps_per_hour_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "time_steps_per_hour", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_SystemControl_time_stop_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "time_stop", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_SystemControl_vacuum_arrays_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "vacuum_arrays", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_FinancialModel_etes_financial_model_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "etes_financial_model", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_SystemDesign_T_CT_cold_htf_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_CT_cold_htf_des", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_SystemDesign_T_CT_hot_htf_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_CT_hot_htf_des", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_SystemDesign_T_HT_cold_htf_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_HT_cold_htf_des", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_SystemDesign_T_HT_hot_htf_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_HT_hot_htf_des", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_SystemDesign_W_dot_pc_thermo_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "W_dot_pc_thermo_des", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_SystemDesign_cop_hp_thermo_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cop_hp_thermo_des", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_SystemDesign_eta_pc_thermo_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "eta_pc_thermo_des", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_SystemDesign_f_hp_parasitic_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "f_hp_parasitic_des", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_SystemDesign_f_pc_parasitic_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "f_pc_parasitic_des", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_SystemDesign_heater_mult_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "heater_mult", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_SystemDesign_tshours_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tshours", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_ThermalStorage_cold_htf_code_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cold_htf_code", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_ThermalStorage_hot_htf_code_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hot_htf_code", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_ThermalStorage_ud_cold_htf_props_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "ud_cold_htf_props", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_EtesPtes_ThermalStorage_ud_hot_htf_props_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "ud_hot_htf_props", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_EtesPtes_Heater_f_q_dot_des_allowable_su_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "f_q_dot_des_allowable_su", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_Heater_f_q_dot_heater_min_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "f_q_dot_heater_min", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_Heater_hrs_startup_at_max_rate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hrs_startup_at_max_rate", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_PowerCycle_CT_pb_pump_coef_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "CT_pb_pump_coef", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_PowerCycle_cycle_cutoff_frac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cycle_cutoff_frac", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_PowerCycle_cycle_max_frac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cycle_max_frac", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_PowerCycle_heat_pump_CT_HTF_pump_coef_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "heat_pump_CT_HTF_pump_coef", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_PowerCycle_heat_pump_HT_HTF_pump_coef_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "heat_pump_HT_HTF_pump_coef", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_PowerCycle_pb_pump_coef_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pb_pump_coef", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_PowerCycle_q_sby_frac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "q_sby_frac", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_PowerCycle_startup_frac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "startup_frac", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_PowerCycle_startup_time_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "startup_time", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_HotThermalStorage_cold_tank_Thtr_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cold_tank_Thtr", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_HotThermalStorage_cold_tank_max_heat_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cold_tank_max_heat", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_HotThermalStorage_h_tank_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "h_tank", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_HotThermalStorage_h_tank_min_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "h_tank_min", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_HotThermalStorage_hot_tank_Thtr_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hot_tank_Thtr", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_HotThermalStorage_hot_tank_max_heat_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hot_tank_max_heat", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_HotThermalStorage_tank_pairs_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tank_pairs", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_HotThermalStorage_tes_init_hot_htf_percent_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tes_init_hot_htf_percent", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_HotThermalStorage_u_tank_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "u_tank", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_ColdThermalStorage_CT_h_tank_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "CT_h_tank", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_ColdThermalStorage_CT_h_tank_min_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "CT_h_tank_min", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_ColdThermalStorage_CT_tank_pairs_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "CT_tank_pairs", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_ColdThermalStorage_CT_u_tank_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "CT_u_tank", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_TimeOfDeliveryFactors_dispatch_factor1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dispatch_factor1", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_TimeOfDeliveryFactors_dispatch_factor2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dispatch_factor2", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_TimeOfDeliveryFactors_dispatch_factor3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dispatch_factor3", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_TimeOfDeliveryFactors_dispatch_factor4_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dispatch_factor4", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_TimeOfDeliveryFactors_dispatch_factor5_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dispatch_factor5", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_TimeOfDeliveryFactors_dispatch_factor6_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dispatch_factor6", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_TimeOfDeliveryFactors_dispatch_factor7_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dispatch_factor7", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_TimeOfDeliveryFactors_dispatch_factor8_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dispatch_factor8", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_TimeOfDeliveryFactors_dispatch_factor9_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dispatch_factor9", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_TimeOfDeliveryFactors_dispatch_factors_ts_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "dispatch_factors_ts", arr, length);
	});
}

SAM_EXPORT void SAM_EtesPtes_TimeOfDeliveryFactors_dispatch_sched_weekday_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "dispatch_sched_weekday", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_EtesPtes_TimeOfDeliveryFactors_dispatch_sched_weekend_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "dispatch_sched_weekend", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_EtesPtes_TimeOfDeliveryFactors_ppa_multiplier_model_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ppa_multiplier_model", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_Revenue_ppa_price_input_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "ppa_price_input", arr, length);
	});
}

SAM_EXPORT void SAM_EtesPtes_SystemCosts_CT_tes_spec_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "CT_tes_spec_cost", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_SystemCosts_bop_spec_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "bop_spec_cost", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_SystemCosts_contingency_rate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "contingency_rate", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_SystemCosts_cycle_spec_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cycle_spec_cost", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_SystemCosts_epc_cost_fixed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "epc_cost_fixed", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_SystemCosts_epc_cost_per_watt_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "epc_cost_per_watt", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_SystemCosts_epc_cost_perc_of_direct_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "epc_cost_perc_of_direct", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_SystemCosts_heat_pump_spec_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "heat_pump_spec_cost", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_SystemCosts_land_cost_fixed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "land_cost_fixed", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_SystemCosts_land_cost_per_watt_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "land_cost_per_watt", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_SystemCosts_land_cost_perc_of_direct_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "land_cost_perc_of_direct", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_SystemCosts_sales_tax_frac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sales_tax_frac", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_SystemCosts_tes_spec_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tes_spec_cost", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_FinancialParameters_const_per_interest_rate1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_interest_rate1", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_FinancialParameters_const_per_interest_rate2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_interest_rate2", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_FinancialParameters_const_per_interest_rate3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_interest_rate3", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_FinancialParameters_const_per_interest_rate4_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_interest_rate4", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_FinancialParameters_const_per_interest_rate5_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_interest_rate5", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_FinancialParameters_const_per_months1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_months1", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_FinancialParameters_const_per_months2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_months2", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_FinancialParameters_const_per_months3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_months3", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_FinancialParameters_const_per_months4_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_months4", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_FinancialParameters_const_per_months5_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_months5", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_FinancialParameters_const_per_percent1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_percent1", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_FinancialParameters_const_per_percent2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_percent2", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_FinancialParameters_const_per_percent3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_percent3", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_FinancialParameters_const_per_percent4_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_percent4", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_FinancialParameters_const_per_percent5_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_percent5", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_FinancialParameters_const_per_upfront_rate1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_upfront_rate1", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_FinancialParameters_const_per_upfront_rate2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_upfront_rate2", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_FinancialParameters_const_per_upfront_rate3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_upfront_rate3", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_FinancialParameters_const_per_upfront_rate4_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_upfront_rate4", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_FinancialParameters_const_per_upfront_rate5_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_upfront_rate5", number);
	});
}

SAM_EXPORT void SAM_EtesPtes_FinancialParameters_sales_tax_rate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sales_tax_rate", number);
	});
}

SAM_EXPORT const char* SAM_EtesPtes_SolarResource_solar_resource_file_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "solar_resource_file");
	if (!result)
		make_access_error("SAM_EtesPtes", "solar_resource_file");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_SystemControl_bop_par_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "bop_par", &result))
		make_access_error("SAM_EtesPtes", "bop_par");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_SystemControl_bop_par_0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "bop_par_0", &result))
		make_access_error("SAM_EtesPtes", "bop_par_0");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_SystemControl_bop_par_1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "bop_par_1", &result))
		make_access_error("SAM_EtesPtes", "bop_par_1");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_SystemControl_bop_par_2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "bop_par_2", &result))
		make_access_error("SAM_EtesPtes", "bop_par_2");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_SystemControl_bop_par_f_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "bop_par_f", &result))
		make_access_error("SAM_EtesPtes", "bop_par_f");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_SystemControl_disp_csu_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_csu_cost", &result))
		make_access_error("SAM_EtesPtes", "disp_csu_cost");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_SystemControl_disp_down_time_min_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_down_time_min", &result))
		make_access_error("SAM_EtesPtes", "disp_down_time_min");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_SystemControl_disp_frequency_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_frequency", &result))
		make_access_error("SAM_EtesPtes", "disp_frequency");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_SystemControl_disp_horizon_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_horizon", &result))
		make_access_error("SAM_EtesPtes", "disp_horizon");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_SystemControl_disp_hsu_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_hsu_cost", &result))
		make_access_error("SAM_EtesPtes", "disp_hsu_cost");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_SystemControl_disp_max_iter_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_max_iter", &result))
		make_access_error("SAM_EtesPtes", "disp_max_iter");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_SystemControl_disp_mip_gap_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_mip_gap", &result))
		make_access_error("SAM_EtesPtes", "disp_mip_gap");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_SystemControl_disp_pen_delta_w_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_pen_delta_w", &result))
		make_access_error("SAM_EtesPtes", "disp_pen_delta_w");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_SystemControl_disp_reporting_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_reporting", &result))
		make_access_error("SAM_EtesPtes", "disp_reporting");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_SystemControl_disp_spec_bb_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_spec_bb", &result))
		make_access_error("SAM_EtesPtes", "disp_spec_bb");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_SystemControl_disp_spec_presolve_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_spec_presolve", &result))
		make_access_error("SAM_EtesPtes", "disp_spec_presolve");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_SystemControl_disp_spec_scaling_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_spec_scaling", &result))
		make_access_error("SAM_EtesPtes", "disp_spec_scaling");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_SystemControl_disp_steps_per_hour_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_steps_per_hour", &result))
		make_access_error("SAM_EtesPtes", "disp_steps_per_hour");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_SystemControl_disp_time_weighting_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_time_weighting", &result))
		make_access_error("SAM_EtesPtes", "disp_time_weighting");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_SystemControl_disp_timeout_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_timeout", &result))
		make_access_error("SAM_EtesPtes", "disp_timeout");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_SystemControl_disp_up_time_min_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "disp_up_time_min", &result))
		make_access_error("SAM_EtesPtes", "disp_up_time_min");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_SystemControl_is_dispatch_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "is_dispatch", &result))
		make_access_error("SAM_EtesPtes", "is_dispatch");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_SystemControl_pb_fixed_par_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pb_fixed_par", &result))
		make_access_error("SAM_EtesPtes", "pb_fixed_par");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_SystemControl_sim_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sim_type", &result))
		make_access_error("SAM_EtesPtes", "sim_type");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_SystemControl_time_start_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "time_start", &result))
		make_access_error("SAM_EtesPtes", "time_start");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_SystemControl_time_steps_per_hour_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "time_steps_per_hour", &result))
		make_access_error("SAM_EtesPtes", "time_steps_per_hour");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_SystemControl_time_stop_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "time_stop", &result))
		make_access_error("SAM_EtesPtes", "time_stop");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_SystemControl_vacuum_arrays_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "vacuum_arrays", &result))
		make_access_error("SAM_EtesPtes", "vacuum_arrays");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_FinancialModel_etes_financial_model_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "etes_financial_model", &result))
		make_access_error("SAM_EtesPtes", "etes_financial_model");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_SystemDesign_T_CT_cold_htf_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_CT_cold_htf_des", &result))
		make_access_error("SAM_EtesPtes", "T_CT_cold_htf_des");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_SystemDesign_T_CT_hot_htf_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_CT_hot_htf_des", &result))
		make_access_error("SAM_EtesPtes", "T_CT_hot_htf_des");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_SystemDesign_T_HT_cold_htf_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_HT_cold_htf_des", &result))
		make_access_error("SAM_EtesPtes", "T_HT_cold_htf_des");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_SystemDesign_T_HT_hot_htf_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_HT_hot_htf_des", &result))
		make_access_error("SAM_EtesPtes", "T_HT_hot_htf_des");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_SystemDesign_W_dot_pc_thermo_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "W_dot_pc_thermo_des", &result))
		make_access_error("SAM_EtesPtes", "W_dot_pc_thermo_des");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_SystemDesign_cop_hp_thermo_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cop_hp_thermo_des", &result))
		make_access_error("SAM_EtesPtes", "cop_hp_thermo_des");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_SystemDesign_eta_pc_thermo_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_pc_thermo_des", &result))
		make_access_error("SAM_EtesPtes", "eta_pc_thermo_des");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_SystemDesign_f_hp_parasitic_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "f_hp_parasitic_des", &result))
		make_access_error("SAM_EtesPtes", "f_hp_parasitic_des");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_SystemDesign_f_pc_parasitic_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "f_pc_parasitic_des", &result))
		make_access_error("SAM_EtesPtes", "f_pc_parasitic_des");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_SystemDesign_heater_mult_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "heater_mult", &result))
		make_access_error("SAM_EtesPtes", "heater_mult");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_SystemDesign_tshours_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tshours", &result))
		make_access_error("SAM_EtesPtes", "tshours");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_ThermalStorage_cold_htf_code_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cold_htf_code", &result))
		make_access_error("SAM_EtesPtes", "cold_htf_code");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_ThermalStorage_hot_htf_code_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hot_htf_code", &result))
		make_access_error("SAM_EtesPtes", "hot_htf_code");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_ThermalStorage_ud_cold_htf_props_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "ud_cold_htf_props", nrows, ncols);
	if (!result)
		make_access_error("SAM_EtesPtes", "ud_cold_htf_props");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_ThermalStorage_ud_hot_htf_props_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "ud_hot_htf_props", nrows, ncols);
	if (!result)
		make_access_error("SAM_EtesPtes", "ud_hot_htf_props");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_Heater_f_q_dot_des_allowable_su_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "f_q_dot_des_allowable_su", &result))
		make_access_error("SAM_EtesPtes", "f_q_dot_des_allowable_su");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_Heater_f_q_dot_heater_min_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "f_q_dot_heater_min", &result))
		make_access_error("SAM_EtesPtes", "f_q_dot_heater_min");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_Heater_hrs_startup_at_max_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hrs_startup_at_max_rate", &result))
		make_access_error("SAM_EtesPtes", "hrs_startup_at_max_rate");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_PowerCycle_CT_pb_pump_coef_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "CT_pb_pump_coef", &result))
		make_access_error("SAM_EtesPtes", "CT_pb_pump_coef");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_PowerCycle_cycle_cutoff_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cycle_cutoff_frac", &result))
		make_access_error("SAM_EtesPtes", "cycle_cutoff_frac");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_PowerCycle_cycle_max_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cycle_max_frac", &result))
		make_access_error("SAM_EtesPtes", "cycle_max_frac");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_PowerCycle_heat_pump_CT_HTF_pump_coef_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "heat_pump_CT_HTF_pump_coef", &result))
		make_access_error("SAM_EtesPtes", "heat_pump_CT_HTF_pump_coef");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_PowerCycle_heat_pump_HT_HTF_pump_coef_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "heat_pump_HT_HTF_pump_coef", &result))
		make_access_error("SAM_EtesPtes", "heat_pump_HT_HTF_pump_coef");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_PowerCycle_pb_pump_coef_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pb_pump_coef", &result))
		make_access_error("SAM_EtesPtes", "pb_pump_coef");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_PowerCycle_q_sby_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_sby_frac", &result))
		make_access_error("SAM_EtesPtes", "q_sby_frac");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_PowerCycle_startup_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "startup_frac", &result))
		make_access_error("SAM_EtesPtes", "startup_frac");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_PowerCycle_startup_time_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "startup_time", &result))
		make_access_error("SAM_EtesPtes", "startup_time");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_HotThermalStorage_cold_tank_Thtr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cold_tank_Thtr", &result))
		make_access_error("SAM_EtesPtes", "cold_tank_Thtr");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_HotThermalStorage_cold_tank_max_heat_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cold_tank_max_heat", &result))
		make_access_error("SAM_EtesPtes", "cold_tank_max_heat");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_HotThermalStorage_h_tank_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "h_tank", &result))
		make_access_error("SAM_EtesPtes", "h_tank");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_HotThermalStorage_h_tank_min_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "h_tank_min", &result))
		make_access_error("SAM_EtesPtes", "h_tank_min");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_HotThermalStorage_hot_tank_Thtr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hot_tank_Thtr", &result))
		make_access_error("SAM_EtesPtes", "hot_tank_Thtr");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_HotThermalStorage_hot_tank_max_heat_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hot_tank_max_heat", &result))
		make_access_error("SAM_EtesPtes", "hot_tank_max_heat");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_HotThermalStorage_tank_pairs_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tank_pairs", &result))
		make_access_error("SAM_EtesPtes", "tank_pairs");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_HotThermalStorage_tes_init_hot_htf_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_init_hot_htf_percent", &result))
		make_access_error("SAM_EtesPtes", "tes_init_hot_htf_percent");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_HotThermalStorage_u_tank_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "u_tank", &result))
		make_access_error("SAM_EtesPtes", "u_tank");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_ColdThermalStorage_CT_h_tank_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "CT_h_tank", &result))
		make_access_error("SAM_EtesPtes", "CT_h_tank");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_ColdThermalStorage_CT_h_tank_min_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "CT_h_tank_min", &result))
		make_access_error("SAM_EtesPtes", "CT_h_tank_min");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_ColdThermalStorage_CT_tank_pairs_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "CT_tank_pairs", &result))
		make_access_error("SAM_EtesPtes", "CT_tank_pairs");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_ColdThermalStorage_CT_u_tank_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "CT_u_tank", &result))
		make_access_error("SAM_EtesPtes", "CT_u_tank");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_TimeOfDeliveryFactors_dispatch_factor1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dispatch_factor1", &result))
		make_access_error("SAM_EtesPtes", "dispatch_factor1");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_TimeOfDeliveryFactors_dispatch_factor2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dispatch_factor2", &result))
		make_access_error("SAM_EtesPtes", "dispatch_factor2");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_TimeOfDeliveryFactors_dispatch_factor3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dispatch_factor3", &result))
		make_access_error("SAM_EtesPtes", "dispatch_factor3");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_TimeOfDeliveryFactors_dispatch_factor4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dispatch_factor4", &result))
		make_access_error("SAM_EtesPtes", "dispatch_factor4");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_TimeOfDeliveryFactors_dispatch_factor5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dispatch_factor5", &result))
		make_access_error("SAM_EtesPtes", "dispatch_factor5");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_TimeOfDeliveryFactors_dispatch_factor6_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dispatch_factor6", &result))
		make_access_error("SAM_EtesPtes", "dispatch_factor6");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_TimeOfDeliveryFactors_dispatch_factor7_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dispatch_factor7", &result))
		make_access_error("SAM_EtesPtes", "dispatch_factor7");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_TimeOfDeliveryFactors_dispatch_factor8_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dispatch_factor8", &result))
		make_access_error("SAM_EtesPtes", "dispatch_factor8");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_TimeOfDeliveryFactors_dispatch_factor9_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dispatch_factor9", &result))
		make_access_error("SAM_EtesPtes", "dispatch_factor9");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_TimeOfDeliveryFactors_dispatch_factors_ts_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "dispatch_factors_ts", length);
	if (!result)
		make_access_error("SAM_EtesPtes", "dispatch_factors_ts");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_TimeOfDeliveryFactors_dispatch_sched_weekday_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "dispatch_sched_weekday", nrows, ncols);
	if (!result)
		make_access_error("SAM_EtesPtes", "dispatch_sched_weekday");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_TimeOfDeliveryFactors_dispatch_sched_weekend_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "dispatch_sched_weekend", nrows, ncols);
	if (!result)
		make_access_error("SAM_EtesPtes", "dispatch_sched_weekend");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_TimeOfDeliveryFactors_ppa_multiplier_model_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ppa_multiplier_model", &result))
		make_access_error("SAM_EtesPtes", "ppa_multiplier_model");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_Revenue_ppa_price_input_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "ppa_price_input", length);
	if (!result)
		make_access_error("SAM_EtesPtes", "ppa_price_input");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_SystemCosts_CT_tes_spec_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "CT_tes_spec_cost", &result))
		make_access_error("SAM_EtesPtes", "CT_tes_spec_cost");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_SystemCosts_bop_spec_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "bop_spec_cost", &result))
		make_access_error("SAM_EtesPtes", "bop_spec_cost");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_SystemCosts_contingency_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "contingency_rate", &result))
		make_access_error("SAM_EtesPtes", "contingency_rate");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_SystemCosts_cycle_spec_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cycle_spec_cost", &result))
		make_access_error("SAM_EtesPtes", "cycle_spec_cost");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_SystemCosts_epc_cost_fixed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "epc_cost_fixed", &result))
		make_access_error("SAM_EtesPtes", "epc_cost_fixed");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_SystemCosts_epc_cost_per_watt_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "epc_cost_per_watt", &result))
		make_access_error("SAM_EtesPtes", "epc_cost_per_watt");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_SystemCosts_epc_cost_perc_of_direct_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "epc_cost_perc_of_direct", &result))
		make_access_error("SAM_EtesPtes", "epc_cost_perc_of_direct");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_SystemCosts_heat_pump_spec_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "heat_pump_spec_cost", &result))
		make_access_error("SAM_EtesPtes", "heat_pump_spec_cost");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_SystemCosts_land_cost_fixed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "land_cost_fixed", &result))
		make_access_error("SAM_EtesPtes", "land_cost_fixed");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_SystemCosts_land_cost_per_watt_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "land_cost_per_watt", &result))
		make_access_error("SAM_EtesPtes", "land_cost_per_watt");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_SystemCosts_land_cost_perc_of_direct_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "land_cost_perc_of_direct", &result))
		make_access_error("SAM_EtesPtes", "land_cost_perc_of_direct");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_SystemCosts_sales_tax_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sales_tax_frac", &result))
		make_access_error("SAM_EtesPtes", "sales_tax_frac");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_SystemCosts_tes_spec_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_spec_cost", &result))
		make_access_error("SAM_EtesPtes", "tes_spec_cost");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_FinancialParameters_const_per_interest_rate1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest_rate1", &result))
		make_access_error("SAM_EtesPtes", "const_per_interest_rate1");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_FinancialParameters_const_per_interest_rate2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest_rate2", &result))
		make_access_error("SAM_EtesPtes", "const_per_interest_rate2");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_FinancialParameters_const_per_interest_rate3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest_rate3", &result))
		make_access_error("SAM_EtesPtes", "const_per_interest_rate3");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_FinancialParameters_const_per_interest_rate4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest_rate4", &result))
		make_access_error("SAM_EtesPtes", "const_per_interest_rate4");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_FinancialParameters_const_per_interest_rate5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest_rate5", &result))
		make_access_error("SAM_EtesPtes", "const_per_interest_rate5");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_FinancialParameters_const_per_months1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_months1", &result))
		make_access_error("SAM_EtesPtes", "const_per_months1");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_FinancialParameters_const_per_months2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_months2", &result))
		make_access_error("SAM_EtesPtes", "const_per_months2");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_FinancialParameters_const_per_months3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_months3", &result))
		make_access_error("SAM_EtesPtes", "const_per_months3");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_FinancialParameters_const_per_months4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_months4", &result))
		make_access_error("SAM_EtesPtes", "const_per_months4");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_FinancialParameters_const_per_months5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_months5", &result))
		make_access_error("SAM_EtesPtes", "const_per_months5");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_FinancialParameters_const_per_percent1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_percent1", &result))
		make_access_error("SAM_EtesPtes", "const_per_percent1");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_FinancialParameters_const_per_percent2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_percent2", &result))
		make_access_error("SAM_EtesPtes", "const_per_percent2");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_FinancialParameters_const_per_percent3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_percent3", &result))
		make_access_error("SAM_EtesPtes", "const_per_percent3");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_FinancialParameters_const_per_percent4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_percent4", &result))
		make_access_error("SAM_EtesPtes", "const_per_percent4");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_FinancialParameters_const_per_percent5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_percent5", &result))
		make_access_error("SAM_EtesPtes", "const_per_percent5");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_FinancialParameters_const_per_upfront_rate1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_upfront_rate1", &result))
		make_access_error("SAM_EtesPtes", "const_per_upfront_rate1");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_FinancialParameters_const_per_upfront_rate2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_upfront_rate2", &result))
		make_access_error("SAM_EtesPtes", "const_per_upfront_rate2");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_FinancialParameters_const_per_upfront_rate3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_upfront_rate3", &result))
		make_access_error("SAM_EtesPtes", "const_per_upfront_rate3");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_FinancialParameters_const_per_upfront_rate4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_upfront_rate4", &result))
		make_access_error("SAM_EtesPtes", "const_per_upfront_rate4");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_FinancialParameters_const_per_upfront_rate5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_upfront_rate5", &result))
		make_access_error("SAM_EtesPtes", "const_per_upfront_rate5");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_FinancialParameters_sales_tax_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sales_tax_rate", &result))
		make_access_error("SAM_EtesPtes", "sales_tax_rate");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_Outputs_COP_net_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "COP_net_des", &result))
		make_access_error("SAM_EtesPtes", "COP_net_des");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_Outputs_CT_tes_cost_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "CT_tes_cost_calc", &result))
		make_access_error("SAM_EtesPtes", "CT_tes_cost_calc");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_Outputs_E_hp_su_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "E_hp_su_des", &result))
		make_access_error("SAM_EtesPtes", "E_hp_su_des");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_Outputs_Q_CT_tes_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Q_CT_tes_des", &result))
		make_access_error("SAM_EtesPtes", "Q_CT_tes_des");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_Outputs_Q_tes_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Q_tes_des", &result))
		make_access_error("SAM_EtesPtes", "Q_tes_des");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_Outputs_T_CT_tes_cold_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_CT_tes_cold", length);
	if (!result)
		make_access_error("SAM_EtesPtes", "T_CT_tes_cold");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_Outputs_T_CT_tes_hot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_CT_tes_hot", length);
	if (!result)
		make_access_error("SAM_EtesPtes", "T_CT_tes_hot");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_Outputs_T_hp_CT_htf_cold_out_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_hp_CT_htf_cold_out", length);
	if (!result)
		make_access_error("SAM_EtesPtes", "T_hp_CT_htf_cold_out");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_Outputs_T_hp_CT_htf_hot_in_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_hp_CT_htf_hot_in", length);
	if (!result)
		make_access_error("SAM_EtesPtes", "T_hp_CT_htf_hot_in");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_Outputs_T_hp_HT_htf_cold_in_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_hp_HT_htf_cold_in", length);
	if (!result)
		make_access_error("SAM_EtesPtes", "T_hp_HT_htf_cold_in");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_Outputs_T_hp_HT_htf_hot_out_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_hp_HT_htf_hot_out", length);
	if (!result)
		make_access_error("SAM_EtesPtes", "T_hp_HT_htf_hot_out");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_Outputs_T_pc_CT_htf_cold_in_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_pc_CT_htf_cold_in", length);
	if (!result)
		make_access_error("SAM_EtesPtes", "T_pc_CT_htf_cold_in");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_Outputs_T_pc_CT_htf_hot_out_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_pc_CT_htf_hot_out", length);
	if (!result)
		make_access_error("SAM_EtesPtes", "T_pc_CT_htf_hot_out");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_Outputs_T_pc_HT_htf_cold_out_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_pc_HT_htf_cold_out", length);
	if (!result)
		make_access_error("SAM_EtesPtes", "T_pc_HT_htf_cold_out");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_Outputs_T_pc_HT_htf_hot_in_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_pc_HT_htf_hot_in", length);
	if (!result)
		make_access_error("SAM_EtesPtes", "T_pc_HT_htf_hot_in");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_Outputs_T_tes_cold_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_tes_cold", length);
	if (!result)
		make_access_error("SAM_EtesPtes", "T_tes_cold");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_Outputs_T_tes_hot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_tes_hot", length);
	if (!result)
		make_access_error("SAM_EtesPtes", "T_tes_hot");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_Outputs_V_CT_tes_htf_avail_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "V_CT_tes_htf_avail", &result))
		make_access_error("SAM_EtesPtes", "V_CT_tes_htf_avail");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_Outputs_V_CT_tes_htf_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "V_CT_tes_htf_total", &result))
		make_access_error("SAM_EtesPtes", "V_CT_tes_htf_total");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_Outputs_V_tes_htf_avail_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "V_tes_htf_avail", &result))
		make_access_error("SAM_EtesPtes", "V_tes_htf_avail");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_Outputs_V_tes_htf_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "V_tes_htf_total", &result))
		make_access_error("SAM_EtesPtes", "V_tes_htf_total");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_Outputs_W_dot_bop_design_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "W_dot_bop_design", &result))
		make_access_error("SAM_EtesPtes", "W_dot_bop_design");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_Outputs_W_dot_bop_parasitics_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_dot_bop_parasitics", length);
	if (!result)
		make_access_error("SAM_EtesPtes", "W_dot_bop_parasitics");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_Outputs_W_dot_fixed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "W_dot_fixed", &result))
		make_access_error("SAM_EtesPtes", "W_dot_fixed");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_Outputs_W_dot_fixed_parasitics_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_dot_fixed_parasitics", length);
	if (!result)
		make_access_error("SAM_EtesPtes", "W_dot_fixed_parasitics");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_Outputs_W_dot_hp_CT_htf_pump_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_dot_hp_CT_htf_pump", length);
	if (!result)
		make_access_error("SAM_EtesPtes", "W_dot_hp_CT_htf_pump");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_Outputs_W_dot_hp_CT_htf_pump_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "W_dot_hp_CT_htf_pump_des", &result))
		make_access_error("SAM_EtesPtes", "W_dot_hp_CT_htf_pump_des");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_Outputs_W_dot_hp_HT_htf_pump_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_dot_hp_HT_htf_pump", length);
	if (!result)
		make_access_error("SAM_EtesPtes", "W_dot_hp_HT_htf_pump");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_Outputs_W_dot_hp_HT_htf_pump_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "W_dot_hp_HT_htf_pump_des", &result))
		make_access_error("SAM_EtesPtes", "W_dot_hp_HT_htf_pump_des");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_Outputs_W_dot_hp_elec_parasitic_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "W_dot_hp_elec_parasitic_des", &result))
		make_access_error("SAM_EtesPtes", "W_dot_hp_elec_parasitic_des");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_Outputs_W_dot_hp_in_net_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "W_dot_hp_in_net_des", &result))
		make_access_error("SAM_EtesPtes", "W_dot_hp_in_net_des");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_Outputs_W_dot_hp_in_thermo_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "W_dot_hp_in_thermo_des", &result))
		make_access_error("SAM_EtesPtes", "W_dot_hp_in_thermo_des");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_Outputs_W_dot_hp_net_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_dot_hp_net", length);
	if (!result)
		make_access_error("SAM_EtesPtes", "W_dot_hp_net");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_Outputs_W_dot_hp_parasitics_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_dot_hp_parasitics", length);
	if (!result)
		make_access_error("SAM_EtesPtes", "W_dot_hp_parasitics");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_Outputs_W_dot_hp_thermo_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_dot_hp_thermo", length);
	if (!result)
		make_access_error("SAM_EtesPtes", "W_dot_hp_thermo");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_Outputs_W_dot_out_net_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_dot_out_net", length);
	if (!result)
		make_access_error("SAM_EtesPtes", "W_dot_out_net");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_Outputs_W_dot_pc_CT_htf_pump_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_dot_pc_CT_htf_pump", length);
	if (!result)
		make_access_error("SAM_EtesPtes", "W_dot_pc_CT_htf_pump");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_Outputs_W_dot_pc_CT_htf_pump_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "W_dot_pc_CT_htf_pump_des", &result))
		make_access_error("SAM_EtesPtes", "W_dot_pc_CT_htf_pump_des");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_Outputs_W_dot_pc_HT_htf_pump_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_dot_pc_HT_htf_pump", length);
	if (!result)
		make_access_error("SAM_EtesPtes", "W_dot_pc_HT_htf_pump");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_Outputs_W_dot_pc_HT_htf_pump_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "W_dot_pc_HT_htf_pump_des", &result))
		make_access_error("SAM_EtesPtes", "W_dot_pc_HT_htf_pump_des");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_Outputs_W_dot_pc_elec_parasitic_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "W_dot_pc_elec_parasitic_des", &result))
		make_access_error("SAM_EtesPtes", "W_dot_pc_elec_parasitic_des");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_Outputs_W_dot_pc_net_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "W_dot_pc_net_des", &result))
		make_access_error("SAM_EtesPtes", "W_dot_pc_net_des");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_Outputs_W_dot_pc_parasitics_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_dot_pc_parasitics", length);
	if (!result)
		make_access_error("SAM_EtesPtes", "W_dot_pc_parasitics");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_Outputs_W_dot_pc_thermo_out_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_dot_pc_thermo_out", length);
	if (!result)
		make_access_error("SAM_EtesPtes", "W_dot_pc_thermo_out");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_Outputs_annual_energy_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_energy", &result))
		make_access_error("SAM_EtesPtes", "annual_energy");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_Outputs_bop_cost_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "bop_cost_calc", &result))
		make_access_error("SAM_EtesPtes", "bop_cost_calc");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_Outputs_charge_capacity_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "charge_capacity", &result))
		make_access_error("SAM_EtesPtes", "charge_capacity");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_Outputs_construction_financing_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "construction_financing_cost", &result))
		make_access_error("SAM_EtesPtes", "construction_financing_cost");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_Outputs_contingency_cost_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "contingency_cost_calc", &result))
		make_access_error("SAM_EtesPtes", "contingency_cost_calc");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_Outputs_cop_hot_hp_thermo_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cop_hot_hp_thermo", length);
	if (!result)
		make_access_error("SAM_EtesPtes", "cop_hot_hp_thermo");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_Outputs_cp_battery_capacity_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cp_battery_capacity", &result))
		make_access_error("SAM_EtesPtes", "cp_battery_capacity");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_Outputs_cp_system_capacity_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cp_system_capacity", &result))
		make_access_error("SAM_EtesPtes", "cp_system_capacity");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_Outputs_cycle_cost_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cycle_cost_calc", &result))
		make_access_error("SAM_EtesPtes", "cycle_cost_calc");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_Outputs_d_CT_tank_tes_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "d_CT_tank_tes", &result))
		make_access_error("SAM_EtesPtes", "d_CT_tank_tes");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_Outputs_d_tank_tes_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "d_tank_tes", &result))
		make_access_error("SAM_EtesPtes", "d_tank_tes");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_Outputs_direct_subtotal_cost_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "direct_subtotal_cost_calc", &result))
		make_access_error("SAM_EtesPtes", "direct_subtotal_cost_calc");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_Outputs_e_ch_tes_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "e_ch_tes", length);
	if (!result)
		make_access_error("SAM_EtesPtes", "e_ch_tes");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_Outputs_elec_purchase_price_mult_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "elec_purchase_price_mult", length);
	if (!result)
		make_access_error("SAM_EtesPtes", "elec_purchase_price_mult");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_Outputs_epc_cost_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "epc_cost_calc", &result))
		make_access_error("SAM_EtesPtes", "epc_cost_calc");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_Outputs_eta_pc_net_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_pc_net_des", &result))
		make_access_error("SAM_EtesPtes", "eta_pc_net_des");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_Outputs_eta_pc_thermo_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "eta_pc_thermo", length);
	if (!result)
		make_access_error("SAM_EtesPtes", "eta_pc_thermo");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_Outputs_flip_target_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "flip_target_percent", &result))
		make_access_error("SAM_EtesPtes", "flip_target_percent");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_Outputs_gen_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "gen", length);
	if (!result)
		make_access_error("SAM_EtesPtes", "gen");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_Outputs_heater_cost_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "heater_cost_calc", &result))
		make_access_error("SAM_EtesPtes", "heater_cost_calc");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_Outputs_installed_per_cap_cost_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "installed_per_cap_cost_calc", &result))
		make_access_error("SAM_EtesPtes", "installed_per_cap_cost_calc");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_Outputs_land_cost_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "land_cost_calc", &result))
		make_access_error("SAM_EtesPtes", "land_cost_calc");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_Outputs_m_dot_balance_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_balance", length);
	if (!result)
		make_access_error("SAM_EtesPtes", "m_dot_balance");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_Outputs_m_dot_hp_CT_htf_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_hp_CT_htf", length);
	if (!result)
		make_access_error("SAM_EtesPtes", "m_dot_hp_CT_htf");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_Outputs_m_dot_hp_CT_htf_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "m_dot_hp_CT_htf_des", &result))
		make_access_error("SAM_EtesPtes", "m_dot_hp_CT_htf_des");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_Outputs_m_dot_hp_HT_htf_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_hp_HT_htf", length);
	if (!result)
		make_access_error("SAM_EtesPtes", "m_dot_hp_HT_htf");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_Outputs_m_dot_hp_HT_htf_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "m_dot_hp_HT_htf_des", &result))
		make_access_error("SAM_EtesPtes", "m_dot_hp_HT_htf_des");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_Outputs_m_dot_pc_CT_htf_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_pc_CT_htf", length);
	if (!result)
		make_access_error("SAM_EtesPtes", "m_dot_pc_CT_htf");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_Outputs_m_dot_pc_CT_htf_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "m_dot_pc_CT_htf_des", &result))
		make_access_error("SAM_EtesPtes", "m_dot_pc_CT_htf_des");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_Outputs_m_dot_pc_HT_htf_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_pc_HT_htf", length);
	if (!result)
		make_access_error("SAM_EtesPtes", "m_dot_pc_HT_htf");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_Outputs_m_dot_pc_HT_htf_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "m_dot_pc_HT_htf_des", &result))
		make_access_error("SAM_EtesPtes", "m_dot_pc_HT_htf_des");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_Outputs_mass_CT_tes_cold_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "mass_CT_tes_cold", length);
	if (!result)
		make_access_error("SAM_EtesPtes", "mass_CT_tes_cold");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_Outputs_mass_CT_tes_hot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "mass_CT_tes_hot", length);
	if (!result)
		make_access_error("SAM_EtesPtes", "mass_CT_tes_hot");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_Outputs_mass_tes_cold_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "mass_tes_cold", length);
	if (!result)
		make_access_error("SAM_EtesPtes", "mass_tes_cold");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_Outputs_mass_tes_hot_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "mass_tes_hot", length);
	if (!result)
		make_access_error("SAM_EtesPtes", "mass_tes_hot");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_Outputs_n_op_modes_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "n_op_modes", length);
	if (!result)
		make_access_error("SAM_EtesPtes", "n_op_modes");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_Outputs_nameplate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "nameplate", &result))
		make_access_error("SAM_EtesPtes", "nameplate");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_Outputs_op_mode_1_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "op_mode_1", length);
	if (!result)
		make_access_error("SAM_EtesPtes", "op_mode_1");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_Outputs_op_mode_2_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "op_mode_2", length);
	if (!result)
		make_access_error("SAM_EtesPtes", "op_mode_2");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_Outputs_op_mode_3_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "op_mode_3", length);
	if (!result)
		make_access_error("SAM_EtesPtes", "op_mode_3");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_Outputs_ppa_soln_mode_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ppa_soln_mode", &result))
		make_access_error("SAM_EtesPtes", "ppa_soln_mode");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_Outputs_q_balance_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_balance", length);
	if (!result)
		make_access_error("SAM_EtesPtes", "q_balance");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_Outputs_q_dot_CT_tes_heater_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_CT_tes_heater", length);
	if (!result)
		make_access_error("SAM_EtesPtes", "q_dot_CT_tes_heater");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_Outputs_q_dot_CT_tes_losses_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_CT_tes_losses", length);
	if (!result)
		make_access_error("SAM_EtesPtes", "q_dot_CT_tes_losses");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_Outputs_q_dot_ch_tes_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_ch_tes", length);
	if (!result)
		make_access_error("SAM_EtesPtes", "q_dot_ch_tes");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_Outputs_q_dot_dc_tes_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_dc_tes", length);
	if (!result)
		make_access_error("SAM_EtesPtes", "q_dot_dc_tes");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_Outputs_q_dot_hp_cold_in_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_dot_hp_cold_in_des", &result))
		make_access_error("SAM_EtesPtes", "q_dot_hp_cold_in_des");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_Outputs_q_dot_hp_from_CT_htf_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_hp_from_CT_htf", length);
	if (!result)
		make_access_error("SAM_EtesPtes", "q_dot_hp_from_CT_htf");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_Outputs_q_dot_hp_hot_out_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_dot_hp_hot_out_des", &result))
		make_access_error("SAM_EtesPtes", "q_dot_hp_hot_out_des");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_Outputs_q_dot_hp_startup_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_hp_startup", length);
	if (!result)
		make_access_error("SAM_EtesPtes", "q_dot_hp_startup");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_Outputs_q_dot_hp_to_HT_htf_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_hp_to_HT_htf", length);
	if (!result)
		make_access_error("SAM_EtesPtes", "q_dot_hp_to_HT_htf");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_Outputs_q_dot_loss_CT_tes_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_dot_loss_CT_tes_des", &result))
		make_access_error("SAM_EtesPtes", "q_dot_loss_CT_tes_des");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_Outputs_q_dot_loss_tes_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_dot_loss_tes_des", &result))
		make_access_error("SAM_EtesPtes", "q_dot_loss_tes_des");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_Outputs_q_dot_pc_cold_out_thermo_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_dot_pc_cold_out_thermo_des", &result))
		make_access_error("SAM_EtesPtes", "q_dot_pc_cold_out_thermo_des");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_Outputs_q_dot_pc_cold_to_CTES_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_dot_pc_cold_to_CTES_des", &result))
		make_access_error("SAM_EtesPtes", "q_dot_pc_cold_to_CTES_des");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_Outputs_q_dot_pc_cold_to_surroundings_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_dot_pc_cold_to_surroundings_des", &result))
		make_access_error("SAM_EtesPtes", "q_dot_pc_cold_to_surroundings_des");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_Outputs_q_dot_pc_from_HT_htf_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_pc_from_HT_htf", length);
	if (!result)
		make_access_error("SAM_EtesPtes", "q_dot_pc_from_HT_htf");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_Outputs_q_dot_pc_hot_in_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_dot_pc_hot_in_des", &result))
		make_access_error("SAM_EtesPtes", "q_dot_pc_hot_in_des");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_Outputs_q_dot_pc_rejected_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_pc_rejected", length);
	if (!result)
		make_access_error("SAM_EtesPtes", "q_dot_pc_rejected");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_Outputs_q_dot_pc_startup_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_pc_startup", length);
	if (!result)
		make_access_error("SAM_EtesPtes", "q_dot_pc_startup");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_Outputs_q_dot_pc_thermo_out_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_pc_thermo_out", length);
	if (!result)
		make_access_error("SAM_EtesPtes", "q_dot_pc_thermo_out");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_Outputs_q_dot_pc_to_CT_htf_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_pc_to_CT_htf", length);
	if (!result)
		make_access_error("SAM_EtesPtes", "q_dot_pc_to_CT_htf");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_Outputs_q_dot_tes_heater_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_tes_heater", length);
	if (!result)
		make_access_error("SAM_EtesPtes", "q_dot_tes_heater");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_Outputs_q_dot_tes_losses_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_tes_losses", length);
	if (!result)
		make_access_error("SAM_EtesPtes", "q_dot_tes_losses");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_Outputs_q_pc_target_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_pc_target", length);
	if (!result)
		make_access_error("SAM_EtesPtes", "q_pc_target");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_Outputs_rte_net_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rte_net", &result))
		make_access_error("SAM_EtesPtes", "rte_net");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_Outputs_rte_thermo_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rte_thermo", &result))
		make_access_error("SAM_EtesPtes", "rte_thermo");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_Outputs_sales_tax_cost_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sales_tax_cost_calc", &result))
		make_access_error("SAM_EtesPtes", "sales_tax_cost_calc");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_Outputs_system_capacity_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_capacity", &result))
		make_access_error("SAM_EtesPtes", "system_capacity");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_Outputs_tdry_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tdry", length);
	if (!result)
		make_access_error("SAM_EtesPtes", "tdry");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_Outputs_tes_cost_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_cost_calc", &result))
		make_access_error("SAM_EtesPtes", "tes_cost_calc");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_Outputs_time_hr_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "time_hr", length);
	if (!result)
		make_access_error("SAM_EtesPtes", "time_hr");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_Outputs_total_direct_cost_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_direct_cost_calc", &result))
		make_access_error("SAM_EtesPtes", "total_direct_cost_calc");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_Outputs_total_indirect_cost_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_indirect_cost_calc", &result))
		make_access_error("SAM_EtesPtes", "total_indirect_cost_calc");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_Outputs_total_installed_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_installed_cost", &result))
		make_access_error("SAM_EtesPtes", "total_installed_cost");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_Outputs_tou_period_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tou_period", length);
	if (!result)
		make_access_error("SAM_EtesPtes", "tou_period");
	});
	return result;
}



SAM_EXPORT double SAM_EtesPtes_Outputs_tshours_heater_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tshours_heater", &result))
		make_access_error("SAM_EtesPtes", "tshours_heater");
	});
	return result;
}



SAM_EXPORT double* SAM_EtesPtes_Outputs_twet_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "twet", length);
	if (!result)
		make_access_error("SAM_EtesPtes", "twet");
	});
	return result;
}



