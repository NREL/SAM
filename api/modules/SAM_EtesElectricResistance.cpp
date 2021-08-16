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

SAM_EXPORT void SAM_EtesElectricResistance_SystemControl_F_wc_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "F_wc", arr, length);
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

SAM_EXPORT void SAM_EtesElectricResistance_ThermalStorage_csp_pt_tes_init_hot_htf_percent_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp.pt.tes.init_hot_htf_percent", number);
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

SAM_EXPORT void SAM_EtesElectricResistance_ThermalStorage_tes_fl_code_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tes_fl_code", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_ThermalStorage_u_tank_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "u_tank", number);
	});
}

SAM_EXPORT void SAM_EtesElectricResistance_ThermalStorage_ud_tes_fl_props_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "ud_tes_fl_props", mat, nrows, ncols);
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



SAM_EXPORT double* SAM_EtesElectricResistance_SystemControl_F_wc_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "F_wc", length);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "F_wc");
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



SAM_EXPORT double SAM_EtesElectricResistance_ThermalStorage_csp_pt_tes_init_hot_htf_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.tes.init_hot_htf_percent", &result))
		make_access_error("SAM_EtesElectricResistance", "csp.pt.tes.init_hot_htf_percent");
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



SAM_EXPORT double SAM_EtesElectricResistance_ThermalStorage_tes_fl_code_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_fl_code", &result))
		make_access_error("SAM_EtesElectricResistance", "tes_fl_code");
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



SAM_EXPORT double* SAM_EtesElectricResistance_ThermalStorage_ud_tes_fl_props_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "ud_tes_fl_props", nrows, ncols);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "ud_tes_fl_props");
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



SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_W_dot_cycle_gross_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_dot_cycle_gross", length);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "W_dot_cycle_gross");
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



SAM_EXPORT double SAM_EtesElectricResistance_Outputs_construction_financing_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "construction_financing_cost", &result))
		make_access_error("SAM_EtesElectricResistance", "construction_financing_cost");
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



SAM_EXPORT double* SAM_EtesElectricResistance_Outputs_gen_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "gen", length);
	if (!result)
		make_access_error("SAM_EtesElectricResistance", "gen");
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



SAM_EXPORT double SAM_EtesElectricResistance_Outputs_system_capacity_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_capacity", &result))
		make_access_error("SAM_EtesElectricResistance", "system_capacity");
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



SAM_EXPORT double SAM_EtesElectricResistance_Outputs_total_installed_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_installed_cost", &result))
		make_access_error("SAM_EtesElectricResistance", "total_installed_cost");
	});
	return result;
}



