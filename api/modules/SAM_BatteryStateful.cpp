#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_BatteryStateful.h"

SAM_EXPORT SAM_BatteryStateful SAM_BatteryStateful_setup(SAM_table data, SAM_error* err){
	SAM_module result = nullptr;
	translateExceptions(err, [&]{
		result = ssc_stateful_module_create("battery_stateful", data);
	});
	return result;
}

SAM_EXPORT void SAM_BatteryStateful_Controls_control_mode_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "control_mode", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_Controls_dt_hr_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dt_hr", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_Controls_input_current_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "input_current", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_Controls_input_power_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "input_power", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_Controls_run_sequentially_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "run_sequentially", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_ParamsCell_C_rate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "C_rate", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_ParamsCell_Qexp_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Qexp", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_ParamsCell_Qfull_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Qfull", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_ParamsCell_Qfull_flow_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Qfull_flow", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_ParamsCell_Qnom_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Qnom", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_ParamsCell_Vexp_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Vexp", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_ParamsCell_Vfull_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Vfull", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_ParamsCell_Vnom_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Vnom", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_ParamsCell_Vnom_default_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Vnom_default", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_ParamsCell_calendar_a_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "calendar_a", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_ParamsCell_calendar_b_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "calendar_b", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_ParamsCell_calendar_c_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "calendar_c", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_ParamsCell_calendar_choice_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "calendar_choice", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_ParamsCell_calendar_matrix_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "calendar_matrix", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_BatteryStateful_ParamsCell_calendar_q0_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "calendar_q0", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_ParamsCell_chem_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "chem", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_ParamsCell_cycling_matrix_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "cycling_matrix", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_BatteryStateful_ParamsCell_initial_SOC_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "initial_SOC", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_ParamsCell_leadacid_q10_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "leadacid_q10", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_ParamsCell_leadacid_q20_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "leadacid_q20", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_ParamsCell_leadacid_qn_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "leadacid_qn", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_ParamsCell_leadacid_tn_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "leadacid_tn", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_ParamsCell_maximum_SOC_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "maximum_SOC", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_ParamsCell_minimum_SOC_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "minimum_SOC", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_ParamsCell_resistance_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "resistance", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_ParamsCell_voltage_choice_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "voltage_choice", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_ParamsCell_voltage_matrix_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "voltage_matrix", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_BatteryStateful_ParamsPack_Cp_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Cp", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_ParamsPack_T_room_init_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_room_init", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_ParamsPack_cap_vs_temp_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "cap_vs_temp", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_BatteryStateful_ParamsPack_h_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "h", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_ParamsPack_loss_choice_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "loss_choice", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_ParamsPack_mass_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mass", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_ParamsPack_monthly_charge_loss_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "monthly_charge_loss", arr, length);
	});
}

SAM_EXPORT void SAM_BatteryStateful_ParamsPack_monthly_discharge_loss_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "monthly_discharge_loss", arr, length);
	});
}

SAM_EXPORT void SAM_BatteryStateful_ParamsPack_monthly_idle_loss_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "monthly_idle_loss", arr, length);
	});
}

SAM_EXPORT void SAM_BatteryStateful_ParamsPack_nominal_energy_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "nominal_energy", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_ParamsPack_nominal_voltage_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "nominal_voltage", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_ParamsPack_replacement_capacity_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "replacement_capacity", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_ParamsPack_replacement_option_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "replacement_option", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_ParamsPack_replacement_schedule_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "replacement_schedule", arr, length);
	});
}

SAM_EXPORT void SAM_BatteryStateful_ParamsPack_replacement_schedule_percent_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "replacement_schedule_percent", arr, length);
	});
}

SAM_EXPORT void SAM_BatteryStateful_ParamsPack_schedule_loss_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "schedule_loss", arr, length);
	});
}

SAM_EXPORT void SAM_BatteryStateful_ParamsPack_surface_area_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "surface_area", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_StatePack_I_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "I", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_StatePack_I_chargeable_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "I_chargeable", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_StatePack_I_dischargeable_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "I_dischargeable", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_StatePack_P_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "P", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_StatePack_P_chargeable_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "P_chargeable", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_StatePack_P_dischargeable_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "P_dischargeable", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_StatePack_Q_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Q", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_StatePack_Q_max_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Q_max", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_StatePack_SOC_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "SOC", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_StatePack_T_batt_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_batt", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_StatePack_T_room_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_room", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_StatePack_V_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "V", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_StatePack_heat_dissipated_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "heat_dissipated", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_StatePack_indices_replaced_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "indices_replaced", arr, length);
	});
}

SAM_EXPORT void SAM_BatteryStateful_StatePack_last_idx_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "last_idx", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_StatePack_loss_percent_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "loss_percent", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_StatePack_n_replacements_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "n_replacements", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_StateCell_I_loss_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "I_loss", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_StateCell_SOC_prev_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "SOC_prev", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_StateCell_T_batt_prev_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_batt_prev", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_StateCell_average_range_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "average_range", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_StateCell_cell_current_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cell_current", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_StateCell_cell_voltage_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cell_voltage", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_StateCell_chargeChange_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "chargeChange", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_StateCell_charge_mode_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "charge_mode", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_StateCell_day_age_of_battery_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "day_age_of_battery", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_StateCell_dq_relative_calendar_old_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dq_relative_calendar_old", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_StateCell_n_cycles_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "n_cycles", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_StateCell_prev_charge_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "prev_charge", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_StateCell_q0_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "q0", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_StateCell_q1_0_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "q1_0", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_StateCell_q2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "q2", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_StateCell_q2_0_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "q2_0", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_StateCell_q_relative_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "q_relative", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_StateCell_q_relative_calendar_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "q_relative_calendar", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_StateCell_q_relative_cycle_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "q_relative_cycle", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_StateCell_q_relative_thermal_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "q_relative_thermal", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_StateCell_qmax_lifetime_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "qmax_lifetime", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_StateCell_qmax_thermal_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "qmax_thermal", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_StateCell_qn_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "qn", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_StateCell_rainflow_Xlt_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rainflow_Xlt", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_StateCell_rainflow_Ylt_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rainflow_Ylt", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_StateCell_rainflow_jlt_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rainflow_jlt", number);
	});
}

SAM_EXPORT void SAM_BatteryStateful_StateCell_rainflow_peaks_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "rainflow_peaks", arr, length);
	});
}

SAM_EXPORT void SAM_BatteryStateful_StateCell_range_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "range", number);
	});
}

SAM_EXPORT double SAM_BatteryStateful_Controls_control_mode_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "control_mode", &result))
		make_access_error("SAM_BatteryStateful", "control_mode");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_Controls_dt_hr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dt_hr", &result))
		make_access_error("SAM_BatteryStateful", "dt_hr");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_Controls_input_current_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "input_current", &result))
		make_access_error("SAM_BatteryStateful", "input_current");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_Controls_input_power_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "input_power", &result))
		make_access_error("SAM_BatteryStateful", "input_power");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_Controls_run_sequentially_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "run_sequentially", &result))
		make_access_error("SAM_BatteryStateful", "run_sequentially");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_ParamsCell_C_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "C_rate", &result))
		make_access_error("SAM_BatteryStateful", "C_rate");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_ParamsCell_Qexp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Qexp", &result))
		make_access_error("SAM_BatteryStateful", "Qexp");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_ParamsCell_Qfull_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Qfull", &result))
		make_access_error("SAM_BatteryStateful", "Qfull");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_ParamsCell_Qfull_flow_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Qfull_flow", &result))
		make_access_error("SAM_BatteryStateful", "Qfull_flow");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_ParamsCell_Qnom_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Qnom", &result))
		make_access_error("SAM_BatteryStateful", "Qnom");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_ParamsCell_Vexp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Vexp", &result))
		make_access_error("SAM_BatteryStateful", "Vexp");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_ParamsCell_Vfull_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Vfull", &result))
		make_access_error("SAM_BatteryStateful", "Vfull");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_ParamsCell_Vnom_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Vnom", &result))
		make_access_error("SAM_BatteryStateful", "Vnom");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_ParamsCell_Vnom_default_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Vnom_default", &result))
		make_access_error("SAM_BatteryStateful", "Vnom_default");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_ParamsCell_calendar_a_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "calendar_a", &result))
		make_access_error("SAM_BatteryStateful", "calendar_a");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_ParamsCell_calendar_b_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "calendar_b", &result))
		make_access_error("SAM_BatteryStateful", "calendar_b");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_ParamsCell_calendar_c_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "calendar_c", &result))
		make_access_error("SAM_BatteryStateful", "calendar_c");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_ParamsCell_calendar_choice_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "calendar_choice", &result))
		make_access_error("SAM_BatteryStateful", "calendar_choice");
	});
	return result;
}



SAM_EXPORT double* SAM_BatteryStateful_ParamsCell_calendar_matrix_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "calendar_matrix", nrows, ncols);
	if (!result)
		make_access_error("SAM_BatteryStateful", "calendar_matrix");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_ParamsCell_calendar_q0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "calendar_q0", &result))
		make_access_error("SAM_BatteryStateful", "calendar_q0");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_ParamsCell_chem_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "chem", &result))
		make_access_error("SAM_BatteryStateful", "chem");
	});
	return result;
}



SAM_EXPORT double* SAM_BatteryStateful_ParamsCell_cycling_matrix_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "cycling_matrix", nrows, ncols);
	if (!result)
		make_access_error("SAM_BatteryStateful", "cycling_matrix");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_ParamsCell_initial_SOC_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "initial_SOC", &result))
		make_access_error("SAM_BatteryStateful", "initial_SOC");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_ParamsCell_leadacid_q10_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "leadacid_q10", &result))
		make_access_error("SAM_BatteryStateful", "leadacid_q10");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_ParamsCell_leadacid_q20_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "leadacid_q20", &result))
		make_access_error("SAM_BatteryStateful", "leadacid_q20");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_ParamsCell_leadacid_qn_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "leadacid_qn", &result))
		make_access_error("SAM_BatteryStateful", "leadacid_qn");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_ParamsCell_leadacid_tn_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "leadacid_tn", &result))
		make_access_error("SAM_BatteryStateful", "leadacid_tn");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_ParamsCell_maximum_SOC_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "maximum_SOC", &result))
		make_access_error("SAM_BatteryStateful", "maximum_SOC");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_ParamsCell_minimum_SOC_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "minimum_SOC", &result))
		make_access_error("SAM_BatteryStateful", "minimum_SOC");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_ParamsCell_resistance_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "resistance", &result))
		make_access_error("SAM_BatteryStateful", "resistance");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_ParamsCell_voltage_choice_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "voltage_choice", &result))
		make_access_error("SAM_BatteryStateful", "voltage_choice");
	});
	return result;
}



SAM_EXPORT double* SAM_BatteryStateful_ParamsCell_voltage_matrix_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "voltage_matrix", nrows, ncols);
	if (!result)
		make_access_error("SAM_BatteryStateful", "voltage_matrix");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_ParamsPack_Cp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Cp", &result))
		make_access_error("SAM_BatteryStateful", "Cp");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_ParamsPack_T_room_init_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_room_init", &result))
		make_access_error("SAM_BatteryStateful", "T_room_init");
	});
	return result;
}



SAM_EXPORT double* SAM_BatteryStateful_ParamsPack_cap_vs_temp_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "cap_vs_temp", nrows, ncols);
	if (!result)
		make_access_error("SAM_BatteryStateful", "cap_vs_temp");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_ParamsPack_h_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "h", &result))
		make_access_error("SAM_BatteryStateful", "h");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_ParamsPack_loss_choice_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "loss_choice", &result))
		make_access_error("SAM_BatteryStateful", "loss_choice");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_ParamsPack_mass_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mass", &result))
		make_access_error("SAM_BatteryStateful", "mass");
	});
	return result;
}



SAM_EXPORT double* SAM_BatteryStateful_ParamsPack_monthly_charge_loss_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_charge_loss", length);
	if (!result)
		make_access_error("SAM_BatteryStateful", "monthly_charge_loss");
	});
	return result;
}



SAM_EXPORT double* SAM_BatteryStateful_ParamsPack_monthly_discharge_loss_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_discharge_loss", length);
	if (!result)
		make_access_error("SAM_BatteryStateful", "monthly_discharge_loss");
	});
	return result;
}



SAM_EXPORT double* SAM_BatteryStateful_ParamsPack_monthly_idle_loss_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_idle_loss", length);
	if (!result)
		make_access_error("SAM_BatteryStateful", "monthly_idle_loss");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_ParamsPack_nominal_energy_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "nominal_energy", &result))
		make_access_error("SAM_BatteryStateful", "nominal_energy");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_ParamsPack_nominal_voltage_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "nominal_voltage", &result))
		make_access_error("SAM_BatteryStateful", "nominal_voltage");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_ParamsPack_replacement_capacity_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "replacement_capacity", &result))
		make_access_error("SAM_BatteryStateful", "replacement_capacity");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_ParamsPack_replacement_option_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "replacement_option", &result))
		make_access_error("SAM_BatteryStateful", "replacement_option");
	});
	return result;
}



SAM_EXPORT double* SAM_BatteryStateful_ParamsPack_replacement_schedule_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "replacement_schedule", length);
	if (!result)
		make_access_error("SAM_BatteryStateful", "replacement_schedule");
	});
	return result;
}



SAM_EXPORT double* SAM_BatteryStateful_ParamsPack_replacement_schedule_percent_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "replacement_schedule_percent", length);
	if (!result)
		make_access_error("SAM_BatteryStateful", "replacement_schedule_percent");
	});
	return result;
}



SAM_EXPORT double* SAM_BatteryStateful_ParamsPack_schedule_loss_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "schedule_loss", length);
	if (!result)
		make_access_error("SAM_BatteryStateful", "schedule_loss");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_ParamsPack_surface_area_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "surface_area", &result))
		make_access_error("SAM_BatteryStateful", "surface_area");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_StatePack_I_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "I", &result))
		make_access_error("SAM_BatteryStateful", "I");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_StatePack_I_chargeable_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "I_chargeable", &result))
		make_access_error("SAM_BatteryStateful", "I_chargeable");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_StatePack_I_dischargeable_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "I_dischargeable", &result))
		make_access_error("SAM_BatteryStateful", "I_dischargeable");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_StatePack_P_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P", &result))
		make_access_error("SAM_BatteryStateful", "P");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_StatePack_P_chargeable_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_chargeable", &result))
		make_access_error("SAM_BatteryStateful", "P_chargeable");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_StatePack_P_dischargeable_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_dischargeable", &result))
		make_access_error("SAM_BatteryStateful", "P_dischargeable");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_StatePack_Q_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Q", &result))
		make_access_error("SAM_BatteryStateful", "Q");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_StatePack_Q_max_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Q_max", &result))
		make_access_error("SAM_BatteryStateful", "Q_max");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_StatePack_SOC_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "SOC", &result))
		make_access_error("SAM_BatteryStateful", "SOC");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_StatePack_T_batt_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_batt", &result))
		make_access_error("SAM_BatteryStateful", "T_batt");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_StatePack_T_room_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_room", &result))
		make_access_error("SAM_BatteryStateful", "T_room");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_StatePack_V_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "V", &result))
		make_access_error("SAM_BatteryStateful", "V");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_StatePack_heat_dissipated_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "heat_dissipated", &result))
		make_access_error("SAM_BatteryStateful", "heat_dissipated");
	});
	return result;
}



SAM_EXPORT double* SAM_BatteryStateful_StatePack_indices_replaced_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "indices_replaced", length);
	if (!result)
		make_access_error("SAM_BatteryStateful", "indices_replaced");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_StatePack_last_idx_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "last_idx", &result))
		make_access_error("SAM_BatteryStateful", "last_idx");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_StatePack_loss_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "loss_percent", &result))
		make_access_error("SAM_BatteryStateful", "loss_percent");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_StatePack_n_replacements_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "n_replacements", &result))
		make_access_error("SAM_BatteryStateful", "n_replacements");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_StateCell_I_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "I_loss", &result))
		make_access_error("SAM_BatteryStateful", "I_loss");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_StateCell_SOC_prev_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "SOC_prev", &result))
		make_access_error("SAM_BatteryStateful", "SOC_prev");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_StateCell_T_batt_prev_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_batt_prev", &result))
		make_access_error("SAM_BatteryStateful", "T_batt_prev");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_StateCell_average_range_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "average_range", &result))
		make_access_error("SAM_BatteryStateful", "average_range");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_StateCell_cell_current_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cell_current", &result))
		make_access_error("SAM_BatteryStateful", "cell_current");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_StateCell_cell_voltage_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cell_voltage", &result))
		make_access_error("SAM_BatteryStateful", "cell_voltage");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_StateCell_chargeChange_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "chargeChange", &result))
		make_access_error("SAM_BatteryStateful", "chargeChange");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_StateCell_charge_mode_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "charge_mode", &result))
		make_access_error("SAM_BatteryStateful", "charge_mode");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_StateCell_day_age_of_battery_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "day_age_of_battery", &result))
		make_access_error("SAM_BatteryStateful", "day_age_of_battery");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_StateCell_dq_relative_calendar_old_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dq_relative_calendar_old", &result))
		make_access_error("SAM_BatteryStateful", "dq_relative_calendar_old");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_StateCell_n_cycles_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "n_cycles", &result))
		make_access_error("SAM_BatteryStateful", "n_cycles");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_StateCell_prev_charge_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "prev_charge", &result))
		make_access_error("SAM_BatteryStateful", "prev_charge");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_StateCell_q0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q0", &result))
		make_access_error("SAM_BatteryStateful", "q0");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_StateCell_q1_0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q1_0", &result))
		make_access_error("SAM_BatteryStateful", "q1_0");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_StateCell_q2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q2", &result))
		make_access_error("SAM_BatteryStateful", "q2");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_StateCell_q2_0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q2_0", &result))
		make_access_error("SAM_BatteryStateful", "q2_0");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_StateCell_q_relative_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_relative", &result))
		make_access_error("SAM_BatteryStateful", "q_relative");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_StateCell_q_relative_calendar_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_relative_calendar", &result))
		make_access_error("SAM_BatteryStateful", "q_relative_calendar");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_StateCell_q_relative_cycle_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_relative_cycle", &result))
		make_access_error("SAM_BatteryStateful", "q_relative_cycle");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_StateCell_q_relative_thermal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_relative_thermal", &result))
		make_access_error("SAM_BatteryStateful", "q_relative_thermal");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_StateCell_qmax_lifetime_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "qmax_lifetime", &result))
		make_access_error("SAM_BatteryStateful", "qmax_lifetime");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_StateCell_qmax_thermal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "qmax_thermal", &result))
		make_access_error("SAM_BatteryStateful", "qmax_thermal");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_StateCell_qn_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "qn", &result))
		make_access_error("SAM_BatteryStateful", "qn");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_StateCell_rainflow_Xlt_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rainflow_Xlt", &result))
		make_access_error("SAM_BatteryStateful", "rainflow_Xlt");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_StateCell_rainflow_Ylt_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rainflow_Ylt", &result))
		make_access_error("SAM_BatteryStateful", "rainflow_Ylt");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_StateCell_rainflow_jlt_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rainflow_jlt", &result))
		make_access_error("SAM_BatteryStateful", "rainflow_jlt");
	});
	return result;
}



SAM_EXPORT double* SAM_BatteryStateful_StateCell_rainflow_peaks_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "rainflow_peaks", length);
	if (!result)
		make_access_error("SAM_BatteryStateful", "rainflow_peaks");
	});
	return result;
}



SAM_EXPORT double SAM_BatteryStateful_StateCell_range_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "range", &result))
		make_access_error("SAM_BatteryStateful", "range");
	});
	return result;
}



