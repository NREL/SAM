#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_Tcsdish.h"

SAM_EXPORT int SAM_Tcsdish_execute(SAM_table data, int verbosity, SAM_error* err){
	return SAM_module_exec("tcsdish", data, verbosity, err);
}

SAM_EXPORT void SAM_Tcsdish_Weather_file_name_sset(SAM_table ptr, const char* str, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_string(ptr, "file_name", str);
	});
}

SAM_EXPORT void SAM_Tcsdish_Dish_system_capacity_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "system_capacity", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type295_A_proj_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "A_proj", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type295_A_total_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "A_total", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type295_I_cut_in_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "I_cut_in", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type295_d_ap_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "d_ap", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type295_d_ap_test_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "d_ap_test", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type295_ew_dish_sep_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ew_dish_sep", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type295_h_slot_gap_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "h_slot_gap", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type295_n_ew_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "n_ew", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type295_n_ns_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "n_ns", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type295_ns_dish_sep_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ns_dish_sep", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type295_rho_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rho", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type295_slope_ew_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "slope_ew", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type295_slope_ns_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "slope_ns", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type295_test_L_focal_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "test_L_focal", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type295_test_if_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "test_if", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type295_w_slot_gap_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "w_slot_gap", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type295_wind_stow_speed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "wind_stow_speed", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type296_A_absorber_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "A_absorber", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type296_A_wall_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "A_wall", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type296_DELTA_T_DIR_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "DELTA_T_DIR", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type296_DELTA_T_REFLUX_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "DELTA_T_REFLUX", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type296_L_cav_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "L_cav", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type296_L_insulation_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "L_insulation", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type296_P_cav_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "P_cav", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type296_T_heater_head_high_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_heater_head_high", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type296_T_heater_head_low_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_heater_head_low", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type296_alpha_absorber_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "alpha_absorber", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type296_alpha_wall_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "alpha_wall", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type296_d_cav_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "d_cav", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type296_k_insulation_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "k_insulation", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type296_rec_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_type", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type296_transmittance_cover_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "transmittance_cover", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type297_Beale_const_coef_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Beale_const_coef", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type297_Beale_first_coef_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Beale_first_coef", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type297_Beale_fourth_coef_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Beale_fourth_coef", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type297_Beale_square_coef_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Beale_square_coef", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type297_Beale_third_coef_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Beale_third_coef", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type297_Pressure_coef_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Pressure_coef", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type297_Pressure_first_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Pressure_first", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type297_T_compression_in_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_compression_in", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type297_V_displaced_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "V_displaced", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type297_engine_speed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "engine_speed", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type298_P_controls_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "P_controls", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type298_P_tower_fan_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "P_tower_fan", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type298_T_cool_speed2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_cool_speed2", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type298_T_cool_speed3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_cool_speed3", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type298_Tower_water_outlet_temp_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Tower_water_outlet_temp", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type298_b_cooler_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "b_cooler", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type298_b_radiator_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "b_radiator", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type298_cooling_fluid_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cooling_fluid", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type298_cooling_tower_on_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cooling_tower_on", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type298_d_pipe_tower_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "d_pipe_tower", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type298_epsilon_cooler_test_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "epsilon_cooler_test", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type298_epsilon_power_test_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "epsilon_power_test", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type298_epsilon_radiator_test_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "epsilon_radiator_test", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type298_eta_tower_pump_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "eta_tower_pump", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type298_ew_dish_separation_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ew_dish_separation", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type298_fan_control_signal_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fan_control_signal", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type298_fan_speed1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fan_speed1", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type298_fan_speed2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fan_speed2", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type298_fan_speed3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fan_speed3", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type298_ns_dish_separation_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ns_dish_separation", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type298_pump_speed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pump_speed", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type298_system_availability_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "system_availability", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type298_test_P_fan_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "test_P_fan", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type298_test_P_pump_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "test_P_pump", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type298_test_T_fluid_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "test_T_fluid", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type298_test_V_dot_fluid_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "test_V_dot_fluid", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type298_test_cooling_fluid_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "test_cooling_fluid", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type298_test_fan_cfm_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "test_fan_cfm", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type298_test_fan_rho_air_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "test_fan_rho_air", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type298_test_fan_speed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "test_fan_speed", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type298_test_pump_speed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "test_pump_speed", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type298_tower_m_dot_water_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tower_m_dot_water", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type298_tower_m_dot_water_test_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tower_m_dot_water_test", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type298_tower_mode_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tower_mode", number);
	});
}

SAM_EXPORT void SAM_Tcsdish_Type298_tower_pipe_material_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tower_pipe_material", number);
	});
}

SAM_EXPORT const char* SAM_Tcsdish_Weather_file_name_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "file_name");
	if (!result)
		make_access_error("SAM_Tcsdish", "file_name");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Dish_system_capacity_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_capacity", &result))
		make_access_error("SAM_Tcsdish", "system_capacity");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type295_A_proj_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "A_proj", &result))
		make_access_error("SAM_Tcsdish", "A_proj");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type295_A_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "A_total", &result))
		make_access_error("SAM_Tcsdish", "A_total");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type295_I_cut_in_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "I_cut_in", &result))
		make_access_error("SAM_Tcsdish", "I_cut_in");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type295_d_ap_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "d_ap", &result))
		make_access_error("SAM_Tcsdish", "d_ap");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type295_d_ap_test_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "d_ap_test", &result))
		make_access_error("SAM_Tcsdish", "d_ap_test");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type295_ew_dish_sep_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ew_dish_sep", &result))
		make_access_error("SAM_Tcsdish", "ew_dish_sep");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type295_h_slot_gap_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "h_slot_gap", &result))
		make_access_error("SAM_Tcsdish", "h_slot_gap");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type295_n_ew_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "n_ew", &result))
		make_access_error("SAM_Tcsdish", "n_ew");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type295_n_ns_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "n_ns", &result))
		make_access_error("SAM_Tcsdish", "n_ns");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type295_ns_dish_sep_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ns_dish_sep", &result))
		make_access_error("SAM_Tcsdish", "ns_dish_sep");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type295_rho_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rho", &result))
		make_access_error("SAM_Tcsdish", "rho");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type295_slope_ew_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "slope_ew", &result))
		make_access_error("SAM_Tcsdish", "slope_ew");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type295_slope_ns_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "slope_ns", &result))
		make_access_error("SAM_Tcsdish", "slope_ns");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type295_test_L_focal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "test_L_focal", &result))
		make_access_error("SAM_Tcsdish", "test_L_focal");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type295_test_if_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "test_if", &result))
		make_access_error("SAM_Tcsdish", "test_if");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type295_w_slot_gap_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "w_slot_gap", &result))
		make_access_error("SAM_Tcsdish", "w_slot_gap");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type295_wind_stow_speed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "wind_stow_speed", &result))
		make_access_error("SAM_Tcsdish", "wind_stow_speed");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type296_A_absorber_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "A_absorber", &result))
		make_access_error("SAM_Tcsdish", "A_absorber");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type296_A_wall_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "A_wall", &result))
		make_access_error("SAM_Tcsdish", "A_wall");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type296_DELTA_T_DIR_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "DELTA_T_DIR", &result))
		make_access_error("SAM_Tcsdish", "DELTA_T_DIR");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type296_DELTA_T_REFLUX_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "DELTA_T_REFLUX", &result))
		make_access_error("SAM_Tcsdish", "DELTA_T_REFLUX");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type296_L_cav_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "L_cav", &result))
		make_access_error("SAM_Tcsdish", "L_cav");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type296_L_insulation_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "L_insulation", &result))
		make_access_error("SAM_Tcsdish", "L_insulation");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type296_P_cav_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_cav", &result))
		make_access_error("SAM_Tcsdish", "P_cav");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type296_T_heater_head_high_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_heater_head_high", &result))
		make_access_error("SAM_Tcsdish", "T_heater_head_high");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type296_T_heater_head_low_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_heater_head_low", &result))
		make_access_error("SAM_Tcsdish", "T_heater_head_low");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type296_alpha_absorber_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "alpha_absorber", &result))
		make_access_error("SAM_Tcsdish", "alpha_absorber");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type296_alpha_wall_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "alpha_wall", &result))
		make_access_error("SAM_Tcsdish", "alpha_wall");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type296_d_cav_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "d_cav", &result))
		make_access_error("SAM_Tcsdish", "d_cav");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type296_k_insulation_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "k_insulation", &result))
		make_access_error("SAM_Tcsdish", "k_insulation");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type296_rec_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_type", &result))
		make_access_error("SAM_Tcsdish", "rec_type");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type296_transmittance_cover_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "transmittance_cover", &result))
		make_access_error("SAM_Tcsdish", "transmittance_cover");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type297_Beale_const_coef_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Beale_const_coef", &result))
		make_access_error("SAM_Tcsdish", "Beale_const_coef");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type297_Beale_first_coef_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Beale_first_coef", &result))
		make_access_error("SAM_Tcsdish", "Beale_first_coef");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type297_Beale_fourth_coef_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Beale_fourth_coef", &result))
		make_access_error("SAM_Tcsdish", "Beale_fourth_coef");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type297_Beale_square_coef_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Beale_square_coef", &result))
		make_access_error("SAM_Tcsdish", "Beale_square_coef");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type297_Beale_third_coef_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Beale_third_coef", &result))
		make_access_error("SAM_Tcsdish", "Beale_third_coef");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type297_Pressure_coef_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Pressure_coef", &result))
		make_access_error("SAM_Tcsdish", "Pressure_coef");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type297_Pressure_first_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Pressure_first", &result))
		make_access_error("SAM_Tcsdish", "Pressure_first");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type297_T_compression_in_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_compression_in", &result))
		make_access_error("SAM_Tcsdish", "T_compression_in");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type297_V_displaced_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "V_displaced", &result))
		make_access_error("SAM_Tcsdish", "V_displaced");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type297_engine_speed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "engine_speed", &result))
		make_access_error("SAM_Tcsdish", "engine_speed");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type298_P_controls_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_controls", &result))
		make_access_error("SAM_Tcsdish", "P_controls");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type298_P_tower_fan_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_tower_fan", &result))
		make_access_error("SAM_Tcsdish", "P_tower_fan");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type298_T_cool_speed2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_cool_speed2", &result))
		make_access_error("SAM_Tcsdish", "T_cool_speed2");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type298_T_cool_speed3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_cool_speed3", &result))
		make_access_error("SAM_Tcsdish", "T_cool_speed3");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type298_Tower_water_outlet_temp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Tower_water_outlet_temp", &result))
		make_access_error("SAM_Tcsdish", "Tower_water_outlet_temp");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type298_b_cooler_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "b_cooler", &result))
		make_access_error("SAM_Tcsdish", "b_cooler");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type298_b_radiator_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "b_radiator", &result))
		make_access_error("SAM_Tcsdish", "b_radiator");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type298_cooling_fluid_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cooling_fluid", &result))
		make_access_error("SAM_Tcsdish", "cooling_fluid");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type298_cooling_tower_on_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cooling_tower_on", &result))
		make_access_error("SAM_Tcsdish", "cooling_tower_on");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type298_d_pipe_tower_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "d_pipe_tower", &result))
		make_access_error("SAM_Tcsdish", "d_pipe_tower");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type298_epsilon_cooler_test_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "epsilon_cooler_test", &result))
		make_access_error("SAM_Tcsdish", "epsilon_cooler_test");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type298_epsilon_power_test_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "epsilon_power_test", &result))
		make_access_error("SAM_Tcsdish", "epsilon_power_test");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type298_epsilon_radiator_test_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "epsilon_radiator_test", &result))
		make_access_error("SAM_Tcsdish", "epsilon_radiator_test");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type298_eta_tower_pump_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_tower_pump", &result))
		make_access_error("SAM_Tcsdish", "eta_tower_pump");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type298_ew_dish_separation_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ew_dish_separation", &result))
		make_access_error("SAM_Tcsdish", "ew_dish_separation");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type298_fan_control_signal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fan_control_signal", &result))
		make_access_error("SAM_Tcsdish", "fan_control_signal");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type298_fan_speed1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fan_speed1", &result))
		make_access_error("SAM_Tcsdish", "fan_speed1");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type298_fan_speed2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fan_speed2", &result))
		make_access_error("SAM_Tcsdish", "fan_speed2");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type298_fan_speed3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fan_speed3", &result))
		make_access_error("SAM_Tcsdish", "fan_speed3");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type298_ns_dish_separation_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ns_dish_separation", &result))
		make_access_error("SAM_Tcsdish", "ns_dish_separation");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type298_pump_speed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pump_speed", &result))
		make_access_error("SAM_Tcsdish", "pump_speed");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type298_system_availability_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_availability", &result))
		make_access_error("SAM_Tcsdish", "system_availability");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type298_test_P_fan_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "test_P_fan", &result))
		make_access_error("SAM_Tcsdish", "test_P_fan");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type298_test_P_pump_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "test_P_pump", &result))
		make_access_error("SAM_Tcsdish", "test_P_pump");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type298_test_T_fluid_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "test_T_fluid", &result))
		make_access_error("SAM_Tcsdish", "test_T_fluid");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type298_test_V_dot_fluid_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "test_V_dot_fluid", &result))
		make_access_error("SAM_Tcsdish", "test_V_dot_fluid");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type298_test_cooling_fluid_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "test_cooling_fluid", &result))
		make_access_error("SAM_Tcsdish", "test_cooling_fluid");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type298_test_fan_cfm_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "test_fan_cfm", &result))
		make_access_error("SAM_Tcsdish", "test_fan_cfm");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type298_test_fan_rho_air_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "test_fan_rho_air", &result))
		make_access_error("SAM_Tcsdish", "test_fan_rho_air");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type298_test_fan_speed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "test_fan_speed", &result))
		make_access_error("SAM_Tcsdish", "test_fan_speed");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type298_test_pump_speed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "test_pump_speed", &result))
		make_access_error("SAM_Tcsdish", "test_pump_speed");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type298_tower_m_dot_water_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tower_m_dot_water", &result))
		make_access_error("SAM_Tcsdish", "tower_m_dot_water");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type298_tower_m_dot_water_test_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tower_m_dot_water_test", &result))
		make_access_error("SAM_Tcsdish", "tower_m_dot_water_test");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type298_tower_mode_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tower_mode", &result))
		make_access_error("SAM_Tcsdish", "tower_mode");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Type298_tower_pipe_material_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tower_pipe_material", &result))
		make_access_error("SAM_Tcsdish", "tower_pipe_material");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsdish_Outputs_Collector_Losses_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Collector_Losses", length);
	if (!result)
		make_access_error("SAM_Tcsdish", "Collector_Losses");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsdish_Outputs_P_SE_losses_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_SE_losses", length);
	if (!result)
		make_access_error("SAM_Tcsdish", "P_SE_losses");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsdish_Outputs_P_out_SE_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_out_SE", length);
	if (!result)
		make_access_error("SAM_Tcsdish", "P_out_SE");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsdish_Outputs_P_out_rec_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_out_rec", length);
	if (!result)
		make_access_error("SAM_Tcsdish", "P_out_rec");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsdish_Outputs_P_parasitic_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_parasitic", length);
	if (!result)
		make_access_error("SAM_Tcsdish", "P_parasitic");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsdish_Outputs_Phi_shade_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Phi_shade", length);
	if (!result)
		make_access_error("SAM_Tcsdish", "Phi_shade");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsdish_Outputs_Power_in_collector_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Power_in_collector", length);
	if (!result)
		make_access_error("SAM_Tcsdish", "Power_in_collector");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsdish_Outputs_Power_in_rec_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Power_in_rec", length);
	if (!result)
		make_access_error("SAM_Tcsdish", "Power_in_rec");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsdish_Outputs_Power_out_col_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Power_out_col", length);
	if (!result)
		make_access_error("SAM_Tcsdish", "Power_out_col");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsdish_Outputs_Q_rec_losses_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Q_rec_losses", length);
	if (!result)
		make_access_error("SAM_Tcsdish", "Q_rec_losses");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsdish_Outputs_T_compression_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_compression", length);
	if (!result)
		make_access_error("SAM_Tcsdish", "T_compression");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsdish_Outputs_T_heater_head_operate_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_heater_head_operate", length);
	if (!result)
		make_access_error("SAM_Tcsdish", "T_heater_head_operate");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsdish_Outputs_T_tower_in_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_tower_in", length);
	if (!result)
		make_access_error("SAM_Tcsdish", "T_tower_in");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsdish_Outputs_T_tower_out_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_tower_out", length);
	if (!result)
		make_access_error("SAM_Tcsdish", "T_tower_out");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Outputs_annual_Collector_Losses_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_Collector_Losses", &result))
		make_access_error("SAM_Tcsdish", "annual_Collector_Losses");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Outputs_annual_P_out_SE_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_P_out_SE", &result))
		make_access_error("SAM_Tcsdish", "annual_P_out_SE");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Outputs_annual_P_out_rec_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_P_out_rec", &result))
		make_access_error("SAM_Tcsdish", "annual_P_out_rec");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Outputs_annual_P_parasitic_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_P_parasitic", &result))
		make_access_error("SAM_Tcsdish", "annual_P_parasitic");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Outputs_annual_Power_in_collector_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_Power_in_collector", &result))
		make_access_error("SAM_Tcsdish", "annual_Power_in_collector");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Outputs_annual_Power_in_rec_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_Power_in_rec", &result))
		make_access_error("SAM_Tcsdish", "annual_Power_in_rec");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Outputs_annual_Power_out_col_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_Power_out_col", &result))
		make_access_error("SAM_Tcsdish", "annual_Power_out_col");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Outputs_annual_Q_rec_losses_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_Q_rec_losses", &result))
		make_access_error("SAM_Tcsdish", "annual_Q_rec_losses");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Outputs_annual_energy_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_energy", &result))
		make_access_error("SAM_Tcsdish", "annual_energy");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsdish_Outputs_annual_energy_distribution_time_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "annual_energy_distribution_time", nrows, ncols);
	if (!result)
		make_access_error("SAM_Tcsdish", "annual_energy_distribution_time");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsdish_Outputs_beam_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "beam", length);
	if (!result)
		make_access_error("SAM_Tcsdish", "beam");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Outputs_capacity_factor_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "capacity_factor", &result))
		make_access_error("SAM_Tcsdish", "capacity_factor");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Outputs_conversion_factor_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "conversion_factor", &result))
		make_access_error("SAM_Tcsdish", "conversion_factor");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsdish_Outputs_engine_pressure_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "engine_pressure", length);
	if (!result)
		make_access_error("SAM_Tcsdish", "engine_pressure");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsdish_Outputs_eta_SE_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "eta_SE", length);
	if (!result)
		make_access_error("SAM_Tcsdish", "eta_SE");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsdish_Outputs_eta_collector_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "eta_collector", length);
	if (!result)
		make_access_error("SAM_Tcsdish", "eta_collector");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsdish_Outputs_eta_net_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "eta_net", length);
	if (!result)
		make_access_error("SAM_Tcsdish", "eta_net");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsdish_Outputs_eta_rec_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "eta_rec", length);
	if (!result)
		make_access_error("SAM_Tcsdish", "eta_rec");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsdish_Outputs_gen_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "gen", length);
	if (!result)
		make_access_error("SAM_Tcsdish", "gen");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsdish_Outputs_hour_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "hour", length);
	if (!result)
		make_access_error("SAM_Tcsdish", "hour");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsdish_Outputs_hourly_Collector_Losses_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "hourly_Collector_Losses", length);
	if (!result)
		make_access_error("SAM_Tcsdish", "hourly_Collector_Losses");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsdish_Outputs_hourly_P_out_SE_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "hourly_P_out_SE", length);
	if (!result)
		make_access_error("SAM_Tcsdish", "hourly_P_out_SE");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsdish_Outputs_hourly_P_out_rec_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "hourly_P_out_rec", length);
	if (!result)
		make_access_error("SAM_Tcsdish", "hourly_P_out_rec");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsdish_Outputs_hourly_P_parasitic_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "hourly_P_parasitic", length);
	if (!result)
		make_access_error("SAM_Tcsdish", "hourly_P_parasitic");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsdish_Outputs_hourly_Power_in_collector_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "hourly_Power_in_collector", length);
	if (!result)
		make_access_error("SAM_Tcsdish", "hourly_Power_in_collector");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsdish_Outputs_hourly_Power_in_rec_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "hourly_Power_in_rec", length);
	if (!result)
		make_access_error("SAM_Tcsdish", "hourly_Power_in_rec");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsdish_Outputs_hourly_Power_out_col_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "hourly_Power_out_col", length);
	if (!result)
		make_access_error("SAM_Tcsdish", "hourly_Power_out_col");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsdish_Outputs_hourly_Q_rec_losses_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "hourly_Q_rec_losses", length);
	if (!result)
		make_access_error("SAM_Tcsdish", "hourly_Q_rec_losses");
	});
	return result;
}



SAM_EXPORT double SAM_Tcsdish_Outputs_kwh_per_kw_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "kwh_per_kw", &result))
		make_access_error("SAM_Tcsdish", "kwh_per_kw");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsdish_Outputs_month_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "month", length);
	if (!result)
		make_access_error("SAM_Tcsdish", "month");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsdish_Outputs_monthly_Collector_Losses_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_Collector_Losses", length);
	if (!result)
		make_access_error("SAM_Tcsdish", "monthly_Collector_Losses");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsdish_Outputs_monthly_P_out_SE_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_P_out_SE", length);
	if (!result)
		make_access_error("SAM_Tcsdish", "monthly_P_out_SE");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsdish_Outputs_monthly_P_out_rec_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_P_out_rec", length);
	if (!result)
		make_access_error("SAM_Tcsdish", "monthly_P_out_rec");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsdish_Outputs_monthly_P_parasitic_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_P_parasitic", length);
	if (!result)
		make_access_error("SAM_Tcsdish", "monthly_P_parasitic");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsdish_Outputs_monthly_Power_in_collector_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_Power_in_collector", length);
	if (!result)
		make_access_error("SAM_Tcsdish", "monthly_Power_in_collector");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsdish_Outputs_monthly_Power_in_rec_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_Power_in_rec", length);
	if (!result)
		make_access_error("SAM_Tcsdish", "monthly_Power_in_rec");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsdish_Outputs_monthly_Power_out_col_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_Power_out_col", length);
	if (!result)
		make_access_error("SAM_Tcsdish", "monthly_Power_out_col");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsdish_Outputs_monthly_Q_rec_losses_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_Q_rec_losses", length);
	if (!result)
		make_access_error("SAM_Tcsdish", "monthly_Q_rec_losses");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsdish_Outputs_monthly_energy_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_energy", length);
	if (!result)
		make_access_error("SAM_Tcsdish", "monthly_energy");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsdish_Outputs_net_power_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "net_power", length);
	if (!result)
		make_access_error("SAM_Tcsdish", "net_power");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsdish_Outputs_pres_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pres", length);
	if (!result)
		make_access_error("SAM_Tcsdish", "pres");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsdish_Outputs_solazi_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "solazi", length);
	if (!result)
		make_access_error("SAM_Tcsdish", "solazi");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsdish_Outputs_solzen_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "solzen", length);
	if (!result)
		make_access_error("SAM_Tcsdish", "solzen");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsdish_Outputs_tdry_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "tdry", length);
	if (!result)
		make_access_error("SAM_Tcsdish", "tdry");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsdish_Outputs_twet_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "twet", length);
	if (!result)
		make_access_error("SAM_Tcsdish", "twet");
	});
	return result;
}



SAM_EXPORT double* SAM_Tcsdish_Outputs_wspd_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "wspd", length);
	if (!result)
		make_access_error("SAM_Tcsdish", "wspd");
	});
	return result;
}



