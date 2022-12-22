#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_MsptSfAndRecIsolated.h"

SAM_EXPORT int SAM_MsptSfAndRecIsolated_execute(SAM_table data, int verbosity, SAM_error* err){
	return SAM_module_exec("mspt_sf_and_rec_isolated", data, verbosity, err);
}

SAM_EXPORT void SAM_MsptSfAndRecIsolated_Simulation_sim_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sim_type", number);
	});
}

SAM_EXPORT void SAM_MsptSfAndRecIsolated_TowerAndReceiver_D_rec_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "D_rec", number);
	});
}

SAM_EXPORT void SAM_MsptSfAndRecIsolated_TowerAndReceiver_Flow_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Flow_type", number);
	});
}

SAM_EXPORT void SAM_MsptSfAndRecIsolated_TowerAndReceiver_N_panels_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "N_panels", number);
	});
}

SAM_EXPORT void SAM_MsptSfAndRecIsolated_TowerAndReceiver_T_htf_cold_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_htf_cold_des", number);
	});
}

SAM_EXPORT void SAM_MsptSfAndRecIsolated_TowerAndReceiver_T_htf_hot_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_htf_hot_des", number);
	});
}

SAM_EXPORT void SAM_MsptSfAndRecIsolated_TowerAndReceiver_crossover_shift_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "crossover_shift", number);
	});
}

SAM_EXPORT void SAM_MsptSfAndRecIsolated_TowerAndReceiver_csp_pt_rec_max_oper_frac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "csp.pt.rec.max_oper_frac", number);
	});
}

SAM_EXPORT void SAM_MsptSfAndRecIsolated_TowerAndReceiver_d_tube_out_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "d_tube_out", number);
	});
}

SAM_EXPORT void SAM_MsptSfAndRecIsolated_TowerAndReceiver_downc_tm_mult_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "downc_tm_mult", number);
	});
}

SAM_EXPORT void SAM_MsptSfAndRecIsolated_TowerAndReceiver_epsilon_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "epsilon", number);
	});
}

SAM_EXPORT void SAM_MsptSfAndRecIsolated_TowerAndReceiver_eta_pump_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "eta_pump", number);
	});
}

SAM_EXPORT void SAM_MsptSfAndRecIsolated_TowerAndReceiver_f_rec_min_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "f_rec_min", number);
	});
}

SAM_EXPORT void SAM_MsptSfAndRecIsolated_TowerAndReceiver_field_fl_props_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "field_fl_props", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_MsptSfAndRecIsolated_TowerAndReceiver_h_tower_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "h_tower", number);
	});
}

SAM_EXPORT void SAM_MsptSfAndRecIsolated_TowerAndReceiver_hl_ffact_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hl_ffact", number);
	});
}

SAM_EXPORT void SAM_MsptSfAndRecIsolated_TowerAndReceiver_is_rec_clearsky_control_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "is_rec_clearsky_control", number);
	});
}

SAM_EXPORT void SAM_MsptSfAndRecIsolated_TowerAndReceiver_is_rec_model_trans_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "is_rec_model_trans", number);
	});
}

SAM_EXPORT void SAM_MsptSfAndRecIsolated_TowerAndReceiver_mat_tube_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mat_tube", number);
	});
}

SAM_EXPORT void SAM_MsptSfAndRecIsolated_TowerAndReceiver_piping_length_const_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "piping_length_const", number);
	});
}

SAM_EXPORT void SAM_MsptSfAndRecIsolated_TowerAndReceiver_piping_length_mult_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "piping_length_mult", number);
	});
}

SAM_EXPORT void SAM_MsptSfAndRecIsolated_TowerAndReceiver_piping_loss_coefficient_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "piping_loss_coefficient", number);
	});
}

SAM_EXPORT void SAM_MsptSfAndRecIsolated_TowerAndReceiver_q_dot_rec_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "q_dot_rec_des", number);
	});
}

SAM_EXPORT void SAM_MsptSfAndRecIsolated_TowerAndReceiver_rec_height_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_height", number);
	});
}

SAM_EXPORT void SAM_MsptSfAndRecIsolated_TowerAndReceiver_rec_htf_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_htf", number);
	});
}

SAM_EXPORT void SAM_MsptSfAndRecIsolated_TowerAndReceiver_rec_qf_delay_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_qf_delay", number);
	});
}

SAM_EXPORT void SAM_MsptSfAndRecIsolated_TowerAndReceiver_rec_su_delay_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_su_delay", number);
	});
}

SAM_EXPORT void SAM_MsptSfAndRecIsolated_TowerAndReceiver_rec_tm_mult_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_tm_mult", number);
	});
}

SAM_EXPORT void SAM_MsptSfAndRecIsolated_TowerAndReceiver_riser_tm_mult_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "riser_tm_mult", number);
	});
}

SAM_EXPORT void SAM_MsptSfAndRecIsolated_TowerAndReceiver_th_riser_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "th_riser", number);
	});
}

SAM_EXPORT void SAM_MsptSfAndRecIsolated_TowerAndReceiver_th_tube_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "th_tube", number);
	});
}

SAM_EXPORT void SAM_MsptSfAndRecIsolated_TowerAndReceiver_u_riser_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "u_riser", number);
	});
}

SAM_EXPORT void SAM_MsptSfAndRecIsolated_ReceiverControl_T_htf_cold_in_od_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "T_htf_cold_in_od", arr, length);
	});
}

SAM_EXPORT void SAM_MsptSfAndRecIsolated_ReceiverControl_plant_defocus_od_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "plant_defocus_od", arr, length);
	});
}

SAM_EXPORT void SAM_MsptSfAndRecIsolated_ReceiverControl_rec_clearsky_fraction_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rec_clearsky_fraction", number);
	});
}

SAM_EXPORT void SAM_MsptSfAndRecIsolated_Timeseries_timestep_od_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "timestep_od", arr, length);
	});
}

SAM_EXPORT void SAM_MsptSfAndRecIsolated_Weather_P_amb_od_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "P_amb_od", arr, length);
	});
}

SAM_EXPORT void SAM_MsptSfAndRecIsolated_Weather_T_amb_od_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "T_amb_od", arr, length);
	});
}

SAM_EXPORT void SAM_MsptSfAndRecIsolated_Weather_clearsky_to_measured_dni_od_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "clearsky_to_measured_dni_od", arr, length);
	});
}

SAM_EXPORT void SAM_MsptSfAndRecIsolated_Weather_deltaT_sky_od_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "deltaT_sky_od", arr, length);
	});
}

SAM_EXPORT void SAM_MsptSfAndRecIsolated_Weather_v_wind_10_od_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "v_wind_10_od", arr, length);
	});
}

SAM_EXPORT void SAM_MsptSfAndRecIsolated_Flux_flux_map_od_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "flux_map_od", mat, nrows, ncols);
	});
}

SAM_EXPORT double SAM_MsptSfAndRecIsolated_Simulation_sim_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sim_type", &result))
		make_access_error("SAM_MsptSfAndRecIsolated", "sim_type");
	});
	return result;
}



SAM_EXPORT double SAM_MsptSfAndRecIsolated_TowerAndReceiver_D_rec_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "D_rec", &result))
		make_access_error("SAM_MsptSfAndRecIsolated", "D_rec");
	});
	return result;
}



SAM_EXPORT double SAM_MsptSfAndRecIsolated_TowerAndReceiver_Flow_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Flow_type", &result))
		make_access_error("SAM_MsptSfAndRecIsolated", "Flow_type");
	});
	return result;
}



SAM_EXPORT double SAM_MsptSfAndRecIsolated_TowerAndReceiver_N_panels_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "N_panels", &result))
		make_access_error("SAM_MsptSfAndRecIsolated", "N_panels");
	});
	return result;
}



SAM_EXPORT double SAM_MsptSfAndRecIsolated_TowerAndReceiver_T_htf_cold_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_htf_cold_des", &result))
		make_access_error("SAM_MsptSfAndRecIsolated", "T_htf_cold_des");
	});
	return result;
}



SAM_EXPORT double SAM_MsptSfAndRecIsolated_TowerAndReceiver_T_htf_hot_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_htf_hot_des", &result))
		make_access_error("SAM_MsptSfAndRecIsolated", "T_htf_hot_des");
	});
	return result;
}



SAM_EXPORT double SAM_MsptSfAndRecIsolated_TowerAndReceiver_crossover_shift_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "crossover_shift", &result))
		make_access_error("SAM_MsptSfAndRecIsolated", "crossover_shift");
	});
	return result;
}



SAM_EXPORT double SAM_MsptSfAndRecIsolated_TowerAndReceiver_csp_pt_rec_max_oper_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp.pt.rec.max_oper_frac", &result))
		make_access_error("SAM_MsptSfAndRecIsolated", "csp.pt.rec.max_oper_frac");
	});
	return result;
}



SAM_EXPORT double SAM_MsptSfAndRecIsolated_TowerAndReceiver_d_tube_out_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "d_tube_out", &result))
		make_access_error("SAM_MsptSfAndRecIsolated", "d_tube_out");
	});
	return result;
}



SAM_EXPORT double SAM_MsptSfAndRecIsolated_TowerAndReceiver_downc_tm_mult_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "downc_tm_mult", &result))
		make_access_error("SAM_MsptSfAndRecIsolated", "downc_tm_mult");
	});
	return result;
}



SAM_EXPORT double SAM_MsptSfAndRecIsolated_TowerAndReceiver_epsilon_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "epsilon", &result))
		make_access_error("SAM_MsptSfAndRecIsolated", "epsilon");
	});
	return result;
}



SAM_EXPORT double SAM_MsptSfAndRecIsolated_TowerAndReceiver_eta_pump_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_pump", &result))
		make_access_error("SAM_MsptSfAndRecIsolated", "eta_pump");
	});
	return result;
}



SAM_EXPORT double SAM_MsptSfAndRecIsolated_TowerAndReceiver_f_rec_min_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "f_rec_min", &result))
		make_access_error("SAM_MsptSfAndRecIsolated", "f_rec_min");
	});
	return result;
}



SAM_EXPORT double* SAM_MsptSfAndRecIsolated_TowerAndReceiver_field_fl_props_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "field_fl_props", nrows, ncols);
	if (!result)
		make_access_error("SAM_MsptSfAndRecIsolated", "field_fl_props");
	});
	return result;
}



SAM_EXPORT double SAM_MsptSfAndRecIsolated_TowerAndReceiver_h_tower_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "h_tower", &result))
		make_access_error("SAM_MsptSfAndRecIsolated", "h_tower");
	});
	return result;
}



SAM_EXPORT double SAM_MsptSfAndRecIsolated_TowerAndReceiver_hl_ffact_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hl_ffact", &result))
		make_access_error("SAM_MsptSfAndRecIsolated", "hl_ffact");
	});
	return result;
}



SAM_EXPORT double SAM_MsptSfAndRecIsolated_TowerAndReceiver_is_rec_clearsky_control_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "is_rec_clearsky_control", &result))
		make_access_error("SAM_MsptSfAndRecIsolated", "is_rec_clearsky_control");
	});
	return result;
}



SAM_EXPORT double SAM_MsptSfAndRecIsolated_TowerAndReceiver_is_rec_model_trans_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "is_rec_model_trans", &result))
		make_access_error("SAM_MsptSfAndRecIsolated", "is_rec_model_trans");
	});
	return result;
}



SAM_EXPORT double SAM_MsptSfAndRecIsolated_TowerAndReceiver_mat_tube_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mat_tube", &result))
		make_access_error("SAM_MsptSfAndRecIsolated", "mat_tube");
	});
	return result;
}



SAM_EXPORT double SAM_MsptSfAndRecIsolated_TowerAndReceiver_piping_length_const_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "piping_length_const", &result))
		make_access_error("SAM_MsptSfAndRecIsolated", "piping_length_const");
	});
	return result;
}



SAM_EXPORT double SAM_MsptSfAndRecIsolated_TowerAndReceiver_piping_length_mult_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "piping_length_mult", &result))
		make_access_error("SAM_MsptSfAndRecIsolated", "piping_length_mult");
	});
	return result;
}



SAM_EXPORT double SAM_MsptSfAndRecIsolated_TowerAndReceiver_piping_loss_coefficient_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "piping_loss_coefficient", &result))
		make_access_error("SAM_MsptSfAndRecIsolated", "piping_loss_coefficient");
	});
	return result;
}



SAM_EXPORT double SAM_MsptSfAndRecIsolated_TowerAndReceiver_q_dot_rec_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_dot_rec_des", &result))
		make_access_error("SAM_MsptSfAndRecIsolated", "q_dot_rec_des");
	});
	return result;
}



SAM_EXPORT double SAM_MsptSfAndRecIsolated_TowerAndReceiver_rec_height_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_height", &result))
		make_access_error("SAM_MsptSfAndRecIsolated", "rec_height");
	});
	return result;
}



SAM_EXPORT double SAM_MsptSfAndRecIsolated_TowerAndReceiver_rec_htf_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_htf", &result))
		make_access_error("SAM_MsptSfAndRecIsolated", "rec_htf");
	});
	return result;
}



SAM_EXPORT double SAM_MsptSfAndRecIsolated_TowerAndReceiver_rec_qf_delay_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_qf_delay", &result))
		make_access_error("SAM_MsptSfAndRecIsolated", "rec_qf_delay");
	});
	return result;
}



SAM_EXPORT double SAM_MsptSfAndRecIsolated_TowerAndReceiver_rec_su_delay_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_su_delay", &result))
		make_access_error("SAM_MsptSfAndRecIsolated", "rec_su_delay");
	});
	return result;
}



SAM_EXPORT double SAM_MsptSfAndRecIsolated_TowerAndReceiver_rec_tm_mult_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_tm_mult", &result))
		make_access_error("SAM_MsptSfAndRecIsolated", "rec_tm_mult");
	});
	return result;
}



SAM_EXPORT double SAM_MsptSfAndRecIsolated_TowerAndReceiver_riser_tm_mult_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "riser_tm_mult", &result))
		make_access_error("SAM_MsptSfAndRecIsolated", "riser_tm_mult");
	});
	return result;
}



SAM_EXPORT double SAM_MsptSfAndRecIsolated_TowerAndReceiver_th_riser_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "th_riser", &result))
		make_access_error("SAM_MsptSfAndRecIsolated", "th_riser");
	});
	return result;
}



SAM_EXPORT double SAM_MsptSfAndRecIsolated_TowerAndReceiver_th_tube_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "th_tube", &result))
		make_access_error("SAM_MsptSfAndRecIsolated", "th_tube");
	});
	return result;
}



SAM_EXPORT double SAM_MsptSfAndRecIsolated_TowerAndReceiver_u_riser_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "u_riser", &result))
		make_access_error("SAM_MsptSfAndRecIsolated", "u_riser");
	});
	return result;
}



SAM_EXPORT double* SAM_MsptSfAndRecIsolated_ReceiverControl_T_htf_cold_in_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_htf_cold_in_od", length);
	if (!result)
		make_access_error("SAM_MsptSfAndRecIsolated", "T_htf_cold_in_od");
	});
	return result;
}



SAM_EXPORT double* SAM_MsptSfAndRecIsolated_ReceiverControl_plant_defocus_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "plant_defocus_od", length);
	if (!result)
		make_access_error("SAM_MsptSfAndRecIsolated", "plant_defocus_od");
	});
	return result;
}



SAM_EXPORT double SAM_MsptSfAndRecIsolated_ReceiverControl_rec_clearsky_fraction_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rec_clearsky_fraction", &result))
		make_access_error("SAM_MsptSfAndRecIsolated", "rec_clearsky_fraction");
	});
	return result;
}



SAM_EXPORT double* SAM_MsptSfAndRecIsolated_Timeseries_timestep_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "timestep_od", length);
	if (!result)
		make_access_error("SAM_MsptSfAndRecIsolated", "timestep_od");
	});
	return result;
}



SAM_EXPORT double* SAM_MsptSfAndRecIsolated_Weather_P_amb_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_amb_od", length);
	if (!result)
		make_access_error("SAM_MsptSfAndRecIsolated", "P_amb_od");
	});
	return result;
}



SAM_EXPORT double* SAM_MsptSfAndRecIsolated_Weather_T_amb_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_amb_od", length);
	if (!result)
		make_access_error("SAM_MsptSfAndRecIsolated", "T_amb_od");
	});
	return result;
}



SAM_EXPORT double* SAM_MsptSfAndRecIsolated_Weather_clearsky_to_measured_dni_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "clearsky_to_measured_dni_od", length);
	if (!result)
		make_access_error("SAM_MsptSfAndRecIsolated", "clearsky_to_measured_dni_od");
	});
	return result;
}



SAM_EXPORT double* SAM_MsptSfAndRecIsolated_Weather_deltaT_sky_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "deltaT_sky_od", length);
	if (!result)
		make_access_error("SAM_MsptSfAndRecIsolated", "deltaT_sky_od");
	});
	return result;
}



SAM_EXPORT double* SAM_MsptSfAndRecIsolated_Weather_v_wind_10_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "v_wind_10_od", length);
	if (!result)
		make_access_error("SAM_MsptSfAndRecIsolated", "v_wind_10_od");
	});
	return result;
}



SAM_EXPORT double* SAM_MsptSfAndRecIsolated_Flux_flux_map_od_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "flux_map_od", nrows, ncols);
	if (!result)
		make_access_error("SAM_MsptSfAndRecIsolated", "flux_map_od");
	});
	return result;
}



SAM_EXPORT double* SAM_MsptSfAndRecIsolated_Outputs_T_htf_rec_out_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_htf_rec_out_od", length);
	if (!result)
		make_access_error("SAM_MsptSfAndRecIsolated", "T_htf_rec_out_od");
	});
	return result;
}



SAM_EXPORT double* SAM_MsptSfAndRecIsolated_Outputs_W_dot_pump_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_dot_pump_od", length);
	if (!result)
		make_access_error("SAM_MsptSfAndRecIsolated", "W_dot_pump_od");
	});
	return result;
}



SAM_EXPORT double* SAM_MsptSfAndRecIsolated_Outputs_eta_rec_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "eta_rec_od", length);
	if (!result)
		make_access_error("SAM_MsptSfAndRecIsolated", "eta_rec_od");
	});
	return result;
}



SAM_EXPORT double SAM_MsptSfAndRecIsolated_Outputs_m_dot_rec_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "m_dot_rec_des", &result))
		make_access_error("SAM_MsptSfAndRecIsolated", "m_dot_rec_des");
	});
	return result;
}



SAM_EXPORT double* SAM_MsptSfAndRecIsolated_Outputs_m_dot_rec_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_rec_od", length);
	if (!result)
		make_access_error("SAM_MsptSfAndRecIsolated", "m_dot_rec_od");
	});
	return result;
}



SAM_EXPORT double* SAM_MsptSfAndRecIsolated_Outputs_q_dot_htf_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_htf_od", length);
	if (!result)
		make_access_error("SAM_MsptSfAndRecIsolated", "q_dot_htf_od");
	});
	return result;
}



SAM_EXPORT double* SAM_MsptSfAndRecIsolated_Outputs_q_dot_rec_conv_loss_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_rec_conv_loss", length);
	if (!result)
		make_access_error("SAM_MsptSfAndRecIsolated", "q_dot_rec_conv_loss");
	});
	return result;
}



SAM_EXPORT double* SAM_MsptSfAndRecIsolated_Outputs_q_dot_rec_inc_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_rec_inc", length);
	if (!result)
		make_access_error("SAM_MsptSfAndRecIsolated", "q_dot_rec_inc");
	});
	return result;
}



SAM_EXPORT double* SAM_MsptSfAndRecIsolated_Outputs_q_dot_rec_inc_pre_defocus_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_rec_inc_pre_defocus", length);
	if (!result)
		make_access_error("SAM_MsptSfAndRecIsolated", "q_dot_rec_inc_pre_defocus");
	});
	return result;
}



SAM_EXPORT double* SAM_MsptSfAndRecIsolated_Outputs_q_dot_rec_piping_loss_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_rec_piping_loss", length);
	if (!result)
		make_access_error("SAM_MsptSfAndRecIsolated", "q_dot_rec_piping_loss");
	});
	return result;
}



SAM_EXPORT double* SAM_MsptSfAndRecIsolated_Outputs_q_dot_rec_rad_loss_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_rec_rad_loss", length);
	if (!result)
		make_access_error("SAM_MsptSfAndRecIsolated", "q_dot_rec_rad_loss");
	});
	return result;
}



SAM_EXPORT double* SAM_MsptSfAndRecIsolated_Outputs_rec_component_defocus_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "rec_component_defocus_od", length);
	if (!result)
		make_access_error("SAM_MsptSfAndRecIsolated", "rec_component_defocus_od");
	});
	return result;
}



