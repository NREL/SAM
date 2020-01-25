#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_UiTesCalcs.h"

SAM_EXPORT SAM_UiTesCalcs SAM_UiTesCalcs_construct(const char* def, SAM_error* err){
	SAM_UiTesCalcs result = nullptr;
	translateExceptions(err, [&]{
		result = ssc_data_create();
	});
	return result;
}

SAM_EXPORT int SAM_UiTesCalcs_execute(SAM_UiTesCalcs data, int verbosity, SAM_error* err){
	int n_err = 0;
	translateExceptions(err, [&]{
		n_err += SAM_module_exec("ui_tes_calcs", data, verbosity, err);
	});
	return n_err;
}


SAM_EXPORT void SAM_UiTesCalcs_destruct(SAM_UiTesCalcs system)
{
	ssc_data_free(system);
}

SAM_EXPORT void SAM_UiTesCalcs_Common_TES_HTF_code_nset(SAM_UiTesCalcs ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "TES_HTF_code", number);
	});
}

SAM_EXPORT void SAM_UiTesCalcs_Common_TES_HTF_props_mset(SAM_UiTesCalcs ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "TES_HTF_props", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_UiTesCalcs_Common_T_HTF_cold_nset(SAM_UiTesCalcs ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_HTF_cold", number);
	});
}

SAM_EXPORT void SAM_UiTesCalcs_Common_T_HTF_hot_nset(SAM_UiTesCalcs ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_HTF_hot", number);
	});
}

SAM_EXPORT void SAM_UiTesCalcs_Common_W_dot_pb_des_nset(SAM_UiTesCalcs ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "W_dot_pb_des", number);
	});
}

SAM_EXPORT void SAM_UiTesCalcs_Common_eta_pb_des_nset(SAM_UiTesCalcs ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "eta_pb_des", number);
	});
}

SAM_EXPORT void SAM_UiTesCalcs_Common_h_tank_nset(SAM_UiTesCalcs ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "h_tank", number);
	});
}

SAM_EXPORT void SAM_UiTesCalcs_Common_h_tank_min_nset(SAM_UiTesCalcs ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "h_tank_min", number);
	});
}

SAM_EXPORT void SAM_UiTesCalcs_Common_tank_pairs_nset(SAM_UiTesCalcs ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tank_pairs", number);
	});
}

SAM_EXPORT void SAM_UiTesCalcs_Common_tes_hrs_nset(SAM_UiTesCalcs ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tes_hrs", number);
	});
}

SAM_EXPORT void SAM_UiTesCalcs_Common_u_tank_nset(SAM_UiTesCalcs ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "u_tank", number);
	});
}

SAM_EXPORT double SAM_UiTesCalcs_Common_TES_HTF_code_nget(SAM_UiTesCalcs ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "TES_HTF_code", &result))
		make_access_error("SAM_UiTesCalcs", "TES_HTF_code");
	});
	return result;
}



SAM_EXPORT double* SAM_UiTesCalcs_Common_TES_HTF_props_mget(SAM_UiTesCalcs ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "TES_HTF_props", nrows, ncols);
	if (!result)
		make_access_error("SAM_UiTesCalcs", "TES_HTF_props");
	});
	return result;
}



SAM_EXPORT double SAM_UiTesCalcs_Common_T_HTF_cold_nget(SAM_UiTesCalcs ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_HTF_cold", &result))
		make_access_error("SAM_UiTesCalcs", "T_HTF_cold");
	});
	return result;
}



SAM_EXPORT double SAM_UiTesCalcs_Common_T_HTF_hot_nget(SAM_UiTesCalcs ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_HTF_hot", &result))
		make_access_error("SAM_UiTesCalcs", "T_HTF_hot");
	});
	return result;
}



SAM_EXPORT double SAM_UiTesCalcs_Common_W_dot_pb_des_nget(SAM_UiTesCalcs ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "W_dot_pb_des", &result))
		make_access_error("SAM_UiTesCalcs", "W_dot_pb_des");
	});
	return result;
}



SAM_EXPORT double SAM_UiTesCalcs_Common_eta_pb_des_nget(SAM_UiTesCalcs ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_pb_des", &result))
		make_access_error("SAM_UiTesCalcs", "eta_pb_des");
	});
	return result;
}



SAM_EXPORT double SAM_UiTesCalcs_Common_h_tank_nget(SAM_UiTesCalcs ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "h_tank", &result))
		make_access_error("SAM_UiTesCalcs", "h_tank");
	});
	return result;
}



SAM_EXPORT double SAM_UiTesCalcs_Common_h_tank_min_nget(SAM_UiTesCalcs ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "h_tank_min", &result))
		make_access_error("SAM_UiTesCalcs", "h_tank_min");
	});
	return result;
}



SAM_EXPORT double SAM_UiTesCalcs_Common_tank_pairs_nget(SAM_UiTesCalcs ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tank_pairs", &result))
		make_access_error("SAM_UiTesCalcs", "tank_pairs");
	});
	return result;
}



SAM_EXPORT double SAM_UiTesCalcs_Common_tes_hrs_nget(SAM_UiTesCalcs ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_hrs", &result))
		make_access_error("SAM_UiTesCalcs", "tes_hrs");
	});
	return result;
}



SAM_EXPORT double SAM_UiTesCalcs_Common_u_tank_nget(SAM_UiTesCalcs ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "u_tank", &result))
		make_access_error("SAM_UiTesCalcs", "u_tank");
	});
	return result;
}



SAM_EXPORT double SAM_UiTesCalcs_Outputs_HTF_dens_nget(SAM_UiTesCalcs ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "HTF_dens", &result))
		make_access_error("SAM_UiTesCalcs", "HTF_dens");
	});
	return result;
}



SAM_EXPORT double SAM_UiTesCalcs_Outputs_d_tank_nget(SAM_UiTesCalcs ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "d_tank", &result))
		make_access_error("SAM_UiTesCalcs", "d_tank");
	});
	return result;
}



SAM_EXPORT double SAM_UiTesCalcs_Outputs_q_dot_loss_nget(SAM_UiTesCalcs ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_dot_loss", &result))
		make_access_error("SAM_UiTesCalcs", "q_dot_loss");
	});
	return result;
}



SAM_EXPORT double SAM_UiTesCalcs_Outputs_q_tes_des_nget(SAM_UiTesCalcs ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_tes_des", &result))
		make_access_error("SAM_UiTesCalcs", "q_tes_des");
	});
	return result;
}



SAM_EXPORT double SAM_UiTesCalcs_Outputs_vol_one_temp_avail_nget(SAM_UiTesCalcs ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "vol_one_temp_avail", &result))
		make_access_error("SAM_UiTesCalcs", "vol_one_temp_avail");
	});
	return result;
}



SAM_EXPORT double SAM_UiTesCalcs_Outputs_vol_one_temp_total_nget(SAM_UiTesCalcs ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "vol_one_temp_total", &result))
		make_access_error("SAM_UiTesCalcs", "vol_one_temp_total");
	});
	return result;
}



