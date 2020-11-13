#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_Sco2AirCooler.h"

SAM_EXPORT int SAM_Sco2AirCooler_execute(SAM_table data, int verbosity, SAM_error* err){
	int n_err = 0;
	translateExceptions(err, [&]{
		n_err += SAM_module_exec("sco2_air_cooler", data, verbosity, err);
	});
	return n_err;
}


SAM_EXPORT void SAM_Sco2AirCooler_Common_P_co2_hot_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "P_co2_hot_des", number);
	});
}

SAM_EXPORT void SAM_Sco2AirCooler_Common_T_amb_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_amb_des", number);
	});
}

SAM_EXPORT void SAM_Sco2AirCooler_Common_T_co2_cold_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_co2_cold_des", number);
	});
}

SAM_EXPORT void SAM_Sco2AirCooler_Common_T_co2_hot_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_co2_hot_des", number);
	});
}

SAM_EXPORT void SAM_Sco2AirCooler_Common_W_dot_fan_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "W_dot_fan_des", number);
	});
}

SAM_EXPORT void SAM_Sco2AirCooler_Common_deltaP_co2_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "deltaP_co2_des", number);
	});
}

SAM_EXPORT void SAM_Sco2AirCooler_Common_od_calc_T_co2_cold_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "od_calc_T_co2_cold", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Sco2AirCooler_Common_od_calc_W_dot_fan_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "od_calc_W_dot_fan", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Sco2AirCooler_Common_q_dot_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "q_dot_des", number);
	});
}

SAM_EXPORT void SAM_Sco2AirCooler_Common_site_elevation_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "site_elevation", number);
	});
}

SAM_EXPORT double SAM_Sco2AirCooler_Common_P_co2_hot_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_co2_hot_des", &result))
		make_access_error("SAM_Sco2AirCooler", "P_co2_hot_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2AirCooler_Common_T_amb_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_amb_des", &result))
		make_access_error("SAM_Sco2AirCooler", "T_amb_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2AirCooler_Common_T_co2_cold_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_co2_cold_des", &result))
		make_access_error("SAM_Sco2AirCooler", "T_co2_cold_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2AirCooler_Common_T_co2_hot_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_co2_hot_des", &result))
		make_access_error("SAM_Sco2AirCooler", "T_co2_hot_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2AirCooler_Common_W_dot_fan_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "W_dot_fan_des", &result))
		make_access_error("SAM_Sco2AirCooler", "W_dot_fan_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2AirCooler_Common_deltaP_co2_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "deltaP_co2_des", &result))
		make_access_error("SAM_Sco2AirCooler", "deltaP_co2_des");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2AirCooler_Common_od_calc_T_co2_cold_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "od_calc_T_co2_cold", nrows, ncols);
	if (!result)
		make_access_error("SAM_Sco2AirCooler", "od_calc_T_co2_cold");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2AirCooler_Common_od_calc_W_dot_fan_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "od_calc_W_dot_fan", nrows, ncols);
	if (!result)
		make_access_error("SAM_Sco2AirCooler", "od_calc_W_dot_fan");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2AirCooler_Common_q_dot_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_dot_des", &result))
		make_access_error("SAM_Sco2AirCooler", "q_dot_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2AirCooler_Common_site_elevation_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "site_elevation", &result))
		make_access_error("SAM_Sco2AirCooler", "site_elevation");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2AirCooler_Outputs_P_co2_cold_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_co2_cold_od", length);
	if (!result)
		make_access_error("SAM_Sco2AirCooler", "P_co2_cold_od");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2AirCooler_Outputs_P_co2_hot_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_co2_hot_od", length);
	if (!result)
		make_access_error("SAM_Sco2AirCooler", "P_co2_hot_od");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2AirCooler_Outputs_T_amb_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_amb_od", length);
	if (!result)
		make_access_error("SAM_Sco2AirCooler", "T_amb_od");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2AirCooler_Outputs_T_co2_cold_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_co2_cold_od", length);
	if (!result)
		make_access_error("SAM_Sco2AirCooler", "T_co2_cold_od");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2AirCooler_Outputs_T_co2_hot_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_co2_hot_od", length);
	if (!result)
		make_access_error("SAM_Sco2AirCooler", "T_co2_hot_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2AirCooler_Outputs_UA_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "UA_total", &result))
		make_access_error("SAM_Sco2AirCooler", "UA_total");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2AirCooler_Outputs_W_dot_fan_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_dot_fan_od", length);
	if (!result)
		make_access_error("SAM_Sco2AirCooler", "W_dot_fan_od");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2AirCooler_Outputs_W_dot_fan_od_ND_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_dot_fan_od_ND", length);
	if (!result)
		make_access_error("SAM_Sco2AirCooler", "W_dot_fan_od_ND");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2AirCooler_Outputs_d_tube_in_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "d_tube_in", &result))
		make_access_error("SAM_Sco2AirCooler", "d_tube_in");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2AirCooler_Outputs_d_tube_out_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "d_tube_out", &result))
		make_access_error("SAM_Sco2AirCooler", "d_tube_out");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2AirCooler_Outputs_deltaP_co2_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "deltaP_co2_od", length);
	if (!result)
		make_access_error("SAM_Sco2AirCooler", "deltaP_co2_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2AirCooler_Outputs_depth_footprint_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "depth_footprint", &result))
		make_access_error("SAM_Sco2AirCooler", "depth_footprint");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2AirCooler_Outputs_length_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "length", &result))
		make_access_error("SAM_Sco2AirCooler", "length");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2AirCooler_Outputs_m_V_hx_material_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "m_V_hx_material", &result))
		make_access_error("SAM_Sco2AirCooler", "m_V_hx_material");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2AirCooler_Outputs_m_dot_co2_od_ND_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_co2_od_ND", length);
	if (!result)
		make_access_error("SAM_Sco2AirCooler", "m_dot_co2_od_ND");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2AirCooler_Outputs_n_passes_series_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "n_passes_series", &result))
		make_access_error("SAM_Sco2AirCooler", "n_passes_series");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2AirCooler_Outputs_number_of_tubes_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "number_of_tubes", &result))
		make_access_error("SAM_Sco2AirCooler", "number_of_tubes");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2AirCooler_Outputs_parallel_paths_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "parallel_paths", &result))
		make_access_error("SAM_Sco2AirCooler", "parallel_paths");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2AirCooler_Outputs_q_dot_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_od", length);
	if (!result)
		make_access_error("SAM_Sco2AirCooler", "q_dot_od");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2AirCooler_Outputs_q_dot_od_ND_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_od_ND", length);
	if (!result)
		make_access_error("SAM_Sco2AirCooler", "q_dot_od_ND");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2AirCooler_Outputs_width_footprint_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "width_footprint", &result))
		make_access_error("SAM_Sco2AirCooler", "width_footprint");
	});
	return result;
}



