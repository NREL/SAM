#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_Sco2DesignPoint.h"

SAM_EXPORT int SAM_Sco2DesignPoint_execute(SAM_table data, int verbosity, SAM_error* err){
	int n_err = 0;
	translateExceptions(err, [&]{
		n_err += SAM_module_exec("sco2_design_point", data, verbosity, err);
	});
	return n_err;
}


SAM_EXPORT void SAM_Sco2DesignPoint_Common_P_high_limit_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "P_high_limit", number);
	});
}

SAM_EXPORT void SAM_Sco2DesignPoint_Common_T_amb_array_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "T_amb_array", arr, length);
	});
}

SAM_EXPORT void SAM_Sco2DesignPoint_Common_T_amb_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_amb_des", number);
	});
}

SAM_EXPORT void SAM_Sco2DesignPoint_Common_T_htf_hot_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_htf_hot_des", number);
	});
}

SAM_EXPORT void SAM_Sco2DesignPoint_Common_W_dot_net_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "W_dot_net_des", number);
	});
}

SAM_EXPORT void SAM_Sco2DesignPoint_Common_deltaT_ACC_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "deltaT_ACC", number);
	});
}

SAM_EXPORT void SAM_Sco2DesignPoint_Common_deltaT_PHX_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "deltaT_PHX", number);
	});
}

SAM_EXPORT void SAM_Sco2DesignPoint_Common_eta_c_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "eta_c", number);
	});
}

SAM_EXPORT void SAM_Sco2DesignPoint_Common_eta_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "eta_des", number);
	});
}

SAM_EXPORT void SAM_Sco2DesignPoint_Common_eta_t_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "eta_t", number);
	});
}

SAM_EXPORT void SAM_Sco2DesignPoint_Common_part_load_fracs_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "part_load_fracs", arr, length);
	});
}

SAM_EXPORT void SAM_Sco2DesignPoint_Common_run_off_des_study_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "run_off_des_study", number);
	});
}

SAM_EXPORT double SAM_Sco2DesignPoint_Common_P_high_limit_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_high_limit", &result))
		make_access_error("SAM_Sco2DesignPoint", "P_high_limit");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2DesignPoint_Common_T_amb_array_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_amb_array", length);
	if (!result)
		make_access_error("SAM_Sco2DesignPoint", "T_amb_array");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2DesignPoint_Common_T_amb_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_amb_des", &result))
		make_access_error("SAM_Sco2DesignPoint", "T_amb_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2DesignPoint_Common_T_htf_hot_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_htf_hot_des", &result))
		make_access_error("SAM_Sco2DesignPoint", "T_htf_hot_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2DesignPoint_Common_W_dot_net_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "W_dot_net_des", &result))
		make_access_error("SAM_Sco2DesignPoint", "W_dot_net_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2DesignPoint_Common_deltaT_ACC_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "deltaT_ACC", &result))
		make_access_error("SAM_Sco2DesignPoint", "deltaT_ACC");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2DesignPoint_Common_deltaT_PHX_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "deltaT_PHX", &result))
		make_access_error("SAM_Sco2DesignPoint", "deltaT_PHX");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2DesignPoint_Common_eta_c_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_c", &result))
		make_access_error("SAM_Sco2DesignPoint", "eta_c");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2DesignPoint_Common_eta_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_des", &result))
		make_access_error("SAM_Sco2DesignPoint", "eta_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2DesignPoint_Common_eta_t_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_t", &result))
		make_access_error("SAM_Sco2DesignPoint", "eta_t");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2DesignPoint_Common_part_load_fracs_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "part_load_fracs", length);
	if (!result)
		make_access_error("SAM_Sco2DesignPoint", "part_load_fracs");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2DesignPoint_Common_run_off_des_study_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "run_off_des_study", &result))
		make_access_error("SAM_Sco2DesignPoint", "run_off_des_study");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2DesignPoint_Outputs_P_comp_in_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_comp_in", &result))
		make_access_error("SAM_Sco2DesignPoint", "P_comp_in");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2DesignPoint_Outputs_P_comp_out_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_comp_out", &result))
		make_access_error("SAM_Sco2DesignPoint", "P_comp_out");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2DesignPoint_Outputs_T_amb_array_out_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_amb_array_out", length);
	if (!result)
		make_access_error("SAM_Sco2DesignPoint", "T_amb_array_out");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2DesignPoint_Outputs_T_amb_coefs_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_amb_coefs", length);
	if (!result)
		make_access_error("SAM_Sco2DesignPoint", "T_amb_coefs");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2DesignPoint_Outputs_T_amb_eta_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_amb_eta", length);
	if (!result)
		make_access_error("SAM_Sco2DesignPoint", "T_amb_eta");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2DesignPoint_Outputs_T_amb_r_squared_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_amb_r_squared", &result))
		make_access_error("SAM_Sco2DesignPoint", "T_amb_r_squared");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2DesignPoint_Outputs_T_htf_cold_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_htf_cold", &result))
		make_access_error("SAM_Sco2DesignPoint", "T_htf_cold");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2DesignPoint_Outputs_UA_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "UA_total", &result))
		make_access_error("SAM_Sco2DesignPoint", "UA_total");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2DesignPoint_Outputs_eta_thermal_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_thermal_calc", &result))
		make_access_error("SAM_Sco2DesignPoint", "eta_thermal_calc");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2DesignPoint_Outputs_part_load_coefs_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "part_load_coefs", length);
	if (!result)
		make_access_error("SAM_Sco2DesignPoint", "part_load_coefs");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2DesignPoint_Outputs_part_load_eta_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "part_load_eta", length);
	if (!result)
		make_access_error("SAM_Sco2DesignPoint", "part_load_eta");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2DesignPoint_Outputs_part_load_fracs_out_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "part_load_fracs_out", length);
	if (!result)
		make_access_error("SAM_Sco2DesignPoint", "part_load_fracs_out");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2DesignPoint_Outputs_part_load_r_squared_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "part_load_r_squared", &result))
		make_access_error("SAM_Sco2DesignPoint", "part_load_r_squared");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2DesignPoint_Outputs_recomp_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "recomp_frac", &result))
		make_access_error("SAM_Sco2DesignPoint", "recomp_frac");
	});
	return result;
}



