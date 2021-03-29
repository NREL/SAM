#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_Sco2DesignCycle.h"

SAM_EXPORT int SAM_Sco2DesignCycle_execute(SAM_table data, int verbosity, SAM_error* err){
	return SAM_module_exec("sco2_design_cycle", data, verbosity, err);
}

SAM_EXPORT void SAM_Sco2DesignCycle_SCO2PowerCycle_I_N_t_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "I_N_t_des", number);
	});
}

SAM_EXPORT void SAM_Sco2DesignCycle_SCO2PowerCycle_I_P_high_limit_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "I_P_high_limit", number);
	});
}

SAM_EXPORT void SAM_Sco2DesignCycle_SCO2PowerCycle_I_T_mc_in_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "I_T_mc_in_des", number);
	});
}

SAM_EXPORT void SAM_Sco2DesignCycle_SCO2PowerCycle_I_T_t_in_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "I_T_t_in_des", number);
	});
}

SAM_EXPORT void SAM_Sco2DesignCycle_SCO2PowerCycle_I_UA_total_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "I_UA_total_des", number);
	});
}

SAM_EXPORT void SAM_Sco2DesignCycle_SCO2PowerCycle_I_W_dot_net_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "I_W_dot_net_des", number);
	});
}

SAM_EXPORT void SAM_Sco2DesignCycle_SCO2PowerCycle_I_eta_mc_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "I_eta_mc", number);
	});
}

SAM_EXPORT void SAM_Sco2DesignCycle_SCO2PowerCycle_I_eta_rc_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "I_eta_rc", number);
	});
}

SAM_EXPORT void SAM_Sco2DesignCycle_SCO2PowerCycle_I_eta_t_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "I_eta_t", number);
	});
}

SAM_EXPORT void SAM_Sco2DesignCycle_SCO2PowerCycle_I_opt_tol_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "I_opt_tol", number);
	});
}

SAM_EXPORT void SAM_Sco2DesignCycle_SCO2PowerCycle_I_tol_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "I_tol", number);
	});
}

SAM_EXPORT double SAM_Sco2DesignCycle_SCO2PowerCycle_I_N_t_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "I_N_t_des", &result))
		make_access_error("SAM_Sco2DesignCycle", "I_N_t_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2DesignCycle_SCO2PowerCycle_I_P_high_limit_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "I_P_high_limit", &result))
		make_access_error("SAM_Sco2DesignCycle", "I_P_high_limit");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2DesignCycle_SCO2PowerCycle_I_T_mc_in_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "I_T_mc_in_des", &result))
		make_access_error("SAM_Sco2DesignCycle", "I_T_mc_in_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2DesignCycle_SCO2PowerCycle_I_T_t_in_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "I_T_t_in_des", &result))
		make_access_error("SAM_Sco2DesignCycle", "I_T_t_in_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2DesignCycle_SCO2PowerCycle_I_UA_total_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "I_UA_total_des", &result))
		make_access_error("SAM_Sco2DesignCycle", "I_UA_total_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2DesignCycle_SCO2PowerCycle_I_W_dot_net_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "I_W_dot_net_des", &result))
		make_access_error("SAM_Sco2DesignCycle", "I_W_dot_net_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2DesignCycle_SCO2PowerCycle_I_eta_mc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "I_eta_mc", &result))
		make_access_error("SAM_Sco2DesignCycle", "I_eta_mc");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2DesignCycle_SCO2PowerCycle_I_eta_rc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "I_eta_rc", &result))
		make_access_error("SAM_Sco2DesignCycle", "I_eta_rc");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2DesignCycle_SCO2PowerCycle_I_eta_t_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "I_eta_t", &result))
		make_access_error("SAM_Sco2DesignCycle", "I_eta_t");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2DesignCycle_SCO2PowerCycle_I_opt_tol_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "I_opt_tol", &result))
		make_access_error("SAM_Sco2DesignCycle", "I_opt_tol");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2DesignCycle_SCO2PowerCycle_I_tol_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "I_tol", &result))
		make_access_error("SAM_Sco2DesignCycle", "I_tol");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2DesignCycle_Outputs_O_LT_frac_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "O_LT_frac_des", &result))
		make_access_error("SAM_Sco2DesignCycle", "O_LT_frac_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2DesignCycle_Outputs_O_N_mc_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "O_N_mc_des", &result))
		make_access_error("SAM_Sco2DesignCycle", "O_N_mc_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2DesignCycle_Outputs_O_PR_mc_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "O_PR_mc_des", &result))
		make_access_error("SAM_Sco2DesignCycle", "O_PR_mc_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2DesignCycle_Outputs_O_P_mc_out_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "O_P_mc_out_des", &result))
		make_access_error("SAM_Sco2DesignCycle", "O_P_mc_out_des");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2DesignCycle_Outputs_O_T_array_des_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "O_T_array_des", length);
	if (!result)
		make_access_error("SAM_Sco2DesignCycle", "O_T_array_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2DesignCycle_Outputs_O_eta_thermal_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "O_eta_thermal_des", &result))
		make_access_error("SAM_Sco2DesignCycle", "O_eta_thermal_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2DesignCycle_Outputs_O_m_dot_PHX_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "O_m_dot_PHX", &result))
		make_access_error("SAM_Sco2DesignCycle", "O_m_dot_PHX");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2DesignCycle_Outputs_O_recomp_frac_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "O_recomp_frac_des", &result))
		make_access_error("SAM_Sco2DesignCycle", "O_recomp_frac_des");
	});
	return result;
}



