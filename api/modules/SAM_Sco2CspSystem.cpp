#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_Sco2CspSystem.h"

SAM_EXPORT int SAM_Sco2CspSystem_execute(SAM_table data, int verbosity, SAM_error* err){
	int n_err = 0;
	translateExceptions(err, [&]{
		n_err += SAM_module_exec("sco2_csp_system", data, verbosity, err);
	});
	return n_err;
}


SAM_EXPORT void SAM_Sco2CspSystem_SystemDesign_T_amb_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_amb_des", number);
	});
}

SAM_EXPORT void SAM_Sco2CspSystem_SystemDesign_T_htf_hot_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_htf_hot_des", number);
	});
}

SAM_EXPORT void SAM_Sco2CspSystem_SystemDesign_W_dot_net_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "W_dot_net_des", number);
	});
}

SAM_EXPORT void SAM_Sco2CspSystem_SystemDesign_dT_PHX_hot_approach_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dT_PHX_hot_approach", number);
	});
}

SAM_EXPORT void SAM_Sco2CspSystem_SystemDesign_dT_mc_approach_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dT_mc_approach", number);
	});
}

SAM_EXPORT void SAM_Sco2CspSystem_SystemDesign_design_method_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "design_method", number);
	});
}

SAM_EXPORT void SAM_Sco2CspSystem_SystemDesign_eta_thermal_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "eta_thermal_des", number);
	});
}

SAM_EXPORT void SAM_Sco2CspSystem_SystemDesign_htf_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "htf", number);
	});
}

SAM_EXPORT void SAM_Sco2CspSystem_SystemDesign_htf_props_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "htf_props", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Sco2CspSystem_SystemDesign_site_elevation_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "site_elevation", number);
	});
}

SAM_EXPORT void SAM_Sco2CspSystem_HeatExchangerDesign_HTR_HP_deltaP_des_in_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "HTR_HP_deltaP_des_in", number);
	});
}

SAM_EXPORT void SAM_Sco2CspSystem_HeatExchangerDesign_HTR_LP_deltaP_des_in_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "HTR_LP_deltaP_des_in", number);
	});
}

SAM_EXPORT void SAM_Sco2CspSystem_HeatExchangerDesign_HTR_UA_des_in_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "HTR_UA_des_in", number);
	});
}

SAM_EXPORT void SAM_Sco2CspSystem_HeatExchangerDesign_HTR_design_code_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "HTR_design_code", number);
	});
}

SAM_EXPORT void SAM_Sco2CspSystem_HeatExchangerDesign_HTR_eff_des_in_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "HTR_eff_des_in", number);
	});
}

SAM_EXPORT void SAM_Sco2CspSystem_HeatExchangerDesign_HTR_min_dT_des_in_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "HTR_min_dT_des_in", number);
	});
}

SAM_EXPORT void SAM_Sco2CspSystem_HeatExchangerDesign_HT_recup_eff_max_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "HT_recup_eff_max", number);
	});
}

SAM_EXPORT void SAM_Sco2CspSystem_HeatExchangerDesign_LTR_HP_deltaP_des_in_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "LTR_HP_deltaP_des_in", number);
	});
}

SAM_EXPORT void SAM_Sco2CspSystem_HeatExchangerDesign_LTR_LP_deltaP_des_in_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "LTR_LP_deltaP_des_in", number);
	});
}

SAM_EXPORT void SAM_Sco2CspSystem_HeatExchangerDesign_LTR_UA_des_in_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "LTR_UA_des_in", number);
	});
}

SAM_EXPORT void SAM_Sco2CspSystem_HeatExchangerDesign_LTR_design_code_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "LTR_design_code", number);
	});
}

SAM_EXPORT void SAM_Sco2CspSystem_HeatExchangerDesign_LTR_eff_des_in_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "LTR_eff_des_in", number);
	});
}

SAM_EXPORT void SAM_Sco2CspSystem_HeatExchangerDesign_LTR_min_dT_des_in_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "LTR_min_dT_des_in", number);
	});
}

SAM_EXPORT void SAM_Sco2CspSystem_HeatExchangerDesign_LT_recup_eff_max_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "LT_recup_eff_max", number);
	});
}

SAM_EXPORT void SAM_Sco2CspSystem_HeatExchangerDesign_UA_recup_tot_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "UA_recup_tot_des", number);
	});
}

SAM_EXPORT void SAM_Sco2CspSystem_HeatExchangerDesign_cycle_config_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cycle_config", number);
	});
}

SAM_EXPORT void SAM_Sco2CspSystem_HeatExchangerDesign_des_objective_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "des_objective", number);
	});
}

SAM_EXPORT void SAM_Sco2CspSystem_HeatExchangerDesign_is_IP_fixed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "is_IP_fixed", number);
	});
}

SAM_EXPORT void SAM_Sco2CspSystem_HeatExchangerDesign_is_PR_fixed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "is_PR_fixed", number);
	});
}

SAM_EXPORT void SAM_Sco2CspSystem_HeatExchangerDesign_is_P_high_fixed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "is_P_high_fixed", number);
	});
}

SAM_EXPORT void SAM_Sco2CspSystem_HeatExchangerDesign_is_recomp_ok_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "is_recomp_ok", number);
	});
}

SAM_EXPORT void SAM_Sco2CspSystem_HeatExchangerDesign_min_phx_deltaT_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "min_phx_deltaT", number);
	});
}

SAM_EXPORT void SAM_Sco2CspSystem_HeatExchangerDesign_rel_tol_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rel_tol", number);
	});
}

SAM_EXPORT void SAM_Sco2CspSystem_CycleDesign_PHX_co2_deltaP_des_in_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "PHX_co2_deltaP_des_in", number);
	});
}

SAM_EXPORT void SAM_Sco2CspSystem_CycleDesign_P_high_limit_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "P_high_limit", number);
	});
}

SAM_EXPORT void SAM_Sco2CspSystem_CycleDesign_deltaP_counterHX_frac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "deltaP_counterHX_frac", number);
	});
}

SAM_EXPORT void SAM_Sco2CspSystem_CycleDesign_eta_isen_mc_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "eta_isen_mc", number);
	});
}

SAM_EXPORT void SAM_Sco2CspSystem_CycleDesign_eta_isen_pc_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "eta_isen_pc", number);
	});
}

SAM_EXPORT void SAM_Sco2CspSystem_CycleDesign_eta_isen_rc_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "eta_isen_rc", number);
	});
}

SAM_EXPORT void SAM_Sco2CspSystem_CycleDesign_eta_isen_t_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "eta_isen_t", number);
	});
}

SAM_EXPORT void SAM_Sco2CspSystem_PHXDesign_dT_PHX_cold_approach_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dT_PHX_cold_approach", number);
	});
}

SAM_EXPORT void SAM_Sco2CspSystem_AirCoolerDesign_deltaP_cooler_frac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "deltaP_cooler_frac", number);
	});
}

SAM_EXPORT void SAM_Sco2CspSystem_AirCoolerDesign_fan_power_frac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fan_power_frac", number);
	});
}

SAM_EXPORT void SAM_Sco2CspSystem_AirCoolerDesign_is_design_air_cooler_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "is_design_air_cooler", number);
	});
}

SAM_EXPORT void SAM_Sco2CspSystem_Common_is_gen_od_polynomials_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "is_gen_od_polynomials", number);
	});
}

SAM_EXPORT void SAM_Sco2CspSystem_Common_od_P_mc_in_sweep_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "od_P_mc_in_sweep", arr, length);
	});
}

SAM_EXPORT void SAM_Sco2CspSystem_Common_od_T_t_in_mode_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "od_T_t_in_mode", number);
	});
}

SAM_EXPORT void SAM_Sco2CspSystem_Common_od_cases_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "od_cases", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Sco2CspSystem_Common_od_generate_udpc_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "od_generate_udpc", arr, length);
	});
}

SAM_EXPORT void SAM_Sco2CspSystem_Common_od_set_control_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "od_set_control", mat, nrows, ncols);
	});
}

SAM_EXPORT double SAM_Sco2CspSystem_SystemDesign_T_amb_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_amb_des", &result))
		make_access_error("SAM_Sco2CspSystem", "T_amb_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_SystemDesign_T_htf_hot_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_htf_hot_des", &result))
		make_access_error("SAM_Sco2CspSystem", "T_htf_hot_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_SystemDesign_W_dot_net_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "W_dot_net_des", &result))
		make_access_error("SAM_Sco2CspSystem", "W_dot_net_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_SystemDesign_dT_PHX_hot_approach_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dT_PHX_hot_approach", &result))
		make_access_error("SAM_Sco2CspSystem", "dT_PHX_hot_approach");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_SystemDesign_dT_mc_approach_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dT_mc_approach", &result))
		make_access_error("SAM_Sco2CspSystem", "dT_mc_approach");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_SystemDesign_design_method_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "design_method", &result))
		make_access_error("SAM_Sco2CspSystem", "design_method");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_SystemDesign_eta_thermal_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_thermal_des", &result))
		make_access_error("SAM_Sco2CspSystem", "eta_thermal_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_SystemDesign_htf_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "htf", &result))
		make_access_error("SAM_Sco2CspSystem", "htf");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_SystemDesign_htf_props_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "htf_props", nrows, ncols);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "htf_props");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_SystemDesign_site_elevation_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "site_elevation", &result))
		make_access_error("SAM_Sco2CspSystem", "site_elevation");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_HeatExchangerDesign_HTR_HP_deltaP_des_in_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "HTR_HP_deltaP_des_in", &result))
		make_access_error("SAM_Sco2CspSystem", "HTR_HP_deltaP_des_in");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_HeatExchangerDesign_HTR_LP_deltaP_des_in_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "HTR_LP_deltaP_des_in", &result))
		make_access_error("SAM_Sco2CspSystem", "HTR_LP_deltaP_des_in");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_HeatExchangerDesign_HTR_UA_des_in_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "HTR_UA_des_in", &result))
		make_access_error("SAM_Sco2CspSystem", "HTR_UA_des_in");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_HeatExchangerDesign_HTR_design_code_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "HTR_design_code", &result))
		make_access_error("SAM_Sco2CspSystem", "HTR_design_code");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_HeatExchangerDesign_HTR_eff_des_in_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "HTR_eff_des_in", &result))
		make_access_error("SAM_Sco2CspSystem", "HTR_eff_des_in");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_HeatExchangerDesign_HTR_min_dT_des_in_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "HTR_min_dT_des_in", &result))
		make_access_error("SAM_Sco2CspSystem", "HTR_min_dT_des_in");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_HeatExchangerDesign_HT_recup_eff_max_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "HT_recup_eff_max", &result))
		make_access_error("SAM_Sco2CspSystem", "HT_recup_eff_max");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_HeatExchangerDesign_LTR_HP_deltaP_des_in_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "LTR_HP_deltaP_des_in", &result))
		make_access_error("SAM_Sco2CspSystem", "LTR_HP_deltaP_des_in");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_HeatExchangerDesign_LTR_LP_deltaP_des_in_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "LTR_LP_deltaP_des_in", &result))
		make_access_error("SAM_Sco2CspSystem", "LTR_LP_deltaP_des_in");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_HeatExchangerDesign_LTR_UA_des_in_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "LTR_UA_des_in", &result))
		make_access_error("SAM_Sco2CspSystem", "LTR_UA_des_in");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_HeatExchangerDesign_LTR_design_code_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "LTR_design_code", &result))
		make_access_error("SAM_Sco2CspSystem", "LTR_design_code");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_HeatExchangerDesign_LTR_eff_des_in_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "LTR_eff_des_in", &result))
		make_access_error("SAM_Sco2CspSystem", "LTR_eff_des_in");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_HeatExchangerDesign_LTR_min_dT_des_in_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "LTR_min_dT_des_in", &result))
		make_access_error("SAM_Sco2CspSystem", "LTR_min_dT_des_in");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_HeatExchangerDesign_LT_recup_eff_max_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "LT_recup_eff_max", &result))
		make_access_error("SAM_Sco2CspSystem", "LT_recup_eff_max");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_HeatExchangerDesign_UA_recup_tot_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "UA_recup_tot_des", &result))
		make_access_error("SAM_Sco2CspSystem", "UA_recup_tot_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_HeatExchangerDesign_cycle_config_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cycle_config", &result))
		make_access_error("SAM_Sco2CspSystem", "cycle_config");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_HeatExchangerDesign_des_objective_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "des_objective", &result))
		make_access_error("SAM_Sco2CspSystem", "des_objective");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_HeatExchangerDesign_is_IP_fixed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "is_IP_fixed", &result))
		make_access_error("SAM_Sco2CspSystem", "is_IP_fixed");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_HeatExchangerDesign_is_PR_fixed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "is_PR_fixed", &result))
		make_access_error("SAM_Sco2CspSystem", "is_PR_fixed");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_HeatExchangerDesign_is_P_high_fixed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "is_P_high_fixed", &result))
		make_access_error("SAM_Sco2CspSystem", "is_P_high_fixed");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_HeatExchangerDesign_is_recomp_ok_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "is_recomp_ok", &result))
		make_access_error("SAM_Sco2CspSystem", "is_recomp_ok");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_HeatExchangerDesign_min_phx_deltaT_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "min_phx_deltaT", &result))
		make_access_error("SAM_Sco2CspSystem", "min_phx_deltaT");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_HeatExchangerDesign_rel_tol_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rel_tol", &result))
		make_access_error("SAM_Sco2CspSystem", "rel_tol");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_CycleDesign_PHX_co2_deltaP_des_in_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "PHX_co2_deltaP_des_in", &result))
		make_access_error("SAM_Sco2CspSystem", "PHX_co2_deltaP_des_in");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_CycleDesign_P_high_limit_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_high_limit", &result))
		make_access_error("SAM_Sco2CspSystem", "P_high_limit");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_CycleDesign_deltaP_counterHX_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "deltaP_counterHX_frac", &result))
		make_access_error("SAM_Sco2CspSystem", "deltaP_counterHX_frac");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_CycleDesign_eta_isen_mc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_isen_mc", &result))
		make_access_error("SAM_Sco2CspSystem", "eta_isen_mc");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_CycleDesign_eta_isen_pc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_isen_pc", &result))
		make_access_error("SAM_Sco2CspSystem", "eta_isen_pc");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_CycleDesign_eta_isen_rc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_isen_rc", &result))
		make_access_error("SAM_Sco2CspSystem", "eta_isen_rc");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_CycleDesign_eta_isen_t_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_isen_t", &result))
		make_access_error("SAM_Sco2CspSystem", "eta_isen_t");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_PHXDesign_dT_PHX_cold_approach_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dT_PHX_cold_approach", &result))
		make_access_error("SAM_Sco2CspSystem", "dT_PHX_cold_approach");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_AirCoolerDesign_deltaP_cooler_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "deltaP_cooler_frac", &result))
		make_access_error("SAM_Sco2CspSystem", "deltaP_cooler_frac");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_AirCoolerDesign_fan_power_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fan_power_frac", &result))
		make_access_error("SAM_Sco2CspSystem", "fan_power_frac");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_AirCoolerDesign_is_design_air_cooler_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "is_design_air_cooler", &result))
		make_access_error("SAM_Sco2CspSystem", "is_design_air_cooler");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Common_is_gen_od_polynomials_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "is_gen_od_polynomials", &result))
		make_access_error("SAM_Sco2CspSystem", "is_gen_od_polynomials");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Common_od_P_mc_in_sweep_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "od_P_mc_in_sweep", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "od_P_mc_in_sweep");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Common_od_T_t_in_mode_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "od_T_t_in_mode", &result))
		make_access_error("SAM_Sco2CspSystem", "od_T_t_in_mode");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Common_od_cases_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "od_cases", nrows, ncols);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "od_cases");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Common_od_generate_udpc_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "od_generate_udpc", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "od_generate_udpc");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Common_od_set_control_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "od_set_control", nrows, ncols);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "od_set_control");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_HTR_HP_T_in_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "HTR_HP_T_in_des", &result))
		make_access_error("SAM_Sco2CspSystem", "HTR_HP_T_in_des");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_HTR_HP_T_in_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "HTR_HP_T_in_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "HTR_HP_T_in_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_HTR_HP_deltaP_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "HTR_HP_deltaP_des", &result))
		make_access_error("SAM_Sco2CspSystem", "HTR_HP_deltaP_des");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_HTR_HP_deltaP_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "HTR_HP_deltaP_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "HTR_HP_deltaP_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_HTR_LP_T_out_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "HTR_LP_T_out_des", &result))
		make_access_error("SAM_Sco2CspSystem", "HTR_LP_T_out_des");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_HTR_LP_T_out_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "HTR_LP_T_out_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "HTR_LP_T_out_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_HTR_LP_deltaP_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "HTR_LP_deltaP_des", &result))
		make_access_error("SAM_Sco2CspSystem", "HTR_LP_deltaP_des");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_HTR_LP_deltaP_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "HTR_LP_deltaP_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "HTR_LP_deltaP_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_HTR_UA_assigned_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "HTR_UA_assigned", &result))
		make_access_error("SAM_Sco2CspSystem", "HTR_UA_assigned");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_HTR_UA_calculated_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "HTR_UA_calculated", &result))
		make_access_error("SAM_Sco2CspSystem", "HTR_UA_calculated");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_HTR_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "HTR_cost", &result))
		make_access_error("SAM_Sco2CspSystem", "HTR_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_HTR_min_dT_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "HTR_min_dT", &result))
		make_access_error("SAM_Sco2CspSystem", "HTR_min_dT");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_HTR_min_dT_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "HTR_min_dT_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "HTR_min_dT_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_IP_cooler_P_in_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "IP_cooler_P_in", &result))
		make_access_error("SAM_Sco2CspSystem", "IP_cooler_P_in");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_IP_cooler_T_in_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "IP_cooler_T_in", &result))
		make_access_error("SAM_Sco2CspSystem", "IP_cooler_T_in");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_IP_cooler_UA_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "IP_cooler_UA", &result))
		make_access_error("SAM_Sco2CspSystem", "IP_cooler_UA");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_IP_cooler_W_dot_fan_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "IP_cooler_W_dot_fan", &result))
		make_access_error("SAM_Sco2CspSystem", "IP_cooler_W_dot_fan");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_IP_cooler_W_dot_fan_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "IP_cooler_W_dot_fan_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "IP_cooler_W_dot_fan_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_IP_cooler_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "IP_cooler_cost", &result))
		make_access_error("SAM_Sco2CspSystem", "IP_cooler_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_IP_cooler_m_dot_co2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "IP_cooler_m_dot_co2", &result))
		make_access_error("SAM_Sco2CspSystem", "IP_cooler_m_dot_co2");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_IP_cooler_q_dot_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "IP_cooler_q_dot", &result))
		make_access_error("SAM_Sco2CspSystem", "IP_cooler_q_dot");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_LP_cooler_P_in_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "LP_cooler_P_in", &result))
		make_access_error("SAM_Sco2CspSystem", "LP_cooler_P_in");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_LP_cooler_T_in_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "LP_cooler_T_in", &result))
		make_access_error("SAM_Sco2CspSystem", "LP_cooler_T_in");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_LP_cooler_T_in_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "LP_cooler_T_in_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "LP_cooler_T_in_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_LP_cooler_UA_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "LP_cooler_UA", &result))
		make_access_error("SAM_Sco2CspSystem", "LP_cooler_UA");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_LP_cooler_W_dot_fan_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "LP_cooler_W_dot_fan", &result))
		make_access_error("SAM_Sco2CspSystem", "LP_cooler_W_dot_fan");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_LP_cooler_W_dot_fan_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "LP_cooler_W_dot_fan_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "LP_cooler_W_dot_fan_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_LP_cooler_co2_deltaP_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "LP_cooler_co2_deltaP_des", &result))
		make_access_error("SAM_Sco2CspSystem", "LP_cooler_co2_deltaP_des");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_LP_cooler_co2_deltaP_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "LP_cooler_co2_deltaP_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "LP_cooler_co2_deltaP_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_LP_cooler_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "LP_cooler_cost", &result))
		make_access_error("SAM_Sco2CspSystem", "LP_cooler_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_LP_cooler_in_isen_deltah_to_P_mc_out_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "LP_cooler_in_isen_deltah_to_P_mc_out", &result))
		make_access_error("SAM_Sco2CspSystem", "LP_cooler_in_isen_deltah_to_P_mc_out");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_LP_cooler_in_isen_deltah_to_P_mc_out_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "LP_cooler_in_isen_deltah_to_P_mc_out_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "LP_cooler_in_isen_deltah_to_P_mc_out_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_LP_cooler_m_dot_co2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "LP_cooler_m_dot_co2", &result))
		make_access_error("SAM_Sco2CspSystem", "LP_cooler_m_dot_co2");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_LP_cooler_q_dot_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "LP_cooler_q_dot", &result))
		make_access_error("SAM_Sco2CspSystem", "LP_cooler_q_dot");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_LP_cooler_rho_in_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "LP_cooler_rho_in", &result))
		make_access_error("SAM_Sco2CspSystem", "LP_cooler_rho_in");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_LP_cooler_rho_in_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "LP_cooler_rho_in_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "LP_cooler_rho_in_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_LTR_HP_T_out_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "LTR_HP_T_out_des", &result))
		make_access_error("SAM_Sco2CspSystem", "LTR_HP_T_out_des");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_LTR_HP_T_out_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "LTR_HP_T_out_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "LTR_HP_T_out_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_LTR_HP_deltaP_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "LTR_HP_deltaP_des", &result))
		make_access_error("SAM_Sco2CspSystem", "LTR_HP_deltaP_des");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_LTR_HP_deltaP_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "LTR_HP_deltaP_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "LTR_HP_deltaP_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_LTR_LP_deltaP_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "LTR_LP_deltaP_des", &result))
		make_access_error("SAM_Sco2CspSystem", "LTR_LP_deltaP_des");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_LTR_LP_deltaP_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "LTR_LP_deltaP_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "LTR_LP_deltaP_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_LTR_UA_assigned_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "LTR_UA_assigned", &result))
		make_access_error("SAM_Sco2CspSystem", "LTR_UA_assigned");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_LTR_UA_calculated_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "LTR_UA_calculated", &result))
		make_access_error("SAM_Sco2CspSystem", "LTR_UA_calculated");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_LTR_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "LTR_cost", &result))
		make_access_error("SAM_Sco2CspSystem", "LTR_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_LTR_min_dT_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "LTR_min_dT", &result))
		make_access_error("SAM_Sco2CspSystem", "LTR_min_dT");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_LTR_min_dT_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "LTR_min_dT_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "LTR_min_dT_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_NTU_HTR_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "NTU_HTR", &result))
		make_access_error("SAM_Sco2CspSystem", "NTU_HTR");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_NTU_LTR_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "NTU_LTR", &result))
		make_access_error("SAM_Sco2CspSystem", "NTU_LTR");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_NTU_PHX_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "NTU_PHX", &result))
		make_access_error("SAM_Sco2CspSystem", "NTU_PHX");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_PHX_co2_deltaP_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "PHX_co2_deltaP_des", &result))
		make_access_error("SAM_Sco2CspSystem", "PHX_co2_deltaP_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_PHX_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "PHX_cost", &result))
		make_access_error("SAM_Sco2CspSystem", "PHX_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_P_co2_PHX_in_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_co2_PHX_in", &result))
		make_access_error("SAM_Sco2CspSystem", "P_co2_PHX_in");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_P_co2_PHX_in_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_co2_PHX_in_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "P_co2_PHX_in_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_P_comp_in_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_comp_in", &result))
		make_access_error("SAM_Sco2CspSystem", "P_comp_in");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_P_comp_in_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_comp_in_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "P_comp_in_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_P_comp_out_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_comp_out", &result))
		make_access_error("SAM_Sco2CspSystem", "P_comp_out");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_P_mc_data_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_mc_data", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "P_mc_data");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_P_mc_out_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_mc_out_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "P_mc_out_od");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_P_pc_data_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_pc_data", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "P_pc_data");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_P_rc_data_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_rc_data", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "P_rc_data");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_P_state_points_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_state_points", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "P_state_points");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_P_t_data_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "P_t_data", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "P_t_data");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_Q_dot_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "Q_dot_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "Q_dot_od");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_T_HTR_HP_data_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_HTR_HP_data", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "T_HTR_HP_data");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_T_HTR_LP_data_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_HTR_LP_data", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "T_HTR_LP_data");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_T_LTR_HP_data_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_LTR_HP_data", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "T_LTR_HP_data");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_T_LTR_LP_data_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_LTR_LP_data", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "T_LTR_LP_data");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_T_PHX_data_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_PHX_data", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "T_PHX_data");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_T_amb_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_amb_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "T_amb_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_T_co2_PHX_in_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_co2_PHX_in", &result))
		make_access_error("SAM_Sco2CspSystem", "T_co2_PHX_in");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_T_co2_PHX_in_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_co2_PHX_in_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "T_co2_PHX_in_od");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_T_co2_PHX_out_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_co2_PHX_out_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "T_co2_PHX_out_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_T_comp_in_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_comp_in", &result))
		make_access_error("SAM_Sco2CspSystem", "T_comp_in");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_T_htf_cold_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_htf_cold_des", &result))
		make_access_error("SAM_Sco2CspSystem", "T_htf_cold_des");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_T_htf_cold_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_htf_cold_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "T_htf_cold_od");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_T_htf_hot_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_htf_hot_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "T_htf_hot_od");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_T_main_cooler_data_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_main_cooler_data", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "T_main_cooler_data");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_T_mc_in_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_mc_in_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "T_mc_in_od");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_T_pre_cooler_data_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_pre_cooler_data", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "T_pre_cooler_data");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_T_state_points_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "T_state_points", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "T_state_points");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_T_turb_in_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_turb_in", &result))
		make_access_error("SAM_Sco2CspSystem", "T_turb_in");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_UA_PHX_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "UA_PHX", &result))
		make_access_error("SAM_Sco2CspSystem", "UA_PHX");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_W_dot_net_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "W_dot_net_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "W_dot_net_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_c_tot_W_dot_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "c_tot_W_dot", &result))
		make_access_error("SAM_Sco2CspSystem", "c_tot_W_dot");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_c_tot_W_dot_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "c_tot_W_dot_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "c_tot_W_dot_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_c_tot_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "c_tot_cost", &result))
		make_access_error("SAM_Sco2CspSystem", "c_tot_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_cooler_tot_UA_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cooler_tot_UA", &result))
		make_access_error("SAM_Sco2CspSystem", "cooler_tot_UA");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_cooler_tot_W_dot_fan_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cooler_tot_W_dot_fan", &result))
		make_access_error("SAM_Sco2CspSystem", "cooler_tot_W_dot_fan");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_cooler_tot_W_dot_fan_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "cooler_tot_W_dot_fan_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "cooler_tot_W_dot_fan_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_cooler_tot_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cooler_tot_cost", &result))
		make_access_error("SAM_Sco2CspSystem", "cooler_tot_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_cycle_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cycle_cost", &result))
		make_access_error("SAM_Sco2CspSystem", "cycle_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_cycle_spec_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cycle_spec_cost", &result))
		make_access_error("SAM_Sco2CspSystem", "cycle_spec_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_cycle_spec_cost_thermal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cycle_spec_cost_thermal", &result))
		make_access_error("SAM_Sco2CspSystem", "cycle_spec_cost_thermal");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_deltaT_HTF_PHX_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "deltaT_HTF_PHX", &result))
		make_access_error("SAM_Sco2CspSystem", "deltaT_HTF_PHX");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_deltaT_HTF_PHX_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "deltaT_HTF_PHX_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "deltaT_HTF_PHX_od");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_diff_E_cycle_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "diff_E_cycle", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "diff_E_cycle");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_diff_Q_HTR_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "diff_Q_HTR", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "diff_Q_HTR");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_diff_Q_LTR_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "diff_Q_LTR", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "diff_Q_LTR");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_diff_m_dot_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "diff_m_dot_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "diff_m_dot_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_eff_HTR_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eff_HTR", &result))
		make_access_error("SAM_Sco2CspSystem", "eff_HTR");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_eff_HTR_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "eff_HTR_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "eff_HTR_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_eff_LTR_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eff_LTR", &result))
		make_access_error("SAM_Sco2CspSystem", "eff_LTR");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_eff_LTR_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "eff_LTR_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "eff_LTR_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_eff_PHX_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eff_PHX", &result))
		make_access_error("SAM_Sco2CspSystem", "eff_PHX");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_eta_thermal_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_thermal_calc", &result))
		make_access_error("SAM_Sco2CspSystem", "eta_thermal_calc");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_eta_thermal_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "eta_thermal_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "eta_thermal_od");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_h_mc_data_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "h_mc_data", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "h_mc_data");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_h_pc_data_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "h_pc_data", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "h_pc_data");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_h_rc_data_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "h_rc_data", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "h_rc_data");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_h_state_points_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "h_state_points", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "h_state_points");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_h_t_data_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "h_t_data", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "h_t_data");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_m_dot_co2_full_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "m_dot_co2_full", &result))
		make_access_error("SAM_Sco2CspSystem", "m_dot_co2_full");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_m_dot_co2_full_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_co2_full_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "m_dot_co2_full_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_m_dot_htf_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "m_dot_htf_des", &result))
		make_access_error("SAM_Sco2CspSystem", "m_dot_htf_des");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_m_dot_htf_fracs_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "m_dot_htf_fracs", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "m_dot_htf_fracs");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_mc_D_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "mc_D", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "mc_D");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_mc_N_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mc_N_des", &result))
		make_access_error("SAM_Sco2CspSystem", "mc_N_des");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_mc_N_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "mc_N_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "mc_N_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_mc_T_out_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mc_T_out", &result))
		make_access_error("SAM_Sco2CspSystem", "mc_T_out");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_mc_T_out_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "mc_T_out_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "mc_T_out_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_mc_W_dot_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mc_W_dot", &result))
		make_access_error("SAM_Sco2CspSystem", "mc_W_dot");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_mc_W_dot_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "mc_W_dot_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "mc_W_dot_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_mc_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mc_cost", &result))
		make_access_error("SAM_Sco2CspSystem", "mc_cost");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_mc_eta_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "mc_eta_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "mc_eta_od");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_mc_eta_stages_des_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "mc_eta_stages_des", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "mc_eta_stages_des");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_mc_eta_stages_od_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "mc_eta_stages_od", nrows, ncols);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "mc_eta_stages_od");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_mc_f_bypass_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "mc_f_bypass_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "mc_f_bypass_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_mc_ideal_spec_work_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mc_ideal_spec_work", &result))
		make_access_error("SAM_Sco2CspSystem", "mc_ideal_spec_work");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_mc_ideal_spec_work_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "mc_ideal_spec_work_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "mc_ideal_spec_work_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_mc_m_dot_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mc_m_dot_des", &result))
		make_access_error("SAM_Sco2CspSystem", "mc_m_dot_des");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_mc_m_dot_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "mc_m_dot_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "mc_m_dot_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_mc_n_stages_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mc_n_stages", &result))
		make_access_error("SAM_Sco2CspSystem", "mc_n_stages");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_mc_phi_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mc_phi_des", &result))
		make_access_error("SAM_Sco2CspSystem", "mc_phi_des");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_mc_phi_od_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "mc_phi_od", nrows, ncols);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "mc_phi_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_mc_phi_surge_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mc_phi_surge", &result))
		make_access_error("SAM_Sco2CspSystem", "mc_phi_surge");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_mc_psi_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mc_psi_des", &result))
		make_access_error("SAM_Sco2CspSystem", "mc_psi_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_mc_psi_max_at_N_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mc_psi_max_at_N_des", &result))
		make_access_error("SAM_Sco2CspSystem", "mc_psi_max_at_N_des");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_mc_psi_od_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "mc_psi_od", nrows, ncols);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "mc_psi_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_mc_rho_in_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mc_rho_in", &result))
		make_access_error("SAM_Sco2CspSystem", "mc_rho_in");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_mc_rho_in_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "mc_rho_in_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "mc_rho_in_od");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_mc_tip_ratio_des_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "mc_tip_ratio_des", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "mc_tip_ratio_des");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_mc_tip_ratio_od_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "mc_tip_ratio_od", nrows, ncols);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "mc_tip_ratio_od");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_od_code_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "od_code", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "od_code");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_od_opt_conv_tol_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "od_opt_conv_tol", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "od_opt_conv_tol");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_od_opt_obj_code_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "od_opt_obj_code", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "od_opt_obj_code");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_pc_D_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pc_D", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "pc_D");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_pc_N_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pc_N_des", &result))
		make_access_error("SAM_Sco2CspSystem", "pc_N_des");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_pc_N_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pc_N_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "pc_N_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_pc_P_in_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pc_P_in_des", &result))
		make_access_error("SAM_Sco2CspSystem", "pc_P_in_des");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_pc_P_in_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pc_P_in_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "pc_P_in_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_pc_T_in_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pc_T_in_des", &result))
		make_access_error("SAM_Sco2CspSystem", "pc_T_in_des");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_pc_T_in_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pc_T_in_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "pc_T_in_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_pc_W_dot_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pc_W_dot", &result))
		make_access_error("SAM_Sco2CspSystem", "pc_W_dot");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_pc_W_dot_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pc_W_dot_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "pc_W_dot_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_pc_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pc_cost", &result))
		make_access_error("SAM_Sco2CspSystem", "pc_cost");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_pc_eta_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pc_eta_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "pc_eta_od");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_pc_eta_stages_des_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pc_eta_stages_des", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "pc_eta_stages_des");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_pc_eta_stages_od_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "pc_eta_stages_od", nrows, ncols);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "pc_eta_stages_od");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_pc_f_bypass_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pc_f_bypass_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "pc_f_bypass_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_pc_ideal_spec_work_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pc_ideal_spec_work_des", &result))
		make_access_error("SAM_Sco2CspSystem", "pc_ideal_spec_work_des");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_pc_ideal_spec_work_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pc_ideal_spec_work_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "pc_ideal_spec_work_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_pc_m_dot_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pc_m_dot_des", &result))
		make_access_error("SAM_Sco2CspSystem", "pc_m_dot_des");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_pc_m_dot_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pc_m_dot_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "pc_m_dot_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_pc_n_stages_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pc_n_stages", &result))
		make_access_error("SAM_Sco2CspSystem", "pc_n_stages");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_pc_phi_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pc_phi_des", &result))
		make_access_error("SAM_Sco2CspSystem", "pc_phi_des");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_pc_phi_od_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "pc_phi_od", nrows, ncols);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "pc_phi_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_pc_phi_surge_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pc_phi_surge", &result))
		make_access_error("SAM_Sco2CspSystem", "pc_phi_surge");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_pc_rho_in_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pc_rho_in_des", &result))
		make_access_error("SAM_Sco2CspSystem", "pc_rho_in_des");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_pc_rho_in_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pc_rho_in_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "pc_rho_in_od");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_pc_tip_ratio_des_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "pc_tip_ratio_des", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "pc_tip_ratio_des");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_pc_tip_ratio_od_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "pc_tip_ratio_od", nrows, ncols);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "pc_tip_ratio_od");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_phx_co2_deltaP_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "phx_co2_deltaP_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "phx_co2_deltaP_od");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_phx_eff_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "phx_eff_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "phx_eff_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_q_dot_HTR_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_dot_HTR", &result))
		make_access_error("SAM_Sco2CspSystem", "q_dot_HTR");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_q_dot_HTR_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_HTR_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "q_dot_HTR_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_q_dot_LTR_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_dot_LTR", &result))
		make_access_error("SAM_Sco2CspSystem", "q_dot_LTR");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_q_dot_LTR_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "q_dot_LTR_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "q_dot_LTR_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_q_dot_PHX_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_dot_PHX", &result))
		make_access_error("SAM_Sco2CspSystem", "q_dot_PHX");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_rc_D_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "rc_D", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "rc_D");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_rc_N_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rc_N_des", &result))
		make_access_error("SAM_Sco2CspSystem", "rc_N_des");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_rc_N_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "rc_N_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "rc_N_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_rc_P_in_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rc_P_in_des", &result))
		make_access_error("SAM_Sco2CspSystem", "rc_P_in_des");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_rc_P_in_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "rc_P_in_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "rc_P_in_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_rc_P_out_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rc_P_out_des", &result))
		make_access_error("SAM_Sco2CspSystem", "rc_P_out_des");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_rc_P_out_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "rc_P_out_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "rc_P_out_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_rc_T_in_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rc_T_in_des", &result))
		make_access_error("SAM_Sco2CspSystem", "rc_T_in_des");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_rc_T_in_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "rc_T_in_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "rc_T_in_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_rc_T_out_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rc_T_out_des", &result))
		make_access_error("SAM_Sco2CspSystem", "rc_T_out_des");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_rc_T_out_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "rc_T_out_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "rc_T_out_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_rc_W_dot_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rc_W_dot", &result))
		make_access_error("SAM_Sco2CspSystem", "rc_W_dot");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_rc_W_dot_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "rc_W_dot_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "rc_W_dot_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_rc_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rc_cost", &result))
		make_access_error("SAM_Sco2CspSystem", "rc_cost");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_rc_eta_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "rc_eta_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "rc_eta_od");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_rc_eta_stages_des_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "rc_eta_stages_des", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "rc_eta_stages_des");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_rc_eta_stages_od_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "rc_eta_stages_od", nrows, ncols);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "rc_eta_stages_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_rc_m_dot_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rc_m_dot_des", &result))
		make_access_error("SAM_Sco2CspSystem", "rc_m_dot_des");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_rc_m_dot_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "rc_m_dot_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "rc_m_dot_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_rc_n_stages_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rc_n_stages", &result))
		make_access_error("SAM_Sco2CspSystem", "rc_n_stages");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_rc_phi_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rc_phi_des", &result))
		make_access_error("SAM_Sco2CspSystem", "rc_phi_des");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_rc_phi_od_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "rc_phi_od", nrows, ncols);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "rc_phi_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_rc_phi_surge_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rc_phi_surge", &result))
		make_access_error("SAM_Sco2CspSystem", "rc_phi_surge");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_rc_psi_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rc_psi_des", &result))
		make_access_error("SAM_Sco2CspSystem", "rc_psi_des");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_rc_psi_max_at_N_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rc_psi_max_at_N_des", &result))
		make_access_error("SAM_Sco2CspSystem", "rc_psi_max_at_N_des");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_rc_psi_od_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "rc_psi_od", nrows, ncols);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "rc_psi_od");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_rc_tip_ratio_des_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "rc_tip_ratio_des", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "rc_tip_ratio_des");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_rc_tip_ratio_od_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "rc_tip_ratio_od", nrows, ncols);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "rc_tip_ratio_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_recomp_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "recomp_frac", &result))
		make_access_error("SAM_Sco2CspSystem", "recomp_frac");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_recomp_frac_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "recomp_frac_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "recomp_frac_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_recup_LTR_UA_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "recup_LTR_UA_frac", &result))
		make_access_error("SAM_Sco2CspSystem", "recup_LTR_UA_frac");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_recup_total_UA_assigned_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "recup_total_UA_assigned", &result))
		make_access_error("SAM_Sco2CspSystem", "recup_total_UA_assigned");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_recup_total_UA_calculated_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "recup_total_UA_calculated", &result))
		make_access_error("SAM_Sco2CspSystem", "recup_total_UA_calculated");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_recup_total_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "recup_total_cost", &result))
		make_access_error("SAM_Sco2CspSystem", "recup_total_cost");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_s_HTR_HP_data_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "s_HTR_HP_data", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "s_HTR_HP_data");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_s_HTR_LP_data_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "s_HTR_LP_data", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "s_HTR_LP_data");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_s_LTR_HP_data_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "s_LTR_HP_data", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "s_LTR_HP_data");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_s_LTR_LP_data_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "s_LTR_LP_data", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "s_LTR_LP_data");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_s_PHX_data_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "s_PHX_data", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "s_PHX_data");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_s_main_cooler_data_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "s_main_cooler_data", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "s_main_cooler_data");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_s_pre_cooler_data_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "s_pre_cooler_data", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "s_pre_cooler_data");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_s_state_points_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "s_state_points", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "s_state_points");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_sim_time_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "sim_time_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "sim_time_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_t_D_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "t_D", &result))
		make_access_error("SAM_Sco2CspSystem", "t_D");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_t_N_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "t_N_des", &result))
		make_access_error("SAM_Sco2CspSystem", "t_N_des");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_t_N_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "t_N_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "t_N_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_t_P_in_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "t_P_in_des", &result))
		make_access_error("SAM_Sco2CspSystem", "t_P_in_des");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_t_P_in_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "t_P_in_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "t_P_in_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_t_P_out_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "t_P_out_des", &result))
		make_access_error("SAM_Sco2CspSystem", "t_P_out_des");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_t_P_out_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "t_P_out_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "t_P_out_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_t_T_out_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "t_T_out_des", &result))
		make_access_error("SAM_Sco2CspSystem", "t_T_out_des");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_t_T_out_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "t_T_out_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "t_T_out_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_t_W_dot_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "t_W_dot", &result))
		make_access_error("SAM_Sco2CspSystem", "t_W_dot");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_t_W_dot_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "t_W_dot_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "t_W_dot_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_t_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "t_cost", &result))
		make_access_error("SAM_Sco2CspSystem", "t_cost");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_t_eta_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "t_eta_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "t_eta_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_t_m_dot_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "t_m_dot_des", &result))
		make_access_error("SAM_Sco2CspSystem", "t_m_dot_des");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_t_m_dot_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "t_m_dot_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "t_m_dot_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_t_nu_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "t_nu_des", &result))
		make_access_error("SAM_Sco2CspSystem", "t_nu_des");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_t_nu_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "t_nu_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "t_nu_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_t_tip_ratio_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "t_tip_ratio_des", &result))
		make_access_error("SAM_Sco2CspSystem", "t_tip_ratio_des");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_t_tip_ratio_od_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "t_tip_ratio_od", length);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "t_tip_ratio_od");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_udpc_n_T_amb_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "udpc_n_T_amb", &result))
		make_access_error("SAM_Sco2CspSystem", "udpc_n_T_amb");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_udpc_n_T_htf_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "udpc_n_T_htf", &result))
		make_access_error("SAM_Sco2CspSystem", "udpc_n_T_htf");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CspSystem_Outputs_udpc_n_m_dot_htf_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "udpc_n_m_dot_htf", &result))
		make_access_error("SAM_Sco2CspSystem", "udpc_n_m_dot_htf");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CspSystem_Outputs_udpc_table_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "udpc_table", nrows, ncols);
	if (!result)
		make_access_error("SAM_Sco2CspSystem", "udpc_table");
	});
	return result;
}



