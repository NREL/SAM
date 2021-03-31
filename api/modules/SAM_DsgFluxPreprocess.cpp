#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_DsgFluxPreprocess.h"

SAM_EXPORT int SAM_DsgFluxPreprocess_execute(SAM_table data, int verbosity, SAM_error* err){
	int n_err = 0;
	translateExceptions(err, [&]{
		n_err += SAM_module_exec("dsg_flux_preprocess", data, verbosity, err);
	});
	return n_err;
}


SAM_EXPORT void SAM_DsgFluxPreprocess_Common_CT_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "CT", number);
	});
}

SAM_EXPORT void SAM_DsgFluxPreprocess_Common_P_HP_in_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "P_HP_in", number);
	});
}

SAM_EXPORT void SAM_DsgFluxPreprocess_Common_P_HP_out_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "P_HP_out", number);
	});
}

SAM_EXPORT void SAM_DsgFluxPreprocess_Common_P_cycle_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "P_cycle_des", number);
	});
}

SAM_EXPORT void SAM_DsgFluxPreprocess_Common_Q_rec_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "Q_rec_des", number);
	});
}

SAM_EXPORT void SAM_DsgFluxPreprocess_Common_T_ITD_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_ITD_des", number);
	});
}

SAM_EXPORT void SAM_DsgFluxPreprocess_Common_T_amb_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_amb_des", number);
	});
}

SAM_EXPORT void SAM_DsgFluxPreprocess_Common_T_approach_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_approach", number);
	});
}

SAM_EXPORT void SAM_DsgFluxPreprocess_Common_T_rh_out_ref_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_rh_out_ref", number);
	});
}

SAM_EXPORT void SAM_DsgFluxPreprocess_Common_T_sh_out_ref_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_sh_out_ref", number);
	});
}

SAM_EXPORT void SAM_DsgFluxPreprocess_Common_b_q_loss_flux_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "b_q_loss_flux", number);
	});
}

SAM_EXPORT void SAM_DsgFluxPreprocess_Common_dT_cooling_ref_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dT_cooling_ref", number);
	});
}

SAM_EXPORT void SAM_DsgFluxPreprocess_Common_eta_cycle_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "eta_cycle_des", number);
	});
}

SAM_EXPORT void SAM_DsgFluxPreprocess_Common_max_flux_b_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "max_flux_b", number);
	});
}

SAM_EXPORT void SAM_DsgFluxPreprocess_Common_max_flux_rh_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "max_flux_rh", number);
	});
}

SAM_EXPORT void SAM_DsgFluxPreprocess_Common_max_flux_sh_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "max_flux_sh", number);
	});
}

SAM_EXPORT void SAM_DsgFluxPreprocess_Common_rh_frac_ref_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rh_frac_ref", number);
	});
}

SAM_EXPORT void SAM_DsgFluxPreprocess_Common_rh_q_loss_flux_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rh_q_loss_flux", number);
	});
}

SAM_EXPORT void SAM_DsgFluxPreprocess_Common_sh_q_loss_flux_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sh_q_loss_flux", number);
	});
}

SAM_EXPORT double SAM_DsgFluxPreprocess_Common_CT_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "CT", &result))
		make_access_error("SAM_DsgFluxPreprocess", "CT");
	});
	return result;
}



SAM_EXPORT double SAM_DsgFluxPreprocess_Common_P_HP_in_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_HP_in", &result))
		make_access_error("SAM_DsgFluxPreprocess", "P_HP_in");
	});
	return result;
}



SAM_EXPORT double SAM_DsgFluxPreprocess_Common_P_HP_out_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_HP_out", &result))
		make_access_error("SAM_DsgFluxPreprocess", "P_HP_out");
	});
	return result;
}



SAM_EXPORT double SAM_DsgFluxPreprocess_Common_P_cycle_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_cycle_des", &result))
		make_access_error("SAM_DsgFluxPreprocess", "P_cycle_des");
	});
	return result;
}



SAM_EXPORT double SAM_DsgFluxPreprocess_Common_Q_rec_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Q_rec_des", &result))
		make_access_error("SAM_DsgFluxPreprocess", "Q_rec_des");
	});
	return result;
}



SAM_EXPORT double SAM_DsgFluxPreprocess_Common_T_ITD_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_ITD_des", &result))
		make_access_error("SAM_DsgFluxPreprocess", "T_ITD_des");
	});
	return result;
}



SAM_EXPORT double SAM_DsgFluxPreprocess_Common_T_amb_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_amb_des", &result))
		make_access_error("SAM_DsgFluxPreprocess", "T_amb_des");
	});
	return result;
}



SAM_EXPORT double SAM_DsgFluxPreprocess_Common_T_approach_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_approach", &result))
		make_access_error("SAM_DsgFluxPreprocess", "T_approach");
	});
	return result;
}



SAM_EXPORT double SAM_DsgFluxPreprocess_Common_T_rh_out_ref_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_rh_out_ref", &result))
		make_access_error("SAM_DsgFluxPreprocess", "T_rh_out_ref");
	});
	return result;
}



SAM_EXPORT double SAM_DsgFluxPreprocess_Common_T_sh_out_ref_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_sh_out_ref", &result))
		make_access_error("SAM_DsgFluxPreprocess", "T_sh_out_ref");
	});
	return result;
}



SAM_EXPORT double SAM_DsgFluxPreprocess_Common_b_q_loss_flux_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "b_q_loss_flux", &result))
		make_access_error("SAM_DsgFluxPreprocess", "b_q_loss_flux");
	});
	return result;
}



SAM_EXPORT double SAM_DsgFluxPreprocess_Common_dT_cooling_ref_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dT_cooling_ref", &result))
		make_access_error("SAM_DsgFluxPreprocess", "dT_cooling_ref");
	});
	return result;
}



SAM_EXPORT double SAM_DsgFluxPreprocess_Common_eta_cycle_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_cycle_des", &result))
		make_access_error("SAM_DsgFluxPreprocess", "eta_cycle_des");
	});
	return result;
}



SAM_EXPORT double SAM_DsgFluxPreprocess_Common_max_flux_b_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "max_flux_b", &result))
		make_access_error("SAM_DsgFluxPreprocess", "max_flux_b");
	});
	return result;
}



SAM_EXPORT double SAM_DsgFluxPreprocess_Common_max_flux_rh_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "max_flux_rh", &result))
		make_access_error("SAM_DsgFluxPreprocess", "max_flux_rh");
	});
	return result;
}



SAM_EXPORT double SAM_DsgFluxPreprocess_Common_max_flux_sh_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "max_flux_sh", &result))
		make_access_error("SAM_DsgFluxPreprocess", "max_flux_sh");
	});
	return result;
}



SAM_EXPORT double SAM_DsgFluxPreprocess_Common_rh_frac_ref_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rh_frac_ref", &result))
		make_access_error("SAM_DsgFluxPreprocess", "rh_frac_ref");
	});
	return result;
}



SAM_EXPORT double SAM_DsgFluxPreprocess_Common_rh_q_loss_flux_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rh_q_loss_flux", &result))
		make_access_error("SAM_DsgFluxPreprocess", "rh_q_loss_flux");
	});
	return result;
}



SAM_EXPORT double SAM_DsgFluxPreprocess_Common_sh_q_loss_flux_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sh_q_loss_flux", &result))
		make_access_error("SAM_DsgFluxPreprocess", "sh_q_loss_flux");
	});
	return result;
}



SAM_EXPORT double SAM_DsgFluxPreprocess_Outputs_f_b_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "f_b", &result))
		make_access_error("SAM_DsgFluxPreprocess", "f_b");
	});
	return result;
}



SAM_EXPORT double SAM_DsgFluxPreprocess_Outputs_f_rh_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "f_rh", &result))
		make_access_error("SAM_DsgFluxPreprocess", "f_rh");
	});
	return result;
}



SAM_EXPORT double SAM_DsgFluxPreprocess_Outputs_f_sh_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "f_sh", &result))
		make_access_error("SAM_DsgFluxPreprocess", "f_sh");
	});
	return result;
}



SAM_EXPORT double SAM_DsgFluxPreprocess_Outputs_max_flux_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "max_flux", &result))
		make_access_error("SAM_DsgFluxPreprocess", "max_flux");
	});
	return result;
}



