#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_UiTesCalcs.h"

SAM_EXPORT int SAM_UiTesCalcs_execute(SAM_table data, int verbosity, SAM_error* err){
	return SAM_module_exec("ui_tes_calcs", data, verbosity, err);
}

SAM_EXPORT void SAM_UiTesCalcs_Common_P_ref_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "P_ref", number);
	});
}

SAM_EXPORT void SAM_UiTesCalcs_Common_T_htf_cold_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_htf_cold_des", number);
	});
}

SAM_EXPORT void SAM_UiTesCalcs_Common_T_htf_hot_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_htf_hot_des", number);
	});
}

SAM_EXPORT void SAM_UiTesCalcs_Common_design_eff_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "design_eff", number);
	});
}

SAM_EXPORT void SAM_UiTesCalcs_Common_dt_hot_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dt_hot", number);
	});
}

SAM_EXPORT void SAM_UiTesCalcs_Common_field_fl_props_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "field_fl_props", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_UiTesCalcs_Common_field_fluid_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "field_fluid", number);
	});
}

SAM_EXPORT void SAM_UiTesCalcs_Common_h_tank_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "h_tank", number);
	});
}

SAM_EXPORT void SAM_UiTesCalcs_Common_h_tank_min_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "h_tank_min", number);
	});
}

SAM_EXPORT void SAM_UiTesCalcs_Common_store_fl_props_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "store_fl_props", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_UiTesCalcs_Common_store_fluid_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "store_fluid", number);
	});
}

SAM_EXPORT void SAM_UiTesCalcs_Common_tank_pairs_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tank_pairs", number);
	});
}

SAM_EXPORT void SAM_UiTesCalcs_Common_tshours_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tshours", number);
	});
}

SAM_EXPORT void SAM_UiTesCalcs_Common_u_tank_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "u_tank", number);
	});
}

SAM_EXPORT double SAM_UiTesCalcs_Common_P_ref_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_ref", &result))
		make_access_error("SAM_UiTesCalcs", "P_ref");
	});
	return result;
}



SAM_EXPORT double SAM_UiTesCalcs_Common_T_htf_cold_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_htf_cold_des", &result))
		make_access_error("SAM_UiTesCalcs", "T_htf_cold_des");
	});
	return result;
}



SAM_EXPORT double SAM_UiTesCalcs_Common_T_htf_hot_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_htf_hot_des", &result))
		make_access_error("SAM_UiTesCalcs", "T_htf_hot_des");
	});
	return result;
}



SAM_EXPORT double SAM_UiTesCalcs_Common_design_eff_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "design_eff", &result))
		make_access_error("SAM_UiTesCalcs", "design_eff");
	});
	return result;
}



SAM_EXPORT double SAM_UiTesCalcs_Common_dt_hot_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dt_hot", &result))
		make_access_error("SAM_UiTesCalcs", "dt_hot");
	});
	return result;
}



SAM_EXPORT double* SAM_UiTesCalcs_Common_field_fl_props_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "field_fl_props", nrows, ncols);
	if (!result)
		make_access_error("SAM_UiTesCalcs", "field_fl_props");
	});
	return result;
}



SAM_EXPORT double SAM_UiTesCalcs_Common_field_fluid_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "field_fluid", &result))
		make_access_error("SAM_UiTesCalcs", "field_fluid");
	});
	return result;
}



SAM_EXPORT double SAM_UiTesCalcs_Common_h_tank_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "h_tank", &result))
		make_access_error("SAM_UiTesCalcs", "h_tank");
	});
	return result;
}



SAM_EXPORT double SAM_UiTesCalcs_Common_h_tank_min_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "h_tank_min", &result))
		make_access_error("SAM_UiTesCalcs", "h_tank_min");
	});
	return result;
}



SAM_EXPORT double* SAM_UiTesCalcs_Common_store_fl_props_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "store_fl_props", nrows, ncols);
	if (!result)
		make_access_error("SAM_UiTesCalcs", "store_fl_props");
	});
	return result;
}



SAM_EXPORT double SAM_UiTesCalcs_Common_store_fluid_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "store_fluid", &result))
		make_access_error("SAM_UiTesCalcs", "store_fluid");
	});
	return result;
}



SAM_EXPORT double SAM_UiTesCalcs_Common_tank_pairs_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tank_pairs", &result))
		make_access_error("SAM_UiTesCalcs", "tank_pairs");
	});
	return result;
}



SAM_EXPORT double SAM_UiTesCalcs_Common_tshours_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tshours", &result))
		make_access_error("SAM_UiTesCalcs", "tshours");
	});
	return result;
}



SAM_EXPORT double SAM_UiTesCalcs_Common_u_tank_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "u_tank", &result))
		make_access_error("SAM_UiTesCalcs", "u_tank");
	});
	return result;
}



SAM_EXPORT double SAM_UiTesCalcs_Outputs_are_htfs_equal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "are_htfs_equal", &result))
		make_access_error("SAM_UiTesCalcs", "are_htfs_equal");
	});
	return result;
}



SAM_EXPORT double SAM_UiTesCalcs_Outputs_csp_pt_tes_htf_density_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp_pt_tes_htf_density", &result))
		make_access_error("SAM_UiTesCalcs", "csp_pt_tes_htf_density");
	});
	return result;
}



SAM_EXPORT double SAM_UiTesCalcs_Outputs_csp_pt_tes_tank_diameter_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "csp_pt_tes_tank_diameter", &result))
		make_access_error("SAM_UiTesCalcs", "csp_pt_tes_tank_diameter");
	});
	return result;
}



SAM_EXPORT double SAM_UiTesCalcs_Outputs_q_dot_tes_est_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_dot_tes_est", &result))
		make_access_error("SAM_UiTesCalcs", "q_dot_tes_est");
	});
	return result;
}



SAM_EXPORT double SAM_UiTesCalcs_Outputs_q_tes_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_tes", &result))
		make_access_error("SAM_UiTesCalcs", "q_tes");
	});
	return result;
}



SAM_EXPORT double SAM_UiTesCalcs_Outputs_tes_avail_vol_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tes_avail_vol", &result))
		make_access_error("SAM_UiTesCalcs", "tes_avail_vol");
	});
	return result;
}



SAM_EXPORT double SAM_UiTesCalcs_Outputs_vol_tank_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "vol_tank", &result))
		make_access_error("SAM_UiTesCalcs", "vol_tank");
	});
	return result;
}



