#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_Sco2CompCurves.h"

SAM_EXPORT int SAM_Sco2CompCurves_execute(SAM_table data, int verbosity, SAM_error* err){
	int n_err = 0;
	translateExceptions(err, [&]{
		n_err += SAM_module_exec("sco2_comp_curves", data, verbosity, err);
	});
	return n_err;
}


SAM_EXPORT void SAM_Sco2CompCurves_Common_P_comp_in_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "P_comp_in", number);
	});
}

SAM_EXPORT void SAM_Sco2CompCurves_Common_T_comp_in_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_comp_in", number);
	});
}

SAM_EXPORT void SAM_Sco2CompCurves_Common_comp_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "comp_type", number);
	});
}

SAM_EXPORT double SAM_Sco2CompCurves_Common_P_comp_in_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_comp_in", &result))
		make_access_error("SAM_Sco2CompCurves", "P_comp_in");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CompCurves_Common_T_comp_in_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_comp_in", &result))
		make_access_error("SAM_Sco2CompCurves", "T_comp_in");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CompCurves_Common_comp_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "comp_type", &result))
		make_access_error("SAM_Sco2CompCurves", "comp_type");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CompCurves_Outputs_eta_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "eta", length);
	if (!result)
		make_access_error("SAM_Sco2CompCurves", "eta");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CompCurves_Outputs_eta_ND_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "eta_ND", length);
	if (!result)
		make_access_error("SAM_Sco2CompCurves", "eta_ND");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CompCurves_Outputs_eta_norm_design_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_norm_design", &result))
		make_access_error("SAM_Sco2CompCurves", "eta_norm_design");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CompCurves_Outputs_phi_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "phi", length);
	if (!result)
		make_access_error("SAM_Sco2CompCurves", "phi");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CompCurves_Outputs_phi_ND_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "phi_ND", length);
	if (!result)
		make_access_error("SAM_Sco2CompCurves", "phi_ND");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CompCurves_Outputs_phi_design_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "phi_design", &result))
		make_access_error("SAM_Sco2CompCurves", "phi_design");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CompCurves_Outputs_psi_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "psi", length);
	if (!result)
		make_access_error("SAM_Sco2CompCurves", "psi");
	});
	return result;
}



SAM_EXPORT double* SAM_Sco2CompCurves_Outputs_psi_ND_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "psi_ND", length);
	if (!result)
		make_access_error("SAM_Sco2CompCurves", "psi_ND");
	});
	return result;
}



SAM_EXPORT double SAM_Sco2CompCurves_Outputs_psi_design_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "psi_design", &result))
		make_access_error("SAM_Sco2CompCurves", "psi_design");
	});
	return result;
}



