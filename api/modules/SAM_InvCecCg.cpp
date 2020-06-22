#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_InvCecCg.h"

SAM_EXPORT int SAM_InvCecCg_execute(SAM_table data, int verbosity, SAM_error* err){
	int n_err = 0;
	translateExceptions(err, [&]{
		n_err += SAM_module_exec("inv_cec_cg", data, verbosity, err);
	});
	return n_err;
}


SAM_EXPORT void SAM_InvCecCg_Common_inv_cec_cg_paco_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inv_cec_cg_paco", number);
	});
}

SAM_EXPORT void SAM_InvCecCg_Common_inv_cec_cg_sample_power_units_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inv_cec_cg_sample_power_units", number);
	});
}

SAM_EXPORT void SAM_InvCecCg_Common_inv_cec_cg_test_samples_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "inv_cec_cg_test_samples", mat, nrows, ncols);
	});
}

SAM_EXPORT double SAM_InvCecCg_Common_inv_cec_cg_paco_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inv_cec_cg_paco", &result))
		make_access_error("SAM_InvCecCg", "inv_cec_cg_paco");
	});
	return result;
}



SAM_EXPORT double SAM_InvCecCg_Common_inv_cec_cg_sample_power_units_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inv_cec_cg_sample_power_units", &result))
		make_access_error("SAM_InvCecCg", "inv_cec_cg_sample_power_units");
	});
	return result;
}



SAM_EXPORT double* SAM_InvCecCg_Common_inv_cec_cg_test_samples_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "inv_cec_cg_test_samples", nrows, ncols);
	if (!result)
		make_access_error("SAM_InvCecCg", "inv_cec_cg_test_samples");
	});
	return result;
}



SAM_EXPORT double SAM_InvCecCg_Outputs_Pdco_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Pdco", &result))
		make_access_error("SAM_InvCecCg", "Pdco");
	});
	return result;
}



SAM_EXPORT double SAM_InvCecCg_Outputs_Pso_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Pso", &result))
		make_access_error("SAM_InvCecCg", "Pso");
	});
	return result;
}



SAM_EXPORT double SAM_InvCecCg_Outputs_Vdco_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Vdco", &result))
		make_access_error("SAM_InvCecCg", "Vdco");
	});
	return result;
}



SAM_EXPORT double SAM_InvCecCg_Outputs_c0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "c0", &result))
		make_access_error("SAM_InvCecCg", "c0");
	});
	return result;
}



SAM_EXPORT double SAM_InvCecCg_Outputs_c1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "c1", &result))
		make_access_error("SAM_InvCecCg", "c1");
	});
	return result;
}



SAM_EXPORT double SAM_InvCecCg_Outputs_c2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "c2", &result))
		make_access_error("SAM_InvCecCg", "c2");
	});
	return result;
}



SAM_EXPORT double SAM_InvCecCg_Outputs_c3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "c3", &result))
		make_access_error("SAM_InvCecCg", "c3");
	});
	return result;
}



SAM_EXPORT double* SAM_InvCecCg_Outputs_inv_cec_cg_C0_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "inv_cec_cg_C0", length);
	if (!result)
		make_access_error("SAM_InvCecCg", "inv_cec_cg_C0");
	});
	return result;
}



SAM_EXPORT double* SAM_InvCecCg_Outputs_inv_cec_cg_C1_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "inv_cec_cg_C1", length);
	if (!result)
		make_access_error("SAM_InvCecCg", "inv_cec_cg_C1");
	});
	return result;
}



SAM_EXPORT double* SAM_InvCecCg_Outputs_inv_cec_cg_C2_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "inv_cec_cg_C2", length);
	if (!result)
		make_access_error("SAM_InvCecCg", "inv_cec_cg_C2");
	});
	return result;
}



SAM_EXPORT double* SAM_InvCecCg_Outputs_inv_cec_cg_C3_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "inv_cec_cg_C3", length);
	if (!result)
		make_access_error("SAM_InvCecCg", "inv_cec_cg_C3");
	});
	return result;
}



SAM_EXPORT double* SAM_InvCecCg_Outputs_inv_cec_cg_Pdco_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "inv_cec_cg_Pdco", length);
	if (!result)
		make_access_error("SAM_InvCecCg", "inv_cec_cg_Pdco");
	});
	return result;
}



SAM_EXPORT double* SAM_InvCecCg_Outputs_inv_cec_cg_Psco_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "inv_cec_cg_Psco", length);
	if (!result)
		make_access_error("SAM_InvCecCg", "inv_cec_cg_Psco");
	});
	return result;
}



SAM_EXPORT double* SAM_InvCecCg_Outputs_inv_cec_cg_Vdc_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "inv_cec_cg_Vdc", length);
	if (!result)
		make_access_error("SAM_InvCecCg", "inv_cec_cg_Vdc");
	});
	return result;
}



SAM_EXPORT double* SAM_InvCecCg_Outputs_inv_cec_cg_Vdc_Vnom_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "inv_cec_cg_Vdc_Vnom", length);
	if (!result)
		make_access_error("SAM_InvCecCg", "inv_cec_cg_Vdc_Vnom");
	});
	return result;
}



SAM_EXPORT double* SAM_InvCecCg_Outputs_inv_cec_cg_Vmax_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "inv_cec_cg_Vmax", nrows, ncols);
	if (!result)
		make_access_error("SAM_InvCecCg", "inv_cec_cg_Vmax");
	});
	return result;
}



SAM_EXPORT double* SAM_InvCecCg_Outputs_inv_cec_cg_Vmax_abc_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "inv_cec_cg_Vmax_abc", length);
	if (!result)
		make_access_error("SAM_InvCecCg", "inv_cec_cg_Vmax_abc");
	});
	return result;
}



SAM_EXPORT double* SAM_InvCecCg_Outputs_inv_cec_cg_Vmin_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "inv_cec_cg_Vmin", nrows, ncols);
	if (!result)
		make_access_error("SAM_InvCecCg", "inv_cec_cg_Vmin");
	});
	return result;
}



SAM_EXPORT double* SAM_InvCecCg_Outputs_inv_cec_cg_Vmin_abc_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "inv_cec_cg_Vmin_abc", length);
	if (!result)
		make_access_error("SAM_InvCecCg", "inv_cec_cg_Vmin_abc");
	});
	return result;
}



SAM_EXPORT double* SAM_InvCecCg_Outputs_inv_cec_cg_Vnom_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "inv_cec_cg_Vnom", nrows, ncols);
	if (!result)
		make_access_error("SAM_InvCecCg", "inv_cec_cg_Vnom");
	});
	return result;
}



SAM_EXPORT double* SAM_InvCecCg_Outputs_inv_cec_cg_Vnom_abc_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "inv_cec_cg_Vnom_abc", length);
	if (!result)
		make_access_error("SAM_InvCecCg", "inv_cec_cg_Vnom_abc");
	});
	return result;
}



