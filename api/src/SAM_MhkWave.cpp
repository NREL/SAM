#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_MhkWave.h"

SAM_EXPORT SAM_MhkWave SAM_MhkWave_construct(const char* def, SAM_error* err){
	SAM_MhkWave result = nullptr;
	translateExceptions(err, [&]{
		result = ssc_data_create();
	});
	return result;
}

SAM_EXPORT int SAM_MhkWave_execute(SAM_MhkWave data, int verbosity, SAM_error* err){
	int n_err = 0;
	translateExceptions(err, [&]{
		n_err += SAM_module_exec("mhk_wave", data, verbosity, err);
	});
	return n_err;
}


SAM_EXPORT void SAM_MhkWave_destruct(SAM_MhkWave system)
{
	ssc_data_free(system);
}

SAM_EXPORT void SAM_MhkWave_MHKWave_annual_energy_loss_nset(SAM_MhkWave ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "annual_energy_loss", number);
	});
}

SAM_EXPORT void SAM_MhkWave_MHKWave_calculate_capacity_nset(SAM_MhkWave ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "calculate_capacity", number);
	});
}

SAM_EXPORT void SAM_MhkWave_MHKWave_rated_capacity_nset(SAM_MhkWave ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rated_capacity", number);
	});
}

SAM_EXPORT void SAM_MhkWave_MHKWave_wave_power_curve_mset(SAM_MhkWave ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "wave_power_curve", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_MhkWave_MHKWave_wave_resource_definition_mset(SAM_MhkWave ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "wave_resource_definition", mat, nrows, ncols);
	});
}

SAM_EXPORT double SAM_MhkWave_MHKWave_annual_energy_loss_nget(SAM_MhkWave ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_energy_loss", &result))
		make_access_error("SAM_MhkWave", "annual_energy_loss");
	});
	return result;
}



SAM_EXPORT double SAM_MhkWave_MHKWave_calculate_capacity_nget(SAM_MhkWave ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "calculate_capacity", &result))
		make_access_error("SAM_MhkWave", "calculate_capacity");
	});
	return result;
}



SAM_EXPORT double SAM_MhkWave_MHKWave_rated_capacity_nget(SAM_MhkWave ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rated_capacity", &result))
		make_access_error("SAM_MhkWave", "rated_capacity");
	});
	return result;
}



SAM_EXPORT double* SAM_MhkWave_MHKWave_wave_power_curve_mget(SAM_MhkWave ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "wave_power_curve", nrows, ncols);
	if (!result)
		make_access_error("SAM_MhkWave", "wave_power_curve");
	});
	return result;
}



SAM_EXPORT double* SAM_MhkWave_MHKWave_wave_resource_definition_mget(SAM_MhkWave ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "wave_resource_definition", nrows, ncols);
	if (!result)
		make_access_error("SAM_MhkWave", "wave_resource_definition");
	});
	return result;
}



SAM_EXPORT double SAM_MhkWave_Outputs_annual_energy_nget(SAM_MhkWave ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_energy", &result))
		make_access_error("SAM_MhkWave", "annual_energy");
	});
	return result;
}



SAM_EXPORT double* SAM_MhkWave_Outputs_annual_energy_distribution_mget(SAM_MhkWave ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "annual_energy_distribution", nrows, ncols);
	if (!result)
		make_access_error("SAM_MhkWave", "annual_energy_distribution");
	});
	return result;
}



SAM_EXPORT double SAM_MhkWave_Outputs_average_power_nget(SAM_MhkWave ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "average_power", &result))
		make_access_error("SAM_MhkWave", "average_power");
	});
	return result;
}



SAM_EXPORT double SAM_MhkWave_Outputs_capacity_factor_nget(SAM_MhkWave ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "capacity_factor", &result))
		make_access_error("SAM_MhkWave", "capacity_factor");
	});
	return result;
}



