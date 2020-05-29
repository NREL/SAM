#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_MhkTidal.h"

SAM_EXPORT int SAM_MhkTidal_execute(SAM_table data, int verbosity, SAM_error* err){
	int n_err = 0;
	translateExceptions(err, [&]{
		n_err += SAM_module_exec("mhk_tidal", data, verbosity, err);
	});
	return n_err;
}


SAM_EXPORT void SAM_MhkTidal_MHKTidal_loss_additional_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "loss_additional", number);
	});
}

SAM_EXPORT void SAM_MhkTidal_MHKTidal_loss_array_spacing_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "loss_array_spacing", number);
	});
}

SAM_EXPORT void SAM_MhkTidal_MHKTidal_loss_downtime_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "loss_downtime", number);
	});
}

SAM_EXPORT void SAM_MhkTidal_MHKTidal_loss_resource_overprediction_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "loss_resource_overprediction", number);
	});
}

SAM_EXPORT void SAM_MhkTidal_MHKTidal_loss_transmission_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "loss_transmission", number);
	});
}

SAM_EXPORT void SAM_MhkTidal_MHKTidal_number_devices_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "number_devices", number);
	});
}

SAM_EXPORT void SAM_MhkTidal_MHKTidal_tidal_power_curve_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "tidal_power_curve", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_MhkTidal_MHKTidal_tidal_resource_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "tidal_resource", mat, nrows, ncols);
	});
}

SAM_EXPORT double SAM_MhkTidal_MHKTidal_loss_additional_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "loss_additional", &result))
		make_access_error("SAM_MhkTidal", "loss_additional");
	});
	return result;
}



SAM_EXPORT double SAM_MhkTidal_MHKTidal_loss_array_spacing_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "loss_array_spacing", &result))
		make_access_error("SAM_MhkTidal", "loss_array_spacing");
	});
	return result;
}



SAM_EXPORT double SAM_MhkTidal_MHKTidal_loss_downtime_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "loss_downtime", &result))
		make_access_error("SAM_MhkTidal", "loss_downtime");
	});
	return result;
}



SAM_EXPORT double SAM_MhkTidal_MHKTidal_loss_resource_overprediction_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "loss_resource_overprediction", &result))
		make_access_error("SAM_MhkTidal", "loss_resource_overprediction");
	});
	return result;
}



SAM_EXPORT double SAM_MhkTidal_MHKTidal_loss_transmission_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "loss_transmission", &result))
		make_access_error("SAM_MhkTidal", "loss_transmission");
	});
	return result;
}



SAM_EXPORT double SAM_MhkTidal_MHKTidal_number_devices_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "number_devices", &result))
		make_access_error("SAM_MhkTidal", "number_devices");
	});
	return result;
}



SAM_EXPORT double* SAM_MhkTidal_MHKTidal_tidal_power_curve_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "tidal_power_curve", nrows, ncols);
	if (!result)
		make_access_error("SAM_MhkTidal", "tidal_power_curve");
	});
	return result;
}



SAM_EXPORT double* SAM_MhkTidal_MHKTidal_tidal_resource_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "tidal_resource", nrows, ncols);
	if (!result)
		make_access_error("SAM_MhkTidal", "tidal_resource");
	});
	return result;
}



SAM_EXPORT double* SAM_MhkTidal_Outputs_annual_cumulative_energy_distribution_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "annual_cumulative_energy_distribution", length);
	if (!result)
		make_access_error("SAM_MhkTidal", "annual_cumulative_energy_distribution");
	});
	return result;
}



SAM_EXPORT double SAM_MhkTidal_Outputs_annual_energy_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_energy", &result))
		make_access_error("SAM_MhkTidal", "annual_energy");
	});
	return result;
}



SAM_EXPORT double* SAM_MhkTidal_Outputs_annual_energy_distribution_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "annual_energy_distribution", length);
	if (!result)
		make_access_error("SAM_MhkTidal", "annual_energy_distribution");
	});
	return result;
}



SAM_EXPORT double SAM_MhkTidal_Outputs_capacity_factor_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "capacity_factor", &result))
		make_access_error("SAM_MhkTidal", "capacity_factor");
	});
	return result;
}



SAM_EXPORT double SAM_MhkTidal_Outputs_device_average_power_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "device_average_power", &result))
		make_access_error("SAM_MhkTidal", "device_average_power");
	});
	return result;
}



SAM_EXPORT double SAM_MhkTidal_Outputs_device_rated_capacity_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "device_rated_capacity", &result))
		make_access_error("SAM_MhkTidal", "device_rated_capacity");
	});
	return result;
}



