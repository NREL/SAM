#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_PtesDesignPoint.h"

SAM_EXPORT int SAM_PtesDesignPoint_execute(SAM_table data, int verbosity, SAM_error* err){
	return SAM_module_exec("ptes_design_point", data, verbosity, err);
}

SAM_EXPORT void SAM_PtesDesignPoint_Common_P0_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "P0", number);
	});
}

SAM_EXPORT void SAM_PtesDesignPoint_Common_P1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "P1", number);
	});
}

SAM_EXPORT void SAM_PtesDesignPoint_Common_T0_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T0", number);
	});
}

SAM_EXPORT void SAM_PtesDesignPoint_Common_T_compressor_inlet_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_compressor_inlet", number);
	});
}

SAM_EXPORT void SAM_PtesDesignPoint_Common_T_compressor_outlet_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_compressor_outlet", number);
	});
}

SAM_EXPORT void SAM_PtesDesignPoint_Common_alpha_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "alpha", number);
	});
}

SAM_EXPORT void SAM_PtesDesignPoint_Common_charge_time_hr_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "charge_time_hr", number);
	});
}

SAM_EXPORT void SAM_PtesDesignPoint_Common_cold_fluid_id_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cold_fluid_id", number);
	});
}

SAM_EXPORT void SAM_PtesDesignPoint_Common_cold_ud_fluid_props_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "cold_ud_fluid_props", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_PtesDesignPoint_Common_discharge_time_hr_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "discharge_time_hr", number);
	});
}

SAM_EXPORT void SAM_PtesDesignPoint_Common_eta_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "eta", number);
	});
}

SAM_EXPORT void SAM_PtesDesignPoint_Common_eta_pump_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "eta_pump", number);
	});
}

SAM_EXPORT void SAM_PtesDesignPoint_Common_gen_eff_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "gen_eff", number);
	});
}

SAM_EXPORT void SAM_PtesDesignPoint_Common_hot_fluid_id_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hot_fluid_id", number);
	});
}

SAM_EXPORT void SAM_PtesDesignPoint_Common_hot_ud_fluid_props_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "hot_ud_fluid_props", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_PtesDesignPoint_Common_hx_eff_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hx_eff", number);
	});
}

SAM_EXPORT void SAM_PtesDesignPoint_Common_motor_eff_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "motor_eff", number);
	});
}

SAM_EXPORT void SAM_PtesDesignPoint_Common_ploss_air_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ploss_air", number);
	});
}

SAM_EXPORT void SAM_PtesDesignPoint_Common_ploss_liquid_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ploss_liquid", number);
	});
}

SAM_EXPORT void SAM_PtesDesignPoint_Common_ploss_working_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ploss_working", number);
	});
}

SAM_EXPORT void SAM_PtesDesignPoint_Common_power_output_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "power_output", number);
	});
}

SAM_EXPORT void SAM_PtesDesignPoint_Common_working_fluid_type_sset(SAM_table ptr, const char* str, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_string(ptr, "working_fluid_type", str);
	});
}

SAM_EXPORT double SAM_PtesDesignPoint_Common_P0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P0", &result))
		make_access_error("SAM_PtesDesignPoint", "P0");
	});
	return result;
}

SAM_EXPORT double SAM_PtesDesignPoint_Common_P1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P1", &result))
		make_access_error("SAM_PtesDesignPoint", "P1");
	});
	return result;
}

SAM_EXPORT double SAM_PtesDesignPoint_Common_T0_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T0", &result))
		make_access_error("SAM_PtesDesignPoint", "T0");
	});
	return result;
}

SAM_EXPORT double SAM_PtesDesignPoint_Common_T_compressor_inlet_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_compressor_inlet", &result))
		make_access_error("SAM_PtesDesignPoint", "T_compressor_inlet");
	});
	return result;
}

SAM_EXPORT double SAM_PtesDesignPoint_Common_T_compressor_outlet_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_compressor_outlet", &result))
		make_access_error("SAM_PtesDesignPoint", "T_compressor_outlet");
	});
	return result;
}

SAM_EXPORT double SAM_PtesDesignPoint_Common_alpha_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "alpha", &result))
		make_access_error("SAM_PtesDesignPoint", "alpha");
	});
	return result;
}

SAM_EXPORT double SAM_PtesDesignPoint_Common_charge_time_hr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "charge_time_hr", &result))
		make_access_error("SAM_PtesDesignPoint", "charge_time_hr");
	});
	return result;
}

SAM_EXPORT double SAM_PtesDesignPoint_Common_cold_fluid_id_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cold_fluid_id", &result))
		make_access_error("SAM_PtesDesignPoint", "cold_fluid_id");
	});
	return result;
}

SAM_EXPORT double* SAM_PtesDesignPoint_Common_cold_ud_fluid_props_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "cold_ud_fluid_props", nrows, ncols);
	if (!result)
		make_access_error("SAM_PtesDesignPoint", "cold_ud_fluid_props");
	});
	return result;
}

SAM_EXPORT double SAM_PtesDesignPoint_Common_discharge_time_hr_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "discharge_time_hr", &result))
		make_access_error("SAM_PtesDesignPoint", "discharge_time_hr");
	});
	return result;
}

SAM_EXPORT double SAM_PtesDesignPoint_Common_eta_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta", &result))
		make_access_error("SAM_PtesDesignPoint", "eta");
	});
	return result;
}

SAM_EXPORT double SAM_PtesDesignPoint_Common_eta_pump_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_pump", &result))
		make_access_error("SAM_PtesDesignPoint", "eta_pump");
	});
	return result;
}

SAM_EXPORT double SAM_PtesDesignPoint_Common_gen_eff_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "gen_eff", &result))
		make_access_error("SAM_PtesDesignPoint", "gen_eff");
	});
	return result;
}

SAM_EXPORT double SAM_PtesDesignPoint_Common_hot_fluid_id_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hot_fluid_id", &result))
		make_access_error("SAM_PtesDesignPoint", "hot_fluid_id");
	});
	return result;
}

SAM_EXPORT double* SAM_PtesDesignPoint_Common_hot_ud_fluid_props_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "hot_ud_fluid_props", nrows, ncols);
	if (!result)
		make_access_error("SAM_PtesDesignPoint", "hot_ud_fluid_props");
	});
	return result;
}

SAM_EXPORT double SAM_PtesDesignPoint_Common_hx_eff_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hx_eff", &result))
		make_access_error("SAM_PtesDesignPoint", "hx_eff");
	});
	return result;
}

SAM_EXPORT double SAM_PtesDesignPoint_Common_motor_eff_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "motor_eff", &result))
		make_access_error("SAM_PtesDesignPoint", "motor_eff");
	});
	return result;
}

SAM_EXPORT double SAM_PtesDesignPoint_Common_ploss_air_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ploss_air", &result))
		make_access_error("SAM_PtesDesignPoint", "ploss_air");
	});
	return result;
}

SAM_EXPORT double SAM_PtesDesignPoint_Common_ploss_liquid_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ploss_liquid", &result))
		make_access_error("SAM_PtesDesignPoint", "ploss_liquid");
	});
	return result;
}

SAM_EXPORT double SAM_PtesDesignPoint_Common_ploss_working_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ploss_working", &result))
		make_access_error("SAM_PtesDesignPoint", "ploss_working");
	});
	return result;
}

SAM_EXPORT double SAM_PtesDesignPoint_Common_power_output_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "power_output", &result))
		make_access_error("SAM_PtesDesignPoint", "power_output");
	});
	return result;
}

SAM_EXPORT const char* SAM_PtesDesignPoint_Common_working_fluid_type_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "working_fluid_type");
	if (!result)
		make_access_error("SAM_PtesDesignPoint", "working_fluid_type");
	});
	return result;
}

SAM_EXPORT double SAM_PtesDesignPoint_Outputs_N_pts_charge_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "N_pts_charge", &result))
		make_access_error("SAM_PtesDesignPoint", "N_pts_charge");
	});
	return result;
}

SAM_EXPORT double SAM_PtesDesignPoint_Outputs_N_pts_discharge_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "N_pts_discharge", &result))
		make_access_error("SAM_PtesDesignPoint", "N_pts_discharge");
	});
	return result;
}

SAM_EXPORT double SAM_PtesDesignPoint_Outputs_Tc_cold_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Tc_cold", &result))
		make_access_error("SAM_PtesDesignPoint", "Tc_cold");
	});
	return result;
}

SAM_EXPORT double SAM_PtesDesignPoint_Outputs_Tc_hot_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Tc_hot", &result))
		make_access_error("SAM_PtesDesignPoint", "Tc_hot");
	});
	return result;
}

SAM_EXPORT double SAM_PtesDesignPoint_Outputs_Th_cold_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Th_cold", &result))
		make_access_error("SAM_PtesDesignPoint", "Th_cold");
	});
	return result;
}

SAM_EXPORT double SAM_PtesDesignPoint_Outputs_Th_hot_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "Th_hot", &result))
		make_access_error("SAM_PtesDesignPoint", "Th_hot");
	});
	return result;
}

SAM_EXPORT double SAM_PtesDesignPoint_Outputs_cycle_eff_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cycle_eff", &result))
		make_access_error("SAM_PtesDesignPoint", "cycle_eff");
	});
	return result;
}

SAM_EXPORT double SAM_PtesDesignPoint_Outputs_hp_COP_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hp_COP", &result))
		make_access_error("SAM_PtesDesignPoint", "hp_COP");
	});
	return result;
}

SAM_EXPORT double SAM_PtesDesignPoint_Outputs_hp_cold_pump_power_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hp_cold_pump_power", &result))
		make_access_error("SAM_PtesDesignPoint", "hp_cold_pump_power");
	});
	return result;
}

SAM_EXPORT double SAM_PtesDesignPoint_Outputs_hp_hot_pump_power_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hp_hot_pump_power", &result))
		make_access_error("SAM_PtesDesignPoint", "hp_hot_pump_power");
	});
	return result;
}

SAM_EXPORT double SAM_PtesDesignPoint_Outputs_hp_parasitic_fraction_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hp_parasitic_fraction", &result))
		make_access_error("SAM_PtesDesignPoint", "hp_parasitic_fraction");
	});
	return result;
}

SAM_EXPORT double SAM_PtesDesignPoint_Outputs_pc_cold_pump_power_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pc_cold_pump_power", &result))
		make_access_error("SAM_PtesDesignPoint", "pc_cold_pump_power");
	});
	return result;
}

SAM_EXPORT double SAM_PtesDesignPoint_Outputs_pc_hot_pump_power_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pc_hot_pump_power", &result))
		make_access_error("SAM_PtesDesignPoint", "pc_hot_pump_power");
	});
	return result;
}

SAM_EXPORT double SAM_PtesDesignPoint_Outputs_pc_parasitic_fraction_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pc_parasitic_fraction", &result))
		make_access_error("SAM_PtesDesignPoint", "pc_parasitic_fraction");
	});
	return result;
}

SAM_EXPORT double* SAM_PtesDesignPoint_Outputs_s_series_charge_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "s_series_charge", length);
	if (!result)
		make_access_error("SAM_PtesDesignPoint", "s_series_charge");
	});
	return result;
}

SAM_EXPORT double* SAM_PtesDesignPoint_Outputs_s_series_discharge_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "s_series_discharge", length);
	if (!result)
		make_access_error("SAM_PtesDesignPoint", "s_series_discharge");
	});
	return result;
}

SAM_EXPORT double* SAM_PtesDesignPoint_Outputs_temp_series_charge_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "temp_series_charge", length);
	if (!result)
		make_access_error("SAM_PtesDesignPoint", "temp_series_charge");
	});
	return result;
}

SAM_EXPORT double* SAM_PtesDesignPoint_Outputs_temp_series_discharge_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "temp_series_discharge", length);
	if (!result)
		make_access_error("SAM_PtesDesignPoint", "temp_series_discharge");
	});
	return result;
}

