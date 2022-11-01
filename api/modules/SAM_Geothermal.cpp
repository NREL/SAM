#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_Geothermal.h"

SAM_EXPORT int SAM_Geothermal_execute(SAM_table data, int verbosity, SAM_error* err){
	return SAM_module_exec("geothermal", data, verbosity, err);
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_CT_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "CT", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_HTF_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "HTF", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_P_boil_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "P_boil", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_P_cond_min_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "P_cond_min", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_P_cond_ratio_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "P_cond_ratio", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_T_ITD_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_ITD_des", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_T_amb_des_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_amb_des", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_T_approach_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_approach", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_T_htf_cold_ref_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_htf_cold_ref", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_T_htf_hot_ref_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "T_htf_hot_ref", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_ambient_pressure_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ambient_pressure", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_analysis_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "analysis_type", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_casing_size_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "casing_size", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_conversion_subtype_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "conversion_subtype", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_conversion_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "conversion_type", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_dT_cw_ref_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dT_cw_ref", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_decline_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "decline_type", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_delta_pressure_equip_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "delta_pressure_equip", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_design_temp_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "design_temp", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_dt_prod_well_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dt_prod_well", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_eta_ref_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "eta_ref", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_excess_pressure_pump_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "excess_pressure_pump", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_file_name_sset(SAM_table ptr, const char* str, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_string(ptr, "file_name", str);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_fracture_angle_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fracture_angle", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_fracture_aperature_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fracture_aperature", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_fracture_length_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fracture_length", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_fracture_spacing_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fracture_spacing", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_fracture_width_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "fracture_width", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_inj_cost_curve_welltype_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "geotherm.cost.inj_cost_curve_welltype", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_inj_prod_well_ratio_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "geotherm.cost.inj_prod_well_ratio", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_prod_cost_curve_welltype_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "geotherm.cost.prod_cost_curve_welltype", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_geothermal_analysis_period_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "geothermal_analysis_period", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_hc_ctl1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hc_ctl1", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_hc_ctl2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hc_ctl2", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_hc_ctl3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hc_ctl3", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_hc_ctl4_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hc_ctl4", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_hc_ctl5_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hc_ctl5", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_hc_ctl6_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hc_ctl6", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_hc_ctl7_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hc_ctl7", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_hc_ctl8_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hc_ctl8", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_hc_ctl9_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hc_ctl9", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_hr_pl_nlev_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hr_pl_nlev", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_hybrid_dispatch_schedule_sset(SAM_table ptr, const char* str, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_string(ptr, "hybrid_dispatch_schedule", str);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_inj_casing_size_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inj_casing_size", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_inj_prod_well_distance_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inj_prod_well_distance", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_inj_well_diam_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inj_well_diam", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_model_choice_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "model_choice", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_nameplate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "nameplate", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_num_fractures_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "num_fractures", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_num_wells_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "num_wells", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_num_wells_getem_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "num_wells_getem", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_pb_bd_frac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pb_bd_frac", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_plant_efficiency_input_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "plant_efficiency_input", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_prod_well_choice_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "prod_well_choice", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_pump_efficiency_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pump_efficiency", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_q_sby_frac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "q_sby_frac", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_reservoir_height_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "reservoir_height", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_reservoir_model_inputs_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "reservoir_model_inputs", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_reservoir_permeability_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "reservoir_permeability", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_reservoir_pressure_change_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "reservoir_pressure_change", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_reservoir_pressure_change_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "reservoir_pressure_change_type", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_reservoir_width_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "reservoir_width", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_resource_depth_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "resource_depth", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_resource_potential_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "resource_potential", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_resource_temp_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "resource_temp", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_resource_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "resource_type", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_rock_density_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rock_density", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_rock_specific_heat_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rock_specific_heat", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_rock_thermal_conductivity_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rock_thermal_conductivity", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_specified_pump_work_amount_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "specified_pump_work_amount", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_specify_pump_work_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "specify_pump_work", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_startup_frac_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "startup_frac", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_startup_time_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "startup_time", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_subsurface_water_loss_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subsurface_water_loss", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_system_use_lifetime_output_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "system_use_lifetime_output", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_temp_decline_max_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "temp_decline_max", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_temp_decline_rate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "temp_decline_rate", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_ui_calculations_only_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ui_calculations_only", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_well_diameter_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "well_diameter", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_well_flow_rate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "well_flow_rate", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_wet_bulb_temp_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "wet_bulb_temp", number);
	});
}

SAM_EXPORT double SAM_Geothermal_GeoHourly_CT_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "CT", &result))
		make_access_error("SAM_Geothermal", "CT");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_HTF_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "HTF", &result))
		make_access_error("SAM_Geothermal", "HTF");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_P_boil_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_boil", &result))
		make_access_error("SAM_Geothermal", "P_boil");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_P_cond_min_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_cond_min", &result))
		make_access_error("SAM_Geothermal", "P_cond_min");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_P_cond_ratio_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "P_cond_ratio", &result))
		make_access_error("SAM_Geothermal", "P_cond_ratio");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_T_ITD_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_ITD_des", &result))
		make_access_error("SAM_Geothermal", "T_ITD_des");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_T_amb_des_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_amb_des", &result))
		make_access_error("SAM_Geothermal", "T_amb_des");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_T_approach_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_approach", &result))
		make_access_error("SAM_Geothermal", "T_approach");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_T_htf_cold_ref_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_htf_cold_ref", &result))
		make_access_error("SAM_Geothermal", "T_htf_cold_ref");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_T_htf_hot_ref_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "T_htf_hot_ref", &result))
		make_access_error("SAM_Geothermal", "T_htf_hot_ref");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_ambient_pressure_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ambient_pressure", &result))
		make_access_error("SAM_Geothermal", "ambient_pressure");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_analysis_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "analysis_type", &result))
		make_access_error("SAM_Geothermal", "analysis_type");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_casing_size_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "casing_size", &result))
		make_access_error("SAM_Geothermal", "casing_size");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_conversion_subtype_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "conversion_subtype", &result))
		make_access_error("SAM_Geothermal", "conversion_subtype");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_conversion_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "conversion_type", &result))
		make_access_error("SAM_Geothermal", "conversion_type");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_dT_cw_ref_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dT_cw_ref", &result))
		make_access_error("SAM_Geothermal", "dT_cw_ref");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_decline_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "decline_type", &result))
		make_access_error("SAM_Geothermal", "decline_type");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_delta_pressure_equip_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "delta_pressure_equip", &result))
		make_access_error("SAM_Geothermal", "delta_pressure_equip");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_design_temp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "design_temp", &result))
		make_access_error("SAM_Geothermal", "design_temp");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_dt_prod_well_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dt_prod_well", &result))
		make_access_error("SAM_Geothermal", "dt_prod_well");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_eta_ref_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eta_ref", &result))
		make_access_error("SAM_Geothermal", "eta_ref");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_excess_pressure_pump_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "excess_pressure_pump", &result))
		make_access_error("SAM_Geothermal", "excess_pressure_pump");
	});
	return result;
}



SAM_EXPORT const char* SAM_Geothermal_GeoHourly_file_name_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "file_name");
	if (!result)
		make_access_error("SAM_Geothermal", "file_name");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_fracture_angle_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fracture_angle", &result))
		make_access_error("SAM_Geothermal", "fracture_angle");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_fracture_aperature_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fracture_aperature", &result))
		make_access_error("SAM_Geothermal", "fracture_aperature");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_fracture_length_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fracture_length", &result))
		make_access_error("SAM_Geothermal", "fracture_length");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_fracture_spacing_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fracture_spacing", &result))
		make_access_error("SAM_Geothermal", "fracture_spacing");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_fracture_width_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fracture_width", &result))
		make_access_error("SAM_Geothermal", "fracture_width");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_inj_cost_curve_welltype_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "geotherm.cost.inj_cost_curve_welltype", &result))
		make_access_error("SAM_Geothermal", "geotherm.cost.inj_cost_curve_welltype");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_inj_prod_well_ratio_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "geotherm.cost.inj_prod_well_ratio", &result))
		make_access_error("SAM_Geothermal", "geotherm.cost.inj_prod_well_ratio");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_prod_cost_curve_welltype_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "geotherm.cost.prod_cost_curve_welltype", &result))
		make_access_error("SAM_Geothermal", "geotherm.cost.prod_cost_curve_welltype");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_geothermal_analysis_period_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "geothermal_analysis_period", &result))
		make_access_error("SAM_Geothermal", "geothermal_analysis_period");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_hc_ctl1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hc_ctl1", &result))
		make_access_error("SAM_Geothermal", "hc_ctl1");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_hc_ctl2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hc_ctl2", &result))
		make_access_error("SAM_Geothermal", "hc_ctl2");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_hc_ctl3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hc_ctl3", &result))
		make_access_error("SAM_Geothermal", "hc_ctl3");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_hc_ctl4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hc_ctl4", &result))
		make_access_error("SAM_Geothermal", "hc_ctl4");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_hc_ctl5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hc_ctl5", &result))
		make_access_error("SAM_Geothermal", "hc_ctl5");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_hc_ctl6_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hc_ctl6", &result))
		make_access_error("SAM_Geothermal", "hc_ctl6");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_hc_ctl7_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hc_ctl7", &result))
		make_access_error("SAM_Geothermal", "hc_ctl7");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_hc_ctl8_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hc_ctl8", &result))
		make_access_error("SAM_Geothermal", "hc_ctl8");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_hc_ctl9_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hc_ctl9", &result))
		make_access_error("SAM_Geothermal", "hc_ctl9");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_hr_pl_nlev_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hr_pl_nlev", &result))
		make_access_error("SAM_Geothermal", "hr_pl_nlev");
	});
	return result;
}



SAM_EXPORT const char* SAM_Geothermal_GeoHourly_hybrid_dispatch_schedule_sget(SAM_table ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "hybrid_dispatch_schedule");
	if (!result)
		make_access_error("SAM_Geothermal", "hybrid_dispatch_schedule");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_inj_casing_size_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inj_casing_size", &result))
		make_access_error("SAM_Geothermal", "inj_casing_size");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_inj_prod_well_distance_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inj_prod_well_distance", &result))
		make_access_error("SAM_Geothermal", "inj_prod_well_distance");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_inj_well_diam_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inj_well_diam", &result))
		make_access_error("SAM_Geothermal", "inj_well_diam");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_model_choice_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "model_choice", &result))
		make_access_error("SAM_Geothermal", "model_choice");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_nameplate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "nameplate", &result))
		make_access_error("SAM_Geothermal", "nameplate");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_num_fractures_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "num_fractures", &result))
		make_access_error("SAM_Geothermal", "num_fractures");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_num_wells_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "num_wells", &result))
		make_access_error("SAM_Geothermal", "num_wells");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_num_wells_getem_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "num_wells_getem", &result))
		make_access_error("SAM_Geothermal", "num_wells_getem");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_pb_bd_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pb_bd_frac", &result))
		make_access_error("SAM_Geothermal", "pb_bd_frac");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_plant_efficiency_input_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "plant_efficiency_input", &result))
		make_access_error("SAM_Geothermal", "plant_efficiency_input");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_prod_well_choice_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "prod_well_choice", &result))
		make_access_error("SAM_Geothermal", "prod_well_choice");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_pump_efficiency_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pump_efficiency", &result))
		make_access_error("SAM_Geothermal", "pump_efficiency");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_q_sby_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "q_sby_frac", &result))
		make_access_error("SAM_Geothermal", "q_sby_frac");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_reservoir_height_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "reservoir_height", &result))
		make_access_error("SAM_Geothermal", "reservoir_height");
	});
	return result;
}



SAM_EXPORT double* SAM_Geothermal_GeoHourly_reservoir_model_inputs_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "reservoir_model_inputs", nrows, ncols);
	if (!result)
		make_access_error("SAM_Geothermal", "reservoir_model_inputs");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_reservoir_permeability_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "reservoir_permeability", &result))
		make_access_error("SAM_Geothermal", "reservoir_permeability");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_reservoir_pressure_change_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "reservoir_pressure_change", &result))
		make_access_error("SAM_Geothermal", "reservoir_pressure_change");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_reservoir_pressure_change_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "reservoir_pressure_change_type", &result))
		make_access_error("SAM_Geothermal", "reservoir_pressure_change_type");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_reservoir_width_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "reservoir_width", &result))
		make_access_error("SAM_Geothermal", "reservoir_width");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_resource_depth_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "resource_depth", &result))
		make_access_error("SAM_Geothermal", "resource_depth");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_resource_potential_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "resource_potential", &result))
		make_access_error("SAM_Geothermal", "resource_potential");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_resource_temp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "resource_temp", &result))
		make_access_error("SAM_Geothermal", "resource_temp");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_resource_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "resource_type", &result))
		make_access_error("SAM_Geothermal", "resource_type");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_rock_density_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rock_density", &result))
		make_access_error("SAM_Geothermal", "rock_density");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_rock_specific_heat_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rock_specific_heat", &result))
		make_access_error("SAM_Geothermal", "rock_specific_heat");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_rock_thermal_conductivity_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rock_thermal_conductivity", &result))
		make_access_error("SAM_Geothermal", "rock_thermal_conductivity");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_specified_pump_work_amount_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "specified_pump_work_amount", &result))
		make_access_error("SAM_Geothermal", "specified_pump_work_amount");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_specify_pump_work_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "specify_pump_work", &result))
		make_access_error("SAM_Geothermal", "specify_pump_work");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_startup_frac_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "startup_frac", &result))
		make_access_error("SAM_Geothermal", "startup_frac");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_startup_time_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "startup_time", &result))
		make_access_error("SAM_Geothermal", "startup_time");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_subsurface_water_loss_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subsurface_water_loss", &result))
		make_access_error("SAM_Geothermal", "subsurface_water_loss");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_system_use_lifetime_output_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_use_lifetime_output", &result))
		make_access_error("SAM_Geothermal", "system_use_lifetime_output");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_temp_decline_max_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "temp_decline_max", &result))
		make_access_error("SAM_Geothermal", "temp_decline_max");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_temp_decline_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "temp_decline_rate", &result))
		make_access_error("SAM_Geothermal", "temp_decline_rate");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_ui_calculations_only_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ui_calculations_only", &result))
		make_access_error("SAM_Geothermal", "ui_calculations_only");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_well_diameter_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "well_diameter", &result))
		make_access_error("SAM_Geothermal", "well_diameter");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_well_flow_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "well_flow_rate", &result))
		make_access_error("SAM_Geothermal", "well_flow_rate");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_GeoHourly_wet_bulb_temp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "wet_bulb_temp", &result))
		make_access_error("SAM_Geothermal", "wet_bulb_temp");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_Outputs_GF_flowrate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "GF_flowrate", &result))
		make_access_error("SAM_Geothermal", "GF_flowrate");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_Outputs_annual_energy_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_energy", &result))
		make_access_error("SAM_Geothermal", "annual_energy");
	});
	return result;
}



SAM_EXPORT double* SAM_Geothermal_Outputs_annual_energy_distribution_time_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "annual_energy_distribution_time", nrows, ncols);
	if (!result)
		make_access_error("SAM_Geothermal", "annual_energy_distribution_time");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_Outputs_bottom_hole_pressure_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "bottom_hole_pressure", &result))
		make_access_error("SAM_Geothermal", "bottom_hole_pressure");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_Outputs_capacity_factor_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "capacity_factor", &result))
		make_access_error("SAM_Geothermal", "capacity_factor");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_Outputs_condensate_pump_power_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "condensate_pump_power", &result))
		make_access_error("SAM_Geothermal", "condensate_pump_power");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_Outputs_cw_pump_head_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cw_pump_head", &result))
		make_access_error("SAM_Geothermal", "cw_pump_head");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_Outputs_cw_pump_work_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cw_pump_work", &result))
		make_access_error("SAM_Geothermal", "cw_pump_work");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_Outputs_cwflow_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cwflow", &result))
		make_access_error("SAM_Geothermal", "cwflow");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_Outputs_eff_secondlaw_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eff_secondlaw", &result))
		make_access_error("SAM_Geothermal", "eff_secondlaw");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_Outputs_first_year_output_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "first_year_output", &result))
		make_access_error("SAM_Geothermal", "first_year_output");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_Outputs_flash_count_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "flash_count", &result))
		make_access_error("SAM_Geothermal", "flash_count");
	});
	return result;
}



SAM_EXPORT double* SAM_Geothermal_Outputs_gen_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "gen", length);
	if (!result)
		make_access_error("SAM_Geothermal", "gen");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_Outputs_gross_cost_output_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "gross_cost_output", &result))
		make_access_error("SAM_Geothermal", "gross_cost_output");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_Outputs_gross_output_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "gross_output", &result))
		make_access_error("SAM_Geothermal", "gross_output");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_Outputs_hp_flash_pressure_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hp_flash_pressure", &result))
		make_access_error("SAM_Geothermal", "hp_flash_pressure");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_Outputs_inj_pump_hp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inj_pump_hp", &result))
		make_access_error("SAM_Geothermal", "inj_pump_hp");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_Outputs_kwh_per_kw_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "kwh_per_kw", &result))
		make_access_error("SAM_Geothermal", "kwh_per_kw");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_Outputs_lifetime_output_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "lifetime_output", &result))
		make_access_error("SAM_Geothermal", "lifetime_output");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_Outputs_lp_flash_pressure_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "lp_flash_pressure", &result))
		make_access_error("SAM_Geothermal", "lp_flash_pressure");
	});
	return result;
}



SAM_EXPORT double* SAM_Geothermal_Outputs_monthly_energy_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_energy", length);
	if (!result)
		make_access_error("SAM_Geothermal", "monthly_energy");
	});
	return result;
}



SAM_EXPORT double* SAM_Geothermal_Outputs_monthly_power_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_power", length);
	if (!result)
		make_access_error("SAM_Geothermal", "monthly_power");
	});
	return result;
}



SAM_EXPORT double* SAM_Geothermal_Outputs_monthly_resource_temperature_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_resource_temperature", length);
	if (!result)
		make_access_error("SAM_Geothermal", "monthly_resource_temperature");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_Outputs_ncg_condensate_pump_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ncg_condensate_pump", &result))
		make_access_error("SAM_Geothermal", "ncg_condensate_pump");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_Outputs_num_wells_getem_inj_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "num_wells_getem_inj", &result))
		make_access_error("SAM_Geothermal", "num_wells_getem_inj");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_Outputs_num_wells_getem_output_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "num_wells_getem_output", &result))
		make_access_error("SAM_Geothermal", "num_wells_getem_output");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_Outputs_plant_brine_eff_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "plant_brine_eff", &result))
		make_access_error("SAM_Geothermal", "plant_brine_eff");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_Outputs_pressure_ratio_1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pressure_ratio_1", &result))
		make_access_error("SAM_Geothermal", "pressure_ratio_1");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_Outputs_pressure_ratio_2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pressure_ratio_2", &result))
		make_access_error("SAM_Geothermal", "pressure_ratio_2");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_Outputs_pressure_ratio_3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pressure_ratio_3", &result))
		make_access_error("SAM_Geothermal", "pressure_ratio_3");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_Outputs_pump_depth_ft_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pump_depth_ft", &result))
		make_access_error("SAM_Geothermal", "pump_depth_ft");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_Outputs_pump_hp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pump_hp", &result))
		make_access_error("SAM_Geothermal", "pump_hp");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_Outputs_pump_watthr_per_lb_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pump_watthr_per_lb", &result))
		make_access_error("SAM_Geothermal", "pump_watthr_per_lb");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_Outputs_pump_work_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pump_work", &result))
		make_access_error("SAM_Geothermal", "pump_work");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_Outputs_pumpwork_inj_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pumpwork_inj", &result))
		make_access_error("SAM_Geothermal", "pumpwork_inj");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_Outputs_pumpwork_prod_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pumpwork_prod", &result))
		make_access_error("SAM_Geothermal", "pumpwork_prod");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_Outputs_qCondenser_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "qCondenser", &result))
		make_access_error("SAM_Geothermal", "qCondenser");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_Outputs_qRejectByStage_1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "qRejectByStage_1", &result))
		make_access_error("SAM_Geothermal", "qRejectByStage_1");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_Outputs_qRejectByStage_2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "qRejectByStage_2", &result))
		make_access_error("SAM_Geothermal", "qRejectByStage_2");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_Outputs_qRejectByStage_3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "qRejectByStage_3", &result))
		make_access_error("SAM_Geothermal", "qRejectByStage_3");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_Outputs_qRejectTotal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "qRejectTotal", &result))
		make_access_error("SAM_Geothermal", "qRejectTotal");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_Outputs_reservoir_avg_temp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "reservoir_avg_temp", &result))
		make_access_error("SAM_Geothermal", "reservoir_avg_temp");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_Outputs_reservoir_pressure_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "reservoir_pressure", &result))
		make_access_error("SAM_Geothermal", "reservoir_pressure");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_Outputs_spec_vol_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "spec_vol", &result))
		make_access_error("SAM_Geothermal", "spec_vol");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_Outputs_spec_vol_lp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "spec_vol_lp", &result))
		make_access_error("SAM_Geothermal", "spec_vol_lp");
	});
	return result;
}



SAM_EXPORT double* SAM_Geothermal_Outputs_system_lifetime_recapitalize_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "system_lifetime_recapitalize", length);
	if (!result)
		make_access_error("SAM_Geothermal", "system_lifetime_recapitalize");
	});
	return result;
}



SAM_EXPORT double* SAM_Geothermal_Outputs_timestep_dry_bulb_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "timestep_dry_bulb", length);
	if (!result)
		make_access_error("SAM_Geothermal", "timestep_dry_bulb");
	});
	return result;
}



SAM_EXPORT double* SAM_Geothermal_Outputs_timestep_pressure_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "timestep_pressure", length);
	if (!result)
		make_access_error("SAM_Geothermal", "timestep_pressure");
	});
	return result;
}



SAM_EXPORT double* SAM_Geothermal_Outputs_timestep_resource_temperature_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "timestep_resource_temperature", length);
	if (!result)
		make_access_error("SAM_Geothermal", "timestep_resource_temperature");
	});
	return result;
}



SAM_EXPORT double* SAM_Geothermal_Outputs_timestep_test_values_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "timestep_test_values", length);
	if (!result)
		make_access_error("SAM_Geothermal", "timestep_test_values");
	});
	return result;
}



SAM_EXPORT double* SAM_Geothermal_Outputs_timestep_wet_bulb_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "timestep_wet_bulb", length);
	if (!result)
		make_access_error("SAM_Geothermal", "timestep_wet_bulb");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_Outputs_v_stage_1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "v_stage_1", &result))
		make_access_error("SAM_Geothermal", "v_stage_1");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_Outputs_v_stage_2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "v_stage_2", &result))
		make_access_error("SAM_Geothermal", "v_stage_2");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_Outputs_v_stage_3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "v_stage_3", &result))
		make_access_error("SAM_Geothermal", "v_stage_3");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_Outputs_x_hp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "x_hp", &result))
		make_access_error("SAM_Geothermal", "x_hp");
	});
	return result;
}



SAM_EXPORT double SAM_Geothermal_Outputs_x_lp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "x_lp", &result))
		make_access_error("SAM_Geothermal", "x_lp");
	});
	return result;
}



