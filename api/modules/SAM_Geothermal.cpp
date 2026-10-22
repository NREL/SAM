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

SAM_EXPORT void SAM_Geothermal_SystemControl_sim_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sim_type", number);
	});
}

SAM_EXPORT void SAM_Geothermal_FinancialModel_geo_financial_model_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "geo_financial_model", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_CT_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "CT", number);
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

SAM_EXPORT void SAM_Geothermal_GeoHourly_allow_reservoir_replacements_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "allow_reservoir_replacements", number);
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

SAM_EXPORT void SAM_Geothermal_GeoHourly_calc_drill_costs_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "calc_drill_costs", number);
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

SAM_EXPORT void SAM_Geothermal_GeoHourly_drilling_success_rate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "drilling_success_rate", number);
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

SAM_EXPORT void SAM_Geothermal_GeoHourly_failed_prod_flow_ratio_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "failed_prod_flow_ratio", number);
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

SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_conf_multiplier_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "geotherm.cost.conf_multiplier", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_conf_non_drill_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "geotherm.cost.conf_non_drill", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_conf_num_wells_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "geotherm.cost.conf_num_wells", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_confirm_wells_percent_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "geotherm.cost.confirm_wells_percent", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_contingency_percent_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "geotherm.cost.contingency_percent", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_drilling_amount_specified_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "geotherm.cost.drilling.amount_specified", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_drilling_calc_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "geotherm.cost.drilling.calc", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_epc_fixed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "geotherm.cost.epc.fixed", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_epc_percent_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "geotherm.cost.epc.percent", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_expl_lump_sum_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "geotherm.cost.expl_lump_sum", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_expl_multiplier_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "geotherm.cost.expl_multiplier", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_expl_non_drill_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "geotherm.cost.expl_non_drill", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_expl_num_wells_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "geotherm.cost.expl_num_wells", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_indirect_amount_specified_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "geotherm.cost.indirect.amount_specified", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_indirect_calc_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "geotherm.cost.indirect.calc", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_inj_cost_curve_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "geotherm.cost.inj_cost_curve", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_inj_cost_curve_welldiam_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "geotherm.cost.inj_cost_curve_welldiam", number);
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

SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_plant_auto_estimate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "geotherm.cost.plant_auto_estimate", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_plant_per_kW_input_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "geotherm.cost.plant_per_kW_input", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_plant_total_calc_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "geotherm.cost.plant_total.calc", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_plm_fixed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "geotherm.cost.plm.fixed", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_plm_percent_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "geotherm.cost.plm.percent", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_prod_cost_curve_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "geotherm.cost.prod_cost_curve", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_prod_cost_curve_welldiam_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "geotherm.cost.prod_cost_curve_welldiam", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_prod_cost_curve_welltype_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "geotherm.cost.prod_cost_curve_welltype", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_prod_inj_non_drill_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "geotherm.cost.prod_inj_non_drill", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_pump_casing_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "geotherm.cost.pump_casing_cost", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_pump_fixed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "geotherm.cost.pump_fixed", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_pump_per_foot_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "geotherm.cost.pump_per_foot", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_pumping_amount_specified_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "geotherm.cost.pumping.amount_specified", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_pumping_calc_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "geotherm.cost.pumping.calc", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_recap_specified_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "geotherm.cost.recap_specified", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_recap_use_calc_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "geotherm.cost.recap_use_calc", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_sales_tax_percent_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "geotherm.cost.sales_tax.percent", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_geotherm_cost_stim_non_drill_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "geotherm.cost.stim_non_drill", number);
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

SAM_EXPORT void SAM_Geothermal_GeoHourly_inj_prod_well_distance_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inj_prod_well_distance", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_injectivity_index_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "injectivity_index", number);
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

SAM_EXPORT void SAM_Geothermal_GeoHourly_ppi_base_year_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ppi_base_year", number);
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

SAM_EXPORT void SAM_Geothermal_GeoHourly_sales_tax_rate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sales_tax_rate", number);
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

SAM_EXPORT void SAM_Geothermal_GeoHourly_start_day_of_year_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "start_day_of_year", number);
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

SAM_EXPORT void SAM_Geothermal_GeoHourly_stim_success_rate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "stim_success_rate", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_stimulation_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "stimulation_type", number);
	});
}

SAM_EXPORT void SAM_Geothermal_GeoHourly_subsurface_water_loss_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subsurface_water_loss", number);
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

SAM_EXPORT void SAM_Geothermal_GeoHourly_use_weather_file_conditions_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "use_weather_file_conditions", number);
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

SAM_EXPORT void SAM_Geothermal_AdjustmentFactors_adjust_constant_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "adjust_constant", number);
	});
}

SAM_EXPORT void SAM_Geothermal_AdjustmentFactors_adjust_en_periods_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "adjust_en_periods", number);
	});
}

SAM_EXPORT void SAM_Geothermal_AdjustmentFactors_adjust_en_timeindex_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "adjust_en_timeindex", number);
	});
}

SAM_EXPORT void SAM_Geothermal_AdjustmentFactors_adjust_periods_mset(SAM_table ptr, double* mat, int nrows, int ncols, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_matrix(ptr, "adjust_periods", mat, nrows, ncols);
	});
}

SAM_EXPORT void SAM_Geothermal_AdjustmentFactors_adjust_timeindex_aset(SAM_table ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "adjust_timeindex", arr, length);
	});
}

SAM_EXPORT void SAM_Geothermal_FinancialParameters_analysis_period_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "analysis_period", number);
	});
}

SAM_EXPORT void SAM_Geothermal_Costs_geotherm_cost_plant_total_amount_specified_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "geotherm.cost.plant_total.amount_specified", number);
	});
}

SAM_EXPORT void SAM_Geothermal_FinancialParameters_const_per_interest_rate1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_interest_rate1", number);
	});
}

SAM_EXPORT void SAM_Geothermal_FinancialParameters_const_per_interest_rate2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_interest_rate2", number);
	});
}

SAM_EXPORT void SAM_Geothermal_FinancialParameters_const_per_interest_rate3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_interest_rate3", number);
	});
}

SAM_EXPORT void SAM_Geothermal_FinancialParameters_const_per_interest_rate4_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_interest_rate4", number);
	});
}

SAM_EXPORT void SAM_Geothermal_FinancialParameters_const_per_interest_rate5_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_interest_rate5", number);
	});
}

SAM_EXPORT void SAM_Geothermal_FinancialParameters_const_per_months1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_months1", number);
	});
}

SAM_EXPORT void SAM_Geothermal_FinancialParameters_const_per_months2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_months2", number);
	});
}

SAM_EXPORT void SAM_Geothermal_FinancialParameters_const_per_months3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_months3", number);
	});
}

SAM_EXPORT void SAM_Geothermal_FinancialParameters_const_per_months4_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_months4", number);
	});
}

SAM_EXPORT void SAM_Geothermal_FinancialParameters_const_per_months5_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_months5", number);
	});
}

SAM_EXPORT void SAM_Geothermal_FinancialParameters_const_per_percent1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_percent1", number);
	});
}

SAM_EXPORT void SAM_Geothermal_FinancialParameters_const_per_percent2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_percent2", number);
	});
}

SAM_EXPORT void SAM_Geothermal_FinancialParameters_const_per_percent3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_percent3", number);
	});
}

SAM_EXPORT void SAM_Geothermal_FinancialParameters_const_per_percent4_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_percent4", number);
	});
}

SAM_EXPORT void SAM_Geothermal_FinancialParameters_const_per_percent5_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_percent5", number);
	});
}

SAM_EXPORT void SAM_Geothermal_FinancialParameters_const_per_upfront_rate1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_upfront_rate1", number);
	});
}

SAM_EXPORT void SAM_Geothermal_FinancialParameters_const_per_upfront_rate2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_upfront_rate2", number);
	});
}

SAM_EXPORT void SAM_Geothermal_FinancialParameters_const_per_upfront_rate3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_upfront_rate3", number);
	});
}

SAM_EXPORT void SAM_Geothermal_FinancialParameters_const_per_upfront_rate4_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_upfront_rate4", number);
	});
}

SAM_EXPORT void SAM_Geothermal_FinancialParameters_const_per_upfront_rate5_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "const_per_upfront_rate5", number);
	});
}

SAM_EXPORT double SAM_Geothermal_SystemControl_sim_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sim_type", &result))
		make_access_error("SAM_Geothermal", "sim_type");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_FinancialModel_geo_financial_model_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "geo_financial_model", &result))
		make_access_error("SAM_Geothermal", "geo_financial_model");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_GeoHourly_CT_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "CT", &result))
		make_access_error("SAM_Geothermal", "CT");
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

SAM_EXPORT double SAM_Geothermal_GeoHourly_allow_reservoir_replacements_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "allow_reservoir_replacements", &result))
		make_access_error("SAM_Geothermal", "allow_reservoir_replacements");
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

SAM_EXPORT double SAM_Geothermal_GeoHourly_calc_drill_costs_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "calc_drill_costs", &result))
		make_access_error("SAM_Geothermal", "calc_drill_costs");
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

SAM_EXPORT double SAM_Geothermal_GeoHourly_drilling_success_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "drilling_success_rate", &result))
		make_access_error("SAM_Geothermal", "drilling_success_rate");
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

SAM_EXPORT double SAM_Geothermal_GeoHourly_failed_prod_flow_ratio_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "failed_prod_flow_ratio", &result))
		make_access_error("SAM_Geothermal", "failed_prod_flow_ratio");
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

SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_conf_multiplier_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "geotherm.cost.conf_multiplier", &result))
		make_access_error("SAM_Geothermal", "geotherm.cost.conf_multiplier");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_conf_non_drill_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "geotherm.cost.conf_non_drill", &result))
		make_access_error("SAM_Geothermal", "geotherm.cost.conf_non_drill");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_conf_num_wells_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "geotherm.cost.conf_num_wells", &result))
		make_access_error("SAM_Geothermal", "geotherm.cost.conf_num_wells");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_confirm_wells_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "geotherm.cost.confirm_wells_percent", &result))
		make_access_error("SAM_Geothermal", "geotherm.cost.confirm_wells_percent");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_contingency_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "geotherm.cost.contingency_percent", &result))
		make_access_error("SAM_Geothermal", "geotherm.cost.contingency_percent");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_drilling_amount_specified_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "geotherm.cost.drilling.amount_specified", &result))
		make_access_error("SAM_Geothermal", "geotherm.cost.drilling.amount_specified");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_drilling_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "geotherm.cost.drilling.calc", &result))
		make_access_error("SAM_Geothermal", "geotherm.cost.drilling.calc");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_epc_fixed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "geotherm.cost.epc.fixed", &result))
		make_access_error("SAM_Geothermal", "geotherm.cost.epc.fixed");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_epc_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "geotherm.cost.epc.percent", &result))
		make_access_error("SAM_Geothermal", "geotherm.cost.epc.percent");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_expl_lump_sum_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "geotherm.cost.expl_lump_sum", &result))
		make_access_error("SAM_Geothermal", "geotherm.cost.expl_lump_sum");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_expl_multiplier_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "geotherm.cost.expl_multiplier", &result))
		make_access_error("SAM_Geothermal", "geotherm.cost.expl_multiplier");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_expl_non_drill_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "geotherm.cost.expl_non_drill", &result))
		make_access_error("SAM_Geothermal", "geotherm.cost.expl_non_drill");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_expl_num_wells_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "geotherm.cost.expl_num_wells", &result))
		make_access_error("SAM_Geothermal", "geotherm.cost.expl_num_wells");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_indirect_amount_specified_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "geotherm.cost.indirect.amount_specified", &result))
		make_access_error("SAM_Geothermal", "geotherm.cost.indirect.amount_specified");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_indirect_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "geotherm.cost.indirect.calc", &result))
		make_access_error("SAM_Geothermal", "geotherm.cost.indirect.calc");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_inj_cost_curve_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "geotherm.cost.inj_cost_curve", &result))
		make_access_error("SAM_Geothermal", "geotherm.cost.inj_cost_curve");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_inj_cost_curve_welldiam_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "geotherm.cost.inj_cost_curve_welldiam", &result))
		make_access_error("SAM_Geothermal", "geotherm.cost.inj_cost_curve_welldiam");
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

SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_plant_auto_estimate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "geotherm.cost.plant_auto_estimate", &result))
		make_access_error("SAM_Geothermal", "geotherm.cost.plant_auto_estimate");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_plant_per_kW_input_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "geotherm.cost.plant_per_kW_input", &result))
		make_access_error("SAM_Geothermal", "geotherm.cost.plant_per_kW_input");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_plant_total_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "geotherm.cost.plant_total.calc", &result))
		make_access_error("SAM_Geothermal", "geotherm.cost.plant_total.calc");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_plm_fixed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "geotherm.cost.plm.fixed", &result))
		make_access_error("SAM_Geothermal", "geotherm.cost.plm.fixed");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_plm_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "geotherm.cost.plm.percent", &result))
		make_access_error("SAM_Geothermal", "geotherm.cost.plm.percent");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_prod_cost_curve_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "geotherm.cost.prod_cost_curve", &result))
		make_access_error("SAM_Geothermal", "geotherm.cost.prod_cost_curve");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_prod_cost_curve_welldiam_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "geotherm.cost.prod_cost_curve_welldiam", &result))
		make_access_error("SAM_Geothermal", "geotherm.cost.prod_cost_curve_welldiam");
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

SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_prod_inj_non_drill_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "geotherm.cost.prod_inj_non_drill", &result))
		make_access_error("SAM_Geothermal", "geotherm.cost.prod_inj_non_drill");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_pump_casing_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "geotherm.cost.pump_casing_cost", &result))
		make_access_error("SAM_Geothermal", "geotherm.cost.pump_casing_cost");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_pump_fixed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "geotherm.cost.pump_fixed", &result))
		make_access_error("SAM_Geothermal", "geotherm.cost.pump_fixed");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_pump_per_foot_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "geotherm.cost.pump_per_foot", &result))
		make_access_error("SAM_Geothermal", "geotherm.cost.pump_per_foot");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_pumping_amount_specified_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "geotherm.cost.pumping.amount_specified", &result))
		make_access_error("SAM_Geothermal", "geotherm.cost.pumping.amount_specified");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_pumping_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "geotherm.cost.pumping.calc", &result))
		make_access_error("SAM_Geothermal", "geotherm.cost.pumping.calc");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_recap_specified_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "geotherm.cost.recap_specified", &result))
		make_access_error("SAM_Geothermal", "geotherm.cost.recap_specified");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_recap_use_calc_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "geotherm.cost.recap_use_calc", &result))
		make_access_error("SAM_Geothermal", "geotherm.cost.recap_use_calc");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_sales_tax_percent_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "geotherm.cost.sales_tax.percent", &result))
		make_access_error("SAM_Geothermal", "geotherm.cost.sales_tax.percent");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_GeoHourly_geotherm_cost_stim_non_drill_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "geotherm.cost.stim_non_drill", &result))
		make_access_error("SAM_Geothermal", "geotherm.cost.stim_non_drill");
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

SAM_EXPORT double SAM_Geothermal_GeoHourly_inj_prod_well_distance_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inj_prod_well_distance", &result))
		make_access_error("SAM_Geothermal", "inj_prod_well_distance");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_GeoHourly_injectivity_index_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "injectivity_index", &result))
		make_access_error("SAM_Geothermal", "injectivity_index");
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

SAM_EXPORT double SAM_Geothermal_GeoHourly_ppi_base_year_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ppi_base_year", &result))
		make_access_error("SAM_Geothermal", "ppi_base_year");
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

SAM_EXPORT double SAM_Geothermal_GeoHourly_sales_tax_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sales_tax_rate", &result))
		make_access_error("SAM_Geothermal", "sales_tax_rate");
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

SAM_EXPORT double SAM_Geothermal_GeoHourly_start_day_of_year_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "start_day_of_year", &result))
		make_access_error("SAM_Geothermal", "start_day_of_year");
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

SAM_EXPORT double SAM_Geothermal_GeoHourly_stim_success_rate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "stim_success_rate", &result))
		make_access_error("SAM_Geothermal", "stim_success_rate");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_GeoHourly_stimulation_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "stimulation_type", &result))
		make_access_error("SAM_Geothermal", "stimulation_type");
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

SAM_EXPORT double SAM_Geothermal_GeoHourly_use_weather_file_conditions_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "use_weather_file_conditions", &result))
		make_access_error("SAM_Geothermal", "use_weather_file_conditions");
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

SAM_EXPORT double SAM_Geothermal_AdjustmentFactors_adjust_constant_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "adjust_constant", &result))
		make_access_error("SAM_Geothermal", "adjust_constant");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_AdjustmentFactors_adjust_en_periods_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "adjust_en_periods", &result))
		make_access_error("SAM_Geothermal", "adjust_en_periods");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_AdjustmentFactors_adjust_en_timeindex_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "adjust_en_timeindex", &result))
		make_access_error("SAM_Geothermal", "adjust_en_timeindex");
	});
	return result;
}

SAM_EXPORT double* SAM_Geothermal_AdjustmentFactors_adjust_periods_mget(SAM_table ptr, int* nrows, int* ncols, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_matrix(ptr, "adjust_periods", nrows, ncols);
	if (!result)
		make_access_error("SAM_Geothermal", "adjust_periods");
	});
	return result;
}

SAM_EXPORT double* SAM_Geothermal_AdjustmentFactors_adjust_timeindex_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "adjust_timeindex", length);
	if (!result)
		make_access_error("SAM_Geothermal", "adjust_timeindex");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_FinancialParameters_analysis_period_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "analysis_period", &result))
		make_access_error("SAM_Geothermal", "analysis_period");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Costs_geotherm_cost_plant_total_amount_specified_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "geotherm.cost.plant_total.amount_specified", &result))
		make_access_error("SAM_Geothermal", "geotherm.cost.plant_total.amount_specified");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_FinancialParameters_const_per_interest_rate1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest_rate1", &result))
		make_access_error("SAM_Geothermal", "const_per_interest_rate1");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_FinancialParameters_const_per_interest_rate2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest_rate2", &result))
		make_access_error("SAM_Geothermal", "const_per_interest_rate2");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_FinancialParameters_const_per_interest_rate3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest_rate3", &result))
		make_access_error("SAM_Geothermal", "const_per_interest_rate3");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_FinancialParameters_const_per_interest_rate4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest_rate4", &result))
		make_access_error("SAM_Geothermal", "const_per_interest_rate4");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_FinancialParameters_const_per_interest_rate5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest_rate5", &result))
		make_access_error("SAM_Geothermal", "const_per_interest_rate5");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_FinancialParameters_const_per_months1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_months1", &result))
		make_access_error("SAM_Geothermal", "const_per_months1");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_FinancialParameters_const_per_months2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_months2", &result))
		make_access_error("SAM_Geothermal", "const_per_months2");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_FinancialParameters_const_per_months3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_months3", &result))
		make_access_error("SAM_Geothermal", "const_per_months3");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_FinancialParameters_const_per_months4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_months4", &result))
		make_access_error("SAM_Geothermal", "const_per_months4");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_FinancialParameters_const_per_months5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_months5", &result))
		make_access_error("SAM_Geothermal", "const_per_months5");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_FinancialParameters_const_per_percent1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_percent1", &result))
		make_access_error("SAM_Geothermal", "const_per_percent1");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_FinancialParameters_const_per_percent2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_percent2", &result))
		make_access_error("SAM_Geothermal", "const_per_percent2");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_FinancialParameters_const_per_percent3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_percent3", &result))
		make_access_error("SAM_Geothermal", "const_per_percent3");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_FinancialParameters_const_per_percent4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_percent4", &result))
		make_access_error("SAM_Geothermal", "const_per_percent4");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_FinancialParameters_const_per_percent5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_percent5", &result))
		make_access_error("SAM_Geothermal", "const_per_percent5");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_FinancialParameters_const_per_upfront_rate1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_upfront_rate1", &result))
		make_access_error("SAM_Geothermal", "const_per_upfront_rate1");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_FinancialParameters_const_per_upfront_rate2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_upfront_rate2", &result))
		make_access_error("SAM_Geothermal", "const_per_upfront_rate2");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_FinancialParameters_const_per_upfront_rate3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_upfront_rate3", &result))
		make_access_error("SAM_Geothermal", "const_per_upfront_rate3");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_FinancialParameters_const_per_upfront_rate4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_upfront_rate4", &result))
		make_access_error("SAM_Geothermal", "const_per_upfront_rate4");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_FinancialParameters_const_per_upfront_rate5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_upfront_rate5", &result))
		make_access_error("SAM_Geothermal", "const_per_upfront_rate5");
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

SAM_EXPORT double SAM_Geothermal_Outputs_atb_drilling_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "atb_drilling_cost", &result))
		make_access_error("SAM_Geothermal", "atb_drilling_cost");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_atb_exploration_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "atb_exploration_cost", &result))
		make_access_error("SAM_Geothermal", "atb_exploration_cost");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_atb_plant_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "atb_plant_cost", &result))
		make_access_error("SAM_Geothermal", "atb_plant_cost");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_baseline_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "baseline_cost", &result))
		make_access_error("SAM_Geothermal", "baseline_cost");
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

SAM_EXPORT double SAM_Geothermal_Outputs_brine_effectiveness_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "brine_effectiveness", &result))
		make_access_error("SAM_Geothermal", "brine_effectiveness");
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

SAM_EXPORT double SAM_Geothermal_Outputs_conf_drilling_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "conf_drilling_cost", &result))
		make_access_error("SAM_Geothermal", "conf_drilling_cost");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_conf_total_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "conf_total_cost", &result))
		make_access_error("SAM_Geothermal", "conf_total_cost");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_const_per_interest1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest1", &result))
		make_access_error("SAM_Geothermal", "const_per_interest1");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_const_per_interest2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest2", &result))
		make_access_error("SAM_Geothermal", "const_per_interest2");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_const_per_interest3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest3", &result))
		make_access_error("SAM_Geothermal", "const_per_interest3");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_const_per_interest4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest4", &result))
		make_access_error("SAM_Geothermal", "const_per_interest4");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_const_per_interest5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest5", &result))
		make_access_error("SAM_Geothermal", "const_per_interest5");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_const_per_interest_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_interest_total", &result))
		make_access_error("SAM_Geothermal", "const_per_interest_total");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_const_per_percent_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_percent_total", &result))
		make_access_error("SAM_Geothermal", "const_per_percent_total");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_const_per_principal1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_principal1", &result))
		make_access_error("SAM_Geothermal", "const_per_principal1");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_const_per_principal2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_principal2", &result))
		make_access_error("SAM_Geothermal", "const_per_principal2");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_const_per_principal3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_principal3", &result))
		make_access_error("SAM_Geothermal", "const_per_principal3");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_const_per_principal4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_principal4", &result))
		make_access_error("SAM_Geothermal", "const_per_principal4");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_const_per_principal5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_principal5", &result))
		make_access_error("SAM_Geothermal", "const_per_principal5");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_const_per_principal_total_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_principal_total", &result))
		make_access_error("SAM_Geothermal", "const_per_principal_total");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_const_per_total1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_total1", &result))
		make_access_error("SAM_Geothermal", "const_per_total1");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_const_per_total2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_total2", &result))
		make_access_error("SAM_Geothermal", "const_per_total2");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_const_per_total3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_total3", &result))
		make_access_error("SAM_Geothermal", "const_per_total3");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_const_per_total4_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_total4", &result))
		make_access_error("SAM_Geothermal", "const_per_total4");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_const_per_total5_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "const_per_total5", &result))
		make_access_error("SAM_Geothermal", "const_per_total5");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_construction_financing_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "construction_financing_cost", &result))
		make_access_error("SAM_Geothermal", "construction_financing_cost");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_contingency_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "contingency_cost", &result))
		make_access_error("SAM_Geothermal", "contingency_cost");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_cp_battery_nameplate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cp_battery_nameplate", &result))
		make_access_error("SAM_Geothermal", "cp_battery_nameplate");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_cp_system_nameplate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cp_system_nameplate", &result))
		make_access_error("SAM_Geothermal", "cp_system_nameplate");
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

SAM_EXPORT double* SAM_Geothermal_Outputs_degradation_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "degradation", length);
	if (!result)
		make_access_error("SAM_Geothermal", "degradation");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_design_temp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "design_temp", &result))
		make_access_error("SAM_Geothermal", "design_temp");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_dt_rock_well_head_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dt_rock_well_head", &result))
		make_access_error("SAM_Geothermal", "dt_rock_well_head");
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

SAM_EXPORT double SAM_Geothermal_Outputs_engineering_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "engineering_cost", &result))
		make_access_error("SAM_Geothermal", "engineering_cost");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_epc_total_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "epc_total_cost", &result))
		make_access_error("SAM_Geothermal", "epc_total_cost");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_expl_drilling_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "expl_drilling_cost", &result))
		make_access_error("SAM_Geothermal", "expl_drilling_cost");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_expl_per_well_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "expl_per_well_cost", &result))
		make_access_error("SAM_Geothermal", "expl_per_well_cost");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_expl_total_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "expl_total_cost", &result))
		make_access_error("SAM_Geothermal", "expl_total_cost");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_field_gathering_num_wells_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "field_gathering_num_wells", &result))
		make_access_error("SAM_Geothermal", "field_gathering_num_wells");
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

SAM_EXPORT double SAM_Geothermal_Outputs_geothermal_analysis_period_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "geothermal_analysis_period", &result))
		make_access_error("SAM_Geothermal", "geothermal_analysis_period");
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

SAM_EXPORT double SAM_Geothermal_Outputs_indirect_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "indirect_cost", &result))
		make_access_error("SAM_Geothermal", "indirect_cost");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_indirect_pump_gathering_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "indirect_pump_gathering_cost", &result))
		make_access_error("SAM_Geothermal", "indirect_pump_gathering_cost");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_inj_num_pumps_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inj_num_pumps", &result))
		make_access_error("SAM_Geothermal", "inj_num_pumps");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_inj_pump_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inj_pump_cost", &result))
		make_access_error("SAM_Geothermal", "inj_pump_cost");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_inj_pump_cost_per_pump_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inj_pump_cost_per_pump", &result))
		make_access_error("SAM_Geothermal", "inj_pump_cost_per_pump");
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

SAM_EXPORT double SAM_Geothermal_Outputs_inj_total_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inj_total_cost", &result))
		make_access_error("SAM_Geothermal", "inj_total_cost");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_inj_well_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inj_well_cost", &result))
		make_access_error("SAM_Geothermal", "inj_well_cost");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_installed_cost_per_kW_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "installed_cost_per_kW", &result))
		make_access_error("SAM_Geothermal", "installed_cost_per_kW");
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

SAM_EXPORT double SAM_Geothermal_Outputs_max_brine_effectiveness_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "max_brine_effectiveness", &result))
		make_access_error("SAM_Geothermal", "max_brine_effectiveness");
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

SAM_EXPORT double* SAM_Geothermal_Outputs_monthly_energy_lifetime_aget(SAM_table ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_energy_lifetime", length);
	if (!result)
		make_access_error("SAM_Geothermal", "monthly_energy_lifetime");
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

SAM_EXPORT double SAM_Geothermal_Outputs_net_plant_output_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "net_plant_output", &result))
		make_access_error("SAM_Geothermal", "net_plant_output");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_num_confirm_wells_to_production_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "num_confirm_wells_to_production", &result))
		make_access_error("SAM_Geothermal", "num_confirm_wells_to_production");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_num_wells_getem_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "num_wells_getem", &result))
		make_access_error("SAM_Geothermal", "num_wells_getem");
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

SAM_EXPORT double SAM_Geothermal_Outputs_num_wells_getem_inj_drilled_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "num_wells_getem_inj_drilled", &result))
		make_access_error("SAM_Geothermal", "num_wells_getem_inj_drilled");
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

SAM_EXPORT double SAM_Geothermal_Outputs_num_wells_getem_prod_drilled_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "num_wells_getem_prod_drilled", &result))
		make_access_error("SAM_Geothermal", "num_wells_getem_prod_drilled");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_num_wells_getem_prod_failed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "num_wells_getem_prod_failed", &result))
		make_access_error("SAM_Geothermal", "num_wells_getem_prod_failed");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_piping_cost_per_well_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "piping_cost_per_well", &result))
		make_access_error("SAM_Geothermal", "piping_cost_per_well");
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

SAM_EXPORT double SAM_Geothermal_Outputs_plm_total_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "plm_total_cost", &result))
		make_access_error("SAM_Geothermal", "plm_total_cost");
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

SAM_EXPORT double SAM_Geothermal_Outputs_prod_inj_total_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "prod_inj_total_cost", &result))
		make_access_error("SAM_Geothermal", "prod_inj_total_cost");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_prod_pump_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "prod_pump_cost", &result))
		make_access_error("SAM_Geothermal", "prod_pump_cost");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_prod_pump_cost_per_well_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "prod_pump_cost_per_well", &result))
		make_access_error("SAM_Geothermal", "prod_pump_cost_per_well");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_prod_total_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "prod_total_cost", &result))
		make_access_error("SAM_Geothermal", "prod_total_cost");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_prod_well_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "prod_well_cost", &result))
		make_access_error("SAM_Geothermal", "prod_well_cost");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_pump_cost_install_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pump_cost_install", &result))
		make_access_error("SAM_Geothermal", "pump_cost_install");
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

SAM_EXPORT double SAM_Geothermal_Outputs_pump_only_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pump_only_cost", &result))
		make_access_error("SAM_Geothermal", "pump_only_cost");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_pump_size_hp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pump_size_hp", &result))
		make_access_error("SAM_Geothermal", "pump_size_hp");
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

SAM_EXPORT double SAM_Geothermal_Outputs_sales_tax_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sales_tax_cost", &result))
		make_access_error("SAM_Geothermal", "sales_tax_cost");
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

SAM_EXPORT double SAM_Geothermal_Outputs_stim_cost_non_drill_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "stim_cost_non_drill", &result))
		make_access_error("SAM_Geothermal", "stim_cost_non_drill");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_stim_cost_per_well_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "stim_cost_per_well", &result))
		make_access_error("SAM_Geothermal", "stim_cost_per_well");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_stim_total_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "stim_total_cost", &result))
		make_access_error("SAM_Geothermal", "stim_total_cost");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_sum_prod_inj_total_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sum_prod_inj_total_cost", &result))
		make_access_error("SAM_Geothermal", "sum_prod_inj_total_cost");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_system_capacity_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_capacity", &result))
		make_access_error("SAM_Geothermal", "system_capacity");
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

SAM_EXPORT double SAM_Geothermal_Outputs_system_recapitalization_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_recapitalization_cost", &result))
		make_access_error("SAM_Geothermal", "system_recapitalization_cost");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_system_use_lifetime_output_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_use_lifetime_output", &result))
		make_access_error("SAM_Geothermal", "system_use_lifetime_output");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_system_use_recapitalization_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_use_recapitalization", &result))
		make_access_error("SAM_Geothermal", "system_use_recapitalization");
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

SAM_EXPORT double SAM_Geothermal_Outputs_total_capital_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_capital_cost", &result))
		make_access_error("SAM_Geothermal", "total_capital_cost");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_total_direct_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_direct_cost", &result))
		make_access_error("SAM_Geothermal", "total_direct_cost");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_total_drilling_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_drilling_cost", &result))
		make_access_error("SAM_Geothermal", "total_drilling_cost");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_total_drilling_cost_used_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_drilling_cost_used", &result))
		make_access_error("SAM_Geothermal", "total_drilling_cost_used");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_total_drilling_permitting_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_drilling_permitting", &result))
		make_access_error("SAM_Geothermal", "total_drilling_permitting");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_total_expl_permitting_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_expl_permitting", &result))
		make_access_error("SAM_Geothermal", "total_expl_permitting");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_total_gathering_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_gathering_cost", &result))
		make_access_error("SAM_Geothermal", "total_gathering_cost");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_total_getem_om_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_getem_om_cost", &result))
		make_access_error("SAM_Geothermal", "total_getem_om_cost");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_total_installed_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_installed_cost", &result))
		make_access_error("SAM_Geothermal", "total_installed_cost");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_total_plant_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_plant_cost", &result))
		make_access_error("SAM_Geothermal", "total_plant_cost");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_total_plant_cost_used_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_plant_cost_used", &result))
		make_access_error("SAM_Geothermal", "total_plant_cost_used");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_total_pump_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_pump_cost", &result))
		make_access_error("SAM_Geothermal", "total_pump_cost");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_total_pump_gathering_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_pump_gathering_cost", &result))
		make_access_error("SAM_Geothermal", "total_pump_gathering_cost");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_total_pump_gathering_cost_used_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_pump_gathering_cost_used", &result))
		make_access_error("SAM_Geothermal", "total_pump_gathering_cost_used");
	});
	return result;
}

SAM_EXPORT double SAM_Geothermal_Outputs_total_surface_equipment_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_surface_equipment_cost", &result))
		make_access_error("SAM_Geothermal", "total_surface_equipment_cost");
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

