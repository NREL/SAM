#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_GeothermalCosts.h"

SAM_EXPORT int SAM_GeothermalCosts_execute(SAM_table data, int verbosity, SAM_error* err){
	return SAM_module_exec("geothermal_costs", data, verbosity, err);
}

SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_GF_flowrate_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "GF_flowrate", number);
	});
}

SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_calc_drill_costs_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "calc_drill_costs", number);
	});
}

SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_condensate_pump_power_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "condensate_pump_power", number);
	});
}

SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_conversion_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "conversion_type", number);
	});
}

SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_cw_pump_head_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cw_pump_head", number);
	});
}

SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_cw_pump_work_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cw_pump_work", number);
	});
}

SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_cwflow_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cwflow", number);
	});
}

SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_design_temp_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "design_temp", number);
	});
}

SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_dt_prod_well_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dt_prod_well", number);
	});
}

SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_eff_secondlaw_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "eff_secondlaw", number);
	});
}

SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_flash_count_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "flash_count", number);
	});
}

SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_geotherm_cost_conf_multiplier_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "geotherm.cost.conf_multiplier", number);
	});
}

SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_geotherm_cost_conf_non_drill_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "geotherm.cost.conf_non_drill", number);
	});
}

SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_geotherm_cost_conf_num_wells_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "geotherm.cost.conf_num_wells", number);
	});
}

SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_geotherm_cost_expl_lump_sum_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "geotherm.cost.expl_lump_sum", number);
	});
}

SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_geotherm_cost_expl_multiplier_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "geotherm.cost.expl_multiplier", number);
	});
}

SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_geotherm_cost_expl_non_drill_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "geotherm.cost.expl_non_drill", number);
	});
}

SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_geotherm_cost_expl_num_wells_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "geotherm.cost.expl_num_wells", number);
	});
}

SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_geotherm_cost_inj_cost_curve_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "geotherm.cost.inj_cost_curve", number);
	});
}

SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_geotherm_cost_inj_cost_curve_welldiam_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "geotherm.cost.inj_cost_curve_welldiam", number);
	});
}

SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_geotherm_cost_inj_cost_curve_welltype_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "geotherm.cost.inj_cost_curve_welltype", number);
	});
}

SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_geotherm_cost_prod_cost_curve_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "geotherm.cost.prod_cost_curve", number);
	});
}

SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_geotherm_cost_prod_cost_curve_welldiam_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "geotherm.cost.prod_cost_curve_welldiam", number);
	});
}

SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_geotherm_cost_prod_cost_curve_welltype_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "geotherm.cost.prod_cost_curve_welltype", number);
	});
}

SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_geotherm_cost_pump_casing_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "geotherm.cost.pump_casing_cost", number);
	});
}

SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_geotherm_cost_pump_fixed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "geotherm.cost.pump_fixed", number);
	});
}

SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_geotherm_cost_pump_per_foot_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "geotherm.cost.pump_per_foot", number);
	});
}

SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_geotherm_cost_stim_non_drill_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "geotherm.cost.stim_non_drill", number);
	});
}

SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_gross_cost_output_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "gross_cost_output", number);
	});
}

SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_gross_output_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "gross_output", number);
	});
}

SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_hp_flash_pressure_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hp_flash_pressure", number);
	});
}

SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_inj_pump_hp_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inj_pump_hp", number);
	});
}

SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_lp_flash_pressure_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "lp_flash_pressure", number);
	});
}

SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_ncg_condensate_pump_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ncg_condensate_pump", number);
	});
}

SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_num_wells_getem_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "num_wells_getem", number);
	});
}

SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_num_wells_getem_inj_drilled_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "num_wells_getem_inj_drilled", number);
	});
}

SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_num_wells_getem_prod_drilled_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "num_wells_getem_prod_drilled", number);
	});
}

SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_num_wells_getem_prod_failed_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "num_wells_getem_prod_failed", number);
	});
}

SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_ppi_base_year_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ppi_base_year", number);
	});
}

SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_pressure_ratio_1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pressure_ratio_1", number);
	});
}

SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_pressure_ratio_2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pressure_ratio_2", number);
	});
}

SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_pressure_ratio_3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pressure_ratio_3", number);
	});
}

SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_pump_depth_ft_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pump_depth_ft", number);
	});
}

SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_pump_size_hp_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pump_size_hp", number);
	});
}

SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_qCondenser_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "qCondenser", number);
	});
}

SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_qRejectByStage_1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "qRejectByStage_1", number);
	});
}

SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_qRejectByStage_2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "qRejectByStage_2", number);
	});
}

SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_qRejectByStage_3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "qRejectByStage_3", number);
	});
}

SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_qRejectTotal_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "qRejectTotal", number);
	});
}

SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_resource_depth_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "resource_depth", number);
	});
}

SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_spec_vol_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "spec_vol", number);
	});
}

SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_spec_vol_lp_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "spec_vol_lp", number);
	});
}

SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_stimulation_type_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "stimulation_type", number);
	});
}

SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_v_stage_1_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "v_stage_1", number);
	});
}

SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_v_stage_2_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "v_stage_2", number);
	});
}

SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_v_stage_3_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "v_stage_3", number);
	});
}

SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_x_hp_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "x_hp", number);
	});
}

SAM_EXPORT void SAM_GeothermalCosts_GeoHourly_x_lp_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "x_lp", number);
	});
}

SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_GF_flowrate_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "GF_flowrate", &result))
		make_access_error("SAM_GeothermalCosts", "GF_flowrate");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_calc_drill_costs_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "calc_drill_costs", &result))
		make_access_error("SAM_GeothermalCosts", "calc_drill_costs");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_condensate_pump_power_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "condensate_pump_power", &result))
		make_access_error("SAM_GeothermalCosts", "condensate_pump_power");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_conversion_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "conversion_type", &result))
		make_access_error("SAM_GeothermalCosts", "conversion_type");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_cw_pump_head_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cw_pump_head", &result))
		make_access_error("SAM_GeothermalCosts", "cw_pump_head");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_cw_pump_work_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cw_pump_work", &result))
		make_access_error("SAM_GeothermalCosts", "cw_pump_work");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_cwflow_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cwflow", &result))
		make_access_error("SAM_GeothermalCosts", "cwflow");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_design_temp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "design_temp", &result))
		make_access_error("SAM_GeothermalCosts", "design_temp");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_dt_prod_well_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dt_prod_well", &result))
		make_access_error("SAM_GeothermalCosts", "dt_prod_well");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_eff_secondlaw_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "eff_secondlaw", &result))
		make_access_error("SAM_GeothermalCosts", "eff_secondlaw");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_flash_count_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "flash_count", &result))
		make_access_error("SAM_GeothermalCosts", "flash_count");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_geotherm_cost_conf_multiplier_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "geotherm.cost.conf_multiplier", &result))
		make_access_error("SAM_GeothermalCosts", "geotherm.cost.conf_multiplier");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_geotherm_cost_conf_non_drill_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "geotherm.cost.conf_non_drill", &result))
		make_access_error("SAM_GeothermalCosts", "geotherm.cost.conf_non_drill");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_geotherm_cost_conf_num_wells_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "geotherm.cost.conf_num_wells", &result))
		make_access_error("SAM_GeothermalCosts", "geotherm.cost.conf_num_wells");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_geotherm_cost_expl_lump_sum_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "geotherm.cost.expl_lump_sum", &result))
		make_access_error("SAM_GeothermalCosts", "geotherm.cost.expl_lump_sum");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_geotherm_cost_expl_multiplier_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "geotherm.cost.expl_multiplier", &result))
		make_access_error("SAM_GeothermalCosts", "geotherm.cost.expl_multiplier");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_geotherm_cost_expl_non_drill_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "geotherm.cost.expl_non_drill", &result))
		make_access_error("SAM_GeothermalCosts", "geotherm.cost.expl_non_drill");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_geotherm_cost_expl_num_wells_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "geotherm.cost.expl_num_wells", &result))
		make_access_error("SAM_GeothermalCosts", "geotherm.cost.expl_num_wells");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_geotherm_cost_inj_cost_curve_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "geotherm.cost.inj_cost_curve", &result))
		make_access_error("SAM_GeothermalCosts", "geotherm.cost.inj_cost_curve");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_geotherm_cost_inj_cost_curve_welldiam_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "geotherm.cost.inj_cost_curve_welldiam", &result))
		make_access_error("SAM_GeothermalCosts", "geotherm.cost.inj_cost_curve_welldiam");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_geotherm_cost_inj_cost_curve_welltype_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "geotherm.cost.inj_cost_curve_welltype", &result))
		make_access_error("SAM_GeothermalCosts", "geotherm.cost.inj_cost_curve_welltype");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_geotherm_cost_prod_cost_curve_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "geotherm.cost.prod_cost_curve", &result))
		make_access_error("SAM_GeothermalCosts", "geotherm.cost.prod_cost_curve");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_geotherm_cost_prod_cost_curve_welldiam_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "geotherm.cost.prod_cost_curve_welldiam", &result))
		make_access_error("SAM_GeothermalCosts", "geotherm.cost.prod_cost_curve_welldiam");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_geotherm_cost_prod_cost_curve_welltype_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "geotherm.cost.prod_cost_curve_welltype", &result))
		make_access_error("SAM_GeothermalCosts", "geotherm.cost.prod_cost_curve_welltype");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_geotherm_cost_pump_casing_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "geotherm.cost.pump_casing_cost", &result))
		make_access_error("SAM_GeothermalCosts", "geotherm.cost.pump_casing_cost");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_geotherm_cost_pump_fixed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "geotherm.cost.pump_fixed", &result))
		make_access_error("SAM_GeothermalCosts", "geotherm.cost.pump_fixed");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_geotherm_cost_pump_per_foot_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "geotherm.cost.pump_per_foot", &result))
		make_access_error("SAM_GeothermalCosts", "geotherm.cost.pump_per_foot");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_geotherm_cost_stim_non_drill_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "geotherm.cost.stim_non_drill", &result))
		make_access_error("SAM_GeothermalCosts", "geotherm.cost.stim_non_drill");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_gross_cost_output_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "gross_cost_output", &result))
		make_access_error("SAM_GeothermalCosts", "gross_cost_output");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_gross_output_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "gross_output", &result))
		make_access_error("SAM_GeothermalCosts", "gross_output");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_hp_flash_pressure_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hp_flash_pressure", &result))
		make_access_error("SAM_GeothermalCosts", "hp_flash_pressure");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_inj_pump_hp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inj_pump_hp", &result))
		make_access_error("SAM_GeothermalCosts", "inj_pump_hp");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_lp_flash_pressure_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "lp_flash_pressure", &result))
		make_access_error("SAM_GeothermalCosts", "lp_flash_pressure");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_ncg_condensate_pump_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ncg_condensate_pump", &result))
		make_access_error("SAM_GeothermalCosts", "ncg_condensate_pump");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_num_wells_getem_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "num_wells_getem", &result))
		make_access_error("SAM_GeothermalCosts", "num_wells_getem");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_num_wells_getem_inj_drilled_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "num_wells_getem_inj_drilled", &result))
		make_access_error("SAM_GeothermalCosts", "num_wells_getem_inj_drilled");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_num_wells_getem_prod_drilled_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "num_wells_getem_prod_drilled", &result))
		make_access_error("SAM_GeothermalCosts", "num_wells_getem_prod_drilled");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_num_wells_getem_prod_failed_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "num_wells_getem_prod_failed", &result))
		make_access_error("SAM_GeothermalCosts", "num_wells_getem_prod_failed");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_ppi_base_year_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ppi_base_year", &result))
		make_access_error("SAM_GeothermalCosts", "ppi_base_year");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_pressure_ratio_1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pressure_ratio_1", &result))
		make_access_error("SAM_GeothermalCosts", "pressure_ratio_1");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_pressure_ratio_2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pressure_ratio_2", &result))
		make_access_error("SAM_GeothermalCosts", "pressure_ratio_2");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_pressure_ratio_3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pressure_ratio_3", &result))
		make_access_error("SAM_GeothermalCosts", "pressure_ratio_3");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_pump_depth_ft_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pump_depth_ft", &result))
		make_access_error("SAM_GeothermalCosts", "pump_depth_ft");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_pump_size_hp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pump_size_hp", &result))
		make_access_error("SAM_GeothermalCosts", "pump_size_hp");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_qCondenser_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "qCondenser", &result))
		make_access_error("SAM_GeothermalCosts", "qCondenser");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_qRejectByStage_1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "qRejectByStage_1", &result))
		make_access_error("SAM_GeothermalCosts", "qRejectByStage_1");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_qRejectByStage_2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "qRejectByStage_2", &result))
		make_access_error("SAM_GeothermalCosts", "qRejectByStage_2");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_qRejectByStage_3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "qRejectByStage_3", &result))
		make_access_error("SAM_GeothermalCosts", "qRejectByStage_3");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_qRejectTotal_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "qRejectTotal", &result))
		make_access_error("SAM_GeothermalCosts", "qRejectTotal");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_resource_depth_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "resource_depth", &result))
		make_access_error("SAM_GeothermalCosts", "resource_depth");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_spec_vol_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "spec_vol", &result))
		make_access_error("SAM_GeothermalCosts", "spec_vol");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_spec_vol_lp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "spec_vol_lp", &result))
		make_access_error("SAM_GeothermalCosts", "spec_vol_lp");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_stimulation_type_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "stimulation_type", &result))
		make_access_error("SAM_GeothermalCosts", "stimulation_type");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_v_stage_1_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "v_stage_1", &result))
		make_access_error("SAM_GeothermalCosts", "v_stage_1");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_v_stage_2_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "v_stage_2", &result))
		make_access_error("SAM_GeothermalCosts", "v_stage_2");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_v_stage_3_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "v_stage_3", &result))
		make_access_error("SAM_GeothermalCosts", "v_stage_3");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_x_hp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "x_hp", &result))
		make_access_error("SAM_GeothermalCosts", "x_hp");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_GeoHourly_x_lp_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "x_lp", &result))
		make_access_error("SAM_GeothermalCosts", "x_lp");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_Outputs_atb_drilling_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "atb_drilling_cost", &result))
		make_access_error("SAM_GeothermalCosts", "atb_drilling_cost");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_Outputs_atb_exploration_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "atb_exploration_cost", &result))
		make_access_error("SAM_GeothermalCosts", "atb_exploration_cost");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_Outputs_atb_plant_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "atb_plant_cost", &result))
		make_access_error("SAM_GeothermalCosts", "atb_plant_cost");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_Outputs_baseline_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "baseline_cost", &result))
		make_access_error("SAM_GeothermalCosts", "baseline_cost");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_Outputs_conf_drilling_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "conf_drilling_cost", &result))
		make_access_error("SAM_GeothermalCosts", "conf_drilling_cost");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_Outputs_conf_total_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "conf_total_cost", &result))
		make_access_error("SAM_GeothermalCosts", "conf_total_cost");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_Outputs_engineering_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "engineering_cost", &result))
		make_access_error("SAM_GeothermalCosts", "engineering_cost");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_Outputs_expl_drilling_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "expl_drilling_cost", &result))
		make_access_error("SAM_GeothermalCosts", "expl_drilling_cost");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_Outputs_expl_total_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "expl_total_cost", &result))
		make_access_error("SAM_GeothermalCosts", "expl_total_cost");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_Outputs_field_gathering_num_wells_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "field_gathering_num_wells", &result))
		make_access_error("SAM_GeothermalCosts", "field_gathering_num_wells");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_Outputs_indirect_pump_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "indirect_pump_cost", &result))
		make_access_error("SAM_GeothermalCosts", "indirect_pump_cost");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_Outputs_indirect_pump_gathering_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "indirect_pump_gathering_cost", &result))
		make_access_error("SAM_GeothermalCosts", "indirect_pump_gathering_cost");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_Outputs_inj_num_pumps_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inj_num_pumps", &result))
		make_access_error("SAM_GeothermalCosts", "inj_num_pumps");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_Outputs_inj_pump_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inj_pump_cost", &result))
		make_access_error("SAM_GeothermalCosts", "inj_pump_cost");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_Outputs_inj_pump_cost_per_pump_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inj_pump_cost_per_pump", &result))
		make_access_error("SAM_GeothermalCosts", "inj_pump_cost_per_pump");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_Outputs_inj_total_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inj_total_cost", &result))
		make_access_error("SAM_GeothermalCosts", "inj_total_cost");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_Outputs_inj_well_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inj_well_cost", &result))
		make_access_error("SAM_GeothermalCosts", "inj_well_cost");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_Outputs_piping_cost_per_well_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "piping_cost_per_well", &result))
		make_access_error("SAM_GeothermalCosts", "piping_cost_per_well");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_Outputs_prod_pump_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "prod_pump_cost", &result))
		make_access_error("SAM_GeothermalCosts", "prod_pump_cost");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_Outputs_prod_pump_cost_per_well_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "prod_pump_cost_per_well", &result))
		make_access_error("SAM_GeothermalCosts", "prod_pump_cost_per_well");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_Outputs_prod_total_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "prod_total_cost", &result))
		make_access_error("SAM_GeothermalCosts", "prod_total_cost");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_Outputs_prod_well_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "prod_well_cost", &result))
		make_access_error("SAM_GeothermalCosts", "prod_well_cost");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_Outputs_pump_cost_install_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pump_cost_install", &result))
		make_access_error("SAM_GeothermalCosts", "pump_cost_install");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_Outputs_pump_only_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pump_only_cost", &result))
		make_access_error("SAM_GeothermalCosts", "pump_only_cost");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_Outputs_stim_cost_non_drill_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "stim_cost_non_drill", &result))
		make_access_error("SAM_GeothermalCosts", "stim_cost_non_drill");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_Outputs_stim_cost_per_well_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "stim_cost_per_well", &result))
		make_access_error("SAM_GeothermalCosts", "stim_cost_per_well");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_Outputs_stim_total_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "stim_total_cost", &result))
		make_access_error("SAM_GeothermalCosts", "stim_total_cost");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_Outputs_total_drilling_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_drilling_cost", &result))
		make_access_error("SAM_GeothermalCosts", "total_drilling_cost");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_Outputs_total_drilling_permitting_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_drilling_permitting", &result))
		make_access_error("SAM_GeothermalCosts", "total_drilling_permitting");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_Outputs_total_expl_permitting_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_expl_permitting", &result))
		make_access_error("SAM_GeothermalCosts", "total_expl_permitting");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_Outputs_total_gathering_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_gathering_cost", &result))
		make_access_error("SAM_GeothermalCosts", "total_gathering_cost");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_Outputs_total_pump_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_pump_cost", &result))
		make_access_error("SAM_GeothermalCosts", "total_pump_cost");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_Outputs_total_pump_gathering_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_pump_gathering_cost", &result))
		make_access_error("SAM_GeothermalCosts", "total_pump_gathering_cost");
	});
	return result;
}

SAM_EXPORT double SAM_GeothermalCosts_Outputs_total_surface_equipment_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_surface_equipment_cost", &result))
		make_access_error("SAM_GeothermalCosts", "total_surface_equipment_cost");
	});
	return result;
}

