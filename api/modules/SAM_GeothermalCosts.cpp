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



SAM_EXPORT double SAM_GeothermalCosts_Outputs_baseline_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "baseline_cost", &result))
		make_access_error("SAM_GeothermalCosts", "baseline_cost");
	});
	return result;
}



