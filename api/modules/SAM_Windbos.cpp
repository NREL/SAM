#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_Windbos.h"

SAM_EXPORT int SAM_Windbos_execute(SAM_table data, int verbosity, SAM_error* err){
	return SAM_module_exec("windbos", data, verbosity, err);
}

SAM_EXPORT void SAM_Windbos_WindBos_access_road_entrances_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "access_road_entrances", number);
	});
}

SAM_EXPORT void SAM_Windbos_WindBos_construction_time_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "construction_time", number);
	});
}

SAM_EXPORT void SAM_Windbos_WindBos_contingency_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "contingency", number);
	});
}

SAM_EXPORT void SAM_Windbos_WindBos_crane_breakdowns_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "crane_breakdowns", number);
	});
}

SAM_EXPORT void SAM_Windbos_WindBos_delivery_assist_required_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "delivery_assist_required", number);
	});
}

SAM_EXPORT void SAM_Windbos_WindBos_development_fee_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "development_fee", number);
	});
}

SAM_EXPORT void SAM_Windbos_WindBos_distance_to_interconnect_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "distance_to_interconnect", number);
	});
}

SAM_EXPORT void SAM_Windbos_WindBos_hub_height_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hub_height", number);
	});
}

SAM_EXPORT void SAM_Windbos_WindBos_interconnect_voltage_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "interconnect_voltage", number);
	});
}

SAM_EXPORT void SAM_Windbos_WindBos_machine_rating_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "machine_rating", number);
	});
}

SAM_EXPORT void SAM_Windbos_WindBos_mv_overhead_collector_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mv_overhead_collector", number);
	});
}

SAM_EXPORT void SAM_Windbos_WindBos_mv_thermal_backfill_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mv_thermal_backfill", number);
	});
}

SAM_EXPORT void SAM_Windbos_WindBos_new_switchyard_required_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "new_switchyard_required", number);
	});
}

SAM_EXPORT void SAM_Windbos_WindBos_number_of_turbines_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "number_of_turbines", number);
	});
}

SAM_EXPORT void SAM_Windbos_WindBos_om_building_size_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "om_building_size", number);
	});
}

SAM_EXPORT void SAM_Windbos_WindBos_overhead_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "overhead", number);
	});
}

SAM_EXPORT void SAM_Windbos_WindBos_pad_mount_transformer_required_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pad_mount_transformer_required", number);
	});
}

SAM_EXPORT void SAM_Windbos_WindBos_performance_bond_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "performance_bond", number);
	});
}

SAM_EXPORT void SAM_Windbos_WindBos_profit_margin_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "profit_margin", number);
	});
}

SAM_EXPORT void SAM_Windbos_WindBos_quantity_permanent_met_towers_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "quantity_permanent_met_towers", number);
	});
}

SAM_EXPORT void SAM_Windbos_WindBos_quantity_test_met_towers_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "quantity_test_met_towers", number);
	});
}

SAM_EXPORT void SAM_Windbos_WindBos_rock_trenching_required_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rock_trenching_required", number);
	});
}

SAM_EXPORT void SAM_Windbos_WindBos_rotor_diameter_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rotor_diameter", number);
	});
}

SAM_EXPORT void SAM_Windbos_WindBos_sales_and_use_tax_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sales_and_use_tax", number);
	});
}

SAM_EXPORT void SAM_Windbos_WindBos_site_terrain_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "site_terrain", number);
	});
}

SAM_EXPORT void SAM_Windbos_WindBos_soil_condition_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "soil_condition", number);
	});
}

SAM_EXPORT void SAM_Windbos_WindBos_tower_top_mass_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tower_top_mass", number);
	});
}

SAM_EXPORT void SAM_Windbos_WindBos_turbine_capital_cost_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "turbine_capital_cost", number);
	});
}

SAM_EXPORT void SAM_Windbos_WindBos_turbine_layout_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "turbine_layout", number);
	});
}

SAM_EXPORT void SAM_Windbos_WindBos_turbine_transportation_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "turbine_transportation", number);
	});
}

SAM_EXPORT void SAM_Windbos_WindBos_warranty_management_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "warranty_management", number);
	});
}

SAM_EXPORT void SAM_Windbos_WindBos_weather_delay_days_nset(SAM_table ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "weather_delay_days", number);
	});
}

SAM_EXPORT double SAM_Windbos_WindBos_access_road_entrances_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "access_road_entrances", &result))
		make_access_error("SAM_Windbos", "access_road_entrances");
	});
	return result;
}



SAM_EXPORT double SAM_Windbos_WindBos_construction_time_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "construction_time", &result))
		make_access_error("SAM_Windbos", "construction_time");
	});
	return result;
}



SAM_EXPORT double SAM_Windbos_WindBos_contingency_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "contingency", &result))
		make_access_error("SAM_Windbos", "contingency");
	});
	return result;
}



SAM_EXPORT double SAM_Windbos_WindBos_crane_breakdowns_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "crane_breakdowns", &result))
		make_access_error("SAM_Windbos", "crane_breakdowns");
	});
	return result;
}



SAM_EXPORT double SAM_Windbos_WindBos_delivery_assist_required_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "delivery_assist_required", &result))
		make_access_error("SAM_Windbos", "delivery_assist_required");
	});
	return result;
}



SAM_EXPORT double SAM_Windbos_WindBos_development_fee_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "development_fee", &result))
		make_access_error("SAM_Windbos", "development_fee");
	});
	return result;
}



SAM_EXPORT double SAM_Windbos_WindBos_distance_to_interconnect_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "distance_to_interconnect", &result))
		make_access_error("SAM_Windbos", "distance_to_interconnect");
	});
	return result;
}



SAM_EXPORT double SAM_Windbos_WindBos_hub_height_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hub_height", &result))
		make_access_error("SAM_Windbos", "hub_height");
	});
	return result;
}



SAM_EXPORT double SAM_Windbos_WindBos_interconnect_voltage_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "interconnect_voltage", &result))
		make_access_error("SAM_Windbos", "interconnect_voltage");
	});
	return result;
}



SAM_EXPORT double SAM_Windbos_WindBos_machine_rating_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "machine_rating", &result))
		make_access_error("SAM_Windbos", "machine_rating");
	});
	return result;
}



SAM_EXPORT double SAM_Windbos_WindBos_mv_overhead_collector_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mv_overhead_collector", &result))
		make_access_error("SAM_Windbos", "mv_overhead_collector");
	});
	return result;
}



SAM_EXPORT double SAM_Windbos_WindBos_mv_thermal_backfill_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mv_thermal_backfill", &result))
		make_access_error("SAM_Windbos", "mv_thermal_backfill");
	});
	return result;
}



SAM_EXPORT double SAM_Windbos_WindBos_new_switchyard_required_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "new_switchyard_required", &result))
		make_access_error("SAM_Windbos", "new_switchyard_required");
	});
	return result;
}



SAM_EXPORT double SAM_Windbos_WindBos_number_of_turbines_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "number_of_turbines", &result))
		make_access_error("SAM_Windbos", "number_of_turbines");
	});
	return result;
}



SAM_EXPORT double SAM_Windbos_WindBos_om_building_size_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "om_building_size", &result))
		make_access_error("SAM_Windbos", "om_building_size");
	});
	return result;
}



SAM_EXPORT double SAM_Windbos_WindBos_overhead_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "overhead", &result))
		make_access_error("SAM_Windbos", "overhead");
	});
	return result;
}



SAM_EXPORT double SAM_Windbos_WindBos_pad_mount_transformer_required_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pad_mount_transformer_required", &result))
		make_access_error("SAM_Windbos", "pad_mount_transformer_required");
	});
	return result;
}



SAM_EXPORT double SAM_Windbos_WindBos_performance_bond_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "performance_bond", &result))
		make_access_error("SAM_Windbos", "performance_bond");
	});
	return result;
}



SAM_EXPORT double SAM_Windbos_WindBos_profit_margin_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "profit_margin", &result))
		make_access_error("SAM_Windbos", "profit_margin");
	});
	return result;
}



SAM_EXPORT double SAM_Windbos_WindBos_quantity_permanent_met_towers_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "quantity_permanent_met_towers", &result))
		make_access_error("SAM_Windbos", "quantity_permanent_met_towers");
	});
	return result;
}



SAM_EXPORT double SAM_Windbos_WindBos_quantity_test_met_towers_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "quantity_test_met_towers", &result))
		make_access_error("SAM_Windbos", "quantity_test_met_towers");
	});
	return result;
}



SAM_EXPORT double SAM_Windbos_WindBos_rock_trenching_required_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rock_trenching_required", &result))
		make_access_error("SAM_Windbos", "rock_trenching_required");
	});
	return result;
}



SAM_EXPORT double SAM_Windbos_WindBos_rotor_diameter_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rotor_diameter", &result))
		make_access_error("SAM_Windbos", "rotor_diameter");
	});
	return result;
}



SAM_EXPORT double SAM_Windbos_WindBos_sales_and_use_tax_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sales_and_use_tax", &result))
		make_access_error("SAM_Windbos", "sales_and_use_tax");
	});
	return result;
}



SAM_EXPORT double SAM_Windbos_WindBos_site_terrain_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "site_terrain", &result))
		make_access_error("SAM_Windbos", "site_terrain");
	});
	return result;
}



SAM_EXPORT double SAM_Windbos_WindBos_soil_condition_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "soil_condition", &result))
		make_access_error("SAM_Windbos", "soil_condition");
	});
	return result;
}



SAM_EXPORT double SAM_Windbos_WindBos_tower_top_mass_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tower_top_mass", &result))
		make_access_error("SAM_Windbos", "tower_top_mass");
	});
	return result;
}



SAM_EXPORT double SAM_Windbos_WindBos_turbine_capital_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "turbine_capital_cost", &result))
		make_access_error("SAM_Windbos", "turbine_capital_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Windbos_WindBos_turbine_layout_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "turbine_layout", &result))
		make_access_error("SAM_Windbos", "turbine_layout");
	});
	return result;
}



SAM_EXPORT double SAM_Windbos_WindBos_turbine_transportation_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "turbine_transportation", &result))
		make_access_error("SAM_Windbos", "turbine_transportation");
	});
	return result;
}



SAM_EXPORT double SAM_Windbos_WindBos_warranty_management_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "warranty_management", &result))
		make_access_error("SAM_Windbos", "warranty_management");
	});
	return result;
}



SAM_EXPORT double SAM_Windbos_WindBos_weather_delay_days_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "weather_delay_days", &result))
		make_access_error("SAM_Windbos", "weather_delay_days");
	});
	return result;
}



SAM_EXPORT double SAM_Windbos_Outputs_access_roads_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "access_roads_cost", &result))
		make_access_error("SAM_Windbos", "access_roads_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Windbos_Outputs_building_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "building_cost", &result))
		make_access_error("SAM_Windbos", "building_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Windbos_Outputs_development_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "development_cost", &result))
		make_access_error("SAM_Windbos", "development_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Windbos_Outputs_electrical_installation_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "electrical_installation_cost", &result))
		make_access_error("SAM_Windbos", "electrical_installation_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Windbos_Outputs_electrical_materials_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "electrical_materials_cost", &result))
		make_access_error("SAM_Windbos", "electrical_materials_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Windbos_Outputs_engineering_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "engineering_cost", &result))
		make_access_error("SAM_Windbos", "engineering_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Windbos_Outputs_erection_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "erection_cost", &result))
		make_access_error("SAM_Windbos", "erection_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Windbos_Outputs_foundation_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "foundation_cost", &result))
		make_access_error("SAM_Windbos", "foundation_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Windbos_Outputs_insurance_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "insurance_cost", &result))
		make_access_error("SAM_Windbos", "insurance_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Windbos_Outputs_markup_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "markup_cost", &result))
		make_access_error("SAM_Windbos", "markup_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Windbos_Outputs_power_performance_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "power_performance_cost", &result))
		make_access_error("SAM_Windbos", "power_performance_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Windbos_Outputs_project_mgmt_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "project_mgmt_cost", &result))
		make_access_error("SAM_Windbos", "project_mgmt_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Windbos_Outputs_project_total_budgeted_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "project_total_budgeted_cost", &result))
		make_access_error("SAM_Windbos", "project_total_budgeted_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Windbos_Outputs_site_compound_security_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "site_compound_security_cost", &result))
		make_access_error("SAM_Windbos", "site_compound_security_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Windbos_Outputs_substation_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "substation_cost", &result))
		make_access_error("SAM_Windbos", "substation_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Windbos_Outputs_transmission_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "transmission_cost", &result))
		make_access_error("SAM_Windbos", "transmission_cost");
	});
	return result;
}



SAM_EXPORT double SAM_Windbos_Outputs_transportation_cost_nget(SAM_table ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "transportation_cost", &result))
		make_access_error("SAM_Windbos", "transportation_cost");
	});
	return result;
}



