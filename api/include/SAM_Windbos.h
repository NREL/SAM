#ifndef SAM_WINDBOS_H_
#define SAM_WINDBOS_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>

#ifdef __cplusplus
extern "C"
{
#endif

//
// Windbos Technology Model
//

/**
 * Create a Windbos variable table.
 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
 * @param[in,out] err: a pointer to an error object
 */

SAM_EXPORT typedef void *SAM_Windbos;

SAM_EXPORT SAM_Windbos SAM_Windbos_construct(const char *def, SAM_error *err);

/// verbosity level 0 or 1. Returns 1 on success
SAM_EXPORT int SAM_Windbos_execute(SAM_Windbos data, int verbosity, SAM_error *err);

SAM_EXPORT void SAM_Windbos_destruct(SAM_Windbos system);


//
// WindBos parameters
//

/**
 * Set access_road_entrances: Access road entrances
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Windbos_WindBos_access_road_entrances_nset(SAM_Windbos ptr, double number, SAM_error *err);

/**
 * Set construction_time: Construction Time [months]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Windbos_WindBos_construction_time_nset(SAM_Windbos ptr, double number, SAM_error *err);

/**
 * Set contingency: Contingency [%]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Windbos_WindBos_contingency_nset(SAM_Windbos ptr, double number, SAM_error *err);

/**
 * Set crane_breakdowns: Crane breakdowns
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Windbos_WindBos_crane_breakdowns_nset(SAM_Windbos ptr, double number, SAM_error *err);

/**
 * Set delivery_assist_required: Delivery Assist Required [y/n]
 * options: None
 * constraints: INTEGER
 * required if: *
 */
SAM_EXPORT void SAM_Windbos_WindBos_delivery_assist_required_nset(SAM_Windbos ptr, double number, SAM_error *err);

/**
 * Set development_fee: Development Fee [$M]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Windbos_WindBos_development_fee_nset(SAM_Windbos ptr, double number, SAM_error *err);

/**
 * Set distance_to_interconnect: Distance to Interconnect [miles]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Windbos_WindBos_distance_to_interconnect_nset(SAM_Windbos ptr, double number, SAM_error *err);

/**
 * Set hub_height: Hub Height [m]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Windbos_WindBos_hub_height_nset(SAM_Windbos ptr, double number, SAM_error *err);

/**
 * Set interconnect_voltage: Interconnect Voltage [kV]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Windbos_WindBos_interconnect_voltage_nset(SAM_Windbos ptr, double number, SAM_error *err);

/**
 * Set machine_rating: Machine Rating [kW]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Windbos_WindBos_machine_rating_nset(SAM_Windbos ptr, double number, SAM_error *err);

/**
 * Set mv_overhead_collector: MV overhead collector [mi]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Windbos_WindBos_mv_overhead_collector_nset(SAM_Windbos ptr, double number, SAM_error *err);

/**
 * Set mv_thermal_backfill: MV thermal backfill [mi]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Windbos_WindBos_mv_thermal_backfill_nset(SAM_Windbos ptr, double number, SAM_error *err);

/**
 * Set new_switchyard_required: New Switchyard Required [y/n]
 * options: None
 * constraints: INTEGER
 * required if: *
 */
SAM_EXPORT void SAM_Windbos_WindBos_new_switchyard_required_nset(SAM_Windbos ptr, double number, SAM_error *err);

/**
 * Set number_of_turbines: Number of Turbines
 * options: None
 * constraints: INTEGER
 * required if: *
 */
SAM_EXPORT void SAM_Windbos_WindBos_number_of_turbines_nset(SAM_Windbos ptr, double number, SAM_error *err);

/**
 * Set om_building_size: O&M Building Size [ft^2]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Windbos_WindBos_om_building_size_nset(SAM_Windbos ptr, double number, SAM_error *err);

/**
 * Set overhead: Overhead [%]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Windbos_WindBos_overhead_nset(SAM_Windbos ptr, double number, SAM_error *err);

/**
 * Set pad_mount_transformer_required: Pad mount Transformer required [y/n]
 * options: None
 * constraints: INTEGER
 * required if: *
 */
SAM_EXPORT void SAM_Windbos_WindBos_pad_mount_transformer_required_nset(SAM_Windbos ptr, double number, SAM_error *err);

/**
 * Set performance_bond: Performance bond [%]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Windbos_WindBos_performance_bond_nset(SAM_Windbos ptr, double number, SAM_error *err);

/**
 * Set profit_margin: Profit Margin [%]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Windbos_WindBos_profit_margin_nset(SAM_Windbos ptr, double number, SAM_error *err);

/**
 * Set quantity_permanent_met_towers: Quantity of Permanent Meteorological Towers for Testing
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Windbos_WindBos_quantity_permanent_met_towers_nset(SAM_Windbos ptr, double number, SAM_error *err);

/**
 * Set quantity_test_met_towers: Quantity of Temporary Meteorological Towers for Testing
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Windbos_WindBos_quantity_test_met_towers_nset(SAM_Windbos ptr, double number, SAM_error *err);

/**
 * Set rock_trenching_required: Rock trenching required [%]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Windbos_WindBos_rock_trenching_required_nset(SAM_Windbos ptr, double number, SAM_error *err);

/**
 * Set rotor_diameter: Rotor Diameter [m]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Windbos_WindBos_rotor_diameter_nset(SAM_Windbos ptr, double number, SAM_error *err);

/**
 * Set sales_and_use_tax: Sales and Use Tax [%]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Windbos_WindBos_sales_and_use_tax_nset(SAM_Windbos ptr, double number, SAM_error *err);

/**
 * Set site_terrain: Site Terrain
 * options: None
 * constraints: INTEGER
 * required if: *
 */
SAM_EXPORT void SAM_Windbos_WindBos_site_terrain_nset(SAM_Windbos ptr, double number, SAM_error *err);

/**
 * Set soil_condition: Soil Condition
 * options: None
 * constraints: INTEGER
 * required if: *
 */
SAM_EXPORT void SAM_Windbos_WindBos_soil_condition_nset(SAM_Windbos ptr, double number, SAM_error *err);

/**
 * Set tower_top_mass: Tower Top Mass [Tonnes]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Windbos_WindBos_tower_top_mass_nset(SAM_Windbos ptr, double number, SAM_error *err);

/**
 * Set turbine_capital_cost: Turbine Capital Cost [$/kW]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Windbos_WindBos_turbine_capital_cost_nset(SAM_Windbos ptr, double number, SAM_error *err);

/**
 * Set turbine_layout: Turbine Layout
 * options: None
 * constraints: INTEGER
 * required if: *
 */
SAM_EXPORT void SAM_Windbos_WindBos_turbine_layout_nset(SAM_Windbos ptr, double number, SAM_error *err);

/**
 * Set turbine_transportation: Turbine Transportation [mi]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Windbos_WindBos_turbine_transportation_nset(SAM_Windbos ptr, double number, SAM_error *err);

/**
 * Set warranty_management: Warranty management [%]
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Windbos_WindBos_warranty_management_nset(SAM_Windbos ptr, double number, SAM_error *err);

/**
 * Set weather_delay_days: Wind / Weather delay days
 * options: None
 * constraints: None
 * required if: *
 */
SAM_EXPORT void SAM_Windbos_WindBos_weather_delay_days_nset(SAM_Windbos ptr, double number, SAM_error *err);


/**
 * WindBos Getters
 */

SAM_EXPORT double SAM_Windbos_WindBos_access_road_entrances_nget(SAM_Windbos ptr, SAM_error *err);

SAM_EXPORT double SAM_Windbos_WindBos_construction_time_nget(SAM_Windbos ptr, SAM_error *err);

SAM_EXPORT double SAM_Windbos_WindBos_contingency_nget(SAM_Windbos ptr, SAM_error *err);

SAM_EXPORT double SAM_Windbos_WindBos_crane_breakdowns_nget(SAM_Windbos ptr, SAM_error *err);

SAM_EXPORT double SAM_Windbos_WindBos_delivery_assist_required_nget(SAM_Windbos ptr, SAM_error *err);

SAM_EXPORT double SAM_Windbos_WindBos_development_fee_nget(SAM_Windbos ptr, SAM_error *err);

SAM_EXPORT double SAM_Windbos_WindBos_distance_to_interconnect_nget(SAM_Windbos ptr, SAM_error *err);

SAM_EXPORT double SAM_Windbos_WindBos_hub_height_nget(SAM_Windbos ptr, SAM_error *err);

SAM_EXPORT double SAM_Windbos_WindBos_interconnect_voltage_nget(SAM_Windbos ptr, SAM_error *err);

SAM_EXPORT double SAM_Windbos_WindBos_machine_rating_nget(SAM_Windbos ptr, SAM_error *err);

SAM_EXPORT double SAM_Windbos_WindBos_mv_overhead_collector_nget(SAM_Windbos ptr, SAM_error *err);

SAM_EXPORT double SAM_Windbos_WindBos_mv_thermal_backfill_nget(SAM_Windbos ptr, SAM_error *err);

SAM_EXPORT double SAM_Windbos_WindBos_new_switchyard_required_nget(SAM_Windbos ptr, SAM_error *err);

SAM_EXPORT double SAM_Windbos_WindBos_number_of_turbines_nget(SAM_Windbos ptr, SAM_error *err);

SAM_EXPORT double SAM_Windbos_WindBos_om_building_size_nget(SAM_Windbos ptr, SAM_error *err);

SAM_EXPORT double SAM_Windbos_WindBos_overhead_nget(SAM_Windbos ptr, SAM_error *err);

SAM_EXPORT double SAM_Windbos_WindBos_pad_mount_transformer_required_nget(SAM_Windbos ptr, SAM_error *err);

SAM_EXPORT double SAM_Windbos_WindBos_performance_bond_nget(SAM_Windbos ptr, SAM_error *err);

SAM_EXPORT double SAM_Windbos_WindBos_profit_margin_nget(SAM_Windbos ptr, SAM_error *err);

SAM_EXPORT double SAM_Windbos_WindBos_quantity_permanent_met_towers_nget(SAM_Windbos ptr, SAM_error *err);

SAM_EXPORT double SAM_Windbos_WindBos_quantity_test_met_towers_nget(SAM_Windbos ptr, SAM_error *err);

SAM_EXPORT double SAM_Windbos_WindBos_rock_trenching_required_nget(SAM_Windbos ptr, SAM_error *err);

SAM_EXPORT double SAM_Windbos_WindBos_rotor_diameter_nget(SAM_Windbos ptr, SAM_error *err);

SAM_EXPORT double SAM_Windbos_WindBos_sales_and_use_tax_nget(SAM_Windbos ptr, SAM_error *err);

SAM_EXPORT double SAM_Windbos_WindBos_site_terrain_nget(SAM_Windbos ptr, SAM_error *err);

SAM_EXPORT double SAM_Windbos_WindBos_soil_condition_nget(SAM_Windbos ptr, SAM_error *err);

SAM_EXPORT double SAM_Windbos_WindBos_tower_top_mass_nget(SAM_Windbos ptr, SAM_error *err);

SAM_EXPORT double SAM_Windbos_WindBos_turbine_capital_cost_nget(SAM_Windbos ptr, SAM_error *err);

SAM_EXPORT double SAM_Windbos_WindBos_turbine_layout_nget(SAM_Windbos ptr, SAM_error *err);

SAM_EXPORT double SAM_Windbos_WindBos_turbine_transportation_nget(SAM_Windbos ptr, SAM_error *err);

SAM_EXPORT double SAM_Windbos_WindBos_warranty_management_nget(SAM_Windbos ptr, SAM_error *err);

SAM_EXPORT double SAM_Windbos_WindBos_weather_delay_days_nget(SAM_Windbos ptr, SAM_error *err);


/**
 * Outputs Getters
 */

SAM_EXPORT double SAM_Windbos_Outputs_access_roads_cost_nget(SAM_Windbos ptr, SAM_error *err);

SAM_EXPORT double SAM_Windbos_Outputs_building_cost_nget(SAM_Windbos ptr, SAM_error *err);

SAM_EXPORT double SAM_Windbos_Outputs_development_cost_nget(SAM_Windbos ptr, SAM_error *err);

SAM_EXPORT double SAM_Windbos_Outputs_electrical_installation_cost_nget(SAM_Windbos ptr, SAM_error *err);

SAM_EXPORT double SAM_Windbos_Outputs_electrical_materials_cost_nget(SAM_Windbos ptr, SAM_error *err);

SAM_EXPORT double SAM_Windbos_Outputs_engineering_cost_nget(SAM_Windbos ptr, SAM_error *err);

SAM_EXPORT double SAM_Windbos_Outputs_erection_cost_nget(SAM_Windbos ptr, SAM_error *err);

SAM_EXPORT double SAM_Windbos_Outputs_foundation_cost_nget(SAM_Windbos ptr, SAM_error *err);

SAM_EXPORT double SAM_Windbos_Outputs_insurance_cost_nget(SAM_Windbos ptr, SAM_error *err);

SAM_EXPORT double SAM_Windbos_Outputs_markup_cost_nget(SAM_Windbos ptr, SAM_error *err);

SAM_EXPORT double SAM_Windbos_Outputs_power_performance_cost_nget(SAM_Windbos ptr, SAM_error *err);

SAM_EXPORT double SAM_Windbos_Outputs_project_mgmt_cost_nget(SAM_Windbos ptr, SAM_error *err);

SAM_EXPORT double SAM_Windbos_Outputs_project_total_budgeted_cost_nget(SAM_Windbos ptr, SAM_error *err);

SAM_EXPORT double SAM_Windbos_Outputs_site_compound_security_cost_nget(SAM_Windbos ptr, SAM_error *err);

SAM_EXPORT double SAM_Windbos_Outputs_substation_cost_nget(SAM_Windbos ptr, SAM_error *err);

SAM_EXPORT double SAM_Windbos_Outputs_transmission_cost_nget(SAM_Windbos ptr, SAM_error *err);

SAM_EXPORT double SAM_Windbos_Outputs_transportation_cost_nget(SAM_Windbos ptr, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif
