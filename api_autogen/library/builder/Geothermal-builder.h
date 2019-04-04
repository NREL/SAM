#ifndef SAM_GEOTHERMAL_FUNCTIONS_H_
#define SAM_GEOTHERMAL_FUNCTIONS_H_

#include "Geothermal-data.h"

#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	/** 
	 * Create a GeothermalResource variable table for a GeothermalPowerNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_Geothermal_GeothermalResource SAM_Geothermal_GeothermalResource_create(const char* def, SAM_error* err);


	/**
	 * Set fracture_angle: Fracture angle
	 * type: numeric
	 * units: deg
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_GeothermalResource_fracture_angle_set(SAM_Geothermal_GeothermalResource ptr, float number, SAM_error* err);

	/**
	 * Set fracture_aperature: Fracture aperature
	 * type: numeric
	 * units: m
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_GeothermalResource_fracture_aperature_set(SAM_Geothermal_GeothermalResource ptr, float number, SAM_error* err);

	/**
	 * Set fracture_width: Fracture width
	 * type: numeric
	 * units: m
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_GeothermalResource_fracture_width_set(SAM_Geothermal_GeothermalResource ptr, float number, SAM_error* err);

	/**
	 * Set inj_prod_well_distance: Distance from injection to production wells
	 * type: numeric
	 * units: m
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_GeothermalResource_inj_prod_well_distance_set(SAM_Geothermal_GeothermalResource ptr, float number, SAM_error* err);

	/**
	 * Set num_fractures: Number of fractures
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: INTEGER
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_GeothermalResource_num_fractures_set(SAM_Geothermal_GeothermalResource ptr, float number, SAM_error* err);

	/**
	 * Set reservoir_height: Reservoir height
	 * type: numeric
	 * units: m
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_GeothermalResource_reservoir_height_set(SAM_Geothermal_GeothermalResource ptr, float number, SAM_error* err);

	/**
	 * Set reservoir_permeability: Reservoir Permeability
	 * type: numeric
	 * units: darcys
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_GeothermalResource_reservoir_permeability_set(SAM_Geothermal_GeothermalResource ptr, float number, SAM_error* err);

	/**
	 * Set reservoir_pressure_change: Pressure change
	 * type: numeric
	 * units: psi-h/1000lb
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_GeothermalResource_reservoir_pressure_change_set(SAM_Geothermal_GeothermalResource ptr, float number, SAM_error* err);

	/**
	 * Set reservoir_pressure_change_type: Reservoir pressure change type
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: INTEGER
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_GeothermalResource_reservoir_pressure_change_type_set(SAM_Geothermal_GeothermalResource ptr, float number, SAM_error* err);

	/**
	 * Set reservoir_width: Reservoir width
	 * type: numeric
	 * units: m
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_GeothermalResource_reservoir_width_set(SAM_Geothermal_GeothermalResource ptr, float number, SAM_error* err);

	/**
	 * Set resource_depth: Resource Depth
	 * type: numeric
	 * units: m
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_GeothermalResource_resource_depth_set(SAM_Geothermal_GeothermalResource ptr, float number, SAM_error* err);

	/**
	 * Set resource_temp: Resource Temperature
	 * type: numeric
	 * units: C
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_GeothermalResource_resource_temp_set(SAM_Geothermal_GeothermalResource ptr, float number, SAM_error* err);

	/**
	 * Set resource_type: Type of Resource
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: INTEGER
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_GeothermalResource_resource_type_set(SAM_Geothermal_GeothermalResource ptr, float number, SAM_error* err);

	/**
	 * Set rock_density: Rock density
	 * type: numeric
	 * units: kg/m^3
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_GeothermalResource_rock_density_set(SAM_Geothermal_GeothermalResource ptr, float number, SAM_error* err);

	/**
	 * Set rock_specific_heat: Rock specific heat
	 * type: numeric
	 * units: J/kg-C
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_GeothermalResource_rock_specific_heat_set(SAM_Geothermal_GeothermalResource ptr, float number, SAM_error* err);

	/**
	 * Set rock_thermal_conductivity: Rock thermal conductivity
	 * type: numeric
	 * units: J/m-day-C
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_GeothermalResource_rock_thermal_conductivity_set(SAM_Geothermal_GeothermalResource ptr, float number, SAM_error* err);

	/**
	 * Set subsurface_water_loss: Subsurface water loss
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_GeothermalResource_subsurface_water_loss_set(SAM_Geothermal_GeothermalResource ptr, float number, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_Geothermal_GeothermalResource_fracture_angle_get(SAM_Geothermal_GeothermalResource ptr, SAM_error* err);

	SAM_EXPORT float SAM_Geothermal_GeothermalResource_fracture_aperature_get(SAM_Geothermal_GeothermalResource ptr, SAM_error* err);

	SAM_EXPORT float SAM_Geothermal_GeothermalResource_fracture_width_get(SAM_Geothermal_GeothermalResource ptr, SAM_error* err);

	SAM_EXPORT float SAM_Geothermal_GeothermalResource_inj_prod_well_distance_get(SAM_Geothermal_GeothermalResource ptr, SAM_error* err);

	SAM_EXPORT float SAM_Geothermal_GeothermalResource_num_fractures_get(SAM_Geothermal_GeothermalResource ptr, SAM_error* err);

	SAM_EXPORT float SAM_Geothermal_GeothermalResource_reservoir_height_get(SAM_Geothermal_GeothermalResource ptr, SAM_error* err);

	SAM_EXPORT float SAM_Geothermal_GeothermalResource_reservoir_permeability_get(SAM_Geothermal_GeothermalResource ptr, SAM_error* err);

	SAM_EXPORT float SAM_Geothermal_GeothermalResource_reservoir_pressure_change_get(SAM_Geothermal_GeothermalResource ptr, SAM_error* err);

	SAM_EXPORT float SAM_Geothermal_GeothermalResource_reservoir_pressure_change_type_get(SAM_Geothermal_GeothermalResource ptr, SAM_error* err);

	SAM_EXPORT float SAM_Geothermal_GeothermalResource_reservoir_width_get(SAM_Geothermal_GeothermalResource ptr, SAM_error* err);

	SAM_EXPORT float SAM_Geothermal_GeothermalResource_resource_depth_get(SAM_Geothermal_GeothermalResource ptr, SAM_error* err);

	SAM_EXPORT float SAM_Geothermal_GeothermalResource_resource_temp_get(SAM_Geothermal_GeothermalResource ptr, SAM_error* err);

	SAM_EXPORT float SAM_Geothermal_GeothermalResource_resource_type_get(SAM_Geothermal_GeothermalResource ptr, SAM_error* err);

	SAM_EXPORT float SAM_Geothermal_GeothermalResource_rock_density_get(SAM_Geothermal_GeothermalResource ptr, SAM_error* err);

	SAM_EXPORT float SAM_Geothermal_GeothermalResource_rock_specific_heat_get(SAM_Geothermal_GeothermalResource ptr, SAM_error* err);

	SAM_EXPORT float SAM_Geothermal_GeothermalResource_rock_thermal_conductivity_get(SAM_Geothermal_GeothermalResource ptr, SAM_error* err);

	SAM_EXPORT float SAM_Geothermal_GeothermalResource_subsurface_water_loss_get(SAM_Geothermal_GeothermalResource ptr, SAM_error* err);



	/** 
	 * Create a PlantAndEquipment variable table for a GeothermalPowerNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_Geothermal_PlantAndEquipment SAM_Geothermal_PlantAndEquipment_create(const char* def, SAM_error* err);


	/**
	 * Set ambient_pressure: Ambient pressure
	 * type: numeric
	 * units: psi
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_PlantAndEquipment_ambient_pressure_set(SAM_Geothermal_PlantAndEquipment ptr, float number, SAM_error* err);

	/**
	 * Set analysis_type: Analysis Type
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: INTEGER
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_PlantAndEquipment_analysis_type_set(SAM_Geothermal_PlantAndEquipment ptr, float number, SAM_error* err);

	/**
	 * Set casing_size: Production pump casing size
	 * type: numeric
	 * units: in
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_PlantAndEquipment_casing_size_set(SAM_Geothermal_PlantAndEquipment ptr, float number, SAM_error* err);

	/**
	 * Set conversion_subtype: Conversion Subtype
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: INTEGER
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_PlantAndEquipment_conversion_subtype_set(SAM_Geothermal_PlantAndEquipment ptr, float number, SAM_error* err);

	/**
	 * Set conversion_type: Conversion Type
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: INTEGER
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_PlantAndEquipment_conversion_type_set(SAM_Geothermal_PlantAndEquipment ptr, float number, SAM_error* err);

	/**
	 * Set delta_pressure_equip: Delta pressure across surface equipment
	 * type: numeric
	 * units: psi
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_PlantAndEquipment_delta_pressure_equip_set(SAM_Geothermal_PlantAndEquipment ptr, float number, SAM_error* err);

	/**
	 * Set excess_pressure_pump: Excess pressure @ pump suction
	 * type: numeric
	 * units: psi
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_PlantAndEquipment_excess_pressure_pump_set(SAM_Geothermal_PlantAndEquipment ptr, float number, SAM_error* err);

	/**
	 * Set geotherm.egs_design_temp_autoselect: If = 1, only run UI calculations
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_PlantAndEquipment_geotherm.egs_design_temp_autoselect_set(SAM_Geothermal_PlantAndEquipment ptr, float number, SAM_error* err);

	/**
	 * Set geotherm.egs_design_temp_input: If = 1, only run UI calculations
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_PlantAndEquipment_geotherm.egs_design_temp_input_set(SAM_Geothermal_PlantAndEquipment ptr, float number, SAM_error* err);

	/**
	 * Set inj_well_diam: Injection well diameter
	 * type: numeric
	 * units: in
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_PlantAndEquipment_inj_well_diam_set(SAM_Geothermal_PlantAndEquipment ptr, float number, SAM_error* err);

	/**
	 * Set nameplate: Desired plant output
	 * type: numeric
	 * units: kW
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_PlantAndEquipment_nameplate_set(SAM_Geothermal_PlantAndEquipment ptr, float number, SAM_error* err);

	/**
	 * Set num_wells: Number of Wells
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_PlantAndEquipment_num_wells_set(SAM_Geothermal_PlantAndEquipment ptr, float number, SAM_error* err);

	/**
	 * Set plant_efficiency_input: Plant efficiency
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_PlantAndEquipment_plant_efficiency_input_set(SAM_Geothermal_PlantAndEquipment ptr, float number, SAM_error* err);

	/**
	 * Set pump_efficiency: Pump efficiency
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_PlantAndEquipment_pump_efficiency_set(SAM_Geothermal_PlantAndEquipment ptr, float number, SAM_error* err);

	/**
	 * Set specified_pump_work_amount: Pump work specified by user
	 * type: numeric
	 * units: MW
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_PlantAndEquipment_specified_pump_work_amount_set(SAM_Geothermal_PlantAndEquipment ptr, float number, SAM_error* err);

	/**
	 * Set specify_pump_work: Did user specify pump work?
	 * type: numeric
	 * units: 0 or 1
	 * options: None
	 * constraints: INTEGER
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_PlantAndEquipment_specify_pump_work_set(SAM_Geothermal_PlantAndEquipment ptr, float number, SAM_error* err);

	/**
	 * Set temp_decline_max: Maximum temperature decline
	 * type: numeric
	 * units: C
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_PlantAndEquipment_temp_decline_max_set(SAM_Geothermal_PlantAndEquipment ptr, float number, SAM_error* err);

	/**
	 * Set temp_decline_rate: Temperature decline rate
	 * type: numeric
	 * units: %/yr
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_PlantAndEquipment_temp_decline_rate_set(SAM_Geothermal_PlantAndEquipment ptr, float number, SAM_error* err);

	/**
	 * Set well_diameter: Production well diameter
	 * type: numeric
	 * units: in
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_PlantAndEquipment_well_diameter_set(SAM_Geothermal_PlantAndEquipment ptr, float number, SAM_error* err);

	/**
	 * Set well_flow_rate: Production flow rate per well
	 * type: numeric
	 * units: kg/s
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_PlantAndEquipment_well_flow_rate_set(SAM_Geothermal_PlantAndEquipment ptr, float number, SAM_error* err);

	/**
	 * Set wet_bulb_temp: Wet Bulb Temperature
	 * type: numeric
	 * units: C
	 * options: None
	 * constraints: None
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_PlantAndEquipment_wet_bulb_temp_set(SAM_Geothermal_PlantAndEquipment ptr, float number, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_Geothermal_PlantAndEquipment_ambient_pressure_get(SAM_Geothermal_PlantAndEquipment ptr, SAM_error* err);

	SAM_EXPORT float SAM_Geothermal_PlantAndEquipment_analysis_type_get(SAM_Geothermal_PlantAndEquipment ptr, SAM_error* err);

	SAM_EXPORT float SAM_Geothermal_PlantAndEquipment_casing_size_get(SAM_Geothermal_PlantAndEquipment ptr, SAM_error* err);

	SAM_EXPORT float SAM_Geothermal_PlantAndEquipment_conversion_subtype_get(SAM_Geothermal_PlantAndEquipment ptr, SAM_error* err);

	SAM_EXPORT float SAM_Geothermal_PlantAndEquipment_conversion_type_get(SAM_Geothermal_PlantAndEquipment ptr, SAM_error* err);

	SAM_EXPORT float SAM_Geothermal_PlantAndEquipment_delta_pressure_equip_get(SAM_Geothermal_PlantAndEquipment ptr, SAM_error* err);

	SAM_EXPORT float SAM_Geothermal_PlantAndEquipment_excess_pressure_pump_get(SAM_Geothermal_PlantAndEquipment ptr, SAM_error* err);

	SAM_EXPORT float SAM_Geothermal_PlantAndEquipment_geotherm.egs_design_temp_autoselect_get(SAM_Geothermal_PlantAndEquipment ptr, SAM_error* err);

	SAM_EXPORT float SAM_Geothermal_PlantAndEquipment_geotherm.egs_design_temp_input_get(SAM_Geothermal_PlantAndEquipment ptr, SAM_error* err);

	SAM_EXPORT float SAM_Geothermal_PlantAndEquipment_inj_well_diam_get(SAM_Geothermal_PlantAndEquipment ptr, SAM_error* err);

	SAM_EXPORT float SAM_Geothermal_PlantAndEquipment_nameplate_get(SAM_Geothermal_PlantAndEquipment ptr, SAM_error* err);

	SAM_EXPORT float SAM_Geothermal_PlantAndEquipment_num_wells_get(SAM_Geothermal_PlantAndEquipment ptr, SAM_error* err);

	SAM_EXPORT float SAM_Geothermal_PlantAndEquipment_plant_efficiency_input_get(SAM_Geothermal_PlantAndEquipment ptr, SAM_error* err);

	SAM_EXPORT float SAM_Geothermal_PlantAndEquipment_pump_efficiency_get(SAM_Geothermal_PlantAndEquipment ptr, SAM_error* err);

	SAM_EXPORT float SAM_Geothermal_PlantAndEquipment_specified_pump_work_amount_get(SAM_Geothermal_PlantAndEquipment ptr, SAM_error* err);

	SAM_EXPORT float SAM_Geothermal_PlantAndEquipment_specify_pump_work_get(SAM_Geothermal_PlantAndEquipment ptr, SAM_error* err);

	SAM_EXPORT float SAM_Geothermal_PlantAndEquipment_temp_decline_max_get(SAM_Geothermal_PlantAndEquipment ptr, SAM_error* err);

	SAM_EXPORT float SAM_Geothermal_PlantAndEquipment_temp_decline_rate_get(SAM_Geothermal_PlantAndEquipment ptr, SAM_error* err);

	SAM_EXPORT float SAM_Geothermal_PlantAndEquipment_well_diameter_get(SAM_Geothermal_PlantAndEquipment ptr, SAM_error* err);

	SAM_EXPORT float SAM_Geothermal_PlantAndEquipment_well_flow_rate_get(SAM_Geothermal_PlantAndEquipment ptr, SAM_error* err);

	SAM_EXPORT float SAM_Geothermal_PlantAndEquipment_wet_bulb_temp_get(SAM_Geothermal_PlantAndEquipment ptr, SAM_error* err);



	/** 
	 * Create a PowerBlock variable table for a GeothermalPowerNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_Geothermal_PowerBlock SAM_Geothermal_PowerBlock_create(const char* def, SAM_error* err);


	/**
	 * Set geothermal_analysis_period: Analysis Lifetime
	 * type: numeric
	 * units: years
	 * options: None
	 * constraints: INTEGER
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_PowerBlock_geothermal_analysis_period_set(SAM_Geothermal_PowerBlock ptr, float number, SAM_error* err);

	/**
	 * Set hr_pl_nlev: # part-load increments
	 * type: numeric
	 * units: (0-9)
	 * options: None
	 * constraints: INTEGER
	 * required if: ui_calculations_only=0
	 */
	SAM_EXPORT void SAM_Geothermal_PowerBlock_hr_pl_nlev_set(SAM_Geothermal_PowerBlock ptr, float number, SAM_error* err);

	/**
	 * Set model_choice: Which model to run (0,1,2)
	 * type: numeric
	 * units: None
	 * options: None
	 * constraints: INTEGER
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_PowerBlock_model_choice_set(SAM_Geothermal_PowerBlock ptr, float number, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_Geothermal_PowerBlock_geothermal_analysis_period_get(SAM_Geothermal_PowerBlock ptr, SAM_error* err);

	SAM_EXPORT float SAM_Geothermal_PowerBlock_hr_pl_nlev_get(SAM_Geothermal_PowerBlock ptr, SAM_error* err);

	SAM_EXPORT float SAM_Geothermal_PowerBlock_model_choice_get(SAM_Geothermal_PowerBlock ptr, SAM_error* err);



	/** 
	 * Create a Common variable table for a GeothermalPowerNone system
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */
	SAM_EXPORT SAM_Geothermal_Common SAM_Geothermal_Common_create(const char* def, SAM_error* err);


	/**
	 * Set adjust:constant: Constant loss adjustment
	 * type: numeric
	 * units: %
	 * options: None
	 * constraints: MAX=100
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_Common_adjust:constant_set(SAM_Geothermal_Common ptr, float number, SAM_error* err);

	/**
	 * Set adjust:hourly: Hourly loss adjustments
	 * type: array
	 * units: %
	 * options: None
	 * constraints: LENGTH=8760
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_Common_adjust:hourly_set(SAM_Geothermal_Common ptr, float* array, int length, SAM_error* err);

	/**
	 * Set adjust:periods: Period-based loss adjustments
	 * type: matrix
	 * units: %
	 * options: n x 3 matrix [ start, end, loss ]
	 * constraints: COLS=3
	 * required if: None
	 */
	SAM_EXPORT void SAM_Geothermal_Common_adjust:periods_set(SAM_Geothermal_Common ptr, float* matrix, int nr, int nc, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_Geothermal_Common_adjust:constant_get(SAM_Geothermal_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Geothermal_Common_adjust:hourly_get(SAM_Geothermal_Common ptr, SAM_error* err);

	SAM_EXPORT float* SAM_Geothermal_Common_adjust:periods_get(SAM_Geothermal_Common ptr, SAM_error* err);



#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif