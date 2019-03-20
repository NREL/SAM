#ifndef _CMOD_GEOTHERMAL_BUILDER_H_
#define _CMOD_GEOTHERMAL_BUILDER_H_

#ifdef LK_USE_WXWIDGETS
#include <lk/env.h>
typedef lk::invoke_t invoke_t;
#else
typedef void invoke_t;
#endif

#include "vartab.h"


//
// Evaluates T_htf_hot_ref for a Geothermal Power Block module
// @param *vt: a var_table* that contains: design_temp
// @param[in,out] *cxt: a invoke_t* that for storing the results
// @returns single value or var_table
//
float Geothermal_GeothermalPowerBlock_T_htf_hot_ref_eval(var_table* vt, invoke_t* cxt = 0)


//
// Evaluates design_temp for a Geothermal Plant and Equipment module
// @param *vt: a var_table* that contains: geotherm.egs_design_temp_autoselect, resource_temp, geotherm.egs_design_temp_input
// @param[in,out] *cxt: a invoke_t* that for storing the results
// @returns single value or var_table
//
float Geothermal_GeothermalPlantAndEquipment_design_temp_eval(var_table* vt, invoke_t* cxt = 0)


//
// Evaluates num_wells_getem, geotherm.plant_efficiency_used, gross_output, pump_depth, pump_work, pump_size_hp, geotherm.delta_pressure_reservoir, geotherm.avg_reservoir_temp, geotherm.bottom_hole_pressure for a Geothermal Plant and Equipment module
// @param *vt: a var_table* that contains: nameplate, resource_type, resource_temp, resource_depth, geothermal_analysis_period, model_choice, analysis_type, num_wells, conversion_type, plant_efficiency_input, conversion_subtype, decline_type, temp_decline_rate, temp_decline_max, wet_bulb_temp, ambient_pressure, well_flow_rate, pump_efficiency, delta_pressure_equip, excess_pressure_pump, well_diameter, casing_size, inj_well_diam, design_temp, specify_pump_work, specified_pump_work_amount, rock_thermal_conductivity, rock_specific_heat, rock_density, reservoir_pressure_change_type, reservoir_pressure_change, reservoir_width, reservoir_height, reservoir_permeability, inj_prod_well_distance, subsurface_water_loss, fracture_aperature, fracture_width, num_fractures, fracture_angle, hr_pl_nlev
// @param[in,out] *cxt: a invoke_t* that for storing the results
// @returns single value or var_table
//
var_table Geothermal_GeothermalPlantAndEquipment_num_wells_getem_MIMO_eval(var_table* vt, invoke_t* cxt = 0)


#endif