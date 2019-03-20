#ifndef _CMOD_WINDPOWER_BUILDER_H_
#define _CMOD_WINDPOWER_BUILDER_H_

#ifdef LK_USE_WXWIDGETS
#include <lk/env.h>
typedef lk::invoke_t invoke_t;
#else
typedef void invoke_t;
#endif

#include "vartab.h"


//
// Evaluates system_capacity for a Wind Farm Specifications module
// @param *vt: a var_table* that contains: wind_farm_num_turbines, wind_turbine_kw_rating
// @param[in,out] *cxt: a invoke_t* that for storing the results
// @returns single value or var_table
//
float Windpower_WindFarmSpecifications_system_capacity_eval(var_table* vt, invoke_t* cxt = 0)


//
// Evaluates wind_farm_num_turbines, wind_farm_xCoordinates, wind_farm_yCoordinates, rows, cols for a Wind Farm Specifications module
// @param *vt: a var_table* that contains: wind_farm_sizing_mode, windfarm.layout.file_or_controls, wind_farm_xCoord_file, wind_farm_yCoord_file, desired_farm_size, wind_turbine_kw_rating, wind_turbine_rotor_diameter, windfarm.farm.shape, windfarm.farm.turbines_per_row, windfarm.farm.number_of_rows, windfarm.farm.offset, windfarm.farm.offset_type, windfarm.farm.layout_angle, windfarm.farm.turbine_spacing, windfarm.farm.row_spacing
// @param[in,out] *cxt: a invoke_t* that for storing the results
// @returns single value or var_table
//
var_table Windpower_WindFarmSpecifications_wind_farm_num_turbines_MIMO_eval(var_table* vt, invoke_t* cxt = 0)


//
// Evaluates wind_turbine_powercurve_windspeeds, wind_turbine_powercurve_powerout, wind_turbine_rated_wind_speed, wind_turbine_powercurve_err_msg, wind_turbine_powercurve_hub_efficiency for a Wind Turbine Design module
// @param *vt: a var_table* that contains: wind.turbine.radio_list_or_design, wind_turbine_powercurve_windspeeds_from_lib, wind_turbine_powercurve_powerout_from_lib, wind_turbine_kw_rating_from_lib, wind_turbine_kw_rating_input, wind_turbine_rotor_diameter_input, wind_turbine_hub_ht, wind.turbine.elevation, wind_resource_model_choice, wind_turbine_max_cp, wind.turbine.max_tip_speed, wind.turbine.max_tspeed_ratio, wind.turbine.region2nhalf_slope, wind_turbine_cutin, wind_turbine_cut_out, wind.turbine.drive_train
// @param[in,out] *cxt: a invoke_t* that for storing the results
// @returns single value or var_table
//
var_table Windpower_WindTurbineDesign_wind_turbine_powercurve_windspeeds_MIMO_eval(var_table* vt, invoke_t* cxt = 0)


#endif