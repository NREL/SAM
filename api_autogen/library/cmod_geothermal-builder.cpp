#include <string>
#include <vector>

#include "vartab.h"

#include "cmod_geothermal-builder.h"

float Geothermal_T_htf_hot_ref_eval(var_table* vt)
{
	// inputs
	float design_temp = vt->lookup("design_temp")->num;

	// outputs
	float T_htf_hot_ref;

	T_htf_hot_ref = design_temp;

	return T_htf_hot_ref;

}



float Geothermal_design_temp_eval(var_table* vt)
{
	// inputs
	float geotherm.egs_design_temp_autoselect = vt->lookup("geotherm.egs_design_temp_autoselect")->num;
	float resource_temp = vt->lookup("resource_temp")->num;
	float geotherm.egs_design_temp_input = vt->lookup("geotherm.egs_design_temp_input")->num;

	// outputs
	float design_temp;

	if ( geotherm.egs_design_temp_autoselect == 1.000000 ) {
		design_temp = resource_temp;
	}
	else {
		design_temp = geotherm.egs_design_temp_input;}

	return design_temp;

}



var_table Geothermal_num_wells_getem_MIMO_eval(var_table* vt)
{
	// inputs
	float nameplate = vt->lookup("nameplate")->num;
	float resource_type = vt->lookup("resource_type")->num;
	float resource_temp = vt->lookup("resource_temp")->num;
	float resource_depth = vt->lookup("resource_depth")->num;
	float geothermal_analysis_period = vt->lookup("geothermal_analysis_period")->num;
	float model_choice = vt->lookup("model_choice")->num;
	float analysis_type = vt->lookup("analysis_type")->num;
	float num_wells = vt->lookup("num_wells")->num;
	float conversion_type = vt->lookup("conversion_type")->num;
	float plant_efficiency_input = vt->lookup("plant_efficiency_input")->num;
	float conversion_subtype = vt->lookup("conversion_subtype")->num;
	float decline_type = vt->lookup("decline_type")->num;
	float temp_decline_rate = vt->lookup("temp_decline_rate")->num;
	float temp_decline_max = vt->lookup("temp_decline_max")->num;
	float wet_bulb_temp = vt->lookup("wet_bulb_temp")->num;
	float ambient_pressure = vt->lookup("ambient_pressure")->num;
	float well_flow_rate = vt->lookup("well_flow_rate")->num;
	float pump_efficiency = vt->lookup("pump_efficiency")->num;
	float delta_pressure_equip = vt->lookup("delta_pressure_equip")->num;
	float excess_pressure_pump = vt->lookup("excess_pressure_pump")->num;
	float well_diameter = vt->lookup("well_diameter")->num;
	float casing_size = vt->lookup("casing_size")->num;
	float inj_well_diam = vt->lookup("inj_well_diam")->num;
	float design_temp = vt->lookup("design_temp")->num;
	float specify_pump_work = vt->lookup("specify_pump_work")->num;
	float specified_pump_work_amount = vt->lookup("specified_pump_work_amount")->num;
	float rock_thermal_conductivity = vt->lookup("rock_thermal_conductivity")->num;
	float rock_specific_heat = vt->lookup("rock_specific_heat")->num;
	float rock_density = vt->lookup("rock_density")->num;
	float reservoir_pressure_change_type = vt->lookup("reservoir_pressure_change_type")->num;
	float reservoir_pressure_change = vt->lookup("reservoir_pressure_change")->num;
	float reservoir_width = vt->lookup("reservoir_width")->num;
	float reservoir_height = vt->lookup("reservoir_height")->num;
	float reservoir_permeability = vt->lookup("reservoir_permeability")->num;
	float inj_prod_well_distance = vt->lookup("inj_prod_well_distance")->num;
	float subsurface_water_loss = vt->lookup("subsurface_water_loss")->num;
	float fracture_aperature = vt->lookup("fracture_aperature")->num;
	float fracture_width = vt->lookup("fracture_width")->num;
	float num_fractures = vt->lookup("num_fractures")->num;
	float fracture_angle = vt->lookup("fracture_angle")->num;
	float hr_pl_nlev = vt->lookup("hr_pl_nlev")->num;

	// outputs
	float num_wells_getem;
	float geotherm.plant_efficiency_used;
	float gross_output;
	float pump_depth;
	float pump_work;
	float pump_size_hp;
	float geotherm.delta_pressure_reservoir;
	float geotherm.avg_reservoir_temp;
	float geotherm.bottom_hole_pressure;

	obj = ssc_create(  );
	ssc_var( obj, ui_calculations_only, 1.000000 );
	ssc_var( obj, system_capacity, nameplate );
	ssc_var( obj, adjust:constant, 0.000000 );
	ssc_var( obj, resource_type, resource_type );
	ssc_var( obj, resource_temp, resource_temp );
	ssc_var( obj, resource_depth, resource_depth );
	ssc_var( obj, geothermal_analysis_period, geothermal_analysis_period );
	ssc_var( obj, model_choice, model_choice );
	ssc_var( obj, nameplate, nameplate );
	ssc_var( obj, analysis_type, analysis_type );
	ssc_var( obj, num_wells, num_wells );
	ssc_var( obj, conversion_type, conversion_type );
	ssc_var( obj, plant_efficiency_input, plant_efficiency_input );
	ssc_var( obj, conversion_subtype, conversion_subtype );
	ssc_var( obj, decline_type, decline_type );
	ssc_var( obj, temp_decline_rate, temp_decline_rate );
	ssc_var( obj, temp_decline_max, temp_decline_max );
	ssc_var( obj, wet_bulb_temp, wet_bulb_temp );
	ssc_var( obj, ambient_pressure, ambient_pressure );
	ssc_var( obj, well_flow_rate, well_flow_rate );
	ssc_var( obj, pump_efficiency, pump_efficiency );
	ssc_var( obj, delta_pressure_equip, delta_pressure_equip );
	ssc_var( obj, excess_pressure_pump, excess_pressure_pump );
	ssc_var( obj, well_diameter, well_diameter );
	ssc_var( obj, casing_size, casing_size );
	ssc_var( obj, inj_well_diam, inj_well_diam );
	ssc_var( obj, design_temp, design_temp );
	ssc_var( obj, specify_pump_work, specify_pump_work );
	ssc_var( obj, specified_pump_work_amount, specified_pump_work_amount );
	ssc_var( obj, rock_thermal_conductivity, rock_thermal_conductivity );
	ssc_var( obj, rock_specific_heat, rock_specific_heat );
	ssc_var( obj, rock_density, rock_density );
	ssc_var( obj, reservoir_pressure_change_type, reservoir_pressure_change_type );
	ssc_var( obj, reservoir_pressure_change, reservoir_pressure_change );
	ssc_var( obj, reservoir_width, reservoir_width );
	ssc_var( obj, reservoir_height, reservoir_height );
	ssc_var( obj, reservoir_permeability, reservoir_permeability );
	ssc_var( obj, inj_prod_well_distance, inj_prod_well_distance );
	ssc_var( obj, subsurface_water_loss, subsurface_water_loss );
	ssc_var( obj, fracture_aperature, fracture_aperature );
	ssc_var( obj, fracture_width, fracture_width );
	ssc_var( obj, num_fractures, num_fractures );
	ssc_var( obj, fracture_angle, fracture_angle );
	ssc_var( obj, hr_pl_nlev, hr_pl_nlev );
	std::string result = ssc_exec( obj, geothermal );
	if ( string != number && result != 0.000000 ) {
		msgbox( geothermal mimo error, result = + result )
	}
	num_wells_getem = ssc_var( obj, num_wells_getem_output );
	geotherm.plant_efficiency_used = ssc_var( obj, plant_brine_eff );
	gross_output = ssc_var( obj, gross_output );
	pump_depth = ssc_var( obj, pump_depth_ft );
	if ( specify_pump_work == 0.000000 ) {
		pump_work = ssc_var( obj, pump_work )
	}
	else {
		pump_work = specified_pump_work_amount}
	pump_size_hp = ssc_var( obj, pump_hp );
	geotherm.delta_pressure_reservoir = ssc_var( obj, reservoir_pressure );
	geotherm.avg_reservoir_temp = ssc_var( obj, reservoir_avg_temp );
	geotherm.bottom_hole_pressure = ssc_var( obj, bottom_hole_pressure );
	ssc_free( obj );
	std::string = 0.000000;


	var_table vt;
	vt.assign( "num_wells_getem", num_wells_getem );
	vt.assign( "geotherm.plant_efficiency_used", geotherm.plant_efficiency_used );
	vt.assign( "gross_output", gross_output );
	vt.assign( "pump_depth", pump_depth );
	vt.assign( "pump_work", pump_work );
	vt.assign( "pump_size_hp", pump_size_hp );
	vt.assign( "geotherm.delta_pressure_reservoir", geotherm.delta_pressure_reservoir );
	vt.assign( "geotherm.avg_reservoir_temp", geotherm.avg_reservoir_temp );
	vt.assign( "geotherm.bottom_hole_pressure", geotherm.bottom_hole_pressure );

}



