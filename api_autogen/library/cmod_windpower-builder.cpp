#include <string>
#include <vector>

#include "vartab.h"

#include "cmod_windpower-builder.h"

float Windpower_WindFarmSpecifications_system_capacity_eval(var_table* vt, invoke_t* cxt)
{
	// inputs
	float wind_farm_num_turbines = vt->lookup("wind_farm_num_turbines")->num;
	float wind_turbine_kw_rating = vt->lookup("wind_turbine_kw_rating")->num;

	// outputs
	float system_capacity;

	system_capacity = wind_farm_num_turbines * wind_turbine_kw_rating;

	if (cxt){
		cxt->result().assign("system_capacity", system_capacity);
	}

	return system_capacity;

}



var_table Windpower_WindFarmSpecifications_wind_farm_num_turbines_MIMO_eval(var_table* vt, invoke_t* cxt)
{
	// inputs
	float wind_farm_sizing_mode = vt->lookup("wind_farm_sizing_mode")->num;
	float windfarm.layout.file_or_controls = vt->lookup("windfarm.layout.file_or_controls")->num;
	util::matrix_t<ssc_number_t> wind_farm_xCoord_file = vt->lookup("wind_farm_xCoord_file")->num;
	util::matrix_t<ssc_number_t> wind_farm_yCoord_file = vt->lookup("wind_farm_yCoord_file")->num;
	float desired_farm_size = vt->lookup("desired_farm_size")->num;
	float wind_turbine_kw_rating = vt->lookup("wind_turbine_kw_rating")->num;
	float wind_turbine_rotor_diameter = vt->lookup("wind_turbine_rotor_diameter")->num;
	float windfarm.farm.shape = vt->lookup("windfarm.farm.shape")->num;
	float windfarm.farm.turbines_per_row = vt->lookup("windfarm.farm.turbines_per_row")->num;
	float windfarm.farm.number_of_rows = vt->lookup("windfarm.farm.number_of_rows")->num;
	float windfarm.farm.offset = vt->lookup("windfarm.farm.offset")->num;
	float windfarm.farm.offset_type = vt->lookup("windfarm.farm.offset_type")->num;
	float windfarm.farm.layout_angle = vt->lookup("windfarm.farm.layout_angle")->num;
	float windfarm.farm.turbine_spacing = vt->lookup("windfarm.farm.turbine_spacing")->num;
	float windfarm.farm.row_spacing = vt->lookup("windfarm.farm.row_spacing")->num;

	// outputs
	float wind_farm_num_turbines;
	util::matrix_t<ssc_number_t> wind_farm_xCoordinates;
	util::matrix_t<ssc_number_t> wind_farm_yCoordinates;
	float rows;
	float cols;

	if ( wind_farm_sizing_mode == 2.000000 && windfarm.layout.file_or_controls == 0.000000 ) {
		wind_farm_num_turbines = sizeof( wind_farm_xCoord_file )/sizeof(wind_farm_xCoord_file[0]);
		wind_farm_xCoordinates = wind_farm_xCoord_file;
		wind_farm_yCoordinates = wind_farm_yCoord_file;
		;
	
	}
	if ( wind_farm_sizing_mode != 2.000000 ) {
		float num_turbines = 0.000000;
		if ( wind_farm_sizing_mode == 0.000000 ) {
			float num_turbines = 1.000000
		}
		else {
			float num = floor( desired_farm_size / wind_turbine_kw_rating );
			if ( num <= 1.000000 ) {
				float num = 1.000000
			}
			float num_turbines = num;
		}
		wind_farm_num_turbines = num_turbines;
		float x = alloc( num_turbines );
		float y = alloc( num_turbines );
		float rows = floor( sqrt( num_turbines ) );
		float cols = num_turbines / rows;
		for ( ; rows * floor( cols ) != num_turbines;  ){
			rows -= 1.000000;
			float cols = num_turbines / rows;
		
		}
		float spacing_x = 8.000000 * wind_turbine_rotor_diameter;
		float spacing_y = 8.000000 * wind_turbine_rotor_diameter;
		x.insert(x.begin()+0, 0.000000);
		y.insert(y.begin()+0, 0.000000);
		for ( float i = 1.000000; i < num_turbines; i += 1 ){
			x.insert(x.begin()+i, i - cols * floor( i / cols ) * spacing_x);
			y.insert(y.begin()+i, floor( i / cols ) * spacing_y);
		
		}
		wind_farm_xCoordinates = x;
		wind_farm_yCoordinates = y;
		rows = rows;
		cols = cols;
		;
	
	}
	float shape = windfarm.farm.shape;
	float turbines_per_row = windfarm.farm.turbines_per_row;
	float row_count = windfarm.farm.number_of_rows;
	float row_offset = windfarm.farm.offset * wind_turbine_rotor_diameter;
	float offset_type = windfarm.farm.offset_type;
	float angle = pi(  ) * windfarm.farm.layout_angle / 180.000000;
	float points = 0.000000;
	float turbine_spacing = windfarm.farm.turbine_spacing * wind_turbine_rotor_diameter;
	float row_spacing = windfarm.farm.row_spacing * wind_turbine_rotor_diameter;
	float turbines_this_row = 0.000000;
	for ( float i = 0.000000; i < row_count; i += 1 ){
		if ( offset_type == 0.000000 ) {
			float offset = if ( mod( i, 2.000000 ) == 0.000000 ) {
				0.000000
			}
			else {
				row_offset}
		}
		else {
			float offset = row_offset * i}
		if ( shape ) {
			float turbines_this_row = turbines_per_row - i
		}
		else {
			float turbines_this_row = turbines_per_row}
		for ( float j = 0.000000; j < turbines_this_row; j += 1 ){
			std::vector<float> xCoordinates;
			xCoordinates.insert(xCoordinates.begin()+points, j * turbine_spacing + offset);
			std::vector<float> yCoordinates;
			yCoordinates.insert(yCoordinates.begin()+points, i * row_spacing);
			points += 1;
		
		}
	
	}
	for ( float k = 0.000000; k < sizeof( xCoordinates )/sizeof(xCoordinates[0]); k += 1 ){
		util::matrix_t<float> newx = xCoordinates[k] * cos( angle ) - yCoordinates[k] * sin( angle );
		util::matrix_t<float> newy = xCoordinates[k] * sin( angle ) + yCoordinates[k] * cos( angle );
		xCoordinates.insert(xCoordinates.begin()+k, newx);
		yCoordinates.insert(yCoordinates.begin()+k, newy);
	
	}
	wind_farm_xCoordinates = xCoordinates;
	wind_farm_yCoordinates = yCoordinates;
	wind_farm_num_turbines = sizeof( xCoordinates )/sizeof(xCoordinates[0]);
	;


	if (cxt){
		cxt->result().empty_hash();
		cxt->result().hash_item("wind_farm_num_turbines", wind_farm_num_turbines);
		cxt->result().hash_item("wind_farm_xCoordinates", wind_farm_xCoordinates);
		cxt->result().hash_item("wind_farm_yCoordinates", wind_farm_yCoordinates);
		cxt->result().hash_item("rows", rows);
		cxt->result().hash_item("cols", cols);
	}

	var_table vt;
	vt.assign( "wind_farm_num_turbines", wind_farm_num_turbines );
	vt.assign( "wind_farm_xCoordinates", wind_farm_xCoordinates );
	vt.assign( "wind_farm_yCoordinates", wind_farm_yCoordinates );
	vt.assign( "rows", rows );
	vt.assign( "cols", cols );

}



var_table Windpower_WindTurbineDesign_wind_turbine_powercurve_windspeeds_MIMO_eval(var_table* vt, invoke_t* cxt)
{
	// inputs
	float wind.turbine.radio_list_or_design = vt->lookup("wind.turbine.radio_list_or_design")->num;
	util::matrix_t<ssc_number_t> wind_turbine_powercurve_windspeeds_from_lib = vt->lookup("wind_turbine_powercurve_windspeeds_from_lib")->num;
	util::matrix_t<ssc_number_t> wind_turbine_powercurve_powerout_from_lib = vt->lookup("wind_turbine_powercurve_powerout_from_lib")->num;
	float wind_turbine_kw_rating_from_lib = vt->lookup("wind_turbine_kw_rating_from_lib")->num;
	float wind_turbine_kw_rating_input = vt->lookup("wind_turbine_kw_rating_input")->num;
	float wind_turbine_rotor_diameter_input = vt->lookup("wind_turbine_rotor_diameter_input")->num;
	float wind_turbine_hub_ht = vt->lookup("wind_turbine_hub_ht")->num;
	float wind.turbine.elevation = vt->lookup("wind.turbine.elevation")->num;
	float wind_resource_model_choice = vt->lookup("wind_resource_model_choice")->num;
	float wind_turbine_max_cp = vt->lookup("wind_turbine_max_cp")->num;
	float wind.turbine.max_tip_speed = vt->lookup("wind.turbine.max_tip_speed")->num;
	float wind.turbine.max_tspeed_ratio = vt->lookup("wind.turbine.max_tspeed_ratio")->num;
	float wind.turbine.region2nhalf_slope = vt->lookup("wind.turbine.region2nhalf_slope")->num;
	float wind_turbine_cutin = vt->lookup("wind_turbine_cutin")->num;
	float wind_turbine_cut_out = vt->lookup("wind_turbine_cut_out")->num;
	float wind.turbine.drive_train = vt->lookup("wind.turbine.drive_train")->num;

	// outputs
	util::matrix_t<ssc_number_t> wind_turbine_powercurve_windspeeds;
	util::matrix_t<ssc_number_t> wind_turbine_powercurve_powerout;
	float wind_turbine_rated_wind_speed;
	const char* wind_turbine_powercurve_err_msg;
	util::matrix_t<ssc_number_t> wind_turbine_powercurve_hub_efficiency;

	if ( wind.turbine.radio_list_or_design == 0.000000 ) {
		wind_turbine_powercurve_windspeeds = wind_turbine_powercurve_windspeeds_from_lib;
		wind_turbine_powercurve_powerout = wind_turbine_powercurve_powerout_from_lib;
		wind_turbine_rated_wind_speed = wind_turbine_kw_rating_from_lib;
		wind_turbine_powercurve_err_msg = ;
		throw std::runtime_error("Wind Turbine Design conditional error: wind.turbine.radio_list_or_design == 0.000000");
	
	}
	std::string errmsg = ;
	float turbine_size = wind_turbine_kw_rating_input;
	float rotor_diameter = wind_turbine_rotor_diameter_input;
	float hubht = wind_turbine_hub_ht;
	float farmElevation = wind.turbine.elevation;
	if ( wind_resource_model_choice == 1.000000 ) {
		float elevation = farmElevation + hubht
	}
	else {
		float elevation = 0.000000}
	float max_cp = wind_turbine_max_cp;
	float max_tip_speed = wind.turbine.max_tip_speed;
	float max_tip_sp_ratio = wind.turbine.max_tspeed_ratio;
	float region2_slope = wind.turbine.region2nhalf_slope;
	float cut_in = wind_turbine_cutin;
	float cut_out = wind_turbine_cut_out;
	float drive_train_type = wind.turbine.drive_train + 1.000000;
	if ( drive_train_type == 1.000000 ) {
		float a = 0.012894;
		float b = 0.085095;
		float c = 0.000000;
	
	}
	else if ( drive_train_type == 2.000000 ) {
		float a = 0.013307;
		float b = 0.036547;
		float c = 0.061067;
	
	}
	else if ( drive_train_type == 3.000000 ) {
		float a = 0.015474;
		float b = 0.044631;
		float c = 0.057898;
	
	}
	else if ( drive_train_type == 4.000000 ) {
		float a = 0.010072;
		float b = 0.019995;
		float c = 0.068990;
	
	}
	float eff = 1.000000 - a + b + c;
	float rated_hub_power = turbine_size / eff;
	float air_density = 101325.000000 * pow( 1.000000 - 0.006500 * elevation / 288.150000, 9.806650 / 0.006500 * 287.150000 ) / 287.150000 * 288.150000 - 0.006500 * elevation;
	float omega_m = max_tip_speed / rotor_diameter / 2.000000;
	float omega_0 = omega_m / 1.000000 + region2_slope / 100.000000;
	float t_sub_m = rated_hub_power * 1000.000000 / omega_m;
	float k = air_density * pi(  ) * pow( rotor_diameter, 5.000000 ) * max_cp / 64.000000 * pow( max_tip_sp_ratio, 3.000000 );
	float omegaT_a = k;
	float omegaT_b = ( t_sub_m * -1 ) / omega_m - omega_0;
	float omegaT_c = t_sub_m * omega_0 / omega_m - omega_0;
	float omegaT = ( omegaT_b / 2.000000 * omegaT_a * -1 ) - sqrt( pow( omegaT_b, 2.000000 ) - 4.000000 * omegaT_a * omegaT_c ) / 2.000000 * omegaT_a;
	float wind_at_omegaT = omegaT * rotor_diameter / 2.000000 * max_tip_sp_ratio;
	float power_at_omegaT = k * pow( omegaT, 3.000000 ) / 1000.000000;
	float rated_wind_speed = 0.330000 * pow( 2.000000 * rated_hub_power * 1000.000000 / air_density * pi(  ) * pow( rotor_diameter, 2.000000 ) / 4.000000 * max_cp, 1.000000 / 3.000000 ) + 0.670000 * 1.000000 / 1.500000 * air_density * pi(  ) * pow( rotor_diameter, 2.000000 ) * 0.250000 * max_cp * pow( wind_at_omegaT, 2.000000 ) * 1000.000000 * rated_hub_power - power_at_omegaT + wind_at_omegaT;
	if ( omegaT > omega_m ) {
		std::string errmsg = sprintf( Turbine inputs are not valid, please adjust the inputs. omegaT: %f, omegaM: %f, omegaT, omega_m )
	}
	float hub_power = 0.000000;
	float step = 0.250000;
	float array_size = 1.000000 + 40.000000 / step;
	float ws = alloc( array_size );
	float pc = alloc( array_size );
	float hub_efficiency = alloc( array_size );
	for ( float i = 0.000000; i < array_size; i += 1 ){
		ws.insert(ws.begin()+i, i * step);
		if ( ws[i] <= cut_in || ws[i] >= cut_out ) {
			float hub_power = 0.000000
		}
		else if ( ws[i] < wind_at_omegaT ) {
			float hub_power = k * pow( ws[i] * max_tip_sp_ratio / rotor_diameter / 2.000000, 3.000000 ) / 1000.000000
		}
		else if ( ws[i] <= rated_wind_speed ) {
			float hub_power = rated_hub_power - power_at_omegaT / rated_wind_speed - wind_at_omegaT * ws[i] - wind_at_omegaT + power_at_omegaT
		}
		else {
			float hub_power = rated_hub_power}
		if ( hub_power > rated_hub_power ) {
			if ( errmsg ==  ) {
				std::string errmsg = User Defined turbine power curve calculation calculated power output > rated output where wind speed =  + ws[i]
			}
			float hub_power = rated_hub_power;
		
		}
		if ( hub_power == 0.000000 ) {
			hub_efficiency.insert(hub_efficiency.begin()+i, 0.000000);
		}
		else {
			hub_efficiency.insert(hub_efficiency.begin()+i, hub_power / rated_hub_power - a + b * hub_power / rated_hub_power + c * pow( hub_power / rated_hub_power, 2.000000 ) / hub_power / rated_hub_power);}
		pc.insert(pc.begin()+i, hub_power * hub_efficiency[i]);
	
	}
	wind_turbine_powercurve_windspeeds = ws;
	wind_turbine_powercurve_powerout = pc;
	wind_turbine_powercurve_hub_efficiency = hub_efficiency;
	wind_turbine_rated_wind_speed = rated_wind_speed;
	wind_turbine_powercurve_err_msg = errmsg;
	throw;


	if (cxt){
		cxt->result().empty_hash();
		cxt->result().hash_item("wind_turbine_powercurve_windspeeds", wind_turbine_powercurve_windspeeds);
		cxt->result().hash_item("wind_turbine_powercurve_powerout", wind_turbine_powercurve_powerout);
		cxt->result().hash_item("wind_turbine_rated_wind_speed", wind_turbine_rated_wind_speed);
		cxt->result().hash_item("wind_turbine_powercurve_err_msg", wind_turbine_powercurve_err_msg);
		cxt->result().hash_item("wind_turbine_powercurve_hub_efficiency", wind_turbine_powercurve_hub_efficiency);
	}

	var_table vt;
	vt.assign( "wind_turbine_powercurve_windspeeds", wind_turbine_powercurve_windspeeds );
	vt.assign( "wind_turbine_powercurve_powerout", wind_turbine_powercurve_powerout );
	vt.assign( "wind_turbine_rated_wind_speed", wind_turbine_rated_wind_speed );
	vt.assign( "wind_turbine_powercurve_err_msg", wind_turbine_powercurve_err_msg );
	vt.assign( "wind_turbine_powercurve_hub_efficiency", wind_turbine_powercurve_hub_efficiency );

}



