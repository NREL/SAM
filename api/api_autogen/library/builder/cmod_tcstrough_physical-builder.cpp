#include <string>
#include <vector>

#include "vartab.h"

#include "cmod_tcstrough_physical-builder.h"

float TcstroughPhysical_system_capacity_eval(var_table* vt)
{
	// inputs
	float csp.dtr.pwrb.nameplate = vt->lookup("csp.dtr.pwrb.nameplate")->num;

	// outputs
	float system_capacity;

	system_capacity = csp.dtr.pwrb.nameplate * 1000.000000;

	return system_capacity;

}



float TcstroughPhysical_total_loop_conversion_efficiency_eval(var_table* vt)
{
	// inputs
	float loop_optical_efficiency = vt->lookup("loop_optical_efficiency")->num;
	float cspdtr_loop_hce_heat_loss = vt->lookup("cspdtr_loop_hce_heat_loss")->num;

	// outputs
	float total_loop_conversion_efficiency;

	total_loop_conversion_efficiency = loop_optical_efficiency * cspdtr_loop_hce_heat_loss;

	return total_loop_conversion_efficiency;

}



var_table TcstroughPhysical_W_aperture_MIMO_eval(var_table* vt)
{
	// inputs
	float csp_dtr_sca_w_profile_1 = vt->lookup("csp_dtr_sca_w_profile_1")->num;
	float csp_dtr_sca_w_profile_2 = vt->lookup("csp_dtr_sca_w_profile_2")->num;
	float csp_dtr_sca_w_profile_3 = vt->lookup("csp_dtr_sca_w_profile_3")->num;
	float csp_dtr_sca_w_profile_4 = vt->lookup("csp_dtr_sca_w_profile_4")->num;
	util::matrix_t<ssc_number_t> arr_collectors_in_loop = vt->lookup("arr_collectors_in_loop")->num;
	float csp_dtr_sca_aperture_1 = vt->lookup("csp_dtr_sca_aperture_1")->num;
	float csp_dtr_sca_aperture_2 = vt->lookup("csp_dtr_sca_aperture_2")->num;
	float csp_dtr_sca_aperture_3 = vt->lookup("csp_dtr_sca_aperture_3")->num;
	float csp_dtr_sca_aperture_4 = vt->lookup("csp_dtr_sca_aperture_4")->num;
	float csp_dtr_sca_tracking_error_1 = vt->lookup("csp_dtr_sca_tracking_error_1")->num;
	float csp_dtr_sca_tracking_error_2 = vt->lookup("csp_dtr_sca_tracking_error_2")->num;
	float csp_dtr_sca_tracking_error_3 = vt->lookup("csp_dtr_sca_tracking_error_3")->num;
	float csp_dtr_sca_tracking_error_4 = vt->lookup("csp_dtr_sca_tracking_error_4")->num;
	float csp_dtr_sca_geometry_effects_1 = vt->lookup("csp_dtr_sca_geometry_effects_1")->num;
	float csp_dtr_sca_geometry_effects_2 = vt->lookup("csp_dtr_sca_geometry_effects_2")->num;
	float csp_dtr_sca_geometry_effects_3 = vt->lookup("csp_dtr_sca_geometry_effects_3")->num;
	float csp_dtr_sca_geometry_effects_4 = vt->lookup("csp_dtr_sca_geometry_effects_4")->num;
	float csp_dtr_sca_clean_reflectivity_1 = vt->lookup("csp_dtr_sca_clean_reflectivity_1")->num;
	float csp_dtr_sca_clean_reflectivity_2 = vt->lookup("csp_dtr_sca_clean_reflectivity_2")->num;
	float csp_dtr_sca_clean_reflectivity_3 = vt->lookup("csp_dtr_sca_clean_reflectivity_3")->num;
	float csp_dtr_sca_clean_reflectivity_4 = vt->lookup("csp_dtr_sca_clean_reflectivity_4")->num;
	float csp_dtr_sca_mirror_dirt_1 = vt->lookup("csp_dtr_sca_mirror_dirt_1")->num;
	float csp_dtr_sca_mirror_dirt_2 = vt->lookup("csp_dtr_sca_mirror_dirt_2")->num;
	float csp_dtr_sca_mirror_dirt_3 = vt->lookup("csp_dtr_sca_mirror_dirt_3")->num;
	float csp_dtr_sca_mirror_dirt_4 = vt->lookup("csp_dtr_sca_mirror_dirt_4")->num;
	float csp_dtr_sca_general_error_1 = vt->lookup("csp_dtr_sca_general_error_1")->num;
	float csp_dtr_sca_general_error_2 = vt->lookup("csp_dtr_sca_general_error_2")->num;
	float csp_dtr_sca_general_error_3 = vt->lookup("csp_dtr_sca_general_error_3")->num;
	float csp_dtr_sca_general_error_4 = vt->lookup("csp_dtr_sca_general_error_4")->num;
	float csp_dtr_sca_ave_focal_len_1 = vt->lookup("csp_dtr_sca_ave_focal_len_1")->num;
	float csp_dtr_sca_ave_focal_len_2 = vt->lookup("csp_dtr_sca_ave_focal_len_2")->num;
	float csp_dtr_sca_ave_focal_len_3 = vt->lookup("csp_dtr_sca_ave_focal_len_3")->num;
	float csp_dtr_sca_ave_focal_len_4 = vt->lookup("csp_dtr_sca_ave_focal_len_4")->num;
	float csp_dtr_sca_length_1 = vt->lookup("csp_dtr_sca_length_1")->num;
	float csp_dtr_sca_length_2 = vt->lookup("csp_dtr_sca_length_2")->num;
	float csp_dtr_sca_length_3 = vt->lookup("csp_dtr_sca_length_3")->num;
	float csp_dtr_sca_length_4 = vt->lookup("csp_dtr_sca_length_4")->num;
	float csp_dtr_sca_ap_length_1 = vt->lookup("csp_dtr_sca_ap_length_1")->num;
	float csp_dtr_sca_ap_length_2 = vt->lookup("csp_dtr_sca_ap_length_2")->num;
	float csp_dtr_sca_ap_length_3 = vt->lookup("csp_dtr_sca_ap_length_3")->num;
	float csp_dtr_sca_ap_length_4 = vt->lookup("csp_dtr_sca_ap_length_4")->num;
	float csp_dtr_sca_ncol_per_sca_1 = vt->lookup("csp_dtr_sca_ncol_per_sca_1")->num;
	float csp_dtr_sca_ncol_per_sca_2 = vt->lookup("csp_dtr_sca_ncol_per_sca_2")->num;
	float csp_dtr_sca_ncol_per_sca_3 = vt->lookup("csp_dtr_sca_ncol_per_sca_3")->num;
	float csp_dtr_sca_ncol_per_sca_4 = vt->lookup("csp_dtr_sca_ncol_per_sca_4")->num;
	float csp_dtr_sca_piping_dist_1 = vt->lookup("csp_dtr_sca_piping_dist_1")->num;
	float csp_dtr_sca_piping_dist_2 = vt->lookup("csp_dtr_sca_piping_dist_2")->num;
	float csp_dtr_sca_piping_dist_3 = vt->lookup("csp_dtr_sca_piping_dist_3")->num;
	float csp_dtr_sca_piping_dist_4 = vt->lookup("csp_dtr_sca_piping_dist_4")->num;

	// outputs
	util::matrix_t<ssc_number_t> W_aperture;
	float max_collector_width;
	util::matrix_t<ssc_number_t> A_aperture;
	util::matrix_t<ssc_number_t> TrackingError;
	util::matrix_t<ssc_number_t> GeomEffects;
	util::matrix_t<ssc_number_t> Rho_mirror_clean;
	util::matrix_t<ssc_number_t> Dirt_mirror;
	util::matrix_t<ssc_number_t> Error;
	util::matrix_t<ssc_number_t> Ave_Focal_Length;
	util::matrix_t<ssc_number_t> L_SCA;
	util::matrix_t<ssc_number_t> L_aperture;
	util::matrix_t<ssc_number_t> ColperSCA;
	util::matrix_t<ssc_number_t> Distance_SCA;

	std::vector<float> acollector;
	acollector.insert(acollector.begin()+0, csp_dtr_sca_w_profile_1);
	acollector.insert(acollector.begin()+1, csp_dtr_sca_w_profile_2);
	acollector.insert(acollector.begin()+2, csp_dtr_sca_w_profile_3);
	acollector.insert(acollector.begin()+3, csp_dtr_sca_w_profile_4);
	W_aperture = acollector;
	util::matrix_t<float> arr = arr_collectors_in_loop;
	util::matrix_t<float> n = arr.size();
	for ( float i = 0.000000; i < n; i += 1 ){
		std::vector<util::matrix_t<float>> widths;
		widths.insert(widths.begin()+i, acollector[arr[i]]);
	}
	max_collector_width = max( widths );
	acollector.insert(acollector.begin()+0, csp_dtr_sca_aperture_1);
	acollector.insert(acollector.begin()+1, csp_dtr_sca_aperture_2);
	acollector.insert(acollector.begin()+2, csp_dtr_sca_aperture_3);
	acollector.insert(acollector.begin()+3, csp_dtr_sca_aperture_4);
	A_aperture = acollector;
	acollector.insert(acollector.begin()+0, csp_dtr_sca_tracking_error_1);
	acollector.insert(acollector.begin()+1, csp_dtr_sca_tracking_error_2);
	acollector.insert(acollector.begin()+2, csp_dtr_sca_tracking_error_3);
	acollector.insert(acollector.begin()+3, csp_dtr_sca_tracking_error_4);
	TrackingError = acollector;
	acollector.insert(acollector.begin()+0, csp_dtr_sca_geometry_effects_1);
	acollector.insert(acollector.begin()+1, csp_dtr_sca_geometry_effects_2);
	acollector.insert(acollector.begin()+2, csp_dtr_sca_geometry_effects_3);
	acollector.insert(acollector.begin()+3, csp_dtr_sca_geometry_effects_4);
	GeomEffects = acollector;
	acollector.insert(acollector.begin()+0, csp_dtr_sca_clean_reflectivity_1);
	acollector.insert(acollector.begin()+1, csp_dtr_sca_clean_reflectivity_2);
	acollector.insert(acollector.begin()+2, csp_dtr_sca_clean_reflectivity_3);
	acollector.insert(acollector.begin()+3, csp_dtr_sca_clean_reflectivity_4);
	Rho_mirror_clean = acollector;
	acollector.insert(acollector.begin()+0, csp_dtr_sca_mirror_dirt_1);
	acollector.insert(acollector.begin()+1, csp_dtr_sca_mirror_dirt_2);
	acollector.insert(acollector.begin()+2, csp_dtr_sca_mirror_dirt_3);
	acollector.insert(acollector.begin()+3, csp_dtr_sca_mirror_dirt_4);
	Dirt_mirror = acollector;
	acollector.insert(acollector.begin()+0, csp_dtr_sca_general_error_1);
	acollector.insert(acollector.begin()+1, csp_dtr_sca_general_error_2);
	acollector.insert(acollector.begin()+2, csp_dtr_sca_general_error_3);
	acollector.insert(acollector.begin()+3, csp_dtr_sca_general_error_4);
	Error = acollector;
	acollector.insert(acollector.begin()+0, csp_dtr_sca_ave_focal_len_1);
	acollector.insert(acollector.begin()+1, csp_dtr_sca_ave_focal_len_2);
	acollector.insert(acollector.begin()+2, csp_dtr_sca_ave_focal_len_3);
	acollector.insert(acollector.begin()+3, csp_dtr_sca_ave_focal_len_4);
	Ave_Focal_Length = acollector;
	acollector.insert(acollector.begin()+0, csp_dtr_sca_length_1);
	acollector.insert(acollector.begin()+1, csp_dtr_sca_length_2);
	acollector.insert(acollector.begin()+2, csp_dtr_sca_length_3);
	acollector.insert(acollector.begin()+3, csp_dtr_sca_length_4);
	L_SCA = acollector;
	acollector.insert(acollector.begin()+0, csp_dtr_sca_ap_length_1);
	acollector.insert(acollector.begin()+1, csp_dtr_sca_ap_length_2);
	acollector.insert(acollector.begin()+2, csp_dtr_sca_ap_length_3);
	acollector.insert(acollector.begin()+3, csp_dtr_sca_ap_length_4);
	L_aperture = acollector;
	acollector.insert(acollector.begin()+0, csp_dtr_sca_ncol_per_sca_1);
	acollector.insert(acollector.begin()+1, csp_dtr_sca_ncol_per_sca_2);
	acollector.insert(acollector.begin()+2, csp_dtr_sca_ncol_per_sca_3);
	acollector.insert(acollector.begin()+3, csp_dtr_sca_ncol_per_sca_4);
	ColperSCA = acollector;
	acollector.insert(acollector.begin()+0, csp_dtr_sca_piping_dist_1);
	acollector.insert(acollector.begin()+1, csp_dtr_sca_piping_dist_2);
	acollector.insert(acollector.begin()+2, csp_dtr_sca_piping_dist_3);
	acollector.insert(acollector.begin()+3, csp_dtr_sca_piping_dist_4);
	Distance_SCA = acollector;


	var_table vt;
	vt.assign( "W_aperture", W_aperture );
	vt.assign( "max_collector_width", max_collector_width );
	vt.assign( "A_aperture", A_aperture );
	vt.assign( "TrackingError", TrackingError );
	vt.assign( "GeomEffects", GeomEffects );
	vt.assign( "Rho_mirror_clean", Rho_mirror_clean );
	vt.assign( "Dirt_mirror", Dirt_mirror );
	vt.assign( "Error", Error );
	vt.assign( "Ave_Focal_Length", Ave_Focal_Length );
	vt.assign( "L_SCA", L_SCA );
	vt.assign( "L_aperture", L_aperture );
	vt.assign( "ColperSCA", ColperSCA );
	vt.assign( "Distance_SCA", Distance_SCA );

}



var_table TcstroughPhysical_W_pb_design_MIMO_eval(var_table* vt)
{
	// inputs
	float P_ref = vt->lookup("P_ref")->num;
	float eta_ref = vt->lookup("eta_ref")->num;

	// outputs
	float W_pb_design;
	float q_pb_design;
	float q_max_aux;

	float gross = P_ref;
	W_pb_design = gross;
	float qdesign = if ( eta_ref != 0.000000 ) {
		gross / eta_ref
	}
	else {
		gross}
	q_pb_design = qdesign;
	q_max_aux = qdesign;


	var_table vt;
	vt.assign( "W_pb_design", W_pb_design );
	vt.assign( "q_pb_design", q_pb_design );
	vt.assign( "q_max_aux", q_max_aux );

}



var_table TcstroughPhysical_PhysicalTroughSolarField_PhysicalTroughSolarField_func(var_table* vt, invoke_t* cxt)
{
	// inputs
	util::matrix_t<ssc_number_t> field_fl_props = vt->lookup("field_fl_props")->num;
	undefined obj
	undefined HTF_code1
	undefined fl_props1
	undefined HTF_code2
	undefined fl_props2
	float store_fluid = vt->lookup("store_fluid")->num;
	float Fluid = vt->lookup("Fluid")->num;
	util::matrix_t<ssc_number_t> store_fl_props = vt->lookup("store_fl_props")->num;

	// outputs
	float is_hx;

float bUserDefined = value( combo_htf_type ) == 9.000000;
enable( field_fl_props, bUserDefined );
enable( field_htf_min_operating_temp, !bUserDefined );
enable( field_htf_max_operating_temp, !bUserDefined );
if ( combo_htf_type < 9.000000 ) {
	float fluid_dens_outlet_temp = substance_density( Fluid, T_loop_out )
}
else {
	function fluid_dens_outlet_temp = cspdtr_solf_userhtf_densT_loop_out
	 = cspdtr_solf_userhtf2.000000
	temperatureC
	std::string aUserDefHTF = value( field_fl_props );
	std::string rows = sizeof( aUserDefHTF );
	float cols = aUserDefHTF[0.000000].size();
	if ( cols != 7.000000 || rows < 2.000000 || col < 0.000000 || col >= 7.000000 ) {
		 = 1.000000;
	}
	for ( float i = 1.000000; i < rows; i += 1 ){
		if ( aUserDefHTF[i][0.000000] >= temperatureC ) {
			float xmin = aUserDefHTF[i - 1.000000][0.000000];
			float xmax = aUserDefHTF[i][0.000000];
			float ymin = aUserDefHTF[i - 1.000000][col];
			std::string ymax = aUserDefHTF[i][col];
			if ( xmax <= xmin ) {
				float = 1.000000;
			}
			float percent = temperatureC - xmin / xmax - xmin;
			float = ymin + percent * ymax - ymin;
		
		}
	}
	float = aUserDefHTF[rows - 1.000000][col];
	;}
value( fluid_dens_outlet_temp, fluid_dens_outlet_temp );
if ( combo_htf_type < 9.000000 ) {
	float fluid_dens_inlet_temp = substance_density( Fluid, T_loop_in_des )
}
else {
	function fluid_dens_inlet_temp = cspdtr_solf_userhtf_densT_loop_in_des
	 = cspdtr_solf_userhtf2.000000
	temperatureC
	std::string aUserDefHTF = value( field_fl_props );
	std::string rows = sizeof( aUserDefHTF );
	float cols = aUserDefHTF[0.000000].size();
	if ( cols != 7.000000 || rows < 2.000000 || col < 0.000000 || col >= 7.000000 ) {
		 = 1.000000;
	}
	for ( float i = 1.000000; i < rows; i += 1 ){
		if ( aUserDefHTF[i][0.000000] >= temperatureC ) {
			float xmin = aUserDefHTF[i - 1.000000][0.000000];
			float xmax = aUserDefHTF[i][0.000000];
			float ymin = aUserDefHTF[i - 1.000000][col];
			std::string ymax = aUserDefHTF[i][col];
			if ( xmax <= xmin ) {
				float = 1.000000;
			}
			float percent = temperatureC - xmin / xmax - xmin;
			float = ymin + percent * ymax - ymin;
		
		}
	}
	float = aUserDefHTF[rows - 1.000000][col];
	;}
value( fluid_dens_inlet_temp, fluid_dens_inlet_temp );
float field_htf_max_operating_temp = switch_combo_htf_type();
value( field_htf_max_operating_temp, field_htf_max_operating_temp );
float field_htf_min_operating_temp = switch_combo_htf_type();
value( field_htf_min_operating_temp, field_htf_min_operating_temp );
hx_derate_update_moduleobj = ssc_create(  );
ssc_var( obj, HTF_code1, store_fluid );
ssc_var( obj, fl_props1, store_fl_props );
ssc_var( obj, HTF_code2, Fluid );
ssc_var( obj, fl_props2, field_fl_props );
std::string ret = ssc_exec( obj, user_htf_comparison, {"show_dialog": "0.000000", "hold_dialog": "0.000000", } );
std::string are_equal = ssc_var( obj, are_equal );
float is_hx = !are_equal == 1.000000;
enable( dt_hot, is_hx );
enable( dt_cold, is_hx );
value( is_hx, is_hx );
ssc_free( obj );
;
;
util::matrix_t<float> aLoopCtl = trough_loop_control;
if ( aLoopCtl.size() != aLoopCtl[0.000000] * 3.000000 + 1.000000 ) {
	msgbox( Array from loop control has inconsistent values. (#values != array[0]*3+1) )
}
value( nSCA, aLoopCtl[0.000000] );
;
std::string state = value( radio_sm_or_area );
enable( specified_solar_multiple, state == 0.000000 );
enable( specified_total_aperture, state == 1.000000 );
;




var_table TcstroughPhysical_collectors_in_field_MIMO_eval(var_table* vt)
{
	// inputs
	util::matrix_t<ssc_number_t> SCAInfoArray = vt->lookup("SCAInfoArray")->num;
	float nColt = vt->lookup("nColt")->num;

	// outputs
	const char* collectors_in_field;
	util::matrix_t<ssc_number_t> arr_collectors_in_loop;

	util::matrix_t<float> arr = SCAInfoArray;
	float n = nColt;
	std::string str = Cold - ;
	for ( float i = 0.000000; i < arr.size(); i += 1 ){
		for ( float j = 0.000000; j < n; j += 1 ){
			if ( arr[i][0.000000] == j + 1.000000 ) {
				std::vector<float> c;
				c.insert(c.begin()+i, j);
				std::vector<float> s;
				s.insert(s.begin()+j, to_string( j + 1.000000 ));
				std::string str = str + s[j] +  - ;
			
			}
		}
	}
	collectors_in_field = str + Hot;
	arr_collectors_in_loop = c;


	var_table vt;
	vt.assign( "collectors_in_field", collectors_in_field );
	vt.assign( "arr_collectors_in_loop", arr_collectors_in_loop );

}



float TcstroughPhysical_PhysicalTroughSolarField_PhysicalTroughSolarField_func(var_table* vt, invoke_t* cxt)
{
	// inputs
	float is_hx = vt->lookup("is_hx")->num;

	// outputs
	float is_hx;

float bUserDefined = value( combo_htf_type ) == 9.000000;
enable( field_fl_props, bUserDefined );
enable( field_htf_min_operating_temp, !bUserDefined );
enable( field_htf_max_operating_temp, !bUserDefined );
if ( combo_htf_type < 9.000000 ) {
	float fluid_dens_outlet_temp = substance_density( Fluid, T_loop_out )
}
else {
	function fluid_dens_outlet_temp = cspdtr_solf_userhtf_densT_loop_out
	 = cspdtr_solf_userhtf2.000000
	temperatureC
	std::string aUserDefHTF = value( field_fl_props );
	std::string rows = sizeof( aUserDefHTF );
	float cols = aUserDefHTF[0.000000].size();
	if ( cols != 7.000000 || rows < 2.000000 || col < 0.000000 || col >= 7.000000 ) {
		 = 1.000000;
	}
	for ( float i = 1.000000; i < rows; i += 1 ){
		if ( aUserDefHTF[i][0.000000] >= temperatureC ) {
			float xmin = aUserDefHTF[i - 1.000000][0.000000];
			float xmax = aUserDefHTF[i][0.000000];
			float ymin = aUserDefHTF[i - 1.000000][col];
			std::string ymax = aUserDefHTF[i][col];
			if ( xmax <= xmin ) {
				float = 1.000000;
			}
			float percent = temperatureC - xmin / xmax - xmin;
			float = ymin + percent * ymax - ymin;
		
		}
	}
	float = aUserDefHTF[rows - 1.000000][col];
	;}
value( fluid_dens_outlet_temp, fluid_dens_outlet_temp );
if ( combo_htf_type < 9.000000 ) {
	float fluid_dens_inlet_temp = substance_density( Fluid, T_loop_in_des )
}
else {
	function fluid_dens_inlet_temp = cspdtr_solf_userhtf_densT_loop_in_des
	 = cspdtr_solf_userhtf2.000000
	temperatureC
	std::string aUserDefHTF = value( field_fl_props );
	std::string rows = sizeof( aUserDefHTF );
	float cols = aUserDefHTF[0.000000].size();
	if ( cols != 7.000000 || rows < 2.000000 || col < 0.000000 || col >= 7.000000 ) {
		 = 1.000000;
	}
	for ( float i = 1.000000; i < rows; i += 1 ){
		if ( aUserDefHTF[i][0.000000] >= temperatureC ) {
			float xmin = aUserDefHTF[i - 1.000000][0.000000];
			float xmax = aUserDefHTF[i][0.000000];
			float ymin = aUserDefHTF[i - 1.000000][col];
			std::string ymax = aUserDefHTF[i][col];
			if ( xmax <= xmin ) {
				float = 1.000000;
			}
			float percent = temperatureC - xmin / xmax - xmin;
			float = ymin + percent * ymax - ymin;
		
		}
	}
	float = aUserDefHTF[rows - 1.000000][col];
	;}
value( fluid_dens_inlet_temp, fluid_dens_inlet_temp );
float field_htf_max_operating_temp = switch_combo_htf_type();
value( field_htf_max_operating_temp, field_htf_max_operating_temp );
float field_htf_min_operating_temp = switch_combo_htf_type();
value( field_htf_min_operating_temp, field_htf_min_operating_temp );
hx_derate_update_moduleobj = ssc_create(  );
ssc_var( obj, HTF_code1, store_fluid );
ssc_var( obj, fl_props1, store_fl_props );
ssc_var( obj, HTF_code2, Fluid );
ssc_var( obj, fl_props2, field_fl_props );
std::string ret = ssc_exec( obj, user_htf_comparison, {"show_dialog": "0.000000", "hold_dialog": "0.000000", } );
std::string are_equal = ssc_var( obj, are_equal );
float is_hx = !are_equal == 1.000000;
enable( dt_hot, is_hx );
enable( dt_cold, is_hx );
value( is_hx, is_hx );
ssc_free( obj );
;
;
util::matrix_t<float> aLoopCtl = trough_loop_control;
if ( aLoopCtl.size() != aLoopCtl[0.000000] * 3.000000 + 1.000000 ) {
	msgbox( Array from loop control has inconsistent values. (#values != array[0]*3+1) )
}
value( nSCA, aLoopCtl[0.000000] );
;
std::string state = value( radio_sm_or_area );
enable( specified_solar_multiple, state == 0.000000 );
enable( specified_total_aperture, state == 1.000000 );
;




float TcstroughPhysical_csp.dtr.tes.hx_derate_eval(var_table* vt)
{
	// inputs
	float is_hx = vt->lookup("is_hx")->num;
	float dt_hot = vt->lookup("dt_hot")->num;
	float dt_cold = vt->lookup("dt_cold")->num;
	float T_loop_out = vt->lookup("T_loop_out")->num;
	float T_loop_in_des = vt->lookup("T_loop_in_des")->num;

	// outputs
	float csp.dtr.tes.hx_derate;

	if ( is_hx != 1.000000 ) {
		csp.dtr.tes.hx_derate = 1.000000;
	}
	else {
		float dt_ave = 0.500000 * dt_hot + dt_cold;
		float T_htf_ave = 0.500000 * T_loop_out + T_loop_in_des;
		float T_cold_baseline = 50.000000;
		csp.dtr.tes.hx_derate = T_htf_ave - 2.000000 * dt_ave - T_cold_baseline / T_htf_ave - T_cold_baseline;
	}

	return csp.dtr.tes.hx_derate;

}



