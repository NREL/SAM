#include <string>
#include <vector>

#include "vartab.h"

#include "cmod_tcsMSLF-builder.h"

float TcsMSLF_MoltenSaltLinearFresnelStorage_MoltenSaltLinearFresnelStorage_func(var_table* vt, invoke_t* cxt)
{
	// inputs
	float is_hx = vt->lookup("is_hx")->num;

	// outputs
	float mslf_is_hx;

mslf_hx_derate_updateobj = ssc_create(  );
ssc_var( obj, HTF_code1, store_fluid );
ssc_var( obj, fl_props1, store_fl_props );
ssc_var( obj, HTF_code2, Fluid );
ssc_var( obj, fl_props2, field_fl_props );
std::string ret = ssc_exec( obj, user_htf_comparison, {"show_dialog": "0.000000", "hold_dialog": "0.000000", } );
std::string are_equal = ssc_var( obj, are_equal );
float is_hx = !are_equal == 1.000000;
enable( dt_hot, is_hx );
enable( dt_cold, is_hx );
value( mslf_is_hx, is_hx );
ssc_free( obj );
;
float bCustomHTF = value( csp.mslf.control.store_fluid ) == 9.000000;
enable( store_fl_props, bCustomHTF );
enable( csp.mslf.tes.htf_min_opt_temp, !bCustomHTF );
enable( csp.mslf.tes.htf_max_opt_temp, !bCustomHTF );




float TcsMSLF_sm1_aperture_eval(var_table* vt)
{
	// inputs
	float sf_q_design = vt->lookup("sf_q_design")->num;
	float I_bn_des = vt->lookup("I_bn_des")->num;
	float loop_eff = vt->lookup("loop_eff")->num;

	// outputs
	float sm1_aperture;

	sm1_aperture = sf_q_design / I_bn_des * loop_eff * 1000000.000000;

	return sm1_aperture;

}



float TcsMSLF_system_capacity_eval(var_table* vt)
{
	// inputs
	float nameplate = vt->lookup("nameplate")->num;

	// outputs
	float system_capacity;

	system_capacity = nameplate * 1000.000000;

	return system_capacity;

}



var_table TcsMSLF_MoltenSaltLinearFresnelStorage_MoltenSaltLinearFresnelStorage_func(var_table* vt, invoke_t* cxt)
{
	// inputs
	util::matrix_t<ssc_number_t> store_fl_props = vt->lookup("store_fl_props")->num;
	undefined HTF_code1
	undefined fl_props1
	undefined HTF_code2
	undefined fl_props2
	float Fluid = vt->lookup("Fluid")->num;
	util::matrix_t<ssc_number_t> field_fl_props = vt->lookup("field_fl_props")->num;
	undefined obj
	float store_fluid = vt->lookup("store_fluid")->num;

	// outputs
	float is_hx;

mslf_hx_derate_updateobj = ssc_create(  );
ssc_var( obj, HTF_code1, store_fluid );
ssc_var( obj, fl_props1, store_fl_props );
ssc_var( obj, HTF_code2, Fluid );
ssc_var( obj, fl_props2, field_fl_props );
std::string ret = ssc_exec( obj, user_htf_comparison, {"show_dialog": "0.000000", "hold_dialog": "0.000000", } );
std::string are_equal = ssc_var( obj, are_equal );
float is_hx = !are_equal == 1.000000;
enable( dt_hot, is_hx );
enable( dt_cold, is_hx );
value( mslf_is_hx, is_hx );
ssc_free( obj );
;
float bCustomHTF = value( csp.mslf.control.store_fluid ) == 9.000000;
enable( store_fl_props, bCustomHTF );
enable( csp.mslf.tes.htf_min_opt_temp, !bCustomHTF );
enable( csp.mslf.tes.htf_max_opt_temp, !bCustomHTF );




float TcsMSLF_hl_derate_eval(var_table* vt)
{
	// inputs
	float hl_des = vt->lookup("hl_des")->num;
	float I_bn_des = vt->lookup("I_bn_des")->num;
	float A_aperture = vt->lookup("A_aperture")->num;
	float L_mod = vt->lookup("L_mod")->num;

	// outputs
	float hl_derate;

	hl_derate = 1.000000 - hl_des / I_bn_des * A_aperture / L_mod;

	return hl_derate;

}



float TcsMSLF_csp.mslf.control.tes_dens_eval(var_table* vt)
{
	// inputs
	float csp.mslf.control.store_fluid = vt->lookup("csp.mslf.control.store_fluid")->num;
	float tes_temp = vt->lookup("tes_temp")->num;
	util::matrix_t<ssc_number_t> store_fl_props = vt->lookup("store_fl_props")->num;

	// outputs
	float csp.mslf.control.tes_dens;

	auto switch_csp.mslf.control.store_fluid = [&]{
		float switch_result;
		switch( csp.mslf.control.store_fluid ){
			case 0:
				switch_result = substance_density( 18.000000, tes_temp );
				break;
			case 1:
				switch_result = substance_density( 19.000000, tes_temp );
				break;
			case 2:
				switch_result = substance_density( 20.000000, tes_temp );
				break;
			case 3:
				switch_result = substance_density( 21.000000, tes_temp );
				break;
			case 4:
				switch_result = substance_density( 22.000000, tes_temp );
				break;
			case 5:
				switch_result = substance_density( 23.000000, tes_temp );
				break;
			case 6:
				switch_result = substance_density( 24.000000, tes_temp );
				break;
			case 7:
				switch_result = substance_density( 30.000000, tes_temp );
				break;
			case 8:
				switch_result = substance_density( 29.000000, tes_temp );
				break;
			case 9:
				switch_result = ;
				break;
			default:
				throw std::runtime_error("Molten Salt Linear Fresnel Storage switch undefined case for csp.mslf.control.store_fluid");
			}
		return switch_result;
	};

	csp.mslf.control.tes_dens = switch_csp.mslf.control.store_fluid();

	return csp.mslf.control.tes_dens;

}



