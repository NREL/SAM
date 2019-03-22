#include <string>
#include <vector>

#include "vartab.h"

#include "cmod_pvsamv1-builder.h"

var_table Pvsamv1_IEC61853SingleDiodeModel_BtnCalcIec65153Par_func(var_table* vt, invoke_t* cxt)
{
	// inputs
	undefined input
	undefined nser
	undefined type
	undefined verbose
	undefined S

	// outputs
	float sd11par_alphaIsc;
	float sd11par_gammaPmp;
	float sd11par_betaVoc;
	float sd11par_n;
	float sd11par_Il;
	float sd11par_Egref;
	float sd11par_Io;
	float sd11par_c1;
	float sd11par_c2;
	float sd11par_c3;
	float sd11par_d2;
	float sd11par_d1;
	float sd11par_d3;

auto S = ssc_create(  );
ssc_var( S, input, iec61853_test_data );
ssc_var( S, nser, sd11par_nser );
ssc_var( S, type, sd11par_type );
ssc_var( S, verbose, 1.000000 );
std::string code = ssc_exec( S, iec61853par, {"show_dialog": "1.000000", "hold_dialog": "1.000000", } );
if ( code == 0.000000 ) {
	sd11par_alphaIsc = ssc_var( S, alphaIsc );
	sd11par_betaVoc = ssc_var( S, betaVoc );
	sd11par_gammaPmp = ssc_var( S, gammaPmp );
	sd11par_n = ssc_var( S, n );
	sd11par_Il = ssc_var( S, Il );
	sd11par_Io = ssc_var( S, Io );
	sd11par_Egref = ssc_var( S, Egref );
	sd11par_c1 = ssc_var( S, C1 );
	sd11par_c2 = ssc_var( S, C2 );
	sd11par_c3 = ssc_var( S, C3 );
	sd11par_d1 = ssc_var( S, D1 );
	sd11par_d2 = ssc_var( S, D2 );
	sd11par_d3 = ssc_var( S, D3 );

}
else {
	msgbox( Error estimating parameters.  Values were not updated. )}
ssc_free( S );
;




var_table Pvsamv1_inv_snl_eff_cec_MIMO_eval(var_table* vt)
{
	// inputs
	float inv_snl_vdco = vt->lookup("inv_snl_vdco")->num;
	float inv_snl_pdco = vt->lookup("inv_snl_pdco")->num;
	float inv_snl_pso = vt->lookup("inv_snl_pso")->num;
	float inv_snl_paco = vt->lookup("inv_snl_paco")->num;
	float inv_snl_c0 = vt->lookup("inv_snl_c0")->num;
	float inv_snl_c1 = vt->lookup("inv_snl_c1")->num;
	float inv_snl_c2 = vt->lookup("inv_snl_c2")->num;
	float inv_snl_c3 = vt->lookup("inv_snl_c3")->num;

	// outputs
	float inv_snl_eff_cec;
	float inv_snl_eff_euro;

	float vdco = inv_snl_vdco;
	float pdco = inv_snl_pdco;
	float pso = inv_snl_pso;
	float paco = inv_snl_paco;
	float c0 = inv_snl_c0;
	float c1 = inv_snl_c1;
	float c2 = inv_snl_c2;
	float c3 = inv_snl_c3;
	if ( pdco <= 0.000000 ) {
		throw std::runtime_error("Inverter CEC Database conditional error: pdco <= 0.000000");
	}
	float pdc5 = 0.050000 * pdco;
	float pac5 = snlinverter( pdc5, vdco, vdco, pdco, pso, paco, c0, c1, c2, c3 );
	float pdc10 = 0.100000 * pdco;
	float pac10 = snlinverter( pdc10, vdco, vdco, pdco, pso, paco, c0, c1, c2, c3 );
	float pdc20 = 0.200000 * pdco;
	float pac20 = snlinverter( pdc20, vdco, vdco, pdco, pso, paco, c0, c1, c2, c3 );
	float pdc30 = 0.300000 * pdco;
	float pac30 = snlinverter( pdc30, vdco, vdco, pdco, pso, paco, c0, c1, c2, c3 );
	float pdc50 = 0.500000 * pdco;
	float pac50 = snlinverter( pdc50, vdco, vdco, pdco, pso, paco, c0, c1, c2, c3 );
	float pdc75 = 0.750000 * pdco;
	float pac75 = snlinverter( pdc75, vdco, vdco, pdco, pso, paco, c0, c1, c2, c3 );
	float pdc100 = pdco;
	float pac100 = snlinverter( pdc100, vdco, vdco, pdco, pso, paco, c0, c1, c2, c3 );
	float cec_F1 = 0.000000;
	float cec_F2 = 0.040000;
	float cec_F3 = 0.050000;
	float cec_F4 = 0.120000;
	float cec_F5 = 0.210000;
	float cec_F6 = 0.530000;
	float cec_F7 = 0.050000;
	float euro_F1 = 0.030000;
	float euro_F2 = 0.060000;
	float euro_F3 = 0.130000;
	float euro_F4 = 0.100000;
	float euro_F5 = 0.480000;
	float euro_F6 = 0.000000;
	float euro_F7 = 0.200000;
	float cec_eff = cec_F1 * pac5 / pdc5 + cec_F2 * pac10 / pdc10 + cec_F3 * pac20 / pdc20 + cec_F4 * pac30 / pdc30 + cec_F5 * pac50 / pdc50 + cec_F6 * pac75 / pdc75 + cec_F7 * pac100 / pdc100;
	float euro_eff = euro_F1 * pac5 / pdc5 + euro_F2 * pac10 / pdc10 + euro_F3 * pac20 / pdc20 + euro_F4 * pac30 / pdc30 + euro_F5 * pac50 / pdc50 + euro_F6 * pac75 / pdc75 + euro_F7 * pac100 / pdc100;
	inv_snl_eff_cec = cec_eff * 100.000000;
	inv_snl_eff_euro = euro_eff * 100.000000;


	var_table vt;
	vt.assign( "inv_snl_eff_cec", inv_snl_eff_cec );
	vt.assign( "inv_snl_eff_euro", inv_snl_eff_euro );

}



float Pvsamv1_cec_p_mp_ref_eval(var_table* vt)
{
	// inputs
	float cec_i_mp_ref = vt->lookup("cec_i_mp_ref")->num;
	float cec_v_mp_ref = vt->lookup("cec_v_mp_ref")->num;

	// outputs
	float cec_p_mp_ref;

	cec_p_mp_ref = cec_i_mp_ref * cec_v_mp_ref;

	return cec_p_mp_ref;

}



float Pvsamv1_inv_pd_pdco_eval(var_table* vt)
{
	// inputs
	float inv_pd_paco = vt->lookup("inv_pd_paco")->num;
	float inv_pd_eff = vt->lookup("inv_pd_eff")->num;

	// outputs
	float inv_pd_pdco;

	inv_pd_pdco = inv_pd_paco / inv_pd_eff / 100.000000;

	return inv_pd_pdco;

}



float Pvsamv1_6par_pmp_eval(var_table* vt)
{
	// inputs
	float 6par_vmp = vt->lookup("6par_vmp")->num;
	float 6par_imp = vt->lookup("6par_imp")->num;

	// outputs
	float 6par_pmp;

	6par_pmp = 6par_vmp * 6par_imp;

	return 6par_pmp;

}



float Pvsamv1_total_modules_eval(var_table* vt)
{
	// inputs
	float subarray1_modules_per_string = vt->lookup("subarray1_modules_per_string")->num;
	float subarray1_nstrings = vt->lookup("subarray1_nstrings")->num;
	float subarray2_modules_per_string = vt->lookup("subarray2_modules_per_string")->num;
	float subarray2_nstrings = vt->lookup("subarray2_nstrings")->num;
	float subarray2_enable = vt->lookup("subarray2_enable")->num;
	float subarray3_modules_per_string = vt->lookup("subarray3_modules_per_string")->num;
	float subarray3_nstrings = vt->lookup("subarray3_nstrings")->num;
	float subarray3_enable = vt->lookup("subarray3_enable")->num;
	float subarray4_modules_per_string = vt->lookup("subarray4_modules_per_string")->num;
	float subarray4_nstrings = vt->lookup("subarray4_nstrings")->num;
	float subarray4_enable = vt->lookup("subarray4_enable")->num;

	// outputs
	float total_modules;

	float a = subarray1_modules_per_string * subarray1_nstrings + subarray2_modules_per_string * subarray2_nstrings * subarray2_enable + subarray3_modules_per_string * subarray3_nstrings * subarray3_enable + subarray4_modules_per_string * subarray4_nstrings * subarray4_enable;
	total_modules = a;


	return total_modules;

}



var_table Pvsamv1_subarray2_enable_MIMO_eval(var_table* vt)
{
	// inputs
	float enable_auto_size = vt->lookup("enable_auto_size")->num;
	float module_model = vt->lookup("module_model")->num;
	float spe_vmp = vt->lookup("spe_vmp")->num;
	float cec_v_mp_ref = vt->lookup("cec_v_mp_ref")->num;
	float 6par_vmp = vt->lookup("6par_vmp")->num;
	float snl_ref_vmp = vt->lookup("snl_ref_vmp")->num;
	float sd11par_Vmp0 = vt->lookup("sd11par_Vmp0")->num;
	float spe_voc = vt->lookup("spe_voc")->num;
	float cec_v_oc_ref = vt->lookup("cec_v_oc_ref")->num;
	float 6par_voc = vt->lookup("6par_voc")->num;
	float snl_ref_voc = vt->lookup("snl_ref_voc")->num;
	float sd11par_Voc0 = vt->lookup("sd11par_Voc0")->num;
	float spe_power = vt->lookup("spe_power")->num;
	float cec_p_mp_ref = vt->lookup("cec_p_mp_ref")->num;
	float 6par_pmp = vt->lookup("6par_pmp")->num;
	float snl_ref_pmp = vt->lookup("snl_ref_pmp")->num;
	float sd11par_Pmp0 = vt->lookup("sd11par_Pmp0")->num;
	float inverter_model = vt->lookup("inverter_model")->num;
	float inv_snl_mppt_low = vt->lookup("inv_snl_mppt_low")->num;
	float inv_ds_mppt_low = vt->lookup("inv_ds_mppt_low")->num;
	float inv_pd_mppt_low = vt->lookup("inv_pd_mppt_low")->num;
	float inv_cec_cg_mppt_low = vt->lookup("inv_cec_cg_mppt_low")->num;
	float inv_snl_vdcmax = vt->lookup("inv_snl_vdcmax")->num;
	float inv_ds_vdcmax = vt->lookup("inv_ds_vdcmax")->num;
	float inv_pd_vdcmax = vt->lookup("inv_pd_vdcmax")->num;
	float inv_cec_cg_vdcmax = vt->lookup("inv_cec_cg_vdcmax")->num;
	float inv_snl_mppt_hi = vt->lookup("inv_snl_mppt_hi")->num;
	float inv_ds_mppt_hi = vt->lookup("inv_ds_mppt_hi")->num;
	float inv_pd_mppt_hi = vt->lookup("inv_pd_mppt_hi")->num;
	float inv_cec_cg_mppt_hi = vt->lookup("inv_cec_cg_mppt_hi")->num;
	float inv_snl_paco = vt->lookup("inv_snl_paco")->num;
	float inv_ds_paco = vt->lookup("inv_ds_paco")->num;
	float inv_pd_paco = vt->lookup("inv_pd_paco")->num;
	float inv_cec_cg_paco = vt->lookup("inv_cec_cg_paco")->num;
	undefined en_batt
	undefined batt_ac_or_dc
	float batt_max_power = vt->lookup("batt_max_power")->num;
	float desired_size = vt->lookup("desired_size")->num;
	float desired_dcac_ratio = vt->lookup("desired_dcac_ratio")->num;

	// outputs
	float subarray2_enable;
	float subarray3_enable;
	float subarray4_enable;
	float subarray1_modules_per_string;
	float subarray1_nstrings;
	float inverter_count;

	auto switch_module_model = [&]{
		float switch_result;
		switch( module_model ){
			case 0:
				switch_result = spe_vmp;
				break;
			case 1:
				switch_result = cec_v_mp_ref;
				break;
			case 2:
				switch_result = 6par_vmp;
				break;
			case 3:
				switch_result = snl_ref_vmp;
				break;
			case 4:
				switch_result = sd11par_Vmp0;
				break;
			default:
				throw std::runtime_error("PV System Design switch undefined case for module_model");
			}
		return switch_result;
	};

	auto switch_inverter_model = [&]{
		float switch_result;
		switch( inverter_model ){
			case 0:
				switch_result = inv_snl_mppt_low;
				break;
			case 1:
				switch_result = inv_ds_mppt_low;
				break;
			case 2:
				switch_result = inv_pd_mppt_low;
				break;
			case 3:
				switch_result = inv_cec_cg_mppt_low;
				break;
			default:
				throw std::runtime_error("PV System Design switch undefined case for inverter_model");
			}
		return switch_result;
	};

	if ( enable_auto_size == 1.000000 ) {
		subarray2_enable = 0.000000;
		subarray3_enable = 0.000000;
		subarray4_enable = 0.000000;
		float mod_vmp = switch_module_model();
		float mod_voc = switch_module_model();
		float mod_power = switch_module_model();
		float inv_vmin = switch_inverter_model();
		float inv_vdcmax = switch_inverter_model();
		float inv_vmax = switch_inverter_model();
		float inv_power = switch_inverter_model();
		float batt_max_power_dc = 0.000000;
		std::string f = financing(  );
		if ( f != LCOE Calculator && f != None ) {
			if ( en_batt ) {
				if ( batt_ac_or_dc == 0.000000 ) {
					float batt_max_power_dc = batt_max_power
				}
			}
		}
		if ( mod_vmp > 0.000000 ) {
			float num_series = 0.500000 * inv_vmin + inv_vmax / mod_vmp
		}
		if ( inv_vdcmax > 0.000000 ) {
			for ( ; num_series > 0.000000 && num_series * mod_voc > inv_vdcmax;  ){
				float num_series = num_series - 1.000000
			}
		}
		if ( num_series < 1.000000 ) {
			float num_series = 1.000000
		}
		float num_series = round( num_series );
		float num_parallel = desired_size * 1000.000000 / num_series * mod_power;
		if ( num_parallel < 1.000000 ) {
			float num_parallel = 1.000000
		}
		float num_parallel = round( num_parallel );
		if ( desired_dcac_ratio > 0.000000 ) {
			float inverters = num_series * num_parallel * mod_power / desired_dcac_ratio * inv_power;
			if ( inverters - floor( inverters ) < 0.500000 ) {
				float num_inverters = floor( inverters )
			}
			else {
				float num_inverters = ceil( inverters )}
		
		}
		else {
			float num_inverters = ceil( num_series * num_parallel * mod_power / inv_power )}
		if ( num_inverters < 1.000000 ) {
			float num_inverters = 1.000000
		}
		float proposed_size = num_series * num_parallel * mod_power / 1000.000000;
		float proposed_ratio = proposed_size / num_inverters * inv_power / 1000.000000;
		if ( abs( proposed_size - desired_size ) / desired_size > 0.200000 ) {
			float num_series = 0.000000;
			float num_parallel = 0.000000;
			float inverter_count = 0.000000;
			msgbox( SAM was not able to automatically size the system close enough to your desired size. Please size the system manually, or enter another size. );
		
		}
		subarray1_modules_per_string = num_series;
		subarray1_nstrings = num_parallel;
		inverter_count = num_inverters;
	
	}

	var_table vt;
	vt.assign( "subarray2_enable", subarray2_enable );
	vt.assign( "subarray3_enable", subarray3_enable );
	vt.assign( "subarray4_enable", subarray4_enable );
	vt.assign( "subarray1_modules_per_string", subarray1_modules_per_string );
	vt.assign( "subarray1_nstrings", subarray1_nstrings );
	vt.assign( "inverter_count", inverter_count );

}



var_table Pvsamv1_inv_cec_cg_eff_cec_MIMO_eval(var_table* vt)
{
	// inputs
	float inv_cec_cg_vdco = vt->lookup("inv_cec_cg_vdco")->num;
	float inv_cec_cg_pdco = vt->lookup("inv_cec_cg_pdco")->num;
	float inv_cec_cg_psco = vt->lookup("inv_cec_cg_psco")->num;
	float inv_cec_cg_paco = vt->lookup("inv_cec_cg_paco")->num;
	float inv_cec_cg_c0 = vt->lookup("inv_cec_cg_c0")->num;
	float inv_cec_cg_c1 = vt->lookup("inv_cec_cg_c1")->num;
	float inv_cec_cg_c2 = vt->lookup("inv_cec_cg_c2")->num;
	float inv_cec_cg_c3 = vt->lookup("inv_cec_cg_c3")->num;

	// outputs
	float inv_cec_cg_eff_cec;
	float inv_cec_cg_eff_euro;

	float vdco = inv_cec_cg_vdco;
	float pdco = inv_cec_cg_pdco;
	float pso = inv_cec_cg_psco;
	float paco = inv_cec_cg_paco;
	float c0 = inv_cec_cg_c0;
	float c1 = inv_cec_cg_c1;
	float c2 = inv_cec_cg_c2;
	float c3 = inv_cec_cg_c3;
	if ( pdco <= 0.000000 ) {
		throw std::runtime_error("Inverter CEC Coefficient Generator conditional error: pdco <= 0.000000");
	}
	float pdc5 = 0.050000 * pdco;
	float pac5 = snlinverter( pdc5, vdco, vdco, pdco, pso, paco, c0, c1, c2, c3 );
	float pdc10 = 0.100000 * pdco;
	float pac10 = snlinverter( pdc10, vdco, vdco, pdco, pso, paco, c0, c1, c2, c3 );
	float pdc20 = 0.200000 * pdco;
	float pac20 = snlinverter( pdc20, vdco, vdco, pdco, pso, paco, c0, c1, c2, c3 );
	float pdc30 = 0.300000 * pdco;
	float pac30 = snlinverter( pdc30, vdco, vdco, pdco, pso, paco, c0, c1, c2, c3 );
	float pdc50 = 0.500000 * pdco;
	float pac50 = snlinverter( pdc50, vdco, vdco, pdco, pso, paco, c0, c1, c2, c3 );
	float pdc75 = 0.750000 * pdco;
	float pac75 = snlinverter( pdc75, vdco, vdco, pdco, pso, paco, c0, c1, c2, c3 );
	float pdc100 = pdco;
	float pac100 = snlinverter( pdc100, vdco, vdco, pdco, pso, paco, c0, c1, c2, c3 );
	float cec_F1 = 0.000000;
	float cec_F2 = 0.040000;
	float cec_F3 = 0.050000;
	float cec_F4 = 0.120000;
	float cec_F5 = 0.210000;
	float cec_F6 = 0.530000;
	float cec_F7 = 0.050000;
	float euro_F1 = 0.030000;
	float euro_F2 = 0.060000;
	float euro_F3 = 0.130000;
	float euro_F4 = 0.100000;
	float euro_F5 = 0.480000;
	float euro_F6 = 0.000000;
	float euro_F7 = 0.200000;
	float cec_eff = cec_F1 * pac5 / pdc5 + cec_F2 * pac10 / pdc10 + cec_F3 * pac20 / pdc20 + cec_F4 * pac30 / pdc30 + cec_F5 * pac50 / pdc50 + cec_F6 * pac75 / pdc75 + cec_F7 * pac100 / pdc100;
	float euro_eff = euro_F1 * pac5 / pdc5 + euro_F2 * pac10 / pdc10 + euro_F3 * pac20 / pdc20 + euro_F4 * pac30 / pdc30 + euro_F5 * pac50 / pdc50 + euro_F6 * pac75 / pdc75 + euro_F7 * pac100 / pdc100;
	inv_cec_cg_eff_cec = cec_eff * 100.000000;
	inv_cec_cg_eff_euro = euro_eff * 100.000000;


	var_table vt;
	vt.assign( "inv_cec_cg_eff_cec", inv_cec_cg_eff_cec );
	vt.assign( "inv_cec_cg_eff_euro", inv_cec_cg_eff_euro );

}



var_table Pvsamv1_InverterCECCoefficientGenerator_InverterCECCoefficientGenerator_func(var_table* vt, invoke_t* cxt)
{
	// inputs
	undefined Vdco
	undefined Pdco
	undefined c1
	undefined Pso
	undefined c0
	undefined c2
	undefined c3

	// outputs
	float inv_cec_cg_c0;
	float inv_cec_cg_vdco;
	float inv_cec_cg_pdco;
	float inv_cec_cg_c3;
	float inv_cec_cg_psco;
	float inv_cec_cg_c1;
	float inv_cec_cg_c2;

calc_coefficientsobj = ssc_create(  );
ssc_var( obj, inv_cec_cg_paco, value( inv_cec_cg_paco ) );
ssc_var( obj, inv_cec_cg_sample_power_units, value( inv_cec_cg_sample_power_units ) );
ssc_var( obj, inv_cec_cg_test_samples, value( inv_cec_cg_test_samples ) );
std::string result = ssc_exec( obj, inv_cec_cg, {"show_dialog": "0.000000", "hold_dialog": "0.000000", } );
if ( result != 0.000000 ) {
	msgbox( Failed to solve for inverter coefficients. Please check your inputs. );
	;

}
std::string Pdco = ssc_var( obj, Pdco );
std::string Vdco = ssc_var( obj, Vdco );
std::string Pso = ssc_var( obj, Pso );
std::string c0 = ssc_var( obj, c0 );
std::string c1 = ssc_var( obj, c1 );
std::string c2 = ssc_var( obj, c2 );
std::string c3 = ssc_var( obj, c3 );
ssc_free( obj );
value( inv_cec_cg_pdco, Pdco );
value( inv_cec_cg_vdco, Vdco );
value( inv_cec_cg_psco, Pso );
value( inv_cec_cg_c0, c0 );
value( inv_cec_cg_c1, c1 );
value( inv_cec_cg_c2, c2 );
value( inv_cec_cg_c3, c3 );
;




var_table Pvsamv1_InverterCECCoefficientGenerator_InverterCECCoefficientGenerator_func(var_table* vt, invoke_t* cxt)
{
	// inputs
	float inv_cec_cg_paco = vt->lookup("inv_cec_cg_paco")->num;
	util::matrix_t<ssc_number_t> inv_cec_cg_test_samples = vt->lookup("inv_cec_cg_test_samples")->num;
	float inv_cec_cg_paco = vt->lookup("inv_cec_cg_paco")->num;
	float inv_cec_cg_sample_power_units = vt->lookup("inv_cec_cg_sample_power_units")->num;
	util::matrix_t<ssc_number_t> inv_cec_cg_test_samples = vt->lookup("inv_cec_cg_test_samples")->num;
	float inv_cec_cg_sample_power_units = vt->lookup("inv_cec_cg_sample_power_units")->num;
	undefined obj

	// outputs

calc_coefficientsobj = ssc_create(  );
ssc_var( obj, inv_cec_cg_paco, value( inv_cec_cg_paco ) );
ssc_var( obj, inv_cec_cg_sample_power_units, value( inv_cec_cg_sample_power_units ) );
ssc_var( obj, inv_cec_cg_test_samples, value( inv_cec_cg_test_samples ) );
std::string result = ssc_exec( obj, inv_cec_cg, {"show_dialog": "0.000000", "hold_dialog": "0.000000", } );
if ( result != 0.000000 ) {
	msgbox( Failed to solve for inverter coefficients. Please check your inputs. );
	;

}
std::string Pdco = ssc_var( obj, Pdco );
std::string Vdco = ssc_var( obj, Vdco );
std::string Pso = ssc_var( obj, Pso );
std::string c0 = ssc_var( obj, c0 );
std::string c1 = ssc_var( obj, c1 );
std::string c2 = ssc_var( obj, c2 );
std::string c3 = ssc_var( obj, c3 );
ssc_free( obj );
value( inv_cec_cg_pdco, Pdco );
value( inv_cec_cg_vdco, Vdco );
value( inv_cec_cg_psco, Pso );
value( inv_cec_cg_c0, c0 );
value( inv_cec_cg_c1, c1 );
value( inv_cec_cg_c2, c2 );
value( inv_cec_cg_c3, c3 );
;




float Pvsamv1_spe_power_eval(var_table* vt)
{
	// inputs
	float spe_reference = vt->lookup("spe_reference")->num;
	float spe_eff0 = vt->lookup("spe_eff0")->num;
	float spe_rad0 = vt->lookup("spe_rad0")->num;
	float spe_eff1 = vt->lookup("spe_eff1")->num;
	float spe_rad1 = vt->lookup("spe_rad1")->num;
	float spe_eff2 = vt->lookup("spe_eff2")->num;
	float spe_rad2 = vt->lookup("spe_rad2")->num;
	float spe_eff3 = vt->lookup("spe_eff3")->num;
	float spe_rad3 = vt->lookup("spe_rad3")->num;
	float spe_eff4 = vt->lookup("spe_eff4")->num;
	float spe_rad4 = vt->lookup("spe_rad4")->num;
	float spe_area = vt->lookup("spe_area")->num;

	// outputs
	float spe_power;

	auto switch_spe_reference = [&]{
		float switch_result;
		switch( spe_reference ){
			case 0:
				switch_result = spe_eff0 / 100.000000 * spe_rad0;
				break;
			case 1:
				switch_result = spe_eff1 / 100.000000 * spe_rad1;
				break;
			case 2:
				switch_result = spe_eff2 / 100.000000 * spe_rad2;
				break;
			case 3:
				switch_result = spe_eff3 / 100.000000 * spe_rad3;
				break;
			case 4:
				switch_result = spe_eff4 / 100.000000 * spe_rad4;
				break;
			default:
				throw std::runtime_error("Simple Efficiency Module Model switch undefined case for spe_reference");
			}
		return switch_result;
	};

	spe_power = switch_spe_reference() * spe_area;

	return spe_power;

}



var_table Pvsamv1_snl_ref_a_MIMO_eval(var_table* vt)
{
	// inputs
	float snl_module_structure = vt->lookup("snl_module_structure")->num;
	float snl_a = vt->lookup("snl_a")->num;
	float snl_b = vt->lookup("snl_b")->num;
	float snl_dtc = vt->lookup("snl_dtc")->num;
	float snl_specified_a = vt->lookup("snl_specified_a")->num;
	float snl_specified_b = vt->lookup("snl_specified_b")->num;
	float snl_specified_dT = vt->lookup("snl_specified_dT")->num;
	float snl_fd = vt->lookup("snl_fd")->num;
	float snl_a0 = vt->lookup("snl_a0")->num;
	float snl_a1 = vt->lookup("snl_a1")->num;
	float snl_a2 = vt->lookup("snl_a2")->num;
	float snl_a3 = vt->lookup("snl_a3")->num;
	float snl_a4 = vt->lookup("snl_a4")->num;
	float snl_b0 = vt->lookup("snl_b0")->num;
	float snl_b1 = vt->lookup("snl_b1")->num;
	float snl_b2 = vt->lookup("snl_b2")->num;
	float snl_b3 = vt->lookup("snl_b3")->num;
	float snl_b4 = vt->lookup("snl_b4")->num;
	float snl_b5 = vt->lookup("snl_b5")->num;
	float snl_isco = vt->lookup("snl_isco")->num;
	float snl_aisc = vt->lookup("snl_aisc")->num;
	float snl_c0 = vt->lookup("snl_c0")->num;
	float snl_c1 = vt->lookup("snl_c1")->num;
	float snl_aimp = vt->lookup("snl_aimp")->num;
	float snl_impo = vt->lookup("snl_impo")->num;
	float snl_bvmpo = vt->lookup("snl_bvmpo")->num;
	float snl_mbvmp = vt->lookup("snl_mbvmp")->num;
	float snl_n = vt->lookup("snl_n")->num;
	float snl_c3 = vt->lookup("snl_c3")->num;
	float snl_series_cells = vt->lookup("snl_series_cells")->num;
	float snl_c2 = vt->lookup("snl_c2")->num;
	float snl_vmpo = vt->lookup("snl_vmpo")->num;
	float snl_bvoco = vt->lookup("snl_bvoco")->num;
	float snl_mbvoc = vt->lookup("snl_mbvoc")->num;
	float snl_voco = vt->lookup("snl_voco")->num;
	float snl_area = vt->lookup("snl_area")->num;

	// outputs
	float snl_ref_a;
	float snl_ref_b;
	float snl_ref_dT;
	float snl_ref_isc;
	float snl_ref_isc_temp_0;
	float snl_ref_isc_temp_1;
	float snl_ref_imp;
	float snl_ref_imp_temp_0;
	float snl_imp_temp_1;
	float snl_ref_vmp;
	float snl_ref_vmp_temp_0;
	float snl_ref_vmp_temp_1;
	float snl_ref_pmp;
	float snl_ref_pmp_temp_0;
	float snl_ref_pmp_temp_1;
	float snl_ref_voc;
	float snl_ref_voc_temp_0;
	float snl_voc_temp_1;
	float snl_ref_eff;

	if ( snl_module_structure == 0.000000 ) {
		float a = snl_a;
		float b = snl_b;
		float dT = snl_dtc;
	
	}
	else if ( snl_module_structure == 1.000000 ) {
		float a = -3.560000;
		float b = -0.075000;
		float dT = 3.000000;
	
	}
	else if ( snl_module_structure == 2.000000 ) {
		float a = -3.470000;
		float b = -0.059400;
		float dT = 3.000000;
	
	}
	else if ( snl_module_structure == 3.000000 ) {
		float a = -3.580000;
		float b = -0.113000;
		float dT = 3.000000;
	
	}
	else if ( snl_module_structure == 4.000000 ) {
		float a = -2.810000;
		float b = -0.045500;
		float dT = 0.000000;
	
	}
	else if ( snl_module_structure == 5.000000 ) {
		float a = -2.980000;
		float b = -0.047100;
		float dT = 1.000000;
	
	}
	else {
		float a = snl_specified_a;
		float b = snl_specified_b;
		float dT = snl_specified_dT;
	}
	snl_ref_a = a;
	snl_ref_b = b;
	snl_ref_dT = dT;
	float fd = snl_fd;
	if ( fd == 0.000000 ) {
		float ama = 1.500000;
		float aoi = 0.000000;
		float Eb = 850.000000;
		float Ed = 0.000000;
		float Ta = 20.000000;
		float WS = 4.000000;
		float Tc = Eb + Ed * exp( a + b * WS ) + Ta + Eb + Ed / 1000.000000 * dT;
		float To = 25.000000;
	
	}
	else {
		float ama = 1.500000;
		float aoi = 0.000000;
		float Eb = 900.000000;
		float Ed = 100.000000;
		float Ta = 0.000000;
		float WS = 0.000000;
		float Tc = 25.000000;
		float To = 25.000000;
	}
	float fama = snl_a0 + snl_a1 * ama + snl_a2 * ama * ama + snl_a3 * ama * ama * ama + snl_a4 * ama * ama * ama * ama;
	float faoi = snl_b0 + snl_b1 * aoi + snl_b2 * aoi * aoi + snl_b3 * aoi * aoi * aoi + snl_b4 * aoi * aoi * aoi * aoi + snl_b5 * aoi * aoi * aoi * aoi * aoi;
	float Ee = fama * Eb * faoi + Ed * fd / 1000.000000;
	if ( Ee == 0.000000 ) {
		throw std::runtime_error("Sandia PV Array Performance Model with Module Database conditional error: Ee == 0.000000");
	}
	float Isc = snl_isco * Ee * 1.000000 + snl_aisc * Tc - To;
	if ( Isc == 0.000000 ) {
		throw std::runtime_error("Sandia PV Array Performance Model with Module Database conditional error: Isc == 0.000000");
	}
	float Asc = 1.000000 + snl_aisc * Tc - To;
	if ( Asc == 0.000000 ) {
		throw std::runtime_error("Sandia PV Array Performance Model with Module Database conditional error: Asc == 0.000000");
	}
	float dIsc_dT = snl_aisc * Isc / Asc;
	snl_ref_isc = Isc;
	snl_ref_isc_temp_0 = dIsc_dT / Isc * 100.000000;
	snl_ref_isc_temp_1 = dIsc_dT;
	float Ez = snl_c0 * Ee + snl_c1 * Ee * Ee;
	float Amp = 1.000000 + snl_aimp * Tc - To;
	float Imp = snl_impo * Ez * Amp;
	if ( Imp == 0.000000 ) {
		throw std::runtime_error("Sandia PV Array Performance Model with Module Database conditional error: Imp == 0.000000");
	}
	float dEe_dT = Asc * dIsc_dT - Isc * snl_aisc / Isc * Asc * Asc;
	float dEz_dt = snl_c0 + 2.000000 * snl_c1 * Ee * dEe_dT;
	float dImp_dT = snl_impo * Amp * dEz_dt + Ez * snl_aimp;
	snl_ref_imp = Imp;
	snl_ref_imp_temp_0 = dImp_dT / Imp * 100.000000;
	snl_imp_temp_1 = dImp_dT;
	float bVmpEe = snl_bvmpo + snl_mbvmp * 1.000000 - Ee;
	float VmpTerm4 = bVmpEe * Tc - To;
	float Tck = Tc + 273.150000;
	float k = 0.000000;
	float q = 0.000000;
	float DeltaTc = snl_n * k * Tck / q;
	float VmpTerm3 = snl_c3 * snl_series_cells * DeltaTc * log( Ee ) * DeltaTc * log( Ee );
	float VmpTerm2 = snl_c2 * snl_series_cells * DeltaTc * log( Ee );
	float Vmp = snl_vmpo + VmpTerm4 + VmpTerm3 + VmpTerm2;
	float dDeltaTc_dT = snl_n * k / q;
	float dVmp_dT_Term1 = snl_c2 * snl_series_cells * DeltaTc / Ee * dEe_dT + log( Ee ) * dDeltaTc_dT;
	float dVmp_dT_Term2 = snl_c3 * snl_series_cells * DeltaTc * DeltaTc * 2.000000 * log( Ee ) / Ee * dEe_dT + 2.000000 * DeltaTc * dDeltaTc_dT * log( Ee ) * log( Ee );
	float dbVmpEe_dT = ( snl_mbvmp * -1 ) * dEe_dT;
	float dVmp_dT_Term3and4 = dbVmpEe_dT * Tc - To + bVmpEe;
	float dVmp_dT = dVmp_dT_Term1 + dVmp_dT_Term2 + dVmp_dT_Term3and4;
	snl_ref_vmp = Vmp;
	snl_ref_vmp_temp_0 = dVmp_dT / Vmp * 100.000000;
	snl_ref_vmp_temp_1 = dVmp_dT;
	float Pmp = Imp * Vmp;
	float dPmp_dT = Imp * dVmp_dT + Vmp * dImp_dT;
	snl_ref_pmp = Pmp;
	snl_ref_pmp_temp_0 = dPmp_dT / Pmp * 100.000000;
	snl_ref_pmp_temp_1 = dPmp_dT;
	float bVocEe = snl_bvoco + snl_mbvoc * 1.000000 - Ee;
	float Voc = snl_voco + snl_series_cells * DeltaTc * log( Ee ) + bVocEe * Tc - To;
	float dVoc_dT_Term1 = snl_series_cells * log( Ee ) * dDeltaTc_dT;
	float dVoc_dT_Term2 = snl_series_cells * DeltaTc / Ee * dEe_dT;
	float dVoc_dT_Term3and4 = ( snl_mbvoc * -1 ) * dEe_dT * Tc - To + snl_bvoco;
	float dVoc_dT = dVoc_dT_Term1 + dVoc_dT_Term2 + dVoc_dT_Term3and4;
	snl_ref_voc = Voc;
	snl_ref_voc_temp_0 = dVoc_dT / Voc * 100.000000;
	snl_voc_temp_1 = dVoc_dT;
	snl_ref_eff = 100.000000 * Pmp / snl_area / Eb + Ed;


	var_table vt;
	vt.assign( "snl_ref_a", snl_ref_a );
	vt.assign( "snl_ref_b", snl_ref_b );
	vt.assign( "snl_ref_dT", snl_ref_dT );
	vt.assign( "snl_ref_isc", snl_ref_isc );
	vt.assign( "snl_ref_isc_temp_0", snl_ref_isc_temp_0 );
	vt.assign( "snl_ref_isc_temp_1", snl_ref_isc_temp_1 );
	vt.assign( "snl_ref_imp", snl_ref_imp );
	vt.assign( "snl_ref_imp_temp_0", snl_ref_imp_temp_0 );
	vt.assign( "snl_imp_temp_1", snl_imp_temp_1 );
	vt.assign( "snl_ref_vmp", snl_ref_vmp );
	vt.assign( "snl_ref_vmp_temp_0", snl_ref_vmp_temp_0 );
	vt.assign( "snl_ref_vmp_temp_1", snl_ref_vmp_temp_1 );
	vt.assign( "snl_ref_pmp", snl_ref_pmp );
	vt.assign( "snl_ref_pmp_temp_0", snl_ref_pmp_temp_0 );
	vt.assign( "snl_ref_pmp_temp_1", snl_ref_pmp_temp_1 );
	vt.assign( "snl_ref_voc", snl_ref_voc );
	vt.assign( "snl_ref_voc_temp_0", snl_ref_voc_temp_0 );
	vt.assign( "snl_voc_temp_1", snl_voc_temp_1 );
	vt.assign( "snl_ref_eff", snl_ref_eff );

}



