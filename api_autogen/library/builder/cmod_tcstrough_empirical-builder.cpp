#include <string>
#include <vector>

#include "vartab.h"

#include "cmod_tcstrough_empirical-builder.h"

float TcstroughEmpirical_PFSmax_eval(var_table* vt)
{
	// inputs
	float ui_tes_htf_type = vt->lookup("ui_tes_htf_type")->num;
	float ui_field_htf_type = vt->lookup("ui_field_htf_type")->num;
	float ui_q_design = vt->lookup("ui_q_design")->num;
	float TurTesOutAdj = vt->lookup("TurTesOutAdj")->num;
	float TurTesEffAdj = vt->lookup("TurTesEffAdj")->num;
	float MaxGrOut = vt->lookup("MaxGrOut")->num;

	// outputs
	float PFSmax;

	if ( ui_tes_htf_type == ui_field_htf_type ) {
		PFSmax = ui_q_design * TurTesOutAdj / TurTesEffAdj * MaxGrOut;
	}
	else {
		PFSmax = ui_q_design * TurTesOutAdj / TurTesEffAdj;}

	return PFSmax;

}



float TcstroughEmpirical_system_capacity_eval(var_table* vt)
{
	// inputs
	float ui_net_capacity = vt->lookup("ui_net_capacity")->num;

	// outputs
	float system_capacity;

	system_capacity = ui_net_capacity * 1000.000000;

	return system_capacity;

}



float TcstroughEmpirical_ui_hce_opt_eff_1_eval(var_table* vt)
{
	// inputs
	float calc_hce_col_factor = vt->lookup("calc_hce_col_factor")->num;
	float ui_hce_broken_glass_1 = vt->lookup("ui_hce_broken_glass_1")->num;
	float ui_hce_HCEdust = vt->lookup("ui_hce_HCEdust")->num;
	float HCEBelShad_1 = vt->lookup("HCEBelShad_1")->num;
	float HCEEnvTrans_1 = vt->lookup("HCEEnvTrans_1")->num;
	float HCEabs_1 = vt->lookup("HCEabs_1")->num;
	float HCEmisc_1 = vt->lookup("HCEmisc_1")->num;

	// outputs
	float ui_hce_opt_eff_1;

	auto switch_ui_hce_broken_glass_1 = [&]{
		float switch_result;
		switch( ui_hce_broken_glass_1 ){
			case 0:
				switch_result = ui_hce_HCEdust;
				break;
			case 1:
				switch_result = 1.000000;
				break;
			default:
				throw std::runtime_error("Empirical Trough HCE switch undefined case for ui_hce_broken_glass_1");
			}
		return switch_result;
	};

	ui_hce_opt_eff_1 = calc_hce_col_factor * switch_ui_hce_broken_glass_1() * HCEBelShad_1 * HCEEnvTrans_1 * HCEabs_1 * HCEmisc_1;

	return ui_hce_opt_eff_1;

}



float TcstroughEmpirical_calc_col_factor_eval(var_table* vt)
{
	// inputs
	float TrkTwstErr = vt->lookup("TrkTwstErr")->num;
	float GeoAcc = vt->lookup("GeoAcc")->num;
	float MirRef = vt->lookup("MirRef")->num;
	float MirCln = vt->lookup("MirCln")->num;
	float ConcFac = vt->lookup("ConcFac")->num;

	// outputs
	float calc_col_factor;

	calc_col_factor = TrkTwstErr * GeoAcc * MirRef * MirCln * ConcFac;

	return calc_col_factor;

}



float TcstroughEmpirical_ui_piping_heat_loss_eval(var_table* vt)
{
	// inputs
	float SfPipeHl3 = vt->lookup("SfPipeHl3")->num;
	float calc_field_htf_average_temp = vt->lookup("calc_field_htf_average_temp")->num;
	float SfPipeHl2 = vt->lookup("SfPipeHl2")->num;
	float SfPipeHl1 = vt->lookup("SfPipeHl1")->num;
	float SfPipeHl300 = vt->lookup("SfPipeHl300")->num;

	// outputs
	float ui_piping_heat_loss;

	ui_piping_heat_loss = SfPipeHl3 * pow( calc_field_htf_average_temp, 3.000000 ) + SfPipeHl2 * pow( calc_field_htf_average_temp, 2.000000 ) + SfPipeHl1 * calc_field_htf_average_temp * SfPipeHl300;

	return ui_piping_heat_loss;

}



float TcstroughEmpirical_HhtfPar_eval(var_table* vt)
{
	// inputs
	float HhtfParPF = vt->lookup("HhtfParPF")->num;
	float ui_par_tes_const = vt->lookup("ui_par_tes_const")->num;
	float ui_par_turb_out_gr = vt->lookup("ui_par_turb_out_gr")->num;

	// outputs
	float HhtfPar;

	HhtfPar = HhtfParPF * ui_par_tes_const * ui_par_turb_out_gr;

	return HhtfPar;

}



