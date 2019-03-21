#include <string>
#include <vector>

#include "vartab.h"

#include "cmod_tcslinear_fresnel-builder.h"

float TcslinearFresnel_csp.lf.geom1.avg_field_temp_dt_design_eval(var_table* vt)
{
	// inputs
	float T_cold_ref = vt->lookup("T_cold_ref")->num;
	float T_hot = vt->lookup("T_hot")->num;
	float T_amb_des_sf = vt->lookup("T_amb_des_sf")->num;

	// outputs
	float csp.lf.geom1.avg_field_temp_dt_design;

	csp.lf.geom1.avg_field_temp_dt_design = T_cold_ref + T_hot / 2.000000 - T_amb_des_sf;

	return csp.lf.geom1.avg_field_temp_dt_design;

}



float TcslinearFresnel_csp.lf.geom2.rec_thermal_derate_eval(var_table* vt)
{
	// inputs
	float csp.lf.geom2.heat_loss_at_design = vt->lookup("csp.lf.geom2.heat_loss_at_design")->num;
	float I_bn_des = vt->lookup("I_bn_des")->num;
	float csp.lf.geom2.refl_aper_area = vt->lookup("csp.lf.geom2.refl_aper_area")->num;
	float csp.lf.geom2.coll_length = vt->lookup("csp.lf.geom2.coll_length")->num;

	// outputs
	float csp.lf.geom2.rec_thermal_derate;

	csp.lf.geom2.rec_thermal_derate = 1.000000 - csp.lf.geom2.heat_loss_at_design / I_bn_des * csp.lf.geom2.refl_aper_area / csp.lf.geom2.coll_length;

	return csp.lf.geom2.rec_thermal_derate;

}



float TcslinearFresnel_system_capacity_eval(var_table* vt)
{
	// inputs
	float nameplate = vt->lookup("nameplate")->num;

	// outputs
	float system_capacity;

	system_capacity = nameplate * 1000.000000;

	return system_capacity;

}



float TcslinearFresnel_csp.lf.sf.geom2_area_frac_eval(var_table* vt)
{
	// inputs
	float csp.lf.sf.sh_geom_unique = vt->lookup("csp.lf.sf.sh_geom_unique")->num;
	float nModSH = vt->lookup("nModSH")->num;
	float csp.lf.geom2.refl_aper_area = vt->lookup("csp.lf.geom2.refl_aper_area")->num;
	float nModBoil = vt->lookup("nModBoil")->num;
	float csp.lf.geom1.refl_aper_area = vt->lookup("csp.lf.geom1.refl_aper_area")->num;

	// outputs
	float csp.lf.sf.geom2_area_frac;

	auto switch_csp.lf.sf.sh_geom_unique = [&]{
		float switch_result;
		switch( csp.lf.sf.sh_geom_unique ){
			case 0:
				switch_result = 0.000000;
				break;
			case 1:
				switch_result = nModSH * csp.lf.geom2.refl_aper_area / nModBoil * csp.lf.geom1.refl_aper_area + nModSH * csp.lf.geom2.refl_aper_area;
				break;
			default:
				throw std::runtime_error("Linear Fresnel Solar Field switch undefined case for csp.lf.sf.sh_geom_unique");
			}
		return switch_result;
	};

	csp.lf.sf.geom2_area_frac = switch_csp.lf.sf.sh_geom_unique();

	return csp.lf.sf.geom2_area_frac;

}



