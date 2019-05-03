#include <string>
#include <vector>

#include "vartab.h"

#include "cmod_linear_fresnel_dsg_iph-builder.h"

float LinearFresnelDsgIph_csp.lf.sf.dp.loop_therm_eff_eval(var_table* vt)
{
	// inputs
	float csp.lf.geom1.rec_thermal_derate = vt->lookup("csp.lf.geom1.rec_thermal_derate")->num;

	// outputs
	float csp.lf.sf.dp.loop_therm_eff;

	csp.lf.sf.dp.loop_therm_eff = csp.lf.geom1.rec_thermal_derate;

	return csp.lf.sf.dp.loop_therm_eff;

}



float LinearFresnelDsgIph_csp.lf.geom1.rec_thermal_derate_eval(var_table* vt)
{
	// inputs
	float csp.lf.geom1.heat_loss_at_design = vt->lookup("csp.lf.geom1.heat_loss_at_design")->num;
	float I_bn_des = vt->lookup("I_bn_des")->num;
	float csp.lf.geom1.refl_aper_area = vt->lookup("csp.lf.geom1.refl_aper_area")->num;
	float csp.lf.geom1.coll_length = vt->lookup("csp.lf.geom1.coll_length")->num;

	// outputs
	float csp.lf.geom1.rec_thermal_derate;

	csp.lf.geom1.rec_thermal_derate = 1.000000 - csp.lf.geom1.heat_loss_at_design / I_bn_des * csp.lf.geom1.refl_aper_area / csp.lf.geom1.coll_length;

	return csp.lf.geom1.rec_thermal_derate;

}



