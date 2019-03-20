#include <string>
#include <vector>

#include "vartab.h"

#include "cmod_generic_system-builder.h"

float GenericSystem_GenericSystemPlant_conv_eff_eval(var_table* vt, invoke_t* cxt)
{
	// inputs
	float heat_rate = vt->lookup("heat_rate")->num;

	// outputs
	float conv_eff;

	if ( heat_rate == 0.000000 ) {
		conv_eff = 0.000000;
	}
	conv_eff = 100.000000 / heat_rate * 0.293100;


	if (cxt){
		cxt->result().assign("conv_eff", conv_eff);
	}

	return conv_eff;

}



