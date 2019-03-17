#include "GenericSystem-data.h"

#include "GenericSystem-builder.h"

float SAM_GenericSystemPlant_conv_eff_eqn(var_table* vt, lk::invoke_t* cxt){
	// inputs
	float heat_rate = vt->lookup("heat_rate")->num;

	if ( heat_rate == 0.000000 ) {
		return 0.000000
	}
	return 100.000000 / heat_rate * 0.293100;
}



