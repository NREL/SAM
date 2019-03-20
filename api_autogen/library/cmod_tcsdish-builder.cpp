#include <string>
#include <vector>

#include "vartab.h"

#include "cmod_tcsdish-builder.h"

float Tcsdish_DishSolarField_csp.ds.ncollectors_eval(var_table* vt, invoke_t* cxt)
{
	// inputs
	float n_ew = vt->lookup("n_ew")->num;
	float n_ns = vt->lookup("n_ns")->num;

	// outputs
	float csp.ds.ncollectors;

	csp.ds.ncollectors = n_ew * n_ns;

	if (cxt){
		cxt->result().assign("csp.ds.ncollectors", csp.ds.ncollectors);
	}

	return csp.ds.ncollectors;

}



