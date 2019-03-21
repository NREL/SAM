#include <string>
#include <vector>

#include "vartab.h"

#include "cmod_tcsdish-builder.h"

float Tcsdish_csp.ds.ncollectors_eval(var_table* vt)
{
	// inputs
	float n_ew = vt->lookup("n_ew")->num;
	float n_ns = vt->lookup("n_ns")->num;

	// outputs
	float csp.ds.ncollectors;

	csp.ds.ncollectors = n_ew * n_ns;

	return csp.ds.ncollectors;

}



