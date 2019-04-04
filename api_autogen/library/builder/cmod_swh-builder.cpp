#include <string>
#include <vector>

#include "vartab.h"

#include "cmod_swh-builder.h"

float Swh_system_capacity_eval(var_table* vt)
{
	// inputs
	float area_coll = vt->lookup("area_coll")->num;
	float ncoll = vt->lookup("ncoll")->num;
	float FRta = vt->lookup("FRta")->num;
	float FRUL = vt->lookup("FRUL")->num;

	// outputs
	float system_capacity;

	system_capacity = area_coll * ncoll * FRta - FRUL * 30.000000 / 1000.000000;

	return system_capacity;

}



