#include <string>
#include <vector>

#include "vartab.h"

#include "cmod_hcpv-builder.h"

float Hcpv_array_num_inverters_eval(var_table* vt)
{
	// inputs
	float hcpv.array.nameplate = vt->lookup("hcpv.array.nameplate")->num;
	float inv_snl_pdco = vt->lookup("inv_snl_pdco")->num;

	// outputs
	float array_num_inverters;

	array_num_inverters = ceil( hcpv.array.nameplate / inv_snl_pdco / 1000.000000 );

	return array_num_inverters;

}



float Hcpv_hcpv.module.area_eval(var_table* vt)
{
	// inputs
	float module_concentration = vt->lookup("module_concentration")->num;
	float module_cell_area = vt->lookup("module_cell_area")->num;
	float module_ncells = vt->lookup("module_ncells")->num;

	// outputs
	float hcpv.module.area;

	hcpv.module.area = module_concentration * module_cell_area * 0.000100 * module_ncells;

	return hcpv.module.area;

}



