#include <string>
#include <vector>

#include "vartab.h"

#include "cmod_hcpv-builder.h"

float Hcpv_HCPVArray_array_num_inverters_eval(var_table* vt, invoke_t* cxt)
{
	// inputs
	float hcpv.array.nameplate = vt->lookup("hcpv.array.nameplate")->num;
	float inv_snl_pdco = vt->lookup("inv_snl_pdco")->num;

	// outputs
	float array_num_inverters;

	array_num_inverters = ceil( hcpv.array.nameplate / inv_snl_pdco / 1000.000000 );

	if (cxt){
		cxt->result().assign("array_num_inverters", array_num_inverters);
	}

	return array_num_inverters;

}



float Hcpv_HCPVModule_hcpv.module.area_eval(var_table* vt, invoke_t* cxt)
{
	// inputs
	float module_concentration = vt->lookup("module_concentration")->num;
	float module_cell_area = vt->lookup("module_cell_area")->num;
	float module_ncells = vt->lookup("module_ncells")->num;

	// outputs
	float hcpv.module.area;

	hcpv.module.area = module_concentration * module_cell_area * 0.000100 * module_ncells;

	if (cxt){
		cxt->result().assign("hcpv.module.area", hcpv.module.area);
	}

	return hcpv.module.area;

}



