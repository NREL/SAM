#include <string>
#include <vector>

#include "vartab.h"

#include "cmod_biomass-builder.h"

float Biomass_biopwr.plant.nameplate_eval(var_table* vt)
{
	// inputs
	float biopwr.plant.boiler.cap_per_boiler = vt->lookup("biopwr.plant.boiler.cap_per_boiler")->num;
	float biopwr.plant.boiler.num = vt->lookup("biopwr.plant.boiler.num")->num;
	float biopwr.plant.boiler.steam_enthalpy = vt->lookup("biopwr.plant.boiler.steam_enthalpy")->num;
	float biopwr.plant.rated_eff = vt->lookup("biopwr.plant.rated_eff")->num;

	// outputs
	float biopwr.plant.nameplate;

	biopwr.plant.nameplate = biopwr.plant.boiler.cap_per_boiler * biopwr.plant.boiler.num * biopwr.plant.boiler.steam_enthalpy * 0.000293 * biopwr.plant.rated_eff;

	return biopwr.plant.nameplate;

}



float Biomass_biopwr.feedstock.total_moisture_eval(var_table* vt)
{
	// inputs
	float biopwr.feedstock.bagasse_frac = vt->lookup("biopwr.feedstock.bagasse_frac")->num;
	float biopwr.feedstock.bagasse_moisture = vt->lookup("biopwr.feedstock.bagasse_moisture")->num;
	float biopwr.feedstock.barley_frac = vt->lookup("biopwr.feedstock.barley_frac")->num;
	float biopwr.feedstock.barley_moisture = vt->lookup("biopwr.feedstock.barley_moisture")->num;
	float biopwr.feedstock.stover_frac = vt->lookup("biopwr.feedstock.stover_frac")->num;
	float biopwr.feedstock.stover_moisture = vt->lookup("biopwr.feedstock.stover_moisture")->num;
	float biopwr.feedstock.rice_frac = vt->lookup("biopwr.feedstock.rice_frac")->num;
	float biopwr.feedstock.rice_moisture = vt->lookup("biopwr.feedstock.rice_moisture")->num;
	float biopwr.feedstock.wheat_frac = vt->lookup("biopwr.feedstock.wheat_frac")->num;
	float biopwr.feedstock.wheat_moisture = vt->lookup("biopwr.feedstock.wheat_moisture")->num;
	float biopwr.feedstock.forest_frac = vt->lookup("biopwr.feedstock.forest_frac")->num;
	float biopwr.feedstock.forest_moisture = vt->lookup("biopwr.feedstock.forest_moisture")->num;
	float biopwr.feedstock.mill_frac = vt->lookup("biopwr.feedstock.mill_frac")->num;
	float biopwr.feedstock.mill_moisture = vt->lookup("biopwr.feedstock.mill_moisture")->num;
	float biopwr.feedstock.urban_frac = vt->lookup("biopwr.feedstock.urban_frac")->num;
	float biopwr.feedstock.urban_moisture = vt->lookup("biopwr.feedstock.urban_moisture")->num;
	float biopwr.feedstock.woody_frac = vt->lookup("biopwr.feedstock.woody_frac")->num;
	float biopwr.feedstock.woody_moisture = vt->lookup("biopwr.feedstock.woody_moisture")->num;
	float biopwr.feedstock.herb_frac = vt->lookup("biopwr.feedstock.herb_frac")->num;
	float biopwr.feedstock.herb_moisture = vt->lookup("biopwr.feedstock.herb_moisture")->num;
	float biopwr.feedstock.feedstock1_frac = vt->lookup("biopwr.feedstock.feedstock1_frac")->num;
	float biopwr.feedstock.feedstock1_moisture = vt->lookup("biopwr.feedstock.feedstock1_moisture")->num;
	float biopwr.feedstock.feedstock2_frac = vt->lookup("biopwr.feedstock.feedstock2_frac")->num;
	float biopwr.feedstock.feedstock2_moisture = vt->lookup("biopwr.feedstock.feedstock2_moisture")->num;
	float biopwr.feedstock.bit_frac = vt->lookup("biopwr.feedstock.bit_frac")->num;
	float biopwr.feedstock.bit_moisture = vt->lookup("biopwr.feedstock.bit_moisture")->num;
	float biopwr.feedstock.subbit_frac = vt->lookup("biopwr.feedstock.subbit_frac")->num;
	float biopwr.feedstock.subbit_moisture = vt->lookup("biopwr.feedstock.subbit_moisture")->num;
	float biopwr.feedstock.lig_frac = vt->lookup("biopwr.feedstock.lig_frac")->num;
	float biopwr.feedstock.lig_moisture = vt->lookup("biopwr.feedstock.lig_moisture")->num;

	// outputs
	float biopwr.feedstock.total_moisture;

	biopwr.feedstock.total_moisture = biopwr.feedstock.bagasse_frac * biopwr.feedstock.bagasse_moisture / 100.000000 + biopwr.feedstock.barley_frac * biopwr.feedstock.barley_moisture / 100.000000 + biopwr.feedstock.stover_frac * biopwr.feedstock.stover_moisture / 100.000000 + biopwr.feedstock.rice_frac * biopwr.feedstock.rice_moisture / 100.000000 + biopwr.feedstock.wheat_frac * biopwr.feedstock.wheat_moisture / 100.000000 + biopwr.feedstock.forest_frac * biopwr.feedstock.forest_moisture / 100.000000 + biopwr.feedstock.mill_frac * biopwr.feedstock.mill_moisture / 100.000000 + biopwr.feedstock.urban_frac * biopwr.feedstock.urban_moisture / 100.000000 + biopwr.feedstock.woody_frac * biopwr.feedstock.woody_moisture / 100.000000 + biopwr.feedstock.herb_frac * biopwr.feedstock.herb_moisture / 100.000000 + biopwr.feedstock.feedstock1_frac * biopwr.feedstock.feedstock1_moisture / 100.000000 + biopwr.feedstock.feedstock2_frac * biopwr.feedstock.feedstock2_moisture / 100.000000 + biopwr.feedstock.bit_frac * biopwr.feedstock.bit_moisture / 100.000000 + biopwr.feedstock.subbit_frac * biopwr.feedstock.subbit_moisture / 100.000000 + biopwr.feedstock.lig_frac * biopwr.feedstock.lig_moisture / 100.000000;

	return biopwr.feedstock.total_moisture;

}



