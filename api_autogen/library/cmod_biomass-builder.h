#ifndef _CMOD_BIOMASS_BUILDER_H_
#define _CMOD_BIOMASS_BUILDER_H_

#ifdef LK_USE_WXWIDGETS
#include <lk/env.h>
typedef lk::invoke_t invoke_t;
#else
typedef void invoke_t;
#endif

#include "vartab.h"


//
// Evaluates biopwr.plant.nameplate for a Biopower Plant Specifications module
// @param *vt: a var_table* that contains: biopwr.plant.boiler.cap_per_boiler, biopwr.plant.boiler.num, biopwr.plant.boiler.steam_enthalpy, biopwr.plant.rated_eff
// @param[in,out] *cxt: a invoke_t* that for storing the results
// @returns single value or var_table
//
float Biomass_BiopowerPlantSpecifications_biopwr.plant.nameplate_eval(var_table* vt, invoke_t* cxt = 0)


//
// Evaluates biopwr.feedstock.total_moisture for a Biopower Feedstock module
// @param *vt: a var_table* that contains: biopwr.feedstock.bagasse_frac, biopwr.feedstock.bagasse_moisture, biopwr.feedstock.barley_frac, biopwr.feedstock.barley_moisture, biopwr.feedstock.stover_frac, biopwr.feedstock.stover_moisture, biopwr.feedstock.rice_frac, biopwr.feedstock.rice_moisture, biopwr.feedstock.wheat_frac, biopwr.feedstock.wheat_moisture, biopwr.feedstock.forest_frac, biopwr.feedstock.forest_moisture, biopwr.feedstock.mill_frac, biopwr.feedstock.mill_moisture, biopwr.feedstock.urban_frac, biopwr.feedstock.urban_moisture, biopwr.feedstock.woody_frac, biopwr.feedstock.woody_moisture, biopwr.feedstock.herb_frac, biopwr.feedstock.herb_moisture, biopwr.feedstock.feedstock1_frac, biopwr.feedstock.feedstock1_moisture, biopwr.feedstock.feedstock2_frac, biopwr.feedstock.feedstock2_moisture, biopwr.feedstock.bit_frac, biopwr.feedstock.bit_moisture, biopwr.feedstock.subbit_frac, biopwr.feedstock.subbit_moisture, biopwr.feedstock.lig_frac, biopwr.feedstock.lig_moisture
// @param[in,out] *cxt: a invoke_t* that for storing the results
// @returns single value or var_table
//
float Biomass_BiopowerFeedstock_biopwr.feedstock.total_moisture_eval(var_table* vt, invoke_t* cxt = 0)


#endif