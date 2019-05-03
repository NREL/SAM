#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_Biomass.h"

SAM_EXPORT SAM_Biomass SAM_Biomass_construct(const char* def, SAM_error* err){
	SAM_Biomass result = nullptr;
	translateExceptions(err, [&]{
		result = ssc_data_create();
	});
	return result;
}

SAM_EXPORT int SAM_Biomass_execute(SAM_Biomass data, int verbosity, SAM_error* err){
	int n_err = 0;
	translateExceptions(err, [&]{
		n_err += SAM_module_exec("biomass", data, verbosity, err);
	});
	return n_err;
}


SAM_EXPORT void SAM_Biomass_destruct(SAM_Biomass system)
{
	ssc_data_free(system);
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_emissions_avoided_cred_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.emissions.avoided_cred", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_emissions_collection_fuel_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.emissions.collection_fuel", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_emissions_grid_intensity_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.emissions.grid_intensity", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_emissions_pre_chipopt_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.emissions.pre_chipopt", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_emissions_pre_grindopt_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.emissions.pre_grindopt", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_emissions_pre_pelletopt_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.emissions.pre_pelletopt", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_emissions_transport_fuel_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.emissions.transport_fuel", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_emissions_transport_legs_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.emissions.transport_legs", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_emissions_transport_long_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.emissions.transport_long", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_emissions_transport_longmiles_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.emissions.transport_longmiles", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_emissions_transport_longopt_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.emissions.transport_longopt", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_emissions_transport_predist_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.emissions.transport_predist", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_additional_opt_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.feedstock.additional_opt", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_bagasse_frac_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.feedstock.bagasse_frac", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_bagasse_moisture_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.feedstock.bagasse_moisture", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_barley_frac_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.feedstock.barley_frac", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_barley_moisture_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.feedstock.barley_moisture", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_bit_frac_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.feedstock.bit_frac", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_bit_moisture_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.feedstock.bit_moisture", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_collection_radius_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.feedstock.collection_radius", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_feedstock1_c_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.feedstock.feedstock1_c", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_feedstock1_frac_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.feedstock.feedstock1_frac", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_feedstock1_h_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.feedstock.feedstock1_h", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_feedstock1_hhv_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.feedstock.feedstock1_hhv", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_feedstock1_moisture_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.feedstock.feedstock1_moisture", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_feedstock1_resource_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.feedstock.feedstock1_resource", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_feedstock2_c_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.feedstock.feedstock2_c", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_feedstock2_frac_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.feedstock.feedstock2_frac", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_feedstock2_h_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.feedstock.feedstock2_h", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_feedstock2_hhv_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.feedstock.feedstock2_hhv", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_feedstock2_moisture_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.feedstock.feedstock2_moisture", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_feedstock2_resource_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.feedstock.feedstock2_resource", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_forest_frac_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.feedstock.forest_frac", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_forest_moisture_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.feedstock.forest_moisture", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_herb_c_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.feedstock.herb_c", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_herb_frac_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.feedstock.herb_frac", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_herb_moisture_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.feedstock.herb_moisture", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_lig_frac_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.feedstock.lig_frac", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_lig_moisture_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.feedstock.lig_moisture", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_mill_c_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.feedstock.mill_c", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_mill_frac_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.feedstock.mill_frac", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_mill_moisture_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.feedstock.mill_moisture", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_rice_frac_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.feedstock.rice_frac", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_rice_moisture_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.feedstock.rice_moisture", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_stover_frac_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.feedstock.stover_frac", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_stover_moisture_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.feedstock.stover_moisture", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_subbit_frac_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.feedstock.subbit_frac", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_subbit_moisture_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.feedstock.subbit_moisture", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_total_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.feedstock.total", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_total_biomass_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.feedstock.total_biomass", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_total_biomass_c_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.feedstock.total_biomass_c", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_total_c_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.feedstock.total_c", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_total_coal_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.feedstock.total_coal", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_total_h_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.feedstock.total_h", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_total_hhv_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.feedstock.total_hhv", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_total_lhv_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.feedstock.total_lhv", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_total_moisture_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.feedstock.total_moisture", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_urban_c_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.feedstock.urban_c", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_urban_frac_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.feedstock.urban_frac", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_urban_moisture_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.feedstock.urban_moisture", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_wheat_frac_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.feedstock.wheat_frac", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_wheat_moisture_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.feedstock.wheat_moisture", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_woody_c_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.feedstock.woody_c", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_woody_frac_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.feedstock.woody_frac", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_woody_moisture_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.feedstock.woody_moisture", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_boiler_air_feed_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.plant.boiler.air_feed", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_boiler_cap_per_boiler_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.plant.boiler.cap_per_boiler", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_boiler_flue_temp_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.plant.boiler.flue_temp", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_boiler_num_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.plant.boiler.num", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_boiler_over_design_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.plant.boiler.over_design", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_boiler_steam_enthalpy_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.plant.boiler.steam_enthalpy", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_boiler_steam_pressure_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.plant.boiler.steam_pressure", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_combustor_type_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.plant.combustor_type", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_cycle_design_temp_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.plant.cycle_design_temp", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_disp_power_aset(SAM_Biomass ptr, double* arr, int length, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_array(ptr, "biopwr.plant.disp.power", arr, length);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_drying_method_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.plant.drying_method", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_drying_spec_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.plant.drying_spec", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_max_over_design_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.plant.max_over_design", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_min_load_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.plant.min_load", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_nameplate_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.plant.nameplate", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_par_percent_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.plant.par_percent", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_pl_eff_f0_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.plant.pl_eff_f0", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_pl_eff_f1_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.plant.pl_eff_f1", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_pl_eff_f2_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.plant.pl_eff_f2", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_pl_eff_f3_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.plant.pl_eff_f3", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_pl_eff_f4_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.plant.pl_eff_f4", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_ramp_rate_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.plant.ramp_rate", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_rated_eff_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.plant.rated_eff", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_temp_corr_mode_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.plant.temp_corr_mode", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_temp_eff_f0_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.plant.temp_eff_f0", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_temp_eff_f1_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.plant.temp_eff_f1", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_temp_eff_f2_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.plant.temp_eff_f2", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_temp_eff_f3_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.plant.temp_eff_f3", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_temp_eff_f4_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.plant.temp_eff_f4", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_tou_grid_sset(SAM_Biomass ptr, const char* str, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_string(ptr, "biopwr.plant.tou_grid", str);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_tou_option_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "biopwr.plant.tou_option", number);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_file_name_sset(SAM_Biomass ptr, const char* str, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_string(ptr, "file_name", str);
	});
}

SAM_EXPORT void SAM_Biomass_Biopower_system_capacity_nset(SAM_Biomass ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "system_capacity", number);
	});
}

SAM_EXPORT double SAM_Biomass_Biopower_biopwr_emissions_avoided_cred_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.emissions.avoided_cred", &result))
		make_access_error("SAM_Biomass", "biopwr.emissions.avoided_cred");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_emissions_collection_fuel_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.emissions.collection_fuel", &result))
		make_access_error("SAM_Biomass", "biopwr.emissions.collection_fuel");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_emissions_grid_intensity_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.emissions.grid_intensity", &result))
		make_access_error("SAM_Biomass", "biopwr.emissions.grid_intensity");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_emissions_pre_chipopt_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.emissions.pre_chipopt", &result))
		make_access_error("SAM_Biomass", "biopwr.emissions.pre_chipopt");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_emissions_pre_grindopt_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.emissions.pre_grindopt", &result))
		make_access_error("SAM_Biomass", "biopwr.emissions.pre_grindopt");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_emissions_pre_pelletopt_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.emissions.pre_pelletopt", &result))
		make_access_error("SAM_Biomass", "biopwr.emissions.pre_pelletopt");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_emissions_transport_fuel_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.emissions.transport_fuel", &result))
		make_access_error("SAM_Biomass", "biopwr.emissions.transport_fuel");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_emissions_transport_legs_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.emissions.transport_legs", &result))
		make_access_error("SAM_Biomass", "biopwr.emissions.transport_legs");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_emissions_transport_long_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.emissions.transport_long", &result))
		make_access_error("SAM_Biomass", "biopwr.emissions.transport_long");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_emissions_transport_longmiles_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.emissions.transport_longmiles", &result))
		make_access_error("SAM_Biomass", "biopwr.emissions.transport_longmiles");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_emissions_transport_longopt_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.emissions.transport_longopt", &result))
		make_access_error("SAM_Biomass", "biopwr.emissions.transport_longopt");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_emissions_transport_predist_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.emissions.transport_predist", &result))
		make_access_error("SAM_Biomass", "biopwr.emissions.transport_predist");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_additional_opt_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.feedstock.additional_opt", &result))
		make_access_error("SAM_Biomass", "biopwr.feedstock.additional_opt");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_bagasse_frac_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.feedstock.bagasse_frac", &result))
		make_access_error("SAM_Biomass", "biopwr.feedstock.bagasse_frac");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_bagasse_moisture_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.feedstock.bagasse_moisture", &result))
		make_access_error("SAM_Biomass", "biopwr.feedstock.bagasse_moisture");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_barley_frac_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.feedstock.barley_frac", &result))
		make_access_error("SAM_Biomass", "biopwr.feedstock.barley_frac");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_barley_moisture_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.feedstock.barley_moisture", &result))
		make_access_error("SAM_Biomass", "biopwr.feedstock.barley_moisture");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_bit_frac_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.feedstock.bit_frac", &result))
		make_access_error("SAM_Biomass", "biopwr.feedstock.bit_frac");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_bit_moisture_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.feedstock.bit_moisture", &result))
		make_access_error("SAM_Biomass", "biopwr.feedstock.bit_moisture");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_collection_radius_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.feedstock.collection_radius", &result))
		make_access_error("SAM_Biomass", "biopwr.feedstock.collection_radius");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_feedstock1_c_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.feedstock.feedstock1_c", &result))
		make_access_error("SAM_Biomass", "biopwr.feedstock.feedstock1_c");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_feedstock1_frac_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.feedstock.feedstock1_frac", &result))
		make_access_error("SAM_Biomass", "biopwr.feedstock.feedstock1_frac");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_feedstock1_h_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.feedstock.feedstock1_h", &result))
		make_access_error("SAM_Biomass", "biopwr.feedstock.feedstock1_h");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_feedstock1_hhv_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.feedstock.feedstock1_hhv", &result))
		make_access_error("SAM_Biomass", "biopwr.feedstock.feedstock1_hhv");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_feedstock1_moisture_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.feedstock.feedstock1_moisture", &result))
		make_access_error("SAM_Biomass", "biopwr.feedstock.feedstock1_moisture");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_feedstock1_resource_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.feedstock.feedstock1_resource", &result))
		make_access_error("SAM_Biomass", "biopwr.feedstock.feedstock1_resource");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_feedstock2_c_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.feedstock.feedstock2_c", &result))
		make_access_error("SAM_Biomass", "biopwr.feedstock.feedstock2_c");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_feedstock2_frac_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.feedstock.feedstock2_frac", &result))
		make_access_error("SAM_Biomass", "biopwr.feedstock.feedstock2_frac");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_feedstock2_h_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.feedstock.feedstock2_h", &result))
		make_access_error("SAM_Biomass", "biopwr.feedstock.feedstock2_h");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_feedstock2_hhv_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.feedstock.feedstock2_hhv", &result))
		make_access_error("SAM_Biomass", "biopwr.feedstock.feedstock2_hhv");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_feedstock2_moisture_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.feedstock.feedstock2_moisture", &result))
		make_access_error("SAM_Biomass", "biopwr.feedstock.feedstock2_moisture");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_feedstock2_resource_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.feedstock.feedstock2_resource", &result))
		make_access_error("SAM_Biomass", "biopwr.feedstock.feedstock2_resource");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_forest_frac_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.feedstock.forest_frac", &result))
		make_access_error("SAM_Biomass", "biopwr.feedstock.forest_frac");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_forest_moisture_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.feedstock.forest_moisture", &result))
		make_access_error("SAM_Biomass", "biopwr.feedstock.forest_moisture");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_herb_c_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.feedstock.herb_c", &result))
		make_access_error("SAM_Biomass", "biopwr.feedstock.herb_c");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_herb_frac_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.feedstock.herb_frac", &result))
		make_access_error("SAM_Biomass", "biopwr.feedstock.herb_frac");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_herb_moisture_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.feedstock.herb_moisture", &result))
		make_access_error("SAM_Biomass", "biopwr.feedstock.herb_moisture");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_lig_frac_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.feedstock.lig_frac", &result))
		make_access_error("SAM_Biomass", "biopwr.feedstock.lig_frac");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_lig_moisture_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.feedstock.lig_moisture", &result))
		make_access_error("SAM_Biomass", "biopwr.feedstock.lig_moisture");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_mill_c_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.feedstock.mill_c", &result))
		make_access_error("SAM_Biomass", "biopwr.feedstock.mill_c");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_mill_frac_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.feedstock.mill_frac", &result))
		make_access_error("SAM_Biomass", "biopwr.feedstock.mill_frac");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_mill_moisture_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.feedstock.mill_moisture", &result))
		make_access_error("SAM_Biomass", "biopwr.feedstock.mill_moisture");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_rice_frac_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.feedstock.rice_frac", &result))
		make_access_error("SAM_Biomass", "biopwr.feedstock.rice_frac");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_rice_moisture_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.feedstock.rice_moisture", &result))
		make_access_error("SAM_Biomass", "biopwr.feedstock.rice_moisture");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_stover_frac_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.feedstock.stover_frac", &result))
		make_access_error("SAM_Biomass", "biopwr.feedstock.stover_frac");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_stover_moisture_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.feedstock.stover_moisture", &result))
		make_access_error("SAM_Biomass", "biopwr.feedstock.stover_moisture");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_subbit_frac_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.feedstock.subbit_frac", &result))
		make_access_error("SAM_Biomass", "biopwr.feedstock.subbit_frac");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_subbit_moisture_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.feedstock.subbit_moisture", &result))
		make_access_error("SAM_Biomass", "biopwr.feedstock.subbit_moisture");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_total_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.feedstock.total", &result))
		make_access_error("SAM_Biomass", "biopwr.feedstock.total");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_total_biomass_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.feedstock.total_biomass", &result))
		make_access_error("SAM_Biomass", "biopwr.feedstock.total_biomass");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_total_biomass_c_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.feedstock.total_biomass_c", &result))
		make_access_error("SAM_Biomass", "biopwr.feedstock.total_biomass_c");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_total_c_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.feedstock.total_c", &result))
		make_access_error("SAM_Biomass", "biopwr.feedstock.total_c");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_total_coal_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.feedstock.total_coal", &result))
		make_access_error("SAM_Biomass", "biopwr.feedstock.total_coal");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_total_h_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.feedstock.total_h", &result))
		make_access_error("SAM_Biomass", "biopwr.feedstock.total_h");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_total_hhv_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.feedstock.total_hhv", &result))
		make_access_error("SAM_Biomass", "biopwr.feedstock.total_hhv");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_total_lhv_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.feedstock.total_lhv", &result))
		make_access_error("SAM_Biomass", "biopwr.feedstock.total_lhv");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_total_moisture_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.feedstock.total_moisture", &result))
		make_access_error("SAM_Biomass", "biopwr.feedstock.total_moisture");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_urban_c_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.feedstock.urban_c", &result))
		make_access_error("SAM_Biomass", "biopwr.feedstock.urban_c");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_urban_frac_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.feedstock.urban_frac", &result))
		make_access_error("SAM_Biomass", "biopwr.feedstock.urban_frac");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_urban_moisture_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.feedstock.urban_moisture", &result))
		make_access_error("SAM_Biomass", "biopwr.feedstock.urban_moisture");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_wheat_frac_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.feedstock.wheat_frac", &result))
		make_access_error("SAM_Biomass", "biopwr.feedstock.wheat_frac");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_wheat_moisture_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.feedstock.wheat_moisture", &result))
		make_access_error("SAM_Biomass", "biopwr.feedstock.wheat_moisture");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_woody_c_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.feedstock.woody_c", &result))
		make_access_error("SAM_Biomass", "biopwr.feedstock.woody_c");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_woody_frac_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.feedstock.woody_frac", &result))
		make_access_error("SAM_Biomass", "biopwr.feedstock.woody_frac");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_woody_moisture_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.feedstock.woody_moisture", &result))
		make_access_error("SAM_Biomass", "biopwr.feedstock.woody_moisture");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_plant_boiler_air_feed_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.plant.boiler.air_feed", &result))
		make_access_error("SAM_Biomass", "biopwr.plant.boiler.air_feed");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_plant_boiler_cap_per_boiler_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.plant.boiler.cap_per_boiler", &result))
		make_access_error("SAM_Biomass", "biopwr.plant.boiler.cap_per_boiler");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_plant_boiler_flue_temp_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.plant.boiler.flue_temp", &result))
		make_access_error("SAM_Biomass", "biopwr.plant.boiler.flue_temp");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_plant_boiler_num_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.plant.boiler.num", &result))
		make_access_error("SAM_Biomass", "biopwr.plant.boiler.num");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_plant_boiler_over_design_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.plant.boiler.over_design", &result))
		make_access_error("SAM_Biomass", "biopwr.plant.boiler.over_design");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_plant_boiler_steam_enthalpy_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.plant.boiler.steam_enthalpy", &result))
		make_access_error("SAM_Biomass", "biopwr.plant.boiler.steam_enthalpy");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_plant_boiler_steam_pressure_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.plant.boiler.steam_pressure", &result))
		make_access_error("SAM_Biomass", "biopwr.plant.boiler.steam_pressure");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_plant_combustor_type_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.plant.combustor_type", &result))
		make_access_error("SAM_Biomass", "biopwr.plant.combustor_type");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_plant_cycle_design_temp_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.plant.cycle_design_temp", &result))
		make_access_error("SAM_Biomass", "biopwr.plant.cycle_design_temp");
	});
	return result;
}



SAM_EXPORT double* SAM_Biomass_Biopower_biopwr_plant_disp_power_aget(SAM_Biomass ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "biopwr.plant.disp.power", length);
	if (!result)
		make_access_error("SAM_Biomass", "biopwr.plant.disp.power");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_plant_drying_method_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.plant.drying_method", &result))
		make_access_error("SAM_Biomass", "biopwr.plant.drying_method");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_plant_drying_spec_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.plant.drying_spec", &result))
		make_access_error("SAM_Biomass", "biopwr.plant.drying_spec");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_plant_max_over_design_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.plant.max_over_design", &result))
		make_access_error("SAM_Biomass", "biopwr.plant.max_over_design");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_plant_min_load_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.plant.min_load", &result))
		make_access_error("SAM_Biomass", "biopwr.plant.min_load");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_plant_nameplate_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.plant.nameplate", &result))
		make_access_error("SAM_Biomass", "biopwr.plant.nameplate");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_plant_par_percent_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.plant.par_percent", &result))
		make_access_error("SAM_Biomass", "biopwr.plant.par_percent");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_plant_pl_eff_f0_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.plant.pl_eff_f0", &result))
		make_access_error("SAM_Biomass", "biopwr.plant.pl_eff_f0");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_plant_pl_eff_f1_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.plant.pl_eff_f1", &result))
		make_access_error("SAM_Biomass", "biopwr.plant.pl_eff_f1");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_plant_pl_eff_f2_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.plant.pl_eff_f2", &result))
		make_access_error("SAM_Biomass", "biopwr.plant.pl_eff_f2");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_plant_pl_eff_f3_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.plant.pl_eff_f3", &result))
		make_access_error("SAM_Biomass", "biopwr.plant.pl_eff_f3");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_plant_pl_eff_f4_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.plant.pl_eff_f4", &result))
		make_access_error("SAM_Biomass", "biopwr.plant.pl_eff_f4");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_plant_ramp_rate_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.plant.ramp_rate", &result))
		make_access_error("SAM_Biomass", "biopwr.plant.ramp_rate");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_plant_rated_eff_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.plant.rated_eff", &result))
		make_access_error("SAM_Biomass", "biopwr.plant.rated_eff");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_plant_temp_corr_mode_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.plant.temp_corr_mode", &result))
		make_access_error("SAM_Biomass", "biopwr.plant.temp_corr_mode");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_plant_temp_eff_f0_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.plant.temp_eff_f0", &result))
		make_access_error("SAM_Biomass", "biopwr.plant.temp_eff_f0");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_plant_temp_eff_f1_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.plant.temp_eff_f1", &result))
		make_access_error("SAM_Biomass", "biopwr.plant.temp_eff_f1");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_plant_temp_eff_f2_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.plant.temp_eff_f2", &result))
		make_access_error("SAM_Biomass", "biopwr.plant.temp_eff_f2");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_plant_temp_eff_f3_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.plant.temp_eff_f3", &result))
		make_access_error("SAM_Biomass", "biopwr.plant.temp_eff_f3");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_plant_temp_eff_f4_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.plant.temp_eff_f4", &result))
		make_access_error("SAM_Biomass", "biopwr.plant.temp_eff_f4");
	});
	return result;
}



SAM_EXPORT const char* SAM_Biomass_Biopower_biopwr_plant_tou_grid_sget(SAM_Biomass ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "biopwr.plant.tou_grid");
	if (!result)
		make_access_error("SAM_Biomass", "biopwr.plant.tou_grid");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_biopwr_plant_tou_option_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "biopwr.plant.tou_option", &result))
		make_access_error("SAM_Biomass", "biopwr.plant.tou_option");
	});
	return result;
}



SAM_EXPORT const char* SAM_Biomass_Biopower_file_name_sget(SAM_Biomass ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "file_name");
	if (!result)
		make_access_error("SAM_Biomass", "file_name");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Biopower_system_capacity_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_capacity", &result))
		make_access_error("SAM_Biomass", "system_capacity");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Outputs_annual_energy_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_energy", &result))
		make_access_error("SAM_Biomass", "annual_energy");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Outputs_annual_fuel_usage_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_fuel_usage", &result))
		make_access_error("SAM_Biomass", "annual_fuel_usage");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Outputs_annual_watter_usage_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "annual_watter_usage", &result))
		make_access_error("SAM_Biomass", "annual_watter_usage");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Outputs_capacity_factor_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "capacity_factor", &result))
		make_access_error("SAM_Biomass", "capacity_factor");
	});
	return result;
}



SAM_EXPORT double* SAM_Biomass_Outputs_gen_aget(SAM_Biomass ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "gen", length);
	if (!result)
		make_access_error("SAM_Biomass", "gen");
	});
	return result;
}



SAM_EXPORT double* SAM_Biomass_Outputs_hourly_boiler_eff_aget(SAM_Biomass ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "hourly_boiler_eff", length);
	if (!result)
		make_access_error("SAM_Biomass", "hourly_boiler_eff");
	});
	return result;
}



SAM_EXPORT double* SAM_Biomass_Outputs_hourly_pbeta_aget(SAM_Biomass ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "hourly_pbeta", length);
	if (!result)
		make_access_error("SAM_Biomass", "hourly_pbeta");
	});
	return result;
}



SAM_EXPORT double* SAM_Biomass_Outputs_hourly_q_to_pb_aget(SAM_Biomass ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "hourly_q_to_pb", length);
	if (!result)
		make_access_error("SAM_Biomass", "hourly_q_to_pb");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Outputs_kwh_per_kw_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "kwh_per_kw", &result))
		make_access_error("SAM_Biomass", "kwh_per_kw");
	});
	return result;
}



SAM_EXPORT double* SAM_Biomass_Outputs_monthly_bagasse_emc_aget(SAM_Biomass ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_bagasse_emc", length);
	if (!result)
		make_access_error("SAM_Biomass", "monthly_bagasse_emc");
	});
	return result;
}



SAM_EXPORT double* SAM_Biomass_Outputs_monthly_barley_emc_aget(SAM_Biomass ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_barley_emc", length);
	if (!result)
		make_access_error("SAM_Biomass", "monthly_barley_emc");
	});
	return result;
}



SAM_EXPORT double* SAM_Biomass_Outputs_monthly_boiler_eff_aget(SAM_Biomass ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_boiler_eff", length);
	if (!result)
		make_access_error("SAM_Biomass", "monthly_boiler_eff");
	});
	return result;
}



SAM_EXPORT double* SAM_Biomass_Outputs_monthly_energy_aget(SAM_Biomass ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_energy", length);
	if (!result)
		make_access_error("SAM_Biomass", "monthly_energy");
	});
	return result;
}



SAM_EXPORT double* SAM_Biomass_Outputs_monthly_forest_emc_aget(SAM_Biomass ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_forest_emc", length);
	if (!result)
		make_access_error("SAM_Biomass", "monthly_forest_emc");
	});
	return result;
}



SAM_EXPORT double* SAM_Biomass_Outputs_monthly_herb_emc_aget(SAM_Biomass ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_herb_emc", length);
	if (!result)
		make_access_error("SAM_Biomass", "monthly_herb_emc");
	});
	return result;
}



SAM_EXPORT double* SAM_Biomass_Outputs_monthly_hhv_heatrate_aget(SAM_Biomass ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_hhv_heatrate", length);
	if (!result)
		make_access_error("SAM_Biomass", "monthly_hhv_heatrate");
	});
	return result;
}



SAM_EXPORT double* SAM_Biomass_Outputs_monthly_lhv_heatrate_aget(SAM_Biomass ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_lhv_heatrate", length);
	if (!result)
		make_access_error("SAM_Biomass", "monthly_lhv_heatrate");
	});
	return result;
}



SAM_EXPORT double* SAM_Biomass_Outputs_monthly_mill_emc_aget(SAM_Biomass ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_mill_emc", length);
	if (!result)
		make_access_error("SAM_Biomass", "monthly_mill_emc");
	});
	return result;
}



SAM_EXPORT double* SAM_Biomass_Outputs_monthly_moist_aget(SAM_Biomass ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_moist", length);
	if (!result)
		make_access_error("SAM_Biomass", "monthly_moist");
	});
	return result;
}



SAM_EXPORT double* SAM_Biomass_Outputs_monthly_pb_eta_aget(SAM_Biomass ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_pb_eta", length);
	if (!result)
		make_access_error("SAM_Biomass", "monthly_pb_eta");
	});
	return result;
}



SAM_EXPORT double* SAM_Biomass_Outputs_monthly_q_to_pb_aget(SAM_Biomass ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_q_to_pb", length);
	if (!result)
		make_access_error("SAM_Biomass", "monthly_q_to_pb");
	});
	return result;
}



SAM_EXPORT double* SAM_Biomass_Outputs_monthly_rh_aget(SAM_Biomass ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_rh", length);
	if (!result)
		make_access_error("SAM_Biomass", "monthly_rh");
	});
	return result;
}



SAM_EXPORT double* SAM_Biomass_Outputs_monthly_rice_emc_aget(SAM_Biomass ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_rice_emc", length);
	if (!result)
		make_access_error("SAM_Biomass", "monthly_rice_emc");
	});
	return result;
}



SAM_EXPORT double* SAM_Biomass_Outputs_monthly_stover_emc_aget(SAM_Biomass ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_stover_emc", length);
	if (!result)
		make_access_error("SAM_Biomass", "monthly_stover_emc");
	});
	return result;
}



SAM_EXPORT double* SAM_Biomass_Outputs_monthly_temp_c_aget(SAM_Biomass ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_temp_c", length);
	if (!result)
		make_access_error("SAM_Biomass", "monthly_temp_c");
	});
	return result;
}



SAM_EXPORT double* SAM_Biomass_Outputs_monthly_urban_emc_aget(SAM_Biomass ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_urban_emc", length);
	if (!result)
		make_access_error("SAM_Biomass", "monthly_urban_emc");
	});
	return result;
}



SAM_EXPORT double* SAM_Biomass_Outputs_monthly_wheat_emc_aget(SAM_Biomass ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_wheat_emc", length);
	if (!result)
		make_access_error("SAM_Biomass", "monthly_wheat_emc");
	});
	return result;
}



SAM_EXPORT double* SAM_Biomass_Outputs_monthly_woody_emc_aget(SAM_Biomass ptr, int* length, SAM_error *err){
	double* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_array(ptr, "monthly_woody_emc", length);
	if (!result)
		make_access_error("SAM_Biomass", "monthly_woody_emc");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Outputs_system_annual_ash_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system.annual.ash", &result))
		make_access_error("SAM_Biomass", "system.annual.ash");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Outputs_system_annual_biomass_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system.annual.biomass", &result))
		make_access_error("SAM_Biomass", "system.annual.biomass");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Outputs_system_annual_boiler_loss_dry_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system.annual.boiler_loss_dry", &result))
		make_access_error("SAM_Biomass", "system.annual.boiler_loss_dry");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Outputs_system_annual_boiler_loss_dry_kwh_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system.annual.boiler_loss_dry_kwh", &result))
		make_access_error("SAM_Biomass", "system.annual.boiler_loss_dry_kwh");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Outputs_system_annual_boiler_loss_fuel_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system.annual.boiler_loss_fuel", &result))
		make_access_error("SAM_Biomass", "system.annual.boiler_loss_fuel");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Outputs_system_annual_boiler_loss_fuel_kwh_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system.annual.boiler_loss_fuel_kwh", &result))
		make_access_error("SAM_Biomass", "system.annual.boiler_loss_fuel_kwh");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Outputs_system_annual_boiler_loss_manu_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system.annual.boiler_loss_manu", &result))
		make_access_error("SAM_Biomass", "system.annual.boiler_loss_manu");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Outputs_system_annual_boiler_loss_manu_kwh_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system.annual.boiler_loss_manu_kwh", &result))
		make_access_error("SAM_Biomass", "system.annual.boiler_loss_manu_kwh");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Outputs_system_annual_boiler_loss_rad_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system.annual.boiler_loss_rad", &result))
		make_access_error("SAM_Biomass", "system.annual.boiler_loss_rad");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Outputs_system_annual_boiler_loss_rad_kwh_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system.annual.boiler_loss_rad_kwh", &result))
		make_access_error("SAM_Biomass", "system.annual.boiler_loss_rad_kwh");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Outputs_system_annual_boiler_loss_total_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system.annual.boiler_loss_total", &result))
		make_access_error("SAM_Biomass", "system.annual.boiler_loss_total");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Outputs_system_annual_boiler_loss_total_kwh_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system.annual.boiler_loss_total_kwh", &result))
		make_access_error("SAM_Biomass", "system.annual.boiler_loss_total_kwh");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Outputs_system_annual_boiler_loss_unburn_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system.annual.boiler_loss_unburn", &result))
		make_access_error("SAM_Biomass", "system.annual.boiler_loss_unburn");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Outputs_system_annual_boiler_loss_unburn_kwh_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system.annual.boiler_loss_unburn_kwh", &result))
		make_access_error("SAM_Biomass", "system.annual.boiler_loss_unburn_kwh");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Outputs_system_annual_boiler_loss_wet_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system.annual.boiler_loss_wet", &result))
		make_access_error("SAM_Biomass", "system.annual.boiler_loss_wet");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Outputs_system_annual_boiler_loss_wet_kwh_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system.annual.boiler_loss_wet_kwh", &result))
		make_access_error("SAM_Biomass", "system.annual.boiler_loss_wet_kwh");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Outputs_system_annual_boiler_output_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system.annual.boiler_output", &result))
		make_access_error("SAM_Biomass", "system.annual.boiler_output");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Outputs_system_annual_coal_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system.annual.coal", &result))
		make_access_error("SAM_Biomass", "system.annual.coal");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Outputs_system_annual_e_net_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system.annual.e_net", &result))
		make_access_error("SAM_Biomass", "system.annual.e_net");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Outputs_system_annual_par_loss_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system.annual.par_loss", &result))
		make_access_error("SAM_Biomass", "system.annual.par_loss");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Outputs_system_annual_par_loss_kwh_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system.annual.par_loss_kwh", &result))
		make_access_error("SAM_Biomass", "system.annual.par_loss_kwh");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Outputs_system_annual_pb_eta_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system.annual.pb_eta", &result))
		make_access_error("SAM_Biomass", "system.annual.pb_eta");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Outputs_system_annual_pb_eta_kwh_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system.annual.pb_eta_kwh", &result))
		make_access_error("SAM_Biomass", "system.annual.pb_eta_kwh");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Outputs_system_annual_qtoboil_tot_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system.annual.qtoboil_tot", &result))
		make_access_error("SAM_Biomass", "system.annual.qtoboil_tot");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Outputs_system_annual_qtopb_tot_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system.annual.qtopb_tot", &result))
		make_access_error("SAM_Biomass", "system.annual.qtopb_tot");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Outputs_system_annual_turbine_output_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system.annual.turbine_output", &result))
		make_access_error("SAM_Biomass", "system.annual.turbine_output");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Outputs_system_capfactor_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system.capfactor", &result))
		make_access_error("SAM_Biomass", "system.capfactor");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Outputs_system_emissions_avoided_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system.emissions.avoided", &result))
		make_access_error("SAM_Biomass", "system.emissions.avoided");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Outputs_system_emissions_biodiesel_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system.emissions.biodiesel", &result))
		make_access_error("SAM_Biomass", "system.emissions.biodiesel");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Outputs_system_emissions_bunker_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system.emissions.bunker", &result))
		make_access_error("SAM_Biomass", "system.emissions.bunker");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Outputs_system_emissions_combustion_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system.emissions.combustion", &result))
		make_access_error("SAM_Biomass", "system.emissions.combustion");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Outputs_system_emissions_diesel_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system.emissions.diesel", &result))
		make_access_error("SAM_Biomass", "system.emissions.diesel");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Outputs_system_emissions_drying_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system.emissions.drying", &result))
		make_access_error("SAM_Biomass", "system.emissions.drying");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Outputs_system_emissions_ems_per_lb_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system.emissions.ems_per_lb", &result))
		make_access_error("SAM_Biomass", "system.emissions.ems_per_lb");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Outputs_system_emissions_growth_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system.emissions.growth", &result))
		make_access_error("SAM_Biomass", "system.emissions.growth");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Outputs_system_emissions_lime_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system.emissions.lime", &result))
		make_access_error("SAM_Biomass", "system.emissions.lime");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Outputs_system_emissions_naturalgas_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system.emissions.naturalgas", &result))
		make_access_error("SAM_Biomass", "system.emissions.naturalgas");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Outputs_system_emissions_nitrogen_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system.emissions.nitrogen", &result))
		make_access_error("SAM_Biomass", "system.emissions.nitrogen");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Outputs_system_emissions_oil_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system.emissions.oil", &result))
		make_access_error("SAM_Biomass", "system.emissions.oil");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Outputs_system_emissions_phosphorus_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system.emissions.phosphorus", &result))
		make_access_error("SAM_Biomass", "system.emissions.phosphorus");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Outputs_system_emissions_potassium_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system.emissions.potassium", &result))
		make_access_error("SAM_Biomass", "system.emissions.potassium");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Outputs_system_emissions_preprocessing_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system.emissions.preprocessing", &result))
		make_access_error("SAM_Biomass", "system.emissions.preprocessing");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Outputs_system_emissions_total_sum_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system.emissions.total_sum", &result))
		make_access_error("SAM_Biomass", "system.emissions.total_sum");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Outputs_system_emissions_transport_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system.emissions.transport", &result))
		make_access_error("SAM_Biomass", "system.emissions.transport");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Outputs_system_emissions_uptake_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system.emissions.uptake", &result))
		make_access_error("SAM_Biomass", "system.emissions.uptake");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Outputs_system_heat_rate_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system_heat_rate", &result))
		make_access_error("SAM_Biomass", "system_heat_rate");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Outputs_system_hhv_heatrate_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system.hhv_heatrate", &result))
		make_access_error("SAM_Biomass", "system.hhv_heatrate");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Outputs_system_hhv_thermeff_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system.hhv_thermeff", &result))
		make_access_error("SAM_Biomass", "system.hhv_thermeff");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Outputs_system_lhv_heatrate_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system.lhv_heatrate", &result))
		make_access_error("SAM_Biomass", "system.lhv_heatrate");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Outputs_system_lhv_thermeff_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system.lhv_thermeff", &result))
		make_access_error("SAM_Biomass", "system.lhv_thermeff");
	});
	return result;
}



SAM_EXPORT double SAM_Biomass_Outputs_system_total_moisture_nget(SAM_Biomass ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "system.total_moisture", &result))
		make_access_error("SAM_Biomass", "system.total_moisture");
	});
	return result;
}



