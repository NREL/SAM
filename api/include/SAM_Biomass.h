#ifndef SAM_BIOMASS_H_
#define SAM_BIOMASS_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// Biomass Technology Model
	//

	/** 
	 * Create a Biomass variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_Biomass;

	SAM_EXPORT SAM_Biomass SAM_Biomass_construct(const char* def, SAM_error* err);

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_Biomass_execute(SAM_Biomass data, int verbosity, SAM_error* err);

	SAM_EXPORT void SAM_Biomass_destruct(SAM_Biomass system);


	//
	// Biopower parameters
	//

	/**
	 * Set biopwr.emissions.avoided_cred: 
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_emissions_avoided_cred_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.emissions.collection_fuel: 
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_emissions_collection_fuel_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.emissions.grid_intensity: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_emissions_grid_intensity_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.emissions.pre_chipopt: 
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_emissions_pre_chipopt_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.emissions.pre_grindopt: 
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_emissions_pre_grindopt_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.emissions.pre_pelletopt: 
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_emissions_pre_pelletopt_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.emissions.transport_fuel: 
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_emissions_transport_fuel_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.emissions.transport_legs: 
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_emissions_transport_legs_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.emissions.transport_long: 
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_emissions_transport_long_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.emissions.transport_longmiles: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_emissions_transport_longmiles_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.emissions.transport_longopt: 
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_emissions_transport_longopt_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.emissions.transport_predist: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_emissions_transport_predist_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.additional_opt: 
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_additional_opt_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.bagasse_frac: Bagasse feedstock fraction
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_bagasse_frac_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.bagasse_moisture: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_bagasse_moisture_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.barley_frac: Barley feedstock fraction
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_barley_frac_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.barley_moisture: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_barley_moisture_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.bit_frac: Bituminos coal feedstock fraction
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_bit_frac_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.bit_moisture: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_bit_moisture_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.collection_radius: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_collection_radius_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.feedstock1_c: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_feedstock1_c_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.feedstock1_frac: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_feedstock1_frac_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.feedstock1_h: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_feedstock1_h_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.feedstock1_hhv: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_feedstock1_hhv_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.feedstock1_moisture: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_feedstock1_moisture_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.feedstock1_resource: Opt feedstock 1 (dt/yr)
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_feedstock1_resource_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.feedstock2_c: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_feedstock2_c_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.feedstock2_frac: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_feedstock2_frac_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.feedstock2_h: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_feedstock2_h_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.feedstock2_hhv: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_feedstock2_hhv_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.feedstock2_moisture: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_feedstock2_moisture_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.feedstock2_resource: Opt feedstock 2 (dt/yr)
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_feedstock2_resource_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.forest_frac: Forest residue feedstock fraction
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_forest_frac_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.forest_moisture: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_forest_moisture_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.herb_c: Carbon fraction in herbaceous energy crop
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_herb_c_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.herb_frac: Herbaceous energy crop feedstock fraction
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_herb_frac_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.herb_moisture: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_herb_moisture_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.lig_frac: Lignite coal feedstock fraction
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_lig_frac_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.lig_moisture: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_lig_moisture_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.mill_c: Carbon fraction in mill residue
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_mill_c_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.mill_frac: Mill residue feedstock fraction
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_mill_frac_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.mill_moisture: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_mill_moisture_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.rice_frac: Rice straw feedstock fraction
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_rice_frac_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.rice_moisture: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_rice_moisture_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.stover_frac: Stover feedstock fraction
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_stover_frac_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.stover_moisture: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_stover_moisture_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.subbit_frac: Sub-bituminous coal feedstock fraction
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_subbit_frac_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.subbit_moisture: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_subbit_moisture_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.total: Total fuel resource (dt/yr)
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_total_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.total_biomass: Total biomass resource (dt/yr)
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_total_biomass_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.total_biomass_c: Biomass fraction carbon
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_total_biomass_c_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.total_c: Mass fraction carbon
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_total_c_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.total_coal: Total coal resource (dt/yr)
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_total_coal_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.total_h: Mass fraction hydrogen
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_total_h_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.total_hhv: Dry feedstock HHV (Btu/lb)
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_total_hhv_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.total_lhv: Dry feedstock LHV (Btu/lb)
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_total_lhv_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.total_moisture: Overall Moisture Content (dry %)
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_total_moisture_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.urban_c: Carbon fraction in urban residue
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_urban_c_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.urban_frac: Urban wood residue feedstock fraction
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_urban_frac_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.urban_moisture: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_urban_moisture_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.wheat_frac: Wheat straw feedstock fraction
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_wheat_frac_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.wheat_moisture: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_wheat_moisture_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.woody_c: Carbon fraction in woody energy crop
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_woody_c_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.woody_frac: Woody energy crop feedstock fraction
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_woody_frac_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.woody_moisture: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_woody_moisture_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.plant.boiler.air_feed: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_boiler_air_feed_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.plant.boiler.cap_per_boiler: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_boiler_cap_per_boiler_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.plant.boiler.flue_temp: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_boiler_flue_temp_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.plant.boiler.num: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_boiler_num_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.plant.boiler.over_design: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_boiler_over_design_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.plant.boiler.steam_enthalpy: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_boiler_steam_enthalpy_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.plant.boiler.steam_pressure: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_boiler_steam_pressure_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.plant.combustor_type: 
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_combustor_type_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.plant.cycle_design_temp: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_cycle_design_temp_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.plant.disp.power: 
	 * options: None
	 * constraints: LENGTH=9
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_disp_power_aset(SAM_Biomass ptr, float* arr, int length, SAM_error *err);

	/**
	 * Set biopwr.plant.drying_method: 
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_drying_method_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.plant.drying_spec: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_drying_spec_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.plant.max_over_design: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_max_over_design_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.plant.min_load: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_min_load_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.plant.nameplate: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_nameplate_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.plant.par_percent: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_par_percent_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.plant.pl_eff_f0: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_pl_eff_f0_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.plant.pl_eff_f1: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_pl_eff_f1_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.plant.pl_eff_f2: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_pl_eff_f2_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.plant.pl_eff_f3: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_pl_eff_f3_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.plant.pl_eff_f4: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_pl_eff_f4_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.plant.ramp_rate: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_ramp_rate_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.plant.rated_eff: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_rated_eff_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.plant.temp_corr_mode: 
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_temp_corr_mode_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.plant.temp_eff_f0: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_temp_eff_f0_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.plant.temp_eff_f1: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_temp_eff_f1_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.plant.temp_eff_f2: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_temp_eff_f2_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.plant.temp_eff_f3: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_temp_eff_f3_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.plant.temp_eff_f4: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_temp_eff_f4_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set biopwr.plant.tou_grid: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_tou_grid_sset(SAM_Biomass ptr, const char* str, SAM_error *err);

	/**
	 * Set biopwr.plant.tou_option: 
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_tou_option_fset(SAM_Biomass ptr, float number, SAM_error *err);

	/**
	 * Set file_name: Local weather file path
	 * options: None
	 * constraints: LOCAL_FILE
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_file_name_sset(SAM_Biomass ptr, const char* str, SAM_error *err);

	/**
	 * Set system_capacity: Nameplate capacity [kW]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_system_capacity_fset(SAM_Biomass ptr, float number, SAM_error *err);


	/**
	 * Biopower Getters
	 */

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_emissions_avoided_cred_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_emissions_collection_fuel_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_emissions_grid_intensity_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_emissions_pre_chipopt_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_emissions_pre_grindopt_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_emissions_pre_pelletopt_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_emissions_transport_fuel_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_emissions_transport_legs_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_emissions_transport_long_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_emissions_transport_longmiles_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_emissions_transport_longopt_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_emissions_transport_predist_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_feedstock_additional_opt_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_feedstock_bagasse_frac_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_feedstock_bagasse_moisture_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_feedstock_barley_frac_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_feedstock_barley_moisture_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_feedstock_bit_frac_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_feedstock_bit_moisture_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_feedstock_collection_radius_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_feedstock_feedstock1_c_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_feedstock_feedstock1_frac_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_feedstock_feedstock1_h_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_feedstock_feedstock1_hhv_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_feedstock_feedstock1_moisture_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_feedstock_feedstock1_resource_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_feedstock_feedstock2_c_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_feedstock_feedstock2_frac_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_feedstock_feedstock2_h_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_feedstock_feedstock2_hhv_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_feedstock_feedstock2_moisture_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_feedstock_feedstock2_resource_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_feedstock_forest_frac_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_feedstock_forest_moisture_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_feedstock_herb_c_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_feedstock_herb_frac_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_feedstock_herb_moisture_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_feedstock_lig_frac_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_feedstock_lig_moisture_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_feedstock_mill_c_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_feedstock_mill_frac_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_feedstock_mill_moisture_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_feedstock_rice_frac_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_feedstock_rice_moisture_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_feedstock_stover_frac_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_feedstock_stover_moisture_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_feedstock_subbit_frac_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_feedstock_subbit_moisture_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_feedstock_total_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_feedstock_total_biomass_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_feedstock_total_biomass_c_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_feedstock_total_c_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_feedstock_total_coal_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_feedstock_total_h_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_feedstock_total_hhv_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_feedstock_total_lhv_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_feedstock_total_moisture_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_feedstock_urban_c_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_feedstock_urban_frac_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_feedstock_urban_moisture_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_feedstock_wheat_frac_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_feedstock_wheat_moisture_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_feedstock_woody_c_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_feedstock_woody_frac_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_feedstock_woody_moisture_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_plant_boiler_air_feed_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_plant_boiler_cap_per_boiler_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_plant_boiler_flue_temp_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_plant_boiler_num_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_plant_boiler_over_design_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_plant_boiler_steam_enthalpy_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_plant_boiler_steam_pressure_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_plant_combustor_type_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_plant_cycle_design_temp_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float* SAM_Biomass_Biopower_biopwr_plant_disp_power_aget(SAM_Biomass ptr, int* length, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_plant_drying_method_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_plant_drying_spec_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_plant_max_over_design_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_plant_min_load_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_plant_nameplate_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_plant_par_percent_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_plant_pl_eff_f0_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_plant_pl_eff_f1_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_plant_pl_eff_f2_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_plant_pl_eff_f3_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_plant_pl_eff_f4_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_plant_ramp_rate_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_plant_rated_eff_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_plant_temp_corr_mode_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_plant_temp_eff_f0_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_plant_temp_eff_f1_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_plant_temp_eff_f2_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_plant_temp_eff_f3_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_plant_temp_eff_f4_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT const char* SAM_Biomass_Biopower_biopwr_plant_tou_grid_sget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_biopwr_plant_tou_option_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT const char* SAM_Biomass_Biopower_file_name_sget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Biopower_system_capacity_fget(SAM_Biomass ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT float SAM_Biomass_Outputs_annual_energy_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Outputs_annual_fuel_usage_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Outputs_annual_watter_usage_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Outputs_capacity_factor_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float* SAM_Biomass_Outputs_gen_aget(SAM_Biomass ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_Biomass_Outputs_hourly_boiler_eff_aget(SAM_Biomass ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_Biomass_Outputs_hourly_pbeta_aget(SAM_Biomass ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_Biomass_Outputs_hourly_q_to_pb_aget(SAM_Biomass ptr, int* length, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Outputs_kwh_per_kw_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float* SAM_Biomass_Outputs_monthly_bagasse_emc_aget(SAM_Biomass ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_Biomass_Outputs_monthly_barley_emc_aget(SAM_Biomass ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_Biomass_Outputs_monthly_boiler_eff_aget(SAM_Biomass ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_Biomass_Outputs_monthly_energy_aget(SAM_Biomass ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_Biomass_Outputs_monthly_forest_emc_aget(SAM_Biomass ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_Biomass_Outputs_monthly_herb_emc_aget(SAM_Biomass ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_Biomass_Outputs_monthly_hhv_heatrate_aget(SAM_Biomass ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_Biomass_Outputs_monthly_lhv_heatrate_aget(SAM_Biomass ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_Biomass_Outputs_monthly_mill_emc_aget(SAM_Biomass ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_Biomass_Outputs_monthly_moist_aget(SAM_Biomass ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_Biomass_Outputs_monthly_pb_eta_aget(SAM_Biomass ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_Biomass_Outputs_monthly_q_to_pb_aget(SAM_Biomass ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_Biomass_Outputs_monthly_rh_aget(SAM_Biomass ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_Biomass_Outputs_monthly_rice_emc_aget(SAM_Biomass ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_Biomass_Outputs_monthly_stover_emc_aget(SAM_Biomass ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_Biomass_Outputs_monthly_temp_c_aget(SAM_Biomass ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_Biomass_Outputs_monthly_urban_emc_aget(SAM_Biomass ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_Biomass_Outputs_monthly_wheat_emc_aget(SAM_Biomass ptr, int* length, SAM_error *err);

	SAM_EXPORT float* SAM_Biomass_Outputs_monthly_woody_emc_aget(SAM_Biomass ptr, int* length, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Outputs_system_annual_ash_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Outputs_system_annual_biomass_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Outputs_system_annual_boiler_loss_dry_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Outputs_system_annual_boiler_loss_dry_kwh_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Outputs_system_annual_boiler_loss_fuel_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Outputs_system_annual_boiler_loss_fuel_kwh_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Outputs_system_annual_boiler_loss_manu_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Outputs_system_annual_boiler_loss_manu_kwh_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Outputs_system_annual_boiler_loss_rad_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Outputs_system_annual_boiler_loss_rad_kwh_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Outputs_system_annual_boiler_loss_total_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Outputs_system_annual_boiler_loss_total_kwh_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Outputs_system_annual_boiler_loss_unburn_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Outputs_system_annual_boiler_loss_unburn_kwh_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Outputs_system_annual_boiler_loss_wet_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Outputs_system_annual_boiler_loss_wet_kwh_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Outputs_system_annual_boiler_output_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Outputs_system_annual_coal_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Outputs_system_annual_e_net_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Outputs_system_annual_par_loss_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Outputs_system_annual_par_loss_kwh_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Outputs_system_annual_pb_eta_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Outputs_system_annual_pb_eta_kwh_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Outputs_system_annual_qtoboil_tot_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Outputs_system_annual_qtopb_tot_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Outputs_system_annual_turbine_output_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Outputs_system_capfactor_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Outputs_system_emissions_avoided_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Outputs_system_emissions_biodiesel_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Outputs_system_emissions_bunker_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Outputs_system_emissions_combustion_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Outputs_system_emissions_diesel_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Outputs_system_emissions_drying_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Outputs_system_emissions_ems_per_lb_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Outputs_system_emissions_growth_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Outputs_system_emissions_lime_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Outputs_system_emissions_naturalgas_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Outputs_system_emissions_nitrogen_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Outputs_system_emissions_oil_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Outputs_system_emissions_phosphorus_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Outputs_system_emissions_potassium_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Outputs_system_emissions_preprocessing_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Outputs_system_emissions_total_sum_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Outputs_system_emissions_transport_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Outputs_system_emissions_uptake_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Outputs_system_heat_rate_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Outputs_system_hhv_heatrate_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Outputs_system_hhv_thermeff_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Outputs_system_lhv_heatrate_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Outputs_system_lhv_thermeff_fget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT float SAM_Biomass_Outputs_system_total_moisture_fget(SAM_Biomass ptr, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif