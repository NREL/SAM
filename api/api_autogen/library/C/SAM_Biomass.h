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
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_emissions_avoided_cred_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.emissions.collection_fuel: 
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_emissions_collection_fuel_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.emissions.grid_intensity: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_emissions_grid_intensity_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.emissions.pre_chipopt: 
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_emissions_pre_chipopt_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.emissions.pre_grindopt: 
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_emissions_pre_grindopt_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.emissions.pre_pelletopt: 
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_emissions_pre_pelletopt_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.emissions.transport_fuel: 
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_emissions_transport_fuel_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.emissions.transport_legs: 
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_emissions_transport_legs_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.emissions.transport_long: 
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_emissions_transport_long_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.emissions.transport_longmiles: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_emissions_transport_longmiles_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.emissions.transport_longopt: 
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_emissions_transport_longopt_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.emissions.transport_predist: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_emissions_transport_predist_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.additional_opt: 
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_additional_opt_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.bagasse_frac: Bagasse feedstock fraction
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_bagasse_frac_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.bagasse_moisture: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_bagasse_moisture_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.barley_frac: Barley feedstock fraction
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_barley_frac_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.barley_moisture: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_barley_moisture_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.bit_frac: Bituminos coal feedstock fraction
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_bit_frac_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.bit_moisture: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_bit_moisture_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.collection_radius: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_collection_radius_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.feedstock1_c: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_feedstock1_c_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.feedstock1_frac: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_feedstock1_frac_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.feedstock1_h: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_feedstock1_h_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.feedstock1_hhv: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_feedstock1_hhv_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.feedstock1_moisture: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_feedstock1_moisture_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.feedstock1_resource: Opt feedstock 1 (dt/yr)
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_feedstock1_resource_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.feedstock2_c: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_feedstock2_c_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.feedstock2_frac: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_feedstock2_frac_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.feedstock2_h: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_feedstock2_h_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.feedstock2_hhv: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_feedstock2_hhv_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.feedstock2_moisture: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_feedstock2_moisture_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.feedstock2_resource: Opt feedstock 2 (dt/yr)
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_feedstock2_resource_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.forest_frac: Forest residue feedstock fraction
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_forest_frac_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.forest_moisture: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_forest_moisture_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.herb_c: Carbon fraction in herbaceous energy crop
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_herb_c_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.herb_frac: Herbaceous energy crop feedstock fraction
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_herb_frac_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.herb_moisture: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_herb_moisture_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.lig_frac: Lignite coal feedstock fraction
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_lig_frac_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.lig_moisture: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_lig_moisture_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.mill_c: Carbon fraction in mill residue
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_mill_c_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.mill_frac: Mill residue feedstock fraction
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_mill_frac_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.mill_moisture: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_mill_moisture_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.rice_frac: Rice straw feedstock fraction
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_rice_frac_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.rice_moisture: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_rice_moisture_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.stover_frac: Stover feedstock fraction
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_stover_frac_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.stover_moisture: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_stover_moisture_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.subbit_frac: Sub-bituminous coal feedstock fraction
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_subbit_frac_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.subbit_moisture: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_subbit_moisture_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.total: Total fuel resource (dt/yr)
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_total_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.total_biomass: Total biomass resource (dt/yr)
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_total_biomass_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.total_biomass_c: Biomass fraction carbon
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_total_biomass_c_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.total_c: Mass fraction carbon
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_total_c_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.total_coal: Total coal resource (dt/yr)
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_total_coal_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.total_h: Mass fraction hydrogen
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_total_h_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.total_hhv: Dry feedstock HHV (Btu/lb)
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_total_hhv_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.total_lhv: Dry feedstock LHV (Btu/lb)
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_total_lhv_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.total_moisture: Overall Moisture Content (dry %)
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_total_moisture_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.urban_c: Carbon fraction in urban residue
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_urban_c_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.urban_frac: Urban wood residue feedstock fraction
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_urban_frac_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.urban_moisture: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_urban_moisture_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.wheat_frac: Wheat straw feedstock fraction
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_wheat_frac_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.wheat_moisture: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_wheat_moisture_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.woody_c: Carbon fraction in woody energy crop
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_woody_c_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.woody_frac: Woody energy crop feedstock fraction
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_woody_frac_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.feedstock.woody_moisture: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_feedstock_woody_moisture_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.plant.boiler.air_feed: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_boiler_air_feed_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.plant.boiler.cap_per_boiler: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_boiler_cap_per_boiler_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.plant.boiler.flue_temp: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_boiler_flue_temp_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.plant.boiler.num: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_boiler_num_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.plant.boiler.over_design: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_boiler_over_design_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.plant.boiler.steam_enthalpy: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_boiler_steam_enthalpy_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.plant.boiler.steam_pressure: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_boiler_steam_pressure_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.plant.combustor_type: 
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_combustor_type_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.plant.cycle_design_temp: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_cycle_design_temp_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.plant.disp.power: 
	 * options: None
	 * constraints: LENGTH=9
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_disp_power_aset(SAM_Biomass ptr, double* arr, int length, SAM_error *err);

	/**
	 * Set biopwr.plant.drying_method: 
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_drying_method_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.plant.drying_spec: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_drying_spec_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.plant.max_over_design: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_max_over_design_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.plant.min_load: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_min_load_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.plant.nameplate: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_nameplate_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.plant.par_percent: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_par_percent_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.plant.pl_eff_f0: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_pl_eff_f0_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.plant.pl_eff_f1: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_pl_eff_f1_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.plant.pl_eff_f2: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_pl_eff_f2_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.plant.pl_eff_f3: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_pl_eff_f3_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.plant.pl_eff_f4: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_pl_eff_f4_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.plant.ramp_rate: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_ramp_rate_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.plant.rated_eff: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_rated_eff_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.plant.temp_corr_mode: 
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_temp_corr_mode_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.plant.temp_eff_f0: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_temp_eff_f0_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.plant.temp_eff_f1: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_temp_eff_f1_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.plant.temp_eff_f2: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_temp_eff_f2_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.plant.temp_eff_f3: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_temp_eff_f3_nset(SAM_Biomass ptr, double number, SAM_error *err);

	/**
	 * Set biopwr.plant.temp_eff_f4: 
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_temp_eff_f4_nset(SAM_Biomass ptr, double number, SAM_error *err);

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
	SAM_EXPORT void SAM_Biomass_Biopower_biopwr_plant_tou_option_nset(SAM_Biomass ptr, double number, SAM_error *err);

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
	SAM_EXPORT void SAM_Biomass_Biopower_system_capacity_nset(SAM_Biomass ptr, double number, SAM_error *err);


	/**
	 * Biopower Getters
	 */

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_emissions_avoided_cred_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_emissions_collection_fuel_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_emissions_grid_intensity_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_emissions_pre_chipopt_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_emissions_pre_grindopt_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_emissions_pre_pelletopt_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_emissions_transport_fuel_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_emissions_transport_legs_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_emissions_transport_long_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_emissions_transport_longmiles_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_emissions_transport_longopt_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_emissions_transport_predist_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_additional_opt_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_bagasse_frac_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_bagasse_moisture_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_barley_frac_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_barley_moisture_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_bit_frac_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_bit_moisture_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_collection_radius_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_feedstock1_c_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_feedstock1_frac_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_feedstock1_h_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_feedstock1_hhv_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_feedstock1_moisture_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_feedstock1_resource_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_feedstock2_c_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_feedstock2_frac_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_feedstock2_h_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_feedstock2_hhv_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_feedstock2_moisture_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_feedstock2_resource_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_forest_frac_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_forest_moisture_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_herb_c_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_herb_frac_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_herb_moisture_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_lig_frac_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_lig_moisture_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_mill_c_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_mill_frac_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_mill_moisture_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_rice_frac_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_rice_moisture_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_stover_frac_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_stover_moisture_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_subbit_frac_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_subbit_moisture_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_total_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_total_biomass_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_total_biomass_c_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_total_c_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_total_coal_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_total_h_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_total_hhv_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_total_lhv_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_total_moisture_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_urban_c_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_urban_frac_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_urban_moisture_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_wheat_frac_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_wheat_moisture_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_woody_c_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_woody_frac_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_feedstock_woody_moisture_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_plant_boiler_air_feed_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_plant_boiler_cap_per_boiler_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_plant_boiler_flue_temp_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_plant_boiler_num_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_plant_boiler_over_design_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_plant_boiler_steam_enthalpy_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_plant_boiler_steam_pressure_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_plant_combustor_type_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_plant_cycle_design_temp_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Biomass_Biopower_biopwr_plant_disp_power_aget(SAM_Biomass ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_plant_drying_method_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_plant_drying_spec_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_plant_max_over_design_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_plant_min_load_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_plant_nameplate_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_plant_par_percent_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_plant_pl_eff_f0_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_plant_pl_eff_f1_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_plant_pl_eff_f2_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_plant_pl_eff_f3_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_plant_pl_eff_f4_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_plant_ramp_rate_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_plant_rated_eff_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_plant_temp_corr_mode_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_plant_temp_eff_f0_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_plant_temp_eff_f1_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_plant_temp_eff_f2_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_plant_temp_eff_f3_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_plant_temp_eff_f4_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT const char* SAM_Biomass_Biopower_biopwr_plant_tou_grid_sget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_biopwr_plant_tou_option_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT const char* SAM_Biomass_Biopower_file_name_sget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Biopower_system_capacity_nget(SAM_Biomass ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double SAM_Biomass_Outputs_annual_energy_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Outputs_annual_fuel_usage_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Outputs_annual_watter_usage_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Outputs_capacity_factor_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Biomass_Outputs_gen_aget(SAM_Biomass ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Biomass_Outputs_hourly_boiler_eff_aget(SAM_Biomass ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Biomass_Outputs_hourly_pbeta_aget(SAM_Biomass ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Biomass_Outputs_hourly_q_to_pb_aget(SAM_Biomass ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Outputs_kwh_per_kw_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double* SAM_Biomass_Outputs_monthly_bagasse_emc_aget(SAM_Biomass ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Biomass_Outputs_monthly_barley_emc_aget(SAM_Biomass ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Biomass_Outputs_monthly_boiler_eff_aget(SAM_Biomass ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Biomass_Outputs_monthly_energy_aget(SAM_Biomass ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Biomass_Outputs_monthly_forest_emc_aget(SAM_Biomass ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Biomass_Outputs_monthly_herb_emc_aget(SAM_Biomass ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Biomass_Outputs_monthly_hhv_heatrate_aget(SAM_Biomass ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Biomass_Outputs_monthly_lhv_heatrate_aget(SAM_Biomass ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Biomass_Outputs_monthly_mill_emc_aget(SAM_Biomass ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Biomass_Outputs_monthly_moist_aget(SAM_Biomass ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Biomass_Outputs_monthly_pb_eta_aget(SAM_Biomass ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Biomass_Outputs_monthly_q_to_pb_aget(SAM_Biomass ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Biomass_Outputs_monthly_rh_aget(SAM_Biomass ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Biomass_Outputs_monthly_rice_emc_aget(SAM_Biomass ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Biomass_Outputs_monthly_stover_emc_aget(SAM_Biomass ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Biomass_Outputs_monthly_temp_c_aget(SAM_Biomass ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Biomass_Outputs_monthly_urban_emc_aget(SAM_Biomass ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Biomass_Outputs_monthly_wheat_emc_aget(SAM_Biomass ptr, int* length, SAM_error *err);

	SAM_EXPORT double* SAM_Biomass_Outputs_monthly_woody_emc_aget(SAM_Biomass ptr, int* length, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Outputs_system_annual_ash_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Outputs_system_annual_biomass_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Outputs_system_annual_boiler_loss_dry_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Outputs_system_annual_boiler_loss_dry_kwh_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Outputs_system_annual_boiler_loss_fuel_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Outputs_system_annual_boiler_loss_fuel_kwh_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Outputs_system_annual_boiler_loss_manu_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Outputs_system_annual_boiler_loss_manu_kwh_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Outputs_system_annual_boiler_loss_rad_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Outputs_system_annual_boiler_loss_rad_kwh_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Outputs_system_annual_boiler_loss_total_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Outputs_system_annual_boiler_loss_total_kwh_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Outputs_system_annual_boiler_loss_unburn_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Outputs_system_annual_boiler_loss_unburn_kwh_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Outputs_system_annual_boiler_loss_wet_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Outputs_system_annual_boiler_loss_wet_kwh_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Outputs_system_annual_boiler_output_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Outputs_system_annual_coal_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Outputs_system_annual_e_net_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Outputs_system_annual_par_loss_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Outputs_system_annual_par_loss_kwh_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Outputs_system_annual_pb_eta_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Outputs_system_annual_pb_eta_kwh_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Outputs_system_annual_qtoboil_tot_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Outputs_system_annual_qtopb_tot_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Outputs_system_annual_turbine_output_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Outputs_system_capfactor_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Outputs_system_emissions_avoided_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Outputs_system_emissions_biodiesel_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Outputs_system_emissions_bunker_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Outputs_system_emissions_combustion_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Outputs_system_emissions_diesel_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Outputs_system_emissions_drying_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Outputs_system_emissions_ems_per_lb_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Outputs_system_emissions_growth_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Outputs_system_emissions_lime_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Outputs_system_emissions_naturalgas_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Outputs_system_emissions_nitrogen_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Outputs_system_emissions_oil_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Outputs_system_emissions_phosphorus_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Outputs_system_emissions_potassium_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Outputs_system_emissions_preprocessing_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Outputs_system_emissions_total_sum_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Outputs_system_emissions_transport_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Outputs_system_emissions_uptake_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Outputs_system_heat_rate_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Outputs_system_hhv_heatrate_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Outputs_system_hhv_thermeff_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Outputs_system_lhv_heatrate_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Outputs_system_lhv_thermeff_nget(SAM_Biomass ptr, SAM_error *err);

	SAM_EXPORT double SAM_Biomass_Outputs_system_total_moisture_nget(SAM_Biomass ptr, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif