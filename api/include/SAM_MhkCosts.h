#ifndef SAM_MHKCOSTS_H_
#define SAM_MHKCOSTS_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// MhkCosts Technology Model
	//

	/** 
	 * Create a MhkCosts variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_MhkCosts;

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_MhkCosts_execute(SAM_table data, int verbosity, SAM_error* err);


	//
	// MHKCosts parameters
	//

	/**
	 * Set annual_energy: Annual energy production [kWh]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_MhkCosts_MHKCosts_annual_energy_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set array_cable_system_cost_input: Array cable system cost [$]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_MhkCosts_MHKCosts_array_cable_system_cost_input_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set array_cable_system_cost_method: Array cable system cost method [0/1/2]
	 * options: 0=Enter in $/kW,1=Enter in $,2=Use modeled value
	 * constraints: MIN=0,MAX=3
	 * required if: *
	 */
	SAM_EXPORT void SAM_MhkCosts_MHKCosts_array_cable_system_cost_method_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set assembly_and_install_cost_input: Assembly and installation cost [$]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_MhkCosts_MHKCosts_assembly_and_install_cost_input_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set assembly_and_install_cost_method: Assembly and installation cost method [0/1/2]
	 * options: 0=Enter in $/kW,1=Enter in $,2=Use modeled value
	 * constraints: MIN=0,MAX=3
	 * required if: *
	 */
	SAM_EXPORT void SAM_MhkCosts_MHKCosts_assembly_and_install_cost_method_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set development_cost_input: Development cost [$]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_MhkCosts_MHKCosts_development_cost_input_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set development_cost_method: Development cost method [0/1/2]
	 * options: 0=Enter in $/kW,1=Enter in $,2=Use modeled value,3=Enter in itemized costs
	 * constraints: MIN=0,MAX=3
	 * required if: *
	 */
	SAM_EXPORT void SAM_MhkCosts_MHKCosts_development_cost_method_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set device_rated_power: Rated capacity of device [kW]
	 * options: None
	 * constraints: MIN=0
	 * required if: *
	 */
	SAM_EXPORT void SAM_MhkCosts_MHKCosts_device_rated_power_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set devices_per_row: Number of wave devices per row in array
	 * options: None
	 * constraints: INTEGER
	 * required if: *
	 */
	SAM_EXPORT void SAM_MhkCosts_MHKCosts_devices_per_row_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set eng_and_mgmt_cost_input: Engineering and management cost [$]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_MhkCosts_MHKCosts_eng_and_mgmt_cost_input_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set eng_and_mgmt_cost_method: Engineering and management cost method [0/1/2]
	 * options: 0=Enter in $/kW,1=Enter in $,2=Use modeled value,3=Enter in itemized costs
	 * constraints: MIN=0,MAX=3
	 * required if: *
	 */
	SAM_EXPORT void SAM_MhkCosts_MHKCosts_eng_and_mgmt_cost_method_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set export_cable_length: Export cable length [m]
	 * options: None
	 * constraints: MIN=0
	 * required if: *
	 */
	SAM_EXPORT void SAM_MhkCosts_MHKCosts_export_cable_length_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set export_cable_system_cost_input: Export cable system cost [$]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_MhkCosts_MHKCosts_export_cable_system_cost_input_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set export_cable_system_cost_method: Export cable system cost method [0/1/2]
	 * options: 0=Enter in $/kW,1=Enter in $,2=Use modeled value
	 * constraints: MIN=0,MAX=3
	 * required if: *
	 */
	SAM_EXPORT void SAM_MhkCosts_MHKCosts_export_cable_system_cost_method_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set inter_array_cable_length: Inter-array cable length [m]
	 * options: None
	 * constraints: MIN=0
	 * required if: *
	 */
	SAM_EXPORT void SAM_MhkCosts_MHKCosts_inter_array_cable_length_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set lib_wave_device: Wave library name
	 * options: None
	 * constraints: None
	 * required if: marine_energy_tech=0
	 */
	SAM_EXPORT void SAM_MhkCosts_MHKCosts_lib_wave_device_sset(SAM_table ptr, const char* str, SAM_error *err);

	/**
	 * Set library_or_input_wec: Wave library or user input
	 * options: 0=Library,1=User
	 * constraints: None
	 * required if: marine_energy_tech=0
	 */
	SAM_EXPORT void SAM_MhkCosts_MHKCosts_library_or_input_wec_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set marine_energy_tech: Marine energy technology [0/1]
	 * options: 0=Wave,1=Tidal
	 * constraints: MIN=0,MAX=1
	 * required if: *
	 */
	SAM_EXPORT void SAM_MhkCosts_MHKCosts_marine_energy_tech_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mooring_found_substruc_cost_input: Mooring, foundation, and substructure cost [$]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_MhkCosts_MHKCosts_mooring_found_substruc_cost_input_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mooring_found_substruc_cost_method: Mooring, foundation, and substructure cost method [0/1/2]
	 * options: 0=Enter in $/kW,1=Enter in $,2=Use modeled value,3=Use itemized costs in $
	 * constraints: MIN=0,MAX=3
	 * required if: *
	 */
	SAM_EXPORT void SAM_MhkCosts_MHKCosts_mooring_found_substruc_cost_method_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set offshore_substation_cost_input: Offshore substation cost [$]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_MhkCosts_MHKCosts_offshore_substation_cost_input_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set offshore_substation_cost_method: Offshore substation cost method [0/1/2]
	 * options: 0=Enter in $/kW,1=Enter in $,2=Use modeled value
	 * constraints: MIN=0,MAX=3
	 * required if: *
	 */
	SAM_EXPORT void SAM_MhkCosts_MHKCosts_offshore_substation_cost_method_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set onshore_substation_cost_input: Onshore substation cost [$]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_MhkCosts_MHKCosts_onshore_substation_cost_input_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set onshore_substation_cost_method: Onshore substation cost method [0/1/2]
	 * options: 0=Enter in $/kW,1=Enter in $,2=Use modeled value
	 * constraints: MIN=0,MAX=3
	 * required if: *
	 */
	SAM_EXPORT void SAM_MhkCosts_MHKCosts_onshore_substation_cost_method_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set other_elec_infra_cost_input: Other electrical infrastructure cost [$]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_MhkCosts_MHKCosts_other_elec_infra_cost_input_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set other_elec_infra_cost_method: Other electrical infrastructure cost method [0/1/2]
	 * options: 0=Enter in $/kW,1=Enter in $,2=Use modeled value
	 * constraints: MIN=0,MAX=3
	 * required if: *
	 */
	SAM_EXPORT void SAM_MhkCosts_MHKCosts_other_elec_infra_cost_method_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set other_infrastructure_cost_input: Other infrastructure cost [$]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_MhkCosts_MHKCosts_other_infrastructure_cost_input_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set other_infrastructure_cost_method: Other infrastructure cost method [0/1/2]
	 * options: 0=Enter in $/kW,1=Enter in $,2=Use modeled value
	 * constraints: MIN=0,MAX=3
	 * required if: *
	 */
	SAM_EXPORT void SAM_MhkCosts_MHKCosts_other_infrastructure_cost_method_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set power_takeoff_system_cost_input: Power take-off system cost [$]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_MhkCosts_MHKCosts_power_takeoff_system_cost_input_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set power_takeoff_system_cost_method: Power take-off system cost method [0/1/2]
	 * options: 0=Enter in $/kW,1=Enter in $,2=Use modeled value,3=Use itemized costs in $
	 * constraints: MIN=0,MAX=3
	 * required if: *
	 */
	SAM_EXPORT void SAM_MhkCosts_MHKCosts_power_takeoff_system_cost_method_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set riser_cable_length: Riser cable length [m]
	 * options: None
	 * constraints: MIN=0
	 * required if: *
	 */
	SAM_EXPORT void SAM_MhkCosts_MHKCosts_riser_cable_length_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set structural_assembly_cost_input: Structural assembly cost [$]
	 * options: None
	 * constraints: None
	 * required if: *
	 */
	SAM_EXPORT void SAM_MhkCosts_MHKCosts_structural_assembly_cost_input_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set structural_assembly_cost_method: Structural assembly cost method [0/1/2]
	 * options: 0=Enter in $/kW,1=Enter in $,2=Use modeled value,3=Use itemized costs in $
	 * constraints: MIN=0,MAX=3
	 * required if: *
	 */
	SAM_EXPORT void SAM_MhkCosts_MHKCosts_structural_assembly_cost_method_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set system_capacity: System Nameplate Capacity [kW]
	 * options: None
	 * constraints: MIN=0
	 * required if: *
	 */
	SAM_EXPORT void SAM_MhkCosts_MHKCosts_system_capacity_nset(SAM_table ptr, double number, SAM_error *err);


	/**
	 * MHKCosts Getters
	 */

	SAM_EXPORT double SAM_MhkCosts_MHKCosts_annual_energy_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkCosts_MHKCosts_array_cable_system_cost_input_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkCosts_MHKCosts_array_cable_system_cost_method_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkCosts_MHKCosts_assembly_and_install_cost_input_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkCosts_MHKCosts_assembly_and_install_cost_method_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkCosts_MHKCosts_development_cost_input_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkCosts_MHKCosts_development_cost_method_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkCosts_MHKCosts_device_rated_power_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkCosts_MHKCosts_devices_per_row_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkCosts_MHKCosts_eng_and_mgmt_cost_input_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkCosts_MHKCosts_eng_and_mgmt_cost_method_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkCosts_MHKCosts_export_cable_length_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkCosts_MHKCosts_export_cable_system_cost_input_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkCosts_MHKCosts_export_cable_system_cost_method_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkCosts_MHKCosts_inter_array_cable_length_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT const char* SAM_MhkCosts_MHKCosts_lib_wave_device_sget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkCosts_MHKCosts_library_or_input_wec_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkCosts_MHKCosts_marine_energy_tech_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkCosts_MHKCosts_mooring_found_substruc_cost_input_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkCosts_MHKCosts_mooring_found_substruc_cost_method_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkCosts_MHKCosts_offshore_substation_cost_input_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkCosts_MHKCosts_offshore_substation_cost_method_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkCosts_MHKCosts_onshore_substation_cost_input_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkCosts_MHKCosts_onshore_substation_cost_method_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkCosts_MHKCosts_other_elec_infra_cost_input_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkCosts_MHKCosts_other_elec_infra_cost_method_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkCosts_MHKCosts_other_infrastructure_cost_input_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkCosts_MHKCosts_other_infrastructure_cost_method_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkCosts_MHKCosts_power_takeoff_system_cost_input_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkCosts_MHKCosts_power_takeoff_system_cost_method_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkCosts_MHKCosts_riser_cable_length_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkCosts_MHKCosts_structural_assembly_cost_input_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkCosts_MHKCosts_structural_assembly_cost_method_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkCosts_MHKCosts_system_capacity_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double SAM_MhkCosts_Outputs_array_cable_system_cost_modeled_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkCosts_Outputs_assembly_and_install_cost_modeled_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkCosts_Outputs_development_cost_modeled_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkCosts_Outputs_eng_and_mgmt_cost_modeled_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkCosts_Outputs_export_cable_system_cost_modeled_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkCosts_Outputs_insurance_during_construction_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkCosts_Outputs_maintenance_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkCosts_Outputs_mooring_found_substruc_cost_modeled_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkCosts_Outputs_offshore_substation_cost_modeled_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkCosts_Outputs_onshore_substation_cost_modeled_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkCosts_Outputs_operations_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkCosts_Outputs_other_elec_infra_cost_modeled_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkCosts_Outputs_other_infrastructure_cost_modeled_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkCosts_Outputs_plant_commissioning_cost_modeled_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkCosts_Outputs_power_takeoff_system_cost_modeled_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkCosts_Outputs_project_contingency_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkCosts_Outputs_reserve_accounts_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkCosts_Outputs_site_access_port_staging_cost_modeled_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkCosts_Outputs_structural_assembly_cost_modeled_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkCosts_Outputs_total_bos_cost_per_kwh_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkCosts_Outputs_total_capital_cost_per_kwh_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkCosts_Outputs_total_device_cost_per_kwh_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkCosts_Outputs_total_financial_cost_per_kwh_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_MhkCosts_Outputs_total_operations_cost_per_kwh_nget(SAM_table ptr, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif