#ifndef SAM_WINDOBOS_H_
#define SAM_WINDOBOS_H_

#include "visibility.h"
#include "SAM_api.h"


#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

	//
	// WindObos Technology Model
	//

	/** 
	 * Create a WindObos variable table.
	 * @param def: the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err: a pointer to an error object
	 */

	SAM_EXPORT typedef void * SAM_WindObos;

	/// verbosity level 0 or 1. Returns 1 on success
	SAM_EXPORT int SAM_WindObos_execute(SAM_table data, int verbosity, SAM_error* err);


	//
	// Wobos parameters
	//

	/**
	 * Set addLocPerm: Additional State and Local Permitting Cost [$]
	 * options: None
	 * constraints: None
	 * required if: ?=200000
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_addLocPerm_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set anchor: Anchor Type
	 * options: None
	 * constraints: INTEGER
	 * required if: ?=DRAGEMBEDMENT
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_anchor_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set arrCab1Mass: Array cable 1 mass [kg/m]
	 * options: None
	 * constraints: None
	 * required if: ?=20.384
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_arrCab1Mass_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set arrCab2Mass: Array cable 2 mass [kg/m]
	 * options: None
	 * constraints: None
	 * required if: ?=21.854
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_arrCab2Mass_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set arrVoltage: Array cable voltage [kV]
	 * options: None
	 * constraints: None
	 * required if: ?=33
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_arrVoltage_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set arrayCables: Inter-array cables to consider by voltage [kV]
	 * options: None
	 * constraints: None
	 * required if: ?=33 66
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_arrayCables_sset(SAM_table ptr, const char* str, SAM_error *err);

	/**
	 * Set arrayX: Spacing Between Turbine Rows [rotor diameters]
	 * options: None
	 * constraints: MIN=1
	 * required if: ?=9
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_arrayX_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set arrayY: Spacing Between Turbines in Rows [rotor diameters]
	 * options: None
	 * constraints: MIN=1
	 * required if: ?=9
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_arrayY_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set backUpGen: Back up Diesel Generator Cost [$]
	 * options: None
	 * constraints: None
	 * required if: ?=1000000
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_backUpGen_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ballCR: Floating Ballast Cost Rate [$/tonne]
	 * options: None
	 * constraints: None
	 * required if: ?=100
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_ballCR_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set bioResStudyMet: Biological Resource Study Met Tower Cost [$]
	 * options: None
	 * constraints: None
	 * required if: ?=1500000
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_bioResStudyMet_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set bioResStudyProj: Biological Resource Study Porject Cost [$]
	 * options: None
	 * constraints: None
	 * required if: ?=500000
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_bioResStudyProj_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set bladeL: Blade Length [m]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_bladeL_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set boltBlade1: Lift and Bolt Blade Individual Components Method [hours]
	 * options: None
	 * constraints: None
	 * required if: ?=3.5
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_boltBlade1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set boltBlade2: Lift and Bolt Blade Bunny Ears Method [hours]
	 * options: None
	 * constraints: None
	 * required if: ?=3.5
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_boltBlade2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set boltNacelle1: Lift and Bolt Nacelle Individual Components Method [hours]
	 * options: None
	 * constraints: None
	 * required if: ?=7
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_boltNacelle1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set boltNacelle2: Lift and Bolt Nacelle Bunny Ears Method [hours]
	 * options: None
	 * constraints: None
	 * required if: ?=7
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_boltNacelle2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set boltNacelle3: Lift and Bolt Nacelle Fully Assembled Rotor Method [hours]
	 * options: None
	 * constraints: None
	 * required if: ?=7
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_boltNacelle3_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set boltRotor: Lift and Bolt Rotor Fully Assembled Rotor Method [hours]
	 * options: None
	 * constraints: None
	 * required if: ?=7
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_boltRotor_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set boltTower: Lift and Bolt Tower Section [hours]
	 * options: None
	 * constraints: None
	 * required if: ?=7
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_boltTower_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set buryDepth: Electrical Cable Burial Depth [m]
	 * options: None
	 * constraints: MIN=0,MAX=15
	 * required if: ?=2
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_buryDepth_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set buryFac: Cable Burial Depth Factor [1/m]
	 * options: None
	 * constraints: None
	 * required if: ?=0.1
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_buryFac_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set buryRate: Cable Burial Rate [m/hour]
	 * options: None
	 * constraints: None
	 * required if: ?=125
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_buryRate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cab1CR: Array cable 1 Cost Rate [$/m]
	 * options: None
	 * constraints: None
	 * required if: ?=185.889
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_cab1CR_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cab1CurrRating: Array cable 1 current rating [A]
	 * options: None
	 * constraints: None
	 * required if: ?=300
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_cab1CurrRating_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cab1TurbInterCR: Cable 1 turbine interface cost [$/interface]
	 * options: None
	 * constraints: None
	 * required if: ?=8410
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_cab1TurbInterCR_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cab2CR: Array cable 2 Cost Rate [$/m]
	 * options: None
	 * constraints: None
	 * required if: ?=202.788
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_cab2CR_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cab2CurrRating: Array cable 2 current rating [A]
	 * options: None
	 * constraints: None
	 * required if: ?=340
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_cab2CurrRating_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cab2SubsInterCR: Cable 2 substation interface cost [$/interface]
	 * options: None
	 * constraints: None
	 * required if: ?=19815
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_cab2SubsInterCR_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cab2TurbInterCR: Cable 2 turbine interface cost [$/interface]
	 * options: None
	 * constraints: None
	 * required if: ?=8615
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_cab2TurbInterCR_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cabDrillCR: Cost Rate for Horizontal Drilling [$/m]
	 * options: None
	 * constraints: None
	 * required if: ?=3200
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_cabDrillCR_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cabDrillDist: Horizontal Drilling distance for Cable Landfall [m]
	 * options: None
	 * constraints: None
	 * required if: ?=500
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_cabDrillDist_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cabLoadout: Array Cable Loadout for Installation [hours]
	 * options: None
	 * constraints: None
	 * required if: ?=14
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_cabLoadout_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cabPullIn: Array Cable Pull in to Interfaces [hours]
	 * options: None
	 * constraints: None
	 * required if: ?=5.5
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_cabPullIn_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cabSurveyCR: Cable Route Survey Cost [$/m]
	 * options: None
	 * constraints: None
	 * required if: ?=240
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_cabSurveyCR_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cabTerm: Cable Termination and Testing [hours]
	 * options: None
	 * constraints: None
	 * required if: ?=5.5
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_cabTerm_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cableOptimizer: Electrical Cable Cost Optimization
	 * options: None
	 * constraints: INTEGER
	 * required if: ?=FALSE
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_cableOptimizer_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set capital_cost_year_0: Capital cost spent in year 0
	 * options: None
	 * constraints: None
	 * required if: ?=0.2
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_capital_cost_year_0_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set capital_cost_year_1: Capital cost spent in year 1
	 * options: None
	 * constraints: None
	 * required if: ?=0.6
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_capital_cost_year_1_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set capital_cost_year_2: Capital cost spent in year 2
	 * options: None
	 * constraints: None
	 * required if: ?=0.1
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_capital_cost_year_2_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set capital_cost_year_3: Capital cost spent in year 3
	 * options: None
	 * constraints: None
	 * required if: ?=0.1
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_capital_cost_year_3_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set capital_cost_year_4: Capital cost spent in year 4
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_capital_cost_year_4_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set capital_cost_year_5: Capital cost spent in year 5
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_capital_cost_year_5_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set catLengFac: Catenary Cable Length Factor
	 * options: None
	 * constraints: None
	 * required if: ?=0.04
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_catLengFac_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set chord: Blade Max Chord [m]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_chord_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set civilWork: Onshore Infrastructure Civil Work Cost [$]
	 * options: None
	 * constraints: None
	 * required if: ?=40000
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_civilWork_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cleanWatAct402: Clean Water Act Section 402 Compliance Cost [$]
	 * options: None
	 * constraints: None
	 * required if: ?=100000
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_cleanWatAct402_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set cleanWatAct404: Clean Water Act Section 404 Compliance Cost [$]
	 * options: None
	 * constraints: None
	 * required if: ?=100000
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_cleanWatAct404_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set coastZoneManAct: Coastal Zone Management Act Compliance Cost [$]
	 * options: None
	 * constraints: None
	 * required if: ?=100000
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_coastZoneManAct_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set compRacks: Component Racks Cost [$]
	 * options: None
	 * constraints: None
	 * required if: ?=1000000
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_compRacks_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set conOpPlan: Construction Operations Plan Cost [$]
	 * options: None
	 * constraints: None
	 * required if: ?=1000000
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_conOpPlan_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set construction_insurance: Insurance During Construction (% of ICC)
	 * options: None
	 * constraints: None
	 * required if: ?=0.01
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_construction_insurance_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set crane1000DR: 1000 t Crawler Crane Day Rate [$/day]
	 * options: None
	 * constraints: None
	 * required if: ?=8000
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_crane1000DR_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set crane600DR: 600 t Crawler Crane Day Rate [$/day]
	 * options: None
	 * constraints: None
	 * required if: ?=5000
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_crane600DR_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set craneMobDemob: Port Crane Mobilization/Demobilization Cost [$]
	 * options: None
	 * constraints: None
	 * required if: ?=150000
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_craneMobDemob_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set deaFixLeng: Fixed Mooring Length for Drag Embedment Anchors [m]
	 * options: None
	 * constraints: None
	 * required if: ?=500
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_deaFixLeng_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set decomDiscRate: Decommissioning Cost Discount Rate
	 * options: None
	 * constraints: None
	 * required if: ?=0.03
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_decomDiscRate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set distAtoS: Distance form Inshore Assembly Area to Site [km]
	 * options: None
	 * constraints: MIN=5,MAX=1000
	 * required if: ?=90
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_distAtoS_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set distInterCon: Distance Over Land to Grid Interconnect [miles]
	 * options: None
	 * constraints: None
	 * required if: ?=3
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_distInterCon_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set distPort: Distance from Installation Port to Site [km]
	 * options: None
	 * constraints: MIN=5,MAX=1000
	 * required if: ?=90
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_distPort_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set distPtoA: Distance from Installation Port to Inshore Assembly Area [km]
	 * options: None
	 * constraints: MIN=5,MAX=1000
	 * required if: ?=90
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_distPtoA_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set distShore: Distance to Landfall [km]
	 * options: None
	 * constraints: MIN=5,MAX=1000
	 * required if: ?=90
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_distShore_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set diveTeamDR: Cable Landfall Dive Team Day Rate [$/day]
	 * options: None
	 * constraints: None
	 * required if: ?=3200
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_diveTeamDR_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dockRate: Quayside Docking Cost Rate [$/day]
	 * options: None
	 * constraints: None
	 * required if: ?=3000
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_dockRate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set dynCabFac: Dynamic Cable Cost Premium Factor
	 * options: None
	 * constraints: None
	 * required if: ?=2
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_dynCabFac_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set elecCont: Electrical Install Weather Contingency [%]
	 * options: None
	 * constraints: None
	 * required if: ?=0.3
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_elecCont_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set elecWork: Onshore Infrastructure Electrical Work Cost [$]
	 * options: None
	 * constraints: None
	 * required if: ?=25000
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_elecWork_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set endSpecAct: Endangered Species Act Compliance Cost [$]
	 * options: None
	 * constraints: None
	 * required if: ?=500000
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_endSpecAct_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set entranceExitRate: Port Entrance and Exit Cost Rate [$/occurrence]
	 * options: None
	 * constraints: None
	 * required if: ?=0.525
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_entranceExitRate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set estEnMFac: Estimated Engineering & Management Cost Factor
	 * options: None
	 * constraints: None
	 * required if: ?=0.04
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_estEnMFac_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set exCabFac: Excess Cable Factor
	 * options: None
	 * constraints: None
	 * required if: ?=0.1
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_exCabFac_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set expCabCR: Export cable cost rate [$/m]
	 * options: None
	 * constraints: None
	 * required if: ?=495.411
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_expCabCR_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set expCabLoad: Export Cable Loadout for Installation [hours]
	 * options: None
	 * constraints: None
	 * required if: ?=24
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_expCabLoad_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set expCabMass: Export cable mass [kg/m]
	 * options: None
	 * constraints: None
	 * required if: ?=71.9
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_expCabMass_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set expCurrRating: Export cable current rating [A]
	 * options: None
	 * constraints: None
	 * required if: ?=530
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_expCurrRating_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set expSubsInterCR: Export cable substation interface cost [$/interface]
	 * options: None
	 * constraints: None
	 * required if: ?=57500
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_expSubsInterCR_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set expVoltage: Export cable voltage [kV]
	 * options: None
	 * constraints: None
	 * required if: ?=220
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_expVoltage_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set exportCables: Export cables to consider by voltage [kV]
	 * options: None
	 * constraints: None
	 * required if: ?=132 220
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_exportCables_sset(SAM_table ptr, const char* str, SAM_error *err);

	/**
	 * Set faaPlan: Federal Aviation Administration Plans & Mitigation Cost [$]
	 * options: None
	 * constraints: None
	 * required if: ?=10000
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_faaPlan_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set feedStudy: FEED Study Cost [$]
	 * options: None
	 * constraints: None
	 * required if: ?=10000000
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_feedStudy_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set groutSpreadDR: Grouting Spread Day Rate [$/day]
	 * options: None
	 * constraints: None
	 * required if: ?=3000
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_groutSpreadDR_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set groutSpreadMob: Grouting Spread Mobilization Cost [$]
	 * options: None
	 * constraints: None
	 * required if: ?=1000000
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_groutSpreadMob_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set groutTP: Grout Transition Piece/Monopile Interface [hours]
	 * options: None
	 * constraints: None
	 * required if: ?=8
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_groutTP_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set hamRate: Pile Hammer Rate [m/hour]
	 * options: None
	 * constraints: None
	 * required if: ?=20
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_hamRate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set highVoltSG: High Voltage Switchgear Cost [$]
	 * options: None
	 * constraints: None
	 * required if: ?=950000
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_highVoltSG_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set hubD: Hub Diameter [m]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_hubD_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set hubH: Hub Height [m]
	 * options: None
	 * constraints: None
	 * required if: ?=90
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_hubH_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set inspectClear: Inspection Clearance [m]
	 * options: None
	 * constraints: None
	 * required if: ?=2
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_inspectClear_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set instScour: Install Scour Protection Around Monopile Base [hours]
	 * options: None
	 * constraints: None
	 * required if: ?=6
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_instScour_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set installStrategy: Installation Vessel Strategy
	 * options: None
	 * constraints: INTEGER
	 * required if: ?=PRIMARYVESSEL
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_installStrategy_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set install_contingency: Installation Contingency
	 * options: None
	 * constraints: None
	 * required if: ?=0.3
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_install_contingency_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set interConVolt: Grid Interconnect Voltage [kV]
	 * options: None
	 * constraints: None
	 * required if: ?=345
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_interConVolt_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set interest_during_construction: Interest During Construction
	 * options: None
	 * constraints: None
	 * required if: ?=0.08
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_interest_during_construction_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set jackFasten: Prepare and Fasten Jacket for Transport [hours/unit]
	 * options: None
	 * constraints: None
	 * required if: ?=20
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_jackFasten_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set jlatticeA: Jacket Main Lattice Footprint Area [m^2]
	 * options: None
	 * constraints: None
	 * required if: ?=26
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_jlatticeA_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set jlatticeCR: Jacket Main Lattice Cost Rate [$/tonne]
	 * options: None
	 * constraints: None
	 * required if: ?=4680
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_jlatticeCR_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set jpileCR: Jacket Pile Cost Rate [$/tonne]
	 * options: None
	 * constraints: None
	 * required if: ?=2250
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_jpileCR_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set jpileD: Jacket Pile Diameter [m]
	 * options: None
	 * constraints: None
	 * required if: ?=1.6
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_jpileD_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set jpileL: Jacket Pile Length [m]
	 * options: None
	 * constraints: None
	 * required if: ?=47.5
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_jpileL_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set jtransCR: Jacket Transition Piece Cost Rate [$/tonne]
	 * options: None
	 * constraints: None
	 * required if: ?=4500
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_jtransCR_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set landConstruct: Onshore Infrastructure Construction [days]
	 * options: None
	 * constraints: None
	 * required if: ?=7
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_landConstruct_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set laydownCR: Laydown and Storage Cost Rate [$/m^2/day]
	 * options: None
	 * constraints: None
	 * required if: ?=0.25
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_laydownCR_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set levJack: Level Jacket Main Lattice [hours]
	 * options: None
	 * constraints: None
	 * required if: ?=24
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_levJack_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set marMamProtAct: Marine Mammal Protection Act Compliance Cost [$]
	 * options: None
	 * constraints: None
	 * required if: ?=500000
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_marMamProtAct_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set medVoltSG: Medium Voltage Switchgear Cost [$]
	 * options: None
	 * constraints: None
	 * required if: ?=500000
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_medVoltSG_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set metTowCR: Meteorological (Met Tower Fabrication & Install Cost [$/MW]
	 * options: None
	 * constraints: None
	 * required if: ?=11518
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_metTowCR_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set migBirdAct: Migratory Bird Treaty Compliance Cost [$]
	 * options: None
	 * constraints: None
	 * required if: ?=500000
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_migBirdAct_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set monoFasten: Prepare and Fasten Monopile for Transport [hours/unit]
	 * options: None
	 * constraints: None
	 * required if: ?=12
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_monoFasten_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set moorCR: Mooring Line Cost Rate [$/m]
	 * options: None
	 * constraints: MIN=399
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_moorCR_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set moorCost: Capital cost of mooring lines and anchors [$]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_moorCost_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set moorDia: Mooring Line Diameter [m]
	 * options: None
	 * constraints: MIN=0.09
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_moorDia_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set moorLines: Number Of Mooring Lines
	 * options: None
	 * constraints: None
	 * required if: ?=3
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_moorLines_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set moorLoadout: Anchor & Mooring Loadout Time [hours]
	 * options: None
	 * constraints: None
	 * required if: ?=5
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_moorLoadout_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set moorSurvey: Survey Mooring Lines & Anchor Positions Time [hours]
	 * options: None
	 * constraints: None
	 * required if: ?=4
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_moorSurvey_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set moorTimeFac: Anchor & Mooring Water Depth Time Factor
	 * options: None
	 * constraints: None
	 * required if: ?=0.005
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_moorTimeFac_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mpEmbedL: Monopile Embedment Length [m]
	 * options: None
	 * constraints: None
	 * required if: ?=30
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_mpEmbedL_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mpileCR: Monopile Cost Rate [$/tonne]
	 * options: None
	 * constraints: None
	 * required if: ?=2250
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_mpileCR_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mpileD: Monopile Diameter [m]
	 * options: None
	 * constraints: MIN=0.01
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_mpileD_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mpileL: Monopile Length [m]
	 * options: None
	 * constraints: MIN=0.01
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_mpileL_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mptCR: Main Power Transformer Cost Rate [$/MVA]
	 * options: None
	 * constraints: None
	 * required if: ?=12500
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_mptCR_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mpvRentalDR: MPV Rental Day Rate [$/day]
	 * options: None
	 * constraints: None
	 * required if: ?=72000
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_mpvRentalDR_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set mtransCR: Monopile Transition Piece Cost Rate [$/tonne]
	 * options: None
	 * constraints: None
	 * required if: ?=3230
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_mtransCR_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set nCrane1000: Number of 1000 t Crawler Cranes
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_nCrane1000_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set nCrane600: Number of 600 t Crawler Cranes
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_nCrane600_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set nTurb: Number of Turbines
	 * options: None
	 * constraints: MIN=2,MAX=200
	 * required if: ?=20
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_nTurb_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set nacelleL: Nacelle Length [m]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_nacelleL_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set nacelleW: Nacelle Width [m]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_nacelleW_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set natHisPresAct: National Historic Preservation Act Compliance Cost [$]
	 * options: None
	 * constraints: None
	 * required if: ?=250000
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_natHisPresAct_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set navStudyMet: Navigation and Transport Study Met Tower Cost [$]
	 * options: None
	 * constraints: None
	 * required if: ?=500000
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_navStudyMet_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set navStudyProj: Navigation and Transport Study Project Cost [$]
	 * options: None
	 * constraints: None
	 * required if: ?=250000
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_navStudyProj_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set nepaEisMet: NEPA Environmental Impact Statement Met Tower Cost [$]
	 * options: None
	 * constraints: None
	 * required if: ?=2000000
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_nepaEisMet_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set nepaEisProj: NEPA Environmental Impact Study Project Cost [$]
	 * options: None
	 * constraints: None
	 * required if: ?=5000000
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_nepaEisProj_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set number_install_seasons: Number of Installation Seasons
	 * options: None
	 * constraints: None
	 * required if: ?=1
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_number_install_seasons_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set otherAncillary: Other Ancillary Systems Costs [$]
	 * options: None
	 * constraints: None
	 * required if: ?=3000000
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_otherAncillary_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set outConShelfLease: Outer Continental Shelf Lease Cost [$]
	 * options: None
	 * constraints: None
	 * required if: ?=1000000
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_outConShelfLease_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set physResStudyMet: Physical Resource Study Met Tower Cost [$]
	 * options: None
	 * constraints: None
	 * required if: ?=1500000
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_physResStudyMet_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set physResStudyProj: Physical Resource Study Project Cost [$]
	 * options: None
	 * constraints: None
	 * required if: ?=500000
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_physResStudyProj_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set pileSpreadDR: Piling Spread Day Rate [$/day]
	 * options: None
	 * constraints: None
	 * required if: ?=2500
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_pileSpreadDR_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set pileSpreadMob: Piling Spread Mobilization Cost [$]
	 * options: None
	 * constraints: None
	 * required if: ?=750000
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_pileSpreadMob_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set placeJack: Place Jacket Main Lattice onto Piles [hours]
	 * options: None
	 * constraints: None
	 * required if: ?=12
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_placeJack_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set placeMP: Lift and Place Monopile for Hammering [hours]
	 * options: None
	 * constraints: None
	 * required if: ?=3
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_placeMP_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set placePiles: Place Jacket Piles [hours]
	 * options: None
	 * constraints: None
	 * required if: ?=12
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_placePiles_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set placeTP: Place Transition Piece onto Monopile [hours]
	 * options: None
	 * constraints: None
	 * required if: ?=3
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_placeTP_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set placeTemplate: Place Jacket Pile Template on Seabed [hours]
	 * options: None
	 * constraints: None
	 * required if: ?=4
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_placeTemplate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set placeTop: Lift and Place Offshore Substation Topside [hours]
	 * options: None
	 * constraints: None
	 * required if: ?=24
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_placeTop_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set plantComm: Plant Commissioning Cost Factor
	 * options: None
	 * constraints: None
	 * required if: ?=0.01
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_plantComm_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set preFEEDStudy: Pre-FEED study Cost [$]
	 * options: None
	 * constraints: None
	 * required if: ?=5000000
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_preFEEDStudy_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set prepAA: Prepare Inshore Assembly Area For Turbine Installation [hours]
	 * options: None
	 * constraints: None
	 * required if: ?=168
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_prepAA_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set prepGripperJack: Prepare Jacket Gripper and Upender [hours]
	 * options: None
	 * constraints: None
	 * required if: ?=8
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_prepGripperJack_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set prepGripperMono: Prepare Monopile Gripper and Upender [hours]
	 * options: None
	 * constraints: None
	 * required if: ?=1.5
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_prepGripperMono_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set prepHamJack: Prepare Hammer for jacket Piles Installation [hours]
	 * options: None
	 * constraints: None
	 * required if: ?=2
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_prepHamJack_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set prepHamMono: Prepare Hammer for Monopile Installation [hours]
	 * options: None
	 * constraints: None
	 * required if: ?=2
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_prepHamMono_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set prepSemi: Prepare Semi-submersible for Turbine Installation [hours]
	 * options: None
	 * constraints: None
	 * required if: ?=12
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_prepSemi_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set prepSpar: Prepare Spar for Tow to Inshore Assembly Area [hours]
	 * options: None
	 * constraints: None
	 * required if: ?=18
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_prepSpar_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set prepTow: Prepare Floating Substructure for Tow to Site [hours]
	 * options: None
	 * constraints: None
	 * required if: ?=12
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_prepTow_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set procurement_contingency: Procurement Contingency
	 * options: None
	 * constraints: None
	 * required if: ?=0.05
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_procurement_contingency_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set projLife: Project Economic Life [years]
	 * options: None
	 * constraints: None
	 * required if: ?=20
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_projLife_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set pwrFac: Power Transfer Efficiency Factor
	 * options: None
	 * constraints: None
	 * required if: ?=0.95
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_pwrFac_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set removeHamJack: Remove Hammer for Jacket Piles Installation [hours]
	 * options: None
	 * constraints: None
	 * required if: ?=4
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_removeHamJack_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set removeHamMono: Remove Hammer for Monopile Installation [hours]
	 * options: None
	 * constraints: None
	 * required if: ?=2
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_removeHamMono_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set rivsnHarbsAct: Rivers & Harbors Act Section 10 Compliance Cost [$]
	 * options: None
	 * constraints: None
	 * required if: ?=100000
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_rivsnHarbsAct_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set rnaM: Rotor-Nacelle Assembly Mass [tonne]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_rnaM_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set rotorD: Rotor Diameter [m]
	 * options: None
	 * constraints: None
	 * required if: ?=120
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_rotorD_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set sSteelCR: Secondary/Outfitting Steel Cost Rate [$/tonne]
	 * options: None
	 * constraints: None
	 * required if: ?=7250
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_sSteelCR_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set saPlan: Site Assessment Plan Cost [$]
	 * options: None
	 * constraints: None
	 * required if: ?=500000
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_saPlan_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set scourMat: Scour Protection Material Cost [$/location]
	 * options: None
	 * constraints: None
	 * required if: ?=250000
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_scourMat_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set scrapVal: Total Scrap Value of Decommissioned Components [$]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_scrapVal_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set seaSpreadDR: Suction Pile Anchor Spread Day Rate [$/day]
	 * options: None
	 * constraints: None
	 * required if: ?=165000
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_seaSpreadDR_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set seaSpreadMob: Suction Pile Anchor Spread Mobilization Cost [$]
	 * options: None
	 * constraints: None
	 * required if: ?=4500000
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_seaSpreadMob_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set shorePullIn: Cable Pull in to Onshore Infrastructure [hours]
	 * options: None
	 * constraints: None
	 * required if: ?=96
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_shorePullIn_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set shuntCR: Shunt Reactor Cost Rate [$/MVA]
	 * options: None
	 * constraints: None
	 * required if: ?=35000
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_shuntCR_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set socEconStudyMet: Socioeconomic and Land use Study Met Tower Cost [$]
	 * options: None
	 * constraints: None
	 * required if: ?=500000
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_socEconStudyMet_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set socEconStudyProj: Socioeconomic and Land use Study Project Cost [$]
	 * options: None
	 * constraints: None
	 * required if: ?=200000
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_socEconStudyProj_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set spMoorCheck: Survey Spar Mooring Lines and Connections [hours]
	 * options: None
	 * constraints: None
	 * required if: ?=16
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_spMoorCheck_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set spMoorCon: Connect Mooring Lines to Spar [hours]
	 * options: None
	 * constraints: None
	 * required if: ?=20
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_spMoorCon_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set spStifColCR: Spar Stiffened Column Cost Rate [$/tonne]
	 * options: None
	 * constraints: None
	 * required if: ?=3120
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_spStifColCR_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set spTapColCR: Spar Tapered Column Cost Rate [$/tonne]
	 * options: None
	 * constraints: None
	 * required if: ?=4220
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_spTapColCR_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ssBall: Ballast Semi-submersible [hours]
	 * options: None
	 * constraints: None
	 * required if: ?=6
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_ssBall_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ssHeaveCR: Semi-submersible Heave Plate Cost Rate [$/tonne]
	 * options: None
	 * constraints: None
	 * required if: ?=6250
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_ssHeaveCR_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ssMoorCheck: Survey Semi-submersible Mooing Lines and Connections [hours]
	 * options: None
	 * constraints: None
	 * required if: ?=12
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_ssMoorCheck_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ssMoorCon: Connect Mooring Lines to Semi-Submersible [hours]
	 * options: None
	 * constraints: None
	 * required if: ?=22
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_ssMoorCon_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ssStifColCR: Semi-submersible Stiffened Column Cost Rate [$/tonne]
	 * options: None
	 * constraints: None
	 * required if: ?=3120
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_ssStifColCR_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set ssTrussCR: Semi-submersible Truss Cost Rate [$/tonne]
	 * options: None
	 * constraints: None
	 * required if: ?=6250
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_ssTrussCR_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set stateLease: State Leasing and Permitting Cost [$]
	 * options: None
	 * constraints: None
	 * required if: ?=250000
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_stateLease_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subTotCost: Substructure & Foundation Total Cost [$]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_subTotCost_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subTotM: Total Substructure Mass per Turbine [tonne]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_subTotM_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subsJackCR: Offshore Substation Jacket Lattice Cost Rate [$/tonne]
	 * options: None
	 * constraints: None
	 * required if: ?=6250
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_subsJackCR_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subsLoad: Offshore Substation Loadout for Installation [hours]
	 * options: None
	 * constraints: None
	 * required if: ?=60
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_subsLoad_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subsPileCR: Offshore Substation Jacket Pile Cost Rate [$/tonne]
	 * options: None
	 * constraints: None
	 * required if: ?=2250
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_subsPileCR_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subsPullIn: Cable Pull in to Offshore Substation [hours]
	 * options: None
	 * constraints: None
	 * required if: ?=48
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_subsPullIn_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subsTopDes: Offshore Substation Design Cost [$]
	 * options: None
	 * constraints: None
	 * required if: ?=4500000
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_subsTopDes_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subsTopFab: Offshore Substation Fabrication Cost [$/tonne]
	 * options: None
	 * constraints: None
	 * required if: ?=14500
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_subsTopFab_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set subsVessPos: Vessel Positioning Time Offshore Substation Installation [hours]
	 * options: None
	 * constraints: None
	 * required if: ?=6
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_subsVessPos_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set substructCont: Substructure Install Weather Contingency [%]
	 * options: None
	 * constraints: None
	 * required if: ?=0.3
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_substructCont_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set substructure: Substructure Type
	 * options: None
	 * constraints: INTEGER
	 * required if: ?=MONOPILE
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_substructure_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set surfLayRate: Cable Surface Lay Rate [m/hour]
	 * options: None
	 * constraints: None
	 * required if: ?=375
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_surfLayRate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tax_rate: Effective Tax Rate
	 * options: None
	 * constraints: None
	 * required if: ?=0.4
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_tax_rate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set topAssemblyFac: Offshore Substation Land-based Assembly Factor
	 * options: None
	 * constraints: None
	 * required if: ?=0.075
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_topAssemblyFac_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set towerD: Tower Base Diameter [m]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_towerD_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set towerInstallMethod: Tower Installation Method
	 * options: None
	 * constraints: INTEGER
	 * required if: ?=ONEPIECE
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_towerInstallMethod_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set towerM: Tower Mass [tonne]
	 * options: None
	 * constraints: None
	 * required if: ?=0
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_towerM_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set tpCover: Install Transition Piece Cover [hours]
	 * options: None
	 * constraints: None
	 * required if: ?=1.5
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_tpCover_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set turbCapEx: Turbine Capital Cost [$/kW]
	 * options: None
	 * constraints: None
	 * required if: ?=1605
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_turbCapEx_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set turbCont: Turbine Install Weather Contingency [%]
	 * options: None
	 * constraints: None
	 * required if: ?=0.3
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_turbCont_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set turbFasten: Prepare and Fasten Turbine for Transport [hours/turbine]
	 * options: None
	 * constraints: None
	 * required if: ?=8
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_turbFasten_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set turbInstallMethod: Turbine Installation Method
	 * options: None
	 * constraints: INTEGER
	 * required if: ?=INDIVIDUAL
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_turbInstallMethod_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set turbR: Turbine Rating [MW]
	 * options: None
	 * constraints: MIN=1,MAX=15
	 * required if: ?=5
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_turbR_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set upendSpar: Upend and Ballast Spar [hours]
	 * options: None
	 * constraints: None
	 * required if: ?=36
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_upendSpar_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set vesselPosJack: Vessel Positioning Time Jacket Installation [hours]
	 * options: None
	 * constraints: None
	 * required if: ?=8
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_vesselPosJack_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set vesselPosMono: Vessel Positioning Time Monopile Installation [hours]
	 * options: None
	 * constraints: None
	 * required if: ?=3
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_vesselPosMono_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set vesselPosTurb: Vessel Positioning Time Turbine Installation [hours]
	 * options: None
	 * constraints: None
	 * required if: ?=2
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_vesselPosTurb_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set waterD: Max Water Depth [m]
	 * options: None
	 * constraints: MIN=3,MAX=1000
	 * required if: ?=30
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_waterD_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set wharfRate: Wharf Loading and Unloading Cost Rate [$/tonne]
	 * options: None
	 * constraints: None
	 * required if: ?=2.75
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_wharfRate_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set winchDR: Cable Landfall Winch Day Rate [$/day]
	 * options: None
	 * constraints: None
	 * required if: ?=1000
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_winchDR_nset(SAM_table ptr, double number, SAM_error *err);

	/**
	 * Set workSpace: Offshore Substation Workspace & Accommodations Cost [$]
	 * options: None
	 * constraints: None
	 * required if: ?=2000000
	 */
	SAM_EXPORT void SAM_WindObos_Wobos_workSpace_nset(SAM_table ptr, double number, SAM_error *err);


	/**
	 * Wobos Getters
	 */

	SAM_EXPORT double SAM_WindObos_Wobos_addLocPerm_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_anchor_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_arrCab1Mass_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_arrCab2Mass_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_arrVoltage_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT const char* SAM_WindObos_Wobos_arrayCables_sget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_arrayX_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_arrayY_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_backUpGen_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_ballCR_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_bioResStudyMet_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_bioResStudyProj_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_bladeL_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_boltBlade1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_boltBlade2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_boltNacelle1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_boltNacelle2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_boltNacelle3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_boltRotor_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_boltTower_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_buryDepth_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_buryFac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_buryRate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_cab1CR_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_cab1CurrRating_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_cab1TurbInterCR_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_cab2CR_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_cab2CurrRating_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_cab2SubsInterCR_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_cab2TurbInterCR_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_cabDrillCR_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_cabDrillDist_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_cabLoadout_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_cabPullIn_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_cabSurveyCR_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_cabTerm_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_cableOptimizer_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_capital_cost_year_0_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_capital_cost_year_1_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_capital_cost_year_2_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_capital_cost_year_3_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_capital_cost_year_4_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_capital_cost_year_5_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_catLengFac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_chord_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_civilWork_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_cleanWatAct402_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_cleanWatAct404_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_coastZoneManAct_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_compRacks_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_conOpPlan_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_construction_insurance_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_crane1000DR_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_crane600DR_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_craneMobDemob_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_deaFixLeng_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_decomDiscRate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_distAtoS_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_distInterCon_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_distPort_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_distPtoA_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_distShore_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_diveTeamDR_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_dockRate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_dynCabFac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_elecCont_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_elecWork_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_endSpecAct_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_entranceExitRate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_estEnMFac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_exCabFac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_expCabCR_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_expCabLoad_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_expCabMass_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_expCurrRating_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_expSubsInterCR_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_expVoltage_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT const char* SAM_WindObos_Wobos_exportCables_sget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_faaPlan_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_feedStudy_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_groutSpreadDR_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_groutSpreadMob_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_groutTP_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_hamRate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_highVoltSG_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_hubD_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_hubH_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_inspectClear_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_instScour_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_installStrategy_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_install_contingency_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_interConVolt_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_interest_during_construction_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_jackFasten_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_jlatticeA_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_jlatticeCR_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_jpileCR_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_jpileD_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_jpileL_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_jtransCR_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_landConstruct_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_laydownCR_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_levJack_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_marMamProtAct_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_medVoltSG_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_metTowCR_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_migBirdAct_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_monoFasten_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_moorCR_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_moorCost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_moorDia_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_moorLines_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_moorLoadout_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_moorSurvey_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_moorTimeFac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_mpEmbedL_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_mpileCR_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_mpileD_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_mpileL_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_mptCR_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_mpvRentalDR_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_mtransCR_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_nCrane1000_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_nCrane600_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_nTurb_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_nacelleL_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_nacelleW_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_natHisPresAct_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_navStudyMet_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_navStudyProj_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_nepaEisMet_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_nepaEisProj_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_number_install_seasons_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_otherAncillary_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_outConShelfLease_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_physResStudyMet_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_physResStudyProj_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_pileSpreadDR_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_pileSpreadMob_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_placeJack_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_placeMP_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_placePiles_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_placeTP_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_placeTemplate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_placeTop_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_plantComm_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_preFEEDStudy_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_prepAA_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_prepGripperJack_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_prepGripperMono_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_prepHamJack_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_prepHamMono_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_prepSemi_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_prepSpar_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_prepTow_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_procurement_contingency_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_projLife_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_pwrFac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_removeHamJack_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_removeHamMono_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_rivsnHarbsAct_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_rnaM_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_rotorD_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_sSteelCR_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_saPlan_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_scourMat_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_scrapVal_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_seaSpreadDR_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_seaSpreadMob_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_shorePullIn_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_shuntCR_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_socEconStudyMet_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_socEconStudyProj_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_spMoorCheck_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_spMoorCon_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_spStifColCR_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_spTapColCR_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_ssBall_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_ssHeaveCR_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_ssMoorCheck_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_ssMoorCon_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_ssStifColCR_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_ssTrussCR_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_stateLease_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_subTotCost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_subTotM_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_subsJackCR_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_subsLoad_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_subsPileCR_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_subsPullIn_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_subsTopDes_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_subsTopFab_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_subsVessPos_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_substructCont_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_substructure_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_surfLayRate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_tax_rate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_topAssemblyFac_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_towerD_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_towerInstallMethod_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_towerM_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_tpCover_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_turbCapEx_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_turbCont_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_turbFasten_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_turbInstallMethod_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_turbR_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_upendSpar_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_vesselPosJack_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_vesselPosMono_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_vesselPosTurb_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_waterD_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_wharfRate_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_winchDR_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Wobos_workSpace_nget(SAM_table ptr, SAM_error *err);


	/**
	 * Outputs Getters
	 */

	SAM_EXPORT double SAM_WindObos_Outputs_arrCab1Cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Outputs_arrCab2Cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Outputs_arrInstTime_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Outputs_array_cable_install_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Outputs_bos_capex_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Outputs_cab1Leng_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Outputs_cab2Leng_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Outputs_cabSurvey_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Outputs_commissioning_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Outputs_construction_finance_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Outputs_construction_finance_factor_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Outputs_construction_insurance_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Outputs_decomCost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Outputs_electrical_install_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Outputs_expCabCost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Outputs_expCabLeng_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Outputs_expInstTime_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Outputs_export_cable_install_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Outputs_fixCabLeng_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Outputs_floatPrepTime_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Outputs_freeCabLeng_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Outputs_mob_demob_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Outputs_moorTime_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Outputs_nExpCab_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Outputs_nSubPerTrip_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Outputs_nTurbPerTrip_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Outputs_soft_costs_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Outputs_subDeckArea_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Outputs_subInstTime_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Outputs_subsInstTime_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Outputs_subsPileM_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Outputs_subsSubM_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Outputs_subsTopM_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Outputs_substation_install_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Outputs_substructure_install_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Outputs_systAngle_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Outputs_totAnICost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Outputs_totDevCost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Outputs_totElecCost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Outputs_totEnMCost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Outputs_totInstTime_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Outputs_totPnSCost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Outputs_total_bos_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Outputs_total_contingency_cost_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Outputs_turbDeckArea_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Outputs_turbInstTime_nget(SAM_table ptr, SAM_error *err);

	SAM_EXPORT double SAM_WindObos_Outputs_turbine_install_cost_nget(SAM_table ptr, SAM_error *err);

#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif