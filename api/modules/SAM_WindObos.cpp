#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>

#include "SAM_api.h"
#include "ErrorHandler.h"
#include "SAM_WindObos.h"

SAM_EXPORT SAM_WindObos SAM_WindObos_construct(const char* def, SAM_error* err){
	SAM_WindObos result = nullptr;
	translateExceptions(err, [&]{
		result = ssc_data_create();
	});
	return result;
}

SAM_EXPORT int SAM_WindObos_execute(SAM_WindObos data, int verbosity, SAM_error* err){
	int n_err = 0;
	translateExceptions(err, [&]{
		n_err += SAM_module_exec("wind_obos", data, verbosity, err);
	});
	return n_err;
}


SAM_EXPORT void SAM_WindObos_destruct(SAM_WindObos system)
{
	ssc_data_free(system);
}

SAM_EXPORT void SAM_WindObos_Wobos_addLocPerm_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "addLocPerm", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_anchor_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "anchor", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_arrCab1Mass_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "arrCab1Mass", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_arrCab2Mass_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "arrCab2Mass", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_arrVoltage_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "arrVoltage", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_arrayCables_sset(SAM_WindObos ptr, const char* str, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_string(ptr, "arrayCables", str);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_arrayX_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "arrayX", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_arrayY_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "arrayY", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_backUpGen_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "backUpGen", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_ballCR_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ballCR", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_bioResStudyMet_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "bioResStudyMet", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_bioResStudyProj_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "bioResStudyProj", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_bladeL_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "bladeL", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_boltBlade1_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "boltBlade1", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_boltBlade2_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "boltBlade2", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_boltNacelle1_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "boltNacelle1", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_boltNacelle2_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "boltNacelle2", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_boltNacelle3_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "boltNacelle3", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_boltRotor_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "boltRotor", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_boltTower_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "boltTower", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_buryDepth_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "buryDepth", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_buryFac_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "buryFac", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_buryRate_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "buryRate", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_cab1CR_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cab1CR", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_cab1CurrRating_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cab1CurrRating", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_cab1TurbInterCR_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cab1TurbInterCR", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_cab2CR_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cab2CR", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_cab2CurrRating_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cab2CurrRating", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_cab2SubsInterCR_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cab2SubsInterCR", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_cab2TurbInterCR_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cab2TurbInterCR", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_cabDrillCR_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cabDrillCR", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_cabDrillDist_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cabDrillDist", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_cabLoadout_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cabLoadout", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_cabPullIn_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cabPullIn", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_cabSurveyCR_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cabSurveyCR", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_cabTerm_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cabTerm", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_cableOptimizer_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cableOptimizer", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_capital_cost_year_0_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "capital_cost_year_0", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_capital_cost_year_1_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "capital_cost_year_1", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_capital_cost_year_2_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "capital_cost_year_2", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_capital_cost_year_3_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "capital_cost_year_3", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_capital_cost_year_4_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "capital_cost_year_4", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_capital_cost_year_5_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "capital_cost_year_5", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_catLengFac_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "catLengFac", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_chord_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "chord", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_civilWork_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "civilWork", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_cleanWatAct402_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cleanWatAct402", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_cleanWatAct404_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "cleanWatAct404", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_coastZoneManAct_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "coastZoneManAct", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_compRacks_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "compRacks", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_conOpPlan_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "conOpPlan", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_construction_insurance_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "construction_insurance", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_crane1000DR_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "crane1000DR", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_crane600DR_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "crane600DR", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_craneMobDemob_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "craneMobDemob", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_deaFixLeng_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "deaFixLeng", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_decomDiscRate_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "decomDiscRate", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_distAtoS_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "distAtoS", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_distInterCon_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "distInterCon", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_distPort_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "distPort", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_distPtoA_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "distPtoA", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_distShore_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "distShore", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_diveTeamDR_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "diveTeamDR", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_dockRate_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dockRate", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_dynCabFac_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "dynCabFac", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_elecCont_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "elecCont", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_elecWork_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "elecWork", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_endSpecAct_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "endSpecAct", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_entranceExitRate_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "entranceExitRate", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_estEnMFac_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "estEnMFac", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_exCabFac_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "exCabFac", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_expCabCR_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "expCabCR", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_expCabLoad_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "expCabLoad", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_expCabMass_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "expCabMass", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_expCurrRating_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "expCurrRating", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_expSubsInterCR_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "expSubsInterCR", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_expVoltage_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "expVoltage", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_exportCables_sset(SAM_WindObos ptr, const char* str, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_string(ptr, "exportCables", str);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_faaPlan_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "faaPlan", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_feedStudy_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "feedStudy", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_groutSpreadDR_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "groutSpreadDR", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_groutSpreadMob_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "groutSpreadMob", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_groutTP_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "groutTP", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_hamRate_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hamRate", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_highVoltSG_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "highVoltSG", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_hubD_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hubD", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_hubH_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "hubH", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_inspectClear_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "inspectClear", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_instScour_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "instScour", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_installStrategy_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "installStrategy", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_install_contingency_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "install_contingency", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_interConVolt_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "interConVolt", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_interest_during_construction_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "interest_during_construction", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_jackFasten_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "jackFasten", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_jlatticeA_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "jlatticeA", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_jlatticeCR_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "jlatticeCR", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_jpileCR_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "jpileCR", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_jpileD_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "jpileD", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_jpileL_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "jpileL", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_jtransCR_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "jtransCR", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_landConstruct_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "landConstruct", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_laydownCR_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "laydownCR", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_levJack_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "levJack", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_marMamProtAct_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "marMamProtAct", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_medVoltSG_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "medVoltSG", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_metTowCR_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "metTowCR", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_migBirdAct_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "migBirdAct", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_monoFasten_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "monoFasten", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_moorCR_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "moorCR", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_moorCost_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "moorCost", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_moorDia_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "moorDia", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_moorLines_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "moorLines", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_moorLoadout_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "moorLoadout", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_moorSurvey_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "moorSurvey", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_moorTimeFac_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "moorTimeFac", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_mpEmbedL_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mpEmbedL", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_mpileCR_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mpileCR", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_mpileD_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mpileD", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_mpileL_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mpileL", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_mptCR_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mptCR", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_mpvRentalDR_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mpvRentalDR", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_mtransCR_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "mtransCR", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_nCrane1000_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "nCrane1000", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_nCrane600_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "nCrane600", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_nTurb_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "nTurb", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_nacelleL_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "nacelleL", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_nacelleW_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "nacelleW", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_natHisPresAct_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "natHisPresAct", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_navStudyMet_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "navStudyMet", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_navStudyProj_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "navStudyProj", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_nepaEisMet_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "nepaEisMet", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_nepaEisProj_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "nepaEisProj", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_number_install_seasons_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "number_install_seasons", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_otherAncillary_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "otherAncillary", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_outConShelfLease_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "outConShelfLease", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_physResStudyMet_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "physResStudyMet", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_physResStudyProj_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "physResStudyProj", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_pileSpreadDR_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pileSpreadDR", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_pileSpreadMob_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pileSpreadMob", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_placeJack_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "placeJack", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_placeMP_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "placeMP", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_placePiles_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "placePiles", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_placeTP_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "placeTP", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_placeTemplate_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "placeTemplate", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_placeTop_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "placeTop", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_plantComm_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "plantComm", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_preFEEDStudy_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "preFEEDStudy", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_prepAA_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "prepAA", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_prepGripperJack_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "prepGripperJack", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_prepGripperMono_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "prepGripperMono", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_prepHamJack_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "prepHamJack", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_prepHamMono_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "prepHamMono", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_prepSemi_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "prepSemi", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_prepSpar_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "prepSpar", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_prepTow_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "prepTow", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_procurement_contingency_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "procurement_contingency", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_projLife_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "projLife", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_pwrFac_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "pwrFac", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_removeHamJack_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "removeHamJack", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_removeHamMono_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "removeHamMono", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_rivsnHarbsAct_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rivsnHarbsAct", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_rnaM_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rnaM", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_rotorD_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "rotorD", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_sSteelCR_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "sSteelCR", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_saPlan_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "saPlan", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_scourMat_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "scourMat", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_scrapVal_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "scrapVal", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_seaSpreadDR_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "seaSpreadDR", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_seaSpreadMob_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "seaSpreadMob", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_shorePullIn_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "shorePullIn", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_shuntCR_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "shuntCR", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_socEconStudyMet_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "socEconStudyMet", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_socEconStudyProj_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "socEconStudyProj", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_spMoorCheck_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "spMoorCheck", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_spMoorCon_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "spMoorCon", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_spStifColCR_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "spStifColCR", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_spTapColCR_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "spTapColCR", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_ssBall_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ssBall", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_ssHeaveCR_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ssHeaveCR", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_ssMoorCheck_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ssMoorCheck", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_ssMoorCon_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ssMoorCon", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_ssStifColCR_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ssStifColCR", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_ssTrussCR_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "ssTrussCR", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_stateLease_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "stateLease", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_subTotCost_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subTotCost", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_subTotM_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subTotM", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_subsJackCR_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subsJackCR", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_subsLoad_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subsLoad", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_subsPileCR_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subsPileCR", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_subsPullIn_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subsPullIn", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_subsTopDes_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subsTopDes", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_subsTopFab_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subsTopFab", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_subsVessPos_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "subsVessPos", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_substructCont_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "substructCont", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_substructure_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "substructure", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_surfLayRate_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "surfLayRate", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_tax_rate_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tax_rate", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_topAssemblyFac_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "topAssemblyFac", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_towerD_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "towerD", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_towerInstallMethod_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "towerInstallMethod", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_towerM_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "towerM", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_tpCover_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "tpCover", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_turbCapEx_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "turbCapEx", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_turbCont_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "turbCont", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_turbFasten_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "turbFasten", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_turbInstallMethod_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "turbInstallMethod", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_turbR_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "turbR", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_upendSpar_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "upendSpar", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_vesselPosJack_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "vesselPosJack", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_vesselPosMono_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "vesselPosMono", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_vesselPosTurb_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "vesselPosTurb", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_waterD_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "waterD", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_wharfRate_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "wharfRate", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_winchDR_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "winchDR", number);
	});
}

SAM_EXPORT void SAM_WindObos_Wobos_workSpace_nset(SAM_WindObos ptr, double number, SAM_error *err){
	translateExceptions(err, [&]{
		ssc_data_set_number(ptr, "workSpace", number);
	});
}

SAM_EXPORT double SAM_WindObos_Wobos_addLocPerm_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "addLocPerm", &result))
		make_access_error("SAM_WindObos", "addLocPerm");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_anchor_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "anchor", &result))
		make_access_error("SAM_WindObos", "anchor");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_arrCab1Mass_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "arrCab1Mass", &result))
		make_access_error("SAM_WindObos", "arrCab1Mass");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_arrCab2Mass_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "arrCab2Mass", &result))
		make_access_error("SAM_WindObos", "arrCab2Mass");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_arrVoltage_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "arrVoltage", &result))
		make_access_error("SAM_WindObos", "arrVoltage");
	});
	return result;
}



SAM_EXPORT const char* SAM_WindObos_Wobos_arrayCables_sget(SAM_WindObos ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "arrayCables");
	if (!result)
		make_access_error("SAM_WindObos", "arrayCables");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_arrayX_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "arrayX", &result))
		make_access_error("SAM_WindObos", "arrayX");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_arrayY_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "arrayY", &result))
		make_access_error("SAM_WindObos", "arrayY");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_backUpGen_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "backUpGen", &result))
		make_access_error("SAM_WindObos", "backUpGen");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_ballCR_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ballCR", &result))
		make_access_error("SAM_WindObos", "ballCR");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_bioResStudyMet_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "bioResStudyMet", &result))
		make_access_error("SAM_WindObos", "bioResStudyMet");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_bioResStudyProj_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "bioResStudyProj", &result))
		make_access_error("SAM_WindObos", "bioResStudyProj");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_bladeL_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "bladeL", &result))
		make_access_error("SAM_WindObos", "bladeL");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_boltBlade1_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "boltBlade1", &result))
		make_access_error("SAM_WindObos", "boltBlade1");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_boltBlade2_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "boltBlade2", &result))
		make_access_error("SAM_WindObos", "boltBlade2");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_boltNacelle1_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "boltNacelle1", &result))
		make_access_error("SAM_WindObos", "boltNacelle1");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_boltNacelle2_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "boltNacelle2", &result))
		make_access_error("SAM_WindObos", "boltNacelle2");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_boltNacelle3_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "boltNacelle3", &result))
		make_access_error("SAM_WindObos", "boltNacelle3");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_boltRotor_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "boltRotor", &result))
		make_access_error("SAM_WindObos", "boltRotor");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_boltTower_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "boltTower", &result))
		make_access_error("SAM_WindObos", "boltTower");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_buryDepth_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "buryDepth", &result))
		make_access_error("SAM_WindObos", "buryDepth");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_buryFac_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "buryFac", &result))
		make_access_error("SAM_WindObos", "buryFac");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_buryRate_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "buryRate", &result))
		make_access_error("SAM_WindObos", "buryRate");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_cab1CR_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cab1CR", &result))
		make_access_error("SAM_WindObos", "cab1CR");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_cab1CurrRating_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cab1CurrRating", &result))
		make_access_error("SAM_WindObos", "cab1CurrRating");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_cab1TurbInterCR_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cab1TurbInterCR", &result))
		make_access_error("SAM_WindObos", "cab1TurbInterCR");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_cab2CR_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cab2CR", &result))
		make_access_error("SAM_WindObos", "cab2CR");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_cab2CurrRating_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cab2CurrRating", &result))
		make_access_error("SAM_WindObos", "cab2CurrRating");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_cab2SubsInterCR_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cab2SubsInterCR", &result))
		make_access_error("SAM_WindObos", "cab2SubsInterCR");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_cab2TurbInterCR_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cab2TurbInterCR", &result))
		make_access_error("SAM_WindObos", "cab2TurbInterCR");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_cabDrillCR_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cabDrillCR", &result))
		make_access_error("SAM_WindObos", "cabDrillCR");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_cabDrillDist_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cabDrillDist", &result))
		make_access_error("SAM_WindObos", "cabDrillDist");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_cabLoadout_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cabLoadout", &result))
		make_access_error("SAM_WindObos", "cabLoadout");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_cabPullIn_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cabPullIn", &result))
		make_access_error("SAM_WindObos", "cabPullIn");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_cabSurveyCR_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cabSurveyCR", &result))
		make_access_error("SAM_WindObos", "cabSurveyCR");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_cabTerm_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cabTerm", &result))
		make_access_error("SAM_WindObos", "cabTerm");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_cableOptimizer_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cableOptimizer", &result))
		make_access_error("SAM_WindObos", "cableOptimizer");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_capital_cost_year_0_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "capital_cost_year_0", &result))
		make_access_error("SAM_WindObos", "capital_cost_year_0");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_capital_cost_year_1_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "capital_cost_year_1", &result))
		make_access_error("SAM_WindObos", "capital_cost_year_1");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_capital_cost_year_2_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "capital_cost_year_2", &result))
		make_access_error("SAM_WindObos", "capital_cost_year_2");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_capital_cost_year_3_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "capital_cost_year_3", &result))
		make_access_error("SAM_WindObos", "capital_cost_year_3");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_capital_cost_year_4_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "capital_cost_year_4", &result))
		make_access_error("SAM_WindObos", "capital_cost_year_4");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_capital_cost_year_5_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "capital_cost_year_5", &result))
		make_access_error("SAM_WindObos", "capital_cost_year_5");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_catLengFac_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "catLengFac", &result))
		make_access_error("SAM_WindObos", "catLengFac");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_chord_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "chord", &result))
		make_access_error("SAM_WindObos", "chord");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_civilWork_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "civilWork", &result))
		make_access_error("SAM_WindObos", "civilWork");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_cleanWatAct402_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cleanWatAct402", &result))
		make_access_error("SAM_WindObos", "cleanWatAct402");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_cleanWatAct404_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cleanWatAct404", &result))
		make_access_error("SAM_WindObos", "cleanWatAct404");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_coastZoneManAct_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "coastZoneManAct", &result))
		make_access_error("SAM_WindObos", "coastZoneManAct");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_compRacks_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "compRacks", &result))
		make_access_error("SAM_WindObos", "compRacks");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_conOpPlan_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "conOpPlan", &result))
		make_access_error("SAM_WindObos", "conOpPlan");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_construction_insurance_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "construction_insurance", &result))
		make_access_error("SAM_WindObos", "construction_insurance");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_crane1000DR_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "crane1000DR", &result))
		make_access_error("SAM_WindObos", "crane1000DR");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_crane600DR_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "crane600DR", &result))
		make_access_error("SAM_WindObos", "crane600DR");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_craneMobDemob_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "craneMobDemob", &result))
		make_access_error("SAM_WindObos", "craneMobDemob");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_deaFixLeng_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "deaFixLeng", &result))
		make_access_error("SAM_WindObos", "deaFixLeng");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_decomDiscRate_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "decomDiscRate", &result))
		make_access_error("SAM_WindObos", "decomDiscRate");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_distAtoS_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "distAtoS", &result))
		make_access_error("SAM_WindObos", "distAtoS");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_distInterCon_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "distInterCon", &result))
		make_access_error("SAM_WindObos", "distInterCon");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_distPort_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "distPort", &result))
		make_access_error("SAM_WindObos", "distPort");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_distPtoA_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "distPtoA", &result))
		make_access_error("SAM_WindObos", "distPtoA");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_distShore_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "distShore", &result))
		make_access_error("SAM_WindObos", "distShore");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_diveTeamDR_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "diveTeamDR", &result))
		make_access_error("SAM_WindObos", "diveTeamDR");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_dockRate_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dockRate", &result))
		make_access_error("SAM_WindObos", "dockRate");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_dynCabFac_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "dynCabFac", &result))
		make_access_error("SAM_WindObos", "dynCabFac");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_elecCont_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "elecCont", &result))
		make_access_error("SAM_WindObos", "elecCont");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_elecWork_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "elecWork", &result))
		make_access_error("SAM_WindObos", "elecWork");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_endSpecAct_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "endSpecAct", &result))
		make_access_error("SAM_WindObos", "endSpecAct");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_entranceExitRate_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "entranceExitRate", &result))
		make_access_error("SAM_WindObos", "entranceExitRate");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_estEnMFac_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "estEnMFac", &result))
		make_access_error("SAM_WindObos", "estEnMFac");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_exCabFac_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "exCabFac", &result))
		make_access_error("SAM_WindObos", "exCabFac");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_expCabCR_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "expCabCR", &result))
		make_access_error("SAM_WindObos", "expCabCR");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_expCabLoad_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "expCabLoad", &result))
		make_access_error("SAM_WindObos", "expCabLoad");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_expCabMass_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "expCabMass", &result))
		make_access_error("SAM_WindObos", "expCabMass");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_expCurrRating_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "expCurrRating", &result))
		make_access_error("SAM_WindObos", "expCurrRating");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_expSubsInterCR_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "expSubsInterCR", &result))
		make_access_error("SAM_WindObos", "expSubsInterCR");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_expVoltage_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "expVoltage", &result))
		make_access_error("SAM_WindObos", "expVoltage");
	});
	return result;
}



SAM_EXPORT const char* SAM_WindObos_Wobos_exportCables_sget(SAM_WindObos ptr, SAM_error *err){
	const char* result = nullptr;
	translateExceptions(err, [&]{
	result = ssc_data_get_string(ptr, "exportCables");
	if (!result)
		make_access_error("SAM_WindObos", "exportCables");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_faaPlan_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "faaPlan", &result))
		make_access_error("SAM_WindObos", "faaPlan");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_feedStudy_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "feedStudy", &result))
		make_access_error("SAM_WindObos", "feedStudy");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_groutSpreadDR_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "groutSpreadDR", &result))
		make_access_error("SAM_WindObos", "groutSpreadDR");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_groutSpreadMob_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "groutSpreadMob", &result))
		make_access_error("SAM_WindObos", "groutSpreadMob");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_groutTP_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "groutTP", &result))
		make_access_error("SAM_WindObos", "groutTP");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_hamRate_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hamRate", &result))
		make_access_error("SAM_WindObos", "hamRate");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_highVoltSG_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "highVoltSG", &result))
		make_access_error("SAM_WindObos", "highVoltSG");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_hubD_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hubD", &result))
		make_access_error("SAM_WindObos", "hubD");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_hubH_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "hubH", &result))
		make_access_error("SAM_WindObos", "hubH");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_inspectClear_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "inspectClear", &result))
		make_access_error("SAM_WindObos", "inspectClear");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_instScour_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "instScour", &result))
		make_access_error("SAM_WindObos", "instScour");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_installStrategy_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "installStrategy", &result))
		make_access_error("SAM_WindObos", "installStrategy");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_install_contingency_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "install_contingency", &result))
		make_access_error("SAM_WindObos", "install_contingency");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_interConVolt_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "interConVolt", &result))
		make_access_error("SAM_WindObos", "interConVolt");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_interest_during_construction_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "interest_during_construction", &result))
		make_access_error("SAM_WindObos", "interest_during_construction");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_jackFasten_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "jackFasten", &result))
		make_access_error("SAM_WindObos", "jackFasten");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_jlatticeA_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "jlatticeA", &result))
		make_access_error("SAM_WindObos", "jlatticeA");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_jlatticeCR_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "jlatticeCR", &result))
		make_access_error("SAM_WindObos", "jlatticeCR");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_jpileCR_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "jpileCR", &result))
		make_access_error("SAM_WindObos", "jpileCR");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_jpileD_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "jpileD", &result))
		make_access_error("SAM_WindObos", "jpileD");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_jpileL_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "jpileL", &result))
		make_access_error("SAM_WindObos", "jpileL");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_jtransCR_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "jtransCR", &result))
		make_access_error("SAM_WindObos", "jtransCR");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_landConstruct_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "landConstruct", &result))
		make_access_error("SAM_WindObos", "landConstruct");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_laydownCR_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "laydownCR", &result))
		make_access_error("SAM_WindObos", "laydownCR");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_levJack_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "levJack", &result))
		make_access_error("SAM_WindObos", "levJack");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_marMamProtAct_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "marMamProtAct", &result))
		make_access_error("SAM_WindObos", "marMamProtAct");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_medVoltSG_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "medVoltSG", &result))
		make_access_error("SAM_WindObos", "medVoltSG");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_metTowCR_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "metTowCR", &result))
		make_access_error("SAM_WindObos", "metTowCR");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_migBirdAct_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "migBirdAct", &result))
		make_access_error("SAM_WindObos", "migBirdAct");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_monoFasten_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "monoFasten", &result))
		make_access_error("SAM_WindObos", "monoFasten");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_moorCR_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "moorCR", &result))
		make_access_error("SAM_WindObos", "moorCR");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_moorCost_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "moorCost", &result))
		make_access_error("SAM_WindObos", "moorCost");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_moorDia_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "moorDia", &result))
		make_access_error("SAM_WindObos", "moorDia");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_moorLines_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "moorLines", &result))
		make_access_error("SAM_WindObos", "moorLines");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_moorLoadout_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "moorLoadout", &result))
		make_access_error("SAM_WindObos", "moorLoadout");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_moorSurvey_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "moorSurvey", &result))
		make_access_error("SAM_WindObos", "moorSurvey");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_moorTimeFac_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "moorTimeFac", &result))
		make_access_error("SAM_WindObos", "moorTimeFac");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_mpEmbedL_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mpEmbedL", &result))
		make_access_error("SAM_WindObos", "mpEmbedL");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_mpileCR_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mpileCR", &result))
		make_access_error("SAM_WindObos", "mpileCR");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_mpileD_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mpileD", &result))
		make_access_error("SAM_WindObos", "mpileD");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_mpileL_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mpileL", &result))
		make_access_error("SAM_WindObos", "mpileL");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_mptCR_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mptCR", &result))
		make_access_error("SAM_WindObos", "mptCR");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_mpvRentalDR_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mpvRentalDR", &result))
		make_access_error("SAM_WindObos", "mpvRentalDR");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_mtransCR_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mtransCR", &result))
		make_access_error("SAM_WindObos", "mtransCR");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_nCrane1000_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "nCrane1000", &result))
		make_access_error("SAM_WindObos", "nCrane1000");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_nCrane600_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "nCrane600", &result))
		make_access_error("SAM_WindObos", "nCrane600");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_nTurb_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "nTurb", &result))
		make_access_error("SAM_WindObos", "nTurb");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_nacelleL_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "nacelleL", &result))
		make_access_error("SAM_WindObos", "nacelleL");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_nacelleW_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "nacelleW", &result))
		make_access_error("SAM_WindObos", "nacelleW");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_natHisPresAct_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "natHisPresAct", &result))
		make_access_error("SAM_WindObos", "natHisPresAct");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_navStudyMet_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "navStudyMet", &result))
		make_access_error("SAM_WindObos", "navStudyMet");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_navStudyProj_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "navStudyProj", &result))
		make_access_error("SAM_WindObos", "navStudyProj");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_nepaEisMet_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "nepaEisMet", &result))
		make_access_error("SAM_WindObos", "nepaEisMet");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_nepaEisProj_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "nepaEisProj", &result))
		make_access_error("SAM_WindObos", "nepaEisProj");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_number_install_seasons_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "number_install_seasons", &result))
		make_access_error("SAM_WindObos", "number_install_seasons");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_otherAncillary_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "otherAncillary", &result))
		make_access_error("SAM_WindObos", "otherAncillary");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_outConShelfLease_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "outConShelfLease", &result))
		make_access_error("SAM_WindObos", "outConShelfLease");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_physResStudyMet_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "physResStudyMet", &result))
		make_access_error("SAM_WindObos", "physResStudyMet");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_physResStudyProj_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "physResStudyProj", &result))
		make_access_error("SAM_WindObos", "physResStudyProj");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_pileSpreadDR_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pileSpreadDR", &result))
		make_access_error("SAM_WindObos", "pileSpreadDR");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_pileSpreadMob_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pileSpreadMob", &result))
		make_access_error("SAM_WindObos", "pileSpreadMob");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_placeJack_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "placeJack", &result))
		make_access_error("SAM_WindObos", "placeJack");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_placeMP_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "placeMP", &result))
		make_access_error("SAM_WindObos", "placeMP");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_placePiles_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "placePiles", &result))
		make_access_error("SAM_WindObos", "placePiles");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_placeTP_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "placeTP", &result))
		make_access_error("SAM_WindObos", "placeTP");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_placeTemplate_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "placeTemplate", &result))
		make_access_error("SAM_WindObos", "placeTemplate");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_placeTop_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "placeTop", &result))
		make_access_error("SAM_WindObos", "placeTop");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_plantComm_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "plantComm", &result))
		make_access_error("SAM_WindObos", "plantComm");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_preFEEDStudy_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "preFEEDStudy", &result))
		make_access_error("SAM_WindObos", "preFEEDStudy");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_prepAA_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "prepAA", &result))
		make_access_error("SAM_WindObos", "prepAA");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_prepGripperJack_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "prepGripperJack", &result))
		make_access_error("SAM_WindObos", "prepGripperJack");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_prepGripperMono_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "prepGripperMono", &result))
		make_access_error("SAM_WindObos", "prepGripperMono");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_prepHamJack_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "prepHamJack", &result))
		make_access_error("SAM_WindObos", "prepHamJack");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_prepHamMono_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "prepHamMono", &result))
		make_access_error("SAM_WindObos", "prepHamMono");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_prepSemi_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "prepSemi", &result))
		make_access_error("SAM_WindObos", "prepSemi");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_prepSpar_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "prepSpar", &result))
		make_access_error("SAM_WindObos", "prepSpar");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_prepTow_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "prepTow", &result))
		make_access_error("SAM_WindObos", "prepTow");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_procurement_contingency_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "procurement_contingency", &result))
		make_access_error("SAM_WindObos", "procurement_contingency");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_projLife_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "projLife", &result))
		make_access_error("SAM_WindObos", "projLife");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_pwrFac_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "pwrFac", &result))
		make_access_error("SAM_WindObos", "pwrFac");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_removeHamJack_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "removeHamJack", &result))
		make_access_error("SAM_WindObos", "removeHamJack");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_removeHamMono_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "removeHamMono", &result))
		make_access_error("SAM_WindObos", "removeHamMono");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_rivsnHarbsAct_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rivsnHarbsAct", &result))
		make_access_error("SAM_WindObos", "rivsnHarbsAct");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_rnaM_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rnaM", &result))
		make_access_error("SAM_WindObos", "rnaM");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_rotorD_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "rotorD", &result))
		make_access_error("SAM_WindObos", "rotorD");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_sSteelCR_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "sSteelCR", &result))
		make_access_error("SAM_WindObos", "sSteelCR");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_saPlan_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "saPlan", &result))
		make_access_error("SAM_WindObos", "saPlan");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_scourMat_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "scourMat", &result))
		make_access_error("SAM_WindObos", "scourMat");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_scrapVal_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "scrapVal", &result))
		make_access_error("SAM_WindObos", "scrapVal");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_seaSpreadDR_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "seaSpreadDR", &result))
		make_access_error("SAM_WindObos", "seaSpreadDR");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_seaSpreadMob_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "seaSpreadMob", &result))
		make_access_error("SAM_WindObos", "seaSpreadMob");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_shorePullIn_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "shorePullIn", &result))
		make_access_error("SAM_WindObos", "shorePullIn");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_shuntCR_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "shuntCR", &result))
		make_access_error("SAM_WindObos", "shuntCR");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_socEconStudyMet_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "socEconStudyMet", &result))
		make_access_error("SAM_WindObos", "socEconStudyMet");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_socEconStudyProj_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "socEconStudyProj", &result))
		make_access_error("SAM_WindObos", "socEconStudyProj");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_spMoorCheck_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "spMoorCheck", &result))
		make_access_error("SAM_WindObos", "spMoorCheck");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_spMoorCon_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "spMoorCon", &result))
		make_access_error("SAM_WindObos", "spMoorCon");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_spStifColCR_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "spStifColCR", &result))
		make_access_error("SAM_WindObos", "spStifColCR");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_spTapColCR_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "spTapColCR", &result))
		make_access_error("SAM_WindObos", "spTapColCR");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_ssBall_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ssBall", &result))
		make_access_error("SAM_WindObos", "ssBall");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_ssHeaveCR_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ssHeaveCR", &result))
		make_access_error("SAM_WindObos", "ssHeaveCR");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_ssMoorCheck_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ssMoorCheck", &result))
		make_access_error("SAM_WindObos", "ssMoorCheck");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_ssMoorCon_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ssMoorCon", &result))
		make_access_error("SAM_WindObos", "ssMoorCon");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_ssStifColCR_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ssStifColCR", &result))
		make_access_error("SAM_WindObos", "ssStifColCR");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_ssTrussCR_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "ssTrussCR", &result))
		make_access_error("SAM_WindObos", "ssTrussCR");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_stateLease_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "stateLease", &result))
		make_access_error("SAM_WindObos", "stateLease");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_subTotCost_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subTotCost", &result))
		make_access_error("SAM_WindObos", "subTotCost");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_subTotM_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subTotM", &result))
		make_access_error("SAM_WindObos", "subTotM");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_subsJackCR_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subsJackCR", &result))
		make_access_error("SAM_WindObos", "subsJackCR");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_subsLoad_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subsLoad", &result))
		make_access_error("SAM_WindObos", "subsLoad");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_subsPileCR_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subsPileCR", &result))
		make_access_error("SAM_WindObos", "subsPileCR");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_subsPullIn_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subsPullIn", &result))
		make_access_error("SAM_WindObos", "subsPullIn");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_subsTopDes_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subsTopDes", &result))
		make_access_error("SAM_WindObos", "subsTopDes");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_subsTopFab_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subsTopFab", &result))
		make_access_error("SAM_WindObos", "subsTopFab");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_subsVessPos_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subsVessPos", &result))
		make_access_error("SAM_WindObos", "subsVessPos");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_substructCont_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "substructCont", &result))
		make_access_error("SAM_WindObos", "substructCont");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_substructure_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "substructure", &result))
		make_access_error("SAM_WindObos", "substructure");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_surfLayRate_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "surfLayRate", &result))
		make_access_error("SAM_WindObos", "surfLayRate");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_tax_rate_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tax_rate", &result))
		make_access_error("SAM_WindObos", "tax_rate");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_topAssemblyFac_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "topAssemblyFac", &result))
		make_access_error("SAM_WindObos", "topAssemblyFac");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_towerD_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "towerD", &result))
		make_access_error("SAM_WindObos", "towerD");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_towerInstallMethod_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "towerInstallMethod", &result))
		make_access_error("SAM_WindObos", "towerInstallMethod");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_towerM_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "towerM", &result))
		make_access_error("SAM_WindObos", "towerM");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_tpCover_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "tpCover", &result))
		make_access_error("SAM_WindObos", "tpCover");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_turbCapEx_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "turbCapEx", &result))
		make_access_error("SAM_WindObos", "turbCapEx");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_turbCont_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "turbCont", &result))
		make_access_error("SAM_WindObos", "turbCont");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_turbFasten_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "turbFasten", &result))
		make_access_error("SAM_WindObos", "turbFasten");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_turbInstallMethod_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "turbInstallMethod", &result))
		make_access_error("SAM_WindObos", "turbInstallMethod");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_turbR_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "turbR", &result))
		make_access_error("SAM_WindObos", "turbR");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_upendSpar_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "upendSpar", &result))
		make_access_error("SAM_WindObos", "upendSpar");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_vesselPosJack_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "vesselPosJack", &result))
		make_access_error("SAM_WindObos", "vesselPosJack");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_vesselPosMono_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "vesselPosMono", &result))
		make_access_error("SAM_WindObos", "vesselPosMono");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_vesselPosTurb_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "vesselPosTurb", &result))
		make_access_error("SAM_WindObos", "vesselPosTurb");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_waterD_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "waterD", &result))
		make_access_error("SAM_WindObos", "waterD");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_wharfRate_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "wharfRate", &result))
		make_access_error("SAM_WindObos", "wharfRate");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_winchDR_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "winchDR", &result))
		make_access_error("SAM_WindObos", "winchDR");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Wobos_workSpace_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "workSpace", &result))
		make_access_error("SAM_WindObos", "workSpace");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Outputs_arrCab1Cost_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "arrCab1Cost", &result))
		make_access_error("SAM_WindObos", "arrCab1Cost");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Outputs_arrCab2Cost_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "arrCab2Cost", &result))
		make_access_error("SAM_WindObos", "arrCab2Cost");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Outputs_arrInstTime_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "arrInstTime", &result))
		make_access_error("SAM_WindObos", "arrInstTime");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Outputs_array_cable_install_cost_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "array_cable_install_cost", &result))
		make_access_error("SAM_WindObos", "array_cable_install_cost");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Outputs_bos_capex_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "bos_capex", &result))
		make_access_error("SAM_WindObos", "bos_capex");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Outputs_cab1Leng_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cab1Leng", &result))
		make_access_error("SAM_WindObos", "cab1Leng");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Outputs_cab2Leng_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cab2Leng", &result))
		make_access_error("SAM_WindObos", "cab2Leng");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Outputs_cabSurvey_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "cabSurvey", &result))
		make_access_error("SAM_WindObos", "cabSurvey");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Outputs_commissioning_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "commissioning", &result))
		make_access_error("SAM_WindObos", "commissioning");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Outputs_construction_finance_cost_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "construction_finance_cost", &result))
		make_access_error("SAM_WindObos", "construction_finance_cost");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Outputs_construction_finance_factor_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "construction_finance_factor", &result))
		make_access_error("SAM_WindObos", "construction_finance_factor");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Outputs_construction_insurance_cost_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "construction_insurance_cost", &result))
		make_access_error("SAM_WindObos", "construction_insurance_cost");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Outputs_decomCost_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "decomCost", &result))
		make_access_error("SAM_WindObos", "decomCost");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Outputs_electrical_install_cost_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "electrical_install_cost", &result))
		make_access_error("SAM_WindObos", "electrical_install_cost");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Outputs_expCabCost_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "expCabCost", &result))
		make_access_error("SAM_WindObos", "expCabCost");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Outputs_expCabLeng_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "expCabLeng", &result))
		make_access_error("SAM_WindObos", "expCabLeng");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Outputs_expInstTime_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "expInstTime", &result))
		make_access_error("SAM_WindObos", "expInstTime");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Outputs_export_cable_install_cost_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "export_cable_install_cost", &result))
		make_access_error("SAM_WindObos", "export_cable_install_cost");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Outputs_fixCabLeng_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "fixCabLeng", &result))
		make_access_error("SAM_WindObos", "fixCabLeng");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Outputs_floatPrepTime_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "floatPrepTime", &result))
		make_access_error("SAM_WindObos", "floatPrepTime");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Outputs_freeCabLeng_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "freeCabLeng", &result))
		make_access_error("SAM_WindObos", "freeCabLeng");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Outputs_mob_demob_cost_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "mob_demob_cost", &result))
		make_access_error("SAM_WindObos", "mob_demob_cost");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Outputs_moorTime_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "moorTime", &result))
		make_access_error("SAM_WindObos", "moorTime");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Outputs_nExpCab_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "nExpCab", &result))
		make_access_error("SAM_WindObos", "nExpCab");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Outputs_nSubPerTrip_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "nSubPerTrip", &result))
		make_access_error("SAM_WindObos", "nSubPerTrip");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Outputs_nTurbPerTrip_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "nTurbPerTrip", &result))
		make_access_error("SAM_WindObos", "nTurbPerTrip");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Outputs_soft_costs_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "soft_costs", &result))
		make_access_error("SAM_WindObos", "soft_costs");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Outputs_subDeckArea_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subDeckArea", &result))
		make_access_error("SAM_WindObos", "subDeckArea");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Outputs_subInstTime_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subInstTime", &result))
		make_access_error("SAM_WindObos", "subInstTime");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Outputs_subsInstTime_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subsInstTime", &result))
		make_access_error("SAM_WindObos", "subsInstTime");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Outputs_subsPileM_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subsPileM", &result))
		make_access_error("SAM_WindObos", "subsPileM");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Outputs_subsSubM_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subsSubM", &result))
		make_access_error("SAM_WindObos", "subsSubM");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Outputs_subsTopM_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "subsTopM", &result))
		make_access_error("SAM_WindObos", "subsTopM");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Outputs_substation_install_cost_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "substation_install_cost", &result))
		make_access_error("SAM_WindObos", "substation_install_cost");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Outputs_substructure_install_cost_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "substructure_install_cost", &result))
		make_access_error("SAM_WindObos", "substructure_install_cost");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Outputs_systAngle_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "systAngle", &result))
		make_access_error("SAM_WindObos", "systAngle");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Outputs_totAnICost_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "totAnICost", &result))
		make_access_error("SAM_WindObos", "totAnICost");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Outputs_totDevCost_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "totDevCost", &result))
		make_access_error("SAM_WindObos", "totDevCost");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Outputs_totElecCost_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "totElecCost", &result))
		make_access_error("SAM_WindObos", "totElecCost");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Outputs_totEnMCost_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "totEnMCost", &result))
		make_access_error("SAM_WindObos", "totEnMCost");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Outputs_totInstTime_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "totInstTime", &result))
		make_access_error("SAM_WindObos", "totInstTime");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Outputs_totPnSCost_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "totPnSCost", &result))
		make_access_error("SAM_WindObos", "totPnSCost");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Outputs_total_bos_cost_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_bos_cost", &result))
		make_access_error("SAM_WindObos", "total_bos_cost");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Outputs_total_contingency_cost_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "total_contingency_cost", &result))
		make_access_error("SAM_WindObos", "total_contingency_cost");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Outputs_turbDeckArea_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "turbDeckArea", &result))
		make_access_error("SAM_WindObos", "turbDeckArea");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Outputs_turbInstTime_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "turbInstTime", &result))
		make_access_error("SAM_WindObos", "turbInstTime");
	});
	return result;
}



SAM_EXPORT double SAM_WindObos_Outputs_turbine_install_cost_nget(SAM_WindObos ptr, SAM_error *err){
	double result;
	translateExceptions(err, [&]{
	if (!ssc_data_get_number(ptr, "turbine_install_cost", &result))
		make_access_error("SAM_WindObos", "turbine_install_cost");
	});
	return result;
}



