#ifndef __geothermalModelDefinitions__
#define __geothermalModelDefinitions__

#include <math.h>
#include <vector>
#include "lib_weatherfile.h"
#include "lib_physics.h"
#include "lib_powerblock.h"

#ifndef __geothermalEnums__
#define __geothermalEnums__

enum calculationBasis { NO_CALCULATION_BASIS, POWER_SALES, NUMBER_OF_WELLS };
enum conversionTypes { NO_CONVERSION_TYPE, BINARY, FLASH }; //}
enum resourceTypes { NO_RESOURCE_TYPE, HYDROTHERMAL, EGS };
enum flashTypes { NO_FLASH_SUBTYPE, SINGLE_FLASH_NO_TEMP_CONSTRAINT, SINGLE_FLASH_WITH_TEMP_CONSTRAINT, DUAL_FLASH_NO_TEMP_CONSTRAINT, DUAL_FLASH_WITH_TEMP_CONSTRAINT };
enum tempDeclineMethod {NO_TEMPERATURE_DECLINE_METHOD, ENTER_RATE, CALCULATE_RATE };
enum makeupAlgorithmType { NO_MAKEUP_ALGORITHM, MA_BINARY, MA_FLASH, MA_EGS }; //}
enum condenserTypes { NO_CONDENSER_TYPE, SURFACE, DIRECT_CONTACT };
enum ncgRemovalTypes { NO_NCG_TYPE, JET, VAC_PUMP, HYBRID };
enum wellCostCurveChoices { NO_COST_CURVE, LOW, MED, HIGH };
enum depthCalculationForEGS { NOT_CHOSEN, DEPTH, TEMPERATURE };
enum reservoirPressureChangeCalculation { NO_PC_CHOICE, ENTER_PC, SIMPLE_FRACTURE, K_AREA };
#endif
struct SGeothermal_Inputs
{
	SGeothermal_Inputs()
	{
		me_cb = NO_CALCULATION_BASIS; me_ct = NO_CONVERSION_TYPE; me_ft = NO_FLASH_SUBTYPE; me_tdm = NO_TEMPERATURE_DECLINE_METHOD;
		me_rt = NO_RESOURCE_TYPE; me_dc = NOT_CHOSEN; me_pc = NO_PC_CHOICE;
		mi_ModelChoice = -1; mb_CalculatePumpWork = true;
		mi_ProjectLifeYears = mi_MakeupCalculationsPerYear = mi_TotalMakeupCalculations = 0;
		md_DesiredSalesCapacityKW = md_NumberOfWells = md_PlantEfficiency = md_TemperatureDeclineRate = md_MaxTempDeclineC = md_TemperatureWetBulbC = 0.0;
		md_PressureAmbientPSI = md_ProductionFlowRateKgPerS = md_GFPumpEfficiency = md_PressureChangeAcrossSurfaceEquipmentPSI = md_ExcessPressureBar = 0.0;
		md_DiameterProductionWellInches = md_DiameterPumpCasingInches = md_DiameterInjectionWellInches = md_UserSpecifiedPumpWorkKW = 0.0;
		md_PotentialResourceMW = md_ResourceDepthM = md_TemperatureResourceC = md_TemperaturePlantDesignC = md_EGSThermalConductivity = md_EGSSpecificHeatConstant = 0.0;
		md_EGSRockDensity = md_ReservoirDeltaPressure = md_ReservoirWidthM = md_ReservoirHeightM = md_ReservoirPermeability = md_DistanceBetweenProductionInjectionWellsM = 0.0;
		md_WaterLossPercent = md_EGSFractureAperature = md_EGSNumberOfFractures = md_EGSFractureWidthM = md_EGSFractureAngle = 0.0;
		md_TemperatureEGSAmbientC = md_RatioInjectionToProduction = 0.0;
		md_AdditionalPressure = -1.0;
	}

	calculationBasis me_cb;									// { NO_CALCULATION_BASIS, POWER_SALES, NUMBER_OF_WELLS };
	conversionTypes me_ct;
	flashTypes me_ft;
	tempDeclineMethod me_tdm;
	resourceTypes me_rt;									// 1=Hydrothermal (default), !1=EGS 
	depthCalculationForEGS me_dc;							// { NOT_CHOSEN, DEPTH, TEMPERATURE };
	reservoirPressureChangeCalculation me_pc;				// 1=user enter pressure change, 2=SAM calculates it using simple fracture flow (for EGS resourceTypes only), 3=SAM calculates it using k*A (permeability x area)

	int mi_ModelChoice;										// -1 on initialization; 0=GETEM, 1=Power Block monthly, 2=Power Block hourly
	bool mb_CalculatePumpWork;								// true (default) = getem calculates pump work

	size_t mi_ProjectLifeYears;
	size_t mi_MakeupCalculationsPerYear;					// 12 (monthly) or 8760 (hourly)
	size_t mi_TotalMakeupCalculations;						// mi_ProjectLifeYears * mi_MakeupCalculationsPerYear

	double md_DesiredSalesCapacityKW;						// entered or calculated, linked to 'cb'
	double md_NumberOfWells;								// entered or calculated, depending on 'cb'
	double md_PlantEfficiency;								// not in GETEM - essentially the ratio of plant brine effectiveness to max possible brine effectiveness
	double md_TemperatureDeclineRate;						// '% per year, 3% is default
	double md_MaxTempDeclineC;								// degrees C, default = 30
	double md_TemperatureWetBulbC;							// degrees celcius - used in Flash brine effectiveness
	double md_PressureAmbientPSI;							// psi, default=14.7, mostly for use in calculating flash brine effectiveness, but also pump work
	double md_ProductionFlowRateKgPerS;						// 70 kilograms per second in one well (default FlowRate in GETEM)
	double md_GFPumpEfficiency;								// default=0.6 or 60%
	double md_PressureChangeAcrossSurfaceEquipmentPSI;		// default 25 psi
	double md_ExcessPressureBar;							// default 3.5 bar, [2B.Resource&Well Input].D205
	double md_DiameterProductionWellInches;					// default 10 inches
	double md_DiameterPumpCasingInches;						// default 9.925 inches
	double md_DiameterInjectionWellInches;					// default 10 inches
	double md_UserSpecifiedPumpWorkKW;
	double md_PotentialResourceMW;							// MW, default = 200 MW, determines how many times reservoir can be replaced
	double md_ResourceDepthM;								// meters, default 2000
	double md_TemperatureResourceC;							// degrees C, default 200
	double md_TemperatureEGSAmbientC;						// Note in GETEM spreadsheet says that this is only used in calculating resource temp or depth.  However, if EGS calculations are based on depth, then resource temp is based on this number, so all power calcs are based on it as well
	double md_TemperaturePlantDesignC;						// degrees C, default 225, only used for EGS
	double md_EGSThermalConductivity;						// default 259,200 Joules per m-day-C, [2B.Resource&Well Input].D240
	double md_EGSSpecificHeatConstant;						// default 950 Joules per kg-C, [2B.Resource&Well Input].D241
	double md_EGSRockDensity;								// default 2600 kg per cubic meter, [2B.Resource&Well Input].D242
	double md_ReservoirDeltaPressure;						// default 0.35 psi-h per 1000lb, [2B.Resource&Well Input].D171
	double md_ReservoirWidthM;
	double md_ReservoirHeightM;
	double md_ReservoirPermeability;
	double md_DistanceBetweenProductionInjectionWellsM;		// default 1500 m [2B.Resource&Well Input].F185
	double md_WaterLossPercent;								// default 2%
	double md_EGSFractureAperature;							// default 0.0004 m
	double md_EGSNumberOfFractures;							// default 6
	double md_EGSFractureWidthM;							// default 175 m
	double md_EGSFractureAngle;								// default 15 degrees
	double md_RatioInjectionToProduction;					// used in non-cost equation, so it needs to be an input
	double md_AdditionalPressure;							// manually enter additional psi for injection pumps


	const char * mc_WeatherFileName;
	int * mia_tou;											// time of use array
};

struct SGeothermal_Outputs
{
	SGeothermal_Outputs()
	{
		md_PumpWorkKW = md_NumberOfWells = md_FlashBrineEffectiveness = md_PressureHPFlashPSI = md_PressureLPFlashPSI = 0.0;
		md_GrossPlantOutputMW = md_PlantBrineEffectiveness = md_PressureChangeAcrossReservoir = md_AverageReservoirTemperatureF = 0;
		md_PumpDepthFt = md_PumpHorsePower = md_BottomHolePressure = 0;
		maf_ReplacementsByYear = maf_monthly_resource_temp = maf_monthly_power = maf_monthly_energy = maf_timestep_resource_temp = NULL;
		maf_timestep_power = maf_timestep_test_values = maf_timestep_pressure = maf_timestep_dry_bulb = maf_timestep_wet_bulb = NULL;
		mb_BrineEffectivenessCalculated = mb_FlashPressuresCalculated = false;
		maf_hourly_power = NULL;
	}

	// single values used in calculations, some also used in UI
	double md_NumberOfWells;
	double md_PumpWorkKW;

	bool mb_BrineEffectivenessCalculated;
	double md_FlashBrineEffectiveness;

	bool mb_FlashPressuresCalculated;
	double md_PressureHPFlashPSI; // D29, D64
	double md_PressureLPFlashPSI; // D30, D65

	// only for use in the interface to show 'calculated' values
	double md_PlantBrineEffectiveness;
	double md_GrossPlantOutputMW;	//double GetGrossPlantOutputMW(void) { return this->PlantOutputKW()/1000; }
	double md_PumpDepthFt;
	double md_PumpHorsePower;
	double md_PressureChangeAcrossReservoir; //double GetPressureChangeAcrossReservoir(void) { return moPPC.GetPressureChangeAcrossReservoir(); }
	double md_AverageReservoirTemperatureF; //double GetAverageReservoirTemperatureUsedF(void) { return moPPC.GetReservoirTemperatureF(); }
	double md_BottomHolePressure; //double GetBottomHolePressure(void) { return moPPC.GetBottomHolePressure(); }

	// output arrays
	float * maf_ReplacementsByYear;			// array of ones and zero's over time, ones representing years where reservoirs are replaced
	float * maf_monthly_resource_temp;
	float * maf_monthly_power;				// monthly values, even if timestep is hourly
	float * maf_monthly_energy;
	float * maf_timestep_resource_temp;
	float * maf_timestep_power;				// could be hourly or monthly, depending on timestep
	float * maf_timestep_test_values;
	float * maf_timestep_pressure;
	float * maf_timestep_dry_bulb;
	float * maf_timestep_wet_bulb;
	float * maf_hourly_power;				// hourly values even if the timestep is monthly
};

int RunGeothermalAnalysis(bool (*update_function)(float,void*),void*user_data, std::string &err_msg, 
	 const SPowerBlockParameters &pbp, SPowerBlockInputs &pbInputs, 
	 const SGeothermal_Inputs &geo_inputs, SGeothermal_Outputs &geo_outputs); 

int FillOutputsForUI( std::string &err_msg, const SGeothermal_Inputs &geo_inputs, SGeothermal_Outputs &geo_outputs);


#endif // __geothermalModelDefinitions__