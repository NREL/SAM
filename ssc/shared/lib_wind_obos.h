#ifndef __wind_obos_h
#define __wind_obos_h

#include <vector>
using namespace std;

struct wobos //WIND OFFSHORE BOS STRUCTURE TO HOLD ALL INPUTS AND OUTPUTS AND ALLOW MEMBER FUNCTIONS TO OPERATE ON THOSE VALUES
{
	//MAIN INPUTS************************************************************************************************************
	double turbCapEx; //turbine capital cost ($/kW)
	double nTurb;//number of turbines
	double rotorD;//rotor diameter (m)
	double turbR;//turbine rating (MW)
	double hubH;//hub height (m)
	double waterD;// water depth (m)
	double distShore;//distance to shore from install site (km)
	double distPort;//distance to install site from install port (km)
	double distPtoA;//distance from install port to inshore assembly area (km) (spar only)
	double distAtoS;//distance from inshore assembly area to install site (km) (spar Only)
	int substructure; //type of substructure
	int anchor; //anchor type
	int turbInstallMethod; //turbine installation method
	int towerInstallMethod; //tower installation method
	int installStrategy; //installation vessel strategy
	int cableOptimizer; //switch to run the cable optimizer or not
	double moorLines;//number of mooring lines for floating substructures
	double buryDepth;//array and export cable burial depth (m)
	double arrayY;//turbine array spacing between turbines on same row (rotor diameters)
	double arrayX;// turbine array spacing between turbine rows (rotor diameters)
	double substructCont;//substructure install weather contingency
	double turbCont;//turbine install weather contingency
	double elecCont;//turbine install weather contingency
	double interConVolt;//grid interconnect voltage (kV)
	double distInterCon;//distance from onshore substation to grid interconnect (miles)
	double scrapVal;//scrap value of decommissioned components ($)
    double number_install_seasons; //number of vessel mobilization/install seasons

	//DETAILED INPUTS************************************************************************************************************
	//General
	double projLife;//economic lifetime of the project (years)
	double inspectClear;//inspection clearance for substructure and turbine components (m)
	double plantComm; //plant commissioning cost factor
    double procurement_contingency; //contingency factor for procurement costs
    double install_contingency; //contingency factor for installation costs
    double construction_insurance; //insurance during construction factor
    double capital_cost_year_0; //capital cost spent in year 0
    double capital_cost_year_1; //capital cost spent in year 1
    double capital_cost_year_2; //capital cost spent in year 2
    double capital_cost_year_3; //capital cost spent in year 3
    double capital_cost_year_4; //capital cost spent in year 4
    double capital_cost_year_5; //capital cost spent in year 5
    double tax_rate; //effective tax_rate (federal & state)
    double interest_during_construction; //interest rate during construction

	//Substructure & Foundation
	double mpileCR;//monopile pile cost rate ($/tonne)
	double mtransCR;//monopile transition piece cost rate ($/tonne)
	double mpileD;//monopile pile diameter (m)
	double mpileL;//monopile length (m)
	double jlatticeCR;//jacket lattice cost rate ($/tonne)
	double jtransCR;//jacket transition piece cost rate ($/tonne)
	double jpileCR;//jacket pile cost rate ($/tonne)
	double jlatticeA;//jacket lattice footprint area
	double jpileL;//jacket pile length
	double jpileD;//jacket pile diameter
	double spStifColCR;//spar stiffened column cost rate ($/tonne)
	double spTapColCR;//spar tapered column cost rate ($/tonne)
	double ballCR;//ballast cost rate ($/tonne)
	double deaFixLeng;//drag embedment anchor fixed mooring line length
	double ssStifColCR;//semisubmersible stiffened column cost rate ($/tonne)
	double ssTrussCR;// semisubmersible truss cost rate ($/tonne)
	double ssHeaveCR;//semisubmersible heave plate cost rate ($/tonne)
	double sSteelCR;//secondary steel cost rate ($/tonne)
	double moorDia;//mooring line diameter
	double moorCR;//mooring line cost rate ($/m)
	double mpEmbedL;//monopile embedment length (m)
    double scourMat;
	
	//Electrical Infrastructure
	double pwrFac;//power factor to estimate losses
	double buryFac;//cable burial factor
	double arrVoltage;//array cable voltage (kV)
	double arrCab1Size;//diameter in square millimeters of array cable 1
	double arrCab1Mass;//mass of array cable 1 (kg/m)
	double cab1CurrRating;//current rating of array cable 1 (amps)
	double cab1CR;//cost rate of array cable 1 ($/m)
	double cab1TurbInterCR;//array cable size 1 turbine interface cost rate ($/interface)
	double arrCab2Size;//diameter in square millimeters of array cable 2
	double arrCab2Mass;//mass of array cable 2 (kg/m)
	double cab2CurrRating;//current rating of array cable 2 (amps)
	double cab2CR;//cost rate of array cable 2 ($/m)
	double cab2TurbInterCR;//array cable size 2 turbine interface cost rate ($/interface)
	double cab2SubsInterCR;//array cable size 2 substation interface cost rate ($/interface)
	double catLengFac;//free hanging or catenary cable length factor
	double exCabFac;// excess cable factor
	double subsTopFab;//substation topside fabrication cost ($/tonne)
	double subsTopDes;//substation topside design cost ($)
	double topAssemblyFac;//land based substation topside assembly factor
	double subsJackCR;//substation jacket substructure cost rate ($/tonne)
	double subsPileCR;//substation jacket pile cost rate ($/tonne)
	double dynCabFac;//dynamic/free hanging cable cost premium
	double shuntCR;//shunt reactor cost rate ($/MVA)
	double highVoltSG;//high voltage switchgear cost ($)
	double medVoltSG;//medium voltage switchgear cost ($)
	double backUpGen;//back up generator cost ($)
	double workSpace;//substation workshop and accommodations cost ($)
	double otherAncillary;//substation other ancillary costs ($)
	double mptCR;//main power transformer cost rate ($/MVA)
	double expVoltage;//export cable voltage (kV)
	double expCabSize;//diameter in square millimeters of the export cable
	double expCabMass;//mass of the export cable (kg/m)
	double expCabCR;//cost rate of the export cable ($/m)
	double expCurrRating;//export cable rating (amps)
	double expSubsInterCR;//cost rate of export cable substation interfaces ($/interface)
	
	//Assembly & Installation
	double moorTimeFac;//mooring installation timing factor (hrs/m)
	double moorLoadout;//mooring system loadout timing (hrs)
	double moorSurvey;//mooring system anchor position survey timing (hrs)
	double prepAA;//prep inshore assembly area timing (hrs)
	double prepSpar;//prep spare for tow out to assembly area timing (hrs)
	double upendSpar;//upend and ballast the spar timing (hrs)
	double prepSemi;//prep semisubmersible for turbine install timing (hrs)
	double turbFasten;//fasten turbine for transport timing (hrs)
	double boltTower;// bolt tower to substructure timing (hrs)
	double boltNacelle1;//bolt nacelle to tower timing individual components method (hrs)
	double boltNacelle2;//bolt nacelle to tower timing bunny ears method (hrs)
	double boltNacelle3;//bolt nacelle to tower timing assembled rotor method (hrs)
	double boltBlade1;//bolt blade to rotor timing individual components method (hrs)
	double boltBlade2;//bolt blade to rotor timing bunny ears method (hrs)
	double boltRotor;//bolt rotor to nacelle timing assembled rotor method (hrs)
	double vesselPosTurb;//vessel positioning timing turbine install (hrs)
	double vesselPosJack;//vessel positioning timing jacket install (hrs)
	double vesselPosMono;//vessel positioning timing monopile install (hrs)
	double subsVessPos;//vessel positioning timing offshore substation install (hrs)
	double monoFasten;//fasten monopile for transport timing (hrs)
	double jackFasten;//fasten jacket for transport timing (hrs)
	double prepGripperMono;//prepare pile gripper and upender timing monopile install (hrs)
	double prepGripperJack;//prepare pile gripper and upender timing iacket install (hrs)
	double placePiles;//lift and place jacket piles timing (hrs)
	double prepHamMono;//prepare pile hammer timing monopile install (hrs)
	double removeHamMono;//remove hammer timing monopile install (hrs)
	double prepHamJack;//prepare pile hammer timing iacket install (hrs)
	double removeHamJack;//remove hammer timing iacket install (hrs)
	double placeJack;//place  jacket timing (hrs)
	double levJack;//level jacket timing (hrs)
	double placeTemplate;//place jacket template timing (hrs)
	double hamRate;//pile hammer rate (m/hr)
	double placeMP;//place monopile pile timing (hrs)
	double instScour;//install scour protection (hrs)
	double placeTP;//place transition piece on monopile timing (hrs)
	double groutTP;//grout transition piece (hrs)
	double tpCover;//install transition piece cover timing (hrs)
	double prepTow;//prep floating substructure for towing timing (hrs)
	double spMoorCon;//connect spar to mooring system timing (hrs)
	double ssMoorCon;//connect semisubmersible to mooring system (hrs)
	double spMoorCheck;//check mooring connections to spar timing (hrs)
	double ssMoorCheck;//check mooring connections to semisubmersible timing (hrs)
	double ssBall;//ballast semisubmersible timing (hrs)
	double surfLayRate;//electrical cable surface lay rate (m/hr)
	double cabPullIn;//array cable pull in to interfaces timing (hrs)
	double cabTerm;//cable termination and testing timing (hrs)
	double cabLoadout;//array cable loadout timing (hrs)
	double buryRate;//cable bury rate (m/hr)
	double subsPullIn;//cable pull in to substation timing (hrs)
	double shorePullIn;//cable pull in to shore timing (hrs)
	double landConstruct;//land construction of required onshore electrical systems timing (days)
	double expCabLoad;//export cable loadout timing (hrs)
	double subsLoad;//substation loadout timing (hrs)
	double placeTop;//lift and place substation topside timing (hrs)
	double pileSpreadDR;//piling equipment spread day rate ($/day)
	double pileSpreadMob;//piling equipment spread mobilization/demobilization cost ($)
	double groutSpreadDR;//grouting equipment spread day rate ($/day)
	double groutSpreadMob;//grouting equipment spread mobilization/demobilization cost ($)
	double seaSpreadDR;//suction pile anchor vessel and equipment spread day rate ($/day)
	double seaSpreadMob;//suction pile anchor vessel and equipment spread mobilization/demobilization cost ($)
	double compRacks;//component racks cost ($)
	double cabSurveyCR;//cost rate of surveying and verifying electrical cable installation ($/)
    double cabDrillDist;//horizontal drilling distance for cable landfall (m)
    double cabDrillCR;//horizontal drilling cost rate ($/m)
    double mpvRentalDR;//MPV rental day rate ($/day)
    double diveTeamDR;//cable landfall dive team day rate ($/day)
    double winchDR;//Cable winch day rate
    double civilWork;//civil construction work cost ($)
    double elecWork;//electrical work cost ($)
	
	//Port & Staging
	double nCrane600;
	double nCrane1000;
	double crane600DR;//600 tonne capacity crawler crane day rate ($/day)
	double crane1000DR;//1000 tonne capacity crawler crane day rate ($/day)
	double craneMobDemob;//crane mobilization and demobilization cost ($)
	double entranceExitRate;//port entrance and exit cost ($/m^2/occurrence)
	double dockRate;//port docking cost ($/day)
	double wharfRate;//port wharf loading and unloading cost ($/tonne)
	double laydownCR;//port laydown and storage cost ($/m/day)
	
	//Engineering & Management
	double estEnMFac;//estimated engineering and management cost factor
	
	//Development
	double preFEEDStudy;//pre-fornt end engineering design (FEED) study cost ($)
	double feedStudy;// FEED study cost ($)
	double stateLease;//state leasing cost ($)
	double outConShelfLease;//outer continental shelf lease cost ($)
	double saPlan;//site assessment plan cost ($)
	double conOpPlan;//construction operations plan cost ($)
	double nepaEisMet;//national environmental protection agency (NEPA) environmental impact (EIS) meteorological (met) tower study cost ($)
	double physResStudyMet;//physical resource met tower study cost ($)
	double bioResStudyMet;//biological resource met tower study ($)
	double socEconStudyMet;//socioeconomic met tower study cost ($)
	double navStudyMet;//navigation met tower study ($)
	double nepaEisProj;// NEPA EIS project site study cost ($)
	double physResStudyProj;//physical resource project site study cost ($)
	double bioResStudyProj;//biological resource project site study cost ($)
	double socEconStudyProj;//socioeconomic project site study cost ($)
	double navStudyProj;//navigation project site study cost ($)
	double coastZoneManAct;//coastal zone management act compliance cost ($)
	double rivsnHarbsAct;//rivers & harbors act section 10 compliance cost ($)
	double cleanWatAct402;//clean water act section 402 compliance cost ($)
	double cleanWatAct404;//clean water act section 404 compliance cost ($)
	double faaPlan;//federal aviation administration (FAA) plans and mitigation cost ($)
	double endSpecAct;//endangered species act compliance cost ($)
	double marMamProtAct;//marine mammal protection act compliance cost ($)
	double migBirdAct;//migratory bird act compliance ($)
	double natHisPresAct;//national historic preservation act compliance cost ($)
	double addLocPerm;//additional local and state permissions and compliance cost ($)
	double metTowCR;//meteorological tower fabrication, design, and install cost rate ($/MW)
	double decomDiscRate;//decommissioning expense discount rate

	//VECTORS TO HOLD VARIABLES************************************************************************************************************
    //cost vectors
	vector< vector<double> > turbCostsByVessel;
	vector<vector<double> > subCostsByVessel;
	vector<vector<double> > elecCostsByVessel;
	vector<vector<double> > mobDemobCostByVessel;
    //cable vectors
    vector<vector<double> > arrayVolt;
    vector<vector<vector<double> > > arrCables;
    vector<vector<double> > expCabVolt;
    vector<vector<vector<double> > > expCables;
    //vessel vectors
    vector<vector<double> > elecTugs;
    vector<double> turbInstVessel;
    vector<double> turbFeederBarge;
    vector<vector<double> >turbSupportVessels;

    vector<double> subInstVessel;
    vector<double> subFeederBarge;
    vector<double> scourProtVessel;
    vector<vector<double> > subSupportVessels;
    vector<double> arrCabInstVessel;
    vector<double> expCabInstVessel;
    vector<double> substaInstVessel;
    vector<vector<double> > elecSupportVessels;
	//OUTPUTS************************************************************************************************************
    //General outputs
    double hubD;
    double bladeL;
    double chord;
    double nacelleW;
    double nacelleL;
    double rnaM;
    double towerD;
    double towerM;
    double construction_insurance_cost;
    double total_contingency_cost;
    double construction_finance_cost;
	double construction_finance_factor; //factor for construction financing
    double soft_costs;

    //Substructure & Foundation outputs
    double mpileM;
    double mtransM;
    double mPileCost;
    double mTransCost;
    double jlatticeM;
    double jtransM;
    double jpileM;
    double jLatticeCost;
    double jTransCost;
    double jPileCost;
    double spStifColM;
    double spTapColM;
    double spStifColCost;
    double spTapColCost;
    double ballM;
    double ballCost;
    double ssStifColM;
    double ssTrussM;
    double ssHeaveM;
    double ssStifColCost;
    double ssTrussCost;
    double ssHeaveCost;
    double moorSysCost;
    double sSteelM;
    double sSteelCost;
    double subTotM;
    //Electrical Infrastructure outputs
	double systAngle;
	double freeCabLeng;
	double fixCabLeng;
    double nExpCab;
    double nSubstation;
    double fullStrings;
    double nTurbPS;
    double nTurbCab1;
    double nTurbCab2;
    double nTurbInter1;
    double nTurbInter2;
    double nSubsInter;
	double cab1Leng;
	double cab2Leng;
	double expCabLeng;
    double nMPT;
	double mptRating;
    double mptCost;
	double subsTopM;
	double subsTopCost;
    double arrCab1Cost;
    double arrCab2Cost;
    double expCabCost;
	double shuntReactors;
    double switchGear;
	double ancillarySys;
	double subsSubM;
    double subsPileM;
	double subsLandAssembly;
    double subsSubCost;
    double switchYard;
    double onShoreSubs;
    double onshoreMisc;
    double transLine;
    double subCabCost;
	double offSubsCost;
    double onshoreTransCost;
    //Assembly & Installation outputs
    double moorTime;
    double floatPrepTime;
    double turbDeckArea;
    double nTurbPerTrip;
    double turbInstTime;
    double subDeckArea;
    double nSubPerTrip;
    double subInstTime;
    double cab1SecM;
    double cab2SecM;
    double cab1SecPerTrip;
    double cab2SecPerTrip;
    double arrInstTime;
    double expCabSecM;
    double expCabSecPerTrip;
    double expInstTime;
    double subsInstTime;
    double totInstTime;
	double cabSurvey;
    double array_cable_install_cost;
    double export_cable_install_cost;
    double substation_install_cost;
    double turbine_install_cost;
    double substructure_install_cost;
    double electrical_install_cost;
    double mob_demob_cost;
    //Port & Staging outputs
	double entrExitCost;
	double wharfCost;
    double dockCost;
	double subLaydownA;
	double subLayCost;
	double turbLaydownA;
	double turbLayCost;
	double craneCost;
    double totPortCost;
    double totStageCost;
    //Development outputs
    double feedCost;
    double permStudyComp;
    double metFabCost;
    double decomCost;
    //Main Cost Outputs
    double subTotCost;
    double totElecCost;
    double totAnICost;
	double totPnSCost;
    double totEnMCost;
    double totDevCost;
    double commissioning;
    double total_bos_cost;
	
	//SUPPORTING FUNCTIONS************************************************************************************************************
	//General Module
	double HubDiameter();
	double BladeLength();
	double NacelleWidth();
	double NacelleLength();
	double RNAMass();
	double TowerDiameter();
	double TowerMass();
    void Soft_costs();
    void Insurance_during_construction();
    void Construction_finance();
    void Construction_finance_factor();
    void Total_contingency();
    void Total_bos_cost();

	//Substructure & Foundation Module
    double MonopileLength();
	double MonoPileMass();
	double MonoPileCost();
	double MonoTransMass();
	double MonoTransCost();
	double JackLatticeMass();
	double JackLatticeCost();
	double JackTransMass();
	double JackTransCost();
	double JackPileMass();
	double JackPileCost();
	double SubstructTotalMass();
	double SparStifColMass();
	double SparTapColMass();
	double BallMass();
	double SparStifColCost();
	double SparTapColCost();
	double BallCost();
	double SemiStifColMass();
	double SemiTrussMass();
	double SemiHeaveMass();
	double SemiStifColCost();
	double SemiTrussCost();
	double SemiHeaveCost();
	double MooringSys();
	double SecondarySteelMass();
	double SecondarySteelCost();
	double SubstructTotCost();

	//Electrical Infrastructure Module
	double Strings(double& cab2CurrRating, double& arrVoltage);
	double NumTurbParStr(double& cab2CurrRating, double& arrVoltage);
	double NumTurbCable1(double& cab1CurrRating, double& arrVoltage);
	double NumTurbCable2(double& cab2CurrRating, double& arrVoltage);
	double InterfacesCable1(double& fullStrings, double& nTurbPS, double& nTurbCab1);
	double InterfacesCable2(double& fullStrings, double& nTurbPS, double&nTurbCab1, double& nTurbCab2);
	double SubstationInterfaces(double& fullStrings, double& nTurbPS);
	double SystemAngle();
	double FreeCable();
	double FixedCable();
	double Cable1Length(double& nTurbInter1);
	double Cable2Length(double& nTurbCab1, double& nTurbCab2, double& fullStrings, double& nTurbPS);
	double NumberExportCable(double& expCurrRating, double& expVoltage);
	double ExportCableLength(double& nExportCab);
	double NumberSubstation(double& nExportCab);
	double NumberMPT();
	double SingleMPTRating();
	double MPTCost();
	double SubstationTopsideMass();
	double SubstationSubMass();
	double SubstationSubPileMass();
	double SubstationSubCost();
	double SubstationTopsideCost();
	double LandTopsideAssembly();
	double OnshoreSubsCost();
	double OnshoreSubsMiscCost();
	double TransLineCost();
	double SwitchYardCost();
	double ArrayCable1Cost(double& cab1Leng, double& cab1CR, double& turbInterCR1, double& nTurbInter1);
	double ArrayCable2Cost(double& cab2Leng, double& cab2CR, double& turbInterCR2, double& nTurbInter2, double& nSubsInter, double& arrSubsInterCR);
	double ExportCableCost(double& expSubsInterCR, double& expCabCR, double& expCabLeng, double& nExpCab);
	double Switchgear();
	double ShuntReactors();
	double AncillarySystems();
	double SubseaCableCost();
	double OffshoreSubstationCost();
	double OnshoreTransCost();
	double TotElectricalCost();

	//Assembly & Installation Module
	double MooringSysInstall();
	double PrepFloatSubstructure();
	double MinTurbDeckArea();
	double TurbsPerTrip();
	double MinSubDeckArea();
	double SubPerTrip();
	double TurbineInstall();
	double SubstructureInstTime();
	double Cab1SecMass(double& arrCab1Mass);
	double Cab2SecMass(double& arrCab2Mass);
	double Cab1SecPerTrip(double& cab1SecM);
	double Cab2SecPerTrip(double cab2SecM);
	double ArrayCabInstTime(double& cab1Leng, double& cab2Leng, double& nTurbInter1, double& nTurbInter2, double& subsInter,double& cab1SecPerTrip, double& cab2SecPerTrip, double& fullStrings, double& nTurbPS,
		double& nTurbCab1, double nTurbCab2);
	double ExportCableSecMass(double& expCabMass, double& exportLeng, double& nExportCab);
	double ExportCabSecPerTrip(double& expCabSecM);
	double ExportCabInstallTime(double& expCabSecPerTrip, double& nExportCab);
	double SubsInstallTime();
	double TotalInstallTime();
	void TurbInstCost();
	void SubInstCost();
	void ElectricalInstCost();
	void VesselMobDemobCost();
	double CableRouteSurveyCost();
	double TotInstCost();

	//Port & Staging Module
	double EntranceExitCost();
	double DockingCost();
	double WharfCost();
	double SubstructureLaydownArea();
	double SubstructureLaydownCost();
	double TurbLaydownArea();
	double TurbLaydownCost();
	double NumCranes();
	double CraneCost();
	double TotalPortCost();
	double TotalStagingCost();
	double TotalPnSCost();

	//Engineering & Management Module
	double TotalEnMCost();

	//Development Module
	double FEEDCost();
	double PermitsStudiesCompliance();
	double MetTowerFabnInst();
	double DecomissExpense();
	double TotalDevCost();
	double PlantCommissioning();

    //cable cost optimizing functions
    void ArrayCabCostOptimizer();
    void ExportCabCostOptimizer();

	//EXECUTE FUNCTION************************************************************************************************************
	void run();

};
extern wobos wobos_obj;
extern wobos *wobos_cm;
#endif
