#ifndef _SOLARFIELD_H_
#define _SOLARFIELD_H_ 1

#include <vector>
#include <string>

#include "string_util.h"
#include "interop.h"

#include "mod_base.h"
#include "Heliostat.h"
#include "Receiver.h"
#include "Financial.h"
#include "Ambient.h"
#include "Land.h"
#include "Plant.h"
#include "Flux.h"
#include "fluxsim.h"
#include "OpticalMesh.h"

//
//struct LAYOUT_DETAIL
//{
//	enum A {
//	//Subset of days/hours=2;Single simulation point=1;Do not filter heliostats=0;Annual simulation=3;Limited annual simulation=4;Representative profiles=5;Map to Annual=6
//	//Associated with _des_sim_detail
//	NO_FILTER=0,
//	SINGLE_POINT,
//	SUBSET_HOURS,
//	FULL_ANNUAL,
//	LIMITED_ANNUAL,
//	AVG_PROFILES,
//	MAP_TO_ANNUAL,
//	FOR_OPTIMIZATION
//	};
//};

//using namespace std;

//Declare any referenced classes first
class Receiver;
class Heliostat;
class Flux;
class Plant;
class LayoutSimThread;

class Ambient;
class Land;
class Plant;

typedef std::vector<Heliostat*> Hvector;
class sim_result;
typedef std::vector<sim_result> sim_results;

struct layout_obj
{
	/*
	This is a lightweight object that contains the information required to build a full heliostat object. 
	These objects are stored in a layout_shell type to keep track of the various layouts that are built
	during the simulation, but without building up large numbers of memory-intensive heliostat objects.
	*/

	int helio_type;
	Point 
		location,
		aim;
	double
		focal_x,
		focal_y;
	Vect
		cant;
	bool
		is_user_cant,	//Data provided on canting
		is_user_aim,	//Data provided on aiming
		is_user_focus;	//Data provided on focusing
};

struct sim_params
{
    double dni; //W/m2
    double Tamb;    //C
    double Patm;    //atm
    double Vwind;   //m/s
    double TOUweight;   //- weighting factor due to time of delivery
    double Simweight;   //- weighting factor due to simulation setup
    bool is_layout;     //Run simulation in layout mode
    
    sim_params();
};

typedef std::vector<layout_obj> layout_shell;
typedef map<int, Heliostat*> htemp_map;

/*The SolarField class will serve as the object that binds together all of the aspects of the solar field
into a single object. Multiple instances of the SolarField will be possible, and this will allow the 
maintenance and reference to a number of unique layouts and field constructions simultaneously.*/
class SolarField : public mod_base
{
protected:
    //std::string _layout_data;   //string form of layout data
	double 
		_q_to_rec,		//[MW] Power to the receiver during performance runs
		_sim_p_to_rec,	//Simulated power to the receiver. Store for plotting and reference
		_estimated_annual_power,	//Calculated in ProcessLayoutResults().. Estimate of total annual heliostat power output
		_q_des_withloss,			//[MW] The design point thermal power that must be met
        _sf_area;       //[m2] total heliostat area in the field
        //_rec_area;      //[m2] total receiver surface area

	bool
		_is_aimpoints_updated,	//Are the heliostat field aim points up to date?
		_cancel_flag,	//Flag indicating the current simulation should be cancelled
		_is_created;	//Has the solar field Create() method been called?


	double _helio_extents[4];	//Extents of the heliostat field [xmax, xmin, ymax, ymin]
	layout_shell _layout;	//All of the layouts associated with this solar field
	std::vector<Heliostat> _helio_objects; //A matrix of heliostats for each position that's evaluated in the field. 
										  //This matrix stores all of the actual heliostat objects. These are pointed 
										  //to by the _heliostats array
	htemp_map _helio_templates;	//A map from the heliostat template integer type to the actual object in memory
	std::vector<Heliostat> _helio_template_objects;	//Actual heliostat objects
	unordered_map<int,Heliostat*> _helio_by_id;	//map of heliostats by ID#
	Hvector _heliostats; //A std::vector containing all of the heliostats in the field that are used in calculation
	matrix_t<Hvector> 
		_helio_groups,	//A 2-D mesh containing vectors that list the heliostats in each field group.
		_neighbors;	//A 2-D mesh where each node lists the heliostats that neighbor each other
	std::vector<Hvector> _layout_groups; //a std::vector of heliostat vectors that share flux intercept factor during layout calculations
	std::vector<Receiver*> _receivers; //A std::vector containing all of the receiver objects
	std::vector<Receiver*> _active_receivers;	//A std::vector containing only active receivers
	Land _land;
	Financial _financial;
    FluxSimData _fluxsim;
	Plant _plant;
	Flux *_flux;	/*This object is a pointer because it has a recursive relationship to the SolarField object.
					  See the SolarField constructor for the associated _flux constructor. Also, the object must
					  be allocated and freed from memory manually.*/

	simulation_info _sim_info;
	simulation_error _sim_error;

	optical_hash_tree _optical_mesh;

    var_map *_var_map;

	class clouds : public mod_base
	{ 
		//members
		std::vector<Point> _all_locs;

	public:
		//methods
		void Create(var_map &V, double extents[2]);
		double ShadowLoss(var_map &V, Point &hloc);
	} _clouds;

public:

    //struct HELIO_SPACING_METHOD { enum A {NO_BLOCK_DENSE=3, DELSOL_EMPIRICAL=1, NO_BLOCKING=2}; };
    //struct SUNPOS_DESIGN { enum A {SOLSTICE_S, EQUINOX, SOLSTICE_W, ZENITH, USER }; };
    //struct TEMPLATE_RULE{ enum A {SINGLE=0, SPEC_RANGE=1, EVEN_DIST=2}; };

	//Constructors - destructor
	SolarField (); //constructor

	SolarField( const SolarField &sf );

	~SolarField ();
	
	//-------Access functions
	//"GETS"
	std::vector<Receiver*> *getReceivers();
	Land *getLandObject();
	Flux *getFluxObject();
	Financial *getFinancialObject();
    FluxSimData *getFluxSimObject();
	Plant *getPlantObject();
	htemp_map *getHeliostatTemplates();
	Hvector *getHeliostats();
	layout_shell *getLayoutShellObject();
	unordered_map<int,Heliostat*> *getHeliostatsByID();
	std::vector<Heliostat> *getHeliostatObjects();
	clouds *getCloudObject();
    var_map *getVarMap();
	
	double calcHeliostatArea();	//[m2] returns the total aperture area of the heliostat field
	double *getPlotBounds(bool use_land=false);	//Returns a pointer to an [4] array [xmax, xmin, ymax, ymin]
	
    std::string* getLayoutData();
    bool getAimpointStatus();
	double getSimulatedPowerToReceiver();
	double calcReceiverTotalArea();
    double calcAverageAttenuation();
	double *getHeliostatExtents();
	void copySimulationStepData(WeatherData &wdata);
	double getAnnualPowerApproximation();
	double getDesignThermalPowerWithLoss();
	double getActualThermalPowerWithLoss();
	
	simulation_info *getSimInfoObject();
	simulation_error *getSimErrorObject();
	optical_hash_tree *getOpticalHashTree();

	//-------"SETS"
	/*min/max field radius.. function sets the value in units of [m]. Can be used as follows:
	1) SetMinFieldRadius(_tht, 0.75); 
	2) SetMinFieldRadius(100, 1.0); 
	*/
	void setAimpointStatus(bool state);
	void setSimulatedPowerToReceiver(double val);
	void setHeliostatExtents(double xmax, double xmin, double ymax, double ymin);
	
	//Scripts
	void setDefaults();
	void Create(var_map &V);
    void updateCalculatedParameters(var_map &V);
    void updateAllCalculatedParameters(var_map &V);
	void Clean();
	bool ErrCheck();
	void CancelSimulation();
	bool CheckCancelStatus();
	
	bool FieldLayout();	//Master layout method for DELSOL solar field geometries
	static bool PrepareFieldLayout(SolarField &SF, WeatherData &wdata, bool refresh_only=false);	//Field layout preparation call for multithreaded apps
	static bool DoLayout( SolarField *SF, sim_results *results, WeatherData *wdata, int sim_first=-1, int sim_last=-1);
	void ProcessLayoutResults(sim_results *results, int nsim_total);	//Call after simulation for multithreaded apps
	void ProcessLayoutResultsNoSim();	//Call after layout with no simulations to process
	static void AnnualEfficiencySimulation( SolarField &SF, sim_results &results); //, double *azs, double *zens, double *met);
	static void AnnualEfficiencySimulation( std::string weather_file, SolarField *SF, sim_results &results); //, double *azs, double *zens, double *met);	//overload
	bool UpdateNeighborList(double lims[4], double zen);
	bool UpdateLayoutGroups(double lims[4]);

	void radialStaggerPositions(std::vector<Point> &HelPos); //Vector of the possible heliostat locations
	void cornfieldPositions(std::vector<Point> &HelPos);
	Heliostat *whichTemplate(int method, Point &pos);		//Function returning a pointer to the template to use
	void TemplateRange(int pos_order, int method, double *rrange, double *azrange);
	void RefactorHeliostatImages(Vect &Sun);
	
    void Simulate(double az, double zen, sim_params &P);		//Method to simulate the performance of the field
	bool SimulateTime(int hour, int day_of_Month, int month, sim_params &P);
	//bool SimulateTime(const std::string &data);
	//bool SimulateTime(double sun_elevation, double sun_azimuth, double *args, int nargs);
	
    static void SimulateHeliostatEfficiency(SolarField *SF, Vect &Sun, Heliostat *helio, sim_params &P);
	double calcShadowBlock(Heliostat *H, Heliostat *HS, int mode, Vect &Sun);	//Calculate the shadowing or blocking between two heliostats
	void updateAllTrackVectors(Vect &Sun);	//Macro for calculating corner positions
	void calcHeliostatShadows(Vect &Sun);	//Macro for calculating heliostat shadows
	void calcAllAimPoints(Vect &Sun, sim_params &P); //bool force_simple=false, bool quiet=true); 
	int getActiveReceiverCount();
	static bool parseHeliostatXYZFile(const std::string &filedat, layout_shell &layout );
	int calcNumRequiredSimulations();
	//double calcLandArea();
	double getReceiverPipingHeatLoss(); //kWt
	double getReceiverTotalHeatLoss();  //kWt
	void HermiteFluxSimulation(Hvector &helios, bool keep_existing_profile = false);
	void AnalyticalFluxSimulation(Hvector &helios);
	void CalcDimensionalFluxProfiles(Hvector &helios);
    bool CalcDesignPtSunPosition(int sun_loc_des, double &az_des, double &zen_des);

 } ;

#endif
