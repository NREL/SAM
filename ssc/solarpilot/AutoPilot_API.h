#ifndef _SOLARFIELD_API_
#define _SOLARFIELD_API_ 1


#include "API_structures.h"
#include "definitions.h"


#if defined(__WINDOWS__)&&defined(__DLL__)
#define SPEXPORT __declspec(dllexport)
#else
#define SPEXPORT
#endif


class simulation_info;
class sim_result;
class SolarField;
class LayoutSimThread;



//-------------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------------

class SPEXPORT AutoPilot 
{

protected:
	bool (*_summary_callback)(simulation_info *siminfo, void *data);
	void *_summary_callback_data;
	bool (*_detail_callback)(simulation_info *siminfo, void *data);
	void *_detail_callback_data;

	SolarField *_SF;
	//var_map _variables;
	int _sim_total;
	int _sim_complete;
	
	bool _cancel_simulation;	//changing this flag to "true" will cause the current simulation to terminate
	bool _has_summary_callback;			//A callback function has been provided to the API.
	bool _has_detail_callback;
	bool _is_solarfield_external;		//Is the SolarField object provided externally? Otherwise, it will be created and destroyed locally
	bool
		_setup_ok,	//The variable structure has been created
		_simflag;	//add bool flags here to indicate simulation/setup status

    sp_optimize _opt;

	vector<double> interpolate_vectors( vector<double> &A, vector<double> &B, double alpha);


	void PrepareFluxSimulation(sp_flux_table &fluxtab, int flux_res_x, int flux_res_y, bool is_normalized);
	void PostProcessLayout(sp_layout &layout);
	void PostProcessFlux(sim_result &result, sp_flux_map &fluxmap, int flux_layer = 0);
	

	bool CalculateFluxMapsOV1(vector<vector<double> > &sunpos, vector<vector<double> > &fluxtab, vector<double> &efficiency, 
		int flux_res_x = 12, int flux_res_y = 10, bool is_normalized = true);

	simulation_info *_summary_siminfo;
	simulation_info *_detail_siminfo;

public:
	AutoPilot();
	~AutoPilot();
	//Callbacks for progress updates
	void SetSummaryCallback( bool (*callback)(simulation_info* siminfo, void *data), void *cdata);
	void SetDetailCallback( bool (*callback)(simulation_info* siminfo, void *data), void *cdata);
	void SetSummaryCallbackStatus(bool is_enabled);
	void SetDetailCallbackStatus(bool is_enabled);
	//setup
	void PreSimCallbackUpdate();
	void SetExternalSFObject(SolarField *SF);
	bool Setup(var_map &V, bool for_optimize = false);
	//generate weather data
	void GenerateDesignPointSimulations(var_map &V, vector<string> &hourly_weather_data);
	//Simulation methods
	bool EvaluateDesign(double &obj_metric, double &flux_max, double &tot_cost);
	void PostEvaluationUpdate(int iter, vector<double> &pos,/* vector<double> &normalizers,*/ double &obj, double &flux, double &cost, std::string *note=0);
	virtual bool CreateLayout(sp_layout &layout, bool do_post_process = true);
	virtual bool CalculateOpticalEfficiencyTable(sp_optical_table &opttab);
	virtual bool CalculateFluxMaps(sp_flux_table &fluxtab, int flux_res_x = 12, int flux_res_y = 10, bool is_normalized = true);
	virtual bool CalculateFluxMaps(vector<vector<double> > &sunpos, vector<vector<double> > &fluxtab, vector<double> &efficiency, 
		int flux_res_x = 12, int flux_res_y = 10, bool is_normalized = true);
	bool Optimize(int method, vector<double*> &optvars, vector<double> &upper_range, vector<double> &lower_range, vector<double> &stepsize, vector<string> *names=0);
	bool OptimizeRSGS(vector<double*> &optvars, vector<double> &upper_range, vector<double> &lower_range, vector<bool> &is_range_constr, vector<string> *names=0);
    bool OptimizeAuto(vector<double*> &optvars, vector<double> &upper_range, vector<double> &lower_range, vector<double> &stepsize, vector<string> *names=0);
    bool OptimizeSemiAuto(vector<double*> &optvars, vector<double> &upper_range, vector<double> &lower_range, vector<bool> &is_range_constr, vector<string> *names=0);
	//cancellation methods
	void CancelSimulation();
	bool IsSimulationCancelled();
    //other
    sp_optimize *GetOptimizationObject();
    
    struct API_CANT_TYPE { enum A {NONE, ON_AXIS, EQUINOX, SOLSTICE_SUMMER, SOLSTICE_WINTER }; };
	//struct FOCUS_TYPE { enum A { FLAT, AT_SLANT, USER_DEFINED }; };
	//struct ATTEN_MODEL { enum A { DELSOL_CLEAR_DAY, DELSOL_HAZY_DAY, USER_DEFINED }; };
	
};


class SPEXPORT AutoPilot_S : public AutoPilot
{
	
	
public:
	//methods
	bool CreateLayout(sp_layout &layout, bool do_post_process = true);
	bool CalculateOpticalEfficiencyTable(sp_optical_table &opttab);
	bool CalculateFluxMaps(sp_flux_table &fluxtab, int flux_res_x = 12, int flux_res_y = 10, bool is_normalized = true);
	bool CalculateFluxMaps(vector<vector<double> > &sunpos, vector<vector<double> > &fluxtab, vector<double> &efficiency, 
		int flux_res_x = 12, int flux_res_y = 10, bool is_normalized = true);

};

#ifdef SP_USE_THREADS

class SPEXPORT AutoPilot_MT : public AutoPilot
{
	int _n_threads;	//the maximum number of threads to simulate
	int _n_threads_active;	//the number of threads currently used for simulation
	LayoutSimThread *_simthread;
	bool _in_mt_simulation;
	void CancelMTSimulation();

public:
	//constructor
	AutoPilot_MT();

	//methods
	bool CreateLayout(sp_layout &layout, bool do_post_process = true);
	bool CalculateOpticalEfficiencyTable(sp_optical_table &opttab);
	bool CalculateFluxMaps(sp_flux_table &fluxtab, int flux_res_x = 12, int flux_res_y = 10, bool is_normalized = true);
	bool CalculateFluxMaps(vector<vector<double> > &sunpos, vector<vector<double> > &fluxtab, vector<double> &efficiency, 
		int flux_res_x = 12, int flux_res_y = 10, bool is_normalized = true);

	//other methods
	bool SetMaxThreadCount(int nt);
	void CancelSimulation();
	
};

#endif // SP_USE_THREADS

#endif
