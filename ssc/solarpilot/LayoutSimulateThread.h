#ifndef _SF_SIMTHREAD_
#define _SF_SIMTHREAD_ 1


#include "SolarField.h"

#ifdef SP_USE_THREADS
#include <thread>
#include <mutex>


using namespace std;

class Heliostat;	//Forward declaration
class SolarField;
class WeatherData;
typedef vector<Heliostat*> Hvector;	//Needs declaring here


class LayoutSimThread 
{
	bool _is_user_sun_pos;		//Has the user specified sun positions? (opposed to day/time combos)
	bool _is_shadow_detail;		//Include shadowing + blocking in performance calculation?
	bool _is_flux_detail;		//Do post process for flux? (Assumes _is_shadow_detail=true)
	bool _is_flux_normalized;	//normalize the flux maps
	
	bool
		Finished,
		CancelFlag,
        FinishedWithErrors;
    
	int Nsim_complete, Nsim_total;
    string _thread_id;

	SolarField *_SF;
	int _sim_first, _sim_last, _sort_metric;
	WeatherData *_wdata;
	sim_results *_results;
	matrix_t<double> *_sol_azzen;
	sim_params _sim_params; 
    vector<string> _sim_messages;

	//wxMutex
	mutex
		StatusLock,
		CancelLock,
		FinishedLock,
        FinErrLock;

public:

	void Setup(string &tname, SolarField *SF, sim_results *results, WeatherData *wdata, 
		int sim_first, int sim_last, bool is_shadow_detail, bool is_flux_detail);

	void Setup(string &tname, SolarField *SF, sim_results *results, matrix_t<double> *sol_azzen, 
		sim_params &simpars, int sim_first, int sim_last, bool is_shadow_detail, bool is_flux_detail);

	void IsFluxmapNormalized(bool is_normal);	//set whether the fluxmap should be normalized (default TRUE)

	void CancelSimulation();

	bool IsSimulationCancelled();

	bool IsFinished();

    bool IsFinishedWithErrors();

	void UpdateStatus(int nsim_complete, int nsim_total);

	void GetStatus( int *nsim_complete, int *nsim_total);

    vector<string> *GetSimMessages();    //can be called only after simulation is terminated

	void StartThread();
//private:
	
	//void *Entry();

};


#endif // SP_USE_THREADS

#endif
