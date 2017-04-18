#ifndef _CSP_COMMON_
#define _CSP_COMMON_ 1

#include "core.h"
#include "AutoPilot_API.h"

class solarpilot_invoke : public var_map
{
    compute_module *m_cmod;
    AutoPilot_S *m_sapi;
	std::vector<std::vector<double> > _optimization_sim_points;
	std::vector<double>
		_optimization_objectives,
		_optimization_fluxes;

public:

	void getOptimizationSimulationHistory(std::vector<std::vector<double> > &sim_points, std::vector<double> &obj_values, std::vector<double> &flux_values);
	void setOptimizationSimulationHistory(std::vector<std::vector<double> > &sim_points, std::vector<double> &obj_values, std::vector<double> &flux_values);

    //sp_optimize opt;
    /*sp_ambient amb;
    sp_cost cost;
    sp_heliostats helios;
    sp_receivers recs;*/
    sp_layout layout;
    sp_flux_table fluxtab;
    sp_layout_table heliotab;

    solarpilot_invoke( compute_module *cm );
    ~solarpilot_invoke();
    AutoPilot_S *GetSAPI();
    bool run();
    bool postsim_calcs( compute_module *cm );
};


#endif