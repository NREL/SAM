#include <stdio.h>

#include "API_structures.h"
#include "exceptions.hpp"
#include "definitions.h"

using namespace std;

// ----------------- optimize --------------------
void sp_optimize::getOptimizationSimulationHistory(vector<vector<double> > &sim_points, vector<double> &obj_values, vector<double> &flux_values)
{
	/* 
	Return the addresses of the optimization simulation history data, if applicable.
	*/
	sim_points = _optimization_sim_points;
	obj_values = _optimization_objectives;
	flux_values = _optimization_fluxes;
}

void sp_optimize::setOptimizationSimulationHistory(vector<vector<double> > &sim_points, vector<double> &obj_values, vector<double> &flux_values)
{
	//Create local copies
	_optimization_sim_points = sim_points;
	_optimization_objectives = obj_values;
	_optimization_fluxes = flux_values;
}

// ----------------- flux table --------------------
sp_flux_table::sp_flux_table()
{
	is_user_spacing = false;
}

// ----------------- optical table --------------------
sp_optical_table::sp_optical_table()
{
	is_user_positions = false;
}
