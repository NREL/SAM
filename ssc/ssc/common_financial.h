#ifndef __common_financial_h
#define __common_financial_h

#include <vector>
#include "core.h"


void save_cf(compute_module *cm, util::matrix_t<double>& mat, int cf_line, int nyears, const std::string &name);



class dispatch_calculations
{
private:
	compute_module *m_cm;
	std::vector<int> m_periods;
	std::string m_error;
	util::matrix_t<double> m_cf;
	std::vector<double> m_degradation;
	std::vector<double> m_hourly_energy;
	int m_nyears;
	bool m_timestep;
	ssc_number_t *m_gen;
	ssc_number_t *m_multipliers;
	size_t m_ngen;
	size_t m_nmultipliers;

public:
	dispatch_calculations() {};
	dispatch_calculations(compute_module *cm, std::vector<double>& degradation, std::vector<double>& hourly_energy);
	bool init(compute_module *cm, std::vector<double>& degradation, std::vector<double>& hourly_energy);
	bool setup();
	bool setup_ts();
	bool compute_outputs(std::vector<double>& ppa);
	bool compute_outputs_ts(std::vector<double>& ppa);
	int operator()(size_t time);
	std::string error() { return m_error; }
	bool process_dispatch_output();
	bool compute_dispatch_output();
	bool process_lifetime_dispatch_output();
	bool compute_lifetime_dispatch_output();
	bool compute_dispatch_output_ts();
	bool compute_lifetime_dispatch_output_ts();
	util::matrix_t<double>& dispatch_output();
	double tod_energy(int period, int year);
	double tod_energy_value(int period, int year);
	double tod_energy_value(int year);
};


class hourly_energy_calculation
{
private:
	compute_module *m_cm;
	std::vector<double> m_hourly_energy;
	std::string m_error;
	int m_nyears;

public:
	bool calculate(compute_module *cm);
	std::vector<double>& hourly_energy() {
		return m_hourly_energy;
	}
	std::string error() { return m_error; }
};




/*
extern var_info vtab_advanced_financing_cost[];


class advanced_financing_cost
{
private:
	compute_module *m_cm;

public:
	advanced_financing_cost(compute_module *cm);
	bool compute_cost(double cost_installed, double equity, double debt, double cbi, double ibi);
};
*/


#endif

