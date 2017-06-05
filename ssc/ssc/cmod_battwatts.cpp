#include <math.h>

#include "common.h"
#include "core.h"
#include "lib_util.h"
#include "lib_battery.h"
#include "cmod_battery.h"




var_info vtab_battwatts[] = {
	/*   VARTYPE           DATATYPE         NAME                               LABEL                                    UNITS      META                   GROUP                  REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	{ SSC_INPUT,        SSC_NUMBER,      "batt_simple_enable",                "Enable Battery",                         "0/1",     "",                 "battwatts",                  "?=0",                        "BOOLEAN",                       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "batt_simple_kwh",                   "Battery Capacity",                       "kWh",     "",                 "battwatts",                  "?=0",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "batt_simple_kw",                    "Battery Power",                          "kW",      "",                 "battwatts",                  "?=0",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "batt_simple_chemistry",             "Battery Chemistry",                      "0/1/2",   "",                 "battwatts",                  "?=0",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "batt_simple_dispatch",              "Battery Dispatch",                       "0/1",     "",                 "battwatts",                  "?=0",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "batt_simple_meter_position",        "Battery Meter Position",                 "0/1",     "",                 "battwatts",                  "?=0",                        "",                              "" },
	{ SSC_INPUT,        SSC_ARRAY,       "dc",								  "DC array power",                         "W",       "",                 "",                           "",                           "",                              "" },
	{ SSC_INPUT,        SSC_ARRAY,       "ac",								  "AC inverter power",                      "W",       "",                 "",                           "",                           "",                              "" },
	{ SSC_INPUT,		SSC_ARRAY,	     "load",			                  "Electricity load (year 1)",              "kW",	   "",		           "",                           "",	                         "",	                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inverter_model",                    "Inverter model specifier",                 "",      "0=cec,1=datasheet,2=partload,3=coefficientgenerator,4=generic", "",     "",                           "INTEGER,MIN=0,MAX=4",           "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inverter_efficiency",               "Inverter Efficiency",                     "%",      "",                  "",                          "",                           "",                               "" },

var_info_invalid  };

class cm_battwatts : public compute_module
{
public:

	cm_battwatts()
	{
		add_var_info(vtab_battwatts);
		add_var_info(vtab_battery_outputs);
		add_var_info(vtab_technology_outputs);
	}


	
	batt_variables * setup_variables()
	{
		batt_variables * batt_vars = new batt_variables();
		batt_vars->system_use_lifetime_output = false;
		batt_vars->analysis_period = 25;
		batt_vars->batt_chem = as_integer("batt_simple_chemistry");

		int dispatch = as_integer("batt_simple_dispatch");
		batt_vars->batt_dispatch = (dispatch == 0 ? dispatch_t::LOOK_AHEAD : dispatch_t::LOOK_BEHIND);
		batt_vars->batt_meter_position = as_integer("batt_simple_meter_position");

		// compute cells, strings, voltage based on desired capacity/power, assume max current is 10 A for a battery
		double batt_kwh = as_double("batt_simple_kwh");
		double batt_kw = as_double("batt_simple_kw");
		double batt_time_hour = batt_kwh / batt_kw;
		double batt_C_rate_discharge = 1. / batt_time_hour;

		double current_max = 15;
		double batt_specific_energy_per_mass = 0;
		double batt_specific_energy_per_volume = 0;

		// allocate vectors
		std::vector<double> * lifetime_matrix = new std::vector < double >;
		std::vector<double> * capacity_vs_temperature = new std::vector<double>;

		// lithium ion NMC
		if (batt_vars->batt_chem == battery_t::LITHIUM_ION)
		{
			lifetime_matrix->push_back(20); lifetime_matrix->push_back(0); lifetime_matrix->push_back(100);
			lifetime_matrix->push_back(20); lifetime_matrix->push_back(650); lifetime_matrix->push_back(96);
			lifetime_matrix->push_back(80); lifetime_matrix->push_back(1500); lifetime_matrix->push_back(87);
			lifetime_matrix->push_back(80); lifetime_matrix->push_back(0); lifetime_matrix->push_back(100);
			lifetime_matrix->push_back(80); lifetime_matrix->push_back(150); lifetime_matrix->push_back(96);
			lifetime_matrix->push_back(80); lifetime_matrix->push_back(300); lifetime_matrix->push_back(87);
			util::matrix_t<double> batt_lifetime_matrix(6, 3, lifetime_matrix);
			batt_vars->batt_lifetime_matrix = batt_lifetime_matrix;

			batt_vars->batt_Vnom_default = 3.6;
			batt_vars->batt_Vfull = 4.1;
			batt_vars->batt_Vexp = 4.05;
			batt_vars->batt_Vnom = 3.4;
			batt_vars->batt_Qfull = 2.25;
			batt_vars->batt_Qexp = 1.78;
			batt_vars->batt_Qnom = 88.9;
			batt_vars->batt_C_rate = 0.2;
			batt_vars->batt_resistance = 0.1;

			capacity_vs_temperature->push_back(-15); capacity_vs_temperature->push_back(65);
			capacity_vs_temperature->push_back(0);  capacity_vs_temperature->push_back(85);
			capacity_vs_temperature->push_back(25); capacity_vs_temperature->push_back(100);
			capacity_vs_temperature->push_back(40); capacity_vs_temperature->push_back(104);
			util::matrix_t<double> batt_capacity_vs_temperature(4, 2, capacity_vs_temperature);
			batt_vars->cap_vs_temp = batt_capacity_vs_temperature;

			batt_vars->batt_Cp = 1004;
			batt_vars->batt_h_to_ambient = 500;
			batt_vars->T_room = 20;
			
			batt_specific_energy_per_mass = 197.33;  // Wh/kg
			batt_specific_energy_per_volume = 501.25; // Wh/L
		}
		// Lead acid AGM defaults
		else if (batt_vars->batt_chem == battery_t::LEAD_ACID)
		{
			lifetime_matrix->push_back(30); lifetime_matrix->push_back(0); lifetime_matrix->push_back(100);
			lifetime_matrix->push_back(30); lifetime_matrix->push_back(1100); lifetime_matrix->push_back(90);
			lifetime_matrix->push_back(30); lifetime_matrix->push_back(1200); lifetime_matrix->push_back(50);
			lifetime_matrix->push_back(50); lifetime_matrix->push_back(0); lifetime_matrix->push_back(100);
			lifetime_matrix->push_back(50); lifetime_matrix->push_back(400); lifetime_matrix->push_back(90);
			lifetime_matrix->push_back(50); lifetime_matrix->push_back(500); lifetime_matrix->push_back(50);
			lifetime_matrix->push_back(100); lifetime_matrix->push_back(0); lifetime_matrix->push_back(100);
			lifetime_matrix->push_back(100); lifetime_matrix->push_back(100); lifetime_matrix->push_back(90);
			lifetime_matrix->push_back(100); lifetime_matrix->push_back(150); lifetime_matrix->push_back(50);
			util::matrix_t<double> batt_lifetime_matrix(9, 3, lifetime_matrix);
			batt_vars->batt_lifetime_matrix = batt_lifetime_matrix;

			batt_vars->batt_Vnom_default = 2;
			batt_vars->batt_Vfull = 2.2;
			batt_vars->batt_Vexp = 2.06;
			batt_vars->batt_Vnom = 2.03;
			batt_vars->batt_Qfull = 20;
			batt_vars->batt_Qexp = 0.25;
			batt_vars->batt_Qnom = 90;
			batt_vars->batt_C_rate = 0.05;
			batt_vars->batt_resistance = 0.1;

			capacity_vs_temperature->push_back(-15); capacity_vs_temperature->push_back(65);
			capacity_vs_temperature->push_back(0);  capacity_vs_temperature->push_back(85);
			capacity_vs_temperature->push_back(25); capacity_vs_temperature->push_back(100);
			capacity_vs_temperature->push_back(40); capacity_vs_temperature->push_back(104);
			util::matrix_t<double> batt_capacity_vs_temperature(4, 2, capacity_vs_temperature);
			batt_vars->cap_vs_temp = batt_capacity_vs_temperature;

			batt_vars->batt_Cp = 600;
			batt_vars->batt_h_to_ambient = 500;
			batt_vars->T_room = 20;

			batt_specific_energy_per_mass = 30;  // Wh/kg
			batt_specific_energy_per_volume = 30; // Wh/L
		}
		batt_vars->batt_ac_dc_efficiency = 92; 
		batt_vars->batt_dc_ac_efficiency = 92;
		batt_vars->batt_dc_dc_bms_efficiency = 92;
		batt_vars->pv_dc_dc_mppt_efficiency = 99;

		double batt_bank_voltage = batt_kw * 1000. / current_max;
		batt_vars->batt_computed_series = std::ceil(batt_bank_voltage / batt_vars->batt_Vnom_default);
		batt_vars->batt_computed_strings = std::ceil((batt_kwh * 1000.) / (batt_vars->batt_Qfull * batt_vars->batt_computed_series * batt_vars->batt_Vnom_default)) - 1;

		if (batt_vars->batt_chem == battery_t::LEAD_ACID)
		{
			double LeadAcid_q20 = 100;
			double LeadAcid_q10 = 93.2;
			double LeadAcid_qn = 58.12;
			double LeadAcid_tn = 1;

			batt_vars->LeadAcid_q10_computed = batt_vars->batt_computed_strings * LeadAcid_q10 * batt_vars->batt_Qfull / 100;
			batt_vars->LeadAcid_q20_computed = batt_vars->batt_computed_strings * LeadAcid_q20 * batt_vars->batt_Qfull / 100;
			batt_vars->LeadAcid_qn_computed = batt_vars->batt_computed_strings * LeadAcid_qn * batt_vars->batt_Qfull / 100;
			batt_vars->LeadAcid_tn = LeadAcid_tn;
		}

		batt_vars->batt_kwh = batt_kwh;
		batt_vars->batt_kw = batt_kw;

		batt_vars->batt_mass = batt_kwh * 1000 / batt_specific_energy_per_mass;
		double batt_volume = batt_kwh / batt_specific_energy_per_volume;
		batt_vars->batt_length = std::pow(batt_volume, 1. / 3.);
		batt_vars->batt_width = std::pow(batt_volume, 1. / 3.);
		batt_vars->batt_height = std::pow(batt_volume, 1. / 3.);
		
		// control constraints
		batt_vars->batt_pv_choice = dispatch_t::MEET_LOAD;
		batt_vars->batt_maximum_SOC = 100.;
		batt_vars->batt_minimum_SOC = 20.;
		batt_vars->batt_current_charge_max = 1000 * batt_C_rate_discharge * batt_kwh / batt_bank_voltage;
		batt_vars->batt_current_discharge_max = 1000 * batt_C_rate_discharge * batt_kwh / batt_bank_voltage;
		batt_vars->batt_minimum_modetime = 10;

		batt_vars->batt_topology = charge_controller::AC_CONNECTED;
		batt_vars->inverter_model = as_integer("inverter_model");
		if (batt_vars->inverter_model > 3)
			batt_vars->inverter_efficiency = as_double("inverter_efficiency");

		// losses
		double_vec batt_losses, batt_losses_monthly;

		size_t n_recs = 0;
		ssc_number_t * p_ac = as_array("ac", &n_recs);
		for (int i = 0; i != n_recs; i++)
			batt_losses.push_back(0.);
		for (int m = 0; m != 12; m++)
			batt_losses_monthly.push_back(0.);

		batt_vars->batt_loss_choice = losses_t::TIMESERIES;
		batt_vars->batt_losses = batt_losses;
		batt_vars->batt_losses_monthly = batt_losses_monthly;

		// replacement
		batt_vars->batt_replacement_capacity = 0.;

		// clean up
		delete lifetime_matrix;
		delete capacity_vs_temperature;

		return batt_vars;
	}
	
	void clean_up(batt_variables * batt_vars)
	{
		if (batt_vars) 
			delete batt_vars;
	}

	void exec() throw(general_error)
	{
		if (as_boolean("batt_simple_enable"))
		{

			/* *********************************************************************************************
			Setup problem
			*********************************************************************************************** */
			ssc_number_t * p_ac;
			ssc_number_t * p_load;
			size_t n_ac, n_load;
			int step_per_hour;
			double ts_hour;

			p_ac = as_array("ac", &n_ac);
			for (int i = 0; i != n_ac; i++)
				p_ac[i] = p_ac[i] * 0.001;

			p_load = as_array("load", &n_load);
			step_per_hour = n_ac / 8760;
			ts_hour = 1. / step_per_hour;

			batt_variables * batt_vars = setup_variables();
			
			battstor batt(*this, true, 0, n_ac, ts_hour, batt_vars);
			batt.initialize_automated_dispatch(p_ac, p_load);
			
			/* *********************************************************************************************
			Run Simulation
			*********************************************************************************************** */
			ssc_number_t *p_gen = allocate("gen", n_ac);
			size_t hour = 0;
			int count = 0;

			for (hour = 0; hour < 8760; hour++)
			{
				for (size_t jj = 0; jj < step_per_hour; jj++)
				{
					batt.advance(*this, 0, hour, jj, p_ac[count], p_load[count]);
					p_gen[count] = batt.outGenPower[count];
					count++;
				}
			}
			batt.calculate_monthly_and_annual_outputs(*this);

			clean_up(batt_vars);
		}
		else
			assign("average_cycle_efficiency", var_data((ssc_number_t)0.));
	}
};

DEFINE_MODULE_ENTRY(battwatts, "simple battery model", 1)
