#ifndef __SCO2_PC_CORE_
#define __SCO2_PC_CORE_

#include <limits>
#include <vector>
#include <algorithm>
#include <string>
#include <math.h>
#include "CO2_properties.h"

#include "heat_exchangers.h"

#include "numeric_solvers.h"

using namespace std;

// 'General' Core Routines: Not class methods and don't require pointers to or instances of classes
void calculate_turbomachinery_outlet_1(double T_in /*K*/, double P_in /*kPa*/, double P_out /*kPa*/, double eta /*-*/, bool is_comp, int & error_code, double & enth_in /*kJ/kg*/, double & entr_in /*kJ/kg-K*/,
	double & dens_in /*kg/m3*/, double & temp_out /*K*/, double & enth_out /*kJ/kg*/, double & entr_out /*kJ/kg-K*/, double & dens_out /*kg/m3*/, double & spec_work /*kJ/kg*/);

//void calculate_hxr_UA_1(int N_hxrs, double Q_dot /*units?*/, double m_dot_c, double m_dot_h, double T_c_in, double T_h_in, double P_c_in, double P_c_out, double P_h_in, double P_h_out,
//	int & error_code, double & UA, double & min_DT);

void isen_eta_from_poly_eta(double T_in /*K*/, double P_in /*kPa*/, double P_out /*kPa*/, double poly_eta /*-*/, bool is_comp, int & error_code, double & isen_eta);

// Heat Exchanger Class
class C_HeatExchanger
{
public:
	struct S_design_parameters
	{
		int m_N_sub;							//[-] Number of sub-heat exchangers used in the model
		std::vector<double> m_m_dot_design;		//[kg/s] Design-point mass flow rates of the two streams
		std::vector<double> m_DP_design;		//[kPa] Design-point pressure drops across the heat exchanger
		double m_UA_design;						//[kW/K] Design-point conductance
		double m_Q_dot_design;					//[kW] Design-point heat transfer
		double m_min_DT_design;					//[K] Minimum temperature difference in heat exchanger
		double m_eff_design;					//[-] Effectiveness at design

		S_design_parameters()
		{
			m_N_sub = -1;

			m_m_dot_design.resize(2);
			std::fill(m_m_dot_design.begin(), m_m_dot_design.end(), std::numeric_limits<double>::quiet_NaN());
			m_DP_design.resize(2);
			std::fill(m_DP_design.begin(), m_DP_design.end(), std::numeric_limits<double>::quiet_NaN());

			m_UA_design = m_Q_dot_design = m_min_DT_design = m_eff_design = std::numeric_limits<double>::quiet_NaN();
		}
	};

private:
	S_design_parameters ms_des_par;

public:
	~C_HeatExchanger(){};

	C_HeatExchanger(){};

	void initialize(const S_design_parameters & des_par_in);

	// Performance Methods:
		// *Some check to ensure member structures are initialized?*
	void hxr_pressure_drops(const std::vector<double> & m_dots, std::vector<double> & hxr_deltaP);

	void hxr_conductance(const std::vector<double> & m_dots, double & hxr_UA);

};

class C_turbine
{
public:
	struct S_design_parameters
	{
		double m_N_design;					//[rpm] turbine shaft speed
		double m_N_comp_design_if_linked;	//[rpm] compressor shaft speed
			// Turbine inlet state
		double m_P_in;						//[kPa]
		double m_T_in;						//[K] 
		double m_D_in;						//[kg/m^3] 
		double m_h_in;						//[kJ/kg]
		double m_s_in;						//[kJ/kg-K]
			// Turbine outlet state
		double m_P_out;						//[kPa]
		double m_h_out;						//[kJ/kg]
			// Mass flow rate
		double m_m_dot;						//[kg/s]
		
		S_design_parameters()
		{
			m_N_design = m_N_comp_design_if_linked =
			m_P_in = m_T_in = m_D_in = m_h_in = m_s_in = m_P_out = m_h_out =
			m_m_dot = std::numeric_limits<double>::quiet_NaN();
		}
	};

	struct S_design_solved
	{
		double m_nu_design;					//[-] ratio of tip speed to spouting velocity
		double m_D_rotor;					//[m] Turbine diameter
		double m_A_nozzle;					//[m^2] Effective nozzle area
		double m_w_tip_ratio;				//[-] ratio of tip speed to local speed of sound
		double m_eta;						//[-] 
		double m_N_design;					//[rpm] shaft speed

		S_design_solved()
		{
			m_nu_design = m_D_rotor = m_A_nozzle = m_w_tip_ratio = m_eta = m_N_design = std::numeric_limits<double>::quiet_NaN();
		}
	};

	struct S_od_solved
	{
		double m_nu;						//[-] ratio of tip speed to spouting velocity
		double m_eta;						//[-] turbine efficiency
		double m_w_tip_ratio;				//[-] ratio of the tip speed to the local (turbine inlet) speed of sound
		double m_N;							//[rpm] off-design turbine shaft speed

		double m_W_dot_out;			//[kW] Turbine power output, expected to be positive

		S_od_solved()
		{
			m_nu = m_eta = m_w_tip_ratio = m_N =
				m_W_dot_out = std::numeric_limits<double>::quiet_NaN();
		}
	};

private:
	S_design_parameters ms_des_par;
	S_design_solved ms_des_solved;
	S_od_solved ms_od_solved;

public:
	~C_turbine(){};

	C_turbine(){};

	static const double m_nu_design;
	
	const S_design_solved * get_design_solved()
	{
		return &ms_des_solved;
	}

	const S_od_solved * get_od_solved()
	{
		return &ms_od_solved;
	}

	void turbine_sizing(const S_design_parameters & des_par_in, int & error_code);	

	void off_design_turbine(double T_in, double P_in, double P_out, double N, int & error_code, double & m_dot, double & T_out);

	void od_turbine_at_N_des(double T_in, double P_in, double P_out, int & error_code, double & m_dot, double & T_out);
};

class C_compressor
{
public:
	struct S_design_parameters
	{
			// Compressor inlet conditions
		double m_T_in;			//[K]
		double m_P_in;			//[kPa]
		double m_D_in;			//[kg/m^3]
		double m_h_in;			//[kJ/kg]
		double m_s_in;			//[kJ/kg-K]
			// Compressor outlet conditions
		double m_T_out;			//[K]
		double m_P_out;			//[kPa]
		double m_h_out;			//[kJ/kg]
		double m_D_out;			//[kg/m^3]
			// Mass flow
		double m_m_dot;			//[kg/s]

		S_design_parameters()
		{
			m_T_in = m_P_in = m_D_in = m_h_in = m_s_in =
			m_T_out = m_P_out = m_h_out = m_D_out = std::numeric_limits<double>::quiet_NaN();
		}
	};
	struct S_design_solved
	{
		double m_D_rotor;		//[m]
		double m_N_design;		//[rpm]
		double m_w_tip_ratio;	//[-]
		double m_eta_design;	//[-]

		double m_phi_des;		//[-]
		double m_phi_surge;		//[-]
		double m_phi_max;		//[-]
		int m_n_stages;			//[-]

		S_design_solved()
		{
			m_D_rotor = m_N_design = m_w_tip_ratio = m_eta_design = 
				m_phi_surge = m_phi_des = m_phi_max = std::numeric_limits<double>::quiet_NaN();

			m_n_stages = -1;
		}
	};
	struct S_od_solved
	{
		bool m_surge;			//[-]
		double m_eta;			//[-]
		double m_phi;			//[-]
		double m_w_tip_ratio;	//[-]

		double m_N;			//[rpm]

		double m_W_dot_in;		//[KWe] Power required by compressor, positive value expected
		double m_P_in;			//[kPa] Inlet pressure
		double m_P_out;			//[kPa] Outlet pressure
		double m_surge_safety;	//[-] Flow coefficient / min flow coefficient

		S_od_solved()
		{
			m_surge = false;
			m_eta = m_phi = m_w_tip_ratio = m_N =
				m_W_dot_in = m_P_in = m_P_out = m_surge_safety = std::numeric_limits<double>::quiet_NaN();
		}
	};

private:
	S_design_parameters ms_des_par;
	S_design_solved ms_des_solved;
	S_od_solved ms_od_solved;

public:
	~C_compressor(){};

	C_compressor(){};

	static const double m_snl_phi_design;		//[-] Design-point flow coef. for Sandia compressor (corresponds to max eta)
	static const double m_snl_phi_min;				//[-] Approximate surge limit for SNL compressor
	static const double m_snl_phi_max;				//[-] Approximate x-intercept for SNL compressor

	const S_design_solved * get_design_solved()
	{
		return &ms_des_solved;
	}

	const S_od_solved * get_od_solved()
	{
		return &ms_od_solved;
	}

	void compressor_sizing(const S_design_parameters & des_par_in, int & error_code);
	
	void off_design_compressor(double T_in, double P_in, double m_dot, double N, int & error_code, double & T_out, double & P_out);
	
	void od_comp_at_N_des(double T_in, double P_in, double m_dot, int & error_code, double & T_out, double & P_out);

	void od_comp_phi_opt(double T_in /*K*/, double P_in /*kPa*/, double m_dot /*kg/s*/, 
							int &error_code, double &T_out /*K*/, double &P_out /*kPa*/);

	void od_comp_phi(double phi_in /*-*/, double T_in /*K*/, double P_in /*kPa*/, double m_dot /*kg/s*/,
		int &error_code, double &T_out /*K*/, double &P_out /*kPa*/);
};

class C_recompressor
{
public:
	struct S_design_parameters
	{
			// Compressor inlet conditions
		double m_P_in;
		double m_D_in;
		double m_h_in;
		double m_s_in;
			// Compressor outlet conditions
		double m_T_out;
		double m_P_out;
		double m_h_out;
		double m_D_out;
			// Flow conditions
		double m_m_dot;

		S_design_parameters()
		{
			m_P_in = m_D_in = m_h_in = m_s_in = 
			m_T_out = m_P_out = m_h_out = m_D_out =
			m_m_dot = std::numeric_limits<double>::quiet_NaN();
		}
	};
	struct S_design_solved
	{
		double m_D_rotor;
		double m_D_rotor_2;
		double m_N_design;
		double m_eta_design;
		
		double m_w_tip_ratio_1;	//[-] Max tip ratio at design, first stage
		double m_w_tip_ratio_2;	//[-] Max tip ratio at design, 2nd stage

		double m_phi_des;		//[-]
		double m_phi_surge;		//[-]
		double m_phi_max;		//[-]
		int m_n_stages;			//[-]

		S_design_solved()
		{
			m_D_rotor = m_D_rotor_2 = m_N_design = m_eta_design =
				m_w_tip_ratio_1 = m_w_tip_ratio_2 = m_phi_surge = m_phi_des = m_phi_max = std::numeric_limits<double>::quiet_NaN();

			m_n_stages = -1;
		}
	};

	struct S_od_inputs
	{
		double m_m_dot;		//[kg/s]
		double m_rho_in;		//[kg/m^3]
		double m_h_in;		//[kJ/kg]
		double m_s_in;		//[kJ/kg-K]

		S_od_inputs()
		{
			m_m_dot = m_rho_in = m_h_in = m_s_in = std::numeric_limits<double>::quiet_NaN();
		}
	};

	struct S_od_solved
	{
		double m_N;
		double m_eta;
		double m_phi;
		double m_phi_2;
		double m_w_tip_ratio;	//[-] Maximum of multiple stages
		bool m_surge;

		double m_T_out;			//[K] Compressor outlet temperature
		double m_W_dot_in;		//[kWe] Power required to operate compressor. Expect positive value.
		double m_surge_safety;	//[-] min of stages: Flow coefficient / min flow coefficient

		S_od_solved()
		{
			m_N = m_eta = m_phi = m_phi_2 = m_w_tip_ratio = 
				m_T_out = m_W_dot_in = m_surge_safety = std::numeric_limits<double>::quiet_NaN();
			m_surge = false;
		}
	};

private:
	S_design_parameters ms_des_par;
	S_design_solved ms_des_solved;
	S_od_solved ms_od_solved;
	S_od_inputs ms_od_inputs;

public:
	~C_recompressor(){};

	C_recompressor(){};

	static const double m_snl_phi_design;		//[-] Design-point flow coef. for Sandia compressor (corresponds to max eta)
	static const double m_snl_phi_min;				//[-] Approximate surge limit for SNL compressor
	static const double m_snl_phi_max;				//[-] Approximate x-intercept for SNL compressor

	class C_mono_eq_phi_off_design : public C_monotonic_equation
	{
	private:
		C_recompressor *mpc_recompressor;

	public:
		C_mono_eq_phi_off_design(C_recompressor *pc_recompressor)
		{
			mpc_recompressor = pc_recompressor;
		}

		virtual int operator()(double phi /*-*/, double *P_high /*kPa*/);
	};

	const S_design_solved * get_design_solved()
	{
		return &ms_des_solved;
	}

	const S_od_solved * get_od_solved()
	{
		return &ms_od_solved;
	}

	void recompressor_sizing(const S_design_parameters & des_par_in, int & error_code);	

	void off_design_recompressor(double T_in, double P_in, double m_dot, double P_out, int & error_code, double & T_out);
	
};

class C_RecompCycle
{
public:
	
	enum E_cycle_state_points
	{
		// index values for c++ 0-based vectors for temperature, pressure, etc.
		MC_IN = 0,
		MC_OUT,
		LTR_HP_OUT,
		MIXER_OUT,
		HTR_HP_OUT,
		TURB_IN,
		TURB_OUT,
		HTR_LP_OUT,
		LTR_LP_OUT,
		RC_OUT		
	};

	struct S_design_limits
	{
		double m_UA_net_power_ratio_max;		//[-/K]
		double m_UA_net_power_ratio_min;		//[-/K]

		double m_T_mc_in_min;					//[K]

		S_design_limits()
		{
			m_UA_net_power_ratio_max = m_UA_net_power_ratio_min = std::numeric_limits<double>::quiet_NaN();
		}
	};

	struct S_design_parameters
	{
		double m_W_dot_net;					//[kW] Target net cycle power
		double m_T_mc_in;					//[K] Compressor inlet temperature
		double m_T_t_in;					//[K] Turbine inlet temperature
		double m_P_mc_in;					//[kPa] Compressor inlet pressure
		double m_P_mc_out;					//[kPa] Compressor outlet pressure
		std::vector<double> m_DP_LT;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		std::vector<double> m_DP_HT;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		std::vector<double> m_DP_PC;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		std::vector<double> m_DP_PHX;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		double m_UA_LT;						//[kW/K] UA in LTR
		double m_UA_HT;						//[kW/K] UA in HTR
		double m_LT_eff_max;				//[-] Maximum allowable effectiveness in LT recuperator
		double m_HT_eff_max;				//[-] Maximum allowable effectiveness in HT recuperator
		double m_recomp_frac;				//[-] Fraction of flow that bypasses the precooler and the main compressor at the design point
		double m_eta_mc;					//[-] design-point efficiency of the main compressor; isentropic if positive, polytropic if negative
		double m_eta_rc;					//[-] design-point efficiency of the recompressor; isentropic if positive, polytropic if negative
		double m_eta_t;						//[-] design-point efficiency of the turbine; isentropic if positive, polytropic if negative
		int m_N_sub_hxrs;					//[-] Number of sub-heat exchangers to use when calculating UA value for a heat exchanger
		double m_P_high_limit;				//[kPa] maximum allowable pressure in cycle
		double m_tol;						//[-] Convergence tolerance
		double m_N_turbine;					//[rpm] Turbine shaft speed (negative values link turbine to compressor)

		S_design_parameters()
		{
			m_W_dot_net = m_T_mc_in = m_T_t_in = m_P_mc_in = m_P_mc_out = m_UA_LT = m_UA_HT = m_LT_eff_max = m_HT_eff_max = m_recomp_frac = 
				m_eta_mc = m_eta_rc = m_eta_t = m_P_high_limit = m_tol = m_N_turbine = std::numeric_limits<double>::quiet_NaN();
			m_N_sub_hxrs = -1;

			m_DP_LT.resize(2);
			std::fill(m_DP_LT.begin(), m_DP_LT.end(), std::numeric_limits<double>::quiet_NaN());
			m_DP_HT.resize(2);
			std::fill(m_DP_HT.begin(), m_DP_HT.end(), std::numeric_limits<double>::quiet_NaN());
			m_DP_PC.resize(2);
			std::fill(m_DP_PC.begin(), m_DP_PC.end(), std::numeric_limits<double>::quiet_NaN());
			m_DP_PHX.resize(2);
			std::fill(m_DP_PHX.begin(), m_DP_PHX.end(), std::numeric_limits<double>::quiet_NaN());
		}
	};

	struct S_opt_design_parameters
	{
		double m_W_dot_net;					//[kW] Target net cycle power
		double m_T_mc_in;					//[K] Compressor inlet temperature
		double m_T_t_in;					//[K] Turbine inlet temperature
		std::vector<double> m_DP_LT;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		std::vector<double> m_DP_HT;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		std::vector<double> m_DP_PC;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		std::vector<double> m_DP_PHX;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		double m_UA_rec_total;				//[kW/K] Total design-point recuperator UA
		double m_LT_eff_max;				//[-] Maximum allowable effectiveness in LT recuperator
		double m_HT_eff_max;				//[-] Maximum allowable effectiveness in HT recuperator
		double m_eta_mc;					//[-] design-point efficiency of the main compressor; isentropic if positive, polytropic if negative
		double m_eta_rc;					//[-] design-point efficiency of the recompressor; isentropic if positive, polytropic if negative
		double m_eta_t;						//[-] design-point efficiency of the turbine; isentropic if positive, polytropic if negative
		int m_N_sub_hxrs;					//[-] Number of sub-heat exchangers to use when calculating UA value for a heat exchanger
		double m_P_high_limit;				//[kPa] maximum allowable pressure in cycle
		double m_tol;						//[-] Convergence tolerance
		double m_opt_tol;					//[-] Optimization tolerance
		double m_N_turbine;					//[rpm] Turbine shaft speed (negative values link turbine to compressor)

		double m_P_mc_out_guess;			//[kPa] Initial guess for main compressor outlet pressure
		bool m_fixed_P_mc_out;				//[-] if true, P_mc_out is fixed at P_mc_out_guess
		
		double m_PR_mc_guess;				//[-] Initial guess for ratio of P_mc_out to P_mc_in
		bool m_fixed_PR_mc;					//[-] if true, ratio of P_mc_out to P_mc_in is fixed at PR_mc_guess

		double m_recomp_frac_guess;			//[-] Initial guess for design-point recompression fraction
		bool m_fixed_recomp_frac;			//[-] if true, recomp_frac is fixed at recomp_frac_guess

		double m_LT_frac_guess;				//[-] Initial guess for fraction of UA_rec_total that is in the low-temperature recuperator
		bool m_fixed_LT_frac;				//[-] if true, LT_frac is fixed at LT_frac_guess

		S_opt_design_parameters()
		{
			m_W_dot_net = m_T_mc_in = m_T_t_in = m_UA_rec_total = m_LT_eff_max = m_HT_eff_max = 
				m_eta_mc = m_eta_rc = m_eta_t = m_P_high_limit = m_tol = m_opt_tol = m_N_turbine =
				m_P_mc_out_guess = m_PR_mc_guess = m_recomp_frac_guess = m_LT_frac_guess = std::numeric_limits<double>::quiet_NaN();

			m_N_sub_hxrs = -1;

			m_DP_LT.resize(2);
			std::fill(m_DP_LT.begin(), m_DP_LT.end(), std::numeric_limits<double>::quiet_NaN());
			m_DP_HT.resize(2);
			std::fill(m_DP_HT.begin(), m_DP_HT.end(), std::numeric_limits<double>::quiet_NaN());
			m_DP_PC.resize(2);
			std::fill(m_DP_PC.begin(), m_DP_PC.end(), std::numeric_limits<double>::quiet_NaN());
			m_DP_PHX.resize(2);
			std::fill(m_DP_PHX.begin(), m_DP_PHX.end(), std::numeric_limits<double>::quiet_NaN());
		}
	};

	struct S_auto_opt_design_hit_eta_parameters
	{
		double m_W_dot_net;					//[kW] Target net cycle power
		double m_eta_thermal;				//[-] Cycle thermal efficiency
		double m_T_mc_in;					//[K] Compressor inlet temperature
		double m_T_t_in;					//[K] Turbine inlet temperature
		std::vector<double> m_DP_LT;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		std::vector<double> m_DP_HT;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		std::vector<double> m_DP_PC;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		std::vector<double> m_DP_PHX;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		double m_LT_eff_max;				//[-] Maximum allowable effectiveness in LT recuperator
		double m_HT_eff_max;				//[-] Maximum allowable effectiveness in HT recuperator
		double m_eta_mc;					//[-] design-point efficiency of the main compressor; isentropic if positive, polytropic if negative
		double m_eta_rc;					//[-] design-point efficiency of the recompressor; isentropic if positive, polytropic if negative
		double m_eta_t;						//[-] design-point efficiency of the turbine; isentropic if positive, polytropic if negative
		int m_N_sub_hxrs;					//[-] Number of sub-heat exchangers to use when calculating UA value for a heat exchanger
		double m_P_high_limit;				//[kPa] maximum allowable pressure in cycle
		double m_tol;						//[-] Convergence tolerance
		double m_opt_tol;					//[-] Optimization tolerance
		double m_N_turbine;					//[rpm] Turbine shaft speed (negative values link turbine to compressor)
		int m_is_recomp_ok;					//[-] 1 = yes, 0 = no, other = invalid

		double m_PR_mc_guess;				//[-] Initial guess for ratio of P_mc_out to P_mc_in
		bool m_fixed_PR_mc;					//[-] if true, ratio of P_mc_out to P_mc_in is fixed at PR_mc_guess

		S_auto_opt_design_hit_eta_parameters()
		{
			m_W_dot_net = m_T_mc_in = m_T_t_in = m_LT_eff_max = m_HT_eff_max = 
				m_eta_mc = m_eta_rc = m_eta_t = m_P_high_limit = 
				m_tol = m_opt_tol = m_N_turbine = 
				m_PR_mc_guess = std::numeric_limits<double>::quiet_NaN();

			m_N_sub_hxrs = -1;

			m_is_recomp_ok = -1;

			m_fixed_PR_mc = false;		//[-] If false, then should default to optimizing this parameter

			m_DP_LT.resize(2);
			std::fill(m_DP_LT.begin(), m_DP_LT.end(), std::numeric_limits<double>::quiet_NaN());
			m_DP_HT.resize(2);
			std::fill(m_DP_HT.begin(), m_DP_HT.end(), std::numeric_limits<double>::quiet_NaN());
			m_DP_PC.resize(2);
			std::fill(m_DP_PC.begin(), m_DP_PC.end(), std::numeric_limits<double>::quiet_NaN());
			m_DP_PHX.resize(2);
			std::fill(m_DP_PHX.begin(), m_DP_PHX.end(), std::numeric_limits<double>::quiet_NaN());
		}
	};
	
	struct S_auto_opt_design_parameters
	{
		double m_W_dot_net;					//[kW] Target net cycle power
		double m_T_mc_in;					//[K] Compressor inlet temperature
		double m_T_t_in;					//[K] Turbine inlet temperature
		std::vector<double> m_DP_LT;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		std::vector<double> m_DP_HT;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		std::vector<double> m_DP_PC;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		std::vector<double> m_DP_PHX;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		double m_UA_rec_total;				//[kW/K] Total design-point recuperator UA
		double m_LT_eff_max;				//[-] Maximum allowable effectiveness in LT recuperator
		double m_HT_eff_max;				//[-] Maximum allowable effectiveness in HT recuperator
		double m_eta_mc;					//[-] design-point efficiency of the main compressor; isentropic if positive, polytropic if negative
		double m_eta_rc;					//[-] design-point efficiency of the recompressor; isentropic if positive, polytropic if negative
		double m_eta_t;						//[-] design-point efficiency of the turbine; isentropic if positive, polytropic if negative
		int m_N_sub_hxrs;					//[-] Number of sub-heat exchangers to use when calculating UA value for a heat exchanger
		double m_P_high_limit;				//[kPa] maximum allowable pressure in cycle
		double m_tol;						//[-] Convergence tolerance
		double m_opt_tol;					//[-] Optimization tolerance
		double m_N_turbine;					//[rpm] Turbine shaft speed (negative values link turbine to compressor)
		int m_is_recomp_ok;					//[-] 1 = yes, 0 = no, other = invalid

		double m_PR_mc_guess;				//[-] Initial guess for ratio of P_mc_out to P_mc_in
		bool m_fixed_PR_mc;					//[-] if true, ratio of P_mc_out to P_mc_in is fixed at PR_mc_guess

		S_auto_opt_design_parameters()
		{
			m_W_dot_net = m_T_mc_in = m_T_t_in = m_UA_rec_total = m_LT_eff_max = m_HT_eff_max =
				m_eta_mc = m_eta_rc = m_eta_t = m_P_high_limit = 
				m_tol = m_opt_tol = m_N_turbine =
				m_PR_mc_guess = std::numeric_limits<double>::quiet_NaN();

			m_N_sub_hxrs = -1;

			m_is_recomp_ok = -1;

			m_fixed_PR_mc = false;		//[-] If false, then should default to optimizing this parameter

			m_DP_LT.resize(2);
			std::fill(m_DP_LT.begin(), m_DP_LT.end(), std::numeric_limits<double>::quiet_NaN());
			m_DP_HT.resize(2);
			std::fill(m_DP_HT.begin(), m_DP_HT.end(), std::numeric_limits<double>::quiet_NaN());
			m_DP_PC.resize(2);
			std::fill(m_DP_PC.begin(), m_DP_PC.end(), std::numeric_limits<double>::quiet_NaN());
			m_DP_PHX.resize(2);
			std::fill(m_DP_PHX.begin(), m_DP_PHX.end(), std::numeric_limits<double>::quiet_NaN());
		}
	};

	struct S_design_solved
	{
		std::vector<double> m_temp, m_pres, m_enth, m_entr, m_dens;		// thermodynamic states (K, kPa, kJ/kg, kJ/kg-K, kg/m3)
		double m_eta_thermal;	//[-]
		double m_W_dot_net;		//[kW]
		double m_m_dot_mc;
		double m_m_dot_rc;
		double m_m_dot_t;
		double m_recomp_frac;
		double m_UA_LT;
		double m_UA_HT;

		bool m_is_rc;

		C_compressor::S_design_solved ms_mc_des_solved;
		C_recompressor::S_design_solved ms_rc_des_solved;
		C_turbine::S_design_solved ms_t_des_solved;
		C_HX_counterflow::S_des_solved ms_LT_recup_des_solved;
		C_HX_counterflow::S_des_solved ms_HT_recup_des_solved;

		S_design_solved()
		{
			m_eta_thermal = m_W_dot_net = m_m_dot_mc = m_m_dot_rc = m_m_dot_t = m_recomp_frac = 
				m_UA_LT = m_UA_HT = std::numeric_limits<double>::quiet_NaN();

			m_is_rc = true;
		}
	};

	struct S_od_turbo_bal_csp_par
	{
		double m_P_mc_in;	//[kPa] Main compressor inlet pressure
		double m_f_recomp;	//[-] Recompression fraction
		double m_T_mc_in;	//[K] Main compressor inlet temperature
		double m_T_t_in;	//[K] Turbine inlet temperature
		double m_phi_mc;	//[-] Main compressor flow coefficient

		double m_co2_to_htf_m_dot_ratio_des;	//[-] m_dot_co2 / m_dot_htf at design
		double m_m_dot_htf;						//[kg/s] (off design) HTF mass flow rate 

		S_od_turbo_bal_csp_par()
		{
			m_P_mc_in = m_f_recomp = m_T_mc_in = m_T_t_in = m_phi_mc =
				m_co2_to_htf_m_dot_ratio_des = m_m_dot_htf = std::numeric_limits<double>::quiet_NaN();
		}
	};

	struct S_od_turbo_bal_csp_solved
	{
		S_od_turbo_bal_csp_par ms_par;

		bool m_is_feasible;			//[-] Did a set of parameters result in a solution that satisfied constraints

		double m_W_dot_net;			//[kWe] W_t - W_mc - W_rc
		double m_W_dot_net_adj;		//[kWe] Adjusted with derates for constraints
		double m_P_high;			//[kPa] Upper pressure in cycle (not considering pressure drops)
		double m_m_dot_total;		//[kg/s] CO2 mass flow rate through turbine & PHX

		double m_N_mc;				//[rpm] Main compressor speed required to hit phi_des
		double m_w_tip_ratio_mc;	//[-] Main compressor tip speed over speed of sound
		double m_eta_mc;			//[-] Main compressor isentropic efficiency
	
		double m_N_rc;				//[rpm] Recompressor speed required to supply m_dot_rc
		double m_phi_rc_1;			//[-] Recompressor flow coefficient, stage 1
		double m_phi_rc_2;			//[-] Recompressor flow coefficient, stage 2
		double m_w_tip_ratio_rc;	//[-] Recompressor tip seed over speed of sound (max of 2 stages)
		double m_eta_rc;			//[-] Recompressor isentropic efficiency

		double m_eta_t;				//[-] Turbine isentropic efficiency

		S_od_turbo_bal_csp_solved()
		{
			m_is_feasible = false;
			
			m_W_dot_net = m_W_dot_net_adj = m_P_high = m_m_dot_total =
				m_N_mc = m_w_tip_ratio_mc = m_eta_mc = 
				m_N_rc = m_phi_rc_1, m_phi_rc_2 = m_w_tip_ratio_rc = m_eta_rc =
				m_eta_t = std::numeric_limits<double>::quiet_NaN();
		}
	};

	struct S_od_solved
	{
		std::vector<double> m_temp, m_pres, m_enth, m_entr, m_dens;		// thermodynamic states (K, kPa, kJ/kg, kJ/kg-K, kg/m3)
		double m_eta_thermal;
		double m_W_dot_net;
		double m_Q_dot;
		double m_m_dot_mc;
		double m_m_dot_rc;
		double m_m_dot_t;
		double m_recomp_frac;
		// double m_N_mc;
		// double m_N_t;

		C_compressor::S_od_solved ms_mc_od_solved;
		C_recompressor::S_od_solved ms_rc_od_solved;
		C_turbine::S_od_solved ms_t_od_solved;
		C_HX_counterflow::S_od_solved ms_LT_recup_od_solved;
		C_HX_counterflow::S_od_solved ms_HT_recup_od_solved;

		S_od_solved()
		{
			m_eta_thermal = m_W_dot_net = m_Q_dot = m_m_dot_mc = m_m_dot_rc = 
				m_m_dot_t = m_recomp_frac = /* m_N_mc = m_N_t =*/ std::numeric_limits<double>::quiet_NaN();
		}
	};

	struct S_od_parameters
	{
		double m_T_mc_in;		//[K] Compressor inlet temperature
		double m_T_t_in;		//[K] Turbine inlet temperature
		double m_P_mc_in;		//[kPa] Compressor inlet pressure
		double m_recomp_frac;	//[-] Fraction of flow that bypasses the precooler and main compressor
		double m_N_mc;			//[rpm] Main compressor shaft speed
		double m_N_t;			//[rpm] Turbine shaft speed
		int m_N_sub_hxrs;		//[-] Number of sub heat exchangers
		double m_tol;			//[-] Convergence tolerance

		S_od_parameters()
		{
			m_T_mc_in = m_T_t_in = m_P_mc_in = m_recomp_frac = m_N_mc = m_N_t = m_tol = std::numeric_limits<double>::quiet_NaN();
			m_N_sub_hxrs = -1;
		}
	};

	struct S_od_phi_par
	{
		double m_T_mc_in;		//[K] Compressor inlet temperature
		double m_T_t_in;		//[K] Turbine inlet temperature
		double m_P_mc_in;		//[kPa] Compressor inlet pressure
		double m_recomp_frac;	//[-] Fraction of flow that bypasses the precooler and main compressor
		double m_phi_mc;		//[-] Main compressor flow coefficient
		double m_N_t;			//[rpm] Turbine shaft speed
		int m_N_sub_hxrs;		//[-] Number of sub heat exchangers
		double m_tol;			//[-] Convergence tolerance

		S_od_phi_par()
		{
			m_T_mc_in = m_T_t_in = m_P_mc_in = m_recomp_frac = m_phi_mc = m_N_t = m_tol = std::numeric_limits<double>::quiet_NaN();
			m_N_sub_hxrs = -1;
		}
	};

	struct S_opt_od_parameters
	{
		double m_T_mc_in;		//[K] Compressor inlet temperature
		double m_T_t_in;		//[K] Turbine inlet temperature

		bool m_is_max_W_dot;	//[-] Value to maximize: true = W_dot, false = eta

		int m_N_sub_hxrs;		//[-] Number of sub heat exchangers

		double m_P_mc_in_guess;	//[kPa] Initial guess for P_mc_in when iterating to hit target
		bool m_fixed_P_mc_in;	//[-] if true, P_mc_in is fixed at P_mc_in_guess

		double m_recomp_frac_guess;		//[-] Initial guess for recompression fraction
		bool m_fixed_recomp_frac;		//[-] If true, recomp_frac is fixed at recomp_frac_guess

		double m_N_mc_guess;			//[rpm] Initial guess for main compressor shaft speed
		bool m_fixed_N_mc;				//[-]   If true, N_mc is fixed at N_mc_guess

		double m_N_t_guess;				//[rpm] Initial guess for turbine shaft speed (negative value links it to N_mc)
		bool m_fixed_N_t;				//[-]   If true, N_t is fixed at N_t_guess

		double m_tol;					//[-] Convergence tolerance
		double m_opt_tol;				//[-] Optimization convergence tolerance

		S_opt_od_parameters()
		{
			m_T_mc_in = m_T_t_in = m_P_mc_in_guess = m_recomp_frac_guess = m_N_mc_guess =
				m_N_t_guess = m_tol = m_opt_tol = std::numeric_limits<double>::quiet_NaN();

			m_N_sub_hxrs = -1;

			m_fixed_P_mc_in = m_fixed_recomp_frac = m_fixed_N_mc = m_fixed_N_t = false;
		}
	};

	struct S_target_od_parameters
	{
		double m_T_mc_in;		//[K] Compressor inlet temperature
		double m_T_t_in;		//[K] Turbine inlet temperature
		double m_recomp_frac;	//[-] Fraction of flow that bypasses the precooler and main compressor
		double m_N_mc;			//[rpm] Main compressor shaft speed
		double m_N_t;			//[rpm] Turbine shaft speed
		int m_N_sub_hxrs;		//[-] Number of sub heat exchangers
		double m_tol;			//[-] Convergence tolerance
		
		double m_target;		//[kW] type of target: 1) W_dot 2) Q_dot_PHX
		bool m_is_target_Q;		//[-] true = solve for Q_dot_PHX, false = solve for W_dot

		double m_lowest_pressure;	//[-] lowest pressure to check
		double m_highest_pressure;	//[-] highest pressure to check
		bool m_use_default_res;		//[-] If true, use 20 intervals in pressure range
									// If q_target is close to q_max, use false apply 100 intervals

		S_target_od_parameters()
		{
			m_T_mc_in = m_T_t_in = m_recomp_frac = m_N_mc = m_N_t = m_tol =
				m_target = m_lowest_pressure = m_highest_pressure = std::numeric_limits<double>::quiet_NaN();

			m_is_target_Q = true;

			m_N_sub_hxrs = -1;

			m_use_default_res = true;
		}
	};

	struct S_opt_target_od_parameters
	{
		double m_T_mc_in;		//[K] Compressor inlet temperature
		double m_T_t_in;		//[K] Turbine inlet temperature

		double m_target;		//[kW] target value
		bool m_is_target_Q;		//[-] true = solve for Q_dot_PHX, false = solve for W_dot

		int m_N_sub_hxrs;				//[-] Number of sub heat exchangers
		double m_lowest_pressure;		//[-] lowest pressure to check
		double m_highest_pressure;		//[-] highest pressure to check

		double m_recomp_frac_guess;		//[-] Initial guess for recompression fraction
		bool m_fixed_recomp_frac;		//[-] If true, recomp_frac is fixed at recomp_frac_guess

		double m_N_mc_guess;			//[rpm] Initial guess for main compressor shaft speed
		bool m_fixed_N_mc;				//[-]   If true, N_mc is fixed at N_mc_guess

		double m_N_t_guess;				//[rpm] Initial guess for turbine shaft speed (negative value links it to N_mc)
		bool m_fixed_N_t;				//[-]   If true, N_t is fixed at N_t_guess

		double m_tol;					//[-] Convergence tolerance
		double m_opt_tol;				//[-] Optimization convergence tolerance

		bool m_use_default_res;		//[-] If true, use 20 intervals in pressure range
									// If q_target is close to q_max, use false apply 100 intervals

		S_opt_target_od_parameters()
		{
			m_T_mc_in = m_T_t_in = m_target = m_lowest_pressure = m_highest_pressure = m_recomp_frac_guess =
				m_N_mc_guess = m_N_t_guess = m_tol = m_opt_tol = std::numeric_limits<double>::quiet_NaN();

			m_N_sub_hxrs = -1;
			
			m_is_target_Q = m_fixed_recomp_frac = m_fixed_N_mc = m_fixed_N_t = true;

			m_use_default_res = true;
		}
	};

	struct S_PHX_od_parameters
	{
		double m_m_dot_htf_des;		//[kg/s] Design point htf mass flow rate
		
		double m_T_htf_hot;			//[K] Current htf inlet temperature
		double m_m_dot_htf;			//[kg/s] Current htf mass flow rate
		double m_T_htf_cold;		//[K] Target htf cold return temp

		double m_UA_PHX_des;		//[kW/K] Design point PHX conductance

		double m_cp_htf;			//[kW/K] Constant HTF specific heat

		S_PHX_od_parameters()
		{
			m_m_dot_htf_des = m_T_htf_hot = m_m_dot_htf = m_T_htf_cold = m_UA_PHX_des = m_cp_htf = std::numeric_limits<double>::quiet_NaN();
		}
	};

private:
		// Component classes
	C_turbine m_t;
	C_compressor m_mc;
	C_recompressor m_rc;
	C_HeatExchanger /*m_LT, m_HT,*/ m_PHX, m_PC;
	
	C_HX_co2_to_co2 mc_LT_recup, mc_HT_recup;
	
		// Input/Ouput structures for class methods
	S_design_limits ms_des_limits;
	S_design_parameters ms_des_par;
	S_opt_design_parameters ms_opt_des_par;
	S_auto_opt_design_parameters ms_auto_opt_des_par;
	S_design_solved ms_des_solved;
	//S_od_turbo_bal_par ms_od_turbo_bal_par;
	S_od_turbo_bal_csp_par ms_od_turbo_bal_csp_par;
	S_od_turbo_bal_csp_solved ms_od_turbo_bal_csp_solved;
	S_od_parameters ms_od_par;
	S_od_phi_par ms_od_phi_par;
	S_opt_od_parameters ms_opt_od_par;
	S_target_od_parameters ms_tar_od_par;
	S_opt_target_od_parameters ms_opt_tar_od_par;
	S_od_solved ms_od_solved;
	S_PHX_od_parameters ms_phx_od_par;

		// Results from last 'design' solution
	std::vector<double> m_temp_last, m_pres_last, m_enth_last, m_entr_last, m_dens_last;		// thermodynamic states (K, kPa, kJ/kg, kJ/kg-K, kg/m3)
	double m_eta_thermal_last;
	double m_W_dot_net_last;
	double m_m_dot_mc, m_m_dot_rc, m_m_dot_t;
	double m_Q_dot_PHX, m_Q_dot_bypass, m_eta_bypass;
	double m_W_dot_mc, m_W_dot_rc, m_W_dot_mc_bypass;
	
		// Structures and data for optimization
	S_design_parameters ms_des_par_optimal;
	double m_eta_thermal_opt;

		// Structures and data for auto-optimization
	double m_eta_thermal_auto_opt;	
	S_design_parameters ms_des_par_auto_opt;

		// Results from last off-design solution
	std::vector<double> m_temp_od, m_pres_od, m_enth_od, m_entr_od, m_dens_od;					// thermodynamic states (K, kPa, kJ/kg, kJ/kg-K, kg/m3)
	double m_eta_thermal_od;
	double m_W_dot_net_od;
	double m_Q_dot_PHX_od;

		// Structures and data for off-design optimization
	S_od_parameters ms_od_par_optimal;
	double m_W_dot_net_max;

		// Structures and data for optimal target off-design
	S_od_parameters ms_od_par_tar_optimal;
	double m_eta_best;
	double m_biggest_target;

		// New opt
	bool m_found_opt;
	double m_eta_phx_max;
	double m_UA_diff_eta_max;
	double m_over_deltaP_eta_max;

	void design_core(int & error_code);	

	void design_core_standard(int & error_code);
	
	//void design_core_bypass(int & error_code);

	//void design_core_bypass150C(int & error_code);

	//void design_core_HTR_hs(int & error_code);

	void opt_design_core(int & error_code);

	void auto_opt_design_core(int & error_code);

	void finalize_design(int & error_code);	

	void off_design_core(int & error_code);

	void off_design_phi_core(int & error_code);

	void off_design_fix_shaft_speeds_core(int & error_code);
	
	void optimal_off_design_core(int & error_code);

	void target_off_design_core(int & error_code);	

	void clear_ms_od_solved();

public:

	C_RecompCycle()
	{
		m_temp_last.resize(10);
		std::fill(m_temp_last.begin(), m_temp_last.end(), std::numeric_limits<double>::quiet_NaN());
		
		m_pres_last = m_enth_last = m_entr_last = m_dens_last = m_temp_last;

		/*
		m_pres_last.resize(10);
		std::fill(m_pres_last.begin(), m_pres_last.end(), std::numeric_limits<double>::quiet_NaN());
		m_enth_last.resize(10);
		std::fill(m_enth_last.begin(), m_enth_last.end(), std::numeric_limits<double>::quiet_NaN());
		m_entr_last.resize(10);
		std::fill(m_entr_last.begin(), m_entr_last.end(), std::numeric_limits<double>::quiet_NaN());
		m_dens_last.resize(10);
		std::fill(m_dens_last.begin(), m_dens_last.end(), std::numeric_limits<double>::quiet_NaN());
		*/		

		m_eta_thermal_last = m_m_dot_mc = m_m_dot_rc = m_m_dot_t = std::numeric_limits<double>::quiet_NaN();
		m_Q_dot_PHX = m_Q_dot_bypass = m_eta_bypass = std::numeric_limits<double>::quiet_NaN();
		m_W_dot_mc = m_W_dot_rc = m_W_dot_mc_bypass = std::numeric_limits<double>::quiet_NaN();

		m_W_dot_net_last = std::numeric_limits<double>::quiet_NaN();

		m_eta_thermal_opt = m_eta_thermal_opt = std::numeric_limits<double>::quiet_NaN();

		m_temp_od = m_pres_od = m_enth_od = m_entr_od = m_dens_od = m_temp_last;

		m_eta_thermal_od = m_W_dot_net_od = m_Q_dot_PHX_od = std::numeric_limits<double>::quiet_NaN();

		m_W_dot_net_max = m_eta_best = m_biggest_target = std::numeric_limits<double>::quiet_NaN();

		m_found_opt = false;

		m_eta_phx_max = m_over_deltaP_eta_max = m_UA_diff_eta_max = std::numeric_limits<double>::quiet_NaN();

		// Set design limits!!!!
		ms_des_limits.m_UA_net_power_ratio_max = 2.0;		//[-/K]
		ms_des_limits.m_UA_net_power_ratio_min = 1.E-5;		//[-/K]

		// Set minimum main compressor inlet temperature
		CO2_info s_co2_info;

		get_CO2_info(&s_co2_info);

		ms_des_limits.m_T_mc_in_min = ceil(s_co2_info.T_critical);		//[K]
	}

	CO2_state mc_co2_props;

	~C_RecompCycle(){}

	void design(S_design_parameters & des_par_in, int & error_code);

	void opt_design(S_opt_design_parameters & opt_des_par_in, int & error_code);

	void od_turbo_bal_csp(const S_od_turbo_bal_csp_par & par_in);

	double od_turbo_bal_csp_Wnet(const std::vector<double> &x);
	
	void reset_ms_od_turbo_bal_csp_solved();

	void optimize_od_turbo_balance_csp(S_od_turbo_bal_csp_par in_params, std::vector<double> &opt_params);

	void auto_opt_design(S_auto_opt_design_parameters & auto_opt_des_par_in, int & error_code);

	void auto_opt_design_hit_eta(S_auto_opt_design_hit_eta_parameters & auto_opt_des_hit_eta_in, int & error_code, string & error_msg);

	void off_design(S_od_parameters & od_par_in, int & error_code);

	void off_design_phi(S_od_phi_par & od_phi_par_in, int & error_code);

	void off_design_fix_shaft_speeds(S_od_phi_par & od_phi_par_in, int & error_code);

	void optimal_off_design(S_opt_od_parameters & opt_od_par_in, int & error_code);
	
	void get_max_output_od(S_opt_target_od_parameters & opt_tar_od_par_in, int & error_code);

	void target_off_design(S_target_od_parameters & tar_od_par_in, int & error_code);

	void optimal_target_off_design(S_opt_target_od_parameters & opt_tar_od_par_in, int & error_code);

	void optimal_target_off_design_no_check(S_opt_target_od_parameters & opt_tar_od_par_in, int & error_code);

	void opt_od_eta_for_hx(S_od_parameters & od_par_in, S_PHX_od_parameters phx_od_par_in, int & error_code);

	double get_od_temp(int n_state_point);

	double get_od_pres(int n_state_point);
	
	void set_od_temp(int n_state_point, double temp_K);

	void set_od_pres(int n_state_point, double pres_kPa);

	void off_design_recompressor(double T_in, double P_in, double m_dot, double P_out, int & error_code, double & T_out);

	void estimate_od_turbo_operation(double T_mc_in /*K*/, double P_mc_in /*kPa*/, double f_recomp /*-*/, double T_t_in /*K*/, double phi_mc /*-*/,
							int & mc_error_code, double & mc_w_tip_ratio /*-*/, double & P_mc_out /*kPa*/,
							int & rc_error_code, double & rc_w_tip_ratio /*-*/, double & rc_phi /*-*/,
							bool is_update_ms_od_solved = false);

	const C_recompressor::S_od_solved * get_rc_od_solved()
	{
		return m_rc.get_od_solved();
	}

	const S_design_solved * get_design_solved()
	{
		return &ms_des_solved;
	}	

	const S_od_turbo_bal_csp_solved *get_od_turbo_bal_csp_solved()
	{
		return &ms_od_turbo_bal_csp_solved;
	}

	const S_od_solved * get_od_solved()
	{
		return &ms_od_solved;
	}

	double get_max_target()
	{
		return m_biggest_target;
	}

	const S_design_limits & get_design_limits()
	{
		return ms_des_limits;
	}

	class C_mono_eq_x_f_recomp_y_N_rc : public C_monotonic_equation
	{
	private:
		C_RecompCycle *mpc_rc_cycle;

		double m_T_mc_in;		//[K] Compressor inlet temperature
		double m_P_mc_in;		//[kPa] Compressor inlet pressure
		double m_T_t_in;		//[K] Turbine inlet temperature

	public:
		
		double m_m_dot_t;		//[kg/s]
		double m_m_dot_rc;		//[kg/s]
		double m_m_dot_mc;		//[kg/s]

		C_mono_eq_x_f_recomp_y_N_rc(C_RecompCycle *pc_rc_cycle, double T_mc_in /*K*/, double P_mc_in /*kPa*/, double T_t_in /*K*/)
		{
			mpc_rc_cycle = pc_rc_cycle;
			m_T_mc_in = T_mc_in;		//[K]
			m_P_mc_in = P_mc_in;		//[kPa]
			m_T_t_in = T_t_in;			//[K]

			m_m_dot_t = m_m_dot_rc = m_m_dot_mc = std::numeric_limits<double>::quiet_NaN();
		}

		virtual int operator()(double f_recomp /*-*/, double *diff_N_rc /*-*/);

		CO2_state mc_co2_props;
	};

	class C_mono_eq_turbo_N_fixed_m_dot : public C_monotonic_equation
	{
	private:
		C_RecompCycle *mpc_rc_cycle;

		double m_T_mc_in;		//[K] Compressor inlet temperature
		double m_P_mc_in;		//[kPa] Compressor inlet pressure
		double m_f_recomp;		//[-] Recompression fraction
		double m_T_t_in;		//[K] Turbine inlet temperature

		bool m_is_update_ms_od_solved;	//[-] Bool to update member structure ms_od_solved
		// that is typically updated after entire cycle off-design solution 

	public:
		C_mono_eq_turbo_N_fixed_m_dot(C_RecompCycle *pc_rc_cycle, double T_mc_in /*K*/, double P_mc_in /*kPa*/,
			double f_recomp /*-*/, double T_t_in /*K*/, bool is_update_ms_od_solved = false)
		{
			mpc_rc_cycle = pc_rc_cycle;
			m_T_mc_in = T_mc_in;			//[K]
			m_P_mc_in = P_mc_in;			//[kPa]
			m_f_recomp = f_recomp;			//[-]
			m_T_t_in = T_t_in;				//[K]

			m_is_update_ms_od_solved = is_update_ms_od_solved;
		}

		virtual int operator()(double m_dot_t /*kg/s*/, double *diff_m_dot_t /*-*/);

		CO2_state mc_co2_props;	
	};

	class C_mono_eq_turbo_m_dot : public C_monotonic_equation
	{
	private:
		C_RecompCycle *mpc_rc_cycle;

		double m_T_mc_in;		//[K] Compressor inlet temperature
		double m_P_mc_in;		//[kPa] Compressor inlet pressure
		double m_f_recomp;		//[-] Recompression fraction
		double m_T_t_in;		//[K] Turbine inlet temperature
		double m_phi_mc;		//[-] Compressor flow coefficient

		bool m_is_update_ms_od_solved;	//[-] Bool to update member structure ms_od_solved
										// that is typically updated after entire cycle off-design solution 

	public:
		C_mono_eq_turbo_m_dot(C_RecompCycle *pc_rc_cycle, double T_mc_in /*K*/, double P_mc_in /*kPa*/,
									double f_recomp /*-*/, double T_t_in /*K*/, double phi_mc /*-*/,
									bool is_update_ms_od_solved = false)
		{
			mpc_rc_cycle = pc_rc_cycle;
			m_T_mc_in = T_mc_in;		//[K]
			m_P_mc_in = P_mc_in;		//[kPa]
			m_f_recomp = f_recomp;		//[-]
			m_T_t_in = T_t_in;			//[K]
			m_phi_mc = phi_mc;			//[-]

			m_is_update_ms_od_solved = is_update_ms_od_solved;
		}
	
		virtual int operator()(double m_dot_t /*kg/s*/, double *diff_m_dot_t /*-*/);

		CO2_state mc_co2_props;
	};

	class C_mono_eq_LTR_od : public C_monotonic_equation
	{
	private:
		C_RecompCycle *mpc_rc_cycle;

	public:
		C_mono_eq_LTR_od(C_RecompCycle *pc_rc_cycle, double m_dot_rc, double m_dot_mc, double m_dot_t)
		{
			mpc_rc_cycle = pc_rc_cycle;
			m_m_dot_rc = m_dot_rc;
			m_m_dot_mc = m_dot_mc;
			m_m_dot_t = m_dot_t;
		}	
		
		// These values are calculated in the operator() method and need to be extracted form this class
		//     after convergence
		double m_Q_dot_LTR;

		// These values are passed in as arguments to Constructor call and should not be reset
		double m_m_dot_rc, m_m_dot_mc, m_m_dot_t;

		virtual int operator()(double T_LTR_LP_out /*K*/, double *diff_T_LTR_LP_out /*K*/);
	};

	class C_mono_eq_LTR_des : public C_monotonic_equation
	{
	private:
		C_RecompCycle *mpc_rc_cycle;

	public:
		C_mono_eq_LTR_des(C_RecompCycle *pc_rc_cycle, double w_mc, double w_t)
		{
			mpc_rc_cycle = pc_rc_cycle;
			m_w_mc = w_mc;
			m_w_t = w_t;
		}
	
		// These values are calculated in the operator() method and need to be extracted from this class
		//     after convergence
		double m_w_rc, m_m_dot_t, m_m_dot_rc, m_m_dot_mc, m_Q_dot_LT;

		// These values are passed in as arguments to Constructor call and should not be reset
		double m_w_mc, m_w_t;

		virtual int operator()(double T_LTR_LP_out /*K*/, double *diff_T_LTR_LP_out /*K*/);
	};

	class C_mono_eq_HTR_od : public C_monotonic_equation
	{
	private:
		C_RecompCycle *mpc_rc_cycle;

	public:
		C_mono_eq_HTR_od(C_RecompCycle *pc_rc_cycle, double m_dot_rc, double m_dot_mc, double m_dot_t)
		{
			mpc_rc_cycle = pc_rc_cycle;
			m_m_dot_rc = m_dot_rc;
			m_m_dot_mc = m_dot_mc;
			m_m_dot_t = m_dot_t;
		}
	
		// These values are passed in as arguments to Constructor call and should not be reset
		double m_m_dot_rc, m_m_dot_mc, m_m_dot_t;

		// These values are calculated in the operator() method and need to be extracted from this class
		//     after convergence
		double m_Q_dot_LTR, m_Q_dot_HTR;

		virtual int operator()(double T_HTR_LP_out_guess /*K*/, double *diff_T_HTR_LP_out /*K*/);
	};

	class C_mono_eq_HTR_des : public C_monotonic_equation
	{
	private:
		C_RecompCycle *mpc_rc_cycle;

	public:
		C_mono_eq_HTR_des(C_RecompCycle *pc_rc_cycle, double w_mc, double w_t)
		{
			mpc_rc_cycle = pc_rc_cycle;
			m_w_mc = w_mc;
			m_w_t = w_t;
		}

		// These values are calculated in the operator() method and need to be extracted from this class
		//     after convergence
		double m_w_rc, m_m_dot_t, m_m_dot_rc, m_m_dot_mc, m_Q_dot_LT, m_Q_dot_HT;

		// These values are passed in as arguments to Constructor call and should not be reset
		double m_w_mc, m_w_t;

		virtual int operator()(double T_HTR_LP_out /*K*/, double *diff_T_HTR_LP_out /*K*/);	
	};

	// Called by 'nlopt_callback_opt_des_1', so needs to be public
	double design_point_eta(const std::vector<double> &x);

	// Called by 'fmin_callback_opt_eta', so needs to be public
	double opt_eta(double P_high_opt);

	// Called by 'nlopt_cb_opt_od', so needs to be public
	double off_design_point_value(const std::vector<double> &x);

	// Called by 'nlopt...', so needs to be public
	double eta_at_target(const std::vector<double> &x);
	
	// Called by 'nlopt...', so needs to be public
	double opt_od_eta(const std::vector<double> &x);
};

double nlopt_callback_opt_des_1(const std::vector<double> &x, std::vector<double> &grad, void *data);

double fmin_callback_opt_eta_1(double x, void *data);

double nlopt_cb_opt_od(const std::vector<double> &x, std::vector<double> &grad, void *data);

double nlopt_cb_eta_at_target(const std::vector<double> &x, std::vector<double> &grad, void *data);

double nlopt_cb_opt_od_eta(const std::vector<double> &x, std::vector<double> &grad, void *data);

double P_pseudocritical_1(double T_K);


double nlopt_callback_tub_bal_opt(const std::vector<double> &x, std::vector<double> &grad, void *data);


bool find_polynomial_coefs(const std::vector<double> x_data, const std::vector<double> y_data, int n_coefs, std::vector<double> & coefs_out, double & r_squared);


class C_poly_curve_r_squared
{
private:
	std::vector<double> m_x;
	std::vector<double> m_y;
	int m_n_points;
	double m_y_bar;
	double m_SS_tot;

public:
	C_poly_curve_r_squared()
	{
		m_x.resize(0);
		m_y.resize(0);
		m_n_points = -1;
		m_y_bar = std::numeric_limits<double>::quiet_NaN();
		m_SS_tot = std::numeric_limits<double>::quiet_NaN();
	}

	bool init(const std::vector<double> x_data, const std::vector<double> y_data);	


	// Called by 'nlopt...', so needs to be public
	double calc_r_squared(const std::vector<double> coefs);

};

double nlopt_callback_poly_coefs(const std::vector<double> &x, std::vector<double> &grad, void *data);

#endif
