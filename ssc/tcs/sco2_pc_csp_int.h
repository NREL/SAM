#ifndef __SCO2_PC_CSP_INT_
#define __SCO2_PC_CSP_INT_

#include "sco2_pc_core.h"
#include "heat_exchangers.h"
#include "csp_solver_util.h"

#include "numeric_solvers.h"

#include "ud_power_cycle.h"

#include <iostream>
#include <fstream>

class C_sco2_rc_csp_template
{
public:

	struct S_des_par
	{
		// System Design
		int m_hot_fl_code;				//[-] Integer coding the HTF type
		util::matrix_t<double> mc_hot_fl_props;	//[-] Custom HTF properties (if applicable)
		double m_T_htf_hot_in;			//[K] Design-point hot inlet temperature
		double m_phx_dt_hot_approach;	//[K/C] Temperature difference between hot HTF and turbine CO2 inlet
		double m_T_amb_des;				//[K] Ambient temperature
		double m_dt_mc_approach;		//[K] Temperature difference between main compressor inlet and ambient air
		double m_elevation;				//[m] Site elevation
		double m_W_dot_net;				//[kW] Target net cycle power
		int m_design_method;			//[-] 1 = Specify efficiency, 2 = Specify total recup UA
		double m_eta_thermal;			//[-] Cycle thermal efficiency
		double m_UA_recup_tot_des;		//[kW/K] Total recuperator conductance

		// Cycle design parameters
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

		// PHX design parameters
		// This is a PHX rather than system parameter because we don't know T_CO2_in until cycle model is solved
		double m_phx_dt_cold_approach;	//[K/C] Temperature difference between cold HTF and PHX CO2 inlet

		// Air cooler parameters
		double m_frac_fan_power;		//[-] Fraction of total cycle power 'S_des_par_cycle_dep.m_W_dot_fan_des' consumed by air fan
		double m_deltaP_cooler_frac;    // [-] Fraction of high side (of cycle, i.e. comp outlet) pressure that is allowed as pressure drop to design the ACC

		S_des_par()
		{
			m_hot_fl_code = m_design_method = m_N_sub_hxrs = -1;

			m_is_recomp_ok = -1;

			m_T_htf_hot_in = m_phx_dt_hot_approach = m_T_amb_des = m_dt_mc_approach =
				m_elevation = m_W_dot_net = m_eta_thermal = m_LT_eff_max = m_HT_eff_max =

				m_eta_mc = m_eta_rc = m_eta_t =
				m_P_high_limit = m_tol = m_opt_tol = m_N_turbine =

				m_PR_mc_guess =

				m_phx_dt_cold_approach = m_frac_fan_power = m_deltaP_cooler_frac =
				std::numeric_limits<double>::quiet_NaN();

			m_fixed_PR_mc = false;		//[-] If false, then should default to optimizing this parameter
		}
	};

	struct S_des_solved
	{
		C_HX_counterflow::S_des_solved ms_phx_des_solved;
		C_RecompCycle::S_design_solved ms_rc_cycle_solved;
	};

	struct S_od_par
	{
		// From CSP System
		double m_T_htf_hot;		//[K] Hot HTF temperature from the receiver or storage
		double m_m_dot_htf;		//[kg/s] HTF mass flow rate 

		// Ambient Conditions
		double m_T_amb;			//[K] Ambient temperature

		S_od_par()
		{
			m_T_htf_hot = m_m_dot_htf = m_T_amb = std::numeric_limits<double>::quiet_NaN();
		}
	};

	struct S_od_operation_inputs
	{
		double m_P_mc_in;		//[kPa]
		double m_recomp_frac;	//[-]
		double m_phi_mc;		//[-]

		S_od_operation_inputs()
		{
			m_P_mc_in = m_recomp_frac = m_phi_mc = std::numeric_limits<double>::quiet_NaN();
		}
	};

	struct S_od_opt_eta_tracking
	{	// The values here are updated on NLOPT calls to the off design models
		bool m_is_opt_found;
		double m_eta_max;
		S_od_operation_inputs ms_od_op_in_max;
		double m_over_T_t_in_at_eta_max;
		double m_over_P_high_at_eta_max;

		S_od_opt_eta_tracking()
		{
			m_is_opt_found = false;

			m_eta_max = m_over_T_t_in_at_eta_max = m_over_P_high_at_eta_max = std::numeric_limits<double>::quiet_NaN();
		}
	};

	struct S_od_solved
	{
		C_RecompCycle::S_od_solved ms_rc_cycle_od_solved;
		C_HX_counterflow::S_od_solved ms_phx_od_solved;
		int m_od_error_code;

		S_od_solved()
		{
			m_od_error_code = 0;
		}
	};

	enum E_off_design_strategies
	{
		E_MAX_ETA = 1,
		E_MAX_ETA_FIX_PHI,
		E_MAX_POWER,
		E_MAX_POWER_FIX_PHI,
		E_MOO_ETA_0p1Wnd,
		E_MOO_ETA_0p1Wnd_FIX_PHI,
		E_MOO_ETA_T_T_IN,
		E_MOO_ETA_T_T_IN_FIX_PHI,
		E_MAX_POWER_IN_ETA_MAX_BAND,
		E_TARGET_POWER_ETA_MAX
	};

	enum E_system_op_constraints
	{
		E_TURBINE_INLET_OVER_TEMP = -15,
		E_OVER_PRESSURE,
		E_TIP_RATIO,
		E_MC_SURGE,
		E_RC_SURGE
	};

	enum E_off_design_turbo_operation
	{
		E_VFD_MC_VFD_RC_FIXED_T,
		E_FIXED_MC_FIXED_RC_FIXED_T
	};

	C_csp_messages mc_messages;

	// Callback function with progress bar
	bool(*mf_callback_update)(std::string &log_msg, std::string &progress_msg, void *data, double progress);
	void *mp_mf_update;
	
	C_sco2_rc_csp_template()
	{
		mf_callback_update = 0;
		mp_mf_update = 0;
	};

	~C_sco2_rc_csp_template(){};

	virtual void design(C_sco2_rc_csp_template::S_des_par des_par) = 0;	

	virtual const C_HX_counterflow::S_des_calc_UA_par * get_phx_des_par() = 0;

	virtual const S_des_par * get_design_par() = 0;

	virtual const S_des_solved * get_design_solved() = 0;

	virtual const S_od_solved * get_od_solved() = 0;

	virtual int off_design_nested_opt(C_sco2_rc_csp_template::S_od_par od_par, int off_design_strategy, double od_opt_tol = 1.E-4) = 0;

	virtual int generate_ud_pc_tables(double T_htf_low /*C*/, double T_htf_high /*C*/, int n_T_htf /*-*/,
		double T_amb_low /*C*/, double T_amb_high /*C*/, int n_T_amb /*-*/,
		double m_dot_htf_ND_low /*-*/, double m_dot_htf_ND_high /*-*/, int n_m_dot_htf_ND,
		util::matrix_t<double> & T_htf_ind, util::matrix_t<double> & T_amb_ind, util::matrix_t<double> & m_dot_htf_ND_ind) = 0;


};

class C_sco2_recomp_csp : public C_sco2_rc_csp_template
{

private:
	C_RecompCycle mc_rc_cycle;
	C_HX_co2_to_htf mc_phx;
	C_CO2_to_air_cooler mc_air_cooler;

	S_des_par ms_des_par;
	C_RecompCycle::S_auto_opt_design_hit_eta_parameters ms_rc_cycle_des_par;
	C_CO2_to_air_cooler::S_des_par_ind ms_air_cooler_des_par_ind;
	C_CO2_to_air_cooler::S_des_par_cycle_dep ms_air_cooler_des_par_dep;
	C_HX_counterflow::S_des_calc_UA_par ms_phx_des_par;
		
	S_des_solved ms_des_solved;

	S_od_par ms_od_par;
	C_RecompCycle::S_od_phi_par ms_rc_cycle_od_phi_par;
	C_HX_counterflow::S_od_par ms_phx_od_par;
	S_od_opt_eta_tracking ms_od_opt_eta_tracking;

	S_od_solved ms_od_solved;
	
	// Optimization variables: could make into structure...
	int m_od_opt_objective;		//[-]
	bool m_is_phi_optimized;	//[-]
	double m_od_opt_ftol;		//[-] Relative tolerance for od optimization: objective function convergence
	double m_od_opt_xtol;		//[-] Relative tolerance for od optimization: independent variable convergence
	bool m_true_nlopt_false_fmin;	//[-]
	double m_eta_max_eta;		//[-] Maximum efficiency from previous optimization 
	// ******************************************************

	int m_off_design_turbo_operation;	//[-] How is turbomachinery controlled off-design?

	double m_T_mc_in_min;		//[K]
	double m_T_co2_crit;		//[K]
	double m_P_co2_crit;		//[kPa]

	void design_core();

	void reset_S_od_opt_eta_tracking();

	double adjust_P_mc_in_away_2phase(double T_co2 /*K*/, double P_mc_in /*kPa*/);

	int od_fix_T_mc__nl_opt_shell__opt_eta();

public:	

	C_sco2_recomp_csp();

	~C_sco2_recomp_csp(){};

	class C_mono_eq_T_t_in : public C_monotonic_equation
	{
	private: 
		C_sco2_recomp_csp *mpc_sco2_rc;

	public:
		C_mono_eq_T_t_in(C_sco2_recomp_csp *pc_sco2_rc)
		{
			mpc_sco2_rc = pc_sco2_rc;
		}
	
		virtual int operator()(double T_t_in /*K*/, double *diff_T_t_in /*-*/);
	};

	class C_sco2_csp_od : public C_od_pc_function
	{
	private:
		C_sco2_recomp_csp *mpc_sco2_rc;

	public:
		C_sco2_csp_od(C_sco2_recomp_csp *pc_sco2_rc)
		{
			mpc_sco2_rc = pc_sco2_rc;
		}
	
		virtual int operator()(S_f_inputs inputs, S_f_outputs & outputs);
	};

	virtual int generate_ud_pc_tables(double T_htf_low /*C*/, double T_htf_high /*C*/, int n_T_htf /*-*/,
		double T_amb_low /*C*/, double T_amb_high /*C*/, int n_T_amb /*-*/,
		double m_dot_htf_ND_low /*-*/, double m_dot_htf_ND_high /*-*/, int n_m_dot_htf_ND,
		util::matrix_t<double> & T_htf_ind, util::matrix_t<double> & T_amb_ind, util::matrix_t<double> & m_dot_htf_ND_ind);

	virtual void design(C_sco2_rc_csp_template::S_des_par des_par);

	void off_design_P_mc_in_parameteric(double P_mc_in_min /*kPa*/, double P_mc_in_max /*kPa*/, double P_mc_in_inc /*kPa*/);

	void off_design_fix_P_mc_in_parametric_f_recomp(double P_mc_in /*kPa*/, double f_recomp_min /*-*/, double f_recomp_max /*-*/, double f_recomp_inc /*-*/);

	virtual int off_design_nested_opt(C_sco2_recomp_csp::S_od_par od_par, int off_design_strategy, double od_opt_tol = 1.E-4);

	bool opt_f_recomp_fix_P_mc_in_max_eta_core();

	bool opt_P_mc_in_nest_f_recomp_max_eta_core();

	bool opt_P_mc_in_nest_f_recomp_max_eta_core_old_but_working();

	int off_design_opt(C_sco2_recomp_csp::S_od_par od_par, int off_design_strategy, double od_opt_tol = 1.E-4);

	int off_design(C_sco2_recomp_csp::S_od_par od_par, S_od_operation_inputs od_op_inputs);

	int find_a_feasible_off_design_solution(C_sco2_recomp_csp::S_od_par od_par, double T_mc_in /*K*/,
				S_od_operation_inputs & od_op_inputs);

	int off_design_core(double & eta_solved);

	// Class methods linked to nlopt callbacks - must be public
	double od_fix_T_mc_approach__nl_opt_shell(const std::vector<double> &x);

	double get_T_mc_in_min()
	{
		return m_T_mc_in_min;		//[K]
	}

	// Methods to private access member data
	virtual const S_des_par * get_design_par();

	virtual const S_des_solved * get_design_solved();

	virtual const C_HX_counterflow::S_des_calc_UA_par * get_phx_des_par();

	virtual const S_od_solved * get_od_solved();

	void sweep_turbomachinery_deltaP(double T_mc_in /*K*/, double P_mc_in /*kPa*/,
							double T_t_in /*K*/, double phi_mc /*-*/);

	double opt_f_recomp_max_eta(double f_recomp);

	double opt_P_mc_in_nest_f_recomp_max_eta(double P_mc_in /*kPa*/);

	bool m_is_write_mc_out_file;
	bool m_is_only_write_frecomp_opt_iters;

	ofstream mc_P_mc_in_fixed_f_recomp_vary_file;
	ofstream mc_P_mc_vary_f_recomp_opt_file;
	std::string mstr_base_name;
};

double nlopt_max_f_recomp_cycle_eta(const std::vector<double> &x, std::vector<double> &grad, void *data);

double fmin_f_recomp_cycle_eta(double x, void *data);

double nlopt_max_opt_P_mc_in_nest_f_recomp(const std::vector<double> &x, std::vector<double> &grad, void *data);

double fmin_opt_P_mc_in_nest_f_recomp_max_eta(double x, void *data);

// Optimization method callbacks
double nlopt_cb_opt_od_eta__float_phx_dt(const std::vector<double> &x, std::vector<double> &grad, void *data);


class C_sco2_recomp_csp_10MWe_scale : public C_sco2_rc_csp_template
{

private:

	C_sco2_recomp_csp mc_rc_csp_10MWe;

	double m_r_W_scale;

	S_des_par ms_des_par;
	C_HX_counterflow::S_des_calc_UA_par ms_phx_des_par;

	S_des_solved ms_des_solved;
	S_od_solved ms_od_solved;

public:

	C_sco2_recomp_csp_10MWe_scale();

	~C_sco2_recomp_csp_10MWe_scale(){};

	virtual void design(C_sco2_rc_csp_template::S_des_par des_par);

	virtual int off_design_nested_opt(C_sco2_rc_csp_template::S_od_par od_par, int off_design_strategy, double od_opt_tol = 1.E-4);
	
	virtual int generate_ud_pc_tables(double T_htf_low /*C*/, double T_htf_high /*C*/, int n_T_htf /*-*/,
		double T_amb_low /*C*/, double T_amb_high /*C*/, int n_T_amb /*-*/,
		double m_dot_htf_ND_low /*-*/, double m_dot_htf_ND_high /*-*/, int n_m_dot_htf_ND,
		util::matrix_t<double> & T_htf_ind, util::matrix_t<double> & T_amb_ind, util::matrix_t<double> & m_dot_htf_ND_ind);

	// Methods to private access member data
	virtual const S_des_par * get_design_par();

	virtual const S_des_solved * get_design_solved();

	virtual const C_HX_counterflow::S_des_calc_UA_par * get_phx_des_par();

	virtual const S_od_solved * get_od_solved();

};


#endif
