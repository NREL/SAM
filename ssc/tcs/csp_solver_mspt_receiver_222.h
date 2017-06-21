#ifndef __csp_solver_mspt_receiver_222_
#define __csp_solver_mspt_receiver_222_

#include "csp_solver_util.h"

#include "htf_props.h"
#include "ngcc_powerblock.h"
#include "csp_solver_core.h"

class C_mspt_receiver_222
{
private:
	HTFProperties field_htfProps;		// Instance of HTFProperties class for field HTF
	HTFProperties tube_material;		// Instance of HTFProperties class for receiver tube material
	HTFProperties ambient_air;			// Instance of HTFProperties class for ambient air

	ngcc_power_cycle cycle_calcs;

	double m_id_tube;
	double m_A_tube;
	int m_n_t;
	double m_A_rec_proj;
	double m_A_node;
	
	double m_Q_dot_piping_loss;		//[Wt] = Constant thermal losses from piping to env. = (THT*length_mult + length_add) * piping_loss_coef

	int m_itermode;
	double m_od_control;
	double m_eta_field_iter_prev;	//[-] Efficiency from heliostat on last iteration. Maybe change if CR gets defocus signal from controller
	double m_tol_od;
	double m_m_dot_htf_des;

	/* declare storage variables here */
	int m_mode;
	int m_mode_prev;
	double m_E_su;
	double m_E_su_prev;
	double m_t_su;
	double m_t_su_prev;

	util::matrix_t<int> m_flow_pattern;
	int m_n_lines;

	util::matrix_t<double> m_flux_in;

	util::matrix_t<double> m_q_dot_inc;

	util::matrix_t<double> m_T_s_guess;
	util::matrix_t<double> m_T_s;
	util::matrix_t<double> m_T_panel_out_guess;
	util::matrix_t<double> m_T_panel_out;
	util::matrix_t<double> m_T_panel_in_guess;
	util::matrix_t<double> m_T_panel_in;
	util::matrix_t<double> m_T_panel_ave;
	util::matrix_t<double> m_T_panel_ave_guess;
	util::matrix_t<double> m_T_film;
	util::matrix_t<double> m_q_dot_conv;
	util::matrix_t<double> m_q_dot_rad;
	util::matrix_t<double> m_q_dot_loss;
	util::matrix_t<double> m_q_dot_abs;

	double m_m_mixed;
	double m_LoverD;
	double m_RelRough;

	// ISCC-specific
	double m_T_amb_low;
	double m_T_amb_high;
	double m_P_amb_low;
	double m_P_amb_high;
	double m_q_iscc_max;

	// member string for exception messages
	std::string error_msg;

	// track number of calls per timestep, reset = -1 in converged() call
	int m_ncall;

public:
	// Class to save messages for up stream classes
	C_csp_messages csp_messages;

	// Data
	int m_n_panels;					//[-]
	double m_d_rec;					//[m]
	double m_h_rec;					//[m]
	double m_h_tower;				//[m]
	double m_od_tube;				//[mm], convert to [m] in init()
	double m_th_tube;				//[mm], convert to [m] in init()
	double m_epsilon;				//[-]
	double m_hl_ffact;				//[-]
	double m_T_htf_hot_des;			//[C], convert to [K] in init()
	double m_T_htf_cold_des;		//[C], convert to [K] in init()
	double m_f_rec_min;				//[-]
	double m_q_rec_des;				//[MW], convert to [W] in init()
	double m_rec_su_delay;			//[-]
	double m_rec_qf_delay;			//[-]
	double m_m_dot_htf_max;			//[kg/hr], convert to [kg/s] in init()
	double m_A_sf;					//[m2]

	// 8.10.2015 twn: add tower piping thermal losses to receiver performance
	double m_pipe_loss_per_m;		//[Wt/m]
	double m_pipe_length_add;		//[m]
	double m_pipe_length_mult;		//[-]

	int m_n_flux_x;
	int m_n_flux_y;

	// Calculate in init()
	double m_q_dot_inc_min;			//[Wt]
	// double m_q_rec_min;				//[W]

		// 4.17.15 twn: former TCS inputs, moved to member data because are constant throughout simulation
	double m_T_salt_hot_target;			//[C], convert to K in init() call
	double m_eta_pump;					//[-]
	int m_night_recirc;					//[-]
	double m_hel_stow_deploy;			//[-]

		// Added for csp_solver/tcs wrappers:
	int m_field_fl;
	util::matrix_t<double> m_field_fl_props;	
	int m_mat_tube;
	int m_flow_type;
    int m_crossover_shift;

		// ISCC specific
	bool m_is_iscc;
	int m_cycle_config;
	
	struct S_inputs
	{
		double m_field_eff;					//[-] 
		int m_input_operation_mode;			//[-]

		const util::matrix_t<double> *m_flux_map_input;		//[-]

		S_inputs()
		{
			m_field_eff = std::numeric_limits<double>::quiet_NaN();

			m_input_operation_mode = -1;
		}
	};

		// Let's put outputs in a structure...
	struct S_outputs
	{
		
		double m_m_dot_salt_tot;		//[kg/hr] 
		double m_eta_therm;				//[-] RECEIVER thermal efficiency
		double m_W_dot_pump;			//[MW] 
		double m_q_conv_sum;			//[MW] 
		double m_q_rad_sum;				//[MW] 
		double m_Q_thermal;				//[MW] Thermal power delivered to TES/PC: subtracts piping losses (q_dot_rec - q_dot_piping_losses)
		double m_T_salt_hot;			//[C]
		double m_field_eff_adj;			//[-] Heliostat field efficiency including component defocus
		double m_component_defocus;		//[-] Defocus applied by component model to stay within mass flow or other constraints
		double m_q_dot_rec_inc;			//[MWt] Receiver incident thermal power (after reflection losses)
		double m_q_startup;				//[MWt-hr]
		double m_dP_receiver;			//[bar] receiver pressure drop
		double m_dP_total;				//[bar] total pressure drop
		double m_vel_htf;				//[m/s] HTF flow velocity through receiver tubes
		double m_T_salt_cold;			//[C] 
		double m_m_dot_ss;				//[kg/hr] 
		double m_q_dot_ss;				//[MW] 
		double m_f_timestep;			//[-]
		double m_time_required_su;		//[s]
		double m_q_dot_piping_loss;		//[MWt] Thermal power lost from piping to surroundings 
	
		S_outputs()
		{
			m_m_dot_salt_tot = m_eta_therm = m_W_dot_pump = m_q_conv_sum = m_q_rad_sum = m_Q_thermal =
				m_T_salt_hot = m_field_eff_adj = m_component_defocus = m_q_dot_rec_inc = m_q_startup = m_dP_receiver = m_dP_total =
				m_vel_htf = m_T_salt_cold = m_m_dot_ss = m_q_dot_ss = m_f_timestep = 
				m_time_required_su = m_q_dot_piping_loss = std::numeric_limits<double>::quiet_NaN();
		}
	};

	S_outputs ms_outputs;

	void clear_outputs();
	
	// Methods
	C_mspt_receiver_222();

	~C_mspt_receiver_222(){};

	void init();

	int get_operating_state();

	void call(const C_csp_weatherreader::S_outputs &weather, 
		const C_csp_solver_htf_1state &htf_state_in, 
		const C_mspt_receiver_222::S_inputs &inputs,
		const C_csp_solver_sim_info &sim_info);

	void off(const C_csp_weatherreader::S_outputs &weather,
		const C_csp_solver_htf_1state &htf_state_in,
		const C_csp_solver_sim_info &sim_info);

	void converged();

    void calc_pump_performance(double rho_f, double mdot, double ffact, double &PresDrop_calc, double &WdotPump_calc);

    HTFProperties *get_htf_property_object();

};
















#endif // __csp_solver_mspt_receiver_222_