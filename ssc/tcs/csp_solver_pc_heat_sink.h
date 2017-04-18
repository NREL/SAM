#ifndef __csp_solver_pc_heat_sink_
#define __csp_solver_pc_heat_sink_

#include "csp_solver_core.h"
#include "csp_solver_util.h"

#include "lib_util.h"
#include "htf_props.h"

class C_pc_heat_sink : public C_csp_power_cycle
{

public:

	enum
	{
		E_Q_DOT_HEAT_SINK,
		E_W_DOT_PUMPING			//[MWe]	
	};
	
	C_csp_reported_outputs mc_reported_outputs;

private:

	double m_max_frac;		//[-]
	double m_m_dot_htf_des;	//[kg/s]

	HTFProperties mc_pc_htfProps;

	void check_double_params_are_set();

public:
	// Class to save messages for up stream classes
	C_csp_messages mc_csp_messages;

	struct S_params
	{
		double m_T_htf_cold_des;	//[C]
		double m_T_htf_hot_des;		//[C]
		double m_q_dot_des;			//[MWt]
		double m_htf_pump_coef;		//[kW/kg/s] Pumping power to move 1 kg/s of HTF through power cycle

		int m_pc_fl;				//[-] integer flag identifying Heat Transfer Fluid (HTF) in power block {1-27}
		util::matrix_t<double> m_pc_fl_props;

		S_params()
		{
			m_T_htf_cold_des = m_T_htf_hot_des = 
				m_q_dot_des = m_htf_pump_coef = std::numeric_limits<double>::quiet_NaN();
		}
	};
	
	S_params ms_params;

	C_pc_heat_sink();

	~C_pc_heat_sink(){};

	virtual void init(C_csp_power_cycle::S_solved_params &solved_params);

	virtual int get_operating_state();

	virtual double get_cold_startup_time();
	virtual double get_warm_startup_time();
	virtual double get_hot_startup_time();
	virtual double get_standby_energy_requirement();    //[MW]
	virtual double get_cold_startup_energy();    //[MWh]
	virtual double get_warm_startup_energy();    //[MWh]
	virtual double get_hot_startup_energy();    //[MWh]
	virtual double get_max_thermal_power();     //MW
	virtual double get_min_thermal_power();     //MW
	virtual double get_efficiency_at_TPH(double T_degC, double P_atm, double relhum_pct, double *w_dot_condenser=0);
	virtual double get_efficiency_at_load(double load_frac, double *w_dot_condenser=0);
	virtual double get_htf_pumping_parasitic_coef();		//[kWe/kWt]

	// This can vary between timesteps for Type224, depending on remaining startup energy and time
	virtual double get_max_q_pc_startup();		//[MWt]


	virtual void call(const C_csp_weatherreader::S_outputs &weather,
		C_csp_solver_htf_1state &htf_state_in,
		const C_csp_power_cycle::S_control_inputs &inputs,
		C_csp_power_cycle::S_csp_pc_out_solver &out_solver,
		//C_csp_power_cycle::S_csp_pc_out_report &out_report,
		const C_csp_solver_sim_info &sim_info);

	virtual void converged();

	virtual void write_output_intervals(double report_time_start,
		const std::vector<double> & v_temp_ts_time_end, double report_time_end);

	virtual void assign(int index, float *p_reporting_ts_array, int n_reporting_ts_array);
};


#endif // __csp_solver_pc_heat_sink