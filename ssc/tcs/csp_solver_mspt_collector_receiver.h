#ifndef __csp_solver_mspt_collector_receiver_
#define __csp_solver_mspt_collector_receiver_

#include "csp_solver_core.h"
#include "csp_solver_pt_heliostatfield.h"
#include "csp_solver_mspt_receiver_222.h"



class C_csp_mspt_collector_receiver : public C_csp_collector_receiver
{

private:
	C_pt_heliostatfield &mc_pt_heliostatfield;
	C_mspt_receiver_222 &mc_mspt_receiver_222;

public:
	
	enum
	{
		E_FIELD_Q_DOT_INC,		//[MWt] Field incident thermal power
		E_FIELD_ETA_OPT,		//[-] Optical efficiency including receiver refl
		E_FIELD_ADJUST,			//[-] Field adjustment factor
		
		E_Q_DOT_INC,			//[MWt] Receiver incident thermal power
		E_ETA_THERMAL,			//[-] Receiver thermal efficiency
		E_Q_DOT_THERMAL,		//[MWt] Field incident thermal power
		E_M_DOT_HTF,			//[kg/hr] Receiver mass flow rate
		E_Q_DOT_STARTUP,		//[MWt] Receiver startup thermal power consumed
		E_T_HTF_IN,				//[C] Receiver HTF inlet temperature
		E_T_HTF_OUT,			//[C] Receiver HTF outlet temperature
		E_Q_DOT_PIPE_LOSS,		//[MWt] Tower piping losses
        E_Q_DOT_LOSS            //[MWt] Receiver convection and radiation losses
	};
	
	C_csp_reported_outputs mc_reported_outputs;
	
	C_csp_mspt_collector_receiver(C_pt_heliostatfield & pt_heliostatfield, 
		C_mspt_receiver_222 & mspt_receiver_222);

	~C_csp_mspt_collector_receiver();

	virtual void init(const C_csp_collector_receiver::S_csp_cr_init_inputs init_inputs, 
			C_csp_collector_receiver::S_csp_cr_solved_params & solved_params);

	virtual int get_operating_state();

    virtual double get_startup_time();
    virtual double get_startup_energy(); //MWh
    virtual double get_pumping_parasitic_coef();  //MWe/MWt
    virtual double get_min_power_delivery();    //MWt
	virtual double get_tracking_power();		//MWe
	virtual double get_col_startup_power();		//MWe-hr

    virtual void off(const C_csp_weatherreader::S_outputs &weather,
		const C_csp_solver_htf_1state &htf_state_in,
		C_csp_collector_receiver::S_csp_cr_out_solver &cr_out_solver,
		//C_csp_collector_receiver::S_csp_cr_out_report &cr_out_report,
		const C_csp_solver_sim_info &sim_info);

	virtual void startup(const C_csp_weatherreader::S_outputs &weather,
		const C_csp_solver_htf_1state &htf_state_in,
		C_csp_collector_receiver::S_csp_cr_out_solver &cr_out_solver,
		//C_csp_collector_receiver::S_csp_cr_out_report &cr_out_report,
		const C_csp_solver_sim_info &sim_info);

	virtual void on(const C_csp_weatherreader::S_outputs &weather,
		const C_csp_solver_htf_1state &htf_state_in,
		double field_control,
		C_csp_collector_receiver::S_csp_cr_out_solver &cr_out_solver,
		//C_csp_collector_receiver::S_csp_cr_out_report &cr_out_report,
		const C_csp_solver_sim_info &sim_info);

	virtual void estimates(const C_csp_weatherreader::S_outputs &weather,
		const C_csp_solver_htf_1state &htf_state_in,
		C_csp_collector_receiver::S_csp_cr_est_out &est_out,
		const C_csp_solver_sim_info &sim_info);

	virtual void converged();

	virtual void write_output_intervals(double report_time_start,
		const std::vector<double> & v_temp_ts_time_end, double report_time_end);

    virtual double calculate_optical_efficiency( const C_csp_weatherreader::S_outputs &weather, const C_csp_solver_sim_info &sim );
  
    virtual double calculate_thermal_efficiency_approx( const C_csp_weatherreader::S_outputs &weather, double q_incident /*MW*/ );

    virtual double get_collector_area();


	void call(const C_csp_weatherreader::S_outputs &weather,
		const C_csp_solver_htf_1state &htf_state_in,
		const C_csp_collector_receiver::S_csp_cr_inputs &inputs,
		C_csp_collector_receiver::S_csp_cr_out_solver &cr_out_solver,
		//C_csp_collector_receiver::S_csp_cr_out_report &cr_out_report,
		const C_csp_solver_sim_info &sim_info);

};








#endif //__csp_solver_mspt_collector_receiver_