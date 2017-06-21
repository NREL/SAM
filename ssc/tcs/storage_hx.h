// HTF_props.h -- function prototypes for HTF property routines
#ifndef __STORAGE_HX_
#define __STORAGE_HX_

#include "htf_props.h"
//#include <shared/lib_util.h>
#include "lib_util.h"
#include "sam_csp_util.h"

class Storage_HX
{
public:
	Storage_HX();
	
	enum {
		Counter_flow = 2,
		Parallel_flow,
		Cross_flow_unmixed,
		Shell_and_tube};

	bool define_storage( HTFProperties &fluid_field, HTFProperties &fluid_store, bool is_direct, 
		int config, double duty_des, double vol_des, double h_des, 
		double u_des, double tank_pairs_des, double hot_htr_set_point_des, double cold_htr_set_point_des,
		double max_q_htr_cold, double max_q_htr_hot, double dt_hot_des, double dt_cold_des, double T_h_in_des, double T_h_out_des );

	//bool hx_size( HTFProperties &fluid_field, HTFProperties &fluid_store, 
	//	int config, double duty_des, double vol_des, double h_des, 
	//	double u_des, double tank_pairs_des, double hot_htr_set_point_des, double cold_htr_set_point_des, 
	//	double max_q_htr, double dt_hot_des, double dt_cold_des, double T_h_in_des, double T_h_out_des );

	bool mixed_tank( bool is_hot_tank, double dt, double m_prev, double T_prev, double m_dot_in, double m_dot_out, 
						double T_in, double T_amb, double &T_ave, double &vol_ave, 
						double &q_loss, double &T_fin, double &vol_fin, double &m_fin, double &q_heater);

	bool hx_performance( bool is_hot_side_mdot, bool is_storage_side, double T_hot_in, double m_dot_known, double T_cold_in, 
							double &eff, double &T_hot_out, double &T_cold_out, double &q_trans, double &m_dot_solved );

	bool hx_perf_q_transfer(bool is_hot_side_mdot, bool is_storage_side, double T_hot_in, double m_dot_known, double T_cold_in, double &q_trans);

private:
	HTFProperties m_field_htfProps;
	HTFProperties m_store_htfProps;
	bool m_is_direct;
	int m_config;
	double m_dt_cold_des;
	double m_dt_hot_des;
	double m_vol_des;
	double m_h_des;
	double m_u_des;
	double m_tank_pairs_des;
	double m_Thtr_hot_des;
	double m_Thtr_cold_des;
	double m_a_cs;
	double m_dia;
	double m_ua;
	double m_dot_des;			//[kg/s]  7/9/14 twn: added
	double m_max_q_htr_cold;
	double m_max_q_htr_hot;

	// HX properties
	double m_eff_des;
	double m_UA_des;
										
};

#endif
