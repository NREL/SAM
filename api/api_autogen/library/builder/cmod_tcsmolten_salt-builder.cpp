#include <string>
#include <vector>

#include "vartab.h"

#include "cmod_tcsmolten_salt-builder.h"

var_table TcsmoltenSalt_SupercriticalCarbonDioxidePowerCycle_BtnSco2DesAndUdpc_func(var_table* vt, invoke_t* cxt)
{
	// inputs
	undefined htf
	undefined htf_props
	float T_htf_hot_des = vt->lookup("T_htf_hot_des")->num;
	undefined dT_PHX_hot_approach
	float T_amb_des = vt->lookup("T_amb_des")->num;
	undefined dT_mc_approach
	undefined site_elevation
	undefined W_dot_net_des
	undefined design_method
	undefined eta_thermal_des
	undefined UA_recup_tot_des
	undefined cycle_config
	undefined is_recomp_ok
	undefined is_P_high_fixed
	undefined is_PR_fixed
	undefined des_objective
	undefined min_phx_deltaT
	undefined rel_tol
	undefined eta_isen_mc
	undefined eta_isen_rc
	undefined eta_isen_pc
	undefined eta_isen_t
	undefined LT_recup_eff_max
	undefined HT_recup_eff_max
	undefined deltaP_counterHX_frac
	float P_high_limit = vt->lookup("P_high_limit")->num;
	undefined dT_PHX_cold_approach
	undefined is_design_air_cooler
	undefined fan_power_frac
	undefined deltaP_cooler_frac
	undefined is_generate_udpc
	undefined is_apply_default_htf_mins
	undefined T_htf_hot_low
	undefined T_htf_hot_high
	undefined n_T_htf_hot
	undefined T_amb_low
	undefined T_amb_high
	undefined n_T_amb
	undefined m_dot_htf_ND_low
	undefined m_dot_htf_ND_high
	undefined n_m_dot_htf_ND

	// outputs
	float HTR_UA_calc;
	float LTR_UA_calc;

sco2_design_point1.000000
obj = ssc_create(  );
ssc_var( obj, htf, rec_htf );
ssc_var( obj, htf_props, field_fl_props );
ssc_var( obj, T_htf_hot_des, T_htf_hot_des );
ssc_var( obj, dT_PHX_hot_approach, deltaT_PHX );
ssc_var( obj, T_amb_des, sco2_T_amb_des );
ssc_var( obj, dT_mc_approach, sco2_T_approach );
ssc_var( obj, site_elevation, elev );
ssc_var( obj, W_dot_net_des, P_ref );
ssc_var( obj, cycle_config, sco2_cycle_config );
ssc_var( obj, design_method, 1.000000 );
ssc_var( obj, eta_thermal_des, design_eff );
ssc_var( obj, eta_isen_mc, eta_c );
ssc_var( obj, eta_isen_rc, eta_c );
ssc_var( obj, eta_isen_pc, eta_c );
ssc_var( obj, eta_isen_t, eta_t );
ssc_var( obj, LT_recup_eff_max, 1.000000 );
ssc_var( obj, HT_recup_eff_max, 1.000000 );
ssc_var( obj, P_high_limit, P_high_limit );
ssc_var( obj, dT_PHX_cold_approach, deltaT_PHX );
ssc_var( obj, fan_power_frac, fan_power_perc_net / 100.000000 );
ssc_var( obj, deltaP_cooler_frac, 0.002000 );
ssc_var( obj, is_generate_udpc, is_gen_udpc );
ssc_var( obj, m_dot_htf_ND_low, cycle_cutoff_frac );
ssc_var( obj, m_dot_htf_ND_high, 1.050000 );
if ( is_gen_udpc ) {
	logmsg( sCO2 Design Point Optimization )
}
else {
	logmsg( sCO2 Design Point Optimization and Off-Design Parameter Calculation )}
std::string ret = ssc_exec( obj, sco2_csp_ud_pc_tables, {"show_dialog": "1.000000", "hold_dialog": "1.000000", } );
if ( ret != 0.000000 ) {
	ssc_free( obj );
	value( is_sco2_designed, 0.000000 );
	value( is_sco2_off_designed, 0.000000 );
	value( is_sco2_des_par_diff, 1.000000 );
	update_uiif ( is_sco2_designed == 0.000000 ) {
		value( HTR_UA_calc, nan(  ) );
		value( LTR_UA_calc, nan(  ) );
		value( sco2ud_T_htf_cold_calc, nan(  ) );
		value( recomp_frac_calc, nan(  ) );
		value( P_in_calc, nan(  ) );
		value( P_IP_calc, nan(  ) );
		value( P_out_calc, nan(  ) );
	
	}
	if ( is_sco2_off_designed == 0.000000 ) {
		value( _sco2_T_htf_hot_des, nan(  ) );
		value( _sco2_deltaT_PHX, nan(  ) );
		value( _sco2_T_amb_des, nan(  ) );
		value( _sco2_T_approach, nan(  ) );
		value( _sco2_P_ref, nan(  ) );
		value( _sco2_design_eff, nan(  ) );
		value( _sco2_eta_c, nan(  ) );
		value( _sco2_eta_t, nan(  ) );
		value( _sco2_recup_eff_max, nan(  ) );
		value( _sco2_P_high_limit, nan(  ) );
		value( _sco2_fan_power_perc_net, nan(  ) );
	
	}
	compare_des_parsfloat is_des_par_diff = 0.000000;
	if ( T_htf_hot_des != _sco2_T_htf_hot_des ) {
		float is_des_par_diff = 1.000000;
		if ( isnan( _sco2_T_htf_hot_des ) ) {
			enable( _sco2_T_htf_hot_des, 0.000000 )
		}
		else {
			enable( _sco2_T_htf_hot_des, 1.000000 )}
	
	}
	else {
		enable( _sco2_T_htf_hot_des, 0.000000 )}
	if ( deltaT_PHX != _sco2_deltaT_PHX ) {
		float is_des_par_diff = 1.000000;
		if ( isnan( _sco2_deltaT_PHX ) ) {
			enable( _sco2_deltaT_PHX, 0.000000 )
		}
		else {
			enable( _sco2_deltaT_PHX, 1.000000 )}
	
	}
	else {
		enable( _sco2_deltaT_PHX, 0.000000 )}
	if ( sco2_T_amb_des != _sco2_T_amb_des ) {
		float is_des_par_diff = 1.000000;
		if ( isnan( _sco2_T_amb_des ) ) {
			enable( _sco2_T_amb_des, 0.000000 )
		}
		else {
			enable( _sco2_T_amb_des, 1.000000 )}
	
	}
	else {
		enable( _sco2_T_amb_des, 0.000000 )}
	if ( sco2_T_approach != _sco2_T_approach ) {
		float is_des_par_diff = 1.000000;
		if ( isnan( _sco2_T_approach ) ) {
			enable( _sco2_T_approach, 0.000000 )
		}
		else {
			enable( _sco2_T_approach, 1.000000 )}
	
	}
	else {
		enable( _sco2_T_approach, 0.000000 )}
	if ( P_ref != _sco2_P_ref ) {
		float is_des_par_diff = 1.000000;
		if ( isnan( _sco2_P_ref ) ) {
			enable( _sco2_P_ref, 0.000000 )
		}
		else {
			enable( _sco2_P_ref, 1.000000 )}
	
	}
	else {
		enable( _sco2_P_ref, 0.000000 )}
	if ( design_eff != _sco2_design_eff ) {
		float is_des_par_diff = 1.000000;
		if ( isnan( _sco2_design_eff ) ) {
			enable( _sco2_design_eff, 0.000000 )
		}
		else {
			enable( _sco2_design_eff, 1.000000 )}
	
	}
	else {
		enable( _sco2_design_eff, 0.000000 )}
	if ( eta_c != _sco2_eta_c ) {
		float is_des_par_diff = 1.000000;
		if ( isnan( _sco2_eta_c ) ) {
			enable( _sco2_eta_c, 0.000000 )
		}
		else {
			enable( _sco2_eta_c, 1.000000 )}
	
	}
	else {
		enable( _sco2_eta_c, 0.000000 )}
	if ( eta_t != _sco2_eta_t ) {
		float is_des_par_diff = 1.000000;
		if ( isnan( _sco2_eta_t ) ) {
			enable( _sco2_eta_t, 0.000000 )
		}
		else {
			enable( _sco2_eta_t, 1.000000 )}
	
	}
	else {
		enable( _sco2_eta_t, 0.000000 )}
	if ( recup_eff_max != _sco2_recup_eff_max ) {
		float is_des_par_diff = 1.000000;
		if ( isnan( _sco2_recup_eff_max ) ) {
			enable( _sco2_recup_eff_max, 0.000000 )
		}
		else {
			enable( _sco2_recup_eff_max, 1.000000 )}
	
	}
	else {
		enable( _sco2_recup_eff_max, 0.000000 )}
	if ( P_high_limit != _sco2_P_high_limit ) {
		float is_des_par_diff = 1.000000;
		if ( isnan( _sco2_P_high_limit ) ) {
			enable( _sco2_P_high_limit, 0.000000 )
		}
		else {
			enable( _sco2_P_high_limit, 1.000000 )}
	
	}
	else {
		enable( _sco2_P_high_limit, 0.000000 )}
	if ( fan_power_perc_net != _sco2_fan_power_perc_net ) {
		float is_des_par_diff = 1.000000;
		if ( isnan( _sco2_fan_power_perc_net ) ) {
			enable( _sco2_fan_power_perc_net, 0.000000 )
		}
		else {
			enable( _sco2_fan_power_perc_net, 1.000000 )}
	
	}
	else {
		enable( _sco2_fan_power_perc_net, 0.000000 )}
	if ( dd_sco2_cycle_config != _dd_sco2_cycle_config ) {
		float is_des_par_diff = 1.000000
	}
	if ( dd_sco2_cycle_config == 0.000000 ) {
		enable( P_IP_calc, 0.000000 )
	}
	else {
		enable( P_IP_calc, 1.000000 )}
	value( is_sco2_des_par_diff, is_des_par_diff );
	update_radio_buttonsif ( is_sco2_off_designed == 1.000000 && is_sco2_des_par_diff == 0.000000 ) {
		enable( is_sco2_preprocess, 1.000000 );
		property( sco2_cb_msg, caption, msg_off_des );
		if ( is_sco2_preprocess == 1.000000 ) {
			show( sco2_cb_msg, 0.000000 )
		}
		else {
			show( sco2_cb_msg, 1.000000 )}
	
	}
	else {
		enable( is_sco2_preprocess, 0.000000 );
		value( is_sco2_preprocess, 0.000000 );
		if ( is_sco2_off_designed == 1.000000 ) {
			property( sco2_cb_msg, caption, msg_des_par_diff )
		}
		else {
			property( sco2_cb_msg, caption, msg_nan )}
		show( sco2_cb_msg, 1.000000 );
	}
	refresh(  );
	;
	;
	refresh(  );
	;
	update_radio_buttonsif ( is_sco2_off_designed == 1.000000 && is_sco2_des_par_diff == 0.000000 ) {
		enable( is_sco2_preprocess, 1.000000 );
		property( sco2_cb_msg, caption, msg_off_des );
		if ( is_sco2_preprocess == 1.000000 ) {
			show( sco2_cb_msg, 0.000000 )
		}
		else {
			show( sco2_cb_msg, 1.000000 )}
	
	}
	else {
		enable( is_sco2_preprocess, 0.000000 );
		value( is_sco2_preprocess, 0.000000 );
		if ( is_sco2_off_designed == 1.000000 ) {
			property( sco2_cb_msg, caption, msg_des_par_diff )
		}
		else {
			property( sco2_cb_msg, caption, msg_nan )}
		show( sco2_cb_msg, 1.000000 );
	}
	refresh(  );
	;
	;

}
value( is_sco2_designed, 1.000000 );
value( is_sco2_des_par_diff, 1.000000 );
value( _dd_sco2_cycle_config, dd_sco2_cycle_config );
value( _sco2_T_htf_hot_des, T_htf_hot_des );
value( _sco2_deltaT_PHX, deltaT_PHX );
value( _sco2_T_amb_des, sco2_T_amb_des );
value( _sco2_T_approach, sco2_T_approach );
value( _sco2_P_ref, P_ref );
value( _sco2_design_eff, design_eff );
value( _sco2_eta_c, eta_c );
value( _sco2_eta_t, eta_t );
value( _sco2_recup_eff_max, recup_eff_max );
value( _sco2_P_high_limit, P_high_limit );
value( _sco2_fan_power_perc_net, fan_power_perc_net );
if ( is_gen_udpc ) {
	value( is_sco2_off_designed, 1.000000 )
}
else {
	value( is_sco2_off_designed, 0.000000 )}
compare_des_parsfloat is_des_par_diff = 0.000000;
if ( T_htf_hot_des != _sco2_T_htf_hot_des ) {
	float is_des_par_diff = 1.000000;
	if ( isnan( _sco2_T_htf_hot_des ) ) {
		enable( _sco2_T_htf_hot_des, 0.000000 )
	}
	else {
		enable( _sco2_T_htf_hot_des, 1.000000 )}

}
else {
	enable( _sco2_T_htf_hot_des, 0.000000 )}
if ( deltaT_PHX != _sco2_deltaT_PHX ) {
	float is_des_par_diff = 1.000000;
	if ( isnan( _sco2_deltaT_PHX ) ) {
		enable( _sco2_deltaT_PHX, 0.000000 )
	}
	else {
		enable( _sco2_deltaT_PHX, 1.000000 )}

}
else {
	enable( _sco2_deltaT_PHX, 0.000000 )}
if ( sco2_T_amb_des != _sco2_T_amb_des ) {
	float is_des_par_diff = 1.000000;
	if ( isnan( _sco2_T_amb_des ) ) {
		enable( _sco2_T_amb_des, 0.000000 )
	}
	else {
		enable( _sco2_T_amb_des, 1.000000 )}

}
else {
	enable( _sco2_T_amb_des, 0.000000 )}
if ( sco2_T_approach != _sco2_T_approach ) {
	float is_des_par_diff = 1.000000;
	if ( isnan( _sco2_T_approach ) ) {
		enable( _sco2_T_approach, 0.000000 )
	}
	else {
		enable( _sco2_T_approach, 1.000000 )}

}
else {
	enable( _sco2_T_approach, 0.000000 )}
if ( P_ref != _sco2_P_ref ) {
	float is_des_par_diff = 1.000000;
	if ( isnan( _sco2_P_ref ) ) {
		enable( _sco2_P_ref, 0.000000 )
	}
	else {
		enable( _sco2_P_ref, 1.000000 )}

}
else {
	enable( _sco2_P_ref, 0.000000 )}
if ( design_eff != _sco2_design_eff ) {
	float is_des_par_diff = 1.000000;
	if ( isnan( _sco2_design_eff ) ) {
		enable( _sco2_design_eff, 0.000000 )
	}
	else {
		enable( _sco2_design_eff, 1.000000 )}

}
else {
	enable( _sco2_design_eff, 0.000000 )}
if ( eta_c != _sco2_eta_c ) {
	float is_des_par_diff = 1.000000;
	if ( isnan( _sco2_eta_c ) ) {
		enable( _sco2_eta_c, 0.000000 )
	}
	else {
		enable( _sco2_eta_c, 1.000000 )}

}
else {
	enable( _sco2_eta_c, 0.000000 )}
if ( eta_t != _sco2_eta_t ) {
	float is_des_par_diff = 1.000000;
	if ( isnan( _sco2_eta_t ) ) {
		enable( _sco2_eta_t, 0.000000 )
	}
	else {
		enable( _sco2_eta_t, 1.000000 )}

}
else {
	enable( _sco2_eta_t, 0.000000 )}
if ( recup_eff_max != _sco2_recup_eff_max ) {
	float is_des_par_diff = 1.000000;
	if ( isnan( _sco2_recup_eff_max ) ) {
		enable( _sco2_recup_eff_max, 0.000000 )
	}
	else {
		enable( _sco2_recup_eff_max, 1.000000 )}

}
else {
	enable( _sco2_recup_eff_max, 0.000000 )}
if ( P_high_limit != _sco2_P_high_limit ) {
	float is_des_par_diff = 1.000000;
	if ( isnan( _sco2_P_high_limit ) ) {
		enable( _sco2_P_high_limit, 0.000000 )
	}
	else {
		enable( _sco2_P_high_limit, 1.000000 )}

}
else {
	enable( _sco2_P_high_limit, 0.000000 )}
if ( fan_power_perc_net != _sco2_fan_power_perc_net ) {
	float is_des_par_diff = 1.000000;
	if ( isnan( _sco2_fan_power_perc_net ) ) {
		enable( _sco2_fan_power_perc_net, 0.000000 )
	}
	else {
		enable( _sco2_fan_power_perc_net, 1.000000 )}

}
else {
	enable( _sco2_fan_power_perc_net, 0.000000 )}
if ( dd_sco2_cycle_config != _dd_sco2_cycle_config ) {
	float is_des_par_diff = 1.000000
}
if ( dd_sco2_cycle_config == 0.000000 ) {
	enable( P_IP_calc, 0.000000 )
}
else {
	enable( P_IP_calc, 1.000000 )}
value( is_sco2_des_par_diff, is_des_par_diff );
update_radio_buttonsif ( is_sco2_off_designed == 1.000000 && is_sco2_des_par_diff == 0.000000 ) {
	enable( is_sco2_preprocess, 1.000000 );
	property( sco2_cb_msg, caption, msg_off_des );
	if ( is_sco2_preprocess == 1.000000 ) {
		show( sco2_cb_msg, 0.000000 )
	}
	else {
		show( sco2_cb_msg, 1.000000 )}

}
else {
	enable( is_sco2_preprocess, 0.000000 );
	value( is_sco2_preprocess, 0.000000 );
	if ( is_sco2_off_designed == 1.000000 ) {
		property( sco2_cb_msg, caption, msg_des_par_diff )
	}
	else {
		property( sco2_cb_msg, caption, msg_nan )}
	show( sco2_cb_msg, 1.000000 );
}
refresh(  );
;
;
refresh(  );
std::string recomp_frac = ssc_var( obj, recomp_frac );
std::string LTR_UA_calc = ssc_var( obj, UA_LTR );
std::string HTR_UA_calc = ssc_var( obj, UA_HTR );
if ( sco2_cycle_config == 1.000000 ) {
	std::string P_comp_in = ssc_var( obj, P_comp_in );
	P_IP_calc = nan(  );

}
else {
	std::string P_comp_in = ssc_var( obj, pc_P_in_des );
	std::string P_IP_calc = ssc_var( obj, P_comp_in );
}
std::string P_comp_out = ssc_var( obj, P_comp_out );
std::string T_htf_cold = ssc_var( obj, T_htf_cold_des );
value( sco2ud_T_htf_cold_calc, T_htf_cold );
value( recomp_frac_calc, recomp_frac );
value( P_in_calc, P_comp_in );
value( P_IP_calc, P_IP_calc );
value( P_out_calc, P_comp_out );
value( LTR_UA_calc, LTR_UA_calc );
value( HTR_UA_calc, HTR_UA_calc );
std::string cycle_type_str = recompression;
float sco2_min_frac = 0.500000;
if ( recomp_frac < 0.010000 ) {
	std::string cycle_type_str = simple;
	float sco2_min_frac = 0.700000;

}
std::string htm_str = Cycle Design Considerations.;
htm_str += 
SAM has calculated values for the HTF cold temperature and minimum turbine operation, and will use the calculated values in the simulation instead of the values on the input pages:;
htm_str += 

HTF cold temperature (System Design page) from  + to_string( value( T_htf_cold_des ) ) +  to  + to_string( T_htf_cold ) +  C.;
htm_str += 
Minimum turbine operation (Power Cycle page) for the  + cycle_type_str +  cycle from  + to_string( value( cycle_cutoff_frac ) ) +  to  + to_string( sco2_min_frac ) + .;
htm_str +=  

Click Yes to replace the current values with calculated ones. This will ensure that the values on the input pages are consistent with the simulation results. You can click No to keep the current values, but SAM will still use the calculated values for simulations.;
if ( yesno( htm_str ) ) {
	value( T_htf_cold_des, T_htf_cold );
	value( cycle_cutoff_frac, sco2_min_frac );
	if ( is_gen_udpc == 1.000000 ) {
		value( sco2ud_m_dot_htf_low, ssc_var( obj, m_dot_htf_ND_low ) );
		value( sco2ud_m_dot_htf_high, ssc_var( obj, m_dot_htf_ND_high ) );
		value( sco2ud_T_htf_ind_od, ssc_var( obj, T_htf_ind ) );
		value( sco2ud_T_amb_low, ssc_var( obj, T_amb_low ) );
		value( sco2ud_T_amb_high, ssc_var( obj, T_amb_high ) );
		value( sco2ud_m_dot_htf_ind_od, ssc_var( obj, m_dot_htf_ND_ind ) );
		value( sco2ud_T_htf_low, ssc_var( obj, T_htf_hot_low ) );
		value( sco2ud_T_htf_high, ssc_var( obj, T_htf_hot_high ) );
		value( sco2ud_T_amb_ind_od, ssc_var( obj, T_amb_ind ) );
	
	}

}
ssc_free( obj );






var_table TcsmoltenSalt_TowerSolarPilotSolarField_TowerSolarPilotSolarField_func(var_table* vt, invoke_t* cxt)
{
	// inputs
	util::matrix_t<ssc_number_t> helio_positions = vt->lookup("helio_positions")->num;
	undefined i
	undefined landbase

	// outputs
	float land_area_base;

logmsg( layoutarea );
util::matrix_t<float> hp = helio_positions;
obj = ssc_create(  );
ssc_var( obj, positions, hp );
std::string ret = ssc_exec( obj, layoutarea );
if ( ret == 0.000000 ) {
	std::string landbase = ssc_var( obj, area );
	value( land_area_base, landbase );

}
else {
	msgbox( Land area update failed:  + ret );
	logmsg( Land area update failed:  + ret );
}
ssc_free( obj );
clearplot( sfplot );
util::matrix_t<float> n = hp.size();
util::matrix_t<float> x = alloc( n );
util::matrix_t<float> y = alloc( n );
for ( float i = 0.000000; i < n; i += 1 ){
	x.insert(x.begin()+i, hp[i][0.000000]);
	y.insert(y.begin()+i, hp[i][1.000000]);

}
plot( x, y, {"type": "scatter", "series": "Heliostat Field", } );
axis( x1, {"label": "Position, east-west (m)", } );
axis( y1, {"label": "Position, north-south (m)", } );
plotopt( {"legend": "0.000000", "coarse": "1.000000", "fine": "0.000000", } );
;
update_button_statusfloat disable_opt = override_opt == 1.000000;
float disable_layout = override_layout == 1.000000;
enable( btn_optimize, !disable_opt );
enable( override_layout, !disable_opt );
enable( btn_solarpilot, !disable_opt || disable_layout );
enable( helio_positions, !disable_opt || disable_layout );
;
update_button_statusfloat disable_opt = override_opt == 1.000000;
float disable_layout = override_layout == 1.000000;
enable( btn_optimize, !disable_opt );
enable( override_layout, !disable_opt );
enable( btn_solarpilot, !disable_opt || disable_layout );
enable( helio_positions, !disable_opt || disable_layout );
;




float TcsmoltenSalt_is_wlim_series_eval(var_table* vt)
{
	// inputs
	float is_dispatch = vt->lookup("is_dispatch")->num;

	// outputs
	float is_wlim_series;

	is_wlim_series = is_dispatch;

	return is_wlim_series;

}



float TcsmoltenSalt_construction_financing_cost_eval(var_table* vt)
{
	// inputs
	float const_per_total1 = vt->lookup("const_per_total1")->num;
	float const_per_total2 = vt->lookup("const_per_total2")->num;
	float const_per_total3 = vt->lookup("const_per_total3")->num;
	float const_per_total4 = vt->lookup("const_per_total4")->num;
	float const_per_total5 = vt->lookup("const_per_total5")->num;

	// outputs
	float construction_financing_cost;

	construction_financing_cost = const_per_total1 + const_per_total2 + const_per_total3 + const_per_total4 + const_per_total5;

	return construction_financing_cost;

}



var_table TcsmoltenSalt_SupercriticalCarbonDioxidePowerCycle_BtnSco2DesAndUdpc_func(var_table* vt, invoke_t* cxt)
{
	// inputs
	undefined P_comp_in
	undefined P_comp_out
	undefined T_htf_cold
	undefined recomp_frac

	// outputs
	float P_in_calc;
	float recomp_frac_calc;
	float P_out_calc;
	float sco2ud_T_htf_cold_calc;

sco2_design_point1.000000
obj = ssc_create(  );
ssc_var( obj, htf, rec_htf );
ssc_var( obj, htf_props, field_fl_props );
ssc_var( obj, T_htf_hot_des, T_htf_hot_des );
ssc_var( obj, dT_PHX_hot_approach, deltaT_PHX );
ssc_var( obj, T_amb_des, sco2_T_amb_des );
ssc_var( obj, dT_mc_approach, sco2_T_approach );
ssc_var( obj, site_elevation, elev );
ssc_var( obj, W_dot_net_des, P_ref );
ssc_var( obj, cycle_config, sco2_cycle_config );
ssc_var( obj, design_method, 1.000000 );
ssc_var( obj, eta_thermal_des, design_eff );
ssc_var( obj, eta_isen_mc, eta_c );
ssc_var( obj, eta_isen_rc, eta_c );
ssc_var( obj, eta_isen_pc, eta_c );
ssc_var( obj, eta_isen_t, eta_t );
ssc_var( obj, LT_recup_eff_max, 1.000000 );
ssc_var( obj, HT_recup_eff_max, 1.000000 );
ssc_var( obj, P_high_limit, P_high_limit );
ssc_var( obj, dT_PHX_cold_approach, deltaT_PHX );
ssc_var( obj, fan_power_frac, fan_power_perc_net / 100.000000 );
ssc_var( obj, deltaP_cooler_frac, 0.002000 );
ssc_var( obj, is_generate_udpc, is_gen_udpc );
ssc_var( obj, m_dot_htf_ND_low, cycle_cutoff_frac );
ssc_var( obj, m_dot_htf_ND_high, 1.050000 );
if ( is_gen_udpc ) {
	logmsg( sCO2 Design Point Optimization )
}
else {
	logmsg( sCO2 Design Point Optimization and Off-Design Parameter Calculation )}
std::string ret = ssc_exec( obj, sco2_csp_ud_pc_tables, {"show_dialog": "1.000000", "hold_dialog": "1.000000", } );
if ( ret != 0.000000 ) {
	ssc_free( obj );
	value( is_sco2_designed, 0.000000 );
	value( is_sco2_off_designed, 0.000000 );
	value( is_sco2_des_par_diff, 1.000000 );
	update_uiif ( is_sco2_designed == 0.000000 ) {
		value( HTR_UA_calc, nan(  ) );
		value( LTR_UA_calc, nan(  ) );
		value( sco2ud_T_htf_cold_calc, nan(  ) );
		value( recomp_frac_calc, nan(  ) );
		value( P_in_calc, nan(  ) );
		value( P_IP_calc, nan(  ) );
		value( P_out_calc, nan(  ) );
	
	}
	if ( is_sco2_off_designed == 0.000000 ) {
		value( _sco2_T_htf_hot_des, nan(  ) );
		value( _sco2_deltaT_PHX, nan(  ) );
		value( _sco2_T_amb_des, nan(  ) );
		value( _sco2_T_approach, nan(  ) );
		value( _sco2_P_ref, nan(  ) );
		value( _sco2_design_eff, nan(  ) );
		value( _sco2_eta_c, nan(  ) );
		value( _sco2_eta_t, nan(  ) );
		value( _sco2_recup_eff_max, nan(  ) );
		value( _sco2_P_high_limit, nan(  ) );
		value( _sco2_fan_power_perc_net, nan(  ) );
	
	}
	compare_des_parsfloat is_des_par_diff = 0.000000;
	if ( T_htf_hot_des != _sco2_T_htf_hot_des ) {
		float is_des_par_diff = 1.000000;
		if ( isnan( _sco2_T_htf_hot_des ) ) {
			enable( _sco2_T_htf_hot_des, 0.000000 )
		}
		else {
			enable( _sco2_T_htf_hot_des, 1.000000 )}
	
	}
	else {
		enable( _sco2_T_htf_hot_des, 0.000000 )}
	if ( deltaT_PHX != _sco2_deltaT_PHX ) {
		float is_des_par_diff = 1.000000;
		if ( isnan( _sco2_deltaT_PHX ) ) {
			enable( _sco2_deltaT_PHX, 0.000000 )
		}
		else {
			enable( _sco2_deltaT_PHX, 1.000000 )}
	
	}
	else {
		enable( _sco2_deltaT_PHX, 0.000000 )}
	if ( sco2_T_amb_des != _sco2_T_amb_des ) {
		float is_des_par_diff = 1.000000;
		if ( isnan( _sco2_T_amb_des ) ) {
			enable( _sco2_T_amb_des, 0.000000 )
		}
		else {
			enable( _sco2_T_amb_des, 1.000000 )}
	
	}
	else {
		enable( _sco2_T_amb_des, 0.000000 )}
	if ( sco2_T_approach != _sco2_T_approach ) {
		float is_des_par_diff = 1.000000;
		if ( isnan( _sco2_T_approach ) ) {
			enable( _sco2_T_approach, 0.000000 )
		}
		else {
			enable( _sco2_T_approach, 1.000000 )}
	
	}
	else {
		enable( _sco2_T_approach, 0.000000 )}
	if ( P_ref != _sco2_P_ref ) {
		float is_des_par_diff = 1.000000;
		if ( isnan( _sco2_P_ref ) ) {
			enable( _sco2_P_ref, 0.000000 )
		}
		else {
			enable( _sco2_P_ref, 1.000000 )}
	
	}
	else {
		enable( _sco2_P_ref, 0.000000 )}
	if ( design_eff != _sco2_design_eff ) {
		float is_des_par_diff = 1.000000;
		if ( isnan( _sco2_design_eff ) ) {
			enable( _sco2_design_eff, 0.000000 )
		}
		else {
			enable( _sco2_design_eff, 1.000000 )}
	
	}
	else {
		enable( _sco2_design_eff, 0.000000 )}
	if ( eta_c != _sco2_eta_c ) {
		float is_des_par_diff = 1.000000;
		if ( isnan( _sco2_eta_c ) ) {
			enable( _sco2_eta_c, 0.000000 )
		}
		else {
			enable( _sco2_eta_c, 1.000000 )}
	
	}
	else {
		enable( _sco2_eta_c, 0.000000 )}
	if ( eta_t != _sco2_eta_t ) {
		float is_des_par_diff = 1.000000;
		if ( isnan( _sco2_eta_t ) ) {
			enable( _sco2_eta_t, 0.000000 )
		}
		else {
			enable( _sco2_eta_t, 1.000000 )}
	
	}
	else {
		enable( _sco2_eta_t, 0.000000 )}
	if ( recup_eff_max != _sco2_recup_eff_max ) {
		float is_des_par_diff = 1.000000;
		if ( isnan( _sco2_recup_eff_max ) ) {
			enable( _sco2_recup_eff_max, 0.000000 )
		}
		else {
			enable( _sco2_recup_eff_max, 1.000000 )}
	
	}
	else {
		enable( _sco2_recup_eff_max, 0.000000 )}
	if ( P_high_limit != _sco2_P_high_limit ) {
		float is_des_par_diff = 1.000000;
		if ( isnan( _sco2_P_high_limit ) ) {
			enable( _sco2_P_high_limit, 0.000000 )
		}
		else {
			enable( _sco2_P_high_limit, 1.000000 )}
	
	}
	else {
		enable( _sco2_P_high_limit, 0.000000 )}
	if ( fan_power_perc_net != _sco2_fan_power_perc_net ) {
		float is_des_par_diff = 1.000000;
		if ( isnan( _sco2_fan_power_perc_net ) ) {
			enable( _sco2_fan_power_perc_net, 0.000000 )
		}
		else {
			enable( _sco2_fan_power_perc_net, 1.000000 )}
	
	}
	else {
		enable( _sco2_fan_power_perc_net, 0.000000 )}
	if ( dd_sco2_cycle_config != _dd_sco2_cycle_config ) {
		float is_des_par_diff = 1.000000
	}
	if ( dd_sco2_cycle_config == 0.000000 ) {
		enable( P_IP_calc, 0.000000 )
	}
	else {
		enable( P_IP_calc, 1.000000 )}
	value( is_sco2_des_par_diff, is_des_par_diff );
	update_radio_buttonsif ( is_sco2_off_designed == 1.000000 && is_sco2_des_par_diff == 0.000000 ) {
		enable( is_sco2_preprocess, 1.000000 );
		property( sco2_cb_msg, caption, msg_off_des );
		if ( is_sco2_preprocess == 1.000000 ) {
			show( sco2_cb_msg, 0.000000 )
		}
		else {
			show( sco2_cb_msg, 1.000000 )}
	
	}
	else {
		enable( is_sco2_preprocess, 0.000000 );
		value( is_sco2_preprocess, 0.000000 );
		if ( is_sco2_off_designed == 1.000000 ) {
			property( sco2_cb_msg, caption, msg_des_par_diff )
		}
		else {
			property( sco2_cb_msg, caption, msg_nan )}
		show( sco2_cb_msg, 1.000000 );
	}
	refresh(  );
	;
	;
	refresh(  );
	;
	update_radio_buttonsif ( is_sco2_off_designed == 1.000000 && is_sco2_des_par_diff == 0.000000 ) {
		enable( is_sco2_preprocess, 1.000000 );
		property( sco2_cb_msg, caption, msg_off_des );
		if ( is_sco2_preprocess == 1.000000 ) {
			show( sco2_cb_msg, 0.000000 )
		}
		else {
			show( sco2_cb_msg, 1.000000 )}
	
	}
	else {
		enable( is_sco2_preprocess, 0.000000 );
		value( is_sco2_preprocess, 0.000000 );
		if ( is_sco2_off_designed == 1.000000 ) {
			property( sco2_cb_msg, caption, msg_des_par_diff )
		}
		else {
			property( sco2_cb_msg, caption, msg_nan )}
		show( sco2_cb_msg, 1.000000 );
	}
	refresh(  );
	;
	;

}
value( is_sco2_designed, 1.000000 );
value( is_sco2_des_par_diff, 1.000000 );
value( _dd_sco2_cycle_config, dd_sco2_cycle_config );
value( _sco2_T_htf_hot_des, T_htf_hot_des );
value( _sco2_deltaT_PHX, deltaT_PHX );
value( _sco2_T_amb_des, sco2_T_amb_des );
value( _sco2_T_approach, sco2_T_approach );
value( _sco2_P_ref, P_ref );
value( _sco2_design_eff, design_eff );
value( _sco2_eta_c, eta_c );
value( _sco2_eta_t, eta_t );
value( _sco2_recup_eff_max, recup_eff_max );
value( _sco2_P_high_limit, P_high_limit );
value( _sco2_fan_power_perc_net, fan_power_perc_net );
if ( is_gen_udpc ) {
	value( is_sco2_off_designed, 1.000000 )
}
else {
	value( is_sco2_off_designed, 0.000000 )}
compare_des_parsfloat is_des_par_diff = 0.000000;
if ( T_htf_hot_des != _sco2_T_htf_hot_des ) {
	float is_des_par_diff = 1.000000;
	if ( isnan( _sco2_T_htf_hot_des ) ) {
		enable( _sco2_T_htf_hot_des, 0.000000 )
	}
	else {
		enable( _sco2_T_htf_hot_des, 1.000000 )}

}
else {
	enable( _sco2_T_htf_hot_des, 0.000000 )}
if ( deltaT_PHX != _sco2_deltaT_PHX ) {
	float is_des_par_diff = 1.000000;
	if ( isnan( _sco2_deltaT_PHX ) ) {
		enable( _sco2_deltaT_PHX, 0.000000 )
	}
	else {
		enable( _sco2_deltaT_PHX, 1.000000 )}

}
else {
	enable( _sco2_deltaT_PHX, 0.000000 )}
if ( sco2_T_amb_des != _sco2_T_amb_des ) {
	float is_des_par_diff = 1.000000;
	if ( isnan( _sco2_T_amb_des ) ) {
		enable( _sco2_T_amb_des, 0.000000 )
	}
	else {
		enable( _sco2_T_amb_des, 1.000000 )}

}
else {
	enable( _sco2_T_amb_des, 0.000000 )}
if ( sco2_T_approach != _sco2_T_approach ) {
	float is_des_par_diff = 1.000000;
	if ( isnan( _sco2_T_approach ) ) {
		enable( _sco2_T_approach, 0.000000 )
	}
	else {
		enable( _sco2_T_approach, 1.000000 )}

}
else {
	enable( _sco2_T_approach, 0.000000 )}
if ( P_ref != _sco2_P_ref ) {
	float is_des_par_diff = 1.000000;
	if ( isnan( _sco2_P_ref ) ) {
		enable( _sco2_P_ref, 0.000000 )
	}
	else {
		enable( _sco2_P_ref, 1.000000 )}

}
else {
	enable( _sco2_P_ref, 0.000000 )}
if ( design_eff != _sco2_design_eff ) {
	float is_des_par_diff = 1.000000;
	if ( isnan( _sco2_design_eff ) ) {
		enable( _sco2_design_eff, 0.000000 )
	}
	else {
		enable( _sco2_design_eff, 1.000000 )}

}
else {
	enable( _sco2_design_eff, 0.000000 )}
if ( eta_c != _sco2_eta_c ) {
	float is_des_par_diff = 1.000000;
	if ( isnan( _sco2_eta_c ) ) {
		enable( _sco2_eta_c, 0.000000 )
	}
	else {
		enable( _sco2_eta_c, 1.000000 )}

}
else {
	enable( _sco2_eta_c, 0.000000 )}
if ( eta_t != _sco2_eta_t ) {
	float is_des_par_diff = 1.000000;
	if ( isnan( _sco2_eta_t ) ) {
		enable( _sco2_eta_t, 0.000000 )
	}
	else {
		enable( _sco2_eta_t, 1.000000 )}

}
else {
	enable( _sco2_eta_t, 0.000000 )}
if ( recup_eff_max != _sco2_recup_eff_max ) {
	float is_des_par_diff = 1.000000;
	if ( isnan( _sco2_recup_eff_max ) ) {
		enable( _sco2_recup_eff_max, 0.000000 )
	}
	else {
		enable( _sco2_recup_eff_max, 1.000000 )}

}
else {
	enable( _sco2_recup_eff_max, 0.000000 )}
if ( P_high_limit != _sco2_P_high_limit ) {
	float is_des_par_diff = 1.000000;
	if ( isnan( _sco2_P_high_limit ) ) {
		enable( _sco2_P_high_limit, 0.000000 )
	}
	else {
		enable( _sco2_P_high_limit, 1.000000 )}

}
else {
	enable( _sco2_P_high_limit, 0.000000 )}
if ( fan_power_perc_net != _sco2_fan_power_perc_net ) {
	float is_des_par_diff = 1.000000;
	if ( isnan( _sco2_fan_power_perc_net ) ) {
		enable( _sco2_fan_power_perc_net, 0.000000 )
	}
	else {
		enable( _sco2_fan_power_perc_net, 1.000000 )}

}
else {
	enable( _sco2_fan_power_perc_net, 0.000000 )}
if ( dd_sco2_cycle_config != _dd_sco2_cycle_config ) {
	float is_des_par_diff = 1.000000
}
if ( dd_sco2_cycle_config == 0.000000 ) {
	enable( P_IP_calc, 0.000000 )
}
else {
	enable( P_IP_calc, 1.000000 )}
value( is_sco2_des_par_diff, is_des_par_diff );
update_radio_buttonsif ( is_sco2_off_designed == 1.000000 && is_sco2_des_par_diff == 0.000000 ) {
	enable( is_sco2_preprocess, 1.000000 );
	property( sco2_cb_msg, caption, msg_off_des );
	if ( is_sco2_preprocess == 1.000000 ) {
		show( sco2_cb_msg, 0.000000 )
	}
	else {
		show( sco2_cb_msg, 1.000000 )}

}
else {
	enable( is_sco2_preprocess, 0.000000 );
	value( is_sco2_preprocess, 0.000000 );
	if ( is_sco2_off_designed == 1.000000 ) {
		property( sco2_cb_msg, caption, msg_des_par_diff )
	}
	else {
		property( sco2_cb_msg, caption, msg_nan )}
	show( sco2_cb_msg, 1.000000 );
}
refresh(  );
;
;
refresh(  );
std::string recomp_frac = ssc_var( obj, recomp_frac );
std::string LTR_UA_calc = ssc_var( obj, UA_LTR );
std::string HTR_UA_calc = ssc_var( obj, UA_HTR );
if ( sco2_cycle_config == 1.000000 ) {
	std::string P_comp_in = ssc_var( obj, P_comp_in );
	P_IP_calc = nan(  );

}
else {
	std::string P_comp_in = ssc_var( obj, pc_P_in_des );
	std::string P_IP_calc = ssc_var( obj, P_comp_in );
}
std::string P_comp_out = ssc_var( obj, P_comp_out );
std::string T_htf_cold = ssc_var( obj, T_htf_cold_des );
value( sco2ud_T_htf_cold_calc, T_htf_cold );
value( recomp_frac_calc, recomp_frac );
value( P_in_calc, P_comp_in );
value( P_IP_calc, P_IP_calc );
value( P_out_calc, P_comp_out );
value( LTR_UA_calc, LTR_UA_calc );
value( HTR_UA_calc, HTR_UA_calc );
std::string cycle_type_str = recompression;
float sco2_min_frac = 0.500000;
if ( recomp_frac < 0.010000 ) {
	std::string cycle_type_str = simple;
	float sco2_min_frac = 0.700000;

}
std::string htm_str = Cycle Design Considerations.;
htm_str += 
SAM has calculated values for the HTF cold temperature and minimum turbine operation, and will use the calculated values in the simulation instead of the values on the input pages:;
htm_str += 

HTF cold temperature (System Design page) from  + to_string( value( T_htf_cold_des ) ) +  to  + to_string( T_htf_cold ) +  C.;
htm_str += 
Minimum turbine operation (Power Cycle page) for the  + cycle_type_str +  cycle from  + to_string( value( cycle_cutoff_frac ) ) +  to  + to_string( sco2_min_frac ) + .;
htm_str +=  

Click Yes to replace the current values with calculated ones. This will ensure that the values on the input pages are consistent with the simulation results. You can click No to keep the current values, but SAM will still use the calculated values for simulations.;
if ( yesno( htm_str ) ) {
	value( T_htf_cold_des, T_htf_cold );
	value( cycle_cutoff_frac, sco2_min_frac );
	if ( is_gen_udpc == 1.000000 ) {
		value( sco2ud_m_dot_htf_low, ssc_var( obj, m_dot_htf_ND_low ) );
		value( sco2ud_m_dot_htf_high, ssc_var( obj, m_dot_htf_ND_high ) );
		value( sco2ud_T_htf_ind_od, ssc_var( obj, T_htf_ind ) );
		value( sco2ud_T_amb_low, ssc_var( obj, T_amb_low ) );
		value( sco2ud_T_amb_high, ssc_var( obj, T_amb_high ) );
		value( sco2ud_m_dot_htf_ind_od, ssc_var( obj, m_dot_htf_ND_ind ) );
		value( sco2ud_T_htf_low, ssc_var( obj, T_htf_hot_low ) );
		value( sco2ud_T_htf_high, ssc_var( obj, T_htf_hot_high ) );
		value( sco2ud_T_amb_ind_od, ssc_var( obj, T_amb_ind ) );
	
	}

}
ssc_free( obj );




float TcsmoltenSalt_A_sf_UI_eval(var_table* vt)
{
	// inputs
	float helio_width = vt->lookup("helio_width")->num;
	float helio_height = vt->lookup("helio_height")->num;
	float dens_mirror = vt->lookup("dens_mirror")->num;
	float n_hel = vt->lookup("n_hel")->num;

	// outputs
	float A_sf_UI;

	A_sf_UI = helio_width * helio_height * dens_mirror * n_hel;

	return A_sf_UI;

}



float TcsmoltenSalt_csp.pt.cost.receiver.area_eval(var_table* vt)
{
	// inputs
	float receiver_type = vt->lookup("receiver_type")->num;
	float rec_height = vt->lookup("rec_height")->num;
	float D_rec = vt->lookup("D_rec")->num;
	float rec_d_spec = vt->lookup("rec_d_spec")->num;
	float csp.pt.rec.cav_ap_height = vt->lookup("csp.pt.rec.cav_ap_height")->num;
	undefined d_rec

	// outputs
	float csp.pt.cost.receiver.area;

	auto switch_receiver_type = [&]{
		float switch_result;
		switch( receiver_type ){
			case 0:
				switch_result = rec_height * D_rec * 3.141593;
				break;
			case 1:
				switch_result = rec_d_spec * csp.pt.rec.cav_ap_height;
				break;
			default:
				throw std::runtime_error("Tower SolarPilot Capital Costs switch undefined case for receiver_type");
			}
		return switch_result;
	};

	auto tech = technology(  );
	float area = 0.000000;
	if ( tech == MSPT || tech == ISCC ) {
		float area = switch_receiver_type()
	}
	else if ( tech == DSPT ) {
		float area = d_rec * rec_height * 3.141593
	}
	csp.pt.cost.receiver.area = area;


	return csp.pt.cost.receiver.area;

}



float TcsmoltenSalt_system_capacity_eval(var_table* vt)
{
	// inputs
	float nameplate = vt->lookup("nameplate")->num;

	// outputs
	float system_capacity;

	system_capacity = nameplate * 1000.000000;

	return system_capacity;

}



float TcsmoltenSalt_q_pb_design_eval(var_table* vt)
{
	// inputs
	float P_ref = vt->lookup("P_ref")->num;
	float design_eff = vt->lookup("design_eff")->num;

	// outputs
	float q_pb_design;

	q_pb_design = P_ref / design_eff;

	return q_pb_design;

}



var_table TcsmoltenSalt_MSPTDispatchControl_CopySchedule_func(var_table* vt, invoke_t* cxt)
{
	// inputs
	util::matrix_t<ssc_number_t> dispatch_sched_weekend = vt->lookup("dispatch_sched_weekend")->num;
	util::matrix_t<ssc_number_t> dispatch_sched_weekday = vt->lookup("dispatch_sched_weekday")->num;

	// outputs
	util::matrix_t<ssc_number_t> weekday_schedule;
	util::matrix_t<ssc_number_t> weekend_schedule;

value( weekday_schedule, value( dispatch_sched_weekday ) );
value( weekend_schedule, value( dispatch_sched_weekend ) );








var_table TcsmoltenSalt_const_per_principal1_MIMO_eval(var_table* vt)
{
	// inputs
	float total_installed_cost = vt->lookup("total_installed_cost")->num;
	float const_per_interest_rate1 = vt->lookup("const_per_interest_rate1")->num;
	float const_per_months1 = vt->lookup("const_per_months1")->num;
	float const_per_percent1 = vt->lookup("const_per_percent1")->num;
	float const_per_upfront_rate1 = vt->lookup("const_per_upfront_rate1")->num;
	float const_per_interest_rate2 = vt->lookup("const_per_interest_rate2")->num;
	float const_per_months2 = vt->lookup("const_per_months2")->num;
	float const_per_percent2 = vt->lookup("const_per_percent2")->num;
	float const_per_upfront_rate2 = vt->lookup("const_per_upfront_rate2")->num;
	float const_per_interest_rate3 = vt->lookup("const_per_interest_rate3")->num;
	float const_per_months3 = vt->lookup("const_per_months3")->num;
	float const_per_percent3 = vt->lookup("const_per_percent3")->num;
	float const_per_upfront_rate3 = vt->lookup("const_per_upfront_rate3")->num;
	float const_per_interest_rate4 = vt->lookup("const_per_interest_rate4")->num;
	float const_per_months4 = vt->lookup("const_per_months4")->num;
	float const_per_percent4 = vt->lookup("const_per_percent4")->num;
	float const_per_upfront_rate4 = vt->lookup("const_per_upfront_rate4")->num;
	float const_per_interest_rate5 = vt->lookup("const_per_interest_rate5")->num;
	float const_per_months5 = vt->lookup("const_per_months5")->num;
	float const_per_percent5 = vt->lookup("const_per_percent5")->num;
	float const_per_upfront_rate5 = vt->lookup("const_per_upfront_rate5")->num;

	// outputs
	float const_per_principal1;
	float const_per_interest1;
	float const_per_total1;
	float const_per_principal2;
	float const_per_interest2;
	float const_per_total2;
	float const_per_principal3;
	float const_per_interest3;
	float const_per_total3;
	float const_per_principal4;
	float const_per_interest4;
	float const_per_total4;
	float const_per_principal5;
	float const_per_interest5;
	float const_per_total5;

	float C = total_installed_cost;
	float r = const_per_interest_rate1 / 100.000000;
	float t = const_per_months1;
	float f = const_per_percent1 / 100.000000;
	float P = f * C;
	float I = P * r / 12.000000 * t / 2.000000;
	float u = const_per_upfront_rate1 / 100.000000;
	float F = P * u;
	const_per_principal1 = P;
	const_per_interest1 = I;
	const_per_total1 = I + F;
	float r = const_per_interest_rate2 / 100.000000;
	float t = const_per_months2;
	float f = const_per_percent2 / 100.000000;
	float P = f * C;
	float I = P * r / 12.000000 * t / 2.000000;
	float u = const_per_upfront_rate2 / 100.000000;
	float F = P * u;
	const_per_principal2 = P;
	const_per_interest2 = I;
	const_per_total2 = I + F;
	float r = const_per_interest_rate3 / 100.000000;
	float t = const_per_months3;
	float f = const_per_percent3 / 100.000000;
	float P = f * C;
	float I = P * r / 12.000000 * t / 2.000000;
	float u = const_per_upfront_rate3 / 100.000000;
	float F = P * u;
	const_per_principal3 = P;
	const_per_interest3 = I;
	const_per_total3 = I + F;
	float r = const_per_interest_rate4 / 100.000000;
	float t = const_per_months4;
	float f = const_per_percent4 / 100.000000;
	float P = f * C;
	float I = P * r / 12.000000 * t / 2.000000;
	float u = const_per_upfront_rate4 / 100.000000;
	float F = P * u;
	const_per_principal4 = P;
	const_per_interest4 = I;
	const_per_total4 = I + F;
	float r = const_per_interest_rate5 / 100.000000;
	float t = const_per_months5;
	float f = const_per_percent5 / 100.000000;
	float P = f * C;
	float I = P * r / 12.000000 * t / 2.000000;
	float u = const_per_upfront_rate5 / 100.000000;
	float F = P * u;
	const_per_principal5 = P;
	const_per_interest5 = I;
	const_per_total5 = I + F;
	throw;


	var_table vt;
	vt.assign( "const_per_principal1", const_per_principal1 );
	vt.assign( "const_per_interest1", const_per_interest1 );
	vt.assign( "const_per_total1", const_per_total1 );
	vt.assign( "const_per_principal2", const_per_principal2 );
	vt.assign( "const_per_interest2", const_per_interest2 );
	vt.assign( "const_per_total2", const_per_total2 );
	vt.assign( "const_per_principal3", const_per_principal3 );
	vt.assign( "const_per_interest3", const_per_interest3 );
	vt.assign( "const_per_total3", const_per_total3 );
	vt.assign( "const_per_principal4", const_per_principal4 );
	vt.assign( "const_per_interest4", const_per_interest4 );
	vt.assign( "const_per_total4", const_per_total4 );
	vt.assign( "const_per_principal5", const_per_principal5 );
	vt.assign( "const_per_interest5", const_per_interest5 );
	vt.assign( "const_per_total5", const_per_total5 );

}



float TcsmoltenSalt_rec_aspect_eval(var_table* vt)
{
	// inputs
	float D_rec = vt->lookup("D_rec")->num;
	float rec_height = vt->lookup("rec_height")->num;

	// outputs
	float rec_aspect;

	float aspect = 1.000000;
	if ( D_rec != 0.000000 ) {
		float aspect = rec_height / D_rec
	}
	rec_aspect = aspect;


	return rec_aspect;

}



var_table TcsmoltenSalt_csp.pt.cost.site_improvements_MIMO_eval(var_table* vt)
{
	// inputs
	float csp.pt.cost.heliostats_m2 = vt->lookup("csp.pt.cost.heliostats_m2")->num;
	float site_spec_cost = vt->lookup("site_spec_cost")->num;
	float heliostat_spec_cost = vt->lookup("heliostat_spec_cost")->num;
	float cost_sf_fixed = vt->lookup("cost_sf_fixed")->num;
	float ui_tower_height = vt->lookup("ui_tower_height")->num;
	float ui_receiver_height = vt->lookup("ui_receiver_height")->num;
	float ui_heliostat_height = vt->lookup("ui_heliostat_height")->num;
	float tower_fixed_cost = vt->lookup("tower_fixed_cost")->num;
	float tower_exp = vt->lookup("tower_exp")->num;
	float csp.pt.cost.receiver.area = vt->lookup("csp.pt.cost.receiver.area")->num;
	float rec_ref_cost = vt->lookup("rec_ref_cost")->num;
	float rec_ref_area = vt->lookup("rec_ref_area")->num;
	float rec_cost_exp = vt->lookup("rec_cost_exp")->num;
	float csp.pt.cost.storage_mwht = vt->lookup("csp.pt.cost.storage_mwht")->num;
	float tes_spec_cost = vt->lookup("tes_spec_cost")->num;
	float csp.pt.cost.power_block_mwe = vt->lookup("csp.pt.cost.power_block_mwe")->num;
	float plant_spec_cost = vt->lookup("plant_spec_cost")->num;
	float bop_spec_cost = vt->lookup("bop_spec_cost")->num;
	float fossil_spec_cost = vt->lookup("fossil_spec_cost")->num;
	float contingency_rate = vt->lookup("contingency_rate")->num;
	float csp.pt.cost.total_land_area = vt->lookup("csp.pt.cost.total_land_area")->num;
	float csp.pt.cost.nameplate = vt->lookup("csp.pt.cost.nameplate")->num;
	float csp.pt.cost.epc.per_acre = vt->lookup("csp.pt.cost.epc.per_acre")->num;
	float csp.pt.cost.epc.percent = vt->lookup("csp.pt.cost.epc.percent")->num;
	float csp.pt.cost.epc.per_watt = vt->lookup("csp.pt.cost.epc.per_watt")->num;
	float csp.pt.cost.epc.fixed = vt->lookup("csp.pt.cost.epc.fixed")->num;
	float land_spec_cost = vt->lookup("land_spec_cost")->num;
	float csp.pt.cost.plm.percent = vt->lookup("csp.pt.cost.plm.percent")->num;
	float csp.pt.cost.plm.per_watt = vt->lookup("csp.pt.cost.plm.per_watt")->num;
	float csp.pt.cost.plm.fixed = vt->lookup("csp.pt.cost.plm.fixed")->num;
	float sales_tax_frac = vt->lookup("sales_tax_frac")->num;
	float csp.pt.cost.sales_tax.value = vt->lookup("csp.pt.cost.sales_tax.value")->num;

	// outputs
	float csp.pt.cost.site_improvements;
	float csp.pt.cost.heliostats;
	float csp.pt.cost.tower;
	float csp.pt.cost.receiver;
	float csp.pt.cost.storage;
	float csp.pt.cost.power_block;
	float csp.pt.cost.bop;
	float csp.pt.cost.fossil;
	float ui_direct_subtotal;
	float csp.pt.cost.contingency;
	float total_direct_cost;
	float csp.pt.cost.epc.total;
	float csp.pt.cost.plm.total;
	float csp.pt.cost.sales_tax.total;
	float total_indirect_cost;
	float total_installed_cost;
	float csp.pt.cost.installed_per_capacity;

	auto obj = ssc_create(  );
	ssc_var( obj, A_sf, csp.pt.cost.heliostats_m2 );
	ssc_var( obj, site_spec_cost, site_spec_cost );
	ssc_var( obj, heliostat_spec_cost, heliostat_spec_cost );
	ssc_var( obj, csp.pt.cost.fixed_sf, cost_sf_fixed );
	ssc_var( obj, h_tower, ui_tower_height );
	ssc_var( obj, H_rec, ui_receiver_height );
	ssc_var( obj, helio_height, ui_heliostat_height );
	ssc_var( obj, tower_fixed_cost, tower_fixed_cost );
	ssc_var( obj, tower_exp, tower_exp );
	ssc_var( obj, csp.pt.cost.receiver.area, csp.pt.cost.receiver.area );
	ssc_var( obj, rec_ref_cost, rec_ref_cost );
	ssc_var( obj, rec_ref_area, rec_ref_area );
	ssc_var( obj, rec_cost_exp, rec_cost_exp );
	ssc_var( obj, csp.pt.cost.storage_mwht, csp.pt.cost.storage_mwht );
	ssc_var( obj, tes_spec_cost, tes_spec_cost );
	ssc_var( obj, P_ref, csp.pt.cost.power_block_mwe );
	ssc_var( obj, csp.pt.cost.power_block_per_kwe, plant_spec_cost );
	ssc_var( obj, bop_spec_cost, bop_spec_cost );
	ssc_var( obj, fossil_spec_cost, fossil_spec_cost );
	ssc_var( obj, contingency_rate, contingency_rate );
	ssc_var( obj, csp.pt.cost.total_land_area, csp.pt.cost.total_land_area );
	ssc_var( obj, system_capacity, csp.pt.cost.nameplate );
	ssc_var( obj, csp.pt.cost.epc.per_acre, csp.pt.cost.epc.per_acre );
	ssc_var( obj, csp.pt.cost.epc.percent, csp.pt.cost.epc.percent );
	ssc_var( obj, csp.pt.cost.epc.per_watt, csp.pt.cost.epc.per_watt );
	ssc_var( obj, csp.pt.cost.epc.fixed, csp.pt.cost.epc.fixed );
	ssc_var( obj, csp.pt.cost.plm.per_acre, land_spec_cost );
	ssc_var( obj, csp.pt.cost.plm.percent, csp.pt.cost.plm.percent );
	ssc_var( obj, csp.pt.cost.plm.per_watt, csp.pt.cost.plm.per_watt );
	ssc_var( obj, csp.pt.cost.plm.fixed, csp.pt.cost.plm.fixed );
	ssc_var( obj, sales_tax_frac, sales_tax_frac );
	ssc_var( obj, sales_tax_rate, csp.pt.cost.sales_tax.value );
	std::string ret = ssc_exec( obj, cb_mspt_system_costs );
	csp.pt.cost.site_improvements = ssc_var( obj, csp.pt.cost.site_improvements );
	csp.pt.cost.heliostats = ssc_var( obj, csp.pt.cost.heliostats );
	csp.pt.cost.tower = ssc_var( obj, csp.pt.cost.tower );
	csp.pt.cost.receiver = ssc_var( obj, csp.pt.cost.receiver );
	csp.pt.cost.storage = ssc_var( obj, csp.pt.cost.storage );
	csp.pt.cost.power_block = ssc_var( obj, csp.pt.cost.power_block );
	csp.pt.cost.bop = ssc_var( obj, csp.pt.cost.bop );
	csp.pt.cost.fossil = ssc_var( obj, csp.pt.cost.fossil );
	ui_direct_subtotal = ssc_var( obj, ui_direct_subtotal );
	csp.pt.cost.contingency = ssc_var( obj, csp.pt.cost.contingency );
	total_direct_cost = ssc_var( obj, total_direct_cost );
	csp.pt.cost.epc.total = ssc_var( obj, csp.pt.cost.epc.total );
	csp.pt.cost.plm.total = ssc_var( obj, csp.pt.cost.plm.total );
	csp.pt.cost.sales_tax.total = ssc_var( obj, csp.pt.cost.sales_tax.total );
	total_indirect_cost = ssc_var( obj, total_indirect_cost );
	total_installed_cost = ssc_var( obj, total_installed_cost );
	csp.pt.cost.installed_per_capacity = ssc_var( obj, csp.pt.cost.installed_per_capacity );
	ssc_free( obj );


	var_table vt;
	vt.assign( "csp.pt.cost.site_improvements", csp.pt.cost.site_improvements );
	vt.assign( "csp.pt.cost.heliostats", csp.pt.cost.heliostats );
	vt.assign( "csp.pt.cost.tower", csp.pt.cost.tower );
	vt.assign( "csp.pt.cost.receiver", csp.pt.cost.receiver );
	vt.assign( "csp.pt.cost.storage", csp.pt.cost.storage );
	vt.assign( "csp.pt.cost.power_block", csp.pt.cost.power_block );
	vt.assign( "csp.pt.cost.bop", csp.pt.cost.bop );
	vt.assign( "csp.pt.cost.fossil", csp.pt.cost.fossil );
	vt.assign( "ui_direct_subtotal", ui_direct_subtotal );
	vt.assign( "csp.pt.cost.contingency", csp.pt.cost.contingency );
	vt.assign( "total_direct_cost", total_direct_cost );
	vt.assign( "csp.pt.cost.epc.total", csp.pt.cost.epc.total );
	vt.assign( "csp.pt.cost.plm.total", csp.pt.cost.plm.total );
	vt.assign( "csp.pt.cost.sales_tax.total", csp.pt.cost.sales_tax.total );
	vt.assign( "total_indirect_cost", total_indirect_cost );
	vt.assign( "total_installed_cost", total_installed_cost );
	vt.assign( "csp.pt.cost.installed_per_capacity", csp.pt.cost.installed_per_capacity );

}



