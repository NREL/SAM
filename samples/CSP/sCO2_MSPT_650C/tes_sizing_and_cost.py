# -*- coding: utf-8 -*-
"""
Created on Mon Oct 15 16:46:29 2018

@author: tneises
"""

import json

import ssc_inout_v2 as ssc_sim

def get_empty_sam_mspt_so_solution_dict():

    sam_mspt_so_solution_dict = {}
    sam_mspt_so_solution_dict["sam_UA_recup_total"] = []
    sam_mspt_so_solution_dict["annual_energy_derated"] = []
    sam_mspt_so_solution_dict["lcoe_nom"] = []
    sam_mspt_so_solution_dict["TES_cost"] = []
    sam_mspt_so_solution_dict["sam_cycle_cost"] = []
    sam_mspt_so_solution_dict["mspt_tot_installed_cost"] = []
    sam_mspt_so_solution_dict["rec_eta_therm"] = []
    sam_mspt_so_solution_dict["annual_cycle_output"] = []
    sam_mspt_so_solution_dict["annual_rec_pump_power"] = []
    sam_mspt_so_solution_dict["rec_pump_perc"] = []
    sam_mspt_so_solution_dict["solar_mult"] = []
    sam_mspt_so_solution_dict["Q_TES"] = []
    sam_mspt_so_solution_dict["V_TES"] = []
    sam_mspt_so_solution_dict["sam_phx_deltat"] = []
    sam_mspt_so_solution_dict["sam_T_htf_cold"] = []
    sam_mspt_so_solution_dict["sam_eta_cycle"] = []
    sam_mspt_so_solution_dict["annual_rec_delivered"] = []
    sam_mspt_so_solution_dict["m_dot_rec_design"] = []
    sam_mspt_so_solution_dict["annual_rec_defocused"] = []
    
    return sam_mspt_so_solution_dict

def sam_mspt_so_sim(i_eta_pc, i_spec_cost_pc_USD_kWe, i_phx_deltat_pc_C, i_W_dot_pc_MWe, i_UA_recup_total_MW_K, dict_in):
        
    # Get copy of default SAM MSPT case
    sam_mspt_so_default_dict = json.load(open("MgCl2-NaCl-KCl_sCO2_650C.json", 'r'))
        
    sam_mspt_dict = sam_mspt_so_default_dict.copy()
    
    # Get some design parameters from SAM MSPT case
    T_htf_hot_mspt = sam_mspt_dict["T_htf_hot_des"]     #C
    #T_htf_cold_mspt = sam_mspt_dict["T_htf_cold_des"]   #C
    eta_mspt = sam_mspt_dict["design_eff"]              #-
    W_dot_pc_mspt = sam_mspt_dict["P_ref"]      #MWe
    Q_dot_pc_mspt = W_dot_pc_mspt / eta_mspt    #MWt
    SM_mspt = sam_mspt_dict["solarm"]           #
    Q_dot_rec_mspt = Q_dot_pc_mspt * SM_mspt    #MWt
    hrs_TES_mspt = sam_mspt_dict["tshours"]     #hr
    #Q_TES_mspt = hrs_TES_mspt * Q_dot_pc_mspt   #MWt - hr
    
        # Hardcoding HTF props for now
    cp_avg_cl = 1.14     #J/g-K
    rho_hot_cl = 1968;	 #[kg/m3]
    kJ_per_MWh = 3600000;	#[kJ/MWh]
        
    print(" eta = ", i_eta_pc, " cost = ", i_spec_cost_pc_USD_kWe, " dT = ", i_phx_deltat_pc_C, " W_dot_pc = ", i_W_dot_pc_MWe)

    # Compare cycle design power to SAM MSPT case power    
    if(abs(i_W_dot_pc_MWe - W_dot_pc_mspt) / W_dot_pc_mspt > 0.001):
        print("The cycle design power is ", i_W_dot_pc_MWe, " but the MSPT case power is ", W_dot_pc_mspt)
        
    # Calculate new SAM MSPT inputs based on power cycle design
    i_T_htf_cold = T_htf_hot_mspt - i_phx_deltat_pc_C      #C
    i_Q_dot_pc = W_dot_pc_mspt / i_eta_pc                  #MWt
    i_SM = Q_dot_rec_mspt / i_Q_dot_pc                     #-
    i_Q_TES = hrs_TES_mspt * i_Q_dot_pc                    #MWt-hr
    i_tes_cost = chloride_tes_cost_excel(i_phx_deltat_pc_C, i_Q_TES)  #[$/kWe]
    i_m_dot_htf_rec_des = i_Q_dot_pc * i_SM * 1000 / i_phx_deltat_pc_C / cp_avg_cl  #[kg/s]
    i_vol_TES = i_Q_TES * kJ_per_MWh / (cp_avg_cl*i_phx_deltat_pc_C) / rho_hot_cl      #[m3]
    
    # Set new SAM MSPT inputs
    sam_mspt_dict["design_eff"] = i_eta_pc   #[-]
    sam_mspt_dict["plant_spec_cost"] = i_spec_cost_pc_USD_kWe   #[$/kWe]
    sam_mspt_dict["T_htf_cold_des"] = i_T_htf_cold   #[C]
    sam_mspt_dict["solarm"] = i_SM     #[-]
    sam_mspt_dict["tes_spec_cost"] = i_tes_cost #[$/kWe]
    
    # Run MSPT simulation
    mspt_so_solved_dict = ssc_sim.cmod_mspt_from_dict(sam_mspt_dict)
    
    is_success = mspt_so_solved_dict["cmod_success"]    
    print ('SSC simulation(s) successful = ', is_success)
    if (is_success == 1):
        annual_energy = mspt_so_solved_dict["annual_energy"]
        print ('Annual energy (year 1) = ', annual_energy)
        flip_actual_irr = mspt_so_solved_dict["flip_actual_irr"]
        print ('Internal rate of return (IRR) = ', flip_actual_irr)
    else:
        print ('FAIL FAIL FAIL FAIL FAIL')
        
    print('\n')

    # Get SAM MSPT outputs
        # 8760 outputs
    i_q_dot_abs = mspt_so_solved_dict["q_dot_rec_inc"]  #[MWt] This already includes reflection losses
    i_q_dot_th_loss = mspt_so_solved_dict["q_thermal_loss"] #[MWt] Total radiative and convective losses
    i_q_dot_htf = mspt_so_solved_dict["Q_thermal"]      #[MWt] Heat delivered to the HTF
    i_W_dot_pc = mspt_so_solved_dict["P_out_net"]       #[MWe] Total electric power to grid
    i_W_dot_rec_pump = mspt_so_solved_dict["P_tower_pump"]  #[MWe] Tower/receiver combined pumping loss
    i_defocus = mspt_so_solved_dict["defocus"]          #[-]
    i_q_dot_defocus = i_q_dot_abs.copy()                #[MWt] Get 8760 array
    
    for ii in range(len(i_defocus)):
        i_q_dot_defocus[ii] = i_q_dot_abs[ii]*(1.0 - i_defocus[ii])
        
        # Get single value outputs
    i_sam_annual_energy = mspt_so_solved_dict["annual_energy"]  #[kWe-hr] Annual total electric power to grid
    i_lcoe_nom = mspt_so_solved_dict["lcoe_nom"]                #[cents/kWh] Nominal LCOE
    i_TES_cost_base = mspt_so_solved_dict["csp.pt.cost.storage"]*1.E-6   #[M$] TES cost
    i_cycle_cost = mspt_so_solved_dict["csp.pt.cost.power_block"]*1.E-6  #[M$] cycle cost
    i_tot_installed_cost = mspt_so_solved_dict["total_installed_cost"]*1.E-6  #[M$]
    
        # Post-process
    i_Q_th_loss_sum = sum(i_q_dot_th_loss)    #[MWt-h]
    i_Q_abs_sum = sum(i_q_dot_abs)            #[MWt-h]
    i_eta_th_annual = (i_Q_abs_sum - i_Q_th_loss_sum)/i_Q_abs_sum   #[-]
    i_Q_rec_delivered = sum(i_q_dot_htf)      #[MWt-h]
    
    i_W_pc_sum = sum(i_W_dot_pc)                      #[MWe-hr]
    i_W_rec_pump_sum = sum(i_W_dot_rec_pump)          #[MWe-hr]
    i_frac_rec_pump = i_W_rec_pump_sum / i_W_pc_sum   #[-]
    
    i_E_defocus = sum(i_q_dot_defocus)                #[MWt-hr]
    
    # Write to output dictionary
    dict_in["sam_UA_recup_total"].append(i_UA_recup_total_MW_K)    #[MWt/K]
    dict_in["annual_energy_derated"].append(i_sam_annual_energy)   #[kWe-hr]
    dict_in["lcoe_nom"].append(i_lcoe_nom)                         #[cent/kWh]
    dict_in["TES_cost"].append(i_TES_cost_base)                    #[M$]
    dict_in["sam_cycle_cost"].append(i_cycle_cost)                 #[M$]
    dict_in["mspt_tot_installed_cost"].append(i_tot_installed_cost)  #[M$]
    dict_in["annual_rec_pump_power"].append(i_W_rec_pump_sum*1.E3)   #[kWe-hr]
    dict_in["rec_eta_therm"].append(i_eta_th_annual)               #[-]
    dict_in["annual_cycle_output"].append(i_W_pc_sum*1.E3)         #[kWe-hr]
    dict_in["rec_pump_perc"].append(i_frac_rec_pump*100)           #[%]
    dict_in["solar_mult"].append(i_SM)                             #[-]
    dict_in["Q_TES"].append(i_Q_TES)                               #[MWt-hr]
    dict_in["V_TES"].append(i_vol_TES)       #[m3]
    dict_in["sam_phx_deltat"].append(i_phx_deltat_pc_C)            #[C]
    dict_in["sam_T_htf_cold"].append(i_T_htf_cold)                 #[C]
    dict_in["sam_eta_cycle"].append(i_eta_pc)                      #[-]
    dict_in["annual_rec_delivered"].append(i_Q_rec_delivered*1.E3) #[kWt-hr]
    dict_in["m_dot_rec_design"].append(i_m_dot_htf_rec_des)        #[kg/s]
    dict_in["annual_rec_defocused"].append(i_E_defocus*1.E3)       #[kWt-hr]

    
    return dict_in

def chloride_tes_cost_excel(deltaT_cl_K, Q_TES_cl_MWthr):
    
    deltaT_cl = deltaT_cl_K
    Q_TES_cl = Q_TES_cl_MWthr
    
    Q_TES_base3 = 8110;		#[MWt-hr]
    salt_cost_base3 = 1100;	#[$/tonne]
    cold_tank_cost_base3 = 10389;	#[$]
    hot_tank_cost_base3 = 23863;		#[$]
    salt_cost_base3 = 90365;			#[$]
    inst_cost_base3 = 504;			#[$]
    steel_cost_base3 = 1587;			#[$]
    insulation_cost_base3 = 8871;	#[$]
    elec_cost_base3 = 1146;			#[$]
    concrete_cost_base3 = 3716;		#[$]
    foamglass_cost_base3 = 2284;		#[$]
    refrac_cost_base3 = 1265;		#[$]
    sitework_cost_base3 = 807;		#[$]
    paint_cost_base3 = 18;			#[$]
    tes_direct_cost_base3_tot = (cold_tank_cost_base3
		+ hot_tank_cost_base3 + salt_cost_base3 + inst_cost_base3 + steel_cost_base3
		+ insulation_cost_base3 + elec_cost_base3 + concrete_cost_base3
		+ foamglass_cost_base3 + refrac_cost_base3 + sitework_cost_base3
		+ paint_cost_base3);
    #outln(tes_direct_cost_base3_tot);
    tes_direct_cost_base3 = tes_direct_cost_base3_tot / Q_TES_base3;
    #outln("3 tank base direct cost = ", tes_direct_cost_base3);
    
    sc_ind = 1.0108477;			#[-] CEPCI cost index
    Q_TES_base1 = Q_TES_base3 / 3;		#[MWt-hr]
    salt_cost_base1 = salt_cost_base3;	#[$/tonne]
    cold_tank_cost_base1 = cold_tank_cost_base3*(1/3.0)**0.8*sc_ind;
    hot_tank_cost_base1 = hot_tank_cost_base3*(1/3.0)**0.8*sc_ind;
    salt_cost_base1 = salt_cost_base3*(1/3.0);
    inst_cost_base1 = inst_cost_base3*(1/3.0)**0.8*sc_ind;
    steel_cost_base1 = steel_cost_base3*(1/3.0)**0.8*sc_ind;
    insulation_cost_base1 = insulation_cost_base3*(1/3.0)**0.8*sc_ind;
    elec_cost_base1 = elec_cost_base3*(1/3.0)**0.8*sc_ind;
    concrete_cost_base1 = concrete_cost_base3*(1/3.0)**0.8*sc_ind;
    foamglass_cost_base1 = foamglass_cost_base3*(1/3.0)**0.8*sc_ind;
    refrac_cost_base1 = refrac_cost_base3*(1/3.0)**0.8*sc_ind;
    sitework_cost_base1 = sitework_cost_base3*(1/3.0)**0.8*sc_ind;
    paint_cost_base1 = paint_cost_base3*(1/3.0)**0.8*sc_ind;
    tes_direct_cost_base1_tot = (cold_tank_cost_base1
		+ hot_tank_cost_base1 + salt_cost_base1 + inst_cost_base1 + steel_cost_base1
		+ insulation_cost_base1 + elec_cost_base1 + concrete_cost_base1
		+ foamglass_cost_base1 + refrac_cost_base1 + sitework_cost_base1
		+ paint_cost_base1);
	#outln(tes_direct_cost_base1_tot);
    tes_direct_cost_base1 = tes_direct_cost_base1_tot / Q_TES_base1;
	#outln("1 v1 tank base direct cost = ", tes_direct_cost_base1);	

	# Tank Design Constants
    tank_heal = 0.05;		#[-]
    tank_freeboard = 0.05;	#[-]
    kJ_per_MWh = 3600000;	#[kJ/MWh]
	# *******************************

	# Solar Salt
    rho_hot_ss = 1730;		#[kg/m3]
    cp_avg_ss = 1.52;		#[J/g-K]
    deltaT_ss = 277;		#[K]
    Q_TES_base = 2790;		#[MWt-h]

    Q_TES_base_kJ = Q_TES_base * kJ_per_MWh;	#[kJ]
    mass_ss = Q_TES_base_kJ / (cp_avg_ss*deltaT_ss) / (1 - tank_heal);	#[kg]
    tank_vol_ss = mass_ss / rho_hot_ss / (1-tank_freeboard);	#[m3]
	#outln("Solar Salt mass = ", mass_ss);
	#outln("Solar Salt hot tank volume = ", tank_vol_ss);

    tes_direct_cost_base = tes_direct_cost_base1_tot / Q_TES_base;	#[$]
	#outln("1 tank base direct cost = ", tes_direct_cost_base);

	# MgCl2 - NaCl - KCl
    rho_hot_cl = 1968;		#[kg/m3]
    cp_avg_cl = 1.14;		#[J/g-K]
	#deltaT_cl = 180;		#[K]
	#Q_TES_cl = 2790;		#[MWt-h]
    Q_TES_cl_kJ = Q_TES_cl * kJ_per_MWh;		#[kJ]
    mass_cl = Q_TES_cl_kJ / (cp_avg_cl*deltaT_cl) / (1 - tank_heal);	#[kg]
    tank_vol_cl = mass_cl / rho_hot_cl / (1-tank_freeboard);	#[m3]
	#outln("Chloride salt mass = ", mass_cl);
	#outln("Chloride hot tank volume = ", tank_vol_cl);
    rel_tank_size = tank_vol_cl / tank_vol_ss;
	#outln("Chloride hot tank vol over ss hot tank vol = ", rel_tank_size);

    cl_cost = 350;			#[$/tonne]
    cold_tank_cost_cl = cold_tank_cost_base1*rel_tank_size**0.8*2.2969;
    hot_tank_cost_cl = hot_tank_cost_base1*rel_tank_size**0.8*2.4359;
    salt_cost_cl = mass_cl/1000 * cl_cost/1000;	#[$]
    steel_cost_cl = steel_cost_base1*rel_tank_size**0.8;
    insulation_cost_cl = insulation_cost_base1*rel_tank_size**0.8;
    elec_cost_cl = (elec_cost_base1 + inst_cost_base1)*rel_tank_size**0.8;
    foundation_cost_cl = (concrete_cost_base1 + foamglass_cost_base1 + refrac_cost_base1)*rel_tank_size**0.8;
    sitework_cost_cl = (sitework_cost_base1 + paint_cost_base1)*rel_tank_size**0.8;
    tes_direct_cost_cl_tot = (cold_tank_cost_cl + hot_tank_cost_cl + salt_cost_cl +
		steel_cost_cl + insulation_cost_cl + elec_cost_cl + foundation_cost_cl +
		sitework_cost_cl);
    tes_direct_cost_cl = tes_direct_cost_cl_tot / Q_TES_cl;
	#outln("Total Chloride TES cost = ", tes_direct_cost_cl);
	
    return tes_direct_cost_cl;

### Test
#deltaT_cl1 = 180;		#[K] (default 180)
#Q_TES_cl1 = 4000;		#[MWt-h] (default 2790)
#tes_cl_cost1 = chloride_tes_cost_excel(deltaT_cl1, Q_TES_cl1);
#print(tes_cl_cost1)
