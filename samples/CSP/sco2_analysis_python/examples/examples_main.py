# -*- coding: utf-8 -*-
"""
Created on Fri Jun  9 10:56:12 2017

@author: tneises
"""
import json

import matplotlib.pyplot as plt

import numpy as np

import matplotlib.lines as mlines

import sys
import os

absFilePath = os.path.abspath(__file__)
fileDir = os.path.dirname(os.path.abspath(__file__))
parentDir = os.path.dirname(fileDir)
newPath = os.path.join(parentDir, 'core')

sys.path.append(newPath)

import sco2_cycle_ssc as sco2_solve

import sco2_plots as cy_plt

def get_sco2_design_parameters():

    des_par = {}

        # System design parameters
    des_par["htf"] = 17  # [-] Solar salt
    des_par["T_htf_hot_des"] = 670.0  # [C] HTF design hot temperature (PHX inlet)
    des_par["dT_PHX_hot_approach"] = 20.0  # [C/K] default 20. Temperature difference between hot HTF and turbine inlet
    des_par["T_amb_des"] = 35.0  # [C] Ambient temperature at design
    des_par["dT_mc_approach"] = 6.0  # [C] Use 6 here per Neises & Turchi 19. Temperature difference between main compressor CO2 inlet and ambient air
    des_par["site_elevation"] = 588  # [m] Elevation of Daggett, CA. Used to size air cooler...
    des_par["W_dot_net_des"] = 50.0  # [MWe] Design cycle power output (no cooling parasitics)

    # Cycle design options
        # Configuration
    des_par["cycle_config"] = 1  # [1] = RC, [2] = PC

        # Recuperator design
    des_par["design_method"] = 2  # [-] 1 = specify efficiency, 2 = specify total recup UA, 3 = Specify each recup design (see inputs below)
    des_par["eta_thermal_des"] = 0.44  # [-] Target power cycle thermal efficiency (used when design_method == 1)
    des_par["UA_recup_tot_des"] = 15 * 1000 * (des_par["W_dot_net_des"]) / 50.0  # [kW/K] (used when design_method == 2)

        # Pressures and recompression fraction
    des_par["is_recomp_ok"] = 1  	# 1 = Yes, 0 = simple cycle only, < 0 = fix f_recomp to abs(input)
    des_par["is_P_high_fixed"] = 1  # 0 = No, optimize. 1 = Yes (=P_high_limit)
    des_par["is_PR_fixed"] = 0      # 0 = No, >0 = fixed pressure ratio at input <0 = fixed LP at abs(input)
    des_par["is_IP_fixed"] = 0      # partial cooling config: 0 = No, >0 = fixed HP-IP pressure ratio at input, <0 = fixed IP at abs(input)
    
    # Convergence and optimization criteria
    des_par["des_objective"] = 1    # [-] 2 = hit min deltaT then max efficiency, != 2 = max efficiency
    des_par["min_phx_deltaT"] = 1000  # [C] Min allowable deltaT across PHX
    des_par["rel_tol"] = 3  # [-] Baseline solver and optimization relative tolerance exponent (10^-rel_tol)

    # Weiland & Thimsen 2016
    # In most studies, 85% is an accepted isentropic efficiency for either the main or recompression compressors, and is the recommended assumption.
    des_par["eta_isen_mc"] = 0.85  # [-] Main compressor isentropic efficiency
    des_par["eta_isen_rc"] = 0.85  # [-] Recompressor isentropic efficiency
    des_par["eta_isen_pc"] = 0.85  # [-] Precompressor isentropic efficiency

    # Weiland & Thimsen 2016
    # Recommended turbine efficiencies are 90% for axial turbines above 30 MW, and 85% for radial turbines below 30 MW.
    des_par["eta_isen_t"] = 0.90  # [-] Turbine isentropic efficiency

    des_par["P_high_limit"] = 25  # [MPa] Cycle high pressure limit

    # Weiland & Thimsen 2016
    # Multiple literature sources suggest that recuperator cold side (high pressure) pressure drop of
    # approximately 140 kPa (20 psid) and a hot side (low pressure) pressure drop of 280 kPa (40 psid) can be reasonably used.
    # Note: Unclear what the low pressure assumption is in this study, could be significantly lower for direct combustion cycles
    eff_max = 1.0
    deltaP_recup_HP = 0.0056  # [-] = 0.14[MPa]/25[MPa]
    deltaP_recup_LP = 0.0311  # [-] = 0.28[MPa]/9[MPa]
    # LTR
    des_par["LTR_design_code"] = 3        # 1 = UA, 2 = min dT, 3 = effectiveness
    des_par["LTR_UA_des_in"] = 2200.0     # [kW/K] (required if LTR_design_code == 1)
    des_par["LTR_min_dT_des_in"] = 12.0   # [C] (required if LTR_design_code == 2)
    des_par["LTR_eff_des_in"] = 0.895     # [-] (required if LTR_design_code == 3)
    des_par["LT_recup_eff_max"] = eff_max    # [-] Maximum effectiveness low temperature recuperator
    des_par["LTR_LP_deltaP_des_in"] = deltaP_recup_LP  # [-]
    des_par["LTR_HP_deltaP_des_in"] = deltaP_recup_HP  # [-]
    # HTR
    des_par["HTR_design_code"] = 3        # 1 = UA, 2 = min dT, 3 = effectiveness
    des_par["HTR_UA_des_in"] = 2800.0     # [kW/K] (required if LTR_design_code == 1)
    des_par["HTR_min_dT_des_in"] = 19.2   # [C] (required if LTR_design_code == 2)
    des_par["HTR_eff_des_in"] = 0.945      # [-] (required if LTR_design_code == 3)
    des_par["HT_recup_eff_max"] = eff_max  # [-] Maximum effectiveness high temperature recuperator
    des_par["HTR_LP_deltaP_des_in"] = deltaP_recup_LP  # [-]
    des_par["HTR_HP_deltaP_des_in"] = deltaP_recup_HP  # [-]
    # PHX
    des_par["PHX_co2_deltaP_des_in"] = deltaP_recup_HP  # [-]
    des_par["dT_PHX_cold_approach"] = 20  # [C/K] default 20. Temperature difference between cold HTF and cold CO2 PHX inlet
    # Air Cooler
    des_par["deltaP_cooler_frac"] = 0.005  # [-] Fraction of CO2 inlet pressure that is design point cooler CO2 pressure drop
    des_par["fan_power_frac"] = 0.02  # [-] Fraction of net cycle power consumed by air cooler fan. 2% here per Turchi et al.
    # Default
    des_par["deltaP_counterHX_frac"] = 0.0054321  # [-] Fraction of CO2 inlet pressure that is design point counterflow HX (recups & PHX) pressure drop

    return des_par

"Save dictionary of design parameters from above"
sco2_des_par_default = get_sco2_design_parameters() 

##########################################
"Cycle design simulation with default parameters"
c_sco2 = sco2_solve.C_sco2_sim(1)   # Initialize to the recompression cycle default (1)
c_sco2.overwrite_default_design_parameters(sco2_des_par_default)
c_sco2.solve_sco2_case()            # Run design simulation
print(c_sco2.m_solve_dict)
print("\nDid the simulation code solve successfully = ",c_sco2.m_solve_success)
c_sco2.m_also_save_csv = True
c_sco2.save_m_solve_dict("design_solution__default_pars")   # Save design solution dictionary
sol_dict__default_pars = c_sco2.m_solve_dict

##########################################
"Plotting a cycle design"
c_plot = cy_plt.C_sco2_TS_PH_plot(sol_dict__default_pars)
c_plot.is_save_plot = True
c_plot.file_name = "cycle_design_plots__default_pars"
c_plot.plot_new_figure()

##########################################
"Modifying the cycle design parameters to fully constrain the cycle design"
mod_base_dict = {"is_recomp_ok" : -0.35}   #[-] fix the recompression fraction to 0.35
mod_base_dict["is_PR_fixed"] = -10          #[MPa] fix the low pressure side to 9 MPa
mod_base_dict["design_method"] = 3         # fix recuperator performance
mod_base_dict["LTR_design_code"] = 3        # 1 = UA, 2 = min dT, 3 = effectiveness
mod_base_dict["LTR_eff_des_in"] = 0.845     # [-] (required if LTR_design_code == 3)
mod_base_dict["HTR_design_code"] = 3        # 1 = UA, 2 = min dT, 3 = effectiveness
mod_base_dict["HTR_eff_des_in"] = 0.895      # [-] (required if LTR_design_code == 3)

c_sco2.overwrite_des_par_base(mod_base_dict)    # Overwrite baseline design parameters
c_sco2.solve_sco2_case()            # Run design simulation
print(c_sco2.m_solve_dict)
print("\nDid the simulation code with" 
      "modified design parameters solve successfully = ",c_sco2.m_solve_success)
c_sco2.m_also_save_csv = True
c_sco2.save_m_solve_dict("design_solution__modified_pars")   # Save design solution dictionary
sol_dict__mod_pars = c_sco2.m_solve_dict

c_plot = cy_plt.C_sco2_TS_PH_plot(sol_dict__mod_pars)
c_plot.is_save_plot = True
c_plot.file_name = "cycle_design_plots__mod_pars"
c_plot.plot_new_figure()

##########################################
"Comparing two cycle designs"
c_comp_plot = cy_plt.C_sco2_TS_PH_overlay_plot(sol_dict__default_pars, sol_dict__mod_pars)
c_comp_plot.is_save_plot = True
c_comp_plot.plot_new_figure()

##########################################
"Running a parametric study on one design parameter"
c_sco2.overwrite_default_design_parameters(sco2_des_par_default)
UA_in_par_list = list(np.arange(5000,21000,1000))
UA_in_par_dict_list = []
for UA_in in UA_in_par_list:
    UA_in_par_dict_list.append({"UA_recup_tot_des": UA_in})
c_sco2.solve_sco2_parametric(UA_in_par_dict_list)
print("\nDid the parametric analyses solve successfully = ",c_sco2.m_par_solve_success)
c_sco2.m_also_save_csv = True
c_sco2.save_m_par_solve_dict("UA_recup_parametric")
sol_dict_parametric = c_sco2.m_par_solve_dict

##########################################
"Plotting a 1D parametric study"
par_plot = cy_plt.C_des_stacked_outputs_plot([sol_dict_parametric])
par_plot.x_var = "recup_tot_UA"
par_plot.y_vars = ["eta", "PHX_dT", "f_recomp","MC_P_in",
                      "recup_tot_UA_calculated", "LTR_UA_calculated", "HTR_UA_calculated","RC_P_in",
                      "LTR_UA", "LTR_eff", "LTR_min_dT","PC_P_in",
                      "HTR_UA", "HTR_eff", "HTR_min_dT","MC_P_out"]
par_plot.is_legend = False
par_plot.max_rows = 4
par_plot.is_save = True
par_plot.var_info_metrics["recup_tot_UA"].y_axis_min_max = [5,20]
par_plot.file_name = "UA_recup_par_plot"
par_plot.create_plot()

##########################################
"Plotting one cycle design from a parametric solution dictionary"
i_plot = len(sol_dict_parametric["UA_recup_tot_des"]) - 1
dict_i_plot = sco2_solve.get_one_des_dict_from_par_des_dict(sol_dict_parametric, "UA_recup_tot_des", i_plot)
c_i_cycle_plot = cy_plt.C_sco2_TS_PH_plot(dict_i_plot)
c_i_cycle_plot.is_save_plot = True
c_i_cycle_plot.file_name = "cycle_design_plots__largest_recup_tot_UA"
c_i_cycle_plot.plot_new_figure()

##########################################
##########################################
##########################################
##########################################
##########################################


     