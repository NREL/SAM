# -*- coding: utf-8 -*-
"""
Created on Fri Jun  9 10:56:12 2017

@author: tneises
"""
import csv
import itertools
import numpy as np
import sys
import os
import math
import time
import datetime
import json
import design_point_tools as design_point_tools
import multiprocessing
from functools import partial
import queue

parentDir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.append(parentDir)

newPath = os.path.join(parentDir, 'core')

sys.path.append(newPath)

import sco2_cycle_ssc as sco2_solve
import sco2_plots as cy_plt

folder_location = "C:\\Users\\tbrown2\\Desktop\\sco2_python\\Alfani2020_Final\\"
Nproc = 10

# Utilities

def get_result_list_v2(input_dict, result_dict, success_flag, label_flag = False):

    # Result Key List
    # First term, -1 = blank line, 0 = input val, 1 = result val
    # Second term, name of variable
    result_key_list = []

    result_key_list.append([1, "cycle_config"])         # Cycle Config
    result_key_list.append([0, "T_bypass_target"])     # Target BP Outlet
    result_key_list.append([3, "T_htf_bp_out_des"])     # BP HTF Outlet
    result_key_list.append([1, "T_htf_cold_des"])       # HTF Final Temp
    result_key_list.append([1, "T_htf_hot_des"])        # HTF Inlet Temp
    result_key_list.append([1, "T_htf_phx_out_des"])    # PHX HTF Outlet
    result_key_list.append([1, "dT_htf_des"])           # Overall Temperature Difference
    

    result_key_list.append([1, "m_dot_htf_des"])

    result_key_list.append([-1])

    result_key_list.append([1, "eta_thermal_calc"])
    result_key_list.append([-1])
    result_key_list.append([-1])

    result_key_list.append([-1])


    result_key_list.append([3, "bypass_frac"])
    result_key_list.append([1, "recomp_frac"])
    result_key_list.append([1, "recup_total_UA_assigned"])
    result_key_list.append([1, "recup_total_UA_calculated"])
    result_key_list.append([1, "recup_LTR_UA_frac"])
    result_key_list.append([1, "LTR_UA_assigned"])
    result_key_list.append([1, "LTR_UA_calculated"])
    result_key_list.append([1, "HTR_UA_assigned"])
    result_key_list.append([1, "HTR_UA_calculated"])
    result_key_list.append([1, "P_comp_in"])
    result_key_list.append([1, "P_comp_out"])

    result_key_list.append([-1])

    result_key_list.append([0, "W_dot_net_des"])
    result_key_list.append([1, "t_W_dot"])
    result_key_list.append([1, "mc_W_dot"])
    result_key_list.append([1, "rc_W_dot"])
    result_key_list.append([1, "mc_cooler_W_dot_fan"])
    result_key_list.append([1, "q_dot_in_total"])
    result_key_list.append([1, "q_dot_PHX"])
    result_key_list.append([3, "q_dot_BPX"])
    result_key_list.append([1, "mc_cooler_q_dot"])
    result_key_list.append([1, "q_dot_LTR"])
    result_key_list.append([-1])
    result_key_list.append([1, "q_dot_HTR"])
    result_key_list.append([-1])

    result_key_list.append([-1])

    result_key_list.append([1, "t_m_dot_des"])
    result_key_list.append([1, "mc_m_dot_des"])
    result_key_list.append([1, "rc_m_dot_des"])
    result_key_list.append([3, "BPX_m_dot"])
    result_key_list.append([3, "HTR_HP_m_dot"])

    result_key_list.append([-1])

    result_key_list.append([1, "UA_PHX"])
    result_key_list.append([1, "PHX_min_dT"])
    result_key_list.append([1, "eff_PHX"])
    result_key_list.append([3, "UA_BPX"])
    result_key_list.append([3, "BPX_min_dT"])
    result_key_list.append([3, "eff_BPX"])
    result_key_list.append([1, "LTR_UA_calculated"])
    result_key_list.append([1, "LTR_min_dT"])
    result_key_list.append([1, "eff_LTR"])
    result_key_list.append([1, "HTR_UA_calculated"])
    result_key_list.append([1, "HTR_min_dT"])
    result_key_list.append([1, "eff_HTR"])
    result_key_list.append([-1])

    # State Points
    result_key_list.append([2, "T_state_points"])
    result_key_list.append([-1])
    result_key_list.append([2, "P_state_points"])
    result_key_list.append([-1])
    result_key_list.append([2, "h_state_points"])
    result_key_list.append([-1])
    result_key_list.append([2, "s_state_points"])
    result_key_list.append([-1])

    # Costs
    result_key_list.append([1, "cycle_cost"])
    result_key_list.append([1, "piping_inventory_etc_cost"])
    result_key_list.append([1, "cycle_spec_cost"])
    result_key_list.append([1, "cycle_spec_cost_thermal"])
    result_key_list.append([1, "t_cost_equipment"])
    result_key_list.append([1, "t_cost_bare_erected"])
    result_key_list.append([1, "c_tot_cost_equip"])
    result_key_list.append([1, "mc_cost_equipment"])
    result_key_list.append([1, "mc_cost_bare_erected"])
    result_key_list.append([1, "rc_cost_equipment"])
    result_key_list.append([1, "rc_cost_bare_erected"])
    result_key_list.append([1, "pc_cost_equipment"])
    result_key_list.append([1, "pc_cost_bare_erected"])
    result_key_list.append([1, "recup_total_cost_equipment"])
    result_key_list.append([1, "recup_total_cost_bare_erected"])
    result_key_list.append([1, "LTR_cost_equipment"])
    result_key_list.append([1, "LTR_cost_bare_erected"])
    result_key_list.append([1, "HTR_cost_equipment"])
    result_key_list.append([1, "HTR_cost_bare_erected"])
    result_key_list.append([1, "PHX_cost_equipment"])
    result_key_list.append([1, "PHX_cost_bare_erected"])
    result_key_list.append([3, "BPX_cost_equipment"])
    result_key_list.append([3, "BPX_cost_bare_erected"])
    result_key_list.append([1, "mc_cooler_cost_equipment"])
    result_key_list.append([1, "mc_cooler_cost_bare_erected"])

    # Compile Result List
    result_list = []
    label_list = ["success"]
    numStatePts = 14

    # Make Label List
    if(label_flag == True):

        for key in result_key_list:
            if key[0] == -1:
                label_list.append("")

            elif key[0] == 0:
                label_list.append(key[1])


            elif key[0] == 1 or key[0] == 3:
                label_list.append(key[1])

            elif key[0] == 2:
                for i in range(numStatePts):
                    label_list.append(key[1] + ' ' + str(i))


        return label_list



    # Make Result List
    success_int = 0
    if(success_flag):
        success_int = 1
    result_list.append(str(success_int))
    for key in result_key_list:
        cycle_config = input_dict["cycle_config"]

        if(success_flag == True):
            if key[0] == -1:
                result_list.append("")

            # Input Parameters
            elif key[0] == 0:
                result_list.append(str(input_dict[key[1]]))

            # Result Parameters
            elif key[0] == 1:
                result_list.append(str(result_dict[key[1]]))

            # Result Lists
            elif key[0] == 2:
                pts = result_dict[key[1]]
                for val in pts:
                    result_list.append(str(val))

            # Bypass specific parameters
            elif key[0] == 3:
                if(cycle_config != 3):
                    result_list.append(str(0))
                else:
                    result_list.append(str(result_dict[key[1]]))

        elif(success_flag == False):
            if key[0] == -1:
                result_list.append("")
            elif key[0] == 2:
                for i in range(numStatePts):
                    result_list.append("0")
            else:
                result_list.append("0")

    
    return result_list

def get_label_list():
    return get_result_list_v2([], [], True, True)

def make_dict_par_list(bp_list = [], recomp_list = [], ltr_ua_frac_list = [], max_pressure_list = [], pres_ratio_list = [], UA_total_list = [], HTF_targ_list = [],
                       min_phx_deltaT_list = [], split_frac_list = []):

    if(len(ltr_ua_frac_list) > 0 and len(UA_total_list) == 0):
        return

    # Create List of non empty input lists
    input_list = [bp_list, recomp_list, ltr_ua_frac_list, max_pressure_list, pres_ratio_list, HTF_targ_list, min_phx_deltaT_list, split_frac_list]

    # UA total is ALWAYS last
    input_list.append(UA_total_list)


    input_non_empty_list = []
    for input in input_list:
        if(len(input) > 0):
            input_non_empty_list.append(input)

    
    # Form list of variable combinations
    combo_list = list(itertools.product(*input_non_empty_list))

    # Create des par string for each combo
    dict_list = []
    for combo in combo_list:

        combo_index = 0

        local_dict = {}

        # Get UA Total
        UA_total_local = 0
        if(len(UA_total_list) > 0):
            UA_total_local = combo[len(combo) - 1]
            local_dict["UA_recup_tot_des"] = UA_total_local

        # BP fraction
        if(len(bp_list) > 0):
            bp_frac = combo[combo_index]
            combo_index += 1

            local_dict["is_bypass_ok"] = -1.0 * bp_frac

        # Recomp fraction
        if(len(recomp_list) > 0):
            recomp_frac = combo[combo_index]
            combo_index += 1

            local_dict["is_recomp_ok"] = -1.0 * recomp_frac

        # UA fraction
        if(len(ltr_ua_frac_list) > 0):
            ltr_ua_frac = combo[combo_index]
            combo_index += 1

            ltr_ua = ltr_ua_frac * UA_total_local
            htr_ua = UA_total_local - ltr_ua

            local_dict["LTR_design_code"] = 1
            local_dict["HTR_design_code"] = 1
            local_dict["HTR_UA_des_in"] = htr_ua
            local_dict["LTR_UA_des_in"] = ltr_ua
            local_dict["design_method"] = 3

        # Max Pressure
        if(len(max_pressure_list) > 0):
            max_pressure = combo[combo_index]
            combo_index += 1

            local_dict["is_P_high_fixed"] = 1
            local_dict["P_high_limit"] = max_pressure

        # Pressure Ratio
        if(len(pres_ratio_list) > 0):
            low_pressure = combo[combo_index]
            combo_index += 1

            local_dict["is_PR_fixed"] = -1.0 * low_pressure

        # HTF Target Outlet Temperature
        if(len(HTF_targ_list) > 0):
            target_temp = combo[combo_index]
            combo_index += 1

            local_dict["T_bypass_target"] = target_temp

        if(len(min_phx_deltaT_list) > 0):
            min_dT = combo[combo_index]
            combo_index += 1

            local_dict["des_objective"] = 2
            local_dict["min_phx_deltaT"] = min_dT

        if(len(split_frac_list) > 0):
            split_frac = combo[combo_index]
            combo_index += 1

            local_dict["is_turbine_split_ok"] = -1.0 * split_frac

        # Add dictionary
        dict_list.append(local_dict)

    return dict_list
    
def make_dict_list_free(input_dict):
    # Create list of data form input_dict
    data_list = []
    index_dict = {}
    count = 0
    for key in input_dict:
        data_list.append(input_dict[key])
        index_dict[count] = key
        count += 1

    # Form list of variable combinations
    combo_list = list(itertools.product(*data_list))

    dict_list = []
    for combo in combo_list:

        local_dict = {}
        index = 0
        for val in combo:
            key_local = index_dict[index]
            local_dict[key_local] = val
            index += 1

        # Add dictionary
        dict_list.append(local_dict)

    return dict_list

    




def make_progress_bar(percent):

    progress_size = 10
    
    num_dashes = math.floor(percent * float(progress_size))
    num_space = progress_size - num_dashes

    progress_bar = "["

    for dash in range(num_dashes):
        progress_bar += "="

    for space in range(num_space):
        progress_bar += " "

    progress_bar += "] " + str(int(round(percent * 100.0, 0))) + "%"

    return progress_bar


# Input parameters

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
    des_par["cycle_config"] = 3  # [1] = RC, [2] = PC

        # Recuperator design
    des_par["design_method"] = 3  # [-] 1 = specify efficiency, 2 = specify total recup UA, 3 = Specify each recup design (see inputs below)
    des_par["eta_thermal_des"] = 0.44  # [-] Target power cycle thermal efficiency (used when design_method == 1)
    des_par["UA_recup_tot_des"] = 15 * 1000 * (des_par["W_dot_net_des"]) / 50.0  # [kW/K] (used when design_method == 2)

        # Pressures and recompression fraction
    des_par["is_recomp_ok"] = -0.8 	# 1 = Yes, 0 = simple cycle only, < 0 = fix f_recomp to abs(input)
    des_par["is_P_high_fixed"] = 1  # 0 = No, optimize. 1 = Yes (=P_high_limit)
    des_par["is_PR_fixed"] = -9.2049      # 0 = No, >0 = fixed pressure ratio at input <0 = fixed LP at abs(input)
    des_par["is_IP_fixed"] = 0      # partial cooling config: 0 = No, >0 = fixed HP-IP pressure ratio at input, <0 = fixed IP at abs(input)
    
    # Convergence and optimization criteria
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
    des_par["LTR_UA_des_in"] = 7500     # [kW/K] (required if LTR_design_code == 1)
    des_par["LTR_min_dT_des_in"] = 12.0   # [C] (required if LTR_design_code == 2)
    des_par["LTR_eff_des_in"] = 0.895     # [-] (required if LTR_design_code == 3)
    des_par["LT_recup_eff_max"] = eff_max    # [-] Maximum effectiveness low temperature recuperator
    des_par["LTR_LP_deltaP_des_in"] = deltaP_recup_LP  # [-]
    des_par["LTR_HP_deltaP_des_in"] = deltaP_recup_HP  # [-]
    # HTR
    des_par["HTR_design_code"] = 3        # 1 = UA, 2 = min dT, 3 = effectiveness
    des_par["HTR_UA_des_in"] = 7500     # [kW/K] (required if LTR_design_code == 1)
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

    # Extra Bypass Configuration Parameters
    des_par["T_bypass_target"] = 752.2099231 - 273.15  # [C] HTF design Bypass Outlet
    des_par["deltaT_bypass"] = 0 # [C] sco2 Bypass Outlet Temp - HTR_HP_OUT Temp
    #des_par["dT_PHX_cold_approach"] = des_par["dT_PHX_hot_approach"] # [C/K] default hot approach. Temperature difference between HTF PHX outlet and MIXER2 sco2


    return des_par

def get_sco2_design_parameters_SCO2_FLEX():

    des_par = {}

    # From Report D1.1 (given)
    des_par["W_dot_net_des"] = 25.0  # [MWe] Design cycle power output (no cooling parasitics)
    des_par["T_htf_hot_des"] = 640.0  # [C] HTF design hot temperature (PHX inlet)
    des_par["dT_PHX_hot_approach"] = 20  # [C/K] default 20. Temperature difference between hot HTF and turbine inlet
    des_par["P_high_limit"] = 25  # [MPa] Cycle high pressure limit
    des_par["eta_isen_mc"] = 0.80  # [-] Main compressor isentropic efficiency
    des_par["eta_isen_rc"] = 0.80  # [-] Recompressor isentropic efficiency
    des_par["eta_isen_t"] = 0.90  # [-] Turbine isentropic efficiency
    deltaP_recup_HP = 0.005  # [-] = 0.14[MPa]/25[MPa]
    deltaP_recup_LP = 0.005  # [-] = 0.28[MPa]/9[MPa]
    des_par["deltaP_cooler_frac"] = 0.005  # [-] Fraction of CO2 inlet pressure that is design point cooler CO2 pressure drop
    des_par["PHX_co2_deltaP_des_in"] = deltaP_recup_HP  # [-]

    # Derived from Report D1.1 Results
    des_par["T_amb_des"] = 27.0  # [C] Ambient temperature at design
    des_par["is_recomp_ok"] = -0.3569 	# 1 = Yes, 0 = simple cycle only, < 0 = fix f_recomp to abs(input)  
    des_par["is_bypass_ok"] = -0.11     # 1 = Yes, 0 = no bypass, < 0 = fix bp_frac to abs(input)  
    des_par["is_P_high_fixed"] = 1  # 0 = No, optimize. 1 = Yes (=P_high_limit)
    des_par["is_PR_fixed"] = -7.979      # 0 = No, >0 = fixed pressure ratio at input <0 = fixed LP at abs(input)
    des_par["dT_PHX_cold_approach"] = 110.72  # [C/K] default 20. Temperature difference between cold HTF and cold CO2 PHX inlet
    des_par["T_bypass_target"] = 569.466  # [C] HTF design Bypass Outlet

        
        # LTR
    des_par["LTR_design_code"] = 3        # 1 = UA, 2 = min dT, 3 = effectiveness
    des_par["LTR_eff_des_in"] = 0.924907     # [-] (required if LTR_design_code == 3)

        # HTR
    des_par["HTR_design_code"] = 3        # 1 = UA, 2 = min dT, 3 = effectiveness
    des_par["HTR_eff_des_in"] = 0.965181      # [-] (required if LTR_design_code == 3)
    

        # System design parameters
    des_par["htf"] = 17  # [-] Solar salt
    
    
    
    des_par["dT_mc_approach"] = 6.0  # [C] Use 6 here per Neises & Turchi 19. Temperature difference between main compressor CO2 inlet and ambient air
    des_par["site_elevation"] = 588  # [m] Elevation of Daggett, CA. Used to size air cooler...
    

    # Cycle design options
        # Configuration
    des_par["cycle_config"] = 3  # [1] = RC, [2] = PC

        # Recuperator design
    des_par["design_method"] = 3  # [-] 1 = specify efficiency, 2 = specify total recup UA, 3 = Specify each recup design (see inputs below)
    des_par["eta_thermal_des"] = 0.44  # [-] Target power cycle thermal efficiency (used when design_method == 1)
    des_par["UA_recup_tot_des"] = 15 * 1000 * (des_par["W_dot_net_des"]) / 50.0  # [kW/K] (used when design_method == 2)

        # Pressures and recompression fraction
    des_par["is_IP_fixed"] = 0      # partial cooling config: 0 = No, >0 = fixed HP-IP pressure ratio at input, <0 = fixed IP at abs(input)
    
    # Convergence and optimization criteria
    des_par["rel_tol"] = 4  # [-] Baseline solver and optimization relative tolerance exponent (10^-rel_tol)

    # Weiland & Thimsen 2016
    # In most studies, 85% is an accepted isentropic efficiency for either the main or recompression compressors, and is the recommended assumption.
    
    des_par["eta_isen_pc"] = 0.85  # [-] Precompressor isentropic efficiency

    # Weiland & Thimsen 2016
    # Recommended turbine efficiencies are 90% for axial turbines above 30 MW, and 85% for radial turbines below 30 MW.
    

    

    # Weiland & Thimsen 2016
    # Multiple literature sources suggest that recuperator cold side (high pressure) pressure drop of
    # approximately 140 kPa (20 psid) and a hot side (low pressure) pressure drop of 280 kPa (40 psid) can be reasonably used.
    # Note: Unclear what the low pressure assumption is in this study, could be significantly lower for direct combustion cycles
    eff_max = 1.0
    
    
    des_par["LTR_UA_des_in"] = 669.637     # [kW/K] (required if LTR_design_code == 1)
    des_par["LTR_min_dT_des_in"] = 12.0   # [C] (required if LTR_design_code == 2)
    
    des_par["LT_recup_eff_max"] = eff_max    # [-] Maximum effectiveness low temperature recuperator
    des_par["LTR_LP_deltaP_des_in"] = deltaP_recup_LP  # [-]
    des_par["LTR_HP_deltaP_des_in"] = deltaP_recup_HP  # [-]
    
    
    des_par["HTR_UA_des_in"] = 649.06     # [kW/K] (required if LTR_design_code == 1)
    des_par["HTR_min_dT_des_in"] = 19.2   # [C] (required if LTR_design_code == 2)
    
    des_par["HT_recup_eff_max"] = eff_max  # [-] Maximum effectiveness high temperature recuperator
    des_par["HTR_LP_deltaP_des_in"] = deltaP_recup_LP  # [-]
    des_par["HTR_HP_deltaP_des_in"] = deltaP_recup_HP  # [-]
    # PHX
    
    
    # Air Cooler
    
    des_par["fan_power_frac"] = 0.02  # [-] Fraction of net cycle power consumed by air cooler fan. 2% here per Turchi et al.
    # Default
    des_par["deltaP_counterHX_frac"] = 0.0054321  # [-] Fraction of CO2 inlet pressure that is design point counterflow HX (recups & PHX) pressure drop

    # Extra Bypass Configuration Parameters
    
    des_par["deltaT_bypass"] = 0 # [C] sco2 Bypass Outlet Temp - HTR_HP_OUT Temp
    #des_par["dT_PHX_cold_approach"] = des_par["dT_PHX_hot_approach"] # [C/K] default hot approach. Temperature difference between HTF PHX outlet and MIXER2 sco2


    return des_par

def get_sco2_design_parameters_SCO2_FLEX_update():

    des_par = {}

    # From Report D1.1 (given)
    des_par["W_dot_net_des"] = 25.0  # [MWe] Design cycle power output (no cooling parasitics)
    des_par["T_htf_hot_des"] = 640.0  # [C] HTF design hot temperature (PHX inlet)
    des_par["dT_PHX_hot_approach"] = 20  # [C/K] default 20. Temperature difference between hot HTF and turbine inlet
    des_par["P_high_limit"] = 25  # [MPa] Cycle high pressure limit
    des_par["eta_isen_mc"] = 0.80  # [-] Main compressor isentropic efficiency
    des_par["eta_isen_rc"] = 0.80  # [-] Recompressor isentropic efficiency
    des_par["eta_isen_t"] = 0.90  # [-] Turbine isentropic efficiency
    deltaP_recup_HP = 0.005  # [-] = 0.14[MPa]/25[MPa]
    deltaP_recup_LP = 0.005  # [-] = 0.28[MPa]/9[MPa]
    des_par["deltaP_cooler_frac"] = 0.005  # [-] Fraction of CO2 inlet pressure that is design point cooler CO2 pressure drop
    des_par["PHX_co2_deltaP_des_in"] = 0.008081  # [-]

    # Derived from Report D1.1 Results
    des_par["T_amb_des"] = 27.0  # [C] Ambient temperature at design
    des_par["is_recomp_ok"] = -0.3569 	# 1 = Yes, 0 = simple cycle only, < 0 = fix f_recomp to abs(input)  
    des_par["is_bypass_ok"] = -0.11     # 1 = Yes, 0 = no bypass, < 0 = fix bp_frac to abs(input)  
    des_par["is_P_high_fixed"] = 1  # 0 = No, optimize. 1 = Yes (=P_high_limit)
    des_par["is_PR_fixed"] = -7.979      # 0 = No, >0 = fixed pressure ratio at input <0 = fixed LP at abs(input)
    des_par["dT_PHX_cold_approach"] = 110.72  # [C/K] default 20. Temperature difference between cold HTF and cold CO2 PHX inlet
    des_par["T_bypass_target"] = 569.466  # [C] HTF design Bypass Outlet

        
        # LTR
    des_par["LTR_design_code"] = 3        # 1 = UA, 2 = min dT, 3 = effectiveness
    des_par["LTR_eff_des_in"] = 0.924907     # [-] (required if LTR_design_code == 3)

        # HTR
    des_par["HTR_design_code"] = 3        # 1 = UA, 2 = min dT, 3 = effectiveness
    des_par["HTR_eff_des_in"] = 0.965181      # [-] (required if LTR_design_code == 3)
    

        # System design parameters
    des_par["htf"] = 17  # [-] Solar salt
    
    
    
    des_par["dT_mc_approach"] = 6.0  # [C] Use 6 here per Neises & Turchi 19. Temperature difference between main compressor CO2 inlet and ambient air
    des_par["site_elevation"] = 588  # [m] Elevation of Daggett, CA. Used to size air cooler...
    

    # Cycle design options
        # Configuration
    des_par["cycle_config"] = 3  # [1] = RC, [2] = PC

        # Recuperator design
    des_par["design_method"] = 3  # [-] 1 = specify efficiency, 2 = specify total recup UA, 3 = Specify each recup design (see inputs below)
    des_par["eta_thermal_des"] = 0.44  # [-] Target power cycle thermal efficiency (used when design_method == 1)
    des_par["UA_recup_tot_des"] = 15 * 1000 * (des_par["W_dot_net_des"]) / 50.0  # [kW/K] (used when design_method == 2)

        # Pressures and recompression fraction
    des_par["is_IP_fixed"] = 0      # partial cooling config: 0 = No, >0 = fixed HP-IP pressure ratio at input, <0 = fixed IP at abs(input)
    
    # Convergence and optimization criteria
    des_par["rel_tol"] = 6  # [-] Baseline solver and optimization relative tolerance exponent (10^-rel_tol)

    # Weiland & Thimsen 2016
    # In most studies, 85% is an accepted isentropic efficiency for either the main or recompression compressors, and is the recommended assumption.
    
    des_par["eta_isen_pc"] = 0.85  # [-] Precompressor isentropic efficiency

    # Weiland & Thimsen 2016
    # Recommended turbine efficiencies are 90% for axial turbines above 30 MW, and 85% for radial turbines below 30 MW.
    

    

    # Weiland & Thimsen 2016
    # Multiple literature sources suggest that recuperator cold side (high pressure) pressure drop of
    # approximately 140 kPa (20 psid) and a hot side (low pressure) pressure drop of 280 kPa (40 psid) can be reasonably used.
    # Note: Unclear what the low pressure assumption is in this study, could be significantly lower for direct combustion cycles
    eff_max = 1.0
    
    
    des_par["LTR_UA_des_in"] = 669.637     # [kW/K] (required if LTR_design_code == 1)
    des_par["LTR_min_dT_des_in"] = 12.0   # [C] (required if LTR_design_code == 2)
    
    des_par["LT_recup_eff_max"] = eff_max    # [-] Maximum effectiveness low temperature recuperator
    des_par["LTR_LP_deltaP_des_in"] = deltaP_recup_LP  # [-]
    des_par["LTR_HP_deltaP_des_in"] = deltaP_recup_HP  # [-]
    
    
    des_par["HTR_UA_des_in"] = 649.06     # [kW/K] (required if LTR_design_code == 1)
    des_par["HTR_min_dT_des_in"] = 19.2   # [C] (required if LTR_design_code == 2)
    
    des_par["HT_recup_eff_max"] = eff_max  # [-] Maximum effectiveness high temperature recuperator
    des_par["HTR_LP_deltaP_des_in"] = deltaP_recup_LP  # [-]
    des_par["HTR_HP_deltaP_des_in"] = deltaP_recup_HP  # [-]
    # PHX
    
    
    # Air Cooler
    
    des_par["fan_power_frac"] = 0.000001  # [-] Fraction of net cycle power consumed by air cooler fan. 2% here per Turchi et al.
    # Default
    des_par["deltaP_counterHX_frac"] = 0.0054321  # [-] Fraction of CO2 inlet pressure that is design point counterflow HX (recups & PHX) pressure drop

    # Extra Bypass Configuration Parameters
    
    des_par["deltaT_bypass"] = -0.4 # [C] sco2 Bypass Outlet Temp - HTR_HP_OUT Temp
    #des_par["dT_PHX_cold_approach"] = des_par["dT_PHX_hot_approach"] # [C/K] default hot approach. Temperature difference between HTF PHX outlet and MIXER2 sco2


    return des_par

def get_sco2_design_parameters_Alfani_2019():

    # from Alfani 2019
    des_par = {}

    des_par["set_HTF_mdot"] = 50 # For HTR Bypass ONLY, 0 = calculate HTF mdot (need to set dT_PHX_cold_approach), > 0 = HTF mdot kg/s

    W_thermo = 5.86276
    des_par["W_dot_net_des"] = W_thermo  # [MWe] Design cycle power output (no cooling parasitics)

    
    des_par["T_htf_hot_des"] = 550.0  # [C] HTF design hot temperature (PHX inlet)
    
    des_par["P_high_limit"] = 18.134  # [MPa] Cycle high pressure limit
    des_par["is_PR_fixed"] = -8.112      # 0 = No, >0 = fixed pressure ratio at input <0 = fixed LP at abs(input)
        # Recuperator design
    des_par["design_method"] = 3  # [-] 1 = specify efficiency, 2 = specify total recup UA, 3 = Specify each recup design (see inputs below)

        # LTR
    des_par["LTR_design_code"] = 2        # 1 = UA, 2 = min dT, 3 = effectiveness
    des_par["LTR_min_dT_des_in"] = 10.0   # [C] (required if LTR_design_code == 2)

        # HTR
    des_par["HTR_design_code"] = 2        # 1 = UA, 2 = min dT, 3 = effectiveness
    des_par["HTR_min_dT_des_in"] = 10.0   # [C] (required if LTR_design_code == 2)

        # Air Cooler
    des_par["deltaP_cooler_frac"] = 0.005  # [-] Fraction of CO2 inlet pressure that is design point cooler CO2 pressure drop

    deltaP_recup_HP = 0.005  # [-] = 0.14[MPa]/25[MPa]
    deltaP_recup_LP = 0.005  # [-] = 0.28[MPa]/9[MPa]

    des_par["eta_isen_t"] = 0.90  # [-] Turbine isentropic efficiency
    des_par["eta_isen_mc"] = 0.8  # [-] Main compressor isentropic efficiency
    des_par["eta_isen_rc"] = 0.8  # [-] Recompressor isentropic efficiency

    des_par["dT_PHX_hot_approach"] = 203.31  # [C/K] default 20. Temperature difference between hot HTF and turbine inlet
    des_par["T_bypass_target"] = 214.06  # [C] HTF design Bypass Outlet
    des_par["dT_PHX_cold_approach"] = 25.146  # [C/K] default 20. Temperature difference between cold HTF and cold CO2 PHX inlet
    

    des_par["is_recomp_ok"] = -0.420418 	# 1 = Yes, 0 = simple cycle only, < 0 = fix f_recomp to abs(input)
    des_par["is_bypass_ok"] = -0.17386     # 1 = Yes, 0 = no bypass, < 0 = fix bp_frac to abs(input)  
    

    des_par["T_amb_des"] = 27.0  # [C] Ambient temperature at design
    des_par["dT_mc_approach"] = 6.0  # [C] Use 6 here per Neises & Turchi 19. Temperature difference between main compressor CO2 inlet and ambient air




        # System design parameters
    des_par["htf"] = 17  # [-] Solar salt

    
    des_par["site_elevation"] = 588  # [m] Elevation of Daggett, CA. Used to size air cooler...

    # Cycle design options
        # Configuration
    des_par["cycle_config"] = 3  # [1] = RC, [2] = PC

        # Recuperator design
    des_par["eta_thermal_des"] = 0.44  # [-] Target power cycle thermal efficiency (used when design_method == 1)
    des_par["UA_recup_tot_des"] = 15 * 1000 * (des_par["W_dot_net_des"]) / 50.0  # [kW/K] (used when design_method == 2)

        # Pressures and recompression fraction
    des_par["is_P_high_fixed"] = 1  # 0 = No, optimize. 1 = Yes (=P_high_limit)
    des_par["is_IP_fixed"] = 0      # partial cooling config: 0 = No, >0 = fixed HP-IP pressure ratio at input, <0 = fixed IP at abs(input)
    
    # Convergence and optimization criteria
    des_par["rel_tol"] = 3  # [-] Baseline solver and optimization relative tolerance exponent (10^-rel_tol)


    
    des_par["eta_isen_pc"] = 0.85  # [-] Precompressor isentropic efficiency


    # Weiland & Thimsen 2016
    # Multiple literature sources suggest that recuperator cold side (high pressure) pressure drop of
    # approximately 140 kPa (20 psid) and a hot side (low pressure) pressure drop of 280 kPa (40 psid) can be reasonably used.
    # Note: Unclear what the low pressure assumption is in this study, could be significantly lower for direct combustion cycles
    eff_max = 1
    
    # LTR
    des_par["LTR_UA_des_in"] = 7500     # [kW/K] (required if LTR_design_code == 1)
    des_par["LTR_eff_des_in"] = 0.895     # [-] (required if LTR_design_code == 3)
    des_par["LT_recup_eff_max"] = eff_max    # [-] Maximum effectiveness low temperature recuperator
    des_par["LTR_LP_deltaP_des_in"] = deltaP_recup_LP  # [-]
    des_par["LTR_HP_deltaP_des_in"] = deltaP_recup_HP  # [-]
    # HTR
    des_par["HTR_UA_des_in"] = 7500     # [kW/K] (required if LTR_design_code == 1)
    des_par["HTR_eff_des_in"] = 0.945      # [-] (required if LTR_design_code == 3)
    des_par["HT_recup_eff_max"] = eff_max  # [-] Maximum effectiveness high temperature recuperator
    des_par["HTR_LP_deltaP_des_in"] = deltaP_recup_LP  # [-]
    des_par["HTR_HP_deltaP_des_in"] = deltaP_recup_HP  # [-]
    # PHX
    des_par["PHX_co2_deltaP_des_in"] = deltaP_recup_HP  # [-]
    
    # Air Cooler
    des_par["fan_power_frac"] = 0.11446 / W_thermo  # [-] Fraction of net cycle power consumed by air cooler fan. 2% here per Turchi et al.
    # Default
    des_par["deltaP_counterHX_frac"] = 0.0054321  # [-] Fraction of CO2 inlet pressure that is design point counterflow HX (recups & PHX) pressure drop

    # Extra Bypass Configuration Parameters
    
    des_par["deltaT_bypass"] = 0 # [C] sco2 Bypass Outlet Temp - HTR_HP_OUT Temp

    return des_par

def get_sco2_design_parameters_Alfani_2020_update2():

    des_par = {}

    # from Alfani 2020

    W_thermo = 108.4279594

    des_par["W_dot_net_des"] = W_thermo  # [MWe] Design cycle power output (no cooling parasitics)
    

    des_par["eta_isen_t"] = 0.898 # [-] Turbine isentropic efficiency
    des_par["eta_isen_mc"] = -0.777  # [-] Main compressor Polytropic efficiency
    des_par["eta_isen_rc"] = -0.767  # [-] Recompressor Polytropic efficiency
 
    # Recuperator design
    des_par["design_method"] = 3  # [-] 1 = specify efficiency, 2 = specify total recup UA, 3 = Specify each recup design (see inputs below)
    #des_par["design_method"] = 2  # [-] 1 = specify efficiency, 2 = specify total recup UA, 3 = Specify each recup design (see inputs below)
    des_par["UA_recup_tot_des"] = 36851.92  # [kW/K] (used when design_method == 2)

        # LTR
    des_par["LTR_design_code"] = 2        # 1 = UA, 2 = min dT, 3 = effectiveness
    des_par["LTR_min_dT_des_in"] = 10.0   # [C] (required if LTR_design_code == 2)

        # HTR
    des_par["HTR_design_code"] = 2        # 1 = UA, 2 = min dT, 3 = effectiveness
    des_par["HTR_min_dT_des_in"] = 10.0   # [C] (required if LTR_design_code == 2)
    
    # ADDED to converge LTR and HTR 
    des_par["HTR_n_sub_hx"] = 10
    des_par["LTR_n_sub_hx"] = 10

    # Ambient
    des_par["T_amb_des"] = 25.0  # [C] Ambient temperature at design
    des_par["dT_mc_approach"] = 8.0  # [C] Use 6 here per Neises & Turchi 19. Temperature difference between main compressor CO2 inlet and ambient air
    
    # Air Cooler
    des_par["fan_power_frac"] = 0.87805/W_thermo  # [-] Fraction of net cycle power consumed by air cooler fan. 2% here per Turchi et al.

    # Recomp and Bypass Fraction
    des_par["is_bypass_ok"] = -0.114
    des_par["is_recomp_ok"] = -0.35 	# 1 = Yes, 0 = simple cycle only, < 0 = fix f_recomp to abs(input)

    # HTF Temperatures
    des_par["T_htf_hot_des"] = 640          # [C] HTF design hot temperature (PHX inlet)
    des_par["dT_PHX_hot_approach"] = 20     # [C/K] default 20. Temperature difference between hot HTF and turbine inlet
    des_par["T_bypass_target"] = 734.396301 - 273.15   # [C] HTF design Bypass Outlet
    des_par["dT_PHX_cold_approach"] = 20    # [C/K] default 20. Temperature difference between cold HTF and cold CO2 PHX inlet

        # Pressure
    des_par["is_P_high_fixed"] = 1  # 0 = No, optimize. 1 = Yes (=P_high_limit)
    des_par["P_high_limit"] = 25  # [MPa] Cycle high pressure limit
    des_par["is_PR_fixed"] = -8.297     # 0 = No, >0 = fixed pressure ratio at input <0 = fixed LP at abs(input)
    des_par["deltaP_cooler_frac"] = 0.005  # [-] Fraction of CO2 inlet pressure that is design point cooler CO2 pressure drop
    des_par["LTR_LP_deltaP_des_in"] = 0.005  # [-]
    des_par["HTR_LP_deltaP_des_in"] = 0.005  # [-]
    des_par["LTR_HP_deltaP_des_in"] = 0.0002  # [-]
    des_par["HTR_HP_deltaP_des_in"] = 0.000440088  # [-]
    des_par["PHX_co2_deltaP_des_in"] = 0.008005123  # [-]

        # System design parameters
    des_par["htf"] = 17  # [-] Solar salt
    des_par["site_elevation"] = 588  # [m] Elevation of Daggett, CA. Used to size air cooler...

    # Cycle design options
        # Configuration
    des_par["cycle_config"] = 3  # [1] = RC, [2] = PC

    des_par["eta_thermal_des"] = 0.44  # [-] Target power cycle thermal efficiency (used when design_method == 1)

    des_par["is_IP_fixed"] = 0      # partial cooling config: 0 = No, >0 = fixed HP-IP pressure ratio at input, <0 = fixed IP at abs(input)
    
    # Convergence and optimization criteria
    des_par["rel_tol"] = 6  # [-] Baseline solver and optimization relative tolerance exponent (10^-rel_tol)

    des_par["eta_isen_pc"] = 0.85  # [-] Precompressor isentropic efficiency

    # Weiland & Thimsen 2016
    # Multiple literature sources suggest that recuperator cold side (high pressure) pressure drop of
    # approximately 140 kPa (20 psid) and a hot side (low pressure) pressure drop of 280 kPa (40 psid) can be reasonably used.
    # Note: Unclear what the low pressure assumption is in this study, could be significantly lower for direct combustion cycles
    eff_max = 1
    
    # LTR
    des_par["LTR_UA_des_in"] = 7500     # [kW/K] (required if LTR_design_code == 1)
    des_par["LTR_eff_des_in"] = 0.895     # [-] (required if LTR_design_code == 3)
    des_par["LT_recup_eff_max"] = eff_max    # [-] Maximum effectiveness low temperature recuperator
    
    
    # HTR
    des_par["HTR_UA_des_in"] = 7500     # [kW/K] (required if LTR_design_code == 1)
    des_par["HTR_eff_des_in"] = 0.945      # [-] (required if LTR_design_code == 3)
    des_par["HT_recup_eff_max"] = eff_max  # [-] Maximum effectiveness high temperature recuperator

    # Default
    des_par["deltaP_counterHX_frac"] = 0.0054321  # [-] Fraction of CO2 inlet pressure that is design point counterflow HX (recups & PHX) pressure drop

    # Extra Bypass Configuration Parameters
    des_par["deltaT_bypass"] = 0 # [C] sco2 Bypass Outlet Temp - HTR_HP_OUT Temp

    return des_par

def get_sco2_tsf_design_parameters_Alfani_2021():

    des_par = {}

    # from Alfani 2021

    # Configuration
    des_par["cycle_config"] = 4  # [1] = RC, [2] = PC, [3] = HTRBP, [4] = TSF

    # Power
    W_thermo_w_eff = 6.924633; # This is actual thermo power produced by alfani (includes cooling parasitics and gen/motor eff)
    W_thermo_net = 6.4
    W_air_cooler = 0.1


    W_thermo = 6.50 # [MWe]
    des_par["W_dot_net_des"] = W_thermo_w_eff  # [MWe] Design cycle power output (no cooling parasitics)
    des_par["fan_power_frac"] = 0.1/W_thermo_w_eff  # [-] Fraction of net cycle power consumed by air cooler fan

    # HTF
    des_par["T_htf_hot_des"] = 550          # [C] HTF design hot temperature (PHX inlet)
    des_par["dT_PHX_hot_approach"] = 25     # [C/K] Temperature difference between hot HTF and turbine inlet
    des_par["set_HTF_mdot"] = 50

    # Ambient
    des_par["T_amb_des"] = 25.0  # [C] Ambient temperature at design
    des_par["dT_mc_approach"] = 8.0  # [C] Use 6 here per Neises & Turchi 19. Temperature difference between main compressor CO2 inlet and ambient air

    # Pressure
    des_par["PHX_co2_deltaP_des_in"] = -200  # [kPa] Absolute pressure loss
    des_par["deltaP_cooler_frac"] = 0.005  # [-] Fraction of CO2 inlet pressure that is design point cooler CO2 pressure drop
    des_par["LTR_LP_deltaP_des_in"] = 0.01  # [-]
    des_par["HTR_LP_deltaP_des_in"] = 0.01  # [-]
    des_par["is_P_high_fixed"] = 1  # 0 = No, optimize. 1 = Yes (=P_high_limit)
    des_par["P_high_limit"] = 25  # [MPa] Cycle high pressure limit
    des_par["is_PR_fixed"] = -7.918     # 0 = No, >0 = fixed pressure ratio at input <0 = fixed LP at abs(input)

    # Efficiency
    des_par["eta_isen_t"] = 0.85 # [-] Turbine isentropic efficiency
    des_par["eta_isen_t2"] = 0.85 # [-] Secondary turbine isentropic efficiency
    des_par["eta_isen_mc"] = 0.80  # [-] Main compressor isentropic efficiency

    # Recuperators
    des_par["design_method"] = 3  # [-] 1 = specify efficiency, 2 = specify total recup UA, 3 = Specify each recup design (see inputs below)
        # LTR
    des_par["LTR_design_code"] = 2        # 1 = UA, 2 = min dT, 3 = effectiveness
    des_par["LTR_min_dT_des_in"] = 10.0   # [C] (required if LTR_design_code == 2)
    des_par["LTR_UA_des_in"] = 1.61506 * 112.18     # [kW/K] (required if LTR_design_code == 1)

        # HTR
    des_par["HTR_design_code"] = 2        # 1 = UA, 2 = min dT, 3 = effectiveness
    des_par["HTR_min_dT_des_in"] = 10.0   # [C] (required if LTR_design_code == 2)
    des_par["HTR_UA_des_in"] = 0.77925 * 959.37     # [kW/K] (required if LTR_design_code == 1)

    # TSF
    #des_par["is_turbine_split_ok"] = -0.301383579
    des_par["is_turbine_split_ok"] = -0.431364843

    # DEFAULTS

    # ADDED to converge LTR and HTR 
    des_par["HTR_n_sub_hx"] = 10
    des_par["LTR_n_sub_hx"] = 10

        # Pressure
    des_par["LTR_HP_deltaP_des_in"] = 0.0 # 0.01  # [-]
    des_par["HTR_HP_deltaP_des_in"] = 0.0 # 0.01  # [-]
 
        # System design parameters
    des_par["htf"] = 17  # [-] Solar salt
    des_par["site_elevation"] = 588  # [m] Elevation of Daggett, CA. Used to size air cooler...

    # Convergence and optimization criteria
    des_par["rel_tol"] = 6  # [-] Baseline solver and optimization relative tolerance exponent (10^-rel_tol)

    # Default
    des_par["deltaP_counterHX_frac"] = 0.0054321  # [-] Fraction of CO2 inlet pressure that is design point counterflow HX (recups & PHX) pressure drop



    # NOT USED

    # LTR
    eff_max = 1
    
    des_par["LTR_eff_des_in"] = 0.895     # [-] (required if LTR_design_code == 3)
    des_par["LT_recup_eff_max"] = eff_max    # [-] Maximum effectiveness low temperature recuperator
    
    # HTR
    des_par["HTR_UA_des_in"] = 7500     # [kW/K] (required if LTR_design_code == 1)
    des_par["HTR_eff_des_in"] = 0.945      # [-] (required if LTR_design_code == 3)
    des_par["HT_recup_eff_max"] = eff_max  # [-] Maximum effectiveness high temperature recuperator

    des_par["eta_isen_rc"] = -0.767  # [-] Recompressor Polytropic efficiency
    des_par["UA_recup_tot_des"] = 36851.92  # [kW/K] (used when design_method == 2)

    des_par["eta_isen_pc"] = 0.85  # [-] Precompressor isentropic efficiency
    des_par["eta_thermal_des"] = 0.44  # [-] Target power cycle thermal efficiency (used when design_method == 1)
    des_par["is_IP_fixed"] = 0      # partial cooling config: 0 = No, >0 = fixed HP-IP pressure ratio at input, <0 = fixed IP at abs(input)

    
    return des_par

def get_sco2_design_parameters_Moullec():

    des_par = {}

    des_par["set_HTF_mdot"] = 80 # For HTR Bypass ONLY, 0 = calculate HTF mdot (need to set dT_PHX_cold_approach), > 0 = HTF mdot kg/s


    des_par["W_dot_net_des"] = 10.01  # [MWe] Design cycle power output (no cooling parasitics)

    # System design parameters
    des_par["htf"] = 17  # [-] Solar salt
    des_par["T_htf_hot_des"] = 530.0  # [C] HTF design hot temperature (PHX inlet)
    des_par["dT_PHX_hot_approach"] = 91.0  # [C/K] default 20. Temperature difference between hot HTF and turbine inlet
    des_par["dT_PHX_cold_approach"] = 20  # [C/K] default 20. Temperature difference between cold HTF and cold CO2 PHX inlet

    deltaP_recup_HP = 0.004  # [-] = 0.1[MPa]/25[MPa]
    deltaP_recup_LP = 0.01148  # [-] = 0.1[MPa]/8.71[MPa]
    des_par["PHX_co2_deltaP_des_in"] = deltaP_recup_HP  # [-]

    # Air Cooler
    des_par["T_amb_des"] = 29.0  # [C] Ambient temperature at design
    des_par["dT_mc_approach"] = 6.0  # [C] Use 6 here per Neises & Turchi 19. Temperature difference between main compressor CO2 inlet and ambient air
    des_par["deltaP_cooler_frac"] = 0.005  # [-] Fraction of CO2 inlet pressure that is design point cooler CO2 pressure drop
    des_par["fan_power_frac"] = 0.02  # [-] Fraction of net cycle power consumed by air cooler fan. 2% here per Turchi et al.

        # Configuration
    des_par["cycle_config"] = 3  # [1] = RC, [2] = PC

        # Recuperator design
    des_par["design_method"] = 2  # [-] 1 = specify efficiency, 2 = specify total recup UA, 3 = Specify each recup design (see inputs below)
    des_par["UA_recup_tot_des"] = 6462.2385 # [kW/K] (used when design_method == 2)
    
    des_par["is_bypass_ok"] = 1
    des_par["is_recomp_ok"] = -0.3417 	# 1 = Yes, 0 = simple cycle only, < 0 = fix f_recomp to abs(input)
    des_par["is_PR_fixed"] = -8.71      # 0 = No, >0 = fixed pressure ratio at input <0 = fixed LP at abs(input)
    des_par["is_P_high_fixed"] = 1  # 0 = No, optimize. 1 = Yes (=P_high_limit)
    des_par["P_high_limit"] = 25  # [MPa] Cycle high pressure limit

    # Convergence and optimization criteria
    des_par["rel_tol"] = 6  # [-] Baseline solver and optimization relative tolerance exponent (10^-rel_tol)

    # Compressor and Turbine Efficiency
    des_par["eta_isen_mc"] = 0.80   # [-] Main compressor isentropic efficiency
    des_par["eta_isen_rc"] = 0.80   # [-] Recompressor isentropic efficiency
    des_par["eta_isen_t"] = 0.85    # [-] Turbine isentropic efficiency
    
    eff_max = 1.0
    

    # LTR
    des_par["LTR_design_code"] = 2        # 1 = UA, 2 = min dT, 3 = effectiveness
    des_par["LTR_UA_des_in"] = 7500     # [kW/K] (required if LTR_design_code == 1)
    des_par["LTR_min_dT_des_in"] = 10   # [C] (required if LTR_design_code == 2)
    des_par["LT_recup_eff_max"] = eff_max    # [-] Maximum effectiveness low temperature recuperator
    des_par["LTR_LP_deltaP_des_in"] = deltaP_recup_LP  # [-]
    des_par["LTR_HP_deltaP_des_in"] = deltaP_recup_HP  # [-]
    # HTR
    des_par["HTR_design_code"] = 2        # 1 = UA, 2 = min dT, 3 = effectiveness
    des_par["HTR_UA_des_in"] = 7500     # [kW/K] (required if LTR_design_code == 1)
    des_par["HTR_min_dT_des_in"] = 10   # [C] (required if LTR_design_code == 2)
    des_par["HT_recup_eff_max"] = eff_max  # [-] Maximum effectiveness high temperature recuperator
    des_par["HTR_LP_deltaP_des_in"] = deltaP_recup_LP  # [-]
    des_par["HTR_HP_deltaP_des_in"] = deltaP_recup_HP  # [-]

    # Extra Bypass Configuration Parameters
    des_par["T_bypass_target"] = 290.0  # [C] HTF design Bypass Outlet
    des_par["deltaT_bypass"] = 0 # [C] sco2 Bypass Outlet Temp - HTR_HP_OUT Temp

    des_par["site_elevation"] = 588  # [m] Elevation of Daggett, CA. Used to size air cooler...
    
    # Cycle design options
    des_par["eta_thermal_des"] = 0.44  # [-] Target power cycle thermal efficiency (used when design_method == 1)

        # Pressures and recompression fraction
    des_par["is_IP_fixed"] = 0      # partial cooling config: 0 = No, >0 = fixed HP-IP pressure ratio at input, <0 = fixed IP at abs(input)
    des_par["eta_isen_pc"] = 0.85  # [-] Precompressor isentropic efficiency

    des_par["LTR_eff_des_in"] = 0.895     # [-] (required if LTR_design_code == 3)
    des_par["HTR_eff_des_in"] = 0.945      # [-] (required if LTR_design_code == 3)

    # Default
    des_par["deltaP_counterHX_frac"] = 0.0054321  # [-] Fraction of CO2 inlet pressure that is design point counterflow HX (recups & PHX) pressure drop

    return des_par

def get_sco2_design_parameters_Alfani_2024():

    des_par = {}

    # from Alfani 2020

    W_thermo = 20

    des_par["W_dot_net_des"] = W_thermo  # [MWe] Design cycle power output (no cooling parasitics)
    des_par["des_objective"] = 2

    des_par["eta_isen_t"] = 0.9 # [-] Turbine isentropic efficiency
    des_par["eta_isen_mc"] = 0.85  # [-] Main compressor isentropic efficiency
    des_par["eta_isen_rc"] = 0.85  # [-] Recompressor isentropic efficiency
 
    # Target sco2 at Bypass Temp
    des_par["T_target_is_HTF"] = 0  # sco2
    des_par["T_bypass_target"] = 190 # deg C

    # Recuperator design
    des_par["design_method"] = 3  # [-] 1 = specify efficiency, 2 = specify total recup UA, 3 = Specify each recup design (see inputs below)

        # LTR
    des_par["LTR_design_code"] = 2        # 1 = UA, 2 = min dT, 3 = effectiveness
    des_par["LTR_min_dT_des_in"] = 5   # [C] (required if LTR_design_code == 2)

        # HTR
    des_par["HTR_design_code"] = 2        # 1 = UA, 2 = min dT, 3 = effectiveness
    des_par["HTR_min_dT_des_in"] = 5   # [C] (required if LTR_design_code == 2)
    
    # ADDED to converge LTR and HTR 
    des_par["HTR_n_sub_hx"] = 500
    des_par["LTR_n_sub_hx"] = 500

    # Ambient
    des_par["T_amb_des"] = 35.0  # [C] Ambient temperature at design
    des_par["dT_mc_approach"] = 10.0  # [C] Temperature difference between main compressor CO2 inlet and ambient air
    
    # Air Cooler
    des_par["fan_power_frac"] = 0.85  # [-] Fraction of net cycle power consumed by air cooler fan. PROCESS AFTER SIMULATION

    # Recomp and Bypass Fraction
    des_par["is_bypass_ok"] = 1
    des_par["is_recomp_ok"] = 1 	# 1 = Yes, 0 = simple cycle only, < 0 = fix f_recomp to abs(input)

    # Targeting 660 -> 190 deg C sco2

    # HTF Temperatures
    des_par["T_htf_hot_des"] = 680        # [C] HTF design hot temperature (PHX inlet)
    des_par["dT_PHX_hot_approach"] = 20     # [C/K] default 20. Temperature difference between hot HTF and turbine inlet
    des_par["dT_PHX_cold_approach"] = 20    # [C/K] default 20. Temperature difference between cold HTF and cold CO2 PHX inlet

        # Pressure
    des_par["is_P_high_fixed"] = 1  # 0 = No, optimize. 1 = Yes (=P_high_limit)
    des_par["P_high_limit"] = 25  # [MPa] Cycle high pressure limit
    des_par["is_PR_fixed"] = 0     # 0 = No, >0 = fixed pressure ratio at input <0 = fixed LP at abs(input)
    des_par["deltaP_cooler_frac"] = 0.0001  # [-] Fraction of CO2 inlet pressure that is design point cooler CO2 pressure drop
    des_par["LTR_LP_deltaP_des_in"] = 0.0001  # [-]
    des_par["HTR_LP_deltaP_des_in"] = 0.0001  # [-]
    des_par["LTR_HP_deltaP_des_in"] = 0.00005  # [-]
    des_par["HTR_HP_deltaP_des_in"] = 0.0005  # [-]
    des_par["PHX_co2_deltaP_des_in"] = 0.0001  # [-]

        # System design parameters
    des_par["htf"] = 17  # [-] Solar salt
    des_par["site_elevation"] = 588  # [m] Elevation of Daggett, CA. Used to size air cooler...

    # Cycle design options
        # Configuration
    des_par["cycle_config"] = 3  # [1] = RC, [2] = PC

    des_par["eta_thermal_des"] = 0.44  # [-] Target power cycle thermal efficiency (used when design_method == 1)

    des_par["is_IP_fixed"] = 0      # partial cooling config: 0 = No, >0 = fixed HP-IP pressure ratio at input, <0 = fixed IP at abs(input)
    
    # Convergence and optimization criteria
    des_par["rel_tol"] = 6  # [-] Baseline solver and optimization relative tolerance exponent (10^-rel_tol)

    des_par["eta_isen_pc"] = 0.85  # [-] Precompressor isentropic efficiency

    # Weiland & Thimsen 2016
    # Multiple literature sources suggest that recuperator cold side (high pressure) pressure drop of
    # approximately 140 kPa (20 psid) and a hot side (low pressure) pressure drop of 280 kPa (40 psid) can be reasonably used.
    # Note: Unclear what the low pressure assumption is in this study, could be significantly lower for direct combustion cycles
    eff_max = 1
    
    # LTR
    des_par["LTR_UA_des_in"] = 7500     # [kW/K] (required if LTR_design_code == 1)
    des_par["LTR_eff_des_in"] = 0.895     # [-] (required if LTR_design_code == 3)
    des_par["LT_recup_eff_max"] = eff_max    # [-] Maximum effectiveness low temperature recuperator
    
    
    # HTR
    des_par["HTR_UA_des_in"] = 7500     # [kW/K] (required if LTR_design_code == 1)
    des_par["HTR_eff_des_in"] = 0.945      # [-] (required if LTR_design_code == 3)
    des_par["HT_recup_eff_max"] = eff_max  # [-] Maximum effectiveness high temperature recuperator

    # Default
    des_par["deltaP_counterHX_frac"] = 0.0054321  # [-] Fraction of CO2 inlet pressure that is design point counterflow HX (recups & PHX) pressure drop

    # Extra Bypass Configuration Parameters
    des_par["deltaT_bypass"] = 0 # [C] sco2 Bypass Outlet Temp - HTR_HP_OUT Temp

    return des_par



def get_sco2_G3P3():

    des_par = {}

    # G3P3 Parameters

    # Power
    W_thermo_net = 10.0     # [MWt] Gross thermo output
    W_air_cooler = 0.1      # [MWe] Air Cooler Parasitic
    W_thermo_gross = W_thermo_net + W_air_cooler    #[MWe]
    des_par["W_dot_net_des"] = W_thermo_gross       #[MWe]
    des_par["fan_power_frac"] = W_air_cooler/W_thermo_net  # [-] Fraction of net cycle power consumed by air cooler fan

    # HTF
    T_HTF_in = 775                          # [C]
    T_turbine_in = 720                      # [C] sco2 turbine inlet temp    
    des_par["T_htf_hot_des"] = T_HTF_in     # [C] HTF design hot temperature (PHX inlet)
    des_par["dT_PHX_hot_approach"] = T_HTF_in - T_turbine_in     # [C/K] Temperature difference between hot HTF and turbine inlet
    des_par["set_HTF_mdot"] = 0

    # Efficiency (ASSUMPTION)
    des_par["eta_isen_t"] = 0.85   # [-] Turbine isentropic efficiency
    des_par["eta_isen_t2"] = 0.85  # [-] Secondary turbine isentropic efficiency
    des_par["eta_isen_mc"] = 0.85  # [-] Main compressor isentropic efficiency
    des_par["eta_isen_pc"] = 0.85  # [-] Precompressor isentropic efficiency
    des_par["eta_isen_rc"] = 0.85  # [-] Recompressor Polytropic efficiency

    # Design Variables
    des_par["is_PR_fixed"] = -7.918     # 0 = No, >0 = fixed pressure ratio at input <0 = fixed LP at abs(input)
    des_par["is_turbine_split_ok"] = -0.431364843
    des_par["is_IP_fixed"] = 0      # partial cooling config: 0 = No, >0 = fixed HP-IP pressure ratio at input, <0 = fixed IP at abs(input)
    des_par["is_bypass_ok"] = -0.114
    des_par["is_recomp_ok"] = -0.35 	# 1 = Yes, 0 = simple cycle only, < 0 = fix f_recomp to abs(input)
    des_par["design_method"] = 2  # [-] 1 = specify efficiency, 2 = specify total recup UA, 3 = Specify each recup design (see inputs below)
    des_par["UA_recup_tot_des"] = 36851.92  # [kW/K] (used when design_method == 2)
    des_par["HTR_UA_des_in"] = 0.77925 * 959.37     # [kW/K] (required if LTR_design_code == 1)
    des_par["LTR_UA_des_in"] = 1.61506 * 112.18     # [kW/K] (required if LTR_design_code == 1)

    des_par["N_nodes_air_cooler_pass"] = 100

    # from Alfani 2021

    # Configuration
    des_par["cycle_config"] = 4  # [1] = RC, [2] = PC, [3] = HTRBP, [4] = TSF

    # Ambient
    des_par["T_amb_des"] = 25.0  # [C] Ambient temperature at design
    des_par["dT_mc_approach"] = 8.0  # [C] Use 6 here per Neises & Turchi 19. Temperature difference between main compressor CO2 inlet and ambient air

    # Pressure
    des_par["PHX_co2_deltaP_des_in"] = -200  # [kPa] Absolute pressure loss
    des_par["deltaP_cooler_frac"] = 0.005  # [-] Fraction of CO2 inlet pressure that is design point cooler CO2 pressure drop
    des_par["LTR_LP_deltaP_des_in"] = 0.01  # [-]
    des_par["HTR_LP_deltaP_des_in"] = 0.01  # [-]
    des_par["is_P_high_fixed"] = 1  # 0 = No, optimize. 1 = Yes (=P_high_limit)
    des_par["P_high_limit"] = 25  # [MPa] Cycle high pressure limit
    

    # Recuperators
    
        # LTR
    des_par["LTR_design_code"] = 2        # 1 = UA, 2 = min dT, 3 = effectiveness
    des_par["LTR_min_dT_des_in"] = 10.0   # [C] (required if LTR_design_code == 2)
    

        # HTR
    des_par["HTR_design_code"] = 2        # 1 = UA, 2 = min dT, 3 = effectiveness
    des_par["HTR_min_dT_des_in"] = 10.0   # [C] (required if LTR_design_code == 2)

    # DEFAULTS

    # ADDED to converge LTR and HTR 
    des_par["HTR_n_sub_hx"] = 10
    des_par["LTR_n_sub_hx"] = 10

        # Pressure
    des_par["LTR_HP_deltaP_des_in"] = 0.01  # [-]
    des_par["HTR_HP_deltaP_des_in"] = 0.01  # [-]
 
        # System design parameters
    des_par["htf"] = 17  # [-] Solar salt
    des_par["site_elevation"] = 588  # [m] Elevation of Daggett, CA. Used to size air cooler...

    # Convergence and optimization criteria
    des_par["rel_tol"] = 6  # [-] Baseline solver and optimization relative tolerance exponent (10^-rel_tol)

    # Default
    des_par["deltaP_counterHX_frac"] = 0.0054321  # [-] Fraction of CO2 inlet pressure that is design point counterflow HX (recups & PHX) pressure drop



    # NOT USED

    # LTR
    eff_max = 1
    
    des_par["LTR_eff_des_in"] = 0.895     # [-] (required if LTR_design_code == 3)
    des_par["LT_recup_eff_max"] = eff_max    # [-] Maximum effectiveness low temperature recuperator
    
    # HTR
    des_par["HTR_eff_des_in"] = 0.945      # [-] (required if LTR_design_code == 3)
    des_par["HT_recup_eff_max"] = eff_max  # [-] Maximum effectiveness high temperature recuperator

    des_par["eta_thermal_des"] = 0.44  # [-] Target power cycle thermal efficiency (used when design_method == 1)
    

    
    return des_par


# Optimization

def run_opt(dict_list, default_par, run_id, result_array_list = None, que = None):

    # Initialize
    result_array = [[] for i in range(len(dict_list))]

    # Make Cycle class
    c_sco2 = sco2_solve.C_sco2_sim(3)

    # Run Progress and Time
    N_runs = len(dict_list)
    #start_time = time.time()

    for i in range(N_runs):

        # Overwrite Variables
        c_sco2.overwrite_default_design_parameters(default_par)
        mod_base_dict = dict_list[i]
        c_sco2.overwrite_des_par_base(mod_base_dict)  

        # Solve
        c_sco2.solve_sco2_case()

        result_array[i] = get_result_list_v2(c_sco2.m_des_par_base, c_sco2.m_solve_dict, c_sco2.m_solve_success)

        #if(que != None):
        #    que.put([run_id, int(float(i) / float(N_runs))])

        if(N_runs > 1):
            progress = int((float(i) / float(N_runs - 1)) * 100)
            print("Processor " + str(run_id) + " is " + str(progress) + "% finished")

        #result_array.append(debug_list)

        # Update Progress Bar
        #percent = float(i + 1) / float(N_runs)
        #print(make_progress_bar(percent))

        # Update Time Prediction
        #current_time = time.time()
        #elapsed_time = current_time - start_time
        #total_time_guess = elapsed_time / percent
        #remaining_time_guess = total_time_guess - elapsed_time
        #remaining_time_guess_hr = str(datetime.timedelta(seconds=int(remaining_time_guess)))
        #print("Total Time: " + str(datetime.timedelta(seconds=int(total_time_guess))))
        #print("Remaining Time: " + remaining_time_guess_hr)

    # Add result to result array (if necessary)
    if(result_array_list != None):
        result_array_list.append(result_array)

    return result_array

def run_once(dict, default_par, result_queue = None, N_run_total = 0):

    # Make Cycle class
    c_sco2 = sco2_solve.C_sco2_sim(3)

    # Overwrite Variables
    c_sco2.overwrite_default_design_parameters(default_par)
    mod_base_dict = dict
    c_sco2.overwrite_des_par_base(mod_base_dict)  

    # Solve
    c_sco2.solve_sco2_case()

    debug_list = get_result_list_v2(c_sco2.m_des_par_base, c_sco2.m_solve_dict, c_sco2.m_solve_success)

    # Add result to queue (if necessary)
    if(result_queue != None):
        result_queue.put(debug_list)

    # Report Progress (if necessary)
    if(N_run_total > 0):
        completed = result_queue.qsize()
        percent = (completed / N_run_total) * 100
        print(str(percent) + "% complete")

    #print(str(run_id) + " has finished")

    return debug_list

def run_opt_parallel(dict_list_total, default_par, nproc, filename = ""):
    # Initialize
    labeled_result_array = []
    label_list = get_result_list_v2([],[],True,True)
    labeled_result_array.append(label_list)
    N_run = len(dict_list_total)

    result_array = [[] for i in range(N_run)]

    start = time.time()

    manager = multiprocessing.Manager()
    result_queue = manager.Queue()

    #with multiprocessing.Pool(nproc, maxtasksperchild=1) as p:
    #    p.map(partial(run_once, default_par=default_par, result_queue=result_queue), dict_list_total)

    with multiprocessing.Pool(nproc, maxtasksperchild=50) as p:
        p.imap_unordered(partial(run_once, default_par=default_par, result_queue=result_queue, N_run_total = N_run), dict_list_total)
        p.close()
        p.join()

    # Collect results from queue
    while not result_queue.empty():
        labeled_result_array.append(result_queue.get())

    # Write File (if necessary)
    if(filename != ""):
        design_point_tools.write_string_array(filename, labeled_result_array, '\t')

    

    end = time.time()

    elapsed = end - start

def run_opt_parallel_experimental(dict_list_total, default_par, nproc, filename = ""):
    # Initialize
    labeled_result_array = []
    label_list = get_result_list_v2([],[],True,True)
    labeled_result_array.append(label_list)
    
    #result_array = [[] for i in range(nproc)]

    # Break dict_list_total into sections
    N_dict = len(dict_list_total)
    run_per_section = math.floor(float(N_dict) / float(nproc))
    num_per_section_list = []

    local_count = 0
    current_section = 0
    for i in range(N_dict):
        if(current_section >= nproc):
            num_per_section_list[current_section - nproc] += 1
            current_section += 1
            continue

        if(local_count == 0):
            num_per_section_list.append(0)

        num_per_section_list[current_section] += 1

        if(local_count == run_per_section - 1):
            local_count = 0
            current_section += 1

            continue
        
        local_count += 1


    dict_list_sections = []
    local_count = 0
    current_section = 0
    for i in range(N_dict):
        
        if(local_count == 0):
            dict_list_sections.append([])

        dict_list_sections[current_section].append(dict_list_total[i])

        if(local_count == num_per_section_list[current_section] - 1):
            local_count = 0
            current_section += 1

        else:
            local_count += 1

    # Multiprocessing Business
    num_runs = len(dict_list_total)

    start = time.time()

    jobs = []
    manager = multiprocessing.Manager()
    return_dict_list = manager.list()

    #q = multiprocessing.Queue()

    for i in range(min(nproc,len(num_per_section_list))):
        p = multiprocessing.Process(target=run_opt, args=(dict_list_sections[i], default_par,i,return_dict_list))
        jobs.append(p)
        p.start()
        

    for proc in jobs:
        proc.join()

    end = time.time()

    #q_list = []
    #while not q.empty():
    #    q_list.append(q.get())

    result_list = list(return_dict_list)

    memory_val = 0
    memory_run = 0
    memory_section = 0

    # Combine result list sections
    for section in result_list:
        for run in section:
            labeled_result_array.append(run)

            for val in run:
                memory_val += sys.getsizeof(val)

            memory_run += sys.getsizeof(run)
        memory_section += sys.getsizeof(section)

    elapsed = end - start
    #print("Total Time: " + str(elapsed))

     # Write File (if necessary)
    if(filename != ""):
        write_string_array(filename, labeled_result_array, '\t')

    return labeled_result_array


def run_once_solve_dict(dict, default_par, solve_dict_queue = None, N_run_total = 0):

    # Make Cycle class
    c_sco2 = sco2_solve.C_sco2_sim(3)

    # Overwrite Variables
    c_sco2.overwrite_default_design_parameters(default_par)
    mod_base_dict = dict
    c_sco2.overwrite_des_par_base(mod_base_dict)  

    # Solve
    c_sco2.solve_sco2_case()

    # Add success to solve dict
    c_sco2.m_solve_dict['cmod_success'] = c_sco2.m_solve_success

    # Add result to queue (if necessary)
    if(solve_dict_queue != None):
        solve_dict_queue.put(c_sco2.m_solve_dict)

    # Report Progress (if necessary)
    if(N_run_total > 0):
        completed = solve_dict_queue.qsize()
        percent = (completed / N_run_total) * 100
        print(str(round(percent, 2)) + "% complete")

    return

def run_opt_parallel_solve_dict(dict_list_total, default_par, nproc):
    # Initialize
    N_run = len(dict_list_total)

    # Make Cycle Collection Class
    sim_collection = sco2_solve.C_sco2_sim_result_collection()

    start = time.time()

    manager = multiprocessing.Manager()
    solve_dict_queue = manager.Queue()

    with multiprocessing.Pool(nproc, maxtasksperchild=50) as p:
        p.imap_unordered(partial(run_once_solve_dict, default_par=default_par, solve_dict_queue=solve_dict_queue, N_run_total = N_run), dict_list_total)
        p.close()
        p.join()

    # Collect results from queue
    q_size = solve_dict_queue.qsize()
    count = 0

    print("Runs complete. Collecting results")

    while not solve_dict_queue.empty():
        sim_collection.add(solve_dict_queue.get())
        count = count + 1

        if(count % 100 == 0):
            print("Collecting " + str(round((count / q_size) * 100.0, 2)) + "%")

    print("Collecting complete.")
    end = time.time()

    return sim_collection


def get_time_string():
    now = datetime.datetime.now()
    return now.strftime("%Y%m%d_%H%M%S")

def run_sweeeeeeep():

    # Get Default Parameters
    default_par = get_sco2_design_parameters_Alfani_2020_update2()

    # Organize Variable Combinations
    Npts = 10
    bp_list = np.linspace(0,0.999,Npts,False)
    recomp_list = np.linspace(0,0.99,Npts, False)
    ltr_ua_frac_list = np.linspace(0.000001,0.999999,Npts, False)
    max_pressure_cycle = default_par["P_high_limit"]
    min_pressure = 7
    max_pressure = 12
    pressure_list = np.linspace(min_pressure, max_pressure, Npts, False)
    min_UA_total = 50000
    max_UA_total = 100
    UA_total_list = np.linspace(min_UA_total, max_UA_total, 5, False)
    max_UA_single_val = default_par["UA_recup_tot_des"]
    HTF_target_list = np.linspace(100, 550, Npts, False)

    # HTR BP: UA Total and Target Temperature
    if False:
        default_par["cycle_config"] = 3
        default_par["is_recomp_ok"] = 1
        default_par["des_objective"] = 2
        default_par["design_method"] = 2
        default_par["is_PR_fixed"] = 0
        default_par["is_bypass_ok"] = 1
        dict_list = make_dict_par_list([], [], [], [], [], [max_UA_single_val], HTF_target_list)

        file_name = "alfani_2020_UATotal_Target_sweep" + str(Npts) + "_results";

        combined_name = folder_location + file_name + get_time_string() + ".txt"

        start = time.time()
        result_list = run_opt_parallel(dict_list, default_par, 10, combined_name)
        end = time.time()


    # BP, Recomp, UA, Pressure
    if False:
        default_par["cycle_config"] = 3
        dict_list = make_dict_par_list(bp_list, recomp_list, ltr_ua_frac_list, [], pressure_list, default_par["UA_recup_tot_des"])

        file_name = "alfani_2020_htrbp_sweep" + str(Npts) + "_results";

        combined_name = folder_location + file_name + get_time_string() + ".txt"

        start = time.time()
        result_list = run_opt_parallel_experimental(dict_list, default_par, 1, combined_name)
        end = time.time()

    # Recompression sweep
    if False:
        default_par["cycle_config"] = 1
        dict_list = make_dict_par_list([], recomp_list, ltr_ua_frac_list, [], pressure_list, [max_UA_single_val])

        file_name = "alfani_2020_recomp_full_sweep" + str(Npts) + "_results";

        combined_name = folder_location + file_name + get_time_string() + ".txt"

        start = time.time()
        result_list = run_opt_parallel(dict_list, default_par, 10, combined_name)
        end = time.time()

    # Partial Cooling Sweep
    if False:
        default_par["cycle_config"] = 2
        default_par["eta_isen_rc"] = 0.73916766 # temporary fix (might be bug in scc code)
        
        dict_list = make_dict_par_list([], recomp_list, ltr_ua_frac_list, [], pressure_list, [max_UA_single_val], [])

        file_name = "alfani_2020_partial_UAtotal_sweep" + str(Npts) + "_results";

        combined_name = folder_location + file_name + get_time_string() + ".txt"

        start = time.time()
        result_list = run_opt_parallel(dict_list, default_par, 10, combined_name)
        end = time.time()


    # HTR BP: UA total and LTR Frac
    if False:
        default_par["cycle_config"] = 3
        default_par["design_method"] = 2
        default_par["is_recomp_ok"] = -0.3
        default_par["des_objective"] = 2
        default_par["is_bypass_ok"] = -0.1

        dict_list = make_dict_par_list([], [], ltr_ua_frac_list, [], [], UA_total_list)

        file_name = "alfani_2020_htrbp_UA_sweep" + str(Npts) + "_results";

        combined_name = folder_location + file_name + get_time_string() + ".txt"

        start = time.time()
        result_list = run_opt_parallel_experimental(dict_list, default_par, 10, combined_name)
        end = time.time()

    # HTR BP: Target Temperature
    if False:
        default_par["cycle_config"] = 3
        default_par["is_recomp_ok"] = 1
        default_par["des_objective"] = 2
        default_par["is_bypass_ok"] = 1
        default_par["design_method"] = 2
        default_par["is_PR_fixed"] = 0

        dict_list = make_dict_par_list([], [], [], [], [], [], HTF_target_list)

        file_name = "alfani_2020_htrbp_temp_sweep" + str(Npts) + "_results";

        combined_name = folder_location + file_name + get_time_string() + ".txt"

        start = time.time()
        result_list = run_opt_parallel_experimental(dict_list, default_par, 10, combined_name)
        end = time.time()
    
    # BP, Recomp, UA, Pressure, Total UA
    if False:
        default_par["cycle_config"] = 3
        dict_list = make_dict_par_list(bp_list, recomp_list, ltr_ua_frac_list, [], pressure_list, UA_total_list)

        file_name = "alfani_2020_htrbp_all_sweep" + str(Npts) + "_results";

        combined_name = folder_location + file_name + get_time_string() + ".txt"

        start = time.time()
        result_list = run_opt_parallel_experimental(dict_list, default_par, 10, combined_name)
        end = time.time()



    stop_here = 50

def run_hardcode_bp_sweep():

    # Get Default Parameters
    default_par = get_sco2_design_parameters_Alfani_2020_update2()

    # Organize Variable Combinations
    Npts = 100
    bp_list = np.linspace(0,0.999,Npts,False)
        
    # BP, Recomp, UA, Pressure
    if True:


        default_par["cycle_config"] = 3
        default_par["is_recomp_ok"] = 1
        default_par["des_objective"] = 2
        default_par["design_method"] = 2
        default_par["is_PR_fixed"] = 0

        dict_list = make_dict_par_list(bp_list, [], [], [], [], default_par["UA_recup_tot_des"])

        file_name = "alfani_2020_hardcode_bp_recomp_sweep" + str(Npts) + "_results";

        combined_name = folder_location + file_name + get_time_string() + ".txt"

        start = time.time()
        result_list = run_opt_parallel_experimental(dict_list, default_par, 10, combined_name)
        end = time.time()


def run_timing():
    
    # Get Default Parameters
    default_par = get_sco2_design_parameters_Alfani_2020_update2()

    # Organize Variable Combinations
    Npts = 3
    bp_list = np.linspace(0,0.999,Npts,False)
    recomp_list = np.linspace(0,1,Npts, False)
    ltr_ua_frac_list = np.linspace(0.000001,0.999999,Npts, False)
    max_pressure_cycle = default_par["P_high_limit"]
    min_pressure = 7
    max_pressure = 12
    pressure_list = np.linspace(min_pressure, max_pressure, Npts, False)

    # BP, Recomp, UA
    #dict_list = make_dict_par_list(bp_list, recomp_list, ltr_ua_frac_list,[],[],default_par["UA_recup_tot_des"])

        
    # BP, Recomp, UA, Pressure
    dict_list = make_dict_par_list(bp_list, recomp_list, ltr_ua_frac_list, [], pressure_list, default_par["UA_recup_tot_des"])

    max_core = 12

    regular_times = []
    exp_times = []

    if True:
        for nproc in range(max_core):
            start = time.time()
            run_opt_parallel(dict_list, default_par, nproc + 1)
            end = time.time()
            regular_times.append(end-start)

    if False:
        for nproc in range(max_core):
            start = time.time()
            result_list = run_opt_parallel_experimental(dict_list, default_par, nproc + 1)
            end = time.time()
            exp_times.append(end-start)

    asdf = 0


# Final Script

def htrbp_sweep(Npts):
    # Get Default Parameters
    default_par = get_sco2_design_parameters_Alfani_2020_update2()

    # Organize Variable Combinations
    bp_list = np.linspace(0,0.999,Npts,False)
    recomp_list = np.linspace(0,0.99,Npts, False)
    ltr_ua_frac_list = np.linspace(0.000001,0.999999,Npts, False)
    max_pressure_cycle = default_par["P_high_limit"]
    min_pressure = 7
    max_pressure = 12
    pressure_list = np.linspace(min_pressure, max_pressure, Npts, False)
    min_UA_total = 50000
    max_UA_total = 100
    UA_total_list = np.linspace(min_UA_total, max_UA_total, 5, False)
    max_UA_single_val = default_par["UA_recup_tot_des"]
    HTF_target_list = np.linspace(100, 550, Npts, False)

    default_par["cycle_config"] = 3
    dict_list = make_dict_par_list(bp_list, recomp_list, ltr_ua_frac_list, [], pressure_list, [default_par["UA_recup_tot_des"]])

    file_name = "alfani_2020_htrbp_sweep" + str(Npts) + "_results";

    combined_name = folder_location + file_name + get_time_string() + ".txt"

    start = time.time()
    result_list = run_opt_parallel(dict_list, default_par, Nproc, combined_name)
    end = time.time()

def htrbp_sweep_highUATotal(Npts):
    # Get Default Parameters
    default_par = get_sco2_design_parameters_Alfani_2020_update2()

    # Organize Variable Combinations
    bp_list = np.linspace(0,0.999,Npts,False)
    recomp_list = np.linspace(0,0.99,Npts, False)
    ltr_ua_frac_list = np.linspace(0.000001,0.999999,Npts, False)
    max_pressure_cycle = default_par["P_high_limit"]
    min_pressure = 7
    max_pressure = 12
    pressure_list = np.linspace(min_pressure, max_pressure, Npts, False)
    min_UA_total = 50000
    max_UA_total = 100
    UA_total_list = np.linspace(min_UA_total, max_UA_total, 5, False)
    max_UA_single_val = default_par["UA_recup_tot_des"]
    HTF_target_list = np.linspace(100, 550, Npts, False)

    UATotal = default_par["UA_recup_tot_des"] * 2

    default_par["cycle_config"] = 3
    dict_list = make_dict_par_list(bp_list, recomp_list, ltr_ua_frac_list, [], pressure_list, [UATotal])

    file_name = "alfani_2020_htrbp_sweep_highUATotal" + str(Npts) + "_results";

    combined_name = folder_location + file_name + get_time_string() + ".txt"

    start = time.time()
    result_list = run_opt_parallel(dict_list, default_par, Nproc, combined_name)
    end = time.time()

def htrbp_sweep_lowUATotal(Npts):
    # Get Default Parameters
    default_par = get_sco2_design_parameters_Alfani_2020_update2()

    # Organize Variable Combinations
    bp_list = np.linspace(0,0.999,Npts,False)
    recomp_list = np.linspace(0,0.99,Npts, False)
    ltr_ua_frac_list = np.linspace(0.000001,0.999999,Npts, False)
    max_pressure_cycle = default_par["P_high_limit"]
    min_pressure = 7
    max_pressure = 12
    pressure_list = np.linspace(min_pressure, max_pressure, Npts, False)
    min_UA_total = 50000
    max_UA_total = 100
    UA_total_list = np.linspace(min_UA_total, max_UA_total, 5, False)
    max_UA_single_val = default_par["UA_recup_tot_des"]
    HTF_target_list = np.linspace(100, 550, Npts, False)

    UATotal = default_par["UA_recup_tot_des"] * 0.5

    default_par["cycle_config"] = 3
    dict_list = make_dict_par_list(bp_list, recomp_list, ltr_ua_frac_list, [], pressure_list, [UATotal])

    file_name = "alfani_2020_htrbp_sweep_lowUATotal" + str(Npts) + "_results";

    combined_name = folder_location + file_name + get_time_string() + ".txt"

    start = time.time()
    result_list = run_opt_parallel(dict_list, default_par, Nproc, combined_name)
    end = time.time()


def simple_sweep(Npts):
    # Get Default Parameters
    default_par = get_sco2_design_parameters_Alfani_2020_update2()
    
    # Organize Variable Combinations
    min_pressure = 7
    max_pressure = 12
    pressure_list = np.linspace(min_pressure, max_pressure, Npts, False)

    UA_total = default_par["UA_recup_tot_des"]

    default_par["cycle_config"] = 1
    default_par["is_recomp_ok"] = 0
    default_par["design_method"] = 3
    default_par["LTR_design_code"] = 1        # 1 = UA, 2 = min dT, 3 = effectiveness
    default_par["LTR_UA_des_in"] = UA_total     # [kW/K] (required if LTR_design_code == 1)
    default_par["HTR_design_code"] = 1        # 1 = UA, 2 = min dT, 3 = effectiveness
    default_par["HTR_UA_des_in"] = 0     # [kW/K] (required if LTR_design_code == 1)
    dict_list = make_dict_par_list([], [], [], [], pressure_list)

    file_name = "alfani_2020_simple_sweep" + str(Npts) + "_results";

    combined_name = folder_location + file_name + get_time_string() + ".txt"

    start = time.time()
    result_list = run_opt_parallel(dict_list, default_par, Nproc, combined_name)
    end = time.time()

def recomp_sweep(Npts):
    # Get Default Parameters
    default_par = get_sco2_design_parameters_Alfani_2020_update2()

    # Organize Variable Combinations
    #bp_list = np.linspace(0,0.999,Npts,False)
    recomp_list = np.linspace(0,0.99,Npts, False)
    ltr_ua_frac_list = np.linspace(0.000001,0.999999,Npts, False)
    #max_pressure_cycle = default_par["P_high_limit"]
    min_pressure = 7
    max_pressure = 12
    pressure_list = np.linspace(min_pressure, max_pressure, Npts, False)
    #min_UA_total = 50000
    #max_UA_total = 100
    #UA_total_list = np.linspace(min_UA_total, max_UA_total, 5, False)
    #max_UA_single_val = default_par["UA_recup_tot_des"]
    #HTF_target_list = np.linspace(100, 550, Npts, False)

    default_par["cycle_config"] = 1
    dict_list = make_dict_par_list([], recomp_list, ltr_ua_frac_list, [], pressure_list, [default_par["UA_recup_tot_des"]])

    file_name = "alfani_2020_recomp_sweep" + str(Npts) + "_results";

    combined_name = folder_location + file_name + get_time_string() + ".txt"

    start = time.time()
    result_list = run_opt_parallel(dict_list, default_par, Nproc, combined_name)
    end = time.time()

def partial_sweep(Npts):
    # Get Default Parameters
    default_par = get_sco2_design_parameters_Alfani_2020_update2()

    # Organize Variable Combinations
    #bp_list = np.linspace(0,0.999,Npts,False)
    recomp_list = np.linspace(0.015,0.99,Npts, False)
    ltr_ua_frac_list = np.linspace(0.000001,0.999999,Npts, False)
    #max_pressure_cycle = default_par["P_high_limit"]
    min_pressure = 3
    max_pressure = 12
    pressure_list = np.linspace(min_pressure, max_pressure, Npts, False)
    #min_UA_total = 50000
    #max_UA_total = 100
    #UA_total_list = np.linspace(min_UA_total, max_UA_total, 5, False)
    #max_UA_single_val = default_par["UA_recup_tot_des"]
    #HTF_target_list = np.linspace(100, 550, Npts, False)

    default_par["cycle_config"] = 2
    default_par["eta_isen_rc"] = 0.73916766 # temporary fix (might be bug in scc code)
    dict_list = make_dict_par_list([], recomp_list, ltr_ua_frac_list, [], pressure_list, [default_par["UA_recup_tot_des"]])

    # Adjust HTR UA value if recomp == 0 (because there is no LTR)
    #for diction in dict_list:
    #    rc_frac = diction["is_recomp_ok"]
    #    if rc_frac == 0:
    #        diction["HTR_UA_des_in"] = default_par["UA_recup_tot_des"]

    #dict_list = []
    #dict_list.append({'UA_recup_tot_des':default_par["UA_recup_tot_des"],
    #                  'is_PR_fixed':-5.798773396121,
    #                  'LTR_design_code':1,
    #                  'HTR_design_code':1,
    #                  'HTR_UA_des_in':12552.64,
    #                  'LTR_UA_des_in':24299.28,
    #                  'design_method':3,
    #                  'is_recomp_ok':-0.426053})

    file_name = "alfani_2020_partial_sweep" + str(Npts) + "_results";

    combined_name = folder_location + file_name + get_time_string() + ".txt"

    start = time.time()
    result_list = run_opt_parallel(dict_list, default_par, Nproc, combined_name)
    end = time.time()



def simple_complete_sweep(Npts):
    # Get Default Parameters
    default_par = get_sco2_design_parameters_Alfani_2020_update2()
    
    # Organize Variable Combinations
    min_pressure = 7
    max_pressure = 12
    pressure_list = np.linspace(min_pressure, max_pressure, Npts, False)

    min_UA_total = 50000
    max_UA_total = 100
    UA_total_list = np.linspace(min_UA_total, max_UA_total, Npts, False)

    default_par["cycle_config"] = 1
    default_par["is_recomp_ok"] = 0
    #default_par["design_method"] = 3
    #default_par["LTR_design_code"] = 1        # 1 = UA, 2 = min dT, 3 = effectiveness
    #default_par["LTR_UA_des_in"] = UA_total     # [kW/K] (required if LTR_design_code == 1)
    #default_par["HTR_design_code"] = 1        # 1 = UA, 2 = min dT, 3 = effectiveness
    #default_par["HTR_UA_des_in"] = 0     # [kW/K] (required if LTR_design_code == 1)
    dict_list = make_dict_par_list([], [], [], [], pressure_list, UA_total_list)

    file_name = "alfani_2020_simple_COMPLETE_sweep" + str(Npts) + "_results";

    combined_name = folder_location + file_name + get_time_string() + ".txt"

    start = time.time()
    result_list = run_opt_parallel(dict_list, default_par, Nproc, combined_name)
    end = time.time()

def recomp_complete_sweep(Npts):
    # Get Default Parameters
    default_par = get_sco2_design_parameters_Alfani_2020_update2()

    # Organize Variable Combinations
    #bp_list = np.linspace(0,0.999,Npts,False)
    recomp_list = np.linspace(0,0.99,Npts, False)
    ltr_ua_frac_list = np.linspace(0.000001,0.999999,Npts, False)
    #max_pressure_cycle = default_par["P_high_limit"]
    min_pressure = 7
    max_pressure = 12
    pressure_list = np.linspace(min_pressure, max_pressure, Npts, False)
    min_UA_total = 50000
    max_UA_total = 100
    UA_total_list = np.linspace(min_UA_total, max_UA_total, Npts, False)
    #max_UA_single_val = default_par["UA_recup_tot_des"]
    #HTF_target_list = np.linspace(100, 550, Npts, False)

    default_par["cycle_config"] = 1
    dict_list = make_dict_par_list([], recomp_list, ltr_ua_frac_list, [], pressure_list, UA_total_list)

    file_name = "alfani_2020_recomp_COMPLETE_sweep" + str(Npts) + "_results";

    combined_name = folder_location + file_name + get_time_string() + ".txt"

    start = time.time()
    result_list = run_opt_parallel(dict_list, default_par, Nproc, combined_name)
    end = time.time()

def partial_complete_sweep(Npts):
    # Get Default Parameters
    default_par = get_sco2_design_parameters_Alfani_2020_update2()

    # Organize Variable Combinations
    #bp_list = np.linspace(0,0.999,Npts,False)
    recomp_list = np.linspace(0.015,0.99,Npts, False)
    ltr_ua_frac_list = np.linspace(0.000001,0.999999,Npts, False)
    #max_pressure_cycle = default_par["P_high_limit"]
    min_pressure = 0.5
    max_pressure = 12
    pressure_list = np.linspace(min_pressure, max_pressure, Npts, False)
    min_UA_total = 50000
    max_UA_total = 100
    UA_total_list = np.linspace(min_UA_total, max_UA_total, Npts, False)

    default_par["cycle_config"] = 2
    default_par["eta_isen_rc"] = 0.73916766 # temporary fix (might be bug in scc code)
    dict_list = make_dict_par_list([], recomp_list, ltr_ua_frac_list, [], pressure_list, UA_total_list)

    # Adjust HTR UA value if recomp == 0 (because there is no LTR)
    #for diction in dict_list:
    #    rc_frac = diction["is_recomp_ok"]
    #    if rc_frac == 0:
    #        diction["HTR_UA_des_in"] = default_par["UA_recup_tot_des"]

    #dict_list = []
    #dict_list.append({'UA_recup_tot_des':default_par["UA_recup_tot_des"],
    #                  'is_PR_fixed':-5.798773396121,
    #                  'LTR_design_code':1,
    #                  'HTR_design_code':1,
    #                  'HTR_UA_des_in':12552.64,
    #                  'LTR_UA_des_in':24299.28,
    #                  'design_method':3,
    #                  'is_recomp_ok':-0.426053})

    file_name = "alfani_2020_partial_COMPLETE_sweep" + str(Npts) + "_results";

    combined_name = folder_location + file_name + get_time_string() + ".txt"

    start = time.time()
    result_list = run_opt_parallel(dict_list, default_par, Nproc, combined_name)
    end = time.time()


def htrbp_tempUATotal_sweep(Npts):
    # Get Default Parameters
    default_par = get_sco2_design_parameters_Alfani_2020_update2()
    default_par["cycle_config"] = 3
    default_par["is_recomp_ok"] = 1
    default_par["is_bypass_ok"] = 1
    default_par["design_method"] = 2
    default_par["is_PR_fixed"] = 0
    default_par["des_objective"] = 2

    # Organize Variable Combinations
    min_UA_total = 36851.92
    max_UA_total = min_UA_total / 10.0
    UA_total_list = np.linspace(min_UA_total, max_UA_total, 10, True)
    HTF_target_list = np.linspace(200, 461.8, Npts, True)

    default_par["cycle_config"] = 3
    dict_list = make_dict_par_list([], [], [], [], [], UA_total_list, HTF_target_list)

    file_name = "alfani_2020_htrbp_UATotal_sweep" + str(Npts) + "_results"

    combined_name = folder_location + file_name + get_time_string() + ".txt"

    start = time.time()
    result_list = run_opt_parallel(dict_list, default_par, Nproc, combined_name)
    end = time.time()

def simple_tempUATotal_sweep(Npts):
    # Get Default Parameters
    default_par = get_sco2_design_parameters_Alfani_2020_update2()
    default_par["cycle_config"] = 1
    default_par["is_recomp_ok"] = 0
    default_par["design_method"] = 2
    default_par["is_PR_fixed"] = 0

    # Organize Variable Combinations
    min_UA_total = 36851.92
    max_UA_total = min_UA_total / 2.0
    UA_total_list = np.linspace(min_UA_total, max_UA_total, 10, True)

    HTF_inlet = default_par['T_htf_hot_des']
    HTF_min_target = 200
    HTF_max_target = 461.8
    HTF_target_list = np.linspace(HTF_min_target, HTF_max_target, Npts, True)
    HTF_min_diff_list = np.linspace(HTF_inlet - HTF_min_target, HTF_inlet - HTF_max_target, Npts, True)


    dict_list = make_dict_par_list([], [], [], [], [], UA_total_list, [], HTF_min_diff_list)

    file_name = "alfani_2020_simple_UATotal_sweep" + str(Npts) + "_results"

    combined_name = folder_location + file_name + get_time_string() + ".txt"

    start = time.time()
    result_list = run_opt_parallel(dict_list, default_par, Nproc, combined_name)
    end = time.time()

def recomp_tempUATotal_sweep(Npts):
    # Get Default Parameters
    default_par = get_sco2_design_parameters_Alfani_2020_update2()
    default_par["cycle_config"] = 1
    default_par["is_recomp_ok"] = 1
    default_par["design_method"] = 2
    default_par["is_PR_fixed"] = 0

    # Organize Variable Combinations
    min_UA_total = 36851.92
    max_UA_total = min_UA_total / 2.0
    UA_total_list = np.linspace(min_UA_total, max_UA_total, 10, True)

    HTF_inlet = default_par['T_htf_hot_des']
    HTF_min_target = 200
    HTF_max_target = 461.8
    HTF_target_list = np.linspace(HTF_min_target, HTF_max_target, Npts, True)
    HTF_min_diff_list = np.linspace(HTF_inlet - HTF_min_target, HTF_inlet - HTF_max_target, Npts, True)

    dict_list = make_dict_par_list([], [], [], [], [], UA_total_list, [], HTF_min_diff_list)

    file_name = "alfani_2020_recomp_UATotal_sweep" + str(Npts) + "_results"

    combined_name = folder_location + file_name + get_time_string() + ".txt"

    start = time.time()
    result_list = run_opt_parallel(dict_list, default_par, Nproc, combined_name)
    end = time.time()

def partial_tempUATotal_sweep(Npts):
    # Get Default Parameters
    default_par = get_sco2_design_parameters_Alfani_2020_update2()
    default_par["cycle_config"] = 2
    default_par["is_recomp_ok"] = 1
    default_par["design_method"] = 2
    default_par["is_PR_fixed"] = 0
    default_par["eta_isen_rc"] = 0.73916766 # temporary fix (might be bug in scc code)

    # Organize Variable Combinations
    min_UA_total = 36851.92
    max_UA_total = min_UA_total / 2.0
    UA_total_list = np.linspace(min_UA_total, max_UA_total, 10, True)

    HTF_inlet = default_par['T_htf_hot_des']
    HTF_min_target = 200
    HTF_max_target = 461.8
    HTF_target_list = np.linspace(HTF_min_target, HTF_max_target, Npts, True)
    HTF_min_diff_list = np.linspace(HTF_inlet - HTF_min_target, HTF_inlet - HTF_max_target, Npts, True)

    dict_list = make_dict_par_list([], [], [], [], [], UA_total_list, [], HTF_min_diff_list)

    file_name = "alfani_2020_partial_UATotal_sweep" + str(Npts) + "_results"

    combined_name = folder_location + file_name + get_time_string() + ".txt"

    start = time.time()
    result_list = run_opt_parallel(dict_list, default_par, Nproc, combined_name)
    end = time.time()




def htrbp_UATotal_sweep(Npts):
    # Get Default Parameters
    default_par = get_sco2_design_parameters_Alfani_2020_update2()
    default_par["cycle_config"] = 3
    default_par["is_recomp_ok"] = 1
    default_par["is_bypass_ok"] = 1
    default_par["design_method"] = 2
    default_par["is_PR_fixed"] = 0
    default_par["des_objective"] = 2

    # Organize Variable Combinations
    min_UA_total = 50000
    max_UA_total = 100
    UA_total_list = np.linspace(min_UA_total, max_UA_total, 20, False)
    HTF_target_list = np.linspace(100, 550, Npts, False)

    default_par["cycle_config"] = 3
    dict_list = make_dict_par_list([], [], [], [], [], UA_total_list, HTF_target_list)

    file_name = "alfani_2020_htrbp_UATotal_sweep" + str(Npts) + "_results"

    combined_name = folder_location + file_name + get_time_string() + ".txt"

    start = time.time()
    result_list = run_opt_parallel(dict_list, default_par, Nproc, combined_name)
    end = time.time()

def simple_UATotal_sweep(Npts):
    # Get Default Parameters
    default_par = get_sco2_design_parameters_Alfani_2020_update2()
    default_par["cycle_config"] = 1
    default_par["is_recomp_ok"] = 0
    default_par["design_method"] = 2
    default_par["is_PR_fixed"] = 0

    # Organize Variable Combinations
    min_UA_total = 50000
    max_UA_total = 100
    UA_total_list = np.linspace(min_UA_total, max_UA_total, Npts, False)

    dict_list = make_dict_par_list([], [], [], [], [], UA_total_list, [])

    file_name = "alfani_2020_simple_UATotal_sweep" + str(Npts) + "_results"

    combined_name = folder_location + file_name + get_time_string() + ".txt"

    start = time.time()
    result_list = run_opt_parallel(dict_list, default_par, Nproc, combined_name)
    end = time.time()

def recomp_UATotal_sweep(Npts):
    # Get Default Parameters
    default_par = get_sco2_design_parameters_Alfani_2020_update2()
    default_par["cycle_config"] = 1
    default_par["is_recomp_ok"] = 1
    default_par["design_method"] = 2
    default_par["is_PR_fixed"] = 0

    # Organize Variable Combinations
    min_UA_total = 50000
    max_UA_total = 100
    UA_total_list = np.linspace(min_UA_total, max_UA_total, Npts, False)

    dict_list = make_dict_par_list([], [], [], [], [], UA_total_list, [])

    file_name = "alfani_2020_recomp_UATotal_sweep" + str(Npts) + "_results"

    combined_name = folder_location + file_name + get_time_string() + ".txt"

    start = time.time()
    result_list = run_opt_parallel(dict_list, default_par, Nproc, combined_name)
    end = time.time()

def partial_UATotal_sweep(Npts):
    # Get Default Parameters
    default_par = get_sco2_design_parameters_Alfani_2020_update2()
    default_par["cycle_config"] = 2
    default_par["is_recomp_ok"] = 1
    default_par["design_method"] = 2
    default_par["is_PR_fixed"] = 0
    default_par["eta_isen_rc"] = 0.73916766 # temporary fix (might be bug in scc code)

    # Organize Variable Combinations
    min_UA_total = 50000
    max_UA_total = 100
    UA_total_list = np.linspace(min_UA_total, max_UA_total, Npts, False)

    dict_list = make_dict_par_list([], [], [], [], [], UA_total_list, [])

    file_name = "alfani_2020_partial_UATotal_sweep" + str(Npts) + "_results"

    combined_name = folder_location + file_name + get_time_string() + ".txt"

    start = time.time()
    result_list = run_opt_parallel(dict_list, default_par, Nproc, combined_name)
    end = time.time()

def htrbp_tempsweep(Npts):
    # Get Default Parameters
    default_par = get_sco2_design_parameters_Alfani_2020_update2()
    
    # Organize Variable Combinations
    HTF_target_list = np.linspace(100, 550, Npts, False)

    default_par["cycle_config"] = 3
    default_par["is_recomp_ok"] = 1
    default_par["des_objective"] = 2
    default_par["is_bypass_ok"] = 1
    default_par["design_method"] = 2
    default_par["is_PR_fixed"] = 0
    dict_list = make_dict_par_list([], [], [], [], [], [], HTF_target_list)

    file_name = "alfani_2020_htrbp_tempsweep" + str(Npts) + "_results";

    combined_name = folder_location + file_name + get_time_string() + ".txt"

    start = time.time()
    result_list = run_opt_parallel(dict_list, default_par, 1, combined_name)
    end = time.time()

def recomp_tempsweep(Npts):
    # Get Default Parameters
    default_par = get_sco2_design_parameters_Alfani_2020_update2()
    
    # Organize Variable Combinations
    min_dT_list = np.linspace(100, 300, Npts, False)
    default_par["cycle_config"] = 1
    default_par["is_recomp_ok"] = 1
    default_par["des_objective"] = 2
    default_par["design_method"] = 2
    default_par["is_PR_fixed"] = 0
    dict_list = make_dict_par_list([], [], [], [], [], [], [], min_dT_list)

    file_name = "alfani_2020_recomp_tempsweep" + str(Npts) + "_results";

    combined_name = folder_location + file_name + get_time_string() + ".txt"

    start = time.time()
    result_list = run_opt_parallel(dict_list, default_par, Nproc, combined_name)
    end = time.time()

def partial_tempsweep(Npts):
    # Get Default Parameters
    default_par = get_sco2_design_parameters_Alfani_2020_update2()
    
    # Organize Variable Combinations
    min_dT_list = np.linspace(100, 300, Npts, False)
    default_par["cycle_config"] = 2
    default_par["is_recomp_ok"] = 1
    default_par["des_objective"] = 2
    default_par["design_method"] = 2
    default_par["is_PR_fixed"] = 0
    default_par["eta_isen_rc"] = 0.73916766
    dict_list = make_dict_par_list([], [], [], [], [], [], [], min_dT_list)

    file_name = "alfani_2020_partial_tempsweep" + str(Npts) + "_results";

    combined_name = folder_location + file_name + get_time_string() + ".txt"

    start = time.time()
    result_list = run_opt_parallel(dict_list, default_par, Nproc, combined_name)
    end = time.time()

def Run_Final_Simulations_Alfani_2020():

    # Run Alfani 2020 Paper
    if True:
        default_par_paper = get_sco2_design_parameters_Alfani_2020_update2()

        # Make Cycle class
        c_sco2 = sco2_solve.C_sco2_sim(3)

        # Overwrite Variables
        c_sco2.overwrite_default_design_parameters(default_par_paper)

        # Solve
        c_sco2.solve_sco2_case()

        result_list_paper = get_result_list_v2(default_par_paper, c_sco2.m_solve_dict, c_sco2.m_solve_success)
        labeled_result_array = []
        label_list = get_label_list()
        labeled_result_array.append(label_list)
        labeled_result_array.append(result_list_paper)

        solve_dict = c_sco2.m_solve_dict
        solve_dict['cycle_config'] = 1
        c_plot = cy_plt.C_sco2_cycle_TS_plot(c_sco2.m_solve_dict)
        c_plot.is_annotate = False
        c_plot.plot_new_figure()

        #file_name = "alfani_2020_paper_results";
        y = 0
        #combined_name = folder_location + file_name + get_time_string() + ".txt"
        #design_point_tools.write_string_array(combined_name, labeled_result_array, '\t')

    # Run Optimal Cases
    if True:
        
        # HTR BP @ Paper Temp
        if False:
            default_par_htrbp_opt = get_sco2_design_parameters_Alfani_2020_update2()
            default_par_htrbp_opt["cycle_config"] = 3
            default_par_htrbp_opt["is_recomp_ok"] = 1
            default_par_htrbp_opt["is_bypass_ok"] = 1
            default_par_htrbp_opt["design_method"] = 2
            default_par_htrbp_opt["is_PR_fixed"] = 0
            default_par_htrbp_opt["T_bypass_target"] = 460.980751771068
            default_par_htrbp_opt["des_objective"] = 2

            # Make Cycle class
            c_sco2 = sco2_solve.C_sco2_sim(3)

            # Overwrite Variables
            c_sco2.overwrite_default_design_parameters(default_par_htrbp_opt)

            # Solve
            c_sco2.solve_sco2_case()

            solve_dict = c_sco2.m_solve_dict
            solve_dict['cycle_config'] = 1
            #c_plot = cy_plt.C_sco2_TS_PH_plot(c_sco2.m_solve_dict)
            c_plot = cy_plt.C_sco2_cycle_TS_plot(c_sco2.m_solve_dict)
            c_plot.is_annotate = False
            c_plot.plot_new_figure()


            result_list_htrbp_opt = get_result_list_v2(default_par_htrbp_opt, c_sco2.m_solve_dict, c_sco2.m_solve_success)
            labeled_result_array = []
            label_list = get_label_list()
            labeled_result_array.append(label_list)
            labeled_result_array.append(result_list_htrbp_opt)

            file_name = "alfani_2020_opt_htrbp_results";

            combined_name = folder_location + file_name + get_time_string() + ".txt"
            design_point_tools.write_string_array(combined_name, labeled_result_array, '\t')

        # HTR BP @ Recomp Temp
        if False:
            default_par_htrbp_opt = get_sco2_design_parameters_Alfani_2020_update2()
            default_par_htrbp_opt["cycle_config"] = 3
            default_par_htrbp_opt["is_recomp_ok"] = 1
            default_par_htrbp_opt["is_bypass_ok"] = 1
            default_par_htrbp_opt["design_method"] = 2
            default_par_htrbp_opt["is_PR_fixed"] = 0
            default_par_htrbp_opt["T_bypass_target"] = 461.28527422354284
            default_par_htrbp_opt["des_objective"] = 2

            # Make Cycle class
            c_sco2 = sco2_solve.C_sco2_sim(3)

            # Overwrite Variables
            c_sco2.overwrite_default_design_parameters(default_par_htrbp_opt)

            # Solve
            c_sco2.solve_sco2_case()

            result_list_htrbp_opt = get_result_list_v2(default_par_htrbp_opt, c_sco2.m_solve_dict, c_sco2.m_solve_success)
            labeled_result_array = []
            label_list = get_label_list()
            labeled_result_array.append(label_list)
            labeled_result_array.append(result_list_htrbp_opt)

            file_name = "alfani_2020_opt_htrbp_recomptemp_results";

            combined_name = folder_location + file_name + get_time_string() + ".txt"
            design_point_tools.write_string_array(combined_name, labeled_result_array, '\t')

        # Simple
        if False:
            default_par_simple_opt = get_sco2_design_parameters_Alfani_2020_update2()
            default_par_simple_opt["cycle_config"] = 1
            default_par_simple_opt["is_recomp_ok"] = 0
            default_par_simple_opt["design_method"] = 2
            default_par_simple_opt["is_PR_fixed"] = 0

            # Make Cycle class
            c_sco2 = sco2_solve.C_sco2_sim(1)

            # Overwrite Variables
            c_sco2.overwrite_default_design_parameters(default_par_simple_opt)

            # Solve
            c_sco2.solve_sco2_case()

            result_list_simple_opt = get_result_list_v2(default_par_simple_opt, c_sco2.m_solve_dict, c_sco2.m_solve_success)
            labeled_result_array = []
            label_list = get_label_list()
            labeled_result_array.append(label_list)
            labeled_result_array.append(result_list_simple_opt)

            file_name = "alfani_2020_opt_simple_results";

            combined_name = folder_location + file_name + get_time_string() + ".txt"
            design_point_tools.write_string_array(combined_name, labeled_result_array, '\t')

        # Recompression
        if False:
            default_par_recomp_opt = get_sco2_design_parameters_Alfani_2020_update2()
            default_par_recomp_opt["cycle_config"] = 1
            default_par_recomp_opt["is_recomp_ok"] = 1
            default_par_recomp_opt["design_method"] = 2
            default_par_recomp_opt["is_PR_fixed"] = 0

            # Make Cycle class
            c_sco2 = sco2_solve.C_sco2_sim(1)

            # Overwrite Variables
            c_sco2.overwrite_default_design_parameters(default_par_recomp_opt)

            # Solve
            c_sco2.solve_sco2_case()

            result_list_recomp_opt = get_result_list_v2(default_par_recomp_opt, c_sco2.m_solve_dict, c_sco2.m_solve_success)
            labeled_result_array = []
            label_list = get_label_list()
            labeled_result_array.append(label_list)
            labeled_result_array.append(result_list_recomp_opt)

            file_name = "alfani_2020_opt_recomp_results";

            combined_name = folder_location + file_name + get_time_string() + ".txt"
            design_point_tools.write_string_array(combined_name, labeled_result_array, '\t')
        
        # Recompression
        if False:
            default_par_recomp_opt = get_sco2_design_parameters_Alfani_2020_update2()
            default_par_recomp_opt["cycle_config"] = 1
            default_par_recomp_opt["is_recomp_ok"] = 1
            default_par_recomp_opt["design_method"] = 2
            default_par_recomp_opt["is_PR_fixed"] = 0

            # Make Cycle class
            c_sco2 = sco2_solve.C_sco2_sim(1)

            # Overwrite Variables
            c_sco2.overwrite_default_design_parameters(default_par_recomp_opt)

            # Solve
            c_sco2.solve_sco2_case()

            result_list_recomp_opt = get_result_list_v2(default_par_recomp_opt, c_sco2.m_solve_dict, c_sco2.m_solve_success)
            labeled_result_array = []
            label_list = get_label_list()
            labeled_result_array.append(label_list)
            labeled_result_array.append(result_list_recomp_opt)

            file_name = "alfani_2020_opt_recomp_results";

            combined_name = folder_location + file_name + get_time_string() + ".txt"
            design_point_tools.write_string_array(combined_name, labeled_result_array, '\t')

        # Partial
        if True:
            default_par_partial_opt = get_sco2_design_parameters_Alfani_2020_update2()
            default_par_partial_opt["cycle_config"] = 2
            default_par_partial_opt["is_recomp_ok"] = 1
            default_par_partial_opt["design_method"] = 2
            default_par_partial_opt["is_PR_fixed"] = 0
            default_par_partial_opt["eta_isen_rc"] = 0.73916766 # temporary fix (might be bug in scc code)

            # Make Cycle class
            c_sco2 = sco2_solve.C_sco2_sim(2)

            # Overwrite Variables
            c_sco2.overwrite_default_design_parameters(default_par_partial_opt)

            # Solve
            c_sco2.solve_sco2_case()

            result_list_partial_opt = get_result_list_v2(default_par_partial_opt, c_sco2.m_solve_dict, c_sco2.m_solve_success)
            labeled_result_array = []
            label_list = get_label_list()
            labeled_result_array.append(label_list)
            labeled_result_array.append(result_list_partial_opt)

            file_name = "alfani_2020_opt_partial_results";

            combined_name = folder_location + file_name + get_time_string() + ".txt"
            design_point_tools.write_string_array(combined_name, labeled_result_array, '\t')

    # Run Sweeps
    if False:
        
        # HTR BP
        if True:
            htrbp_sweep(20)

        # Simple
        if True:
            simple_sweep(20)

        # Recomp
        if True:
            recomp_sweep(20)

        # Partial
        if True:
            partial_sweep(20)

    # Run UA Sweeps
    if False:
        
        # HTR BP
        if True:
            htrbp_UATotal_sweep(100)

        # Simple
        if True:
            simple_UATotal_sweep(500)

        # Recomp
        if True:
            recomp_UATotal_sweep(500)

        # Partial
        if True:
            partial_UATotal_sweep(500)

    # Run Temperature Sweeps
    if False:

        # HTR BP Temperature Sweep
        if True:
            htrbp_tempsweep(10)

        # Recomp Temperature Sweep
        if True:
            recomp_tempsweep(100)

        # Partial Temperature Sweep
        if True:
            partial_tempsweep(100)

def Run_Final_Simulations_Alfani_2020_BONUS():
    
    #htrbp_tempUATotal_sweep(50)
    #partial_tempUATotal_sweep(20)
    #recomp_tempUATotal_sweep(20)
    #simple_tempUATotal_sweep(20)
    

    # Run Complete Sweep
    if False:
        # Simple
        if False:
            simple_complete_sweep(10)

        # Recomp
        if True:
            recomp_complete_sweep(5)

        # Partial
        if True:
            partial_complete_sweep(5)



def Run_Anomalous_Recomp():
    # Alfani 2020 Recompression Case
    
    default_par = get_sco2_design_parameters_Alfani_2020_update2()
    default_par["cycle_config"] = 1
    default_par["is_recomp_ok"] = -0.029699
    default_par["is_PR_fixed"] = -7.8
    default_par["design_method"] = 3

    default_par["HTR_n_sub_hx"] = 50
    default_par["LTR_n_sub_hx"] = 50

    # LTR
    default_par["LTR_design_code"] = 1        # 1 = UA, 2 = min dT, 3 = effectiveness
    default_par["LTR_UA_des_in"] = 30587.069277732795     # [kW/K] (required if LTR_design_code == 1)

    # HTR
    default_par["HTR_design_code"] = 1        # 1 = UA, 2 = min dT, 3 = effectiveness
    default_par["HTR_UA_des_in"] = 6264.850722267205     # [kW/K] (required if LTR_design_code == 1)


    # Make Cycle class
    c_sco2 = sco2_solve.C_sco2_sim(1)

    # Overwrite Variables
    c_sco2.overwrite_default_design_parameters(default_par)

    # Solve
    c_sco2.solve_sco2_case()

    # Save c_sco2 Case
    folder = "C:\\Users\\tbrown2\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020\\UA Assigned Anomaly\\"
    if False:
        c_sco2.m_also_save_csv = True
        c_sco2.save_m_solve_dict(folder + "anomaly_sco2_result.txt")

        anomaly_input_file = folder + "anomaly_input.txt"
        design_point_tools.write_dict(anomaly_input_file, default_par, '\t')

    result_list = get_result_list_v2(default_par, c_sco2.m_solve_dict, c_sco2.m_solve_success)
    labeled_result_array = []
    label_list = get_result_list_v2([],[],True,True)
    labeled_result_array.append(label_list)
    labeled_result_array.append(result_list)

    anomaly_result_file = folder + "anomaly_ud50_result.txt"
    design_point_tools.write_string_array(anomaly_result_file, labeled_result_array, '\t')

def Run_Anomalous_Partial():
     # Partial
    default_par_partial = get_sco2_design_parameters_Alfani_2020_update2()
    default_par_partial["cycle_config"] = 2
    default_par_partial["eta_isen_rc"] = 0.73916766 # temporary fix (might be bug in scc code)

    # Recomp Fraction
    default_par_partial["is_recomp_ok"] = -0.01

    # LTR + HTR
    default_par_partial["design_method"] = 2
    default_par_partial["UA_recup_tot_des"] = 36851.92
    
    # Pressure
    default_par_partial["is_PR_fixed"] = -4.8

    # Make Cycle class
    c_sco2 = sco2_solve.C_sco2_sim(2)

    # Overwrite Variables
    c_sco2.overwrite_default_design_parameters(default_par_partial)

    # Solve
    c_sco2.solve_sco2_case()

    result_list_partial_opt = get_result_list_v2(default_par_partial, c_sco2.m_solve_dict, c_sco2.m_solve_success)
    labeled_result_array = []
    label_list = get_label_list()
    labeled_result_array.append(label_list)
    labeled_result_array.append(result_list_partial_opt)

    file_name = "alfani2020_partial_anomaly";

    combined_name = folder_location + file_name + get_time_string() + "1.txt"
    design_point_tools.write_string_array(combined_name, labeled_result_array, '\t')
    
def Run_Anomalous_Partial_NaN():
     # Partial
    default_par_partial = get_sco2_design_parameters_Alfani_2020_update2()
    default_par_partial["cycle_config"] = 2
    default_par_partial["eta_isen_rc"] = 0.73916766 # temporary fix (might be bug in scc code)

    # Recomp Fraction
    default_par_partial["is_recomp_ok"] = -0.01

    # LTR + HTR
    default_par_partial["design_method"] = 2
    default_par_partial["UA_recup_tot_des"] = 36851.92
    
    # Pressure
    default_par_partial["is_PR_fixed"] = -6.6

    # Make Cycle class
    c_sco2 = sco2_solve.C_sco2_sim(2)

    # Overwrite Variables
    c_sco2.overwrite_default_design_parameters(default_par_partial)

    # Solve
    c_sco2.solve_sco2_case()

    result_list_partial_opt = get_result_list_v2(default_par_partial, c_sco2.m_solve_dict, c_sco2.m_solve_success)
    labeled_result_array = []
    label_list = get_label_list()
    labeled_result_array.append(label_list)
    labeled_result_array.append(result_list_partial_opt)

    file_name = "alfani2020_partial_NaN_anomaly";

    combined_name = folder_location + file_name + get_time_string() + "1.txt"
    design_point_tools.write_string_array(combined_name, labeled_result_array, '\t')

def Run_Anomalous_Partial_cases():
     # Partial
    default_par_partial = get_sco2_design_parameters_Alfani_2020_update2()
    default_par_partial["cycle_config"] = 2
    default_par_partial["eta_isen_rc"] = 0.73916766 # temporary fix (might be bug in scc code)

    # Recomp Fraction
    default_par_partial["is_recomp_ok"] = -0.01

    # LTR + HTR
    default_par_partial["design_method"] = 2
    default_par_partial["UA_recup_tot_des"] = 36851.92
    
    # Pressure
    default_par_partial["is_PR_fixed"] = -4.8

    # Make Cycle class
    c_sco2 = sco2_solve.C_sco2_sim(2)

    # Overwrite Variables
    c_sco2.overwrite_default_design_parameters(default_par_partial)

    # Solve
    c_sco2.solve_sco2_case()

    result_list_partial_opt = get_result_list_v2(default_par_partial, c_sco2.m_solve_dict, c_sco2.m_solve_success)
    labeled_result_array = []
    label_list = get_label_list()
    labeled_result_array.append(label_list)
    labeled_result_array.append(result_list_partial_opt)

    file_name = "alfani2020_partial_anomaly";

    combined_name = folder_location + file_name + get_time_string() + "1.txt"
    design_point_tools.write_string_array(combined_name, labeled_result_array, '\t')


def Run_Bypass_Opening(Npts):
    # Get Default Parameters
    default_par = get_sco2_design_parameters_Alfani_2020_update2()
    default_par["cycle_config"] = 3
    default_par["is_recomp_ok"] = -0.3425844738692358
    default_par["design_method"] = 3
    default_par["LTR_design_code"] = 1
    default_par["LTR_UA_des_in"] = 21954
    default_par["HTR_design_code"] = 1
    default_par["HTR_UA_des_in"] = 14897.6
    default_par["is_PR_fixed"] = -7.937


    # Organize Variable Combinations
    bp_list = np.linspace(0,0.999,Npts,False)

    default_par["cycle_config"] = 3
    dict_list = make_dict_par_list(bp_list, [], [], [], [], [])

    file_name = "alfani_2020_htrbp_opening_sweep" + str(Npts) + "_results";

    combined_name = folder_location + file_name + get_time_string() + ".txt"

    start = time.time()
    result_list = run_opt_parallel(dict_list, default_par, Nproc, combined_name)
    end = time.time()


def test_Alfani_2024():
    # Get Default Parameters
    default_par = get_sco2_design_parameters_Alfani_2024()

    # Make Cycle class
    c_sco2 = sco2_solve.C_sco2_sim(3)

    # Overwrite Variables
    c_sco2.overwrite_default_design_parameters(default_par)

    c_sco2.solve_sco2_case()

    # Initialize
    labeled_result_array = []
    label_list = get_result_list_v2([],[],True,True)
    labeled_result_array.append(label_list)

    debug_list = get_result_list_v2(c_sco2.m_des_par_base, c_sco2.m_solve_dict, c_sco2.m_solve_success)

    labeled_result_array.append(debug_list);

    file_name = "alfani_2024_results";

    combined_name = folder_location + file_name + get_time_string() + ".txt"

    design_point_tools.write_string_array(combined_name, labeled_result_array, '\t')

def sweep_Alfani_2024(Npts):
    # Get Default Parameters
    default_par = get_sco2_design_parameters_Alfani_2024()

    # Want sco2 into Turbine: 600 -> 740 deg C
    # T_htf_hot_des: 620 -> 760 (20 deg C approach)
    T_htf_hot_des_min = 620 # deg C
    T_htf_hot_des_max = 760 # deg C
    T_htf_hot_des_list = np.linspace(T_htf_hot_des_min, T_htf_hot_des_max, Npts, True)

    # T_bypass_target: 179 -> 215 deg C
    T_bypass_target_min = 179 # deg C
    T_bypass_target_max = 215 # deg C
    T_bypass_target_list = np.linspace(T_bypass_target_min, T_bypass_target_max, Npts, True)

    # HTR_min_dT_des_in: 5 -> 75 deg C
    HTR_min_dT_des_in_min = 5 # deg C
    HTR_min_dT_des_in_max = 75 # deg C
    HTR_min_dT_des_in_list = np.linspace(HTR_min_dT_des_in_min, HTR_min_dT_des_in_max, Npts, True)

    dict_list = make_dict_list_free({"T_htf_hot_des":T_htf_hot_des_list,
                                     "T_bypass_target":T_bypass_target_list,
                                     "HTR_min_dT_des_in":HTR_min_dT_des_in_list})
    
    file_name = "alfani_2024_results";
    combined_name = folder_location + file_name + str(Npts) + get_time_string() + ".txt"

    start = time.time()
    result_list = run_opt_parallel(dict_list, default_par, Nproc, combined_name)
    end = time.time()

def run_err_case():
    # Get Default Parameters
    default_par = get_sco2_design_parameters_Alfani_2020_update2()
    default_par["is_bypass_ok"] = -0.751667
    default_par["is_recomp_ok"] = -0.001572
    default_par["LTR_design_code"] = 1
    default_par["HTR_design_code"] = 1
    default_par["LTR_UA_des_in"] = 0
    default_par["HTR_UA_des_in"] = 15000.07
    default_par["is_PR_fixed"] = -7.808405
    default_par["HTR_n_sub_hx"] = 50000
    default_par["LTR_n_sub_hx"] = 50000

    # Make Cycle class
    c_sco2 = sco2_solve.C_sco2_sim(3)

    # Overwrite Variables
    c_sco2.overwrite_default_design_parameters(default_par)

    c_sco2.solve_sco2_case()

    # Initialize
    labeled_result_array = []
    label_list = get_result_list_v2([],[],True,True)
    labeled_result_array.append(label_list)

    debug_list = get_result_list_v2(c_sco2.m_des_par_base, c_sco2.m_solve_dict, c_sco2.m_solve_success)

    labeled_result_array.append(debug_list);

    file_name = "alfani_2024_results";

    combined_name = folder_location + file_name + get_time_string() + ".txt"

    design_point_tools.write_string_array(combined_name, labeled_result_array, '\t')

def run_STRANGE_case():
    # Get Default Parameters
    default_par = get_sco2_design_parameters_Alfani_2020_update2()
    default_par["is_bypass_ok"] = -0.6993
    default_par["is_recomp_ok"] = 0
    default_par["LTR_design_code"] = 1
    default_par["HTR_design_code"] = 1
    default_par["LTR_UA_des_in"] = 3.685192e-02
    default_par["HTR_UA_des_in"] = 36851.88314808
    default_par["is_PR_fixed"] = -8
    #default_par["HTR_n_sub_hx"] = 50000
    #default_par["LTR_n_sub_hx"] = 50000

    # Make Cycle class
    c_sco2 = sco2_solve.C_sco2_sim(3)

    # Overwrite Variables
    c_sco2.overwrite_default_design_parameters(default_par)

    c_sco2.solve_sco2_case()

    # Initialize
    labeled_result_array = []
    label_list = get_result_list_v2([],[],True,True)
    labeled_result_array.append(label_list)

    debug_list = get_result_list_v2(c_sco2.m_des_par_base, c_sco2.m_solve_dict, c_sco2.m_solve_success)

    labeled_result_array.append(debug_list);

    file_name = "STRANGE_CASE_RUN";

    combined_name = folder_location + file_name + get_time_string() + ".txt"

    design_point_tools.write_string_array(combined_name, labeled_result_array, '\t')
   
def run_fixed_eff_case(Npts):
    # Get Default Parameters
    default_par = get_sco2_design_parameters_Alfani_2020_update2()


    # Organize Variable Combinations
    bp_list = np.linspace(0,0.999,Npts,False)
    recomp_list = np.linspace(0,0.99,Npts, False)
    ltr_ua_frac_list = np.linspace(0.000001,0.999999,Npts, False)
    max_pressure_cycle = default_par["P_high_limit"]
    min_pressure = 7
    max_pressure = 12
    pressure_list = np.linspace(min_pressure, max_pressure, Npts, False)
    max_UA_single_val = default_par["UA_recup_tot_des"]
    HTF_target_list = np.linspace(100, 550, Npts, False)

    default_par["cycle_config"] = 3
    default_par["HT_recup_eff_max"] = 0.9
    default_par["LT_recup_eff_max"] = 0.9
    dict_list = make_dict_par_list(bp_list, recomp_list, ltr_ua_frac_list, [], pressure_list, [default_par["UA_recup_tot_des"]])

    file_name = "alfani_2020_htrbp_fixed_eff" + str(Npts) + "_results";

    combined_name = folder_location + file_name + get_time_string() + ".txt"

    start = time.time()
    result_list = run_opt_parallel(dict_list, default_par, Nproc, combined_name)
    end = time.time()

    return combined_name

def htrbp_sweep_special(Npts):
    # Get Default Parameters
    default_par = get_sco2_design_parameters_Alfani_2020_update2()

    # Organize Variable Combinations
    bp_list = np.linspace(0,0.999,Npts,False)
    recomp_list = np.linspace(0,0.99,Npts, False)
    ltr_ua_frac_list = np.linspace(0.000001,0.999999,Npts, False)
    max_pressure_cycle = default_par["P_high_limit"]
    min_pressure = 3
    max_pressure = 12
    pressure_list = np.linspace(min_pressure, max_pressure, Npts, False)
    min_UA_total = 50000
    max_UA_total = 100
    UA_total_list = np.linspace(min_UA_total, max_UA_total, 5, False)
    max_UA_single_val = default_par["UA_recup_tot_des"]
    HTF_target_list = np.linspace(100, 550, Npts, False)

    default_par["cycle_config"] = 3
    #default_par["is_recomp_ok"] = 0
    dict_list = make_dict_par_list(bp_list, recomp_list, ltr_ua_frac_list, [], pressure_list, [default_par["UA_recup_tot_des"]])

    file_name = "alfani_2020_htrbp_sweep" + str(Npts) + "_results";

    combined_name = folder_location + file_name + get_time_string() + ".txt"

    start = time.time()
    result_list = run_opt_parallel(dict_list, default_par, Nproc, combined_name)
    end = time.time()

def run_alfani_2020():

    default_par_paper = get_sco2_design_parameters_Alfani_2020_update2()
    default_par_paper['cycle_config'] = 1
    # Make Cycle class
    c_sco2 = sco2_solve.C_sco2_sim(1)

    # Overwrite Variables
    c_sco2.overwrite_default_design_parameters(default_par_paper)

    # Solve
    c_sco2.solve_sco2_case()

    result_list_paper = get_result_list_v2(default_par_paper, c_sco2.m_solve_dict, c_sco2.m_solve_success)
    labeled_result_array = []
    label_list = get_label_list()
    labeled_result_array.append(label_list)
    labeled_result_array.append(result_list_paper)

    file_name = "Alfani_2020";

    combined_name = folder_location + file_name + get_time_string() + ".txt"

    design_point_tools.write_string_array(combined_name, labeled_result_array, '\t')

    solve_dict = c_sco2.m_solve_dict
    solve_dict['cycle_config'] = 1
    c_plot = cy_plt.C_sco2_cycle_TS_plot(c_sco2.m_solve_dict)
    c_plot.is_annotate = True
    c_plot.plot_new_figure()

    #design_point_tools.plot_dict_from_file(combined_name, "eta_thermal_calc", "T_htf_cold_des")

def run_alfani_2020_mult_UA():
    # Get Default Parameters
    default_par = get_sco2_design_parameters_Alfani_2020_update2()
    default_par["design_method"] = 2 

    # Make Cycle class
    c_sco2 = sco2_solve.C_sco2_sim(3)

    # Overwrite Variables
    c_sco2.overwrite_default_design_parameters(default_par)

    c_sco2.solve_sco2_case()

    # Initialize
    labeled_result_array = []
    label_list = get_result_list_v2([],[],True,True)
    labeled_result_array.append(label_list)
    debug_list = get_result_list_v2(c_sco2.m_des_par_base, c_sco2.m_solve_dict, c_sco2.m_solve_success)
    labeled_result_array.append(debug_list);

    ############### SECOND RUN

    # Overwrite Variables
    default_par["UA_recup_tot_des"] = default_par["UA_recup_tot_des"] / 2.0
    c_sco2.overwrite_default_design_parameters(default_par)

    c_sco2.solve_sco2_case()
    debug_list_2 = get_result_list_v2(c_sco2.m_des_par_base, c_sco2.m_solve_dict, c_sco2.m_solve_success)
    labeled_result_array.append(debug_list_2);



    file_name = "alfani2020_2UA";
    combined_name = folder_location + file_name + get_time_string() + ".txt"
    design_point_tools.write_string_array(combined_name, labeled_result_array, '\t')

    design_point_tools.plot_dict_from_file(combined_name)

def run_refactor_test_case():
    # Get Default Parameters
    default_par = get_sco2_design_parameters_Alfani_2020_update2()
    #default_par["is_bypass_ok"] = -0.6993
    default_par["is_bypass_ok"] = 1
    default_par["is_recomp_ok"] = 1
    default_par["LTR_design_code"] = 1
    default_par["HTR_design_code"] = 1
    default_par["LTR_UA_des_in"] = 3.685192e-02
    default_par["HTR_UA_des_in"] = 36851.88314808
    default_par["is_PR_fixed"] = -8
    #default_par["HTR_n_sub_hx"] = 50000
    #default_par["LTR_n_sub_hx"] = 50000

    # Make Cycle class
    c_sco2 = sco2_solve.C_sco2_sim(3)

    # Overwrite Variables
    c_sco2.overwrite_default_design_parameters(default_par)

    c_sco2.solve_sco2_case()

    # Initialize
    labeled_result_array = []
    label_list = get_result_list_v2([],[],True,True)
    labeled_result_array.append(label_list)

    debug_list = get_result_list_v2(c_sco2.m_des_par_base, c_sco2.m_solve_dict, c_sco2.m_solve_success)

    labeled_result_array.append(debug_list);

    file_name = "refactor_run";

    combined_name = folder_location + file_name + get_time_string() + ".txt"

    design_point_tools.write_string_array(combined_name, labeled_result_array, '\t')
   
def run_alfani_2020_htrbp():

    default_par_paper = get_sco2_design_parameters_Alfani_2020_update2()

    default_par_paper["design_method"] = 1

    #default_par_paper["design_method"] = 2
    default_par_paper["is_bypass_ok"] = -0.1
    default_par_paper["is_recomp_ok"] = -0.3
    default_par_paper["des_objective"] = 2
    default_par_paper["is_PR_fixed"] = 0
    default_par_paper["is_P_high_fixed"] = 0

    #default_par_paper["cycle_config"] = 1
    #default_par_paper["is_P_high_fixed"] = 0

    # Make Cycle class
    c_sco2 = sco2_solve.C_sco2_sim(3)

    # Overwrite Variables
    c_sco2.overwrite_default_design_parameters(default_par_paper)

    # Solve
    c_sco2.solve_sco2_case()

    result_list_paper = get_result_list_v2(default_par_paper, c_sco2.m_solve_dict, c_sco2.m_solve_success)
    labeled_result_array = []
    label_list = get_label_list()
    labeled_result_array.append(label_list)
    labeled_result_array.append(result_list_paper)

    file_name = "Alfani_2020";

    combined_name = folder_location + file_name + get_time_string() + ".txt"

    #design_point_tools.plot_dict_from_file(combined_name, "eta_thermal_calc", "T_htf_cold_des")

def test_tsf_alfani_2020():

    default_par = get_sco2_tsf_design_parameters_Alfani_2021()
    #default_par["HTR_design_code"] = 3  # Test define UA

    #default_par['is_turbine_split_ok'] = 1

    # Make Cycle class
    c_sco2 = sco2_solve.C_sco2_sim(3)

    # Overwrite Variables
    c_sco2.overwrite_default_design_parameters(default_par)

    # Solve
    c_sco2.solve_sco2_case()


    file_name = "test_json_";

    combined_name = folder_location + file_name + get_time_string()


    c_sco2.m_also_save_csv = True
    c_sco2.save_m_solve_dict(combined_name)
    

    x = "this is a test"

def test_tsf_alfani_2020_bad():

    default_par = get_sco2_tsf_design_parameters_Alfani_2021()
    del default_par['design_method']
    
    #default_par['is_turbine_split_ok'] = 1

    # Make Cycle class
    c_sco2 = sco2_solve.C_sco2_sim(3)

    # Overwrite Variables
    c_sco2.overwrite_default_design_parameters(default_par)

    del c_sco2.m_des_par_default['design_method']
    # Solve
    c_sco2.solve_sco2_case()


    file_name = "test_json_";

    combined_name = folder_location + file_name + get_time_string()


    c_sco2.m_also_save_csv = True
    c_sco2.save_m_solve_dict(combined_name)
    

    x = "this is a test"


def test_tsf_sweep():

    default_par = get_sco2_tsf_design_parameters_Alfani_2021()

    # Make Cycle Collection Class
    sim_collection = sco2_solve.C_sco2_sim_result_collection()

    # Make Cycle class
    c_sco2 = sco2_solve.C_sco2_sim(3)

    for i in range(10):
        default_par["W_dot_net_des"] = 1 * i

        # Overwrite Variables
        c_sco2.overwrite_default_design_parameters(default_par)

        # Solve
        c_sco2.solve_sco2_case()

        # Add results to cycle collection class
        sim_collection.add(c_sco2)

    file_name = "test_collection"

    combined_name = folder_location + file_name + get_time_string() + ".csv"
    sim_collection.write_to_csv(combined_name)


def test_tsf_sweep_correct():
    default_par = get_sco2_tsf_design_parameters_Alfani_2021()

    # Organize Variable Combinations
    Npts = 10
    ltr_ua_frac_list = np.linspace(0.1,0.9,Npts, True)
    min_pressure = 5
    max_pressure = 15
    pressure_list = np.linspace(min_pressure, max_pressure, Npts, True)
    max_UA_single_val = default_par["UA_recup_tot_des"]
    split_frac_list = np.linspace(0.3, 0.7, Npts, True)

    dict_list = make_dict_par_list(ltr_ua_frac_list=ltr_ua_frac_list, 
                                   UA_total_list=[max_UA_single_val], 
                                   split_frac_list=split_frac_list,
                                   pres_ratio_list=pressure_list)

    solve_collection = run_opt_parallel_solve_dict(dict_list, default_par, Nproc)

    file_name = "test_collection"
    combined_name = folder_location + file_name + get_time_string() + ".csv"
    solve_collection.write_to_csv(combined_name)

    finished = ""
    
def run_bad_tsf():
    default_par = get_sco2_tsf_design_parameters_Alfani_2021()
    
    default_par["is_PR_fixed"] = -11.667
    default_par["design_method"] = 3
    default_par["LTR_design_code"] = 1
    default_par["LTR_UA_des_in"] = 0.0368519
    default_par["HTR_design_code"] = 1
    default_par["HTR_UA_des_in"] = 36851.883
    default_par["is_turbine_split_ok"] = -0.1

    # Make Cycle class
    c_sco2 = sco2_solve.C_sco2_sim(3)

    # Overwrite Variables
    c_sco2.overwrite_default_design_parameters(default_par)

    # Solve
    c_sco2.solve_sco2_case()

    i = 5

def test_UA_tsf():

    default_par = get_sco2_tsf_design_parameters_Alfani_2021()
    #default_par["HTR_design_code"] = 1

    # Make Cycle Collection Class
    sim_collection = sco2_solve.C_sco2_sim_result_collection()

    # Make Cycle class
    c_sco2 = sco2_solve.C_sco2_sim(3)

    for i in range(10):
        
        #default_par["HTR_UA_des_in"] = (i + 1) * 1000

        # Overwrite Variables
        c_sco2.overwrite_default_design_parameters(default_par)

        # Solve
        c_sco2.solve_sco2_case()

        # Add results to cycle collection class
        sim_collection.add(c_sco2.m_solve_dict)

    file_name = "test_collection"

    combined_name = folder_location + file_name + get_time_string() + ".csv"
    sim_collection.write_to_csv(combined_name)

def run_G3P3_tsf_sweep(n_par):

    # Define constant parameters
    default_par = get_sco2_G3P3()
    default_par["cycle_config"] = 4

    # Organize Variable Combinations
    Npts = n_par
    ltr_ua_frac_list = np.linspace(0,1.0,Npts, True)
    min_pressure = 5
    max_pressure = 13
    pressure_list = np.linspace(min_pressure, max_pressure, Npts, True)
    UA_total_list = np.linspace(100, 5000, Npts, True)
    split_frac_list = np.linspace(0, 0.7, Npts, True)
    default_par["dT_PHX_cold_approach"] = default_par["dT_PHX_hot_approach"]

    dict_list = make_dict_par_list(ltr_ua_frac_list=ltr_ua_frac_list, 
                                   UA_total_list=UA_total_list, 
                                   split_frac_list=split_frac_list,
                                   pres_ratio_list=pressure_list)

    solve_collection = run_opt_parallel_solve_dict(dict_list, default_par, Nproc)

    file_name = "TSF_G3P3_collection"
    combined_name = folder_location + file_name  + '_' + str(n_par) + '_' + get_time_string() + ".csv"
    solve_collection.write_to_csv(combined_name)

    finished = ""

def run_G3P3_recomp_sweep(n_par):

    # Define constant parameters
    default_par = get_sco2_G3P3()
    default_par["cycle_config"] = 1

    # Organize Variable Combinations
    Npts = n_par
    ltr_ua_frac_list = np.linspace(0,1.0,Npts, True)
    min_pressure = 5
    max_pressure = 13
    pressure_list = np.linspace(min_pressure, max_pressure, Npts, True)
    UA_total_list = np.linspace(100, 5000, Npts, True)
    recomp_frac_list = np.linspace(0, 0.7, Npts, True)
    default_par["dT_PHX_cold_approach"] = default_par["dT_PHX_hot_approach"]

    dict_list = make_dict_par_list(ltr_ua_frac_list=ltr_ua_frac_list, 
                                   UA_total_list=UA_total_list, 
                                   recomp_list=recomp_frac_list,
                                   pres_ratio_list=pressure_list)

    solve_collection = run_opt_parallel_solve_dict(dict_list, default_par, Nproc)

    file_name = "recomp_G3P3_collection"
    combined_name = folder_location + file_name  + '_' + str(n_par) + '_' +  get_time_string() + ".csv"
    solve_collection.write_to_csv(combined_name)

    finished = ""

def run_G3P3_htrbp_sweep(n_par):

    # Define constant parameters
    default_par = get_sco2_G3P3()
    default_par["cycle_config"] = 3
    default_par["T_bypass_target"] = 0 # (not used)
    default_par["deltaT_bypass"] = 0
    default_par["dT_PHX_cold_approach"] = default_par["dT_PHX_hot_approach"]

    # Organize Variable Combinations
    Npts = n_par
    ltr_ua_frac_list = np.linspace(0,1.0,Npts, True)
    min_pressure = 5
    max_pressure = 13
    pressure_list = np.linspace(min_pressure, max_pressure, Npts, True)
    UA_total_list = np.linspace(100, 5000, Npts, True)
    recomp_frac_list = np.linspace(0, 0.7, Npts, True)
    bp_frac_list = np.linspace(0, 0.99, Npts, True)

    dict_list = make_dict_par_list(ltr_ua_frac_list=ltr_ua_frac_list, 
                                   UA_total_list=UA_total_list, 
                                   recomp_list=recomp_frac_list,
                                   pres_ratio_list=pressure_list,
                                   bp_list=bp_frac_list)

    solve_collection = run_opt_parallel_solve_dict(dict_list, default_par, Nproc)

    print("Saving htrbp results...")
    file_name = "htrbp_G3P3_collection"
    combined_name = folder_location + file_name  + '_' + str(n_par) + '_' + get_time_string() + ".csv"
    solve_collection.write_to_csv(combined_name)

    print("htrbp finished")

    finished = ""

def run_G3P3_partial_sweep(n_par):

    # Define constant parameters
    default_par = get_sco2_G3P3()
    default_par["cycle_config"] = 2
    default_par["dT_PHX_cold_approach"] = default_par["dT_PHX_hot_approach"]

    # Organize Variable Combinations
    Npts = n_par
    ltr_ua_frac_list = np.linspace(0,1.0,Npts, True)
    min_pressure = 1
    max_pressure = 13
    pressure_list = np.linspace(min_pressure, max_pressure, Npts, True)
    UA_total_list = np.linspace(100, 5000, Npts, True)
    recomp_frac_list = np.linspace(0, 0.7, Npts, True)

    dict_list = make_dict_par_list(ltr_ua_frac_list=ltr_ua_frac_list, 
                                   UA_total_list=UA_total_list, 
                                   recomp_list=recomp_frac_list,
                                   pres_ratio_list=pressure_list)

    solve_collection = run_opt_parallel_solve_dict(dict_list, default_par, Nproc)

    file_name = "partial_G3P3_collection"
    combined_name = folder_location + file_name + '_' + str(n_par) + '_' + get_time_string() + ".csv"
    solve_collection.write_to_csv(combined_name)

    finished = ""


def run_G3P3_sweeps(n_par):
    run_G3P3_partial_sweep(n_par)
    run_G3P3_recomp_sweep(n_par)
    run_G3P3_tsf_sweep(n_par)
    run_G3P3_htrbp_sweep(n_par)
    
    
    
def test_bad_G3P3_case():

    if True:
        default_par = get_sco2_G3P3()
        default_par["cycle_config"] = 3
        default_par["T_bypass_target"] = 0 # (not used)
        default_par["deltaT_bypass"] = 0
        default_par["dT_PHX_cold_approach"] = default_par["dT_PHX_hot_approach"]

        # Design Variables
        default_par["design_method"] = 3
        default_par["LTR_design_code"] = 1
        default_par["LTR_UA_des_in"] = 29481.54
        default_par["HTR_design_code"] = 1
        default_par["HTR_UA_des_in"] = 7370.38
        default_par["is_recomp_ok"] = -0.35
        default_par["is_PR_fixed"] = -5
        default_par["is_bypass_ok"] = -0.45

        # Make Cycle class
        c_sco2 = sco2_solve.C_sco2_sim(3)

        # Overwrite Variables
        c_sco2.overwrite_default_design_parameters(default_par)

        # Solve
        c_sco2.solve_sco2_case()
        c_sco2.m_solve_dict['cmod_success'] = c_sco2.m_solve_success

        # Make Cycle Collection Class
        sim_collection = sco2_solve.C_sco2_sim_result_collection()
        sim_collection.add(c_sco2.m_solve_dict)
        file_name = "bad htrbp_G3P3_collection"
        combined_name = folder_location + file_name + get_time_string() + ".csv"
        sim_collection.write_to_csv(combined_name)

        i = 5

    if False:
        default_par = get_sco2_G3P3()
        default_par["cycle_config"] = 3
        default_par["T_bypass_target"] = 0 # (not used)
        default_par["deltaT_bypass"] = 0
        default_par["dT_PHX_cold_approach"] = default_par["dT_PHX_hot_approach"]

        # Design Variables
        default_par["design_method"] = 3
        default_par["LTR_design_code"] = 1
        default_par["LTR_UA_des_in"] = 22111.15
        default_par["HTR_design_code"] = 1
        default_par["HTR_UA_des_in"] = 14740.77
        default_par["is_recomp_ok"] = 0
        default_par["is_PR_fixed"] = -12
        default_par["is_bypass_ok"] = 0

        # Make Cycle class
        c_sco2 = sco2_solve.C_sco2_sim(3)

        # Overwrite Variables
        c_sco2.overwrite_default_design_parameters(default_par)

        # Solve
        c_sco2.solve_sco2_case()

        i = 5


# Main Script

if __name__ == "__main__":

    run_G3P3_sweeps(10)
    #run_bad_tsf()
    #test_bad_G3P3_case()
    #run_G3P3_sweeps(10)
    #run_G3P3_partial_sweep()
    #run_G3P3_recomp_sweep()
    #run_G3P3_tsf_sweep()
    #test_tsf_alfani_2020()
    #test_tsf_alfani_2020_bad()
    #test_tsf_sweep_correct()
    #test_UA_tsf()
    #run_bad_tsf()
    #test_tsf_sweep_correct()
    #test_tsf_alfani_2020()
    #run_alfani_2020_htrbp()
    #run_alfani_2020_mult_UA()
    #run_alfani_2020()
    #file_name = run_fixed_eff_case(5)
    #design_point_tools.plot_dict_from_file(file_name, "eta_thermal_calc", "T_htf_cold_des", "recup_total_UA_calculated")
    #htrbp_sweep_special(4)
    #run_refactor_test_case()
    #run_err_case()
    #sweep_Alfani_2024(10)
    #sweep_Alfani_2024(20)
    #sweep_Alfani_2024(50)
    #Run_Final_Simulations_Alfani_2020()
    #Run_Final_Simulations_Alfani_2020_BONUS()
    #Run_Anomalous_Recomp()
    #Run_Anomalous_Partial()
    #Run_Anomalous_Partial_NaN()
    #Run_Recomp_TempTarget_Sweep(10)
    #Run_Bypass_Opening(100)
    #htrbp_tempsweep(5)
    #htrbp_sweep_highUATotal(2)
    #htrbp_sweep_lowUATotal(2)

    # Run Individual Cases
    if False:
        
        # Alfani 2020 Recompression Case
        if False:
            
            default_par = get_sco2_design_parameters_Alfani_2020_update2()
            default_par["cycle_config"] = 1
            default_par["is_recomp_ok"] = 1
            default_par["is_PR_fixed"] = 0
            default_par["design_method"] = 2
            # Make Cycle class
            c_sco2 = sco2_solve.C_sco2_sim(1)

            # Overwrite Variables
            c_sco2.overwrite_default_design_parameters(default_par)

            # Solve
            c_sco2.solve_sco2_case()

            result_list = get_result_list_v2(default_par, c_sco2.m_solve_dict, c_sco2.m_solve_success)
            labeled_result_array = []
            label_list = get_result_list_v2([],[],True,True)
            labeled_result_array.append(label_list)
            labeled_result_array.append(result_list)

            file_name = "alfani_2020_opt_recomp_results";

            combined_name = folder_location + file_name + get_time_string() + ".txt"

            write_string_array(combined_name, labeled_result_array, '\t')

        # Alfani 2020 Partial Cooling Case
        if False:
            
            default_par = get_sco2_design_parameters_Alfani_2020_update2()
            #default_par = get_sco2_design_parameters()
            default_par["cycle_config"] = 2
            default_par["is_recomp_ok"] = 1
            default_par["eta_isen_rc"] = 0.73916766 # temporary fix (might be bug in scc code)
            default_par["is_PR_fixed"] = 0
            default_par["design_method"] = 2

            # Make Cycle class
            c_sco2 = sco2_solve.C_sco2_sim(2)

            # Overwrite Variables
            c_sco2.overwrite_default_design_parameters(default_par)

            # Solve
            c_sco2.solve_sco2_case()

            result_list = get_result_list_v2(default_par, c_sco2.m_solve_dict, c_sco2.m_solve_success)
            labeled_result_array = []
            label_list = get_result_list_v2([],[],True,True)
            labeled_result_array.append(label_list)
            labeled_result_array.append(result_list)

            file_name = "alfani_2020_opt_partial_results";

            combined_name = folder_location + file_name + get_time_string() + ".txt"

            c_sco2.m_also_save_csv = True
            c_sco2.save_m_solve_dict(combined_name)   # Save design solution dictionary
            
            write_string_array(combined_name, labeled_result_array, '\t')

            #c_plot = cy_plt.C_sco2_TS_PH_plot(c_sco2.m_solve_dict)
            #c_plot.is_save_plot = True
            #c_plot.file_name = combined_name + "pic"
            #c_plot.plot_new_figure()

            #x = json.load(open(combined_name+".json"))
            #c_plot2 = cy_plt.C_sco2_TS_PH_plot(x)
            #c_plot2.is_save_plot = True
            #c_plot2.file_name = combined_name + "pic"
            #c_plot2.plot_new_figure()


            asf = 0

        # Alfani 2020 Optimal HTR Bypass Case
        if False:
            default_par = get_sco2_design_parameters_Alfani_2020_update2()
            default_par["cycle_config"] = 3
            default_par["is_recomp_ok"] = 1
            default_par["is_bypass_ok"] = 1
            default_par["design_method"] = 2
            #default_par["is_PR_fixed"] = -8.297
            default_par["T_bypass_target"] = 460.980751771068

            # Make Cycle class
            c_sco2 = sco2_solve.C_sco2_sim(3)

            # Overwrite Variables
            c_sco2.overwrite_default_design_parameters(default_par)

            # Solve
            c_sco2.solve_sco2_case()

            result_list = get_result_list_v2(default_par, c_sco2.m_solve_dict, c_sco2.m_solve_success)
            labeled_result_array = []
            label_list = get_label_list()
            labeled_result_array.append(label_list)
            labeled_result_array.append(result_list)

            file_name = "alfani_2020_opt_htrbp_results";

            combined_name = folder_location + file_name + get_time_string() + ".txt"
            write_string_array(combined_name, labeled_result_array, '\t')

        # Alfani 2020 Faulty HTR BP Case
        if False:
            default_par = get_sco2_design_parameters_Alfani_2020_update2()
            default_par["cycle_config"] = 3
            default_par["is_recomp_ok"] = 0
            default_par["is_bypass_ok"] = -0.421667
            default_par["design_method"] = 2
            default_par["des_objective"] = 2
            default_par["is_PR_fixed"] = -7.844265
            default_par["T_bypass_target"] = 370
 
            # Make Cycle class
            c_sco2 = sco2_solve.C_sco2_sim(3)

            # Overwrite Variables
            c_sco2.overwrite_default_design_parameters(default_par)

            # Solve
            c_sco2.solve_sco2_case()

            result_list = get_result_list_v2(default_par, c_sco2.m_solve_dict, c_sco2.m_solve_success)
            labeled_result_array = []
            label_list = get_result_list_v2([],[],True,True)
            labeled_result_array.append(label_list)
            labeled_result_array.append(result_list)

            file_name = "alfani_2020_faulty_result";
            combined_name = folder_location + file_name + get_time_string() + ".txt"

            write_string_array(combined_name, labeled_result_array, '\t')

        # Alfani 2019 HTR BP
        if True:
            default_par = get_sco2_design_parameters_Alfani_2019()
            
            # Make Cycle class
            c_sco2 = sco2_solve.C_sco2_sim(3)

            # Overwrite Variables
            c_sco2.overwrite_default_design_parameters(default_par)

            # Solve
            c_sco2.solve_sco2_case()

            result_list = get_result_list_v2(default_par, c_sco2.m_solve_dict, c_sco2.m_solve_success)
            labeled_result_array = []
            label_list = get_result_list_v2([],[],True,True)
            labeled_result_array.append(label_list)
            labeled_result_array.append(result_list)

            file_name = "alfani_2019_opt_recomp_results";

            combined_name = folder_location + file_name + get_time_string() + ".txt"

            design_point_tools.write_string_array(combined_name, labeled_result_array, '\t')


        y = 0

    # Test des objective == 2 Script
    if False:
        default_par = get_sco2_design_parameters_Alfani_2020_update2()
        default_par["cycle_config"] = 3
        default_par["is_recomp_ok"] = 1
        default_par["des_objective"] = 2
        default_par["is_bypass_ok"] = -0.2
        # Make Cycle class
        c_sco2 = sco2_solve.C_sco2_sim(3)

        # Overwrite Variables
        c_sco2.overwrite_default_design_parameters(default_par)

        # Solve
        c_sco2.solve_sco2_case()

        result_list = get_result_list_v2(default_par, c_sco2.m_solve_dict, c_sco2.m_solve_success)
        labeled_result_array = []
        label_list = get_result_list_v2([],[],True,True)
        labeled_result_array.append(label_list)
        labeled_result_array.append(result_list)

        file_name = "alfani_2020_test_results";
        combined_name = folder_location + file_name + get_time_string() + ".txt"

        write_string_array(file_name, labeled_result_array, '\t')

    # Optimize variables (with bypass outside others)
    if False:
        default_par = get_sco2_design_parameters_Alfani_2020_update2()
        default_par["cycle_config"] = 3
        default_par["is_recomp_ok"] = 1
        default_par["des_objective"] = 2
        default_par["is_bypass_ok"] = 1
        default_par["is_PR_fixed"] = 0
        default_par["design_method"] = 2

        # Make Cycle class
        c_sco2 = sco2_solve.C_sco2_sim(3)

        # Overwrite Variables
        c_sco2.overwrite_default_design_parameters(default_par)

        # Solve
        c_sco2.solve_sco2_case()

        result_list = get_result_list_v2(default_par, c_sco2.m_solve_dict, c_sco2.m_solve_success)
        labeled_result_array = []
        label_list = get_result_list_v2([],[],True,True)
        labeled_result_array.append(label_list)
        labeled_result_array.append(result_list)

        file_name = "alfani_2020_test_results";
        combined_name = folder_location + file_name + get_time_string() + ".txt"

        write_string_array(combined_name, labeled_result_array, '\t')


    # Test des objective == 2 Script
    # Manually Sweep with objective == 2
    if False:
        result_list_list = []
        default_par = get_sco2_design_parameters_Alfani_2020_update2()
        default_par["cycle_config"] = 3
        default_par["is_recomp_ok"] = 1
        default_par["des_objective"] = 2
        default_par["design_method"] = 2
        default_par["is_PR_fixed"] = 0

        bp_list = np.arange(0, 0.99, 0.01).tolist()
        
        for bp in bp_list:

            default_par["is_bypass_ok"] = -1.0 * bp
            # Make Cycle class
            c_sco2 = sco2_solve.C_sco2_sim(3)

            # Overwrite Variables
            c_sco2.overwrite_default_design_parameters(default_par) 

            # Solve
            c_sco2.solve_sco2_case()

            result_list_list.append(get_result_list_v2(default_par, c_sco2.m_solve_dict, c_sco2.m_solve_success))
        
        
        labeled_result_array = []
        label_list = get_result_list_v2([],[],True,True)
        labeled_result_array.append(label_list)

        for result_list in result_list_list:
            labeled_result_array.append(result_list)

        file_name = "bp_full_sweep_test_results";
        combined_name = folder_location + file_name + get_time_string() + ".txt"

        write_string_array(combined_name, labeled_result_array, '\t')


    #run_sweeeeeeep()
    #run_hardcode_bp_sweep()





    


# Original Script
if(False):

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
    print("\nDid the simulation code with " 
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


     