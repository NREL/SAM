##################################################
## Set relative file paths ##
import csv
import sys
import os
import numpy as np
import json

import multiprocessing
import time

from datetime import datetime
import random

absFilePath = os.path.abspath(__file__)
fileDir = os.path.dirname(os.path.abspath(__file__))
parentDir = os.path.dirname(fileDir)
newPath = os.path.join(parentDir, 'core')

sys.path.append(newPath)

import sco2_cycle_ssc as sco2_solve

import sco2_plots as cy_plt

##################################################
##################################################

def get_sco2_design_parameters():
    des_par = {}
    des_par["htf"] = 17                 # [-] Solar salt
    des_par["T_htf_hot_des"] = 574.0    # [C] HTF design hot temperature (PHX inlet) [cite: sunshot]
    des_par["dT_PHX_hot_approach"] = 20.0  # [C/K] default 20. Temperature difference between hot HTF and turbine inlet [cite: neises/turchi]
    des_par["dT_PHX_cold_approach"] = 20  # [C/K] default 20. Temperature difference between cold HTF and cold CO2 PHX inlet [enforces CR = 1]
    des_par["T_amb_des"] = 40.0  # [C] Ambient temperature at design [cite: neises/turchi]
    des_par["dT_mc_approach"] = 6.0  # [C] Use 6 here per [Neises & Turchi 19]. Temperature difference between main compressor CO2 inlet and ambient air
    des_par["site_elevation"] = 588  # [m] Elevation of Daggett, CA. Used to size air cooler...
    des_par["W_dot_net_des"] = 115.0  # [MWe] Design cycle power output (no cooling parasitics)

    des_par["design_method"] = 3  # [-] 1 = specify efficiency, 2 = specify total recup UA, 3 = Specify each recuperator design (see inputs below)
    des_par["eta_thermal_des"] = -1  # [-] Power cycle thermal efficiency, not used here
    des_par["UA_recup_tot_des"] = -1  # [kW/K]

    des_par["cycle_config"] = 1  # [1] = RC, [2] = PC

    des_par["is_recomp_ok"] = 1  # [-] Use simple cycle for now. 1 = Yes, 0 = simple cycle only
    des_par["is_P_high_fixed"] = 1  # [-] 0 = No, optimize. 1 = Yes
    des_par["is_PR_fixed"] = 0  # [-] 0 = No, >0 = Yes
    des_par["des_objective"] = 1  # [-] 2 = hit min deltaT then max efficiency, != 2 = max efficiency
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
    deltaP_recup_HP = 0.0056  # [-] 0.0056 = 0.14[MPa]/25[MPa]
    deltaP_recup_LP = 0.0311  # [-] 0.0311 = 0.28[MPa]/9[MPa]
    # LTR
    des_par["LTR_design_code"] = 2  # 1 = UA, 2 = min dT, 3 = effectiveness
    des_par["LTR_UA_des_in"] = -1  # [kW/K] (required if LTR_design_code == 1 and design_method == 3) not used
    des_par[
        "LTR_min_dT_des_in"] = 10.0  # [C] (required if LTR_design_code == 2 and design_method == 3) "reasonable value" from Neises/Turchi
    des_par["LTR_eff_des_in"] = -1  # [-] (required if LTR_design_code == 3 and design_method == 3)
    des_par["LT_recup_eff_max"] = eff_max  # [-] Maximum effectiveness low temperature recuperator
    des_par["LTR_LP_deltaP_des_in"] = deltaP_recup_LP  # [-]
    des_par["LTR_HP_deltaP_des_in"] = deltaP_recup_HP  # [-]
    # HTR
    des_par["HTR_design_code"] = 2  # 1 = UA, 2 = min dT, 3 = effectiveness
    des_par["HTR_UA_des_in"] = -1  # [kW/K] (required if LTR_design_code == 1 and design_method == 3)
    des_par[
        "HTR_min_dT_des_in"] = 10.0  # [C] (required if LTR_design_code == 2 and design_method == 3) "reasonable value" from Neises/Turchi
    des_par["HTR_eff_des_in"] = -1  # [-] (required if LTR_design_code == 3 and design_method == 3)
    des_par["HT_recup_eff_max"] = eff_max  # [-] Maximum effectiveness high temperature recuperator
    des_par["HTR_LP_deltaP_des_in"] = deltaP_recup_LP  # [-]
    des_par["HTR_HP_deltaP_des_in"] = deltaP_recup_HP  # [-]
    # PHX
    des_par["PHX_co2_deltaP_des_in"] = deltaP_recup_HP  # [-]
    # Air Cooler
    des_par[
        "deltaP_cooler_frac"] = 0.005  # [-] Fraction of CO2 inlet pressure that is design point cooler CO2 pressure drop
    des_par[
        "fan_power_frac"] = 0.02  # [-] Fraction of net cycle power consumed by air cooler fan. 2% here per Turchi et al.
    # Default
    des_par[
        "deltaP_counterHX_frac"] = -1  # [-] Fraction of CO2 inlet pressure that is design point counterflow HX (recups & PHX) pressure drop

    # Off Design
    des_par["od_rel_tol"] = 3          # [-] Baseline off-design relative convergence tolerance exponent (10^-od_rel_tol)

    return des_par

def od_matrix_var_names():
    
    var_names = ["m_dot_HTF","T_amb",
                 "W_dot","eta",
                 "t_m_dot","t_T_in","t_P_in","t_T_out","t_P_out",
                 "PHX_co2_deltaP",
                 "PHX_T_co2_in","PHX_P_co2_in","HTR_eff","HTR_HP_deltaP",
                 "LTR_eff","LTR_HP_deltaP",
                 "MC_T_in","MC_m_dot","MC_eta",
                 "RC_eta",
                 "cooler_tot_W_dot_fan"]
    
    return var_names
"***************************"
"***************************"

def sco2_off_design_des_par_in(data_case_in):
    
    "Main simulation parameters"
    # Instantiate sco2 cycle simulation class
    c_sco2 = sco2_solve.C_sco2_sim(1)  # Initialize as same cycle config as specified above

    # Get default design parameters. These are different than the "baseline" default parameters in "sco2_cycle_ssc.py"
    sco2_des_par_default = data_case_in[0]
    c_sco2.overwrite_default_design_parameters(sco2_des_par_default)

    od_case_in = data_case_in[1]

    od_case_local = []
    od_case_local.append([od_case_in[0], od_case_in[1], od_case_in[2], 1.0, 1.0])

    mod_base_dict = {"od_cases": od_case_local}  # [[mod_base_dict["T_htf_hot_des"],1.0,mod_base_dict["T_amb_des"],-1.0]]

    c_sco2.overwrite_des_par_base(mod_base_dict)  # Overwrite baseline design parameters
    c_sco2.solve_sco2_case()  # Run design simulation

    print("OD eta = ", c_sco2.m_solve_dict["eta_thermal_od"])

    return c_sco2.m_solve_dict

if __name__ == '__main__':

    # Set number of threads
    n_threads = int(multiprocessing.cpu_count()/2 - 1)

    time_start = time.time()

    # Get default design parameters. These are different than the "baseline" default parameters in "sco2_cycle_ssc.py"
    sco2_des_par_in = get_sco2_design_parameters()

    # Setup string for naming files
    str_date = datetime.today().strftime('%Y_%m_%d')
        # assign random number in case difference between simulations aren't captured by below parameters
    rnd_str = "RND" + "".join(["{}".format(random.randint(0, 9)) for num in range(0, 3)])
    T_amb_des_str = "T_amb_des" + '{:.1f}'.format(sco2_des_par_in["T_amb_des"])
    dT_mc_approach_str = "dT_mc_app" + '{:.1f}'.format(sco2_des_par_in["dT_mc_approach"])
    fan_frac_power_str = "fan_power" + '{:.1f}'.format(sco2_des_par_in["fan_power_frac"])
    des_sim_label_str = str_date + "_" + T_amb_des_str + "_" + dT_mc_approach_str + "_" + fan_frac_power_str + "_" + rnd_str

    # Get design cycle inputs
    T_htf_hot_des = sco2_des_par_in["T_htf_hot_des"]
    T_amb_des = sco2_des_par_in["T_amb_des"]
    m_dot_htf_ND_des = 1.0

    # Set up off-design cases
        # HTF mass flow
    m_dot_htf_ND_low = 0.5
    m_dot_htf_ND_high = 1.05
    n_m_dot_htf_ND = 12
        # HTF temperature
    T_htf_delta_cold = 30.
    T_htf_delta_hot = 15.
    n_T_htf_hot = 4
        # Ambient temperature
    T_amb_low = 0
    T_amb_high = max(45, T_amb_des + 5)
    n_T_amb = int(T_amb_high - T_amb_low + 1)

        # Shaft speed settings
    f_N_rc = 1.0
    f_N_mc = 1.0

    udpc_od_cases = sco2_solve.generate_udpc_inputs(m_dot_htf_ND_low, m_dot_htf_ND_high, m_dot_htf_ND_des, n_m_dot_htf_ND,
            T_htf_delta_cold, T_htf_delta_hot, T_htf_hot_des, n_T_htf_hot,
            T_amb_low, T_amb_high, T_amb_des, n_T_amb,
            f_N_rc, f_N_mc)

    udpc_od_cases_comb = []
    for i in range(len(udpc_od_cases)):
        udpc_od_cases_comb.append([sco2_des_par_in, udpc_od_cases[i]])

    # Uncomment block to test off-design function with manual off-design entries
    # od_case = [T_htf_hot_des, m_dot_htf_ND_des, T_amb_des, 1.0, 1.0]
    # od_case2 = [T_htf_hot_des, 0.9, T_amb_des, 1.0, 1.0]
    # od_cases_comb = [[sco2_des_par_default, od_case],[sco2_des_par_default, od_case2]]
    #     # Test off-design function 
    # od_result = []
    # od_result.append(sco2_off_design_des_par_in([sco2_des_par_default, od_case]))
    # od_result.append(sco2_off_design_des_par_in([sco2_des_par_default, od_case2]))

    # Use multi-processing for off-design cases
    
    print("\nUsing ", n_threads, " threads\n")
    p = multiprocessing.Pool(n_threads)
    od_result = p.map(sco2_off_design_des_par_in, udpc_od_cases_comb)


    # 'od_result' is a list of with each index containing a dictionary
    #                  with results from an off-design simultion
    #                 We want to combine the list into a single dictionary
    od_combined = {}
    for i, i_od_case in enumerate(od_result):

        for key in od_result[0]:
            
            if i == 0:

                od_combined[key] = []

            i_data = i_od_case[key]

            str_type, data_type, l_d1, l_d2 = sco2_solve.get_entry_data_properties(i_data)

            i_data_out = i_data
            if str_type == "list" and l_d1 == 1:
                i_data_out = i_data[0]

            elif str_type == "matrix" and l_d1 == 1:
                i_data_out = i_data[0]

            od_combined[key].append(i_data_out)

    # Add udpc metrics for downstream processing
    od_combined["udpc_n_T_htf"] = n_T_htf_hot
    od_combined["udpc_n_T_amb"] = n_T_amb
    od_combined["udpc_n_m_dot_htf"] = n_m_dot_htf_ND

    time_end = time.time()
    od_combined["python_sim_time"] = time_end - time_start

    # Write json
    j_output = open(des_sim_label_str + "_UDPC" + ".json", 'w')            
    json.dump(od_combined, j_output)
    j_output.close()

    # Write csv
    with open(des_sim_label_str + "_UDPC" + ".csv",'w', newline='') as f:
        w = csv.writer(f)
        sco2_solve.write_dictionary_to_csv(od_combined, w)
        f.close()    

    # Post-process for UDPC SAM inputs and plots
    sco2_solve.process_sco2_udpc_dict(od_combined, des_sim_label_str + "_UDPC")