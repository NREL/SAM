##################################################
## Set relative file paths ##
import csv

import sys
import os
import numpy as np
import json

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


######################################
######################################

"Generate data for SAM's User Defined Power Cycle Model"
# Instantiate sco2 cycle simulation class
c_sco2 = sco2_solve.C_sco2_sim(1)           # Initialize as same cycle config as specified above

# Get default design parameters. These are different than the "baseline" default parameters in "sco2_cycle_ssc.py"
sco2_des_par_default = get_sco2_design_parameters()
c_sco2.overwrite_default_design_parameters(sco2_des_par_default)

# Setup string for naming files
des_sim_label_str = "T_amb_des" + '{:.1f}'.format(sco2_des_par_default["T_amb_des"])

mod_base_dict = {"od_generate_udpc": [1.0]}
mod_base_dict["od_rel_tol"] = 2

c_sco2.overwrite_des_par_base(mod_base_dict)  # Overwrite baseline design parameters
c_sco2.solve_sco2_case()  # Run design simulation

print(c_sco2.m_solve_dict["eta_thermal_calc"])
print("\nDid the simulation code with "
      "modified design parameters solve successfully = ", c_sco2.m_solve_success)

c_sco2.m_also_save_csv = True
c_sco2.save_m_solve_dict("" + des_sim_label_str + "_UDPC_mspt_default")  # Save design

solved_dict = c_sco2.m_solve_dict

c_sco2.process_sco2_udpc_dict(solved_dict, "mspt_default_temps")

