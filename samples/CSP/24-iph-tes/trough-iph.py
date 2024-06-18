import csv
import json
import numpy as np
import sys
import os
import copy

CSP_Dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.append(CSP_Dir)

core_Path = os.path.join(CSP_Dir, 'sco2_analysis_python_V2/core')
sys.path.append(core_Path)

import ssc_inout_v2 as ssc_sim

sam_iph_dict_base = json.load(open("IPH_with_oil_field.json"))

# dictionaries are "pointers", so use "copy.deepcopy(dict_name)" to copy/move dicts between variable names

is_des_pt_only = True

if(is_des_pt_only):

    sim_type = 2
    sam_iph_dict_des_pt = copy.deepcopy(sam_iph_dict_base)
    sam_iph_dict_des_pt["sim_type"] = 2

    iph_des_solved_dict = ssc_sim.cmod_trough_iph_lcoh_from_dict(sam_iph_dict_des_pt, True)  # ssc_sim.cmod_mspt_from_dict(sam_mspt_dict)

    is_success = iph_des_solved_dict["cmod_success"]    
    print ('SSC simulation(s) successful = ', is_success)

    if (is_success == 1):

        W_dot_pump = iph_des_solved_dict["W_dot_pump_SS"]
        print ('Design pumping power = ', W_dot_pump)

    else:
        print ('DESIGN FAIL FAIL FAIL FAIL FAIL')

else:

    sam_iph_dict_annual_sim = copy.deepcopy(sam_iph_dict_base)

    iph_solved_dict = ssc_sim.cmod_trough_iph_lcoh_from_dict(sam_iph_dict_annual_sim, True)  # ssc_sim.cmod_mspt_from_dict(sam_mspt_dict)

    is_success = iph_solved_dict["cmod_success"]    
    print ('SSC simulation(s) successful = ', is_success)
    if (is_success == 1):
        annual_energy = iph_solved_dict["annual_energy"]
        print ('Annual energy (year 1) = ', annual_energy)

        print("total installed cost = ", iph_solved_dict["total_installed_cost"])
        
        print("lcoe_fcr = ", iph_solved_dict["lcoe_fcr"], " cents/kwh")


    else:
        print ('ANNUAL SIM FAIL FAIL FAIL FAIL FAIL')
