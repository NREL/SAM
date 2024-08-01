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

solution_mode = "design_point"
solution_mode = "annual_sim"
#solution_mode = "LCOH_demo"
#solution_mode = "CST_sim_then_hybrid_lcoh"

if(solution_mode == "design_point"):

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

elif(solution_mode == "LCOH_demo"):

    sam_iph_dict_lcoh_demo = copy.deepcopy(sam_iph_dict_base)
    # want sim_type = 1

    # add info that would come from the CST annual simulation
    sam_iph_dict_lcoh_demo["annual_electricity_consumption"] = 0.0
    sam_iph_dict_lcoh_demo["electricity_rate"] = 0.0

    # need to replace annual energy with PV + CST
    sam_iph_dict_lcoh_demo["annual_energy"] = 1e7   #kWh

    # add costs that need to be updated with PV + CST
    sam_iph_dict_lcoh_demo["total_installed_cost"] = 15352231.048 * 1.5
    sam_iph_dict_lcoh_demo["fixed_operating_cost"] = 103758 * 1.4

    lcoh_solved_dict = ssc_sim.cmod_lcoefcr_design_from_dict(sam_iph_dict_lcoh_demo, True)

    is_success = lcoh_solved_dict["cmod_success"]
    print ('SSC simulation(s) successful = ', is_success)
    if (is_success == 1):
        print("lcoe_fcr = ", lcoh_solved_dict["lcoe_fcr"], " cents/kwh")

    else:
        print ('ANNUAL SIM FAIL FAIL FAIL FAIL FAIL')

elif(solution_mode == "annual_sim"):

    sam_iph_dict_annual_sim = copy.deepcopy(sam_iph_dict_base)

    # Assume we change T_loop_out here, then makes sense to apply startup/shutdown temp as a temperature difference
    deltaT_outlet_threshold = 20
    sam_iph_dict_annual_sim["T_startup"] = sam_iph_dict_annual_sim["T_loop_out"] - deltaT_outlet_threshold
    sam_iph_dict_annual_sim["T_shutdown"] = sam_iph_dict_annual_sim["T_startup"]

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

elif(solution_mode == "CST_sim_then_hybrid_lcoh"):

    sam_iph_dict_annual_sim = copy.deepcopy(sam_iph_dict_base)

    iph_solved_dict = ssc_sim.cmod_trough_iph_lcoh_from_dict(sam_iph_dict_annual_sim, True)  # ssc_sim.cmod_mspt_from_dict(sam_mspt_dict)

    is_success = iph_solved_dict["cmod_success"]    
    print ('SSC simulation(s) successful = ', is_success)
    if (is_success == 1):

        print("CST-only lcoe_fcr = ", iph_solved_dict["lcoe_fcr"], " cents/kwh")

        # Get CST values
        cst_annual_energy = iph_solved_dict["annual_energy"]
        cst_total_installed_cost = iph_solved_dict["total_installed_cost"]
        cst_annual_electricity_consumption = iph_solved_dict["annual_electricity_consumption"]
        cst_electricity_rate = iph_solved_dict["electricity_rate"]
        cst_fixed_operating_cost = iph_solved_dict["fixed_operating_cost"]

        print(cst_annual_energy, cst_total_installed_cost, cst_annual_electricity_consumption,
              cst_electricity_rate, cst_fixed_operating_cost)


        # Get PV values
        pv_annual_energy = 0.678 * cst_annual_energy      # EXAMPLE
        pv_total_installed_cost = 0.5678 * cst_total_installed_cost    # EXAMPLE
        pv_fixed_operating_cost = 0.456 * cst_fixed_operating_cost  # EXAMPLE


        # Set up dictionary for LCOH model
        sam_iph_dict_lcoh_demo = copy.deepcopy(sam_iph_dict_base)

        # add info that would come from the CST annual simulation
        sam_iph_dict_lcoh_demo["annual_electricity_consumption"] = cst_annual_electricity_consumption
        sam_iph_dict_lcoh_demo["electricity_rate"] = cst_electricity_rate

        # need to replace annual energy with PV + CST
        sam_iph_dict_lcoh_demo["annual_energy"] = cst_annual_energy + pv_annual_energy

        # add costs that need to be updated with PV + CST
        sam_iph_dict_lcoh_demo["total_installed_cost"] = cst_total_installed_cost + pv_total_installed_cost
        sam_iph_dict_lcoh_demo["fixed_operating_cost"] = cst_fixed_operating_cost + pv_fixed_operating_cost

        lcoh_solved_dict = ssc_sim.cmod_lcoefcr_design_from_dict(sam_iph_dict_lcoh_demo, True)

        is_success = lcoh_solved_dict["cmod_success"]
        print ('SSC simulation(s) successful = ', is_success)
        if (is_success == 1):
            print("CST-PV lcoe_fcr = ", lcoh_solved_dict["lcoe_fcr"], " cents/kwh")

        else:
            print ('ANNUAL SIM FAIL FAIL FAIL FAIL FAIL')


    else:
        print ('CST simulation failed')
