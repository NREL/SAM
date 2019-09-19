##################################################
## Set relative file paths ##
import csv

import sys
import os
import numpy as np

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
    des_par["htf"] = 17;                   #[-] Solar salt
    des_par["T_htf_hot_des"] = 720.0;       #[C] HTF design hot temperature (PHX inlet)
    des_par["dT_PHX_hot_approach"] = 20.0; #[C/K] default 20. Temperature difference between hot HTF and turbine inlet       
    des_par["T_amb_des"] = 35.0;           #[C] Ambient temperature at design 
    des_par["dT_mc_approach"] = 6.0;       #[C] Use 6 here per Neises & Turchi 19. Temperature difference between main compressor CO2 inlet and ambient air
    des_par["site_elevation"] = 588;       #[m] Elevation of Daggett, CA. Used to size air cooler...
    des_par["W_dot_net_des"] = 50.0;       #[MWe] Design cycle power output (no cooling parasitics)
    
    des_par["design_method"] = 2;          #[-] 1 = specify efficiency, 2 = specify total recup UA, 3 = Specify each recuperator design (see inputs below)
    des_par["eta_thermal_des"] = 0.44;     #[-] Power cycle thermal efficiency
    des_par["UA_recup_tot_des"] = 8*1000   #[kW/K]
    
    des_par["cycle_config"] = 1            #[1] = RC, [2] = PC
           
    des_par["is_recomp_ok"] = 1;           #[-] Use simple cycle for now. 1 = Yes, 0 = simple cycle only
    des_par["is_P_high_fixed"] = 1;        #[-] 0 = No, optimize. 1 = Yes
    des_par["is_PR_fixed"] = 0;            #[-] 0 = No, >0 = Yes
    des_par["des_objective"] = 1;          #[-] 2 = hit min deltaT then max efficiency, != 2 = max efficiency
    des_par["min_phx_deltaT"] = 1000;      #[C] Min allowable deltaT across PHX
    des_par["rel_tol"] = 3;                #[-] Baseline solver and optimization relative tolerance exponent (10^-rel_tol)
           
    # Weiland & Thimsen 2016
    # In most studies, 85% is an accepted isentropic efficiency for either the main or recompression compressors, and is the recommended assumption.
    des_par["eta_isen_mc"] = 0.80;         #[-] Main compressor isentropic efficiency
    des_par["eta_isen_rc"] = 0.82;         #[-] Recompressor isentropic efficiency
    des_par["eta_isen_pc"] = 0.82;         #[-] Precompressor isentropic efficiency
    
    # Weiland & Thimsen 2016
    # Recommended turbine efficiencies are 90% for axial turbines above 30 MW, and 85% for radial turbines below 30 MW.
    des_par["eta_isen_t"] = 0.87;          #[-] Turbine isentropic efficiency

    des_par["P_high_limit"] = 25;          #[MPa] Cycle high pressure limit

    # Weiland & Thimsen 2016
    # Multiple literature sources suggest that recuperator cold side (high pressure) pressure drop of
    # approximately 140 kPa (20 psid) and a hot side (low pressure) pressure drop of 280 kPa (40 psid) can be reasonably used.
    # Note: Unclear what the low pressure assumption is in this study, could be significantly lower for direct combustion cycles
    eff_max = 1.0
    deltaP_recup_HP = 0.005  #[-] 0.0056 = 0.14[MPa]/25[MPa]
    deltaP_recup_LP = 0.005	 #[-] 0.0311 = 0.28[MPa]/9[MPa]
        # LTR
    des_par["LTR_design_code"] = 3  # 1 = UA, 2 = min dT, 3 = effectiveness
    des_par["LTR_UA_des_in"] = 2200.0  # [kW/K] (required if LTR_design_code == 1 and design_method == 3)
    des_par["LTR_min_dT_des_in"] = 8.0  # [C] (required if LTR_design_code == 2 and design_method == 3)
    des_par["LTR_eff_des_in"] = 0.895  # [-] (required if LTR_design_code == 3 and design_method == 3)
    des_par["LT_recup_eff_max"] = eff_max #[-] Maximum effectiveness low temperature recuperator
    des_par["LTR_LP_deltaP_des_in"] = deltaP_recup_LP 	#[-]
    des_par["LTR_HP_deltaP_des_in"] = deltaP_recup_HP	#[-]    
        # HTR
    des_par["HTR_design_code"] = 3  # 1 = UA, 2 = min dT, 3 = effectiveness
    des_par["HTR_UA_des_in"] = 2800.0  # [kW/K] (required if LTR_design_code == 1 and design_method == 3)
    des_par["HTR_min_dT_des_in"] = 14.0  # [C] (required if LTR_design_code == 2 and design_method == 3)
    des_par["HTR_eff_des_in"] = 0.943  # [-] (required if LTR_design_code == 3 and design_method == 3)
    des_par["HT_recup_eff_max"] = eff_max #[-] Maximum effectiveness high temperature recuperator
    des_par["HTR_LP_deltaP_des_in"] = deltaP_recup_LP	#[-]
    des_par["HTR_HP_deltaP_des_in"] = deltaP_recup_HP	#[-]    
        # PHX
    des_par["PHX_co2_deltaP_des_in"] = deltaP_recup_HP	#[-]
    des_par["dT_PHX_cold_approach"] = 20;  #[C/K] default 20. Temperature difference between cold HTF and cold CO2 PHX inlet
        # Air Cooler
    des_par["deltaP_cooler_frac"] = 0.005; #[-] Fraction of CO2 inlet pressure that is design point cooler CO2 pressure drop
    des_par["fan_power_frac"] = 0.02;      #[-] Fraction of net cycle power consumed by air cooler fan. 2% here per Turchi et al.
        # Default    
    des_par["deltaP_counterHX_frac"] = 0.0054321; #[-] Fraction of CO2 inlet pressure that is design point counterflow HX (recups & PHX) pressure drop

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

"Main simulation parameters"


"False only solves the cycle design and creates T-s & P-h diagrams"
"True will run off-design simulation defined by 'part_load_list' and 'T_amb_od_' parameters"
"The default off-design simulation cases solve in around 45 minutes"
is_run_off_design = True
"If not running off-design sweeps/matrix, should the code generate UDPC tables?"
is_generate_udpc = False
    
"Setup off-deign cases"
if(is_run_off_design):
    "Choose off design temperature method"
    "True will use 'T_amb_od_' parameters and result in a lot of off-design cases and a longer run time"
    "It is helpful to visualize off-design trends to identify potential unrealistic behavior"
    "False will only solve for the values in 'od_matrix_temps'"
    is_od_temp_sweep = False
    part_load_list = [1.0, 0.75, 0.5, 0.4]   # Part-load levels. Results at part loads < 0.5 should be carefully reviewed"
    T_amb_od_low = 20                        # Coldest off-design temperature (min allowable comp inlet is 32 C)
    T_amb_od_high = 55                       # Warmest off-design temperature
    T_amb_od_step = 1                        # Spacing betwen off-design temperatures

    "True will write a csv with results from 'od_matrix_var_names()' for each part load level and OD temperature from 'od_matrix_temps'"
    is_write_od_matrix = True
    od_matrix_temps = [25,35,40]
"***************************"
"***************************"




"Design point simulation and plotting"
# Instantiate sco2 cycle simulation class
c_sco2 = sco2_solve.C_sco2_sim(1)           # Initialize as same cycle config as specified above
		
# Get default design parameters. These are different than the "baseline" default parameters in "sco2_cycle_ssc.py"
sco2_des_par_default = get_sco2_design_parameters()
c_sco2.overwrite_default_design_parameters(sco2_des_par_default)

# Setup string for naming files
des_sim_label_str = "T_amb_des" + '{:.1f}'.format(sco2_des_par_default["T_amb_des"]) + "C__UA_recup" + '{:.1f}'.format(sco2_des_par_default["UA_recup_tot_des"]/1000)

# Solve the design point and plot
c_sco2.solve_sco2_case()            # Run design simulation
print("\nWas the design point solution successful = ",c_sco2.m_solve_success)
c_sco2.m_also_save_csv = True
c_sco2.save_m_solve_dict(des_sim_label_str)
sol_dict = c_sco2.m_solve_dict

    
c_i_cycle_plot = cy_plt.C_sco2_TS_PH_plot(sol_dict)
c_i_cycle_plot.is_save_plot = True
c_i_cycle_plot.file_name = "cycle_design_plot__" + des_sim_label_str
c_i_cycle_plot.plot_new_figure()
###################################
###################################

"Off-design simulation and plotting"
if(is_run_off_design):

    T_od_str = ""
    
    ## Reset to run off-design analysis of single design point
    sco2_des_par_default = get_sco2_design_parameters()
    c_sco2.overwrite_default_design_parameters(sco2_des_par_default)

    # Set up off-design
    if(is_od_temp_sweep):
        od_sim_label_str = des_sim_label_str + "__T_amb_od_par__part_load"
    else:
        od_sim_label_str = des_sim_label_str + "__T_amb_od_par__array__part_load"
        
    
    od_sol_dict_list = 0
    od_matrix = 0
        
    matrix_var_names = od_matrix_var_names()
    var_info_metrics = sco2_solve.get_des_od_label_unit_info__combined()
    
    for i_part_load in part_load_list:
    
        i_part_load_str = '{:.2f}'.format(i_part_load)
        i_sim_label_str = od_sim_label_str + i_part_load_str
        
        i_T = T_amb_od_low
        T_amb_od = []
        if(is_od_temp_sweep):
            while i_T <= T_amb_od_high:    
                T_amb_od.append([sco2_des_par_default["T_htf_hot_des"],i_part_load,i_T,1.0,1.0])
                i_T = i_T + T_amb_od_step
        else:
            for i_T_matrix in od_matrix_temps:
                T_amb_od.append([sco2_des_par_default["T_htf_hot_des"],i_part_load,i_T_matrix,1.0,1.0])
        
        mod_base_dict = {"od_cases" : T_amb_od}    # [[mod_base_dict["T_htf_hot_des"],1.0,mod_base_dict["T_amb_des"],-1.0]]
    
        c_sco2.overwrite_des_par_base(mod_base_dict)    # Overwrite baseline design parameters
        c_sco2.solve_sco2_case()            # Run design simulation
    
        print(c_sco2.m_solve_dict["eta_thermal_calc"])
        print("\nDid the simulation code with " 
          "modified design parameters solve successfully = ",c_sco2.m_solve_success)
    
        c_sco2.m_also_save_csv = True
        c_sco2.save_m_solve_dict("" + i_sim_label_str)   # Save design 
    
        if(is_write_od_matrix):
    
            for od_matrix_T in od_matrix_temps:
    
                i_mT = list(c_sco2.m_solve_dict["T_amb_od"]).index(od_matrix_T)                
    
                od_list_vals = [0 for i in range(len(matrix_var_names))]
                for i_index, i_matrix_var_name in enumerate(matrix_var_names):
                    od_list_vals[i_index] = c_sco2.m_solve_dict[var_info_metrics[i_matrix_var_name].od_var][i_mT]
                    
                if(not(isinstance(od_matrix,list))):
                    od_matrix = [od_list_vals]
                else:
                    od_matrix.append(od_list_vals)
    
    
        if(od_sol_dict_list == 0):
            od_sol_dict_list = [c_sco2.m_solve_dict]
            od_leg_list = ["part load = " + i_part_load_str]
        else:
            od_sol_dict_list.append(c_sco2.m_solve_dict)
            od_leg_list.append("part load = " + i_part_load_str)
    
    if(is_write_od_matrix):
        
        with open(od_sim_label_str+'__matrix.csv','w', newline='') as f:
            w = csv.writer(f)
        
            n_cols = len(od_matrix)
            n_rows = len(od_matrix[0])
            
            for i in range(n_rows):
                
                row_var_name = matrix_var_names[i]
                
                row = [var_info_metrics[row_var_name].l_label]
                            
                for j in range(n_cols):
                    row.append(od_matrix[j][i])
                    
                w.writerow(row)
                
            f.close()
    
    #"Make off-design plot: Performance vs. Ambient Temperature"
    od_plot = cy_plt.C_OD_stacked_outputs_plot(od_sol_dict_list)
    od_plot.x_var = "T_amb"
    od_plot.var_info_metrics["W_dot"].limit_var = ""
    od_plot.is_label_leg_cols = ["Recompression Cycle - Inventory Control"]
    od_plot.list_leg_spec = od_leg_list
    od_plot.n_leg_cols = len(part_load_list)
    od_plot.max_rows = 5
    od_plot.bb_y_max_is_leg = 0.95
    od_plot.is_save = True
    od_plot.file_name = od_sim_label_str
    
    od_plot.y_vars = ["eta", "W_dot", "Q_dot", "f_recomp", "LP_cooler_W_dot_fan",           #system
                      "MC_T_in", "MC_P_in", "MC_T_out", "MC_P_out", "MC_rho_in",   #MC states
                      "MC_m_dot", "MC_W_dot", "MC_eta", "MC_phi", "MC_tip_speed",   #MC performance
                      "RC_m_dot", "RC_W_dot", "RC_eta", "RC_phi", "RC_tip_speed",   #RC performance
                      "RC_T_in", "RC_P_in", "RC_T_out", "RC_P_out", "LP_cooler_rho_in",  #RC states
                      "t_m_dot", "t_W_dot", "t_eta", "t_nu", "t_tip_speed",         #turbine performance
                      "t_T_in", "t_P_in", "t_T_out", "t_P_out", "t_N",              #turbine states
                      "T_htf_cold", "PHX_T_co2_in", "HTR_HP_T_in", "HTR_LP_T_out", "LTR_HP_T_out", #Remaining temps      
                      "LTR_eff", "LTR_q_dot", "LTR_min_dT", "LTR_LP_deltaP", "LTR_HP_deltaP",   #LTR performance
                      "HTR_eff", "HTR_q_dot", "HTR_min_dT", "HTR_LP_deltaP", "HTR_HP_deltaP",   #HTR performance
                      "PHX_eff", "Q_dot", "PHX_co2_deltaP", "m_dot_HTF", "T_HTF"]               #PHX performance
    
    od_plot.create_plot()

elif(is_generate_udpc):

    ## Reset to run off-design analysis of single design point
    sco2_des_par_default = get_sco2_design_parameters()
    c_sco2.overwrite_default_design_parameters(sco2_des_par_default)

    mod_base_dict = {"od_generate_udpc": [1.0]}

    c_sco2.overwrite_des_par_base(mod_base_dict)  # Overwrite baseline design parameters
    c_sco2.solve_sco2_case()  # Run design simulation

    print(c_sco2.m_solve_dict["eta_thermal_calc"])
    print("\nDid the simulation code with "
          "modified design parameters solve successfully = ", c_sco2.m_solve_success)

    c_sco2.m_also_save_csv = True
    c_sco2.save_m_solve_dict(des_sim_label_str + "__udpc_results")  # Save design

    udpc_data = c_sco2.m_solve_dict["udpc_table"]

    with open("udpc_output" + '.csv', 'w', newline='') as f:
        w = csv.writer(f)
        w.writerows(udpc_data)
    f.close()

    n_T_htf = int(c_sco2.m_solve_dict["udpc_n_T_htf"])
    n_T_amb = int(c_sco2.m_solve_dict["udpc_n_T_amb"])
    n_m_dot_htf = int(c_sco2.m_solve_dict["udpc_n_m_dot_htf"])

    cy_plt.plot_udpc_results(udpc_data, n_T_htf, n_T_amb, n_m_dot_htf)

