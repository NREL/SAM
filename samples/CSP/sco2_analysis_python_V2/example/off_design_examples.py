##################################################
## Set relative file paths ##
import sys
import os

parentDir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.append(parentDir)

from core import sco2_cycle_ssc as sco2_solve
from core import sco2_plots as cy_plt

##################################################
##################################################

def get_sco2_design_parameters():
    des_par = {}
    des_par["htf"] = 6                  # [-] 68% KCl, 32% MgCl2.
    des_par["T_htf_hot_des"] = 720.0    # [C] HTF design hot temperature (PHX inlet) [cite: sunshot]
    des_par[
        "dT_PHX_hot_approach"] = 20.0  # [C/K] default 20. Temperature difference between hot HTF and turbine inlet [cite: neises/turchi]
    des_par[
        "dT_PHX_cold_approach"] = 20  # [C/K] default 20. Temperature difference between cold HTF and cold CO2 PHX inlet [enforces CR = 1]
    des_par["T_amb_des"] = 40.0  # [C] Ambient temperature at design [cite: neises/turchi]
    des_par[
        "dT_mc_approach"] = 6.0  # [C] Use 6 here per [Neises & Turchi 19]. Temperature difference between main compressor CO2 inlet and ambient air
    des_par["site_elevation"] = 588  # [m] Elevation of Daggett, CA. Used to size air cooler...
    des_par["W_dot_net_des"] = 50.0  # [MWe] Design cycle power output (no cooling parasitics)

    des_par[
        "design_method"] = 3  # [-] 1 = specify efficiency, 2 = specify total recup UA, 3 = Specify each recuperator design (see inputs below)
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

def print_useful_OD_outputs(dict_results):

    var_info =  sco2_solve.get_des_od_label_unit_info__combined()

    # See 'get_sam_mspt_sco2_label_unit_info' and 'get_des_od_label_unit_info__ind_inputs' methods in 'sco2_cycle_ssc.py'
    vars = ["eta", "eta_less_cooling", "W_dot_net_less_cooling", "MC_P_in", "MC_T_in", "MC_P_out", "mc_cooler_W_dot_fan", "T_htf_cold"]

    print("At the design-point solution where the HTF temp input = ", '{:.4}'.format(dict_results[var_info["T_HTF"].des_var]),
              "C, the normalized HTF mass flow = 1.0",
              "[-], the ambient temperature = ", '{:.3}'.format(dict_results[var_info["T_amb"].des_var]),
              "C and:")

    for i_var in vars:
        print("   ", var_info[i_var].l_label, "is =",
              '{:.4}'.format((dict_results[var_info[i_var].des_var])))

    for i in range(len(dict_results[var_info["eta"].od_var])):

        print("\nAt a HTF temp input = ", '{:.4}'.format(dict_results[var_info["T_HTF"].od_var][i]),
              "C, a normalized HTF mass flow = ", '{:.3}'.format(dict_results[var_info["m_dot_HTF"].od_var][i]),
              "[-], ambient temperature = ", '{:.3}'.format(dict_results[var_info["T_amb"].od_var][i]),
              "C")

        for i_var in vars:

            print("   The off-design", var_info[i_var].l_label, "is =",
                  '{:.4}'.format((dict_results[var_info[i_var].od_var][i])))


######################################
"Example #1"
"Simulate off-design performance w/ optimized control and fixed shaft speeds"
######################################
def run_example_1():

    # Instantiate sco2 cycle simulation class
    c_sco2 = sco2_solve.C_sco2_sim(1)           # Initialize as same cycle config as specified above

    # Get default design parameters. These are different than the "baseline" default parameters in "sco2_cycle_ssc.py"
    sco2_des_par_default = get_sco2_design_parameters()
    c_sco2.overwrite_default_design_parameters(sco2_des_par_default)

    # Setup string for naming files
    des_sim_label_str = "T_amb_des" + '{:.1f}'.format(sco2_des_par_default["T_amb_des"])

    # Get design values of off-design inputs for reference
    T_htf_hot_des = sco2_des_par_default["T_htf_hot_des"]   # C
    part_load_des = 1.0   # - (normalized)
    T_amb_des = sco2_des_par_default["T_amb_des"]   # C
    f_N_rc = 1.0    # Use design point shaft speed
    f_N_mc = 1.0    # Use design point shaft speed
    f_N_pc = 1.0    # Use design point shaft speed

    # One off-design simulation at design conditions
    T_amb_od_parametric = [[T_htf_hot_des, part_load_des, T_amb_des, f_N_rc, f_N_mc, f_N_pc]]

    # Another off-design simulation at a new off-design temperature
    T_amb_OD = T_amb_des - 5.0      # C
    T_amb_od_parametric.append([T_htf_hot_des, part_load_des, T_amb_OD, f_N_rc, f_N_mc, f_N_pc])

    # Create a new dictionary the defines only the simulation inputs variables that change versus 'sco2_des_par_default' above
    # Here, only new variable is 'od_cases'
    mod_base_dict = {"od_cases" : T_amb_od_parametric}    #
    # Modify the current baseline inputs with new partial dictionary
    c_sco2.overwrite_des_par_base(mod_base_dict)    # Overwrite baseline input parameters
    # Set flag to not print key design solution metrics
    c_sco2.m_is_print_key_outputs = False
    # Run simulation. First the code will solve the design point, and then it will solve all off-design cases using the design solution
    c_sco2.solve_sco2_case()            # Run design simulation

    # Check if the code threw exceptions. This can happen if the code is missing required input or potentially if inputs are significantly different than expected
    print("\nDid the off-design simulation solve without the code throwing exceptions = ",c_sco2.m_solve_success)

    # Print some useful outputs
    print_useful_OD_outputs(c_sco2.m_solve_dict)

    # Set flag to tell class to save a csv when 'save_m_solve_dict' is called
    c_sco2.m_also_save_csv = True
    # Then call 'save_m_solve_dict' to save results to both a json (default) and csv (optionally set by 'm_also_save_csv')
    c_sco2.save_m_solve_dict(des_sim_label_str + "_OD_T_amb")   # Save design

    ###############################################################
    "Plot off-design results: Performance vs. Ambient Temperature"
    ###############################################################
    # Create a list result dictionaries from independent off-design simulations
    # In this example, there is only one off-design parametric analysis
    od_sol_dict_list = [c_sco2.m_solve_dict]
    # Create another list defining string labels for each dictionary in 'od_sol_dict_list'
    od_leg_list = ["part load = " + '{:.2f}'.format(part_load_des)]
    # Instantiate the off-design plot class using the list of result dictionaries
    od_plot = cy_plt.C_OD_stacked_outputs_plot(od_sol_dict_list)
    # Define the independent variable in the off-design simulations
    od_plot.x_var = "T_amb"
    # Set plot legend parameters
    od_plot.list_leg_spec = od_leg_list
    od_plot.n_leg_cols = len(od_leg_list)
    # Set number of rows - ideally this is informed by '.y_vars' to create a full grid of subplots
    od_plot.max_rows = 5
    od_plot.bb_y_max_is_leg = 0.95
    # Optionally save plot during 'create_plot' method
    od_plot.is_save = True
    od_plot.file_name = des_sim_label_str + "__OD_vs_T_amb"
    # Define variables for each subplot
    # See 'get_des_od_label_unit_info__calc_metrics' method in 'sco2_plots.py' in the Core folder for more information
    od_plot.y_vars = ["eta_less_cooling", "W_dot_net_less_cooling", "Q_dot", "f_recomp", "mc_cooler_W_dot_fan",           #system
                      "MC_T_in", "MC_P_in", "MC_T_out", "MC_P_out", "MC_rho_in",   #MC states
                      "MC_m_dot", "MC_W_dot", "MC_eta", "MC_phi", "MC_N_perc",   #MC performance
                      "RC_m_dot", "RC_W_dot", "RC_eta", "RC_phi", "RC_N_perc",   #RC performance
                      "RC_T_in", "RC_P_in", "RC_T_out", "RC_P_out", "mc_cooler_rho_in",  #RC states
                      "t_m_dot", "t_W_dot", "t_eta", "t_nu", "t_tip_speed",         #turbine performance
                      "t_T_in", "t_P_in", "t_T_out", "t_P_out", "t_N",              #turbine states
                      "T_htf_cold", "PHX_T_co2_in", "HTR_HP_T_in", "HTR_LP_T_out", "LTR_HP_T_out", #other temps
                      "LTR_eff", "LTR_q_dot", "LTR_min_dT", "LTR_LP_deltaP", "LTR_HP_deltaP",   #LTR performance
                      "HTR_eff", "HTR_q_dot", "HTR_min_dT", "HTR_LP_deltaP", "HTR_HP_deltaP",   #HTR performance
                      "PHX_eff", "Q_dot", "PHX_co2_deltaP", "m_dot_HTF", "T_HTF"]               #PHX performance

    # Create plot. If 'is_save' is True, this should save a png of the plot of the working directory
    od_plot.create_plot()

######################################
"Example #2"
"Simulate off-design performance w/ user specified control parameters"
######################################
def run_example_2():

    # Instantiate sco2 cycle simulation class
    c_sco2 = sco2_solve.C_sco2_sim(1)           # Initialize as same cycle config as specified above

    # Get default design parameters. These are different than the "baseline" default parameters in "sco2_cycle_ssc.py"
    sco2_des_par_default = get_sco2_design_parameters()
    c_sco2.overwrite_default_design_parameters(sco2_des_par_default)

    # Setup string for naming files
    des_sim_label_str = "T_amb_des" + '{:.1f}'.format(sco2_des_par_default["T_amb_des"])

    # Get design values of off-design inputs for reference
    T_htf_hot_des = sco2_des_par_default["T_htf_hot_des"]   # C
    part_load_des = 1.0   # - (normalized)
    T_amb_des = sco2_des_par_default["T_amb_des"]   # C
    P_mc_in_des = 9.878 # MPa (design inlet pressure when using parameters from 'get_sco2_design_parameters')
    T_mc_in_des = 46    # C (design inlet temperature when using parameters from 'get_sco2_design_parameters')
    T_pc_in_des = -999  # C (default case is a recompression cycle, so no precompressor inlet)
    f_N_rc = 1.0    # Use design point shaft speed
    f_N_mc = 1.0    # Use design point shaft speed
    f_N_pc = 1.0    # Use design point shaft speed

    # One off-design simulation at design conditions
    P_mc_in_od_parametric = [[T_htf_hot_des, part_load_des, T_amb_des,
                              P_mc_in_des, T_mc_in_des, T_pc_in_des,
                              f_N_rc, f_N_mc, f_N_pc]]

    # Another off-design simulation at a new off-design temperature
    P_mc_in_OD = P_mc_in_des - 1.0      # MPa
    P_mc_in_od_parametric.append([T_htf_hot_des, part_load_des, T_amb_des,
                                  P_mc_in_OD, T_mc_in_des, T_pc_in_des,
                                  f_N_rc, f_N_mc, f_N_pc])

    # Create a new dictionary the defines only the simulation inputs variables that change versus 'sco2_des_par_default' above
    # Here, only new variable is 'od_set_control'
    mod_base_dict = {"od_set_control" : P_mc_in_od_parametric}

    # Modify the current baseline inputs with new partial dictionary
    c_sco2.overwrite_des_par_base(mod_base_dict)    # Overwrite baseline input parameters
    # Set flag to not print key design solution metrics
    c_sco2.m_is_print_key_outputs = False
    # Run simulation. First the code will solve the design point, and then it will solve all off-design cases using the design solution
    c_sco2.solve_sco2_case()            # Run design simulation

    # Check if the code threw exceptions. This can happen if the code is missing required input or potentially if inputs are significantly different than expected
    print("\nDid the off-design simulation solve without the code throwing exceptions = ",c_sco2.m_solve_success)

    # Print some useful outputs
    print_useful_OD_outputs(c_sco2.m_solve_dict)

    # Set flag to tell class to save a csv when 'save_m_solve_dict' is called
    c_sco2.m_also_save_csv = True
    # Then call 'save_m_solve_dict' to save results to both a json (default) and csv (optionally set by 'm_also_save_csv')
    c_sco2.save_m_solve_dict(des_sim_label_str + "_OD_set_control__P_mc_in")   # Save design

    ###############################################################
    "Plot off-design results: Performance vs. Compressor Inlet Pressure"
    ###############################################################
    # Create a list result dictionaries from independent off-design simulations
    # In this example, there is only one off-design parametric analysis
    od_sol_dict_list = [c_sco2.m_solve_dict]
    # Create another list defining string labels for each dictionary in 'od_sol_dict_list'
    od_leg_list = ["part load = " + '{:.2f}'.format(part_load_des)]
    # Instantiate the off-design plot class using the list of result dictionaries
    od_plot = cy_plt.C_OD_stacked_outputs_plot(od_sol_dict_list)
    # Define the independent variable in the off-design simulations
    od_plot.x_var = "MC_P_in"
    # Set plot legend parameters
    od_plot.list_leg_spec = od_leg_list
    od_plot.n_leg_cols = len(od_leg_list)
    # Set number of rows - ideally this is informed by '.y_vars' to create a full grid of subplots
    od_plot.max_rows = 5
    od_plot.bb_y_max_is_leg = 0.95
    # Optionally save plot during 'create_plot' method
    od_plot.is_save = True
    od_plot.file_name = des_sim_label_str + "__OD_vs_P_MC_in"
    # Define variables for each subplot
    # See 'get_des_od_label_unit_info__calc_metrics' method in 'sco2_plots.py' in the Core folder for more information
    od_plot.y_vars = ["eta_less_cooling", "W_dot_net_less_cooling", "Q_dot", "f_recomp", "mc_cooler_W_dot_fan",
                      # system
                      "MC_T_in", "MC_P_in", "MC_T_out", "MC_P_out", "MC_rho_in",  # MC states
                      "MC_m_dot", "MC_W_dot", "MC_eta", "MC_phi", "MC_N_perc",  # MC performance
                      "RC_m_dot", "RC_W_dot", "RC_eta", "RC_phi", "RC_N_perc",  # RC performance
                      "RC_T_in", "RC_P_in", "RC_T_out", "RC_P_out", "mc_cooler_rho_in",  # RC states
                      "t_m_dot", "t_W_dot", "t_eta", "t_nu", "t_tip_speed",  # turbine performance
                      "t_T_in", "t_P_in", "t_T_out", "t_P_out", "t_N",  # turbine states
                      "T_htf_cold", "PHX_T_co2_in", "HTR_HP_T_in", "HTR_LP_T_out", "LTR_HP_T_out",  # other temps
                      "LTR_eff", "LTR_q_dot", "LTR_min_dT", "LTR_LP_deltaP", "LTR_HP_deltaP",  # LTR performance
                      "HTR_eff", "HTR_q_dot", "HTR_min_dT", "HTR_LP_deltaP", "HTR_HP_deltaP",  # HTR performance
                      "PHX_eff", "Q_dot", "PHX_co2_deltaP", "m_dot_HTF", "T_HTF"]  # PHX performance

    # Create plot. If 'is_save' is True, this should save a png of the plot of the working directory
    od_plot.create_plot()


######################################
"Example #1"
"Simulate off-design performance w/ optimized control and fixed shaft speeds"
######################################
run_example_1()

######################################
"Example #2"
"Simulate off-design performance w/ user specified control parameters"
######################################
run_example_2()
