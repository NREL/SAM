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



##########################################
"Cycle design simulation with default parameters"
c_sco2 = sco2_solve.C_sco2_sim(1)   # Initialize to the recompression cycle default (1)
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
"Modifying the cycle design parameters"
mod_base_dict = {"T_htf_hot_des" : 620, "cycle_config" : 2}
c_sco2.overwrite_des_par_base(mod_base_dict)    # Overwrite baseline design parameters
c_sco2.solve_sco2_case()            # Run design simulation
print(c_sco2.m_solve_dict)
print("\nDid the simulation code with" 
      "modified design parameters solve successfully = ",c_sco2.m_solve_success)
c_sco2.m_also_save_csv = True
c_sco2.save_m_solve_dict("design_solution__modified_pars")   # Save design solution dictionary
sol_dict__mod_pars = c_sco2.m_solve_dict

##########################################
"Comparing two cycle designs"
c_comp_plot = cy_plt.C_sco2_TS_PH_overlay_plot(sol_dict__default_pars, sol_dict__mod_pars)
c_comp_plot.is_save_plot = True
c_comp_plot.plot_new_figure()

##########################################
"Running a parametric study on one design parameter"
c_sco2.reset_des_par_base_to_default_RC()
T_HTF_in_par_list = list(np.arange(570,721,25))
T_HTF_in_par_dict_list = []
for T_HTF_in in T_HTF_in_par_list:
    T_HTF_in_par_dict_list.append({"T_htf_hot_des" : T_HTF_in})
c_sco2.solve_sco2_parametric(T_HTF_in_par_dict_list)
print("\nDid the parametric analyses solve successfully = ",c_sco2.m_par_solve_success)
c_sco2.m_also_save_csv = True
c_sco2.save_m_par_solve_dict("T_HTF_parametric")
sol_dict_parametric = c_sco2.m_par_solve_dict

##########################################
"Plotting a 1D parametric study"
par_plot = cy_plt.C_des_stacked_outputs_plot([sol_dict_parametric])
par_plot.x_var = "T_HTF"
par_plot.y_vars = ["eta","MC_P_in","PHX_dT"]
par_plot.is_legend = False
par_plot.max_rows = 2
par_plot.is_save = True;
par_plot.file_name = "T_HTF_par_plot"
par_plot.create_plot()

##########################################
"Plotting one cycle design from a parametric solution dictionary"
i_plot = len(sol_dict_parametric["T_htf_hot_des"]) - 1
dict_i_plot = sco2_solve.get_one_des_dict_from_par_des_dict(sol_dict_parametric, "T_htf_hot_des", i_plot)
c_i_cycle_plot = cy_plt.C_sco2_TS_PH_plot(dict_i_plot)
c_i_cycle_plot.is_save_plot = True
c_i_cycle_plot.file_name = "cycle_design_plots__T_HTF_hottest"
c_i_cycle_plot.plot_new_figure()

##########################################
##########################################
##########################################
##########################################
##########################################


     