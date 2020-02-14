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

ac_par_dict = sco2_solve.get_air_cooler_default_des_pars()
c_ac = sco2_solve.C_sco2_air_cooler_design(ac_par_dict)
print(c_ac.m_solve_dict)

od_mode = "fix_T_co2_out__solve_W_dot"
#od_mode = "fix_W_dot_fan__solve_T_co2_out_vs_T_amb"
#od_mode = "fix_W_dot_fan__solve_T_co2_out_vs_P_co2"

if(od_mode == "fix_T_co2_out__solve_W_dot"):
    # Columns: T_co2_hot_C, P_co2_hot_MPa, T_co2_cold_C, m_dot_CO2_ND, T_amb_C. Rows: cases
    od_calc_W_dot_fan_array =[ac_par_dict["T_co2_hot_des"],ac_par_dict["P_co2_hot_des"],ac_par_dict["T_co2_cold_des"],
                     0.8, ac_par_dict["T_amb_des"]]

    m_dot_ND_list = list(np.arange(1.1,0.49,-0.1))
    od_calc_W_dot_fan_matrix = []
    for i_mdot in m_dot_ND_list:
        od_calc_W_dot_fan_array[3] = i_mdot
        od_calc_W_dot_fan_matrix.append(od_calc_W_dot_fan_array.copy())

    ac_od_dict = {"od_calc_W_dot_fan": od_calc_W_dot_fan_matrix}
    c_ac_od = sco2_solve.C_sco2_air_cooler_des_and_offdes(ac_par_dict, ac_od_dict)
    print(c_ac_od.m_solve_dict)

    fig1, a_ax = plt.subplots(nrows = 2,num = 1,figsize=(3.5,3.5))

    a_ax[0].plot(c_ac_od.m_solve_dict["m_dot_co2_od_ND"], c_ac_od.m_solve_dict["W_dot_fan_od"])
    a_ax[0].plot(1.0, c_ac_od.m_solve_dict["W_dot_fan_des"], 'ko')
    a_ax[0].set_ylabel("Fan\n Power [MWe]")
    a_ax[0].grid(which = 'both', color = 'gray', alpha = 1)

    a_ax[1].plot(c_ac_od.m_solve_dict["m_dot_co2_od_ND"], c_ac_od.m_solve_dict["deltaP_co2_od"])
    a_ax[1].plot(1.0, c_ac_od.m_solve_dict["deltaP_co2_des"], 'ko')
    a_ax[1].set_xlabel("Normalized CO2 mass flow")
    a_ax[1].set_ylabel("CO2\n Pressure Drop [MPa]")
    a_ax[1].grid(which = 'both', color = 'gray', alpha = 1)

    fig1.suptitle("Constant Inlet T & P\n Constant Outlet T", fontsize=14)

    plt.tight_layout(rect=(0.01,0.01,1.0,0.90))

    plt.savefig("W_dot_fan_vs_m_dot_ND.png")
    plt.close()

elif(od_mode == "fix_W_dot_fan__solve_T_co2_out_vs_T_amb"):

    # Columns: T_co2_hot_C, P_co2_hot_MPa, W_dot_fan_ND, m_dot_CO2_ND, T_amb_C. Rows: cases
    od_calc_T_co2_out_array = [ac_par_dict["T_co2_hot_des"], ac_par_dict["P_co2_hot_des"], 1.0,
                               1.0, ac_par_dict["T_amb_des"]]

    T_amb_des = ac_par_dict["T_amb_des"]
    T_amb_od_list = list(np.arange(T_amb_des - 5, T_amb_des + 5.5, 1.0))
    od_calc_T_co2_out_matrix = []
    for i_T_amb in T_amb_od_list:
        od_calc_T_co2_out_array[4] = i_T_amb
        od_calc_T_co2_out_matrix.append(od_calc_T_co2_out_array.copy())

    ac_od_dict = {"od_calc_T_co2_cold": od_calc_T_co2_out_matrix}
    c_ac_od = sco2_solve.C_sco2_air_cooler_des_and_offdes(ac_par_dict, ac_od_dict)
    print(c_ac_od.m_solve_dict)

    fig1, a_ax = plt.subplots(nrows=2, num=1, figsize=(3.5, 3.5))

    a_ax[0].plot(c_ac_od.m_solve_dict["T_amb_od"], c_ac_od.m_solve_dict["T_co2_cold_od"])
    a_ax[0].plot(c_ac_od.m_solve_dict["T_amb_des"], c_ac_od.m_solve_dict["T_co2_cold_des"], 'ko')
    a_ax[0].set_ylabel("CO2 Outlet\n Temp [C]")
    a_ax[0].grid(which='both', color='gray', alpha=1)

    a_ax[1].plot(c_ac_od.m_solve_dict["T_amb_od"], c_ac_od.m_solve_dict["q_dot_od_ND"])
    a_ax[1].plot(c_ac_od.m_solve_dict["T_amb_des"], 1.0, 'ko')
    a_ax[1].set_xlabel("Ambient Temperature [C]")
    a_ax[1].set_ylabel("Normalized\n Heat Rejection")
    a_ax[1].grid(which='both', color='gray', alpha=1)

    fig1.suptitle("Constant Inlet T & P\n Constant Fan Power", fontsize=14)

    plt.tight_layout(rect=(0.01, 0.01, 1.0, 0.90))

    plt.savefig("T_co2_out_vs_T_amb.png")
    plt.close()

elif(od_mode == "fix_W_dot_fan__solve_T_co2_out_vs_P_co2"):

    # Columns: T_co2_hot_C, P_co2_hot_MPa, W_dot_fan_ND, m_dot_CO2_ND, T_amb_C. Rows: cases
    od_calc_T_co2_out_array = [ac_par_dict["T_co2_hot_des"], ac_par_dict["P_co2_hot_des"], 1.0,
                               1.0, ac_par_dict["T_amb_des"]]

    P_co2_hot_des = ac_par_dict["P_co2_hot_des"]
    P_co2_hot_list = list(np.arange(0.98*P_co2_hot_des, 1.021*P_co2_hot_des, 0.0025*P_co2_hot_des))
    od_calc_T_co2_out_matrix = []
    for i_P_co2_hot in P_co2_hot_list:
        od_calc_T_co2_out_array[1] = i_P_co2_hot
        od_calc_T_co2_out_matrix.append(od_calc_T_co2_out_array.copy())

    ac_od_dict = {"od_calc_T_co2_cold": od_calc_T_co2_out_matrix}
    c_ac_od = sco2_solve.C_sco2_air_cooler_des_and_offdes(ac_par_dict, ac_od_dict)
    print(c_ac_od.m_solve_dict)

    fig1, a_ax = plt.subplots(nrows=2, num=1, figsize=(3.5, 3.5))

    a_ax[0].plot(c_ac_od.m_solve_dict["P_co2_hot_od"], c_ac_od.m_solve_dict["T_co2_cold_od"])
    a_ax[0].plot(c_ac_od.m_solve_dict["P_co2_hot_des"], c_ac_od.m_solve_dict["T_co2_cold_des"], 'ko')
    a_ax[0].set_ylabel("CO2 Outlet\n Temp [C]")
    a_ax[0].grid(which='both', color='gray', alpha=1)

    a_ax[1].plot(c_ac_od.m_solve_dict["P_co2_hot_od"], c_ac_od.m_solve_dict["q_dot_od_ND"])
    a_ax[1].plot(c_ac_od.m_solve_dict["P_co2_hot_des"], 1.0, 'ko')
    a_ax[1].set_xlabel("CO2 Inlet Pressure [MPa]")
    a_ax[1].set_ylabel("Normalized\n Heat Rejection")
    a_ax[1].grid(which='both', color='gray', alpha=1)

    fig1.suptitle("Constant Inlet T & P\n Constant Fan Power", fontsize=14)

    plt.tight_layout(rect=(0.01, 0.01, 1.0, 0.90))

    plt.savefig("T_co2_out_vs_P_co2.png")
    plt.close()
     